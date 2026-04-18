# =========================================================
# XYLO DATA CONTRACT LAYER
# =========================================================
# This file defines:
# 1. Raw reader (Excel ingestion only)
# 2. Clean builder (all parsing + fixing)
# 3. Optional schema enforcement
# =========================================================


# ---------------------------------------------------------
# REQUIRED SCHEMA (light contract)
# ---------------------------------------------------------

XYLO_OBS_SCHEMA <- c(
  "sample_date",
  "sample_id",
  "tree_species",
  "species_code",
  "tree_label",
  "plot_label",
  "site_label",
  "network_label",
  "sample_label",
  "measure_type",
  "measure_repetition",
  "cz",
  "ez",
  "tz",
  "mz",
  "pr",
  "sample_comment"
)


# =========================================================
# 1. RAW READER (NO CLEANING)
# =========================================================

#' Read XYLO observation Excel file (RAW)
#'
#' @param path path to .xlsx file
#' @return list of raw sheets
#' @export
read_xylo_obs_raw <- function(path) {
  
  stopifnot(is.character(path), file.exists(path))
  
  wb <- openxlsx::loadWorkbook(path)
  
  df <- openxlsx::readWorkbook(
    wb,
    sheet = "Xylo_obs_data",
    startRow = 1,
    colNames = TRUE
  )
  
  # -------------------------------------------------------
  # REMOVE TEMPLATE ROWS (CONTENT-BASED SAFE FILTER)
  # -------------------------------------------------------
  df <- df[-c(1:6), , drop = FALSE]  # Remove first 6 rows (template headers)
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  list(
    obs = df,
    droplist = openxlsx::readWorkbook(wb, sheet = "DropList"),
    raw = TRUE
  )
}

# =========================================================
# 2. CLEAN BUILDER (MAIN FIX LAYER)
# =========================================================

#' Build clean XYLO observation dataset
#'
#' @param raw output of read_xylo_obs_raw()
#' @return cleaned data.frame
#' @export
build_xylo_obs_clean <- function(raw) {
  
  if (is.null(raw) || is.null(raw$obs)) {
    stop("Invalid raw input to build_xylo_obs_clean()")
  }
  
  df <- raw$obs

  # -------------------------------------------------------
  # SAFE: force character AFTER we confirm structure
  # -------------------------------------------------------
  df[] <- lapply(df, as.character)
  

  # -------------------------------------------------------
  # SAFE DATE PARSING (ROBUST + FLEXIBLE)
  # -------------------------------------------------------
  
  if ("sample_date" %in% names(df)) {
    df$sample_date <- parse_sample_dates(df$sample_date)
  } else {
    df$sample_date <- as.Date(NA)
  }
  
  # -------------------------------------------------------
  # JOIN DROPLIST SAFELY
  # -------------------------------------------------------
  if (!is.null(raw$droplist)) {
    
    if (all(c("tree_species") %in% names(df))) {
      df <- dplyr::left_join(df, raw$droplist, by = "tree_species")
    }
  }
  
  # -------------------------------------------------------
  # SCHEMA SAFE-FILL (NO HARD FAILURE)
  # -------------------------------------------------------
  XYLO_OBS_SCHEMA <- c(
    "sample_date","sample_id","tree_species","species_code",
    "tree_label","plot_label","site_label","network_label",
    "sample_label","measure_type","measure_repetition",
    "cz","ez","tz","mz","pr","sample_comment"
  )
  
  missing_cols <- setdiff(XYLO_OBS_SCHEMA, names(df))
  
  for (col in missing_cols) {
    df[[col]] <- NA_character_
  }
  
  df <- df[, XYLO_OBS_SCHEMA, drop = FALSE]
  
  df
}

# =========================================================
# 3. MAIN PUBLIC API (OPTIONAL CONVENIENCE WRAPPER)
# =========================================================

#' Load XYLO observation data (clean)
#'
#' @param obs_file_path path to Excel file
#' @return cleaned data.frame
#' @export
load_xylo_obs_clean_contract <- function(obs_file_path) {
  
  raw <- read_xylo_obs_raw(obs_file_path)
  build_xylo_obs_clean(raw)
}

# =========================================================
# RAW META READER (NEW)
# =========================================================
read_xylo_meta_raw <- function(meta_path) {
  
  if (is.null(meta_path) || !nzchar(meta_path)) {
    stop("Invalid meta path")
  }
  
  sheets <- readxl::excel_sheets(meta_path)
  
  read_safe <- function(sheet) {
    
    df <- readxl::read_excel(
      meta_path,
      sheet = sheet,
      col_names = TRUE,
      guess_max = 10000
    )
    
    # ❌ REMOVED: df[-c(1:5), ]  (THIS WAS BREAKING EVERYTHING)
    
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df
  }
  
  data <- lapply(sheets, read_safe)
  names(data) <- tolower(sheets)
  
  list(
    sample = data[["sample"]],
    tree   = data[["tree"]],
    site   = data[["site"]],
    raw_path = meta_path
  )
}

# =========================================================
# META CLEAN BUILDER
# =========================================================
build_xylo_meta_clean <- function(raw) {
  
  if (is.null(raw) || !is.list(raw)) {
    stop("Invalid meta input (not a list)")
  }
  
  if (is.null(raw$sample) || is.null(raw$tree) || is.null(raw$site)) {
    stop("Invalid meta raw structure")
  }
  
  sample <- tibble::as_tibble(raw$sample)
  tree   <- tibble::as_tibble(raw$tree)
  site   <- tibble::as_tibble(raw$site)
  
  names(sample) <- tolower(names(sample))
  names(tree)   <- tolower(names(tree))
  names(site)   <- tolower(names(site))
  
  # SAFE numeric conversion
  numeric_cols <- intersect(c("latitude","longitude","elevation"), names(site))
  
  for (col in numeric_cols) {
    site[[col]] <- suppressWarnings(as.numeric(site[[col]]))
  }
  
  list(
    sample = sample,
    tree   = tree,
    site   = site
  )
}