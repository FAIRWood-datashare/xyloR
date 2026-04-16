# =========================================================
# xylo_io.R
# Centralized Excel IO + data normalization layer
# =========================================================

# =========================================================
# 1. RAW IO LAYER (PRIVATE HELPERS)
# =========================================================

.read_wb <- function(file) {
  openxlsx::loadWorkbook(file)
}

.read_sheet <- function(wb, sheet, startRow = 1, colNames = FALSE) {
  openxlsx::readWorkbook(
    wb,
    sheet = sheet,
    startRow = startRow,
    colNames = colNames
  )
}

.read_cell <- function(wb, sheet, row, col) {
  val <- openxlsx::readWorkbook(
    wb,
    sheet = sheet,
    rows = row,
    cols = col,
    colNames = FALSE
  )
  
  if (nrow(val) == 0 || ncol(val) == 0) {
    return(NA_character_)
  }
  
  cell <- val[[1, 1]]
  if (is.na(cell)) return(NA_character_)
  
  as.character(cell)
}

# =========================================================
# 2. OBSERVATION DATA LOADER
# =========================================================

read_xylo_cell <- function(file, sheet, row, col) {
  
  val <- openxlsx::readWorkbook(
    file,
    sheet = sheet,
    rows = row,
    cols = col,
    colNames = FALSE
  )
  
  if (nrow(val) == 0 || ncol(val) == 0) return(NA_character_)
  
  cell <- val[1, 1, drop = TRUE]
  
  if (is.na(cell)) return(NA_character_)
  
  as.character(cell)
}

load_xylo_obs_clean <- function(file) {
  
  wb <- .read_wb(file)
  
  raw <- .read_sheet(
    wb,
    sheet = "Xylo_obs_data",
    startRow = 1,
    colNames = FALSE
  )
  
  header <- make.names(as.character(raw[1, ]), unique = TRUE)
  
  df <- raw[8:nrow(raw), , drop = FALSE]
  colnames(df) <- header
  
  df <- tibble::as_tibble(df)
  
  # ---------------------------------------------------------
  # standardization
  # ---------------------------------------------------------
  
  date_col <- grep("date", names(df), ignore.case = TRUE, value = TRUE)[1]
  
  if (!is.na(date_col)) {
    df$sample_date <- parse_sample_dates(df[[date_col]])
    df <- df[!is.na(df$sample_date), ]
  }
  
  df
}

# =========================================================
# 3. SITE INFO LOADER
# =========================================================

extract_site_info <- function(file) {
  
  wb <- .read_wb(file)
  
  info <- .read_sheet(
    wb,
    sheet = "obs_data_info",
    startRow = 6,
    colNames = FALSE
  )
  
  info <- as.data.frame(info)
  
  # ---------------------------------------------------------
  # handle missing site labels
  # ---------------------------------------------------------
  
  if (ncol(info) == 3) {
    
    obs <- .read_sheet(
      wb,
      sheet = "Xylo_obs_data",
      startRow = 1,
      colNames = FALSE
    )
    
    site_labels <- unique(as.character(obs[[1]]))
    site_labels <- site_labels[!is.na(site_labels)]
    site_labels <- site_labels[seq_len(nrow(info))]
    
    info <- cbind(site_labels, info)
  }
  
  colnames(info) <- c("site_label", "latitude", "longitude", "elevation")
  
  info
}

# =========================================================
# 4. METADATA LOADER
# =========================================================

load_xylo_metadata_clean <- function(file) {
  
  sheets <- setdiff(
    readxl::excel_sheets(file),
    c("instructions", "DropList", "ListOfVariables")
  )
  
  meta <- setNames(
    lapply(sheets, function(sh) {
      readxl::read_excel(file, sheet = sh)[-c(1:6), , drop = FALSE]
    }),
    sheets
  )
  
  meta
}


# =========================================================
# 5. HIERARCHY BUILDER (SUNBURST STRUCTURE)
# =========================================================

build_xylo_hierarchy <- function(meta) {
  
  # =========================================================
  # 1. SAFE COLUMN RESOLVER
  # =========================================================
  safe_col <- function(df, candidates) {
    hit <- candidates[candidates %in% names(df)][1]
    if (is.na(hit) || is.null(hit)) return(NULL)
    df[[hit]]
  }
  
  # =========================================================
  # 2. EXTRACT TABLES SAFELY
  # =========================================================
  sample <- meta[["sample"]]
  tree   <- meta[["tree"]]
  site   <- meta[["site"]]
  
  # ensure required columns exist early
  stopifnot("tree_label" %in% names(sample))
  stopifnot("tree_label" %in% names(tree))
  stopifnot("site_label" %in% names(site))
  
  # =========================================================
  # 3. JOIN (safe but minimal)
  # =========================================================
  df <- dplyr::left_join(sample, tree, by = "tree_label")
  df <- dplyr::left_join(df, site, by = "site_label")
  
  # =========================================================
  # 4. RESOLVE AMBIGUOUS / DUPLICATED COLUMNS
  # =========================================================
  plot_col <- safe_col(df, c("plot_label", "plot_label.x", "plot_label.y"))
  site_col <- safe_col(df, c("site_label", "site_label.x", "site_label.y"))
  net_col  <- safe_col(df, c("network_label", "network_label.x", "network_label.y"))
  
  if (is.null(site_col)) stop("Missing site_label after join")
  if (is.null(plot_col)) plot_col <- site_col
  if (is.null(net_col)) net_col <- site_col
  
  df$plot_label    <- as.character(plot_col)
  df$site_label    <- as.character(site_col)
  df$network_label <- as.character(net_col)
  
  # =========================================================
  # 5. DATE HANDLING (safe)
  # =========================================================
  if ("sample_date" %in% names(df)) {
    df$sample_date <- as.Date(as.numeric(df$sample_date), origin = "1899-12-30")
    df$year <- lubridate::year(df$sample_date)
  } else {
    df$year <- NA_integer_
  }
  
  # =========================================================
  # 6. HIERARCHY KEYS (STRICT STRUCTURE)
  # =========================================================
  df$network_label <- dplyr::coalesce(df$network_label, df$site_label)
  
  df$site_label_full <- ifelse(
    df$site_label == df$network_label,
    paste0(df$network_label, "_site"),
    paste0(df$network_label, "__", df$site_label)
  )
  
  df$plot_label_full <- ifelse(
    is.na(df$plot_label) | df$plot_label == df$site_label,
    paste0(df$site_label_full, "_plot"),
    paste0(df$site_label_full, "__", df$plot_label)
  )
  
  df$tree_label_full <- paste0(df$plot_label_full, "__", df$tree_label)
  df$year_label      <- paste0(df$tree_label_full, "__", df$year)
  df$sample_label    <- paste0(df$year_label, "__", df$sample_id)
  
  # =========================================================
  # 7. BUILD CLEAN LEVELS (NO OVERCOUNTING)
  # =========================================================
  
  df_network <- df |>
    dplyr::distinct(network_label) |>
    dplyr::mutate(
      id = network_label,
      parent = "",
      value = 1
    )
  
  df_site <- df |>
    dplyr::distinct(site_label_full, network_label) |>
    dplyr::count(site_label_full, network_label, name = "value") |>
    dplyr::rename(id = site_label_full, parent = network_label)
  
  df_plot <- df |>
    dplyr::distinct(plot_label_full, site_label_full) |>
    dplyr::count(plot_label_full, site_label_full, name = "value") |>
    dplyr::rename(id = plot_label_full, parent = site_label_full)
  
  df_tree <- df |>
    dplyr::distinct(tree_label_full, plot_label_full) |>
    dplyr::count(tree_label_full, plot_label_full, name = "value") |>
    dplyr::rename(id = tree_label_full, parent = plot_label_full)
  
  # =========================================================
  # 8. FINAL CLEAN STRUCTURE
  # =========================================================
  out <- dplyr::bind_rows(df_network, df_site, df_plot, df_tree) |>
    dplyr::distinct(id, parent, .keep_all = TRUE) |>
    dplyr::mutate(
      label = sub(".*__", "", id),
      value = as.numeric(value),
      text  = paste0(label, " (", value, ")")
    )
  
  return(out)
}

# =========================================================
# 6. PIPELINE ENTRY POINT (OPTIONAL BUT USEFUL)
# =========================================================

load_xylo_pipeline_data <- function(obs_file, meta_file = NULL) {
  
  obs <- load_xylo_obs_clean(obs_file)
  
  meta <- if (!is.null(meta_file)) {
    load_xylo_metadata_clean(meta_file)
  } else {
    NULL
  }
  
  list(
    obs = obs,
    meta = meta
  )
}