

#' @export
normalize_xylo_metadata <- function(meta) {
  
  sheets <- meta$sheets
  
  # -----------------------------
  # SAMPLE SHEET FIXES
  # -----------------------------
  if (!is.null(sheets$sample)) {
    
    df <- sheets$sample
    
    # fix date conversion safely
    if ("sample_date" %in% names(df)) {
      df$sample_date <- suppressWarnings(
        as.Date(as.numeric(df$sample_date), origin = "1899-12-30")
      )
    }
    
    sheets$sample <- df
  }
  
  # -----------------------------
  # SITE SHEET FIXES
  # -----------------------------
  if (!is.null(sheets$site)) {
    
    df <- sheets$site
    
    numeric_cols <- c("latitude", "longitude", "elevation")
    
    for (col in numeric_cols) {
      if (col %in% names(df)) {
        df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      }
    }
    
    sheets$site <- df
  }
  
  # -----------------------------
  # TREE SHEET FIXES
  # -----------------------------
  if (!is.null(sheets$tree)) {
    
    df <- sheets$tree
    
    numeric_cols <- c("tree_dbh", "tree_height", "tree_age")
    
    for (col in numeric_cols) {
      if (col %in% names(df)) {
        df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      }
    }
    
    sheets$tree <- df
  }
  
  meta$sheets <- sheets
  meta
}