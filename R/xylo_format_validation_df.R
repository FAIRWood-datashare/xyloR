
#' @export
xylo_format_validation_df <- function(obs_df) {
  
  # -------------------------------------------------------
  # 1. TYPE SAFETY
  # -------------------------------------------------------
  if (is.null(obs_df) || !is.data.frame(obs_df)) {
    return(data.frame(
      type = "error",
      source = "obs",
      message = "obs is not a data.frame",
      stringsAsFactors = FALSE
    ))
  }
  
  # -------------------------------------------------------
  # 2. BASIC COLUMN CHECK (SAFE GUARD)
  # -------------------------------------------------------
  required_cols <- c("sample_date", "tree_label", "site_label")
  missing <- setdiff(required_cols, names(obs_df))
  
  if (length(missing) > 0) {
    return(data.frame(
      type = "error",
      source = "obs",
      message = paste0("Missing required columns: ", paste(missing, collapse = ", ")),
      stringsAsFactors = FALSE
    ))
  }
  
  # -------------------------------------------------------
  # 3. REMOVE TEMPLATE ROWS SAFELY
  # -------------------------------------------------------
  data <- if (nrow(obs_df) > 6) obs_df[-c(1:6), , drop = FALSE] else obs_df
  
  # -------------------------------------------------------
  # 4. PLACEHOLDER VALIDATION (SAFE DEFAULT = OK)
  # -------------------------------------------------------
  return(data.frame(
    type = character(0),
    source = character(0),
    message = character(0)
  ))
}