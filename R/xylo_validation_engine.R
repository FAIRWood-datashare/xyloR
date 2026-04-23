xylo_validation_engine <- function(obs, meta) {
  
  message("ENGINE OBS CLASS: ", class(obs))
  message("ENGINE META CLASS: ", class(meta))
  
  safe_df <- function(x, source_name) {
    
    if (is.null(x) || !is.data.frame(x)) {
      return(data.frame(
        type = "error",
        source = source_name,
        message = "Validator returned invalid output",
        stringsAsFactors = FALSE
      ))
    }
    
    # enforce required columns
    required <- c("type", "source", "message")
    missing <- setdiff(required, names(x))
    
    if (length(missing) > 0) {
      return(data.frame(
        type = "error",
        source = source_name,
        message = paste("Missing columns:", paste(missing, collapse = ", ")),
        stringsAsFactors = FALSE
      ))
    }
    
    x
  }
  
  # ----------------------------
  # OBS VALIDATION
  # ----------------------------
  obs_val <- tryCatch({
    safe_df(xylo_format_validation(obs), "obs")
  }, error = function(e) {
    data.frame(
      type = "error",
      source = "obs",
      message = e$message,
      stringsAsFactors = FALSE
    )
  })
  
  # ----------------------------
  # META VALIDATION
  # ----------------------------
  meta_val <- tryCatch({
    safe_df(xylo_meta_validation(meta), "meta")
  }, error = function(e) {
    data.frame(
      type = "error",
      source = "meta",
      message = e$message,
      stringsAsFactors = FALSE
    )
  })
  
  # ----------------------------
  # SAFE BIND
  # ----------------------------
  all_tbl <- dplyr::bind_rows(obs_val, meta_val)
  
  if (!is.data.frame(all_tbl)) {
    all_tbl <- data.frame(
      type = "error",
      source = "engine",
      message = "Validation engine output invalid",
      stringsAsFactors = FALSE
    )
  }
  
  all_tbl
  # list(
  #   obs = obs_val,
  #   meta = meta_val,
  #   all = all_tbl,
  #   all_valid = nrow(all_tbl) == 0
  # )
  
  message("---- OBS VALIDATION ----")
  print(obs_val)
  
  message("---- META VALIDATION ----")
  print(meta_val)
  
  message("---- FINAL TABLE ----")
  print(all_tbl)
}
