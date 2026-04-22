
#' @export
xylo_validation_engine <- function(ctx) {
  
  # -------------------------
  # OBS VALIDATION
  # -------------------------
  obs_val <- tryCatch({
    
    req(ctx$data$obs)
    
    xylo_format_validation(ctx$files$obs_file$datapath)
    
  }, error = function(e) {
    
    data.frame(
      type = "error",
      source = "obs",
      message = e$message
    )
  })
  
  # -------------------------
  # META VALIDATION
  # -------------------------
  meta_val <- tryCatch({
    
    req(ctx$data$meta)
    
    xylo_meta_validation(ctx$data$meta)
    
  }, error = function(e) {
    
    data.frame(
      type = "error",
      source = "meta",
      message = e$message
    )
  })
  
  # -------------------------
  # COMBINED TABLE
  # -------------------------
  all_tbl <- dplyr::bind_rows(obs_val, meta_val)
  
  list(
    obs = obs_val,
    meta = meta_val,
    all = all_tbl,
    all_valid = is.data.frame(all_tbl) && nrow(all_tbl) == 0
  )
}