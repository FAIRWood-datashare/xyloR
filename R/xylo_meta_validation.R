

#' @export
xylo_meta_validation <- function(meta_path) {
  
  if (is.null(meta_path) || !file.exists(meta_path)) {
    return(data.frame(
      type = "error",
      source = "metadata",
      message = "Invalid or missing meta file path"
    ))
  }
  
  # STEP 1: load metadata
  raw <- read_xylo_meta_raw(meta_path)
  meta <- build_xylo_meta_clean(raw)
  
  # STEP 2: run pipeline (your existing system)
  result <- tryCatch({
    
    validate_metadata_pipeline(meta)
    
  }, error = function(e) {
    
    data.frame(
      type = "error",
      source = "metadata",
      message = e$message
    )
  })
  
  # STEP 3: normalize output
  if (is.null(result) || nrow(result) == 0) {
    return(data.frame(
      type = "ok",
      source = "metadata",
      message = "validation passed"
    ))
  }
  
  result
}