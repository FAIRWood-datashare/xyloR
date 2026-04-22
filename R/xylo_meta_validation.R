

#' @export
xylo_meta_validation <- function(meta) {
  
  # Guard: meta must exist
  if (is.null(meta)) {
    return(data.frame(
      type = "error",
      source = "metadata",
      message = "Metadata object is NULL"
    ))
  }
  
  # Run validation pipeline safely
  result <- tryCatch({
    
    validate_metadata_pipeline(meta)
    
  }, error = function(e) {
    
    data.frame(
      type = "error",
      source = "metadata",
      message = e$message
    )
  })
  
  # Normalize output:
  # ✅ VALID = empty data frame
  if (is.null(result) || nrow(result) == 0) {
    return(data.frame())
  }
  
  # Otherwise return validation issues
  result
}