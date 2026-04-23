

#' @export
#' @export
validate_metadata_pipeline <- function(meta, obs = NULL) {
  
  # -------------------------
  # META VALIDATION (contract object)
  # -------------------------
  meta_results <- tryCatch({
    
    xylo_meta_validation(meta)
    
  }, error = function(e) {
    
    data.frame(
      type = "error",
      source = "meta",
      message = e$message
    )
  })
  
  # -------------------------
  # OBS VALIDATION (contract object)
  # -------------------------
  obs_results <- NULL
  
  if (!is.null(obs)) {
    
    obs_results <- tryCatch({
      
      xylo_format_validation(obs)
      
    }, error = function(e) {
      
      data.frame(
        type = "error",
        source = "obs",
        message = e$message
      )
    })
  }
  
  # -------------------------
  # COMBINE RESULTS
  # -------------------------
  if (!is.null(obs_results)) {
    return(dplyr::bind_rows(obs_results, meta_results))
  } else {
    return(meta_results)
  }
}