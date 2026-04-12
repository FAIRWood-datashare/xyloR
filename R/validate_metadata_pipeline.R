

#' @export
validate_metadata_pipeline <- function(meta_path, obs_path = NULL) {
  
  meta_results <- meta_format_validation(meta_path)
  
  obs_results <- NULL
  if (!is.null(obs_path) && file.exists(obs_path)) {
    obs_results <- xylo_format_validation(obs_path)
  }
  
  if (!is.null(obs_results)) {
    return(rbind(obs_results, meta_results))
  } else {
    return(meta_results)
  }
}