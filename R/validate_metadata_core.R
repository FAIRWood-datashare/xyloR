
#' @export
validate_metadata_core <- function(meta) {
  
  # NO FILES, NO PATHS, NO READING
  
  if (is.null(meta$site) || nrow(meta$site) == 0) {
    return(data.frame(
      type = "error",
      source = "site",
      message = "Site table missing or empty"
    ))
  }
  
  return(data.frame(
    type = character(0),
    source = character(0),
    message = character(0),
    stringsAsFactors = FALSE
  ))
}