

#' @export
validate_xylo_pipeline <- function(obs = NULL, meta = NULL) {
  
  report_list <- list()
  
  # ----------------------------
  # META
  # ----------------------------
  if (!is.null(meta)) {
    
    # NEW: schema validation FIRST
    schema_check <- validate_list_variables_schema(meta$list_variables)
    
    if (!identical(schema_check$Issue, "OK")) {
      return(schema_check)  # stop early (fail fast)
    }
    
    meta_report <- meta_format_validation_core(
      meta$sheets,
      meta$list_variables,
      meta$droplist
    )
    
    if (nrow(meta_report) > 0) {
      report_list$meta <- meta_report
    }
  }
  
  # ----------------------------
  # OBS
  # ----------------------------
  if (!is.null(obs)) {
    
    obs_report <- xylo_format_validation_core(
      obs$sheets,
      obs$list_variables,
      obs$droplist
    )
    
    if (nrow(obs_report) > 0) {
      report_list$obs <- obs_report
    }
  }
  
  # ----------------------------
  # MERGE
  # ----------------------------
  if (length(report_list) == 0) {
    return(data.frame(Sheet=character(), Column=character(), Issue=character()))
  }
  
  dplyr::bind_rows(report_list)
}