

#' @export
meta_format_validation_core <- function(sheet_data, sheet_listvariables, sheet_droplist) {
  
  rules <- build_validation_rules(sheet_listvariables)
  
  validate_with_rules(sheet_data, rules, sheet_droplist)
}