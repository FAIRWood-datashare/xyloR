

#' @export
obs_format_validation <- function(obs_path) {
  
  sheet_names <- setdiff(
    readxl::excel_sheets(obs_path),
    c("instructions", "DropList", "ListOfVariables")
  )
  
  sheet_data <- setNames(
    lapply(sheet_names, function(sheet) {
      readxl::read_excel(obs_path, sheet = sheet)
    }),
    sheet_names
  )
  
  sheet_listvariables <- readxl::read_excel(obs_path, sheet = "ListOfVariables")
  sheet_droplist      <- readxl::read_excel(obs_path, sheet = "DropList")
  
  xylo_format_validation_core(
    sheet_data,
    sheet_listvariables,
    sheet_droplist
  )
}