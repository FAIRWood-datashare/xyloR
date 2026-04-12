

#' Load XYLO observation data
#'
#' @param obs_file_path path to file
#' @export
load_xylo_obs_clean <- function(obs_file_path) {
  
  sheet_names <- setdiff(
    readxl::excel_sheets(obs_file_path),
    c("instructions", "DropList", "ListOfVariables")
  )
  
  sheet_data <- setNames(
    lapply(sheet_names, function(sheet) {
      readxl::read_excel(obs_file_path, sheet = sheet)
    }),
    sheet_names
  )
  
  sheet_listvariables <- readxl::read_excel(obs_file_path, sheet = "ListOfVariables")
  sheet_droplist      <- readxl::read_excel(obs_file_path, sheet = "DropList")
  
  list(
    sheets = sheet_data,
    list_variables = sheet_listvariables,
    droplist = sheet_droplist,
    raw_path = obs_file_path
  )
}
