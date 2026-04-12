#' Xylodata Format Validation
#'
#' @param xylo_file Path to the metadata Excel file.
#' @return A tibble containing validation issues for each sheet and column in the xylo data.
#' @export
#' 
#' @examples
#' \dontrun{
#' xylo_file <- system.file("extdata", "Ltal.2007_xylo_data_2025-09-01.xlsx", package = "xyloR")
#' report <- xylo_format_validation(xylo_file)
#' }
#' 
#' @importFrom utils read.csv 
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate pull
#' @importFrom purrr map_dfr 
#' @importFrom readxl excel_sheets read_excel
#' @importFrom stats na.omit setNames
#' 
#' 
xylo_format_validation <- function(xylo_file) {
  
  sheet_names <- setdiff(
    readxl::excel_sheets(xylo_file),
    c("instructions", "DropList", "ListOfVariables")
  )
  
  sheet_data <- setNames(
    lapply(sheet_names, function(sheet)
      readxl::read_excel(xylo_file, sheet = sheet)
    ),
    sheet_names
  )
  
  sheet_listvariables <- readxl::read_excel(xylo_file, sheet = "ListOfVariables")
  sheet_droplist <- readxl::read_excel(xylo_file, sheet = "DropList")
  
  col_constraints <- sheet_listvariables %>%
    dplyr::select(Table, Name, `cell constraints`, Mandatory, Domain, `data origin`)
  
  report <- list()
  
  for (sheet in sheet_names) {
    
    data <- sheet_data[[sheet]][-1:-6, ]
    
    constraints <- col_constraints %>%
      dplyr::filter(Table == sheet)
    
    report[[sheet]] <- validate_table_by_constraints(
      data = data,
      constraints = constraints,
      droplist = sheet_droplist
    )
  }
  
  tibble::tibble(
    Sheet  = rep(names(report), lengths(report)),
    Column = unlist(lapply(report, `[[`, "Column")),
    Issue  = unlist(lapply(report, `[[`, "Issue"))
  )
}
