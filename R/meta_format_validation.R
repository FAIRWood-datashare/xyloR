#' Metadata Format Validation
#'
#' @param meta_file Path to the metadata Excel file.
#' @return A tibble containing validation issues for each sheet and column in the metadata.
#' @export
#' 
#' @examples
#' \dontrun{
#' meta_file <- system.file("extdata", "Ltal.2007_xylo_meta_2025-03-08.xlsx", package = "xyloR")
#' report <- meta_format_validation(meta_file)
#' }
#' 
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate pull count summarise
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#' @importFrom readxl excel_sheets read_excel
#' @importFrom stats na.omit setNames
#' 
meta_format_validation <- function(meta_input) {
  
  # if file path → load it
  if (is.character(meta_input)) {
    
    sheet_names <- setdiff(
      readxl::excel_sheets(meta_input),
      c("instructions", "DropList", "ListOfVariables")
    )
    
    sheet_data <- setNames(
      lapply(sheet_names, function(sheet) {
        readxl::read_excel(meta_input, sheet = sheet)
      }),
      sheet_names
    )
    
    sheet_listvariables <- readxl::read_excel(meta_input, sheet = "ListOfVariables")
    sheet_droplist      <- readxl::read_excel(meta_input, sheet = "DropList")
    
  } else {
    stop("meta_input must be file path for now")
  }
  
  meta_format_validation_core(
    sheet_data,
    sheet_listvariables,
    sheet_droplist
  )
}


