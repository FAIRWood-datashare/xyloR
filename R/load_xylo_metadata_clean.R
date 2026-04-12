

#' @export
load_xylo_metadata_clean <- function(meta_file_path) {
  
  stopifnot(file.exists(meta_file_path))
  
  # Required system sheets
  system_sheets <- c("instructions", "DropList", "ListOfVariables")
  
  sheet_names <- readxl::excel_sheets(meta_file_path)
  data_sheets <- setdiff(sheet_names, system_sheets)
  
  # Load system sheets
  list_variables <- readxl::read_excel(meta_file_path, sheet = "ListOfVariables")
  droplist <- readxl::read_excel(meta_file_path, sheet = "DropList")
  
  # Load data sheets
  sheets <- setNames(
    lapply(data_sheets, function(sheet) {
      
      df <- readxl::read_excel(meta_file_path, sheet = sheet)
      
      # remove Excel template header rows (your format rule)
      df <- df[-c(1:6), , drop = FALSE]
      
      # remove fully empty rows
      df <- df[rowSums(is.na(df)) != ncol(df), , drop = FALSE]
      
      df
    }),
    data_sheets
  )
  
  list(
    sheets = sheets,
    list_variables = list_variables,
    droplist = droplist,
    raw_path = meta_file_path
  )
}