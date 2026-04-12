

#' @export
load_xylo_metadata <- function(meta_file_path) {
  
  sheet_names <- readxl::excel_sheets(meta_file_path)
  
  sheet_names <- setdiff(
    sheet_names,
    c("instructions", "DropList", "ListOfVariables")
  )
  
  sheet_data <- setNames(
    lapply(sheet_names, function(sheet) {
      readxl::read_excel(meta_file_path, sheet = sheet)
    }),
    sheet_names
  )
  
  # ensure consistent column names
  sheet_data <- lapply(sheet_data, function(df) {
    names(df) <- trimws(names(df))
    df
  })
  
  # normalize sample_date safely (only if present)
  if ("sample" %in% names(sheet_data)) {
    if ("sample_date" %in% names(sheet_data$sample)) {
      suppressWarnings(
        sheet_data$sample$sample_date <- as.Date(
          as.numeric(sheet_data$sample$sample_date),
          origin = "1899-12-30"
        )
      )
    }
  }
  
  return(sheet_data)
}