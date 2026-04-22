


#' Validate schema of list_variables table
#' @export
validate_list_variables_schema <- function(sheet_listvariables) {
  
  required_cols <- c(
    "Table",
    "Name",
    "cell constraints",
    "Mandatory",
    "Domain",
    "data origin"
  )
  
  report <- list()
  
  # ---------------------------
  # CHECK REQUIRED COLUMNS
  # ---------------------------
  missing_cols <- setdiff(required_cols, names(sheet_listvariables))
  
  if (length(missing_cols) > 0) {
    return(data.frame(
      Issue = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  # ---------------------------
  # CHECK EMPTY TABLE
  # ---------------------------
  if (nrow(sheet_listvariables) == 0) {
    return(data.frame(
      Issue = "list_variables is empty"
    ))
  }
  
  # ---------------------------
  # CHECK CORE FIELD INTEGRITY
  # ---------------------------
  
  # Table must not be NA
  if (any(is.na(sheet_listvariables$Table))) {
    report <- append(report, list(
      data.frame(Issue = "NA values found in 'Table'")
    ))
  }
  
  # Name must not be NA
  if (any(is.na(sheet_listvariables$Name))) {
    report <- append(report, list(
      data.frame(Issue = "NA values found in 'Name'")
    ))
  }
  
  # Mandatory column sanity check
  if (any(!is.na(sheet_listvariables$Mandatory))) {
    invalid_mandatory <- sheet_listvariables$Mandatory[
      !is.na(sheet_listvariables$Mandatory) &
        !grepl("mandatory|optional", sheet_listvariables$Mandatory, ignore.case = TRUE)
    ]
    
    if (length(invalid_mandatory) > 0) {
      report <- append(report, list(
        data.frame(
          Issue = paste0(
            "Invalid Mandatory values: ",
            paste(unique(invalid_mandatory), collapse = ", ")
          )
        )
      ))
    }
  }
  
  # ---------------------------
  # CHECK CELL CONSTRAINT FORMAT
  # ---------------------------
  if (any(!is.na(sheet_listvariables$`cell constraints`))) {
    
    bad_constraints <- sheet_listvariables$`cell constraints`[
      !is.na(sheet_listvariables$`cell constraints`) &
        !grepl("varchar|int|decimal|numeric|text", sheet_listvariables$`cell constraints`)
    ]
    
    if (length(bad_constraints) > 0) {
      report <- append(report, list(
        data.frame(
          Issue = paste0(
            "Unknown cell constraints: ",
            paste(unique(bad_constraints), collapse = ", ")
          )
        )
      ))
    }
  }
  
  # ---------------------------
  # RETURN
  # ---------------------------
  if (length(report) == 0) {
    return(data.frame(Issue = "OK"))
  }
  
  dplyr::bind_rows(report)
}