

#' @export
meta_format_validation_core <- function(sheet_data, sheet_listvariables, sheet_droplist) {
  
  col_constraints <- sheet_listvariables %>%
    dplyr::select(Table, Name, `cell constraints`, Mandatory, Domain, `data origin`)
  
  report <- list()
  
  for (sheet in names(sheet_data)) {
    
    data <- sheet_data[[sheet]][-1:-6, , drop = FALSE]
    
    constraints <- col_constraints %>%
      dplyr::filter(Table == sheet)
    
    for (col in colnames(data)) {
      
      constraint_row <- constraints %>%
        dplyr::filter(Name == col)
      
      if (nrow(constraint_row) == 0) next
      
      # --------------------------
      # SAFE NORMALISATION
      # --------------------------
      mandatory <- as.character(constraint_row$Mandatory)
      mandatory <- ifelse(is.na(mandatory), "", mandatory)
      
      cell_constraints <- as.character(constraint_row$`cell constraints`)
      cell_constraints <- ifelse(is.na(cell_constraints), "", cell_constraints)
      
      domain <- as.character(constraint_row$Domain)
      domain <- ifelse(is.na(domain), "", domain)
      
      data_origin <- as.character(constraint_row$`data origin`)
      data_origin <- ifelse(is.na(data_origin), "", data_origin)
      
      x <- data[[col]]
      
      # ---------------------------
      # MANDATORY CHECK
      # ---------------------------
      if (isTRUE(grepl("mandatory", mandatory, ignore.case = TRUE))) {
        if (any(is.na(x) | x == "", na.rm = TRUE)) {
          report[[sheet]][[col]] <- "Contains missing values"
        }
      }
      
      # ---------------------------
      # VARCHAR LENGTH
      # ---------------------------
      if (grepl("varchar", cell_constraints, ignore.case = TRUE)) {
        
        max_length <- as.numeric(gsub("\\D", "", cell_constraints))
        
        if (!is.na(max_length)) {
          too_long <- nchar(as.character(x)) > max_length
          
          if (any(too_long, na.rm = TRUE)) {
            report[[sheet]][[col]] <- paste("Values exceed", max_length, "characters")
          }
        }
      }
      
      # ---------------------------
      # DROPLIST CHECK
      # ---------------------------
      if (grepl("droplist", data_origin, ignore.case = TRUE)) {
        
        # safe column mapping
        lookup_col <- col
        if (col == "organization_country_code") lookup_col <- "country_code"
        if (col == "organization_country") lookup_col <- "country"
        
        if (!lookup_col %in% names(sheet_droplist)) next
        
        valid_values <- sheet_droplist[[lookup_col]] |> na.omit()
        
        invalid_values <- setdiff(na.omit(x), valid_values)
        
        if (length(invalid_values) > 0) {
          report[[sheet]][[col]] <- paste(
            "Invalid values:",
            paste(unique(invalid_values), collapse = ", ")
          )
        }
      }
    }
  }
  
  if (length(report) == 0) {
    return(data.frame(Sheet=character(), Column=character(), Issue=character()))
  }
  
  dplyr::bind_rows(
    purrr::imap_dfr(report, function(cols, sheet) {
      data.frame(
        Sheet = sheet,
        Column = names(cols),
        Issue = unlist(cols)
      )
    })
  )
}