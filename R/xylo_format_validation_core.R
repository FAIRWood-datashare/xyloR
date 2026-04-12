
#' @export
xylo_format_validation_core <- function(sheet_data, sheet_listvariables, sheet_droplist) {
  
  col_constraints <- sheet_listvariables %>%
    dplyr::select(Table, Name, `cell constraints`, Mandatory, Domain, `data origin`)
  
  report <- list()
  
  for (sheet in names(sheet_data)) {
    
    if (sheet == "Xylo_obs_data") {
      data <- sheet_data[[sheet]][-1:-6, ]
    } else {
      data <- sheet_data[[sheet]][-1:-4, ]
    }
    
    constraints <- col_constraints %>%
      dplyr::filter(Table == sheet)
    
    for (col in colnames(data)) {
      
      constraint_row <- constraints %>%
        dplyr::filter(Name == col)
      
      if (nrow(constraint_row) == 0) next
      
      # ---- SAME LOGIC AS BEFORE ----
      if (grepl("mandatory", constraint_row$Mandatory, ignore.case = TRUE)) {
        if (any(is.na(data[[col]]) | data[[col]] == "")) {
          report[[sheet]][[col]] <- "Contains missing values"
        }
      }
      
      if (grepl("varchar", constraint_row$`cell constraints`)) {
        max_length <- as.numeric(gsub("\\D", "", constraint_row$`cell constraints`))
        if (any(nchar(data[[col]]), na.rm = TRUE) > max_length) {
          report[[sheet]][[col]] <- paste("Values exceed", max_length)
        }
      }
      
      if (grepl("droplist", constraint_row$`data origin`, ignore.case = TRUE)) {
        
        valid_values <- sheet_droplist[[col]] %>% na.omit()
        invalid_values <- setdiff(na.omit(data[[col]]), valid_values)
        
        if (length(invalid_values) > 0) {
          report[[sheet]][[col]] <- paste("Invalid values:", paste(invalid_values, collapse = ", "))
        }
      }
    }
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