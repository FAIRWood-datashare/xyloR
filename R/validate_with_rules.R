

#' @export
validate_with_rules <- function(sheet_data, rules, sheet_droplist) {
  
  report <- list()
  
  for (sheet in names(sheet_data)) {
    
    data <- sheet_data[[sheet]][-1:-6, , drop = FALSE]
    
    sheet_rules <- rules %>%
      dplyr::filter(table == sheet)
    
    for (col in colnames(data)) {
      
      rule <- sheet_rules %>%
        dplyr::filter(name == col)
      
      if (nrow(rule) == 0) next
      rule <- rule[1, ]
      
      x <- data[[col]]
      
      issues <- c()
      
      # -------------------------
      # 1. MANDATORY
      # -------------------------
      if (isTRUE(rule$mandatory)) {
        if (any(is.na(x) | x == "", na.rm = TRUE)) {
          issues <- c(issues, "contains missing values")
        }
      }
      
      # -------------------------
      # 2. TYPE CHECK
      # -------------------------
      if (rule$type == "numeric") {
        if (any(!is.na(x) & is.na(suppressWarnings(as.numeric(x))))) {
          issues <- c(issues, "non-numeric values present")
        }
      }
      
      # -------------------------
      # 3. LENGTH CHECK
      # -------------------------
      if (!is.na(rule$max_length)) {
        if (any(nchar(x) > rule$max_length, na.rm = TRUE)) {
          issues <- c(issues, paste0("exceeds ", rule$max_length, " characters"))
        }
      }
      
      # -------------------------
      # 4. DROPLIST CHECK
      # -------------------------
      if (!is.na(rule$source) && grepl("droplist", rule$source, ignore.case = TRUE)) {
        
        valid_values <- sheet_droplist[[col]]
        valid_values <- valid_values[!is.na(valid_values)]
        
        invalid <- setdiff(na.omit(x), valid_values)
        
        if (length(invalid) > 0) {
          issues <- c(
            issues,
            paste("invalid values:", paste(unique(invalid), collapse = ", "))
          )
        }
      }
      
      # -------------------------
      # 5. SAVE REPORT (SAFE STRUCTURE)
      # -------------------------
      if (length(issues) > 0) {
        
        report[[sheet]] <- append(
          report[[sheet]],
          list(setNames(list(paste(issues, collapse = "; ")), col))
        )
      }
    }
  }
  
  # -------------------------
  # SAFE FLATTENING (FIXED CRASH HERE)
  # -------------------------
  result <- purrr::imap_dfr(report, function(cols, sheet) {
    
    if (is.null(cols) || length(cols) == 0) {
      return(NULL)
    }
    
    issues <- unlist(cols, use.names = FALSE)
    col_names <- names(cols)
    
    if (length(issues) == 0 || length(col_names) == 0) {
      return(NULL)
    }
    
    # protect against mismatch
    if (length(col_names) == 1) {
      col_vec <- rep(col_names, length(issues))
    } else {
      col_vec <- rep(names(cols), lengths(cols))
    }
    
    data.frame(
      Sheet = rep(sheet, length(issues)),
      Column = col_vec,
      Issue  = issues,
      stringsAsFactors = FALSE
    )
  })
  
  if (nrow(result) == 0) {
    return(data.frame())
  }
  
  result
  
  # Ensure valid empty return
  if (is.null(result)) {
    return(data.frame(Sheet = character(), Column = character(), Issue = character()))
  }
  
  result
}