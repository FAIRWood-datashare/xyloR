

#' @export
validate_table_by_constraints <- function(data, constraints, droplist) {
  
  report <- list()
  
  # remove rows without valid column mapping
  constraints <- constraints %>%
    dplyr::filter(!is.na(Name))
  
  for (i in seq_len(nrow(constraints))) {
    
    col <- constraints$Name[i]
    
    if (!col %in% names(data)) next
    
    col_data <- data[[col]]
    
    constraint_mandatory <- constraints$Mandatory[i]
    constraint_length    <- constraints$`cell constraints`[i]
    constraint_domain    <- constraints$Domain[i]
    constraint_origin    <- constraints$`data origin`[i]
    
    # ----------------------------
    # 1. MANDATORY
    # ----------------------------
    if (grepl("mandatory", constraint_mandatory, ignore.case = TRUE)) {
      
      if (any(is.na(col_data) | col_data == "")) {
        report[[col]] <- "Contains missing values"
      }
    }
    
    # ----------------------------
    # 2. VARCHAR
    # ----------------------------
    if (grepl("varchar", constraint_length)) {
      
      max_length <- as.numeric(gsub("\\D", "", constraint_length))
      
      if (any(nchar(col_data) > max_length, na.rm = TRUE)) {
        report[[col]] <- paste("Values exceed", max_length, "characters")
      }
    }
    
    # ----------------------------
    # 3. DROPLIST
    # ----------------------------
    if (grepl("droplist", constraint_origin, ignore.case = TRUE)) {
      
      valid_values <- droplist[[col]] %>% na.omit()
      
      invalid_values <- setdiff(na.omit(col_data), valid_values)
      
      if (length(invalid_values) > 0) {
        report[[col]] <- paste(
          "Invalid values:",
          paste(invalid_values, collapse = ", ")
        )
      }
    }
    
    # ----------------------------
    # 4. REGEX
    # ----------------------------
    if (grepl("regex", constraint_domain, ignore.case = TRUE)) {
      
      regex_pattern <- sub("Regex: ", "", constraint_domain)
      
      invalid_values <- col_data[
        !grepl(regex_pattern, na.omit(col_data), perl = TRUE)
      ]
      
      if (length(invalid_values) > 0) {
        report[[col]] <- paste(
          "Invalid format:",
          paste(invalid_values, collapse = ", ")
        )
      }
    }
    
    # ----------------------------
    # 5. TRUE/FALSE
    # ----------------------------
    if (grepl("True/False", constraint_domain, ignore.case = TRUE)) {
      
      valid_values <- c("TRUE", "FALSE", "True", "False", "T", "F")
      
      invalid_values <- setdiff(na.omit(col_data), valid_values)
      
      if (length(invalid_values) > 0) {
        report[[col]] <- "Contains invalid True/False values"
      }
    }
    
    # ----------------------------
    # 6. RANGE
    # ----------------------------
    if (grepl("^Range:", constraint_domain, ignore.case = TRUE)) {
      
      range_values <- as.numeric(
        strsplit(gsub("Range: ", "", constraint_domain), " to ")[[1]]
      )
      
      min_val <- range_values[1]
      max_val <- range_values[2]
      
      values <- suppressWarnings(as.numeric(col_data))
      
      invalid_values <- values[
        !is.na(values) & (values < min_val | values > max_val)
      ]
      
      if (length(invalid_values) > 0) {
        report[[col]] <- paste(
          "Values out of range (", min_val, "-", max_val, "):",
          paste(invalid_values, collapse = ", ")
        )
      }
    }
  }
  
  tibble::tibble(
    Column = names(report),
    Issue  = unlist(report)
  )
}