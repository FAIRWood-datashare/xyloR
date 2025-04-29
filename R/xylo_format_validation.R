#' Xylodata Format Validation
#'
#' @param xylo_file Path to the metadata Excel file.
#' @return A tibble containing validation issues for each sheet and column in the xylo data.
#' @export
#' 
#' @examples
#' \dontrun{
#' xylo_file <- system.file("extdata", "Ltal.2007_xylo_data_2025-03-06_test.xlsx", package = "xyloR")
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
  
  # Load sheets excluding instructions and lookup tables
  sheet_names <- setdiff(readxl::excel_sheets(xylo_file), c("instructions", "DropList", "ListOfVariables"))
  sheet_data <- setNames(lapply(sheet_names, function(sheet) readxl::read_excel(xylo_file, sheet = sheet)), sheet_names)
  sheet_listvariables <- readxl::read_excel(xylo_file, sheet = "ListOfVariables")
  sheet_droplist <- readxl::read_excel(xylo_file, sheet = "DropList")
  
  # Extract column constraints
  col_constraints <- sheet_listvariables %>% dplyr::select(Table, Name, `cell constraints`, Mandatory, Domain, `data origin`)
  
  # Initialize report lists
  report <- list()
  
  for (sheet in sheet_names) {
    ifelse(sheet == "Xylo_obs_data", 
          data <- sheet_data[[sheet]][-1:-6,],  
          data <- sheet_data[[sheet]][-1:-4,] %>% stats::setNames(c("site_label", "latitude", "longitude", "elevation") ) 
          ) 
    constraints <- col_constraints %>% dplyr::filter(Table == sheet)
    
    for (col in colnames(data)) {
      constraint_row <- constraints %>% dplyr::filter(Name == col)
      
      if (nrow(constraint_row) == 0) next  # Skip columns without constraints
      
      constraint_mandatory <- constraint_row$Mandatory
      constraint_length <- constraint_row$`cell constraints`
      constraint_domain <- constraint_row$Domain
      constraint_origin <- constraint_row$`data origin`
      
      # Check mandatory fields
      if (grepl("mandatory", constraint_mandatory, ignore.case = TRUE) && any(
        (!apply(data, 1, function(row) all(is.na(row)))) & 
        (is.na(data[[col]]) | data[[col]] == "")
      )) {
        report[[sheet]][[col]] <- "Contains missing values"
      }
      
      # Check varchar length
      if (grepl("varchar", constraint_length)) {
        max_length <- as.numeric(gsub("\\D", "", constraint_length))
        if (any(nchar(data[[col]]) > max_length, na.rm = TRUE)) {
          report[[sheet]][[col]] <- paste("Values exceed", max_length, "characters")
        }
      }
      
      # Check droplist values
      if (grepl("droplist", constraint_origin, ignore.case = TRUE)) {
        ifelse(col == "person_country_code", col <- "country_code", col <- col)
        valid_values <- sheet_droplist[[col]] %>% na.omit()
        invalid_values <- setdiff(na.omit(data[[col]]), valid_values)
        if (length(invalid_values) > 0) {
          report[[sheet]][[col]] <- paste("Invalid values:", paste(invalid_values, collapse = ", "))
        }
      }
      
      # Check regex pattern
      if (grepl("regex", constraint_domain, ignore.case = TRUE)) {
        regex_pattern <- sub("Regex: ", "", constraint_domain)
        invalid_values <- data[[col]][!grepl(regex_pattern, na.omit(data[[col]]), perl = TRUE)]
        if (length(invalid_values) > 0) {
          report[[sheet]][[col]] <- paste("Invalid format:", paste(invalid_values, collapse = ", "))
        }
      }
      
      # Check True/False values
      if (grepl("True/False", constraint_domain, ignore.case = TRUE)) {
        valid_values <- c("TRUE", "FALSE", "True", "False", "T", "F")
        invalid_values <- setdiff(na.omit(data[[col]]), valid_values)
        if (length(invalid_values) > 0) {
          report[[sheet]][[col]] <- "Contains invalid True/False values"
        }
      }
      
      # Check range values
      if (grepl("^Range:", constraint_domain, ignore.case = TRUE)) {
        range_values <- as.numeric(strsplit(gsub("Range: ", "", constraint_domain), " to ")[[1]])
        min_val <- range_values[1]
        max_val <- range_values[2]
        numeric_values <- as.numeric(na.omit(data[[col]]))
        invalid_values <- numeric_values[numeric_values < min_val | numeric_values > max_val]
        if (length(invalid_values) > 0) {
          report[[sheet]][[col]] <- paste("Values out of range (", min_val, "-", max_val, "):", paste(invalid_values, collapse = ", "))
        }
      }
    }
  }
  
  # Convert report list to tibble
  convert_report_to_tibble <- function(report_list) {
    purrr::map_dfr(names(report_list), function(sheet) {
      tibble::tibble(Sheet = sheet, Column = names(report_list[[sheet]]), Issue = unlist(report_list[[sheet]]))
    })
  }
  
  # additional ad-hoc checks 
  # 1. check site_label match 
  obs_info_labels <- sheet_data[["obs_data_info"]][-1:-4,] %>%
    stats::setNames(c("site_label", "latitude", "longitude", "elevation")) %>%
    dplyr::pull(site_label)
  
  xylo_obs_labels <- unique(na.omit(sheet_data[["Xylo_obs_data"]][-1:-6, "site_label"])) %>% dplyr::pull()
  identical(sort(obs_info_labels), sort(xylo_obs_labels))
  if (!identical(sort(obs_info_labels), sort(xylo_obs_labels))) {
    report[["obs_data_info"]][["site_label"]] <- "Site labels in obs_data_info do not match those in Xylo_obs_data"
  }
  
  # 2. check site_label in obs_data_info are unique
  if (any(duplicated(obs_info_labels))) {
    report[["obs_data_info"]][["site_label"]] <- "Site labels in obs_data_info are not unique"
  }
 
  # 3. check sample_label in Xylo_obs_data are unique
  xylo_obs_sample_labels <- na.omit(sheet_data[["Xylo_obs_data"]][-1:-6, "sample_label"]) %>% dplyr::pull()
  if (any(duplicated(xylo_obs_sample_labels))) {
    report[["Xylo_obs_data"]][["sample_label"]] <- "Sample labels in Xylo_obs_data are not unique"
  }
   
  return(convert_report_to_tibble(report))
}
