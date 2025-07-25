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
meta_format_validation <- function(meta_file) {
  req(meta_file)
  # Load sheets excluding instructions and lookup tables
  sheet_names <- setdiff(readxl::excel_sheets(meta_file), c("instructions", "DropList", "ListOfVariables"))
  sheet_data <- stats::setNames(lapply(sheet_names, function(sheet) readxl::read_excel(meta_file, sheet = sheet)), sheet_names)
  sheet_listvariables <- readxl::read_excel(meta_file, sheet = "ListOfVariables")
  sheet_droplist <- readxl::read_excel(meta_file, sheet = "DropList")
  
  # Extract column constraints
  col_constraints <- sheet_listvariables %>% dplyr::select(Table, Name, `cell constraints`, Mandatory, Domain, `data origin`)
  
  # Initialize report lists
  report <- list()
  
  for (sheet in sheet_names) {
    data <- sheet_data[[sheet]][-1:-6,]  # Extract data from row 8 onward
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
        ifelse(col == "organization_country_code", col <- "country_code", col <- col)
        ifelse(col == "organization_country", col <- "country", col <- col)
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
      tibble(Sheet = sheet, Column = names(report_list[[sheet]]), Issue = unlist(report_list[[sheet]]))
    })
  }

  # 1. check plot_label in tree are unique
  meta_tree_plot_label <- unique(na.omit(sheet_data[["tree"]][-1:-6, "plot_label"])) %>% pull()
  if (any(duplicated(meta_tree_plot_label))) {
    report[["tree"]][["plot_label"]] <- "Plot label in tree are not unique"
  }
  
  # 2. check tree_label grouped by site and plot in tree are unique
  meta_tree_tree_label_in_plot <- na.omit(sheet_data[["tree"]][-1:-6, c("site_label", "plot_label", "tree_label")])
  is_unique <- meta_tree_tree_label_in_plot %>%
    dplyr::count(site_label, plot_label, tree_label) %>%
    dplyr::summarise(all_unique = all(n == 1)) %>%
    dplyr::pull(all_unique)
  if (!is_unique) {
    report[["tree"]][["plot_label"]] <- "Tree_label by plot_label and site_label in tree are not unique"
  }
  
  # 3. check doi in publication are unique
  meta_publication_doi <- na.omit(sheet_data[["publication"]][-1:-6, "doi"]) %>% pull()
  if (any(duplicated(meta_publication_doi))) {
    report[["publication"]][["doi"]] <- "doi in publication are not unique"
  }  
  return(convert_report_to_tibble(report))
}


