#' Generate Exchange Files for GloboXylo DB
#'
#' This function processes metadata and observation data to create six exchange files
#' for the GloboXylo database, including sample, tree, and study zone data.
#'
#' @param obs_file Path to the observations Excel file.
#' @param meta_file Path to the metadata Excel file.
#' @param output_dir Directory where the output files should be saved.
#' @return Six Excel files saved in the specified directory.
#' @import dplyr readxl writexl
#' @export
#' 
#' @examples
#' \dontrun{
#' obs_filepath <- system.file("extdata", "Ltal.2007_xylo_data_2025-03-06.xlsx", package = "xyloR")
#' meta_filepath <- system.file("extdata", "Ltal.2007_xylo_meta_2025-03-08.xlsx", package = "xyloR")
#' output_dir <- tempdir()  # Use a temporary directory for output
#' }
#' 
create_exchange_files <- function(obs_file, meta_file, output_dir) {
  
  # Load observation data
  obs_sheet_names <- setdiff(readxl::excel_sheets(obs_file), c("Instructions", "DropList", "ListOfVariables"))
  obs_sheet_data <- setNames(lapply(obs_sheet_names, function(sheet) readxl::read_excel(obs_file, sheet = sheet)), obs_sheet_names)
  obs_sheet_data[[1]] <- obs_sheet_data[[1]][-1:-6, ]  # Remove first 6 rows for the first sheet
  colnames(obs_sheet_data[[2]]) <- as.character(obs_sheet_data[[2]][3, ])
  obs_sheet_data[[2]] <- obs_sheet_data[[2]][-1:-4, ]  # Remove first 3 rows for the second sheet
  
  # Load metadata
  meta_sheet_names <- setdiff(readxl::excel_sheets(meta_file), c("instructions", "DropList", "ListOfVariables"))
  meta_sheet_data <- setNames(lapply(meta_sheet_names, function(sheet) readxl::read_excel(meta_file, sheet = sheet)[-1:-6,]), meta_sheet_names)
  # ensure there is no row with just NA
  meta_sheet_data <- lapply(meta_sheet_data, function(df) df[apply(df, 1, function(x) !all(is.na(x))), ])
  
  # Process metadata
  dfmeta_joined <- dplyr::left_join(meta_sheet_data[["sample"]], meta_sheet_data[["tree"]], by = "tree_label", relationship = "many-to-many") %>%
    dplyr::left_join(meta_sheet_data[["site"]], by = "site_label", relationship = "many-to-many") %>%
    dplyr::group_by(network_label, network_code, site_label, site_code, plot_label, plot_code, tree_label, tree_code, year = as.numeric(format(sample_date, "%Y")), sample_label, sample_code, sample_id, sample_date) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(site_label = paste0(network_label, "__", site_label),
                  plot_label = paste0(site_label, "__", plot_label),
                  tree_label = paste0(plot_label, "__", tree_label),
                  year_label = paste0(tree_label, "__", year),
                  sample_label = paste0(year_label, "__", sample_id)) # Ensures uniqueness
  
  # Generate sample_table
  sample_data <- dplyr::left_join(meta_sheet_data[["sample"]], meta_sheet_data[["tree"]], by = "tree_label", relationship = "many-to-many") %>%
    dplyr::left_join(meta_sheet_data[["site"]], by = "site_label", relationship = "many-to-many") %>%
    dplyr::group_by(zone_hierarchy = paste(network_code, site_code, plot_code, sep = "."), tree_code, sample_code, sampling_date = as.Date(sample_date)) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::select(-n) 
  
  writexl::write_xlsx(sample_data, paste("~/Desktop/sample_table_", "xxx_", Sys.Date(), ".xlsx", sep = ""), col_names = TRUE)
  
  # Generate measure_sample
  measure_sample <- sample_data %>%
    dplyr::left_join(., meta_sheet_data[["sample"]], by = "sample_code", relationship = "many-to-many") %>%
    dplyr::left_join(., obs_sheet_data[[1]] %>% dplyr::select(-tree_label, -sample_id, -sample_date, -sample_comment), by = "sample_label", relationship = "many-to-many")
  
  writexl::write_xlsx(measure_sample, file.path(output_dir, paste0("measure_sample_", Sys.Date(), ".xlsx")))
  
  # Generate tree_table
  tree_data <- dplyr::left_join(meta_sheet_data[["sample"]], meta_sheet_data[["tree"]], by = "tree_label", relationship = "many-to-many") %>%
    dplyr::left_join(meta_sheet_data[["site"]], by = "site_label", relationship = "many-to-many") %>%
    dplyr::group_by(zone_hierarchy = paste(network_code, site_code, plot_code, sep = "."), species_code = itrdb_species_code, tree_code) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::select(-n) 
  
  writexl::write_xlsx(tree_data, file.path(output_dir, paste0("tree_table_", Sys.Date(), ".xlsx")))
  
  # Generate measure_tree
  measure_tree <- tree_data %>%
    dplyr::left_join(., meta_sheet_data[["tree"]], by = "tree_code", relationship = "many-to-many") %>%
    dplyr::select(zone_hierarchy, tree_code, plot_code, tree_species, itrdb_species_code, wood_type, leaf_habit, tree_ring_structure, tree_treatment, tree_dbh, tree_height, tree_age, tree_sex, tree_social_status, tree_health_status, tree_origin, tree_latitude, tree_longitude, on_tree_dendrometer_data, on_tree_sapflux_data, on_tree_phenological_observation, on_tree_weather_data, on_tree_shoot_growth_data, tree_ring_width_data, tree_ring_anatomical_data, tree_ring_isotope_data, number_of_samples, tree_comment)  
  
  writexl::write_xlsx(measure_tree, file.path(output_dir, paste0("measure_tree_", Sys.Date(), ".xlsx")))
  
  # Generate study_zone
  study_zone <- dplyr::left_join(meta_sheet_data[["sample"]], meta_sheet_data[["tree"]], by = "tree_label", relationship = "many-to-many") %>%
    dplyr::left_join(meta_sheet_data[["site"]], by = "site_label", relationship = "many-to-many") %>%
    dplyr::group_by(zone_hierarchy = paste(network_code, site_code, plot_code, sep = "."), network_label, network_code, site_label, site_code, plot_code) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::select(-n) 
  
  table_zone <- tibble::tibble(
    zone_code = c(unique(study_zone$network_code), unique(study_zone$site_code), unique(study_zone$plot_code)),
    zone_type = dplyr::case_when(
      zone_code %in% study_zone$network_code ~ "network",
      zone_code %in% study_zone$site_code ~ "site",
      zone_code %in% unique(study_zone$plot_code) ~ "plot"
    )
  ) %>%
    dplyr::mutate(
      zone_hierarchy = dplyr::case_when(
        zone_type == "network" ~ zone_code,  # network_code directly
        zone_type == "site" ~ paste(study_zone$network_code[match(zone_code, study_zone$site_code)], zone_code, sep = "."),  # network_code + site_code
        zone_type == "plot" ~ paste(
          study_zone$network_code[match(zone_code, study_zone$plot_code)],
          study_zone$site_code[match(zone_code, study_zone$plot_code)],
          zone_code,
          sep = "."
        )  # network_code + site_code + plot_code
      )
    ) %>%
    dplyr::mutate(
      zone_name = dplyr::case_when(
        zone_type == "network" ~ unique(study_zone$network_label),  # network_code directly
        zone_type == "site" ~ paste(study_zone$network_label[match(zone_code, study_zone$site_code)], zone_code, sep = "."),  # network_code + site_code
        zone_type == "plot" ~ paste(
          study_zone$network_label[match(zone_code, study_zone$plot_code)],
          study_zone$site_label[match(zone_code, study_zone$plot_code)],
          zone_code,
          sep = "."
        )  # network_code + site_code + plot_code
      )
    ) %>% 
    dplyr::select(zone_hierarchy, zone_code, zone_name, zone_type)
  
  # create data for measure_zone
  measure_zone_sitelevel <- table_zone %>%
    dplyr::filter(zone_type == "site") %>%
    dplyr::left_join(., meta_sheet_data[["site"]], by = c("zone_code" = "site_code"))
  
  measure_zone_networklevel <- table_zone %>%
    dplyr::filter(zone_type == "network") %>%
    dplyr::mutate(`principal investigator (pi)` = meta_sheet_data[["person"]]$last_name[grepl("Data owner", meta_sheet_data[["person"]]$person_role, ignore.case = TRUE)],
                  `email (email)` = meta_sheet_data[["person"]]$email[grepl("Data owner", meta_sheet_data[["person"]]$person_role, ignore.case = TRUE)])
  
  # Write the results to output files
  writexl::write_xlsx(list(measure_zone_networklevel = measure_zone_networklevel, measure_zone_sitelevel = measure_zone_sitelevel),
                      file.path(output_dir, paste0("measure_zone_", Sys.Date(), ".xlsx")))
  
  # Generate the study_zone file
  writexl::write_xlsx(study_zone, file.path(output_dir, paste0("study_zone_", Sys.Date(), ".xlsx")))
  
  # Success message
  return("Exchange files have been created successfully.")
}
