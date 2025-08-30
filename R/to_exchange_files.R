#' @title Transfer Observations and Metadata to GloboXylo DB
#'
#' @description
#' This function processes the observations and metadata data from Excel files and prepares them for insertion into the GloboXylo database. 
#' It loads observation and metadata data, processes the data frames, and generates various exchange files for different categories (e.g., sample, tree, zone).
#'
#' @param obs_file_path Character. The file path to the observations Excel file.
#' @param meta_file_path Character. The file path to the metadata Excel file.
#' @param dir Character. Directory where files should be saved. Defaults to tempdir().
#' @param dataset_name Character. Name of the dataset. Defaults to NULL.
#'
#' @return
#' Saves processed data frames as Excel files in the specified directory.
#' 
#' @examples
#' \dontrun{
#' obs_file_path <- system.file("extdata", "Ltal.2007_xylo_data_2025-06-22.xlsx", package = "xyloR")
#' meta_file_path <- system.file("extdata", "Ltal.2007_xylo_meta_2025-06-22.xlsx", package = "xyloR")
#' dir <- tempdir()
#' dataset_name <- "test"
#' to_exchange_files(obs_file_path, meta_file_path)
#' }
#'
#' @import dplyr
#' @import readxl
#' @import writexl
#' @importFrom readr write_csv
#' @export
to_exchange_files <- function(obs_file_path, meta_file_path, dir = tempdir(), dataset_name = "name_your_dataset") {

  # Ensure directory exists
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # Ensure dataset_name is not NULL
  if (is.null(dataset_name)) {
    dataset_name <- "name_your_dataset"
  }
  

# OBSERVATIONS DATA
# obs_file <- openxlsx::loadWorkbook(input$obs_file$datapath)  # Load the workbook
# obs_file <- system.file("extdata", "Ltal.2007_xylo_data_2025-03-06.xlsx", package = "xyloR")
obs_sheet_names <- setdiff(readxl::excel_sheets(obs_file_path), c("instructions", "DropList", "ListOfVariables"))
obs_sheet_data <- setNames(lapply(obs_sheet_names, function(sheet) readxl::read_excel(obs_file_path, sheet = sheet)), obs_sheet_names)
obs_sheet_data[[1]] <- obs_sheet_data[[1]][-1:-6, ]  # Remove first 6 rows for the first sheet
colnames(obs_sheet_data[[2]]) <- as.character(obs_sheet_data[[2]][3, ])
obs_sheet_data[[2]] <- obs_sheet_data[[2]][-1:-4, ]  # Remove first 3 rows for the second sheet
head(obs_sheet_data)


# METADATA DATA
# meta_file <- system.file("extdata", "Ltal.2007_xylo_meta_2025-03-08.xlsx", package = "xyloR")
meta_sheet_names <- setdiff(readxl::excel_sheets(meta_file_path), c("instructions", "DropList", "ListOfVariables"))
meta_sheet_data <- setNames(lapply(meta_sheet_names, function(sheet) readxl::read_excel(meta_file_path, sheet = sheet)[-1:-6,]), meta_sheet_names)
# ensure there is no row with just NA
meta_sheet_data <- lapply(meta_sheet_data, function(df) df[apply(df, 1, function(x) !all(is.na(x))), ])


# Group all samples per year, tree, plot, site, and network from sheet_data into a single data frame and count the number of samples per group
dfmeta_joined <- left_join(meta_sheet_data[["sample"]], meta_sheet_data[["tree"]] %>% select(-plot_label, -suggested_plot_code), by = "tree_label", relationship = "many-to-many") %>%
  left_join(meta_sheet_data[["site"]], by = "site_label", relationship = "many-to-many") %>%
  group_by(network_label, suggested_network_code, site_label, suggested_site_code, plot_label, suggested_plot_code, tree_label, suggested_tree_code, year = as.numeric(format(sample_date, "%Y")), sample_label, suggested_sample_code, sample_id, sample_date) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(site_label = paste0(network_label, "__", site_label),
         plot_label = paste0(site_label, "__", plot_label),
         tree_label = paste0(plot_label, "__", tree_label),
         year_label = paste0(tree_label, "__", year),
         sample_label = paste0(year_label, "__", sample_id)) # Ensures uniqueness
 
### SAMPLE_TABLE exchange files
 # create data for excel 
sample_data <- left_join(meta_sheet_data[["sample"]], meta_sheet_data[["tree"]] %>% select(-plot_label, -suggested_plot_code) %>% distinct(tree_label, .keep_all = TRUE), by = "tree_label", relationship = "many-to-many") %>%
  left_join(meta_sheet_data[["site"]] %>% distinct(site_label, .keep_all = TRUE), by = "site_label", relationship = "many-to-many") %>%
  group_by(zone_hierarchy = paste(suggested_network_code, suggested_site_code, suggested_plot_code, sep = "."), suggested_tree_code, suggested_sample_code, sampling_date = as.Date(sample_date)) %>%
  summarise(n = n(), .groups = "drop") %>% 
  select(-n) %>% 
  dplyr::mutate(comment = NA) %>% 
  dplyr::arrange(zone_hierarchy, suggested_tree_code, sampling_date) 
 # save file on desktop
# sample_data %>% 
#   writexl::write_xlsx(., file.path(dir, paste0(dataset_name, "_sample_table_", Sys.Date(), ".xlsx")), col_names = TRUE)
sample_data %>%
  readr::write_csv(file.path(dir, paste0(dataset_name, "_sample_table_", Sys.Date(), ".csv")))

# create data for measure_sample
measure_sample <- sample_data %>%
  left_join(., meta_sheet_data[["sample"]], by = "suggested_sample_code", relationship = "many-to-many") %>%
  left_join(., obs_sheet_data[[1]] %>% select(-tree_label, -sample_id, -sample_date, -sample_comment), by = "sample_label", relationship = "many-to-many") %>%
  dplyr::mutate(
    Observation_measure = NA,
    Date_measure = NA,
    Precision_date_measure = NA
  ) %>%
  dplyr::relocate(Observation_measure, Date_measure, Precision_date_measure, .after = sampling_date) %>% 
  dplyr::select(-tree_label, -sample_date)

# save file on desktop
# measure_sample %>% 
#   writexl::write_xlsx(., file.path(dir, paste0(dataset_name, "_measure_sample_", Sys.Date(), ".xlsx")), col_names = TRUE)  
measure_sample %>% 
  readr::write_csv(file.path(dir, paste0(dataset_name, "_measure_sample_", Sys.Date(), ".csv")))


### TREE_TABLE exchange files
# create data for excel 
tree_data <- left_join(meta_sheet_data[["sample"]], meta_sheet_data[["tree"]] %>% select(-plot_label, -suggested_plot_code) %>% distinct(tree_label, .keep_all = TRUE), by = "tree_label", relationship = "many-to-many") %>%
  left_join(meta_sheet_data[["site"]], by = "site_label", relationship = "many-to-many") %>% 
  group_by(zone_hierarchy = paste(suggested_network_code, suggested_site_code, suggested_plot_code, sep = "."), species_code = species_code, suggested_tree_code) %>%
  summarise(n = n(), .groups = "drop") %>% 
  select(-n) %>% 
  dplyr::mutate(comment = NA)
# save file on desktop
# tree_data %>% 
#   writexl::write_xlsx(., file.path(dir, paste0(dataset_name, "_tree_table_", Sys.Date(), ".xlsx")), col_names = TRUE)
tree_data %>% 
  readr::write_csv(file.path(dir, paste0(dataset_name, "_tree_table_", Sys.Date(), ".csv")))

# create data for measure_tree
measure_tree <- tree_data %>%
  left_join(., meta_sheet_data[["tree"]] %>% select(-species_code) %>% distinct(tree_label, .keep_all = TRUE), by = "suggested_tree_code", relationship = "many-to-many") %>%
  select(zone_hierarchy, suggested_tree_code, suggested_plot_code, tree_species, species_code, phylogenetic_group, leaf_habit, tree_ring_structure, tree_treatment, tree_sampling_pattern, tree_dbh, tree_height, tree_age, tree_sex, tree_social_status, tree_health_status, tree_origin, tree_latitude, tree_longitude, on_tree_dendrometer_monitoring, on_tree_sapflux_monitoring, on_tree_primary_phenological_observation, on_tree_weather_monitoring, on_tree_shoot_growth_monitoring, tree_ring_width_data, tree_ring_density_data, tree_ring_anatomical_data, tree_ring_isotope_data, number_of_samples, tree_comment) %>%
  dplyr::mutate(
    Observation_measure = NA,
    Date_measure = NA,
    Precision_date_measure = NA
  ) %>%
  dplyr::relocate(Observation_measure, Date_measure, Precision_date_measure, .after = suggested_tree_code) %>% 
  dplyr::select(-suggested_plot_code, -tree_species, -species_code)
  
 
# save file on desktop
# measure_tree %>% 
#   writexl::write_xlsx(., file.path(dir, paste0(dataset_name, "_measure_tree_", Sys.Date(), ".xlsx")), col_names = TRUE)  
measure_tree %>% 
  readr::write_csv(file.path(dir, paste0(dataset_name, "_measure_tree_", Sys.Date(), ".csv"))) 



### STUDY_ZONE exchange files
# create data for excel 
study_zone <- left_join(meta_sheet_data[["sample"]], meta_sheet_data[["tree"]] %>% select(-suggested_plot_code), by = "tree_label", relationship = "many-to-many") %>%
  left_join(meta_sheet_data[["site"]], by = "site_label", relationship = "many-to-many") %>% 
  group_by(zone_hierarchy = paste(suggested_network_code, suggested_site_code, suggested_plot_code, sep = "."), network_label, suggested_network_code, site_label, suggested_site_code, suggested_plot_code) %>%
  summarise(n = n(), .groups = "drop") %>% 
  select(-n) 


# Step 1: Compute hierarchy for each row
study_zone <- study_zone %>%
  mutate(
    zone_hierarchy_network = suggested_network_code,
    zone_hierarchy_site    = paste(suggested_network_code, suggested_site_code, sep = "."),
    zone_hierarchy_plot    = paste(suggested_network_code, suggested_site_code, suggested_plot_code, sep = ".")
  )

# Step 2: Create separate tables for network, site, and plot
table_network <- study_zone %>%
  # emove cases where network == site
  filter(suggested_network_code != suggested_site_code) %>%
  distinct(suggested_network_code, zone_hierarchy_network) %>%
  transmute(
    zone_code = suggested_network_code,
    zone_hierarchy = zone_hierarchy_network,
    zone_name = zone_hierarchy_network,
    zone_type = "network"
  )

table_site <- study_zone %>%
  distinct(suggested_site_code, zone_hierarchy_site) %>%
  transmute(zone_code = suggested_site_code,
            zone_hierarchy = zone_hierarchy_site,
            zone_name = zone_hierarchy_site,
            zone_type = "site")

table_plot <- study_zone %>%
  # remove cases where plot == site
  filter(suggested_plot_code != suggested_site_code) %>%
  distinct(suggested_plot_code, suggested_site_code, zone_hierarchy_plot) %>%
  transmute(
    zone_code = suggested_plot_code,
    zone_hierarchy = zone_hierarchy_plot,
    zone_name = zone_hierarchy_plot,
    zone_type = "plot"
  )


# Step 3: Combine into a single table
table_zone <- bind_rows(table_network, table_site, table_plot)

# save file on desktop
# table_zone %>% 
#   writexl::write_xlsx(., file.path(dir, paste0(dataset_name, "_study_zone_", Sys.Date(), ".xlsx")), col_names = TRUE)
table_zone %>% 
  readr::write_csv(file.path(dir, paste0(dataset_name, "_study_zone_", Sys.Date(), ".csv")))

# create data for measure_zone
measure_zone_sitelevel <- table_zone %>%
  filter(zone_type == "site") %>% left_join(., meta_sheet_data[["site"]], by = c("zone_code" = "suggested_site_code"))


measure_zone_networklevel <- table_zone %>%
  filter(zone_type == "network") %>%
  mutate(`principal investigator (pi)` = meta_sheet_data[["person"]]$last_name[grepl("Contact", meta_sheet_data[["person"]]$person_role, ignore.case = TRUE)],
    `email (email)` = meta_sheet_data[["person"]]$email[grepl("Contact", meta_sheet_data[["person"]]$person_role, ignore.case = TRUE)],
    `organization_name` = meta_sheet_data[["person"]]$main_organization_name[grepl("Contact", meta_sheet_data[["person"]]$person_role, ignore.case = TRUE)],
    `country name organization (country_name)` = meta_sheet_data[["person"]]$organization_country[grepl("Contact", meta_sheet_data[["person"]]$person_role, ignore.case = TRUE)],
    `country code organization (country_code)` = meta_sheet_data[["person"]]$organization_country_code[grepl("Contact", meta_sheet_data[["person"]]$person_role, ignore.case = TRUE)],
    `comment (zone_com)` = meta_sheet_data[["site"]]$site_comment[match(zone_code, meta_sheet_data[["site"]]$suggested_site_code)]
  )
measure_zone_networklevel


measure_zone <- full_join(measure_zone_networklevel, measure_zone_sitelevel, 
                                      by = c("zone_hierarchy", "zone_code", "zone_name", "zone_type")) %>% 
  dplyr::select(-zone_hierarchy) %>%
  dplyr::mutate(
    Observation_measure = NA,
    Date_measure = NA,
    Precision_date_measure = NA
  ) %>%
  dplyr::relocate(Observation_measure, Date_measure, Precision_date_measure, .after = zone_type)


# save file on desktop
# measure_zone %>% 
#   writexl::write_xlsx(., file.path(dir, paste0(dataset_name, "_measure_zone_", Sys.Date(), ".xlsx")), col_names = TRUE)
measure_zone %>% 
  readr::write_csv(file.path(dir, paste0(dataset_name, "_measure_zone_", Sys.Date(), ".csv")))

# # Print list of saved files
# saved_files <- list.files(dir, full.names = TRUE)
# print(saved_files)

return(paste("Files saved in", dir))

}

                      