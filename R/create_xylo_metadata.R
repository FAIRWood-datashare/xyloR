# -------------------------------------------------------------------
# Script Name: create_xylo_metadata.R
# Description: Prepare metadata file from standardized xylo files
# Author: Patrick Fonti (Refactored by Assistant)
# Created: 27.11.2004 | Last Modified: 07.12.2024 | Version: 0.1
# R Version: 4.4.0 | Required Packages: openxlsx, dplyr, lubridate
# -------------------------------------------------------------------

#' Create Metadata from Xylo Files
#'
#' This function prepares metadata from standardized xylo files.
#'
#' @param xylo_file Path to the data file with xylogenesys observations (CSV or XLSX).
#' @param template_meta Path to the template for metadata (XLSM).
#' @param destdir Path to the output directory.
#' @param output_name (Optional) Output filename. Defaults to a name generated from metadata.
#'
#' @return Saves the updated workbook to the specified output path.
#' @import dplyr
#' @import purrr
#' @import sf
#' @import tibble
#' @importFrom raster extract brick
#' @importFrom rnaturalearth ne_countries
#' @importFrom openxlsx loadWorkbook readWorkbook writeData saveWorkbook
#' @importFrom utils download.file
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' xylo_file <- system.file("extdata", "Ltal.2007_xylo_data_2025-03-06.xlsx", package = "xyloR")
#' template_meta <- system.file("extdata", "Datasetname_xylo_meta_yyyy-mm-dd.xlsx", package = "xyloR")
#' destdir <- "~/Desktop/"  # tempdir()  # Use a temporary directory for output
#' create_xylo_metadata(xylo_file, template_meta, destdir = destdir, output_name = "test.xlsx")
#' }



create_xylo_metadata <- function(xylo_file, template_meta, destdir = tempdir(), output_name = NULL) {
  # if (missing(xylo_file) || !file.exists(xylo_file)) {
  #   stop("The argument 'xylo_file' is missing or the file does not exist.")
  # }
  # if (missing(template_meta) || !file.exists(template_meta)) {
  #  stop("The argument 'template_meta' is missing or the file does not exist.")
  # }
  # 
  # # Ensure the destdir directory is valid
  # if (!dir.exists(destdir)) {
  #   dir.create(destdir, recursive = TRUE)
  #   cat("Directory created:", destdir, "\n")
  # }

  # Load country polygons for ISO code determination
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Preload Köppen datasets
  # koppen_data <- raster::brick(system.file("extdata", "CHELSA_kg1_1981-2010_V.2.1.tif", package = "xyloR"))

  # Helper Functions
  get_iso_country <- function(lat, lon) {
    point <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
    country <- sf::st_join(point, countries, join = sf::st_within)
    return(country$iso_a2)
  }

  extract_Koppen <- function(long, lat) {
    raster::extract(koppen_data, tibble::tibble(long, lat)) %>% as.data.frame()
  }

  write_to_sheet <- function(wb, sheet, data) {
    openxlsx::writeData(wb, sheet = sheet, x = data, startCol = 1, startRow = 8, colNames = FALSE, rowNames = FALSE)
  }

  write_to_sheet_person <- function(wb, sheet, data) {
    openxlsx::writeData(wb, sheet = sheet, x = data, startCol = 1, startRow = 8, colNames = FALSE, rowNames = FALSE)
  }
  
  # Load Template and Observation Files
  template_workbook <- openxlsx::loadWorkbook(template_meta)
  xylo_workbook <- openxlsx::loadWorkbook(xylo_file)

  # Read Droplist and Variables
  tbl_droplist <- openxlsx::readWorkbook(template_workbook, sheet = "DropList") %>% dplyr::tibble()
  tbl_variables <- openxlsx::readWorkbook(template_workbook, sheet = "ListOfVariables") %>% dplyr::tibble()

  # Read Header and Observation Data
  xylo_header <- openxlsx::readWorkbook(xylo_workbook, sheet = "obs_data_info", rows = 1:3, colNames = FALSE)
  xylo_obs <- openxlsx::readWorkbook(xylo_workbook, sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>% 
    dplyr::tibble() 
  
    # Check if sample_date is numeric (Excel date format)
  if (!is.numeric(xylo_obs$sample_date)) {
    xylo_obs <- xylo_obs %>%
      dplyr::mutate(sample_date = as.Date(as.numeric(sample_date), origin = "1899-12-30")) %>% 
      dplyr::filter(!is.na(sample_date))
  } else {
    # If not numeric, assume it's in character format and parse using lubridate
    xylo_obs <- xylo_obs %>%
      dplyr::mutate(sample_date = lubridate::parse_date_time(sample_date, orders = c("ymd", "dmy", "mdy"))) %>% 
      dplyr::filter(!is.na(sample_date))
  }
  
  obs_data_info <- openxlsx::readWorkbook(xylo_workbook, sheet = "obs_data_info", startRow = 6, colNames = FALSE) %>% setNames(c("site_label", "latitude", "longitude", "elevation"))

  
  
  
  # Prepare Person Tab
  person_role <- if_else(xylo_header[3, 2] == xylo_header[3, 4], "Contact and Data owner", "Data owner")
  
  if(person_role == "Contact and Data owner") {
    metadata_person <- tibble::tibble(
    person_role = person_role,
    person_order = NA,
    last_name = xylo_header[2, 2],
    first_name = xylo_header[1, 2],
    email = xylo_header[3, 2],
    orcid = NA,
    organization_name = NA,
    research_organization_registry = NA,
    organization_name_helper = NA,
    ) }
  else {metadata_person <- tibble::tibble(
    person_role = c("Contact",person_role),
    last_name = c(xylo_header[2, 4],xylo_header[2, 2]),
    first_name = c(xylo_header[1, 4],xylo_header[1, 2]),
    email = c(xylo_header[3, 4],xylo_header[3, 2]),
    orcid = c(NA,NA),
    organization_name = c(NA,NA),
    research_organization_registry = c(NA,NA),
    organization_name_helper = c(NA,NA),
  )
  }

  # Prepare Site Tab
  metadata_site <- xylo_obs %>%
    dplyr::count(network_label, site_label, name = "number_of_samples") %>% 
    dplyr::arrange(network_label, site_label) %>%
    dplyr::left_join(., obs_data_info, by = c("site_label")) %>% 
    dplyr::transmute(
      network_label,
      network_code = suppressWarnings(abbreviate(network_label, 5)),
      country_code = get_iso_country(latitude, longitude),
      site_label,
      site_code = suppressWarnings(abbreviate(site_label, 5)), 
      latitude,
      longitude,
      elevation,
      koppen_climate_class = NA, #extract_Koppen(xylo_header[2, 6], xylo_header[1, 6])[1, 1],
      koppen_climate_code = NA, #tbl_droplist$Koppen.Climate.Code[which(tbl_droplist$Koppen.Climate.Class == Koppen_climate_class)],
      koppen_climate_classification = NA, #tbl_droplist$Koppen.Climate.Classifications[which(tbl_droplist$Koppen.Climate.Class == Koppen_climate_class)],
      site_aspect = NA,
      site_slope = NA,
      site_topography = NA,
      temp = NA, #get_climate_data(xylo_header[1, 6], xylo_header[2, 6], tempdir())[[1]],
      precip = NA, #get_climate_data(xylo_header[1, 6], xylo_header[2, 6], tempdir())[[2]],
      soil_depth = NA,
      soil_water_holding_capacity = NA,
      forest_stand_type = NA,
      forest_stand_structure = NA,
      forest_stand_age = NA,
      forest_stand_main_species_composition = NA,
      forest_stand_management_intensity = NA,
      in_stand_dendrometer_data = NA,
      in_stand_sapflux_data = NA,
      in_stand_phenological_observation = NA,
      in_stand_weather_data = NA,
      in_stand_soil_data = NA,
      in_stand_other_data = NA,
      number_of_trees = dplyr::n_distinct(xylo_obs$tree_label),
      site_comment = NA
    )

  # Prepare Tree Tab
  metadata_tree <- xylo_obs %>%
    dplyr::count(site_label, plot_label, tree_label, tree_species, name = "number_of_samples") %>%
    dplyr::arrange(site_label, plot_label, tree_label, tree_species) %>%
    dplyr::mutate(itrdb_species_code = tbl_droplist$itrdb_species_code[match(tree_species, tbl_droplist$tree_species)],
                  wood_type = tbl_droplist$tree_ring_structure[match(tree_species, tbl_droplist$tree_species)],
                  leaf_habit = tbl_droplist$leaf_habit[match(tree_species, tbl_droplist$tree_species)],
                  tree_ring_structure = tbl_droplist$tree_ring_structure[match(tree_species, tbl_droplist$tree_species)])  %>% 
    dplyr::transmute(
      site_label,
      tree_label,
      tree_code = suppressWarnings(abbreviate(tree_label, 5)),
      plot_label,
      plot_code = suppressWarnings(abbreviate(plot_label, 5)),
      tree_species,
      itrdb_species_code,
      wood_type,
      leaf_habit,
      tree_ring_structure,
      tree_treatment = NA,
      tree_dbh = NA,
      tree_height = NA,
      tree_age = NA,
      tree_sex = NA,
      tree_social_status = NA,
      tree_health_status = NA,
      tree_origin = NA,
      tree_latitude = NA,
      tree_longitude = NA,
      on_tree_dendrometer_data = NA,
      on_tree_sapflux_data = NA,
      on_tree_phenological_observation = NA,
      on_tree_weather_data = NA,
      on_tree_shoot_growth_data = NA,
      tree_ring_width_data = NA,
      tree_ring_anatomical_data = NA,
      tree_ring_isotopes_data = NA,
      number_of_samples,
      tree_comment = NA
    )

  # Prepare Sample Tab
  metadata_sample <- xylo_obs %>%
    dplyr::group_by(network_label, site_label, tree_label, sample_id, sample_label, sample_date) %>%
    dplyr::summarise(number_of_samples = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(network_label, site_label, tree_label, sample_id, sample_date) %>%
    dplyr::transmute(tree_label,
                     sample_id,
                     sample_date,
                     sample_label,
                     sample_code = suppressWarnings(abbreviate(sample_label, 5)),
                     sample_organ = NA,
                     sample_preparation_method = NA,
                     sample_staining_method = NA,
                     sample_mounting_method = NA,
                     sample_observation_method = NA,
                     sample_image_file_name = NA,
                     sample_section_archived = NA,
                     sample__archived = NA,
                     sampling_height = NA,
                     sampling_apex_distance = NA,
                     section_thickness = NA,
                     on_section_anatomical_data = NA,
                     sample_comment = NA)


  # Write Metadata to Template Workbook
  write_to_sheet_person(template_workbook, "person", metadata_person)
  write_to_sheet(template_workbook, "site", metadata_site)
  write_to_sheet(template_workbook, "tree", metadata_tree)
  write_to_sheet(template_workbook, "sample", metadata_sample)

  # Save the Updated Workbook
  # if (is.null(output_name)) {
  #   output_name <- paste0("GX_", metadata_site$Country_code, "_", metadata_site$Network_name, ".", metadata_site$Site, "_meta.xlsx")
  # }
  # output_filepath <- file.path(destdir, output_name)
  # openxlsx::saveWorkbook(template_workbook, file = output_filepath, overwrite = TRUE)

  # return(output_filepath)
  return(template_workbook)
}

