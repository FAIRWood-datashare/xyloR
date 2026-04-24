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
#' xylo_file <- system.file("extdata", "Datasetname_xylo_data_yyyy-mm-dd.xlsx", package = "xyloR")
#' template_meta <- system.file("extdata", "Datasetname_xylo_meta_yyyy-mm-dd.xlsx", package = "xyloR")
#' destdir <- "~/Desktop/"  # tempdir()  # Use a temporary directory for output
#' create_xylo_metadata(xylo_file, template_meta, destdir = destdir, output_name = "test.xlsx")
#' }



create_xylo_metadata <- function(obs_data, obs_info, destdir = NULL, output_name = NULL) {
  
  # =====================================================
  # LOAD DEPENDENCIES
  # =====================================================
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  koppen_data <- raster::brick(
    system.file("extdata", "CHELSA_kg1_1981-2010_V.2.1.tif", package = "xyloR")
  )
  
  extract_Koppen <- function(long, lat) {
    raster::extract(koppen_data, tibble::tibble(long, lat)) %>% as.data.frame()
  }
  
  write_to_sheet <- function(wb, sheet, data, startCol = 1, startRow = 8) {
    openxlsx::writeData(
      wb, sheet = sheet, x = data,
      startCol = startCol, startRow = startRow,
      colNames = FALSE, rowNames = FALSE
    )
    
    date_cols <- which(sapply(data, inherits, "Date"))
    
    if (length(date_cols) > 0) {
      date_style <- openxlsx::createStyle(numFmt = "yyyy-mm-dd")
      
      for (col in date_cols) {
        openxlsx::addStyle(
          wb, sheet = sheet, style = date_style,
          cols = startCol + col - 1,
          rows = startRow:(startRow + nrow(data) - 1),
          gridExpand = TRUE, stack = TRUE
        )
      }
    }
  }
  
  write_to_sheet_person <- function(wb, sheet, data) {
    openxlsx::writeData(
      wb, sheet = sheet, x = data,
      startCol = 1, startRow = 8,
      colNames = FALSE, rowNames = FALSE
    )
  }
  
  # =====================================================
  # LOAD FILES
  # =====================================================
  template_workbook <- openxlsx::loadWorkbook(template_meta)
  xylo_obs <- obs_data
  
  # =====================================================
  # DATE HANDLING
  # =====================================================
  if (all(!is.na(as.numeric(xylo_obs$sample_date)))) {
    xylo_obs <- xylo_obs %>%
      dplyr::mutate(sample_date = as.Date(as.numeric(sample_date), origin = "1899-12-30")) %>%
      dplyr::filter(!is.na(sample_date))
  } else {
    xylo_obs <- xylo_obs %>%
      dplyr::mutate(sample_date = lubridate::parse_date_time(sample_date, orders = c("ymd", "dmy", "mdy"))) %>%
      dplyr::filter(!is.na(sample_date))
  }
  
  obs_data_info <- ctx$data$tbl1
  
  # =====================================================
  # PERSON TAB
  # =====================================================
  person_role <- if_else(
    xylo_header[3, 2] == xylo_header[3, 4],
    "Contact and Principal Investigator",
    "Principal Investigator"
  )
  
  if (person_role == "Contact and Principal Investigator") {
    metadata_person <- tibble::tibble(
      person_role = person_role,
      person_order = NA,
      last_name = xylo_header[2, 2],
      first_name = xylo_header[1, 2],
      email = xylo_header[3, 2],
      orcid = NA,
      main_organization_name = NA,
      main_organization_registry = NA
    )
  } else {
    metadata_person <- tibble::tibble(
      person_role = c("Contact", person_role),
      person_order = NA,
      last_name = c(xylo_header[2, 4], xylo_header[2, 2]),
      first_name = c(xylo_header[1, 4], xylo_header[1, 2]),
      email = c(xylo_header[3, 4], xylo_header[3, 2]),
      orcid = c(NA, NA),
      main_organization_name = c(NA, NA),
      main_organization_registry = c(NA, NA)
    )
  }
  
  # =====================================================
  # SITE TAB (NO API ANYMORE 🚀)
  # =====================================================
  tree_counts <- xylo_obs %>%
    dplyr::group_by(network_label, site_label, plot_label) %>%
    dplyr::summarise(number_of_trees = n_distinct(tree_label), .groups = "drop")
  
  metadata_site <- xylo_obs %>%
    dplyr::group_by(network_label, site_label, plot_label) %>%
    dplyr::summarise(number_of_samples = n_distinct(sample_label), .groups = "drop") %>%
    dplyr::arrange(network_label, site_label) %>%
    dplyr::left_join(tree_counts, by = c("network_label", "site_label", "plot_label")) %>%
    dplyr::left_join(obs_data_info, by = "site_label") %>%
    dplyr::mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )
  
  # =====================================================
  # SPATIAL JOIN (ROBUST VERSION)
  # =====================================================
  
  metadata_site <- metadata_site %>%
    dplyr::mutate(.row_id = dplyr::row_number())
  
  points_sf <- sf::st_as_sf(
    metadata_site,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    dplyr::mutate(.row_id = metadata_site$.row_id)
  
  # IMPORTANT: explicitly keep only iso_a2
  countries_sub <- countries[, "iso_a2"]
  
  points_joined <- sf::st_join(
    points_sf,
    countries_sub,
    join = sf::st_intersects,
    left = TRUE
  )
  
  metadata_site <- points_joined %>%
    sf::st_drop_geometry() %>%
    dplyr::arrange(.row_id) %>%
    dplyr::mutate(
      site_country_code = iso_a2
    ) %>%
    dplyr::select(-iso_a2, -.row_id)
  
  # Optional debug
  if (any(is.na(metadata_site$site_country_code))) {
    message("⚠️ Some country codes could not be determined")
  }
  
  # Continue transformation
  metadata_site <- metadata_site %>%
    dplyr::transmute(
      network_label = coalesce(network_label, site_label),
      suggested_network_code = suppressWarnings(abbreviate(network_label, 5)),
      site_country_code,
      site_label,
      suggested_site_code = suppressWarnings(abbreviate(site_label, 5)),
      plot_label = coalesce(plot_label, site_label),
      suggested_plot_code = suppressWarnings(abbreviate(plot_label, 5)),
      latitude,
      longitude,
      elevation,
      koppen_climate_class = extract_Koppen(longitude, latitude)[1, 1],
      koppen_climate_code = tbl_droplist$koppen_climate_code[
        match(extract_Koppen(longitude, latitude)[1, 1], tbl_droplist$koppen_climate_value)
      ],
      koppen_climate_classification = tbl_droplist$koppen_climate_classification[
        match(extract_Koppen(longitude, latitude)[1, 1], tbl_droplist$koppen_climate_value)
      ],
      site_aspect = NA,
      site_slope = NA,
      site_topography = NA,
      number_of_trees,
      site_comment = NA
    )
  
  # =====================================================
  # TREE TAB
  # =====================================================
  metadata_tree <- xylo_obs %>%
    dplyr::group_by(site_label, plot_label, tree_label, tree_species) %>%
    dplyr::summarise(number_of_samples = n_distinct(sample_label), .groups = "drop") %>%
    dplyr::arrange(site_label, plot_label, tree_label, tree_species) %>%
    dplyr::mutate(
      species_code = tbl_droplist$species_code[match(tree_species, tbl_droplist$tree_species)],
      phylogenetic_group = tbl_droplist$phylogenetic_group[match(tree_species, tbl_droplist$tree_species)],
      leaf_habit = tbl_droplist$leaf_habit[match(tree_species, tbl_droplist$tree_species)],
      tree_ring_structure = tbl_droplist$tree_ring_structure[match(tree_species, tbl_droplist$tree_species)]
    ) %>%
    dplyr::transmute(
      site_label,
      tree_label,
      suggested_tree_code = suppressWarnings(abbreviate(tree_label, 5)),
      plot_label = coalesce(plot_label, site_label),
      suggested_plot_code = suppressWarnings(abbreviate(plot_label, 5)),
      tree_species,
      species_code,
      phylogenetic_group,
      leaf_habit,
      tree_ring_structure,
      number_of_samples,
      tree_comment = NA
    )
  
  # =====================================================
  # SAMPLE TAB
  # =====================================================
  metadata_sample <- xylo_obs %>%
    dplyr::group_by(network_label, site_label, tree_label, sample_id, sample_label, sample_date) %>%
    dplyr::summarise(number_of_samples = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(network_label, site_label, tree_label, sample_id, sample_date) %>%
    dplyr::transmute(
      tree_label,
      sample_id,
      sample_date = as.Date(sample_date, origin = "1899-12-30"),
      sample_label,
      suggested_sample_code = suppressWarnings(abbreviate(sample_label, 5)),
      number_of_samples,
      sample_comment = NA
    )
  
  # =====================================================
  # WRITE OUTPUT
  # =====================================================
  write_to_sheet_person(template_workbook, "person", metadata_person)
  write_to_sheet(template_workbook, "site", metadata_site)
  write_to_sheet(template_workbook, "tree", metadata_tree)
  write_to_sheet(template_workbook, "sample", metadata_sample)
  
  return(template_workbook)
}
