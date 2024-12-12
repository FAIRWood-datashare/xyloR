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
#' @param path_out Path to the output directory.
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
#' @examplesIf file.exists(system.file("extdata", "example_Xylo_file.xlsx", package = "xyloR"))
#' xylo_file <- system.file("extdata", "example_Xylo_file.xlsx", package = "xyloR")
#' template_meta <- system.file("extdata", "XX_XX_XXX_meta.xltm", package = "xyloR")
#' path_out <- tempdir()  # Use a temporary directory for output
#' create_xylo_metadata(xylo_file, template_meta, path_out = path_out)
#'
#'
create_xylo_metadata <- function(xylo_file, template_meta, path_out = tempdir(), output_name = NULL) {
  if (missing(xylo_file) || !file.exists(xylo_file)) {
    stop("The argument 'xylo_file' is missing or the file does not exist.")
  }
  if (missing(template_meta) || !file.exists(template_meta)) {
    stop("The argument 'template_meta' is missing or the file does not exist.")
  }

  # Ensure the path_out directory is valid
  if (!dir.exists(path_out)) {
    dir.create(path_out, recursive = TRUE)
    cat("Directory created:", path_out, "\n")
  }

  # Load country polygons for ISO code determination
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # check for large files and download if not present
  file_path1 <- system.file("extdata", "chelsa_clim.tif", package = "xyloR")
  file_path2 <- system.file("extdata", "CHELSA_kg1_1981-2010_V.2.1.tif", package = "xyloR")
  if (file_path1 == "") {
    download_large_file(url = "https://www.dropbox.com/scl/fi/mk8ljmydllr0pdeq0jkiz/chelsa_clim.tif?rlkey=daczhluce541uepb7199zy3ng&st=yforpnms&dl=1", destfile = system.file("extdata", package = "xyloR"), filename = "chelsa_clim.tif")
  }
  if (file_path2 == "") {
    download_large_file(url = "https://www.dropbox.com/scl/fi/rnrkjot1ymtwgx71ey3if/CHELSA_kg1_1981-2010_V.2.1.tif?rlkey=744il1b6gb5hxvzzkuz2ft95q&st=bsw3fzas&dl=1", destfile = system.file("extdata", package = "xyloR"), filename = "CHELSA_kg1_1981-2010_V.2.1.tif")
  }

  # Preload climate and Köppen datasets
  climate_data <- raster::brick(system.file("extdata", "chelsa_clim.tif", package = "xyloR"))
  koppen_data <- raster::brick(system.file("extdata", "CHELSA_kg1_1981-2010_V.2.1.tif", package = "xyloR"))
  # Usage example in your package code:


  # Helper Functions
  get_iso_country <- function(lat, lon) {
    point <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
    country <- sf::st_join(point, countries, join = sf::st_within)
    return(country$iso_a2)
  }

  extract_climate_data <- function(long, lat) {
    raster::extract(climate_data, tibble::tibble(long, lat), method = "bilinear") %>%
      as.data.frame() %>%
      purrr::set_names(c('temp', 'prec'))
  }

  extract_Koppen <- function(long, lat) {
    raster::extract(koppen_data, tibble::tibble(long, lat)) %>% as.data.frame()
  }

  write_to_sheet <- function(wb, sheet, data) {
    openxlsx::writeData(wb, sheet = sheet, x = data, startCol = 1, startRow = 8, colNames = FALSE, rowNames = FALSE)
  }

  # Load Template and Observation Files
  template_workbook <- openxlsx::loadWorkbook(template_meta)
  xylo_workbook <- openxlsx::loadWorkbook(xylo_file)

  # Read Droplist and Variables
  tbl_droplist <- openxlsx::readWorkbook(template_workbook, sheet = "DropList") %>% dplyr::tibble()
  tbl_variables <- openxlsx::readWorkbook(template_workbook, sheet = "ListOfVariables") %>% dplyr::tibble()

  # Read Header and Observation Data
  xylo_header <- openxlsx::readWorkbook(xylo_workbook, sheet = "Xylo_obs_data", rows = 1:3, colNames = FALSE)
  xylo_obs <- openxlsx::readWorkbook(xylo_workbook, sheet = "Xylo_obs_data", startRow = 5) %>%
    dplyr::tibble() %>%
    dplyr::mutate(Date = as.Date(Date, origin = "1899-12-30")) %>%
    dplyr::filter(!is.na(Date))

  # Prepare Person Tab
  person_role <- if_else(xylo_header[3, 2] == xylo_header[3, 4], "Contact and Data owner", "Data owner")
  metadata_person <- tibble::tibble(
    Person_role = person_role,
    Last_name = xylo_header[2, 2],
    First_name = xylo_header[1, 2],
    Email = xylo_header[3, 2],
    Orcid = NA,
    Organization_name = NA,
    Research_organization_registry = tbl_droplist$Research_organization_registry[which(tbl_droplist$Research_organization_registry == Organization_name)],
    Organization_name_helper = NA,
  )

  # Prepare Site Tab
  metadata_site <- xylo_obs %>%
    dplyr::group_by(Site) %>%
    dplyr::summarize(
      Network_name = unique(Network),
      Country_code = get_iso_country(xylo_header[1, 6], xylo_header[2, 6]),
      Site_code = paste(unique(Network), unique(Site), sep = "."),
      Site_label = unique(Site),
      Latitude = xylo_header[1, 6],
      Longitude = xylo_header[2, 6],
      Elevation = xylo_header[3, 6],
      Koppen_climate_class = extract_Koppen(xylo_header[2, 6], xylo_header[1, 6])[1, 1],
      Koppen_climate_code = tbl_droplist$Koppen.Climate.Code[which(tbl_droplist$Koppen.Climate.Class == Koppen_climate_class)],
      Koppen_climate_classification = tbl_droplist$Koppen.Climate.Classifications[which(tbl_droplist$Koppen.Climate.Class == Koppen_climate_class)],
      Aspect = NA,
      Slope = NA,
      Site_microtopography = NA,
      Temp = extract_climate_data(xylo_header[2, 6], xylo_header[1, 6])[1, 1] / 10,
      Precip = extract_climate_data(xylo_header[2, 6], xylo_header[1, 6])[1, 2],
      Soil_depth = NA,
      Soil_water_holding_capacity = NA,
      Forest_stand_origin = NA,
      Forest_stand_type = NA,
      Forest_stand_structure = NA,
      Forest_stand_main_species_composition = NA,
      Forest_stand_management_intensity = NA,
      In_stand_dendrometer_data = NA,
      In_stand_sapflux_data = NA,
      In_stand_phenological_observation = NA,
      In_stand_weather_data = NA,
      In_stand_soil_data = NA,
      In_stand_other_data = NA,
      Number.of.sampled.trees = dplyr::n_distinct(Tree),
      Comment = NA
    ) %>% dplyr::select(-Site)

  # Prepare Tree Tab
  metadata_tree <- xylo_obs %>%
    dplyr::count(Site, Tree, name = "Number.of.samples") %>%
    dplyr::transmute(
      Tree_label = paste(Site, Tree, sep = "_"),
      Tree_species = NA,
      ITRDB_Species_code = NA,
      Wood_type = NA,
      Leaf_habit = NA,
      Wood_plane = NA,
      Tree_manip = NA,
      Tree_DBH = NA,
      Tree_height = NA,
      Tree_age = NA,
      Tree_sex = NA,
      Tree_social_status = NA,
      Tree_health_status = NA,
      Tree_origin = NA,
      Tree_latitude = NA,
      Tree_longitude = NA,
      Tree_coordinate_precision = NA,
      On_tree_dendrometer_data = NA,
      On_tree_sapflux_data = NA,
      On_tree_phenological_observation = NA,
      On_tree_weather_data = NA,
      On_tree_shoot_growth_data = NA,
      On_tree_other_data = NA,
      Tree_ring_width_data = NA,
      Tree_ring_anatomical_data = NA,
      Tree_ring_other_data = NA,
      Number.of.samples,
      Comment = NA
    )

  # Prepare Sample Tab
  metadata_sample <- xylo_obs %>%
    dplyr::group_by(Network, Site,  Tree,  Sample_id, Date) %>%
    dplyr::summarise(Number.of.samples = dplyr::n(), .groups = "drop") %>%
    dplyr::transmute(Tree_label = Tree,
                     Sample_label = paste(paste(Network, Site, sep ="."), Tree, Sample_id, Date, sep='_'),
                     Sample_original_name = NA,
                     Sampled_organ = NA,
                     Sample_preparation_method = NA,
                     Sample_staining_method = NA,
                     Sample_mounting_method = NA,
                     Sample_observation_method = NA,
                     Sample_image_file_name = NA,
                     Sample_section_archived = NA,
                     Sample__archived = NA,
                     Sampling_height = NA,
                     Sampling_apex_distance = NA,
                     Sample_thickness = NA,
                     In_sample_IADF = NA,
                     Near_sample_anatomical_data = NA,
                     Near_sample_other_data = NA,
                     Number.of.samples = Number.of.samples,
                     Comment = NA)


  # Write Metadata to Template Workbook
  write_to_sheet(template_workbook, "person", metadata_person)
  write_to_sheet(template_workbook, "site", metadata_site)
  write_to_sheet(template_workbook, "tree", metadata_tree)
  write_to_sheet(template_workbook, "sample", metadata_sample)

  # Save the Updated Workbook
  if (is.null(output_name)) {
    output_name <- paste0("GX_", metadata_site$Country_code, "_", metadata_site$Network_name, ".", metadata_site$Site_label, "_meta.xlsx")
  }
  output_filepath <- file.path(path_out, output_name)
  openxlsx::saveWorkbook(template_workbook, file = output_filepath, overwrite = TRUE)

  return(output_filepath)
}
