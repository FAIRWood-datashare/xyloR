# -------------------------------------------------------------------
# Script Name: create_long_format_xylo_obs.R
# Description: Prepare the observation file from standardized xylo files
# Author: Patrick Fonti
# Created: 28.11.2004 | Last Modified: 07.12.2024 | Version: 0.1
# R Version: 4.4.0 | Required Packages: openxlsx, dplyr, tidyr, stringr, utils
# -------------------------------------------------------------------

#' Process Xylo Observation File
#'
#' This function processes an observation file from standardized xylo files,
#' reshaping it into a long format, extracting metadata, and saving the result.
#'
#' @param xylo_file A string. Path to the xylo Excel file.
#' @param dest_dir A string. Path to the output directory. Defaults to the temporary directory.
#'
#' @return A data frame containing the processed observation data in a long format.
#' Additionally, saves the processed data as a CSV file in the specified directory.
#'
#' @import openxlsx
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_split str_detect
#' @importFrom utils write.csv
#' @importFrom magrittr %>%
#' @importFrom rnaturalearth ne_countries
#' @export
#'
#' @examples
#' \dontrun{
#' xylo_file <- system.file("extdata", "example_Xylo_file.xlsx", package = "xyloR")
#' path_out <- tempdir()
#' process_xylo_data(xylo_file, dest_dir = path_out)
#' }

process_xylo_data <- function(xylo_file, dest_dir = tempdir()) {
  # Validate inputs
  if (!file.exists(xylo_file)) {
    stop("The specified xylo file does not exist.")
  }
  if (!dir.exists(dest_dir)) {
    dir.create(path_out, recursive = TRUE)
    cat("Output directory created:", dest_dir, "\n")
  } else {
    cat("Using existing output directory:", dest_dir, "\n")
  }

  # Load country polygons for ISO code determination
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Helper Functions
  get_iso_country <- function(lat, lon) {
    point <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
    country <- sf::st_join(point, countries, join = sf::st_within)
    return(country$iso_a2)
  }

  # Load the workbook and required sheets
  xylo_workbook <- openxlsx::loadWorkbook(xylo_file)

  # Read Header and Observation Data
  xylo_header <- openxlsx::readWorkbook(xylo_workbook, sheet = "Xylo_obs_data", rows = 1:3, colNames = FALSE)
  xylo_obs <- openxlsx::readWorkbook(xylo_workbook, sheet = "Xylo_obs_data", startRow = 5) %>%
    dplyr::tibble() %>%
    dplyr::mutate(
      Date = as.Date(Date, origin = "1899-12-30")
    ) %>%
    dplyr::filter(!is.na(Date))  # Filter valid dates

  # Read variable metadata
  tbl_variables <- openxlsx::readWorkbook(xylo_workbook, sheet = "ListOfVariables") %>%
    dplyr::tibble()

  # Transform observation data into long format
  xylo_obs_long <- xylo_obs %>%
    tidyr::pivot_longer(
      cols = matches(".*_(count|width)$"),  # Match columns ending with '_count' or '_width'
      names_to = "Measure_Radial",
      values_to = "Value"
    ) %>%
    dplyr::mutate(
      Measure = str_split(Measure_Radial, "(?<=\\D)(?=\\d)", simplify = TRUE)[, 1],  # Extract Measure
      Radial_file = as.integer(sub("\\D+", "", str_split(Measure_Radial, "(?<=\\D)(?=\\d)", simplify = TRUE)[, 2])),  # Extract Radial file number
      Measure = paste0(Measure, if_else(str_detect(Measure_Radial, "_count$"), "_count", "_width"))  # Adjust Measure
    ) %>%
    dplyr::select(-Measure_Radial) %>%  # Drop intermediate column
    tidyr::pivot_wider(
      names_from = Measure,
      values_from = Value
    )

  # Extract metadata for the output file name
  Country_code = get_iso_country(xylo_header[1, 6], xylo_header[2, 6])
  Site_label = paste(unique(unique(xylo_obs$Network)), unique(xylo_obs$Site_code), sep = ".")


  # Construct output filename
  output_filename <- paste0(
    dest_dir, "/GX_", Country_code, "_", Site_label, "_data_f.csv"
  )

  # Save the processed data to a CSV file
  utils::write.csv(xylo_obs_long, output_filename, row.names = FALSE)
  cat("Processed data saved to:", output_filename, "\n")

  # Return the processed data
  return(xylo_obs_long)
}
