#' Get Average Climate Data for Coordinates
#'
#' This function extracts the average annual temperature and precipitation
#' from WorldClim for a given latitude and longitude using the geodata package.
#'
#' @param lat Numeric. Latitude of the location.
#' @param lon Numeric. Longitude of the location.
#' @param data_dir Character. Directory where the downloaded tiles will be stored.
#' @return A list containing the average temperature (°C) and precipitation (mm).
#'
#' @importFrom geodata worldclim_tile
#' @importFrom terra extract vect
#'
#' @export
#'
#' @examples
#' lat <- 45.1   # Latitude of the location
#' lon <- -93.6  # Longitude of the location
#' data_dir <- tempdir() # Directory where tiles will be stored
#' climate_data <- get_climate_data(lat, lon, data_dir)
#' print(climate_data)
#'
#'
get_climate_data <- function(lat, lon, data_dir) {
  # Validate input parameters
  if (!is.numeric(lat) || !is.numeric(lon)) {
    stop("Latitude and longitude must be numeric.")
  }
  if (!dir.exists(data_dir)) {
    stop("The specified data directory does not exist.")
  }

     # Download average temperature (Tavg) and precipitation (Prec) data
  temp_tile <- geodata::worldclim_tile(var = "tavg", lon, lat, res = 0.5, path = data_dir)  # Average temperature data (5-minute resolution)
  prec_tile <- geodata::worldclim_tile(var = "prec", lon, lat, res = 0.5, path = data_dir)  # Precipitation data (5-minute resolution)

  # Create a terra vector for the coordinates
  coords <- terra::vect(matrix(c(lon, lat), ncol = 2), crs = "EPSG:4326")

  # Extract temperature and precipitation values at the coordinates
  temp_val <- terra::extract(temp_tile, coords)[, -1]  # Remove ID column
  prec_val <- terra::extract(prec_tile, coords)[, -1]  # Remove ID column

  # Return the results as a list
  return(list(
    avg_temp = mean(as.numeric(temp_val), na.rm = TRUE),
    avg_prec = mean(as.numeric(prec_val), na.rm = TRUE)
  ))
}

# Example usage:
# lat <- 45.1
# lon <- -93.6
# data_dir <- "path_to_directory" # Specify the local path to store the downloaded tiles
# climate_data <- get_climate_data(lat, lon, data_dir)
# print(paste("Average Temperature (Tavg):", climate_data$avg_temp, "°C"))
# print(paste("Precipitation (Prec):", climate_data$avg_prec, "mm"))
