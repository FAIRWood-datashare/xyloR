#' Download large file
#'
#' This function downloads the large file from an external URL and stores it
#' in the appropriate location within the package's extdata directory.
#'
#' @param url The URL to the large file.
#' @param destfile The path where the file should be saved.
#' @param filename The name of the file to be saved.
#' @export
#'
#' @importFrom utils download.file
#'
#' @examplesIf file.exists(system.file("extdata", "chelsa_clim.tif", package = "xyloR"))
#' url <- "https://www.dropbox.com/scl/fi/rnrkjot1ymtwgx71ey3if/CHELSA_kg1_1981-2010_V.2.1.tif?rlkey=744il1b6gb5hxvzzkuz2ft95q&st=bsw3fzas&dl=1"
#' download_large_file(url, destfile = system.file("extdata", package = "xyloR"), filename = "CHELSA_kg1_1981-2010_V.2.1.tif")
#'
#'
download_large_file <- function(url, destfile, filename) {
  if (missing(url) || missing(destfile)) {
    stop("The arguments 'url' and 'destfile' are required.")
  }

  file_exist = system.file("extdata", filename, package = "xyloR")
  file_path <- file.path(file_exist)
  if (file_path == "") {
    message("Downloading ", file.path(destfile, filename), "...")
    utils::download.file(url, file.path(destfile, filename), mode = "wb")
    message(filename, " downloaded successfully!")
  } else {
    message(filename, " already exists in ", destfile)
  }
}

