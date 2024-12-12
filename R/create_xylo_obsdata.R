#' Create an Empty Xylo Observation Data File
#'
#' This function creates an empty Xylo observation data file using a predefined template
#' and saves it as an `.xlsx` file in the specified directory.
#'
#' @param filename Character. Name of the output file. Defaults to `"your_Xylo_file.xlsx"`.
#' @param destdir Character. Directory where the output file will be saved. Defaults to `tempdir()`.
#' @return None. The function saves the file to the specified directory.
#' @export
#'
#' @examples
#' # Create an empty Xylo file in the working directory
#' create_xylo_obsdata("my_Xylo_file.xlsx", destdir = getwd())
#'
#'
create_xylo_obsdata <- function(filename = "my_Xylo_file.xlsx", destdir = tempdir()) {
  # Check if the destination directory exists
  if (!dir.exists(destdir)) {
    stop("The specified destination directory does not exist.")
  }

  # Define the full output path
  output_path <- file.path(destdir, filename)

  # Locate the template file in the package's extdata folder
  template_obs <- system.file("extdata", "Xylo_file.xltm", package = "xyloR")

  if (template_obs == "") {
    stop("Template file 'Xylo_file.xltm' not found in the package.")
  }

  # Load the template workbook
  workbook <- openxlsx::loadWorkbook(template_obs)

  # Save the workbook as the specified filename
  openxlsx::saveWorkbook(workbook, output_path, overwrite = TRUE)

  message("Xylo observation data file created: ", output_path)
}
