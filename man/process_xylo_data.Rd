% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_xylo_data.R
\name{process_xylo_data}
\alias{process_xylo_data}
\title{Process Xylo Observation File}
\usage{
process_xylo_data(xylo_file, dest_dir = tempdir())
}
\arguments{
\item{xylo_file}{A string. Path to the xylo Excel file.}

\item{dest_dir}{A string. Path to the output directory. Defaults to the temporary directory.}
}
\value{
A data frame containing the processed observation data in a long format.
Additionally, saves the processed data as a CSV file in the specified directory.
}
\description{
This function processes an observation file from standardized xylo files,
reshaping it into a long format, extracting metadata, and saving the result.
}
\examples{
\dontrun{
xylo_file <- system.file("extdata", "example_Xylo_file.xlsx", package = "xyloR")
path_out <- tempdir()
process_xylo_data(xylo_file, dest_dir = path_out)
}
}
