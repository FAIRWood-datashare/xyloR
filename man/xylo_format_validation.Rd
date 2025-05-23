% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xylo_format_validation.R
\name{xylo_format_validation}
\alias{xylo_format_validation}
\title{Xylodata Format Validation}
\usage{
xylo_format_validation(xylo_file)
}
\arguments{
\item{xylo_file}{Path to the metadata Excel file.}
}
\value{
A tibble containing validation issues for each sheet and column in the xylo data.
}
\description{
Xylodata Format Validation
}
\examples{
\dontrun{
xylo_file <- system.file("extdata", "Ltal.2007_xylo_data_2025-03-06_test.xlsx", package = "xyloR")
report <- xylo_format_validation(xylo_file)
}

}
