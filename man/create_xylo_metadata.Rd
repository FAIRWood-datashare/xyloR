% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_xylo_metadata.R
\name{create_xylo_metadata}
\alias{create_xylo_metadata}
\title{Create Metadata from Xylo Files}
\usage{
create_xylo_metadata(
  xylo_file,
  template_meta,
  destdir = tempdir(),
  output_name = NULL
)
}
\arguments{
\item{xylo_file}{Path to the data file with xylogenesys observations (CSV or XLSX).}

\item{template_meta}{Path to the template for metadata (XLSM).}

\item{destdir}{Path to the output directory.}

\item{output_name}{(Optional) Output filename. Defaults to a name generated from metadata.}
}
\value{
Saves the updated workbook to the specified output path.
}
\description{
This function prepares metadata from standardized xylo files.
}
\examples{
\dontrun{
xylo_file <- system.file("extdata", "Ltal.2007_xylo_data_2025-03-06.xlsx", package = "xyloR")
template_meta <- system.file("extdata", "Datasetname_xylo_meta_yyyy-mm-dd.xlsx", package = "xyloR")
destdir <- "~/Desktop/"  # tempdir()  # Use a temporary directory for output
create_xylo_metadata(xylo_file, template_meta, destdir = destdir, output_name = "test.xlsx")
}
}
