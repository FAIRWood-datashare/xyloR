% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_tab4.R
\name{mod_tab4_server}
\alias{mod_tab4_server}
\title{Server logic for Site Metadata Tab}
\usage{
mod_tab4_server(id, out_tab1, out_tab2, out_tab3)
}
\arguments{
\item{id}{The module ID, which is used to create unique namespaces for the inputs
and outputs within the module.}

\item{out_tab1}{A reactive object containing the dataset name and observation file.}

\item{out_tab2}{A reactive object containing the metadata file and validation results.}

\item{out_tab3}{A reactive object containing the workbook reference and column configurations.}
}
\value{
A server-side function for managing the site metadata table, synchronizing
Koppen climate data, and saving the updated table.
}
\description{
This module handles the server-side logic for the "Site" tab. It manages the reactive
data, updates the site metadata table (\code{rHandsontable}), and synchronizes the Koppen
climate data. It also provides functionality for saving the updated data to a workbook.
}
