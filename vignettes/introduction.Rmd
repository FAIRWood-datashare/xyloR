---
title: "Introduction to xyloR"
author: "Patrick Fonti"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to xyloR}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

## Package descrption

The `xyloR` package helps researchers prepare and format observation data and metadata for upload to the GloboXylo database. This vignette introduces its key functionalities and provides a step-by-step guide to using the package effectively.

## Installation

You can install the package locally if you have the source files:

```r
# Install xyloR from a local source
install.packages("path/to/xyloR_0.0.0.9000.tar.gz", repos = NULL, type = "source")

library(xyloR)
```

## Show vignette
```r
vignette("introduction", package = "xyloR")
```

## Key Functions

The package includes three primary functions to streamline your workflow:

1. `create_xylo_obsdata()`: Creates an empty Xylo observation data file using a predefined template.
2. `create_xylo_metadata()`: Generates a prefilled metadata file based on an observation data file.
3. `process_xylo_data()`: Formats observation data for uploading to GloboXylo.

## Workflow Overview

The typical workflow for using `xyloR` involves the following steps:

1. **Create an Empty Xylo Observation File**: Generate a standardized template for recording observation data.
2. **Fill the Observation File**: Populate the template with your data (manual step).
3. **Generate Prefilled Metadata**: Use your completed observation file to create a prefilled metadata file.
4. **Fill the Metadata File**: Add necessary details to the metadata file (manual step).
5. **Format Data for Upload**: Reformat both the observation and metadata files for GloboXylo compatibility.

Steps 2 and 4 require manual input, while Steps 1, 3, and 5 are automated using the package.

## Example Workflow

Below is an example of how to use the `xyloR` package:

```r
# Load the xyloR package
library(xyloR)

# Step 1: Create an empty Xylo observation data file
create_xylo_obsdata(
  filename = "empty_Xylo_file.xlsx",
  destdir = tempdir()
)

# Step 2: Fill the empty Xylo file manually based on the instructions provided

# Step 3: Create a prefilled metadata file from a completed Xylo observation data file
xylo_file <- system.file("extdata", "example_Xylo_file.xlsx", package = "xyloR")
template_meta <- system.file("extdata", "XX_XX_XXX_meta.xltm", package = "xyloR")
destdir <- tempdir()  # Temporary directory for output

create_xylo_metadata(
  xylo_file = xylo_file,
  template_meta = template_meta,
  destdir = destdir
)

# Step 4: Fill the metadata file manually based on the instructions provided

# Step 5: Process the observation data file for upload
process_xylo_data(
  xylo_file = xylo_file,
  destdir = "~/Desktop" # Output directory for processed files
)
```

## Additional Notes

- Make sure to follow the instruction sheets provided for filling out both the observation and metadata files.
- Save all final outputs with the appropriate codes for upload into GloboXylo.

---

This vignette provides a guide to using `xyloR`, ensuring users can efficiently manage and format their data.
