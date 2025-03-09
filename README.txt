## Package description

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

# 1. Get the standardized template for recording the Xylo_obs_data 
# The template for the Xylo_file.xlsx can be found in the package's extdata folder.
template_obs <- system.file("extdata", "Xylo_file.xltm", package = "xyloR") # get the path of the template
template_your_Xylo_file.xlsx <- openxlsx::loadWorkbook(template_obs) # load the template
openxlsx::saveWorkbook(template_your_Xylo_file.xlsx, "~/Desktop/My_Xylo_file.xlsx", overwrite = TRUE) # save where you want

# 2. User fills the Xylo_file.xlsx with his observation data and save it
# ideally, the file name reflects this GloboXylo file naming structure "GX_XX_YYY_data.xlsx" where 
  # GX is the GloboXylo file code, 
  # XX is the 2-letter country code, 
  # and YYY is the site code


# 3. Generate the prefilled Metadata from the filled Xylo_file.xlsx
# Example run on the example Xylo_file.xlsx
xylo_file <- system.file("extdata", "example_Xylo_file.xlsx", package = "xyloR") # get the path of a filled example Xylo_file.xlsx
template_meta <- system.file("extdata", "XX_XX_XXX_meta.xltm", package = "xyloR") # get the path of the metadata template
path_out <- "~/Desktop"  # Use a temporary directory for saving the prefilled Metadata
create_xylo_metadata(xylo_file, template_meta, destdir = path_out) # create the prefilled metadata and save it with the GloboXylo file naming structure, i.e. "GX_XX_YYY_meta.xlsx". It takes long because it downloads large files climate files

# 4. User fills the Xylo_metadata corresponding to his observation data and save it
# ideally, the file name reflects this GloboXylo file naming structure "GX_XX_YYY_meta.xlsx" where 
# GX is the GloboXylo file code, 
# XX is the 2-letter country code, 
# and YYY is the site code

# 5. Format adjustments and quality check (to be further developped) (for DB curaator and contributors)

# 5.1 Reformat Xylo Observation File and save it with the GloboXylo file naming structure, i.e. "GX_XX_YYY_data_f.xlsx"
xylo_file <- system.file("extdata", "example_Xylo_file.xlsx", package = "xyloR")
path_out <- "~/Desktop"
process_xylo_data(xylo_file, dest_dir = path_out)

# 5.2 ... (to be further developped) (for DB curaator and contributors)
```

## Additional Notes

- Make sure to follow the instruction sheets provided for filling out both the observation and metadata files.
- Save all final outputs with the appropriate codes for upload into GloboXylo.

---

This vignette provides a guide to using `xyloR`, ensuring users can efficiently manage and format their data.
