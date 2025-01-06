## Master


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
