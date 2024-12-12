## Master



# Change the example_Xylo_file.xlsx with your_Xylo_file.xlsx to run this script with your data
# The template for the Xylo_file.xlsx can be found in the package
template_obs <- system.file("extdata", "Xylo_file.xltm", package = "xyloR")
template_your_Xylo_file.xlsx <- openxlsx::loadWorkbook(template_obs)
openxlsx::saveWorkbook(template_your_Xylo_file.xlsx, "~/Desktop/your_Xylo_file.xlsx", overwrite = TRUE)


# Create Metadata from Xylo Files
xylo_file <- system.file("extdata", "example_Xylo_file.xlsx", package = "xyloR")
template_meta <- system.file("extdata", "XX_XX_XXX_meta.xltm", package = "xyloR")
path_out <- "~/Desktop"  # Use a temporary directory for output
create_xylo_metadata(xylo_file, template_meta, path_out = path_out)


# Process Xylo Observation File
xylo_file <- system.file("extdata", "example_Xylo_file.xlsx", package = "xyloR")
path_out <- "~/Desktop"
process_xylo_data(xylo_file, path_out)
