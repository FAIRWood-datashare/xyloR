## Master

# Download large files if not already present
download_large_file(url = "https://www.dropbox.com/scl/fi/rnrkjot1ymtwgx71ey3if/CHELSA_kg1_1981-2010_V.2.1.tif?rlkey=744il1b6gb5hxvzzkuz2ft95q&st=bsw3fzas&dl=1", destfile = system.file("extdata", package = "xyloR"), filename = "CHELSA_kg1_1981-2010_V.2.1.tif")
download_large_file(url = "https://www.dropbox.com/scl/fi/mk8ljmydllr0pdeq0jkiz/chelsa_clim.tif?rlkey=daczhluce541uepb7199zy3ng&st=yforpnms&dl=1", destfile = system.file("extdata", package = "xyloR"), filename = "chelsa_clim.tif")


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
