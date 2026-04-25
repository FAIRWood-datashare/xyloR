renv::restore()

# Increase upload limit to 50 MB (example)
options(shiny.maxRequestSize = 50*1024^2)
devtools::document()
devtools::load_all(".")
xyloR()

# fs::dir_tree(".", recurse = TRUE)



