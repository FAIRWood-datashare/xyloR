renv::restore()

# Increase upload limit to 50 MB (example)
options(shiny.maxRequestSize = 50*1024^2)

devtools::load_all(".")
xyloR()

