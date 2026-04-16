

#' @export
rules <- list(
  
  sample = list(
    columns = list(
      
      sample_date = list(
        type = "date",
        required = TRUE
      ),
      
      sample_id = list(
        type = "character",
        required = TRUE,
        unique = FALSE
      ),
      
      tree_species = list(
        type = "character"
      ),
      
      species_code = list(
        type = "dropdown",
        domain = "species"
      ),
      
      tree_label = list(
        type = "character"
      ),
      
      plot_label = list(
        type = "character"
      ),
      
      site_label = list(
        type = "character"
      ),
      
      network_label = list(
        type = "character"
      )
      
    )
  ),
  
  site = list(
    columns = list(
      
      site_label = list(
        type = "character",
        required = TRUE
      ),
      
      latitude = list(
        type = "numeric",
        min_val = -90,
        max_val = 90
      ),
      
      longitude = list(
        type = "numeric",
        min_val = -180,
        max_val = 180
      ),
      
      elevation = list(
        type = "numeric",
        min_val = 0
      ),
      
      koppen_climate_value = list(
        type = "dropdown",
        domain = "koppen"
      ),
      
      koppen_climate_code = list(
        type = "character",
        read_only = TRUE
      ),
      
      koppen_climate_classification = list(
        type = "character",
        read_only = TRUE
      )
      
    )
  )
  
)