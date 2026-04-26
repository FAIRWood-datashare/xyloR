# server/column_configs.R

# Defines a reactive called `column_configs` in whatever environment you source it into.
#### validation configuration ####
# column_configs <- reactive({
get_column_configs <- function(WB, WB_meta, dobs) {
  droplist1 <- openxlsx::readWorkbook(WB, sheet = "DropList")
  
  # tbl2 Obs
  tree_species_droplist <- droplist1 %>% select(tree_species) %>% filter(!is.na(tree_species)) %>% pull()
  species_code_droplist <- droplist1 %>% select(species_code) %>% filter(!is.na(species_code)) %>% pull()
  
  base_configs <- list(
    tbl1 = list(
      #site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 5, regex_pattern = NULL, unique = TRUE, readOnly = TRUE),
      site_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(dobs$site_label)), unique = TRUE, readOnly = TRUE),
      latitude = list(type = 'numeric', required = TRUE, min_val = -90, max_val = 90),
      longitude = list(type = 'numeric', required = TRUE, min_val = -180, max_val = 180),
      elevation = list(type = 'numeric', required = TRUE, min_val = 0, max_val = NULL)
    ),
    
    tbl2 = list(
      sample_date = list(type = "date", required = TRUE),
      sample_id = list(type = "character", required = FALSE, min_length = 1, max_length = 64),
      tree_species = list(type = "character", required = TRUE, min_length = 1, max_length = 128, readOnly = TRUE),
      species_code = list(type = "dropdown", required = TRUE, options = species_code_droplist),
      tree_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
      plot_label = list(type = "character", min_length = 1, max_length = 64),
      site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 5),
      network_label = list(type = "character", min_length = 1, max_length = 64),
      sample_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64, unique = FALSE),
      measure_type = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
      measure_repetition = list(type = "character", required = TRUE, min_length = 1, max_length = 6),
      sample_comment = list(type = 'character', min_length = 1, max_length = NULL)
    )
  )
  
  # If WB_meta is loaded, add the rest
  
  if (!is.null(WB_meta)) {
    droplist2 <- tryCatch(
      openxlsx::readWorkbook(WB_meta, sheet = "DropList"),
      error = function(e) {
        message("⚠️ DropList sheet not found in meta file: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(droplist2)) return(configs)

    safe_pull <- function(df, col) {
      if (col %in% names(df)) df |> dplyr::select(dplyr::all_of(col)) |> dplyr::filter(!is.na(.data[[col]])) |> dplyr::pull()
      else character(0)
    }

    # tbl3 Site
    country_code_droplist                    <- safe_pull(droplist2, "country_code")
    koppen_climate_value_droplist            <- safe_pull(droplist2, "koppen_climate_value") |> as.character()
    koppen_climate_code_droplist             <- safe_pull(droplist2, "koppen_climate_code")
    koppen_climate_classification_droplist   <- safe_pull(droplist2, "koppen_climate_classification")
    site_topography_droplist                 <- safe_pull(droplist2, "site_topography")
    soil_depth_droplist                      <- safe_pull(droplist2, "soil_depth")
    soil_water_holding_capacity_droplist     <- safe_pull(droplist2, "soil_water_holding_capacity")
    soil_moisture_droplist                   <- safe_pull(droplist2, "soil_moisture")
    forest_stand_composition_droplist        <- safe_pull(droplist2, "forest_stand_composition")
    forest_stand_structure_droplist          <- safe_pull(droplist2, "forest_stand_structure")
    forest_stand_age_structure_droplist      <- safe_pull(droplist2, "forest_stand_age_structure")
    forest_stand_age_droplist                <- safe_pull(droplist2, "forest_stand_age")
    forest_stand_management_intensity        <- safe_pull(droplist2, "forest_stand_management_intensity")

    # tbl4 Tree
    phylogenetic_group_droplist     <- safe_pull(droplist2, "phylogenetic_group")
    leaf_habit_droplist             <- safe_pull(droplist2, "leaf_habit")
    tree_ring_structure_droplist    <- safe_pull(droplist2, "tree_ring_structure")
    tree_sex_droplist               <- safe_pull(droplist2, "tree_sex")
    tree_social_status_droplist     <- safe_pull(droplist2, "tree_social_status")
    tree_health_status_droplist     <- safe_pull(droplist2, "tree_health_status")
    tree_treatment_droplist         <- safe_pull(droplist2, "tree_treatment")
    tree_sampling_pattern_droplist  <- safe_pull(droplist2, "tree_sampling_pattern")
    tree_origin_droplist            <- safe_pull(droplist2, "tree_origin")
    
    # tbl5 Sample
    sample_organ_droplist              <- safe_pull(droplist2, "sample_organ")
    sample_type_droplist               <- safe_pull(droplist2, "sample_type")
    sample_embedding_droplist          <- safe_pull(droplist2, "sample_embedding")
    sample_staining_method_droplist    <- safe_pull(droplist2, "sample_staining_method")
    sample_mounting_method_droplist    <- safe_pull(droplist2, "sample_mounting_method")
    sample_observation_method_droplist <- safe_pull(droplist2, "sample_observation_method")
    sample_section_archived_droplist   <- toupper(safe_pull(droplist2, "sample_section_archived"))
    sample_archived_droplist           <- toupper(safe_pull(droplist2, "sample_archived"))
    sample_image_archived_droplist     <- toupper(safe_pull(droplist2, "sample_image_archived"))
    sample_image_annotated_droplist    <- toupper(safe_pull(droplist2, "sample_image_annotated"))
    coupled_anatomical_data_droplist   <- toupper(safe_pull(droplist2, "coupled_anatomical_data"))
    reaction_wood_droplist             <- toupper(safe_pull(droplist2, "reaction_wood"))

    # tbl6
    person_role_droplist      <- safe_pull(droplist2, "person_role")
    country_droplist          <- safe_pull(droplist2, "country")

    # tbl7
    publication_type_droplist <- safe_pull(droplist2, "publication_type")
    
    
    extended_configs <- list(
      # Site table
      tbl3 = list(
        # network_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$network_label)), unique = TRUE, readOnly = TRUE),
        network_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        suggested_network_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
        site_country_code = list(type = 'dropdown', options = country_code_droplist, readOnly = TRUE),
        # site_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$site_label)), unique = TRUE, readOnly = TRUE),
        site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        suggested_site_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
        plot_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), 
        suggested_plot_code = list(type = 'character', min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
        latitude = list(type = 'numeric', required = TRUE, min_val = -90, max_val = 90, readOnly = TRUE), # calculated
        longitude = list(type = 'numeric', required = TRUE, min_val = -180, max_val = 180, readOnly = TRUE), # calculated
        elevation = list(type = 'numeric', required = TRUE, min_val = 0, max_val = 10000, readOnly = TRUE), # integer
        koppen_climate_value = list(type = 'dropdown', required = TRUE, options = koppen_climate_value_droplist, readOnly = TRUE), # calculated
        koppen_climate_code = list(type = 'dropdown', required = TRUE, options = koppen_climate_code_droplist, readOnly = TRUE),
        koppen_climate_classification = list(type = 'dropdown', options = koppen_climate_classification_droplist, readOnly = TRUE),
        site_aspect = list(type = 'numeric', min_val = 0, max_val = 360, regex_pattern = NULL, unique = TRUE), # integer
        site_slope = list(type = 'numeric', min_val = 0, max_val = NULL, regex_pattern = NULL, unique = TRUE), # integer
        site_topography = list(type = 'dropdown', options = site_topography_droplist),
        temp = list(type = 'numeric', required = TRUE, min_val = -50, max_val = 50, readOnly = TRUE),
        precip = list(type = 'numeric', required = TRUE, min_val = 0, max_val = 5000, readOnly = TRUE),
        soil_depth = list(type = 'dropdown', options = soil_depth_droplist),
        soil_water_holding_capacity = list(type = 'dropdown', options = soil_water_holding_capacity_droplist),
        soil_moisture = list(type = 'dropdown', options = soil_moisture_droplist),
        forest_stand_composition = list(type = 'dropdown', options = forest_stand_composition_droplist),
        forest_stand_structure = list(type = 'dropdown', options = forest_stand_structure_droplist),
        forest_stand_age_structure = list(type = 'dropdown', options = forest_stand_age_structure_droplist),
        forest_stand_age = list(type = 'dropdown', options = forest_stand_age_droplist),
        forest_stand_main_species_composition = list(type = 'character', min_length = 1, max_length = 128, regex_pattern = NULL), # %in% ITRDB
        forest_stand_management_intensity = list(type = 'dropdown', options = forest_stand_management_intensity),
        in_stand_soil_description = list(type = 'checkbox'),
        in_stand_dendrometer_monitoring = list(type = 'checkbox'),
        in_stand_phloem_observation = list(type = 'checkbox'),
        in_stand_sapflux_monitoring = list(type = 'checkbox'),
        in_stand_primary_phenological_observation = list(type = 'checkbox'),
        in_stand_weather_monitoring = list(type = 'checkbox'),
        in_stand_soil_monitoring = list(type = 'checkbox'),
        number_of_trees = list(type = 'numeric', required = TRUE, min_val = 0, max_val = NULL), # calculated
        site_comment = list(type = 'character', min_length = 1, max_length = NULL)
      ),
      
      # Tree table
      tbl4 = list(
        # site_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$site_label)), unique = TRUE, readOnly = TRUE),
        site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        # tree_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$tree_label)), unique = TRUE, readOnly = TRUE),
        tree_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = FALSE, unique.comp = TRUE, readOnly = TRUE),
        suggested_tree_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 20, regex_pattern = NULL, unique = FALSE, unique.comp = TRUE, readOnly = TRUE), # calculated
        # plot_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$plot_label)), unique = TRUE, readOnly = TRUE), # calculated
        plot_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        suggested_plot_code = list(type = 'character', min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
        tree_species = list(type = 'dropdown', required = TRUE, options = tree_species_droplist, readOnly = TRUE), # drop
        species_code = list(type = "dropdown", required = TRUE, options = species_code_droplist, readOnly = TRUE),
        phylogenetic_group = list(type = 'dropdown', required = TRUE, options = phylogenetic_group_droplist,  readOnly = TRUE),
        leaf_habit = list(type = 'dropdown', required = TRUE, options = leaf_habit_droplist,  readOnly = TRUE),
        tree_ring_structure = list(type = 'dropdown', required = TRUE, options = tree_ring_structure_droplist,  readOnly = TRUE),
        tree_treatment = list(type = 'dropdown', required = TRUE, options = tree_treatment_droplist),
        tree_sampling_pattern = list(type = 'dropdown', required = TRUE, options = tree_sampling_pattern_droplist),
        tree_dbh = list(type = 'numeric', min_val = 0, max_val = 500),
        tree_height = list(type = 'numeric', min_val = 0, max_val = 100),
        tree_age = list(type = 'numeric', min_val = 0, max_val = 1000),
        tree_sex = list(type = 'dropdown', options = tree_sex_droplist),
        tree_social_status = list(type = 'dropdown', options = tree_social_status_droplist),
        tree_health_status = list(type = 'dropdown', options = tree_health_status_droplist),
        tree_origin = list(type = 'dropdown', options = tree_origin_droplist),
        tree_latitude = list(type = 'numeric', min_val = -90, max_val = 90),
        tree_longitude = list(type = 'numeric', min_val = -180, max_val = 180),
        on_tree_dendrometer_monitoring = list(type = 'checkbox'),
        on_tree_sapflux_monitoring = list(type = 'checkbox'),
        on_tree_primary_phenological_observation = list(type = 'checkbox'),
        on_tree_weather_monitoring = list(type = 'checkbox'),
        on_tree_shoot_growth_monitoring = list(type = 'checkbox'),
        tree_ring_width_data = list(type = 'checkbox'),
        tree_ring_density_data = list(type = 'checkbox'),
        tree_ring_anatomical_data = list(type = 'checkbox'),
        tree_ring_isotope_data = list(type = 'checkbox'),
        number_of_samples = list(type = 'numeric', required = TRUE, min_val = 0, max_val = 200), # calculated
        tree_comment = list(type = 'character', min_length = 1, max_length = NULL)
      ),
      
      # Sample table
      tbl5 = list(
        # tree_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$tree_label)), unique = TRUE, readOnly = TRUE), # calculated
        tree_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        # sample_id = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$sample_id)), unique = TRUE, readOnly = TRUE), # calculated
        sample_id = list(type = 'character', required = FALSE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        sample_date = list(type = "date", required = TRUE, readOnly = TRUE), # calculated
        #sample_label	= list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$sample_label)), unique = TRUE, readOnly = TRUE), # calculated
        sample_label	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
        suggested_sample_code	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, unique = TRUE, readOnly = TRUE), # calculated
        sample_organ	= list(type = 'dropdown', required = TRUE, options = sample_organ_droplist),
        sample_type = list(type = 'dropdown', required = TRUE, options = sample_type_droplist),
        sample_embedding	= list(type = 'dropdown', required = TRUE, options = sample_embedding_droplist),
        sample_staining_method	= list(type = 'dropdown', required = TRUE, options = sample_staining_method_droplist),
        sample_mounting_method	= list(type = 'dropdown', required = TRUE, options = sample_mounting_method_droplist),
        sample_observation_method	= list(type = 'dropdown', required = TRUE, options = sample_observation_method_droplist),
        sample_image_file_name	= list(type = 'character', min_length = 1, max_length = 128, regex_pattern = NULL),
        sample_section_archived	= list(type = 'dropdown', required = TRUE, options = sample_section_archived_droplist),
        sample_archived	= list(type = 'dropdown', required = TRUE, options = sample_archived_droplist),
        sample_image_archived	= list(type = 'dropdown', required = TRUE, options = sample_image_archived_droplist),
        sample_image_annotated = list(type = 'dropdown', required = TRUE, options = sample_image_annotated_droplist),
        sampling_height	= list(type = 'numeric', min_val = 0, max_val = 100),
        sample_apex_distance	= list(type = 'numeric', min_val = 0, max_val = 100),
        section_thickness	= list(type = 'numeric', min_val = 0, max_val = NULL),
        coupled_anatomical_data	= list(type = 'dropdown', required = TRUE, options = coupled_anatomical_data_droplist),
        reaction_wood = list(type = 'dropdown', options = reaction_wood_droplist),
        sample_comment	= list(type = 'character', min_length = 1, max_length = NULL)
      ),
      
      # Person table
      tbl6 = list(
        person_role = list(type = 'dropdown', required = TRUE, options = person_role_droplist),
        person_order = list(type = 'numeric', required = TRUE, min_val = 1, max_val = NULL),
        last_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        first_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        email = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"),
        orcid = list(type = 'character', required = FALSE, min_length = 1, max_length = 19),# , regex_pattern = "^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]{1}$"
        main_organization_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 128), # drop # calculated
        main_organization_registry = list(type = 'character', required = TRUE, min_length = 1, max_length = 64), # drop # calculated , regex_pattern = "^\\+?[0-9 ()-]{7,20}$"
        organization_name_finder = NULL, # empty
        department = list(type = 'character', min_length = 1, max_length = 64),
        street = list(type = 'character', min_length = 1, max_length = 64),
        postal_code = list(type = 'character', min_length = 1, max_length = 64),
        city = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        #country = list(type = 'dropdown', required = TRUE, options = country_droplist), # drop
        #person_country_code = list(type = 'dropdown', required = TRUE, options = country_code_droplist), # calculated
        # organization_country = list(type = 'character', required = TRUE, min_length = 1, max_length = 64), # drop
        # organization_country_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 64) # calculated
        organization_country = list(type = 'dropdown', required = TRUE, options = country_droplist), # drop
        organization_country_code = list(type = 'dropdown', required = TRUE, options = country_code_droplist) # calculated
        
      ),
      
      # Publication table
      tbl7 = list(
        first_author_last_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        title = list(type = 'character', required = TRUE, min_length = 1, max_length = NULL, regex_pattern = NULL),
        publication_year = list(type = 'numeric', required = TRUE, min_val = 1950, max_val = format(Sys.Date(), "%Y")),
        publication_type = list(type = 'dropdown', required = TRUE, options = publication_type_droplist),
        journal = list(type = 'character', required = FALSE, min_length = 1, max_length = 64),
        doi = list(type = 'character', required = FALSE, min_length = 1, max_length = 64, "^(10\\.\\d{4,9}/[-._;()/:A-Z0-9]+|10\\.1002/[^\\s]+)$", unique = TRUE)
      )
      
    )
    return(modifyList(base_configs, extended_configs))
  }
  
  # If WB_meta not available, return just base
  return(base_configs)
}
