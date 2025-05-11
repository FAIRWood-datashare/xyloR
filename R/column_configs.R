# server/column_configs.R

# Defines a reactive called `column_configs` in whatever environment you source it into.
#### validation configuration ####
# column_configs <- reactive({
get_column_configs <- function(WB, WB_meta, dobs) {
  droplist1 <- openxlsx::readWorkbook(WB, sheet = "DropList")
  
  # tbl2 Obs
  tree_species_droplist <- droplist1 %>% select(tree_species) %>% filter(!is.na(tree_species)) %>% pull()
  itrdb_species_code_droplist <- droplist1 %>% select(ITRDB_species_code) %>% filter(!is.na(ITRDB_species_code)) %>% pull()
  
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
      sample_id = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
      tree_species = list(type = "character", required = TRUE, min_length = 1, max_length = 64, readOnly = TRUE),
      # tree_species = list(type = "dropdown", required = TRUE, options =c("Picea abies (L.) Karst.", "Larix decidua Mill.")),
      # tree_species = list(type = "dropdown", required = TRUE, options = tree_species_droplist), # drop
      itrdb_species_code = list(type = "dropdown", required = TRUE, options = itrdb_species_code_droplist),
      tree_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
      plot_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
      site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 5),
      network_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
      sample_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64, unique = TRUE),
      radial_file = list(type = "character", required = TRUE, min_length = 1, max_length = 6),
      sample_comment = list(type = 'character', min_length = 1, max_length = NULL)
    )
  )
  
  # If WB_meta is loaded, add the rest
  
  if (!is.null(WB_meta)) {
    droplist2 <- openxlsx::readWorkbook(WB_meta, sheet = "DropList")
    
    # tbl3 Site
    country_code_droplist <- droplist2 %>% select(country_code) %>% filter(!is.na(country_code)) %>% pull()
    koppen_climate_value_droplist <- droplist2 %>% select(koppen_climate_value) %>% filter(!is.na(koppen_climate_value)) %>% pull() %>% as.character()
    koppen_climate_code_droplist <- droplist2 %>% select(koppen_climate_code) %>% filter(!is.na(koppen_climate_code)) %>% pull()
    koppen_climate_classification_droplist <- droplist2 %>% select(koppen_climate_classification) %>% filter(!is.na(koppen_climate_classification)) %>% pull()
    site_topography_droplist <- droplist2 %>% select(site_topography) %>% filter(!is.na(site_topography)) %>% pull()
    soil_depth_droplist <- droplist2 %>% select(soil_depth) %>% filter(!is.na(soil_depth)) %>% pull()
    soil_water_holding_capacity_droplist <- droplist2 %>% select(soil_water_holding_capacity) %>% filter(!is.na(soil_water_holding_capacity)) %>% pull()
    forest_stand_type_droplist <- droplist2 %>% select(forest_stand_type) %>% filter(!is.na(forest_stand_type)) %>% pull()
    forest_stand_structure_droplist <- droplist2 %>% select(forest_stand_structure) %>% filter(!is.na(forest_stand_structure)) %>% pull()
    forest_stand_age_droplist <- droplist2 %>% select(forest_stand_age) %>% filter(!is.na(forest_stand_age)) %>% pull()
    forest_stand_management_intensity <- droplist2 %>% select(forest_stand_management_intensity) %>% filter(!is.na(forest_stand_management_intensity)) %>% pull()
    # tbl4 Tree
    # tree_species_droplist <- droplist2 %>% select(tree_species) %>% filter(!is.na(tree_species)) %>% pull()
    # itrddb_species_code_droplist <- droplist2 %>% select(itrddb_species_code) %>% filter(!is.na(itrddb_species_code)) %>% pull()
    wood_type_droplist <- droplist2 %>% select(wood_type) %>% filter(!is.na(wood_type)) %>% pull()
    leaf_habit_droplist <- droplist2 %>% select(leaf_habit) %>% filter(!is.na(leaf_habit)) %>% pull()
    tree_ring_structure_droplist <- droplist2 %>% select(tree_ring_structure) %>% filter(!is.na(tree_ring_structure)) %>% pull()
    tree_treatment_droplist <- droplist2 %>% select(tree_treatment) %>% filter(!is.na(tree_treatment)) %>% pull()
    tree_sex_droplist <- droplist2 %>% select(tree_sex) %>% filter(!is.na(tree_sex)) %>% pull()
    tree_social_status_droplist <- droplist2 %>% select(tree_social_status) %>% filter(!is.na(tree_social_status)) %>% pull()
    tree_health_status_droplist <- droplist2 %>% select(tree_health_status) %>% filter(!is.na(tree_health_status)) %>% pull()
    tree_origin_droplist <- droplist2 %>% select(tree_origin) %>% filter(!is.na(tree_origin)) %>% pull()
    # tbl5 Sample
    sample_organ_droplist <- droplist2 %>% select(sample_organ) %>% filter(!is.na(sample_organ)) %>% pull()
    sample_preparation_method_droplist <- droplist2 %>% select(sample_preparation_method) %>% filter(!is.na(sample_preparation_method)) %>% pull()
    sample_staining_method_droplist <- droplist2 %>% select(sample_staining_method) %>% filter(!is.na(sample_staining_method)) %>% pull()
    sample_mounting_method_droplist <- droplist2 %>% select(sample_mounting_method) %>% filter(!is.na(sample_mounting_method)) %>% pull()
    sample_observation_method_droplist <- droplist2 %>% select(sample_observation_method) %>% filter(!is.na(sample_observation_method)) %>% pull()
    # tbl6
    person_role_droplist <- droplist2 %>% select(person_role) %>% filter(!is.na(person_role)) %>% pull()
    #country_code_droplist <- droplist2 %>% select(country_code) %>% filter(!is.na(country_code)) %>% pull()
    country_droplist <- droplist2 %>% select(country) %>% filter(!is.na(country)) %>% pull()
    # organization_name_droplist <- droplist2 %>% select(organization_name) %>% filter(!is.na(organization_name)) %>% pull()
    
    
    extended_configs <- list(
      # Site table
      tbl3 = list(
        # network_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$network_label)), unique = TRUE, readOnly = TRUE),
        network_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        network_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
        country_code = list(type = 'dropdown', options = country_code_droplist, readOnly = TRUE),
        # site_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$site_label)), unique = TRUE, readOnly = TRUE),
        site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
        site_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
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
        forest_stand_type = list(type = 'dropdown', options = forest_stand_type_droplist),
        forest_stand_structure = list(type = 'dropdown', options = forest_stand_structure_droplist),
        forest_stand_age = list(type = 'dropdown', options = forest_stand_age_droplist),
        forest_stand_main_species_composition = list(type = 'character', min_length = 1, max_length = 128, regex_pattern = NULL), # %in% ITRDB
        forest_stand_management_intensity = list(type = 'dropdown', options = forest_stand_management_intensity),
        in_stand_dendrometer_data = list(type = 'checkbox'),
        in_stand_sapflux_data = list(type = 'checkbox'),
        in_stand_phenological_observation = list(type = 'checkbox'),
        in_stand_weather_data = list(type = 'checkbox'),
        in_stand_soil_data = list(type = 'checkbox'),
        in_stand_other_data = list(type = 'character', min_length = 1, max_length = NULL),
        site_comment = list(type = 'character', min_length = 1, max_length = NULL)
      ),
      
      # Tree table
      tbl4 = list(
        # site_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$site_label)), unique = TRUE, readOnly = TRUE),
        site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        # tree_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$tree_label)), unique = TRUE, readOnly = TRUE),
        tree_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = FALSE, unique.comp = TRUE, readOnly = TRUE),
        tree_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, unique = FALSE, unique.comp = TRUE, readOnly = TRUE), # calculated
        # plot_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$plot_label)), unique = TRUE, readOnly = TRUE), # calculated
        plot_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        plot_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
        tree_species = list(type = 'dropdown', required = TRUE, options = tree_species_droplist, readOnly = TRUE), # drop
        itrdb_species_code = list(type = "dropdown", required = TRUE, options = itrdb_species_code_droplist, readOnly = TRUE),
        wood_type = list(type = 'dropdown', required = TRUE, options = wood_type_droplist,  readOnly = TRUE),
        leaf_habit = list(type = 'dropdown', required = TRUE, options = leaf_habit_droplist,  readOnly = TRUE),
        tree_ring_structure = list(type = 'dropdown', required = TRUE, options = tree_ring_structure_droplist,  readOnly = TRUE),
        tree_treatment = list(type = 'dropdown', required = TRUE, options = tree_treatment_droplist),
        tree_dbh = list(type = 'numeric', min_val = 0, max_val = 500),
        tree_height = list(type = 'numeric', min_val = 0, max_val = 100),
        tree_age = list(type = 'numeric', min_val = 0, max_val = 1000),
        tree_sex = list(type = 'dropdown', options = tree_sex_droplist),
        tree_social_status = list(type = 'dropdown', options = tree_social_status_droplist),
        tree_health_status = list(type = 'dropdown', options = tree_health_status_droplist),
        tree_origin = list(type = 'dropdown', options = tree_origin_droplist),
        tree_latitude = list(type = 'numeric', min_val = -90, max_val = 90),
        tree_longitude = list(type = 'numeric', min_val = -180, max_val = 180),
        on_tree_dendrometer_data = list(type = 'checkbox'),
        on_tree_sapflux_data = list(type = 'checkbox'),
        on_tree_phenological_observation = list(type = 'checkbox'),
        on_tree_weather_data = list(type = 'checkbox'),
        on_tree_shoot_growth_data = list(type = 'checkbox'),
        tree_ring_width_data = list(type = 'checkbox'),
        tree_ring_anatomical_data = list(type = 'checkbox'),
        tree_ring_isotope_data = list(type = 'checkbox'),
        number_of_samples = list(type = 'numeric', required = TRUE, min_val = 0, max_val = NULL), # calculated
        tree_comment = list(type = 'character', min_length = 1, max_length = NULL)
      ),
      
      # Sample table
      tbl5 = list(
        # tree_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$tree_label)), unique = TRUE, readOnly = TRUE), # calculated
        tree_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        # sample_id = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$sample_id)), unique = TRUE, readOnly = TRUE), # calculated
        sample_id = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        sample_date = list(type = "date", required = TRUE, readOnly = TRUE), # calculated
        #sample_label	= list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$sample_label)), unique = TRUE, readOnly = TRUE), # calculated
        sample_label	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
        sample_code	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, unique = TRUE, readOnly = TRUE), # calculated
        sample_organ	= list(type = 'dropdown', required = TRUE, options = sample_organ_droplist),
        sample_preparation_method	= list(type = 'dropdown', required = TRUE, options = sample_preparation_method_droplist),
        sample_staining_method	= list(type = 'dropdown', required = TRUE, options = sample_staining_method_droplist),
        sample_mounting_method	= list(type = 'dropdown', required = TRUE, options = sample_mounting_method_droplist),
        sample_observation_method	= list(type = 'dropdown', required = TRUE, options = sample_observation_method_droplist),
        sample_image_file_name	= list(type = 'character', min_length = 1, max_length = 128, regex_pattern = NULL),
        sample_section_archived	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL),
        sample_archived	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL),
        sampling_height	= list(type = 'numeric', min_val = 0, max_val = 100),
        sample_apex_distance	= list(type = 'numeric', min_val = 0, max_val = 100),
        section_thickness	= list(type = 'numeric', min_val = 0, max_val = NULL),
        on_section_anatomical_data	= list(type = 'checkbox'),
        sample_comment	= list(type = 'character', min_length = 1, max_length = NULL)
      ),
      
      # Person table
      tbl6 = list(
        person_role = list(type = 'dropdown', required = TRUE, options = person_role_droplist),
        person_order = list(type = 'numeric', required = TRUE, min_val = 1, max_val = NULL),
        last_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        first_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        email = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"),
        orcid = list(type = 'character', required = TRUE, min_length = 1, max_length = 19),# , regex_pattern = "^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]{1}$"
        organization_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 128), # drop # calculated
        research_organization_registry = list(type = 'character', required = TRUE, min_length = 1, max_length = 64), # drop # calculated , regex_pattern = "^\\+?[0-9 ()-]{7,20}$"
        organization_name_finder = NULL, # empty
        department = list(type = 'character', min_length = 1, max_length = 64),
        street = list(type = 'character', min_length = 1, max_length = 64),
        postal_code = list(type = 'character', min_length = 1, max_length = 64),
        city = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        #country = list(type = 'dropdown', required = TRUE, options = country_droplist), # drop
        #person_country_code = list(type = 'dropdown', required = TRUE, options = country_code_droplist), # calculated
        country = list(type = 'character', required = TRUE, min_length = 1, max_length = 64), # drop
        person_country_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 64), # calculated
        webpage = list(type = 'character', min_length = 1, max_length = 64, regex_pattern = "^https?://.+"),
        phone_number = list(type = 'character', min_length = 1, max_length = 128, regex_pattern = NULL)
      ),
      
      # Publication table
      tbl7 = list(
        first_author_last_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        title = list(type = 'character', required = TRUE, min_length = 1, max_length = NULL, regex_pattern = NULL),
        publication_year = list(type = 'numeric', required = TRUE, min_val = 1950, max_val = format(Sys.Date(), "%Y")),
        journal = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        doi = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, "^(10\\.\\d{4,9}/[-._;()/:A-Z0-9]+|10\\.1002/[^\\s]+)$", unique = TRUE)
      )
      
    )
    return(modifyList(base_configs, extended_configs))
  }
  
  # If WB_meta not available, return just base
  return(base_configs)
}
