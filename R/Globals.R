

utils::globalVariables(c(
  "path_out", "Network", "Site_code", "Tree_label", "Number.of.samples",
  "Sample_id", "Measure_Radial", "Measure", "Value", "Date", "tbl_Person", "tbl_Site", "tbl_Tree", 
  "tbl_Sample", "tbl_Publication", "Koppen_climate_class", "Organization_name", "Name", "Table", 
  "cell", "constraints", 'data origin', "elevation", "itrdb_species_code", "latitude", "leaf_habit", 
  "longitude", "navPanel", "navsetCardTab", "network_code", "network_label", "number_of_samples", "observe",
  "observeEvent", "on_tree_dendrometer_data",
  "on_tree_phenological_observation", "on_tree_sapflux_data",
  "on_tree_shoot_growth_data", "on_tree_weather_data", "plot_code", "plot_label",
  "reactive", "renderLeaflet", "renderPlotly", "renderReactable", "renderText", "req",
  "sample_code", "sample_comment", "sample_date", "sample_id", "sample_label",
  "setNames", "shinyApp", "site_code", "site_label", "tree_age", "tree_code",
  "tree_comment", "tree_dbh", "tree_health_status", "tree_height", "tree_label",
  "tree_latitude", "tree_longitude", "tree_origin", "tree_ring_anatomical_data",
  "tree_ring_isotope_data", "tree_ring_structure", "tree_ring_width_data",
  "tree_sex", "tree_social_status", "tree_species", "tree_treatment",
  "updateActionButton", "updateSelectInput", "updateTabsetPanel", "wood_type", "year",
  "year_label", "zone_code", "zone_hierarchy", "zone_name", "zone_type", ".", "cell constraints", 
  "Mandatory", "Domain", "toggleState", "parent", "value", "label", "head", "obs_file", "Date_measure", 
  "Observation_measure", "Precision_date_measure", "address", "country_name", "geonames_details", "name", "sampling_date", "types"
))

# hot_col_wrapper <- function(ht, col, col_config, tbl_ref_data = NULL, ref_col_name = NULL) {
#   readOnly <- ifelse(is.null(col_config$readOnly), FALSE, col_config$readOnly)
#   
#   # for column validation based on reference table data
#   if (!is.null(tbl_ref_data) && !is.null(ref_col_name) && col == ref_col_name) {
#     renderer_js <- renderer_unique_column(tbl_ref_data, ref_col_name)  # Ensure tbl2$site_code is valid
#     ht <- ht %>%
#       rhandsontable::hot_col(
#         col,
#         renderer = renderer_js,
#         readOnly = readOnly
#       )
#   } else if (col_config$type == 'character') {  # for char cols
    
    
hot_col_wrapper <- function(ht, col, col_config) {
  readOnly <- ifelse(is.null(col_config$readOnly), FALSE, col_config$readOnly)
  # for char cols:
  if (col_config$type == 'character'){
    renderer_js <- renderer_char(
      required = col_config$required,
      min_length = col_config$min_length,
      max_length = col_config$max_length,
      regex_pattern = col_config$regex_pattern,
      unique = col_config$unique
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        renderer = renderer_js,
        readOnly = readOnly
      )
    # for numeric cols:
  } else if (col_config$type == 'numeric') {
    renderer_js <- renderer_num(
      required = col_config$required,
      min_val = col_config$min_val,
      max_val = col_config$max_val
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        type = 'numeric',
        renderer = renderer_js,
        readOnly = readOnly
      )
    # for dropdown cols:
  } else if (col_config$type == 'dropdown') {
    renderer_js <- renderer_drop(
      required = col_config$required,
      options = col_config$options
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        type = 'dropdown',
        source = col_config$options,
        renderer = renderer_js,
        readOnly = readOnly
      )
    # for checkbox cols:
  } else if (col_config$type == 'checkbox') {
    renderer_js <- renderer_check(
      required = col_config$required,
      min_checks = col_config$min_checks,
      max_checks = col_config$max_checks
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        type = 'checkbox',
        renderer = renderer_js,
        readOnly = readOnly
      )
    # for date cols:
  } else if (col_config$type == 'date') {
    renderer_js <- renderer_date(
      required = col_config$required
    )
    ht %>%
      rhandsontable::hot_col(
        col,
        type = 'date',
        dateFormat = "YYYY-MM-DD",
        renderer = renderer_js,
        readOnly = readOnly
      )
  }
}

renderer_char <- function(required = NULL, min_length = NULL, max_length = NULL, regex_pattern = NULL, unique = NULL){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  minl <- ifelse(is.null(min_length), -1, min_length)
  maxl <- ifelse(is.null(max_length), 10000, max_length)
  regp <- ifelse(is.null(regex_pattern), "", regex_pattern)
  check_regex <- ifelse(is.null(regex_pattern), "false", 'true')
  check_unique <- ifelse(is.null(unique), "false", ifelse(unique, "true", "false"))
  
  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      // remove old tippy if necessary
      if(td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';

      // check if value is empty
      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      // check char length
      } else if (value.length < %s || value.length > %s) {
        isValid = false;
        message = 'invalid length';
      // check regex pattern
      } else if (%s) {
        var regex = new RegExp('%s');
        if (!regex.test(value)) {
          isValid = false;
          message = 'invalid format';
        }
      // check uniqueness
      } else if (%s) {
        var data = instance.getDataAtCol(col);
        var duplicates = data.filter(function(val, index, arr) {
          return arr.indexOf(val) !== index && val === value;
        });
        if (duplicates.length > 0) {
          isValid = false;
          message = 'duplicate values';
        }
      }

      if (!isValid) {
        // set background color and tooltip
        td.style.background = '#ff4c42';
        tippy(td, { content: message });
      } else {
        td.style.background = '';
      }

      Handsontable.renderers.TextRenderer.apply(this, arguments);

      return td;
    }", check_required, minl, maxl, check_regex, regp, check_unique)))
}

renderer_drop <- function(required = NULL, options){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  options_js <- paste0("[", paste0(sprintf("'%s'", options), collapse = ", "), "]")
  
  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      // remove old tippy if necessary
      if (td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';

      // check if value is empty
      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      } else if (%s.indexOf(value) === -1) {
        // check if value is in options
        isValid = false;
        message = 'invalid choice';
      }

      if (!isValid) {
        // set background color and tooltip
        td.style.background = '#ff4c42';
        tippy(td, { content: message });
      } else {
        td.style.background = '';
      }


      Handsontable.renderers.DropdownRenderer.apply(this, arguments);

      return td;
    }", check_required, options_js)))
  
}

renderer_num <- function(required = NULL, min_val = NULL, max_val = NULL){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  check_min_val <- ifelse(is.null(min_val), "false", "true")
  minv <- ifelse(is.null(min_val), "null", min_val)
  check_max_val <- ifelse(is.null(max_val), "false", "true")
  maxv <- ifelse(is.null(max_val), "null", max_val)
  
  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      // remove old tippy if necessary
      if(td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';
      var num_value = parseFloat(value)

      // check if value is empty, and if required, set invalid
      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      } else if (isNaN(num_value)) {
        // check if value is a number
        isValid = false;
        message = 'not a number';
      } else {
        // check value against min_val if provided
        if (%s && num_value < %s) {
          isValid = false;
          message = 'out of range';
        }
        // check value againts max_val if provided
        if (%s && num_value > %s) {
          isValid = false;
          message = 'out of range';
        }
      }

      if (!isValid) {
        // set background color and tooltip
        td.style.background = '#ff4c42';
        tippy(td, { content: message });
      } else {
        td.style.background = '';
      }

      Handsontable.renderers.NumericRenderer.apply(this, arguments);

      return td;
    }", check_required, check_min_val, minv, check_max_val, maxv)))
}

renderer_check <- function(required = NULL, min_checks = NULL, max_checks = NULL){
  # if required, we need at least one checked box
  mincb <- ifelse(is.null(required), 0, ifelse(required, 1, 0))
  # if min_checks is given and > 1, we update mincb
  mincb <- ifelse(is.null(min_checks), mincb, ifelse(min_checks > 1, min_checks, mincb))
  maxcb <- ifelse(is.null(max_checks), 10000, max_checks)
  
  htmlwidgets::JS(htmltools::HTML(sprintf("
  function(instance, td, row, col, prop, value, cellProperties) {
    // remove old tippy if necessary
        if(td.hasOwnProperty('_tippy')) {
          td._tippy.destroy();
        }

    var isValid = true;
    var message = '';

    // find how many boxes are checked
    var data = instance.getDataAtCol(col);
    var true_count = data.filter(function(val) { return val === true; }).length;
    console.log(true_count);

    if (true_count < %s) {
      isValid = false;
      message = 'too few checked';
    } else if (true_count > %s) {
      isValid = false;
      message = 'too many checked';
    }

    if (!isValid) {
      // set background color and tooltip
      td.style.background = '#ff4c42';
      tippy(td, { content: message });
    } else {
      td.style.background = '';
    }

    Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
    return td;
  }", mincb, maxcb)))
}

renderer_date <- function(required = NULL){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  
  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      // remove old tippy if necessary
      if(td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';
      // expect YYYY-MM-DD format
      var dateRegex = /^\\d{4}-\\d{2}-\\d{2}$/;

      // check if value is empty, and if required, set invalid
      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      } else if (!dateRegex.test(value) || isNaN(Date.parse(value))) {
        isValid = false;
        message = 'invalid date';
      }

      if (!isValid) {
          // set background color and tooltip
          td.style.background = '#ff4c42';
          tippy(td, { content: message });
      } else {
        td.style.background = '';
      }

      Handsontable.renderers.DateRenderer.apply(this, arguments);
      return td;
      }", check_required)))
}

# renderer_unique_column <- function(tbl_ref_data, ref_col_name) {
#   # Get unique values from the reference column (e.g., tbl1$site_code)
#   unique(na.omit(tbl_ref_data[[ref_col_name]]))
#   
#   htmlwidgets::JS(htmltools::HTML(sprintf("
#     function(instance, td, row, col, prop, value, cellProperties) {
#       // Remove old tippy if present
#       if(td.hasOwnProperty('_tippy')) {
#         td._tippy.destroy();
#       }
# 
#       var isValid = true;
#       var message = 'not in observation data';
# 
#       // Check if value is empty
#       if (value === null || value === '') {
#         isValid = false;
#         message = 'required field';
#       }
#       // Check if value exists in the reference column (from tbl1$site_code)
#       else if (%s.indexOf(value) === -1) {
#         isValid = false;
#         message = 'value does not exist in tbl1$site_code';
#       }
# 
#       // Apply validation result
#       if (!isValid) {
#         td.style.background = '#ff4c42';  // Invalid background
#         tippy(td, { content: message });
#       } else {
#         td.style.background = '';  // Reset valid background
#       }
# 
#       Handsontable.renderers.TextRenderer.apply(this, arguments);
#       return td;
#     }
#   ", jsonlite::toJSON(ref_values, auto_unbox = TRUE))))
# }



# #### validation configuration
# column_configs <- list(
#   tbl1 = list(
#     site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 5, regex_pattern = NULL, unique = TRUE, readOnly = TRUE),
#     latitude = list(type = 'numeric', required = TRUE, min_val = -90, max_val = 90),
#     longitude = list(type = 'numeric', required = TRUE, min_val = -180, max_val = 180),
#     elevation = list(type = 'numeric', required = TRUE, min_val = 0, max_val = NULL)
#   ),
#   
#   tbl2 = list(
#     sample_date = list(type = "date", required = TRUE),
#     sample_id = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
#     tree_species = list(type = "dropdown", required = TRUE, options =c("Picea abies (L.) Karst.", "Larix decidua Mill.")),
#     # tree_species = list(type = "dropdown", required = TRUE, options = tree_species_droplist), # drop
#     tree_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
#     plot_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
#     site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 5, readOnly = FALSE),
#     network_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64, unique = TRUE),
#     sample_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64, unique = TRUE),
#     radial_file = list(type = "character", required = TRUE, min_length = 1, max_length = 6)
#   ),
#   
#   tbl3 = list(
#     network_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL), # calculated
#     network_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
#     country_code = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#     site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = TRUE),
#     site_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
#     latitude = list(type = 'numeric', required = TRUE, min_val = -90, max_val = 90), # calculated
#     longitude = list(type = 'numeric', required = TRUE, min_val = -180, max_val = 180), # calculated
#     elevation = list(type = 'numeric', required = TRUE, min_val = 0, max_val = 10000), # integer
#     koppen_climate_value = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill."), readOnly = TRUE), # calculated # drop
#     koppen_climate_code = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill."), readOnly = TRUE), # calculated # drop
#     koppen_climate_classification = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill."), readOnly = TRUE), # calculated # drop
#     site_aspect = list(type = 'numeric', min_val = 0, max_val = 360, regex_pattern = NULL, unique = TRUE), # integer
#     site_slope = list(type = 'numeric', min_val = 0, max_val = NULL, regex_pattern = NULL, unique = TRUE), # integer
#     site_topography = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#     soil_depth = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#     soil_water_holding_capacity = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#     forest_stand_type = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#     forest_stand_structure = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#     forest_stand_age = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#     forest_stand_main_species_composition = list(type = 'character', min_length = 1, max_length = 5, regex_pattern = NULL, unique = TRUE, readOnly = FALSE), # %in% ITRDB
#     forest_stand_management_intensity = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#     in_stand_dendrometer_data = list(type = 'checkbox'),
#     in_stand_sapflux_data = list(type = 'checkbox'),
#     in_stand_phenological_observation = list(type = 'checkbox'),
#     in_stand_weather_data = list(type = 'checkbox'),
#     in_stand_soil_data = list(type = 'checkbox'),
#     in_stand_other_data = list(type = 'character', min_length = 1, max_length = NULL),
#     site_comment = list(type = 'character', min_length = 1, max_length = NULL)
#   ), 
#     
#     tbl4 = list(
#       tree_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = TRUE), # calculated
#       tree_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
#       plot_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL), # calculated
#       plot_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
#       tree_species = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       itrdb_species_code = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # calculated # drop
#       wood_type = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # calculated # drop
#       leaf_habit = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # calculated # drop
#       tree_ring_structure = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # calculated # drop
#       tree_treatment = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # calculated# drop
#       tree_dbh = list(type = 'numeric', min_val = 0, max_val = 500),
#       tree_height = list(type = 'numeric', min_val = 0, max_val = 100),
#       tree_age = list(type = 'numeric', min_val = 0, max_val = 1000),
#       tree_sex = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       tree_social_status = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       tree_health_status = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       tree_origin = list(type = 'dropdown', options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       tree_latitude = list(type = 'numeric', min_val = -90, max_val = 90),
#       tree_longitude = list(type = 'numeric', min_val = -180, max_val = 180),
#       on_tree_dendrometer_data = list(type = 'checkbox'),
#       on_tree_sapflux_data = list(type = 'checkbox'),
#       on_tree_phenological_observation = list(type = 'checkbox'),
#       on_tree_weather_data = list(type = 'checkbox'),
#       on_tree_shoot_growth_data = list(type = 'checkbox'),
#       tree_ring_width_data = list(type = 'checkbox'),
#       tree_ring_anatomical_data = list(type = 'checkbox'),
#       tree_ring_isotope_data = list(type = 'checkbox'),
#       number_of_samples = list(type = 'numeric', required = TRUE, min_val = 0, max_val = NULL), # calculated
#       tree_comment = list(type = 'character', min_length = 1, max_length = NULL)
#     ),
#     
#     tbl5 = list(
#       sample_label	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, unique = TRUE, readOnly = TRUE), # calculated
#       sample_code	= list(type = 'character', required = TRUE, min_length = 1, max_length = 10, unique = TRUE, readOnly = TRUE), # calculated
#       sample_organ	= list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       sample_preparation_method	= list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       sample_staining_method	= list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       sample_mounting_method	= list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       sample_observation_method	= list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       sample_image_file_name	= list(type = 'character', min_length = 1, max_length = 128, regex_pattern = NULL),
#       sample_section_archived	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL),
#       sample_archived	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL),
#       sampling_height	= list(type = 'numeric', min_val = 0, max_val = 100),
#       sample_apex_distance	= list(type = 'numeric', min_val = 0, max_val = 100),
#       section_thickness	= list(type = 'numeric', min_val = 0, max_val = NULL),
#       on_section_anatomical_data	= list(type = 'checkbox'),
#       sample_comment	= list(type = 'character', min_length = 1, max_length = NULL)
#     ),
#     
#     tbl6 = list(
#       person_role = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       person_order = list(type = 'numeric', required = TRUE, min_val = 0, max_val = NULL),
#       last_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
#       first_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
#       email = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = "^\\S+@\\S+\\.\\S+$"),
#       orcid = list(type = 'character', required = TRUE, min_length = 1, max_length = 19, regex_pattern = "^\\d{4}-\\d{4}-\\d{4}-\\d{4}$"),
#       organization_name = list(type = 'dropdown', required = TRUE, min_length = 1, max_length = 128, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop # calculated
#       research_organization_registry = list(type = 'dropdown', required = TRUE, max_length = 64, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop # calculated
#       organization_name_finder = NULL, # empty
#       department = list(type = 'character', min_length = 1, max_length = 64, regex_pattern = NULL),
#       street = list(type = 'character', min_length = 1, max_length = 64, regex_pattern = NULL),
#       postal_code = list(type = 'character', min_length = 1, max_length = 64, regex_pattern = NULL),
#       city = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL),
#       country = list(type = 'dropdown', required = TRUE, options = c("Picea abies (L.) Karst.", "Larix decidua Mill.")), # drop
#       person_country_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 2, regex_pattern = NULL), # calculated
#       webpage = list(type = 'character', min_length = 1, max_length = 64, regex_pattern = "^https?://.+"),
#       phone_number = list(type = 'character', min_length = 1, max_length = 15, regex_pattern = "^\\+?[0-9 ()-]{7,20}$")
#     ),
#     
#     tbl7 = list(
#       first_author_last_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, unique = TRUE),
#       title = list(type = 'character', required = TRUE, min_length = 1, max_length = NULL, regex_pattern = NULL),
#       publication_year = list(type = 'numeric', required = TRUE, min_val = 1950, max_val = format(Sys.Date(), "%Y")),
#       journal = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
#       doi = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = "^https?://.+")
#     )
#     
#   )



