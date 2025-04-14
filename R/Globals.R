

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
      options = col_config$options,
      readOnly = readOnly
    )
    # Only set dropdown if editable
    if (readOnly) {
      ht %>%
        rhandsontable::hot_col(
          col,
          renderer = renderer_js,
          readOnly = TRUE
        )
    } else {
      ht %>%
        rhandsontable::hot_col(
          col,
          type = 'dropdown',
          source = col_config$options,
          renderer = renderer_js,
          readOnly = FALSE
        )
    }
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
      
      if (!cellProperties.readOnly) {
        td.style.color = '#e91e63';
      } else {
        td.style.color = '';
      }

      Handsontable.renderers.TextRenderer.apply(this, arguments);

      return td;
    }", check_required, minl, maxl, check_regex, regp, check_unique)))
}

renderer_drop <- function(required = NULL, options, readOnly = FALSE){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  options_js <- paste0("[", paste0(sprintf("'%s'", options), collapse = ", "), "]")
  
  renderer_type <- if (readOnly) "TextRenderer" else "DropdownRenderer"
  
  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      if (td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';

      if (value === null || value === '') {
        if (%s) {
          isValid = false;
          message = 'required field';
        }
      } else if (%s.indexOf(value) === -1) {
        isValid = false;
        message = 'invalid choice';
      }

      if (!isValid) {
        td.style.background = '#ff4c42';
        tippy(td, { content: message });
      } else {
        td.style.background = '';
      }
      
      if (!cellProperties.readOnly) {
        td.style.color = '#e91e63';
      } else {
        td.style.color = '';
      }

      Handsontable.renderers.%s.apply(this, arguments);
      return td;
    }", check_required, options_js, renderer_type)))
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
      
      if (!cellProperties.readOnly) {
        td.style.color = '#e91e63';
      } else {
        td.style.color = '';
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
      
      if (!cellProperties.readOnly) {
        td.style.color = '#e91e63';
      } else {
        td.style.color = '';
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
      
      if (!cellProperties.readOnly) {
        td.style.color = '#e91e63';
      } else {
        td.style.color = '';
      }

      Handsontable.renderers.DateRenderer.apply(this, arguments);
      return td;
      }", check_required)))
}



