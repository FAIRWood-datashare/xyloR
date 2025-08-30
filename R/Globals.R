# global.R

# 1) Packages
library(shiny)
library(shinyjs)
library(bslib)
library(openxlsx)

# 2) Pure helper functions (no `input`, no `reactive()`)
hot_col_wrapper <- function(ht, col, col_config) {
  readOnly <- ifelse(is.null(col_config$readOnly), FALSE, col_config$readOnly)
  # for char cols:
  if (col_config$type == 'character'){
    renderer_js <- renderer_char(
      required = col_config$required,
      min_length = col_config$min_length,
      max_length = col_config$max_length,
      regex_pattern = col_config$regex_pattern,
      unique = col_config$unique,
      unique.comp = col_config$unique.comp
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
        readOnly = readOnly,
        allowInvalid = TRUE,
        validator = htmlwidgets::JS(
          "function (value, callback) {
           if (value === null || value === '' || value === undefined) {
             callback(true);   // allow NA / empty
           } else {
             callback(!isNaN(value)); // valid if numeric
           }
         }"
        )
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

renderer_char <- function(required = NULL, min_length = NULL, max_length = NULL, regex_pattern = NULL, unique = FALSE, unique.comp = FALSE) {
  if (isTRUE(unique.comp)) {unique <- FALSE}
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  minl <- ifelse(is.null(min_length), -1, min_length)
  maxl <- ifelse(is.null(max_length), 10000, max_length)
  regp <- ifelse(is.null(regex_pattern), "", regex_pattern)
  check_regex <- ifelse(is.null(regex_pattern), "false", "true")
  check_unique <- ifelse(isTRUE(unique), "true", "false")
  check_unique_comp <- ifelse(isTRUE(unique.comp), "true", "false")
  
  
  htmlwidgets::JS(htmltools::HTML(sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      if(td.hasOwnProperty('_tippy')) {
        td._tippy.destroy();
      }

      var isValid = true;
      var message = '';

      if (value === null || value === '') {
  if (%s) {
    isValid = false;
    message = 'required field';
  }
}

if (value && (value.length < %s || value.length > %s)) {
  isValid = false;
  message = 'invalid length';
}

if (%s && value) {
  var regex = new RegExp('%s');
  if (!regex.test(value)) {
    isValid = false;
    message = 'invalid format';
  }
}

if (%s && value) {
  var data = instance.getDataAtCol(col);
  var duplicates = data.filter(function(val, index, arr) {
    return arr.indexOf(val) !== index && val === value;
  });
  if (duplicates.length > 0) {
    isValid = false;
    message = 'duplicate values';
  }
}

if (%s) {
  var data = instance.getData();
  console.log('Full data:', data);

  var composites = data.map(function(r) {
    return r[0] + '_' + r[3] + '_' + r[1];
  });

  var currentComposite = composites[row];
  console.log('Composite keys:', composites);
  console.log('Current row composite:', currentComposite);

  var duplicates = composites.filter(function(val, i) {
    return i !== row && val === currentComposite;
  });

  console.log('Duplicate entries for current:', duplicates);

  if (duplicates.length > 0) {
    isValid = false;
    message = 'duplicate tree if grouped by site and plot';
  }
}


      if (!isValid) {
        td.style.background = '#ff4c42';
        tippy(td, { content: message });
      } else {
        td.style.background = '';
      }

      if (!cellProperties.readOnly) {
        td.style.color = '#FFFF00';
      } else {
        td.style.color = '';
      }

      Handsontable.renderers.TextRenderer.apply(this, arguments);
      return td;
    }", check_required, minl, maxl, check_regex, regp, check_unique, check_unique_comp)))
}

renderer_drop <- function(required = NULL, options, readOnly = FALSE){
  check_required <- ifelse(is.null(required), "false", ifelse(required, "true", "false"))
  #options_js <- paste0("[", paste0(sprintf("'%s'", options), collapse = ", "), "]")
  options_js <- jsonlite::toJSON(options, auto_unbox = TRUE)
  
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
        td.style.color = '#FFFF00';
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
        td.style.color = '#FFFF00';
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
        td.style.color = '#FFFF00';
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
        td.style.color = '#FFFF00';
      } else {
        td.style.color = '';
      }

      Handsontable.renderers.DateRenderer.apply(this, arguments);
      return td;
      }", check_required)))
}

# save_and_validate <- function(data_reactive, sheet_name, wb_reactive, temp_folder, update_validation) {
#   req(data_reactive)
#   req(wb_reactive())
#   
#   # Determine whether the sheet belongs to meta or obs
#   meta_sheets <- c("site", "tree", "sample", "person", "publication")
#   print(sheet_name)
#   is_meta_sheet <- sheet_name %in% meta_sheets
#   
#   # Get workbook
#   wb <- wb_reactive()
#   
#   # Write data to the sheet (starting row depends on sheet name)
#   openxlsx::writeData(
#     wb,
#     sheet = sheet_name,
#     x = data_reactive,
#     startCol = 1,
#     startRow = ifelse(sheet_name == "obs_data_info", 6, 8),
#     colNames = FALSE,
#     rowNames = FALSE
#   )
#   
#   files <- list.files(temp_folder(), pattern = "*.xlsx", recursive = TRUE) 
#   xylo_file_name <- files[grep("xylo_data", files)]
#   meta_file_name <- files[grep("xylo_meta", files)]
#   xylo_file_path <- paste(temp_folder(), xylo_file_name, sep = "/")
#   meta_file_path <- paste(temp_folder(), meta_file_name, sep = "/")
#   
#   # Choose file paths based on the type of sheet
#   print("meta_file_path")
#   print(meta_file_path)
#   print("xylo_file_path")
#   print(xylo_file_path)
#   path_of_file_changed_by_user <- if (is_meta_sheet) meta_file_path else xylo_file_path
#   
#   # Save workbook
#   openxlsx::saveWorkbook(wb, path_of_file_changed_by_user, overwrite = TRUE)
#   
#   # Notify user
#   showNotification(
#     paste0("Data written to sheet '", sheet_name, "' of ", path_of_file_changed_by_user),
#     type = "message"
#   )
#   print(paste("File saved at:", path_of_file_changed_by_user))
#   
#   
#   tbl_validation <- rbind(
#       xylo_format_validation(xylo_file_path),
#       meta_format_validation(meta_file_path)
#     )
#   
#   # Update validation reactive
#   update_validation(tbl_validation)
# }

save_and_validate <- function(data_reactive, sheet_name, wb_reactive, temp_folder, update_validation) {
  req(data_reactive)
  req(wb_reactive())
  
  # Start the progress bar
  shiny::withProgress(message = 'Saving and validating data...', value = 0, {
    
    # Step 1: Determine whether the sheet belongs to meta or obs
    meta_sheets <- c("site", "tree", "sample", "person", "publication")
    is_meta_sheet <- sheet_name %in% meta_sheets
    
    # Update progress to indicate the determination step
    shiny::setProgress(value = 0.1, detail = "Determining sheet type...")
    
    # Step 2: Get the workbook
    wb <- wb_reactive()
    
    # Step 3: Write data to the sheet
    shiny::setProgress(value = 0.3, detail = "Writing data to sheet...")
    
    openxlsx::writeData(
      wb,
      sheet = sheet_name,
      x = data_reactive,
      startCol = 1,
      startRow = ifelse(sheet_name == "obs_data_info", 6, 8),
      colNames = FALSE,
      rowNames = FALSE
    )
    
    # Step 4: Get the list of files and choose the correct file path
    shiny::setProgress(value = 0.5, detail = "Fetching file paths...")
    
    files <- list.files(temp_folder(), pattern = "*.xlsx", recursive = TRUE)
    xylo_file_name <- files[grep("xylo_data", files)]
    meta_file_name <- files[grep("xylo_meta", files)]
    xylo_file_path <- paste(temp_folder(), xylo_file_name, sep = "/")
    meta_file_path <- paste(temp_folder(), meta_file_name, sep = "/")
    
    # Choose file paths based on the type of sheet
    path_of_file_changed_by_user <- if (is_meta_sheet) meta_file_path else xylo_file_path
    
    # Step 5: Save workbook to the correct file
    shiny::setProgress(value = 0.7, detail = "Saving workbook...")
    
    openxlsx::saveWorkbook(wb, path_of_file_changed_by_user, overwrite = TRUE)
    
    # Step 6: Notify user that the data has been saved
    shiny::setProgress(value = 0.9, detail = "Data saved, validating format...")
    
    showNotification(
      paste0("Data written to sheet '", sheet_name, "' of ", path_of_file_changed_by_user),
      type = "message"
    )
    
    tbl_validation <- rbind(
      xylo_format_validation(xylo_file_path),
      meta_format_validation(meta_file_path)
    )
    
    # Step 7: Update validation reactive
    update_validation(tbl_validation)
    
    # Complete the progress bar
    shiny::setProgress(value = 1, detail = "Validation complete.")
  })
}


# 3) Static data you can load once (instead of inside a reactive)
# obs_template_path  <- system.file("extdata","Datasetname_xylo_data_yyyy-mm-dd.xlsx", package="xyloR")
# meta_template_path <- system.file("extdata","Datasetname_xylo_meta_yyyy-mm-dd.xlsx", package="xyloR")
# drop_list_obs  <- openxlsx::readWorkbook(obs_template_path,  sheet="DropList")
# drop_list_meta <- openxlsx::readWorkbook(meta_template_path, sheet="DropList")




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





#' Get country codes
#' 
#' @export
#' 
get_country_codes <- function(){
  file_path <- system.file("extdata", "country_ISO3166-1-alpha-2_20241205.csv", package = "xyloR")
  iso_countries <- read.csv(file_path, stringsAsFactors = FALSE, na.strings=c(""))
  country_list <- setNames(iso_countries$Code, 
                           paste(iso_countries$Name, "  (", 
                                 iso_countries$Code, ")", sep = ""))
  return(country_list)
}

