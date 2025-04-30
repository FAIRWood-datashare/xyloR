#' Site Metadata Tab UI
#'
#' Creates the UI for the "Site" tab in the Shiny application, which allows the user
#' to view and edit site metadata in a reactive `rHandsontable`.
#'
#' @param id The module ID, which is used to create unique namespaces for the inputs
#' and outputs within the module.
#' @return A `fluidRow` containing a `Save site` button and a table for site metadata.
#' 
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' 
mod_tab4_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # TAB 4: Site -----------------------------------------------
  bslib::nav_panel(
    title = htmltools::div(id = ns("site_tab"), "Site"),
    value = "Site",
    
    shiny::fluidRow(
      # Sidebar (left) — Save button
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_header(NULL),
          bslib::card_body(
            shiny::actionButton(ns("save_site"), label = htmltools::tagList(bsicons::bs_icon("save"), "Save site"), class = "btn-primary")
          )
        )
      ),
      
      
      # Main content (right) — rhandsontable
      shiny::column(
        11, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Site Metadata"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl3"))
          )
        )
      )
    )
  )
}

#' Server logic for Site Metadata Tab
#'
#' This module handles the server-side logic for the "Site" tab. It manages the reactive
#' data, updates the site metadata table (`rHandsontable`), and synchronizes the Koppen
#' climate data. It also provides functionality for saving the updated data to a workbook.
#'
#' @param id The module ID, which is used to create unique namespaces for the inputs
#' and outputs within the module.
#' @param shared A shared object containing global reactive values or functions, such as the workbook.
#' @return A server-side function for managing the site metadata table, synchronizing
#' Koppen climate data, and saving the updated table.
#' 
#' @import shiny
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @importFrom dplyr select mutate filter left_join ends_with
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r
#' @importFrom magrittr %>%
#' 
#' 
mod_tab4_server <- function(id, out_tab1, out_tab2, out_tab3) {
  moduleServer(id, function(input, output, session) {
    
    # Accessing shared elements
    
    # TAB 4 site: -------------------------------------------------------------------
    
    #### dsite ####
    dsite <- shiny::reactiveVal()
    
    shiny::observe({
      shiny::req(out_tab2$meta_file())  # Ensure file is uploaded
      
      # Read the dataset
      site_meta_info <- openxlsx::readWorkbook(out_tab3$WB_meta(), sheet = "site", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
        tibble::tibble()
      dsite(site_meta_info)  # Store in reactive value
    })
    
    # # INITIALIZE REACTIVE INPUT DATA
    data_meta <- shiny::reactiveValues()
    
    # Reactive context to store initial data and ensure it's updated in a proper context
    observe({
      data_meta$tbl3 <- dsite()  # Access dsite in a valid reactive context
    })
    
    # Koppen_family
    koppen_family <- shiny::reactiveVal()
    
    # Read the initial data for Koppen families
    shiny::observe({
      shiny::req(out_tab3$WB_meta())
      df <- openxlsx::readWorkbook(out_tab3$WB_meta(), sheet = "DropList", colNames = TRUE) %>%
        dplyr::select(koppen_climate_value, koppen_climate_code, koppen_climate_classification) %>%
        dplyr::mutate(koppen_climate_value = as.character(koppen_climate_value)) %>%
        data.frame(stringsAsFactors = FALSE)
      koppen_family(df)
    })
    
    # Function to synchronize Koppen climate data
    sync_koppen_code <- function(df, koppen_family, remove_na = TRUE) {
      df <- df %>%
        dplyr::mutate(koppen_climate_value = as.character(koppen_climate_value))
      
      if (remove_na) {
        df <- df %>%
          dplyr::filter(!is.na(koppen_climate_value) & koppen_climate_value != "")
      }
      
      updated <- df %>%
        dplyr::left_join(
          koppen_family %>%
            dplyr::mutate(koppen_climate_value = as.character(koppen_climate_value)),
          by = "koppen_climate_value",
          suffix = c("", "_from_list")
        ) %>%
        dplyr::mutate(
          koppen_climate_code = koppen_climate_code_from_list,
          koppen_climate_classification = koppen_climate_classification_from_list
        ) %>%
        dplyr::select(-dplyr::ends_with("_from_list"))
      
      return(updated)
    }
    
    
    # RENDER TABLES
    output$tbl3 <- rhandsontable::renderRHandsontable({
      shiny::req(data_meta$tbl3)  # Ensure data is available
      rhandsontable::rhandsontable(
        data_meta$tbl3, 
        rowHeaders = NULL, contextMenu = TRUE, stretchH = 'all', height=150) %>%
        hot_col_wrapper('network_label', out_tab3$column_configs()$tbl3$network_label) %>%
        hot_col_wrapper('network_code', out_tab3$column_configs()$tbl3$network_code) %>%
        hot_col_wrapper('country_code', out_tab3$column_configs()$tbl3$country_code) %>%
        hot_col_wrapper('site_label', out_tab3$column_configs()$tbl3$site_label) %>%
        hot_col_wrapper('site_code', out_tab3$column_configs()$tbl3$site_code) %>%
        hot_col_wrapper('latitude', out_tab3$column_configs()$tbl3$latitude) %>%
        hot_col_wrapper('longitude', out_tab3$column_configs()$tbl3$longitude) %>%
        hot_col_wrapper('elevation', out_tab3$column_configs()$tbl3$elevation) %>%
        hot_col_wrapper('koppen_climate_value', out_tab3$column_configs()$tbl3$koppen_climate_value) %>%
        hot_col_wrapper('koppen_climate_code', out_tab3$column_configs()$tbl3$koppen_climate_code) %>%
        hot_col_wrapper('koppen_climate_classification', out_tab3$column_configs()$tbl3$koppen_climate_classification) %>%
        hot_col_wrapper('site_aspect', out_tab3$column_configs()$tbl3$site_aspect) %>%
        hot_col_wrapper('site_slope', out_tab3$column_configs()$tbl3$site_slope) %>%
        hot_col_wrapper('site_topography', out_tab3$column_configs()$tbl3$site_topography) %>%
        hot_col_wrapper('soil_depth', out_tab3$column_configs()$tbl3$soil_depth) %>%
        hot_col_wrapper('soil_water_holding_capacity', out_tab3$column_configs()$tbl3$soil_water_holding_capacity) %>%
        hot_col_wrapper('forest_stand_type', out_tab3$column_configs()$tbl3$forest_stand_type) %>%
        hot_col_wrapper('forest_stand_structure', out_tab3$column_configs()$tbl3$forest_stand_structure) %>%
        hot_col_wrapper('forest_stand_age', out_tab3$column_configs()$tbl3$forest_stand_age) %>%
        hot_col_wrapper('forest_stand_main_species_composition', out_tab3$column_configs()$tbl3$forest_stand_main_species_composition) %>%
        hot_col_wrapper('forest_stand_management_intensity', out_tab3$column_configs()$tbl3$forest_stand_management_intensity) %>%
        hot_col_wrapper('in_stand_dendrometer_data', out_tab3$column_configs()$tbl3$in_stand_dendrometer_data) %>%
        hot_col_wrapper('in_stand_sapflux_data', out_tab3$column_configs()$tbl3$in_stand_sapflux_data) %>%
        hot_col_wrapper('in_stand_phenological_observation', out_tab3$column_configs()$tbl3$in_stand_phenological_observation) %>%
        hot_col_wrapper('in_stand_weather_data', out_tab3$column_configs()$tbl3$in_stand_weather_data) %>%
        hot_col_wrapper('in_stand_soil_data', out_tab3$column_configs()$tbl3$in_stand_soil_data) %>%
        hot_col_wrapper('in_stand_other_data', out_tab3$column_configs()$tbl3$in_stand_other_data) %>%
        hot_col_wrapper('site_comment', out_tab3$column_configs()$tbl3$site_comment)
    })
    
    # Sync data on user input
    shiny::observeEvent(input$tbl3, {
      shiny::req(input$tbl3)
      user_data <- rhandsontable::hot_to_r(input$tbl3)
      updated_data <- sync_koppen_code(user_data, koppen_family())
      # Update the reactive data object
      isolate({data_meta$tbl3 <- updated_data
      })
      
    })
    
    observeEvent(input$save_site, {
      save_and_validate(
        data_reactive = data_meta$tbl3,
        sheet_name = "site",
        wb_reactive = out_tab3$WB_meta,
        temp_folder = out_tab1$temp_folder,
        update_validation = out_tab2$validation_results
      )
    })
    
    return(
      list(
        data_meta = data_meta
      )
    )
    
  })
}
