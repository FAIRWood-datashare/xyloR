#' Module UI for Sample Metadata Tab
#'
#' @description This function generates the UI components for the "Sample" tab, including a table for displaying and editing sample metadata.
#'
#' @param id A string representing the namespace for the UI components. This is required for module-based UI.
#'
#' @return A `fluidRow` containing the UI elements for the sample metadata table and a save button.
#' 
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' 
#' @export
#' 
mod_tab6_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # TAB 6: View sample -----------------------------------------------
  bslib::nav_panel(
    title = htmltools::div(id = ns("sample_tab"), "Sample"),
    value = "Sample",
    
    shiny::fluidRow(
      # Sidebar (left) — Actions or instructions
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_header(NULL),
          bslib::card_body(
            shiny::actionButton(ns("save_sample"), label = htmltools::tagList(bsicons::bs_icon("save"), "Save sample"), class = "btn-primary")
          )
        )
      ),
      
      
      # Main content (right) — Editable metadata table
      shiny::column(
        11, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Sample Metadata"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl5"))
          )
        )
      )
    )
  )
}

#' Server function for Sample Metadata Tab
#'
#' @description This function defines the server-side logic for the "Sample" tab, including loading sample metadata from an uploaded file, rendering the sample metadata table, and handling save events.
#'
#' @param id A string representing the namespace for the module's server logic. This is required for module-based server functions.
#' @param shared A reactive object containing shared data, including workbook references and validation results.
#'
#' @return NULL. This function performs side-effects, such as rendering the sample table and saving data.
#' 
#' @import shiny
#' @importFrom rhandsontable renderRHandsontable 
#' @importFrom dplyr mutate case_when select
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' 
#' 
#' @export
mod_tab6_server <- function(id, out_tab1, out_tab2, out_tab3, out_tab4) {
  moduleServer(id, function(input, output, session) {
    

    # TAB 6 sample: -------------------------------------------------------------------
    
    #### dsample  ####
    dsample <- shiny::reactiveVal()
    
    shiny::observe({
      req(out_tab2$meta_file)  # Ensure file is uploaded
      
      # Read the dataset
      sample_meta_info <- openxlsx::readWorkbook(out_tab3$WB_meta(), sheet = "sample", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
        tibble::tibble()
      
      # Check if sample_date is numeric and convert to Date
      sample_meta_info <- sample_meta_info %>%
        dplyr::mutate(
          sample_date = dplyr::case_when(
            # If sample_date is numeric (Excel date format)
            !is.na(sample_date) & is.numeric(as.numeric(sample_date)) ~
              as.Date(as.numeric(sample_date), origin = "1899-12-30"),
            
            TRUE ~ as.Date(NA)  # Handle NAs
          )
        ) %>% 
        dplyr::mutate(sample_date = as.character(sample_date))
      
      dsample(sample_meta_info)  # Store in reactive value
    })
    
    # # INITALIZE REACTIVE INPUT DATA
    # data_meta <- reactiveValues()
    
    # Reactive context to store initial data and ensure it's updated in a proper context
    observe({
      out_tab4$data_meta$tbl5 <- dsample()  # Access dinfo in a valid reactive context
    })
    
    # RENDER TABLES
    output$tbl5 <- rhandsontable::renderRHandsontable({
      shiny::req(out_tab4$data_meta$tbl5)  # Ensure data is available
      rhandsontable::rhandsontable(
        out_tab4$data_meta$tbl5,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = 'all') %>%
        hot_col_wrapper('tree_label', out_tab3$column_configs()$tbl5$tree_label) %>%
        hot_col_wrapper('sample_id', out_tab3$column_configs()$tbl5$sample_id) %>%
        hot_col_wrapper('sample_date', out_tab3$column_configs()$tbl5$sample_date) %>%
        hot_col_wrapper('sample_label', out_tab3$column_configs()$tbl5$sample_label) %>%
        hot_col_wrapper('sample_code', out_tab3$column_configs()$tbl5$sample_code) %>%
        hot_col_wrapper('sample_organ', out_tab3$column_configs()$tbl5$sample_organ) %>%
        hot_col_wrapper('sample_preparation_method', out_tab3$column_configs()$tbl5$sample_preparation_method) %>%
        hot_col_wrapper('sample_staining_method', out_tab3$column_configs()$tbl5$sample_staining_method) %>%
        hot_col_wrapper('sample_mounting_method', out_tab3$column_configs()$tbl5$sample_mounting_method) %>%
        hot_col_wrapper('sample_observation_method', out_tab3$column_configs()$tbl5$sample_observation_method) %>%
        hot_col_wrapper('sample_image_file_name', out_tab3$column_configs()$tbl5$sample_image_file_name) %>%
        hot_col_wrapper('sample_section_archived', out_tab3$column_configs()$tbl5$sample_section_archived) %>%
        hot_col_wrapper('sample_archived', out_tab3$column_configs()$tbl5$sample_archived) %>%
        hot_col_wrapper('sampling_height', out_tab3$column_configs()$tbl5$sampling_height) %>%
        hot_col_wrapper('sample_apex_distance', out_tab3$column_configs()$tbl5$sample_apex_distance) %>%
        hot_col_wrapper('section_thickness', out_tab3$column_configs()$tbl5$section_thickness) %>%
        hot_col_wrapper('on_section_anatomical_data', out_tab3$column_configs()$tbl5$on_section_anatomical_data) %>%
        hot_col_wrapper('sample_comment', out_tab3$column_configs()$tbl5$sample_comment)
    }) 
    
    # Sync data on user input
    shiny::observeEvent(input$tbl5, {
      shiny::req(input$tbl5)
      out_tab4$data_meta$tbl5 <- rhandsontable::hot_to_r(input$tbl5)
      # Update the reactive data object
    })
    
    shiny::observeEvent(input$save_sample, {
      save_and_validate(
        data_reactive = out_tab4$data_meta$tbl5,
        sheet_name = "sample",
        wb_reactive = out_tab3$WB_meta,
        temp_folder = out_tab1$temp_folder,
        update_validation = out_tab2$validation_results
      )
    })
    
  })
  
}