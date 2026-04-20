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
            shiny::actionButton(ns("save_site"), label = htmltools::tagList(bsicons::bs_icon("save"), "Save"), class = "btn-primary")
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
#' @param out_tab1 A reactive object containing the dataset name and observation file.
#' @param out_tab2 A reactive object containing the metadata file and validation results.
#' @param out_tab3 A reactive object containing the workbook reference and column configurations.
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
mod_tab4_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================
    # WORKBOOK (CTX ONLY)
    # =========================================================
    WB <- reactive({
      req(ctx$files$wb_meta)
      ctx$files$wb_meta
    })
    
    # =========================================================
    # STATE
    # =========================================================
    tbl4_data <- reactiveVal(NULL)
    
    # =========================================================
    # INITIAL LOAD
    # =========================================================
    observe({
      req(WB())
      
      df <- crud_load_excel(
        wb = WB(),
        sheet = "Xylo_obs_data",   # adjust if needed
        skip_rows = ctx$config$skip_rows_excel
      )
      
      tbl4_data(df)
      ctx$data$tbl4 <- df
    })
    
    # =========================================================
    # RENDER
    # =========================================================
    output$tbl4 <- rhandsontable::renderRHandsontable({
      req(tbl4_data())
      
      rhandsontable::rhandsontable(
        tbl4_data(),
        rowHeaders = NULL,
        contextMenu = TRUE,
        stretchH = "all",
        selectCallback = TRUE
      )
    })
    
    # =========================================================
    # SYNC
    # =========================================================
    observeEvent(input$tbl4, {
      req(input$tbl4)
      
      df <- rhandsontable::hot_to_r(input$tbl4)
      
      tbl4_data(df)
      ctx$data$tbl4 <- df
    })
    
    # =========================================================
    # SAVE
    # =========================================================
    observeEvent(input$save_tab4, {
      
      save_and_validate(
        data_reactive = tbl4_data(),
        sheet_name = "Xylo_obs_data",   # ⚠️ confirm this
        wb_reactive = ctx$files$wb_meta,
        temp_folder = ctx$files$temp_folder,
        update_validation = ctx$validation$results
      )
    })
    
  })
}
