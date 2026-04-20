#' mod_tab3 UI Function
#'
#' @description A shiny module for the "Observation" tab.
#'
#' This module provides a user interface for managing the observation data.
#' It consists of:
#' 1. An action button for saving observations,
#' 2. A basic information section that displays a table of site data,
#' 3. A detailed observation table with data fields such as sample date, species, and labels,
#' 4. Real-time syncing of data with user input for tables,
#' 5. Handling data formatting and species synchronization.
#'
#' @param id A string that serves as the module namespace identifier.
#'
#' @return A `shiny.tag.list` containing the UI elements of the module.
#' 
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' 
mod_tab3_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Observations",
    value = "tab3",
    
    shiny::fluidRow(
      
      # LEFT ACTION PANEL
      shiny::column(
        2,
        class = "bg-light p-2 border-end",
        
        bslib::card(
          bslib::card_body(
            shiny::actionButton(
              ns("save_obs"),
              label = htmltools::tagList(bsicons::bs_icon("save"), "Save"),
              class = "btn-primary"
            )
          )
        )
      ),
      
      # MAIN PANEL
      shiny::column(
        10,
        
        bslib::card(
          bslib::card_header("Site Info"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl_site"))
          )
        ),
        
        bslib::card(
          bslib::card_header("Observations"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl_obs"))
          )
        )
      )
    )
  )
}

#' mod_tab3 Server Function
#'
#' @description Server logic for the "Observation" tab module.
#'
#' Handles:
#' - Observations data loading and synchronization,
#' - Saving observation data to a workbook,
#' - Table rendering with synchronization and real-time updates,
#' - Data validation, and managing input data transformations.
#'
#' @param id A string that serves as the module namespace identifier.
#' @param out_tab1 A reactive object containing the dataset name and observation file.
#' @param out_tab2 A reactive object containing the metadata file and validation results.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @import shiny 
#' @importFrom shinyjs enable disable
#' @importFrom DT renderDataTable datatable
#' @importFrom plotly plotlyOutput
#' @importFrom openxlsx loadWorkbook saveWorkbook 
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' 
mod_tab3_server <- function(id, ctx, session) {
  
  moduleServer(id, function(input, output, session) {
    
    message("🟢 TAB3 — FSM CLEAN VERSION")
    
    ns <- session$ns
    
    # =====================================================
    # 1. WORKBOOK LOADERS (OBS + META)
    # =====================================================
    WB <- reactive({
      req(ctx$files$obs_file)
      openxlsx::loadWorkbook(ctx$files$obs_file$datapath)
    })
    
    WB_meta <- reactive({
      req(ctx$files$meta_file)
      openxlsx::loadWorkbook(ctx$files$meta_file$datapath)
    })
    
    # =====================================================
    # 2. DATA INITIALIZATION
    # =====================================================
    data_in <- reactiveValues(
      tbl1 = NULL,
      tbl2 = NULL
    )
    
    observe({
      
      req(WB())
      
      data_in$tbl1 <- openxlsx::readWorkbook(WB(), sheet = "obs_data_info", startRow = 6)
      data_in$tbl2 <- openxlsx::readWorkbook(WB(), sheet = "Xylo_obs_data", startRow = 1)
    })
    
    # =====================================================
    # 3. DROP LIST / SPECIES MAP
    # =====================================================
    drop_species <- reactive({
      
      req(WB())
      
      openxlsx::readWorkbook(WB(), sheet = "DropList") |>
        dplyr::filter(!is.na(tree_species))
    })
    
    # =====================================================
    # 4. TABLE RENDERING
    # =====================================================
    output$tbl1 <- rhandsontable::renderRHandsontable({
      
      req(data_in$tbl1)
      
      rhandsontable::rhandsontable(data_in$tbl1)
    })
    
    output$tbl2 <- rhandsontable::renderRHandsontable({
      
      req(data_in$tbl2)
      
      rhandsontable::rhandsontable(data_in$tbl2)
    })
    
    # =====================================================
    # 5. TABLE SYNC (USER INPUT)
    # =====================================================
    observeEvent(input$tbl1, {
      req(input$tbl1)
      data_in$tbl1 <- rhandsontable::hot_to_r(input$tbl1)
    })
    
    observeEvent(input$tbl2, {
      
      req(input$tbl2)
      
      df <- rhandsontable::hot_to_r(input$tbl2)
      
      data_in$tbl2 <- df
    })
    
    # =====================================================
    # 6. SAVE LOGIC (NO NAVIGATION HERE)
    # =====================================================
    observeEvent(input$save_obs, {
      
      req(data_in$tbl1, data_in$tbl2)
      
      save_and_validate(
        data_reactive = data_in$tbl1,
        sheet_name = "obs_data_info",
        wb_reactive = WB,
        temp_folder = ctx$files$temp_folder
      )
      
      save_and_validate(
        data_reactive = data_in$tbl2,
        sheet_name = "Xylo_obs_data",
        wb_reactive = WB,
        temp_folder = ctx$files$temp_folder
      )
      
      message("💾 TAB3 SAVE COMPLETE")
    })
    
    # =====================================================
    # 7. FSM COMPLETION FLAG (OPTIONAL)
    # =====================================================
    observe({
      
      ctx$fsm$flags$tab3_complete <-
        !is.null(data_in$tbl1) &&
        !is.null(data_in$tbl2)
    })
    
  })
}

