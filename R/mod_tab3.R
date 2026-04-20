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
  
  # TAB 3: Observation -----------------------------------------------
  bslib::nav_panel(
    title = htmltools::div(id = ns("observation_tab"), "Observations"),
    value = "Observations",
    
    shiny::fluidRow(
      # Left side (sidebar) - Action Button and Info Section
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_header(NULL),
          bslib::card_body(
            shiny::actionButton(ns("save_obs"), label = htmltools::tagList(bsicons::bs_icon("save"), "Save"), class = "btn-primary")
          )
        )
      ),
      
      # Right side - Main Content with Observation Table
      shiny::column(
        11, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Basic Info"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl1")),
            #shiny::verbatimTextOutput(ns("testing"))
          )
        ),
        
        bslib::card(
          bslib::card_header("Observation table:"),
          bslib::card_body(
            rhandsontable:: rHandsontableOutput(ns("tbl2")),
            #shiny::verbatimTextOutput(ns("testing1")),
            #shiny::verbatimTextOutput(ns("testing2"))
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
    
    message("🟢 TAB3 FIXED")
    
    # =========================================================
    # OBS DATA (ONLY SOURCE OF TRUTH)
    # =========================================================
    obs_data <- reactive({
      req(ctx$data$obs$clean)
      ctx$data$obs$clean
    })
    
    # =========================================================
    # DEBUG
    # =========================================================
    observe({
      
      req(obs_data())
      
      df <- obs_data()
      
      message("\n🔥 OBS_DATA DEBUG (TAB3)")
      message("dim: ", nrow(df), " x ", ncol(df))
      print(head(df, 10))
      message("=========================\n")
    })
    
    # =========================================================
    # META
    # =========================================================
    meta_path <- reactive({
      req(input$meta_file)
      input$meta_file$datapath
    })
    
    meta_data <- reactive({
      req(meta_path())
      build_xylo_meta_clean(read_xylo_meta_raw(meta_path()))
    })
    
    site_info <- reactive({
      req(meta_data())
      meta_data()$site
    })
    
    # =========================================================
    # DROPLIST
    # =========================================================
    drop_species <- reactive({
      req(ctx$files$obs_file)
      
      raw <- read_xylo_obs_raw(ctx$files$obs_file$datapath)
      
      raw$droplist |>
        dplyr::select(tree_species, species_code) |>
        dplyr::filter(!is.na(tree_species))
    })
    
    # =========================================================
    # STATE
    # =========================================================
    tbl3_info <- reactiveVal()
    tbl3_obs  <- reactiveVal()
    
    observe({
      req(site_info(), obs_data())
      
      tbl3_info(site_info())
      tbl3_obs(obs_data())
    })
    
    # =========================================================
    # SYNC
    # =========================================================
    sync_species_itrdb <- function(df, drop_species) {
      
      df |>
        dplyr::left_join(drop_species, by = "species_code") |>
        dplyr::mutate(
          tree_species = dplyr::coalesce(tree_species_from_list, tree_species)
        ) |>
        dplyr::select(-dplyr::any_of("tree_species_from_list"))
    }
    
    # =========================================================
    # TABLES
    # =========================================================
    output$tbl1 <- rhandsontable::renderRHandsontable({
      req(tbl3_info())
      rhandsontable::rhandsontable(tbl3_info(), rowHeaders = NULL)
    })
    
    output$tbl2 <- rhandsontable::renderRHandsontable({
      req(tbl3_obs())
      rhandsontable::rhandsontable(tbl3_obs(), rowHeaders = NULL, height = 300)
    })
    
    # =========================================================
    # UPDATE
    # =========================================================
    observeEvent(input$tbl1, {
      tbl3_info(rhandsontable::hot_to_r(input$tbl1))
    })
    
    observeEvent(input$tbl2, {
      df <- rhandsontable::hot_to_r(input$tbl2)
      tbl3_obs(sync_species_itrdb(df, drop_species()))
    })
    
    # =========================================================
    # SAVE
    # =========================================================
    observeEvent(input$save_obs, {
      
      req(tbl3_info(), tbl3_obs(), ctx$files$obs_file)
      
      wb <- openxlsx::loadWorkbook(ctx$files$obs_file$datapath)
      
      save_and_validate(
        data_reactive = tbl3_info(),
        sheet_name = "obs_data_info",
        wb_reactive = function() wb,
        ctx = ctx
      )
      
      save_and_validate(
        data_reactive = tbl3_obs(),
        sheet_name = "Xylo_obs_data",
        wb_reactive = function() wb,
        ctx = ctx
      )
    })
    
  })
}

