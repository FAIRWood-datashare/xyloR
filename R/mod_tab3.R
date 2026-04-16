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
mod_tab3_server <- function(id, ctx) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================
    # WORKBOOK (CTX ONLY)
    # =========================================================
    WB <- reactive({
      req(ctx$files$wb_meta)
      ctx$files$wb_meta
    })
    
    # =========================================================
    # CRUD STATE (LOCAL CACHE)
    # =========================================================
    tbl3_info <- reactiveVal(NULL)
    tbl3_obs  <- reactiveVal(NULL)
    
    # =========================================================
    # LOAD INFO TABLE
    # =========================================================
    observe({
      req(WB())
      
      site_info <- crud_load_excel(
        wb = WB(),
        sheet = "obs_data_info",
        skip_rows = ctx$config$skip_rows_excel
      )
      
      obs_data <- crud_load_excel(
        wb = WB(),
        sheet = "Xylo_obs_data",
        skip_rows = ctx$config$skip_rows_excel
      )
      
      if (ncol(site_info) == 3) {
        site_label <- obs_data$site_label |>
          unique() |>
          tibble::tibble() |>
          dplyr::filter(!is.na(.))
        
        site_info <- cbind(site_label, site_info)
        
      } else if (ncol(site_info) != 4) {
        stop("Invalid obs_data_info format")
      }
      
      site_info <- setNames(
        site_info,
        c("site_label", "latitude", "longitude", "elevation")
      )
      
      site_info$elevation <- as.integer(site_info$elevation)
      
      tbl3_info(site_info)
      ctx$data$tbl3_info <- site_info
    })
    
    # =========================================================
    # DROP LIST
    # =========================================================
    drop_species <- reactive({
      req(WB())
      
      openxlsx::readWorkbook(WB(), sheet = "DropList") |>
        dplyr::select(tree_species, species_code) |>
        dplyr::filter(!is.na(tree_species))
    })
    
    # =========================================================
    # OBS DATA
    # =========================================================
    observe({
      req(WB())
      
      df <- crud_load_excel(
        wb = WB(),
        sheet = "Xylo_obs_data",
        skip_rows = ctx$config$skip_rows_excel
      ) |>
        dplyr::left_join(drop_species(), by = "tree_species") |>
        dplyr::relocate(species_code, .after = tree_species) |>
        dplyr::mutate(
          sample_date = as.character(
            as.Date(as.numeric(sample_date), origin = "1899-12-30")
          )
        )
      
      tbl3_obs(df)
      ctx$data$tbl3_obs <- df
    })
    
    # =========================================================
    # RENDER TABLES
    # =========================================================
    output$tbl1 <- rhandsontable::renderRHandsontable({
      req(tbl3_info())
      
      rhandsontable::rhandsontable(
        tbl3_info(),
        rowHeaders = NULL,
        contextMenu = TRUE,
        stretchH = "all"
      ) |>
        apply_rules_to_table("tbl1", rules)
    })
    
    output$tbl2 <- rhandsontable::renderRHandsontable({
      req(tbl3_obs())
      
      rhandsontable::rhandsontable(
        tbl3_obs(),
        rowHeaders = NULL,
        contextMenu = TRUE,
        stretchH = "all",
        height = "300px"
      ) |>
        apply_rules_to_table("sample", rules)
    })
    
    # =========================================================
    # SYNC EDITS
    # =========================================================
    observeEvent(input$tbl1, {
      req(input$tbl1)
      
      df <- rhandsontable::hot_to_r(input$tbl1)
      tbl3_info(df)
      ctx$data$tbl3_info <- df
    })
    
    observeEvent(input$tbl2, {
      req(input$tbl2)
      
      df <- rhandsontable::hot_to_r(input$tbl2) |>
        sync_species_itrdb(drop_species())
      
      tbl3_obs(df)
      ctx$data$tbl3_obs <- df
    })
    
    # =========================================================
    # SAVE
    # =========================================================
    observeEvent(input$save_obs, {
      
      save_and_validate(
        data_reactive = tbl3_info(),
        sheet_name = "obs_data_info",
        wb_reactive = ctx$files$wb_meta,
        temp_folder = ctx$files$temp_folder,
        update_validation = ctx$validation$results
      )
      
      save_and_validate(
        data_reactive = tbl3_obs() |> dplyr::select(-species_code),
        sheet_name = "Xylo_obs_data",
        wb_reactive = ctx$files$wb_meta,
        temp_folder = ctx$files$temp_folder,
        update_validation = ctx$validation$results
      )
    })
    
  })
}
