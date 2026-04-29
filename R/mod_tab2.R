#' mod_tab2 UI Function
#'
#' @description A shiny module for the "Metadata Management" tab.
#'
#' This module provides a user interface for managing metadata files and validating them:
#' 1. Uploading and validating metadata files,
#' 2. Downloading metadata templates or example data,
#' 3. Visualizing hierarchical metadata structures using a sunburst plot,
#' 4. Displaying validation messages and feedback.
#'
#' @param id A string that serves as the module namespace identifier.
#'
#' @return A `shiny.tag.list` containing the UI elements of the module.
#' @export
#'
#' @import shiny shinyjs plotly openxlsx
#' @importFrom shiny NS tagList fluidRow column div actionButton fileInput uiOutput
#' @importFrom plotly plotlyOutput
#' @importFrom openxlsx loadWorkbook saveWorkbook
mod_tab2_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Data ingestion",
    value = "tab2",
    
    shiny::fluidRow(
      
      shiny::column(
        12,
        
        bslib::card(
          bslib::card_header("2. Upload Observation Data"),
          bslib::card_body(
            
            fileInput(ns("obs_file"), "Upload observation file (.xlsx)"),
            
            br(),
            
            shiny::uiOutput(ns("obs_status"))
          )
        )
      )
    )
  )
}

#' mod_tab2 Server Function
#'
#' @description Server logic for the "Metadata Management" tab module.
#'
#' Handles:
#' - Uploading and validating metadata files,
#' - Providing metadata template download functionality,
#' - Rendering a sunburst plot to visualize hierarchical metadata structure,
#' - Displaying validation results and feedback messages based on metadata quality.
#'
#' @param id A string that serves as the module namespace identifier.
#' @param out_tab1 A reactive object containing the dataset name and observation file.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @import shiny
#' @importFrom shinyjs addClass removeClass show runjs toggleClass
#' @importFrom openxlsx loadWorkbook saveWorkbook readWorkbook
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter mutate select arrange group_by summarise distinct rename
#' @importFrom bsicons bs_icon
#' @importFrom htmltools div
#' @importFrom zip zipr
#' @importFrom DT renderDataTable datatable
#' @importFrom readxl read_excel excel_sheets
#' @importFrom lubridate year
#' @importFrom tibble tibble
#' 
mod_tab2_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    # =====================================================
    # 🟩 INGESTION PIPELINE (SAFE + ATOMIC)
    # =====================================================
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      # -----------------------------
      # 1. LOAD DATA (SAFE)
      # -----------------------------
      obs <- tryCatch(
        load_xylo_obs_clean_contract(input$obs_file$datapath),
        error = function(e) {
          shiny::showNotification(e$message, type = "error")
          NULL
        }
      )
      
      if (is.null(obs)) {
        ctx$v2$obs_uploaded    <- FALSE
        ctx$v2$ingestion_valid <- FALSE
        return()
      }
      
      # -----------------------------
      # 2. EXTRACT SITE INFO
      # -----------------------------
      site_info <- tryCatch(
        extract_site_info(input$obs_file$datapath),
        error = function(e) {
          shiny::showNotification(e$message, type = "error")
          NULL
        }
      )
      
      # -----------------------------
      # 3. VALIDATION (DERIVED)
      # -----------------------------
      ingestion_valid <- !is.null(obs)
      
      # =====================================================
      # 🧠 V2 STATE WRITE (ONLY PLACE)
      # =====================================================
      ctx$data$obs_truth <- obs
      ctx$data$site_info <- site_info
      ctx$files$obs_file <- input$obs_file
      
      ctx$v2$obs_uploaded    <- TRUE
      ctx$v2$ingestion_valid <- ingestion_valid
      
      # =====================================================
      # 🚀 NAVIGATION (ONLY ON SUCCESS)
      # =====================================================
      if (ingestion_valid) {
        ctx$v2$stage <- "tab3"
        
        shiny::showNotification(
          "Observation data uploaded successfully",
          type = "message"
        )
      }
    })
    
    # =====================================================
    # 🟨 STATUS UI (V2-ONLY READ)
    # =====================================================
    output$obs_status <- renderUI({
      
      if (isTRUE(ctx$v2$obs_uploaded)) {
        
        tags$div(
          class = "alert alert-success",
          "✔ Observation data uploaded"
        )
        
      } else {
        
        tags$div(
          class = "alert alert-secondary",
          "No file uploaded yet"
        )
      }
    })
    
  })
}