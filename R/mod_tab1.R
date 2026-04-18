#' mod_tab1 UI Function
#'
#' @description A shiny module for the "Upload Observation" tab.
#'
#' This module provides a user interface to guide users through:
#' 1. Naming their dataset,
#' 2. Downloading a data template or example,
#' 3. Uploading their filled observation data file,
#' 4. Validating uploaded data using checkboxes,
#' 5. Displaying interactive visualizations such as a Leaflet map,
#'    Plotly-based data coverage plots, and summary tables.
#'
#' @param id A string that serves as the module namespace identifier.
#'
#' @return A `shiny.tag.list` containing the UI elements of the module.
#' @export
#'
#' @import shiny 
#' @importFrom bslib tooltip popover nav_panel card card_header card_body
#' @importFrom bsicons bs_icon
#' @importFrom leaflet leafletOutput
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @importFrom shinyjs addClass removeClass show runjs
#' @importFrom openxlsx loadWorkbook saveWorkbook readWorkbook
#' @importFrom rhandsontable rHandsontableOutput
#' 
mod_tab1_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    
    shiny::column(
      3,
      class = "bg-light p-2 border-end",
      
      bslib::card(
        
        bslib::card_header(
          "1.1 Dataset",
          id = ns("card_header1_1"),
          class = "bg-danger"
        ),
        
        bslib::card_body(
          
          shiny::textInput(ns("dataset_name"), "Name"),
          shiny::numericInput(ns("version"), "Version", 1),
          shiny::textAreaInput(ns("description"), "Description"),
          
          shiny::textOutput(ns("validation_status")),
          
          shiny::actionButton(ns("submit"), "Validate")
        )
      ),
      
      bslib::card(
        id = ns("card_1"),
        style = "display:none;",
        bslib::card_header("Template"),
        bslib::card_body(shiny::downloadButton(ns("download_template")))
      ),
      
      bslib::card(
        id = ns("card_2"),
        style = "display:none;",
        bslib::card_header("Upload"),
        bslib::card_body(
          shiny::fileInput(ns("obs_file"), NULL),
          shiny::selectInput(ns("site_filter"), "Site", choices = NULL)
        )
      ),
      
      bslib::card(
        id = ns("card_1_4"),
        style = "display:none;",
        
        bslib::card_header("Validate"),
        bslib::card_body(
          
          shiny::checkboxInput(ns("validate_location"), "Location"),
          shiny::checkboxInput(ns("validate_data_coverage"), "Coverage"),
          shiny::checkboxInput(ns("validate_observation"), "Observation"),
          
          shiny::actionButton(ns("next_btn"), "Next")
        )
      )
    ),
    
    shiny::column(
      9,
      
      bslib::card(
        leaflet::leafletOutput(ns("mymap"), height = "400px")
      ),
      
      bslib::card(
        DT::DTOutput(ns("key_info_table"))
      )
    )
  )
}


#' mod_tab1 Server Function
#'
#' @description Server logic for the "Upload Observation" tab module.
#'
#' Handles:
#' - Dataset name validation,
#' - Template download and example data,
#' - File upload and site filtering,
#' - Rendering of map, summary tables, and plotly charts,
#' - Checkbox-based validation to proceed.
#'
#' @param id A string that serves as the module namespace identifier.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @import shiny shinyjs openxlsx dplyr tibble
#' @importFrom shiny moduleServer observeEvent observe reactive req showModal modalDialog updateSelectInput
#' @importFrom shinyjs addClass removeClass show runjs
#' @importFrom openxlsx loadWorkbook saveWorkbook readWorkbook
#' @importFrom dplyr tibble filter
mod_tab1_server <- function(id, ctx, session) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 1. PURE VALIDATION (NO SIDE EFFECTS)
    # =====================================================
    validate_tab1 <- function() {
      
      name <- input$dataset_name %||% ""
      version <- suppressWarnings(as.numeric(input$version))
      desc <- input$description %||% ""
      
      list(
        ok = nzchar(name) &&
          nchar(name) >= 3 &&
          nchar(name) <= 8 &&
          grepl("^[A-Z0-9]+$", name) &&
          !is.na(version) &&
          version >= 1 && version <= 99 &&
          nzchar(trimws(desc)) &&
          nchar(trimws(desc)) >= 50
      )
    }
    
    # =====================================================
    # 2. VALIDATE BUTTON → STATE ONLY
    # =====================================================
    observeEvent(input$submit, {
      
      res <- validate_tab1()
      
      message("🧪 TAB1 VALIDATE → ", res$ok)
      
      ctx <- set_state(ctx, "tab1.metadata_valid", res$ok)
      ctx <- set_state(ctx, "tab1.ready_to_continue", res$ok)
      
    })
    
    # =====================================================
    # 3. FILE UPLOAD → STATE ONLY
    # =====================================================
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      message("📥 TAB1 FILE UPLOADED")
      
      ctx$files$obs_file <- input$obs_file
      
      ctx <- set_state(ctx, "tab1.file_uploaded", TRUE)
      ctx <- set_state(ctx, "tab1.obs_ready", TRUE)
      
    })
    
    # =====================================================
    # 4. UI RENDER ENGINE (ONLY ONE OBSERVER)
    # =====================================================
    observe({
      
      s <- ctx$state$tab1
      
      # --------------------------
      # HEADER 1.1
      # --------------------------
      if (isTRUE(s$metadata_valid)) {
        shinyjs::removeClass("card_header1_1", "bg-danger")
        shinyjs::addClass("card_header1_1", "bg-success")
      } else {
        shinyjs::removeClass("card_header1_1", "bg-success")
        shinyjs::addClass("card_header1_1", "bg-danger")
      }
      
      # --------------------------
      # TEMPLATE CARD
      # --------------------------
      if (isTRUE(s$metadata_valid)) {
        shinyjs::show("card_1")
      } else {
        shinyjs::hide("card_1")
      }
      
      # --------------------------
      # UPLOAD CARD
      # --------------------------
      if (isTRUE(s$metadata_valid)) {
        shinyjs::show("card_2")
      } else {
        shinyjs::hide("card_2")
      }
      
      # --------------------------
      # VALIDATION CARD (1.4)
      # --------------------------
      if (isTRUE(s$file_uploaded) && isTRUE(s$obs_ready)) {
        shinyjs::show("card_1_4")
      } else {
        shinyjs::hide("card_1_4")
      }
      
      # --------------------------
      # HEADER 1.3
      # --------------------------
      if (isTRUE(s$file_uploaded)) {
        shinyjs::removeClass("card_header1_3", "bg-danger")
        shinyjs::addClass("card_header1_3", "bg-success")
      }
      
    })
    
    # =====================================================
    # 5. NEXT BUTTON → STATE ONLY
    # =====================================================
    observeEvent(input$next_btn, {
      
      req(ctx$state$tab1$ready_to_continue)
      
      message("➡️ TAB1 COMPLETE")
      
      ctx <- set_state(ctx, "tab1.done", TRUE)
      
    })
    
  })
}



