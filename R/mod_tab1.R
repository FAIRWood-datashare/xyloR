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
mod_tab1_server <- function(id, ctx, parent_session) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =========================================================
    # 1. LIVE VALIDATION STATE (THIS IS THE ONLY SOURCE OF TRUTH)
    # =========================================================
    validation <- reactive({
      
      name <- input$dataset_name %||% ""
      version <- suppressWarnings(as.numeric(input$version))
      desc <- input$description %||% ""
      
      name_ok <- nzchar(name) &&
        nchar(name) >= 3 &&
        nchar(name) <= 8 &&
        grepl("^[A-Z0-9]+$", name)
      
      version_ok <- !is.na(version) && version >= 1 && version <= 99
      
      desc_ok <- nzchar(trimws(desc)) &&
        nchar(trimws(desc)) >= 50
      
      list(
        ok = name_ok && version_ok && desc_ok,
        name_ok = name_ok,
        version_ok = version_ok,
        desc_ok = desc_ok
      )
    })
    
    # =========================================================
    # 2. LIVE HEADER COLOR (NO CLICK NEEDED)
    # =========================================================
    observe({
      
      if (validation()$ok) {
        shinyjs::removeClass("card_header1_1", "bg-danger")
        shinyjs::addClass("card_header1_1", "bg-success")
      } else {
        shinyjs::removeClass("card_header1_1", "bg-success")
        shinyjs::addClass("card_header1_1", "bg-danger")
      }
      
    })
    
    # =========================================================
    # 3. SHOW NEXT CARDS WHEN VALID (AUTOMATIC)
    # =========================================================
    observe({
      
      req(input$dataset_name, input$version, input$description)
      
      if (validation()$ok) {
        
        shinyjs::show("card_1")
        shinyjs::show("card_2")
        
        ctx$state$tab1_ready <- TRUE
        
      } else {
        
        ctx$state$tab1_ready <- FALSE
      }
      
    })
    
    # =========================================================
    # 4. SUBMIT BUTTON (ONLY FINAL CONFIRMATION, NOT STATE DRIVER)
    # =========================================================
    observeEvent(input$submit, {
      
      message("🧪 VALIDATE CLICKED")
      
      if (validation()$ok) {
        
        message("✅ Validation passed")
        
        ctx$state$tab1_valid <- TRUE
        ctx$state$tab1_ready <- TRUE
        
        shinyjs::show("card_1")
        shinyjs::show("card_2")
        
      } else {
        
        message("❌ Validation failed")
        
        ctx$state$tab1_valid <- FALSE
        ctx$state$tab1_ready <- FALSE
      }
    })
    
    # =========================================================
    # 5. FILE UPLOAD (FIXED STATE TRIGGER)
    # =========================================================
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      message("📥 TAB1 file received")
      
      ctx$files$obs_file <- input$obs_file
      
      ctx$data$obs <- tryCatch({
        load_xylo_obs_clean(input$obs_file$datapath)
      }, error = function(e) NULL)
      
      ctx$state$obs_ready <- !is.null(ctx$data$obs)
      
      if (ctx$state$obs_ready) {
        
        shinyjs::addClass("card_header1_3", "bg-success")
        shinyjs::show("card_1_4")
      }
      
    })
    
    # =========================================================
    # 6. NEXT BUTTON (SINGLE SOURCE LOGIC)
    # =========================================================
    observe({
      
      ready <- isTRUE(ctx$state$tab1_valid) &&
        isTRUE(ctx$state$obs_ready)
      
      if (ready) {
        shinyjs::enable("next_btn")
      } else {
        shinyjs::disable("next_btn")
      }
      
    })
    
    observeEvent(input$next_btn, {
      
      req(ctx$state$tab1_valid, ctx$state$obs_ready)
      
      ctx$state$tab1_done <- TRUE
      
      bslib::nav_select(
        id = "tabs",
        selected = "tab2",
        session = parent_session
      )
    })
    
    # =========================================================
    # 7. LIVE VALIDATION TEXT (FIX MISSING RENDER)
    # =========================================================
    output$validation_status <- renderText({
      
      v <- validation()
      
      paste0(
        "Name: ", ifelse(v$name_ok, "OK", "❌"),
        " | Version: ", ifelse(v$version_ok, "OK", "❌"),
        " | Description: ", ifelse(v$desc_ok, "OK", "❌")
      )
      
    })
    
  })
}



