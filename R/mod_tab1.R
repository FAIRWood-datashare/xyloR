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
    
    # =====================================================
    # LEFT COLUMN (FORMS)
    # =====================================================
    shiny::column(
      3,
      class = "bg-light p-2 border-end",
      
      # =====================================================
      # CARD 1.1 — DATASET SETUP
      # =====================================================
      
      tags$div(
        tags$small("Step 1 — Dataset setup", class = "text-muted"),
        tags$hr(style = "margin: 6px 0;")
      ),
      
      bslib::card(
        
        div(
          id = ns("card_header1_1"),
          class = "card-header bg-danger py-1",
          "1.1 Dataset"
        ),
        
        bslib::card_body(
          class = "py-2",
          
          shiny::textInput(ns("dataset_name"), "Name"),
          shiny::numericInput(ns("version"), "Version", value = 1, min = 1, max = 99),
          shiny::textAreaInput(ns("description"), "Description"),
          
          shiny::actionButton(
            ns("submit"),
            "Validate",
            class = "btn btn-primary w-100"
          )
        )
      ),
      
      # =====================================================
      # CARD 1.2 — DOWNLOAD (OPTIONAL)
      # =====================================================
      
      tags$div(
        tags$small("Step 2 — Data preparation", class = "text-muted"),
        tags$hr(style = "margin: 6px 0;")
      ),
      
      bslib::card(
        id = ns("card_1"),
        style = "display:none;",
        
        bslib::card_header(
          "Templates (optional)",
          id = ns("card_header1_2"),
          class = "bg-warning py-1",
          
          bslib::tooltip(
            bsicons::bs_icon("question-circle"),
            "Optional: download a blank template or example file.",
            placement = "right"
          )
        ),
        
        bslib::card_body(
          class = "py-2",
          
          shiny::fluidRow(
            
            shiny::column(
              6,
              tags$div(
                
                shiny::downloadButton(
                  ns("download_template"),
                  "Blank",
                  class = "btn btn-primary w-100"
                ),
                
                tags$small("Your input file", class = "text-muted")
              )
            ),
            
            shiny::column(
              6,
              tags$div(
                
                shiny::downloadButton(
                  ns("download_example_obs"),
                  "Example",
                  class = "btn btn-secondary w-100"
                ),
                
                tags$small("Reference format", class = "text-muted")
              )
            )
          )
        )
      ),
      
      # =====================================================
      # CARD 1.3 — UPLOAD
      # =====================================================
      
      bslib::card(
        id = ns("card_2"),
        style = "display:none;",
        
        bslib::card_header(
          id = ns("card_header2"),
          class = "card-header bg-danger py-1",
          "1.3 Upload"
        ),
        
        bslib::card_body(
          class = "py-2",
          
          shiny::fileInput(ns("obs_file"), "File"),
          shiny::selectInput(ns("site_filter"), "Site", choices = NULL)
        )
      ),
      
      # =====================================================
      # CARD 1.4 — VALIDATION
      # =====================================================
      
      tags$div(
        tags$small("Step 3 — Validation & review", class = "text-muted"),
        tags$hr(style = "margin: 6px 0;")
      ),
      
      bslib::card(
        id = ns("card_1_4"),
        style = "display:none;",
        
        bslib::card_header(
          id = ns("card_header1_4"),
          class = "card-header bg-danger py-1",
          "1.4 Validate"
        ),
        
        bslib::card_body(
          class = "py-2",
          
          shiny::checkboxInput(ns("validate_location"), "Location"),
          shiny::checkboxInput(ns("validate_data_coverage"), "Coverage"),
          shiny::checkboxInput(ns("validate_observation"), "Observation"),
          
          shiny::actionButton(
            ns("next_btn"),
            "Continue",
            class = "btn btn-primary w-100"
          )
        )
      )
    ),
    
    # =====================================================
    # RIGHT COLUMN (OUTPUTS)
    # =====================================================
    shiny::column(
      9,
      
      bslib::card(
        leaflet::leafletOutput(ns("mymap"), height = "360px")
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
    # 1. VALIDATION
    # =====================================================
    observe({
      
      valid <-
        nzchar(input$dataset_name %||% "") &&
        nchar(input$dataset_name %||% "") >= 3 &&
        nchar(input$dataset_name %||% "") <= 8 &&
        grepl("^[A-Z0-9]+$", input$dataset_name %||% "") &&
        !is.na(suppressWarnings(as.numeric(input$version))) &&
        suppressWarnings(as.numeric(input$version)) >= 1 &&
        suppressWarnings(as.numeric(input$version)) <= 99 &&
        nzchar(trimws(input$description %||% "")) &&
        nchar(trimws(input$description %||% "")) >= 50
      
      ctx$state$tab1$inputdata$valid <- valid
    })
    
    # =====================================================
    # 2. HEADER COLOR
    # =====================================================
    observe({
      if (isTRUE(ctx$state$tab1$inputdata$valid)) {
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('bg-danger').addClass('bg-success')",
          ns("card_header1_1")
        ))
      }
    })
    
    # =====================================================
    # 3. BUTTON ENABLE
    # =====================================================
    observe({
      shinyjs::toggleState(
        id = "submit",
        condition = isTRUE(ctx$state$tab1$inputdata$valid)
      )
    })
    
    # =====================================================
    # 4. SUBMIT (UI UNLOCK ONLY)
    # =====================================================
    observeEvent(input$submit, {
      
      req(ctx$state$tab1$inputdata$valid)
      
      ctx$state$tab1$inputdata$confirmed <- TRUE
      
      shinyjs::show("card_1")
      shinyjs::show("card_2")
    })
    
    # =====================================================
    # 5. FILE UPLOAD
    # =====================================================
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      ctx$files$obs_file <- input$obs_file
      ctx$state$tab1$file$uploaded <- TRUE
      
      shinyjs::show("card_1_4")
    }, ignoreInit = TRUE)
    
    # =====================================================
    # 6. VALIDATION CHECKBOXES
    # =====================================================
    observe({
      
      ctx$state$tab1$validation$all_valid <-
        isTRUE(input$validate_location) &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
    })
    
    # =====================================================
    # 7. HEADER VALIDATION
    # =====================================================
    observe({
      if (isTRUE(ctx$state$tab1$validation$all_valid)) {
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('bg-danger').addClass('bg-success')",
          ns("card_header1_4")
        ))
      }
    })
    
    # =====================================================
    # 8. NEXT BUTTON (ONLY SIGNAL STATE)
    # =====================================================
    observe({
      shinyjs::toggleState(
        id = "next_btn",
        condition = isTRUE(ctx$state$tab1$validation$all_valid)
      )
    })
    
    observeEvent(input$next_btn, {
      
      req(
        ctx$state$tab1$inputdata$valid,
        ctx$state$tab1$file$uploaded,
        ctx$state$tab1$validation$all_valid
      )
      
      # ONLY SET FLAG — NOTHING ELSE
      ctx$state$tab1$nav_ready <- TRUE
      
      ctx$fsm$events$go_next <- TRUE
      
      ctx$fsm_trigger(ctx$fsm_trigger() + 1)
    })
  })
}



