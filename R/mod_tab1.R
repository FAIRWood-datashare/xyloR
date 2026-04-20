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
      
      # =====================================================
      # CARD 1.1 METADATA
      # =====================================================
      bslib::card(
        
        div(
          id = ns("card_header1_1"),
          class = "card-header bg-danger",
          "1.1 Dataset"
        ),
        
        bslib::card_body(
          
          shiny::textInput(ns("dataset_name"), "Name"),
          shiny::numericInput(ns("version"), "Version", 1),
          shiny::textAreaInput(ns("description"), "Description"),
          
          shiny::actionButton(
            ns("submit"),
            "Validate",
            disabled = TRUE,
            class = "btn btn-primary"
          )
        )
      ),
      
      # CARD 1.2
      bslib::card(
        id = ns("card_1"),
        style = "display:none;",
        bslib::card_header("Template"),
        bslib::card_body(shiny::downloadButton(ns("download_template")))
      ),
      
      # CARD 1.3
      bslib::card(
        id = ns("card_2"),
        style = "display:none;",
        bslib::card_header("Upload"),
        bslib::card_body(
          shiny::fileInput(ns("obs_file"), NULL),
          shiny::selectInput(ns("site_filter"), "Site", choices = NULL)
        )
      ),
      
      # CARD 1.4
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
      bslib::card(leaflet::leafletOutput(ns("mymap"), height = "400px")),
      bslib::card(DT::DTOutput(ns("key_info_table")))
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
      
      name <- input$dataset_name %||% ""
      version <- suppressWarnings(as.numeric(input$version))
      desc <- input$description %||% ""
      
      valid <-
        nzchar(name) &&
        nchar(name) >= 3 &&
        nchar(name) <= 8 &&
        grepl("^[A-Z0-9]+$", name) &&
        !is.na(version) &&
        version >= 1 &&
        version <= 99 &&
        nzchar(trimws(desc)) &&
        nchar(trimws(desc)) >= 50
      
      ctx$state$tab1$inputdata$valid <- valid
      
      message("inputdata valid =", valid)
    })
    
    # =====================================================
    # 2. HEADER COLOR
    # =====================================================
    observe({
      
      valid <- isTRUE(ctx$state$tab1$inputdata$valid)
      
      header_id <- ns("card_header1_1")
      
      if (valid) {
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('bg-danger').addClass('bg-success')",
          header_id
        ))
      } else {
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('bg-success').addClass('bg-danger')",
          header_id
        ))
      }
    })
    
    # =====================================================
    # 3. ENABLE BUTTON
    # =====================================================
    observe({
      shinyjs::toggleState(
        id = "submit",
        condition = isTRUE(ctx$state$tab1$inputdata$valid)
      )
    })
    
    # =====================================================
    # 4. SUBMIT
    # =====================================================
    observeEvent(input$submit, {
      
      req(ctx$state$tab1$inputdata$valid)
      
      ctx$state$tab1$inputdata$confirmed <- TRUE
      
      shinyjs::show("card_1")
      shinyjs::show("card_2")
      
      message("CLICKED BUTTON")
    })
    
    # =====================================================
    # 5. FILE UPLOAD
    # =====================================================
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      ctx$files$obs_file <- input$obs_file
      ctx$state$tab1$file$uploaded <- TRUE
      
      shinyjs::show("card_1_4")
      
      message("📦 obs file stored")
      
    }, ignoreInit = TRUE)
    
    # =====================================================
    # 6. VALIDATION CHECKBOXES
    # =====================================================
    observe({
      
      valid <- 
        isTRUE(input$validate_location) &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
      
      ctx$state$tab1$validation$all_valid <- valid
    })
    
    # =====================================================
    # 7. FINAL DONE FLAG
    # =====================================================
    observe({
      
      ctx$state$tab1$done <-
        isTRUE(ctx$state$tab1$inputdata$valid) &&
        isTRUE(ctx$state$tab1$file$uploaded) &&
        isTRUE(ctx$state$tab1$validation$all_valid)
    })
    
    # =====================================================
    # 8. ENABLE NEXT BUTTON
    # =====================================================
    observe({
      
      shinyjs::toggleState(
        id = "next_btn",
        condition = isTRUE(ctx$state$tab1$done)
      )
    })
    
    # =====================================================
    # 9. NEXT BUTTON → NAVIGATION TRIGGER
    # =====================================================
    observeEvent(input$next_btn, {
      
      req(ctx$state$tab1$done)
      
      message("➡️ TAB1 COMPLETE")
      
      ctx$state$tab1$nav_ready <- TRUE
    })
    
  })
}



