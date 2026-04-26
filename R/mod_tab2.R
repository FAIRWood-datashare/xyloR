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
    title = "Metadata",
    value = "tab2",
    
    shiny::fluidRow(
      
      # LEFT PANEL
      shiny::column(
        3,
        class = "bg-light p-2 border-end",
        
        bslib::card(
          bslib::card_header("2.1 Template"),
          bslib::card_body(
            shiny::downloadButton(ns("download_meta_template"), "Template"),
            shiny::downloadButton(ns("download_example_meta"), "Example")
          )
        ),
        
        bslib::card(
          bslib::card_header("2.2 Upload"),
          bslib::card_body(
            shiny::fileInput(ns("meta_file"), NULL, accept = ".xlsx"),
            shiny::textOutput(ns("meta_status"))
          )
        ),
        
        shiny::div(
          id = ns("validation_card"),
          style = "display:none;",
          
          bslib::card(
            bslib::card_header("Validation"),
            bslib::card_body(
              DT::DTOutput(ns("validation_table")),
              shiny::uiOutput(ns("validation_message")),
              shiny::actionButton(ns("next_btn"), "Continue →")
            )
          )
        )
      ),
      
      # RIGHT PANEL
      shiny::column(
        9,
        bslib::card(
          bslib::card_header("Structure"),
          plotly::plotlyOutput(ns("hierarchy"))
        ),
        
        bslib::card(
          bslib::card_header("Metadata"),
          DT::DTOutput(ns("meta_table"))
        )
      )
    ),
    
    shiny::div(
      id = ns("zip_card"),
      style = "display:none;",
      shiny::downloadButton(ns("download_zip"), "Download ZIP")
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
mod_tab2_server <- function(id, ctx, session) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$meta_file, {
      
      req(input$meta_file)
      
      meta <- tryCatch(
        read_xylo_meta_raw(input$meta_file$datapath) |>
          build_xylo_meta_clean(),
        error = function(e) {
          shiny::showNotification(e$message, type = "error")
          NULL
        }
      )
      
      req(!is.null(meta))
      
      ctx$data$meta <- meta
      message("TAB2: meta loaded")
    })
    
    validation_tbl <- reactive({
      
      req(ctx$data$obs_truth, ctx$data$meta)
      
      tryCatch(
        xylo_validation_engine(ctx$data$obs_truth, ctx$data$meta),
        error = function(e) {
          data.frame(type = "error", message = e$message)
        }
      )
    })
    
    is_valid <- reactive({
      df <- validation_tbl()
      is.data.frame(df) && nrow(df) == 0
    })
    
    observe({
      ctx$signals$tab2_done <- isTRUE(is_valid())
      message("TAB2 signal:", ctx$signals$tab2_done)
    })
    
    output$validation_table <- DT::renderDT({
      DT::datatable(validation_tbl())
    })
    
    output$validation_message <- shiny::renderUI({
      if (isTRUE(is_valid())) "✔ Valid" else "✖ Invalid"
    })
    
    invisible(NULL)
  })
}




