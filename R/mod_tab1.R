#' mod_tab1 UI Function
#'
#' @description A shiny module for the "Upload Observation" tab.
#'
#' Guides users through:
#' 1. Naming the dataset (name, version, description),
#' 2. Downloading a blank template or filled example,
#' 3. Uploading the filled observation .xlsx,
#' 4. Reviewing the Leaflet map, data-coverage plot, and info table,
#' 5. Ticking validation checkboxes before proceeding to Tab 2.
#'
#' @param id Module namespace identifier.
#' @return A `shiny.tag` (fluidRow) with the full tab 1 UI.
#' @export
#'
#' @import shiny
#' @importFrom bslib card card_header card_body tooltip
#' @importFrom bsicons bs_icon
#' @importFrom leaflet leafletOutput
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @importFrom shinyjs runjs
#' 
mod_tab1_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Upload observation",
    value = "tab1",
    
    shiny::fluidRow(
      
      # LEFT PANEL
      shiny::column(
        3,
        class = "bg-light p-2 border-end",
        
        bslib::card(
          bslib::card_header("1.1 Upload observation file"),
          bslib::card_body(
            shiny::fileInput(ns("obs_file"), "Observation Excel (.xlsx)"),
            shiny::textOutput(ns("upload_status"))
          )
        ),
        
        bslib::card(
          bslib::card_header("1.2 Sanity check"),
          bslib::card_body(
            DT::DTOutput(ns("sanity_table")),
            shiny::uiOutput(ns("sanity_message"))
          )
        ),
        
        bslib::card(
          bslib::card_body(
            shiny::actionButton(
              ns("next_btn"),
              "Continue →",
              class = "btn btn-primary w-100"
            )
          )
        )
      ),
      
      # RIGHT PANEL (preview)
      shiny::column(
        9,
        bslib::card(
          bslib::card_header("Observation preview"),
          bslib::card_body(
            DT::DTOutput(ns("obs_preview"))
          )
        )
      )
    )
  )
}


#' mod_tab1 Server Function
#'
#' @description Server logic for the "Upload Observation" tab module.
#'
#' Handles:
#' - Dataset name / version / description validation,
#' - Template and example downloads,
#' - File upload, `ctx` population, and site-filter update,
#' - Leaflet map, data-coverage (Plotly), key-info table, repetition table,
#' - Checkbox-based UI validation gate,
#' - FSM trigger on "Continue".
#'
#' @param id Module namespace identifier.
#' @param ctx Shared reactive-values context object.
#' @param session Top-level Shiny session (passed by the app server).
#' @return No return value; called for side effects.
#' @export
#'
#' @import shiny shinyjs dplyr tibble
#' @importFrom leaflet renderLeaflet leaflet addTiles setView addMarkers leafletOptions
#' @importFrom plotly renderPlotly plot_ly layout
#' @importFrom DT renderDataTable datatable
mod_tab1_server <- function(id, ctx, session) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      ctx$files$obs_file <- input$obs_file
      
      obs_raw <- tryCatch(
        openxlsx::readWorkbook(
          input$obs_file$datapath,
          sheet = "Xylo_obs_data",
          startRow = 1
        )[-(1:6), ] |> tibble::as_tibble(),
        error = function(e) {
          shiny::showNotification(e$message, type = "error")
          NULL
        }
      )
      
      req(!is.null(obs_raw))
      
      ctx$data$obs_truth <- obs_raw
      ctx$data$draft_obs <- obs_raw
      
      message("TAB1: file loaded")
    })
    
    sanity_ok <- reactive({
      
      req(ctx$data$obs_truth)
      df <- ctx$data$obs_truth
      
      all(
        nrow(df) > 0,
        "site_label" %in% names(df),
        !any(is.na(df$site_label))
      )
    })
    
    observe({
      ctx$signals$tab1_done <- sanity_ok()
      message("TAB1 signal:", ctx$signals$tab1_done)
    })
    
    output$sanity_table <- DT::renderDT({
      req(ctx$data$obs_truth)
      DT::datatable(ctx$data$obs_truth)
    })
    
    output$sanity_message <- shiny::renderUI({
      if (isTRUE(sanity_ok())) "✔ OK" else "✖ ERROR"
    })
    
    invisible(NULL)
  })
}
