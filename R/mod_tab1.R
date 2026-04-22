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
    # LEFT COLUMN (CONTROL PANEL)
    # =====================================================
    shiny::column(
      3,
      class = "bg-light p-2 border-end d-flex flex-column",
      style = "height: calc(100vh - 120px); overflow-y: auto;",
      
      # -----------------------------------------------------
      # STEP 1 — METADATA COLLECTION
      # -----------------------------------------------------
      bslib::card(
        class = "mb-2",
        
        bslib::card_header(
          "Step 1 — Dataset metadata",
          id = ns("card_header1_1"),
          class = "bg-danger py-1"
        ),
        
        bslib::card_body(
          class = "py-2",
          
          shiny::textInput(
            ns("dataset_name"),
            "Dataset name",
            placeholder = "e.g. LTS2025"
          ),
          
          shiny::numericInput(
            ns("version"),
            "Version",
            value = 1, min = 1, max = 99
          ),
          
          shiny::textAreaInput(
            ns("description"),
            "Description",
            placeholder = "Minimum 50 characters..."
          ),
          
          shiny::actionButton(
            ns("submit"),
            "Validate metadata",
            class = "btn btn-primary w-100"
          )
        )
      ),
      
      # -----------------------------------------------------
      # STEP 2 — TEMPLATE DOWNLOAD
      # -----------------------------------------------------
      bslib::card(
        id = ns("card_1"),
        class = "mb-2",
        style = "display:none;",
        
        bslib::card_header(
          "Step 2 — Templates (optional)",
          id = ns("card_header1_2"),
          class = "bg-warning py-1"
        ),
        
        bslib::card_body(
          class = "py-2",
          
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::downloadButton(
                ns("download_template"),
                "Blank template",
                class = "btn btn-primary w-100"
              )
            ),
            shiny::column(
              6,
              shiny::downloadButton(
                ns("download_example_obs"),
                "Example file",
                class = "btn btn-secondary w-100"
              )
            )
          ),
          
          tags$small(
            "Download a template or example before uploading.",
            class = "text-muted"
          )
        )
      ),
      
      # -----------------------------------------------------
      # STEP 3 — UPLOAD (NOW MORE PROMINENT)
      # -----------------------------------------------------
      bslib::card(
        id = ns("card_2"),
        class = "mb-2",
        style = "display:none;",
        
        bslib::card_header(
          "Step 3 — Upload data",
          id = ns("card_header2"),
          class = "bg-danger py-1"
        ),
        
        bslib::card_body(
          class = "py-2",
          
          shiny::fileInput(
            ns("obs_file"),
            "Upload observation file",
            buttonLabel = "Browse..."
          ),
          
          shiny::selectInput(
            ns("site_filter"),
            "Select site",
            choices = NULL
          ),
          
          tags$small(
            "After upload, the map and plots will update automatically.",
            class = "text-muted"
          )
        )
      ),
      
      # -----------------------------------------------------
      # STEP 4 — VALIDATION
      # -----------------------------------------------------
      bslib::card(
        id = ns("card_1_4"),
        class = "mb-2",
        style = "display:none;",
        
        bslib::card_header(
          "Step 4 — Validate dataset",
          id = ns("card_header1_4"),
          class = "bg-danger py-1"
        ),
        
        bslib::card_body(
          class = "py-2",
          
          shiny::checkboxInput(ns("validate_location"), "Location"),
          shiny::checkboxInput(ns("validate_data_coverage"), "Coverage"),
          shiny::checkboxInput(ns("validate_observation"), "Observation"),
          
          shiny::actionButton(
            ns("next_btn"),
            "Continue",
            class = "btn btn-success w-100"
          )
        )
      )
    ),
    
    
    # =====================================================
    # RIGHT COLUMN (OUTPUTS)
    # =====================================================
    shiny::column(
      9,
      
      # -----------------------------------------------------
      # MAP
      # -----------------------------------------------------
      bslib::card(
        class = "mb-2",
        
        bslib::card_header("Map preview", class = "bg-light py-1"),
        
        bslib::card_body(
          style = "height: 350px; padding: 0;",
          
          leaflet::leafletOutput(ns("mymap"), height = "100%")
        )
      ),
      
      # -----------------------------------------------------
      # DATA COVERAGE
      # -----------------------------------------------------
      bslib::card(
        id = ns("card_data_coverage"),
        style = "display:none;",
        
        bslib::card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            
            span("Data coverage"),
            
            shiny::selectInput(
              ns("color"),
              NULL,
              choices = c("tree_species", "sample_id", "plot_label"),
              selected = "tree_species",
              width = "180px"
            )
          ),
          class = "bg-light py-1"
        ),
        
        bslib::card_body(
          plotly::plotlyOutput(ns("data_coverage_plot"), height = "350px")
        )
      ),
      
      # -----------------------------------------------------
      # KEY INFO TABLE
      # -----------------------------------------------------
      bslib::card(
        
        bslib::card_header("Dataset summary", class = "bg-light py-1"),
        
        bslib::card_body(
          DT::DTOutput(ns("key_info_table"))
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
    # 1. METADATA VALIDATION
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
      
      # 🎯 HEADER COLOR = GREEN WHEN VALID
      if (isTRUE(valid)) {
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('bg-danger').addClass('bg-success')",
          ns("card_header1_1")
        ))
      } else {
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('bg-success').addClass('bg-danger')",
          ns("card_header1_1")
        ))
      }
    })
    
    # =====================================================
    # 2. ENABLE SUBMIT BUTTON ONLY WHEN VALID
    # =====================================================
    observe({
      shinyjs::toggleState(
        id = ns("submit"),
        condition = isTRUE(ctx$state$tab1$inputdata$valid)
      )
    })
    
    # =====================================================
    # 3. USER-GATED CONFIRMATION
    # =====================================================
    observeEvent(input$submit, {
      
      req(ctx$state$tab1$inputdata$valid)
      
      # mark metadata as CONFIRMED (not just valid)
      ctx$state$tab1$inputdata$confirmed <- TRUE
      
      # unlock next UI steps
      shinyjs::show("card_1")
      shinyjs::show("card_2")
      
      # # visual feedback
      # shinyjs::runjs(sprintf(
      #   "$('#%s').removeClass('bg-danger').addClass('bg-success')",
      #   ns("card_header1_1")
      #))
    })
    
    # # =====================================================
    # # 4. DOWNLOAD ENABLE LOGIC (FIXED)
    # # =====================================================
    # observe({
    #   
    #   ready <- isTRUE(ctx$state$tab1$inputdata$confirmed)
    #   
    #   shinyjs::toggleState(
    #     id = ns("download_template"),
    #     condition = ready
    #   )
    #   
    #   shinyjs::toggleState(
    #     id = ns("download_example_obs"),
    #     condition = ready
    #   )
    # })
    
    # =====================================================
    # 5. FILE UPLOAD
    # =====================================================
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      ctx$files$obs_file <- input$obs_file
      
      ctx$data$obs <- load_xylo_obs_clean(input$obs_file$datapath)
      ctx$data$site_info <- extract_site_info(input$obs_file$datapath)
      
      ctx$state$tab1$file$loaded <- TRUE
      ctx$state$tab1$file$uploaded <- TRUE
      
      sites <- unique(trimws(as.character(ctx$data$site_info$site_label)))
      
      updateSelectInput(
        session,
        "site_filter",
        choices = sites,
        selected = sites[1]
      )
      
      shinyjs::removeClass("card_header2", "bg-danger")
      shinyjs::addClass("card_header2", "bg-success")
      
      shinyjs::show("card_1_4")
    }, ignoreInit = TRUE)
    
    # =====================================================
    # 6. CENTRAL DATA LAYER
    # =====================================================
    filtered_data <- reactive({
      
      req(ctx$state$tab1$file$loaded)
      req(input$site_filter)
      
      sel <- trimws(as.character(input$site_filter))
      
      si <- ctx$data$site_info
      si$site_label <- trimws(as.character(si$site_label))
      si_sel <- si[si$site_label == sel, , drop = FALSE]
      
      obs <- ctx$data$obs
      obs$site_label <- trimws(as.character(obs$site_label))
      obs_sel <- obs[obs$site_label == sel, , drop = FALSE]
      
      list(
        site = si_sel,
        obs = obs_sel
      )
    })
    
    # =====================================================
    # 7. MAP (UNCHANGED)
    # =====================================================
    output$mymap <- leaflet::renderLeaflet({
      
      req(filtered_data())
      
      si <- filtered_data()$site
      
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::setView(
          lng = as.numeric(si$longitude[1]),
          lat = as.numeric(si$latitude[1]),
          zoom = 11
        ) %>%
        leaflet::addMarkers(
          lng = as.numeric(si$longitude[1]),
          lat = as.numeric(si$latitude[1]),
          popup = si$site_label[1]
        )
    })
    
    # =====================================================
    # 8. DATA COVERAGE PLOT (FIXED SAFETY)
    # =====================================================
    output$data_coverage_plot <- plotly::renderPlotly({
      
      req(filtered_data(), input$color)
      
      df <- filtered_data()$obs
      
      validate(
        need(nrow(df) > 0, "No data"),
        need(input$color %in% names(df), "Invalid color column")
      )
      
      plotly::plot_ly(
        df,
        x = ~sample_date,
        y = ~tree_label,
        color = as.factor(df[[input$color]]),
        type = "scatter",
        mode = "markers"
      )
    })
  })
}



