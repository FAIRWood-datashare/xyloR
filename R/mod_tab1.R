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
  
  load_obs_contract <- function(path) {
    raw <- read_xylo_obs_raw(path)
    build_xylo_obs_clean(raw)
  }
  
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
        
        bslib::card_header(
          "Map preview",
          class = "bg-light py-1"
        ),
        bslib::card_body(
          style = "height: 400px; padding: 0; overflow: hidden;",
          
          div(
            style = "height: 100%; width: 100%;",
            leaflet::leafletOutput(ns("mymap"), height = "100%")
          )
        )
      ),
      
      bslib::card(
        
        bslib::card_header(
          "Data coverage",
          class = "bg-light py-1"
        ),
        
        bslib::card_body(
          
          # -----------------------------------------------------
          # CONTROLS (MOVED OUT OF POPOVER)
          # -----------------------------------------------------
          shiny::selectInput(
            ns("color"),
            "Color by",
            choices = c("tree_species", "sample_id", "plot_label"),
            selected = "tree_species"
          ),
          
          # -----------------------------------------------------
          # PLOT
          # -----------------------------------------------------
          plotly::plotlyOutput(
            ns("data_coverage_plot"),
            height = "350px"
          )
        )
      ),
      
      bslib::card(
          bslib::card_header(
            "Info Table",
            class = "bg-light py-1"
          ),
          bslib::card_body(
            style = "height: 400px;",
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
    
    load_obs_contract <- function(path) {
      raw <- read_xylo_obs_raw(path)
      build_xylo_obs_clean(raw)
    }
    
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
    # 5a. FILE UPLOAD (ROBUST + CLEAN)
    # =====================================================
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      ctx$files$obs_file <- input$obs_file
      
      # -----------------------------------------------------
      # LOAD DATA
      # -----------------------------------------------------
      ctx$data$obs <- load_xylo_obs_clean(input$obs_file$datapath)
      ctx$data$site_info <- extract_site_info(input$obs_file$datapath)
      
      ctx$state$tab1$file$loaded <- TRUE
      
      # -----------------------------------------------------
      # INIT SITE FILTER
      # -----------------------------------------------------
      sites <- unique(trimws(as.character(ctx$data$site_info$site_label)))
      
      updateSelectInput(
        session,
        "site_filter",
        choices = sites,
        selected = sites[1]
      )
      
      # -----------------------------------------------------
      # UI STATE
      # -----------------------------------------------------
      ctx$state$tab1$file$uploaded <- TRUE
      
      shinyjs::removeClass("card_header2", "bg-danger")
      shinyjs::addClass("card_header2", "bg-success")
      
      shinyjs::show("card_1_4")
    }, ignoreInit = TRUE)
    
    
    # =====================================================
    # 5b. SITE INFO REACTIVE
    # =====================================================
    site_info <- reactive({
      req(ctx$data$site_info)
      ctx$data$site_info
    })
    
    # =====================================================
    # 5c. MAP RENDERING
    # =====================================================
    output$mymap <- leaflet::renderLeaflet({
      
      req(ctx$state$tab1$file$loaded)
      req(input$site_filter)
      
      si <- ctx$data$site_info
      
      si$site_label <- trimws(as.character(si$site_label))
      sel <- trimws(as.character(input$site_filter))
      
      si <- si[si$site_label == sel, , drop = FALSE]
      
      validate(
        need(nrow(si) > 0, "No site found"),
        need(!is.na(si$latitude[1]), "Missing lat"),
        need(!is.na(si$longitude[1]), "Missing lon")
      )
      
      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) %>%
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
    
    # =========================================================
    # 5d. DATA COVERAGE PLOT
    # =========================================================
    output$data_coverage_plot <- plotly::renderPlotly({
      
      req(ctx$state$tab1$file$loaded)
      req(input$site_filter, input$color)
      
      df <- ctx$data$obs
      df <- df[df$site_label == input$site_filter, , drop = FALSE]
      
      # safety: prevent crash if column not ready
      validate(
        need(input$color %in% names(df), "Invalid color column"),
        need("sample_date" %in% names(df), "Missing sample_date"),
        need("tree_label" %in% names(df), "Missing tree_label")
      )
      
      plotly::plot_ly(
        df,
        x = ~sample_date,
        y = ~tree_label,
        color = as.factor(df[[input$color]]),
        type = "scatter",
        mode = "markers",
        text = ~paste(
          "Tree:", tree_label,
          "<br>Date:", sample_date,
          "<br>", input$color, ":", df[[input$color]]
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          plot_bgcolor = "#2e2e2e",
          paper_bgcolor = "#2e2e2e",
          font = list(color = "white")
        )
    })
    
    # =========================================================
    # 5e. KEY_INFO_TABLE 
    # =========================================================
    output$key_info_table <- DT::renderDataTable({
      
      ctx$state$tab1$file$loaded
      
      df <- ctx$data$obs
      req(nrow(df) > 0)
      req(site_info(), input$site_filter)
      
      si <- site_info() %>%
        dplyr::filter(site_label == input$site_filter)
      
      validate(
        need(nrow(si) > 0, "No site selected"),
        need(!is.na(si$latitude[1]), "Missing lat"),
        need(!is.na(si$longitude[1]), "Missing lon")
      )
      
      key_info <- tibble::tibble(
        "Site" = si$site_label[1],
        "Coordinates" = paste(
          "Lat =", round(as.numeric(si$latitude[1]), 4),
          "Long =", round(as.numeric(si$longitude[1]), 4)
        ),
        "Elevation" = si$elevation[1],
        "Network" = paste(unique(df$network_label), collapse = ", "),
        "Date From" = format(min(df$sample_date), "%Y-%m-%d"),
        "Date To"   = format(max(df$sample_date), "%Y-%m-%d"),
        "n_Trees"   = length(unique(df$tree_label))
      ) %>%
        t() %>%
        setNames("Key Info")
      
      DT::datatable(key_info, options = list(dom = "t"), class = "table-dark")
    })
    
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



