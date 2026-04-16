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

  # ─── Panel wrapper ──────────────────────────────────────────────
  bslib::nav_panel(
    title = shiny::div(id = ns("upload_observation"), "1. Upload observation"),
    value = "Upload observation",
    shiny::fluidRow(
      # ─── Left column: Upload section ─────────────────────────────
      shiny::column(
        width = 3,
        class = "bg-light p-2 border-end",
        style = "height: 100%;",

        # 1.1 Dataset framing
        bslib::card(
          bslib::card_header(
            "1.1 Framing your dataset",
            id = ns("card_header1_1"), class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Provide a unique identifier for your dataset. This will be used to name the output files. Then move to 1.2 Download observation data template",
              placement = "right"
            )
          ),
          bslib::card_body(
            
            shiny::textInput(
              ns("dataset_name"), 
              "Enter the DATASET NAME. 
          This must be alphanumeric, in uppercase letters, and 3–8 characters long", 
              value = "", 
              width = "100%", 
              placeholder = "3-8 characters"
            ) %>% 
              shiny::tagAppendAttributes(maxlength = 8),
            
            # Add a horizontal line
            shiny::tags$hr(style = "border-top: 2px solid #ccc;"),
            
            shiny::numericInput(
              ns("version"),
              "Enter the DATASET VERSION. 
              This must be a number between 1 and 99.",
              value = 1,
              min = 1,
              max = 99,
              step = 1,
              width = "100%"
            ),
            
            shiny::p("Note: Creating a dataset with the same NAME but a higher VERSION (e.g., 2 after 1) will overwrite the file previously imported in the DataBase", style = "color:#ff8c00; font-style: italic;"),
            
            # Add a horizontal line
            shiny::tags$hr(style = "border-top: 2px solid #ccc;"),
            
            shiny::dateInput(
              ns("embargo"),
              "Enter the EMBARGO END DATE. 
              This should be maximally 10 years from today, by default set to today's date.",
              value = Sys.Date(),       # default to today
              min = Sys.Date(),       # optional lower bound
              max = Sys.Date() + 3650,   # optional upper bound
              format = "yyyy-mm-dd",    # display format
              width = "100%"
            ),
            
            # Add a horizontal line
            shiny::tags$hr(style = "border-top: 2px solid #ccc;"),
            
            shiny::textAreaInput(
              ns("description"),            # input ID
              label = shiny::div(
                "Enter a DATASET DESCRIPTION.",
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Describe what the dataset is about, its scope, and any key details relevant to understanding its content and purpose. 
                  HERE a potential template:
                  The dataset was collected between [start year] and [end year] as part of a study designed to investigate [main research question or objective]. The study included [number] sites and focused on [number] species of trees, selected according to [sampling or selection criteria].
              
              Data recorded include [measurements, observations, or variables collected, e.g., tree height, diameter, species identity, phenology], along with metadata about the sites such as [environmental conditions, location coordinates, etc.]. The study design ensured consistent sampling across all sites to allow comparison of [key parameters, e.g., growth rates, biodiversity, ecological trends].
              
              This dataset can be used for [research applications, modeling, or monitoring purposes], providing insights into [ecosystem processes, forest dynamics, or other study-specific focus].",
                  placement = "right"
                )
              ),
              value = "",                   # initial content
              width = "100%",               # full width
              height = "500px",             # enough height for 2-3 paragraphs
              placeholder = "The dataset ..."
            ),
            
            shiny::p("Note: click on the questinonmark for a description template. This should be at least 50 chracters long to be validated. this will be uded to provide a short description text on the database webpage", style = "color:#ff8c00; font-style: italic;"),
            
            shiny::actionButton(ns("submit"), "Validate", class = "btn btn-primary")
          )
          ),

        # 1.2 Template download
        bslib::card(
          bslib::card_header(
            "1.2 Download observation data template",
            id = ns("card_header1_2"), class = "bg-warning",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Click 'Download Template' to save an empty Excel template for observation data. Then move to 1.3 Upload.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::fluidRow(
              shiny::column(6, shiny::downloadButton(ns("download_template"), "Download Template", class = "btn btn-primary")),
              shiny::column(6, shiny::downloadButton(ns("download_example_obs"), "Download example", class = "btn btn-secondary"))
            )
          ),
          style = "display: none;",
          id = ns("card_1")
        ),

        # 1.3 Upload file
        bslib::card(
          card_header(
            "1.3 Upload the filled observation data file!",
            id = ns("card_header1_3"), class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Upload your filled Excel file. A map and summary tables will appear.",
              placement = "right"
            )
          ),
          shiny::fileInput(ns("obs_file"), NULL, accept = c(".xlsx")),
          shiny::selectInput(ns("site_filter"), "Select Site", choices = NULL),
          style = "height: 300px; display: none;",
          id = ns("card_2")
        ),

        # Key information table
        bslib::card(
          bslib::card_header("Key Information Table"),
          bslib::card_body(DT::DTOutput(ns("key_info_table"))),
          style = "display: none; min-height: 600px; overflow: visible;",
          id = ns("card_3")
        ),

        # 1.4 Validate checkboxes
        bslib::card(
          bslib::card_header(
            "1.4 Validate your data",
            id = ns("card_header1_4"), class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Check all boxes to enable Next.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::checkboxInput(ns("validate_location"), "Validate Location", value = FALSE),
            shiny::checkboxInput(ns("validate_data_coverage"), "Validate Data Coverage", value = FALSE),
            shiny::checkboxInput(ns("validate_observation"), "Validate Observation list", value = FALSE),
            shiny::textOutput(ns("validation_status")),
            shiny::actionButton(ns("next_btn"), "Next", icon = shiny::icon("angle-double-right"), class = "btn btn-primary")
          ),
          style = "display: none;",
          id = ns("card_7")
        )
      ),

      # ─── Right column: Map, plotly, tables ──────────────────────
      shiny::column(
        width = 9,
        style = "height:100%;",

        # Geolocation Map
        bslib::card(
          bslib::card_header("Geolocation Map"),
          bslib::card_body(leaflet::leafletOutput(ns("mymap"), height = "400px")),
          style = "display: none;",
          id = ns("card_4")
        ),

        # Data Coverage Plot
        bslib::card(
          bslib::card_header(
            "Data Coverage Overview",
            bslib::popover(
              bsicons::bs_icon("gear", class = "ms-auto"),
              shiny::selectInput(ns("color"), "Color by", choices = c("tree_species", "sample_id", "plot_label")),
              title = "Plot Settings"
            )
          ),
          bslib::card_body(plotly::plotlyOutput(ns("data_coverage_plot"), height = "300px")),
          style = "display: none;",
          id = ns("card_5")
        ),

        # Observations table
        bslib::card(
          bslib::card_header("average repetition per sample and measure_type"),
          bslib::card_body(DT::DTOutput(ns("obs_table"))),
          style = "display: none;",
          id = ns("card_6")
        )
      )
    ),
    shiny::br()
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
mod_tab1_server <- function(id, ctx) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      cat("\n🔥 TAB1 DEBUG STATE 🔥\n")
      print(ctx$state)
      print(ctx$files)
    })
    
    # =========================================================
    # METADATA (SAFE UPDATE)
    # =========================================================
    observeEvent(input$dataset_name, {
      ctx$state$dataset_name <- input$dataset_name
    })
    
    observeEvent(input$version, {
      ctx$state$version <- input$version
    })
    
    observeEvent(input$description, {
      ctx$state$description <- input$description
    })
    
    observeEvent(input$embargo, {
      ctx$state$embargo <- input$embargo
    })
    
    observeEvent(input$contact_lastname, {
      ctx$state$contact_lastname <- input$contact_lastname
    })
    
    # =========================================================
    # TEMP FOLDER (SINGLE SOURCE OF TRUTH)
    # =========================================================
    observe({
      req(input$dataset_name)
      
      tmp <- file.path(tempdir(), input$dataset_name)
      if (!dir.exists(tmp)) dir.create(tmp, recursive = TRUE)
      
      ctx$files$temp_folder <- tmp
    })
    
    # =========================================================
    # OBS FILE
    # =========================================================
    observeEvent(input$obs_file, {
      req(input$obs_file)
      
      req(ctx$files$temp_folder)
      
      file_path <- file.path(
        ctx$files$temp_folder,
        input$obs_file$name
      )
      
      file.copy(input$obs_file$datapath, file_path, overwrite = TRUE)
      
      ctx$files$obs_file <- list(
        datapath = file_path,
        name = input$obs_file$name
      )
    })
    
  })
}
