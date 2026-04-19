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
  
  shiny::tagList(
    
    shiny::fluidRow(
      
      # =========================================================
      # LEFT PANEL
      # =========================================================
      shiny::column(
        3, class = "bg-light p-2 border-end", style = "height: 100%;",
        
        bslib::card(
          bslib::card_header(
            "2.1 Download prefilled metadata template",
            id = ns("card_header2_1"),
            class = "bg-warning",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Download a prefilled Excel template.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::downloadButton(
                  ns("download_meta_template"),
                  "Download Metadata Template",
                  class = "btn btn-primary"
                )
              ),
              shiny::column(
                6,
                shiny::downloadButton(
                  ns("download_example_meta"),
                  "Download filled example",
                  class = "btn btn-secondary"
                )
              )
            )
          )
        ),
        
        bslib::card(
          bslib::card_header(
            "2.2 Load completed metadata for validation",
            id = ns("card_header2_2"),
            class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Upload your completed metadata Excel file.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::fileInput(ns("meta_file"), label = NULL, accept = c(".xlsx")),
            shiny::textOutput(ns("meta_validation_status")),
            shiny::verbatimTextOutput(ns("meta_validation_errors"))
          )
        ),
        
        # =========================
        # VALIDATION CARD
        # =========================
        bslib::card(
          id = ns("validation_card"),
          style = "display: none; margin-top: 10px;",
          
          bslib::card_header(
            "Validation Report",
            id = ns("card_header2_3"),
            class = "bg-danger"
          ),
          
          bslib::card_body(
            DT::DTOutput(ns("validation_table")),
            shiny::uiOutput(ns("validation_message"))
          )
        )
      ),
      
      # =========================================================
      # RIGHT PANEL
      # =========================================================
      shiny::column(
        9,
        bslib::card(
          bslib::card_header("Overview of data structure"),
          bslib::card_body(
            plotly::plotlyOutput(ns("hierarchical_structure"), height = "500px")
          ),
          bslib::card_body(
            DT::DTOutput(ns("meta_table"))
          )
        )
      )
    ),
    
    # =========================================================
    # VALIDATION + ZIP SECTION
    # =========================================================
    shiny::fluidRow(
      shiny::column(
        12,
        
        # =========================
        # ZIP CARD (THIS WAS MISSING)
        # =========================
        bslib::card(
          id = ns("zip_card"),
          style = "display: none; text-align: center;",
          
          bslib::card_body(
            shiny::downloadButton(
              ns("download_zip"),
              "2.3 Download Exchange Files as ZIP",
              class = "btn btn-primary"
            )
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
mod_tab2_server <- function(id, ctx, meta_template_r) {
  
  moduleServer(id, function(input, output, session) {
    
    message("🟢 TAB2 STABLE FIXED (CTX SINGLE SOURCE)")
    
    # =========================================================
    # 🔥 GATE: TAB1 MUST BE READY
    # =========================================================
    tab1_ready <- reactive({
      isTRUE(ctx$state$obs_ready) &&
        !is.null(ctx$data$obs$clean)
    })
    
    # =========================================================
    # OBS DATA (FIXED: NO FILE RE-READING)
    # =========================================================
    obs_data <- reactive({
      req(tab1_ready())
      ctx$data$obs$clean
    })
    
    # =========================================================
    # META FILE
    # =========================================================
    meta_data <- reactive({
      req(input$meta_file)
      build_xylo_meta_clean(
        read_xylo_meta_raw(input$meta_file$datapath)
      )
    })
    
    # =========================================================
    # VALIDATION
    # =========================================================
    validation_results <- reactive({
      
      req(tab1_ready(), input$meta_file)
      
      bind_rows(
        xylo_format_validation(ctx$files$obs_file$datapath),
        validate_metadata_pipeline(input$meta_file$datapath)
      )
    })
    
    # =========================================================
    # TEMPLATE DOWNLOAD
    # =========================================================
    output$download_meta_template <- downloadHandler(
      filename = function() {
        paste0(ctx$state$dataset_name, "_meta.xlsx")
      },
      content = function(file) {
        wb <- meta_template_r()
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # =========================================================
    # TEMPLATE CREATION
    # =========================================================
    observeEvent(ctx$files$obs_file, {
      
      req(ctx$files$obs_file)
      
      if (!is.null(ctx$files$meta_template)) return()
      
      template_path <- system.file(
        "extdata",
        "Datasetname_xylo_meta_yyyy-mm-dd.xlsx",
        package = "xyloR"
      )
      
      ctx$files$meta_template <- tryCatch({
        create_xylo_metadata(
          ctx$files$obs_file$datapath,
          template_path,
          destdir = ctx$files$temp_folder
        )
      }, error = function(e) {
        message("❌ meta error: ", e$message)
        NULL
      })
    })
    
    # =========================================================
    # UI STATE CONTROL
    # =========================================================
    observe({
      
      tbl <- validation_results()
      
      shinyjs::hide("validation_card")
      shinyjs::hide("zip_card")
      
      shinyjs::removeClass("card_header2_3", "bg-success")
      shinyjs::removeClass("card_header2_3", "bg-danger")
      
      if (!tab1_ready()) return()
      
      shinyjs::show("validation_card")
      
      if (nrow(tbl) == 0) {
        shinyjs::addClass("card_header2_3", "bg-success")
        shinyjs::show("zip_card")
      } else {
        shinyjs::addClass("card_header2_3", "bg-danger")
      }
    })
    
    # =========================================================
    # 📍 MAP (FIXED RENDER TRIGGER)
    # =========================================================
    output$mymap <- leaflet::renderLeaflet({
      
      req(obs_data())
      
      df <- obs_data()
      
      leaflet::leaflet(df) |>
        leaflet::addTiles()
    })
    
    # =========================================================
    # 📊 COVERAGE PLOT (FIXED TRIGGER)
    # =========================================================
    output$data_coverage_plot <- plotly::renderPlotly({
      
      req(obs_data())
      
      df <- obs_data()
      
      plotly::plot_ly(df, x = ~sample_date, type = "histogram")
    })
    
    # =========================================================
    # 📋 VALIDATION TABLE
    # =========================================================
    output$obs_table <- DT::renderDT({
      
      req(obs_data())
      
      df <- obs_data()
      
      DT::datatable(df)
    })
    
    # =========================================================
    # NEXT BUTTON (FIXED GATE)
    # =========================================================
    can_proceed <- reactive({
      isTRUE(ctx$state$obs_ready) &&
        nrow(validation_results()) == 0 &&
        !is.null(input$meta_file)
    })
    
    observe({
      if (can_proceed()) shinyjs::enable("next_btn")
      else shinyjs::disable("next_btn")
    })
    
    observeEvent(input$next_btn, {
      req(can_proceed())
      
      ctx$pipeline$tab2 <- "done"
      
      # bslib::nav_select(
      #   id = "tabs",
      #   selected = "tab3",
      #   session = session$parent
      # )
    })
    
  })
}



