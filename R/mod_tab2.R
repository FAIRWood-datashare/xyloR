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
            id = ns("card_header2"),
            class = "card-header bg-danger",
            "Upload meta_data",
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
mod_tab2_server <- function(id, ctx, session) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    message("🟢 TAB2 — Step 9 clean version")
    
    # =====================================================
    # 1. SAFETY INIT (ensure structure exists)
    # =====================================================
    observe({

      ctx$state$tab2$metadata   <- ctx$state$tab2$metadata   %||% list()
      ctx$state$tab2$file       <- ctx$state$tab2$file       %||% list()
      ctx$state$tab2$validation <- ctx$state$tab2$validation %||% list()
    })
    
    # =====================================================
    # 2. TAB1 GATE
    # =====================================================
    tab1_ready <- reactive({
      isTRUE(ctx$state$tab1_complete) &&
        !is.null(ctx$data$obs$clean)
    })
    
    # =====================================================
    # 3a. META FILE INFO UPLOAD → CTX
    # it includes name, size, type, and datapath (server path to the file)
    # =====================================================
    observeEvent(input$meta_file, {
      
      req(input$meta_file)
      
      ctx$files$meta_file <- input$meta_file
      ctx$state$tab2$file$uploaded <- TRUE
      
      message("📥 meta_file info stored in ctx")
    }, ignoreInit = TRUE)
    
    # =====================================================
    # 3b. HEADER COLOR
    # =====================================================
    observe({
      
      valid <- isTRUE(ctx$state$tab2$file$uploaded)
      
      if (valid) {
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('bg-danger').addClass('bg-success')",
          ns("card_header2_2")
        ))
      } 
    })
    
    
    # =====================================================
    # 4. LOAD META DATA (CLEAN)
    # =====================================================
    observe({
      
      req(ctx$files$meta_file)
      
      ctx$data$meta$clean <- tryCatch({
        load_xylo_metadata_clean(ctx$files$meta_file$datapath)
      }, error = function(e) {
        message("❌ meta load failed: ", e$message)
        NULL
      })
    })
    
    # =====================================================
    # 5. VALIDATION (OBS + META)
    # =====================================================
    observe({
      
      req(tab1_ready(), ctx$files$meta_file)
      
      tbl <- tryCatch({
        
        obs_val <- tryCatch({
          xylo_format_validation(ctx$files$obs_file$datapath)
        }, error = function(e) {
          data.frame(issue = "Observation validation failed")
        })
        
        meta_val <- tryCatch({
          meta_format_validation(ctx$files$meta_file$datapath)
        }, error = function(e) {
          data.frame(issue = "Metadata validation failed")
        })
        
        rbind(obs_val,meta_val)
        
      }, error = function(e) {
        data.frame(issue = "Validation crashed")
      })
      
      ctx$data$tab2_validation <- tbl
      
      # ✅ write to ctx state
      ctx$state$tab2$validation$all_valid <-
        is.data.frame(tbl) && nrow(tbl) == 0
    })
    
    # =====================================================
    # 6. UI VALIDATION CARD
    # =====================================================
    observe({
      
      req(tab1_ready())
      
      tbl <- ctx$data$tab2_validation
      
      shinyjs::hide("validation_card")
      shinyjs::hide("zip_card")
      
      if (is.null(tbl)) return()
      
      shinyjs::show("validation_card")
      
      if (nrow(tbl) == 0) {
        shinyjs::addClass("card_header2_3", "bg-success")
        shinyjs::show("zip_card")
      } else {
        shinyjs::addClass("card_header2_3", "bg-danger")
      }
    })
    
    # # =====================================================
    # # 7. VALIDATION TABLE
    # # =====================================================
    # output$validation_table <- DT::renderDT({
    #   
    #   tbl <- ctx$data$tab2_validation
    #   
    #   req(tbl, nrow(tbl) > 0)
    #   
    #   DT::datatable(tbl)
    # })
    # 
    # =====================================================
    # 8. NEXT BUTTON ENABLE
    # =====================================================
    observe({
      shinyjs::toggleState(
        "next_btn", 
        isTRUE(ctx$state$tab2$validation$all_valid) &&
          isTRUE(ctx$state$tab2$file$uploaded)
        )
    })
    
    # =====================================================
    # 8. NEXT BUTTON CLICK
    # =====================================================
    observeEvent(input$next_btn, {
      
      req(
        ctx$state$tab2$validation$all_valid,
        ctx$state$tab2$file$uploaded
      )
      
      ctx$fsm$flags$tab2_complete <- TRUE
      ctx$fsm$events$go_next <- TRUE
      
      ctx$fsm_trigger(ctx$fsm_trigger() + 1)
      
      message("➡️ TAB2 COMPLETE")
    })
    
  })
}




