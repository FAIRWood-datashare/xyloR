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
    
    # =====================================================
    # LOCAL REACTIVES
    # =====================================================
    
    zip_ready <- reactiveVal(FALSE)
    validation_tbl <- reactiveVal(NULL)
    meta_hierarchy <- reactiveVal(NULL)
    validation_trigger <- reactiveVal(0)
    
    # =====================================================
    # 1. TAB1 GATE
    # =====================================================
    
    tab1_ready <- reactive({
      isTRUE(ctx$state$tab1$nav_ready)
    })
    
    # =====================================================
    # 2. META FILE UPLOAD
    # =====================================================
    
    observeEvent(input$meta_file, {
      
      req(input$meta_file)
      
      ctx$files$meta_file <- input$meta_file
      ctx$state$tab2$file$uploaded <- TRUE
      
      message("META FILE UPLOADED: ", input$meta_file$name)
      
      # LOAD META VIA IO LAYER
      meta <- load_xylo_metadata_clean(input$meta_file$datapath)
      ctx$data$meta <- meta
      
      message("META LOADED | sheets: ", paste(names(meta), collapse = ", "))
      
      # TRIGGER VALIDATION ONCE
      validation_trigger(validation_trigger() + 1)
      
    }, ignoreInit = TRUE)
    
    # =====================================================
    # 3. HEADER STATE
    # =====================================================
    
    observe({
      if (isTRUE(ctx$state$tab2$file$uploaded)) {
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('bg-danger').addClass('bg-success')",
          ns("card_header2")
        ))
      }
    })
    
    # =====================================================
    # 4. VALIDATION ENGINE (SINGLE SOURCE OF TRUTH)
    # =====================================================
    
    observeEvent(validation_trigger(), {
      
      req(ctx$files$meta_file)
      req(ctx$files$obs_file)
      
      message("RUNNING VALIDATION...")
      
      tbl <- tryCatch({
        xylo_meta_validation(ctx$files$meta)
      }, error = function(e) {
        data.frame(
          type = "error",
          source = "validation",
          message = e$message
        )
      })
      
      # Debug
      print("VALIDATION TABLE")
      print(head(tbl))
      
      validation_tbl(tbl)
      ctx$data$tab2_validation <- tbl
      
      is_valid <- is.data.frame(tbl) && nrow(tbl) == 0
      
      ctx$state$tab2$validation$all_valid <- is_valid
      
      message("VALIDATION DONE | valid=", is_valid, " rows=", nrow(tbl))
      
    }, ignoreInit = TRUE)
    
    # =====================================================
    # 5. UI STATE CONTROL (VALIDATION + ZIP VISIBILITY)
    # =====================================================
    
    observe({
      
      req(ctx$state$tab2$file$uploaded)
      
      shinyjs::show("validation_card")
      
      tbl <- validation_tbl()
      
      if (is.null(tbl)) return()
      
      # ALWAYS SHOW VALIDATION RESULTS (important)
      output$validation_table <- renderTable({
        tbl
      })
      
      # VALID STATE
      if (isTRUE(ctx$state$tab2$validation$all_valid)) {
        
        shinyjs::show("zip_card")
        
      } else {
        
        shinyjs::hide("zip_card")
        
        # OPTIONAL: show next-step blocker card
        shinyjs::show("validation_blocker_card")
      }
    })
    
    output$validation_status <- renderText({
      
      tbl <- validation_tbl()
      
      if (is.null(tbl)) return("No validation yet")
      
      if (nrow(tbl) == 0) {
        "Validation passed"
      } else {
        paste("Validation failed:", nrow(tbl), "issue(s) detected")
      }
    })
    
    output$validation_table <- renderTable({
      req(validation_tbl())
      validation_tbl()
    })
    
    # =====================================================
    # 6. ZIP BUTTON STATE
    # =====================================================
    
    observe({
      shinyjs::toggleState(
        id = "download_zip",
        condition = isTRUE(zip_ready())
      )
    })
    
    # =====================================================
    # 7. NEXT BUTTON
    # =====================================================
    
    observe({
      shinyjs::toggleState(
        id = "next_btn",
        condition = isTRUE(ctx$state$tab2$validation$all_valid)
      )
    })
    
    observeEvent(input$next_btn, {
      
      req(ctx$state$tab2$validation$all_valid)
      
      ctx$state$tab2$nav_ready <- TRUE
      
      ctx$fsm$events$go_next <- TRUE
      ctx$fsm_trigger(ctx$fsm_trigger() + 1)
    })
    
    # =====================================================
    # 8. HIERARCHY BUILDER
    # =====================================================
    
    observe({
      
      req(meta_hierarchy())
      
      h <- build_xylo_hierarchy(meta_hierarchy())
      
      ctx$data$tab2_hierarchy <- h
      
      message("HIERARCHY BUILT | rows=", nrow(h))
    })
    
    # =====================================================
    # 9. DEBUG PANEL (VERY IMPORTANT)
    # =====================================================
    
    output$debug_tab2 <- renderPrint({
      
      list(
        uploaded = ctx$state$tab2$file$uploaded,
        meta_file = if (!is.null(ctx$files$meta_file)) ctx$files$meta_file$name else NULL,
        validation_rows = if (!is.null(validation_tbl())) nrow(validation_tbl()) else NULL,
        zip_ready = zip_ready(),
        validation_trigger = validation_trigger(),
        hierarchy_loaded = if (!is.null(meta_hierarchy())) names(meta_hierarchy()) else NULL,
        validation_state = ctx$state$tab2$validation$all_valid
      )
    })
    
    # =====================================================
    # EXPORTS (DEBUG / TESTING)
    # =====================================================
    
    return(list(
      validation = validation_tbl,
      zip_ready = zip_ready,
      hierarchy = meta_hierarchy
    ))
  })
}




