#' Site Metadata Tab UI
#'
#' @param id The module ID.
#' @return A nav_panel UI for the Site tab.
#' 
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' 
#' Site Metadata Tab UI
#'
#' @param id The module ID.
#' @return A nav_panel UI for the Site tab.
#' 
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' 
mod_tab4_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Metadata",
    value = "tab4",
    
    fluidRow(
      
      column(
        3,
        
        bslib::card(
          bslib::card_header("Metadata mode"),
          bslib::card_body(
            radioButtons(
              ns("mode"),
              "Choose mode",
              choices = c("Upload file" = "upload",
                          "In-app editor" = "in_app")
            ),
            
            br(),
            
            fileInput(ns("meta_file"), "Upload metadata (optional)")
          )
        ),
        
        bslib::card(
          bslib::card_header("Validation"),
          bslib::card_body(
            uiOutput(ns("validation_message")),
            actionButton(ns("continue"), "Continue →", class = "btn btn-primary")
          )
        )
      ),
      
      column(
        9,
        
        bslib::card(
          bslib::card_header("Metadata preview"),
          bslib::card_body(
            DT::DTOutput(ns("meta_table"))
          )
        )
      )
    )
  )
}

#' Server logic for Site Metadata Tab
#'
#' @param id The module ID.
#' @param ctx The centralized app context (environment).
#' @param session The parent Shiny session.
#'
#' @return A list with data_meta for downstream tabs.
#'
#' @import shiny
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @importFrom dplyr select mutate filter left_join ends_with
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r
#'
mod_tab4_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # MODE SELECTION (V2 ONLY)
    # =====================================================
    observeEvent(input$mode, {
      ctx$v2$meta_mode <- input$mode
    })
    
    # =====================================================
    # METADATA INGESTION (ONLY IF UPLOAD MODE)
    # =====================================================
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
      ctx$v2$meta_ready <- TRUE
    })
    
    # =====================================================
    # SIMPLE VALIDATION (V2 ONLY)
    # =====================================================
    meta_valid <- reactive({
      !is.null(ctx$data$meta) && nrow(ctx$data$meta) > 0
    })
    
    observe({
      ctx$v2$meta_valid <- meta_valid()
    })
    
    # =====================================================
    # TABLE
    # =====================================================
    output$meta_table <- DT::renderDT({
      
      req(ctx$data$meta)
      
      DT::datatable(ctx$data$meta)
    })
    
    # =====================================================
    # MESSAGE
    # =====================================================
    output$validation_message <- renderUI({
      
      if (isTRUE(meta_valid())) {
        tags$div("✔ Metadata OK", style="color:green")
      } else {
        tags$div("✖ Missing metadata", style="color:red")
      }
    })
    
    # =====================================================
    # CONTINUE → NEXT STATE
    # =====================================================
    observeEvent(input$continue, {
      
      if (!meta_valid()) {
        showNotification("Fix metadata first", type = "error")
        return()
      }
      
      ctx$v2$meta_ready <- TRUE
      ctx$v2$stage <- "DONE"
    })
    
  })
}
