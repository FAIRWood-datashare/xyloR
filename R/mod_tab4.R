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
#' 
mod_tab4_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Export",
    value = "tab4",
    
    fluidRow(
      
      # =====================================================
      # LEFT PANEL (CONTROL)
      # =====================================================
      column(
        4,
        
        bslib::card(
          bslib::card_header("4.1 Final validation"),
          
          bslib::card_body(
            
            uiOutput(ns("final_status")),
            
            tags$hr(),
            
            actionButton(
              ns("run_final_check"),
              "Run final check",
              class = "btn btn-primary w-100"
            )
          )
        ),
        
        bslib::card(
          bslib::card_header("4.2 Export"),
          
          bslib::card_body(
            
            uiOutput(ns("export_state")),
            
            downloadButton(
              ns("download_zip"),
              "Download package",
              class = "btn btn-success w-100"
            )
          )
        )
      ),
      
      # =====================================================
      # RIGHT PANEL (SUMMARY)
      # =====================================================
      column(
        8,
        
        bslib::card(
          bslib::card_header("Dataset summary"),
          bslib::card_body(
            DT::DTOutput(ns("summary_table"))
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
    # 🧠 SINGLE SOURCE OF TRUTH (EXPORT GATE)
    # =====================================================
    export_ready <- reactive({
      
      isTRUE(ctx$v2$dataset_valid) &&
        isTRUE(ctx$v2$obs_uploaded) &&
        isTRUE(ctx$v2$qa_ready) &&
        isTRUE(ctx$v2$meta_ready)
    })
    
    # =====================================================
    # 🧠 SYNC V2 EXPORT FLAG (ONLY HERE)
    # =====================================================
    observe({
      ctx$v2$export_ready <- export_ready()
    })
    
    # =====================================================
    # 📊 FINAL STATUS UI
    # =====================================================
    output$final_status <- renderUI({
      
      if (export_ready()) {
        tags$div(class = "alert alert-success", "✔ Dataset ready for export")
      } else {
        tags$div(class = "alert alert-danger", "✖ Dataset incomplete")
      }
    })
    
    # =====================================================
    # 📦 EXPORT STATE UI
    # =====================================================
    output$export_state <- renderUI({
      
      if (export_ready()) {
        tags$div(class = "alert alert-success", "Export unlocked")
      } else {
        tags$div(class = "alert alert-secondary", "Export locked")
      }
    })
    
    # =====================================================
    # 📋 SUMMARY (READ ONLY V2)
    # =====================================================
    output$summary_table <- DT::renderDT({
      
      data.frame(
        step = c("Dataset", "Observation", "QA", "Metadata", "Export"),
        status = c(
          ctx$v2$dataset_valid,
          ctx$v2$obs_uploaded,
          ctx$v2$qa_ready,
          ctx$v2$meta_ready,
          ctx$v2$export_ready
        )
      )
    })
    
    # =====================================================
    # 📦 DOWNLOAD HANDLER (GUARDED)
    # =====================================================
    output$download_zip <- downloadHandler(
      
      filename = function() {
        paste0("xylo_export_", Sys.Date(), ".zip")
      },
      
      content = function(file) {
        
        req(export_ready())
        
        tmp <- tempdir()
        
        saveRDS(ctx$data$obs_truth, file.path(tmp, "obs.rds"))
        saveRDS(ctx$data$meta, file.path(tmp, "meta.rds"))
        
        zip::zipr(file, files = c(
          file.path(tmp, "obs.rds"),
          file.path(tmp, "meta.rds")
        ))
      }
    )
    
    # =====================================================
    # 🧪 FINAL CHECK (OPTIONAL DEBUG ONLY)
    # =====================================================
    observeEvent(input$run_final_check, {
      
      if (export_ready()) {
        showNotification("All checks passed", type = "message")
      } else {
        showNotification("Missing required steps", type = "error")
      }
    })
    
  })
}
