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
    title = "Metadata",
    value = "tab4",
    
    fluidRow(
      
      # LEFT: metadata editor
      column(
        6,
        
        bslib::card(
          bslib::card_header("4.1 Site Metadata Editor"),
          
          bslib::card_body(
            
            fileInput(ns("meta_upload"), "Upload metadata Excel (optional)"),
            
            tags$hr(),
            
            uiOutput(ns("meta_editor_ui")),
            
            tags$hr(),
            
            actionButton(
              ns("save_meta"),
              "Save metadata working copy",
              class = "btn btn-primary w-100"
            )
          )
        )
      ),
      
      # RIGHT: validation + preview
      column(
        6,
        
        bslib::card(
          bslib::card_header("4.2 Metadata validation"),
          
          bslib::card_body(
            
            uiOutput(ns("meta_status")),
            
            tags$hr(),
            
            DT::DTOutput(ns("meta_preview"))
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
    # 📦 METADATA SOURCE (prefill from obs if available)
    # =====================================================
    meta_base <- reactive({
      
      req(ctx$data$site_info)
      
      ctx$data$site_info
    })
    
    # =====================================================
    # 📥 UPLOAD OVERRIDE (optional external Excel)
    # =====================================================
    uploaded_meta <- reactive({
      
      req(input$meta_upload)
      
      readxl::read_excel(input$meta_upload$datapath)
    })
    
    # =====================================================
    # 🧠 WORKING COPY (CORE STATE FOR TAB 4)
    # =====================================================
    meta_working <- reactiveVal()
    
    observe({
      
      if (!is.null(input$meta_upload)) {
        meta_working(uploaded_meta())
      } else {
        meta_working(meta_base())
      }
    })
    
    # =====================================================
    # 💾 SAVE ACTION (locks into ctx)
    # =====================================================
    observeEvent(input$save_meta, {
      
      req(meta_working())
      
      ctx$data$meta$working_copy <- meta_working()
      
      ctx$state$meta_ready <- TRUE
    })
    
    # =====================================================
    # 🧪 METADATA VALIDATION (LOCAL ONLY)
    # =====================================================
    meta_validation <- reactive({
      
      req(meta_working())
      
      df <- meta_working()
      
      list(
        complete = !any(is.na(df)),
        n_sites = nrow(df)
      )
    })
    
    # push into global validation layer
    observe({
      
      ctx$validation$meta_initial_report <- meta_validation()
    })
    
    # =====================================================
    # 📊 STATUS UI
    # =====================================================
    output$meta_status <- renderUI({
      
      v <- meta_validation()
      
      if (isTRUE(v$complete)) {
        tags$div(class = "alert alert-success", "✔ Metadata complete")
      } else {
        tags$div(class = "alert alert-warning", "⚠ Missing metadata fields")
      }
    })
    
    # =====================================================
    # 📋 PREVIEW TABLE
    # =====================================================
    output$meta_preview <- DT::renderDT({
      
      req(meta_working())
      
      meta_working()
    })
    
    # =====================================================
    # 🧩 EDITOR SLOT (future handsontable plug-in)
    # =====================================================
    output$meta_editor_ui <- renderUI({
      
      tags$div(
        class = "text-muted",
        "Metadata editor will be inserted here (Handsontable or form UI)"
      )
    })
  })
}
