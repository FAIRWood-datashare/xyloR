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
      
      column(
        6,
        
        bslib::card(
          bslib::card_header("Metadata Editor"),
          
          bslib::card_body(
            fileInput(ns("meta_upload"), "Upload metadata Excel (optional)"),
            tags$hr(),
            uiOutput(ns("meta_editor_ui")),
            tags$hr(),
            actionButton(ns("save_meta"), "Save metadata", class = "btn btn-primary w-100")
          )
        )
      ),
      
      column(
        6,
        
        bslib::card(
          bslib::card_header("System Validation Dashboard"),
          
          bslib::card_body(
            
            uiOutput(ns("meta_status")),
            tags$hr(),
            
            DT::dataTableOutput(ns("engine_debug")),
            tags$hr(),
            
            DT::dataTableOutput(ns("validation_obs_preview")),
            tags$hr(),
            
            verbatimTextOutput(ns("engine_probe")),
            
            tags$hr(),
            
            DT::dataTableOutput(ns("validation_table")),
            textOutput(ns("engine_status"))
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
    
    # =====================================================
    # METADATA SOURCE
    # =====================================================
    meta_base <- reactive({
      req(ctx$data$site_info)
      ctx$data$site_info
    })
    
    uploaded_meta <- reactive({
      req(input$meta_upload)
      readxl::read_excel(input$meta_upload$datapath)
    })
    
    meta_working <- reactiveVal()
    
    observe({
      if (!is.null(input$meta_upload)) {
        meta_working(uploaded_meta())
      } else {
        meta_working(meta_base())
      }
    })
    
    observeEvent(input$save_meta, {
      req(meta_working())
      ctx$data$meta$working_copy <- meta_working()
      ctx$state$meta_ready <- TRUE
    })
    
    # =====================================================
    # SAFE ENGINE ACCESS
    # =====================================================
    safe_engine <- reactive({
      req(ctx$engine_cache())
      ctx$engine()
    })
    
    output$engine_status <- renderText({
      if (is.null(ctx$engine_cache())) {
        "❌ Engine not ready (missing obs data)"
      } else {
        "✅ Engine ready"
      }
    })
    
    # =====================================================
    # 🔥 NORMALIZED VALIDATION TABLE (CORE FIX)
    # =====================================================
    output$validation_table <- DT::renderDataTable({
      
      eng <- safe_engine()
      
      df <- dplyr::bind_rows(
        normalize_validation(eng$validation$obs, "obs"),
        normalize_validation(eng$validation$site, "site"),
        normalize_validation(eng$validation$tree, "tree"),
        normalize_validation(eng$validation$sample, "sample")
      )
      
      if (nrow(df) == 0) {
        return(data.frame(message = "No validation issues"))
      }
      
      df
    })
    
    # =====================================================
    # ENGINE DEBUG VIEW
    # =====================================================
    output$engine_debug <- DT::renderDataTable({
      
      eng <- safe_engine()
      
      head(normalize_validation(eng$validation$obs, "obs"), 20)
    })
    
    # =====================================================
    # VALIDATION PREVIEW
    # =====================================================
    output$validation_obs_preview <- DT::renderDataTable({
      
      eng <- safe_engine()
      
      head(normalize_validation(eng$validation$obs, "obs"), 20)
    })
    
    # =====================================================
    # ENGINE PROBE (STRUCTURE CHECK)
    # =====================================================
    output$engine_probe <- renderPrint({
      
      eng <- safe_engine()
      
      list(
        obs = class(eng$validation$obs),
        site = class(eng$validation$site),
        tree = class(eng$validation$tree),
        sample = class(eng$validation$sample)
      )
    })
    
    # =====================================================
    # STATUS
    # =====================================================
    output$meta_status <- renderUI({
      
      v <- meta_working()
      
      if (is.null(v)) {
        return(tags$div(class = "alert alert-warning", "No metadata loaded"))
      }
      
      tags$div(class = "alert alert-success", "Metadata loaded")
    })
    
    # =====================================================
    # EDITOR PLACEHOLDER
    # =====================================================
    output$meta_editor_ui <- renderUI({
      tags$div("Metadata editor placeholder")
    })
  })
}
