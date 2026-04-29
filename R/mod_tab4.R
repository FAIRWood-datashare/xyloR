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
    
    meta_working <- reactiveVal(NULL)
    
    observe({
      
      if (!is.null(input$meta_upload)) {
        meta_working(uploaded_meta())
      } else {
        meta_working(meta_base())
      }
    })
    
    # =====================================================
    # SAVE METADATA (CORE FIX)
    # =====================================================
    observeEvent(input$save_meta, {
      
      req(meta_working())
      
      # 🔥 unified metadata target
      ctx$data$meta <- meta_working()
      
      # 🔥 remove legacy state flag usage
      # ctx$state$meta_ready <- TRUE  ❌ REMOVED
      
      # 🔥 trigger readiness update
      ctx$update_ready()
    })
    
    # =====================================================
    # SAFE ENGINE ACCESS (CLEANED)
    # =====================================================
    safe_engine <- reactive({
      
      eng <- ctx$engine()
      
      req(!is.null(eng))
      
      eng
    })
    
    # =====================================================
    # ENGINE STATUS
    # =====================================================
    output$engine_status <- renderText({
      
      if (is.null(ctx$snapshot())) {
        "❌ Engine not ready (missing data or metadata)"
      } else {
        "✅ Engine ready"
      }
    })
    
    # =====================================================
    # 🔥 NORMALIZED VALIDATION TABLE
    # =====================================================
    output$validation_table <- DT::renderDataTable({
      
      eng <- safe_engine()
      
      df <- dplyr::bind_rows(
        normalize_validation(eng$validation$obs, "obs"),
        normalize_validation(eng$validation$site, "site"),
        normalize_validation(eng$validation$tree, "tree"),
        normalize_validation(eng$validation$sample, "sample")
      )
      
      df
    })
    
    # =====================================================
    # ENGINE DEBUG VIEW
    # =====================================================
    output$engine_debug <- DT::renderDataTable({
      
      eng <- safe_engine()
      
      normalize_validation(eng$validation$obs, "obs")
    })
    
    # =====================================================
    # VALIDATION PREVIEW
    # =====================================================
    output$validation_obs_preview <- DT::renderDataTable({
      
      eng <- safe_engine()
      
      normalize_validation(eng$validation$obs, "obs")
    })
    
    # =====================================================
    # ENGINE PROBE
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
    # STATUS UI
    # =====================================================
    output$meta_status <- renderUI({
      
      v <- ctx$data$meta
      
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
