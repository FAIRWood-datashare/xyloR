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
    # 📦 METADATA SOURCE (UNCHANGED LOGIC)
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
    # 💾 SAVE METADATA (SSOT SAFE)
    # =====================================================
    observeEvent(input$save_meta, {
      
      req(meta_working())
      
      ctx$data$meta <- meta_working()
      
      ctx$invalidate_engine()
      
      ctx$update_ready()
    })
    
    # =====================================================
    # ⚙️ ENGINE STATUS (SAFE DISPLAY ONLY)
    # =====================================================
    output$engine_status <- renderText({
      
      if (!ctx$has_engine()) {
        "❌ Engine not ready (run update_ready / ingestion)"
      } else {
        "✅ Engine ready"
      }
    })
    
    # =====================================================
    # 🧪 DEBUG (SAFE INSPECTION ONLY)
    # =====================================================
    observe({
      print(str(ctx$data$obs))
      print(str(ctx$data$meta))
      print(str(ctx$get_engine()))
    })
    
    # =====================================================
    # 📊 VALIDATION TABLE (SAFE ENGINE ACCESS)
    # =====================================================
    output$validation_table <- DT::renderDataTable({
      
      eng <- ctx$get_engine()
      req(!is.null(eng))
      
      dplyr::bind_rows(
        normalize_validation(eng$validation$obs, "obs"),
        normalize_validation(eng$validation$site, "site"),
        normalize_validation(eng$validation$tree, "tree"),
        normalize_validation(eng$validation$sample, "sample")
      )
    })
    
    # =====================================================
    # 🔍 ENGINE DEBUG VIEW (SAFE)
    # =====================================================
    output$engine_debug <- DT::renderDataTable({
      
      eng <- ctx$get_engine()
      req(!is.null(eng))
      
      normalize_validation(eng$validation$obs, "obs")
    })
  })
}
