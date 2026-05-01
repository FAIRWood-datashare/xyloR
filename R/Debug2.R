
#' @export
#'
mod_debug_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Debug",
    value = "debug",
    
    fluidRow(
      
      # =====================================================
      # LEFT: STATE SNAPSHOT
      # =====================================================
      column(
        4,
        
        bslib::card(
          bslib::card_header("🧠 System State"),
          bslib::card_body(
            
            verbatimTextOutput(ns("state_view"))
          )
        ),
        
        bslib::card(
          bslib::card_header("📦 Dataset Info"),
          bslib::card_body(
            
            verbatimTextOutput(ns("dataset_info"))
          )
        )
      ),
      
      # =====================================================
      # RIGHT: ENGINE INSPECTION
      # =====================================================
      column(
        8,
        
        bslib::card(
          bslib::card_header("⚙️ Engine Snapshot"),
          bslib::card_body(
            
            verbatimTextOutput(ns("engine_view"))
          )
        ),
        
        bslib::card(
          bslib::card_header("🧪 Validation Summary"),
          bslib::card_body(
            
            DT::DTOutput(ns("validation_table"))
          )
        )
      )
    )
  )
}

mod_debug_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    # =====================================================
    # 🧠 SYSTEM STATE (SAFE)
    # =====================================================
    output$state_view <- renderPrint({
      
      list(
        stage           = ctx$state$stage,
        frozen          = !!ctx$has_engine(),
        dataset_ready   = ctx$state$dataset_ready,
        ingestion_ready = ctx$state$ingestion_ready,
        qa_ready        = ctx$state$qa_ready,
        export_ready    = ctx$state$export_ready
      )
    })
    
    # =====================================================
    # 📦 DATASET INFO
    # =====================================================
    output$dataset_info <- renderPrint({
      
      list(
        name    = ctx$form$dataset_name,
        version = ctx$form$dataset_version,
        
        has_obs  = !is.null(ctx$data$obs$working_copy),
        has_site = !is.null(ctx$data$site$working_copy),
        has_tree = !is.null(ctx$data$tree$working_copy)
      )
    })
    
    # =====================================================
    # 🧠 ENGINE RESOLVER (STANDARDIZED)
    # =====================================================
    get_engine <- reactive({
      
      eng <- ctx$get_engine()
      req(!is.null(eng))
      
      eng
    })
    
    # =====================================================
    # ⚙️ ENGINE INSPECTION (SAFE)
    # =====================================================
    output$engine_view <- renderPrint({
      
      if (!ctx$has_engine()) {
        return("ENGINE NOT INITIALIZED")
      }
      
      str(get_engine(), max.level = 2)
    })
    
    # =====================================================
    # 🧪 VALIDATION TABLE (SAFE + CONSISTENT)
    # =====================================================
    output$validation_table <- DT::renderDT({
      
      if (!ctx$has_engine()) {
        
        return(data.frame(
          layer = character(),
          valid = character(),
          note  = "Engine not available"
        ))
      }
      
      v <- get_engine()$validation
      
      data.frame(
        layer = names(v),
        valid = sapply(v, function(x) {
          if (is.list(x) && !is.null(x$valid)) {
            x$valid
          } else {
            NA
          }
        })
      )
    })
    
  })
}