
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
    
    ns <- session$ns
    
    # =====================================================
    # 🧠 SYSTEM STATE
    # =====================================================
    output$state_view <- renderPrint({
      
      list(
        stage = ctx$v2$stage,
        frozen = !is.null(ctx$snapshot()),
        obs_ready = ctx$state$obs_valid,
        site_ready = ctx$state$site_valid,
        tree_ready = ctx$state$tree_valid,
        export_ready = ctx$v2$export_ready
      )
    })
    
    # =====================================================
    # 📦 DATASET INFO
    # =====================================================
    output$dataset_info <- renderPrint({
      
      list(
        name = ctx$form$dataset_name,
        version = ctx$form$dataset_version,
        has_obs = !is.null(ctx$data$obs$working_copy),
        has_site = !is.null(ctx$data$site$working_copy),
        has_tree = !is.null(ctx$data$tree$working_copy)
      )
    })
    
    # =====================================================
    # ⚙️ ENGINE INSPECTION (CORE)
    # =====================================================
    output$engine_view <- renderPrint({
      
      req(ctx$engine())
      
      str(ctx$engine(), max.level = 2)
    })
    
    # =====================================================
    # 🧪 VALIDATION TABLE
    # =====================================================
    output$validation_table <- DT::renderDT({
      
      req(ctx$engine())
      
      v <- ctx$engine()$validation
      
      data.frame(
        layer = names(v),
        valid = sapply(v, function(x) if (!is.null(x$valid)) x$valid else NA)
      )
    })
  })
}