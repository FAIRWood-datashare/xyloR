

#' @export
#'
mod_tab12_ui <- function(id) {

ns <- shiny::NS(id)

bslib::nav_panel(
  title = "Export",
  value = "tab11",
  
  fluidRow(
    
    # =====================================================
    # LEFT: EXPORT ACTIONS
    # =====================================================
    column(
      4,
      
      bslib::card(
        bslib::card_header("11.1 Final export"),
        
        bslib::card_body(
          
          uiOutput(ns("export_status")),
          
          tags$hr(),
          
          downloadButton(
            ns("download_package"),
            "Download ZIP",
            class = "btn btn-success w-100"
          ),
          
          tags$hr(),
          
          actionButton(
            ns("freeze_state"),
            "Freeze dataset",
            class = "btn btn-warning w-100"
          )
        )
      )
    ),
    
    # =====================================================
    # RIGHT: EXPORT SUMMARY
    # =====================================================
    column(
      8,
      
      bslib::card(
        bslib::card_header("11.2 Final summary"),
        
        bslib::card_body(
          
          DT::DTOutput(ns("export_summary")),
          tags$hr(),
          DT::DTOutput(ns("lineage_log"))
        )
      )
    )
  )
)
}

mod_tab12_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 🧠 FREEZE STATE
    # =====================================================
    frozen <- reactiveVal(FALSE)
    
    # =====================================================
    # 📦 EXPORT REGISTRY
    # =====================================================
    if (is.null(ctx$registry)) {
      ctx$registry <- list()
    }
    
    if (is.null(ctx$registry$exports)) {
      ctx$registry$exports <- list()
    }
    
    # =====================================================
    # 🧊 FREEZE ACTION (SNAPSHOT ONLY)
    # =====================================================
    observeEvent(input$freeze_state, {
      
      req(ctx$get_engine())
      
      frozen(TRUE)
      
      # 🧠 snapshot ONLY (no mutation of live engine)
      ctx$snapshot$engine <- ctx$get_engine()
      
      snapshot_id <- paste0(
        "snap_",
        format(Sys.time(), "%Y%m%d_%H%M%S")
      )
      
      ctx$snapshot_meta <- list(
        id              = snapshot_id,
        timestamp       = Sys.time(),
        dataset_name    = ctx$form$dataset_name,
        dataset_version = ctx$form$dataset_version
      )
      
      ctx$edit$lock <- TRUE
      
      showNotification(
        "Dataset frozen. Snapshot captured.",
        type = "message"
      )
    })
    
    # =====================================================
    # 📦 EXPORT STATUS
    # =====================================================
    output$export_status <- shiny::renderUI({
      
      if (!ctx$has_engine()) {
        return(tags$div(class = "alert alert-warning", "⚠ Engine not ready"))
      }
      
      if (frozen()) {
        tags$div(class = "alert alert-success", "✔ Ready for export")
      } else {
        tags$div(class = "alert alert-warning", "⚠ Please freeze dataset first")
      }
    })
    
    # =====================================================
    # 📋 EXPORT SUMMARY
    # =====================================================
    output$export_summary <- DT::renderDT({
      
      eng <- ctx$get_engine()
      req(eng)
      
      data.frame(
        layer = c("OBS", "SITE", "TREE", "SAMPLE", "AUTHORS", "PUBLICATIONS"),
        status = c(
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          !is.null(eng$enrichment$authors),
          !is.null(eng$enrichment$publications)
        )
      )
    })
    
    # =====================================================
    # 🧬 LINEAGE LOG
    # =====================================================
    output$lineage_log <- DT::renderDT({
      
      data.frame(
        step = c("OBS", "SITE", "TREE", "SAMPLE", "AUTHOR", "PUBLICATION"),
        source = c("raw", "obs", "site", "tree", "sample", "sample"),
        transformed_by = c(
          "ingestion",
          "site_engine",
          "tree_engine",
          "sample_engine",
          "orcid_resolver",
          "doi_resolver"
        )
      )
    })
    
    # =====================================================
    # 📦 DOWNLOAD PACKAGE (CLEAN)
    # =====================================================
    output$download_package <- downloadHandler(
      
      filename = function() {
        paste0(
          "xylo_",
          ctx$form$dataset_name, "_",
          ctx$form$dataset_version, "_",
          Sys.Date(),
          ".zip"
        )
      },
      
      content = function(file) {
        
        req(frozen())
        req(ctx$snapshot$engine)
        
        tmp <- tempdir()
        
        # 👉 use centralized export function
        export_snapshot(ctx, file)
      }
    )
    
  })
}
