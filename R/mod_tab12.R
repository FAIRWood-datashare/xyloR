

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
    # 📦 EXPORT REGISTRY (NEW)
    # =====================================================
    if (is.null(ctx$registry)) {
      ctx$registry <- list()
    }
    
    if (is.null(ctx$registry$exports)) {
      ctx$registry$exports <- list()
    }
    
    # =====================================================
    # 🧊 FREEZE ACTION (SNAPSHOT + VERSIONING)
    # =====================================================
    observeEvent(input$freeze_state, {
      
      frozen(TRUE)
      
      # 🧠 CAPTURE SNAPSHOT
      snap <- ctx$engine()
      
      ctx$snapshot(snap)
      
      # 🧾 CREATE SNAPSHOT METADATA (NEW)
      snapshot_id <- paste0(
        "snap_",
        format(Sys.time(), "%Y%m%d_%H%M%S")
      )
      
      ctx$snapshot_meta <- list(
        id = snapshot_id,
        timestamp = Sys.time(),
        dataset_name = ctx$form$dataset_name,
        dataset_version = ctx$form$dataset_version
      )
      
      # 🔒 LOCK EDITING
      ctx$edit$lock <- TRUE
      
      showNotification(
        "Dataset frozen. Snapshot captured for export.",
        type = "message"
      )
    })
    
    # =====================================================
    # 📦 EXPORT STATUS
    # =====================================================
    output$export_status <- shiny::renderUI({
      
      req(ctx$engine())
      
      if (frozen()) {
        
        tags$div(
          class = "alert alert-success",
          "✔ Dataset frozen and export-ready"
        )
        
      } else {
        
        tags$div(
          class = "alert alert-warning",
          "⚠ Please freeze dataset before export"
        )
      }
    })
    
    # =====================================================
    # 📋 EXPORT SUMMARY
    # =====================================================
    output$export_summary <- DT::renderDT({
      
      req(ctx$engine())
      
      e <- ctx$engine()
      
      data.frame(
        layer = c(
          "OBS",
          "SITE",
          "TREE",
          "SAMPLE",
          "AUTHORS",
          "PUBLICATIONS"
        ),
        status = c(
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          !is.null(e$enrichment$authors),
          !is.null(e$enrichment$publications)
        )
      )
    })
    
    # =====================================================
    # 🧬 LINEAGE LOG
    # =====================================================
    output$lineage_log <- DT::renderDT({
      
      data.frame(
        step = c(
          "OBS",
          "SITE",
          "TREE",
          "SAMPLE",
          "AUTHOR",
          "PUBLICATION"
        ),
        source = c(
          "raw",
          "obs",
          "site",
          "tree",
          "sample",
          "sample"
        ),
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
    # 📦 DOWNLOAD PACKAGE (VERSIONED + SNAPSHOT SAFE)
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
        req(ctx$snapshot())
        
        tmp <- tempdir()
        
        # =================================================
        # 📦 EXPORT DATA
        # =================================================
        saveRDS(ctx$data, file.path(tmp, "dataset.rds"))
        
        saveRDS(
          ctx$snapshot(),
          file.path(tmp, "engine_snapshot.rds")
        )
        
        # =================================================
        # 🧾 SNAPSHOT METADATA
        # =================================================
        write.csv(
          data.frame(
            snapshot_id = ctx$snapshot_meta$id,
            timestamp = ctx$snapshot_meta$timestamp,
            dataset_name = ctx$snapshot_meta$dataset_name,
            dataset_version = ctx$snapshot_meta$dataset_version
          ),
          file.path(tmp, "snapshot_meta.csv"),
          row.names = FALSE
        )
        
        # =================================================
        # 📊 VALIDATION LOG
        # =================================================
        write.csv(
          ctx$validation,
          file.path(tmp, "validation_log.csv"),
          row.names = FALSE
        )
        
        # =================================================
        # 📦 ZIP PACKAGE
        # =================================================
        zip::zipr(
          zipfile = file,
          files = list.files(tmp, full.names = TRUE)
        )
        
        # =================================================
        # 🧠 REGISTER EXPORT EVENT
        # =================================================
        ctx$registry$exports[[length(ctx$registry$exports) + 1]] <- list(
          dataset = ctx$form$dataset_name,
          version = ctx$form$dataset_version,
          snapshot = ctx$snapshot_meta$id,
          timestamp = Sys.time()
        )
      }
    )
  })
}