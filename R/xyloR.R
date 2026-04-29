#' GloboXylo Shiny App
#'
#' This is the main UI and server definition for the GloboXylo data collector
#' Shiny application. The app provides tools to upload, validate, and export
#' xylogenesis-related datasets using a structured, tab-based interface.
#'
#' @import shiny
#' @import shinyjs
#' @import bslib
#' @importFrom htmltools includeCSS includeScript tags
#' @return A Shiny app object
#' @export
#' @examples
#' if (interactive()) {
#'   shiny::runApp(system.file("app", package = "GloboXyloApp"))
#' }
#'
xyloR <- function() {
  
  ui <- shiny::fluidPage(
    
    shinyjs::useShinyjs(),
    
    theme = bslib::bs_theme(
      bootswatch = "darkly",
      primary = "#375A7F"
    ),
    
    bslib::navset_card_tab(
      id = "tabs",
      
      bslib::nav_panel("Tab1", value = "tab1", mod_tab1_ui("tab1")),
      bslib::nav_panel("Tab2", value = "tab2", mod_tab2_ui("tab2")),
      bslib::nav_panel("Tab3", value = "tab3", mod_tab3_ui("tab3")),
      bslib::nav_panel("Tab4", value = "tab4", mod_tab4_ui("tab4")),
      bslib::nav_panel("Tab5", value = "tab5", mod_tab5_ui("tab5")),
      bslib::nav_panel("Tab6", value = "tab6", mod_tab6_ui("tab6")),
      bslib::nav_panel("Tab7", value = "tab7", mod_tab7_ui("tab7")),
      bslib::nav_panel("Tab8", value = "tab8", mod_tab8_ui("tab8")),
      bslib::nav_panel("Tab9", value = "tab9", mod_tab9_ui("tab9")),
      bslib::nav_panel("Tab10", value = "tab10", mod_tab10_ui("tab10")),
      bslib::nav_panel("Tab11", value = "tab11", mod_tab11_ui("tab11")),
      
      bslib::nav_panel(
        "Debug",
        value = "debug",
        mod_debug_ui("debug"),
        
        shiny::fluidRow(
          
          shiny::column(
            4,
            shiny::tags$div(
              style = "padding:10px; border:1px solid #444; border-radius:6px;",
              
              shiny::tags$h4("V2 Runtime"),
              shiny::tags$hr(),
              
              shiny::tags$b("Stage: "),
              shiny::textOutput("v2_state"),
              
              shiny::br(),
              shiny::tags$b("Transitions:"),
              DT::dataTableOutput("history_table")
            )
          ),
          
          shiny::column(
            8,
            visNetwork::visNetworkOutput("graph", height = "450px")
          )
        )
      )
    )
  )

  
  server <- function(input, output, session) {
    
    # =====================================================
    # CONTEXT
    # =====================================================
    ctx <- create_app_context()
    
    # =====================================================
    # ENGINE CACHE INITIALIZATION (🔥 CRITICAL FIX)
    # =====================================================
    ctx$engine_cache <- shiny::reactiveVal(NULL)
    
    ctx$v2 <- create_minimal_state()
    
    # =====================================================
    # ENGINE REBUILD OBSERVER
    # =====================================================
    observe({
      
      obs    <- ctx$data$obs_raw %||% NULL
      site   <- ctx$data$site_info %||% NULL
      tree   <- ctx$data$tree_info %||% NULL
      sample <- ctx$data$sample_info %||% NULL
      
      ctx$engine_cache(
        update_state_engine(obs, site, tree, sample)
      )
    })
    
    # =====================================================
    # DERIVED ENGINE (SAFE WRAPPER)
    # =====================================================
    ctx$engine <- reactive({
      eng <- ctx$engine_cache()
      req(!is.null(eng))
      eng
    })
    
    # =====================================================
    # EXTERNAL API
    # =====================================================
    ctx$external_api <- external_metadata_module(ctx)
    
    # =====================================================
    # CONTRACT CHECK
    # =====================================================
    ctx_contract_lock <- function(ctx) {
      
      if (!is.null(ctx$external)) {
        stop("❌ ctx$external is deprecated. Use ctx$external_api only.")
      }
      
      if (is.null(ctx$external_api)) {
        stop("❌ ctx$external_api missing")
      }
      
      if (is.null(ctx$data)) {
        stop("❌ ctx$data missing")
      }
      
      invisible(TRUE)
    }
    
    ctx_contract_lock(ctx)
    
    observe({
      invalidateLater(30000, session)
      ctx_contract_lock(ctx)
    })
    
    # =====================================================
    # V2 STATE
    # =====================================================
    v2_state <- shiny::reactiveVal(NULL)
    
    observe({
      req(ctx$v2)
      v2_state(compute_v2_state(ctx))
    })
    
    observe({
      
      state <- v2_state()
      req(state)
      
      isolate({
        ctx$v2$dataset_ready   <- state$dataset_valid
        ctx$v2$ingestion_ready <- state$ingestion_ready
        ctx$v2$qa_ready        <- state$qa_ready
        ctx$v2$meta_ready      <- state$meta_ready
        ctx$v2$export_ready    <- state$export_ready
      })
    })
    
    # =====================================================
    # HISTORY
    # =====================================================
    ctx$debug$history <- shiny::reactiveVal(data.frame(
      from = character(),
      to = character(),
      trigger = character(),
      timestamp = as.POSIXct(character())
    ))
    
    log_transition <- function(from, to, trigger) {
      
      old <- ctx$debug$history()
      
      ctx$debug$history(rbind(old, data.frame(
        from = ifelse(is.null(from), "START", from),
        to = to,
        trigger = trigger,
        timestamp = Sys.time()
      )))
    }
    
    # =====================================================
    # MODULES
    # =====================================================
    mod_tab1_server("tab1", ctx, session)
    mod_tab2_server("tab2", ctx)
    mod_tab3_server("tab3", ctx)
    mod_tab4_server("tab4", ctx)
    mod_tab5_server("tab5", ctx)
    mod_tab6_server("tab6", ctx)
    mod_tab7_server("tab7", ctx)
    mod_tab8_server("tab8", ctx)
    mod_tab9_server("tab9", ctx)
    mod_tab10_server("tab10", ctx)
    mod_tab11_server("tab11", ctx)
    mod_debug_server("debug", ctx)
    
    # =====================================================
    # NAVIGATION
    # =====================================================
    observe({
      
      req(ctx$v2$stage)
      
      isolate({
        bslib::nav_select(
          id = "tabs",
          selected = ctx$v2$stage,
          session = session
        )
      })
    })
    
    # =====================================================
    # TRANSITIONS
    # =====================================================
    last_stage <- shiny::reactiveVal(NULL)
    
    observe({
      
      req(ctx$v2$stage)
      
      if (!identical(ctx$v2$stage, last_stage())) {
        
        log_transition(last_stage(), ctx$v2$stage, "stage_change")
        last_stage(ctx$v2$stage)
      }
    })
    
    # =====================================================
    # DEBUG OUTPUT
    # =====================================================
    output$v2_state <- renderText({
      paste0("V2 stage: ", ctx$v2$stage)
    })
    
    output$history_table <- DT::renderDataTable({
      DT::datatable(ctx$debug$history(), options = list(pageLength = 5))
    })
    
    # =====================================================
    # GRAPH
    # =====================================================
    output$graph <- visNetwork::renderVisNetwork({
      
      nodes <- data.frame(
        id = c("tab1","tab2","tab3","tab4"),
        label = c("Tab 1","Tab 2","Tab 3","Tab 4"),
        color = ifelse(c("tab1","tab2","tab3","tab4") == ctx$v2$stage,
                       "#00C853", "#2C3E50"),
        shape = "box"
      )
      
      edges <- data.frame(
        from = c("tab1","tab2","tab3"),
        to   = c("tab2","tab3","tab4"),
        arrows = "to"
      )
      
      visNetwork::visNetwork(nodes, edges) |>
        visNetwork::visNodes(font = list(color = "white")) |>
        visNetwork::visEdges(color = "#888")
    })
  }
  
  shiny::shinyApp(ui, server)
}