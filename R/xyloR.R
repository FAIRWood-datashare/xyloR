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
      
      bslib::nav_panel(
        "Debug",
        value = "debug",
        
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
    ctx$v2 <- create_minimal_state()
    
    # =====================================================
    # SAFE DERIVED STATE STORE (IMPORTANT FIX)
    # =====================================================
    v2_state <- reactiveVal(NULL)
    
    # =====================================================
    # V2 REACTOR (FIXED: NO SELF-WRITE LOOP)
    # =====================================================
    observe({
      
      req(ctx$v2)
      
      state <- compute_v2_state(ctx)
      
      v2_state(state)
    })
    
    # =====================================================
    # APPLY DERIVED STATE (ONE DIRECTION ONLY)
    # =====================================================
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
    # SAFE HISTORY STORE
    # =====================================================
    ctx$debug$history <- shiny::reactiveVal(
      data.frame(
        from = character(),
        to = character(),
        trigger = character(),
        timestamp = as.POSIXct(character()),
        stringsAsFactors = FALSE
      )
    )
    
    log_transition <- function(from, to, trigger) {
      
      old <- ctx$debug$history()
      
      new_row <- data.frame(
        from = ifelse(is.null(from), "START", from),
        to = ifelse(is.null(to), NA_character_, to),
        trigger = trigger,
        timestamp = Sys.time(),
        stringsAsFactors = FALSE
      )
      
      ctx$debug$history(rbind(old, new_row))
    }
    
    # =====================================================
    # MODULES
    # =====================================================
    mod_tab1_server("tab1", ctx, session)
    mod_tab2_server("tab2", ctx)
    mod_tab3_server("tab3", ctx)
    mod_tab4_server("tab4", ctx)
    
    # =====================================================
    # NAVIGATION (SAFE)
    # =====================================================
    observe({
      req(ctx$v2$stage)
      
      bslib::nav_select(
        id = "tabs",
        selected = ctx$v2$stage,
        session = session
      )
    })
    
    # =====================================================
    # TRANSITION TRACKING
    # =====================================================
    last_stage <- reactiveVal(NULL)
    
    observe({
      
      req(ctx$v2$stage)
      
      if (!identical(ctx$v2$stage, last_stage())) {
        
        log_transition(
          from = last_stage(),
          to = ctx$v2$stage,
          trigger = "stage_change"
        )
        
        last_stage(ctx$v2$stage)
      }
    })
    
    # =====================================================
    # DEBUG UI
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