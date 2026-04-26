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
      
      # =====================================================
      # 🔥 DEBUG / FSM INSPECTOR TAB
      # =====================================================
      bslib::nav_panel(
        "FSM Debug",
        value = "debug",
        
        shiny::fluidRow(
          
          shiny::column(
            4,
            
            shiny::tags$div(
              style = "padding:10px; border:1px solid #444; border-radius:6px;",
              
              shiny::tags$h4("FSM Runtime Inspector"),
              
              shiny::tags$hr(),
              
              shiny::tags$b("State: "),
              shiny::textOutput("fsm_state"),
              
              shiny::tags$br(),
              
              shiny::tags$b("Transition: "),
              shiny::textOutput("fsm_transition"),
              
              shiny::tags$br(),
              
              shiny::tags$b("Trigger: "),
              shiny::textOutput("fsm_trigger"),
              
              shiny::tags$br(),
              
              shiny::tags$b("Timestamp: "),
              shiny::textOutput("fsm_timestamp")
            )
          ),
          
          shiny::column(
            8,
            
            # =====================================================
            # 🔥 FSM LIVE GRAPH VISUALIZER
            # =====================================================
            visNetwork::visNetworkOutput("fsm_graph", height = "450px")
          )
        ),
        
        shiny::tags$hr(),
        
        shiny::tags$h5("Signals"),
        
        shiny::tags$ul(
          shiny::tags$li(shiny::textOutput("sig_tab1")),
          shiny::tags$li(shiny::textOutput("sig_tab2")),
          shiny::tags$li(shiny::textOutput("sig_tab3"))
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    ctx <- create_app_context()
    
    # =====================================================
    # MODULES
    # =====================================================
    mod_tab1_server("tab1", ctx, session)
    mod_tab2_server("tab2", ctx, session)
    mod_tab3_server("tab3", ctx, session)
    
    # =====================================================
    # FSM v2 (STATE + DEBUG TRACKING)
    # =====================================================
    observe({
      
      req(ctx$fsm$state)
      
      current <- ctx$fsm$state
      next_state <- current
      
      tab1_ok <- isTRUE(ctx$signals$tab1_done)
      tab2_ok <- isTRUE(ctx$signals$tab2_done)
      tab3_ok <- isTRUE(ctx$signals$tab3_done)
      
      trigger <- NULL
      
      if (current == "tab1" && tab1_ok) {
        next_state <- "tab2"
        trigger <- "tab1_done"
      }
      
      if (current == "tab2" && tab2_ok) {
        next_state <- "tab3"
        trigger <- "tab2_done"
      }
      
      if (current == "tab3" && tab3_ok) {
        next_state <- "DONE"
        trigger <- "tab3_done"
      }
      
      if (!identical(current, next_state)) {
        
        ctx$fsm$state <- next_state
        
        ctx$debug$last_transition_from <- current
        ctx$debug$last_transition_to <- next_state
        ctx$debug$last_trigger <- trigger
        ctx$debug$timestamp <- Sys.time()
        
        message("FSM: ", current, " → ", next_state,
                " | trigger: ", trigger)
      }
    })
    
    # =====================================================
    # NAVIGATION LAYER
    # =====================================================
    observe({
      
      req(ctx$fsm$state)
      
      isolate({
        bslib::nav_select(
          id = "tabs",
          selected = ctx$fsm$state,
          session = session
        )
      })
    })
    
    # =====================================================
    # 🔥 FSM DEBUG TEXT OUTPUTS
    # =====================================================
    
    output$fsm_state <- shiny::renderText({
      ctx$fsm$state
    })
    
    output$fsm_transition <- shiny::renderText({
      paste(ctx$debug$last_transition_from,
            "→",
            ctx$debug$last_transition_to)
    })
    
    output$fsm_trigger <- shiny::renderText({
      ctx$debug$last_trigger
    })
    
    output$fsm_timestamp <- shiny::renderText({
      as.character(ctx$debug$timestamp)
    })
    
    output$sig_tab1 <- shiny::renderText({
      paste("tab1_done:", ctx$signals$tab1_done)
    })
    
    output$sig_tab2 <- shiny::renderText({
      paste("tab2_done:", ctx$signals$tab2_done)
    })
    
    output$sig_tab3 <- shiny::renderText({
      paste("tab3_done:", ctx$signals$tab3_done)
    })
    
    # =====================================================
    # 🔥 FSM LIVE GRAPH VISUALIZER
    # =====================================================
    output$fsm_graph <- visNetwork::renderVisNetwork({
      
      req(ctx$fsm$state)
      
      nodes <- data.frame(
        id = c("tab1", "tab2", "tab3", "DONE"),
        label = c("Tab 1", "Tab 2", "Tab 3", "DONE"),
        color = c(
          if (ctx$fsm$state == "tab1") "#00C853" else "#2C3E50",
          if (ctx$fsm$state == "tab2") "#00C853" else "#2C3E50",
          if (ctx$fsm$state == "tab3") "#00C853" else "#2C3E50",
          if (ctx$fsm$state == "DONE") "#00C853" else "#2C3E50"
        ),
        shape = "box"
      )
      
      edges <- data.frame(
        from = c("tab1", "tab2", "tab3"),
        to   = c("tab2", "tab3", "DONE"),
        arrows = "to"
      )
      
      visNetwork::visNetwork(nodes, edges) |>
        visNetwork::visNodes(font = list(color = "white")) |>
        visNetwork::visEdges(color = list(color = "#888")) |>
        visNetwork::visOptions(
          highlightNearest = TRUE,
          nodesIdSelection = TRUE
        )
    })
  }
  
  shiny::shinyApp(ui, server)
}