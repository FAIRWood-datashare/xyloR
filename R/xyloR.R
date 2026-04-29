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
              
              shiny::br(),
              shiny::tags$b("Last Transition Table:"),
              DT::dataTableOutput("fsm_history_table")
            )
          ),
          
          shiny::column(
            8,
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
    ctx$v2 <- create_minimal_state()
    
    observe({
      
      req(ctx$v2$stage)
      
      stage <- isolate(ctx$v2$stage)
      
      switch(stage,
             
             "tab1" = nav_select("tab1"),
             "tab2" = nav_select("tab2"),
             "tab3" = nav_select("tab3"),
             "tab4" = nav_select("tab4"),
             "tab5" = nav_select("tab5"),
             "tab6" = nav_select("tab6"),
             "tab7" = nav_select("tab7"),
             "tab8" = nav_select("tab8"),
             "tab9" = nav_select("tab9")
             
      )
    })
    
    last_stage <- reactiveVal(NULL)
    
    observe({
      
      req(ctx$v2$stage)
      
      if (identical(ctx$v2$stage, last_stage()))
        return()
      
      last_stage(ctx$v2$stage)
      
    })
    
    # =====================================================
    # INIT DEBUG HISTORY (CRITICAL FIX)
    # =====================================================
    ctx$debug$history <- shiny::reactiveVal(
      data.frame(
        from = character(),
        to = character(),
        trigger = character(),
        timestamp = as.POSIXct(character())
      )
    )
    
    # =====================================================
    # MODULES
    # =====================================================
    mod_tab1_server("tab1", ctx, session)
    mod_tab2_server("tab2", ctx)
    mod_tab3_server("tab3", ctx)
    mod_tab4_server("tab4", ctx)
    
    # =====================================================
    # FSM ENGINE
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
        
        # =====================================================
        # SAFE HISTORY APPEND (NO rbind growth issues later)
        # =====================================================
        hist <- ctx$debug$history()
        
        new_row <- data.frame(
          from = current,
          to = next_state,
          trigger = trigger,
          timestamp = Sys.time()
        )
        
        ctx$debug$history(rbind(hist, new_row))
      }
    })
    
    # =====================================================
    # NAVIGATION
    # =====================================================
    observe({
      
      stage <- ctx$v2$stage
      req(stage)
      
      cat("NAV TARGET:", stage, "\n")
      
      bslib::nav_select(
        id = "tabs",
        selected = stage,
        session = session
      )
    })
    
    # =====================================================
    # FSM STATE OUTPUT
    # =====================================================
    output$fsm_state <- shiny::renderText({
      ctx$fsm$state
    })
    
    # =====================================================
    # HISTORY TABLE
    # =====================================================
    output$fsm_history_table <- DT::renderDataTable({
      DT::datatable(ctx$debug$history(), options = list(pageLength = 5))
    })
    
    # =====================================================
    # SIGNALS
    # =====================================================
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
    # FSM GRAPH (SAFE VERSION)
    # =====================================================
    output$fsm_graph <- visNetwork::renderVisNetwork({
      
      nodes <- data.frame(
        id = c("tab1", "tab2", "tab3", "DONE"),
        label = c("Tab 1", "Tab 2", "Tab 3", "DONE"),
        color = ifelse(
          c("tab1","tab2","tab3","DONE") == ctx$fsm$state,
          "#00C853",
          "#2C3E50"
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
        visNetwork::visEdges(color = "#888")
    })
  }
  
  shiny::shinyApp(ui, server)
}