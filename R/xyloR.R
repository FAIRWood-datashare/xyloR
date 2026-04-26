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
      bslib::nav_panel("Tab3", value = "tab3", mod_tab3_ui("tab3"))
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
    # FSM ENGINE (SINGLE SOURCE OF TRUTH)
    # =====================================================
    observe({
      
      req(ctx$fsm$state)
      
      current <- ctx$fsm$state
      next_state <- current
      
      tab1_ok <- isTRUE(ctx$signals$tab1_done)
      tab2_ok <- isTRUE(ctx$signals$tab2_done)
      tab3_ok <- isTRUE(ctx$signals$tab3_done)
      
      if (current == "tab1" && tab1_ok) {
        next_state <- "tab2"
        message("FSM: tab1 → tab2")
      }
      
      if (current == "tab2" && tab2_ok) {
        next_state <- "tab3"
        message("FSM: tab2 → tab3")
      }
      
      if (current == "tab3" && tab3_ok) {
        next_state <- "DONE"
        message("FSM: tab3 → DONE")
      }
      
      if (!identical(current, next_state)) {
        ctx$fsm$state <- next_state
      }
    })
    
    # =====================================================
    # NAVIGATION LAYER
    # =====================================================
    observe({
      
      req(ctx$fsm$state)
      
      if (ctx$fsm$state != "DONE") {
        
        bslib::nav_select(
          id = "tabs",
          selected = ctx$fsm$state,
          session = session
        )
        
      } else {
        message("APP COMPLETE (FSM in DONE state)")
      }
    })
  }
  
  shiny::shinyApp(ui, server)
}