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
  
  # ======================================================
  # UI
  # ======================================================
  ui <- shiny::fluidPage(
    
    shinyjs::useShinyjs(),
    
    theme = bslib::bs_theme(
      bootswatch = "darkly",
      primary    = "#375A7F",
      secondary  = "#3498DB",
      font_scale = 0.8
    ),
    
    htmltools::tags$head(
      htmltools::tags$script(src = "https://unpkg.com/@popperjs/core@2"),
      htmltools::tags$script(src = "https://unpkg.com/tippy.js@6")
    ),
    
    htmltools::includeCSS("www/custom_styles.css"),
    htmltools::includeScript("www/custom_scripts.js"),
    
    shiny::titlePanel("GloboXylo Data Collector"),
    
    bslib::navset_card_tab(
      id = "tabs",
      
      bslib::nav_panel("1. Upload observation", value = "tab1", mod_tab1_ui("tab1")),
      bslib::nav_panel("2. Upload metadata", value = "tab2", mod_tab2_ui("tab2")),
      bslib::nav_panel("Observation", value = "tab3", mod_tab3_ui("tab3")),
      bslib::nav_panel("Site", value = "tab4", mod_tab2_ui("tab4")),
      bslib::nav_panel("Tree", value = "tab5", mod_tab2_ui("tab5")),
      bslib::nav_panel("Sample", value = "tab6", mod_tab2_ui("tab6")),
      bslib::nav_panel("Person", value = "tab7", mod_tab2_ui("tab7")),
      bslib::nav_panel("Publication", value = "tab8", mod_tab2_ui("tab8"))
    )
  )
  
  # ======================================================
  # SERVER
  # ======================================================
  server <- function(input, output, session) {
    
    ctx <- create_app_context()
    
    # ======================================================
    # FSM INIT (ONCE ONLY)
    # ======================================================
    ctx$fsm <- list(
      state = "TAB1",
      
      flags = list(
        tab1_complete = FALSE,
        tab2_complete = FALSE,
        tab3_complete = FALSE
      ),
      
      events = list(
        go_next = FALSE
      )
    )
    
    # ======================================================
    # MODULES
    # ======================================================
    mod_tab1_server("tab1", ctx, session)
    mod_tab2_server("tab2", ctx, session)
    mod_tab3_server("tab3", ctx, session)
    
    mod_tab4_server("tab4", ctx, session)
    mod_tab5_server("tab5", ctx, session)
    mod_tab6_server("tab6", ctx, session)
    mod_tab7_server("tab7", ctx, session)
    mod_tab8_server("tab8", ctx, session)
    
    # ======================================================
    # FSM INPUT LAYER (DERIVE FLAGS ONLY)
    # ======================================================
    observe({
      
      ctx$fsm$flags$tab1_complete <-
        isTRUE(ctx$state$tab1$inputdata$valid) &&
        isTRUE(ctx$state$tab1$file$uploaded) &&
        isTRUE(ctx$state$tab1$validation$all_valid)
      
      ctx$fsm$flags$tab2_complete <-
        isTRUE(ctx$state$tab2$validation$all_valid)
    })
    
    # ======================================================
    # FSM ENGINE (PURE TRANSITION LOGIC)
    # ======================================================
    fsm_transition <- function(ctx) {
      
      message("🧠 FSM RUN: ", ctx$fsm$state)
      
      if (ctx$fsm$state == "TAB1" &&
          isTRUE(ctx$fsm$flags$tab1_complete) &&
          isTRUE(ctx$fsm$events$go_next)) {
        
        ctx$fsm$state <- "TAB2"
        ctx$fsm$events$go_next <- FALSE
        
        message("➡️ FSM: TAB1 → TAB2")
      }
      
      if (ctx$fsm$state == "TAB2" &&
          isTRUE(ctx$fsm$flags$tab2_complete) &&
          isTRUE(ctx$fsm$events$go_next)) {
        
        ctx$fsm$state <- "TAB3"
        ctx$fsm$events$go_next <- FALSE
        
        message("➡️ FSM: TAB2 → TAB3")
      }
    }
    
    # ======================================================
    # TRIGGER FSM ON DEMAND (NO POLLING)
    # ======================================================
    ctx$fsm_trigger <- reactiveVal(0)
    
    observeEvent(ctx$fsm_trigger(), {
      
      fsm_transition(ctx)
      
    })
    
    # ======================================================
    # ROUTER (UI NAVIGATION)
    # ======================================================
    observeEvent(ctx$fsm_trigger(), {
      
      state <- ctx$fsm$state
      
      message("🧭 ROUTER STATE = ", state)
      
      bslib::nav_select(
        id = "tabs",
        selected = tolower(state),
        session = session
      )
    })
    
    # ======================================================
    # NEXT BUTTON TRIGGER (GLOBAL LISTENER OR INSIDE TAB1)
    # ======================================================
    observe({
      
      # Example: Tab1 next button
      if (isTRUE(ctx$fsm$flags$tab1_complete) &&
          ctx$fsm$state == "TAB1") {
        
        # NOTHING automatic — wait for click event from module
      }
    })
    
    # ======================================================
    # OPTIONAL: GLOBAL NEXT HANDLER (RECOMMENDED)
    # ======================================================
    observeEvent(input$next_btn, {
      
      ctx$fsm$events$go_next <- TRUE
      
      # 🔥 THIS IS THE ONLY TRIGGER NOW
      ctx$fsm_trigger(ctx$fsm_trigger() + 1)
      
      message("➡️ NEXT CLICK → FSM TRIGGERED")
    })
  }
  
  shiny::shinyApp(ui, server)
}
