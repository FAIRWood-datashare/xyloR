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
    # FSM SNAPSHOT
    # ======================================================
    get_fsm_snapshot <- function(ctx) {
      
      list(
        tab1_complete = all(c(
          isTRUE(ctx$state$tab1$inputdata$valid %||% FALSE),
          isTRUE(ctx$state$tab1$file$uploaded %||% FALSE),
          isTRUE(ctx$state$tab1$file$loaded %||% FALSE),
          isTRUE(ctx$state$tab1$validation$ui_valid %||% FALSE)
        )),
        
        tab2_complete = isTRUE(ctx$state$tab2$validation$all_valid %||% FALSE)
      )
    }
    
    # ======================================================
    # FSM INIT
    # ======================================================
    ctx$fsm <- list(
      state = "tab1",
      events = list()
    )
    
    # ======================================================
    # CENTRAL VALIDATION ENGINE
    # ======================================================
    ctx$validation <- reactive({
      
      req(ctx$data$obs)
      req(ctx$data$meta)
      
      xylo_validation_engine(ctx)
    })
    
    observe({
      ctx$state$validation <- ctx$validation()
    })
    
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
    # FSM TRANSITION ENGINE
    # ======================================================
    fsm_transition <- function(ctx) {
      
      snapshot <- get_fsm_snapshot(ctx)
      
      message("🔁 FSM STATE BEFORE: ", ctx$fsm$state)
      message("📦 SNAPSHOT tab1=", snapshot$tab1_complete,
              " tab2=", snapshot$tab2_complete)
      
      # -------------------------
      # TAB1 → TAB2
      # -------------------------
      if (ctx$fsm$state == "tab1") {
        
        if (isTRUE(snapshot$tab1_complete)) {
          ctx$fsm$state <- "tab2"
          message("➡️ FSM: tab1 → tab2")
        }
        
        return()
      }
      
      # -------------------------
      # TAB2 → TAB3
      # -------------------------
      if (ctx$fsm$state == "tab2") {
        
        if (isTRUE(snapshot$tab2_complete)) {
          ctx$fsm$state <- "tab3"
          message("➡️ FSM: tab2 → tab3")
        }
        
        return()
      }
      
      message("🔁 FSM STATE AFTER: ", ctx$fsm$state)
    }
    
    # ======================================================
    # FSM TRIGGER
    # ======================================================
    ctx$fsm_trigger <- reactiveVal(0)
    
    observeEvent(ctx$fsm_trigger(), {
      fsm_transition(ctx)
    })
    
    # ======================================================
    # ROUTER (TAB SWITCHING)
    # ======================================================
    observeEvent(ctx$fsm_trigger(), {
      
      state <- ctx$fsm$state
      
      message("🧭 ROUTER STATE = ", state)
      
      req(state %in% c("tab1", "tab2", "tab3"))
      
      bslib::nav_select(
        id = "tabs",
        selected = state,
        session = session
      )
    })
    
    # ======================================================
    # NEXT BUTTON (ONLY ONE HANDLER)
    # ======================================================
    observeEvent(input$next_btn, {
      
      message("➡️ NEXT CLICK RECEIVED")
      
      ctx$fsm_trigger(ctx$fsm_trigger() + 1)
    })
  }
  
  shiny::shinyApp(ui, server)
}