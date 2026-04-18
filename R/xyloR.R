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
      
      bslib::nav_panel("1. Upload observation", "tab1", mod_tab1_ui("tab1")),
      bslib::nav_panel("2. Upload metadata", "tab2", mod_tab2_ui("tab2")),
      bslib::nav_panel("Observation", "tab3", mod_tab3_ui("tab3")),
      bslib::nav_panel("Site", "tab4", mod_tab2_ui("tab4")),
      bslib::nav_panel("Tree", "tab5", mod_tab2_ui("tab5")),
      bslib::nav_panel("Sample", "tab6", mod_tab2_ui("tab6")),
      bslib::nav_panel("Person", "tab7", mod_tab2_ui("tab7")),
      bslib::nav_panel("Publication", "tab8", mod_tab2_ui("tab8"))
    )
  )
  
  # ======================================================
  # SERVER
  # ======================================================
  server <- function(input, output, session) {
    
    ctx <- create_app_context()
    
    meta_template_r <- reactive({
      req(ctx$files$meta_template)
      ctx$files$meta_template
    })
    
    # ======================================================
    # MODULES (NO NAVIGATION INSIDE MODULES)
    # ======================================================
    mod_tab1_server("tab1", ctx, session)
    mod_tab2_server("tab2", ctx, meta_template_r)
    mod_tab3_server("tab3", ctx)
    
    mod_tab4_server("tab4", ctx)
    mod_tab5_server("tab5", ctx)
    mod_tab6_server("tab6", ctx)
    mod_tab7_server("tab7", ctx)
    mod_tab8_server("tab8", ctx)
    
    # ======================================================
    # 🔥 GLOBAL NAVIGATION CONTROLLER (THIS IS THE FIX)
    # ======================================================
    observe({
      
      req(ctx$state$tab1_ready)
      
      if (isTRUE(ctx$state$tab1_ready)) {
        
        message("➡️ NAVIGATING TO TAB2")
        
        bslib::nav_select(
          id = "tabs",
          selected = "tab2"
        )
      }
    })
    
    observe({
      
      req(ctx$state$tab2_ready)
      
      if (isTRUE(ctx$state$tab2_ready)) {
        
        message("➡️ NAVIGATING TO TAB3")
        
        bslib::nav_select(
          id = "tabs",
          selected = "tab3"
        )
      }
    })
  }
  
  shiny::shinyApp(ui, server)
}
