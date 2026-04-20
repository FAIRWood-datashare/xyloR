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
    # MODULES (NO NAVIGATION INSIDE MODULES)
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
    # NAVIGATION ENGINE 
    # ======================================================
    navigation_engine(ctx, session)
  }
  
  shiny::shinyApp(ui, server)
}
