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
xyloR.App <- function() {
# ─── Load Helpers ─────────────────────────────────────────────────────────────
source("R/helpers.R")

# ─── 1) Build the UI ──────────────────────────────────────────────────────────
ui <- shiny::fluidPage(
  theme = bslib::bs_theme(
    bootswatch = "darkly",
    primary    = "#375A7F",
    secondary  = "#3498DB",
    font_scale = 0.8,
    success    = "#00BC8C",
    warning    = "#F39C12",
    danger     = "#E74C3C",
    info       = "#3498DB"
  ),
  htmltools::tags$head(
    htmltools::tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    htmltools::tags$script(src = "https://unpkg.com/tippy.js@6")
  ),
  htmltools::includeCSS("www/custom_styles.css"),
  htmltools::includeScript("www/custom_scripts.js"),
  shinyjs::useShinyjs(),
  shiny::titlePanel("Welcome to the GloboXylo data collector"),
  htmltools::tags$div(
    "This application allows you to prepare your GloboXylo data efficiently. ",
    "Get the template, upload your data, visualize the structure, validate the requirement ",
    "and export results easily. Follow the instructions below to be guided along the process.",
    style = "font-size:16px; color:#666; margin-bottom:20px;"
  ),
  bslib::navset_card_tab(
    id = "tabs",
    mod_tab1_ui("tab1"),
    mod_tab2_ui("tab2"),
    mod_tab3_ui("tab3"),
    mod_tab4_ui("tab4"),
    mod_tab5_ui("tab5"),
    mod_tab6_ui("tab6"),
    mod_tab7_ui("tab7"),
    mod_tab8_ui("tab8")
  )
)

# ─── 2) Build the server ──────────────────────────────────────────────────────
server <- function(input, output, session) {
  out_tab1 <- mod_tab1_server("tab1")
  out_tab2 <- mod_tab2_server("tab2", out_tab1)
  out_tab3 <- mod_tab3_server("tab3", out_tab1, out_tab2)
  out_tab4 <- mod_tab4_server("tab4", out_tab1, out_tab2, out_tab3)
  out_tab5 <- mod_tab5_server("tab5", out_tab1, out_tab2, out_tab3, out_tab4)
  out_tab6 <- mod_tab6_server("tab6", out_tab1, out_tab2, out_tab3, out_tab4)
  out_tab7 <- mod_tab7_server("tab7", out_tab1, out_tab2, out_tab3, out_tab4)
  out_tab8 <- mod_tab8_server("tab8", out_tab1, out_tab2, out_tab3, out_tab4)
}

# ─── 3) Launch the app ────────────────────────────────────────────────────────
shiny::shinyApp(ui, server)
}
