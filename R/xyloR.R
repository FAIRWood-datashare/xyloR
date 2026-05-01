#' GloboXylo Shiny App
#'
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
    
    htmltools::tags$head(
      htmltools::tags$script(src = "https://unpkg.com/@popperjs/core@2"),
      htmltools::tags$script(src = "https://unpkg.com/tippy.js@6")
    ),
    
    htmltools::includeCSS("www/custom_styles.css"),
    htmltools::includeScript("www/custom_scripts.js"),
    
    shiny::titlePanel("GloboXylo Data Collector"),
    
    bslib::navset_card_tab(
      id = "tabs",
      
      bslib::nav_panel("Tab1",  value = "tab1",  mod_tab1_ui("tab1")),
      bslib::nav_panel("Tab2",  value = "tab2",  mod_tab2_ui("tab2")),
      bslib::nav_panel("Tab3",  value = "tab3",  mod_tab3_ui("tab3")),
      bslib::nav_panel("Tab4",  value = "tab4",  mod_tab4_ui("tab4")),
      bslib::nav_panel("Tab5",  value = "tab5",  mod_tab5_ui("tab5")),
      bslib::nav_panel("Tab6",  value = "tab6",  mod_tab6_ui("tab6")),
      bslib::nav_panel("Tab7",  value = "tab7",  mod_tab7_ui("tab7")),
      bslib::nav_panel("Tab8",  value = "tab8",  mod_tab8_ui("tab8")),
      bslib::nav_panel("Tab9",  value = "tab9",  mod_tab9_ui("tab9")),
      bslib::nav_panel("Tab10", value = "tab10", mod_tab10_ui("tab10")),
      bslib::nav_panel("Tab11", value = "tab11", mod_tab11_ui("tab11")),
      
      bslib::nav_panel("Debug", value = "debug", mod_debug_ui("debug"))
    )
  )
  
  server <- function(input, output, session) {
    
    # =====================================================
    # CONTEXT
    # =====================================================
    ctx <- create_app_context()
    
    # =====================================================
    # 🧠 ENGINE API LAYER (FINAL FORM — CLEAN)
    # =====================================================
    
    ctx$get_engine <- function() {
      ctx$state$engine
    }
    
    ctx$has_engine <- function() {
      !is.null(ctx$state$engine)
    }
    
    ctx$set_engine <- function(engine) {
      ctx$state$engine <- engine
    }
    
    ctx$invalidate_engine <- function(trigger = NULL) {
      
      ctx$state$engine <- NULL
      ctx$state$engine_tick <- Sys.time()
      
      ctx$state$qa_ready <- FALSE
      ctx$state$export_ready <- FALSE
      
      ctx$debug$last_invalidation <- list(
        time = Sys.time(),
        trigger = trigger
      )
    }
    
    ctx$rebuild_engine <- function() {
      
      req(ctx$data$obs_raw)
      req(ctx$data$site_info)
      
      candidate <- update_state_engine(
        ctx$data$obs_raw,
        ctx$data$site_info,
        ctx$data$tree_info,
        ctx$data$sample_info
      )
      
      if (!validate_engine_structure(candidate)) return()
      
      ctx$set_engine(candidate)
    }
    
    # =====================================================
    # ENGINE AUTO REBUILD OBSERVER
    # =====================================================
    shiny::observe({
      ctx$rebuild_engine()
    })
    
    # =====================================================
    # NAVIGATION
    # =====================================================
    shiny::observe({
      
      req(ctx$state$stage)
      
      bslib::nav_select(
        id       = "tabs",
        selected = ctx$state$stage,
        session  = session
      )
    })
    
    # =====================================================
    # HISTORY TRACKING
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
    
    last_stage <- shiny::reactiveVal(NULL)
    
    shiny::observe({
      
      req(ctx$state$stage)
      
      if (!identical(ctx$state$stage, last_stage())) {
        log_transition(last_stage(), ctx$state$stage, "stage_change")
        last_stage(ctx$state$stage)
      }
    })
    
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
    # DEBUG OUTPUTS
    # =====================================================
    output$debug_stage <- shiny::renderText({
      paste0("stage: ", ctx$state$stage)
    })
    
    output$history_table <- DT::renderDataTable({
      DT::datatable(ctx$debug$history(), options = list(pageLength = 5))
    })
    
    output$graph <- visNetwork::renderVisNetwork({
      
      tabs <- paste0("tab", 1:4)
      
      nodes <- data.frame(
        id = tabs,
        label = paste("Tab", 1:4),
        color = ifelse(tabs == ctx$state$stage, "#00C853", "#2C3E50"),
        shape = "box"
      )
      
      edges <- data.frame(
        from = tabs[1:3],
        to   = tabs[2:4],
        arrows = "to"
      )
      
      visNetwork::visNetwork(nodes, edges) |>
        visNetwork::visNodes(font = list(color = "white")) |>
        visNetwork::visEdges(color = "#888")
    })
  }
  
  shiny::shinyApp(ui, server)
}