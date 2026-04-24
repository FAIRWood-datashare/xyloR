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
    # FSM STATE
    # ======================================================
    ctx$fsm <- list(
      state = "tab1",
      events = list()
    )
    
    ctx$fsm_trigger <- reactiveVal(0)
    
    # ======================================================
    # 🔒 SAFE WRITE GUARD (STEP 1)
    # ======================================================
    safe_write <- function(name, value, allow_overwrite = FALSE) {
      
      locked_fields <- c("obs_truth", "tbl1")
      
      if (allow_overwrite) {
        ctx$data[[name]] <- value
        return(invisible(TRUE))
      }
      
      if (name %in% locked_fields && !is.null(ctx$data[[name]])) {
        message("⚠️ blocked overwrite of ctx$data$", name)
        return(invisible(FALSE))
      }
      
      ctx$data[[name]] <- value
      invisible(TRUE)
    }
    
    # ======================================================
    # 📦 BOOT LAYER (SINGLE SOURCE OF TRUTH INIT)
    # ======================================================
    ctx$boot_done <- FALSE
    
    observe({
      
      req(ctx$files$obs_file)
      req(ctx$files$meta_file)
      
      if (isTRUE(ctx$boot_done)) return()
      
      message("🚀 BOOT: ONE-TIME initialization")
      
      # -------------------------
      # OBS LOAD
      # -------------------------
      obs_raw <- openxlsx::readWorkbook(
        ctx$files$obs_file$datapath,
        sheet = "Xylo_obs_data",
        startRow = 1
      )[-(1:6), ] |> tibble::tibble()
      
      drop_species <- openxlsx::readWorkbook(
        ctx$files$obs_file$datapath,
        sheet = "DropList",
        colNames = TRUE
      ) |> dplyr::select(tree_species, species_code)
      
      obs_raw <- obs_raw |>
        dplyr::left_join(drop_species, by = "tree_species") |>
        dplyr::relocate(species_code, .after = tree_species)
      
      # -------------------------
      # META LOAD
      # -------------------------
      meta_raw <- openxlsx::readWorkbook(
        ctx$files$meta_file$datapath,
        sheet = "site"
      )
      
      # -------------------------
      # GLOBAL STATE (SAFE WRITE)
      # -------------------------
      safe_write("obs_truth", obs_raw)
      safe_write("draft_obs", obs_raw)
      safe_write("tbl1", meta_raw)
      
      safe_write("obs_raw", obs_raw)
      safe_write("meta_raw", meta_raw)
      
      ctx$boot_done <- TRUE
      
      message("✅ BOOT COMPLETE")
    })
    
    # ======================================================
    # 📊 CENTRAL VALIDATION ENGINE
    # ======================================================
    ctx$validation <- reactive({
      
      req(ctx$data$obs_truth)
      req(ctx$data$tbl1)
      
      invalidateLater(100, session)  # 🔥 forces re-check across modules
      
      xylo_validation_engine(
        ctx$data$obs_truth,
        ctx$data$tbl1
      )
    })
    
    # observe({
    #   ctx$state$validation <- ctx$validation()
    # })
    
    # ======================================================
    # 🔁 FSM SNAPSHOT
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
    # FSM TRANSITION ENGINE
    # ======================================================
    fsm_transition <- function(ctx) {
      
      snapshot <- get_fsm_snapshot(ctx)
      
      if (ctx$fsm$state == "tab1" && isTRUE(snapshot$tab1_complete)) {
        ctx$fsm$state <- "tab2"
        message("➡️ FSM: tab1 → tab2")
        return()
      }
      
      if (ctx$fsm$state == "tab2" && isTRUE(snapshot$tab2_complete)) {
        ctx$fsm$state <- "tab3"
        message("➡️ FSM: tab2 → tab3")
        return()
      }
    }
    
    observeEvent(ctx$fsm_trigger(), {
      fsm_transition(ctx)
    })
    
    observeEvent(ctx$fsm_trigger(), {
      
      state <- ctx$fsm$state
      
      bslib::nav_select(
        id = "tabs",
        selected = state,
        session = session
      )
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
  }
  shiny::shinyApp(ui, server)
}