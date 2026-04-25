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
    
    shiny::titlePanel("GloboXylo Data Collector"),
    
    bslib::navset_card_tab(
      id = "tabs",
      
      bslib::nav_panel("1. Upload observation", value = "tab1", mod_tab1_ui("tab1")),
      bslib::nav_panel("2. Upload metadata", value = "tab2", mod_tab2_ui("tab2")),
      bslib::nav_panel("Observation", value = "tab3", mod_tab3_ui("tab3")),
      bslib::nav_panel("Site", value = "tab4", mod_tab4_ui("tab4")),
      bslib::nav_panel("Tree", value = "tab5", mod_tab5_ui("tab5")),
      bslib::nav_panel("Sample", value = "tab6", mod_tab6_ui("tab6")),
      bslib::nav_panel("Person", value = "tab7", mod_tab7_ui("tab7")),
      bslib::nav_panel("Publication", value = "tab8", mod_tab8_ui("tab8"))
    )
  )
  
  # ======================================================
  # SERVER
  # ======================================================
  server <- function(input, output, session) {
    
    ctx <- create_app_context()
    
    # ======================================================
    # FSM (STATE ONLY — SINGLE SOURCE OF TRUTH)
    # ======================================================
    # ctx$fsm already initialised in create_app_context() — do not re-assign here
    
    # ======================================================
    # SAFE WRITE (DATA ONLY)
    # ======================================================
    safe_write <- function(name, value, allow_overwrite = FALSE) {
      
      locked_fields <- c("obs_truth", "tbl1")
      
      if (!allow_overwrite &&
          name %in% locked_fields &&
          !is.null(ctx$data[[name]])) {
        return(invisible(FALSE))
      }
      
      ctx$data[[name]] <- value
      invisible(TRUE)
    }
    
    # ======================================================
    # BOOT — obs file (tab1): populate obs_truth immediately
    # ======================================================
    observeEvent(ctx$files$obs_file, {
      req(ctx$files$obs_file)

      obs_raw <- openxlsx::readWorkbook(
        ctx$files$obs_file$datapath,
        sheet    = "Xylo_obs_data",
        startRow = 1
      )[-(1:6), ] |> tibble::tibble()

      safe_write("obs_truth", obs_raw)
      safe_write("draft_obs", obs_raw)
      message("✅ BOOT: obs_truth loaded")
    })

    # ======================================================
    # BOOT — sync tbl1 + site_info from meta (set by mod_tab2)
    # Fires whenever ctx$data$meta is assigned in mod_tab2
    # ======================================================
    observe({
      req(is.list(ctx$data$meta), !is.null(ctx$data$meta$site))
      ctx$data$tbl1 <- ctx$data$meta$site
      ctx$data$site_info <- ctx$data$meta$site |>
        dplyr::select(dplyr::any_of(c("site_label", "latitude", "longitude"))) |>
        dplyr::distinct()
      message("✅ BOOT: tbl1 + site_info synced from ctx$data$meta$site")
    })

    # ======================================================
    # VALIDATION ENGINE (runs once when both inputs are ready — NO invalidateLater)
    # ======================================================
    ctx$validation <- reactive({
      req(is.data.frame(ctx$data$obs_truth), is.list(ctx$data$meta))
      tryCatch(
        xylo_validation_engine(ctx$data$obs_truth, ctx$data$meta),
        error = function(e) {
          message("⚠️ validation engine error: ", e$message)
          list(all = data.frame())
        }
      )
    })

    # ======================================================
    # CONTRACTS
    # ======================================================
    contract_tab1 <- function(ctx) {

      ui_ok     <- isTRUE(ctx$data$tab1_ui_valid  %||% FALSE)
      validated <- isTRUE(ctx$data$tab1_validated %||% FALSE)
      data_ok   <- is.data.frame(ctx$data$obs_truth) && nrow(ctx$data$obs_truth) > 0
      checks_ok <- isTRUE(ctx$data$tab1_checks_ok  %||% FALSE)
      complete  <- isTRUE(ctx$data$tab1_complete    %||% FALSE)

      list(
        ready     = ui_ok && validated && data_ok && checks_ok && complete,
        ui_ok     = ui_ok,
        validated = validated,
        data_ok   = data_ok,
        checks_ok = checks_ok,
        complete  = complete
      )
    }

    contract_tab2 <- function(ctx) {

      obs      <- ctx$data$obs_truth
      meta     <- ctx$data$meta
      complete <- isTRUE(ctx$data$tab2_complete %||% FALSE)  # set by next_btn in mod_tab2

      data_ok <- is.data.frame(obs) && nrow(obs) > 0 &&
                 is.list(meta)      && !is.null(meta$site)

      list(
        ready    = data_ok && complete,
        data_ok  = data_ok,
        complete = complete
      )
    }
    
    # ======================================================
    # MODULES
    # ======================================================
    tab1 <- mod_tab1_server("tab1", ctx, session)
    tab2 <- mod_tab2_server("tab2", ctx, session)
    tab3 <- mod_tab3_server("tab3", ctx, session)
    
    mod_tab4_server("tab4", ctx, session)
    mod_tab5_server("tab5", ctx, session)
    mod_tab6_server("tab6", ctx, session)
    mod_tab7_server("tab7", ctx, session)
    mod_tab8_server("tab8", ctx, session)
    
    # ======================================================
    # FSM REACTIVE DRIVER (CLEAN SINGLE SOURCE OF TRUTH)
    # ======================================================
    observe({
      
      state      <- ctx$fsm$state
      tab1_ready <- contract_tab1(ctx)$ready
      tab2_ready <- contract_tab2(ctx)$ready

      new_state <- dplyr::case_when(
        state == "tab1" && isTRUE(tab1_ready) ~ "tab2",
        state == "tab2" && isTRUE(tab2_ready) ~ "tab3",
        .default = state
      )

      # Guard: only write when state actually changes — prevents reactive loop
      if (!identical(new_state, state)) {
        message("➡️ FSM: ", state, " → ", new_state)
        ctx$fsm$state <- new_state
      }
    })
    
    # ======================================================
    # NAVIGATION (STATE ONLY)
    # ======================================================
    observe({
      
      req(ctx$fsm$state)
      
      bslib::nav_select(
        id = "tabs",
        selected = ctx$fsm$state,
        session = session
      )
    })
  }
  
  shiny::shinyApp(ui, server)
}