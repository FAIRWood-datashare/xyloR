#' mod_tab3 UI Function
#'
#' @description A shiny module for the "Observation" tab.
#'
#' @param id A string that serves as the module namespace identifier.
#' @return A `shiny.tag.list` containing the UI elements of the module.
#'
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#'
mod_tab3_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "Observations",
    value = "tab3",

    shiny::fluidRow(

      # LEFT ACTION PANEL
      shiny::column(
        2,
        class = "bg-light p-2 border-end",
        bslib::card(
          bslib::card_body(
            shiny::actionButton(
              ns("save_obs"),
              label = htmltools::tagList(bsicons::bs_icon("save"), "Save"),
              class = "btn-primary"
            )
          )
        )
      ),

      # MAIN PANEL
      shiny::column(
        10,
        bslib::card(
          bslib::card_header("Basic Info"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl1"))
          )
        ),
        bslib::card(
          bslib::card_header("Observation table"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl2"))
          )
        )
      )
    )
  )
}

#' mod_tab3 Server Function
#'
#' @description Server logic for the "Observation" tab module.
#'
#' @param id A string that serves as the module namespace identifier.
#' @param ctx The centralized app context (environment).
#' @param session The parent Shiny session.
#'
#' @return A list with WB, WB_meta, data_in, column_configs (for downstream tabs).
#' @export
#'
#' @import shiny
#' @importFrom shinyjs enable disable
#' @importFrom openxlsx loadWorkbook readWorkbook saveWorkbook
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#'
mod_tab3_server <- function(id, ctx, session) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    contract_tab3 <- function(ctx) {
      
      obs  <- ctx$data$obs_truth
      meta <- ctx$data$meta
      
      data_ok <- is.data.frame(obs) &&
        is.data.frame(meta)
      
      ui_ok <- TRUE
      
      engine_ok <- TRUE
      
      ready <- data_ok && ui_ok && engine_ok
      
      list(
        ready = ready,
        data_ok = data_ok,
        ui_ok = ui_ok,
        engine_ok = engine_ok
      )
    }
    
    message("🟢 TAB3 — CLEAN ARCHITECTURE (STEP 1–3 APPLIED)")
    
    # =====================================================
    # 1. WORKBOOK LOADERS (READ ONLY, NO SIDE EFFECTS)
    # =====================================================
    WB <- reactive({
      req(ctx$files$obs_file)
      openxlsx::loadWorkbook(ctx$files$obs_file$datapath)
    })
    
    WB_meta <- reactive({
      req(ctx$files$meta_file)
      openxlsx::loadWorkbook(ctx$files$meta_file$datapath)
    })
    
    # =====================================================
    # 2. TAB ENABLE/DISABLE (SINGLE CONTROL FLOW)
    # =====================================================
    tab_ids <- c("Observations", "Site", "Tree", "Sample", "Person", "Publication")
    
    # initial state (runs once safely)
    observe({
      lapply(tab_ids, function(tid)
        shinyjs::disable(selector = sprintf("a[data-value='%s']", tid))
      )
    })
    
    # enable only when workbook is valid
    observe({
      req(WB_meta())
      
      ok <- try(openxlsx::readWorkbook(WB_meta(), sheet = "site"), silent = TRUE)
      if (inherits(ok, "try-error")) return()
      
      lapply(tab_ids, function(tid)
        shinyjs::enable(selector = sprintf("a[data-value='%s']", tid))
      )
    })
    
    # =====================================================
    # 3. DROP LIST (PURE REACTIVE)
    # =====================================================
    drop_species <- reactive({
      req(WB())
      
      openxlsx::readWorkbook(WB(), sheet = "DropList", colNames = TRUE) |>
        dplyr::select(tree_species, species_code) |>
        dplyr::filter(!is.na(tree_species))
    })
    
    # =====================================================
    # 4. SITE INFO (SINGLE SOURCE OF TRUTH)
    # =====================================================
    observe({
      req(WB())
      
      site_info <- openxlsx::readWorkbook(
        WB(),
        sheet = "obs_data_info",
        startRow = 6,
        colNames = FALSE
      ) |> tibble::tibble()
      
      obs_data <- openxlsx::readWorkbook(
        WB(),
        sheet = "Xylo_obs_data",
        startRow = 1
      )[-(1:6), ] |> tibble::tibble()
      
      if (ncol(site_info) == 3) {
        site_label <- unique(obs_data$site_label) |>
          tibble::tibble() |>
          dplyr::filter(!is.na(.))
        
        site_info <- cbind(site_label, site_info)
        
      } else if (ncol(site_info) != 4) {
        stop("Expected 3 or 4 columns in obs_data_info.")
      }
      
      site_info <- setNames(
        site_info,
        c("site_label", "latitude", "longitude", "elevation")
      ) |>
        dplyr::mutate(elevation = as.integer(elevation))
      
      ctx$data$site_info <- site_info
    })
    
    # =====================================================
    # 5. INITIAL OBS LOAD (ONE-TIME ONLY)
    # =====================================================
    observe({
      req(WB())
      
      if (!is.null(ctx$data$obs_truth)) return()
      
      data <- openxlsx::readWorkbook(
        WB(),
        sheet = "Xylo_obs_data",
        startRow = 1
      )[-(1:6), ] |>
        tibble::tibble() |>
        dplyr::left_join(drop_species(), by = "tree_species") |>
        dplyr::relocate(species_code, .after = tree_species)
      
      ctx$data$obs_truth <- data
      ctx$data$draft_obs <- data
      
      message("✅ TAB3: obs_truth initialized")
    })
    
    # =====================================================
    # 6. COLUMN CONFIGS (PURE DERIVATION)
    # =====================================================
    column_configs <- reactive({
      req(WB(), WB_meta(), ctx$data$obs_truth)
      get_column_configs(WB(), WB_meta(), ctx$data$obs_truth)
    })
    
    observe({
      ctx$data$column_configs <- column_configs()
    })
    
    # =====================================================
    # 7. TABLE RENDERING
    # =====================================================
    output$tbl1 <- rhandsontable::renderRHandsontable({
      req(ctx$data$tbl1)
      rhandsontable::rhandsontable(ctx$data$tbl1)
    })
    
    output$tbl2 <- rhandsontable::renderRHandsontable({
      req(ctx$data$draft_obs)
      rhandsontable::rhandsontable(ctx$data$draft_obs)
    })
    
    # =====================================================
    # 8. TABLE SYNC (PURE DATA FLOW)
    # =====================================================
    observeEvent(input$tbl1, {
      req(input$tbl1)
      ctx$data$tbl1 <- rhandsontable::hot_to_r(input$tbl1)
    })
    
    observeEvent(input$tbl2, {
      req(input$tbl2)
      ctx$data$draft_obs <- rhandsontable::hot_to_r(input$tbl2)
    })
    
    # =====================================================
    # 9. SAVE (ONLY PLACE THAT COMMITS STATE)
    # =====================================================
    observeEvent(input$save_obs, {
      
      ctx$data$obs_truth <- ctx$data$draft_obs
      
      save_and_validate(
        data_reactive = ctx$data$tbl1,
        sheet_name = "obs_data_info",
        wb_reactive = WB,
        temp_folder = ctx$files$temp_folder
      )
      
      save_and_validate(
        data_reactive = ctx$data$obs_truth |> dplyr::select(-species_code),
        sheet_name = "Xylo_obs_data",
        wb_reactive = WB,
        temp_folder = ctx$files$temp_folder
      )
      
      message("💾 TAB3 SAVE COMPLETE")
    })
    
    # =====================================================
    # RETURN
    # =====================================================
    return(invisible(NULL))
  })
}

