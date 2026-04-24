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
    
    message("🟢 TAB3 — CLEAN SINGLE SOURCE OF TRUTH")
    
    # =====================================================
    # 1. WORKBOOK LOADERS
    # =====================================================
    WB <- shiny::reactive({
      shiny::req(ctx$files$obs_file)
      openxlsx::loadWorkbook(ctx$files$obs_file$datapath)
    })
    
    WB_meta <- shiny::reactive({
      shiny::req(ctx$files$meta_file)
      openxlsx::loadWorkbook(ctx$files$meta_file$datapath)
    })
    
    shiny::observe({
      shiny::req(WB())
      ctx$files$wb_obs <- WB()
    })
    
    shiny::observe({
      shiny::req(WB_meta())
      ctx$files$wb_meta <- WB_meta()
    })
    
    # =====================================================
    # 2. TAB ENABLE/DISABLE VIA FSM
    # =====================================================
    tab_ids <- c("Observations", "Site", "Tree", "Sample", "Person", "Publication")
    lapply(tab_ids, function(tid)
      shinyjs::disable(selector = sprintf("a[data-value='%s']", tid))
    )
    
    shiny::observe({
      shiny::req(WB_meta())
      try(openxlsx::readWorkbook(WB_meta(), sheet = "site"), silent = TRUE)
      
      lapply(tab_ids, function(tid)
        shinyjs::enable(selector = sprintf("a[data-value='%s']", tid))
      )
    })
    
    # =====================================================
    # 3. SPECIES DROP LIST
    # =====================================================
    drop_species <- shiny::reactive({
      shiny::req(WB())
      openxlsx::readWorkbook(WB(), sheet = "DropList", colNames = TRUE) |>
        dplyr::select(tree_species, species_code) |>
        dplyr::filter(!is.na(tree_species))
    })
    
    # =====================================================
    # 4. SITE INFO (tbl1)
    # =====================================================
    dinfo <- shiny::reactiveVal()
    
    shiny::observe({
      shiny::req(WB())
      
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
      
      dinfo(site_info)
    })
    
    # =====================================================
    # 5. INITIALIZE OBS TRUTH (NO dobs anymore)
    # =====================================================
    shiny::observe({
      shiny::req(WB())
      
      # 🚫 prevent overwriting user edits
      if (!is.null(ctx$data$obs_truth)) return()
      
      data <- openxlsx::readWorkbook(
        WB(),
        sheet = "Xylo_obs_data",
        startRow = 1
      )[-(1:6), ] |>
        tibble::tibble() |>
        dplyr::left_join(drop_species(), by = "tree_species") |>
        dplyr::relocate(species_code, .after = tree_species) |>
        dplyr::mutate(
          sample_date = dplyr::case_when(
            !is.na(sample_date) &
              is.numeric(suppressWarnings(as.numeric(sample_date))) ~
              as.Date(as.numeric(sample_date), origin = "1899-12-30"),
            TRUE ~ as.Date(NA)
          ),
          sample_date = as.character(sample_date)
        )
      
      # ✅ SINGLE SOURCE OF TRUTH
      ctx$data$obs_truth <- data
      
      message("✅ OBS initialized into ctx$data$obs_truth")
    })
    
    # =====================================================
    # 6. SINGLE SOURCE OF TRUTH (NO MIRROR LAYER)
    # =====================================================
    shiny::observe({
      
      req(WB())
      
      if (!is.null(ctx$data$obs_truth)) return()
      
      data <- openxlsx::readWorkbook(WB(), sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] |>
        tibble::tibble() |>
        dplyr::left_join(drop_species(), by = "tree_species") |>
        dplyr::relocate(species_code, .after = tree_species)
      
      if (is.null(ctx$data$draft_obs)) {
        ctx$data$draft_obs <- ctx$data$obs_truth
      }
    })
    
    # =====================================================
    # 7. COLUMN CONFIGS
    # =====================================================
    column_configs <- shiny::reactive({
      shiny::req(WB(), WB_meta(), ctx$data$obs_truth)
      get_column_configs(WB(), WB_meta(), ctx$data$obs_truth)
    })
    
    shiny::observe({
      ctx$data$column_configs <- column_configs()
    })
    
    # =====================================================
    # 8. RENDER TABLES
    # =====================================================
    output$tbl1 <- rhandsontable::renderRHandsontable({
      shiny::req(ctx$data$tbl1)
      
      rhandsontable::rhandsontable(
        ctx$data$tbl1,
        rowHeaders = NULL,
        contextMenu = TRUE,
        stretchH = "all"
      ) |>
        hot_col_wrapper("site_label", column_configs()$tbl1$site_label) |>
        hot_col_wrapper("latitude", column_configs()$tbl1$latitude) |>
        hot_col_wrapper("longitude", column_configs()$tbl1$longitude) |>
        hot_col_wrapper("elevation", column_configs()$tbl1$elevation)
    })
    
    output$tbl2 <- rhandsontable::renderRHandsontable({
      shiny::req(ctx$data$draft_obs)
      
      rhandsontable::rhandsontable(
        ctx$data$draft_obs,
        rowHeaders = NULL,
        contextMenu = TRUE,
        stretchH = "all",
        height = "300px"
      ) |>
        hot_col_wrapper("sample_date", column_configs()$tbl2$sample_date) |>
        hot_col_wrapper("sample_id", column_configs()$tbl2$sample_id) |>
        hot_col_wrapper("tree_species", column_configs()$tbl2$tree_species) |>
        hot_col_wrapper("species_code", column_configs()$tbl2$species_code) |>
        hot_col_wrapper("tree_label", column_configs()$tbl2$tree_label) |>
        hot_col_wrapper("plot_label", column_configs()$tbl2$plot_label) |>
        hot_col_wrapper("site_label", column_configs()$tbl2$site_label) |>
        hot_col_wrapper("network_label", column_configs()$tbl2$network_label) |>
        hot_col_wrapper("sample_label", column_configs()$tbl2$sample_label) |>
        hot_col_wrapper("measure_type", column_configs()$tbl2$measure_type) |>
        hot_col_wrapper("measure_repetition", column_configs()$tbl2$measure_repetition) |>
        hot_col_wrapper("sample_comment", column_configs()$tbl2$sample_comment) |>
        rhandsontable::hot_cols(manualColumnResize = TRUE)
    })
    
    # =====================================================
    # SPECIES SYNC HELPER
    # =====================================================
    sync_species_itrdb <- function(df, drop_species, remove_na = TRUE) {
      if (remove_na) {
        df <- df |> dplyr::filter(!is.na(tree_species) & tree_species != "")
      }
      
      df |>
        dplyr::left_join(drop_species, by = "species_code", suffix = c("", "_from_list")) |>
        dplyr::mutate(tree_species = tree_species_from_list) |>
        dplyr::select(-dplyr::ends_with("_from_list"))
    }
    
    # =====================================================
    # 9. TABLE SYNC
    # =====================================================
    shiny::observeEvent(input$tbl1, {
      shiny::req(input$tbl1)
      ctx$data$tbl1 <- rhandsontable::hot_to_r(input$tbl1)
    })
    
    shiny::observeEvent(input$tbl2, {
      shiny::req(input$tbl2)
      
      user_data <- rhandsontable::hot_to_r(input$tbl2)
      updated   <- sync_species_itrdb(user_data, drop_species())
      
      # 🔥 EDIT BUFFER UPDATE (NOT COMMIT)
      ctx$data$draft_obs <- updated
    })
    
    # =====================================================
    # 10. SAVE
    # =====================================================
    shiny::observeEvent(input$save_obs, {
      
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
    # 11. FSM FLAG
    # =====================================================
    shiny::observe({
      ctx$fsm$flags$tab3_complete <- !is.null(ctx$data$tbl1) &&
        !is.null(ctx$data$obs_truth) &&
        !is.null(ctx$data$draft_obs)
    })
    
  })
}

