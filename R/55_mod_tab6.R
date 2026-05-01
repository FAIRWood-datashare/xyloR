#' Module UI for Sample Metadata Tab
#'
#' @param id A string representing the namespace for the UI components.
#' @return A nav_panel for the Sample tab.
#'
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @export
mod_tab6_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = htmltools::div(id = ns("sample_tab"), "Sample"),
    value = "Sample",

    shiny::fluidRow(
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_header(NULL),
          bslib::card_body(
            shiny::actionButton(ns("save_sample"),
              label = htmltools::tagList(bsicons::bs_icon("save"), "Save"),
              class = "btn-primary")
          )
        )
      ),
      shiny::column(
        11, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Sample Metadata"),
          bslib::card_body(rhandsontable::rHandsontableOutput(ns("tbl5")))
        )
      )
    )
  )
}

#' Server function for Sample Metadata Tab
#'
#' @param id A string representing the namespace for the module's server logic.
#' @param ctx The centralized app context (environment).
#' @param session The parent Shiny session.
#'
#' @import shiny
#' @importFrom rhandsontable renderRHandsontable hot_to_r
#' @importFrom dplyr mutate case_when
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @export
mod_tab6_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {
    
    # =====================================================
    # 1. DATA INIT
    # =====================================================
    dsample <- shiny::reactiveVal()
    
    shiny::observe({
      
      req(ctx$files$wb_meta)
      
      df <- openxlsx::readWorkbook(
        ctx$files$wb_meta,
        sheet = "sample",
        startRow = 1,
        colNames = TRUE
      )[-(1:6), ] |>
        tibble::tibble() |>
        dplyr::mutate(
          sample_date = dplyr::case_when(
            !is.na(sample_date) &
              !is.na(suppressWarnings(as.numeric(sample_date))) ~
              as.Date(as.numeric(sample_date), origin = "1899-12-30"),
            TRUE ~ as.Date(NA)
          ),
          sample_date = as.character(sample_date)
        )
      
      dsample(df)
    })
    
    shiny::observe({
      ctx$data$tbl5 <- dsample()
    })
    
    # =====================================================
    # 2. RENDER
    # =====================================================
    output$tbl5 <- rhandsontable::renderRHandsontable({
      
      req(ctx$data$tbl5)
      req(ctx$data$column_configs)
      
      col_cfg <- ctx$data$column_configs
      
      rhandsontable::rhandsontable(
        ctx$data$tbl5,
        rowHeaders = NULL,
        contextMenu = TRUE,
        stretchH = "all"
      ) |>
        hot_col_wrapper("tree_label",              col_cfg$tbl5$tree_label) |>
        hot_col_wrapper("sample_id",               col_cfg$tbl5$sample_id) |>
        hot_col_wrapper("sample_date",             col_cfg$tbl5$sample_date) |>
        hot_col_wrapper("sample_label",            col_cfg$tbl5$sample_label) |>
        hot_col_wrapper("suggested_sample_code",   col_cfg$tbl5$suggested_sample_code) |>
        hot_col_wrapper("sample_organ",            col_cfg$tbl5$sample_organ) |>
        hot_col_wrapper("sample_type",             col_cfg$tbl5$sample_type) |>
        hot_col_wrapper("sample_embedding",        col_cfg$tbl5$sample_embedding) |>
        hot_col_wrapper("sample_staining_method",  col_cfg$tbl5$sample_staining_method) |>
        hot_col_wrapper("sample_mounting_method",  col_cfg$tbl5$sample_mounting_method) |>
        hot_col_wrapper("sample_observation_method", col_cfg$tbl5$sample_observation_method) |>
        hot_col_wrapper("sample_image_file_name",  col_cfg$tbl5$sample_image_file_name) |>
        hot_col_wrapper("sample_section_archived", col_cfg$tbl5$sample_section_archived) |>
        hot_col_wrapper("sample_archived",         col_cfg$tbl5$sample_archived) |>
        hot_col_wrapper("sample_image_archived",   col_cfg$tbl5$sample_image_archived) |>
        hot_col_wrapper("sample_image_annotated",  col_cfg$tbl5$sample_image_annotated) |>
        hot_col_wrapper("sampling_height",         col_cfg$tbl5$sampling_height) |>
        hot_col_wrapper("sample_apex_distance",    col_cfg$tbl5$sample_apex_distance) |>
        hot_col_wrapper("section_thickness",       col_cfg$tbl5$section_thickness) |>
        hot_col_wrapper("coupled_anatomical_data", col_cfg$tbl5$coupled_anatomical_data) |>
        hot_col_wrapper("reaction_wood",           col_cfg$tbl5$reaction_wood) |>
        hot_col_wrapper("sample_comment",          col_cfg$tbl5$sample_comment)
    })
    
    # =====================================================
    # 3. SYNC
    # =====================================================
    shiny::observeEvent(input$tbl5, {
      
      req(input$tbl5)
      
      df <- rhandsontable::hot_to_r(input$tbl5)
      
      ctx$data$tbl5 <- df
    })
    
    # =====================================================
    # 4. SAVE
    # =====================================================
    shiny::observeEvent(input$save_sample, {
      
      req(ctx$data$tbl5)
      
      save_and_validate(
        data_reactive = ctx$data$tbl5,
        sheet_name    = "sample",
        wb_reactive   = shiny::reactive(ctx$files$wb_meta),
        temp_folder   = ctx$files$temp_folder
      )
    })
    
  })
}
