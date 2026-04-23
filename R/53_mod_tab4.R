#' Site Metadata Tab UI
#'
#' @param id The module ID.
#' @return A nav_panel UI for the Site tab.
#' 
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' 
mod_tab4_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = htmltools::div(id = ns("site_tab"), "Site"),
    value = "Site",

    shiny::fluidRow(
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_header(NULL),
          bslib::card_body(
            shiny::actionButton(ns("save_site"),
              label = htmltools::tagList(bsicons::bs_icon("save"), "Save"),
              class = "btn-primary")
          )
        )
      ),
      shiny::column(
        11, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Site Metadata"),
          bslib::card_body(rhandsontable::rHandsontableOutput(ns("tbl3")))
        )
      )
    )
  )
}

#' Server logic for Site Metadata Tab
#'
#' @param id The module ID.
#' @param ctx The centralized app context (environment).
#' @param session The parent Shiny session.
#'
#' @return A list with data_meta for downstream tabs.
#'
#' @import shiny
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @importFrom dplyr select mutate filter left_join ends_with
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r
#'
mod_tab4_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {

    # =====================================================
    # 1. DATA INIT
    # =====================================================
    dsite <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(ctx$files$wb_meta)

      df <- openxlsx::readWorkbook(ctx$files$wb_meta, sheet = "site",
                                   startRow = 1, colNames = TRUE)[-(1:6), ] |>
        tibble::tibble()
      dsite(df)
    })

    data_meta <- shiny::reactiveValues(tbl3 = NULL)

    shiny::observe({
      data_meta$tbl3 <- dsite()
      ctx$data$tbl3  <- dsite()
    })

    # =====================================================
    # 2. KOPPEN SYNC
    # =====================================================
    koppen_family <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(ctx$files$wb_meta)
      df <- openxlsx::readWorkbook(ctx$files$wb_meta, sheet = "DropList", colNames = TRUE) |>
        dplyr::select(koppen_climate_value, koppen_climate_code, koppen_climate_classification) |>
        dplyr::mutate(koppen_climate_value = as.character(koppen_climate_value)) |>
        data.frame(stringsAsFactors = FALSE)
      koppen_family(df)
    })

    sync_koppen_code <- function(df, koppen_family, remove_na = TRUE) {
      df <- df |> dplyr::mutate(koppen_climate_value = as.character(koppen_climate_value))
      if (remove_na) {
        df <- df |> dplyr::filter(!is.na(koppen_climate_value) & koppen_climate_value != "")
      }
      df |>
        dplyr::left_join(
          koppen_family |> dplyr::mutate(koppen_climate_value = as.character(koppen_climate_value)),
          by = "koppen_climate_value", suffix = c("", "_from_list")
        ) |>
        dplyr::mutate(
          koppen_climate_code           = koppen_climate_code_from_list,
          koppen_climate_classification = koppen_climate_classification_from_list
        ) |>
        dplyr::select(-dplyr::ends_with("_from_list"))
    }

    # =====================================================
    # 3. RENDER
    # =====================================================
    output$tbl3 <- rhandsontable::renderRHandsontable({
      shiny::req(data_meta$tbl3)
      col_cfg <- ctx$data$column_configs

      rhandsontable::rhandsontable(
        data_meta$tbl3,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = "all", height = 150
      ) |>
        hot_col_wrapper("network_label",                          col_cfg$tbl3$network_label) |>
        hot_col_wrapper("suggested_network_code",                 col_cfg$tbl3$suggested_network_code) |>
        hot_col_wrapper("site_country_code",                      col_cfg$tbl3$site_country_code) |>
        hot_col_wrapper("site_label",                             col_cfg$tbl3$site_label) |>
        hot_col_wrapper("suggested_site_code",                    col_cfg$tbl3$suggested_site_code) |>
        hot_col_wrapper("plot_label",                             col_cfg$tbl3$plot_label) |>
        hot_col_wrapper("suggested_plot_code",                    col_cfg$tbl3$suggested_plot_code) |>
        hot_col_wrapper("latitude",                               col_cfg$tbl3$latitude) |>
        hot_col_wrapper("longitude",                              col_cfg$tbl3$longitude) |>
        hot_col_wrapper("elevation",                              col_cfg$tbl3$elevation) |>
        hot_col_wrapper("koppen_climate_value",                   col_cfg$tbl3$koppen_climate_value) |>
        hot_col_wrapper("koppen_climate_code",                    col_cfg$tbl3$koppen_climate_code) |>
        hot_col_wrapper("koppen_climate_classification",          col_cfg$tbl3$koppen_climate_classification) |>
        hot_col_wrapper("site_aspect",                            col_cfg$tbl3$site_aspect) |>
        hot_col_wrapper("site_slope",                             col_cfg$tbl3$site_slope) |>
        hot_col_wrapper("site_topography",                        col_cfg$tbl3$site_topography) |>
        hot_col_wrapper("temp",                                   col_cfg$tbl3$temp) |>
        hot_col_wrapper("precip",                                 col_cfg$tbl3$precip) |>
        hot_col_wrapper("soil_depth",                             col_cfg$tbl3$soil_depth) |>
        hot_col_wrapper("soil_water_holding_capacity",            col_cfg$tbl3$soil_water_holding_capacity) |>
        hot_col_wrapper("soil_moisture",                          col_cfg$tbl3$soil_moisture) |>
        hot_col_wrapper("forest_stand_composition",               col_cfg$tbl3$forest_stand_composition) |>
        hot_col_wrapper("forest_stand_structure",                 col_cfg$tbl3$forest_stand_structure) |>
        hot_col_wrapper("forest_stand_age_structure",             col_cfg$tbl3$forest_stand_age_structure) |>
        hot_col_wrapper("forest_stand_age",                       col_cfg$tbl3$forest_stand_age) |>
        hot_col_wrapper("forest_stand_main_species_composition",  col_cfg$tbl3$forest_stand_main_species_composition) |>
        hot_col_wrapper("forest_stand_management_intensity",      col_cfg$tbl3$forest_stand_management_intensity) |>
        hot_col_wrapper("in_stand_soil_description",              col_cfg$tbl3$in_stand_soil_description) |>
        hot_col_wrapper("in_stand_dendrometer_monitoring",        col_cfg$tbl3$in_stand_dendrometer_monitoring) |>
        hot_col_wrapper("in_stand_phloem_observation",            col_cfg$tbl3$in_stand_phloem_observation) |>
        hot_col_wrapper("in_stand_sapflux_monitoring",            col_cfg$tbl3$in_stand_sapflux_monitoring) |>
        hot_col_wrapper("in_stand_primary_phenological_observation", col_cfg$tbl3$in_stand_primary_phenological_observation) |>
        hot_col_wrapper("in_stand_weather_monitoring",            col_cfg$tbl3$in_stand_weather_monitoring) |>
        hot_col_wrapper("in_stand_soil_monitoring",               col_cfg$tbl3$in_stand_soil_monitoring) |>
        hot_col_wrapper("number_of_trees",                        col_cfg$tbl3$number_of_trees) |>
        hot_col_wrapper("site_comment",                           col_cfg$tbl3$site_comment)
    })

    # =====================================================
    # 4. SYNC
    # =====================================================
    shiny::observeEvent(input$tbl3, {
      shiny::req(input$tbl3)
      user_data <- rhandsontable::hot_to_r(input$tbl3)
      updated   <- sync_koppen_code(user_data, koppen_family())
      shiny::isolate({
        data_meta$tbl3 <- updated
        ctx$data$tbl3  <- updated
      })
    })

    # =====================================================
    # 5. SAVE
    # =====================================================
    shiny::observeEvent(input$save_site, {
      save_and_validate(
        data_reactive    = data_meta$tbl3,
        sheet_name       = "site",
        wb_reactive      = shiny::reactive(ctx$files$wb_meta),
        temp_folder      = ctx$files$temp_folder
      )
    })

  })
}
