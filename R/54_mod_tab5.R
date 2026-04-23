#' Module UI for Tree Metadata Tab
#'
#' @param id The module ID.
#' @return A nav_panel UI for the Tree tab.
#'
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @export
mod_tab5_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = htmltools::div(id = ns("tree_tab"), "Tree"),
    value = "Tree",

    shiny::fluidRow(
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_header(NULL),
          bslib::card_body(
            shiny::actionButton(ns("save_tree"),
              label = htmltools::tagList(bsicons::bs_icon("save"), "Save"),
              class = "btn-primary")
          )
        )
      ),
      shiny::column(
        11, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Tree Metadata"),
          bslib::card_body(rhandsontable::rHandsontableOutput(ns("tbl4")))
        )
      )
    )
  )
}

#' Module Server for Tree Metadata Tab
#'
#' @param id The module ID.
#' @param ctx The centralized app context (environment).
#' @param session The parent Shiny session.
#'
#' @import shiny
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r
#' @importFrom dplyr filter left_join mutate select ends_with
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @export
mod_tab5_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {

    # =====================================================
    # 1. DATA INIT
    # =====================================================
    dtree <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(ctx$files$wb_meta)
      df <- openxlsx::readWorkbook(ctx$files$wb_meta, sheet = "tree",
                                   startRow = 1, colNames = TRUE)[-(1:6), ] |>
        tibble::tibble()
      dtree(df)
    })

    shiny::observe({
      ctx$data$tbl4 <- dtree()
    })

    # =====================================================
    # 2. SPECIES SYNC
    # =====================================================
    species_family <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(ctx$files$wb_meta)
      df <- openxlsx::readWorkbook(ctx$files$wb_meta, sheet = "DropList", colNames = TRUE) |>
        dplyr::select(tree_species, species_code, phylogenetic_group, leaf_habit, tree_ring_structure) |>
        data.frame(stringsAsFactors = FALSE)
      species_family(df)
    })

    sync_species_code <- function(df, species_family, remove_na = TRUE) {
      if (remove_na) df <- df |> dplyr::filter(!is.na(tree_species) & tree_species != "")
      df |>
        dplyr::left_join(species_family, by = "tree_species", suffix = c("", "_from_list")) |>
        dplyr::mutate(
          species_code      = species_code_from_list,
          phylogenetic_group = phylogenetic_group_from_list,
          leaf_habit        = leaf_habit_from_list,
          tree_ring_structure = tree_ring_structure_from_list
        ) |>
        dplyr::select(-dplyr::ends_with("_from_list"))
    }

    # =====================================================
    # 3. RENDER
    # =====================================================
    output$tbl4 <- rhandsontable::renderRHandsontable({
      shiny::req(ctx$data$tbl4)
      col_cfg <- ctx$data$column_configs

      rhandsontable::rhandsontable(
        ctx$data$tbl4,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = "all"
      ) |>
        hot_col_wrapper("site_label",                            col_cfg$tbl4$site_label) |>
        hot_col_wrapper("tree_label",                            col_cfg$tbl4$tree_label) |>
        hot_col_wrapper("suggested_tree_code",                   col_cfg$tbl4$suggested_tree_code) |>
        hot_col_wrapper("plot_label",                            col_cfg$tbl4$plot_label) |>
        hot_col_wrapper("suggested_plot_code",                   col_cfg$tbl4$suggested_plot_code) |>
        hot_col_wrapper("tree_species",                          col_cfg$tbl4$tree_species) |>
        hot_col_wrapper("species_code",                          col_cfg$tbl4$species_code) |>
        hot_col_wrapper("phylogenetic_group",                    col_cfg$tbl4$phylogenetic_group) |>
        hot_col_wrapper("leaf_habit",                            col_cfg$tbl4$leaf_habit) |>
        hot_col_wrapper("tree_ring_structure",                   col_cfg$tbl4$tree_ring_structure) |>
        hot_col_wrapper("tree_treatment",                        col_cfg$tbl4$tree_treatment) |>
        hot_col_wrapper("tree_sampling_pattern",                 col_cfg$tbl4$tree_sampling_pattern) |>
        hot_col_wrapper("tree_dbh",                              col_cfg$tbl4$tree_dbh) |>
        hot_col_wrapper("tree_height",                           col_cfg$tbl4$tree_height) |>
        hot_col_wrapper("tree_age",                              col_cfg$tbl4$tree_age) |>
        hot_col_wrapper("tree_sex",                              col_cfg$tbl4$tree_sex) |>
        hot_col_wrapper("tree_social_status",                    col_cfg$tbl4$tree_social_status) |>
        hot_col_wrapper("tree_health_status",                    col_cfg$tbl4$tree_health_status) |>
        hot_col_wrapper("tree_origin",                           col_cfg$tbl4$tree_origin) |>
        hot_col_wrapper("tree_latitude",                         col_cfg$tbl4$tree_latitude) |>
        hot_col_wrapper("tree_longitude",                        col_cfg$tbl4$tree_longitude) |>
        hot_col_wrapper("on_tree_dendrometer_monitoring",        col_cfg$tbl4$on_tree_dendrometer_monitoring) |>
        hot_col_wrapper("on_tree_sapflux_monitoring",            col_cfg$tbl4$on_tree_sapflux_monitoring) |>
        hot_col_wrapper("on_tree_primary_phenological_observation", col_cfg$tbl4$on_tree_primary_phenological_observation) |>
        hot_col_wrapper("on_tree_weather_monitoring",            col_cfg$tbl4$on_tree_weather_monitoring) |>
        hot_col_wrapper("on_tree_shoot_growth_monitoring",       col_cfg$tbl4$on_tree_shoot_growth_monitoring) |>
        hot_col_wrapper("tree_ring_width_data",                  col_cfg$tbl4$tree_ring_width_data) |>
        hot_col_wrapper("tree_ring_density_data",                col_cfg$tbl4$tree_ring_density_data) |>
        hot_col_wrapper("tree_ring_anatomical_data",             col_cfg$tbl4$tree_ring_anatomical_data) |>
        hot_col_wrapper("tree_ring_isotope_data",                col_cfg$tbl4$tree_ring_isotope_data) |>
        hot_col_wrapper("number_of_samples",                     col_cfg$tbl4$number_of_samples) |>
        hot_col_wrapper("tree_comment",                          col_cfg$tbl4$tree_comment)
    })

    # =====================================================
    # 4. SYNC
    # =====================================================
    shiny::observeEvent(input$tbl4, {
      shiny::req(input$tbl4)
      user_data    <- rhandsontable::hot_to_r(input$tbl4)
      updated      <- sync_species_code(user_data, species_family())
      ctx$data$tbl4 <- updated
    })

    # =====================================================
    # 5. SAVE
    # =====================================================
    shiny::observeEvent(input$save_tree, {
      save_and_validate(
        data_reactive = ctx$data$tbl4,
        sheet_name    = "tree",
        wb_reactive   = shiny::reactive(ctx$files$wb_meta),
        temp_folder   = ctx$files$temp_folder
      )
    })

  })
}
