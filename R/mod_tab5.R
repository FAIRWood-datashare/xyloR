#' Module UI for Tree Metadata Tab
#'
#' @param id The module ID.
#' @return A UI element (fluidRow, card layout) for displaying the tree metadata table.
#' 
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' 
#' @export
mod_tab5_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # TAB 5: View tree -----------------------------------------------
  bslib::nav_panel(
    title = div(id = ns("tree_tab"), "Tree"),
    value = "Tree",
    
    shiny::fluidRow(
      # Left side (sidebar) - Action Button
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_header(NULL),
          bslib::card_body(
            shiny::actionButton(ns("save_tree"), label = htmltools::tagList(bsicons::bs_icon("save"), "Save"), class = "btn-primary")
          )
        )
      ),
      
      
      # Right side - Main Content with Table
      shiny::column(
        11, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Tree Metadata:"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl4"))
          )
        )
      )
    )
  )
}

#' Module Server for Tree Metadata Tab
#'
#' This module handles the server-side logic for the Tree Metadata tab. It manages data
#' loading, data synchronization for species codes, and updates to the metadata table. 
#' Additionally, it handles the save functionality for the tree metadata.
#'
#' @param id The module ID.
#' @param out_tab1 A reactive object containing the dataset name and observation file.
#' @param out_tab2 A reactive object containing the metadata file and validation results.
#' @param out_tab3 A reactive object containing the workbook reference and column configurations.
#' @param out_tab4 A reactive object containing the tree metadata table and other related data.
#' @return A module server function that performs actions for the Tree Metadata tab.
#' 
#' @import shiny
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r 
#' @importFrom dplyr filter left_join mutate select ends_with
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' 
#' @export
mod_tab5_server <- function(id, out_tab1, out_tab2, out_tab3, out_tab4) {
  moduleServer(id, function(input, output, session) {
    
    # TAB 5 tree: -------------------------------------------------------------------
    

    #### dtree  ####
    dtree <- shiny::reactiveVal()
    
    shiny::observe({
      shiny::req(out_tab2$meta_file)  # Ensure file is uploaded
      
      # Read the dataset
      tree_meta_info <- openxlsx::readWorkbook(out_tab3$WB_meta(), sheet = "tree", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
        tibble::tibble() 
      dtree(tree_meta_info)  # Store in reactive value
    })
    
    # # INITALIZE REACTIVE INPUT DATA
    # data_meta <- reactiveValues()
    
    # Reactive context to store initial data and ensure it's updated in a proper context
    shiny::observe({
      out_tab4$data_meta$tbl4 <- dtree()  # Access dinfo in a valid reactive context
      
    })
    
    # Species_family
    species_family <- shiny::reactiveVal()
    
    # Read the initial data for Species families
    shiny::observe({
      shiny::req(out_tab3$WB_meta())
      df <- openxlsx::readWorkbook(out_tab3$WB_meta(), sheet = "DropList", colNames = TRUE) %>%
        dplyr::select(tree_species,	species_code,	phylogenetic_group,	leaf_habit,	tree_ring_structure) %>%
        data.frame(stringsAsFactors = FALSE)
      species_family(df)
    })
    
    # Function to synchronize Species data
    sync_species_code <- function(df, species_family, remove_na = TRUE) {
      if (remove_na) {
        df <- df %>%
          dplyr::filter(!is.na(tree_species) & tree_species != "")
      }
      
      updated <- df %>%
        dplyr::left_join(
          species_family,
          by = "tree_species",
          suffix = c("", "_from_list")
        ) %>%
        dplyr::mutate(
          species_code = species_code_from_list,	
          phylogenetic_group = phylogenetic_group_from_list,
          leaf_habit = leaf_habit_from_list,
          tree_ring_structure = tree_ring_structure_from_list
        ) %>%
        dplyr::select(-dplyr::ends_with("_from_list"))
      
      return(updated)
    }
    
    # RENDER TABLES
    output$tbl4 <- rhandsontable::renderRHandsontable({
      shiny::req(out_tab4$data_meta$tbl4)  # Ensure data is available
      rhandsontable::rhandsontable(
        out_tab4$data_meta$tbl4,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = 'all') %>%
        hot_col_wrapper('site_label', out_tab3$column_configs()$tbl4$site_label) %>%
        hot_col_wrapper('tree_label', out_tab3$column_configs()$tbl4$tree_label) %>%
        hot_col_wrapper('suggested_tree_code', out_tab3$column_configs()$tbl4$suggested_tree_code) %>%
        hot_col_wrapper('plot_label', out_tab3$column_configs()$tbl4$plot_label) %>%
        hot_col_wrapper('suggested_plot_code', out_tab3$column_configs()$tbl4$suggested_plot_code) %>%
        hot_col_wrapper('tree_species', out_tab3$column_configs()$tbl4$tree_species) %>%
        hot_col_wrapper('species_code', out_tab3$column_configs()$tbl4$species_code) %>%
        hot_col_wrapper('phylogenetic_group', out_tab3$column_configs()$tbl4$phylogenetic_group) %>%
        hot_col_wrapper('leaf_habit', out_tab3$column_configs()$tbl4$leaf_habit) %>%
        hot_col_wrapper('tree_ring_structure', out_tab3$column_configs()$tbl4$tree_ring_structure) %>%
        hot_col_wrapper('tree_treatment', out_tab3$column_configs()$tbl4$tree_treatment) %>%
        hot_col_wrapper('tree_sampling_pattern', out_tab3$column_configs()$tbl4$tree_sampling_pattern) %>%
        hot_col_wrapper('tree_dbh', out_tab3$column_configs()$tbl4$tree_dbh) %>%
        hot_col_wrapper('tree_height', out_tab3$column_configs()$tbl4$tree_height) %>%
        hot_col_wrapper('tree_age', out_tab3$column_configs()$tbl4$tree_age) %>%
        hot_col_wrapper('tree_sex', out_tab3$column_configs()$tbl4$tree_sex) %>% 
        hot_col_wrapper('tree_social_status', out_tab3$column_configs()$tbl4$tree_social_status) %>%
        hot_col_wrapper('tree_health_status', out_tab3$column_configs()$tbl4$tree_health_status) %>%
        hot_col_wrapper('tree_origin', out_tab3$column_configs()$tbl4$tree_origin) %>%
        hot_col_wrapper('tree_latitude', out_tab3$column_configs()$tbl4$tree_latitude) %>% 
        hot_col_wrapper('tree_longitude', out_tab3$column_configs()$tbl4$tree_longitude) %>%
        hot_col_wrapper('on_tree_dendrometer_monitoring', out_tab3$column_configs()$tbl4$on_tree_dendrometer_monitoring) %>%
        hot_col_wrapper('on_tree_sapflux_monitoring', out_tab3$column_configs()$tbl4$on_tree_sapflux_monitoring) %>%
        hot_col_wrapper('on_tree_primary_phenological_observation', out_tab3$column_configs()$tbl4$on_tree_primary_phenological_observation) %>%
        hot_col_wrapper('on_tree_weather_monitoring', out_tab3$column_configs()$tbl4$on_tree_weather_monitoring) %>%
        hot_col_wrapper('on_tree_shoot_growth_monitoring', out_tab3$column_configs()$tbl4$on_tree_shoot_growth_monitoring) %>%
        hot_col_wrapper('tree_ring_width_data', out_tab3$column_configs()$tbl4$tree_ring_width_data) %>%
        hot_col_wrapper('tree_ring_density_data', out_tab3$column_configs()$tbl4$tree_ring_density_data) %>%
        hot_col_wrapper('tree_ring_anatomical_data', out_tab3$column_configs()$tbl4$tree_ring_anatomical_data) %>%
        hot_col_wrapper('tree_ring_isotope_data', out_tab3$column_configs()$tbl4$tree_ring_isotope_data) %>%
        hot_col_wrapper('number_of_samples', out_tab3$column_configs()$tbl4$number_of_samples) %>%
        hot_col_wrapper('tree_comment', out_tab3$column_configs()$tbl4$tree_comment)
    })
    
    # Sync data on user input
    shiny::observeEvent(input$tbl4, {
      shiny::req(input$tbl4)
      user_data <- rhandsontable::hot_to_r(input$tbl4)
      out_tab4$data_meta$tbl4 <- sync_species_code(user_data, species_family())
    })
    
    shiny::observeEvent(input$save_tree, {
      save_and_validate(
        data_reactive = out_tab4$data_meta$tbl4,
        sheet_name = "tree",
        wb_reactive = out_tab3$WB_meta,
        temp_folder = out_tab1$temp_folder,
        update_validation = out_tab2$validation_results
      )
    })
    
  })
}
