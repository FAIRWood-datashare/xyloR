#' mod_tab3 UI Function
#'
#' @description A shiny module for the "Observation" tab.
#'
#' This module provides a user interface for managing the observation data.
#' It consists of:
#' 1. An action button for saving observations,
#' 2. A basic information section that displays a table of site data,
#' 3. A detailed observation table with data fields such as sample date, species, and labels,
#' 4. Real-time syncing of data with user input for tables,
#' 5. Handling data formatting and species synchronization.
#'
#' @param id A string that serves as the module namespace identifier.
#'
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
  
  # TAB 3: Observation -----------------------------------------------
  bslib::nav_panel(
    title = htmltools::div(id = ns("observation_tab"), "Observations"),
    value = "Observations",
    
    shiny::fluidRow(
      # Left side (sidebar) - Action Button and Info Section
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_header(NULL),
          bslib::card_body(
            shiny::actionButton(ns("save_obs"), label = htmltools::tagList(bsicons::bs_icon("save"), "Save obs"), class = "btn-primary")
          )
        )
      ),
      
      # Right side - Main Content with Observation Table
      shiny::column(
        11, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Basic Info"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl1")),
            #shiny::verbatimTextOutput(ns("testing"))
          )
        ),
        
        bslib::card(
          bslib::card_header("Observation table:"),
          bslib::card_body(
            rhandsontable:: rHandsontableOutput(ns("tbl2")),
            #shiny::verbatimTextOutput(ns("testing1")),
            #shiny::verbatimTextOutput(ns("testing2"))
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
#' Handles:
#' - Observations data loading and synchronization,
#' - Saving observation data to a workbook,
#' - Table rendering with synchronization and real-time updates,
#' - Data validation, and managing input data transformations.
#'
#' @param id A string that serves as the module namespace identifier.
#' @param shared A list or reactiveValues object shared across modules.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @import shiny 
#' @importFrom shinyjs enable disable
#' @importFrom DT renderDataTable datatable
#' @importFrom plotly plotlyOutput
#' @importFrom openxlsx loadWorkbook saveWorkbook 
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' 
mod_tab3_server <- function(id, out_tab1, out_tab2) {
  moduleServer(id, function(input, output, session) {
    
    # TAB 3 observations: ---------------------------------------------------------------------
    

    WB <- shiny::reactive({
      shiny::req(out_tab1$obs_file(), out_tab1$site_filter())
      loadWorkbook(out_tab1$obs_file()$datapath)
    })
    
    WB_meta <- shiny::reactive({
      shiny::req(out_tab2$meta_file())
      openxlsx::loadWorkbook(out_tab2$meta_file()$datapath)
    })
    
    tab_ids <- c("Site", "Tree", "Sample", "Person", "Publication")
    lapply(tab_ids, function(id) shinyjs::disable(selector = sprintf("a[data-value='%s']", id)))
    
    shiny::observe({
      shiny::req(WB_meta())
      try(openxlsx::readWorkbook(WB_meta(), sheet = "site"), silent = TRUE)
      lapply(tab_ids, function(id) shinyjs::enable(selector = sprintf("a[data-value='%s']", id)))
    })
    
    
    #### dinfo  #### 
    dinfo <- shiny::reactiveVal()
    
    shiny::observe({
      shiny::req(out_tab1$obs_file())  # Ensure file is uploaded
      
      # Read the dataset
      site_info <- openxlsx::readWorkbook(WB(), sheet = "obs_data_info", startRow = 6, colNames = FALSE) %>%
        tibble::tibble()
      obs_data <- openxlsx::readWorkbook(WB(), sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>%
        tibble::tibble()
      
      # Check the number of columns
      if (ncol(site_info) == 3) {
        site_label <- unique(obs_data$site_label) %>% tibble::tibble() %>% dplyr::filter(!is.na(.))  # Extract unique site labels
        site_info <- cbind(site_label, site_info)   # Add it as the first column
      } else if (ncol(site_info) != 4) {
        stop("⚠️ Error: Expected 3 or 4 columns in 'Xylo_obs_data'. Please check your data format.")
      }
      
      # Apply column names only if the check passes
      site_info <- setNames(site_info, c("site_label", "latitude", "longitude", "elevation")) %>% 
        dplyr::mutate(
          elevation = as.integer(elevation)
        )
      dinfo(site_info)  # Store in reactive value
    })
    
    drop_species <- shiny::reactive({
      shiny::req(WB())  # Ensure the workbook is available
      drop_species <- openxlsx::readWorkbook(WB(), sheet = "DropList", colNames = TRUE) %>% 
        dplyr::mutate(itrdb_species_code = ITRDB_species_code) %>% 
        dplyr::select(tree_species, itrdb_species_code) %>%
        dplyr::filter(!is.na(tree_species))
    })
    
    #### dobs ####
    dobs <- shiny::reactiveVal()
    
    shiny::observe({
      req(out_tab1$obs_file)  # Ensure file is uploaded
      
      # Read data from Excel, skipping first 6 rows
      data <- openxlsx::readWorkbook(WB(), sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>%
        tibble::tibble() %>% 
        dplyr::left_join(drop_species(), by = "tree_species") %>%
        dplyr::relocate(itrdb_species_code, .after = tree_species)
      
      # Check if sample_date is numeric and convert to Date
      data <- data %>%
        dplyr::mutate(
          sample_date = dplyr::case_when(
            # If sample_date is numeric (Excel date format)
            !is.na(sample_date) & is.numeric(as.numeric(sample_date)) ~
              as.Date(as.numeric(sample_date), origin = "1899-12-30"),
            
            TRUE ~ as.Date(NA)  # Handle NAs
          )
        ) %>% 
        dplyr::mutate(sample_date = as.character(sample_date))
      dobs(data)  # Store in reactive value
    })
    
    # # INITALIZE REACTIVE INPUT DATA
    data_in <- shiny::reactiveValues()
    
    # Reactive context to store initial data and ensure it's updated in a proper context
    shiny::observe({
      data_in$tbl1 <- dinfo()  # Access dinfo in a valid reactive context
      data_in$tbl2 <- dobs()   # Access dobs in a valid reactive context
    })
    
    column_configs <- shiny::reactive({
      shiny::req(WB())  # Ensure the workbook is available
      get_column_configs(WB(), WB_meta(), dobs())
    })
    
    
    # RENDER TABLES
    output$tbl1 <- rhandsontable::renderRHandsontable({
      shiny::req(data_in$tbl1)  # Ensure data is available
      shiny::req(data_in$tbl2)
      rhandsontable::rhandsontable(
        data_in$tbl1,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = 'all') %>% # , overflow = 'visible'
       hot_col_wrapper('site_label', column_configs()$tbl1$site_label) %>%
       hot_col_wrapper('latitude', column_configs()$tbl1$latitude) %>%
       hot_col_wrapper('longitude', column_configs()$tbl1$longitude) %>%
       hot_col_wrapper('elevation', column_configs()$tbl1$elevation)
    })
    
    # Function to synchronize Species data
    sync_species_itrdb <- function(df, drop_species, remove_na = TRUE) {
      if (remove_na) {
        df <- df %>%
          dplyr::filter(!is.na(tree_species) & tree_species != "")
      }
      
      updated <- df %>%
        dplyr::left_join(
          drop_species,
          by = "itrdb_species_code",
          suffix = c("", "_from_list")
        ) %>%
        dplyr::mutate(
          tree_species = tree_species_from_list
        ) %>%
        dplyr::select(-dplyr::ends_with("_from_list"))
      
      return(updated)
    }
    
    output$tbl2 <- rhandsontable::renderRHandsontable({
      shiny::req(data_in$tbl2)  # Ensure data is available
        rhandsontable::rhandsontable(
        data_in$tbl2,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = 'all', height = '300px') %>% # overflow = 'visible',
        hot_col_wrapper('sample_date', column_configs()$tbl2$sample_date) %>%
        hot_col_wrapper('sample_id', column_configs()$tbl2$sample_id) %>%
        hot_col_wrapper('tree_species', column_configs()$tbl2$tree_species) %>%
        hot_col_wrapper('itrdb_species_code', column_configs()$tbl2$itrdb_species_code) %>%
        hot_col_wrapper('tree_label', column_configs()$tbl2$tree_label) %>%
        hot_col_wrapper('plot_label', column_configs()$tbl2$plot_label) %>%
        hot_col_wrapper('site_label', column_configs()$tbl2$site_label) %>%
        hot_col_wrapper('network_label', column_configs()$tbl2$network_label)%>%
        hot_col_wrapper('sample_label', column_configs()$tbl2$sample_label) %>%
        hot_col_wrapper('radial_file', column_configs()$tbl2$radial_file) %>%
        hot_col_wrapper('sample_comment', column_configs()$tbl2$sample_comment) %>%
        hot_cols(manualColumnResize = TRUE)
    })

    # Sync tbl1 data on user input
    shiny::observeEvent(input$tbl1, {
      shiny::req(input$tbl1)
      data_in$tbl1 <- hot_to_r(input$tbl1)
      # Update the reactive data object
    })
    
    # Sync tbl2 data on user input
    shiny::observeEvent(input$tbl2, {
      shiny::req(input$tbl2)
      user_data <- rhandsontable::hot_to_r(input$tbl2)
      updated_data <- sync_species_itrdb(user_data, drop_species())
      # Update the reactive data object
      data_in$tbl2 <- updated_data
    })
    
    shiny::observeEvent(input$save_obs, {
      save_and_validate(
        data_reactive = data_in$tbl1,
        sheet_name = "obs_data_info",
        wb_reactive = WB,
        temp_folder = out_tab1$temp_folder,
        update_validation = out_tab2$validation_results()
      )
      
      save_and_validate(
        data_reactive = data_in$tbl2 %>% dplyr::select(-itrdb_species_code),
        sheet_name = "Xylo_obs_data",
        wb_reactive = WB,
        temp_folder = out_tab1$temp_folder,
        update_validation = out_tab2$validation_results()
      )
    })
    
    return(
      list(
        WB = WB,
        WB_meta = WB_meta,
        data_in = data_in,
        column_configs = column_configs
      )
    )
    
  })
  
}