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
mod_tab5_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================
    # LOCAL STATE
    # =========================================================
    tbl5_data <- shiny::reactiveVal(NULL)
    
    # =========================================================
    # INITIAL LOAD
    # =========================================================
    observe({
      
      req(ctx$files$wb_meta)
      
      df <- openxlsx::readWorkbook(
        ctx$files$wb_meta,
        sheet = "tab5",
        startRow = 1,
        colNames = TRUE
      )
      
      df <- df[-(1:ctx$config$skip_rows_excel), , drop = FALSE] |>
        tibble::as_tibble()
      
      tbl5_data(df)
      ctx$data$tbl5 <- df
    })
    
    # =========================================================
    # SYNC TABLE EDITS
    # =========================================================
    observeEvent(input$tbl5, {
      req(input$tbl5)
      
      df <- rhandsontable::hot_to_r(input$tbl5)
      
      tbl5_data(df)
      ctx$data$tbl5 <- df
    })
    
    # =========================================================
    # RENDER TABLE
    # =========================================================
    output$tbl5 <- rhandsontable::renderRHandsontable({
      req(tbl5_data())
      
      rhandsontable::rhandsontable(
        tbl5_data(),
        rowHeaders = NULL,
        stretchH = "all",
        selectCallback = TRUE,
        height = 150
      )
    })
    
    # =========================================================
    # SAVE
    # =========================================================
    observeEvent(input$save_tab5, {
      
      save_and_validate(
        data_reactive = ctx$data$tbl5,
        sheet_name = "tab5",
        wb_reactive = ctx$files$wb_meta,
        temp_folder = ctx$files$temp_folder,
        update_validation = ctx$validation$results
      )
    })
    
  })
}
