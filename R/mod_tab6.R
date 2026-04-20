#' Module UI for Sample Metadata Tab
#'
#' @description This function generates the UI components for the "Sample" tab, including a table for displaying and editing sample metadata.
#'
#' @param id A string representing the namespace for the UI components. This is required for module-based UI.
#'
#' @return A `fluidRow` containing the UI elements for the sample metadata table and a save button.
#' 
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' 
#' @export
#' 
mod_tab6_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # TAB 6: View sample -----------------------------------------------
  bslib::nav_panel(
    title = htmltools::div(id = ns("sample_tab"), "Sample"),
    value = "Sample",
    
    shiny::fluidRow(
      # Sidebar (left) — Actions or instructions
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_header(NULL),
          bslib::card_body(
            shiny::actionButton(ns("save_sample"), label = htmltools::tagList(bsicons::bs_icon("save"), "Save"), class = "btn-primary")
          )
        )
      ),
      
      
      # Main content (right) — Editable metadata table
      shiny::column(
        11, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Sample Metadata"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl5"))
          )
        )
      )
    )
  )
}

#' Server function for Sample Metadata Tab
#'
#' @description This function defines the server-side logic for the "Sample" tab, including loading sample metadata from an uploaded file, rendering the sample metadata table, and handling save events.
#'
#' @param id A string representing the namespace for the module's server logic. This is required for module-based server functions.
#' @param out_tab1 A reactive object containing the dataset name and observation file.
#' @param out_tab2 A reactive object containing the metadata file and validation results.
#' @param out_tab3 A reactive object containing the workbook reference and column configurations.
#' @param out_tab4 A reactive object containing the tree metadata table and other related data.
#'
#' @return NULL. This function performs side-effects, such as rendering the sample table and saving data.
#' 
#' @import shiny
#' @importFrom rhandsontable renderRHandsontable 
#' @importFrom dplyr mutate case_when select
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' 
#' 
#' @export
mod_tab6_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================
    # LOCAL STATE (SOURCE OF TRUTH INSIDE MODULE)
    # =========================================================
    tbl6_data <- shiny::reactiveVal(NULL)
    selected_row <- shiny::reactiveVal(NULL)
    
    # =========================================================
    # INITIAL LOAD
    # =========================================================
    observe({
      req(ctx$files$wb_meta)
      
      df <- openxlsx::readWorkbook(
        ctx$files$wb_meta,
        sheet = "person",
        startRow = 1,
        colNames = TRUE
      )
      
      df <- df[-(1:ctx$config$skip_rows_excel), , drop = FALSE] |>
        tibble::as_tibble()
      
      tbl6_data(df)
      
      # SAFE WRITE TO CTX (OK)
      ctx$data$tbl6 <- df
    })
    
    # =========================================================
    # SYNC TABLE EDITS
    # =========================================================
    observeEvent(input$tbl6, {
      req(input$tbl6)
      
      df <- rhandsontable::hot_to_r(input$tbl6)
      
      tbl6_data(df)
      
      # SAFE WRITE ONLY
      ctx$data$tbl6 <- df
    })
    
    # =========================================================
    # SELECTION
    # =========================================================
    observeEvent(input$tbl6_select$select$r, {
      selected_row(input$tbl6_select$select$r)
    })
    
    # =========================================================
    # ADD / UPDATE
    # =========================================================
    observeEvent(input$add_person, {
      
      req(tbl6_data())
      
      new_row <- tibble::tibble(
        person_role = input$person_role,
        person_order = as.integer(input$person_order),
        last_name = input$last_name,
        first_name = input$first_name,
        email = input$email,
        orcid = input$orcid,
        main_organization_name = input$main_organization_name,
        main_organization_registry = input$main_organization_registry,
        department = input$department,
        street = input$street,
        postal_code = input$postal_code,
        city = input$city,
        organization_country = input$organization_country,
        organization_country_code = input$organization_country_code
      )
      
      df <- tbl6_data()
      
      if (!is.null(selected_row())) {
        df[selected_row(), ] <- new_row
      } else {
        df <- dplyr::bind_rows(df, new_row)
      }
      
      tbl6_data(df)
      ctx$data$tbl6 <- df
      
      selected_row(NULL)
    })
    
    # =========================================================
    # DELETE
    # =========================================================
    observeEvent(input$delete_person, {
      
      req(selected_row())
      
      df <- tbl6_data()
      i <- selected_row()
      
      if (!is.null(i) && i <= nrow(df)) {
        df <- df[-i, , drop = FALSE]
      }
      
      tbl6_data(df)
      ctx$data$tbl6 <- df
      
      selected_row(NULL)
    })
    
    # =========================================================
    # ORDER
    # =========================================================
    observeEvent(input$apply_order, {
      
      df <- tbl6_data()
      
      df$person_order <- as.integer(df$person_order)
      df <- df[order(df$person_order), , drop = FALSE]
      df$person_order <- seq_len(nrow(df))
      
      tbl6_data(df)
      ctx$data$tbl6 <- df
    })
    
    # =========================================================
    # RENDER
    # =========================================================
    output$tbl6 <- rhandsontable::renderRHandsontable({
      
      req(tbl6_data())
      
      rhandsontable::rhandsontable(
        tbl6_data(),
        rowHeaders = NULL,
        stretchH = "all",
        selectCallback = TRUE,
        height = 150
      )
    })
    
    # =========================================================
    # SAVE
    # =========================================================
    observeEvent(input$save_person, {
      
      save_and_validate(
        data_reactive = tbl6_data(),   # 👈 IMPORTANT FIX
        sheet_name = "person",
        wb_reactive = ctx$files$wb_meta,
        temp_folder = ctx$files$temp_folder,
        update_validation = ctx$validation$results
      )
    })
    
  })
}
