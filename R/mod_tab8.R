#' UI for Publication Tab
#'
#' This function generates the UI for the "Publication" tab in the Shiny app.
#' It allows users to view, add, update, delete, and reorder publication metadata.
#'
#' @param id The module ID for the UI element.
#' @return A UI element for the Publication tab.
#' 
#' @import shiny
#' @import shinyjs
#' @import bslib 
#' @importFrom bsicons bs_icon
#' @importFrom htmltools tagList br HTML div
#' @importFrom rhandsontable rHandsontableOutput
#' 
#' @export
mod_tab8_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # TAB 8: View Publication -----------------------------------------------
  bslib::nav_panel(
    title = htmltools::div(id = ns("publication_tab"), "Publication"),
    value = "Publication",
    shiny::fluidRow(
      # Left Sidebar - Save Button
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_body(
            shiny::actionButton(ns("save_publication"), label = htmltools::tagList(bsicons::bs_icon("save"), 'Save publication'), class = "btn-primary")
          )
        )
      ),
      # Right Column - Main Content
      shiny::column(
        11, style = "height: 100%;",
        shinyjs::useShinyjs(),
        hidden(shiny::verbatimTextOutput(ns("publication_form_visible"), placeholder = TRUE)),
        
        # Publication Metadata Table and Action Buttons
        bslib::card(
          bslib::tooltip(
            bslib::card_header("Publication Table"),
            bsicons::bs_icon("question-circle"),
            htmltools::HTML(
              "This is the publication metadata table. You can view, add, update, delete, and reorder publications using the buttons below.<br><br>
               Cells with <b>yellow text</b> are editable.<br>
               Cells highlighted in <b>red</b> indicate validation issues and require correction.<br>
               Hover over a red cell to see a tooltip explaining the issue."
            ), placement = "right"
          ),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl7")),
            htmltools::br(),
            htmltools::div(
              style = "display: flex; gap: 10px; flex-wrap: wrap; align-items: center;",
              bslib::tooltip(
                shiny::actionButton(ns("show_add_publication"), label = htmltools::tagList(bsicons::bs_icon("book"), "Add New Publication"), class = "btn-success"),
                "Add a new publication to the table. This opens the metadata form to help guide data entry.", placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("update_publication"), label = htmltools::tagList(bsicons::bs_icon("pencil-square"), "Update Publication"), class = "btn-warning"),
                "Edit metadata of a selected publication. Choose a publication in the table to activate this button.", placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("delete_publication"), label = htmltools::tagList(bsicons::bs_icon("trash"), "Delete Publication"), class = "btn-danger"),
                "Delete the selected publication from the table. You must first select a row.", placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("apply_publication_order"), label = htmltools::tagList(bsicons::bs_icon("sort-down"), "Apply Publication Order")),
                "Reorder publications in the table according to the values in the 'publication_order' column. Edit that column and click this button to sort accordingly.", placement = "right"
              )
            )
          )
        ),
        # Conditional Panel - Show Form When Active
        shiny::conditionalPanel(
          condition = sprintf("output['%s'] == 'TRUE'", ns("publication_form_visible")),
          bslib::accordion(
            open= c("DOI Search", "Publication Metadata"),
            bslib::accordion_panel(
              title = "DOI Search",
              id = ns("card_header_doi"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle"),
                "Use this tool to search for a publication using its DOI. Retrieved info will populate the form below.", placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(3,
                       bslib::tooltip(
                         shiny::actionButton(ns("search_doi"), label = htmltools::tagList(bsicons::bs_icon("search"), "Retrieve from DOI DB")),
                         "Search the DOI database with the provided DOI. This will fill the form below with the retrieved data.", placement = "right"
                       )
                ),
                shiny::column(3, shiny::textInput(ns("doi_input"), "Enter DOI:", placeholder = "e.g. 10.1111/j.1469-8137.2005.00492.x"))
              ),
              shiny::verbatimTextOutput(ns("doi_result"))
            ),
            bslib::accordion_panel(
              title = "Publication Metadata",
              id = ns("card_header_metadata"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle"),
                "Fill in or edit the metadata of a publication. Use DOI search to auto-complete relevant fields.", placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("first_author_last_name"), htmltools::HTML("First Author <span style='color:red;'>*</span>"))),
                shiny::column(9, shiny::textInput(ns("title"), htmltools::HTML("Title <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(4, shiny::textInput(ns("publication_year"), htmltools::HTML("Year <span style='color:red;'>*</span>"))),
                shiny::column(4, shiny::textInput(ns("journal"), htmltools::HTML("Journal <span style='color:red;'>*</span>"))),
                shiny::column(4, shiny::textInput(ns("doi"), htmltools::HTML("DOI <span style='color:red;'>*</span>")))
              ),
              htmltools::br(),
              bslib::tooltip(
                shiny::actionButton(ns("add_publication_data"), label = htmltools::tagList(bsicons::bs_icon("book-fill"), "Add Publication"), class = "btn-success"),
                "Click to add the current publication metadata to the publication table above. All required fields must be filled.", placement = "right"
              )
            )
          )
        )
      )
    )
  )
}

#' Server Logic for Publication Tab
#'
#' This function handles the server-side logic for the "Publication" tab in the Shiny app.
#' It manages the interactions for adding, editing, deleting, and ordering publications.
#'
#' @param id The module ID for the server logic.
#' @param out_tab1 A reactive object containing the dataset name and observation file.
#' @param out_tab2 A reactive object containing the metadata file and validation results.
#' @param out_tab3 A reactive object containing the workbook reference and column configurations.
#' @param out_tab4 A reactive object containing the tree metadata table and other related data.

#' @return A server-side function for managing the Publication tab.
#' 
#' @import shiny
#' @import shinyjs
#' @importFrom rhandsontable renderRHandsontable hot_to_r
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code content timeout
#' 
#' @export
mod_tab8_server <- function(id, out_tab1, out_tab2, out_tab3, out_tab4) {
  moduleServer(id, function(input, output, session) {
    

    # TAB 8: -------------------------------------------------------------------
    # Clear publication fields
    clear_publication_fields <- function(session) {
      shiny::updateTextInput(session, "first_author_last_name", value = "")
      shiny::updateTextInput(session, "title", value = "")
      shiny::updateTextInput(session, "publication_year", value = "")
      shiny::updateTextInput(session, "journal", value = "")
      shiny::updateTextInput(session, "doi", value = "")
      shiny::updateTextInput(session, "doi_input", value = "")
    }
    
    publication_form_visible <- shiny::reactiveVal(FALSE)
    publication_edit_mode <- shiny::reactiveVal(FALSE)
    publication_selected_row <- shiny::reactiveVal(NULL)
    
    # Show form when Add Publication is clicked
    shiny::observeEvent(input$show_add_publication, {
      publication_form_visible(TRUE)
      publication_edit_mode(FALSE)
      clear_publication_fields(session)
    })
    
    output$publication_form_visible <- shiny::renderText({
      as.character(publication_form_visible())
    })
    shiny::outputOptions(output, "publication_form_visible", suspendWhenHidden = FALSE)
    
    #### dpublication ####
    dpublication <- shiny::reactiveVal()
    out_tab4$data_meta <- shiny::reactiveValues(tbl7 = NULL, last_doi_citation = NULL, last_doi_metadata = NULL)
    
    # Load from file
    shiny::observe({
      shiny::req(out_tab2$meta_file)
      publication_meta_info <- openxlsx::readWorkbook(out_tab3$WB_meta(), sheet = "publication", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
        tibble::tibble()
      dpublication(publication_meta_info)
    })
    
    # Sync to table
    shiny::observeEvent(dpublication(), {
      out_tab4$data_meta$tbl7 <- dpublication()
    })
    
    # Render publication table
    output$tbl7 <- rhandsontable::renderRHandsontable({
      shiny::req(out_tab4$data_meta$tbl7)
      rhandsontable::rhandsontable(
        out_tab4$data_meta$tbl7,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = 'all', selectCallback = TRUE, height=150) %>%
        hot_col_wrapper('first_author_last_name', out_tab3$column_configs()$tbl7$first_author_last_name) %>%
        hot_col_wrapper('title', out_tab3$column_configs()$tbl7$title) %>%
        hot_col_wrapper('publication_year', out_tab3$column_configs()$tbl7$publication_year) %>%
        hot_col_wrapper('journal', out_tab3$column_configs()$tbl7$journal) %>%
        hot_col_wrapper('doi', out_tab3$column_configs()$tbl7$doi)
    })
    
    shiny::observeEvent(input$tbl7_select$select$r, {
      selected_row <- input$tbl7_select$select$r
      publication_selected_row(selected_row)  # Store the selected row
      
      # Enable buttons when a row is selected
      if (!is.null(selected_row) && length(selected_row) > 0) {
        shinyjs::enable("update_publication")
        shinyjs::enable("delete_publication")
      } else {
        shinyjs::disable("update_publication")
        shinyjs::disable("delete_publication")
      }
    })
    
    
    # Add or Edit
    shiny::observeEvent(input$add_publication_data, {
      shiny::req(input$first_author_last_name)
      shiny::req(input$title)
      shiny::req(input$publication_year)
      shiny::req(input$journal)
      shiny::req(input$doi)
      
      new_row <- data.frame(
        first_author_last_name = input$first_author_last_name,
        title = input$title,
        publication_year = as.numeric(input$publication_year),
        journal = input$journal,
        doi = input$doi,
        stringsAsFactors = FALSE
      )
      
      if (publication_edit_mode() && !is.null(publication_selected_row())) {
        new_row$publication_year <- as.character(new_row$publication_year)
        out_tab4$data_meta$tbl7[publication_selected_row(), names(new_row)] <- as.list(new_row[1, ])
      } else {
        out_tab4$data_meta$tbl7 <- rbind(out_tab4$data_meta$tbl7, new_row)
      }
      
      clear_publication_fields(session)
      publication_form_visible(FALSE)
      # updateActionButton(session, "show_add_publication", label = tagList(bsicons::bs_icon("book"), "Add New Publication"))
      publication_edit_mode(FALSE)
      publication_selected_row(NULL)
    })
    
    
    # Edit
    shiny::observeEvent(input$update_publication, {
      shiny::req(publication_selected_row())
      row <- publication_selected_row()
      publication_form_visible(TRUE)
      publication_edit_mode(TRUE)
      
      shiny::updateTextInput(session, "first_author_last_name", value = out_tab4$data_meta$tbl7$first_author_last_name[row])
      shiny::updateTextInput(session, "title", value = out_tab4$data_meta$tbl7$title[row])
      shiny::updateTextInput(session, "publication_year", value = as.character(out_tab4$data_meta$tbl7$publication_year[row]))
      shiny::updateTextInput(session, "journal", value = out_tab4$data_meta$tbl7$journal[row])
      shiny::updateTextInput(session, "doi", value = out_tab4$data_meta$tbl7$doi[row])
    })
    
    # Delete
    shiny::observeEvent(input$delete_publication, {
      row <- publication_selected_row()
      if (!is.null(row) && row <= nrow(out_tab4$data_meta$tbl7)) {
        out_tab4$data_meta$tbl7 <- out_tab4$data_meta$tbl7[-row, ]
        publication_selected_row(NULL)
        showNotification("Publication deleted.", type = "message")
      } else {
        showNotification("Please select a row to delete.", type = "warning")
      }
      clear_publication_fields(session)
      publication_form_visible(FALSE)
      publication_edit_mode(FALSE)
    })
    
    # Toggle button state
    shiny::observe({
      shinyjs::toggleState("update_publication", condition = !is.null(publication_selected_row()))
      shinyjs::toggleState("delete_publication", condition = !is.null(publication_selected_row()))
    })
    
    shiny::observeEvent(input$apply_publication_order, {
      shiny::req(input$tbl7)
      if (!is.null(input$tbl7)) {
        ordered_data <- rhandsontable::hot_to_r(input$tbl7)
        ordered_data$publication_year <- as.numeric(ordered_data$publication_year)
        out_tab4$data_meta$tbl7 <- ordered_data[order(ordered_data$publication_year), ]
        out_tab4$data_meta$tbl7$publication_year <- as.character(out_tab4$data_meta$tbl7$publication_year)
      }
    })
    
    
    # DOI Search - Ensuring valid metadata retrieval
    shiny::observeEvent(input$search_doi, {
      shiny::req(input$doi_input)
      doi_input <- URLencode(input$doi_input)
      
      citation_url <- sprintf("https://citation.doi.org/format?doi=%s&style=apa&lang=en-US", doi_input)
      metadata_url <- sprintf("https://citation.doi.org/metadata?doi=%s", doi_input)
      
      citation_res <- httr::GET(citation_url, httr::timeout(5))
      metadata_res <- httr::GET(metadata_url, httr::timeout(5))
      
      # Handle citation response
      if (httr::status_code(citation_res) == 200) {
        citation_text <- httr::content(citation_res, as = "text", encoding = "UTF-8")
        out_tab4$data_meta$last_doi_citation <- citation_text
      } else {
        out_tab4$data_meta$last_doi_citation <- NULL
      }
      
      # Handle metadata response
      if (httr::status_code(metadata_res) == 200) {
        meta_data <- tryCatch({
          jsonlite::fromJSON(httr::content(metadata_res, as = "text", encoding = "UTF-8"))
        }, error = function(e) NULL)
        
        if (!is.null(meta_data)) {
          out_tab4$data_meta$last_doi_metadata <- meta_data
        } else {
          out_tab4$data_meta$last_doi_metadata <- NULL
        }
      } else {
        out_tab4$data_meta$last_doi_metadata <- NULL
      }
    })
    
    # Autofill metadata - Enhanced with checks for valid metadata
    shiny::observeEvent(out_tab4$data_meta$last_doi_metadata, {
      meta <- out_tab4$data_meta$last_doi_metadata
      
      # Ensure the metadata is available and valid
      shiny::req(meta, msg = "Metadata not available")
      
      # Check the type of meta and ensure it's a list or data frame
      if (!is.list(meta) && !is.data.frame(meta)) {
        showNotification("Metadata is not in the expected format", type = "error")
        return()  # Exit early if meta is not a valid list/data frame
      }
      
      # Safely access metadata fields with fallback to default values
      title <- if (!is.null(meta$title)) meta$title else ""
      authors <- if (!is.null(meta$author) && length(meta$author) > 0) meta$author else NULL
      first_author <- if (!is.null(authors) && length(authors) > 0) authors$family[1] else ""
      journal <- if (!is.null(meta$`container-title`)) meta$`container-title` else ""
      year <- if (!is.null(meta$issued) && length(meta$issued) > 0) meta$issued$`date-parts`[1] else ""
      doi <- if (!is.null(meta$DOI)) meta$DOI else ""
      
      # Debug prints to help check what's being retrieved
      print(paste("Title: ", title))
      print(paste("First Author: ", first_author))
      print(paste("Journal: ", journal))
      print(paste("Year: ", year))
      print(paste("DOI: ", doi))
      
      # Update the inputs with the retrieved metadata
      shiny::updateTextInput(session, "title", value = title)
      shiny::updateTextInput(session, "first_author_last_name", value = first_author)
      shiny::updateTextInput(session, "journal", value = journal)
      shiny::updateTextInput(session, "publication_year", value = year)
      shiny::updateTextInput(session, "doi", value = doi)
      # Optionally make the form visible
      publication_form_visible(TRUE)
    })
    
    
    # Sync data on user input
    shiny::observeEvent(input$tbl7, {
      shiny::req(input$tbl7)
      out_tab4$data_meta$tbl7 <- rhandsontable::hot_to_r(input$tbl7)
      # Update the reactive data object
    })
    
    
    shiny::observeEvent(input$save_publication, {
      save_and_validate(
        data_reactive = out_tab4$data_meta$tbl7,
        sheet_name = "publication",
        wb_reactive = out_tab3$WB_meta,
        temp_folder = out_tab1$temp_folder,
        update_validation = out_tab2$validation_results
      )
    })
    
  })
}