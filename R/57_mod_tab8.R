#' UI for Publication Tab
#'
#' @param id The module ID for the UI element.
#' @return A nav_panel UI for the Publication tab.
#'
#' @import shiny
#' @import shinyjs
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom htmltools tagList br HTML div
#' @importFrom rhandsontable rHandsontableOutput
#' @export
mod_tab8_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = htmltools::div(id = ns("publication_tab"), "Publication"),
    value = "Publication",

    shiny::fluidRow(
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_body(
            shiny::actionButton(ns("save_publication"),
              label = htmltools::tagList(bsicons::bs_icon("save"), "Save"),
              class = "btn-primary")
          )
        )
      ),

      shiny::column(
        11, style = "height: 100%;",
        shinyjs::useShinyjs(),
        shinyjs::hidden(shiny::verbatimTextOutput(ns("publication_form_visible"), placeholder = TRUE)),

        bslib::card(
          bslib::tooltip(
            bslib::card_header("Publication Table"),
            bsicons::bs_icon("question-circle"),
            htmltools::HTML(
              "View, add, update, delete, and reorder publications.<br><br>
               Cells with <b>yellow text</b> are editable.<br>
               Cells highlighted in <b>red</b> indicate validation issues."
            ),
            placement = "right"
          ),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl7")),
            htmltools::br(),
            htmltools::div(
              style = "display: flex; gap: 10px; flex-wrap: wrap; align-items: center;",
              bslib::tooltip(
                shiny::actionButton(ns("show_add_publication"),
                  label = htmltools::tagList(bsicons::bs_icon("book"), "Add New Publication"),
                  class = "btn-success"),
                "Add a new publication.", placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("update_publication"),
                  label = htmltools::tagList(bsicons::bs_icon("pencil-square"), "Update Publication"),
                  class = "btn-warning"),
                "Edit selected publication.", placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("delete_publication"),
                  label = htmltools::tagList(bsicons::bs_icon("trash"), "Delete Publication"),
                  class = "btn-danger"),
                "Delete selected publication.", placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("apply_publication_order"),
                  label = htmltools::tagList(bsicons::bs_icon("sort-down"), "Apply Publication Order")),
                "Sort by publication_year.", placement = "right"
              )
            )
          )
        ),

        shiny::conditionalPanel(
          condition = sprintf("output['%s'] == 'TRUE'", ns("publication_form_visible")),
          bslib::accordion(
            open = c("DOI Search", "Publication Metadata"),
            bslib::accordion_panel(
              title = "DOI Search",
              id = ns("card_header_doi"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle"),
                "Search by DOI to auto-fill metadata.", placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(3,
                  bslib::tooltip(
                    shiny::actionButton(ns("search_doi"),
                      label = htmltools::tagList(bsicons::bs_icon("search"), "Retrieve from DOI DB")),
                    "Search DOI database.", placement = "right"
                  )
                ),
                shiny::column(3, shiny::textInput(ns("doi_input"), "Enter DOI:",
                  placeholder = "e.g. 10.1111/j.1469-8137.2005.00492.x"))
              ),
              shiny::verbatimTextOutput(ns("doi_result"))
            ),
            bslib::accordion_panel(
              title = "Publication Metadata",
              id = ns("card_header_metadata"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle"),
                "Fill in or edit publication metadata.", placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(4, shiny::textInput(ns("first_author_last_name"),
                  htmltools::HTML("First Author <span style='color:red;'>*</span>"))),
                shiny::column(4, shiny::textInput(ns("title"),
                  htmltools::HTML("Title <span style='color:red;'>*</span>"))),
                shiny::column(4, shiny::selectInput(ns("publication_type"),
                  label = htmltools::HTML("Type <span style='color:red;'>*</span>"),
                  choices = c("article", "thesis", "report", "other"),
                  selected = NULL, multiple = FALSE, selectize = TRUE, width = "100%"))
              ),
              shiny::fluidRow(
                shiny::column(4, shiny::textInput(ns("publication_year"),
                  htmltools::HTML("Year <span style='color:red;'>*</span>"))),
                shiny::column(4, shiny::textInput(ns("journal"), "Journal")),
                shiny::column(4, shiny::textInput(ns("doi"), "DOI"))
              ),
              htmltools::br(),
              bslib::tooltip(
                shiny::actionButton(ns("add_publication_data"),
                  label = htmltools::tagList(bsicons::bs_icon("book-fill"), "Add Publication"),
                  class = "btn-success"),
                "Add publication metadata to the table.", placement = "right"
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
#' @param id The module ID for the server logic.
#' @param ctx The centralized app context (environment).
#' @param session The parent Shiny session.
#'
#' @import shiny
#' @import shinyjs
#' @importFrom rhandsontable renderRHandsontable hot_to_r
#' @importFrom openxlsx readWorkbook
#' @importFrom tibble tibble
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code content timeout
#' @export
#' 
#' # ---- DOI HELPERS -------------------------------------------------
mod_tab8_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {
    
    # =====================================================
    # LOCAL STATE
    # =====================================================
    publication_form_visible <- shiny::reactiveVal(FALSE)
    publication_edit_mode    <- shiny::reactiveVal(FALSE)
    publication_selected_row <- shiny::reactiveVal(NULL)
    
    # =====================================================
    # CLEAR FORM
    # =====================================================
    clear_publication_fields <- function(session) {
      shiny::updateTextInput(session, "first_author_last_name", value = "")
      shiny::updateTextInput(session, "title", value = "")
      shiny::updateSelectInput(session, "publication_type", selected = "")
      shiny::updateTextInput(session, "publication_year", value = "")
      shiny::updateTextInput(session, "journal", value = "")
      shiny::updateTextInput(session, "doi", value = "")
      shiny::updateTextInput(session, "doi_input", value = "")
    }
    
    # =====================================================
    # 1. DATA INIT
    # =====================================================
    dpublication <- shiny::reactiveVal()
    
    shiny::observe({
      shiny::req(ctx$files$wb_meta)
      
      df <- openxlsx::readWorkbook(
        ctx$files$wb_meta,
        sheet = "publication",
        startRow = 1,
        colNames = TRUE
      )[-(1:6), ] |>
        tibble::tibble()
      
      dpublication(df)
    })
    
    shiny::observe({
      shiny::req(dpublication())
      ctx$data$tbl7 <- dpublication()
    })
    
    # =====================================================
    # 2. FORM VISIBILITY
    # =====================================================
    shiny::observeEvent(input$show_add_publication, {
      publication_form_visible(TRUE)
      publication_edit_mode(FALSE)
      clear_publication_fields(session)
    })
    
    output$publication_form_visible <- shiny::renderText(
      as.character(publication_form_visible())
    )
    
    shiny::outputOptions(output, "publication_form_visible", suspendWhenHidden = FALSE)
    
    # =====================================================
    # 3. TABLE RENDER
    # =====================================================
    output$tbl7 <- rhandsontable::renderRHandsontable({
      shiny::req(ctx$data$tbl7)
      
      col_cfg <- ctx$data$column_configs
      
      rhandsontable::rhandsontable(
        ctx$data$tbl7,
        rowHeaders = NULL,
        contextMenu = TRUE,
        stretchH = "all",
        selectCallback = TRUE,
        height = 150
      ) |>
        hot_col_wrapper("first_author_last_name", col_cfg$tbl7$first_author_last_name) |>
        hot_col_wrapper("title", col_cfg$tbl7$title) |>
        hot_col_wrapper("publication_type", col_cfg$tbl7$publication_type) |>
        hot_col_wrapper("publication_year", col_cfg$tbl7$publication_year) |>
        hot_col_wrapper("journal", col_cfg$tbl7$journal)
    })
    
    # =====================================================
    # 4. ROW SELECTION
    # =====================================================
    shiny::observeEvent(input$tbl7_select$select$r, {
      
      publication_selected_row(input$tbl7_select$select$r)
      
      shinyjs::toggleState(
        "update_publication",
        condition = !is.null(publication_selected_row())
      )
      
      shinyjs::toggleState(
        "delete_publication",
        condition = !is.null(publication_selected_row())
      )
    })
    
    # =====================================================
    # 5. ADD / EDIT
    # =====================================================
    shiny::observeEvent(input$add_publication_data, {
      
      shiny::req(input$first_author_last_name,
                 input$title,
                 input$publication_year)
      
      new_row <- data.frame(
        first_author_last_name = input$first_author_last_name,
        title = input$title,
        publication_type = input$publication_type,
        publication_year = as.numeric(input$publication_year),
        journal = input$journal,
        doi = input$doi,
        stringsAsFactors = FALSE
      )
      
      if (publication_edit_mode() && !is.null(publication_selected_row())) {
        ctx$data$tbl7[publication_selected_row(), ] <- new_row
      } else {
        ctx$data$tbl7 <- rbind(ctx$data$tbl7, new_row)
      }
      
      clear_publication_fields(session)
      publication_form_visible(FALSE)
      publication_edit_mode(FALSE)
      publication_selected_row(NULL)
    })
    
    # =====================================================
    # 6. UPDATE FORM
    # =====================================================
    shiny::observeEvent(input$update_publication, {
      
      req(publication_selected_row())
      row <- publication_selected_row()
      
      publication_form_visible(TRUE)
      publication_edit_mode(TRUE)
      
      shiny::updateTextInput(session, "first_author_last_name",
                             value = ctx$data$tbl7$first_author_last_name[row])
      shiny::updateTextInput(session, "title",
                             value = ctx$data$tbl7$title[row])
      shiny::updateSelectInput(session, "publication_type",
                               selected = ctx$data$tbl7$publication_type[row])
      shiny::updateTextInput(session, "publication_year",
                             value = as.character(ctx$data$tbl7$publication_year[row]))
      shiny::updateTextInput(session, "journal",
                             value = ctx$data$tbl7$journal[row])
      shiny::updateTextInput(session, "doi",
                             value = ctx$data$tbl7$doi[row])
    })
    
    # =====================================================
    # 7. DELETE
    # =====================================================
    shiny::observeEvent(input$delete_publication, {
      
      row <- publication_selected_row()
      
      if (!is.null(row) && row <= nrow(ctx$data$tbl7)) {
        ctx$data$tbl7 <- ctx$data$tbl7[-row, ]
        publication_selected_row(NULL)
        shiny::showNotification("Publication deleted.", type = "message")
      }
      
      clear_publication_fields(session)
      publication_form_visible(FALSE)
      publication_edit_mode(FALSE)
    })
    
    # =====================================================
    # 8. APPLY ORDER
    # =====================================================
    shiny::observeEvent(input$apply_publication_order, {
      
      shiny::req(input$tbl7)
      
      ordered <- rhandsontable::hot_to_r(input$tbl7)
      
      ordered$publication_year <- as.numeric(ordered$publication_year)
      ordered <- ordered[order(ordered$publication_year), ]
      ordered$publication_year <- as.character(ordered$publication_year)
      
      ctx$data$tbl7 <- ordered
    })
    
    # =====================================================
    # 9. TABLE SYNC
    # =====================================================
    shiny::observeEvent(input$tbl7, {
      ctx$data$tbl7 <- rhandsontable::hot_to_r(input$tbl7)
    })
    
    # =====================================================
    # 10. DOI SEARCH (STANDARDIZED API)
    # =====================================================
    observeEvent(input$search_doi, {
      
      req(input$doi_input)
      
      ctx$external_api$doi$fetch(input$doi_input)
    })
    
    observeEvent(ctx$external_api$doi$metadata, {
      
      meta <- format_doi_metadata(ctx$external_api$doi$metadata)
      req(meta)
      
      updateTextInput(session, "title", meta$title)
      updateTextInput(session, "first_author_last_name", meta$first_author)
      updateTextInput(session, "journal", meta$journal)
      updateTextInput(session, "publication_year", meta$year)
      updateTextInput(session, "doi", meta$doi)
      
      publication_form_visible(TRUE)
    })
    
    # =====================================================
    # 11. SAVE
    # =====================================================
    shiny::observeEvent(input$save_publication, {
      
      save_and_validate(
        data_reactive = ctx$data$tbl7,
        sheet_name = "publication",
        wb_reactive = shiny::reactive(ctx$files$wb_meta),
        temp_folder = ctx$files$temp_folder
      )
    })
    
  })
}
