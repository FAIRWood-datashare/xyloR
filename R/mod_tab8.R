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
#' UI for Publication Tab
mod_tab8_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = htmltools::div(id = ns("publication_tab"), "Publication"),
    value = "Publication",
    
    shiny::fluidRow(
      
      # Sidebar save
      shiny::column(
        1, class = "bg-light p-2 border-end",
        bslib::card(
          bslib::card_body(
            shiny::actionButton(
              ns("save_publication"),
              label = htmltools::tagList(bsicons::bs_icon("save"), "Save"),
              class = "btn-primary"
            )
          )
        )
      ),
      
      # Main panel
      shiny::column(
        11,
        shinyjs::useShinyjs(),
        
        shinyjs::hidden(
          shiny::verbatimTextOutput(ns("publication_form_visible"))
        ),
        
        bslib::card(
          bslib::tooltip(
            bslib::card_header("Publication Table"),
            bsicons::bs_icon("question-circle"),
            htmltools::HTML("Manage publication metadata."),
            placement = "right"
          ),
          
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl7")),
            htmltools::br(),
            
            htmltools::div(
              style = "display:flex; gap:10px; flex-wrap:wrap;",
              
              bslib::tooltip(
                shiny::actionButton(ns("show_add_publication"),
                                    htmltools::tagList(bsicons::bs_icon("book"), "Add Publication"),
                                    class = "btn-success"
                ),
                "Add publication",
                placement = "right"
              ),
              
              bslib::tooltip(
                shiny::actionButton(ns("update_publication"),
                                    htmltools::tagList(bsicons::bs_icon("pencil-square"), "Update"),
                                    class = "btn-warning"
                ),
                "Update selected publication",
                placement = "right"
              ),
              
              bslib::tooltip(
                shiny::actionButton(ns("delete_publication"),
                                    htmltools::tagList(bsicons::bs_icon("trash"), "Delete"),
                                    class = "btn-danger"
                ),
                "Delete selected publication",
                placement = "right"
              ),
              
              shiny::actionButton(
                ns("apply_publication_order"),
                htmltools::tagList(bsicons::bs_icon("sort-down"), "Apply Order")
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
              id = ns("doi_panel"),
              
              shiny::fluidRow(
                shiny::column(
                  3,
                  shiny::actionButton(
                    ns("search_doi"),
                    htmltools::tagList(bsicons::bs_icon("search"), "Search DOI")
                  )
                ),
                shiny::column(
                  9,
                  shiny::textInput(ns("doi_input"), "DOI")
                )
              ),
              
              shiny::verbatimTextOutput(ns("doi_result"))
            ),
            
            bslib::accordion_panel(
              title = "Publication Metadata",
              
              shiny::fluidRow(
                shiny::column(4, shiny::textInput(ns("first_author_last_name"), "First Author")),
                shiny::column(4, shiny::textInput(ns("title"), "Title")),
                shiny::column(4, shiny::selectInput(
                  ns("publication_type"),
                  "Type",
                  choices = c("article", "thesis", "report", "other")
                ))
              ),
              
              shiny::fluidRow(
                shiny::column(4, shiny::textInput(ns("publication_year"), "Year")),
                shiny::column(4, shiny::textInput(ns("journal"), "Journal")),
                shiny::column(4, shiny::textInput(ns("doi"), "DOI"))
              ),
              
              shiny::actionButton(
                ns("add_publication_data"),
                "Add Publication",
                class = "btn-success"
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
mod_tab8_server <- function(id, ctx) {
  moduleServer(id, function(input, output, session) {
    
    selected_row <- reactiveVal(NULL)
    edit_mode <- reactiveVal(FALSE)
    
    # =========================
    # INITIAL LOAD
    # =========================
    observe({
      req(ctx$files$wb_meta)
      
      ctx$data$tbl7 <- crud_load_excel(
        wb = ctx$files$wb_meta,
        sheet = "tab8",
        skip_rows = ctx$config$skip_rows_excel
      )
    })
    
    # =========================
    # RENDER
    # =========================
    output$tbl7 <- rhandsontable::renderRHandsontable({
      req(ctx$data$tbl7)
      
      rhandsontable::rhandsontable(
        ctx$data$tbl7,
        rowHeaders = NULL,
        stretchH = "all",
        selectCallback = TRUE,
        height = 150
      )
    })
    
    # =========================
    # SYNC
    # =========================
    observeEvent(input$tbl7, {
      req(input$tbl7)
      ctx$data$tbl7 <- rhandsontable::hot_to_r(input$tbl7)
    })
    
    # =========================
    # ADD ROW
    # =========================
    observeEvent(input$add_row, {
      
      req(ctx$data$tbl7)
      
      new_row <- tibble::tibble(
        field1 = input$field1,
        field2 = input$field2
      )
      
      ctx$data$tbl7 <- crud_add_update(
        data = ctx$data$tbl7,
        new_row = new_row,
        edit_mode = edit_mode(),
        selected_row = selected_row()
      )
      
      edit_mode(FALSE)
      selected_row(NULL)
    })
    
    # =========================
    # DELETE
    # =========================
    observeEvent(input$delete_row, {
      
      ctx$data$tbl7 <- crud_delete(
        data = ctx$data$tbl7,
        selected_row = selected_row()
      )
      
      selected_row(NULL)
      edit_mode(FALSE)
    })
    
    # =========================
    # SAVE
    # =========================
    observeEvent(input$save_tab8, {
      save_and_validate(
        data_reactive = ctx$data$tbl7,
        sheet_name = "tab8",
        wb_reactive = ctx$files$wb_meta,
        temp_folder = ctx$files$temp_folder,
        update_validation = ctx$validation$results
      )
    })
    
  })
}
