#' Author Metadata Tab UI
#'
#' This UI module is designed for managing author metadata in a Shiny application. 
#' It allows users to view, add, update, delete, and reorder persons within a table. 
#' Additionally, it includes tools to retrieve information from ORCID and ROR databases 
#' to auto-fill author metadata fields.
#'
#' @param id A unique identifier for the namespace of the module.
#'
#' @return A Shiny UI component for managing author metadata.
#'
#' @import shiny
#' @import shinyjs
#' @import bslib 
#' @importFrom bsicons bs_icon
#' @importFrom htmltools tagList br HTML div
#' @importFrom DT DTOutput
#' @importFrom rhandsontable rHandsontableOutput
#' 
#' 
#' @export
mod_tab7_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # TAB 7: View Author -----------------------------------------------
  bslib::nav_panel(
    title = htmltools::div(id = ns("author_tab"), "Person"),
    value = "Person",
    
    shiny::fluidRow(
      # Left Sidebar - Save Button
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_body(
            shiny::actionButton(ns("save_person"), label = htmltools::tagList(bsicons::bs_icon("save"), 'Save'), class = "btn-primary")
          )
        )
      ),
      
      # Right Column - Main Content
      column(
        11, style = "height: 100%;",
        shinyjs::useShinyjs(),
        shinyjs::hidden(shiny::verbatimTextOutput(ns("form_visible"), placeholder = TRUE)),
        
        # Author Metadata Table and Action Buttons
        bslib::card(
          bslib::tooltip(
            bslib::card_header("Persons Table"),
            bsicons::bs_icon("question-circle"),
            htmltools::HTML(
              "This is the person metadata table. You can view, add, update, delete, and reorder persons using the buttons below.<br><br>
               Cells with <b>yellow text</b> are editable.<br>
               Cells highlighted in <b>red</b> indicate validation issues and require correction.<br>
               Hover over a red cell to see a tooltip explaining the issue."
            ),
            placement = "right"
          ),
          card_body(
            rhandsontable::rHandsontableOutput(ns("tbl6")),
            htmltools::br(),
            htmltools::div(
              style = "display: flex; gap: 10px; flex-wrap: wrap; align-items: center;",
              bslib::tooltip(
                shiny::actionButton(ns("show_add_person"), label = htmltools::tagList(bsicons::bs_icon("person-plus"), "Add New person"), class = "btn-success"),
                "Add a new person to the table. This opens the metadata form to help guide data entry.",
                placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("update_person"), label = htmltools::tagList(bsicons::bs_icon("pencil-square"), "Update person"), class = "btn-warning"),
                "Edit metadata of a selected person. Choose an person in the table to activate this button.",
                placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("delete_person"), label = htmltools::tagList(bsicons::bs_icon("trash"), "Delete person"), class = "btn-danger"),
                "Delete the selected person from the table. You must first select a row.",
                placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("apply_order"), label = htmltools::tagList(bsicons::bs_icon("sort-down"), "Apply person Order")),
                "Reorder persons in the table according to the values in the 'person_order' column. Edit that column and click this button to sort accordingly.",
                placement = "right"
              )
            )
          )
        ),
        
        # Conditional Panel - Show Form When Active
        conditionalPanel(
          condition = sprintf("output['%s'] == 'TRUE'", ns("form_visible")),
          
          # Accordion for ORCID and ROR Search
          bslib::accordion(
            open = c("ORCID Search", "ROR Search", "Person Metadata"),
            bslib::accordion_panel(
              title = "ORCID Search",
              id = ns("card_header_orcid"),
              tooltip(
                bsicons::bs_icon("question-circle"),
                htmltools::HTML("<b>Use this tool to search for a persons ORCID ID using their name or an existing ORCID.</b><br>
                                A table with the search results will appear.<br>
                                Select the correct row to retrieve the info in the Metadata form below.<br>"),
                placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(3,
                       bslib::tooltip(
                         shiny::actionButton(ns("search_orcid"), label = htmltools::tagList(bsicons::bs_icon("search"), "Retrieve from ORCID DB")),
                         "Search the ORCID database with the provided first and last name, or ORCID ID. This will fill the form below.",
                         placement = "right"
                       )
                ),
                shiny::column(3, shiny::textInput(ns("first_name_search"), "First Name", placeholder = "First Name")),
                shiny::column(3, shiny::textInput(ns("last_name_search"), "Last Name",    placeholder = "Last Name")),
                shiny::column(3, shiny::textInput(ns("orcid_search"), "ORCID ID",         placeholder = "e.g. 0000-0002-1825-0097"))
              )
            ),
            shiny::fluidRow(
              shiny::column(12,
                            h4("ORCID Search Results:"),
                            DT::DTOutput(ns("orcid_results_table"))
              )
            ),
            
            bslib::accordion_panel(
              title = "ROR Search",
              id = ns("card_header_ror"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle"),
                htmltools::HTML(
                  "<b>Use this tool to search for a Research Organization Registry (ROR) using their name or an existing ORCID.</b><br>
                    A table with the search results will appear.<br>
                    Select the correct row to retrieve the info in the Metadata form below.<br>"
                ),
                placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::actionButton(ns("search_ror"), label = htmltools::tagList(bsicons::bs_icon("search"), "Retrieve from ROR DB"))),
                shiny::column(3, shiny::selectInput(ns("country_code"), "Select country:", choices = c(Choose = '', get_country_codes()), selectize = TRUE)),
                shiny::column(3, shiny::textInput(ns("search_string"), "Search ROR", placeholder = "e.g. University of Oxford"))
              ),
              shiny::fluidRow(
                shiny::column(12,
                       h4("ROR Search Results:"),
                       DT::DTOutput(ns("ror_results"))
                )
              )
            ),
            
            bslib::accordion_panel(
              title = "Person Metadata",
              id = ns("card_header_metadata"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle"),
                "Fill in or edit the metadata of an person. Use ORCID and ROR tools to auto-complete relevant fields.",
                placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::selectInput(ns("person_role"), htmltools::HTML("Person Role <span style='color:red;'>*</span>"), choices = c("Contact and Principal Investigator","Principal Investigator","Contact","Contributor"))),
                shiny::column(3, shiny::numericInput(ns("person_order"), htmltools::HTML("Order <span style='color:red;'>*</span>"), value = 1, min = 1)),
                shiny::column(3, shiny::textInput(ns("last_name"), htmltools::HTML("Last Name <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("first_name"), htmltools::HTML("First Name <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("email"), htmltools::HTML("Email <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("orcid"), htmltools::HTML("ORCID ID <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("main_organization_name"), htmltools::HTML("Organization <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("main_organization_registry"), htmltools::HTML("ROR ID <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("department"), "Department")),
                shiny::column(3, shiny::textInput(ns("street"),     "Street")),
                shiny::column(3, shiny::textInput(ns("postal_code"),"Postal Code")),
                shiny::column(3, shiny::textInput(ns("city"),       htmltools::HTML("City <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("organization_country"), htmltools::HTML("Country <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("organization_country_code"), htmltools::HTML("Country Code <span style='color:red;'>*</span>")))
              ),
              htmltools::br(),
              bslib::tooltip(
                shiny::actionButton(ns("add_person"), label = htmltools::tagList(bsicons::bs_icon("person-fill-add"), "Add person"), class = "btn-success"),
                "Click to add the current person metadata to the persons table above. All required fields must be filled.",
                placement = "right"
              )
            )
          )
        )
      )
    )
  )
}

#' Server Logic for the person Metadata Tab
#'
#' This module handles the server-side logic for the person Metadata Tab in the app.
#' It includes functionalities for adding, editing, and deleting person records, 
#' validating form inputs, and interacting with external APIs (ORCID, ROR) for person data retrieval.
#'
#' @param id A string representing the unique identifier for the module UI.
#' @param out_tab1 A reactive object containing the dataset name and observation file.
#' @param out_tab2 A reactive object containing the metadata file and validation results.
#' @param out_tab3 A reactive object containing the workbook reference and column configurations.
#' @param out_tab4 A reactive object containing the tree metadata table and other related data.
#'
#' @return NULL
#'
#' @import shiny
#' @import rhandsontable
#' @import dplyr 
#' @import shinyjs
#' @import httr
#' @import jsonlite
#' @import openxlsx
#' @importFrom DT renderDT
#' @importFrom tidyr separate
#' 
mod_tab7_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {
    
    selected_row <- reactiveVal(NULL)
    edit_mode <- reactiveVal(FALSE)
    
    # =========================
    # INITIAL LOAD
    # =========================
    observe({
      req(ctx$files$wb_meta)
      
      ctx$data$tbl6 <- crud_load_excel(
        wb = ctx$files$wb_meta,
        sheet = "person",
        skip_rows = ctx$config$skip_rows_excel
      )
    })
    
    # =========================
    # RENDER
    # =========================
    output$tbl6 <- rhandsontable::renderRHandsontable({
      req(ctx$data$tbl6)
      
      rh <- rhandsontable::rhandsontable(
        ctx$data$tbl6,
        rowHeaders = NULL,
        stretchH = "all",
        selectCallback = TRUE,
        height = 150
      )
      
      rh
    })
    
    # =========================
    # SYNC
    # =========================
    observeEvent(input$tbl6, {
      req(input$tbl6)
      ctx$data$tbl6 <- rhandsontable::hot_to_r(input$tbl6)
    })
    
    # =========================
    # SELECTION
    # =========================
    observeEvent(input$tbl6_select$select$r, {
      selected_row(input$tbl6_select$select$r)
    })
    
    # =========================
    # FORM CONTROL
    # =========================
    observeEvent(input$show_add_person, {
      ctx$ui$tab7_form_visible <- TRUE
      edit_mode(FALSE)
      selected_row(NULL)
    })
    
    # =========================
    # ADD / UPDATE
    # =========================
    observeEvent(input$add_person, {
      
      req(ctx$data$tbl6)
      
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
      
      ctx$data$tbl6 <- crud_add_update(
        data = ctx$data$tbl6,
        new_row = new_row,
        edit_mode = edit_mode(),
        selected_row = selected_row()
      )
      
      ctx$ui$tab7_form_visible <- FALSE
      edit_mode(FALSE)
      selected_row(NULL)
    })
    
    # =========================
    # DELETE
    # =========================
    observeEvent(input$delete_person, {
      
      ctx$data$tbl6 <- crud_delete(
        data = ctx$data$tbl6,
        selected_row = selected_row()
      )
      
      selected_row(NULL)
      ctx$ui$tab7_form_visible <- FALSE
      edit_mode(FALSE)
    })
    
    # =========================
    # SAVE
    # =========================
    observeEvent(input$save_person, {
      save_and_validate(
        data_reactive = ctx$data$tbl6,
        sheet_name = "person",
        wb_reactive = ctx$files$wb_meta,
        temp_folder = ctx$files$temp_folder,
        update_validation = ctx$validation$results
      )
    })
    
  })
}
