#' Author Metadata Tab UI
#'
#' @param id A unique identifier for the namespace of the module.
#' @return A Shiny UI component for managing author metadata.
#'
#' @import shiny
#' @import shinyjs
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom htmltools tagList br HTML div
#' @importFrom DT DTOutput
#' @importFrom rhandsontable rHandsontableOutput
#' @export
Zmod_tab7_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = htmltools::div(id = ns("author_tab"), "Person"),
    value = "Person",

    shiny::fluidRow(
      shiny::column(
        1, class = "bg-light p-2 border-end", style = "height: 100%;",
        bslib::card(
          bslib::card_body(
            shiny::actionButton(ns("save_person"),
              label = htmltools::tagList(bsicons::bs_icon("save"), "Save"),
              class = "btn-primary")
          )
        )
      ),

      shiny::column(
        11, style = "height: 100%;",
        shinyjs::useShinyjs(),
        shinyjs::hidden(shiny::verbatimTextOutput(ns("form_visible"), placeholder = TRUE)),

        bslib::card(
          bslib::tooltip(
            bslib::card_header("Persons Table"),
            bsicons::bs_icon("question-circle"),
            htmltools::HTML(
              "View, add, update, delete, and reorder persons.<br><br>
               Cells with <b>yellow text</b> are editable.<br>
               Cells highlighted in <b>red</b> indicate validation issues."
            ),
            placement = "right"
          ),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl6")),
            htmltools::br(),
            htmltools::div(
              style = "display: flex; gap: 10px; flex-wrap: wrap; align-items: center;",
              bslib::tooltip(
                shiny::actionButton(ns("show_add_person"),
                  label = htmltools::tagList(bsicons::bs_icon("person-plus"), "Add New person"),
                  class = "btn-success"),
                "Add a new person to the table.", placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("update_person"),
                  label = htmltools::tagList(bsicons::bs_icon("pencil-square"), "Update person"),
                  class = "btn-warning"),
                "Edit metadata of a selected person.", placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("delete_person"),
                  label = htmltools::tagList(bsicons::bs_icon("trash"), "Delete person"),
                  class = "btn-danger"),
                "Delete the selected person from the table.", placement = "right"
              ),
              bslib::tooltip(
                shiny::actionButton(ns("apply_order"),
                  label = htmltools::tagList(bsicons::bs_icon("sort-down"), "Apply person Order")),
                "Reorder persons by person_order column.", placement = "right"
              )
            )
          )
        ),

        shiny::conditionalPanel(
          condition = sprintf("output['%s'] == 'TRUE'", ns("form_visible")),
          bslib::accordion(
            open = c("ORCID Search", "ROR Search", "Person Metadata"),

            bslib::accordion_panel(
              title = "ORCID Search",
              id = ns("card_header_orcid"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle"),
                htmltools::HTML("<b>Search for a person's ORCID using name or ORCID ID.</b>"),
                placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(3,
                  bslib::tooltip(
                    shiny::actionButton(ns("search_orcid"),
                      label = htmltools::tagList(bsicons::bs_icon("search"), "Retrieve from ORCID DB")),
                    "Search ORCID database.", placement = "right"
                  )
                ),
                shiny::column(3, shiny::textInput(ns("first_name_search"), "First Name", placeholder = "First Name")),
                shiny::column(3, shiny::textInput(ns("last_name_search"),  "Last Name",  placeholder = "Last Name")),
                shiny::column(3, shiny::textInput(ns("orcid_search"),      "ORCID ID",   placeholder = "e.g. 0000-0002-1825-0097"))
              )
            ),
            shiny::fluidRow(
              shiny::column(12,
                shiny::h4("ORCID Search Results:"),
                DT::DTOutput(ns("orcid_results_table"))
              )
            ),

            bslib::accordion_panel(
              title = "ROR Search",
              id = ns("card_header_ror"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle"),
                htmltools::HTML("<b>Search for a Research Organization Registry (ROR).</b>"),
                placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::actionButton(ns("search_ror"),
                  label = htmltools::tagList(bsicons::bs_icon("search"), "Retrieve from ROR DB"))),
                shiny::column(3, shiny::selectInput(ns("country_code"), "Select country:",
                  choices = c(Choose = "", get_country_codes()), selectize = TRUE)),
                shiny::column(3, shiny::textInput(ns("search_string"), "Search ROR",
                  placeholder = "e.g. University of Oxford"))
              ),
              shiny::fluidRow(
                shiny::column(12,
                  shiny::h4("ROR Search Results:"),
                  DT::DTOutput(ns("ror_results"))
                )
              )
            ),

            bslib::accordion_panel(
              title = "Person Metadata",
              id = ns("card_header_metadata"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle"),
                "Fill in or edit person metadata. Use ORCID and ROR tools to auto-complete.",
                placement = "right"
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::selectInput(ns("person_role"),
                  htmltools::HTML("Person Role <span style='color:red;'>*</span>"),
                  choices = c("Contact and Principal Investigator", "Principal Investigator", "Contact", "Contributor"))),
                shiny::column(3, shiny::numericInput(ns("person_order"),
                  htmltools::HTML("Order <span style='color:red;'>*</span>"), value = 1, min = 1)),
                shiny::column(3, shiny::textInput(ns("last_name"),
                  htmltools::HTML("Last Name <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("first_name"),
                  htmltools::HTML("First Name <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("email"),
                  htmltools::HTML("Email <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("orcid"),
                  htmltools::HTML("ORCID ID <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("main_organization_name"),
                  htmltools::HTML("Organization <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("main_organization_registry"),
                  htmltools::HTML("ROR ID <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("department"), "Department")),
                shiny::column(3, shiny::textInput(ns("street"),     "Street")),
                shiny::column(3, shiny::textInput(ns("postal_code"), "Postal Code")),
                shiny::column(3, shiny::textInput(ns("city"),
                  htmltools::HTML("City <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("organization_country"),
                  htmltools::HTML("Country <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("organization_country_code"),
                  htmltools::HTML("Country Code <span style='color:red;'>*</span>")))
              ),
              htmltools::br(),
              bslib::tooltip(
                shiny::actionButton(ns("add_person"),
                  label = htmltools::tagList(bsicons::bs_icon("person-fill-add"), "Add person"),
                  class = "btn-success"),
                "Add current person metadata to the table.", placement = "right"
              )
            )
          )
        )
      )
    )
  )
}

#' Server Logic for the Person Metadata Tab
#'
#' @param id A string representing the unique identifier for the module UI.
#' @param ctx The centralized app context (environment).
#' @param session The parent Shiny session.
#'
#' @import shiny
#' @import rhandsontable
#' @import dplyr
#' @import shinyjs
#' @import httr
#' @import jsonlite
#' @import openxlsx
#' @importFrom DT renderDT datatable
#' @importFrom tidyr separate
#' @export
#' 
#' # ---- ORCID HELPERS -------------------------------------------------

Zmod_tab7_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {
    
    # =====================================================
    # LOCAL STATE
    # =====================================================
    form_visible <- shiny::reactiveVal(FALSE)
    edit_mode    <- shiny::reactiveVal(FALSE)
    selected_row <- shiny::reactiveVal(NULL)
    
    orcid_data <- shiny::reactiveVal(NULL)
    
    # =====================================================
    # CLEAR FORM
    # =====================================================
    clear_person_fields <- function(session) {
      
      fields <- c(
        "person_role", "person_order",
        "last_name", "first_name", "email", "orcid",
        "main_organization_name", "main_organization_registry",
        "department", "street", "postal_code", "city",
        "organization_country", "organization_country_code",
        "first_name_search", "last_name_search", "orcid_search"
      )
      
      lapply(fields, function(f) {
        shiny::updateTextInput(session, f, value = "")
      })
      
      shiny::updateSelectInput(session, "person_role", selected = "")
      shiny::updateNumericInput(session, "person_order", value = NA)
    }
    
    # =====================================================
    # DATA INIT
    # =====================================================
    dperson <- shiny::reactiveVal()
    
    observe({
      req(ctx$files$wb_meta)
      
      df <- openxlsx::readWorkbook(
        ctx$files$wb_meta,
        sheet = "person",
        startRow = 1,
        colNames = TRUE
      )[-(1:6), ] |>
        tibble::as_tibble()
      
      dperson(df)
    })
    
    observe({
      req(dperson())
      ctx$data$tbl6 <- dperson()
    })
    
    # =====================================================
    # FORM VISIBILITY
    # =====================================================
    observeEvent(input$show_add_person, {
      form_visible(TRUE)
      edit_mode(FALSE)
      clear_person_fields(session)
    })
    
    # =====================================================
    # TABLE
    # =====================================================
    output$tbl6 <- rhandsontable::renderRHandsontable({
      req(ctx$data$tbl6)
      
      rhandsontable::rhandsontable(
        ctx$data$tbl6,
        rowHeaders = NULL,
        contextMenu = TRUE,
        stretchH = "all",
        selectCallback = TRUE,
        height = 150
      )
    })
    
    # =====================================================
    # ROW SELECT
    # =====================================================
    observeEvent(input$tbl6_select$select$r, {
      
      selected_row(input$tbl6_select$select$r)
      
      shinyjs::toggleState("update_person",
                           condition = !is.null(selected_row()))
      shinyjs::toggleState("delete_person",
                           condition = !is.null(selected_row()))
    })
    
    # =====================================================
    # ADD / EDIT
    # =====================================================
    observeEvent(input$add_person, {
      
      req(input$last_name, input$first_name)
      
      new_entry <- data.frame(
        person_role = input$person_role,
        person_order = input$person_order,
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
        organization_country_code = input$organization_country_code,
        stringsAsFactors = FALSE
      )
      
      if (edit_mode() && !is.null(selected_row())) {
        ctx$data$tbl6[selected_row(), ] <- new_entry
      } else {
        ctx$data$tbl6 <- rbind(ctx$data$tbl6, new_entry)
      }
      
      clear_person_fields(session)
      form_visible(FALSE)
      edit_mode(FALSE)
      selected_row(NULL)
    })
    
    # =====================================================
    # UPDATE FORM
    # =====================================================
    observeEvent(input$update_person, {
      
      req(selected_row())
      row <- selected_row()
      
      form_visible(TRUE)
      edit_mode(TRUE)
      
      for (f in names(ctx$data$tbl6)) {
        val <- ctx$data$tbl6[[f]][row]
        if (is.na(val)) val <- ""
        
        if (f %in% c("person_role")) {
          updateSelectInput(session, f, selected = val)
        } else if (f == "person_order") {
          updateNumericInput(session, f, value = val)
        } else {
          updateTextInput(session, f, value = val)
        }
      }
    })
    
    # =====================================================
    # DELETE
    # =====================================================
    observeEvent(input$delete_person, {
      
      row <- selected_row()
      
      if (!is.null(row) && row <= nrow(ctx$data$tbl6)) {
        ctx$data$tbl6 <- ctx$data$tbl6[-row, ]
        selected_row(NULL)
      }
      
      clear_person_fields(session)
      form_visible(FALSE)
      edit_mode(FALSE)
    })
    
    # =====================================================
    # ORCID SEARCH (CTX CLEAN)
    # =====================================================
    observeEvent(input$search_orcid, {
      
      req(
        nzchar(input$orcid_search) ||
          nzchar(input$first_name_search) ||
          nzchar(input$last_name_search)
      )
      
      ctx$external_api$orcid$fetch(
        orcid  = input$orcid_search,
        given  = input$first_name_search,
        family = input$last_name_search
      )
    })
    
    observe({
      orcid_data(ctx$external_api$orcid$results)
    })
    
    # =====================================================
    # ORCID TABLE
    # =====================================================
    output$orcid_results_table <- DT::renderDT({
      
      req(orcid_data())
      
      DT::datatable(orcid_data(), escape = FALSE)
    })
    
    # =====================================================
    # APPLY ORCID ROW
    # =====================================================
    observeEvent(input$orcid_results_table_rows_selected, {
      
      sel <- input$orcid_results_table_rows_selected
      req(sel)
      
      row <- orcid_data()[sel, ]
      
      updateTextInput(session, "orcid", value = row$orcid_id)
      updateTextInput(session, "first_name", value = row$first_name)
      updateTextInput(session, "last_name", value = row$last_name)
      updateTextInput(session, "email", value = row$email)
      updateTextInput(session, "main_organization_name", value = row$org_name)
    })
    
    # =====================================================
    # SAVE
    # =====================================================
    observeEvent(input$save_person, {
      
      save_and_validate(
        data_reactive = ctx$data$tbl6,
        sheet_name = "person",
        wb_reactive = shiny::reactive(ctx$files$wb_meta),
        temp_folder = ctx$files$temp_folder
      )
    })
  })
}
