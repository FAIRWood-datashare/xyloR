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
            shiny::actionButton(ns("save_person"), label = htmltools::tagList(bsicons::bs_icon("save"), 'Save person'), class = "btn-primary")
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
                shiny::column(3, shiny::selectInput(ns("person_role"), htmltools::HTML("Person Role <span style='color:red;'>*</span>"), choices = c("Contact and Data owner","Data owner","Contact","Contributor"))),
                shiny::column(3, shiny::numericInput(ns("person_order"), htmltools::HTML("Order <span style='color:red;'>*</span>"), value = 1, min = 1)),
                shiny::column(3, shiny::textInput(ns("last_name"), htmltools::HTML("Last Name <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("first_name"), htmltools::HTML("First Name <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("email"), htmltools::HTML("Email <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("orcid"), htmltools::HTML("ORCID ID <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("organization_name"), htmltools::HTML("Organization <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("research_organization_registry"), htmltools::HTML("ROR ID <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("department"), "Department")),
                shiny::column(3, shiny::textInput(ns("street"),     "Street")),
                shiny::column(3, shiny::textInput(ns("postal_code"),"Postal Code")),
                shiny::column(3, shiny::textInput(ns("city"),       htmltools::HTML("City <span style='color:red;'>*</span>")))
              ),
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("country"), htmltools::HTML("Country <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("person_country_code"), htmltools::HTML("Country Code <span style='color:red;'>*</span>"))),
                shiny::column(3, shiny::textInput(ns("webpage"),  "Webpage")),
                shiny::column(3, shiny::textInput(ns("phone_number"), "Phone Number"))
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
mod_tab7_server <- function(id, out_tab1, out_tab2, out_tab3, out_tab4) {
  moduleServer(id, function(input, output, session) {
    

    # TAB 7 person: -------------------------------------------------------------------
    
    clear_person_fields <- function(session) {
      updateSelectInput(session, "person_role", selected = "")
      updateNumericInput(session, "person_order", value = NA)
      updateTextInput(session, "last_name", value = "")
      updateTextInput(session, "first_name", value = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "orcid", value = "")
      updateTextInput(session, "organization_name", value = "")
      updateTextInput(session, "research_organization_registry", value = "")
      # updateTextInput(session, "organization_name_finder", value = "")
      updateTextInput(session, "department", value = "")
      updateTextInput(session, "street", value = "")
      updateTextInput(session, "postal_code", value = "")
      updateTextInput(session, "city", value = "")
      updateTextInput(session, "country", value = "")
      updateTextInput(session, "person_country_code", value = "")
      updateTextInput(session, "webpage", value = "")
      updateTextInput(session, "phone_number", value = "")
      
      # Also clear the ORCID search fields
      updateTextInput(session, "first_name_search", value = "")
      updateTextInput(session, "last_name_search", value = "")
      updateTextInput(session, "orcid_search", value = "")
      
      # Clear the ROR search fields
      updateTextInput(session, "ror_search", value = "")
      updateTextInput(session, "ror_name_search", value = "")
      updateTextInput(session, "ror_id_search", value = "")
    }
    
    form_visible <- shiny::reactiveVal(FALSE)
    edit_mode <- shiny::reactiveVal(FALSE)
    selected_row <- shiny::reactiveVal(NULL)
    
    
    # Show form when Insert new person is clicked
    observeEvent(input$show_add_person, {
      form_visible(TRUE)
    })
    
    output$form_visible <- renderText({
      as.character(form_visible())
    })
    
    outputOptions(output, "form_visible", suspendWhenHidden = FALSE)
    
    # Color field red or green
    observe({
      fields <- c("person_role", "last_name", "first_name", "email", "orcid", 
                  "organization_name", "research_organization_registry", 
                  "department", "street", 
                  "postal_code", "city", "country", "person_country_code", 
                  "webpage", "phone_number")
      
      lapply(fields, function(field) {
        if (!is.null(input[[field]])) {
          border_color <- if (isTruthy(input[[field]])) "green" else "red"
          
          # Attempt both normal and selectize style (textInput / selectInput)
          shinyjs::runjs(sprintf('
        var el = $("#%s");
        if (el.length) {
          el.css("border", "2px solid %s");
        } else {
          var selectizeEl = $("#%s .selectize-input");
          if (selectizeEl.length) {
            selectizeEl.css("border", "2px solid %s");
          }
        }',
                                 field, border_color, field, border_color
          ))
        }
      })
    })
    
    
    #### dperson  ####
    dperson <- shiny::reactiveVal()
    out_tab4$data_meta <- shiny::reactiveValues(tbl6 = NULL)
    
    observe({
      req(out_tab2$meta_file)
      person_meta_info <- openxlsx::readWorkbook(out_tab3$WB_meta(), sheet = "person", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
        tibble::tibble() %>%
        dplyr::rename(organization_name_finder = `organization_name.(finder)`)
      dperson(person_meta_info)  # Store in reactive value
    })
    
    # Reactive context to store initial data and ensure it's updated in a proper context
    observe({
      req(dperson())
      out_tab4$data_meta$tbl6 <- dperson()
    })
    
    # RENDER TABLES
    output$tbl6 <- rhandsontable::renderRHandsontable({
      shiny::req(out_tab4$data_meta$tbl6)  # Ensure data is available
      rhandsontable::rhandsontable(
        out_tab4$data_meta$tbl6,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = 'all', selectCallback = TRUE, height=150) %>%
        hot_col_wrapper('person_role', out_tab3$column_configs()$tbl6$person_role) %>%
        hot_col_wrapper('person_order', out_tab3$column_configs()$tbl6$person_order) %>%
        hot_col_wrapper('last_name', out_tab3$column_configs()$tbl6$last_name) %>%
        hot_col_wrapper('first_name', out_tab3$column_configs()$tbl6$first_name) %>%
        hot_col_wrapper('email', out_tab3$column_configs()$tbl6$email) %>%
        hot_col_wrapper('orcid', out_tab3$column_configs()$tbl6$orcid) %>%
        hot_col_wrapper('organization_name', out_tab3$column_configs()$tbl6$organization_name) %>%
        hot_col_wrapper('research_organization_registry', out_tab3$column_configs()$tbl6$research_organization_registry) %>%
        # hot_col_wrapper('organization_name_finder', column_configs$tbl6$organization_name_finder) %>%
        hot_col_wrapper('department', out_tab3$column_configs()$tbl6$department) %>%
        hot_col_wrapper('street', out_tab3$column_configs()$tbl6$street) %>%
        hot_col_wrapper('postal_code', out_tab3$column_configs()$tbl6$postal_code) %>%
        hot_col_wrapper('city', out_tab3$column_configs()$tbl6$city) %>%
        hot_col_wrapper('country', out_tab3$column_configs()$tbl6$country) %>%
        hot_col_wrapper('person_country_code', out_tab3$column_configs()$tbl6$person_country_code) %>%
        hot_col_wrapper('webpage', out_tab3$column_configs()$tbl6$webpage) %>%
        hot_col_wrapper('phone_number', out_tab3$column_configs()$tbl6$phone_number)
    })
    
    observeEvent(input$tbl6_select$select$r, {
      selected_row(input$tbl6_select$select$r)
    })
    
    observeEvent(input$add_person, {
      new_entry <- data.frame(
        person_role = as.character(input$person_role),
        person_order = input$person_order,
        last_name = as.character(input$last_name),
        first_name = as.character(input$first_name),
        email = as.character(input$email),
        orcid = as.character(input$orcid),
        organization_name = as.character(input$organization_name),
        research_organization_registry = as.character(input$research_organization_registry),
        # organization_name_finder = as.character(input$organization_name_finder),
        organization_name_finder = NA_character_,
        department = as.character(input$department),
        street = as.character(input$street),
        postal_code = as.character(input$postal_code),
        city = as.character(input$city),
        country = as.character(input$country),
        person_country_code = as.character(input$person_country_code),
        webpage = as.character(input$webpage),
        phone_number = as.character(input$phone_number),
        stringsAsFactors = FALSE
      )
      
      if (edit_mode() && !is.null(selected_row())) {
        new_row$person_order <- as.character(new_row$person_order)
        out_tab4$data_meta$tbl6[selected_row(), names(new_entry)] <- as.list(new_entry[1, ])
      } else {
        out_tab4$data_meta$tbl6 <- rbind(out_tab4$data_meta$tbl6, new_entry)
      }
      
      clear_person_fields(session)
      form_visible(FALSE)
      # updateActionButton(session, "show_add_person", label = tagList(bsicons::bs_icon("person-plus"), "Add New person"))
      
      edit_mode(FALSE)
      selected_row(NULL)
    })
    
    observeEvent(input$update_person, {
      req(selected_row())
      
      row <- selected_row()
      form_visible(TRUE)
      edit_mode(TRUE)
      
      isolate({
        fields <- c("person_role", "last_name", "first_name", "email", "orcid", 
                    "organization_name", "research_organization_registry", 
                    "department", "street", 
                    "postal_code", "city", "country", "person_country_code", 
                    "webpage", "phone_number")
        
        for (field in fields) {
          value <- out_tab4$data_meta$tbl6[[field]][row]
          
          # Defensive conversion if needed
          if (is.na(value) || is.null(value)) value <- ""
          if (!is.character(value)) value <- as.character(value)
          
          updateTextInput(session, field, value = value)
        }
      })
    })
    
    observeEvent(input$delete_person, {
      row <- selected_row()
      
      if (!is.null(row) && row <= nrow(out_tab4$data_meta$tbl6)) {
        out_tab4$data_meta$tbl6 <- out_tab4$data_meta$tbl6[-row, ]
        selected_row(NULL)
        
        showNotification("Person deleted.", type = "message")
      } else {
        showNotification("Please select a row to delete.", type = "warning")
      }
      
      out_tab4$data_meta$tbl6$person_order <- seq_len(nrow(out_tab4$data_meta$tbl6))
      
      clear_person_fields(session)
      form_visible(FALSE)
      edit_mode(FALSE)
    })
    
    observe({
      shinyjs::toggleState("update_person", condition = !is.null(selected_row()))
      shinyjs::toggleState("delete_person", condition = !is.null(selected_row()))
    })
    
    observeEvent(input$apply_order, {
      req(input$tbl6)
      if (!is.null(input$tbl6)) {
        ordered_data <- rhandsontable::hot_to_r(input$tbl6)
        ordered_data$person_order <- as.integer(ordered_data$person_order)
        ordered_data <- ordered_data[order(ordered_data$person_order), ]
        ordered_data$person_order <- seq_len(nrow(ordered_data))
        out_tab4$data_meta$tbl6 <- ordered_data
      }
    })
    
    # ORCID Search
    orcid_data <- shiny::reactiveValues(results = NULL)
    
    # Search via Orcid
    # observeEvent(input$search_orcid, {
    #   # If ORCID ID is provided, use that
    #   if (nzchar(input$orcid_search)) {
    #     orcid_id <- gsub("https?://orcid.org/", "", trimws(input$orcid_search))
    #     orcid_url <- paste0("https://pub.orcid.org/v3.0/", orcid_id)
    #     
    #     res <- httr::GET(orcid_url, httr::add_headers(Accept = "application/json"))
    #     
    #     if (httr::status_code(res) == 200) {
    #       parsed <- jsonlite::fromJSON(rawToChar(res$content))
    #       
    #       updateTextInput(session, "orcid", value = orcid_id)
    #       updateTextInput(session, "last_name", value = parsed$`person`$`name`$`family-name`$value)
    #       updateTextInput(session, "first_name", value = parsed$`person`$`name`$`given-names`$value)
    #       
    #       email_val <- tryCatch(parsed$`person`$emails$email[[1]]$email, error = function(e) "")
    #       updateTextInput(session, "email", value = email_val)
    #       
    #       if (!is.null(parsed$`activities-summary`$`employments`$`employment-summary`)) {
    #         org_name <- parsed$`activities-summary`$`employments`$`employment-summary`[[1]]$`organization`$`name`
    #         updateTextInput(session, "organization_name", value = org_name)
    #       }
    #       
    #     } else {
    #       showNotification("Failed to retrieve data for provided ORCID ID.", type = "error")
    #     }
    #     
    #     # Else if names are provided, do a name-based search
    #   } else if (nzchar(input$first_name_search) && nzchar(input$last_name_search)) {
    #     given <- trimws(input$first_name_search)
    #     family <- trimws(input$last_name_search)
    #     query_name <- URLencode(paste0("given-names:", given, " AND family-name:", family))
    #     search_url <- sprintf("https://pub.orcid.org/v3.0/expanded-search?q=%s", query_name)
    #     
    #     res <- httr::GET(search_url, httr::add_headers(Accept = "application/json"))
    #     
    #     if (httr::status_code(res) == 200) {
    #       parsed <- jsonlite::fromJSON(rawToChar(res$content))
    #       if (!is.null(parsed$`expanded-result`) && nrow(parsed$`expanded-result`) > 0) {
    #         results <- tibble::tibble(
    #           ORCID = parsed$`expanded-result`$`orcid-id`,
    #           LastName = parsed$`expanded-result`$`family-names`,
    #           FirstName = parsed$`expanded-result`$`given-names`,
    #           Email = if (!is.null(parsed$`expanded-result`$email) && length(parsed$`expanded-result`$email) > 0) paste(parsed$`expanded-result`$email[[1]], collapse = ", ") else "",
    #           Organization = if (!is.null(parsed$`expanded-result`$`institution-name`) && length(parsed$`expanded-result`$`institution-name`) > 0) paste(parsed$`expanded-result`$`institution-name`[[1]], collapse = ", ") else ""
    #         )
    #         
    #         updateTextInput(session, "orcid", value = results$ORCID[1])
    #         updateTextInput(session, "last_name", value = results$LastName[1])
    #         updateTextInput(session, "first_name", value = results$FirstName[1])
    #         updateTextInput(session, "email", value = results$Email[1])
    #         updateTextInput(session, "organization_name", value = results$Organization[1])
    #         
    #         orcid_data$results <- results
    #       } else {
    #         showNotification("No ORCID results found for that name.", type = "message")
    #       }
    #     } else {
    #       showNotification("ORCID name search failed.", type = "error")
    #     }
    #     
    #   } else {
    #     showNotification("Please provide either an ORCID ID or both first and last name.", type = "warning")
    #   }
    # })
    
    observeEvent(input$search_orcid, {
      
      # Build query
      query <- NULL
      if (nzchar(input$orcid_search)) {
        orcid_id <- gsub("https?://orcid.org/", "", trimws(input$orcid_search))
        if (grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", orcid_id)) {
          query <- paste0("?q=orcid:", orcid_id)
        } else {
          showNotification("Invalid ORCID format.", type = "error")
          return()
        }
      } else if (nzchar(input$first_name_search) || nzchar(input$last_name_search)) {
        query_ln <- if (nzchar(input$last_name_search)) {
          sprintf('(family-name:(%s))', URLencode(gsub(" ", "+AND+", input$last_name_search)))
        } else ""
        
        query_fn <- if (nzchar(input$first_name_search)) {
          sprintf('(given-names:(%s))', URLencode(gsub(" ", "+AND+", input$first_name_search)))
        } else ""
        
        query <- paste0("?q=", query_ln, ifelse(nzchar(query_ln) && nzchar(query_fn), "+AND+", ""), query_fn)
      } else {
        showNotification("Please provide an ORCID or a name to search.", type = "warning")
        return()
      }
      
      # Perform CSV API request
      search_url <- paste0(
        "https://pub.orcid.org/v3.0/csv-search/", query,
        "&fl=family-name,given-names,email,orcid,current-institution-affiliation-name,other-names",
        "&rows=50"
      )
      
      res <- httr::GET(search_url, httr::timeout(5))
      
      if (httr::status_code(res) == 200) {
        orcid_df <- read.table(text = rawToChar(res$content), sep = ",", header = TRUE, stringsAsFactors = FALSE)
        
        if (nrow(orcid_df) > 0) {
          results <- orcid_df %>%
            rename(
              last_name = family.name,
              first_name = given.names,
              orcid_id = orcid,
              org_name = current.institution.affiliation.name,
              other_names = other.names
            ) %>%
            tidyr::separate(email, into = c("email"), sep = ",(?!\\s)", extra = "drop", remove = FALSE) %>%
            tidyr::separate(org_name, into = c("org_name"), sep = ",(?!\\s)", extra = "drop", remove = FALSE) %>%
            mutate(orcid_link = sprintf("<a href='https://orcid.org/%s' target='_blank'>%s</a>", orcid_id, orcid_id))
          
          orcid_data$results <- results
        } else {
          orcid_data$results <- NULL
          showNotification("No ORCID results found.", type = "message")
        }
        
      } else {
        orcid_data$results <- NULL
        showNotification("ORCID API request failed.", type = "error")
      }
    })
    
    output$orcid_results_table <- DT::renderDT({
      req(orcid_data$results)
      
      df <- orcid_data$results %>%
        select(
          `ORCID (clickable)` = orcid_link,
          `First Name` = first_name,
          `Last Name` = last_name,
          Email = email,
          Organization = org_name
        )
      
      DT::datatable(df,
                escape = FALSE,
                rownames = FALSE,
                selection = "single",
                options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    observeEvent(input$orcid_results_table_rows_selected, {
      selected <- input$orcid_results_table_rows_selected
      if (!is.null(selected) && length(selected) == 1) {
        row <- orcid_data$results[selected, ]
        updateTextInput(session, "orcid", value = row$orcid_id)
        updateTextInput(session, "first_name", value = row$first_name)
        updateTextInput(session, "last_name", value = row$last_name)
        updateTextInput(session, "email", value = row$email)
        updateTextInput(session, "organization_name", value = row$org_name)
      }
    })
  
  
  
  # ROR Search
    ror_data <- shiny::reactiveValues(results = NULL)
    
    observeEvent(input$search_ror, {
      req(input$country_code)
      req(input$search_string)
      
      search_url <- sprintf(
        'https://api.ror.org/v2/organizations?query=%s&filter=country.country_code:%s',
        URLencode(input$search_string),
        input$country_code)
      
      ror_res <- httr::GET(search_url, httr::timeout(5))
      
      if (httr::status_code(ror_res) == 200) {
        json <- jsonlite::fromJSON(rawToChar(ror_res$content))
        
        if (json$number_of_results > 0) {
          res_names <- json$items$names %>%
            dplyr::bind_rows() %>%
            filter(grepl('ror_display', types)) %>%
            dplyr::pull(value)
          
          res_locs <- json$items$locations %>% 
            dplyr::bind_rows() %>% 
            dplyr::pull(geonames_details) %>% 
            tidyr::unite(col = 'address', name, country_name, sep = ', ') %>% 
            dplyr::pull(address)
          
          
          res_web <- json$items$links %>%
            map_dfr(bind_rows) %>%
            filter(type == "website") %>%
            pull(value)
          
          res_df <- data.frame(
            ROR = json$items$id, 
            Name = res_names, 
            Location = res_locs,
            Website = res_web
          )
          
          # STORE into the reactiveValues object
          ror_data$results <- res_df |>
            tidyr::separate(Location, into = c("city", "country"), sep = ", ", remove = FALSE)
          
          
          output$ror_results <- DT::renderDT({
            DT::datatable(res_df, rownames = FALSE, selection = "single", escape = FALSE,
                          options = list(pageLength = 10, autoWidth = TRUE))
          })
        }
        else {
          showNotification("No ROR results found. Try again.", type = "message")
        }
      } else {
        showNotification("ROR API request failed. Try again.", type = "error")
      }
      
    })
    
    # Handle row selection from ROR results
    observeEvent(input$ror_results_rows_selected, {
      req(input$ror_results_rows_selected)
      sel <- input$ror_results_rows_selected
      ror_df <- ror_data$results
      ror_row <- ror_df[sel, ]
      
      # insert results into their corresponding fields
      updateTextInput(session, "organization_name", value = ror_row$Name[1])
      updateTextInput(session, "research_organization_registry", value = ror_row$ROR[1])
      updateTextInput(session, "city", value = ror_row$city[1])
      updateTextInput(session, "country", value = ror_row$country[1])
      updateTextInput(session, "person_country_code", value = countrycode::countrycode(ror_row$country[1], origin = "country.name", destination = "iso2c"))
      
      updateTextInput(session, "webpage", value = ror_row$Website[1])
      
      
    })
    
    # Sync data on user input
    shiny::observeEvent(input$tbl6, {
      shiny::req(input$tbl6)
      out_tab4$data_meta$tbl6 <- rhandsontable::hot_to_r(input$tbl6)
      # Update the reactive data object
    })
    
    
    shiny::observeEvent(input$save_person, {
      save_and_validate(
        data_reactive = out_tab4$data_meta$tbl6,
        sheet_name = "person",
        wb_reactive = out_tab3$WB_meta,
        temp_folder = out_tab1$temp_folder,
        update_validation = out_tab2$validation_results
      )
    })
    
  })
}