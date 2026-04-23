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
mod_tab7_ui <- function(id) {
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
mod_tab7_server <- function(id, ctx, session) {
  moduleServer(id, function(input, output, session) {

    # =====================================================
    # LOCAL STATE
    # =====================================================
    form_visible <- shiny::reactiveVal(FALSE)
    edit_mode    <- shiny::reactiveVal(FALSE)
    selected_row <- shiny::reactiveVal(NULL)

    clear_person_fields <- function(session) {
      shiny::updateSelectInput(session,  "person_role",               selected = "")
      shiny::updateNumericInput(session, "person_order",              value = NA)
      shiny::updateTextInput(session,    "last_name",                 value = "")
      shiny::updateTextInput(session,    "first_name",                value = "")
      shiny::updateTextInput(session,    "email",                     value = "")
      shiny::updateTextInput(session,    "orcid",                     value = "")
      shiny::updateTextInput(session,    "main_organization_name",    value = "")
      shiny::updateTextInput(session,    "main_organization_registry",value = "")
      shiny::updateTextInput(session,    "department",                value = "")
      shiny::updateTextInput(session,    "street",                    value = "")
      shiny::updateTextInput(session,    "postal_code",               value = "")
      shiny::updateTextInput(session,    "city",                      value = "")
      shiny::updateTextInput(session,    "organization_country",      value = "")
      shiny::updateTextInput(session,    "organization_country_code", value = "")
      shiny::updateTextInput(session,    "first_name_search",         value = "")
      shiny::updateTextInput(session,    "last_name_search",          value = "")
      shiny::updateTextInput(session,    "orcid_search",              value = "")
    }

    # =====================================================
    # 1. DATA INIT
    # =====================================================
    dperson <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(ctx$files$wb_meta)
      df <- openxlsx::readWorkbook(ctx$files$wb_meta, sheet = "person",
                                   startRow = 1, colNames = TRUE)[-(1:6), ] |>
        tibble::tibble()
      dperson(df)
    })

    shiny::observe({
      shiny::req(dperson())
      ctx$data$tbl6 <- dperson()
    })

    # =====================================================
    # 2. FORM VISIBILITY
    # =====================================================
    shiny::observeEvent(input$show_add_person, { form_visible(TRUE) })

    output$form_visible <- shiny::renderText(as.character(form_visible()))
    shiny::outputOptions(output, "form_visible", suspendWhenHidden = FALSE)

    # Field border colors
    shiny::observe({
      fields <- c("person_role", "last_name", "first_name", "email", "orcid",
                  "main_organization_name", "main_organization_registry",
                  "department", "street", "postal_code", "city",
                  "organization_country", "organization_country_code")
      lapply(fields, function(field) {
        if (!is.null(input[[field]])) {
          color <- if (shiny::isTruthy(input[[field]])) "green" else "red"
          shinyjs::runjs(sprintf(
            'var el=$("#%s");if(el.length){el.css("border","2px solid %s");}else{var s=$("#%s .selectize-input");if(s.length){s.css("border","2px solid %s");}}',
            field, color, field, color
          ))
        }
      })
    })

    # =====================================================
    # 3. RENDER TABLE
    # =====================================================
    output$tbl6 <- rhandsontable::renderRHandsontable({
      shiny::req(ctx$data$tbl6)
      col_cfg <- ctx$data$column_configs

      rhandsontable::rhandsontable(
        ctx$data$tbl6,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = "all",
        selectCallback = TRUE, height = 150
      ) |>
        hot_col_wrapper("person_role",                col_cfg$tbl6$person_role) |>
        hot_col_wrapper("person_order",               col_cfg$tbl6$person_order) |>
        hot_col_wrapper("last_name",                  col_cfg$tbl6$last_name) |>
        hot_col_wrapper("first_name",                 col_cfg$tbl6$first_name) |>
        hot_col_wrapper("email",                      col_cfg$tbl6$email) |>
        hot_col_wrapper("orcid",                      col_cfg$tbl6$orcid) |>
        hot_col_wrapper("main_organization_name",     col_cfg$tbl6$main_organization_name) |>
        hot_col_wrapper("main_organization_registry", col_cfg$tbl6$main_organization_registry) |>
        hot_col_wrapper("department",                 col_cfg$tbl6$department) |>
        hot_col_wrapper("street",                     col_cfg$tbl6$street) |>
        hot_col_wrapper("postal_code",                col_cfg$tbl6$postal_code) |>
        hot_col_wrapper("city",                       col_cfg$tbl6$city) |>
        hot_col_wrapper("organization_country",       col_cfg$tbl6$organization_country) |>
        hot_col_wrapper("organization_country_code",  col_cfg$tbl6$organization_country_code)
    })

    # =====================================================
    # 4. ROW SELECTION
    # =====================================================
    shiny::observeEvent(input$tbl6_select$select$r, {
      selected_row(input$tbl6_select$select$r)
    })

    shiny::observe({
      shinyjs::toggleState("update_person", condition = !is.null(selected_row()))
      shinyjs::toggleState("delete_person", condition = !is.null(selected_row()))
    })

    # =====================================================
    # 5. ADD / EDIT PERSON
    # =====================================================
    shiny::observeEvent(input$add_person, {
      new_entry <- data.frame(
        person_role                = as.character(input$person_role),
        person_order               = input$person_order,
        last_name                  = as.character(input$last_name),
        first_name                 = as.character(input$first_name),
        email                      = as.character(input$email),
        orcid                      = as.character(input$orcid),
        main_organization_name     = as.character(input$main_organization_name),
        main_organization_registry = as.character(input$main_organization_registry),
        department                 = as.character(input$department),
        street                     = as.character(input$street),
        postal_code                = as.character(input$postal_code),
        city                       = as.character(input$city),
        organization_country       = as.character(input$organization_country),
        organization_country_code  = as.character(input$organization_country_code),
        stringsAsFactors = FALSE
      )

      if (edit_mode() && !is.null(selected_row())) {
        new_entry$person_order <- as.character(new_entry$person_order)
        ctx$data$tbl6[selected_row(), names(new_entry)] <- as.list(new_entry[1, ])
      } else {
        ctx$data$tbl6 <- rbind(ctx$data$tbl6, new_entry)
      }

      clear_person_fields(session)
      form_visible(FALSE)
      edit_mode(FALSE)
      selected_row(NULL)
    })

    # =====================================================
    # 6. UPDATE (FILL FORM FROM SELECTION)
    # =====================================================
    shiny::observeEvent(input$update_person, {
      shiny::req(selected_row())
      row <- selected_row()
      form_visible(TRUE)
      edit_mode(TRUE)

      shiny::isolate({
        fields <- c("person_role", "last_name", "first_name", "email", "orcid",
                    "main_organization_name", "main_organization_registry",
                    "department", "street", "postal_code", "city",
                    "organization_country", "organization_country_code")
        for (field in fields) {
          val <- ctx$data$tbl6[[field]][row]
          if (is.na(val) || is.null(val)) val <- ""
          if (!is.character(val)) val <- as.character(val)
          shiny::updateTextInput(session, field, value = val)
        }
      })
    })

    # =====================================================
    # 7. DELETE
    # =====================================================
    shiny::observeEvent(input$delete_person, {
      row <- selected_row()
      if (!is.null(row) && row <= nrow(ctx$data$tbl6)) {
        ctx$data$tbl6 <- ctx$data$tbl6[-row, ]
        selected_row(NULL)
        shiny::showNotification("Person deleted.", type = "message")
      } else {
        shiny::showNotification("Please select a row to delete.", type = "warning")
      }
      ctx$data$tbl6$person_order <- seq_len(nrow(ctx$data$tbl6))
      clear_person_fields(session)
      form_visible(FALSE)
      edit_mode(FALSE)
    })

    # =====================================================
    # 8. APPLY ORDER
    # =====================================================
    shiny::observeEvent(input$apply_order, {
      shiny::req(input$tbl6)
      ordered <- rhandsontable::hot_to_r(input$tbl6)
      ordered$person_order <- as.integer(ordered$person_order)
      ordered <- ordered[order(ordered$person_order), ]
      ordered$person_order <- seq_len(nrow(ordered))
      ctx$data$tbl6 <- ordered
    })

    # =====================================================
    # 9. TABLE SYNC
    # =====================================================
    shiny::observeEvent(input$tbl6, {
      shiny::req(input$tbl6)
      ctx$data$tbl6 <- rhandsontable::hot_to_r(input$tbl6)
    })

    # =====================================================
    # 10. ORCID SEARCH
    # =====================================================
    orcid_data <- shiny::reactiveValues(results = NULL)

    shiny::observeEvent(input$search_orcid, {
      query <- NULL
      if (nzchar(input$orcid_search)) {
        orcid_id <- gsub("https?://orcid.org/", "", trimws(input$orcid_search))
        if (grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", orcid_id)) {
          query <- paste0("?q=orcid:", orcid_id)
        } else {
          shiny::showNotification("Invalid ORCID format.", type = "error"); return()
        }
      } else if (nzchar(input$first_name_search) || nzchar(input$last_name_search)) {
        qln <- if (nzchar(input$last_name_search))
          sprintf("(family-name:(%s))", URLencode(gsub(" ", "+AND+", input$last_name_search))) else ""
        qfn <- if (nzchar(input$first_name_search))
          sprintf("(given-names:(%s))", URLencode(gsub(" ", "+AND+", input$first_name_search))) else ""
        query <- paste0("?q=", qln, ifelse(nzchar(qln) && nzchar(qfn), "+AND+", ""), qfn)
      } else {
        shiny::showNotification("Provide an ORCID or a name.", type = "warning"); return()
      }

      url <- paste0("https://pub.orcid.org/v3.0/csv-search/", query,
                    "&fl=family-name,given-names,email,orcid,current-institution-affiliation-name,other-names&rows=50")
      res <- httr::GET(url, httr::timeout(5))

      if (httr::status_code(res) == 200) {
        df <- read.table(text = rawToChar(res$content), sep = ",", header = TRUE, stringsAsFactors = FALSE)
        if (nrow(df) > 0) {
          results <- df |>
            dplyr::rename(last_name = family.name, first_name = given.names,
                          orcid_id = orcid, org_name = current.institution.affiliation.name,
                          other_names = other.names) |>
            tidyr::separate(email,    into = c("email"),    sep = ",(?!\\s)", extra = "drop", remove = FALSE) |>
            tidyr::separate(org_name, into = c("org_name"), sep = ",(?!\\s)", extra = "drop", remove = FALSE) |>
            dplyr::mutate(orcid_link = sprintf("<a href='https://orcid.org/%s' target='_blank'>%s</a>", orcid_id, orcid_id))
          orcid_data$results <- results
        } else {
          orcid_data$results <- NULL
          shiny::showNotification("No ORCID results found.", type = "message")
        }
      } else {
        orcid_data$results <- NULL
        shiny::showNotification("ORCID API request failed.", type = "error")
      }
    })

    output$orcid_results_table <- DT::renderDT({
      shiny::req(orcid_data$results)
      df <- orcid_data$results |>
        dplyr::select(`ORCID (clickable)` = orcid_link, `First Name` = first_name,
                      `Last Name` = last_name, Email = email, Organization = org_name)
      DT::datatable(df, escape = FALSE, rownames = FALSE, selection = "single",
                    options = list(pageLength = 10, autoWidth = TRUE))
    })

    shiny::observeEvent(input$orcid_results_table_rows_selected, {
      sel <- input$orcid_results_table_rows_selected
      if (!is.null(sel) && length(sel) == 1) {
        row <- orcid_data$results[sel, ]
        shiny::updateTextInput(session, "orcid",                   value = row$orcid_id)
        shiny::updateTextInput(session, "first_name",              value = row$first_name)
        shiny::updateTextInput(session, "last_name",               value = row$last_name)
        shiny::updateTextInput(session, "email",                   value = row$email)
        shiny::updateTextInput(session, "main_organization_name",  value = row$org_name)
      }
    })

    # =====================================================
    # 11. ROR SEARCH
    # =====================================================
    ror_data <- shiny::reactiveValues(results = NULL)

    shiny::observeEvent(input$search_ror, {
      shiny::req(input$country_code, input$search_string)
      url <- sprintf("https://api.ror.org/v2/organizations?query=%s&filter=country.country_code:%s",
                     URLencode(input$search_string), input$country_code)
      ror_res <- httr::GET(url, httr::timeout(5))

      if (httr::status_code(ror_res) == 200) {
        json <- jsonlite::fromJSON(rawToChar(ror_res$content))
        if (json$number_of_results > 0) {
          res_names <- json$items$names |> dplyr::bind_rows() |>
            dplyr::filter(grepl("ror_display", types)) |> dplyr::pull(value)
          res_locs <- json$items$locations |> dplyr::bind_rows() |>
            dplyr::pull(geonames_details) |>
            tidyr::unite(col = "address", name, country_name, sep = ", ") |>
            dplyr::pull(address)
          res_web <- json$items$links |>
            purrr::map_dfr(dplyr::bind_rows) |>
            dplyr::filter(type == "website") |> dplyr::pull(value)

          res_df <- data.frame(ROR = json$items$id, Name = res_names,
                               Location = res_locs, Website = res_web)
          ror_data$results <- res_df |>
            tidyr::separate(Location, into = c("city", "country"), sep = ", ", remove = FALSE)

          output$ror_results <- DT::renderDT({
            DT::datatable(res_df, rownames = FALSE, selection = "single", escape = FALSE,
                          options = list(pageLength = 10, autoWidth = TRUE))
          })
        } else {
          shiny::showNotification("No ROR results found.", type = "message")
        }
      } else {
        shiny::showNotification("ROR API request failed.", type = "error")
      }
    })

    shiny::observeEvent(input$ror_results_rows_selected, {
      shiny::req(input$ror_results_rows_selected)
      ror_row <- ror_data$results[input$ror_results_rows_selected, ]
      shiny::updateTextInput(session, "main_organization_name",    value = ror_row$Name[1])
      shiny::updateTextInput(session, "main_organization_registry",value = ror_row$ROR[1])
      shiny::updateTextInput(session, "city",                      value = ror_row$city[1])
      shiny::updateTextInput(session, "organization_country_code",
        value = countrycode::countrycode(ror_row$country[1], origin = "country.name", destination = "iso2c"))
    })

    # =====================================================
    # 12. SAVE
    # =====================================================
    shiny::observeEvent(input$save_person, {
      save_and_validate(
        data_reactive = ctx$data$tbl6,
        sheet_name    = "person",
        wb_reactive   = shiny::reactive(ctx$files$wb_meta),
        temp_folder   = ctx$files$temp_folder
      )
    })

  })
}
