#' mod_tab1 UI Function
#'
#' @description A shiny module for the "Upload Observation" tab.
#'
#' Guides users through:
#' 1. Naming the dataset (name, version, description),
#' 2. Downloading a blank template or filled example,
#' 3. Uploading the filled observation .xlsx,
#' 4. Reviewing the Leaflet map, data-coverage plot, and info table,
#' 5. Ticking validation checkboxes before proceeding to Tab 2.
#'
#' @param id Module namespace identifier.
#' @return A `shiny.tag` (fluidRow) with the full tab 1 UI.
#' @export
#'
#' @import shiny
#' @importFrom bslib card card_header card_body tooltip
#' @importFrom bsicons bs_icon
#' @importFrom leaflet leafletOutput
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @importFrom shinyjs runjs
#' 
mod_tab1_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::fluidRow(

    # =========================================================
    # LEFT COLUMN — FORMS
    # =========================================================
    shiny::column(
      3,
      class = "bg-light p-2 border-end",

      # ── Step 1: dataset setup ─────────────────────────────
      tags$div(
        tags$small("Step 1 — Dataset setup", class = "text-muted"),
        tags$hr(style = "margin: 6px 0;")
      ),

      bslib::card(
        div(
          id    = ns("card_header1_1"),
          class = "card-header bg-danger py-1",
          "1.1 Dataset"
        ),

        bslib::card_body(
          class = "py-2",
          shiny::textInput(ns("dataset_name"), "Name (3–8 uppercase chars)"),
          shiny::numericInput(ns("version"), "Version", value = 1, min = 1, max = 99),
          shiny::dateInput(
            ns("embargo"),
            "Embargo end date",
            value = Sys.Date(),
            min   = Sys.Date(),
            max   = Sys.Date() + 3650,
            format = "yyyy-mm-dd"
          ),
          shiny::textAreaInput(ns("description"), "Description (≥ 50 chars)", height = "120px"),
          shiny::actionButton(ns("submit"), "Validate", class = "btn btn-primary w-100")
        )
      ),

      # ── Step 2: templates (optional) ─────────────────────
      tags$div(
        tags$small("Step 2 — Data preparation", class = "text-muted"),
        tags$hr(style = "margin: 6px 0;")
      ),

      bslib::card(
        id    = ns("card_1"),
        style = "display:none;",

        bslib::card_header(
          "1.2 Templates (optional)",
          id    = ns("card_header1_2"),
          class = "bg-warning py-1",
          bslib::tooltip(
            bsicons::bs_icon("question-circle"),
            "Download a blank template to fill in, or an example for reference.",
            placement = "right"
          )
        ),

        bslib::card_body(
          class = "py-2",
          shiny::fluidRow(
            shiny::column(6,
              shiny::downloadButton(ns("download_template"),    "Blank",   class = "btn btn-primary   w-100"),
              tags$small("Your input file",  class = "text-muted")
            ),
            shiny::column(6,
              shiny::downloadButton(ns("download_example_obs"), "Example", class = "btn btn-secondary w-100"),
              tags$small("Reference format", class = "text-muted")
            )
          )
        )
      ),

      # ── Step 3: upload ────────────────────────────────────
      bslib::card(
        id    = ns("card_2"),
        style = "display:none;",

        bslib::card_header(
          "1.3 Upload",
          id    = ns("card_header2"),
          class = "card-header bg-danger py-1"
        ),

        bslib::card_body(
          class = "py-2",
          shiny::fileInput(ns("obs_file"), "Observation file (.xlsx)"),
          shiny::selectInput(ns("site_filter"), "Site", choices = NULL)
        )
      ),

      # ── Step 4: validation checkboxes ────────────────────
      tags$div(
        tags$small("Step 3 — Validation & review", class = "text-muted"),
        tags$hr(style = "margin: 6px 0;")
      ),

      bslib::card(
        id    = ns("card_1_4"),
        style = "display:none;",

        bslib::card_header(
          "1.4 Validate",
          id    = ns("card_header1_4"),
          class = "card-header bg-danger py-1"
        ),

        bslib::card_body(
          class = "py-2",
          shiny::checkboxInput(ns("validate_location"),      "Location ok"),
          shiny::checkboxInput(ns("validate_data_coverage"), "Coverage ok"),
          shiny::checkboxInput(ns("validate_observation"),   "Observations ok"),
          shiny::actionButton(ns("next_btn"), "Continue →", class = "btn btn-primary w-100")
        )
      )
    ),

    # =========================================================
    # RIGHT COLUMN — OUTPUTS
    # =========================================================
    shiny::column(
      9,

      # Leaflet map
      bslib::card(
        bslib::card_header("Map preview", class = "bg-light py-1"),
        bslib::card_body(
          style = "height:400px; padding:0; overflow:hidden;",
          div(
            style = "height:100%; width:100%;",
            leaflet::leafletOutput(ns("mymap"), height = "100%")
          )
        )
      ),

      # Data-coverage plot + colour selector
      bslib::card(
        id = ns("card_data_coverage"),

        bslib::card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            span("Data coverage"),
            shiny::selectInput(
              ns("color"), NULL,
              choices  = c("tree_species", "sample_id", "plot_label"),
              selected = "tree_species",
              width    = "180px"
            )
          ),
          class = "bg-light py-1"
        ),
        bslib::card_body(
          plotly::plotlyOutput(ns("data_coverage_plot"), height = "300px")
        )
      ),

      # Key info table
      bslib::card(
        bslib::card_header("Key information", class = "bg-light py-1"),
        bslib::card_body(
          DT::DTOutput(ns("key_info_table"))
        )
      ),

      # Average repetition table
      bslib::card(
        bslib::card_header("Average repetitions per sample & measure type", class = "bg-light py-1"),
        bslib::card_body(
          DT::DTOutput(ns("obs_table"))
        )
      )
    )
  )
}


#' mod_tab1 Server Function
#'
#' @description Server logic for the "Upload Observation" tab module.
#'
#' Handles:
#' - Dataset name / version / description validation,
#' - Template and example downloads,
#' - File upload, `ctx` population, and site-filter update,
#' - Leaflet map, data-coverage (Plotly), key-info table, repetition table,
#' - Checkbox-based UI validation gate,
#' - FSM trigger on "Continue".
#'
#' @param id Module namespace identifier.
#' @param ctx Shared reactive-values context object.
#' @param session Top-level Shiny session (passed by the app server).
#' @return No return value; called for side effects.
#' @export
#'
#' @import shiny shinyjs dplyr tibble
#' @importFrom leaflet renderLeaflet leaflet addTiles setView addMarkers leafletOptions
#' @importFrom plotly renderPlotly plot_ly layout
#' @importFrom DT renderDataTable datatable
mod_tab1_server <- function(id, ctx, session) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # =========================================================
    # SAFE INIT
    # =========================================================
    isolate({
      ctx$state$tab1 <- list(
        data_ready = FALSE,
        file       = list(uploaded = FALSE, loaded = FALSE),
        inputdata  = list(valid = FALSE, confirmed = FALSE),
        validation = list(ui_valid = FALSE)
      )
    })

    # =========================================================
    # DEBUG OBSERVER
    # =========================================================
    observe({
      message(
        "🔍 TAB1 STATE:",
        " input_valid=", ctx$state$tab1$inputdata$valid,
        " uploaded=",    ctx$state$tab1$file$uploaded,
        " loaded=",      ctx$state$tab1$file$loaded,
        " ui_valid=",    ctx$state$tab1$validation$ui_valid
      )
    })

    # =========================================================
    # 1. INPUT VALIDATION (LIVE)
    # =========================================================
    observe({
      nm  <- input$dataset_name %||% ""
      ver <- suppressWarnings(as.numeric(input$version))
      dsc <- trimws(input$description %||% "")

      valid <-
        nzchar(nm) &&
        nchar(nm) >= 3 && nchar(nm) <= 8 &&
        grepl("^[A-Z0-9]+$", nm) &&
        !is.na(ver) && ver >= 1 && ver <= 99 &&
        nzchar(dsc) && nchar(dsc) >= 50

      ctx$state$tab1$inputdata$valid <- valid
    })

    # =========================================================
    # 2. HEADER COLOUR (1.1)
    # =========================================================
    observe({
      cls <- if (isTRUE(ctx$state$tab1$inputdata$valid)) "bg-success" else "bg-danger"
      opp <- if (cls == "bg-success") "bg-danger" else "bg-success"
      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('%s').addClass('%s')",
        ns("card_header1_1"), opp, cls
      ))
    })

    # =========================================================
    # 3. SUBMIT BUTTON STATE
    # =========================================================
    observe({
      shinyjs::toggleState("submit", condition = isTRUE(ctx$state$tab1$inputdata$valid))
    })

    # =========================================================
    # 4. SUBMIT — REVEAL CARDS 1.2 + 1.3
    # =========================================================
    observeEvent(input$submit, {
      req(ctx$state$tab1$inputdata$valid)
      ctx$state$tab1$inputdata$confirmed <- TRUE
      shinyjs::show("card_1")
      shinyjs::show("card_2")
    })

    # =========================================================
    # 5. DOWNLOAD TEMPLATE
    # =========================================================
    output$download_template <- downloadHandler(
      filename = function() {
        paste0(input$dataset_name %||% "Dataset", "_xylo_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        tp <- system.file("extdata", "Datasetname_xylo_data_yyyy-mm-dd.xlsx", package = "xyloR")
        file.copy(tp, file, overwrite = TRUE)
      }
    )

    # =========================================================
    # 6. DOWNLOAD EXAMPLE
    # =========================================================
    output$download_example_obs <- downloadHandler(
      filename = function() "Example_xylo_data.xlsx",
      content = function(file) {
        tp <- system.file("extdata", "Ltal2007_xylo_data_2025-09-01.xlsx", package = "xyloR")
        file.copy(tp, file, overwrite = TRUE)
      }
    )

    # =========================================================
    # 7. FILE UPLOAD — POPULATE CTX
    # =========================================================
    observeEvent(input$obs_file, {
      req(input$obs_file)

      obs       <- load_xylo_obs_clean_contract(input$obs_file$datapath)
      site_info <- extract_site_info(input$obs_file$datapath)

      ctx$files$obs_file         <- input$obs_file
      ctx$data$obs               <- obs
      ctx$data$site_info         <- site_info
      ctx$data$dataset_name      <- input$dataset_name
      ctx$data$version           <- input$version
      ctx$data$embargo           <- input$embargo
      ctx$data$description       <- input$description
      ctx$data$contact_lastname  <- tryCatch({
        wb_tmp <- openxlsx::loadWorkbook(input$obs_file$datapath)
        as.character(
          openxlsx::readWorkbook(wb_tmp, sheet = "obs_data_info",
                                 rows = 2, cols = 2, colNames = FALSE)[1, 1]
        )
      }, error = function(e) "—")

      ctx$state$tab1$file$uploaded <- TRUE
      ctx$state$tab1$file$loaded   <- TRUE
      ctx$state$tab1$data_ready    <- TRUE

      message("🟢 TAB1 DATA READY = TRUE")

      sites <- unique(trimws(as.character(site_info$site_label)))
      updateSelectInput(session, "site_filter", choices = sites, selected = sites[1])

      shinyjs::removeClass("card_header2", "bg-danger")
      shinyjs::addClass("card_header2",    "bg-success")
      shinyjs::show("card_1_4")

    }, ignoreInit = TRUE)

    # =========================================================
    # 8. CHECKBOX VALIDATION (TAB1 GATE)
    # =========================================================
    observe({
      ctx$state$tab1$validation$ui_valid <-
        isTRUE(input$validate_location)      &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
    })

    # =========================================================
    # 9. HEADER COLOUR (1.4)
    # =========================================================
    observe({
      cls <- if (isTRUE(ctx$state$tab1$validation$ui_valid)) "bg-success" else "bg-danger"
      opp <- if (cls == "bg-success") "bg-danger" else "bg-success"
      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('%s').addClass('%s')",
        ns("card_header1_4"), opp, cls
      ))
    })

    # =========================================================
    # 10. NEXT BUTTON STATE + FSM TRIGGER
    # =========================================================
    observe({
      shinyjs::toggleState(
        "next_btn",
        condition = isTRUE(
          ctx$state$tab1$inputdata$valid &&
            ctx$state$tab1$file$uploaded &&
            ctx$state$tab1$validation$ui_valid
        )
      )
    })

    observeEvent(input$next_btn, {
      req(
        ctx$state$tab1$inputdata$valid,
        ctx$state$tab1$file$uploaded,
        ctx$state$tab1$validation$ui_valid
      )
      ctx$fsm$events$go_next <- TRUE
      ctx$fsm_trigger(ctx$fsm_trigger() + 1)
      message("➡️ TAB1 NEXT CLICK → FSM TRIGGERED")
    })

    # =========================================================
    # 11. LEAFLET MAP
    # =========================================================
    output$mymap <- leaflet::renderLeaflet({
      req(isTRUE(ctx$state$tab1$data_ready), input$site_filter)

      si  <- ctx$data$site_info
      si$site_label <- trimws(as.character(si$site_label))
      sel <- trimws(as.character(input$site_filter))
      si  <- si[si$site_label == sel, , drop = FALSE]

      shiny::validate(
        need(nrow(si) > 0,         "No matching site"),
        need(!is.na(si$latitude[1]),  "Missing latitude"),
        need(!is.na(si$longitude[1]), "Missing longitude")
      )

      lat <- as.numeric(si$latitude[1])
      lng <- as.numeric(si$longitude[1])

      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
        leaflet::addTiles() |>
        leaflet::setView(lng = lng, lat = lat, zoom = 11) |>
        leaflet::addMarkers(lng = lng, lat = lat, popup = si$site_label[1])
    })

    # =========================================================
    # 12. DATA COVERAGE PLOT
    # =========================================================
    output$data_coverage_plot <- plotly::renderPlotly({
      req(isTRUE(ctx$state$tab1$data_ready), input$site_filter, input$color)

      df <- ctx$data$obs
      df <- df[df$site_label == trimws(input$site_filter), , drop = FALSE]

      shiny::validate(
        need(nrow(df) > 0,                    "No data for selected site"),
        need("sample_date" %in% names(df),    "Missing column: sample_date"),
        need("tree_label"  %in% names(df),    "Missing column: tree_label"),
        need(input$color    %in% names(df),   paste("Missing column:", input$color))
      )

      plotly::plot_ly(
        df,
        x     = ~sample_date,
        y     = ~tree_label,
        color = as.factor(df[[input$color]]),
        type  = "scatter",
        mode  = "markers",
        text  = ~paste(
          "Tree:", tree_label,
          "<br>Date:", sample_date,
          "<br>", input$color, ":", df[[input$color]]
        ),
        hoverinfo = "text",
        marker = list(size = 8, opacity = 0.75)
      ) |>
        plotly::layout(
          xaxis       = list(title = "Date",       color = "white", showgrid = FALSE),
          yaxis       = list(title = "Tree label", color = "white", categoryorder = "category ascending"),
          plot_bgcolor  = "#2e2e2e",
          paper_bgcolor = "#2e2e2e",
          font          = list(color = "white"),
          legend        = list(orientation = "h", y = -0.2)
        )
    })

    # =========================================================
    # 13. KEY INFO TABLE
    # =========================================================
    output$key_info_table <- DT::renderDataTable({
      req(isTRUE(ctx$state$tab1$data_ready), input$site_filter)

      si <- ctx$data$site_info |>
        dplyr::filter(trimws(site_label) == trimws(input$site_filter))

      shiny::validate(
        need(nrow(si) > 0,            "No site selected"),
        need(!is.na(si$latitude[1]),  "Missing latitude"),
        need(!is.na(si$longitude[1]), "Missing longitude")
      )

      df <- ctx$data$obs

      key_info <- tibble::tibble(
        Field = c(
          "Site", "Coordinates", "Elevation",
          "Network",
          "Contact",
          "Date From", "Date To",
          "N Trees", "N Dates", "N Samples"
        ),
        Value = c(
          si$site_label[1],
          paste0(
            "Lat=",  round(as.numeric(si$latitude[1]),  4),
            " | Lon=", round(as.numeric(si$longitude[1]), 4)
          ),
          as.character(si$elevation[1]),
          paste(unique(df$network_label), collapse = ", "),
          ctx$data$contact_lastname %||% "—",
          format(min(df$sample_date, na.rm = TRUE), "%Y-%m-%d"),
          format(max(df$sample_date, na.rm = TRUE), "%Y-%m-%d"),
          as.character(length(unique(df$tree_label))),
          as.character(length(unique(df$sample_date))),
          as.character(length(unique(paste(df$sample_label, df$sample_id, sep = "_"))))
        )
      )

      DT::datatable(
        key_info,
        rownames = FALSE,
        colnames = c("Field", "Value"),
        class    = "table-dark compact",
        options  = list(dom = "t", paging = FALSE, autoWidth = TRUE,
                        columnDefs = list(list(className = "dt-left", targets = "_all")))
      )
    })

    # =========================================================
    # 14. REPETITION TABLE (obs_table)
    # =========================================================
    output$obs_table <- DT::renderDataTable({
      req(isTRUE(ctx$state$tab1$data_ready), input$site_filter)

      df <- ctx$data$obs

      excluded <- c(
        "sample_date", "sample_id", "tree_species", "tree_label", "plot_label",
        "site_label", "network_label", "sample_label", "measure_type",
        "measure_repetition", "sample_comment"
      )
      cols_inc <- setdiff(names(df), excluded)

      shiny::validate(need(length(cols_inc) > 0, "No measurement columns found"))

      grouped <- df |>
        dplyr::filter(dplyr::if_any(dplyr::all_of(cols_inc), ~ !is.na(.))) |>
        tidyr::pivot_longer(dplyr::all_of(cols_inc), names_to = "variable", values_to = "value") |>
        dplyr::filter(!is.na(value)) |>
        dplyr::group_by(measure_type, sample_label, sample_id, variable) |>
        dplyr::summarise(non_na_count = dplyr::n(), .groups = "drop") |>
        dplyr::group_by(measure_type, variable) |>
        dplyr::summarise(avg_non_na = round(mean(non_na_count), 4), .groups = "drop") |>
        dplyr::mutate(
          variable = factor(variable, levels = intersect(c("cz", "ez", "tz", "mz", "pr"), unique(variable)))
        ) |>
        dplyr::arrange(variable) |>
        tidyr::pivot_wider(names_from = variable, values_from = avg_non_na)

      DT::datatable(
        grouped,
        rownames = FALSE,
        class    = "table-dark compact",
        options  = list(dom = "t", paging = FALSE, autoWidth = TRUE,
                        columnDefs = list(list(className = "dt-center", targets = "_all")))
      )
    })

  })
}
