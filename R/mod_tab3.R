#' mod_tab3 UI Function
#'
#' @description A shiny module for the "Observation" tab.
#'
#' @param id A string that serves as the module namespace identifier.
#' @return A `shiny.tag.list` containing the UI elements of the module.
#'
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#'
mod_tab3_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Observation QA",
    value = "tab3",
    
    fluidRow(
      
      # ================= LEFT PANEL =================
      column(
        4,
        
        selectInput(ns("site_filter"), "Select site", choices = NULL),
        
        bslib::card(
          bslib::card_header("Key information"),
          bslib::card_body(
            DT::DTOutput(ns("key_info_table"))
          )
        ),
        
        bslib::card(
          div(
            id = ns("card_header_validation"),
            class = "card-header bg-danger py-1",
            "Validation"
          ),
          bslib::card_body(
            checkboxInput(ns("validate_location"), "Validate location"),
            checkboxInput(ns("validate_data_coverage"), "Validate coverage"),
            checkboxInput(ns("validate_observation"), "Validate observations"),
            
            actionButton(
              ns("next_btn"),
              "Continue →",
              class = "btn btn-secondary w-100",
              disabled = TRUE
            )
          )
        )
      ),
      
      # ================= RIGHT PANEL =================
      column(
        8,
        
        bslib::card(
          bslib::card_header("Map"),
          bslib::card_body(
            leaflet::leafletOutput(ns("mymap"), height = "300px")
          )
        ),
        
        bslib::card(
          bslib::card_header("Data coverage"),
          bslib::card_body(
            selectInput(ns("color"), "Color by",
                        choices = c("tree_species", "sample_id", "plot_label")),
            plotly::plotlyOutput(ns("data_coverage_plot"))
          )
        ),
        
        bslib::card(
          bslib::card_header("Coverage table"),
          bslib::card_body(
            DT::DTOutput(ns("obs_table"))
          )
        )
      )
    )
  )
}

#' mod_tab3 Server Function
#'
#' @description Server logic for the "Observation" tab module.
#'
#' @param id A string that serves as the module namespace identifier.
#' @param ctx The centralized app context (environment).
#' @param session The parent Shiny session.
#'
#' @return A list with WB, WB_meta, data_in, column_configs (for downstream tabs).
#' @export
#'
#' @import shiny
#' @importFrom shinyjs enable disable
#' @importFrom openxlsx loadWorkbook readWorkbook saveWorkbook
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#'
mod_tab3_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 🧠 DATA SOURCES
    # =====================================================
    xylo_obs <- reactive({
      req(ctx$data$obs_raw)
      ctx$data$obs_raw
    })
    
    site_info <- reactive({
      req(ctx$data$site_info)
      ctx$data$site_info
    })
    
    # =====================================================
    # 🧠 SITE STATE
    # =====================================================
    selected_site <- reactiveVal(NULL)
    
    observeEvent(site_info(), {
      
      sites <- unique(trimws(as.character(site_info()$site_label)))
      if (length(sites) == 0) return()
      
      if (is.null(selected_site())) {
        selected_site(sites[1])
      }
      
      updateSelectInput(
        session,
        "site_filter",
        choices = sites,
        selected = selected_site()
      )
    })
    
    observeEvent(input$site_filter, {
      selected_site(input$site_filter)
    }, ignoreInit = TRUE)
    
    df_site <- reactive({
      req(xylo_obs(), selected_site())
      dplyr::filter(xylo_obs(), site_label == selected_site())
    })
    
    # =====================================================
    # 📊 QA (ENGINE-AGNOSTIC — CLEAN SSOT RULE)
    # =====================================================
    qa_valid <- reactive({
      
      req(
        input$validate_location,
        input$validate_data_coverage,
        input$validate_observation
      )
      
      isTRUE(input$validate_location) &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
    })
    
    observe({
      ctx$state$qa_ready <- qa_valid()
    })
    
    # =====================================================
    # 🎨 UI STATE
    # =====================================================
    observe({
      
      valid <- qa_valid()
      
      shinyjs::toggleState("next_btn", valid)
      
      btn_class <- if (valid) "btn-success" else "btn-secondary"
      
      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('btn-success btn-secondary').addClass('%s')",
        ns("next_btn"),
        btn_class
      ))
      
      header_class <- if (valid) "bg-success" else "bg-danger"
      
      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('bg-success bg-danger').addClass('%s')",
        ns("card_header_validation"),
        header_class
      ))
    })
    
    # =====================================================
    # 📊 KEY TABLE
    # =====================================================
    output$key_info_table <- DT::renderDataTable({
      
      df <- df_site()
      req(nrow(df) > 0)
      
      si <- site_info() %>%
        dplyr::filter(site_label == selected_site())
      
      validate(
        need(nrow(si) > 0, "No site selected"),
        need(!is.na(si$latitude[1]), "Missing lat"),
        need(!is.na(si$longitude[1]), "Missing lon")
      )
      
      key_info <- tibble::tibble(
        "Site" = si$site_label[1],
        "Coordinates" = paste(
          "Lat =", round(as.numeric(si$latitude[1]), 4),
          "Long =", round(as.numeric(si$longitude[1]), 4)
        ),
        "Elevation" = si$elevation[1],
        "Network" = paste(unique(df$network_label), collapse = ", "),
        "Date From" = format(min(df$sample_date), "%Y-%m-%d"),
        "Date To"   = format(max(df$sample_date), "%Y-%m-%d"),
        "n_Trees"   = length(unique(df$tree_label))
      ) %>%
        t() %>%
        setNames("Value")
      
      DT::datatable(
        key_info,
        options = list(dom = "t", paging = FALSE),
        class = "table-dark compact stripe hover"
      )
    })
    
    # =====================================================
    # 🗺 MAP
    # =====================================================
    output$mymap <- leaflet::renderLeaflet({
      
      req(selected_site())
      
      si <- site_info() %>%
        dplyr::filter(site_label == selected_site())
      
      validate(
        need(nrow(si) > 0, "No site found"),
        need(!is.na(si$latitude[1]), "Missing lat"),
        need(!is.na(si$longitude[1]), "Missing lon")
      )
      
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::setView(
          lng = as.numeric(si$longitude[1]),
          lat = as.numeric(si$latitude[1]),
          zoom = 10
        ) %>%
        leaflet::addMarkers(
          lng = as.numeric(si$longitude[1]),
          lat = as.numeric(si$latitude[1]),
          popup = si$site_label[1]
        )
    })
    
    # =====================================================
    # 📈 PLOT
    # =====================================================
    output$data_coverage_plot <- plotly::renderPlotly({
      
      df <- df_site()
      req(input$color)
      
      validate(
        need(input$color %in% names(df), "Invalid color column"),
        need("sample_date" %in% names(df), "Missing sample_date"),
        need("tree_label" %in% names(df), "Missing tree_label")
      )
      
      plotly::plot_ly(
        df,
        x = ~sample_date,
        y = ~tree_label,
        color = as.factor(df[[input$color]]),
        type = "scatter",
        mode = "markers"
      )
    })
    
    # =====================================================
    # 🚀 NAVIGATION
    # =====================================================
    observeEvent(input$next_btn, {
      
      req(qa_valid())
      
      ctx$state$stage <- "tab4"
      ctx$update_ready()
    })
  })
}

