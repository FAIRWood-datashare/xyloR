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
          bslib::card_header(id = ns("card_header_validation"), "Validation"),
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
    # 🧠 DATA SOURCES (IMMUTABLE READ)
    # =====================================================
    xylo_obs <- reactive({
      req(ctx$data$obs_truth)
      ctx$data$obs_truth
    })
    
    site_info <- reactive({
      req(ctx$data$site_info)
      ctx$data$site_info
    })
    
    # =====================================================
    # 🧭 SITE STATE
    # =====================================================
    selected_site <- reactiveVal(NULL)
    
    observe({
      
      req(site_info())
      
      sites <- unique(trimws(as.character(site_info()$site_label)))
      
      updateSelectInput(session, "site_filter", choices = sites)
      
      if (is.null(selected_site()) && length(sites) > 0) {
        selected_site(sites[1])
      }
    })
    
    observeEvent(input$site_filter, {
      selected_site(input$site_filter)
    }, ignoreInit = TRUE)
    
    df_site <- reactive({
      
      req(xylo_obs(), selected_site())
      
      dplyr::filter(xylo_obs(), site_label == selected_site())
    })
    
    # =====================================================
    # 📊 VALIDATION (UNCHANGED LOGIC)
    # =====================================================
    qa_valid <- reactive({
      
      isTRUE(input$validate_location) &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
    })
    
    # =====================================================
    # 🧭 READINESS INTEGRATION (NEW)
    # =====================================================
    observe({
      
      # data layer readiness
      data_ready <- !is.null(ctx$data$obs_truth) &&
        !is.null(ctx$data$site_info)
      
      # expose via central controller
      ctx$update_ready()
      
      # optional sync into legacy v2 (kept for compatibility)
      ctx$v2$qa_ready <- qa_valid()
      ctx$v2$obs_verified <- qa_valid()
    })
    
    # =====================================================
    # 🎨 UI STATE (UNCHANGED LOGIC, safer JS)
    # =====================================================
    observe({
      
      valid <- qa_valid()
      
      if (valid) shinyjs::enable("next_btn")
      else shinyjs::disable("next_btn")
      
      color <- if (valid) "#198754" else "#dc3545"
      
      shinyjs::runjs(sprintf(
        "
        var header = document.getElementById('%s');
        if (header) {
          header.style.backgroundColor = '%s';
          header.style.color = '#fff';
        }
        ",
        ns("card_header_validation"),
        color
      ))
    })
    
    # =====================================================
    # 📊 TABLE
    # =====================================================
    output$key_info_table <- DT::renderDataTable({
      
      df <- df_site()
      req(nrow(df) > 0)
      
      si <- site_info() |> dplyr::filter(site_label == selected_site())
      
      tibble::tibble(
        Metric = c("Site", "Trees", "Dates"),
        Value = c(
          si$site_label,
          length(unique(df$tree_label)),
          length(unique(df$sample_date))
        )
      )
    })
    
    # =====================================================
    # 🗺 MAP
    # =====================================================
    output$mymap <- leaflet::renderLeaflet({
      
      si <- site_info() |> dplyr::filter(site_label == selected_site())
      
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addMarkers(
          lng = as.numeric(si$longitude),
          lat = as.numeric(si$latitude),
          popup = si$site_label
        )
    })
    
    # =====================================================
    # 📈 PLOT (UNCHANGED)
    # =====================================================
    output$data_coverage_plot <- plotly::renderPlotly({
      
      df <- df_site()
      req(input$color)
      
      plotly::plot_ly(
        df,
        x = ~sample_date,
        y = ~tree_label,
        color = ~.data[[input$color]],
        type = "scatter",
        mode = "markers"
      )
    })
    
    # =====================================================
    # 🚀 NAVIGATION (NOW SAFE-GATED)
    # =====================================================
    observeEvent(input$next_btn, {
      
      req(qa_valid())
      
      # 🔥 only advance if system is consistent
      ctx$nav$stage <- "tab4"
      
      # 🔥 trigger readiness update (important bridge to next layer)
      ctx$update_ready()
    })
    
  })
}