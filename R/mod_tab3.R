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
    # 🧠 REQUIRE DATA FROM TAB2
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
    # 🧭 SITE SELECTION
    # =====================================================
    selected_site <- reactiveVal(NULL)
    
    observe({
      req(site_info())
      
      sites <- unique(trimws(as.character(site_info()$site_label)))
      
      updateSelectInput(session, "site_filter", choices = sites)
      
      if (is.null(selected_site())) {
        selected_site(sites[1])
      }
    })
    
    observeEvent(input$site_filter, {
      selected_site(input$site_filter)
    }, ignoreInit = TRUE)
    
    df_site <- reactive({
      req(xylo_obs(), selected_site())
      xylo_obs() |> dplyr::filter(site_label == selected_site())
    })
    
    # =====================================================
    # 📊 KEY INFO TABLE
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
    # 📈 COVERAGE PLOT
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
        mode = "markers",
        text = ~paste(
          "Tree:", tree_label,
          "<br>Date:", sample_date,
          "<br>", input$color, ":", .data[[input$color]]
        ),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          plot_bgcolor = "#2e2e2e",
          paper_bgcolor = "#2e2e2e",
          
          font = list(color = "white"),
          
          xaxis = list(
            gridcolor = "#444444",
            zerolinecolor = "#666666",
            tickfont = list(color = "white"),
            titlefont = list(color = "white")
          ),
          
          yaxis = list(
            gridcolor = "#444444",
            zerolinecolor = "#666666",
            tickfont = list(color = "white"),
            titlefont = list(color = "white")
          ),
          
          legend = list(
            font = list(color = "white"),
            bgcolor = "rgba(0,0,0,0)"
          )
        )
    })
    
    # =====================================================
    # 📋 COVERAGE TABLE
    # =====================================================
    output$obs_table <- DT::renderDataTable({
      
      df <- df_site()
      DT::datatable(head(df, 100))
    })
    
    # =====================================================
    # ✅ VALIDATION LOGIC
    # =====================================================
    validation_ok <- reactive({
      isTRUE(input$validate_location) &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
    })
    
    observe({
      valid <- validation_ok()
      
      ctx$v2$qa_ready <- valid
    })
    
    # =====================================================
    # 🎨 BUTTON ENABLE/DISABLE
    # =====================================================
    observe({
      
      valid <- validation_ok()
      
      if (valid) {
        shinyjs::enable("next_btn")
      } else {
        shinyjs::disable("next_btn")
      }
      
      color <- if (valid) "#198754" else "#dc3545"
      
      shinyjs::runjs(sprintf("
  var header = document.getElementById('%s');
  if (header) {
    header.style.backgroundColor = '%s';
    header.style.color = '#fff';
  }

  var btn = document.getElementById('%s');
  if (btn) {
    if (%s) {
      btn.classList.remove('btn-secondary');
      btn.classList.add('btn-success');
    } else {
      btn.classList.remove('btn-success');
      btn.classList.add('btn-secondary');
    }
  }
",
                             ns("card_header_validation"),
                             color,
                             ns("next_btn"),
                             tolower(as.character(valid))
      ))
    })
    
    # =====================================================
    # 🚀 NEXT → TAB4
    # =====================================================
    observeEvent(input$next_btn, {
      
      req(validation_ok())
      
      ctx$v2$obs_verified <- TRUE
      ctx$v2$stage <- "tab4"
    })
    
  })
}