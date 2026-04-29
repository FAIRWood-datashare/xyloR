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
  
  bslib::nav_panel(
    title = "Dataset definition",
    value = "tab1",
    
    fluidRow(
      column(
        12,
        
        bslib::card(
          bslib::card_header(
            id = ns("card_header1"),
            "1. Dataset definition"
          ),
          
          bslib::card_body(
            
            div(
              textInput(ns("dataset_name"), "Dataset name"),
              uiOutput(ns("v_name"))
            ),
            
            div(
              numericInput(ns("version"), "Data version", value = 1, min = 1, max = 99),
              uiOutput(ns("v_version"))
            ),
            
            div(
              dateInput(
                ns("embargo"),
                "Embargo date",
                value = Sys.Date(),
                min = Sys.Date(),
                max = Sys.Date() + 365*10
              ),
              uiOutput(ns("v_embargo"))
            ),
            
            div(
              textAreaInput(ns("description"), "Description"),
              uiOutput(ns("v_description"))
            ),
            
            br(),
            
            actionButton(
              ns("submit"),
              "Continue →",
              class = "btn btn-secondary",
              disabled = TRUE
            )
          )
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
# =====================================================
# SERVER
# =====================================================
mod_tab1_server <- function(id, ctx, session_global) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 🧠 VALIDATION (UNCHANGED LOGIC)
    # =====================================================
    dataset_valid <- reactive({
      
      name    <- input$dataset_name
      version <- input$version
      desc    <- input$description
      embargo <- input$embargo
      
      if (is.null(name) || is.null(version) || is.null(desc) || is.null(embargo))
        return(FALSE)
      
      valid_name <-
        nchar(name) >= 3 &&
        nchar(name) <= 8 &&
        grepl("^[A-Z0-9]+$", name)
      
      valid_version <- version >= 1 && version <= 99
      
      valid_desc <- nchar(trimws(desc)) >= 50
      
      valid_date <- as.Date(embargo) >= Sys.Date()
      
      valid_name && valid_version && valid_desc && valid_date
    })
    
    # =====================================================
    # 🧭 READINESS UPDATE (NO NAVIGATION HERE)
    # =====================================================
    observe({
      
      valid <- dataset_valid()
      
      # ONLY readiness state (safe separation)
      ctx$ready$dataset_valid <- valid
      ctx$ready$dataset_ready <- valid
    })
    
    # =====================================================
    # 🎨 UI STATE (UNCHANGED LOGIC)
    # =====================================================
    observe({
      
      valid <- dataset_valid()
      
      if (valid) shinyjs::enable("submit")
      else shinyjs::disable("submit")
      
      color <- if (valid) "#198754" else "#dc3545"
      
      shinyjs::runjs(sprintf("
        var header = document.getElementById('%s');
        if (header) {
          header.style.backgroundColor = '%s';
          header.style.color = '#fff';
        }

        var btn = document.getElementById('%s');
        if (btn) {
          btn.classList.remove('btn-success','btn-secondary');
          btn.classList.add(%s ? 'btn-success' : 'btn-secondary');
        }
      ",
                             ns("card_header1"),
                             color,
                             ns("submit"),
                             tolower(as.character(valid))
      ))
    })
    
    # =====================================================
    # 🚀 NAVIGATION (FIXED)
    # =====================================================
    observeEvent(input$submit, {
      
      req(dataset_valid())
      
      # 🔥 NOW USING NAV CONTROLLER (NOT v2)
      ctx$nav$stage <- "tab2"
    })
    
    # =====================================================
    # 🧾 VALIDATION UI (UNCHANGED)
    # =====================================================
    output$v_name <- renderUI({
      
      name <- input$dataset_name
      
      ok <- !is.null(name) &&
        nchar(name) >= 3 &&
        nchar(name) <= 8 &&
        grepl("^[A-Z0-9]*$", name)
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:14px;")
      else tags$span("✖ 3–8 uppercase letters/numbers", style="color:red;font-size:14px;")
    })
    
    output$v_version <- renderUI({
      
      v <- input$version
      
      ok <- !is.null(v) && v >= 1 && v <= 99
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:14px;")
      else tags$span("✖ must be 1–99", style="color:red;font-size:14px;")
    })
    
    output$v_description <- renderUI({
      
      d <- input$description
      
      ok <- !is.null(d) && nchar(trimws(d)) >= 50
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:14px;")
      else tags$span("✖ at least 50 characters", style="color:red;font-size:14px;")
    })
    
    output$v_embargo <- renderUI({
      
      e <- input$embargo
      
      ok <- !is.null(e) && as.Date(e) >= Sys.Date()
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:14px;")
      else tags$span("✖ invalid date", style="color:red;font-size:14px;")
    })
    
  })
}
