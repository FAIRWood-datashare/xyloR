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
    
    observe({
      
      ctx$form$dataset_name <- isolate(input$dataset_name)
      ctx$form$version      <- isolate(input$version)
      ctx$form$description  <- isolate(input$description)
      ctx$form$embargo      <- isolate(input$embargo)
      
    })
    
    # =====================================================
    # 🧠 FORM → CTX SYNC
    # =====================================================
    observeEvent(input$dataset_name, {
      ctx$form$dataset_name <- input$dataset_name
    }, ignoreInit = TRUE)
    
    observeEvent(input$version, {
      ctx$form$version <- input$version
    }, ignoreInit = TRUE)
    
    observeEvent(input$description, {
      ctx$form$description <- input$description
    }, ignoreInit = TRUE)
    
    observeEvent(input$embargo, {
      ctx$form$embargo <- input$embargo
    }, ignoreInit = TRUE)
    
    # =====================================================
    # 🧠 VALIDATION
    # =====================================================
    dataset_valid <- reactive({
      
      name    <- ctx$form$dataset_name
      version <- ctx$form$version
      desc    <- ctx$form$description
      embargo <- ctx$form$embargo
      
      if (is.null(name) || is.null(version) || is.null(desc) || is.null(embargo))
        return(FALSE)
      
      nchar(name) >= 3 &&
        nchar(name) <= 8 &&
        grepl("^[A-Z0-9]+$", name) &&
        version >= 1 && version <= 99 &&
        nchar(trimws(desc)) >= 50 &&
        as.Date(embargo) >= Sys.Date()
    })
    
    # =====================================================
    # 🧭 V2 STATE SYNC
    # =====================================================
    observe({
      valid <- dataset_valid()
      
      ctx$v2$dataset_valid <- valid
      ctx$v2$dataset_ready <- valid
    })
    
    # =====================================================
    # 🎨 UI FEEDBACK
    # =====================================================
    observe({
      
      valid <- dataset_valid()
      
      if (valid) {
        shinyjs::enable("submit")
      } else {
        shinyjs::disable("submit")
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
                             ns("card_header1"),
                             color,
                             ns("submit"),
                             tolower(as.character(valid))
      ))
    })
    
    # =====================================================
    # 🚀 NAVIGATION → TAB2
    # =====================================================
    observeEvent(input$submit, {
      
      req(dataset_valid())
      
      ctx$v2$stage <- "tab2"
    })
    
    # =====================================================
    # 🧾 VALIDATION LABELS (UNCHANGED)
    # =====================================================
    output$v_name <- renderUI({
      ok <- !is.null(ctx$form$dataset_name) &&
        nchar(ctx$form$dataset_name) >= 3 &&
        nchar(ctx$form$dataset_name) <= 8 &&
        grepl("^[A-Z0-9]*$", ctx$form$dataset_name)
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:14px;")
      else tags$span("✖ 3–8 uppercase letters/numbers", style="color:red;font-size:14px;")
    })
    
    output$v_version <- renderUI({
      ok <- !is.null(ctx$form$version) &&
        ctx$form$version >= 1 && ctx$form$version <= 99
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:14px;")
      else tags$span("✖ must be 1–99", style="color:red;font-size:14px;")
    })
    
    output$v_description <- renderUI({
      ok <- !is.null(ctx$form$description) &&
        nchar(trimws(ctx$form$description)) >= 50
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:14px;")
      else tags$span("✖ at least 50 characters", style="color:red;font-size:14px;")
    })
    
    output$v_embargo <- renderUI({
      ok <- !is.null(ctx$form$embargo) &&
        as.Date(ctx$form$embargo) >= Sys.Date()
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:14px;")
      else tags$span("✖ invalid date", style="color:red;font-size:14px;")
    })
    
  })
}
