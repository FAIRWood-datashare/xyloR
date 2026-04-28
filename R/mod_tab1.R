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
    title = "Upload observation",
    value = "tab1",
    
    shiny::fluidRow(
      shiny::column(
        12,
        uiOutput(ns("tab1_ui"))
      )
    )
  )
}

# =====================================================
# UI RENDER FUNCTION
# =====================================================
render_tab1_ui <- function(step, ns) {
  
  switch(as.character(step),
         
         "0" = div(
           class = "p-5 text-center",
           h3("📊 Upload Observation Wizard"),
           p("Start by creating or loading a dataset"),
           actionButton(ns("start_wizard"), "Start")
         ),
         
         "1" = bslib::card(
           bslib::card_header(id = ns("card_header1"), "1. Dataset definition"),
           
           bslib::card_body(
             
             # Dataset name + status
             div(
               textInput(ns("dataset_name"), "Dataset name"),
               uiOutput(ns("v_name"))
             ),
             
             # Version + status
             div(
               numericInput(ns("version"), "Data version", 1, 1, 99),
               uiOutput(ns("v_version"))
             ),
             
             # Embargo + status
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
             
             # Description + status
             div(
               textAreaInput(ns("description"), "Description"),
               uiOutput(ns("v_description"))
             ),
             
             br(),
             
             actionButton(
               ns("submit"),
               "Continue →",
               class = "btn btn-primary"
             )
           )
         ),
         
         "2" = tagList(
           actionButton(ns("back_btn"), "← Back", class = "btn btn-secondary mb-2"),
           
           bslib::card(
             bslib::card_header("2. Dataset actions"),
             bslib::card_body(
               downloadButton(ns("download_template"), "Download template"),
               br(), br(),
               fileInput(ns("obs_file"), "Upload observation file")
             )
           )
         ),
         
         "3" = tagList(
           actionButton(ns("back_btn"), "← Back", class = "btn btn-secondary mb-2"),
           
           fluidRow(
             
             column(
               4,
               
               bslib::card(
                 bslib::card_header("Sanity check"),
                 bslib::card_body(
                   DT::DTOutput(ns("sanity_table")),
                   uiOutput(ns("sanity_message"))
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
                     class = "btn btn-primary w-100"
                   )
                 )
               )
             ),
             
             column(
               8,
               bslib::card(
                 bslib::card_header("Preview"),
                 bslib::card_body(
                   DT::DTOutput(ns("obs_preview"))
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
    # STEP STATE
    # =====================================================
    step <- reactiveVal(0)
    
    # =====================================================
    # FORM SYNC HELPERS (SOURCE OF TRUTH)
    # =====================================================
    
    sync_form_from_inputs <- function() {
      
      ctx$form$dataset_name <- input$dataset_name
      ctx$form$version      <- input$version
      ctx$form$description  <- input$description
      ctx$form$embargo      <- input$embargo
    }
    
    commit_form <- function() {
      
      ctx$data$meta <- list(
        dataset_name = ctx$form$dataset_name,
        version      = ctx$form$version,
        description  = ctx$form$description,
        embargo      = ctx$form$embargo
      )
    }
    
    # =====================================================
    # UI RENDER
    # =====================================================
    output$tab1_ui <- renderUI({
      render_tab1_ui(step(), ns)
    })
    
    # =====================================================
    # START
    # =====================================================
    observeEvent(input$start_wizard, {
      step(1)
    })
    
    # =====================================================
    # CENTRAL VALIDATION (SINGLE SOURCE OF TRUTH)
    # =====================================================
    dataset_valid <- reactive({
      
      req(ctx$form$dataset_name,
          ctx$form$version,
          ctx$form$description,
          ctx$form$embargo)
      
      nchar(ctx$form$dataset_name) >= 3 &&
        nchar(ctx$form$dataset_name) <= 8 &&
        grepl("^[A-Z0-9]+$", ctx$form$dataset_name) &&
        ctx$form$version >= 1 && ctx$form$version <= 99 &&
        nchar(trimws(ctx$form$description)) >= 50 &&
        as.Date(ctx$form$embargo) >= Sys.Date() &&
        as.Date(ctx$form$embargo) <= Sys.Date() + 365*10
    })
    
    observeEvent(input$dataset_name, sync_form_from_inputs())
    observeEvent(input$version, sync_form_from_inputs())
    observeEvent(input$description, sync_form_from_inputs())
    observeEvent(input$embargo, sync_form_from_inputs())
    
    # =====================================================
    # FIELD VALIDATION HELPERS (LIVE FEEDBACK)
    # =====================================================
    
    output$v_name <- renderUI({
      req(input$dataset_name)
      
      ok <- nchar(input$dataset_name) >= 3 &&
        nchar(input$dataset_name) <= 8 &&
        grepl("^[A-Z0-9]+$", input$dataset_name)
      
      if (ok) tags$span("✔ valid", style = "color: green; font-size: 12px;")
      else tags$span("✖ 3–8 uppercase letters/numbers", style = "color: red; font-size: 12px;")
    })
    
    output$v_version <- renderUI({
      req(input$version)
      
      ok <- input$version >= 1 && input$version <= 99
      
      if (ok) tags$span("✔ valid", style = "color: green; font-size: 12px;")
      else tags$span("✖ must be 1–99", style = "color: red; font-size: 12px;")
    })
    
    output$v_description <- renderUI({
      req(input$description)
      
      ok <- nchar(trimws(input$description)) >= 50
      
      if (ok) tags$span("✔ valid", style = "color: green; font-size: 12px;")
      else tags$span("✖ at least 50 characters", style = "color: red; font-size: 12px;")
    })
    
    output$v_embargo <- renderUI({
      req(input$embargo)
      
      ok <- as.Date(input$embargo) >= Sys.Date() &&
        as.Date(input$embargo) <= Sys.Date() + 365*10
      
      if (ok) tags$span("✔ valid", style = "color: green; font-size: 12px;")
      else tags$span("✖ invalid date range", style = "color: red; font-size: 12px;")
    })
    
    # =====================================================
    # BUTTON ENABLE/DISABLE
    # =====================================================
    observe({
      shinyjs::toggleState(id = ns("submit"), condition = dataset_valid())
    })
    
    # =====================================================
    # HEADER COLOR (LIVE)
    # =====================================================
    observe({
      
      if (step() != 1) return()
      
      if (dataset_valid()) {
        shinyjs::addClass(id = ns("card_header1"), class = "bg-success")
        shinyjs::removeClass(id = ns("card_header1"), class = "bg-danger")
      } else {
        shinyjs::addClass(id = ns("card_header1"), class = "bg-danger")
        shinyjs::removeClass(id = ns("card_header1"), class = "bg-success")
      }
    })
    
    # =====================================================
    # CONTINUE → STEP 2
    # =====================================================
    observeEvent(input$submit, {
      
      if (!dataset_valid()) {
        showNotification("Please fix validation errors", type = "error")
        return()
      }
      
      showNotification("Dataset validated", type = "message")
      
      # 🔥 SAVE FORM → DATA
      commit_form()
      
      step(2)
    })
    
    # =====================================================
    # BACK BUTTON (SAFE EXTENSION)
    # =====================================================
    observeEvent(input$back_btn, {
      
      current <- isolate(step())
      
      if (current > 0) step(current - 1)
      
      # 🔥 RESTORE FORM VALUES INTO UI
      updateTextInput(session, "dataset_name",
                      value = ctx$form$dataset_name %||% "")
      
      updateNumericInput(session, "version",
                         value = ctx$form$version %||% 1)
      
      updateTextAreaInput(session, "description",
                          value = ctx$form$description %||% "")
      
      updateDateInput(session, "embargo",
                      value = ctx$form$embargo %||% Sys.Date())
    })
    
    # =====================================================
    # UPLOAD (STEP 2 → STEP 3)
    # =====================================================
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      df <- tryCatch(
        openxlsx::readWorkbook(
          input$obs_file$datapath,
          sheet = "Xylo_obs_data",
          startRow = 1
        )[-(1:6), ] |> tibble::as_tibble(),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
      
      req(!is.null(df))
      
      ctx$form$obs_file <- input$obs_file
      ctx$data$obs_raw   <- df
      ctx$data$obs_truth <- df
      ctx$data$draft_obs <- df
      
      step(3)
    })
    
    # =====================================================
    # SANITY TABLE
    # =====================================================
    output$sanity_table <- DT::renderDT({
      
      req(ctx$data$obs_truth)
      
      df <- ctx$data$obs_truth
      
      DT::datatable(data.frame(
        check = c("rows", "site_label", "no NA site_label"),
        status = c(
          nrow(df) > 0,
          "site_label" %in% names(df),
          !any(is.na(df$site_label))
        )
      ), rownames = FALSE)
    })
    
    # =====================================================
    # PREVIEW
    # =====================================================
    output$obs_preview <- DT::renderDT({
      req(ctx$data$obs_truth)
      DT::datatable(head(ctx$data$obs_truth, 50))
    })
    
    # =====================================================
    # STEP 3 VALIDATION GATE
    # =====================================================
    validation_ok <- reactive({
      isTRUE(input$validate_location) &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
    })
    
    observe({
      shinyjs::toggleState(id = ns("next_btn"), condition = validation_ok())
    })
    
    observe({
      if (validation_ok()) {
        shinyjs::addClass(id = ns("card_header_validation"), class = "bg-success")
        shinyjs::removeClass(id = ns("card_header_validation"), class = "bg-danger")
      } else {
        shinyjs::addClass(id = ns("card_header_validation"), class = "bg-danger")
        shinyjs::removeClass(id = ns("card_header_validation"), class = "bg-success")
      }
    })
    
    # =====================================================
    # FINAL → FSM
    # =====================================================
    observeEvent(input$next_btn, {
      
      if (!validation_ok()) {
        showNotification("Please complete validation", type = "error")
        return()
      }
      
      showNotification("Validation complete → moving forward", type = "message")
      
      ctx$signals$tab1_done <- TRUE
    })
    
  })
}
