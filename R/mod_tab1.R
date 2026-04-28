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
render_tab1_ui <- function(step, ctx, ns) {
  
  switch(as.character(step),
         
         # =====================================================
         # STEP 0
         # =====================================================
         "0" = div(
           class = "p-5 text-center",
           h3("📊 Upload Observation Wizard"),
           p("Start by creating or loading a dataset"),
           actionButton(ns("start_wizard"), "Start")
         ),
         
         # =====================================================
         # STEP 1 (FIXED: STABLE INPUTS)
         # =====================================================
         "1" = bslib::card(
           
           bslib::card_header(
             id = ns("card_header1"),
             # class = "bg-danger",   # default only (NO reactive UI rebuild here)
             "1. Dataset definition"
           ),
           
           bslib::card_body(
             
             # --- stable inputs (IMPORTANT FIX) ---
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
             
             # --- button (kept stable, no reactive class here) ---
             actionButton(
               ns("submit"),
               "Continue →",
               class = "btn btn-secondary",
               disabled = TRUE
             )
           )
         ),
         
         # =====================================================
         # STEP 2
         # =====================================================
         "2" = tagList(
           actionButton(ns("back_btn"), "← Back", class = "btn btn-secondary mb-2"),
           
           bslib::card(
             bslib::card_header(id = ns("hdr_step2"), "2. Dataset actions"),
             bslib::card_body(
               downloadButton(ns("download_template"), "Download template"),
               br(), br(),
               fileInput(ns("obs_file"), "Upload observation file"),
               uiOutput(ns("file_status"))
             )
           )
         ),
         
         # =====================================================
         # STEP 3
         # =====================================================
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
                 bslib::card_header(id = ns("card_header_validation"), "3. Validation"),
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
    
    apply_ui_state <- function(header_id, button_id, valid, ns) {
      
      hid <- (header_id)
      bid <- (button_id)
      
      # =========================
      # HEADER COLOR
      # =========================
      shinyjs::removeClass(hid, "bg-success")
      shinyjs::removeClass(hid, "bg-danger")
      
      if (isTRUE(valid)) {
        shinyjs::addClass(hid, "bg-success")
      } else {
        shinyjs::addClass(hid, "bg-danger")
      }
      
      # =========================
      # BUTTON ENABLE/DISABLE
      # =========================
      shinyjs::toggleState(id = bid, condition = isTRUE(valid))
      
      # =========================
      # BUTTON COLOR (THIS WAS MISSING)
      # =========================
      shinyjs::removeClass(bid, "btn-success")
      shinyjs::removeClass(bid, "btn-secondary")
      
      if (isTRUE(valid)) {
        shinyjs::addClass(bid, "btn-success")
      } else {
        shinyjs::addClass(bid, "btn-secondary")
      }
    }
    
    # =====================================================
    # STEP STATE
    # =====================================================
    step <- reactiveVal(0)
    
    # =====================================================
    # FORM SYNC (UI → CTX ONLY)
    # =====================================================
    sync_form <- function() {
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
    # UI RENDER (STABLE)
    # =====================================================
    output$tab1_ui <- renderUI({
      render_tab1_ui(step(), ctx, ns)
    })
    
    # =====================================================
    # START
    # =====================================================
    observeEvent(input$start_wizard, {
      step(1)
    })
    
    # =====================================================
    # FORM SYNC OBSERVERS (SAFE)
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
    # CENTRAL VALIDATION (SOURCE OF TRUTH)
    # =====================================================
    dataset_valid <- reactive({
      
      name <- ctx$form$dataset_name
      version <- ctx$form$version
      desc <- ctx$form$description
      embargo <- ctx$form$embargo
      
      if (is.null(name) || is.null(version) || is.null(desc) || is.null(embargo))
        return(FALSE)
      
      name_ok <- nchar(name) >= 3 &&
        nchar(name) <= 8 &&
        grepl("^[A-Z0-9]+$", name)
      
      version_ok <- version >= 1 && version <= 99
      
      desc_ok <- nchar(trimws(desc)) >= 50
      
      embargo_ok <- as.Date(embargo) >= Sys.Date() &&
        as.Date(embargo) <= Sys.Date() + 365*10
      
      name_ok && version_ok && desc_ok && embargo_ok
    })
    
    # =====================================================
    # LIVE VALIDATION LABELS (SAFE)
    # =====================================================
    
    output$v_name <- renderUI({
      ok <- !is.null(ctx$form$dataset_name) &&
        nchar(ctx$form$dataset_name) >= 3 &&
        nchar(ctx$form$dataset_name) <= 8 &&
        grepl("^[A-Z0-9]*$", ctx$form$dataset_name)
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:12px;")
      else tags$span("✖ 3–8 uppercase letters/numbers", style="color:red;font-size:12px;")
    })
    
    output$v_version <- renderUI({
      ok <- !is.null(ctx$form$version) &&
        ctx$form$version >= 1 && ctx$form$version <= 99
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:12px;")
      else tags$span("✖ must be 1–99", style="color:red;font-size:12px;")
    })
    
    output$v_description <- renderUI({
      ok <- !is.null(ctx$form$description) &&
        nchar(trimws(ctx$form$description)) >= 50
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:12px;")
      else tags$span("✖ at least 50 characters", style="color:red;font-size:12px;")
    })
    
    output$v_embargo <- renderUI({
      ok <- !is.null(ctx$form$embargo) &&
        as.Date(ctx$form$embargo) >= Sys.Date() &&
        as.Date(ctx$form$embargo) <= Sys.Date() + 365*10
      
      if (ok) tags$span("✔ valid", style="color:green;font-size:12px;")
      else tags$span("✖ invalid date range", style="color:red;font-size:12px;")
    })
    
    # =====================================================
    # STEP 1 HEADER + BUTTON (FIXED — LIKE STEP 3 STYLE)
    # =====================================================
    observe({
      
      req(step() == 1)
      
      apply_ui_state(
        header_id = "card_header1",
        button_id = ("submit"),
        valid     = dataset_valid()
      )
    })
    
    # =====================================================
    # STEP 1 → STEP 2
    # =====================================================
    observeEvent(input$submit, {
      
      if (!dataset_valid()) {
        showNotification("Fix validation errors first", type = "error")
        return()
      }
      
      commit_form()
      step(2)
    })
    
    # =====================================================
    # BACK BUTTON
    # =====================================================
    observeEvent(input$back_btn, {
      
      current <- step()
      step(max(0, current - 1))
      
      if (current - 1 == 1) {
        
        updateTextInput(session, "dataset_name", value = ctx$form$dataset_name %||% "")
        updateNumericInput(session, "version", value = ctx$form$version %||% 1)
        updateTextAreaInput(session, "description", value = ctx$form$description %||% "")
        updateDateInput(session, "embargo", value = ctx$form$embargo %||% Sys.Date())
      }
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
    # STEP 2 + 3 (UNCHANGED WORKING LOGIC)
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
      
      ctx$data$obs_raw <- df
      ctx$data$obs_truth <- df
      ctx$data$draft_obs <- df
      
      step(3)
    })
    
    validation_ok <- reactive({
      isTRUE(input$validate_location) &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
    })
    
    # observe({
    #   shinyjs::toggleState(id = ns("next_btn"), condition = validation_ok())
    # })
    # 
    # observe({
    #   shinyjs::toggleClass(
    #     id = ns("card_header_validation"),
    #     class = "bg-success",
    #     condition = validation_ok()
    #   )
    #   
    #   shinyjs::toggleClass(
    #     id = ns("card_header_validation"),
    #     class = "bg-danger",
    #     condition = !validation_ok()
    #   )
    # })
    
    observe({
      
      req(step() == 3)
      
      apply_ui_state(
        header_id = "card_header_validation",
        button_id = "next_btn",
        valid     = validation_ok()
      )
    })
    
    observeEvent(input$next_btn, {
      
      if (!validation_ok()) {
        showNotification("Please complete validation", type = "error")
        return()
      }
      
      ctx$signals$tab1_done <- TRUE
    })
    
  })
}
