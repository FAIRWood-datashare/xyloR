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
             bslib::card_header("2. Upload data"),
             bslib::card_body(
               
               downloadButton(ns("download_template"), "Download template"),
               downloadButton(ns("download_example_obs"), "Download example"),
               
               br(), br(),
               
               fileInput(ns("obs_file"), "Upload observation file")
             )
           )
         ),
         
         # =====================================================
         # STEP 3
         # =====================================================
         "3" = tagList(
           actionButton(ns("back_btn"), "← Back", class = "btn btn-secondary mb-2"),
           
           fluidRow(
             
             # ================= LEFT PANEL =================
             column(
               4,
               
               # ✅ Sanity
               # bslib::card(
               #   bslib::card_header("Sanity check"),
               #   bslib::card_body(
               #     DT::DTOutput(ns("sanity_table"))
               #   )
               # ),
               
               # ✅ Site selector (MOVED HERE)
               selectInput(ns("site_filter"), "Select site", choices = NULL),
               
               # ✅ Key info
               bslib::card(
                 bslib::card_header("Key information"),
                 bslib::card_body(
                   DT::DTOutput(ns("key_info_table"))
                 )
               ),
               
               # ✅ Validation
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
             
             # ================= RIGHT PANEL =================
             column(
               8,
               
               # ✅ Preview (you already have it)
               # bslib::card(
               #   bslib::card_header("Preview"),
               #   bslib::card_body(
               #     DT::DTOutput(ns("obs_preview"))
               #   )
               # ),
               
               # ✅ Map
               bslib::card(
                 bslib::card_header("Map"),
                 bslib::card_body(
                   leaflet::leafletOutput(ns("mymap"), height = "300px")
                 )
               ),
               
               # ✅ Plot
               bslib::card(
                 bslib::card_header("Data coverage"),
                 bslib::card_body(
                   selectInput(ns("color"), "Color by",
                               choices = c("tree_species", "sample_id", "plot_label")),
                   plotly::plotlyOutput(ns("data_coverage_plot"))
                 )
               ),
               
               # ✅ Coverage table
               bslib::card(
                 bslib::card_header("Coverage table"),
                 bslib::card_body(
                   DT::DTOutput(ns("obs_table"))
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
    
    apply_ui_state <- function(header_id, button_id, valid) {

      # ----------------------------------------------------------
      # BUTTON: shinyjs::enable/disable use the module session
      # automatically — pass the plain (non-ns) id only.
      # ----------------------------------------------------------
      if (isTRUE(valid)) {
        shinyjs::enable(button_id)
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').classList.remove('btn-secondary');
           document.getElementById('%s').classList.add('btn-success');",
          ns(button_id), ns(button_id)
        ))
      } else {
        shinyjs::disable(button_id)
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').classList.remove('btn-success');
           document.getElementById('%s').classList.add('btn-secondary');",
          ns(button_id), ns(button_id)
        ))
      }

      # ----------------------------------------------------------
      # HEADER: bslib card_header nests the id inside a <div
      # class="card-header"> wrapper, so we target by id directly
      # via querySelector. ns() is needed here for the DOM id.
      # ----------------------------------------------------------
      color <- if (isTRUE(valid)) "#198754" else "#dc3545"   # BS5 success / danger
      shinyjs::runjs(sprintf(
        "var el = document.getElementById('%s');
         if (el) { el.style.backgroundColor = '%s'; el.style.color = '#fff'; }",
        ns(header_id), color
      ))
    }
    
    # =====================================================
    # STEP STATE
    # =====================================================
    step <- reactiveVal(0)

    # Increments every time the UI is (re)rendered for a step.
    # The effect observer watches this so it always re-fires
    # after DOM rebuild, even when dataset_valid() hasn't changed.
    step_rendered <- reactiveVal(0)

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
      isolate({
        render_tab1_ui(step(), ctx, ns)
      })
    }) |> bindEvent(step(), ignoreNULL = FALSE, ignoreInit = FALSE)

    # Bump step_rendered after every renderUI completes so the
    # effect observer knows the DOM is ready.
    outputOptions(output, "tab1_ui", suspendWhenHidden = FALSE)
    
    observe({
      step()                          # take dependency on step
      shinyjs::delay(50, {            # wait 50 ms for DOM to paint
        step_rendered(step_rendered() + 1L)
      })
    }) |> bindEvent(step(), ignoreNULL = FALSE, ignoreInit = FALSE)

    # =====================================================
    # START
    # =====================================================
    observeEvent(input$start_wizard, {
      step(1)
    })
    
    # =====================================================
    # FORM SYNC OBSERVERS — write to ctx$form only,
    # never recreate inputs (step() not touched here)
    # ======================================================
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
    # Reads from ctx$form so it works after back-navigation
    # =====================================================
    dataset_valid <- reactive({
      
      name    <- ctx$form$dataset_name
      version <- ctx$form$version
      desc    <- ctx$form$description
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
    # STEP 1 EFFECT
    # Depends on dataset_valid() AND step_rendered() so it
    # re-fires both when validation changes (typing) AND right
    # after the DOM is rebuilt on back-navigation.
    # =====================================================
    observe({
      step_rendered()          # re-run after every DOM rebuild
      valid <- dataset_valid()
      s     <- step()
      if (s == 1) {
        apply_ui_state(
          header_id = "card_header1",
          button_id = "submit",
          valid     = valid
        )
      }
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
      target  <- max(0L, current - 1L)
      
      if (target == 1L) {
        updateTextInput(session,     "dataset_name", value = ctx$form$dataset_name %||% "")
        updateNumericInput(session,  "version",      value = ctx$form$version      %||% 1)
        updateTextAreaInput(session, "description",  value = ctx$form$description  %||% "")
        updateDateInput(session,     "embargo",      value = ctx$form$embargo      %||% Sys.Date())
      }
      
      step(target)
    })
    
    # =====================================================
    # STEP 2a - DOWNLOAD TEMPLATES
    # =====================================================
    
    output$download_template <- downloadHandler(
      filename = function() {
        paste0(ctx$form$dataset_name %||% "Dataset",
               "_xylo_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        tp <- system.file(
          "extdata",
          "Datasetname_xylo_data_yyyy-mm-dd.xlsx",
          package = "xyloR"
        )
        file.copy(tp, file, overwrite = TRUE)
      }
    )
    
    output$download_example_obs <- downloadHandler(
      filename = function() "Example_xylo_data.xlsx",
      content = function(file) {
        tp <- system.file(
          "extdata",
          "Ltal2007_xylo_data_2025-09-01.xlsx",
          package = "xyloR"
        )
        file.copy(tp, file, overwrite = TRUE)
      }
    )
    
    # =====================================================
    # STEP 3 - MAR, COVERAGE, TABLE RENDERERS
    # =====================================================
    xylo_obs <- reactive({
      req(ctx$data$obs_truth)
      ctx$data$obs_truth
    })
    
    site_info <- reactive({
      req(ctx$data$site_info)
      ctx$data$site_info
    })
    
    obs_file_path <- reactive({
      req(input$obs_file)
      input$obs_file$datapath
    })
    
    # =====================================================
    # STEP 3 STATE (NEW)
    # =====================================================
    
    selected_site <- reactiveVal(NULL)
    
    xylo_obs <- reactive({
      req(ctx$data$obs_truth)
      ctx$data$obs_truth
    })
    
    site_info <- reactive({
      req(ctx$data$site_info)
      ctx$data$site_info
    })
    
    df_site <- reactive({
      req(xylo_obs(), selected_site())
      xylo_obs() %>%
        dplyr::filter(site_label == selected_site())
    })
    
    observe({
      req(site_info())
      
      sites <- unique(trimws(as.character(site_info()$site_label)))
      
      updateSelectInput(session, "site_filter", choices = sites)
      
      # initialize only once
      if (is.null(selected_site())) {
        selected_site(sites[1])
      }
    })
    
    observeEvent(input$site_filter, {
      selected_site(input$site_filter)
    }, ignoreInit = TRUE)
    
    # =========================================================
    # KEY INFO TABLE
    # =========================================================
    output$key_info_table <- DT::renderDataTable({
      
      df <- df_site()
      req(nrow(df) > 0)
      
      si <- site_info() %>%
        dplyr::filter(site_label == selected_site())
      
      file <- obs_file_path()
      
      owner_lastname   <- read_xylo_cell(file, "obs_data_info", 2, 4)
      owner_firstname  <- read_xylo_cell(file, "obs_data_info", 1, 4)
      owner_email      <- read_xylo_cell(file, "obs_data_info", 3, 4)
      
      contact_lastname  <- read_xylo_cell(file, "obs_data_info", 2, 2)
      contact_firstname <- read_xylo_cell(file, "obs_data_info", 1, 2)
      contact_email     <- read_xylo_cell(file, "obs_data_info", 3, 2)
      
      key_info <- tibble::tibble(
        "PI" = paste(owner_lastname, owner_firstname),
        "PI Email" = owner_email,
        "Contact" = paste(contact_lastname, contact_firstname),
        "Contact Email" = contact_email,
        "Network" = paste(unique(df$network_label), collapse = ", "),
        "Site" = si$site_label,
        "Coordinates" = paste(
          "Lat =", round(as.numeric(si$latitude), 4),
          "Long =", round(as.numeric(si$longitude), 4)
        ),
        "Elevation" = as.numeric(si$elevation),
        "Date From" = format(min(df$sample_date), "%Y-%m-%d"),
        "Date To"   = format(max(df$sample_date), "%Y-%m-%d"),
        "n_Trees"   = length(unique(df$tree_label)),
        "n_Dates"   = length(unique(df$sample_date)),
        "n_Samples" = length(unique(paste(df$sample_label, df$sample_id)))
      ) %>%
        t() %>%
        setNames("Key Info")
      
      DT::datatable(key_info, options = list(dom = "t"), class = "table-dark")
    })
    
    # =========================================================
    # COVERAGE TABLE
    # =========================================================
    output$obs_table <- DT::renderDataTable({
      
      df <- df_site()
      
      excluded <- c(
        "sample_date","sample_id","tree_species","tree_label",
        "plot_label","site_label","network_label",
        "sample_label","measure_type","measure_repetition",
        "sample_comment"
      )
      
      cols <- setdiff(names(df), excluded)
      
      summary <- df %>%
        dplyr::filter(dplyr::if_any(dplyr::all_of(cols), ~ !is.na(.))) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(cols)) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::group_by(measure_type, sample_label, sample_id, name) %>%
        dplyr::summarise(n = n(), .groups = "drop") %>%
        dplyr::group_by(measure_type, name) %>%
        dplyr::summarise(avg = mean(n), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = name, values_from = avg)
      
      DT::datatable(summary, options = list(dom = "t"), class = "table-dark")
    })
    
    # =========================================================
    # MAP LEAFLET
    # =========================================================
    output$mymap <- leaflet::renderLeaflet({
      
      si <- site_info() %>%
        dplyr::filter(site_label == selected_site())
      
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(
          lng = as.numeric(si$longitude),
          lat = as.numeric(si$latitude),
          popup = si$site_label
        )
    })
    
    # =========================================================
    # AVERAGE OBS PLOT
    # =========================================================
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
          font = list(color = "white")
        )
    })
    
    # =========================================================
    # SANITY TABLE
    # =========================================================
    
    # output$sanity_table <- DT::renderDT({
    #   
    #   req(ctx$data$obs_truth)
    #   
    #   df <- ctx$data$obs_truth
    #   
    #   DT::datatable(
    #     data.frame(
    #       check = c("rows", "site_label", "no NA site_label"),
    #       status = c(
    #         nrow(df) > 0,
    #         "site_label" %in% names(df),
    #         !any(is.na(df$site_label))
    #       )
    #     ),
    #     rownames = FALSE
    #   )
    # })
    
    # =========================================================
    # DATA PREVIEW
    # =========================================================
    # output$obs_preview <- DT::renderDT({
    # 
    #   req(ctx$data$obs_truth)
    # 
    #   DT::datatable(head(ctx$data$obs_truth, 50))
    # })
    
    # =====================================================
    # STEP 2 + 3 (UNCHANGED WORKING LOGIC)
    # =====================================================
    
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      # -------------------------------------------------
      # LOAD DATA
      # -------------------------------------------------
      obs <- tryCatch(
        load_xylo_obs_clean_contract(input$obs_file$datapath),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
      
      req(!is.null(obs))
      
      site_info <- tryCatch(
        extract_site_info(input$obs_file$datapath),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
      
      # =====================================================
      # FIX 1: SET THE MISSING OBJECT (CRITICAL)
      # =====================================================
      ctx$data$obs_truth <- obs
      
      # -------------------------------------------------
      # UPDATE CTX
      # -------------------------------------------------
      ctx$files$obs_file   <- input$obs_file
      ctx$data$obs         <- obs
      ctx$data$site_info   <- site_info
      
      ctx$data$dataset_name <- ctx$form$dataset_name
      ctx$data$version      <- ctx$form$version
      ctx$data$embargo      <- ctx$form$embargo
      ctx$data$description  <- ctx$form$description
      
      # -------------------------------------------------
      # FSM TRANSITION
      # -------------------------------------------------
      step(3)
    })
    
    validation_ok <- reactive({
      isTRUE(input$validate_location) &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
    })
    
    # STEP 3 EFFECT
    # Same pattern: step_rendered() ensures re-application
    # after DOM rebuild (back navigation from a future step).
    # =====================================================
    observe({
      step_rendered()
      valid <- validation_ok()
      s     <- step()
      if (s == 3) {
        apply_ui_state(
          header_id = "card_header_validation",
          button_id = "next_btn",
          valid     = valid
        )
      }
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
