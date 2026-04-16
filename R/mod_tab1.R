#' mod_tab1 UI Function
#'
#' @description A shiny module for the "Upload Observation" tab.
#'
#' This module provides a user interface to guide users through:
#' 1. Naming their dataset,
#' 2. Downloading a data template or example,
#' 3. Uploading their filled observation data file,
#' 4. Validating uploaded data using checkboxes,
#' 5. Displaying interactive visualizations such as a Leaflet map,
#'    Plotly-based data coverage plots, and summary tables.
#'
#' @param id A string that serves as the module namespace identifier.
#'
#' @return A `shiny.tag.list` containing the UI elements of the module.
#' @export
#'
#' @import shiny 
#' @importFrom bslib tooltip popover nav_panel card card_header card_body
#' @importFrom bsicons bs_icon
#' @importFrom leaflet leafletOutput
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @importFrom shinyjs addClass removeClass show runjs
#' @importFrom openxlsx loadWorkbook saveWorkbook readWorkbook
#' @importFrom rhandsontable rHandsontableOutput
#' 
mod_tab1_ui <- function(id) {
  ns <- shiny::NS(id)

   shiny::fluidRow(
      # ─── Left column: Upload section ─────────────────────────────
      shiny::column(
        width = 3,
        class = "bg-light p-2 border-end",
        style = "height: 100%;",

        # 1.1 Dataset framing
        bslib::card(
          bslib::card_header(
            "1.1 Framing your dataset",
            id = ns("card_header1_1"), class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Provide a unique identifier for your dataset. This will be used to name the output files. Then move to 1.2 Download observation data template",
              placement = "right"
            )
          ),
          bslib::card_body(
            
            shiny::textInput(
              ns("dataset_name"), 
              "Enter the DATASET NAME. 
          This must be alphanumeric, in uppercase letters, and 3–8 characters long", 
              value = "", 
              width = "100%", 
              placeholder = "3-8 characters"
            ) %>% 
              shiny::tagAppendAttributes(maxlength = 8),
            
            # Add a horizontal line
            shiny::tags$hr(style = "border-top: 2px solid #ccc;"),
            
            shiny::numericInput(
              ns("version"),
              "Enter the DATASET VERSION. 
              This must be a number between 1 and 99.",
              value = 1,
              min = 1,
              max = 99,
              step = 1,
              width = "100%"
            ),
            
            shiny::p("Note: Creating a dataset with the same NAME but a higher VERSION (e.g., 2 after 1) will overwrite the file previously imported in the DataBase", style = "color:#ff8c00; font-style: italic;"),
            
            # Add a horizontal line
            shiny::tags$hr(style = "border-top: 2px solid #ccc;"),
            
            shiny::dateInput(
              ns("embargo"),
              "Enter the EMBARGO END DATE. 
              This should be maximally 10 years from today, by default set to today's date.",
              value = Sys.Date(),       # default to today
              min = Sys.Date(),       # optional lower bound
              max = Sys.Date() + 3650,   # optional upper bound
              format = "yyyy-mm-dd",    # display format
              width = "100%"
            ),
            
            # Add a horizontal line
            shiny::tags$hr(style = "border-top: 2px solid #ccc;"),
            
            shiny::textAreaInput(
              ns("description"),            # input ID
              label = shiny::div(
                "Enter a DATASET DESCRIPTION.",
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Describe what the dataset is about, its scope, and any key details relevant to understanding its content and purpose. 
                  HERE a potential template:
                  The dataset was collected between [start year] and [end year] as part of a study designed to investigate [main research question or objective]. The study included [number] sites and focused on [number] species of trees, selected according to [sampling or selection criteria].
              
              Data recorded include [measurements, observations, or variables collected, e.g., tree height, diameter, species identity, phenology], along with metadata about the sites such as [environmental conditions, location coordinates, etc.]. The study design ensured consistent sampling across all sites to allow comparison of [key parameters, e.g., growth rates, biodiversity, ecological trends].
              
              This dataset can be used for [research applications, modeling, or monitoring purposes], providing insights into [ecosystem processes, forest dynamics, or other study-specific focus].",
                  placement = "right"
                )
              ),
              value = "",                   # initial content
              width = "100%",               # full width
              height = "500px",             # enough height for 2-3 paragraphs
              placeholder = "The dataset ..."
            ),
            
            shiny::p("Note: click on the questinonmark for a description template. This should be at least 50 chracters long to be validated. this will be uded to provide a short description text on the database webpage", style = "color:#ff8c00; font-style: italic;"),
            
            shiny::actionButton(ns("submit"), "Validate", class = "btn btn-primary")
          )
          ),

        # 1.2 Template download
        bslib::card(
          bslib::card_header(
            "1.2 Download observation data template",
            id = ns("card_header1_2"), class = "bg-warning",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Click 'Download Template' to save an empty Excel template for observation data. Then move to 1.3 Upload.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::fluidRow(
              shiny::column(6, shiny::downloadButton(ns("download_template"), "Download Template", class = "btn btn-primary")),
              shiny::column(6, shiny::downloadButton(ns("download_example_obs"), "Download example", class = "btn btn-secondary"))
            )
          ),
          style = "display: none;",
          id = ns("card_1")
        ),

        # 1.3 Upload file
        bslib::card(
          bslib::card_header(
            "1.3 Upload the filled observation data file!",
            id = ns("card_header1_3"), class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Upload your filled Excel file. A map and summary tables will appear.",
              placement = "right"
            )
          ),
          shiny::fileInput(ns("obs_file"), NULL, accept = c(".xlsx")),
          shiny::selectInput(ns("site_filter"), "Select Site", choices = NULL),
          style = "height: 300px; display: none;",
          id = ns("card_2")
        ),

        # Key information table
        bslib::card(
          bslib::card_header("Key Information Table"),
          bslib::card_body(DT::DTOutput(ns("key_info_table"))),
          style = "display: none; min-height: 600px; overflow: visible;",
          id = ns("card_3")
        ),

        # 1.4 Validate checkboxes
        bslib::card(
          bslib::card_header(
            "1.4 Validate your data",
            id = ns("card_header1_4"), class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Check all boxes to enable Next.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::checkboxInput(ns("validate_location"), "Validate Location", value = FALSE),
            shiny::checkboxInput(ns("validate_data_coverage"), "Validate Data Coverage", value = FALSE),
            shiny::checkboxInput(ns("validate_observation"), "Validate Observation list", value = FALSE),
            shiny::textOutput(ns("validation_status")),
            shiny::actionButton(ns("next_btn"), "Next", icon = shiny::icon("angle-double-right"), class = "btn btn-primary")
          ),
          style = "display: none;",
          id = ns("card_7")
        )
      ),

      # ─── Right column: Map, plotly, tables ──────────────────────
      shiny::column(
        width = 9,
        style = "height:100%;",

        # Geolocation Map
        bslib::card(
          bslib::card_header("Geolocation Map"),
          bslib::card_body(leaflet::leafletOutput(ns("mymap"), height = "400px")),
          style = "display: none;",
          id = ns("card_4")
        ),

        # Data Coverage Plot
        bslib::card(
          bslib::card_header(
            "Data Coverage Overview",
            bslib::popover(
              bsicons::bs_icon("gear", class = "ms-auto"),
              shiny::selectInput(ns("color"), "Color by", choices = c("tree_species", "sample_id", "plot_label")),
              title = "Plot Settings"
            )
          ),
          bslib::card_body(plotly::plotlyOutput(ns("data_coverage_plot"), height = "300px")),
          style = "display: none;",
          id = ns("card_5")
        ),

        # Observations table
        bslib::card(
          bslib::card_header("average repetition per sample and measure_type"),
          bslib::card_body(DT::DTOutput(ns("obs_table"))),
          style = "display: none;",
          id = ns("card_6")
        )
    ),
    shiny::br()
  )
}


#' mod_tab1 Server Function
#'
#' @description Server logic for the "Upload Observation" tab module.
#'
#' Handles:
#' - Dataset name validation,
#' - Template download and example data,
#' - File upload and site filtering,
#' - Rendering of map, summary tables, and plotly charts,
#' - Checkbox-based validation to proceed.
#'
#' @param id A string that serves as the module namespace identifier.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @import shiny shinyjs openxlsx dplyr tibble
#' @importFrom shiny moduleServer observeEvent observe reactive req showModal modalDialog updateSelectInput
#' @importFrom shinyjs addClass removeClass show runjs
#' @importFrom openxlsx loadWorkbook saveWorkbook readWorkbook
#' @importFrom dplyr tibble filter
mod_tab1_server <- function(id, ctx, parent_session) {
  
  moduleServer(id, function(input, output, session) {
    
    # =========================================================
    # STATE SYNC
    # =========================================================
    observeEvent(input$dataset_name, ctx$state$dataset_name <- input$dataset_name)
    observeEvent(input$version,      ctx$state$version      <- input$version)
    observeEvent(input$description,  ctx$state$description  <- input$description)
    observeEvent(input$embargo,      ctx$state$embargo      <- input$embargo)
    
    # =========================================================
    # SAFE NUMERIC
    # =========================================================
    safe_numeric <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      ifelse(is.na(x), NA_real_, x)
    }
    
    # =========================================================
    # VALIDATION (METADATA ONLY)
    # =========================================================
    validation <- reactive({
      
      dataset_name <- input$dataset_name %||% ""
      version      <- safe_numeric(input$version)
      description  <- input$description %||% ""
      
      name_valid <- nzchar(dataset_name) &&
        nchar(dataset_name) >= 3 &&
        nchar(dataset_name) <= 8 &&
        grepl("^[A-Z0-9]+$", dataset_name)
      
      version_valid <- !is.na(version) &&
        version >= 1 &&
        version <= 99
      
      description_valid <- nzchar(trimws(description)) &&
        nchar(trimws(description)) >= 50
      
      list(
        ok = isTRUE(name_valid && version_valid && description_valid),
        name_valid = name_valid,
        version_valid = version_valid,
        description_valid = description_valid
      )
    })
    
    # =========================================================
    # FILE PATH (SINGLE SOURCE OF TRUTH)
    # =========================================================
    obs_file_path <- reactive({
      req(input$obs_file)
      input$obs_file$datapath
    })
    
    # =========================================================
    # SITE INFO (FROM IO LAYER)
    # =========================================================
    site_info <- reactive({
      req(obs_file_path())
      extract_site_info(obs_file_path())
    })
    
    observe({
      req(site_info())
      updateSelectInput(
        session,
        "site_filter",
        choices = unique(site_info()$site_label)
      )
    })
    
    # =========================================================
    # OBS DATA (FROM IO LAYER)
    # =========================================================
    xylo_obs <- reactive({
      
      req(obs_file_path(), input$site_filter)
      
      df <- load_xylo_obs_clean(obs_file_path())
      
      df <- df[df$site_label == input$site_filter, , drop = FALSE]
      
      df
    })
    
    # =========================================================
    # HEADER COLOR UPDATE
    # =========================================================
    observe({
      if (validation()$ok) {
        shinyjs::removeClass("card_header1_1", "bg-danger")
        shinyjs::addClass("card_header1_1", "bg-success")
      } else {
        shinyjs::removeClass("card_header1_1", "bg-success")
        shinyjs::addClass("card_header1_1", "bg-danger")
      }
    })
    
    observe({
      if (isTRUE(input$validate_observation)) {
        shinyjs::removeClass("card_header1_4", "bg-danger")
        shinyjs::addClass("card_header1_4", "bg-success")
      } else {
        shinyjs::removeClass("card_header1_4", "bg-success")
        shinyjs::addClass("card_header1_4", "bg-danger")
      }
    })
    
    # =========================================================
    # SUBMIT BUTTON
    # =========================================================
    observeEvent(input$submit, {
      req(validation()$ok)
      shinyjs::show("card_1")
      shinyjs::show("card_2")
    })
    
    # =========================================================
    # NEXT BUTTON LOGIC
    # =========================================================
    can_proceed <- reactive({
      validation()$ok &&
        isTRUE(input$validate_location) &&
        isTRUE(input$validate_data_coverage) &&
        isTRUE(input$validate_observation)
    })
    
    observe({
      if (can_proceed()) {
        shinyjs::enable("next_btn")
      } else {
        shinyjs::disable("next_btn")
      }
    })
    
    observeEvent(input$next_btn, {
      req(can_proceed())
      
      bslib::nav_select(
        id = "tabs",
        selected = "tab2",
        session = parent_session
      )
    })
    
    
    # =========================================================
    # DOWNLOAD TEMPLATE
    # =========================================================
    output$download_template <- downloadHandler(
      filename = function() {
        req(input$dataset_name)
        paste0(input$dataset_name, "_xylo_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        
        template_path <- system.file(
          "extdata",
          "Datasetname_xylo_data_yyyy-mm-dd.xlsx",
          package = "xyloR"
        )
        
        file.copy(template_path, file, overwrite = TRUE)
      }
    )
    
    observeEvent(ctx$files$obs_file, {
      
      req(ctx$files$obs_file)
      
      message("📥 TAB1: file received")
      
      # =========================================================
      # STEP 2 FIX: prevent duplicate metadata regeneration
      # =========================================================
      
      if (!is.null(ctx$files$meta_template)) {
        message("⚠️ meta_template already exists → skipping regeneration")
        return()
      }
      
      template_path <- system.file(
        "extdata",
        "Datasetname_xylo_meta_yyyy-mm-dd.xlsx",
        package = "xyloR"
      )
      
      message("⚙️ Creating metadata...")
      
      result <- tryCatch({
        
        create_xylo_metadata(
          ctx$files$obs_file$datapath,
          template_path,
          destdir = ctx$files$temp_folder
        )
        
      }, error = function(e) {
        
        message("❌ Metadata creation FAILED: ", e$message)
        NULL
        
      })
      
      ctx$files$meta_template <- result
      
      message("✅ Metadata creation SUCCESS")
      message("📦 meta_template is NULL? ", is.null(ctx$files$meta_template))
      
    }, ignoreInit = TRUE)
    
    # =========================================================
    # DOWNLOAD EXAMPLE
    # =========================================================
    output$download_example_obs <- downloadHandler(
      filename = function() {
        "Example_xylo_data.xlsx"
      },
      content = function(file) {
        
        template_path <- system.file(
          "extdata",
          "Ltal2007_xylo_data_2025-09-01.xlsx",
          package = "xyloR"
        )
        
        file.copy(template_path, file, overwrite = TRUE)
      }
    )
    
    # =========================================================
    # OBS FILE UPLOADED STYLE
    # =========================================================
    observeEvent(input$obs_file, {
      
      req(input$obs_file)
      
      message("📥 TAB1 upload triggered")
      
      # =========================================================
      # STEP 1 FIX: avoid re-trigger loops
      # =========================================================
      
      previous <- ctx$files$obs_file
      
      is_same_file <- !is.null(previous) &&
        identical(previous$name, input$obs_file$name) &&
        identical(previous$size, input$obs_file$size)
      
      if (is_same_file) {
        message("⚠️ Same file detected → skipping ctx update")
        return()
      }
      
      # =========================================================
      # STORE IN CTX (SAFE)
      # =========================================================
      isolate({
        ctx$files$obs_file <- input$obs_file
      })
      
      message("📦 assigned to ctx")
      
      # UI updates
      shinyjs::removeClass("card_header1_3", "bg-danger")
      shinyjs::addClass("card_header1_3", "bg-success")
      
    }, ignoreInit = TRUE)
    
    # =========================================================
    # KEY INFO TABLE
    # =========================================================
    output$key_info_table <- DT::renderDataTable({
      
      df <- xylo_obs()
      req(nrow(df) > 0)
      
      si <- site_info() %>%
        dplyr::filter(site_label == input$site_filter)
      
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
      
      df <- xylo_obs()
      
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
    # MAP
    # =========================================================
    output$mymap <- leaflet::renderLeaflet({
      
      si <- site_info() %>%
        dplyr::filter(site_label == input$site_filter)
      
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(
          lng = as.numeric(si$longitude),
          lat = as.numeric(si$latitude),
          popup = si$site_label
        )
    })
    
    # =========================================================
    # PLOT
    # =========================================================
    output$data_coverage_plot <- plotly::renderPlotly({
      
      df <- xylo_obs()
      req(input$color)
      
      plotly::plot_ly(
        df,
        x = ~sample_date,
        y = ~tree_label,
        color = as.factor(df[[input$color]]),
        type = "scatter",
        mode = "markers",
        text = ~paste(
          "Tree:", tree_label,
          "<br>Date:", sample_date,
          "<br>", input$color, ":", df[[input$color]]
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          plot_bgcolor = "#2e2e2e",
          paper_bgcolor = "#2e2e2e",
          font = list(color = "white")
        )
    })
    
    # =========================================================
    # UI VISIBILITY
    # =========================================================
    observe({
      req(input$obs_file)
      
      shinyjs::show("card_3")
      shinyjs::show("card_4")
      shinyjs::show("card_5")
      shinyjs::show("card_6")
      shinyjs::show("card_7")
    })
    
  })
}



