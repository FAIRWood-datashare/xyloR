#' mod_tab2 UI Function
#'
#' @description A shiny module for the "Metadata Management" tab.
#'
#' This module provides a user interface for managing metadata files and validating them:
#' 1. Uploading and validating metadata files,
#' 2. Downloading metadata templates or example data,
#' 3. Visualizing hierarchical metadata structures using a sunburst plot,
#' 4. Displaying validation messages and feedback.
#'
#' @param id A string that serves as the module namespace identifier.
#'
#' @return A `shiny.tag.list` containing the UI elements of the module.
#' @export
#'
#' @import shiny shinyjs plotly openxlsx
#' @importFrom shiny NS tagList fluidRow column div actionButton fileInput uiOutput
#' @importFrom plotly plotlyOutput
#' @importFrom openxlsx loadWorkbook saveWorkbook
mod_tab2_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    shiny::fluidRow(
      
      # =========================================================
      # LEFT PANEL
      # =========================================================
      shiny::column(
        3, class = "bg-light p-2 border-end", style = "height: 100%;",
        
        bslib::card(
          bslib::card_header(
            "2.1 Download prefilled metadata template",
            id = ns("card_header2_1"),
            class = "bg-warning",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Download a prefilled Excel template.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::downloadButton(
                  ns("download_meta_template"),
                  "Download Metadata Template",
                  class = "btn btn-primary"
                )
              ),
              shiny::column(
                6,
                shiny::downloadButton(
                  ns("download_example_meta"),
                  "Download filled example",
                  class = "btn btn-secondary"
                )
              )
            )
          )
        ),
        
        bslib::card(
          bslib::card_header(
            "2.2 Load completed metadata for validation",
            id = ns("card_header2_2"),
            class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Upload your completed metadata Excel file.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::fileInput(ns("meta_file"), label = NULL, accept = c(".xlsx")),
            shiny::textOutput(ns("meta_validation_status")),
            shiny::verbatimTextOutput(ns("meta_validation_errors"))
          )
        )
      ),
      
      # =========================================================
      # RIGHT PANEL
      # =========================================================
      shiny::column(
        9,
        bslib::card(
          bslib::card_header("Overview of data structure"),
          bslib::card_body(
            plotly::plotlyOutput(ns("hierarchical_structure"), height = "500px")
          ),
          bslib::card_body(
            DT::DTOutput(ns("meta_table"))
          )
        )
      )
    ),
    
    # =========================================================
    # VALIDATION + ZIP SECTION
    # =========================================================
    shiny::fluidRow(
      shiny::column(
        12,
        
        bslib::card(
          id = ns("validation_card"),
          style = "display: none;",
          bslib::card_header(
            "Validation Report",
            id = ns("card_header2_3"),
            class = "bg-danger"
          ),
          bslib::card_body(
            DT::DTOutput(ns("validation_table")),
            shiny::uiOutput(ns("validation_message"))
          )
        ),
        
        br(),
        
        bslib::card(
          id = ns("zip_card"),
          style = "display: none; text-align: center;",
          bslib::card_body(
            shiny::downloadButton(
              ns("download_zip"),
              "2.3 Download Exchange Files as ZIP",
              class = "btn btn-primary"
            )
          )
        )
      )
    )
  )
}

#' mod_tab2 Server Function
#'
#' @description Server logic for the "Metadata Management" tab module.
#'
#' Handles:
#' - Uploading and validating metadata files,
#' - Providing metadata template download functionality,
#' - Rendering a sunburst plot to visualize hierarchical metadata structure,
#' - Displaying validation results and feedback messages based on metadata quality.
#'
#' @param id A string that serves as the module namespace identifier.
#' @param out_tab1 A reactive object containing the dataset name and observation file.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @import shiny
#' @importFrom shinyjs addClass removeClass show runjs toggleClass
#' @importFrom openxlsx loadWorkbook saveWorkbook readWorkbook
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter mutate select arrange group_by summarise distinct rename
#' @importFrom bsicons bs_icon
#' @importFrom htmltools div
#' @importFrom zip zipr
#' @importFrom DT renderDataTable datatable
#' @importFrom readxl read_excel excel_sheets
#' @importFrom lubridate year
#' @importFrom tibble tibble
#' 
mod_tab2_server <- function(id, ctx, meta_template_r) {
  
  moduleServer(id, function(input, output, session) {
    
    # =========================================================
    # STATE MACHINE
    # =========================================================
    tab2_state <- reactiveVal("empty")
    validation_results <- reactiveVal(NULL)
    
    # =========================================================
    # SAFETY: ensure temp folder exists
    # =========================================================
    observe({
      if (is.null(ctx$files$temp_folder) || !dir.exists(ctx$files$temp_folder)) {
        ctx$files$temp_folder <- tempdir()
        message("📁 temp_folder set to: ", ctx$files$temp_folder)
      }
    })
    
    # =========================================================
    # DEBUG
    # =========================================================
    observe({
      message("📍 TAB2 CTX CHECK")
      
      if (is.null(ctx$files$obs_file)) {
        message("❌ obs_file is NULL in TAB2")
      } else {
        message("✅ obs_file EXISTS in TAB2")
      }
    })
    
    # =========================================================
    # FILE PATHS
    # =========================================================
    obs_file_path <- reactive({
      req(ctx$files$obs_file)
      ctx$files$obs_file$datapath
    })
    
    meta_file_path <- reactive({
      req(input$meta_file)
      input$meta_file$datapath
    })
    
    # =========================================================
    # TEMPLATE DOWNLOAD
    # =========================================================
    output$download_meta_template <- downloadHandler(
      filename = function() {
        req(ctx$state$dataset_name)
        paste0(ctx$state$dataset_name, "_xylo_meta_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        
        message("=== DOWNLOAD TRIGGERED ===")
        message("CTX ID: ", ctx$.id)
        
        wb <- meta_template_r()
        req(wb)
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # =========================================================
    # CREATE TEMPLATE
    # =========================================================
    observeEvent(ctx$files$obs_file, {
      
      req(ctx$files$obs_file)
      
      # prevent loop
      if (!is.null(ctx$files$meta_template)) {
        message("⚠️ meta_template already exists → skipping")
        return()
      }
      
      message("⚙️ Creating metadata template...")
      
      template_path <- system.file(
        "extdata",
        "Datasetname_xylo_meta_yyyy-mm-dd.xlsx",
        package = "xyloR"
      )
      
      ctx$files$meta_template <- tryCatch({
        create_xylo_metadata(
          ctx$files$obs_file$datapath,
          template_path,
          destdir = ctx$files$temp_folder
        )
      }, error = function(e) {
        message("❌ meta load failed: ", e$message)
        
        tab2_state("invalid")
        
        # ✅ ADD THIS
        shinyjs::removeClass("card_header2_2", "bg-success")
        shinyjs::addClass("card_header2_2", "bg-danger")
        
        return(NULL)
      })
    }, ignoreInit = TRUE)
    
    # =========================================================
    # VALIDATION
    # =========================================================
    observeEvent(input$meta_file, {
      
      req(input$meta_file)
      
      message("📥 TAB2: metadata uploaded")
      
      tab2_state("meta_loaded")
      
      # ✅ ADD THIS (2.2 COLOR FIX)
      shinyjs::removeClass("card_header2_2", "bg-danger")
      shinyjs::addClass("card_header2_2", "bg-success")
      
      obs <- if (!is.null(ctx$files$obs_file)) {
        load_xylo_obs_clean(ctx$files$obs_file$datapath)
      } else NULL
      
      meta_raw <- tryCatch({
        load_xylo_metadata_clean(meta_file_path())
      }, error = function(e) {
        message("❌ meta load failed: ", e$message)
        tab2_state("invalid")
        return(NULL)
      })
      
      if (is.null(meta_raw)) return()
      
      tbl <- tryCatch({
        
        obs_val <- tryCatch({
          xylo_format_validation(obs_file_path())
        }, error = function(e) {
          message("❌ obs validation failed: ", e$message)
          data.frame(issue = "Observation validation failed")
        })
        
        meta_val <- tryCatch({
          meta_format_validation(meta_file_path())
        }, error = function(e) {
          
          full_msg <- paste(capture.output(print(e)), collapse = " | ")
          
          message("❌ FULL meta validation error: ", full_msg)
          
          data.frame(
            issue = "Metadata validation crashed",
            detail = full_msg
          )
        })
        
        # ensure both are data.frames
        if (is.null(obs_val))  obs_val  <- data.frame()
        if (is.null(meta_val)) meta_val <- data.frame()
        
        rbind(obs_val, meta_val)
        
      }, error = function(e) {
        message("❌ rbind failed: ", e$message)
        data.frame(issue = "Validation failed (internal error)")
      })
      
      validation_results(tbl)
      
      # =====================================================
      # STATE DECISION
      # =====================================================
      if (is.null(tbl) || nrow(tbl) == 0) {
        tab2_state("valid")
      } else {
        tab2_state("invalid")
      }
    })
    
    # =========================================================
    # UI CONTROLLER (FIXED & COMPLETE)
    # =========================================================
    observe({
      
      state <- tab2_state()
      message("📊 TAB2 STATE: ", state)
      
      # =====================================================
      # ZIP BUTTON (ONLY VALID STATE)
      # =====================================================
      if (identical(state, "valid")) {
        shinyjs::show("download_zip")
      } else {
        shinyjs::hide("download_zip")
      }
      
      # =====================================================
      # CARD VISIBILITY (ALWAYS SHOW AFTER ANY ACTION)
      # =====================================================
      if (state == "empty") {
        
        shinyjs::hide("zip_card")
        
      } else {
        
        if (identical(state, "valid")) {
          shinyjs::show("zip_card")
        } else {
          shinyjs::hide("zip_card")
        }
      }
      
      # =====================================================
      # HEADER COLOR (safe version)
      # =====================================================
      if (state == "valid") {
        
        shinyjs::addClass("card_header2_3", "bg-success")
        shinyjs::removeClass("card_header2_3", "bg-danger")
        
      } else if (state == "invalid") {
        
        shinyjs::addClass("card_header2_3", "bg-danger")
        shinyjs::removeClass("card_header2_3", "bg-success")
        
      } else {
        
        shinyjs::removeClass("card_header2_3", "bg-success")
        shinyjs::removeClass("card_header2_3", "bg-danger")
      }
    })
    
    # =========================================================
    # VALIDATION TABLE
    # =========================================================
    output$validation_table <- DT::renderDataTable({
      req(validation_results(), nrow(validation_results()) > 0)
      
      DT::datatable(
        validation_results(),
        options = list(
          paging = FALSE,
          searching = FALSE,
          autoWidth = TRUE,
          dom = 'Blfrtip',
          scrollX = FALSE,
          scrollY = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          ),
          rowCallback = DT::JS("
        function(row, data, index) {
          $('td', row).css('color', '#E74C3C');
        }
      ")
        ),
        filter = "none",
        class = "table-dark"
      )
    })
    
    # =========================================================
    # VALIDATION MESSAGE
    # =========================================================
    output$validation_message <- renderUI({
      
      tbl <- validation_results()
      state <- tab2_state()
      
      # ❌ NOTHING YET
      if (state == "empty") {
        return(NULL)
      }
      
      # ✅ SUCCESS
      if (!is.null(tbl) && nrow(tbl) == 0) {
        
        shinyjs::addClass("card_header2_3", "bg-success")
        shinyjs::removeClass("card_header2_3", "bg-danger")
        shinyjs::show("zip_card")
        
        return(
          htmltools::div(
            class = "alert alert-success p-3 rounded",
            shiny::tags$h4(
              shiny::icon("check-circle"),
              " Success!",
              class = "mb-2"
            ),
            shiny::tags$p(
              "Congratulations! Your files are ready to be submitted.",
              class = "mb-2"
            ),
            shiny::tags$p(
              "You can now click on the button ",
              shiny::tags$b("'Download exchange files as ZIP'!"),
              class = "mb-0"
            )
          )
        )
      }
      
      # ⚠️ ERRORS
      if (!is.null(tbl) && nrow(tbl) > 0) {
        
        shinyjs::addClass("card_header2_3", "bg-danger")
        shinyjs::removeClass("card_header2_3", "bg-success")
        shinyjs::show("zip_card")
        
        return(
          htmltools::div(
            class = "alert alert-danger p-3 rounded",
            shiny::tags$h4(
              shiny::icon("exclamation-triangle"),
              " Validation Issues Found!",
              class = "mb-2"
            ),
            shiny::tags$p(
              "Please review the validation issues listed above, before proceeding.",
              class = "mb-3"
            )
          )
        )
      }
      
      # ⏳ LOADING STATE
      if (state == "meta_loaded") {
        
        shinyjs::show("zip_card")
        
        return(
          htmltools::div(
            class = "alert alert-info p-3 rounded",
            shiny::tags$h4("Processing..."),
            shiny::tags$p("Running validation...")
          )
        )
      }
    })
    
    # =========================================================
    # HIERARCHY
    # =========================================================
    df_hierarchy_reactive <- reactive({
      req(meta_file_path())
      meta_raw <- load_xylo_metadata_clean(meta_file_path())
      build_xylo_hierarchy(meta_raw)
    })
    
    output$hierarchical_structure <- renderPlotly({
      
      df <- df_hierarchy_reactive()
      
      plotly::plot_ly(
        data = df,
        ids = ~id,
        labels = ~text,
        parents = ~parent,
        values = ~value,
        type = "sunburst"
      ) %>%
        layout(
          paper_bgcolor = "#1E1E1E",
          plot_bgcolor  = "#1E1E1E",
          font = list(color = "white")
        )
    })
    
    # =========================================================
    # META TABLE
    # =========================================================
    output$meta_table <- DT::renderDataTable({
      
      meta_raw <- load_xylo_metadata_clean(meta_file_path())
      
      df_joined <- dplyr::left_join(
        meta_raw[["sample"]],
        meta_raw[["tree"]],
        by = "tree_label"
      ) |>
        dplyr::left_join(meta_raw[["site"]], by = "site_label")
      
      DT::datatable(df_joined, filter = "top", class = "table-dark")
    })
    
    # =========================================================
    # ZIP DOWNLOAD (FIXED SAFETY)
    # =========================================================
    output$download_zip <- downloadHandler(
      filename = function() {
        req(ctx$state$dataset_name)
        paste0(ctx$state$dataset_name, "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        
        obs_file <- obs_file_path()
        meta_file <- meta_file_path()
        
        req(file.exists(obs_file), file.exists(meta_file))
        
        if (is.null(ctx$files$temp_folder) || !dir.exists(ctx$files$temp_folder)) {
          stop("temp_folder is missing or invalid")
        }
        
        to_exchange_files(
          obs_file,
          meta_file,
          dir = ctx$files$temp_folder,
          dataset_name = ctx$state$dataset_name,
          version = ctx$state$version,
          embargo = ctx$state$embargo,
          description = ctx$state$description
        )
        
        files <- list.files(ctx$files$temp_folder, full.names = TRUE)
        zip::zipr(file, files)
      }
    )
    
  })
}


