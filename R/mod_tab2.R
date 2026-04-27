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
  
  bslib::nav_panel(
    title = "Metadata",
    value = "tab2",
    
    shiny::fluidRow(
      
      # =====================================================
      # LEFT PANEL
      # =====================================================
      shiny::column(
        3,
        class = "bg-light p-2 border-end",
        
        bslib::card(
          bslib::card_header("2.1 Template"),
          bslib::card_body(
            shiny::downloadButton(ns("download_meta_template"), "Template"),
            shiny::downloadButton(ns("download_example_meta"), "Example")
          )
        ),
        
        bslib::card(
          bslib::card_header("2.2 Upload"),
          bslib::card_body(
            shiny::fileInput(ns("meta_file"), NULL, accept = ".xlsx"),
            shiny::textOutput(ns("meta_status"))
          )
        ),
        
        # =====================================================
        # VALIDATION + EXPORT (CONTROL ROOM)
        # =====================================================
        shiny::div(
          id = ns("validation_card"),
          style = "display:none;",
          
          bslib::card(
            bslib::card_header("Validation & Export"),
            bslib::card_body(
              
              DT::DTOutput(ns("validation_table")),
              shiny::uiOutput(ns("validation_message")),
              
              shiny::br(),
              shiny::uiOutput(ns("export_button")),
              
              shiny::br(),
              
              # 🔥 NEW BLOCKER PANEL
              shiny::uiOutput(ns("blocker_panel")),
              
              shiny::br(),
              
              shiny::actionButton(ns("next_btn"), "Continue →")
            )
          )
        )
      ),
      
      # =====================================================
      # RIGHT PANEL
      # =====================================================
      shiny::column(
        9,
        bslib::card(
          bslib::card_header("Structure"),
          plotly::plotlyOutput(ns("hierarchy"))
        ),
        
        bslib::card(
          bslib::card_header("Metadata"),
          DT::DTOutput(ns("meta_table"))
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
mod_tab2_server <- make_tab_module(
  tab_id = "tab2",
  
  # =====================================================
  # INIT (META INGESTION)
  # =====================================================
  init_fn = function(input, ctx) {
    
    observeEvent(input$meta_file, {
      
      meta <- tryCatch(
        read_xylo_meta_raw(input$meta_file$datapath) |>
          build_xylo_meta_clean(),
        error = function(e) {
          shiny::showNotification(e$message, type = "error")
          NULL
        }
      )
      
      req(!is.null(meta))
      
      ctx$data$meta <- meta
      
      # GLOBAL VALIDATION
      ctx <<- validate_cross_tab(ctx)
      ctx <<- compute_export_state(ctx)
    })
  },
  
  # =====================================================
  # VIEW
  # =====================================================
  view_fn = function(ctx) {
    
    validation_tbl <- reactive({
      req(ctx$data$obs_truth, ctx$data$meta)
      
      tryCatch(
        xylo_validation_engine(ctx$data$obs_truth, ctx$data$meta),
        error = function(e) {
          data.frame(type = "error", message = e$message)
        }
      )
    })
    
    is_valid <- reactive({
      df <- validation_tbl()
      is.data.frame(df) && nrow(df) == 0
    })
    
    export_ready <- reactive({
      isTRUE(ctx$state$export_ready)
    })
    
    focus_field <- reactive({
      ctx$fsm$focus_field
    })
    
    list(
      validation_tbl = validation_tbl,
      is_valid = is_valid,
      export_ready = export_ready,
      focus_field = focus_field
    )
  },
  
  # =====================================================
  # ACTION (EXPORT)
  # =====================================================
  action_fn = function(input, output, ctx, view) {
    
    observeEvent(input$export, {
      
      req(ctx$state$export_ready)
      
      zipfile <- tempfile(fileext = ".zip")
      
      saveRDS(ctx$data$obs_truth, "obs_clean.rds")
      saveRDS(ctx$data$meta, "meta.rds")
      
      zip(zipfile, c("obs_clean.rds", "meta.rds"))
      
      shiny::showNotification("Export complete", type = "message")
    })
  },
  
  # =====================================================
  # SIGNAL
  # =====================================================
  signal_fn = function(ctx, view) {
    isTRUE(view$is_valid())
  },
  
  # =====================================================
  # UI OUTPUTS
  # =====================================================
  ui_fn = function(output, input, ctx, ns, view) {
    
    # =====================================================
    # VALIDATION TABLE
    # =====================================================
    output$validation_table <- DT::renderDT({
      
      df <- view$validation_tbl()
      
      if (nrow(df) == 0) {
        return(DT::datatable(data.frame(Status = "✔ Valid")))
      }
      
      DT::datatable(df)
    })
    
    # =====================================================
    # MESSAGE
    # =====================================================
    output$validation_message <- shiny::renderUI({
      
      if (isTRUE(view$is_valid())) {
        shiny::tags$div(class = "alert alert-success", "Validation passed")
      } else {
        shiny::tags$div(class = "alert alert-danger", "Fix validation errors")
      }
    })
    
    # =====================================================
    # BLOCKER PANEL (SAFE REACTIVE ACCESS)
    # =====================================================
    output$blocker_panel <- shiny::renderUI({
      
      val <- ctx$validation$global
      if (is.null(val)) return(NULL)
      
      issues <- c(val$blockers, val$errors)
      if (length(issues) == 0) return(NULL)
      
      shiny::tags$div(
        class = "alert alert-danger",
        
        shiny::tags$strong("❌ Validation issues (click to navigate)"),
        shiny::br(), shiny::br(),
        
        lapply(seq_along(issues), function(i) {
          
          issue <- issues[[i]]
          
          shiny::tags$div(
            style = "
            margin-bottom:10px;
            padding:8px;
            border:1px solid #ddd;
            border-radius:6px;
            cursor:pointer;
            background:#fff;
          ",
            
            onclick = sprintf(
              "Shiny.setInputValue('%s', %d, {priority: 'event'})",
              ns("jump_issue"), i
            ),
            
            shiny::tags$strong(issue$message),
            shiny::br(),
            
            if (!is.null(issue$tab)) {
              shiny::tags$div(paste0("Tab: ", issue$tab), style="color:#666")
            },
            
            if (!is.null(issue$field)) {
              shiny::tags$div(paste0("Field: ", issue$field), style="color:#666")
            },
            
            if (!is.null(issue$hint)) {
              shiny::tags$div(shiny::tags$em(paste0("💡 ", issue$hint)))
            }
          )
        })
      )
    })
    
    # =====================================================
    # CLICK HANDLER
    # =====================================================
    observeEvent(input$jump_issue, {
      
      req(ctx$validation$global)
      
      issues <- c(
        ctx$validation$global$blockers,
        ctx$validation$global$errors
      )
      
      issue <- issues[[input$jump_issue]]
      req(!is.null(issue))
      
      ctx$fsm$focus_tab  <- issue$tab
      ctx$fsm$focus_field <- issue$field
      ctx$fsm$focus_row   <- ifelse(is.null(issue$row), NA, as.integer(issue$row))
      
      ctx$fsm$state <- issue$tab
      
      shiny::showNotification(
        paste("Navigating to", issue$tab),
        type = "message"
      )
    })
    
    # =====================================================
    # EXPORT BUTTON
    # =====================================================
    output$export_button <- shiny::renderUI({
      
      if (isTRUE(view$export_ready())) {
        shiny::actionButton(ns("export"), "Export ZIP", class = "btn-success")
      } else {
        shiny::tags$button(
          "Export locked",
          class = "btn btn-secondary",
          disabled = NA
        )
      }
    })
    
    # =====================================================
    # META TABLE (FIXED + SAFE HIGHLIGHTING)
    # =====================================================
    output$meta_table <- DT::renderDT({
      
      df <- ctx$data$meta
      
      if (is.null(df) || !is.data.frame(df)) {
        return(DT::datatable(data.frame(Status = "Meta not loaded or invalid")))
      }
      
      val <- ctx$validation$global
      if (is.null(val)) val <- list(errors=list(), warnings=list(), blockers=list())
      
      focus_row <- ctx$fsm$focus_row
      focus_col <- ctx$fsm$focus_field
      
      dt <- DT::datatable(
        df,
        options = list(pageLength = 10),
        selection = "none"
      )
      
      # =====================================================
      # ROW HIGHLIGHT (FOCUS ONLY - SIMPLE + RELIABLE)
      # =====================================================
      if (!is.null(focus_row) && !is.na(focus_row)) {
        dt <- DT::formatStyle(
          dt,
          columns = names(df),
          target = "row",
          valueColumns = NULL,
          backgroundColor = DT::styleEqual(focus_row, "#fff3cd")
        )
      }
      
      # =====================================================
      # FIELD HIGHLIGHT (SAFE)
      # =====================================================
      if (!is.null(focus_col) && focus_col %in% names(df)) {
        dt <- DT::formatStyle(
          dt,
          columns = focus_col,
          backgroundColor = "#ffe8a1",
          fontWeight = "bold"
        )
      }
      
      dt
    })
    
    # =====================================================
    # SCROLL TO ROW
    # =====================================================
    observe({
      
      req(ctx$fsm$focus_row)
      
      shinyjs::runjs(sprintf("
      setTimeout(function(){
        var table = document.getElementById('%s');
        if (!table) return;

        var row = table.querySelector('tbody tr:nth-child(%d)');
        if (row) {
          row.scrollIntoView({behavior: 'smooth', block: 'center'});
        }
      }, 150);
    ", ns("meta_table"), ctx$fsm$focus_row))
    })
    
    # =====================================================
    # EDIT LOCK GUARD
    # =====================================================
    observeEvent(input$meta_table_cell_edit, {
      
      if (isTRUE(ctx$edit$lock)) return()
      ctx$edit$lock <- TRUE
      on.exit(ctx$edit$lock <- FALSE, add = TRUE)
      
      info <- input$meta_table_cell_edit
      
      df <- ctx$data$meta
      req(df)
      
      i <- info$row
      j <- info$col
      v <- info$value
      
      df[i, j] <- DT::coerceValue(v, df[i, j])
      
      ctx$data$meta <- df
      
      ctx <<- validate_cross_tab(ctx)
      ctx <<- compute_export_state(ctx)
      
      shiny::showNotification("Cell updated & revalidated", type = "message")
    })
  }
)