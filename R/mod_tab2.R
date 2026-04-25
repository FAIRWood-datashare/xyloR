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
            id = ns("card_header2"),
            class = "card-header bg-danger",
            "Upload meta_data",
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
        ),

        # =========================
        # VALIDATION CARD — wrapped in plain div for reliable getElementById targeting
        # =========================
        shiny::div(
          id = ns("validation_card"),
          style = "display: none; margin-top: 10px;",
          bslib::card(
            bslib::card_header(
              "Validation Report",
              id = ns("card_header2_3"),
              class = "bg-danger"
            ),
            bslib::card_body(
              DT::DTOutput(ns("validation_table")),
              shiny::uiOutput(ns("validation_message")),
              shiny::actionButton(
                ns("next_btn"), "Continue →",
                class = "btn btn-primary w-100 mt-2",
                disabled = NA
              )
            )
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
    # ZIP SECTION — wrapped in plain div for reliable getElementById targeting
    # =========================================================
    shiny::fluidRow(
      shiny::column(
        12,
        
        shiny::div(
          id = ns("zip_card"),
          style = "display: none; text-align: center;",
          bslib::card(
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
mod_tab2_server <- function(id, ctx, session) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # DERIVED STATE
    # =====================================================
    meta_loaded <- reactive({
      is.list(ctx$data$meta) && !is.null(ctx$data$meta$site)
    })
    
    meta_hierarchy <- reactiveVal(NULL)
    
    # =====================================================
    # CONTRACT (PURE)
    # =====================================================
    contract_tab2 <- function(ctx) {
      obs      <- ctx$data$obs_truth
      meta     <- ctx$data$meta
      complete <- isTRUE(ctx$data$tab2_complete %||% FALSE)
      data_ok  <- is.data.frame(obs) && nrow(obs) > 0 &&
                  is.list(meta) && !is.null(meta$site)
      list(ready = data_ok && complete, data_ok = data_ok, complete = complete)
    }
    
    # =====================================================
    # 2.1 DOWNLOAD PREFILLED TEMPLATE
    # =====================================================
    output$download_meta_template <- shiny::downloadHandler(
      filename = function() {
        paste0(ctx$data$dataset_name %||% "Dataset", "_xylo_meta_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(is.data.frame(ctx$data$obs_truth))
        template_path <- system.file(
          "extdata", "Datasetname_xylo_meta_yyyy-mm-dd.xlsx", package = "xyloR"
        )
        shiny::withProgress(message = "Generating metadata template...", value = 0, {
          shiny::setProgress(0.3)
          # tbl1 may not be set yet — pass obs_truth only if tbl1 absent
          obs_info <- if (is.data.frame(ctx$data$tbl1)) ctx$data$tbl1 else NULL
          meta_wb  <- tryCatch(
            create_xylo_metadata(
              obs_data      = ctx$data$obs_truth,
              obs_info      = obs_info,
              template_meta = template_path
            ),
            error = function(e) {
              shiny::showNotification(paste("Template error:", e$message), type = "error")
              stop(e)
            }
          )
          shiny::setProgress(0.9)
          openxlsx::saveWorkbook(meta_wb, file, overwrite = TRUE)
        })
      }
    )
    
    # =====================================================
    # 2.1 DOWNLOAD EXAMPLE META
    # =====================================================
    output$download_example_meta <- shiny::downloadHandler(
      filename = function() "Example_xylo_meta.xlsx",
      content = function(file) {
        tp <- system.file("extdata", "Ltal2007_xylo_meta_2025-09-01.xlsx", package = "xyloR")
        if (!nzchar(tp) || !file.exists(tp)) {
          shiny::showNotification("Example meta file not found.", type = "error")
          return(NULL)
        }
        file.copy(tp, file, overwrite = TRUE)
      }
    )
    
    # =====================================================
    # 2.2 META UPLOAD
    # =====================================================
    observeEvent(input$meta_file, {
      req(input$meta_file)
      ctx$files$meta_file <- input$meta_file
      
      meta <- tryCatch(
        read_xylo_meta_raw(input$meta_file$datapath) |> build_xylo_meta_clean(),
        error = function(e) {
          message("❌ META LOAD FAILED: ", conditionMessage(e))
          shiny::showNotification(paste("Meta file error:", conditionMessage(e)),
                                  type = "error", duration = 8)
          NULL
        }
      )
      req(!is.null(meta))
      
      ctx$data$meta <- meta
      meta_hierarchy(meta)
      message("🟢 META LOADED")
      
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').classList.remove('bg-danger');
         document.getElementById('%s').classList.add('bg-success');",
        ns("card_header2"), ns("card_header2")
      ))
    })
    
    # =====================================================
    # VALIDATION ENGINE
    # =====================================================
    validation_tbl <- reactive({
      req(meta_loaded())
      req(is.data.frame(ctx$data$obs_truth))
      req(is.list(ctx$data$meta), !is.null(ctx$data$meta$site))
      
      result <- tryCatch(
        xylo_validation_engine(ctx$data$obs_truth, ctx$data$meta),
        error = function(e) {
          message("⚠️ validation engine error: ", e$message)
          list(all = data.frame(type = "error", message = e$message))
        }
      )
      tbl <- result$all %||% data.frame()
      if (!is.data.frame(tbl)) tbl <- data.frame()
      tbl
    })
    
    is_valid <- reactive({
      tbl <- tryCatch(validation_tbl(), error = function(e) NULL)
      is.data.frame(tbl) && nrow(tbl) == 0
    })
    
    # =====================================================
    # UI CONTROL — show validation card when meta loaded
    # =====================================================
    observe({
      req(meta_loaded())
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').style.display = 'block';",
        ns("validation_card")
      ))
    })
    
    # Header colour + zip visibility driven by data_ok (not complete)
    observe({
      ok  <- contract_tab2(ctx)$data_ok
      cls <- if (isTRUE(ok) && isTRUE(is_valid())) "bg-success" else "bg-danger"
      opp <- if (cls == "bg-success") "bg-danger" else "bg-success"
      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('%s').addClass('%s');",
        ns("card_header2_3"), opp, cls
      ))
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').style.display = '%s';",
        ns("zip_card"),
        if (isTRUE(ok) && isTRUE(is_valid())) "block" else "none"
      ))
    })
    
    # =====================================================
    # OUTPUTS
    # =====================================================
    output$validation_table <- DT::renderDT({
      req(meta_loaded())
      tbl <- validation_tbl()
      if (is.data.frame(tbl) && nrow(tbl) == 0) {
        return(DT::datatable(
          data.frame(Status = "\u2714 No issues found — validation passed"),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      DT::datatable(tbl, rownames = FALSE, class = "table-dark")
    })
    
    output$validation_message <- shiny::renderUI({
      req(meta_loaded())
      if (isTRUE(is_valid())) {
        htmltools::div(
          class = "alert alert-success p-3 rounded mt-2",
          shiny::tags$h4(shiny::icon("check-circle"), " Validation passed!", class = "mb-2"),
          shiny::tags$p("Click", shiny::tags$b("'Continue \u2192'"), "to proceed or download the ZIP below.", class = "mb-0")
        )
      } else {
        htmltools::div(
          class = "alert alert-danger p-3 rounded mt-2",
          shiny::tags$h4(shiny::icon("exclamation-triangle"), " Validation issues found", class = "mb-2"),
          shiny::tags$p("Fix the issues listed above, then re-upload your metadata file.", class = "mb-0")
        )
      }
    })
    
    # =====================================================
    # SUNBURST HIERARCHY PLOT
    # =====================================================
    df_hierarchy_reactive <- reactive({
      req(input$meta_file$datapath)
      
      meta_path   <- input$meta_file$datapath
      sheet_names <- setdiff(
        readxl::excel_sheets(meta_path),
        c("instructions", "DropList", "ListOfVariables")
      )
      sheet_data <- setNames(
        lapply(sheet_names, function(s) readxl::read_excel(meta_path, sheet = s)[-1:-6, ]),
        sheet_names
      )
      sheet_data[["sample"]]$sample_date <- as.Date(
        as.numeric(sheet_data[["sample"]]$sample_date), origin = "1899-12-30"
      )
      
      df_joined <- dplyr::left_join(
        sheet_data[["sample"]], sheet_data[["tree"]], by = "tree_label",
        relationship = "many-to-many"
      ) |>
        dplyr::left_join(sheet_data[["site"]], by = "site_label",
                         relationship = "many-to-many") |>
        dplyr::mutate(
          plot_label_clean = dplyr::coalesce(plot_label.x, plot_label.y),
          year             = lubridate::year(sample_date),
          network_label    = dplyr::coalesce(network_label, site_label),
          site_label_full  = dplyr::if_else(
            site_label == network_label | is.na(site_label),
            paste0(network_label, "_site"),
            paste0(network_label, "__", site_label)
          ),
          plot_label_full  = dplyr::if_else(
            is.na(plot_label_clean) | plot_label_clean == site_label,
            paste0(site_label_full, "_plot"),
            paste0(site_label_full, "__", plot_label_clean)
          ),
          tree_label_full  = paste0(plot_label_full, "__", tree_label),
          year_label       = paste0(tree_label_full, "__", year),
          sample_label_full = paste0(year_label, "__", sample_id)
        )
      
      df_tree    <- dplyr::count(df_joined, tree_label_full, plot_label_full, name = "value") |>
        dplyr::rename(id = tree_label_full, parent = plot_label_full)
      df_plot    <- dplyr::distinct(df_joined, plot_label_full, site_label_full) |>
        dplyr::count(plot_label_full, site_label_full, name = "value") |>
        dplyr::rename(id = plot_label_full, parent = site_label_full)
      df_site    <- dplyr::distinct(df_joined, site_label_full, network_label) |>
        dplyr::count(site_label_full, network_label, name = "value") |>
        dplyr::rename(id = site_label_full, parent = network_label)
      df_network <- dplyr::distinct(df_joined, network_label) |>
        dplyr::mutate(id = network_label, parent = "", value = 1L)
      
      dplyr::bind_rows(df_network, df_site, df_plot, df_tree) |>
        dplyr::distinct(id, parent, value) |>
        dplyr::arrange(parent, id) |>
        dplyr::mutate(
          label = sub(".*__", "", id),
          text  = paste0(label, " (", value, ")")
        )
    })
    
    output$hierarchical_structure <- plotly::renderPlotly({
      df <- df_hierarchy_reactive()
      plotly::plot_ly(
        data    = df,
        ids     = ~id,
        labels  = ~text,
        parents = ~parent,
        values  = ~value,
        type    = "sunburst",
        source  = "sunburst_selection"
      ) |>
        plotly::layout(paper_bgcolor = "#1E1E1E")
    })
    
    # =====================================================
    # META TABLE
    # =====================================================
    output$meta_table <- DT::renderDataTable({
      req(input$meta_file$datapath)
      
      meta_path   <- input$meta_file$datapath
      sheet_names <- setdiff(readxl::excel_sheets(meta_path),
                             c("instructions", "DropList", "ListOfVariables"))
      sheet_data  <- setNames(
        lapply(sheet_names, function(s) readxl::read_excel(meta_path, sheet = s)[-1:-6, ]),
        sheet_names
      )
      
      df_joined <- dplyr::left_join(sheet_data[["sample"]], sheet_data[["tree"]],
                                    by = "tree_label") |>
        dplyr::left_join(sheet_data[["site"]], by = c("site_label", "plot_label")) |>
        dplyr::group_by(
          network_label, site_label, plot_label, tree_label,
          year      = lubridate::year(as.Date(as.numeric(sample_date), origin = "1899-12-30")),
          sample_id
        ) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop")
      
      selection <- plotly::event_data("plotly_click", source = "sunburst_selection")
      if (!is.null(selection)) {
        df_h    <- df_hierarchy_reactive()
        sel_row <- df_h[selection$pointNumber + 1, ]
        parts   <- strsplit(as.character(sel_row$id), "__")[[1]]
        site_s  <- parts[2]; plot_s <- parts[3]; tree_s <- parts[4]
        df_joined <- dplyr::filter(
          df_joined,
          (is.na(site_s) | site_label == site_s),
          (is.na(plot_s) | plot_label == plot_s),
          (is.na(tree_s) | tree_label == tree_s)
        )
      }
      
      DT::datatable(
        df_joined,
        rownames = FALSE, filter = "top", class = "table-dark",
        options  = list(paging = TRUE, searching = TRUE, autoWidth = TRUE,
                        scrollY = TRUE, dom = "Blfrtip",
                        columnDefs = list(list(className = "dt-center", targets = "_all")))
      )
    })
    
    # =====================================================
    # ZIP DOWNLOAD
    # =====================================================
    output$download_zip <- shiny::downloadHandler(
      filename = function() {
        parts <- Filter(nzchar, c(
          ctx$data$contact_lastname %||% "",
          ctx$data$dataset_name     %||% "exchange",
          as.character(ctx$data$version %||% 1)
        ))
        paste0(paste(parts, collapse = "_"), "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(is_valid())
        req(!is.null(ctx$files$obs_file), !is.null(ctx$files$meta_file))
        obs_path  <- ctx$files$obs_file$datapath
        meta_path <- ctx$files$meta_file$datapath
        shiny::withProgress(message = "Preparing exchange files...", value = 0, {
          shiny::setProgress(0.1)
          out_dir <- file.path(tempdir(), paste0("exchange_", format(Sys.time(), "%Y%m%d%H%M%S")))
          dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
          on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
          shiny::setProgress(0.3)
          tryCatch(
            create_exchange_files(obs_file = obs_path, meta_file = meta_path, output_dir = out_dir),
            error = function(e) {
              shiny::showNotification(paste("Error:", e$message), type = "error")
              stop(e)
            }
          )
          shiny::setProgress(0.7)
          files_to_zip <- list.files(out_dir, full.names = TRUE, recursive = TRUE)
          extra <- c(obs_path, meta_path)
          extra <- extra[file.exists(extra)]
          zip::zipr(zipfile = file, files = c(files_to_zip, extra))
          shiny::setProgress(1)
        })
      },
      contentType = "application/zip"
    )
    
    # =====================================================
    # NEXT BUTTON (TAB2 → TAB3 via FSM)
    # =====================================================
    observe({
      shinyjs::toggleState("next_btn", condition = contract_tab2(ctx)$data_ok)
    })
    
    observeEvent(input$next_btn, {
      req(contract_tab2(ctx)$data_ok)
      ctx$data$tab2_complete <- TRUE
      message("🔥 tab2 complete — FSM will advance")
    })
    
    # =====================================================
    # RETURN
    # =====================================================
    return(list(
      validation = validation_tbl,
      hierarchy  = meta_hierarchy
    ))
  })
}




