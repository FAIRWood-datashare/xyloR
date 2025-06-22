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
  
  # TAB 2: Upload Metadata -----------------------------------------------
  bslib::nav_panel(
    title = htmltools::div(id = ns("upload_metadata"), "2. Upload metadata"),
    value = "upload_metadata",
    
    shiny::fluidRow(
      # Sidebar (left panel)
      shiny::column(
        3, class = "bg-light p-2 border-end", style = "height: 100%;",
        
        # 2.1 Download Metadata Template
        bslib::card(
          bslib::card_header(
            "2.1 Download prefilled metadata template",
            id    = ns("card_header2_1"), class = "bg-warning",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Click 'Download Metadata Template' to save a prefilled Excel file with metadata based on your observations. Click 'Download filled example' to view a filled-in example. Then continue to 2.2 to upload your completed file.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::fluidRow(
              shiny::column(6, shiny::downloadButton(ns("download_meta_template"), "Download Metadata Template", class = "btn btn-primary")),
              shiny::column(6, shiny::downloadButton(ns("download_example_meta"),   "Download filled example",     class = "btn btn-secondary"))
            )
          ),
          id    = ns("card_2_1"),
          style = ""
        ),
        
        # 2.2 Load completed metadata for validation
        bslib::card(
          bslib::card_header(
            "2.2 Load completed metadata for validation",
            id    = ns("card_header2_2"), class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Upload your filled metadata Excel file. A sunburst plot and hierarchical table will appear. Fix any validation issues shown in red, then re-upload until all issues are resolved and the 'Download Exchange Files' button activates.",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::fileInput(ns("meta_file"), label = NULL, accept = c(".xlsx")),
            shiny::textOutput(ns("meta_validation_status")),
            shiny::verbatimTextOutput(ns("meta_validation_errors"))
          ),
          id    = ns("card_2_2"),
          style = ""
        ),
        
        # Validation report
        bslib::card(
          id    = ns("card_8"),
          style = "display: none; height: fit-content; overflow: hidden;",
          bslib::card_header(
            "Report of validation check!",
            id    = ns("card_header2_3"), class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Green = All good! You can proceed. Red = Problems to fix in your metadata file. Return to 2.2, correct the file, and re-upload until all rows are green.",
              placement = "right"
            )
          ),
          bslib::card_body(
            DT::DTOutput(ns("validation_table")),
            shiny::uiOutput(ns("validation_message")),
            style = "min-height: 0; padding: 10px;"
          )
        )
      ),
      
      # Main content (right panel)
      shiny::column(
        9, style = "height: 100%;",
        bslib::card(
          bslib::card_header("Overview of data structure"),
          bslib::card_body(
            plotly::plotlyOutput(ns("hierarchical_structure"), height = "500px")
          ),
          bslib::card_body(
            DT::DTOutput(ns("meta_table"))
          ),
          id    = ns("card_2_main"),
          style = ""
        )
      )
    ),
    br(),
    
    # Download ZIP section (below full width)
    shiny::fluidRow(
      shiny::column(
        12,
        bslib::card(
          id    = ns("card_9"),
          class = "border border-0 text-center",
          style = "display: none;",
          shiny::downloadButton(ns("download_zip"), "2.3 Download Exchange Files as ZIP", class = "btn btn-primary"),
          shiny::uiOutput(ns("modal_ui"))
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
mod_tab2_server <- function(id, out_tab1) {
  moduleServer(id, function(input, output, session) {

    output$download_meta_template <- shiny::downloadHandler(
      filename = function() {
        paste0(out_tab1$dataset_name(), "_xylo_meta_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        shiny::withProgress(message = 'Processing metadata...', value = 0, {
          shiny::setProgress(value = 0.2, detail = "Loading the template...")
          
          template_path <- system.file("extdata", "Datasetname_xylo_meta_yyyy-mm-dd.xlsx", package = "xyloR")
          
          shiny::setProgress(value = 0.5, detail = "Prefilling the template...")
          meta_template <- create_xylo_metadata(out_tab1$obs_file()$datapath, template_path, destdir = out_tab1$temp_folder())
          
          shiny::setProgress(value = 0.8, detail = "Saving the file...")
          
            openxlsx::saveWorkbook(meta_template, file, overwrite = TRUE)
          
          
          shiny::setProgress(value = 1, detail = "File ready for download")
          shinyjs::addClass(id = "card_header2_1", class = "bg-success")
          shinyjs::removeClass(id = "card_header2_1", class = "bg-danger")
        })
      }
    )
    
    # output$download_example_meta <- shiny::downloadHandler(
    #   filename = function() {
    #     paste0("Example_Filled_Meta.xlsx")
    #   },
    #   content = function(file) {
    #     # Define the template path
    #     template_path <- system.file("extdata", "Ltal.2007_xylo_meta_2025-03-08.xlsx", package = "xyloR")
    #     
    #     # Load the template
    #     obs_template <- openxlsx::loadWorkbook(template_path)
    #     
    #     # Save directly to the user-selected location
    #       openxlsx::saveWorkbook(obs_template, file, overwrite = TRUE)
    #   }
    # )
    # 
    output$download_example_meta <- shiny::downloadHandler(
      filename = function() {
        # Define the filename dynamically, if needed
        paste0("Example_xylo_meta.xlsx")
      },
      content = function(file) {
        # Provide progress feedback to the user
        shiny::withProgress(message = "Preparing the metadata file...", value = 0, {
          
          # Step 1: Define the template path
          shiny::setProgress(value = 0.1, detail = "Locating template file...")
          template_path <- system.file("extdata", "Ltal.2007_xylo_meta_2025-06-22.xlsx", package = "xyloR")
          
          # Check if the template file exists
          if (template_path == "") {
            showNotification("Template file not found. Please contact support.", type = "error")
            return(NULL)
          }
          
          # Step 2: Load the template
          shiny::setProgress(value = 0.5, detail = "Loading template...")
          obs_template <- openxlsx::loadWorkbook(template_path)
          
          # Step 3: Save the workbook to the user-selected location
          shiny::setProgress(value = 0.8, detail = "Saving file...")
          openxlsx::saveWorkbook(obs_template, file, overwrite = TRUE)
          
          # Final step: Complete the progress bar
          shiny::setProgress(value = 1, detail = "Download complete!")
        })
      }
    )
    
    
    # ============================================================================
    # CARD 2.2: Validate uploaded metadata and observation files
    # ============================================================================
    
    # Logging helper
    log_step <- function(label, value = NULL) {
      cat(paste0("[", Sys.time(), "] ", label, "\n"))
      if (!is.null(value)) print(value)
    }
    
    validation_results <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(input$meta_file, {
      shiny::req(input$meta_file)
      
      meta_file_saved <- normalizePath(
        file.path(out_tab1$temp_folder(), input$meta_file$name),
        winslash = "/", mustWork = FALSE
      )
      log_step("meta_file_saved", meta_file_saved)
      
      # Copy file if it doesn't exist
      if (!file.exists(meta_file_saved)) {
        file.copy(input$meta_file$datapath, meta_file_saved, overwrite = TRUE)
      }
      
      # Abort if copy failed
      if (!file.exists(meta_file_saved)) {
        shiny::showNotification("Metadata file path is invalid", type = "error")
        return()
      }
      
      # Start validation with progress
      shiny::withProgress(message = 'Validating metadata...', value = 0, {
        shiny::setProgress(value = 0.2, detail = "Loading file...")
        
        obs_file_data <- out_tab1$obs_file()
        has_valid_obs <- !is.null(obs_file_data) &&
          !is.null(obs_file_data$datapath) &&
          file.exists(obs_file_data$datapath)
        
        tbl_validation <- if (has_valid_obs) {
          shiny::setProgress(value = 0.5, detail = "Validating observation file...")
          rbind(
            xylo_format_validation(obs_file_data$datapath),
            meta_format_validation(input$meta_file$datapath)
          )
        } else {
          shiny::showNotification("Observation file is missing or invalid", type = "error")
          meta_format_validation(input$meta_file$datapath)
        }
        
        validation_results(tbl_validation)
        shinyjs::addClass(id = "card_header2_2", class = "bg-success")
        shinyjs::removeClass(id = "card_header2_2", class = "bg-danger")
        shinyjs::show("card_8")
      })
    })
    
    
    # Render DT VALIDATION table
    output$validation_table <- DT::renderDataTable({
      shiny::req(validation_results(), nrow(validation_results()) > 0)
      
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
    
    # Render OUTPUT VALIDATION REPORT
    output$validation_message <- shiny::renderUI({
      tbl <- validation_results()
      
      if (is.null(tbl) || nrow(tbl) == 0) {
        # Apply success styling
        shinyjs::addClass(id = "card_header2.3", class = "bg-success")
        shinyjs::removeClass(id = "card_header2.3", class = "bg-danger")
        shinyjs::show("card_9")
        
        return(
          htmltools::div(
            class = "alert alert-success p-3 rounded",  # Bootstrap alert with padding
            shiny::tags$h4(
              shiny::icon("check-circle"),  # Success icon
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
      } else {
        # Apply error styling
        shinyjs::addClass(id = "card_header2.3", class = "bg-danger")
        shinyjs::removeClass(id = "card_header2.3", class = "bg-success")
        
        return(
          htmltools::div(
            class = "alert alert-danger p-3 rounded",  # Bootstrap alert for errors
            shiny::tags$h4(
              shiny::icon("exclamation-triangle"),  # Error icon
              " Validation Issues Found!", 
              class = "mb-2"
            ),
            shiny::tags$p(
              "Please review the validation issues listed above, before proceeding.",
              class = "mb-3"
            ),
           )
        )
      }
    })
    
    # ============================================================================
    # CARD 2.3: Validation message box
    # ============================================================================
    
    output$validation_message <- shiny::renderUI({
      tbl <- validation_results()
      
      if (is.null(tbl) || nrow(tbl) == 0) {
        shinyjs::addClass(id = "card_header2_3", class = "bg-success")
        shinyjs::removeClass(id = "card_header2_3", class = "bg-danger")
        shinyjs::show("card_9")
        
        htmltools::div(
          class = "alert alert-success p-3 rounded",
          shiny::tags$h4(icon("check-circle"), " Success!", class = "mb-2"),
          shiny::tags$p("Congratulations! Your files are ready to be submitted.", class = "mb-2"),
          shiny::tags$p("You can now click on the button ",
                        shiny::tags$b("'Download exchange files as ZIP'!"), class = "mb-0")
        )
      } else {
        shinyjs::addClass(id = "card_header2_3", class = "bg-danger")
        shinyjs::removeClass(id = "card_header2_3", class = "bg-success")
        
        htmltools::div(
          class = "alert alert-danger p-3 rounded",
          shiny::tags$h4(icon("exclamation-triangle"), " Validation Issues Found!", class = "mb-2"),
          shiny::tags$p("Please review the validation issues listed before proceeding.", class = "mb-3")
        )
      }
    })
    
    
    # ============================================================================
    # CARD 2.4: Hierarchical plotting from metadata (sunburst plot)
    # ============================================================================

    # Enable submit button only if validation is successful
    shiny::observe({
      tbl <- validation_results()
      shinyjs::toggleState(id = "download_zip", condition = !is.null(tbl) && nrow(tbl) == 0)
    })
    
    # Folder path where the files will be saved
    # output$download_zip <- shiny::downloadHandler(
    #   filename = function() {
    #     shiny::req(out_tab1$dataset_name())
    #     paste(out_tab1$dataset_name(), "_Exchange_Files_", Sys.Date(), ".zip", sep = "")
    #   },
    #   
    #   content = function(file) {
    #     
    #     # Get the paths to the uploaded final files
    #     # req(input$obs_file, input$meta_file)
    #     # obs_file_path <- input$obs_file$name
    #     # meta_file_path <- input$meta_file$name
    #     # Define paths to the saved files in the reactive temp folder
    #     obs_file_saved <- file.path(out_tab1$temp_folder(), basename(out_tab1$obs_file()$name))
    #     meta_file_saved <- file.path(out_tab1$temp_folder(), basename(input$meta_file$name))
    #     
    #     
    #     # Call the function to process and save the exchange files
    #     result <- tryCatch({
    #       print("obs_file_saved")
    #       print(obs_file_saved)
    #       print("meta_file_saved")
    #       print(meta_file_saved)
    #       print("out_tab1$temp_folder()")
    #       print(out_tab1$temp_folder())
    #       print("out_tab1$dataset_name()")
    #       print(out_tab1$dataset_name())
    #       
    #       to_exchange_files(obs_file_saved, meta_file_saved, dir = out_tab1$temp_folder(), dataset_name = out_tab1$dataset_name())  # Process the files
    #     }, error = function(e) {
    #       stop(paste("Error: ", e$message))
    #     })
    #     
    #     # print(list.files(temp_folder()))
    #     
    #     # List all files inside the folder without the parent directory
    #     files_to_zip <- list.files(out_tab1$temp_folder(), full.names = TRUE, recursive = TRUE)
    #     
    #     # Clean the file paths, removing any redundant slashes
    #     files_to_zip <- gsub("//", "/", files_to_zip)
    #     
    #     # Compress the files into a ZIP file, avoiding the parent folder structure
    #     zip::zipr(zipfile = file, files = files_to_zip)      
    #   }
    # )
    # 
    # # Placeholder for modal UI output (if needed)
    # output$modal_ui <- renderUI({
    #   NULL
    # })
    # 
    
    output$download_zip <- shiny::downloadHandler(
      filename = function() {
        shiny::req(out_tab1$dataset_name())  # Ensure the dataset name is available
        paste(out_tab1$dataset_name(), "_Exchange_Files_", Sys.Date(), ".zip", sep = "")
      },
      
      content = function(file) {
        # Ensure the required files are present in the temp folder
        shiny::withProgress(message = 'Preparing the exchange files...', value = 0, {
          
          # Initialize progress
          shiny::setProgress(value = 0.1, detail = "Getting file paths...")
          
          # Get the paths to the uploaded final files from the reactive temp folder
          obs_file_saved <- file.path(out_tab1$temp_folder(), basename(out_tab1$obs_file()$name))
          meta_file_saved <- file.path(out_tab1$temp_folder(), basename(input$meta_file$name))
          
          # Print paths for debugging (optional)
          print("obs_file_saved")
          print(obs_file_saved)
          print("meta_file_saved")
          print(meta_file_saved)
          
          # Check if the files exist in the temporary folder
          if (!file.exists(obs_file_saved)) {
            shiny::showModal(modalDialog(
              title = "Error",
              "Observation file is missing or not found.",
              easyClose = TRUE,
              footer = NULL
            ))
            return(NULL)  # Exit function if the file doesn't exist
          }
          if (!file.exists(meta_file_saved)) {
            shiny::showModal(modalDialog(
              title = "Error",
              "Metadata file is missing or not found.",
              easyClose = TRUE,
              footer = NULL
            ))
            return(NULL)  # Exit function if the file doesn't exist
          }
          
          # Provide progress feedback for processing files
          shiny::setProgress(value = 0.3, detail = "Processing files...")
          
          # Process and save the exchange files
          result <- tryCatch({
            to_exchange_files(obs_file_saved, meta_file_saved, dir = out_tab1$temp_folder(), dataset_name = out_tab1$dataset_name())  # Process the files
          }, error = function(e) {
            shiny::showModal(modalDialog(
              title = "Error",
              paste("Error processing files:", e$message),
              easyClose = TRUE,
              footer = NULL
            ))
            return(NULL)  # Exit function on error
          })
          
          # Provide feedback that the files are being zipped
          shiny::setProgress(value = 0.6, detail = "Creating ZIP archive...")
          
          # List all files inside the folder without the parent directory
          files_to_zip <- list.files(out_tab1$temp_folder(), full.names = TRUE, recursive = TRUE)
          
          # Clean the file paths, removing any redundant slashes
          files_to_zip <- gsub("//", "/", files_to_zip)
          
          # Create the ZIP file
          zip::zipr(zipfile = file, files = files_to_zip)
          
          # Final progress update when the ZIP file is ready
          shiny::setProgress(value = 1, detail = "File ready for download")
          
          # Optionally, update the UI card to show success
          shinyjs::addClass(id = "card_header", class = "bg-success")
          shinyjs::removeClass(id = "card_header", class = "bg-danger")
        })
      }
    )
    
    
    # ============================================================================
    # CARD 2.4: Hierarchical plotting from metadata (sunburst plot)
    # ============================================================================
    
    df_hierarchy_reactive <- reactive({
      shiny::req(input$meta_file$datapath)
      meta_file_data <- input$meta_file$datapath
      
      sheet_names <- setdiff(readxl::excel_sheets(meta_file_data), c("instructions", "DropList", "ListOfVariables"))
      sheet_data <- setNames(lapply(sheet_names, function(sheet) readxl::read_excel(meta_file_data, sheet)[-1:-6,]), sheet_names)
      
      df_joined <- dplyr::left_join(sheet_data[["sample"]], sheet_data[["tree"]], by = "tree_label", relationship = "many-to-many") %>%
        dplyr::left_join(sheet_data[["site"]], by = c("site_label", "plot_label"), relationship = "many-to-many") %>%
        dplyr::group_by(network_label, site_label, plot_label, tree_label, year = lubridate::year(sample_date), sample_id) %>%
        dplyr::summarise(n = n(), .groups = "drop") %>%
        dplyr::mutate(
          site_label = paste0(network_label, "__", site_label),
          plot_label = paste0(site_label, "__", plot_label),
          tree_label = paste0(plot_label, "__", tree_label),
          year_label = paste0(tree_label, "__", year),
          sample_label = paste0(year_label, "__", sample_id)
        )
      
      df_tree <- df_joined %>%
        dplyr::group_by(tree_label, plot_label) %>%
        dplyr::summarise(value = sum(n), .groups = "drop") %>%
        dplyr::rename(id = tree_label, parent = plot_label)
      
      df_plot <- df_joined %>%
        dplyr::distinct(plot_label, site_label, tree_label) %>%
        dplyr::group_by(plot_label, site_label) %>%
        dplyr::summarise(value = n(), .groups = "drop") %>%
        dplyr::rename(id = plot_label, parent = site_label)
      
      df_site <- df_joined %>%
        dplyr::distinct(site_label, network_label) %>%
        dplyr::group_by(site_label, network_label) %>%
        dplyr::summarise(value = n(), .groups = "drop") %>%
        dplyr::rename(id = site_label, parent = network_label)
      
      dplyr::bind_rows(df_tree, df_plot, df_site) %>%
        dplyr::distinct(id, parent, value) %>%
        dplyr::arrange(parent, id) %>%
        dplyr::mutate(label = sub(".*__", "", id),
               text = paste0(label, " (", value, ")"))
    })
    
    # Render the sunburst plot
    output$hierarchical_structure <- plotly::renderPlotly({
      df_hierarchy <- df_hierarchy_reactive()
      plotly::plot_ly(
        data = df_hierarchy, 
        ids = ~id, 
        labels = ~text, 
        parents = ~parent, 
        values = ~value, 
        type = "sunburst",
        source = "sunburst_selection"
      ) %>%
        plotly::layout(paper_bgcolor = "#1E1E1E")
    })
    
    
    # ============================================================================
    # CARD 2.4: View data table
    # ============================================================================
    
    # Render the summary table based on selection
    output$meta_table <- DT::renderDataTable({
      shiny::req(input$meta_file$datapath)
      
      # Load the meta file
      meta_file <- input$meta_file$datapath
      sheet_names <- setdiff(readxl::excel_sheets(meta_file), c("instructions", "DropList", "ListOfVariables"))
      sheet_data <- setNames(lapply(sheet_names, function(sheet) readxl::read_excel(meta_file, sheet = sheet)[-1:-6,]), sheet_names)
      
      # Join sample, tree, and site data and group by relevant columns
      df_joined <- dplyr::left_join(sheet_data[["sample"]], sheet_data[["tree"]], by = "tree_label") %>%
        dplyr::left_join(sheet_data[["site"]], by = c("site_label", "plot_label")) %>%
        dplyr::group_by(network_label, site_label, plot_label, tree_label, year = lubridate::year(sample_date), sample_id) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")  # Ensure correct summarization
      
      # Capture selection from sunburst plot
      selection <- plotly::event_data("plotly_click", source = "sunburst_selection")
      
      # Filter data based on selection
      if (!is.null(selection)) {
        point_number <- selection$pointNumber
        df_hierarchy <- df_hierarchy_reactive()  # Access the reactive df_hierarchy
        selected_row <- df_hierarchy[point_number + 1, ]  # Adding 1 because point Number is 0-based
        
        # Split the 'id' of the selected row into components
        site_label_split <- strsplit(as.character(selected_row$id), "__")[[1]][2]  # Site label (2nd element)
        plot_label_split <- strsplit(as.character(selected_row$id), "__")[[1]][3]  # Plot label (3rd element)
        tree_label_split <- strsplit(as.character(selected_row$id), "__")[[1]][4]  # Tree label (4th element)
        
        # Filter df_joined based on selected row in df_hierarchy
        df_joined <- df_joined %>%
          dplyr::filter(
            (is.na(site_label_split) | site_label == site_label_split) &
              (is.na(plot_label_split) | plot_label == plot_label_split) &
              (is.na(tree_label_split) | tree_label == tree_label_split)
          )
      }
      
      # Render the table with filtered data
      DT::datatable(
        df_joined,
        options = list(
          paging = TRUE,
          searching = TRUE,
          autoWidth = TRUE,
          dom = 'Blfrtip',  # Show table (no search/pagination)
          scrollX = FALSE,
          scrollY = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")  # Center-align all columns
          )
        ),
        filter = "top",
        class = "table-dark"  # Apply dark theme
      )
    })
    
    return(
      list(
        meta_file = reactive(input$meta_file),
        validation_results = validation_results
      )
    )

  })
}


