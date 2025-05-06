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

  # ─── Panel wrapper ──────────────────────────────────────────────
  bslib::nav_panel(
    title = shiny::div(id = ns("upload_observation"), "1. Upload observation"),
    value = "Upload observation",
    shiny::fluidRow(
      # ─── Left column: Upload section ─────────────────────────────
      shiny::column(
        width = 3,
        class = "bg-light p-2 border-end",
        style = "height: 100%;",

        # 1.1 Dataset naming
        bslib::card(
          bslib::card_header(
            "1.1 Name your dataset",
            id = ns("card_header1_1"), class = "bg-danger",
            bslib::tooltip(
              bsicons::bs_icon("question-circle"),
              "Provide a unique identifier for your dataset. This will be used to name the output files. Then move to 1.2 Download observation data template",
              placement = "right"
            )
          ),
          bslib::card_body(
            shiny::p("Enter the name of your dataset. Should be alphanumeric, max 10 chars."),
            shiny::textInput(ns("dataset_name"), "Enter the name of your dataset", value = ""),
            shiny::actionButton(ns("submit"), "Validate name", class = "btn btn-primary")
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
          card_header(
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
          bslib::card_header("Observations performed [# radial files]"),
          bslib::card_body(DT::DTOutput(ns("obs_table"))),
          style = "display: none;",
          id = ns("card_6")
        )
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
mod_tab1_server <- function(id, session_global) {
  moduleServer(id, function(input, output, session) {
    
    # Include Observations and Upload metadata in the list of tabs to disable
    tab_ids <- c("upload_metadata")
    
    # Disable the "upload_metadata" tab initially
    shinyjs::disable(selector = "a[data-value='upload_metadata']")
    
    # Enable the tab and navigate to it when the button is clicked
    shiny::observeEvent(input$next_btn, {
      shinyjs::enable(selector = "a[data-value='upload_metadata']")
      delay(200, {
        bslib::nav_select(id = "tabs", selected = "upload_metadata", session = session_global)
      })
    })
    
    
    # # Accessing shared elements
    # temp_folder <- reactive({ shared$temp_folder })

    # TAB 1: ---------------------------------------------------------------------

    ### CARD 1.1 with the DATASET NAME

    # Listen for submit button click and perform validation
    shiny::observeEvent(input$submit,
      {
        name <- shiny::isolate(input$dataset_name)

        # Check if the dataset name is empty
        if (is.null(name) || name == "") {
          shiny::showNotification("Dataset name is required.", type = "error")
          return() # Stop execution if validation fails
        }

        # Check if the dataset name contains only valid characters
        if (!grepl("^[a-zA-Z0-9_-]+$", name)) {
          shiny::showNotification("Dataset name must contain only letters, numbers, underscores, or dashes.", type = "error")
          return() # Stop execution if validation fails
        }

        # If validation passes, continue with the rest of your logic
        # For example, you can show a success notification or proceed to the next step
        shiny::showNotification("Dataset name is valid!", type = "message")

        # shinyjs::addClass("card_header1_1", "bg-success")  # Green header
        # shinyjs::removeClass("card_header1_1", "bg-danger")

        # Show the cards after validation
        shinyjs::show("card_1")
        shinyjs::show("card_2")
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Run validation when Enter is pressed (simulate submit on Enter key press)
    shinyjs::runjs("
  $('#dataset_name').keypress(function(e) {
    if(e.which == 13) {  // 13 = Enter key
      $('#submit').click();  // Simulate clicking the submit button
    }
  });
")

    # Listen for changes in the dataset name field and update header color if empty
    shiny::observe({
      dataset_name <- input$dataset_name
      if (nchar(dataset_name) == 0) {
        update_card_header_class(FALSE) # Set to red if input is empty
      } else {
        # Update the header to show a valid input (e.g., set the color to green or reset to default)
        update_card_header_class(TRUE)
      }
    })



    ### CARD 1.2 with the DOWNLOAD TEMPLATE

    # Download TEMPLATE for OBSERVATION FILE
    # output$download_template <- shiny::downloadHandler(
    #   filename = function() {
    #     # Ensure dataset name is provided
    #     shiny::req(input$dataset_name) # Ensure that dataset_name is available before downloading
    #     paste0(input$dataset_name, "_xylo_data_", Sys.Date(), ".xlsx")
    #   },
    #   content = function(file) {
    #     # Validate dataset name before proceeding
    #     if (is.null(input$dataset_name) || input$dataset_name == "") {
    #       stop("Please enter a dataset name before downloading.")
    #     }
    # 
    #     # Define the paths for the template and temporary file
    #     template_path <- system.file("extdata", "Datasetname_xylo_data_yyyy-mm-dd.xlsx", package = "xyloR")
    #     if (template_path == "") {
    #       shiny::showModal(modalDialog(
    #         title = "Error",
    #         "Template file not found. Please check the template package.",
    #         easyClose = TRUE,
    #         footer = NULL
    #       ))
    #       return(NULL) # Exit function if the template is not found
    #     }
    # 
    #     obs_path_temporary <- file.path(tempdir(), paste0(input$dataset_name, "_xylo_data_", Sys.Date(), ".xlsx"))
    # 
    #     # Load the template
    #     tryCatch(
    #       {
    #         obs_template <- openxlsx::loadWorkbook(template_path)
    #       },
    #       error = function(e) {
    #         shiny::showModal(modalDialog(
    #           title = "Error",
    #           paste("Error loading template file:", e$message),
    #           easyClose = TRUE,
    #           footer = NULL
    #         ))
    #         return(NULL) # Exit function on error
    #       }
    #     )
    # 
    #     # Save the template to a temporary file
    #     openxlsx::saveWorkbook(obs_template, obs_path_temporary, overwrite = TRUE)
    # 
    #     # Copy the file to the user-selected location for downloading
    #     file.copy(obs_path_temporary, file)
    # 
    #     # Update card header to 'success' after download
    #     update_card_header_success() # Call helper function to update header color
    #   }
    # )
    
    output$download_template <- shiny::downloadHandler(
      filename = function() {
        # Ensure dataset name is provided
        shiny::req(input$dataset_name) # Ensure that dataset_name is available before downloading
        paste0(input$dataset_name, "_xylo_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        # Validate dataset name before proceeding
        if (is.null(input$dataset_name) || input$dataset_name == "") {
          shiny::showModal(modalDialog(
            title = "Error",
            "Please enter a dataset name before downloading.",
            easyClose = TRUE,
            footer = NULL
          ))
          return(NULL) # Exit function if the dataset name is missing
        }
        
        # Define the paths for the template and temporary file
        template_path <- system.file("extdata", "Datasetname_xylo_data_yyyy-mm-dd.xlsx", package = "xyloR")
        if (template_path == "") {
          shiny::showModal(modalDialog(
            title = "Error",
            "Template file not found. Please check the template package.",
            easyClose = TRUE,
            footer = NULL
          ))
          return(NULL) # Exit function if the template is not found
        }
        
        obs_path_temporary <- file.path(tempdir(), paste0(input$dataset_name, "_xylo_data_", Sys.Date(), ".xlsx"))
        
        # Provide progress feedback to the user
        shiny::withProgress(message = "Preparing dataset template...", value = 0, {
          
          shiny::setProgress(value = 0.2, detail = "Loading the template...")
          
          # Load the template
          tryCatch(
            {
              obs_template <- openxlsx::loadWorkbook(template_path)
            },
            error = function(e) {
              shiny::showModal(modalDialog(
                title = "Error",
                paste("Error loading template file:", e$message),
                easyClose = TRUE,
                footer = NULL
              ))
              return(NULL) # Exit function on error
            }
          )
          
          shiny::setProgress(value = 0.5, detail = "Prefilling the template...")
          
          # You can insert any logic here if you need to modify the template before saving it
          
          shiny::setProgress(value = 0.8, detail = "Saving the template...")
          
          # Save the template to a temporary file
          openxlsx::saveWorkbook(obs_template, obs_path_temporary, overwrite = TRUE)
          
          shiny::setProgress(value = 1, detail = "File ready for download")
          
          # Copy the file to the user-selected location for downloading
          file.copy(obs_path_temporary, file)
          
          # Optionally, you can update UI elements after completion
          shinyjs::addClass(id = "card_header", class = "bg-success")
          shinyjs::removeClass(id = "card_header", class = "bg-danger")
        })
      }
    )
    

    # Download FILLED EXAMPLE OBSERVATION FILE
    # output$download_example_obs <- shiny::downloadHandler(
    #   filename = function() {
    #     paste0("Example_Filled_Obs.xlsx")
    #   },
    #   content = function(file) {
    #     # Define the template path for filled example observation
    #     template_path <- system.file("extdata", "Ltal.2007_xylo_data_2025-03-06.xlsx", package = "xyloR")
    # 
    #     # Check if the template exists
    #     if (template_path == "") {
    #       shiny::showModal(modalDialog(
    #         title = "Error",
    #         "Example template file not found. Please check the template package.",
    #         easyClose = TRUE,
    #         footer = NULL
    #       ))
    #       return(NULL) # Exit function if the template is not found
    #     }
    # 
    #     # Load the template
    #     tryCatch(
    #       {
    #         obs_template <- openxlsx::loadWorkbook(template_path)
    #       },
    #       error = function(e) {
    #         shiny::showModal(modalDialog(
    #           title = "Error",
    #           paste("Error loading example template file:", e$message),
    #           easyClose = TRUE,
    #           footer = NULL
    #         ))
    #         return(NULL) # Exit function on error
    #       }
    #     )
    # 
    #     # Save directly to the user-selected location
    #     openxlsx::saveWorkbook(obs_template, file, overwrite = TRUE)
    #   }
    # )

    output$download_example_obs <- shiny::downloadHandler(
      filename = function() {
        paste0("Example_Filled_Obs.xlsx")
      },
      content = function(file) {
        # Provide progress feedback to the user
        shiny::withProgress(message = "Preparing the observation file...", value = 0, {
          
          # Step 1: Define the template path for filled example observation
          shiny::setProgress(value = 0.1, detail = "Locating template file...")
          template_path <- system.file("extdata", "Ltal.2007_xylo_data_2025-03-06.xlsx", package = "xyloR")
          
          # Check if the template exists
          if (template_path == "") {
            shiny::showModal(modalDialog(
              title = "Error",
              "Example template file not found. Please check the template package.",
              easyClose = TRUE,
              footer = NULL
            ))
            return(NULL) # Exit function if the template is not found
          }
          
          # Step 2: Load the template
          shiny::setProgress(value = 0.5, detail = "Loading template...")
          tryCatch(
            {
              obs_template <- openxlsx::loadWorkbook(template_path)
            },
            error = function(e) {
              shiny::showModal(modalDialog(
                title = "Error",
                paste("Error loading example template file:", e$message),
                easyClose = TRUE,
                footer = NULL
              ))
              return(NULL) # Exit function on error
            }
          )
          
          # Step 3: Save the workbook to the user-selected location
          shiny::setProgress(value = 0.8, detail = "Saving file...")
          openxlsx::saveWorkbook(obs_template, file, overwrite = TRUE)
          
          # Final step: Complete the progress bar
          shiny::setProgress(value = 1, detail = "Download complete!")
        })
      }
    )
    


    ### CARD 1.3 with the UPLOAD OBSERVATION FILE
    # Upload FILLED OBSERVATION FILE and RENDER INFORMATION

    temp_folder <- shiny::reactiveVal()
    # File upload observer
    shiny::observe({
      shiny::req(input$obs_file)

      # Logging helper
      log_step <- function(label, value = NULL) {
        cat(paste0("[", Sys.time(), "] ", label, "\n"))
        if (!is.null(value)) print(value)
      }

      # Load workbook
      wb <- tryCatch(
        {
          openxlsx::loadWorkbook(input$obs_file$datapath)
        },
        error = function(e) {
          shiny::showModal(modalDialog(
            title = "Error",
            paste("Error loading workbook:", e$message),
            easyClose = TRUE,
            footer = NULL
          ))
          return(NULL)
        }
      )

      # create the temp_folder
      shiny::observeEvent(input$dataset_name, {
        tmp <- file.path(
          normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
          paste0("GloboXylo_", input$dataset_name, "_", Sys.Date())
        )
        dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
        temp_folder(tmp)
        log_step("temp_folder created", tmp)
      })

      shiny::observeEvent(input$obs_file, {
        shiny::req(temp_folder(), input$obs_file$name) # Ensure dependencies exist
        
        # Initialize the path for saving the file
        obs_file_saved <- normalizePath(
          file.path(temp_folder(), input$obs_file$name),
          winslash = "/", mustWork = FALSE
        )
        
        # Actually save the file if it doesn't exist
        if (!file.exists(obs_file_saved)) {
          file.copy(input$obs_file$datapath, obs_file_saved, overwrite = TRUE)
        }
        
        log_step("obs_file_saved", obs_file_saved)
      })
      

      # Read site info
      site_info <- tryCatch(
        {
          openxlsx::readWorkbook(wb, sheet = "obs_data_info", startRow = 6, colNames = FALSE)
        },
        error = function(e) {
          shiny::showModal(modalDialog(
            title = "Error",
            paste("Error reading site information:", e$message),
            easyClose = TRUE,
            footer = NULL
          ))
          return(NULL)
        }
      )

      # Read xylo observation data
      obs_data <- tryCatch(
        {
          openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>%
            dplyr::tibble()
        },
        error = function(e) {
          shiny::showModal(modalDialog(
            title = "Error",
            paste("Error reading observation data:", e$message),
            easyClose = TRUE,
            footer = NULL
          ))
          return(NULL)
        }
      )

      # Fix site_info formatting if needed
      if (ncol(site_info) == 3) {
        site_label <- unique(obs_data$site_label) %>%
          tibble::tibble() %>%
          dplyr::filter(!is.na(.))
        site_info <- cbind(site_label, site_info)
      } else if (ncol(site_info) != 4) {
        shiny::showModal(modalDialog(
          title = "Error",
          "Expected 3 or 4 columns in 'obs_data_info'. Please check your data format.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }

      site_info <- setNames(site_info, c("site_label", "latitude", "longitude", "elevation"))

      # Update UI dropdown
      shiny::updateSelectInput(session, "site_filter", choices = unique(site_info$site_label))

      # Show subsequent UI cards
      shinyjs::show("card_3")
      shinyjs::show("card_4")
      shinyjs::show("card_5")
      shinyjs::show("card_6")
      shinyjs::show("card_7")

      shinyjs::addClass(id = "card_header1_3", class = "bg-success")
      shinyjs::removeClass(id = "card_header1_3", class = "bg-danger")
    })

    # Reactive xylo_obs data
    xylo_obs <- shiny::reactive({
      shiny::req(input$obs_file)
      shiny::req(input$site_filter)

      wb <- tryCatch(
        {
          openxlsx::loadWorkbook(input$obs_file$datapath)
        },
        error = function(e) {
          shiny::showModal(modalDialog(
            title = "Error",
            paste("Error loading workbook:", e$message),
            easyClose = TRUE,
            footer = NULL
          ))
          return(NULL)
        }
      )

      df <- tryCatch(
        {
          openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>%
            dplyr::tibble()
        },
        error = function(e) {
          shiny::showModal(modalDialog(
            title = "Error",
            paste("Error reading observation data:", e$message),
            easyClose = TRUE,
            footer = NULL
          ))
          return(NULL)
        }
      )


      # Robust date parsing
      if (is.numeric(df$sample_date)) {
        df <- df %>%
          dplyr::mutate(sample_date = as.Date(as.numeric(sample_date), origin = "1899-12-30"))
      } else {
        df <- df %>%
          dplyr::mutate(sample_date = parse_sample_dates(sample_date))
      }

      # Check if any parsing errors occurred
      if (any(is.na(df$sample_date))) {
        shiny::showModal(modalDialog(
          title = "Date Parsing Issue",
          "Some sample_date entries could not be parsed. Please check date formats.",
          easyClose = TRUE,
          footer = NULL
        ))
      }

      df <- df %>% dplyr::filter(!is.na(sample_date))
      df <- df %>% dplyr::filter(site_label == input$site_filter)

      return(df)
    })


    output$key_info_table <- DT::renderDataTable({
      shiny::req(input$obs_file) # Ensure the file is uploaded
      shiny::req(input$site_filter) # Ensure the site is selected

      # Use the reactive xylo_obs data
      df <- xylo_obs()

      # Load the workbook and site info
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)
      site_info <- openxlsx::readWorkbook(wb, sheet = "obs_data_info", startRow = 6, colNames = FALSE)

      # Ensure the correct number of columns
      if (ncol(site_info) == 3) {
        site_label <- unique(df$site_label) %>%
          tibble::tibble() %>%
          dplyr::filter(!is.na(.))
        site_info <- cbind(site_label, site_info) # Add site_label as the first column
      } else if (ncol(site_info) != 4) {
        stop("Error: 'obs_data_info' sheet must contain either 3 or 4 columns. Found ", ncol(site_info), " columns.")
      }

      # Set proper column names and filter for selected site
      site_info <- site_info %>%
        setNames(c("site_label", "latitude", "longitude", "elevation")) %>%
        dplyr::filter(site_label == input$site_filter) %>%
        dplyr::mutate(
          latitude = as.numeric(latitude),
          longitude = as.numeric(longitude),
          elevation = as.numeric(elevation)
        )

      # Extract additional key information
      owner_lastname <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 2, cols = 4, colNames = FALSE))
      owner_firstname <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 1, cols = 4, colNames = FALSE))
      owner_email <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 3, cols = 4, colNames = FALSE))
      contact_lastname <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 2, cols = 2, colNames = FALSE))
      contact_firstname <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 1, cols = 2, colNames = FALSE))
      contact_email <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 3, cols = 2, colNames = FALSE))

      # Extract site-specific details
      latitude <- site_info %>%
        dplyr::pull(latitude) %>%
        as.numeric()
      longitude <- site_info %>%
        dplyr::pull(longitude) %>%
        as.numeric()
      elevation <- site_info %>%
        dplyr::pull(elevation) %>%
        as.numeric()

      # Extract other key data from df
      network <- unique(df$network_label)
      site <- unique(df$site_label) %>%
        tibble::tibble() %>%
        dplyr::filter(!is.na(.))
      from <- as.Date(min(df$sample_date, na.rm = TRUE), origin = "1899-12-30")
      to <- as.Date(max(df$sample_date, na.rm = TRUE), origin = "1899-12-30")
      n_trees <- length(unique(df$tree_label))
      n_dates <- length(unique(df$sample_date))
      n_samples <- nrow(df)

      # Prepare the key information table
      key_info <- tibble(
        "Owner" = paste(owner_lastname, owner_firstname, sep = ", "),
        "Owner Email" = owner_email,
        "Contact" = paste(contact_lastname, contact_firstname, sep = ", "),
        "Contact Email" = contact_email,
        "Network" = paste(network, collapse = ", "),
        "Site" = paste(site, collapse = ", "),
        "Coordinates" = paste("Lat =", latitude, "Long =", longitude),
        "Elevation" = elevation,
        "Date From" = format(from, "%Y-%m-%d"),
        "Date To" = format(to, "%Y-%m-%d"),
        "n_Trees" = n_trees,
        "n_Dates" = n_dates,
        "n_Samples" = n_samples
      ) %>%
        t() %>%
        setNames("Key Info")

      # Return the table to render with DT
      DT::datatable(key_info,
        options = list(
          paging = FALSE,
          searching = FALSE,
          pageLength = 13,
          autoWidth = TRUE,
          dom = "t",
          scrollX = FALSE,
          scrollY = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = TRUE,
        class = "table-dark"
      )
    })


    ### CARD 6 with DT KEY STRUCTURE of OBS TABLE
    # Render ReacTable with OBS table structure
    output$obs_table <- DT::renderDataTable({
      shiny::req(input$obs_file) # Ensure the file is uploaded

      # Load and process data from the uploaded file
      df <- xylo_obs()

      # Extract obs information
      obs_list <- df %>%
        dplyr::select(-c(1:5, ncol(df))) %>%
        colnames()

      # Define the regex patterns for categories
      var_categories <- list(
        CZ = "^cz", EZ = "ez", TZ = "tz", MZ = "mz", PR = "pr"
      )

      # Extract count/width categories
      count_vars <- grep("n", obs_list, value = TRUE)
      width_vars <- grep("w", obs_list, value = TRUE)

      # Extract C/E/W/M/Py categories
      C_vars <- grep("^cz", obs_list, value = TRUE)
      E_vars <- grep("ez", obs_list, value = TRUE)
      T_vars <- grep("tz", obs_list, value = TRUE)
      M_vars <- grep("mz", obs_list, value = TRUE)
      Pr_vars <- grep("pr", obs_list, value = TRUE)

      # Group the results based on occurrences in the subset
      grouped <- data.frame(
        Category = c("Count", "Width"),
        CZ = c(length(intersect(C_vars, count_vars)), length(intersect(C_vars, width_vars))),
        EZ = c(length(intersect(E_vars, count_vars)), length(intersect(E_vars, width_vars))),
        TZ = c(length(intersect(T_vars, count_vars)), length(intersect(T_vars, width_vars))),
        MZ = c(length(intersect(M_vars, count_vars)), length(intersect(M_vars, width_vars))),
        PR = c(length(intersect(Pr_vars, count_vars)), length(intersect(Pr_vars, width_vars)))
      )

      # Define table styling options
      table_options <- list(
        paging = FALSE,
        searching = FALSE,
        autoWidth = TRUE,
        dom = "t",
        scrollX = FALSE,
        scrollY = FALSE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )

      # Render the table with DT and apply dark theme styles
      DT::datatable(grouped, options = table_options, rownames = FALSE, class = "table-dark")
    })


    ### CARD 5 with LEAFLET MAP
    # Render Leaflet map when file is uploaded
    output$mymap <- leaflet::renderLeaflet({
      shiny::req(input$obs_file)
      shiny::req(input$site_filter)

      # Load workbook and sheets
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath) # Read the OBS file
      site_info <- openxlsx::readWorkbook(wb, sheet = "obs_data_info", startRow = 6, colNames = FALSE)
      obs_data <- openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>%
        tibble::tibble()

      # Check the number of columns in site_info
      if (ncol(site_info) == 3) {
        site_label <- unique(obs_data$site_label) %>%
          tibble::tibble() %>%
          dplyr::filter(!is.na(.)) # Extract unique site labels
        site_info <- cbind(site_label, site_info) # Add it as the first column
      } else if (ncol(site_info) != 4) {
        stop("Error: Expected 3 or 4 columns in 'Xylo_obs_data'. Please check your data format.")
      }

      # Apply column names
      site_info <- site_info %>%
        setNames(c("site_label", "latitude", "longitude", "elevation")) %>%
        dplyr::filter(site_label == input$site_filter)

      # Check for missing lat/long values before continuing
      if (any(is.na(site_info$latitude)) | any(is.na(site_info$longitude))) {
        stop("Error: Missing latitude or longitude data for the selected site.")
      }

      # Convert lat/lng to numeric
      site_info <- site_info %>%
        dplyr::mutate(
          latitude = as.numeric(latitude),
          longitude = as.numeric(longitude)
        )

      # Create leaflet map
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(
          lng = site_info$longitude,
          lat = site_info$latitude,
          popup = paste0("Site: ", site_info$site_label)
        )
    })

    ### CARD 7 with DATA COVERAGE
    # Render PLOTLY PLOT with data coverage
    output$data_coverage_plot <- plotly::renderPlotly({
      df <- xylo_obs()
      shiny::req(df) # Ensure df exists

      # Ensure the column exists in df
      if (!(input$color %in% colnames(df))) {
        stop("Selected color column does not exist in the dataset.")
      }

      # Plot the data
      plotly::plot_ly(df,
        x = ~sample_date,
        y = ~tree_label,
        type = "scatter",
        mode = "markers",
        color = as.factor(df[[input$color]]), # Color by the selected variable
        marker = list(size = 10, opacity = 0.7)
      ) %>%
        layout(
          title = "Tree Sample Collection Dates",
          xaxis = list(title = "Date", showgrid = FALSE, zeroline = FALSE, color = "white"),
          yaxis = list(title = "Tree Label", categoryorder = "category ascending", color = "white"),
          plot_bgcolor = "#2e2e2e", # Dark background for the plot
          paper_bgcolor = "#2e2e2e", # Dark background for paper area
          font = list(color = "white")
        ) # White font color for axis titles
    })


    ### CARD 1.4 with the VALIDATION CHECKBOXES
    # Consolidate validation checks into a reactive expression for efficiency
    validation_check <- shiny::reactive({
      validate_location <- input$validate_location
      validate_data_coverage <- input$validate_data_coverage
      validate_observation <- input$validate_observation

      # Return TRUE if all validations are true
      validate_location && validate_data_coverage && validate_observation
    })

    # Toggle next button and update status based on validation_check reactive expression
    shiny::observe({
      if (validation_check()) {
        # Enable the next button and show success message
        shinyjs::toggleState(id = "next_btn", condition = TRUE)
        output$validation_status <- shiny::renderText("Validation complete! You can proceed.")

        # Update card header color to green (success)
        shinyjs::addClass(id = "card_header1_4", class = "bg-success")
        shinyjs::removeClass(id = "card_header1_4", class = "bg-danger")
      } else {
        # Disable the next button and show failure message
        shinyjs::toggleState(id = "next_btn", condition = FALSE)
        output$validation_status <- shiny::renderText("Please validate location, data coverage, and observation list before proceeding.")

        # Update card header color to red (danger)
        shinyjs::addClass(id = "card_header1_4", class = "bg-danger")
        shinyjs::removeClass(id = "card_header1_4", class = "bg-success")
      }
    })

    # # Handle navigation after validation
    # shiny::observeEvent(input$next_btn, {
    #   # Navigate to the next tab after validation
    #   bslib::nav_select(id = "tabs", selected = "upload_metadata", session = session)
    # })
    

    # Optionally, improve feedback messages for specific validation states
    shiny::observe({
      if (!input$validate_location) {
        output$validation_status <- renderText("Please validate the location.")
      } else if (!input$validate_data_coverage) {
        output$validation_status <- renderText("Please validate the data coverage.")
      } else if (!input$validate_observation) {
        output$validation_status <- renderText("Please validate the observation list.")
      } else {
        output$validation_status <- renderText("Validation complete! You can proceed.")
      }
    })


    # # Make reactives accessible to other modules
    # shared$xylo_obs <- xylo_obs
    # shared$dataset_name <- reactive(input$dataset_name)


    return(
      list(
        dataset_name = reactive(input$dataset_name),
        obs_file = reactive(input$obs_file),
        temp_folder = temp_folder,
        site_filter = reactive(input$site_filter)
      )
    )
  })
}
