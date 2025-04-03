#' GloboXylo Shiny App: Upload Observation and Metadata Data
#'
#' This Shiny application facilitates the upload of observation and metadata files,
#' visualizes the data using maps and plots, and includes validation checks for data consistency.
#'
#' @import shiny shinyjs bslib readxl writexl openxlsx leaflet plotly dplyr reactable zip
#' @importFrom shinyjs useShinyjs toggleState hide show
#' @export
#'
#' @examples
#' \dontrun{
#' xyloglobal_upload()
#' }
xyloglobal_upload <- function() {

  ui <- shiny::fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "darkly", primary = "#375A7F", secondary = "#3498DB", font_scale = 0.8, success = "#00BC8C", warning = "#F39C12", danger = "#E74C3C", info = "#3498DB"),
    
    # Add custom CSS to change the background color of the card
    tags$style(HTML("
  .bg-light-green {
    background-color: #f4f7f7 !important;
  }

  .card-body {
    height: auto;  /* Allows height to adjust dynamically based on content */
    overflow: visible;  /* No scrolling, visible overflow */
  }

  /* Dark theme for DataTable */
  .table-dark {
    background-color: #343a40 !important; /* Dark grey background */
    color: #ffffff; /* White text */
  }

  /* DataTable Search Field (Global Search) */
  .dataTables_filter input {
    background-color: #444444;  /* Dark grey for search input */
    color: #ffffff;  /* White text */
    border: 1px solid #6c757d;  /* Subtle border */
  }

  /* DataTable Column Filters (per-column filtering inputs) */
  thead input {
    background-color: #444444 !important; /* Dark grey for column filters */
    color: #ffffff !important; /* White text */
    border: 1px solid #6c757d !important; /* Subtle border */
  }

  /* Dark background for select dropdowns (including DataTable Show Entries) */
  .dataTables_length select, 
  .dataTables_filter input, 
  .dataTables_info {
    background-color: #444444 !important;  /* Dark grey */
    color: #ffffff !important;  /* White text */
    border: 1px solid #6c757d !important;  /* Subtle border */
  }

  /* Dark pagination controls */
  .dataTables_paginate {
    background-color: #343a40 !important; /* Dark grey */
  }

  /* Alternating row colors for readability */
  .table-dark tbody tr:nth-child(even) {
    background-color: #474747 !important;
  }
  .table-dark tbody tr:nth-child(odd) {
    background-color: #343a40 !important;
  }

  /* Dark theme for selectInput fields */
  .form-control, .selectize-input {
    background-color: #444444 !important; /* Dark grey background */
    color: #ffffff !important;  /* White text */
    border: 1px solid #6c757d !important; /* Subtle border */
  }

  /* Ensure placeholder text in selectInput is visible */
  .selectize-input::placeholder {
    color: #bbbbbb !important; /* Light grey for visibility */
  }

  /* Ensure the dropdown menu in selectInput is also dark */
  .selectize-dropdown {
    background-color: #444444 !important; /* Dark grey */
    color: #ffffff !important;  /* White text */
    border: 1px solid #6c757d !important; /* Subtle border */
  }

  /* Dark theme for options in selectInput */
  .selectize-dropdown-content .option {
    background-color: #444444 !important; /* Dark grey */
    color: #ffffff !important; /* White text */
  }

  /* Darker grey background for selected option */
  .selectize-dropdown-content .option.selected {
    background-color: #555555 !important;
  }

  /* Dark theme for textInput fields */
  .shiny-input-container input[type='text'], 
  .shiny-input-container textarea {
    background-color: #444444 !important;
    color: #ffffff !important;
    border: 1px solid #6c757d !important;
  }

  /* Dark theme for fileInput fields */
  .shiny-file-input {
    background-color: #444444 !important;
    color: #ffffff !important;
    border: 1px solid #6c757d !important;
  }

  /* Hover effect for fileInput */
  .shiny-file-input button:hover {
    background-color: #555555 !important;
    color: #ffffff !important;
  }

  /* Adjust the labels for inputs (dark theme) */
  .shiny-input-container label {
    color: #ffffff !important;
  }

  /* Checkbox Input (Make background dark and checkbox visible) */
  .shiny-input-container input[type='checkbox'] {
    background-color: #444444 !important;
    border: 1px solid #6c757d !important;
  }

  /* Style checkbox when checked */
  .shiny-input-container input[type='checkbox']:checked {
    background-color: #007bff !important; /* Bootstrap primary blue */
  }

  /* Dark theme for checkbox label */
  .shiny-input-container .checkbox label {
    color: #ffffff !important;
  }
"))
    ,
    
    
    shinyjs::useShinyjs(),
    
    
    
    shiny::titlePanel("Welcome to the GloboXylo data collector"),
    
    # Short explanation below the title
    shiny::div("This application allows you to prepare your GloboXylo data efficiently. 
            Get the template, upload your data, visualize the structure, validate the requirement and export results easily. Follow the instructions below to be guided along the process.", 
               style = "font-size: 16px; color: #666; margin-bottom: 20px;"),
    
    
    navset_card_tab(id = 'tabs',
                         
      # TAB 1: Upload Observation Data -----------------------------------------
                         nav_panel(title = "1. Upload observation data",
                                         
                                         shiny::fluidRow(
                                           # Left Side: Upload Section (With Background Color)
                                           shiny::column(3, class = "bg-light p-2 border-end",
                                                         style = "height: 100%;",
                                                         bslib::card(
                                                           bslib::card_header('1.1 Name your dataset', id = "card_header1.1", class = "bg-danger",
                                                                              tooltip(
                                                             bsicons::bs_icon("question-circle"),
                                                             "Provide a uniquely identifier for your dataset. This will be used to name the output files. Then move to 1.2 Download observation data template",
                                                             placement = "right"
                                                           )),
                                                           bslib::card_body(
                                                             fillable = FALSE,
                                                             shiny::p("Enter the name of your dataset. This should only contains letters/numbers and not be more than 10 characters"),
                                                             textInput("dataset_name", "Enter the name of your dataset", value = ""),
                                                             actionButton("submit", "Validate name", class = "btn btn-primary")
                                                           )
                                                         ),
                                                         
                                                         bslib::card(
                                                           bslib::card_header('1.2 Download observation data template', id = "card_header1.2", class = "bg-warning",
                                                                              tooltip(
                                                                                bsicons::bs_icon("question-circle"),
                                                                                "Click 'download template' to save an empty Excel template file for your observation data. Click 'download filled example' to download a prefilled example file. Then move to 1.3 Upload the filled observation data file",
                                                                                placement = "right"
                                                                              )),
                                                           bslib::card_body(
                                                             fillable = FALSE,
                                                             shiny::p("Click the button to download the template. Fill it in, save it, then browse and load the file."),
                                                             # Use fluidRow and column to position buttons next to each other
                                                             shiny::fluidRow(
                                                               shiny::column(6,  # Adjust width of each button as needed
                                                                             downloadButton("download_template", "Download Template", class = "btn btn-primary")
                                                               ),
                                                               shiny::column(6,  # Adjust width of each button as needed
                                                                             downloadButton("download_example_obs", "Download filled example", class = "btn btn-secondary")
                                                               )
                                                             )
                                                           ),
                                                           style = "display: none;",
                                                           id = "card_1"
                                                         ),
                                                         
                                                         bslib::card(
                                                           bslib::card_header('1.3 Upload the filled observation data file!', id = "card_header1.3", class = "bg-danger",
                                                                              tooltip(
                                                                                bsicons::bs_icon("question-circle"),
                                                                                "Use the browser to upload your filled excel file filled with your observation data. A map with the location of your site and a two tables with an overview of the sampling coverage and a summary of type of observation will open. You can switch between sites with the dropdown field here below. Then move to 1.4 Validate your data",
                                                                                placement = "right"
                                                                              )),
                                                           shiny::fileInput("obs_file", "", accept = c(".xlsx"), multiple = FALSE),
                                                           shiny::selectInput("site_filter", "Select Site", choices = NULL, selected = NULL, selectize = TRUE),                                                            style = "height: 300px; display: none;",
                                                           id = "card_2"
                                                         ),
                                                         
                                                         # Add a DT Table of key info below the cards
                                                         bslib::card(
                                                           bslib::card_header("Key Information Table"),
                                                           bslib::card_body(
                                                             DT::dataTableOutput("key_info_table")
                                                           ),
                                                           style = "display: none; height: auto; min-height: 600px; overflow: visible;",  # Hidden by default
                                                           id = "card_3"  # Add an ID to reference it later
                                                         )
                                           ),
                                           
                                           # Right Side: Map & Plotly + Add Observations Table here
                                           shiny::column(9,
                                                         style = "height: 100%;",  
                                                         bslib::card(
                                                           bslib::card_header("Geolocation Map"),
                                                           bslib::card_body(
                                                             leaflet::leafletOutput("mymap", height = "400px")
                                                           ),
                                                           style = "display: none;",  # Hidden by default
                                                           id = "card_4"  # Add an ID to reference it later
                                                         ),
                                                         
                                                         bslib::card(
                                                           bslib::card_header("Data Coverage Overview",
                                                           # Plot settings popover button
                                                           popover(
                                                             bsicons::bs_icon("gear", class = "ms-auto"),  # Icon for settings
                                                             selectInput("color", "Color by", choices = c("tree_species", "sample_id", "plot_label"), selected = "tree_species"),
                                                             title = "Plot Settings"
                                                           ),
                                                           class = "d-flex align-items-center gap-1"),
                                                           bslib::card_body(
                                                             plotly::plotlyOutput("data_coverage_plot", height = "300px")
                                                           ),
                                                           style = "display: none;",  # Hidden by default
                                                           id = "card_5"  # Add an ID to reference it later
                                                         ),
                                                         
                                                         # Move Observations performed section here
                                                         bslib::card(
                                                           bslib::card_header("Observations performed [number of radial files]"),
                                                           bslib::card_body(
                                                             DT::dataTableOutput("obs_table")
                                                           ),
                                                           style = "display: none; height: auto; min-height: 200px; overflow: visible;",  # Hidden by default
                                                           id = "card_6"  # Add an ID to reference it later
                                                         ),
                                                         
                                                         # Adding validation checkboxes and next button
                                                         bslib::card(
                                                           bslib::card_header("1.4 Validate your data", id = "card_header1.4", class = "bg-danger",
                                                                              tooltip(
                                                                                bsicons::bs_icon("question-circle"),
                                                                                "Validate your data by chcking that the location, the data coverage and the observation type are correct by checking the checkbox below. Then the next button will activate. Click it to access the next tab and move to 2.1 Download prefilled metadata template",
                                                                                placement = "right"
                                                                              )),
                                                           bslib::card_body(
                                                             shiny::checkboxInput("validate_location", "Validate Location", value = FALSE),
                                                             shiny::checkboxInput("validate_data_coverage", "Validate Data Coverage", value = FALSE),
                                                             shiny::checkboxInput("validate_observation", "Validate Observation list", value = FALSE),
                                                             shiny::textOutput("validation_status"),
                                                             shiny::actionButton('next_btn', 'Next', icon = shiny::icon('angle-double-right'), class = "btn btn-primary", disabled = TRUE)
                                                           ),
                                                           style = "display: none;",  # Hidden by default
                                                           id = "card_7",  # Add an ID to reference it later
                                                         )
                                           )
                                         ),
                                         
                                         shiny::br(),
                                         
                         ),
                         
      # TAB 2: Upload Metadata -----------------------------------------------
                         nav_panel("2. Upload metadata",
                                   
                                         shiny::fluidRow(
                                           # Left side
                                           shiny::column(3, class = "bg-light p-2 border-end",
                                                         style = "height: 100%;",  
                                                         bslib::card(
                                                           bslib::card_header('2.1 Download prefilled metadata template', id = "card_header2.1", class = "bg-warning",
                                                                              tooltip(
                                                                                bsicons::bs_icon("question-circle"),
                                                                                "Click 'download template' to save an empty Excel template file for your meta data. Click 'download filled example' to download a prefilled example file. Then move to 2.2 Load the completed metadata file",
                                                                                placement = "right"
                                                                              )),
                                                           bslib::card_body(
                                                             fillable = FALSE,
                                                             shiny::p("Click to open, prefill, and download the metadata template, prefilled with data from the observations file"),
                                                             shiny::p("Complete the prefilled metadata, save it again, and then browse to load the filled file for format check."),
                                                             shiny::p("Note: It may take a few seconds for the prefilled template to open.", style = "color: red;"),
                                                             # Use fluidRow and column to position buttons next to each other
                                                             shiny::fluidRow(
                                                               shiny::column(6,  # Adjust width of each button as needed
                                                                             downloadButton("download_meta_template", "Download Metadata Template", class = "btn btn-primary")
                                                               ),
                                                               shiny::column(6,  # Adjust width of each button as needed
                                                                             downloadButton("download_example_meta", "Download filled example", class = "btn btn-secondary")
                                                               )
                                                             )
                                                           )
                                                         ),
                                                         
                                                         bslib::card(
                                                           bslib::card_header('2.2 Load the completed metadata file (you just saved!) to perform validation', id = "card_header2.2", class = "bg-danger",
                                                                              tooltip(
                                                                                bsicons::bs_icon("question-circle"),
                                                                                "Use the browser to upload your filled excel file filled with your metadata data. A sunburst plot and a table with the hierarchical structure of yoru data will appear You can explore within the structure by clicking on the plot. A 'Validation table' will also open showing in red the open issues to comply the requirement of the DB formatting. These need to be fixed by correcting the metadata file and upload again untill the 'validation table' get green The 2.3 'Download exchange files as ZIP' get active and you can click it to export your files for submission the the GLOBOxylo DB.",
                                                                                placement = "right"
                                                                              )),
                                                           bslib::card_body(
                                                             shiny::fileInput("meta_file", "", accept = c(".xlsx")),
                                                             # actionButton("validate_meta", "Validate Metadata", class = "btn btn-success"),
                                                             shiny::textOutput("meta_validation_status"),
                                                             shiny::verbatimTextOutput("meta_validation_errors")
                                                           )
                                                         )
                                                         
                                           ),
                                           
                                           # Right side (New ReactTable)
                                           shiny::column(9,
                                                         style = "height: 100%;",  
                                                         bslib::card(
                                                           bslib::card_header("Overview of data structure"),
                                                           bslib::card_body(
                                                             plotly::plotlyOutput("hierarchical_structure", height = "500px")
                                                           ),
                                                           bslib::card_body(
                                                             DT::dataTableOutput("meta_table", height = "500px")  # ReactTable placeholder
                                                           )
                                                         ),
                                                         bslib::card(
                                                           bslib::card_header("Report of validation check!", id = "card_header2.3", class = "bg-danger",
                                                                              tooltip(
                                                                                bsicons::bs_icon("question-circle"),
                                                                                "Green = all fine! you can proceed. Red = Problems) Need to be fixed to comply DB formatting. check your metadafile and fix where necessary and move back to 2.2 Load the completed metadata file",
                                                                                placement = "right"
                                                                              )),
                                                           bslib::card_body(
                                                             DT::dataTableOutput("validation_table"),  
                                                             shiny::uiOutput("validation_message"),
                                                             style = "min-height: 0; padding: 10px;"  # Reduces padding and enforces minimal height
                                                           ),                                                          
                                                           style = "display: none; height: fit-content; overflow: hidden;",  # Adjust height dynamically
                                                           id = "card_8"  # Add an ID to reference it later
                                                         )
                                           ),
                                         ),
                                         
                                         shiny::br(),
                                         
                                         shiny::fluidRow(
                                           shiny::column(12,
                                                         bslib::card(
                                                           class = "border border-0 text-center",
                                                           # shiny::actionButton('submit_btn', 'Create exchange files', disabled = TRUE, icon = shiny::icon('angle-double-right'), class = "btn btn-primary"),
                                                           downloadButton("download_zip", "2.3 Download Exchange Files as ZIP", class = "btn btn-primary"),
                                                           # Modal for submission feedback
                                                           shiny::uiOutput("modal_ui"),
                                                           style = "display: none;",  # Hidden by default
                                                           id = "card_9"  # Add an ID to reference it later
                                                           
                                                         )
                                           )
                                         )
                         ),
                    
      # TAB 3: Upload Metadata -----------------------------------------------
      nav_panel("View Observations", value = "View Observations",
                conditionalPanel(
                  condition = "input.obs_file.size > 0",  # Make sure this is triggered when file is uploaded
                  bslib::card(
                    h4('Basic Info on Observation:'),
                    div(style = "height: auto;", DT::dataTableOutput("obs_data_info"))
                  ),
                  bslib::card(
                    h4('Observation:'),
                    DT::dataTableOutput("observation"),
                    fill = TRUE  # Allows the card to expand
                  )
                )
      ),
      
      # TAB 4: Upload Metadata -----------------------------------------------
      nav_panel("Authors' Info",
        bslib::card(
          class = "bg-light-green p-3 border-end",
          max_height = 400,
          bslib::card_header('ROR search tool'),
          bslib::layout_columns(
            bslib::card(
              shiny::selectInput("country_code", "Select country:",
                                 choices = c(Choose='', get_country_codes()),
                                 selectize = TRUE),
              shiny::textInput("search_string", "Enter search string:"),
              shiny::actionButton("search_ror", "Search for ROR", class = "btn btn-primary")
              #shiny::selectizeInput("result_choice", "Select Result:", choices = NULL)
            ),
            bslib::card(
              h4('ROR search results:'),
              DT::DTOutput("ror_results")
            ),
            col_widths = c(3,9)
          )
        ),
        bslib::card(
          bslib::card_header('Provide Authors'),
          p("Please list all authors (data owners) of the dataset. Note that
           the order provided here will be used as the order of authorship."),
          card_body(
            fillable = FALSE,
            actionButton("add_author_btn", "Add author", style = "width: 100px",
                         class = "btn btn-primary"),
            actionButton("del_author_btn", "Delete author", style = "width: 100px",
                         class = "btn btn-danger")
          ),
          # First author input
          author_input(1),
          
          # the dynamic author inputs
          uiOutput("author_inputs"),
          
          # dynamic contact person
          h5('Contact person'),
          uiOutput('contact_person')
          
        ),
        bslib::card(
          class="border border-0",
          bslib::card_body(
            fillable = FALSE,
            shiny::actionButton('save_authors', 'Submit author info', icon = icon('angle-double-right')))
        )
      ),
                    
      # TAB 5: Upload Metadata -----------------------------------------------
      nav_panel("Publications Info",
                bslib::card()
      ),
      
      
     # -----------------------------------------------
    )
  )
  

# -----------------------------------------------
# -----------------------------------------------
  
  
  server <- function(input, output, session) {
    
    # Initialize reactive variable for temp folder
    reactive_temp_folder <- reactiveVal()
    
    # Set temporary folder on startup for saving the created and uploaded excel files
    observe({
      dataset_name <- dataset_name_reactive()  # Access the reactive value within observe
      temp_folder <- file.path(tempdir(), paste0("GloboXylo_", dataset_name, "_", Sys.Date()))
      
      # Set the temp folder in reactive value
      reactive_temp_folder(temp_folder)
      
      dir.create(temp_folder, showWarnings = FALSE, recursive = TRUE)
    })
    
    
    # TAB 1: ---------------------------------------------------------------------
    
    ### CARD 1.1 with the DATASET NAME 
    # Observe event for the Validate name button
    observeEvent(input$submit, {
      validate_dataset_name()
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Run validation when Enter is pressed
    shinyjs::runjs("
  $('#dataset_name').keypress(function(e) {
    if(e.which == 13) {  // 13 = Enter key
      $('#submit').click();  // Simulate clicking the submit button
    }
  });
")
    
    # Define validation as a function to avoid duplication
    validate_dataset_name <- function() {
      dataset_name <- isolate(input$dataset_name)  # Prevent reactivity while typing
      
      # Ensure name is 1-10 characters and only contains letters/numbers
      if (nchar(dataset_name) > 0 && nchar(dataset_name) <= 10 && grepl("^[a-zA-Z0-9]+$", dataset_name)) {
        
        shinyjs::addClass("card_header1.1", "bg-success")  # Green header
        shinyjs::removeClass("card_header1.1", "bg-danger")
        
        # Show the cards after validation
        shinyjs::show("card_1")  
        shinyjs::show("card_2")  
        
      } else {
        # If invalid, show error message
        shinyjs::addClass("card_header1.1", "bg-danger")  # Red header
        shinyjs::removeClass("card_header1.1", "bg-success")
        
        showModal(modalDialog(
          title = "Invalid Name",
          "Please enter a valid dataset name (1 to 10 **alphanumeric** characters, no spaces or special characters).",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
    
    # Listen for changes in text input field and update header color if empty
    observe({
      dataset_name <- input$dataset_name
      if (nchar(dataset_name) == 0) {
        # If input is empty, set card header to danger
        shinyjs::addClass("card_header1.1", "bg-danger")
        shinyjs::removeClass("card_header1.1", "bg-success")
      }
    })
    
    # Reactive expression for dataset name
    dataset_name_reactive <- reactive({
      input$dataset_name
    })
  
    ### CARD 1.2 with the DOWNLOAD TEMPLATE 
    # Download TEMPLATE for OBSERVATION FILE
    output$download_template <- downloadHandler(
      filename = function() {
        paste0(input$dataset_name, "_xylo_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        # Ensure input is provided
        if (is.null(input$dataset_name) || input$dataset_name == "") {
          stop("Please enter a dataset name before downloading.")
        }
        
        # Define the paths for the template and temporary file
        template_path <- system.file("extdata", "Datasetname_xylo_data_yyyy-mm-dd.xlsx", package = "xyloR")
        obs_path_temporary <- file.path(tempdir(), paste0(input$dataset_name, "_xylo_data_", Sys.Date(), ".xlsx"))
        
        # Load the template
        obs_template <- openxlsx::loadWorkbook(template_path)
        openxlsx::saveWorkbook(obs_template, obs_path_temporary, overwrite = TRUE)
        
        # Copy the file to the location for downloading
        file.copy(obs_path_temporary, file)
        
        # Update the header color to 'success' after the template is downloaded
        shinyjs::addClass(id = "card_header1.2", class = "bg-success")  # Green header for success
        shinyjs::removeClass(id = "card_header1.2", class = "bg-warning")  # Remove warning (yellow)
        
        # # Optionally, show a modal to confirm the download
        # showModal(modalDialog(
        #   title = "Template Downloaded",
        #   "The template has been successfully downloaded.",
        #   easyClose = TRUE,
        #   footer = NULL
        # ))
      }
    )

    # Download FILLED EXAMPLE OBSERVATION FILE
    output$download_example_obs <- downloadHandler(
      filename = function() {
        paste0("Example_Filled_Obs.xlsx")
      },
      content = function(file) {
        # Define the template path
        template_path <- system.file("extdata", "Ltal.2007_xylo_data_2025-03-06.xlsx", package = "xyloR")
        
        # Load the template
        obs_template <- openxlsx::loadWorkbook(template_path)
        
        # Save directly to the user-selected location
        openxlsx::saveWorkbook(obs_template, file, overwrite = TRUE)
      }
    )




    ### CARD 1.3 with the UPLOAD OBSERVATION FILE
    # upload FILLED OBSERVATION FILE and RENDER INFORMATION
    
    # get possible sites in file to fill selectInput$site_filter dropdown
    observe({
      req(input$obs_file)  # Ensure a file is uploaded
      
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)  # Load the workbook
      
      # Save a copy in temp folder
      obs_file_saved <- file.path(reactive_temp_folder(), basename(input$obs_file$name))
      openxlsx::saveWorkbook(wb, obs_file_saved, overwrite = TRUE)
      
      # print(list.files(reactive_temp_folder()))
      
      # Read site information from the "obs_data_info" sheet
      site_info <- openxlsx::readWorkbook(wb, sheet = "obs_data_info", startRow = 6, colNames = FALSE)
      obs_data <- openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>% 
        dplyr::tibble() 
      
      # Check the number of columns
      if (ncol(site_info) == 3) {
        site_label <- unique(obs_data$site_label) %>% tibble() %>% filter(!is.na(.))  # Extract unique site labels
        site_info <- cbind(site_label, site_info)   # Add it as the first column
      } else if (ncol(site_info) != 4) {
        stop("⚠️ Error: Expected 3 or 4 columns in 'Xylo_obs_data'. Please check your data format.")
      }
      
      
      # Apply column names only if the check passes
      site_info <- setNames(site_info, c("site_label", "latitude", "longitude", "elevation"))
      
      
      # Extract unique site labels and update the dropdown
      updateSelectInput(session, "site_filter", choices = unique(site_info$site_label))
      
      # After the file is uploaded, show the subsequent cards
      shinyjs::show("card_3")
      shinyjs::show("card_4")
      shinyjs::show("card_5")
      shinyjs::show("card_6")
      shinyjs::show("card_7")
      
      # Apply the validated color to the header of the card where the file upload happens
      shinyjs::addClass(id = "card_header1.3", class = "bg-success")
      shinyjs::removeClass(id = "card_header1.3", class = "bg-danger")
    })
    
    # Reactive xylo_obs data
    xylo_obs <- reactive({
      req(input$obs_file)
      req(input$site_filter)
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)
      df <- openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>% 
        dplyr::tibble() 
      
      # Check if sample_date is numeric (Excel date format)
      if (!is.numeric(df$sample_date)) {
        df <- df %>%
          dplyr::mutate(sample_date = as.Date(as.numeric(sample_date), origin = "1899-12-30"))
      } else {
        # If not numeric, assume it's in character format and parse using lubridate
        df <- df %>%
          dplyr::mutate(sample_date = lubridate::parse_date_time(sample_date, orders = c("ymd", "dmy", "mdy")))
      }
      
      df <- df %>% dplyr::filter(!is.na(sample_date))

      
      # Filter by site_code if selected
      df <- df %>% dplyr::filter(site_label == input$site_filter)
      return(df)
      
      
      
      shinyjs::show("card_7")
    })
    
    # Reactive observation data
    WB <- reactive({
      req(input$obs_file)
      req(input$site_filter)
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)
      
      
      return(wb)
    })
    
    ### CARD 4 with DT KEY INFO TABLE
    # Render ReacTable with key info when file is uploaded
    output$key_info_table <- DT::renderDataTable({
      req(input$obs_file)  # Ensure the file is uploaded
      req(input$site_filter)  # Ensure the site is selected
      
      # Load and process data from the uploaded file
      df <- xylo_obs()
      
      # filtered Site info
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)

      # Read site information from the "obs_data_info" sheet
      site_info <- openxlsx::readWorkbook(wb, sheet = "obs_data_info", startRow = 6, colNames = FALSE)
      obs_data <- openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>% 
        dplyr::tibble() 
      
      # Check the number of columns
      if (ncol(site_info) == 3) {
        site_label <- unique(obs_data$site_label) %>% tibble() %>% filter(!is.na(.))  # Extract unique site labels
        site_info <- cbind(site_label, site_info)   # Add it as the first column
      } else if (ncol(site_info) != 4) {
        stop("⚠️ Error: Expected 3 or 4 columns in 'Xylo_obs_data'. Please check your data format.")
      }
      
      
      # Apply column names only if the check passes
      site_info <- setNames(site_info, c("site_label", "latitude", "longitude", "elevation")) %>%
        dplyr::filter(site_label == input$site_filter)
  
      
      # Extract key information
      owner_lastname <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 2, cols = 4, colNames = FALSE))
      owner_fistname <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 1, cols = 4, colNames = FALSE))
      owner_email <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 3, cols = 4, colNames = FALSE))
      contact_lastname <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 2, cols = 2, colNames = FALSE))
      contact_fistname <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 1, cols = 2, colNames = FALSE))
      contact_email <- as.character(openxlsx::readWorkbook(wb, sheet = "obs_data_info", rows = 3, cols = 2, colNames = FALSE))
      latitude <- site_info %>% dplyr::select(latitude) %>% dplyr::pull() %>% as.numeric()
      longitude <- site_info %>% dplyr::select(longitude) %>% dplyr::pull() %>% as.numeric()
      elevation <- site_info %>% dplyr::select(elevation) %>% dplyr::pull() %>% as.numeric()
      network <- unique(df$network_label)
      site <- unique(df$site_label) %>% tibble() %>% filter(!is.na(.))
      from <- as.Date(min(df$sample_date, na.rm = TRUE), origin = "1899-12-30")
      to <- as.Date(max(df$sample_date, na.rm = TRUE), origin = "1899-12-30")
      n_trees <- length(unique(df$tree_label))
      n_dates <- length(unique(df$sample_date))
      n_samples <- nrow(df)
      
      # Prepare the key information table
      key_info <- t(data.frame(
        "Owner" = paste(owner_lastname, owner_fistname, sep = ", "),
        "Owner Email" = owner_email,
        "Contact" = paste(contact_lastname, contact_fistname, sep = ", "),
        "Contact Email" = contact_email,
        "Network" = paste(network, collapse = ", "),
        "Site" = paste(site, collapse = ", "),
        "Coordinates" = paste(paste0("Lat = ", latitude), paste0("Long = ", longitude), sep = ", "),
        "Elevation" = elevation,
        "Date From" = format(from, "%Y-%m-%d"),
        "Date To" = format(to, "%Y-%m-%d"),
        "n_Trees" = n_trees,
        "n_Dates" = n_dates,
        "n_Samples" = n_samples
      ))
      
      colnames(key_info) <- c("Key Info")

      # Return the table to render with DT
      DT::datatable(key_info, 
                    options = list(
                      paging = FALSE,    # Disable pagination
                      searching = FALSE,
                      pageLength = 13,  # Limit the number of rows shown to 13
                      autoWidth = TRUE,  # Automatically adjust column widths
                      dom = 't',  # Use pagination controls
                      scrollX = FALSE,   # Disable horizontal scrolling
                      scrollY = FALSE,   # Disable vertical scrolling
                      columnDefs = list(list(className = 'dt-center', targets = "_all")) # Center align columns
                    ),
                    rownames = TRUE,  # Hide row names
                    class = "table-dark"  # Apply dark theme
      )
    })
    
    ### CARD 6 with DT KEY STRUCTURE of OBS TABLE
    # Render ReacTable with OBS table structure
    output$obs_table <- DT::renderDataTable({
      req(input$obs_file)  # Ensure the file is uploaded
      
      # Load and process data from the uploaded file
      df <- xylo_obs()
      
      # Extract obs information
      obs_list <- colnames(df[, -c(1:5, ncol(df))])
      
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
      
      # Render the table with DT and apply dark theme styles
      DT::datatable(grouped, 
                    options = list(
                      paging = FALSE,    # Disable pagination
                      searching = FALSE, # Remove search bar (optional)
                      autoWidth = TRUE,
                      dom = 't',  # Use pagination controls
                      scrollX = FALSE,  # Enable horizontal scrolling
                      scrollY = FALSE,  # Limit the height
                      columnDefs = list(list(className = 'dt-center', targets = "_all")) # Center align columns
                    ),
                    rownames = FALSE, # Hide row names
                    class = "table-dark"  # Apply dark theme
      )
    })
    
    ### CARD 5 with LEAFLET MAP
    # Render Leaflet map when file is uploaded
    output$mymap <- renderLeaflet({
      req(input$obs_file)
      req(input$site_filter)
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)  # Read the OBS file
      # Read site information from the "obs_data_info" sheet
      site_info <- openxlsx::readWorkbook(wb, sheet = "obs_data_info", startRow = 6, colNames = FALSE)
      obs_data <- openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>% 
        dplyr::tibble() 
      
      # Check the number of columns
      if (ncol(site_info) == 3) {
        site_label <- unique(obs_data$site_label) %>% tibble() %>% filter(!is.na(.))  # Extract unique site labels
        site_info <- cbind(site_label, site_info)   # Add it as the first column
      } else if (ncol(site_info) != 4) {
        stop("⚠️ Error: Expected 3 or 4 columns in 'Xylo_obs_data'. Please check your data format.")
      }
      
      
      # Apply column names only if the check passes
      site_info <- setNames(site_info, c("site_label", "latitude", "longitude", "elevation")) %>%
        dplyr::filter(site_label == input$site_filter)
      lng <- site_info %>% dplyr::select(longitude) %>% dplyr::pull() %>% as.numeric()
      lat <- site_info %>% dplyr::select(latitude) %>% dplyr::pull() %>% as.numeric()
      
      # Create leaflet map
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(lng = lng, lat = lat, popup = paste0("Site: ", site_info$site_label))
    })
    
    ### CARD 7 with DATA COVERAGE
    # Render PLOTLY PLOT with data coverage
    output$data_coverage_plot <- renderPlotly({
      df <- xylo_obs()
      req(df)  # Ensure df exists
      
      # Ensure the column exists in df
      if (!(input$color %in% colnames(df))) {
        stop("Selected color column does not exist in the dataset.")
      }
      
      # Plot the data
      plot_ly(df, 
              x = ~sample_date, 
              y = ~tree_label, 
              type = 'scatter', 
              mode = 'markers',
              color = as.factor(df[[input$color]]),  # Color by the selected variable
              marker = list(size = 10, opacity = 0.7)) %>%
        layout(title = "Tree Sample Collection Dates",
               xaxis = list(title = "Date", showgrid = FALSE, zeroline = FALSE, color = "white"),
               yaxis = list(title = "Tree Label", categoryorder = "category ascending", color = "white"),
               plot_bgcolor = "#2e2e2e",  # Dark background for the plot
               paper_bgcolor = "#2e2e2e",  # Dark background for paper area
               font = list(color = "white"))  # White font color for axis titles
    })
    
    ### CARD 1.4 with the VALIDATION CHECKBOXES
    observe({
      validate_location <- input$validate_location
      validate_data_coverage <- input$validate_data_coverage
      validate_observation <- input$validate_observation  # Correct the reference to the checkbox ID
      
      # Enable the next button only if all checkboxes are TRUE
      shinyjs::toggleState(id = "next_btn", condition = validate_location && validate_data_coverage && validate_observation)
      
      # Update validation status
      if (validate_location && validate_data_coverage && validate_observation) {
        output$validation_status <- renderText("Validation complete! You can proceed.")
        
        # Update card header color to green (success)
        shinyjs::addClass(id = "card_header1.4", class = "bg-success")
        shinyjs::removeClass(id = "card_header1.4", class = "bg-danger")
        
      } else {
        output$validation_status <- renderText("Please validate location, data coverage, and observation list before proceeding.")
        
        # Update card header color to red (danger)
        shinyjs::addClass(id = "card_header1.4", class = "bg-danger")
        shinyjs::removeClass(id = "card_header1.4", class = "bg-success")
        
      }
    })
    
    observeEvent(input$next_btn, {
      # bslib::update_navs(session, "inTabset", selected = "Upload metadata")
      bslib::nav_select(id = "tabs", selected = "2. Upload metadata", session = session)
    })
    
    # TAB 2: ---------------------------------------------------------------------
    
    xylo_file <- shiny::reactive({
      openxlsx::loadWorkbook(shiny::req(input$obs_file$datapath))
    })
    
    # print the tempdir() path just in case
    output$meta_file_copied <- shiny::renderText({
      shiny::req(input$open_meta_temp)
      paste0("File opened in temporary dir ", tempdir(),
             " . Clicking the button again will overwrite the file, so be sure to save it to a different location!")
    })
    
    # Download Handler for Metadata Template
    output$download_meta_template <- shiny::downloadHandler(
      filename = function() {
         paste0(input$dataset_name, "_xylo_meta_", Sys.Date(), ".xlsx")
      },
      content = function(file) {

        #shiny::req(input$obs_file)  
        
        # print(input$obs_file$datapath)
 
        shiny::withProgress(message = 'Processing metadata...', value = 0, {
          shiny::setProgress(value = 0.2, detail = "Loading the template...")
          
          template_path <- system.file("extdata", "Datasetname_xylo_meta_yyyy-mm-dd.xlsx", package = "xyloR")

          shiny::setProgress(value = 0.5, detail = "Prefilling the template...")
          meta_template <- create_xylo_metadata(input$obs_file$datapath, template_path)
          
          shiny::setProgress(value = 0.8, detail = "Saving the file...")
          
          openxlsx::saveWorkbook(meta_template, file, overwrite = TRUE)
          
          shiny::setProgress(value = 1, detail = "File ready for download")
          
          # Apply the validated color to the header of the card where the file upload happens
          shinyjs::addClass(id = "card_header2.1", class = "bg-success")
          shinyjs::removeClass(id = "card_header2.1", class = "bg-danger")
          
        })
      }
    )
    
    output$download_example_meta <- downloadHandler(
      filename = function() {
        paste0("Example_Filled_Meta.xlsx")
      },
      content = function(file) {
        # Define the template path
        template_path <- system.file("extdata", "Ltal.2007_xylo_meta_2025-03-08.xlsx", package = "xyloR")
        
        # Load the template
        obs_template <- openxlsx::loadWorkbook(template_path)
        
        # Save directly to the user-selected location
        openxlsx::saveWorkbook(obs_template, file, overwrite = TRUE)
      }
    )
    
    
    # Check format validation
    validation_results <- shiny::reactiveVal(NULL)  # Store validation results
    
    shiny::observeEvent(input$meta_file, {
      shiny::req(input$meta_file)  # Ensure the metadata file is uploaded before proceeding
      
      # Define the save path for the metadata file
      meta_file_saved <- file.path(reactive_temp_folder(), basename(input$meta_file$name))
      
      # print(list.files(reactive_temp_folder()))
      
  # Save a copy in temp folder
      file.copy(input$meta_file$datapath, meta_file_saved, overwrite = TRUE)
       
      # Show progress bar for validation
      shiny::withProgress(message = 'Validating metadata...', value = 0, {
        shiny::setProgress(value = 0.2, detail = "Loading file...")
        
        # Run the metadata validation
        tbl_validation <- meta_format_validation(input$meta_file$datapath)
        
        # Store the results
        validation_results(tbl_validation)
        
        # Apply the validated color to the header of the card where the file upload happens
        shinyjs::addClass(id = "card_header2.2", class = "bg-success")
        shinyjs::removeClass(id = "card_header2.2", class = "bg-danger")
        shinyjs::show("card_8")
      })
    })
    
    # Render validation table or success message
    output$validation_message <- shiny::renderUI({
      tbl <- validation_results()
      
      if (is.null(tbl) || nrow(tbl) == 0) {
        # Apply success styling
        shinyjs::addClass(id = "card_header2.3", class = "bg-success")
        shinyjs::removeClass(id = "card_header2.3", class = "bg-danger")
        shinyjs::show("card_9")
        
        return(
          shiny::div(
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
          shiny::div(
            class = "alert alert-danger p-3 rounded",  # Bootstrap alert for errors
            shiny::tags$h4(
              shiny::icon("exclamation-triangle"),  # Error icon
              " Validation Issues Found!", 
              class = "mb-2"
            ),
            shiny::tags$p(
              "Please review the validation issues listed before proceeding.",
              class = "mb-3"
            ),
            
            # Render DataTable (uncomment if needed)
            # DT::datatable(tbl, 
            #               options = list(
            #                 pageLength = 10,
            #                 searching = TRUE, 
            #                 autoWidth = TRUE,
            #                 dom = 'Bfrtip',  
            #                 scrollX = TRUE,  
            #                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            #               ), 
            #               rownames = FALSE,
            #               class = "table table-dark table-striped table-hover"
            # )
          )
        )
      }
    })
    
    # Render Plotly sunburst plot with hierarchical data
    # Store df_hierarchy as a reactive value
    df_hierarchy_reactive <- shiny::reactive({
      shiny::req(input$meta_file$datapath)
      
      # Load meta file
      meta_file <- input$meta_file$datapath
      sheet_names <- setdiff(readxl::excel_sheets(meta_file), c("instructions", "DropList", "ListOfVariables"))
      sheet_data <- setNames(lapply(sheet_names, function(sheet) readxl::read_excel(meta_file, sheet = sheet)[-1:-6,]), sheet_names)
      
      # Process hierarchical data
      df_joined <- dplyr::left_join(sheet_data[["sample"]], sheet_data[["tree"]], by = "tree_label", relationship = "many-to-many") %>%
        dplyr::left_join(sheet_data[["site"]], by = "site_label", relationship = "many-to-many") %>%
        dplyr::group_by(network_label, site_label, plot_label, tree_label, year = lubridate::year(sample_date), sample_id) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(
          site_label = paste0(network_label, "__", site_label),
          plot_label = paste0(site_label, "__", plot_label),
          tree_label = paste0(plot_label, "__", tree_label),
          year_label = paste0(tree_label, "__", year),
          sample_label = paste0(year_label, "__", sample_id)
        )
      
      # Create hierarchical dataset
      df_tree <- df_joined %>%
        dplyr::group_by(tree_label, plot_label) %>%
        dplyr::summarise(value = sum(n), .groups = "drop") %>%
        dplyr::rename(id = tree_label, parent = plot_label)
      
      df_plot <- df_joined %>%
        dplyr::distinct(plot_label, site_label, tree_label) %>%
        dplyr::group_by(plot_label, site_label) %>%
        dplyr::summarise(value = dplyr::n(), .groups = "drop") %>%
        dplyr::rename(id = plot_label, parent = site_label)
      
      df_site <- df_joined %>%
        dplyr::distinct(site_label, network_label) %>%
        dplyr::group_by(site_label, network_label) %>%
        dplyr::summarise(value = dplyr::n(), .groups = "drop") %>%
        dplyr::rename(id = site_label, parent = network_label)
      
      df_hierarchy <- dplyr::bind_rows(df_tree, df_plot, df_site) %>%
        dplyr::distinct(id, parent, value) %>%
        dplyr::arrange(parent, id) %>%
        dplyr::mutate(
          label = sub(".*__", "", id),  
          text = paste0(label, " (", value, ")")  
        )
      
      return(df_hierarchy)
    })
    
    # Render the sunburst plot
    output$hierarchical_structure <- plotly::renderPlotly({
      df_hierarchy <- df_hierarchy_reactive()  # Access the reactive df_hierarchy
      
      sunburst_plot <- plotly::plot_ly(
        data = df_hierarchy, 
        ids = ~id, 
        labels = ~text, 
        parents = ~parent, 
        values = ~value, 
        type = "sunburst",
        source = "sunburst_selection"  # Correctly set source ID
      ) %>%
        plotly::layout(
          paper_bgcolor = "#1E1E1E",  # Set background color for the whole plot area (dark)
          plot_bgcolor = "#1E1E1E",   # Set background color for the plotting area
          font = list(color = "#FFFFFF"),  # Set font color to white
          colorway = c("#00BC8C", "#F39C12", "#E74C3C", "#3498DB"),  # Correct color palette for the sunburst
          hoverlabel = list(bgcolor = "#333333", font = list(color = "#FFFFFF")),  # Set hover label style
          title = list(text = "Hierarchical Structure", font = list(color = "#FFFFFF")),  # Title color in white
          margin = list(t = 50, b = 50, l = 50, r = 50)  # Set margins around the plot
        )
      
      # Register the plotly click event
      sunburst_plot <- plotly::event_register(sunburst_plot, "plotly_click")
      
      sunburst_plot
    })
    
    # Render the summary table based on selection
    output$meta_table <- DT::renderDataTable({
      shiny::req(input$meta_file$datapath)
      
      # Load the meta file
      meta_file <- input$meta_file$datapath
      sheet_names <- setdiff(readxl::excel_sheets(meta_file), c("instructions", "DropList", "ListOfVariables"))
      sheet_data <- setNames(lapply(sheet_names, function(sheet) readxl::read_excel(meta_file, sheet = sheet)[-1:-6,]), sheet_names)
      
      # Join sample, tree, and site data and group by relevant columns
      df_joined <- dplyr::left_join(sheet_data[["sample"]], sheet_data[["tree"]], by = "tree_label") %>%
        dplyr::left_join(sheet_data[["site"]], by = "site_label") %>%
        dplyr::group_by(network_label, site_label, plot_label, tree_label, year = lubridate::year(sample_date), sample_id) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")  # Ensure correct summarization
      
      # Capture selection from sunburst plot
      selection <- plotly::event_data("plotly_click", source = "sunburst_selection")
      
      # Filter data based on selection
      if (!is.null(selection)) {
        point_number <- selection$pointNumber
        df_hierarchy <- df_hierarchy_reactive()  # Access the reactive df_hierarchy
        selected_row <- df_hierarchy[point_number + 1, ]  # Adding 1 because pointNumber is 0-based
 
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
          shiny::div(
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
          shiny::div(
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
            
            # Render DataTable (uncomment if needed)
            # DT::datatable(tbl, 
            #               options = list(
            #                 pageLength = 10,
            #                 searching = TRUE, 
            #                 autoWidth = TRUE,
            #                 dom = 'Bfrtip',  
            #                 scrollX = TRUE,  
            #                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            #               ), 
            #               rownames = FALSE,
            #               class = "table table-dark table-striped table-hover"
            # )
          )
        )
      }
    })
    
    # Enable submit button only if validation is successful
    shiny::observe({
      tbl <- validation_results()
      shinyjs::toggleState(id = "download_zip", condition = !is.null(tbl) && nrow(tbl) == 0)
    })
    

    # Folder path where the files will be saved
    output$download_zip <- downloadHandler(
      filename = function() {
        req(input$dataset_name)
        paste(input$dataset_name, "_Exchange_Files_", Sys.Date(), ".zip", sep = "")
      },
      
      content = function(file) {
        
        # Ensure the user has uploaded both files
        # if (is.null(file.path(temp_folder, basename(obs_file))) || is.null(file.path(temp_folder, basename(meta_file)))) {
        #   stop("Please upload both the final observation and metadata files")
        # }

        # Get the paths to the uploaded final files
        req(input$obs_file, input$meta_file)
        # obs_file_path <- input$obs_file$name
        # meta_file_path <- input$meta_file$name
        # Define paths to the saved files in the reactive temp folder
        obs_file_saved <- file.path(reactive_temp_folder(), basename(input$obs_file$name))
        meta_file_saved <- file.path(reactive_temp_folder(), basename(input$meta_file$name))
        
        
        # Call the function to process and save the exchange files
        result <- tryCatch({
          to_exchange_files(obs_file_saved, meta_file_saved, dir = reactive_temp_folder(), dataset_name = dataset_name_reactive())  # Process the files
        }, error = function(e) {
          stop(paste("Error: ", e$message))
        })
        
        # print(list.files(reactive_temp_folder()))
        
        # List all files inside the folder without the parent directory
        files_to_zip <- list.files(reactive_temp_folder(), full.names = TRUE, recursive = TRUE)
        
        # Clean the file paths, removing any redundant slashes
        files_to_zip <- gsub("//", "/", files_to_zip)
        
        # Compress the files into a ZIP file, avoiding the parent folder structure
        zip::zipr(zipfile = file, files = files_to_zip)      
      }
    )
    
    # Placeholder for modal UI output (if needed)
    output$modal_ui <- renderUI({
      NULL
    })

    # TAB 3: ---------------------------------------------------------------------
    
    # check for mandatory cells
    checkMandatory <- function(data, column_name, value_column, condition_value = TRUE, color = "red") {
      data %>%
        formatStyle(
          columns = column_name, 
          valueColumns = value_column, 
          backgroundColor = styleEqual(
            levels = condition_value, 
            values = c(color)
          )
        )
    }
    
    # check for character length
    checkLength <- function(data, column_name, value_column, cuts, color_values = c("", "red")) {
      data %>%
        formatStyle(
          columns = column_name,
          valueColumns = value_column,
          backgroundColor = styleInterval(cuts = cuts, values = color_values)
        )
    }
    
    # check for character length
    checkFormatting <- function(data, column, validation_column, false_color = "red", true_color = "") {
      data %>%
        formatStyle(
          columns = column,
          valueColumns = validation_column,
          backgroundColor = styleEqual(
            levels = c(TRUE, FALSE),
            values = c(true_color, false_color)
          )
        )
    }
    
  # dinfo  #### 
    # Reactive dataset
    dinfo <- reactiveVal()
    
    observe({
      req(input$obs_file)  # Ensure file is uploaded
      
      # Read the dataset
      site_info <- openxlsx::readWorkbook(WB(), sheet = "obs_data_info", startRow = 6, colNames = FALSE) %>%
        dplyr::tibble() 
      obs_data <- openxlsx::readWorkbook(WB(), sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>% 
        dplyr::tibble() 
      
      # Check the number of columns
      if (ncol(site_info) == 3) {
        site_label <- unique(obs_data$site_label) %>% tibble() %>% filter(!is.na(.))  # Extract unique site labels
        site_info <- cbind(site_label, site_info)   # Add it as the first column
      } else if (ncol(site_info) != 4) {
        stop("⚠️ Error: Expected 3 or 4 columns in 'Xylo_obs_data'. Please check your data format.")
      }
      
      
      # Apply column names only if the check passes
      site_info <- setNames(site_info, c("site_label", "latitude", "longitude", "elevation")) 
      
      
      # Add validation columns
      site_info <- site_info %>%
        dplyr::mutate(
          valid_latitute = is.numeric(latitude) & !is.na(latitude) & latitude >= -90 & latitude <= 90,
          valid_longitude = is.numeric(longitude) & !is.na(longitude) & longitude >= -180 & longitude <= 180,
          valid_elevation = is.numeric(elevation) & !is.na(elevation) & elevation >= 0 & elevation <= 10000
        )
      
      dinfo(site_info)  # Store in reactive value
    })
    
    # Render the editable DT table obs_info
    output$obs_data_info  <- DT::renderDT({
      DT::datatable(dinfo(), 
                    rownames = FALSE,
                    editable = list(target = "cell", disable = list(columns = c())), # Disable editing for certain columns
                    options = list(
                      pageLength = 10,  # Limit the number of rows shown to 10
                      autoWidth = TRUE,  # Automatically adjust column widths
                      dom = 'Bfrtip',  # Use pagination controls
                      scrollX = TRUE,   # Enable horizontal scrolling
                      scrollY = FALSE,  # Disable vertical scrolling
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Add export buttons
                      initComplete = JS(
                        "function(settings, json) {",
                        "  var table = this.api();",
                        "  table.columns().every(function(index) {",
                        "    var column = table.column(index);",
                        "    var redFound = false;",
                        "    column.nodes().each(function(cell, i) {",
                        "      if ($(cell).css('background-color') == 'rgb(255, 0, 0)') {  // Detect red cell background color",
                        "        redFound = true;",
                        "      }",
                        "    });",
                        "    if (!redFound) {",
                        "      $(column.header()).css({'background-color': 'green', 'color': 'black'});",
                        "    }",
                        "  });",
                        "}"
                      ),
                      columnDefs = list(list(visible=FALSE, targets=c(4:(ncol(dinfo())-1))))
                    )) %>% 
        checkFormatting("latitude", "valid_latitute") %>% 
        checkFormatting("longitude", "valid_longitude") %>% 
        checkFormatting("elevation", "valid_elevation")
    })
    
    # Observe cell edits and save back into xlsx
    observeEvent(input$obs_data_info_cell_edit, {
      info <- input$obs_data_info_cell_edit
      
      df <- dinfo()  # Get current data
      
      # Update the specific cell
      df[info$row, info$col + 1] <- as.numeric(info$value)  # Ensure numeric conversion
      
      # Recalculate validations
      df <- df %>%
        dplyr::mutate(
          valid_latitute = is.numeric(latitude) & !is.na(latitude) & latitude >= -90 & latitude <= 90,
          valid_longitude = is.numeric(longitude) & !is.na(longitude) & longitude >= -180 & longitude <= 180,
          valid_elevation = is.numeric(elevation) & !is.na(elevation) & elevation >= 0 & elevation <= 10000
        )
      
      dinfo(df)  # Save updated data
      
      # Write updated data back to the workbook
      openxlsx::writeData(WB(), sheet = "obs_data_info", df, startRow = 5, colNames = TRUE)
      
      # Save the workbook
      openxlsx::saveWorkbook(WB(), file = input$obs_file$datapath, overwrite = TRUE)
    })    
    
    #### dobs   ####
    # Reactive dataset
    dobs <- reactiveVal()
    
    observe({
      req(input$obs_file)  # Ensure file is uploaded
      
      # Read data from Excel, skipping first 6 rows
      data <- openxlsx::readWorkbook(WB(), sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>% 
        dplyr::tibble()
      
      # Check if sample_date is numeric and convert to Date
      data <- data %>%
        dplyr::mutate(
          sample_date = case_when(
            !is.na(sample_date) & is.numeric(as.numeric(sample_date)) ~ as.Date(as.numeric(sample_date), origin = "1899-12-30"),
            !is.na(sample_date) & !is.numeric(as.numeric(sample_date)) ~ lubridate::parse_date_time(sample_date, orders = c("ymd", "dmy", "mdy")),
            TRUE ~ NA_Date_  # Set NA for any invalid date or missing values
          )
        )
      
      dobs(data)  # Store in reactive value
    })
    
    # Render the editable DT table observation
    output$observation <- DT::renderDT({
      # Get the data from the reactive expression
      data_to_render <- dobs()
      # data_to_render[is.na(data_to_render)] <- "NA"
      # data_to_render$site_label[1] <- NA
      # data_to_render$network_label[1] <- "LOT"
      
      data_to_render <- data_to_render %>%
        mutate(
          nchar_sample_id = nchar(sample_id),
          nchar_tree_species = nchar(tree_species),
          nchar_tree_label = nchar(tree_label),
          nchar_plot_label = nchar(plot_label),
          nchar_site_label = nchar(site_label),
          nchar_network_label = nchar(network_label),
          nchar_sample_label = nchar(sample_label),
          nchar_radial_file = nchar(radial_file),
          
          NA_sample_date = is.na(sample_date) | sample_date %in% c("", "NA"),
          NA_tree_species = is.na(tree_species) | tree_species %in% c("", "NA"),
          NA_plot_label = is.na(plot_label) | plot_label %in% c("", "NA"),
          NA_site_label = is.na(site_label) | site_label %in% c("", "NA"),
          NA_radial_file = is.na(radial_file) | radial_file %in% c("", "NA"),
          sample_date = as.character(sample_date),  # Ensure sample_date is a character
          sample_date = ifelse(sample_date %in% c("", "NA"), NA, sample_date),  # Convert "NA" and empty strings to NA
          valid_sample_date = !is.na(parse_date_time(sample_date, orders = c("ymd", "dmy", "mdy"))),
          duplicate_sample_label = duplicated(sample_label) | duplicated(sample_label, fromLast = TRUE)
          )
      
      
      # List of valid species
      species_list <- openxlsx::readWorkbook(WB(), sheet = "DropList") %>%  
        select(Tree_species) %>% pull() 
      
      
      DT::datatable(data_to_render, 
                    rownames = FALSE,
                    editable = list(target = "cell", disable = list(columns = c())), # Disable editing for certain columns
                    options = list(
                      pageLength = 100,  # Limit the number of rows shown to 10
                      autoWidth = TRUE,  # Automatically adjust column widths
                      dom = 'Bfrtip',  # Use pagination controls
                      scrollX = TRUE,   # Enable horizontal scrolling
                      scrollY = FALSE,  # Disable vertical scrolling
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Add export buttons
                      initComplete = JS(
                        "function(settings, json) {",
                        "  var table = this.api();",
                        "  table.columns().every(function(index) {",
                        "    var column = table.column(index);",
                        "    var redFound = false;",
                        "    column.nodes().each(function(cell, i) {",
                        "      if ($(cell).css('background-color') == 'rgb(255, 0, 0)') {  // Detect red cell background color",
                        "        redFound = true;",
                        "      }",
                        "    });",
                        "    if (!redFound) {",
                        "      $(column.header()).css({'background-color': 'green', 'color': 'black'});",
                        "    }",
                        "  });",
                        "}"
                      ),
                      columnDefs = list(list(visible=FALSE, targets=c(15:(ncol(data_to_render)-1))))
                      
                    ))  %>%
        
        ### Droplist
        formatStyle("tree_species", 
                    backgroundColor = styleEqual(
                      levels = length(unique(data_to_render$tree_species)[!unique(data_to_render$tree_species) %in% species_list]) > 0,  # Non-matching species
                      values = c("red")  # Red for non-matching species
                    )) %>% 
        
        ### date format
        formatStyle(columns = "sample_date", valueColumns = "valid_sample_date",
                    backgroundColor = styleEqual(
                      levels = FALSE,
                      values = c("red")
                    )) %>%
        
        ### uniqueness
        formatStyle(columns = "sample_date", valueColumns = "duplicate_sample_label",
          backgroundColor = styleEqual(
            levels = TRUE,
            values = c("red")
          )
        ) %>%
        
        # Use the function checkLength
        checkLength("sample_id", "nchar_sample_id", cuts = 64, color_values = c("", "red")) %>%
        checkLength("tree_species", "nchar_tree_species", cuts = 64, color_values = c("", "red")) %>%
        checkLength("tree_label", "nchar_tree_label", cuts = 64, color_values = c("", "red")) %>%
        checkLength("plot_label", "nchar_plot_label", cuts = 64, color_values = c("", "red")) %>%
        checkLength("site_label", "nchar_site_label", cuts = 64, color_values = c("", "red")) %>%
        checkLength("network_label", "nchar_network_label", cuts = 64, color_values = c("", "red")) %>%
        checkLength("sample_label", "nchar_sample_label", cuts = 64, color_values = c("", "red")) %>%
        checkLength("radial_file", "nchar_radial_file", cuts = 6, color_values = c("", "red")) %>%
        
        # Use the function checkMandatory
        checkMandatory("sample_date", "NA_sample_date", color = "red") %>%
        checkMandatory("tree_species", "NA_tree_species", color = "red") %>%
        checkMandatory("plot_label", "NA_plot_label", color = "red") %>% 
        checkMandatory("site_label", "NA_site_label", color = "red") %>%
        checkMandatory("radial_file", "NA_radial_file", color = "red")
      
    })
    
    # Observe Cell Edits
    observeEvent(input$observation_cell_edit, {
      info <- input$observation_cell_edit
      
      data <- dobs()  # Get current data
      
      # Update the specific cell
      data[info$row, info$col + 1] <- info$value
      
      # List of valid species
      species_list <- openxlsx::readWorkbook(WB(), sheet = "DropList") %>%  
        select(Tree_species) %>% pull() 
      
      
      # Recalculate validations
      data <- data %>%
        mutate(
          sample_date = case_when(
            is.numeric(sample_date) ~ as.Date(as.numeric(sample_date), origin = "1899-12-30"),
            TRUE ~ lubridate::parse_date_time(sample_date, orders = c("ymd", "dmy", "mdy"))
          ),
          valid_sample_date = !is.na(parse_date_time(as.character(sample_date), orders = c("ymd", "dmy", "mdy"))),
          # Update character length columns for the edited row
          nchar_sample_id = nchar(sample_id),
          nchar_tree_species = nchar(tree_species),
          nchar_tree_label = nchar(tree_label),
          nchar_plot_label = nchar(plot_label),
          nchar_site_label = nchar(site_label),
          nchar_network_label = nchar(network_label),
          nchar_sample_label = nchar(sample_label),
          nchar_radial_file = nchar(radial_file),
          # Mandatory checks
          NA_sample_date = is.na(sample_date) | sample_date %in% c("", "NA"),
          NA_tree_species = is.na(tree_species) | tree_species %in% c("", "NA"),
          NA_plot_label = is.na(plot_label) | plot_label %in% c("", "NA"),
          NA_site_label = is.na(site_label) | site_label %in% c("", "NA"),
          NA_radial_file = is.na(radial_file) | radial_file %in% c("", "NA"),
          sample_date = as.character(sample_date),  # Ensure sample_date is a character
          sample_date = ifelse(sample_date %in% c("", "NA"), NA, sample_date),  # Convert "NA" and empty strings to NA
          valid_sample_date = !is.na(parse_date_time(sample_date, orders = c("ymd", "dmy", "mdy"))),
          duplicate_sample_label = duplicated(sample_label) | duplicated(sample_label, fromLast = TRUE)
        )
      
      dobs(data)  # Save updated data
      
      # Directly update the table with only the affected row's validation results
      output$observation <- DT::renderDT({
        # Get the updated data (only the modified row will be different)
        data_to_render <- dobs()
        
        DT::datatable(data_to_render, 
                      rownames = FALSE,
                      editable = list(target = "cell", disable = list(columns = c())), # Disable editing for certain columns
                      options = list(
                        pageLength = 100,  # Limit the number of rows shown to 10
                        autoWidth = TRUE,  # Automatically adjust column widths
                        dom = 'Bfrtip',  # Use pagination controls
                        scrollX = TRUE,   # Enable horizontal scrolling
                        scrollY = FALSE,  # Disable vertical scrolling
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Add export buttons
                        initComplete = JS(
                          "function(settings, json) {",
                          "  var table = this.api();",
                          "  table.columns().every(function(index) {",
                          "    var column = table.column(index);",
                          "    var redFound = false;",
                          "    column.nodes().each(function(cell, i) {",
                          "      if ($(cell).css('background-color') == 'rgb(255, 0, 0)') {  // Detect red cell background color",
                          "        redFound = true;",
                          "      }",
                          "    });",
                          "    if (!redFound) {",
                          "      $(column.header()).css({'background-color': 'green', 'color': 'black'});",
                          "    }",
                          "  });",
                          "}"
                        ),
                        columnDefs = list(list(visible=FALSE, targets=c(15:(ncol(data_to_render)-1))))
                      )) %>%
          
          # Apply styles and validations only for the affected row (info$row)
          formatStyle("tree_species", 
                      backgroundColor = styleEqual(
                        levels = length(unique(data_to_render$tree_species)[!unique(data_to_render$tree_species) %in% species_list]) > 0,
                        values = c("red")
                      )) %>%
          formatStyle(columns = "sample_date", valueColumns = "valid_sample_date",
                      backgroundColor = styleEqual(levels = FALSE, values = c("red"))
          ) %>%
          formatStyle(columns = "sample_date", valueColumns = "duplicate_sample_label",
                      backgroundColor = styleEqual(levels = TRUE, values = c("red"))
          ) %>%
          
          # Apply length checks only for affected row
          checkLength("sample_id", "nchar_sample_id", cuts = 64, color_values = c("", "red")) %>%
          checkLength("tree_species", "nchar_tree_species", cuts = 64, color_values = c("", "red")) %>%
          checkLength("tree_label", "nchar_tree_label", cuts = 64, color_values = c("", "red")) %>%
          checkLength("plot_label", "nchar_plot_label", cuts = 64, color_values = c("", "red")) %>%
          checkLength("site_label", "nchar_site_label", cuts = 64, color_values = c("", "red")) %>%
          checkLength("network_label", "nchar_network_label", cuts = 64, color_values = c("", "red")) %>%
          checkLength("sample_label", "nchar_sample_label", cuts = 64, color_values = c("", "red")) %>%
          checkLength("radial_file", "nchar_radial_file", cuts = 6, color_values = c("", "red")) %>%
          
          # Apply mandatory field checks
          checkMandatory("sample_date", "NA_sample_date", color = "red") %>%
          checkMandatory("tree_species", "NA_tree_species", color = "red") %>%
          checkMandatory("plot_label", "NA_plot_label", color = "red") %>% 
          checkMandatory("site_label", "NA_site_label", color = "red") %>%
          checkMandatory("radial_file", "NA_radial_file", color = "red")
      })
      
      # Write updated data back to the workbook
      openxlsx::writeData(WB(), sheet = "Xylo_obs_data", data, startRow = 7, colNames = TRUE)
      
      # Save the workbook
      openxlsx::saveWorkbook(WB(), file = input$obs_file$datapath, overwrite = TRUE)
    })
    
    # TAB 4: -------------------------------------------------------------------
    
    # toggle: only enable in case we have a country and search string
    shiny::observe({
      shinyjs::toggleState(id = "search_ror", 
                           condition = !((input$search_string=="") && 
                                          (input$search_country=="")))
    })
    
    # run the search via ROR API
    shiny::observeEvent(input$search_ror, {
      req(input$country_code)
      req(input$search_string)

      search_url <- sprintf(
        'https://api.ror.org/v2/organizations?query=%s&filter=country.country_code:%s',
        URLencode(input$search_string),
        input$country_code)

      ror_res <- httr::GET(search_url, httr::timeout(5))

      if (httr::status_code(ror_res) == 200) {
        ror_data <- jsonlite::fromJSON(rawToChar(ror_res$content))

        if (ror_data$number_of_results > 0) {

          # get the names (assuming that there is always exactly one ror_display name)
          res_names <- ror_data$items$names %>%
            dplyr::bind_rows() %>%
            filter(grepl('ror_display', types)) %>%
            dplyr::pull(value)
        
          # get the locations
          res_locs <- ror_data$items$locations %>% 
            dplyr::bind_rows() %>% 
            dplyr::pull(geonames_details) %>% 
            tidyr::unite(col = 'address', name, country_name, sep = ', ') %>% 
            dplyr::pull(address)
          
          # a dataframe of res_locs, res_names and res_ror_ids
          res_df <- data.frame(ROR = ror_data$items$id, Name = res_names, 
                               Location = res_locs)
          
          #updateSelectizeInput(session, "result_choice", choices = res_names)
          output$ror_results <- DT::renderDT({
            DT::datatable(res_df, rownames = FALSE)
          })
        } else {
          showNotification("No ROR results found. Try again.", type = "message")
        }
      } else {
        showNotification("ROR API request failed. Try again.", type = "error")
      }

    })

    # Keep track of the number of authors
    author_count <- shiny::reactiveVal(1)
    
    # Enable delete button only if there are more than 1 authors
    shiny::observe({
      shinyjs::toggleState(id = "del_author_btn", condition = author_count() > 1)
    })
    
    # Add author input if button is clicked
    shiny::observeEvent(input$add_author_btn, {
      author_count(author_count() + 1)
      author_id <- paste0('aut', author_count())
      shiny::insertUI(
        selector = "#author_inputs",
        where = "beforeBegin",
        ui = div(id = author_id,
                 author_input(author_count()))
      )
      #iv_gen$add_rule(paste0('autname_',author_count()), sv_required())
    })
    
    # Delete author input
    shiny::observeEvent(input$del_author_btn, {
      shiny::removeUI(selector = paste0("#aut", author_count()))
      #iv_gen$remove_rules(paste0('autname_',author_count()))
      author_count(author_count() - 1)
      
    })
    
    # Dynamic selection of contact person
    output$contact_person <- shiny::renderUI({
      shiny::radioButtons("contact_person", NULL,
                   choices = paste("Author", 1:author_count()))
    })
    
    
    
    # TAB 5: -------------------------------------------------------------------

    
  } # end of server function
  
  
  shinyApp(ui, server)



}
