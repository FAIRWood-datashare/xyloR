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
  
  

  

  # UI -------------------------------------------------------------------------
  ui <- shiny::fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "darkly", primary = "#375A7F", secondary = "#3498DB", font_scale = 0.8, success = "#00BC8C", warning = "#F39C12", danger = "#E74C3C", info = "#3498DB"),
    
    tags$head(
      # for the tippy tooltip
      tags$script(src = "https://unpkg.com/@popperjs/core@2"),
      tags$script(src = "https://unpkg.com/tippy.js@6")
    ),
    
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
      
      /* MN: new */
      /* Dark mode styles for rhandsontable */
      .htCore {
        background-color: #333333;
        color: #ffffff;
      }
      
      .htCore th {
        background-color: #444444;
        color: #ffffff;
      }
      
      .htCore td {
        background-color: #333333;
        color: #ffffff;
      }
      
      .htCore .htDimmed {
        color: #999999;
      }
      
      .htCore .current {
        background-color: #555555 !important;
      }
      
      .htCore .area {
        background-color: #666666 !important;
      }
      
      /* red background for the tab header */
      .red-tab {
      background-color:red;
      }
      
       /* Dropdown choices style for rhandsontable */
      .handsontable.listbox td {
        background: black;
      }
      .handsontable.listbox td.htDimmed {
        color: red;
      }
      .handsontable.listbox tr:hover td {
        background: yellow;
      }
      .handsontable.listbox tr td.current {
        background: green;
      }
    ")),
    
    shinyjs::useShinyjs(),
    

    shiny::titlePanel("Welcome to the GloboXylo data collector"),
    
    # Short explanation below the title
    shiny::div("This application allows you to prepare your GloboXylo data efficiently. 
            Get the template, upload your data, visualize the structure, validate the requirement and export results easily. Follow the instructions below to be guided along the process.", 
               style = "font-size: 16px; color: #666; margin-bottom: 20px;"),
    
    
    navset_card_tab(id = 'tabs',
                           
      # TAB 1: Upload Observation Data -----------------------------------------
      nav_panel(title = div(id="1. Upload observation", "1. Upload observation"),
                value = "Upload observation",
                       
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
                                         id = "card_7"  # Add an ID to reference it later
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
                                       )
                         )
                       ),
                       
                       shiny::br(),
                       
       ),
                         
      # TAB 2: Upload Metadata -----------------------------------------------
      nav_panel(
        title = div(id = "2. Upload metadata", "2. Upload meta"),
        value = "Upload meta",
        
        shiny::fluidRow(
          
          # Sidebar (left panel)
          shiny::column(
            3, class = "bg-light p-2 border-end", style = "height: 100%;",
            
            # 2.1 Download Metadata Template
            bslib::card(
              bslib::card_header(
                "2.1 Download prefilled metadata template",
                id = "card_header2.1", class = "bg-warning",
                tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Click 'Download Metadata Template' to save a prefilled Excel file with metadata based on your observations. \
Click 'Download filled example' to view a filled-in example. Then continue to 2.2 to upload your completed file.",
                  placement = "right"
                )
              ),
              bslib::card_body(
                fillable = FALSE,
                p("Click to open, prefill, and download the metadata template."),
                p("Complete the prefilled metadata, save it, then upload for validation."),
                p("Note: Opening the prefilled template may take a few seconds.", style = "color: red;"),
                
                shiny::fluidRow(
                  shiny::column(6, downloadButton("download_meta_template", "Download Metadata Template", class = "btn btn-primary")),
                  shiny::column(6, downloadButton("download_example_meta", "Download filled example", class = "btn btn-secondary"))
                )
              )
            ),
            
            # 2.2 Load completed metadata
            bslib::card(
              bslib::card_header(
                "2.2 Load completed metadata for validation",
                id = "card_header2.2", class = "bg-danger",
                tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Upload your filled metadata Excel file. \
A sunburst plot and hierarchical table will appear. \
Fix any validation issues shown in red, then re-upload until all issues are resolved and the 'Download Exchange Files' button activates.",
                  placement = "right"
                )
              ),
              bslib::card_body(
                fileInput("meta_file", "", accept = c(".xlsx")),
                textOutput("meta_validation_status"),
                verbatimTextOutput("meta_validation_errors")
              )
            ),
            
            # Validation report
            bslib::card(
              id = "card_8",
              style = "display: none; height: fit-content; overflow: hidden;",
              bslib::card_header(
                "Report of validation check!",
                id = "card_header2.3", class = "bg-danger",
                tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Green = All good! You can proceed. \
Red = Problems to fix in your metadata file. Return to 2.2, correct the file, and re-upload until all rows are green.",
                  placement = "right"
                )
              ),
              bslib::card_body(
                DT::dataTableOutput("validation_table"),
                uiOutput("validation_message"),
                style = "min-height: 0; padding: 10px;"
              )
            )
          ),
          
          # Main content (right panel)
          shiny::column(
            9, style = "height: 100%;",
            
            # Hierarchical structure plot and table
            bslib::card(
              bslib::card_header("Overview of data structure"),
              bslib::card_body(
                plotly::plotlyOutput("hierarchical_structure", height = "500px")
              ),
              bslib::card_body(
                DT::dataTableOutput("meta_table", height = "500px")
              )
            )
          )
        ),
        
        br(),
        
        # Download ZIP section (below full width)
        shiny::fluidRow(
          shiny::column(
            12,
            bslib::card(
              id = "card_9",
              class = "border border-0 text-center",
              style = "display: none;",
              downloadButton("download_zip", "2.3 Download Exchange Files as ZIP", class = "btn btn-primary"),
              uiOutput("modal_ui")
            )
          )
        )
      )
      ,
                    
      # TAB 3: Observation -----------------------------------------------
      nav_panel(
        title = div(id="observation_tab", "Observations"),
        value = "Observations",
        
        # Layout: Use a fluidRow to split into left (sidebar) and right (content)
        shiny::fluidRow(
          
          # Left side (sidebar) - Action Button and Info Section
          shiny::column(1, class = "bg-light p-2 border-end", style = "height: 100%;",  
                        bslib::card(
                          bslib::card_header(''),
                          bslib::card_body(
                            # Save button for observation
                            actionButton('save_obs', "Save obs", icon = icon('save'))
                          )
                        )
          ),
          
          # Right side - Main Content with Observation Table
          shiny::column(11, style = "height: 100%;",  
                        bslib::card(
                          bslib::card_header('Basic Info'),
                          bslib::card_body(
                            # Information or controls like text output or small table
                            div(rhandsontable::rHandsontableOutput("tbl1"))
                          )
                        ),
                        
                        bslib::card(
                          h4('Observation table:'),
                          div(rhandsontable::rHandsontableOutput("tbl2")),
                          verbatimTextOutput("testing1"),
                          verbatimTextOutput("testing2")
                        )
          )
        )
      ),
      
      # TAB 4: Site -----------------------------------------------
      nav_panel(
        title = div(id = "meta_site", "Site"),
        value = "Site",
        
        shiny::fluidRow(
          
          # Sidebar (left) — Save button or controls
          shiny::column(1, class = "bg-light p-2 border-end", style = "height: 100%;",
            
            bslib::card(
              bslib::card_header(""),
              bslib::card_body(
                actionButton('save_site', "Save site", icon = icon('save'))
              )
            )
          ),
          
          # Main content (right) — rhandsontable
          shiny::column(11, style = "height: 100%;",
            
            bslib::card(
              bslib::card_header("Site Metadata"),
              bslib::card_body(
                div(rhandsontable::rHandsontableOutput("tbl3"))
              )
            )
          )
        )
      ),
      
      # TAB 5: View tree -----------------------------------------------
      nav_panel(
        title = div(id="meta_tree", "Tree"),
        value = "Tree",
        
        # Layout: Use a fluidRow to split into left (sidebar) and right (content)
        shiny::fluidRow(
          
          # Left side (sidebar) - Action Button
          shiny::column(1, class = "bg-light p-2 border-end", style = "height: 100%;",  
                        bslib::card(
                          bslib::card_header(''),
                          bslib::card_body(
                            # Save tree button here
                            actionButton('save_tree', "Save tree", icon = icon('save'))
                          )
                        )
          ),
          
          # Right side - Main Content with Table
          shiny::column(11, style = "height: 100%;",  
                        bslib::card(
                          h4('Tree Metadata:'),
                          div(rhandsontable::rHandsontableOutput("tbl4"))
                        )
          )
        )
      )
      ,
      
      # TAB 6: View sample -----------------------------------------------
      nav_panel(
        title = div(id = "meta_sample", "Sample"),
        value = "Sample",
        
        shiny::fluidRow(
          
          # Sidebar (left) — Actions or instructions
          shiny::column(1, class = "bg-light p-2 border-end", style = "height: 100%;",
            
            bslib::card(
              bslib::card_header(""),
              bslib::card_body(
                actionButton('save_sample', "Save sample", icon = icon('save'))
              )
            )
          ),
          
          # Main content (right) — Editable metadata table
          shiny::column(11, style = "height: 100%;",
            
            bslib::card(
              bslib::card_header("Sample Metadata"),
              bslib::card_body(
                div(rhandsontable::rHandsontableOutput("tbl5"))
              )
            )
          )
        )
      ),
      
      # TAB 7: View Author -----------------------------------------------
      # TAB 7: View Author -----------------------------------------------
      nav_panel(
        title = div(id = "meta_author", "Author"),
        value = "Author",
        
        fluidRow(
          # Left Sidebar - Save Button
          column(1, class = "bg-light p-2 border-end", style = "height: 100%;",
                 bslib::card(
                   bslib::card_body(
                     actionButton('save_authors', 'Submit author info', icon = icon('save'))
                   )
                 )
          ),
          
          # Right Column - Main Content
          column(11, style = "height: 100%;",
                 
                 shinyjs::useShinyjs(),
                 shinyjs::hidden(verbatimTextOutput("form_visible", placeholder = TRUE)),
                 
                 ## Author Metadata Table and Action Buttons
                 bslib::card(
                   bslib::card_header("Authors Table"),
                   bslib::card_body(
                     rHandsontableOutput("tbl6"),
                     br(),
                     div(
                       style = "display: flex; gap: 10px; flex-wrap: wrap;",
                       actionButton("show_add_author", "Add New Author"),
                       actionButton("update_author", "Update an Author"),
                       actionButton("delete_author", "Delete Selected Author", icon = icon("trash")),
                       actionButton("apply_order", "Apply Author Order")
                     )
                   )
                 ),
                 
                 ## Conditional Panel - Show Form When Active
                 conditionalPanel(
                   condition = "output.form_visible == 'TRUE'",
                   
                   # ORCID Search Card
                   bslib::card(
                     bslib::card_header("ORCID Search"),
                     bslib::card_body(
                       fluidRow(
                         column(6, actionButton("search_orcid", "Search ORCID")),
                         column(6,
                                textInput("first_name_search", "First Name", placeholder = "First Name"),
                                textInput("last_name_search", "Last Name", placeholder = "Last Name"),
                                textInput("orcid_search", "ORCID ID", placeholder = "e.g. 0000-0002-1825-0097")
                         )
                       )
                     )
                   ),
                   
                   # ROR Search Card
                   bslib::card(
                     bslib::card_header("ROR Search"),
                     bslib::card_body(
                       fluidRow(
                         column(6, actionButton("search_ror", "Search ROR")),
                         column(6,
                                selectInput("country_code", "Select country:",
                                            choices = c(Choose = '', get_country_codes()),
                                            selectize = TRUE),
                                textInput("search_string", "Search ROR", placeholder = "e.g. University of Oxford")
                         )
                       ),
                       fluidRow(
                         column(12,
                                h4("ROR Search Results:"),
                                DT::DTOutput("ror_results")
                         )
                       )
                     )
                   ),
                   
                   # Author Metadata Entry Card
                   bslib::card(
                     bslib::card_header("Author Metadata"),
                     bslib::card_body(
                       fluidRow(
                         column(4, selectInput("person_role", "Person Role",
                                               choices = c("Contact and Data owner", "Data owner", "Contact", "Contributor"),
                                               selected = NULL)),
                         column(2, numericInput("person_order", "Order", value = 1, min = 1)),
                         column(3, textInput("last_name", "Last Name")),
                         column(3, textInput("first_name", "First Name"))
                       ),
                       fluidRow(
                         column(6, textInput("email", "Email")),
                         column(6, textInput("orcid", "ORCID (ID format or use search below)"))
                       ),
                       fluidRow(
                         column(6, textInput("organization_name", "Organization")),
                         column(6, textInput("research_organization_registry", "ROR ID"))
                       ),
                       fluidRow(
                         column(6, textInput("organization_name_finder", "Organization (Finder)")),
                         column(6, textInput("department", "Department"))
                       ),
                       fluidRow(
                         column(4, textInput("street", "Street")),
                         column(2, textInput("postal_code", "Postal Code")),
                         column(3, textInput("city", "City")),
                         column(3, textInput("country", "Country"))
                       ),
                       fluidRow(
                         column(6, textInput("person_country_code", "Country Code")),
                         column(6, textInput("webpage", "Webpage"))
                       ),
                       fluidRow(
                         column(6, textInput("phone_number", "Phone Number"))
                       ),
                       br(),
                       actionButton("add_author", "Add Author", class = "btn-success")
                     )
                   )
                 )
          )
        )
      )
      ,
      
      # TAB 8: View publication -----------------------------------------------
      nav_panel(
          title = div(id = "meta_publication", "Publication"),
          value = "Publication",
          
          fluidRow(
            column(1, class = "bg-light p-2 border-end", style = "height: 100%;",
                   bslib::card(
                     bslib::card_body(
                       actionButton('save_publication', "Save publication", icon = icon('save'))
                     )
                   )
            ),
            
            column(11, style = "height: 100%;",
                   bslib::card(
                     textInput("doi_input", "Enter DOI:"),
                     actionButton("search_doi", "Search for DOI", class = "btn btn-info"),
                     actionButton("add_publication", "Add Publication")
                   ),
                   bslib::card(
                     h6('DOI search result:'),
                     textOutput("doi_result"),
                     verbatimTextOutput("testing")
                   ),
                   bslib::card(
                     bslib::card_header("Publication Metadata"),
                     bslib::card_body(
                       div(rhandsontable::rHandsontableOutput("tbl7"))
                     )
                   )
            )
          )
        )
      ,
                    
      
     # -----------------------------------------------
    )
  )
  

# -----------------------------------------------
# -----------------------------------------------
  
  # SERVER ---------------------------------------------------------------------
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
    
    tab_ids <- c("Site", "Tree", "Sample", "Author", "Publication")
    
    lapply(tab_ids, function(id) {
      shinyjs::disable(selector = sprintf("a[data-value='%s']", id))
    })
    
    # Enable them once WB_meta() becomes available
    
    observe({
      req(WB_meta())
      
      # Try reading a key sheet to ensure the file is OK
      try({
        openxlsx::readWorkbook(WB_meta(), sheet = "site", startRow = 1, colNames = TRUE)
        
        # Enable the tabs once the workbook is successfully read
        lapply(tab_ids, function(id) {
          shinyjs::enable(selector = sprintf("a[data-value='%s']", id))
        })
      }, silent = TRUE)
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
      bslib::nav_select(id = "tabs", selected = "Upload meta", session = session)
    })
    
    # Reactive observation data
    WB <- reactive({
      req(input$obs_file)
      req(input$site_filter)
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)
      return(wb)
    })
    
    # column_configs1 <- reactive({
    #   req(WB())
    #   droplist1 <- openxlsx::readWorkbook(WB(), sheet = "DropList")
    #   tree_species_droplist <- droplist1 %>% select(tree_species) %>% pull()
    #   
    #   #### validation configuration
    #   column_configs1 <- list(
    #     tbl1 = list(
    #       site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 5, regex_pattern = NULL, unique = TRUE, readOnly = TRUE),
    #       latitude = list(type = 'numeric', required = TRUE, min_val = -90, max_val = 90),
    #       longitude = list(type = 'numeric', required = TRUE, min_val = -180, max_val = 180),
    #       elevation = list(type = 'numeric', required = TRUE, min_val = 0, max_val = NULL)
    #     ),
    #     
    #     tbl2 = list(
    #       sample_date = list(type = "date", required = TRUE),
    #       sample_id = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
    #       tree_species = list(type = "dropdown", required = TRUE, options =c("Picea abies (L.) Karst.", "Larix decidua Mill.")),
    #       # tree_species = list(type = "dropdown", required = TRUE, options = tree_species_droplist), # drop
    #       tree_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
    #       plot_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
    #       site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 5, readOnly = FALSE),
    #       network_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64, unique = TRUE),
    #       sample_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64, unique = TRUE),
    #       radial_file = list(type = "character", required = TRUE, min_length = 1, max_length = 6)
    #     )
    #   )
    #   return(column_configs1)
    #   
    # })
    
    
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
    
    # Reactive observation data
    WB_meta <- reactive({
      tryCatch({
        req(input$meta_file)
      wb <- openxlsx::loadWorkbook(input$meta_file$datapath)
      return(wb)
      }, error = function(e) {
        return(NULL)  # Returning NULL in case of error, ensuring continuity
      })
    })
    


    #### validation configuration ####
    column_configs <- reactive({
      req(WB())
      droplist1 <- openxlsx::readWorkbook(WB(), sheet = "DropList")

      # tbl2 Obs
      tree_species_droplist <- droplist1 %>% select(tree_species) %>% filter(!is.na(tree_species)) %>% pull()
      itrdb_species_code_droplist <- droplist1 %>% select(ITRDB_species_code) %>% filter(!is.na(ITRDB_species_code)) %>% pull()

      base_configs <- list(
        tbl1 = list(
          #site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 5, regex_pattern = NULL, unique = TRUE, readOnly = TRUE),
          site_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$site_label)), unique = TRUE, readOnly = TRUE),
          latitude = list(type = 'numeric', required = TRUE, min_val = -90, max_val = 90),
          longitude = list(type = 'numeric', required = TRUE, min_val = -180, max_val = 180),
          elevation = list(type = 'numeric', required = TRUE, min_val = 0, max_val = NULL)
        ),

        tbl2 = list(
          sample_date = list(type = "date", required = TRUE),
          sample_id = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
          tree_species = list(type = "character", required = TRUE, min_length = 1, max_length = 64, readOnly = TRUE),
          # tree_species = list(type = "dropdown", required = TRUE, options =c("Picea abies (L.) Karst.", "Larix decidua Mill.")),
          # tree_species = list(type = "dropdown", required = TRUE, options = tree_species_droplist), # drop
          itrdb_species_code = list(type = "dropdown", required = TRUE, options = itrdb_species_code_droplist),
          tree_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
          plot_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
          site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 5),
          network_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64),
          sample_label = list(type = "character", required = TRUE, min_length = 1, max_length = 64, unique = TRUE),
          radial_file = list(type = "character", required = TRUE, min_length = 1, max_length = 6),
          sample_comment = list(type = 'character', min_length = 1, max_length = NULL)
        )
      )

      # If WB_meta is loaded, add the rest

      if (!is.null(WB_meta())) {
      req(WB_meta())
      droplist2 <- openxlsx::readWorkbook(WB_meta(), sheet = "DropList")

      # tbl3 Site
      country_code_droplist <- droplist2 %>% select(country_code) %>% filter(!is.na(country_code)) %>% pull()
      koppen_climate_value_droplist <- droplist2 %>% select(koppen_climate_value) %>% filter(!is.na(koppen_climate_value)) %>% pull()
      koppen_climate_code_droplist <- droplist2 %>% select(koppen_climate_code) %>% filter(!is.na(koppen_climate_code)) %>% pull()
      koppen_climate_classification_droplist <- droplist2 %>% select(koppen_climate_classification) %>% filter(!is.na(koppen_climate_classification)) %>% pull()
      site_topography_droplist <- droplist2 %>% select(site_topography) %>% filter(!is.na(site_topography)) %>% pull()
      soil_depth_droplist <- droplist2 %>% select(soil_depth) %>% filter(!is.na(soil_depth)) %>% pull()
      soil_water_holding_capacity_droplist <- droplist2 %>% select(soil_water_holding_capacity) %>% filter(!is.na(soil_water_holding_capacity)) %>% pull()
      forest_stand_type_droplist <- droplist2 %>% select(forest_stand_type) %>% filter(!is.na(forest_stand_type)) %>% pull()
      forest_stand_structure_droplist <- droplist2 %>% select(forest_stand_structure) %>% filter(!is.na(forest_stand_structure)) %>% pull()
      forest_stand_age_droplist <- droplist2 %>% select(forest_stand_age) %>% filter(!is.na(forest_stand_age)) %>% pull()
      forest_stand_management_intensity <- droplist2 %>% select(forest_stand_management_intensity) %>% filter(!is.na(forest_stand_management_intensity)) %>% pull()
      # tbl4 Tree
      # tree_species_droplist <- droplist2 %>% select(tree_species) %>% filter(!is.na(tree_species)) %>% pull()
      # itrddb_species_code_droplist <- droplist2 %>% select(itrddb_species_code) %>% filter(!is.na(itrddb_species_code)) %>% pull()
      wood_type_droplist <- droplist2 %>% select(wood_type) %>% filter(!is.na(wood_type)) %>% pull()
      leaf_habit_droplist <- droplist2 %>% select(leaf_habit) %>% filter(!is.na(leaf_habit)) %>% pull()
      tree_ring_structure_droplist <- droplist2 %>% select(tree_ring_structure) %>% filter(!is.na(tree_ring_structure)) %>% pull()
      tree_treatment_droplist <- droplist2 %>% select(tree_treatment) %>% filter(!is.na(tree_treatment)) %>% pull()
      tree_sex_droplist <- droplist2 %>% select(tree_sex) %>% filter(!is.na(tree_sex)) %>% pull()
      tree_social_status_droplist <- droplist2 %>% select(tree_social_status) %>% filter(!is.na(tree_social_status)) %>% pull()
      tree_health_status_droplist <- droplist2 %>% select(tree_health_status) %>% filter(!is.na(tree_health_status)) %>% pull()
      tree_origin_droplist <- droplist2 %>% select(tree_origin) %>% filter(!is.na(tree_origin)) %>% pull()
      # tbl5 Sample
      sample_organ_droplist <- droplist2 %>% select(sample_organ) %>% filter(!is.na(sample_organ)) %>% pull()
      sample_preparation_method_droplist <- droplist2 %>% select(sample_preparation_method) %>% filter(!is.na(sample_preparation_method)) %>% pull()
      sample_staining_method_droplist <- droplist2 %>% select(sample_staining_method) %>% filter(!is.na(sample_staining_method)) %>% pull()
      sample_mounting_method_droplist <- droplist2 %>% select(sample_mounting_method) %>% filter(!is.na(sample_mounting_method)) %>% pull()
      sample_observation_method_droplist <- droplist2 %>% select(sample_observation_method) %>% filter(!is.na(sample_observation_method)) %>% pull()
      # tbl6
      person_role_droplist <- droplist2 %>% select(person_role) %>% filter(!is.na(person_role)) %>% pull()
      country_code_droplist <- droplist2 %>% select(country_code) %>% filter(!is.na(country_code)) %>% pull()
      country_droplist <- droplist2 %>% select(country) %>% filter(!is.na(country)) %>% pull()
      # organization_name_droplist <- droplist2 %>% select(organization_name) %>% filter(!is.na(organization_name)) %>% pull()


    extended_configs <- list(
      # Site table
      tbl3 = list(
        # network_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$network_label)), unique = TRUE, readOnly = TRUE),
        network_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        network_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
        country_code = list(type = 'dropdown', options = country_code_droplist, readOnly = TRUE),
        # site_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$site_label)), unique = TRUE, readOnly = TRUE),
        site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
        site_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
        latitude = list(type = 'numeric', required = TRUE, min_val = -90, max_val = 90, readOnly = TRUE), # calculated
        longitude = list(type = 'numeric', required = TRUE, min_val = -180, max_val = 180, readOnly = TRUE), # calculated
        elevation = list(type = 'numeric', required = TRUE, min_val = 0, max_val = 10000, readOnly = TRUE), # integer
        koppen_climate_value = list(type = 'dropdown', required = TRUE, options = koppen_climate_value_droplist, readOnly = TRUE), # calculated
        koppen_climate_code = list(type = 'dropdown', required = TRUE, options = koppen_climate_code_droplist, readOnly = TRUE),
        koppen_climate_classification = list(type = 'dropdown', options = koppen_climate_classification_droplist, readOnly = TRUE),
        site_aspect = list(type = 'numeric', min_val = 0, max_val = 360, regex_pattern = NULL, unique = TRUE), # integer
        site_slope = list(type = 'numeric', min_val = 0, max_val = NULL, regex_pattern = NULL, unique = TRUE), # integer
        site_topography = list(type = 'dropdown', options = site_topography_droplist),
        soil_depth = list(type = 'dropdown', options = soil_depth_droplist),
        soil_water_holding_capacity = list(type = 'dropdown', options = soil_water_holding_capacity_droplist),
        forest_stand_type = list(type = 'dropdown', options = forest_stand_type_droplist),
        forest_stand_structure = list(type = 'dropdown', options = forest_stand_structure_droplist),
        forest_stand_age = list(type = 'dropdown', options = forest_stand_age_droplist),
        forest_stand_main_species_composition = list(type = 'character', min_length = 1, max_length = 128, regex_pattern = NULL, unique = TRUE), # %in% ITRDB
        forest_stand_management_intensity = list(type = 'dropdown', options = forest_stand_management_intensity),
        in_stand_dendrometer_data = list(type = 'checkbox'),
        in_stand_sapflux_data = list(type = 'checkbox'),
        in_stand_phenological_observation = list(type = 'checkbox'),
        in_stand_weather_data = list(type = 'checkbox'),
        in_stand_soil_data = list(type = 'checkbox'),
        in_stand_other_data = list(type = 'character', min_length = 1, max_length = NULL),
        site_comment = list(type = 'character', min_length = 1, max_length = NULL)
      ),

      # Tree table
      tbl4 = list(
        # site_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$site_label)), unique = TRUE, readOnly = TRUE),
        site_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        # tree_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$tree_label)), unique = TRUE, readOnly = TRUE),
        tree_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
        tree_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
        # plot_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$plot_label)), unique = TRUE, readOnly = TRUE), # calculated
        plot_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        plot_code = list(type = 'character', required = TRUE, min_length = 1, max_length = 10, regex_pattern = NULL, readOnly = TRUE), # calculated
        tree_species = list(type = 'dropdown', required = TRUE, options = tree_species_droplist, readOnly = TRUE), # drop
        itrdb_species_code = list(type = 'dropdown', required = TRUE, options = c("PIAB", "LADE"), readOnly = TRUE),
        wood_type = list(type = 'dropdown', required = TRUE, options = wood_type_droplist,  readOnly = TRUE),
        leaf_habit = list(type = 'dropdown', required = TRUE, options = leaf_habit_droplist,  readOnly = TRUE),
        tree_ring_structure = list(type = 'dropdown', required = TRUE, options = tree_ring_structure_droplist,  readOnly = TRUE),
        tree_treatment = list(type = 'dropdown', required = TRUE, options = tree_treatment_droplist),
        tree_dbh = list(type = 'numeric', min_val = 0, max_val = 500),
        tree_height = list(type = 'numeric', min_val = 0, max_val = 100),
        tree_age = list(type = 'numeric', min_val = 0, max_val = 1000),
        tree_sex = list(type = 'dropdown', options = tree_sex_droplist),
        tree_social_status = list(type = 'dropdown', options = tree_social_status_droplist),
        tree_health_status = list(type = 'dropdown', options = tree_health_status_droplist),
        tree_origin = list(type = 'dropdown', options = tree_origin_droplist),
        tree_latitude = list(type = 'numeric', min_val = -90, max_val = 90),
        tree_longitude = list(type = 'numeric', min_val = -180, max_val = 180),
        on_tree_dendrometer_data = list(type = 'checkbox'),
        on_tree_sapflux_data = list(type = 'checkbox'),
        on_tree_phenological_observation = list(type = 'checkbox'),
        on_tree_weather_data = list(type = 'checkbox'),
        on_tree_shoot_growth_data = list(type = 'checkbox'),
        tree_ring_width_data = list(type = 'checkbox'),
        tree_ring_anatomical_data = list(type = 'checkbox'),
        tree_ring_isotope_data = list(type = 'checkbox'),
        number_of_samples = list(type = 'numeric', required = TRUE, min_val = 0, max_val = NULL), # calculated
        tree_comment = list(type = 'character', min_length = 1, max_length = NULL)
      ),

      # Sample table
      tbl5 = list(
        # tree_label = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$tree_label)), unique = TRUE, readOnly = TRUE), # calculated
        tree_label = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        # sample_id = list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$sample_id)), unique = TRUE, readOnly = TRUE), # calculated
        sample_id = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, readOnly = TRUE), # calculated
        sample_date = list(type = "date", required = TRUE, readOnly = TRUE), # calculated
        #sample_label	= list(type = "dropdown", required = TRUE, options = unique(na.omit(data_in$tbl2$sample_label)), unique = TRUE, readOnly = TRUE), # calculated
        sample_label	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL, unique = TRUE, readOnly = TRUE), # calculated
        sample_code	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, unique = TRUE, readOnly = TRUE), # calculated
        sample_organ	= list(type = 'dropdown', required = TRUE, options = sample_organ_droplist),
        sample_preparation_method	= list(type = 'dropdown', required = TRUE, options = sample_preparation_method_droplist),
        sample_staining_method	= list(type = 'dropdown', required = TRUE, options = sample_staining_method_droplist),
        sample_mounting_method	= list(type = 'dropdown', required = TRUE, options = sample_mounting_method_droplist),
        sample_observation_method	= list(type = 'dropdown', required = TRUE, options = sample_observation_method_droplist),
        sample_image_file_name	= list(type = 'character', min_length = 1, max_length = 128, regex_pattern = NULL),
        sample_section_archived	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL),
        sample_archived	= list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = NULL),
        sampling_height	= list(type = 'numeric', min_val = 0, max_val = 100),
        sample_apex_distance	= list(type = 'numeric', min_val = 0, max_val = 100),
        section_thickness	= list(type = 'numeric', min_val = 0, max_val = NULL),
        on_section_anatomical_data	= list(type = 'checkbox'),
        sample_comment	= list(type = 'character', min_length = 1, max_length = NULL)
      ),

      # Person table
      tbl6 = list(
        person_role = list(type = 'dropdown', required = TRUE, options = person_role_droplist),
        person_order = list(type = 'numeric', required = TRUE, min_val = 1, max_val = NULL),
        last_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        first_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        email = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, regex_pattern = "/^[a-zA-Z0-9. _%+-]+@[a-zA-Z0-9"),
        orcid = list(type = 'character', required = TRUE, min_length = 1, max_length = 19),# , regex_pattern = "^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]{1}$"
        organization_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 128), # drop # calculated
        research_organization_registry = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, "^\\+?[0-9 ()-]{7,20}$"), # drop # calculated
        organization_name_finder = NULL, # empty
        department = list(type = 'character', min_length = 1, max_length = 64),
        street = list(type = 'character', min_length = 1, max_length = 64),
        postal_code = list(type = 'character', min_length = 1, max_length = 64),
        city = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        country = list(type = 'dropdown', required = TRUE, options = country_droplist), # drop
        person_country_code = list(type = 'dropdown', required = TRUE, options = country_code_droplist), # calculated
        webpage = list(type = 'character', min_length = 1, max_length = 64, regex_pattern = "^https?://.+"),
        phone_number = list(type = 'character', min_length = 1, max_length = 15, regex_pattern = "^\\+?[0-9 ()-]{7,20}$")
      ),

      # Publication table
      tbl7 = list(
        first_author_last_name = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        title = list(type = 'character', required = TRUE, min_length = 1, max_length = NULL, regex_pattern = NULL),
        publication_year = list(type = 'numeric', required = TRUE, min_val = 1950, max_val = format(Sys.Date(), "%Y")),
        journal = list(type = 'character', required = TRUE, min_length = 1, max_length = 64),
        doi = list(type = 'character', required = TRUE, min_length = 1, max_length = 64, "^(10\\.\\d{4,9}/[-._;()/:A-Z0-9]+|10\\.1002/[^\\s]+)$", unique = TRUE)
      )

    )
    return(modifyList(base_configs, extended_configs))
      }
      
      # If WB_meta not available, return just base
      return(base_configs)
   })
    

    # TAB 3 observations: ---------------------------------------------------------------------

    #### dinfo  #### 
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
      site_info <- setNames(site_info, c("site_label", "latitude", "longitude", "elevation")) %>% 
        dplyr::mutate(
          elevation = as.integer(elevation)
        )
      dinfo(site_info)  # Store in reactive value
    })
    
    drop_species <- reactive({
      req(WB())  # Ensure the workbook is available
      drop_species <- openxlsx::readWorkbook(WB(), sheet = "DropList", colNames = TRUE) %>% 
        dplyr::mutate(itrdb_species_code = ITRDB_species_code) %>% 
        dplyr::select(tree_species, itrdb_species_code) %>%
        dplyr::filter(!is.na(tree_species))
    })
    
    #### dobs ####
    dobs <- reactiveVal()
    
    observe({
      req(input$obs_file)  # Ensure file is uploaded
      
      # Read data from Excel, skipping first 6 rows
      data <- openxlsx::readWorkbook(WB(), sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>%
        dplyr::tibble() %>% 
        dplyr::left_join(drop_species(), by = "tree_species") %>%
        dplyr::relocate(itrdb_species_code, .after = tree_species)
      
      # Check if sample_date is numeric and convert to Date
      data <- data %>%
        mutate(
          sample_date = case_when(
            # If sample_date is numeric (Excel date format)
            !is.na(sample_date) & is.numeric(as.numeric(sample_date)) ~
              as.Date(as.numeric(sample_date), origin = "1899-12-30"),
            
            TRUE ~ as.Date(NA)  # Handle NAs
          )
        ) %>% 
        dplyr::mutate(sample_date = as.character(sample_date))
      dobs(data)  # Store in reactive value
    })
    
    # INITALIZE REACTIVE INPUT DATA
    data_in <- reactiveValues()
    
    # Reactive context to store initial data and ensure it's updated in a proper context
    observe({
      data_in$tbl1 <- dinfo()  # Access dinfo in a valid reactive context
      data_in$tbl2 <- dobs()   # Access dobs in a valid reactive context
    })
    
    # RENDER TABLES
    output$tbl1 <- rhandsontable::renderRHandsontable({
      req(data_in$tbl1)  # Ensure data is available
      req(data_in$tbl2)
      column_configs <- column_configs()
      rhandsontable::rhandsontable(
        data_in$tbl1,
        rowHeaders = NULL, contextMenu = FALSE, stretchH = 'all') %>% # , overflow = 'visible'
        hot_col_wrapper('site_label', column_configs$tbl1$site_label) %>%
        hot_col_wrapper('latitude', column_configs$tbl1$latitude) %>%
        hot_col_wrapper('longitude', column_configs$tbl1$longitude) %>%
        hot_col_wrapper('elevation', column_configs$tbl1$elevation)
    })
    
    
    # # Read the initial data for Species families
    # observe({
    #   req(WB_meta())
    #   df <- openxlsx::readWorkbook(WB(), sheet = "DropList", colNames = TRUE) %>%
    #     dplyr::select(tree_species,	itrdb_species_code,	wood_type,	leaf_habit,	tree_ring_structure) %>%
    #     data.frame(stringsAsFactors = FALSE)
    #   species_family(df)
    # })
    
    # Function to synchronize Species data
    sync_species_itrdb <- function(df, drop_species, remove_na = TRUE) {
      if (remove_na) {
        df <- df %>%
          dplyr::filter(!is.na(tree_species) & tree_species != "")
      }
      
      updated <- df %>%
        dplyr::left_join(
          drop_species,
          by = "itrdb_species_code",
          suffix = c("", "_from_list")
        ) %>%
        dplyr::mutate(
          tree_species = tree_species_from_list
        ) %>%
        dplyr::select(-dplyr::ends_with("_from_list"))
      
      return(updated)
    }
    
    output$tbl2 <- rhandsontable::renderRHandsontable({
      req(data_in$tbl2)  # Ensure data is available
      column_configs <- column_configs()
      rhandsontable::rhandsontable(
        data_in$tbl2,
        rowHeaders = NULL, contextMenu = FALSE, stretchH = 'all', height = '300px') %>% # overflow = 'visible', 
        hot_col_wrapper('sample_date', column_configs$tbl2$sample_date) %>%
        hot_col_wrapper('sample_id', column_configs$tbl2$sample_id) %>%
        hot_col_wrapper('tree_species', column_configs$tbl2$tree_species) %>%
        hot_col_wrapper('itrdb_species_code', column_configs$tbl2$itrdb_species_code) %>%
        hot_col_wrapper('tree_label', column_configs$tbl2$tree_label) %>%
        hot_col_wrapper('plot_label', column_configs$tbl2$plot_label) %>%
        hot_col_wrapper('site_label', column_configs$tbl2$site_label) %>%
        hot_col_wrapper('network_label', column_configs$tbl2$network_label)%>%
        hot_col_wrapper('sample_label', column_configs$tbl2$sample_label) %>%
        hot_col_wrapper('radial_file', column_configs$tbl2$radial_file) %>%
        hot_col_wrapper('sample_comment', column_configs$tbl2$sample_comment) %>%
        hot_cols(manualColumnResize = TRUE)
    })

    # Sync data on user input
    observeEvent(input$tbl2, {
      req(input$tbl2)
      user_data <- hot_to_r(input$tbl2)
      updated_data <- sync_species_itrdb(user_data, drop_species())
      # Update the reactive data object
      data_in$tbl2 <- updated_data
    })

    
    #     output$testing1 <- renderPrint(
    #   rhandsontable::hot_to_r(input$tbl1)
    # )
    # 
    # output$testing2 <- renderPrint(
    #   rhandsontable::hot_to_r(input$tbl2)
    # )
    
    # TAB 4 site: -------------------------------------------------------------------

    #### dsite ####
    dsite <- reactiveVal()
    
    observe({
      req(input$meta_file)  # Ensure file is uploaded
      
      # Read the dataset
      site_meta_info <- openxlsx::readWorkbook(WB_meta(), sheet = "site", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
        dplyr::tibble()
      dsite(site_meta_info)  # Store in reactive value
    })
    
    # INITIALIZE REACTIVE INPUT DATA
    data_meta <- reactiveValues()
    
    # Reactive context to store initial data and ensure it's updated in a proper context
    observe({
      data_meta$tbl3 <- dsite()  # Access dsite in a valid reactive context
    })
    
    # Koppen_family
    koppen_family <- reactiveVal()
    
    # Read the initial data for Koppen families
    observe({
      req(WB_meta())
      df <- openxlsx::readWorkbook(WB_meta(), sheet = "DropList", colNames = TRUE) %>%
        dplyr::select(koppen_climate_value, koppen_climate_code, koppen_climate_classification) %>%
        dplyr::mutate(koppen_climate_value = as.character(koppen_climate_value)) %>%
        data.frame(stringsAsFactors = FALSE)
      koppen_family(df)
    })
    
    # Function to synchronize Koppen climate data
    sync_koppen_code <- function(df, koppen_family, remove_na = TRUE) {
      df <- df %>%
        dplyr::mutate(koppen_climate_value = as.character(koppen_climate_value))
      
      if (remove_na) {
        df <- df %>%
          dplyr::filter(!is.na(koppen_climate_value) & koppen_climate_value != "")
      }
      
      updated <- df %>%
        dplyr::left_join(
          koppen_family %>%
            dplyr::mutate(koppen_climate_value = as.character(koppen_climate_value)),
          by = "koppen_climate_value",
          suffix = c("", "_from_list")
        ) %>%
        dplyr::mutate(
          koppen_climate_code = koppen_climate_code_from_list,
          koppen_climate_classification = koppen_climate_classification_from_list
        ) %>%
        dplyr::select(-dplyr::ends_with("_from_list"))
      
      return(updated)
    }
    
    # RENDER TABLES
    output$tbl3 <- rhandsontable::renderRHandsontable({
      req(data_meta$tbl3)  # Ensure data is available
      req(!is.null(column_configs()$tbl3))  
      
      # Column configuration
      column_configs <- column_configs()
      
      rhandsontable::rhandsontable(
        data_meta$tbl3, 
        rowHeaders = NULL, contextMenu = FALSE, stretchH = 'all') %>%
        hot_col_wrapper('network_label', column_configs$tbl3$network_label) %>%
        hot_col_wrapper('network_code', column_configs$tbl3$network_code) %>%
        hot_col_wrapper('country_code', column_configs$tbl3$country_code) %>%
        hot_col_wrapper('site_label', column_configs$tbl3$site_label) %>%
        hot_col_wrapper('site_code', column_configs$tbl3$site_code) %>%
        hot_col_wrapper('latitude', column_configs$tbl3$latitude) %>%
        hot_col_wrapper('longitude', column_configs$tbl3$longitude) %>%
        hot_col_wrapper('elevation', column_configs$tbl3$elevation) %>%
        hot_col_wrapper('koppen_climate_value', column_configs$tbl3$koppen_climate_value) %>%
        hot_col_wrapper('koppen_climate_code', column_configs$tbl3$koppen_climate_code) %>%
        hot_col_wrapper('koppen_climate_classification', column_configs$tbl3$koppen_climate_classification) %>%
        hot_col_wrapper('site_aspect', column_configs$tbl3$site_aspect) %>%
        hot_col_wrapper('site_slope', column_configs$tbl3$site_slope) %>%
        hot_col_wrapper('site_topography', column_configs$tbl3$site_topography) %>%
        hot_col_wrapper('soil_depth', column_configs$tbl3$soil_depth) %>%
        hot_col_wrapper('soil_water_holding_capacity', column_configs$tbl3$soil_water_holding_capacity) %>%
        hot_col_wrapper('forest_stand_type', column_configs$tbl3$forest_stand_type) %>%
        hot_col_wrapper('forest_stand_structure', column_configs$tbl3$forest_stand_structure) %>%
        hot_col_wrapper('forest_stand_age', column_configs$tbl3$forest_stand_age) %>%
        hot_col_wrapper('forest_stand_main_species_composition', column_configs$tbl3$forest_stand_main_species_composition) %>%
        hot_col_wrapper('forest_stand_management_intensity', column_configs$tbl3$forest_stand_management_intensity) %>%
        hot_col_wrapper('in_stand_dendrometer_data', column_configs$tbl3$in_stand_dendrometer_data) %>%
        hot_col_wrapper('in_stand_sapflux_data', column_configs$tbl3$in_stand_sapflux_data) %>%
        hot_col_wrapper('in_stand_phenological_observation', column_configs$tbl3$in_stand_phenological_observation) %>%
        hot_col_wrapper('in_stand_weather_data', column_configs$tbl3$in_stand_weather_data) %>%
        hot_col_wrapper('in_stand_soil_data', column_configs$tbl3$in_stand_soil_data) %>%
        hot_col_wrapper('in_stand_other_data', column_configs$tbl3$in_stand_other_data) %>%
        hot_col_wrapper('site_comment', column_configs$tbl3$site_comment)
    })
    
    # Sync data on user input
    observeEvent(input$tbl3, {
      req(input$tbl3)
      user_data <- hot_to_r(input$tbl3)
      updated_data <- sync_koppen_code(user_data, koppen_family())
      # Update the reactive data object
      data_meta$tbl3 <- updated_data
    })
    
    
    
    
     # TAB 5 tree: -------------------------------------------------------------------

    #### dtree  ####
    dtree <- reactiveVal()

    observe({
      req(input$meta_file)  # Ensure file is uploaded

      # Read the dataset
      tree_meta_info <- openxlsx::readWorkbook(WB_meta(), sheet = "tree", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
         dplyr::tibble()
      dtree(tree_meta_info)  # Store in reactive value
    })

    # # INITALIZE REACTIVE INPUT DATA
    # data_meta <- reactiveValues()

    # Reactive context to store initial data and ensure it's updated in a proper context
    observe({
      data_meta$tbl4 <- dtree()  # Access dinfo in a valid reactive context

    })

    # Species_family
    species_family <- reactiveVal()
    
    # Read the initial data for Species families
    observe({
      req(WB_meta())
      df <- openxlsx::readWorkbook(WB_meta(), sheet = "DropList", colNames = TRUE) %>%
        dplyr::select(tree_species,	itrdb_species_code,	wood_type,	leaf_habit,	tree_ring_structure) %>%
        data.frame(stringsAsFactors = FALSE)
      species_family(df)
    })
    
    # Function to synchronize Species data
    sync_species_code <- function(df, species_family, remove_na = TRUE) {
      if (remove_na) {
        df <- df %>%
          dplyr::filter(!is.na(tree_species) & tree_species != "")
      }
      
      updated <- df %>%
        dplyr::left_join(
          species_family,
          by = "tree_species",
          suffix = c("", "_from_list")
        ) %>%
        dplyr::mutate(
          itrdb_species_code = itrdb_species_code_from_list,	
          wood_type = wood_type_from_list,
          leaf_habit = leaf_habit_from_list,
          tree_ring_structure = tree_ring_structure_from_list
        ) %>%
        dplyr::select(-dplyr::ends_with("_from_list"))
      
      return(updated)
    }

        # RENDER TABLES
    output$tbl4 <- rhandsontable::renderRHandsontable({
      req(data_meta$tbl4)  # Ensure data is available
      req(!is.null(column_configs()$tbl4))  # Ensure tbl3 config exists
      column_configs <- column_configs()
      rhandsontable::rhandsontable(
        data_meta$tbl4,
        rowHeaders = NULL, contextMenu = FALSE, stretchH = 'all') %>%
        hot_col_wrapper('site_label', column_configs$tbl4$site_label) %>%
        hot_col_wrapper('tree_label', column_configs$tbl4$tree_label) %>%
        hot_col_wrapper('tree_code', column_configs$tbl4$tree_code) %>%
        hot_col_wrapper('plot_label', column_configs$tbl4$plot_label) %>%
        hot_col_wrapper('plot_code', column_configs$tbl4$plot_code) %>%
        hot_col_wrapper('tree_species', column_configs$tbl4$tree_species) %>%
        hot_col_wrapper('itrdb_species_code', column_configs$tbl4$itrdb_species_code) %>%
        hot_col_wrapper('wood_type', column_configs$tbl4$wood_type) %>%
        hot_col_wrapper('leaf_habit', column_configs$tbl4$leaf_habit) %>%
        hot_col_wrapper('tree_ring_structure', column_configs$tbl4$tree_ring_structure) %>%
        hot_col_wrapper('tree_treatment', column_configs$tbl4$tree_treatment) %>%
        hot_col_wrapper('tree_dbh', column_configs$tbl4$tree_dbh) %>%
        hot_col_wrapper('tree_height', column_configs$tbl4$tree_height) %>%
        hot_col_wrapper('tree_age', column_configs$tbl4$tree_age) %>%
        hot_col_wrapper('tree_sex',column_configs$tbl4$tree_sex) %>% 
        hot_col_wrapper('tree_social_status', column_configs$tbl4$tree_social_status) %>%
        hot_col_wrapper('tree_health_status', column_configs$tbl4$tree_health_status) %>%
        hot_col_wrapper('tree_origin', column_configs$tbl4$tree_origin) %>%
        hot_col_wrapper('tree_latitude', column_configs$tbl4$tree_latitude) %>% 
        hot_col_wrapper('tree_longitude', column_configs$tbl4$tree_longitude) %>%
        hot_col_wrapper('on_tree_dendrometer_data', column_configs$tbl4$on_tree_dendrometer_data) %>%
        hot_col_wrapper('on_tree_sapflux_data', column_configs$tbl4$on_tree_sapflux_data) %>%
        hot_col_wrapper('on_tree_phenological_observation', column_configs$tbl4$on_tree_phenological_observation) %>%
        hot_col_wrapper('on_tree_weather_data', column_configs$tbl4$on_tree_weather_data) %>%
        hot_col_wrapper('on_tree_shoot_growth_data', column_configs$tbl4$on_tree_shoot_growth_data) %>%
        hot_col_wrapper('tree_ring_width_data', column_configs$tbl4$tree_ring_width_data) %>%
        hot_col_wrapper('tree_ring_anatomical_data', column_configs$tbl4$tree_ring_anatomical_data) %>%
        hot_col_wrapper('tree_ring_isotope_data', column_configs$tbl4$tree_ring_isotope_data) %>%
        hot_col_wrapper('tree_comment', column_configs$tbl4$tree_comment)
    })
    
    # Sync data on user input
    observeEvent(input$tbl4, {
      req(input$tbl4)
      user_data <- hot_to_r(input$tbl4)
      updated_data <- sync_species_code(user_data, species_family())
      # Update the reactive data object
      data_meta$tbl4 <- updated_data
    })
    
    
    # TAB 6 sample: -------------------------------------------------------------------
    
    #### dsample  ####
    dsample <- reactiveVal()
    
    observe({
      req(input$meta_file)  # Ensure file is uploaded
      
      # Read the dataset
      sample_meta_info <- openxlsx::readWorkbook(WB_meta(), sheet = "sample", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
        dplyr::tibble()
      
      # Check if sample_date is numeric and convert to Date
      sample_meta_info <- sample_meta_info %>%
        mutate(
          sample_date = case_when(
            # If sample_date is numeric (Excel date format)
            !is.na(sample_date) & is.numeric(as.numeric(sample_date)) ~
              as.Date(as.numeric(sample_date), origin = "1899-12-30"),
            
            TRUE ~ as.Date(NA)  # Handle NAs
          )
        ) %>% 
        dplyr::mutate(sample_date = as.character(sample_date))
      
      dsample(sample_meta_info)  # Store in reactive value
    })
    
    # # INITALIZE REACTIVE INPUT DATA
    # data_meta <- reactiveValues()
    
    # Reactive context to store initial data and ensure it's updated in a proper context
    observe({
      data_meta$tbl5 <- dsample()  # Access dinfo in a valid reactive context
    })
    
    # RENDER TABLES
    output$tbl5 <- rhandsontable::renderRHandsontable({
      req(data_meta$tbl5)  # Ensure data is available
      req(!is.null(column_configs()$tbl5))  # Ensure tbl3 config exists
      column_configs <- column_configs()
      rhandsontable::rhandsontable(
        data_meta$tbl5,
        rowHeaders = NULL, contextMenu = FALSE, stretchH = 'all') %>%
        hot_col_wrapper('tree_label', column_configs$tbl5$tree_label) %>%
        hot_col_wrapper('sample_id', column_configs$tbl5$sample_id) %>%
        hot_col_wrapper('sample_date', column_configs$tbl5$sample_date) %>%
        hot_col_wrapper('sample_label', column_configs$tbl5$sample_label) %>%
        hot_col_wrapper('sample_code', column_configs$tbl5$sample_code) %>%
        hot_col_wrapper('sample_organ', column_configs$tbl5$sample_organ) %>%
        hot_col_wrapper('sample_preparation_method', column_configs$tbl5$sample_preparation_method) %>%
        hot_col_wrapper('sample_staining_method', column_configs$tbl5$sample_staining_method) %>%
        hot_col_wrapper('sample_mounting_method', column_configs$tbl5$sample_mounting_method) %>%
        hot_col_wrapper('sample_observation_method', column_configs$tbl5$sample_observation_method) %>%
        hot_col_wrapper('sample_image_file_name', column_configs$tbl5$sample_image_file_name) %>%
        hot_col_wrapper('sample_section_archived', column_configs$tbl5$sample_section_archived) %>%
        hot_col_wrapper('sample_archived', column_configs$tbl5$sample_archived) %>%
        hot_col_wrapper('sampling_height', column_configs$tbl5$sampling_height) %>%
        hot_col_wrapper('sample_apex_distance', column_configs$tbl5$sample_apex_distance) %>%
        hot_col_wrapper('section_thickness', column_configs$tbl5$section_thickness) %>%
        hot_col_wrapper('on_section_anatomical_data', column_configs$tbl5$on_section_anatomical_data) %>%
        hot_col_wrapper('sample_comment', column_configs$tbl5$sample_comment)
    }) 
    
        # TAB 7 person: -------------------------------------------------------------------
    
    clear_author_fields <- function(session) {
      updateSelectInput(session, "person_role", selected = "")
      updateNumericInput(session, "person_order", value = NA)
      updateTextInput(session, "last_name", value = "")
      updateTextInput(session, "first_name", value = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "orcid", value = "")
      updateTextInput(session, "organization_name", value = "")
      updateTextInput(session, "research_organization_registry", value = "")
      updateTextInput(session, "organization_name_finder", value = "")
      updateTextInput(session, "department", value = "")
      updateTextInput(session, "street", value = "")
      updateTextInput(session, "postal_code", value = "")
      updateTextInput(session, "city", value = "")
      updateTextInput(session, "country", value = "")
      updateTextInput(session, "person_country_code", value = "")
      updateTextInput(session, "webpage", value = "")
      updateTextInput(session, "phone_number", value = "")
      
      # Also clear the ORCID search fields
      updateTextInput(session, "first_name_search", value = "")
      updateTextInput(session, "last_name_search", value = "")
      updateTextInput(session, "orcid_search", value = "")
      
      # Clear the ROR search fields
      updateTextInput(session, "ror_search", value = "")
      updateTextInput(session, "ror_name_search", value = "")
      updateTextInput(session, "ror_id_search", value = "")
    }
    
    form_visible <- reactiveVal(FALSE)
    edit_mode <- reactiveVal(FALSE)
    selected_row <- reactiveVal(NULL)
    
    
    # Show form when Insert new Author is clicked
    observeEvent(input$show_add_author, {
      form_visible(TRUE)
    })
    
    output$form_visible <- renderText({
      as.character(form_visible())
    })
    
    outputOptions(output, "form_visible", suspendWhenHidden = FALSE)
    
    # Color field red or green
    observe({
      fields <- c("person_role", "last_name", "first_name", "email", "orcid", 
                  "organization_name", "research_organization_registry", 
                  "organization_name_finder", "department", "street", 
                  "postal_code", "city", "country", "person_country_code", 
                  "webpage", "phone_number")
      
      lapply(fields, function(field) {
        # Proceed only if input[[field]] exists and is not NULL
        if (!is.null(input[[field]]) && is.character(input[[field]])) {
          border_color <- if (input[[field]] == "") "red" else "green"
          shinyjs::runjs(sprintf('$("#%s").css("border", "2px solid %s");', field, border_color))
        }
      })
    })
    
    #### dperson  ####
    dperson <- reactiveVal()
    data_meta <- reactiveValues(tbl6 = NULL)
    
    observe({
      req(input$meta_file)
      person_meta_info <- openxlsx::readWorkbook(WB_meta(), sheet = "person", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
        dplyr::tibble() %>%
        dplyr::rename(organization_name_finder = `organization_name.(finder)`)
      dperson(person_meta_info)  # Store in reactive value
    })
    
     # Reactive context to store initial data and ensure it's updated in a proper context
    observe({
      req(dperson())
      data_meta$tbl6 <- dperson()
    })
    
    # RENDER TABLES
    output$tbl6 <- rhandsontable::renderRHandsontable({
      req(data_meta$tbl6)  # Ensure data is available
      data_meta$tbl6$person_order <- seq_len(nrow(data_meta$tbl6))
      column_configs <- column_configs()
      rhandsontable::rhandsontable(
        data_meta$tbl6,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = 'all', selectCallback = TRUE) %>%
        hot_col_wrapper('person_role', column_configs$tbl6$person_role) %>%
        hot_col_wrapper('person_order', column_configs$tbl6$person_order) %>%
        hot_col_wrapper('last_name', column_configs$tbl6$last_name) %>%
        hot_col_wrapper('first_name', column_configs$tbl6$first_name) %>%
        hot_col_wrapper('email', column_configs$tbl6$email) %>%
        hot_col_wrapper('orcid', column_configs$tbl6$orcid) %>%
        hot_col_wrapper('organization_name', column_configs$tbl6$organization_name) %>%
        hot_col_wrapper('research_organization_registry', column_configs$tbl6$research_organization_registry) %>%
        # hot_col_wrapper('organization_name_finder', column_configs$tbl6$organization_name_finder) %>%
        hot_col_wrapper('department', column_configs$tbl6$department) %>%
        hot_col_wrapper('street', column_configs$tbl6$street) %>%
        hot_col_wrapper('postal_code', column_configs$tbl6$postal_code) %>%
        hot_col_wrapper('city', column_configs$tbl6$city) %>%
        hot_col_wrapper('country', column_configs$tbl6$country) %>%
        hot_col_wrapper('person_country_code', column_configs$tbl6$person_country_code) %>%
        hot_col_wrapper('webpage', column_configs$tbl6$webpage) %>%
        hot_col_wrapper('phone_number', column_configs$tbl6$phone_number)
    }) 
    
    observeEvent(input$tbl6_select$select$r, {
      selected_row(input$tbl6_select$select$r)
    })
    
    observeEvent(input$update_author, {
      req(selected_row())
      
      row <- selected_row()
      form_visible(TRUE)
      edit_mode(TRUE)
      
      isolate({
        fields <- c("person_role", "last_name", "first_name", "email", "orcid", 
                    "organization_name", "research_organization_registry", 
                    "organization_name_finder", "department", "street", 
                    "postal_code", "city", "country", "person_country_code", 
                    "webpage", "phone_number")
        
        for (field in fields) {
          value <- data_meta$tbl6[[field]][row]
          
          # Defensive conversion if needed
          if (is.na(value) || is.null(value)) value <- ""
          if (!is.character(value)) value <- as.character(value)
          
          updateTextInput(session, field, value = value)
        }
      })
    })
    
    observeEvent(input$add_author, {
      new_entry <- data.frame(
        person_role = as.character(input$person_role),
        person_order = input$person_order,
        last_name = as.character(input$last_name),
        first_name = as.character(input$first_name),
        email = as.character(input$email),
        orcid = as.character(input$orcid),
        organization_name = as.character(input$organization_name),
        research_organization_registry = as.character(input$research_organization_registry),
        organization_name_finder = as.character(input$organization_name_finder),
        department = as.character(input$department),
        street = as.character(input$street),
        postal_code = as.character(input$postal_code),
        city = as.character(input$city),
        country = as.character(input$country),
        person_country_code = as.character(input$person_country_code),
        webpage = as.character(input$webpage),
        phone_number = as.character(input$phone_number),
        stringsAsFactors = FALSE
      )
      
      if (edit_mode() && !is.null(selected_row())) {
        data_meta$tbl6[selected_row(), names(new_entry)] <- as.list(new_entry[1, ])
      } else {
        data_meta$tbl6 <- rbind(data_meta$tbl6, new_entry)
      }
      
      clear_author_fields(session)
      form_visible(FALSE)
      updateActionButton(session, "show_add_author", label = "Insert new Author")
      
      edit_mode(FALSE)
      selected_row(NULL)
    })
    
    observeEvent(input$delete_author, {
      row <- selected_row()
      
      if (!is.null(row) && row <= nrow(data_meta$tbl6)) {
        data_meta$tbl6 <- data_meta$tbl6[-row, ]
        selected_row(NULL)
        
        showNotification("Author deleted.", type = "message")
      } else {
        showNotification("Please select a row to delete.", type = "warning")
      }
      
      data_meta$tbl6$person_order <- seq_len(nrow(data_meta$tbl6))
      
      clear_author_fields(session)
      form_visible(FALSE)
      edit_mode(FALSE)
    })
    
    observe({
      shinyjs::toggleState("update_author", condition = !is.null(selected_row()))
      shinyjs::toggleState("delete_author", condition = !is.null(selected_row()))
    })
    
    observeEvent(input$apply_order, {
      req(input$tbl6)
      if (!is.null(input$tbl6)) {
        ordered_data <- hot_to_r(input$tbl6)
        ordered_data$person_order <- as.numeric(ordered_data$person_order)
        data_meta$tbl6 <- ordered_data[order(ordered_data$person_order), ]
      }
    })
    
    # ORCID Search
    orcid_data <- reactiveValues(results = NULL)
    
    # Search via Orcid
    observeEvent(input$search_orcid, {
      # If ORCID ID is provided, use that
      if (nzchar(input$orcid_search)) {
        orcid_id <- gsub("https?://orcid.org/", "", trimws(input$orcid_search))
        orcid_url <- paste0("https://pub.orcid.org/v3.0/", orcid_id)
        
        res <- httr::GET(orcid_url, httr::add_headers(Accept = "application/json"))
        
        if (httr::status_code(res) == 200) {
          parsed <- jsonlite::fromJSON(rawToChar(res$content))
          
          updateTextInput(session, "orcid", value = orcid_id)
          updateTextInput(session, "last_name", value = parsed$`person`$`name`$`family-name`$value)
          updateTextInput(session, "first_name", value = parsed$`person`$`name`$`given-names`$value)
          
          email_val <- tryCatch(parsed$`person`$emails$email[[1]]$email, error = function(e) "")
          updateTextInput(session, "email", value = email_val)
          
          if (!is.null(parsed$`activities-summary`$`employments`$`employment-summary`)) {
            org_name <- parsed$`activities-summary`$`employments`$`employment-summary`[[1]]$`organization`$`name`
            updateTextInput(session, "organization_name", value = org_name)
          }
          
        } else {
          showNotification("Failed to retrieve data for provided ORCID ID.", type = "error")
        }
        
        # Else if names are provided, do a name-based search
      } else if (nzchar(input$first_name_search) && nzchar(input$last_name_search)) {
        given <- trimws(input$first_name_search)
        family <- trimws(input$last_name_search)
        query_name <- URLencode(paste0("given-names:", given, " AND family-name:", family))
        search_url <- sprintf("https://pub.orcid.org/v3.0/expanded-search?q=%s", query_name)
        
        res <- httr::GET(search_url, httr::add_headers(Accept = "application/json"))
        
        if (httr::status_code(res) == 200) {
          parsed <- jsonlite::fromJSON(rawToChar(res$content))
          if (!is.null(parsed$`expanded-result`) && nrow(parsed$`expanded-result`) > 0) {
            results <- tibble::tibble(
              ORCID = parsed$`expanded-result`$`orcid-id`,
              LastName = parsed$`expanded-result`$`family-names`,
              FirstName = parsed$`expanded-result`$`given-names`,
              Email = if (!is.null(parsed$`expanded-result`$email) && length(parsed$`expanded-result`$email) > 0) paste(parsed$`expanded-result`$email[[1]], collapse = ", ") else "",
              Organization = if (!is.null(parsed$`expanded-result`$`institution-name`) && length(parsed$`expanded-result`$`institution-name`) > 0) paste(parsed$`expanded-result`$`institution-name`[[1]], collapse = ", ") else ""
            )
            
            updateTextInput(session, "orcid", value = results$ORCID[1])
            updateTextInput(session, "last_name", value = results$LastName[1])
            updateTextInput(session, "first_name", value = results$FirstName[1])
            updateTextInput(session, "email", value = results$Email[1])
            updateTextInput(session, "organization_name", value = results$Organization[1])
            
            orcid_data$results <- results
          } else {
            showNotification("No ORCID results found for that name.", type = "message")
          }
        } else {
          showNotification("ORCID name search failed.", type = "error")
        }
        
      } else {
        showNotification("Please provide either an ORCID ID or both first and last name.", type = "warning")
      }
    })
    
    # ROR Search
    ror_data <- reactiveValues(results = NULL)
    
    observeEvent(input$search_ror, {
      req(input$country_code)
      req(input$search_string)
      
      search_url <- sprintf(
        'https://api.ror.org/v2/organizations?query=%s&filter=country.country_code:%s',
        URLencode(input$search_string),
        input$country_code)
      
      ror_res <- httr::GET(search_url, httr::timeout(5))
      
      if (httr::status_code(ror_res) == 200) {
        json <- jsonlite::fromJSON(rawToChar(ror_res$content))
        
        if (json$number_of_results > 0) {
          res_names <- json$items$names %>%
            dplyr::bind_rows() %>%
            filter(grepl('ror_display', types)) %>%
            dplyr::pull(value)
          
          res_locs <- json$items$locations %>% 
            dplyr::bind_rows() %>% 
            dplyr::pull(geonames_details) %>% 
            tidyr::unite(col = 'address', name, country_name, sep = ', ') %>% 
            dplyr::pull(address)

          
          res_web <- json$items$links %>%
            map_dfr(bind_rows) %>%
            filter(type == "website") %>%
            pull(value)
          
          res_df <- data.frame(
            ROR = json$items$id, 
            Name = res_names, 
            Location = res_locs,
            Website = res_web
          )
          
          # STORE into the reactiveValues object
          ror_data$results <- res_df |>
            tidyr::separate(Location, into = c("city", "country"), sep = ", ", remove = FALSE)

          
          output$ror_results <- DT::renderDT({
            DT::datatable(res_df, rownames = FALSE)
          })
        }
        else {
          showNotification("No ROR results found. Try again.", type = "message")
        }
      } else {
        showNotification("ROR API request failed. Try again.", type = "error")
      }
      
    })
    
    # Handle row selection from ROR results
    observeEvent(input$ror_results_rows_selected, {
      req(input$ror_results_rows_selected)
      sel <- input$ror_results_rows_selected
      ror_df <- ror_data$results
      ror_row <- ror_df[sel, ]
      
      # insert results into their corresponding fields
      updateTextInput(session, "organization_name", value = ror_row$Name[1])
      updateTextInput(session, "research_organization_registry", value = ror_row$ROR[1])
      updateTextInput(session, "city", value = ror_row$Location[1])
      updateTextInput(session, "country", value = ror_row$Location[1])
      updateTextInput(session, "person_country_code", value = countrycode::countrycode(ror_row$Location[1], origin = "country.name", destination = "iso2c"))
      updateTextInput(session, "webpage", value = ror_row$Website[1])
      
      
    })
    
    
    # TAB 8 publication: -------------------------------------------------------------------

    # Reactive values to store citation and metadata
    data_meta <- reactiveValues(
      last_doi_citation = NULL,
      last_doi_metadata = NULL,
      tbl7 = NULL
    )
    
    # On Search: Fetch citation and metadata
    observeEvent(input$search_doi, {
      req(input$doi_input)
      doi_input <- URLencode(input$doi_input)
      
      # Citation string (APA style)
      citation_url <- sprintf("https://citation.doi.org/format?doi=%s&style=apa&lang=en-US", doi_input)
      citation_res <- httr::GET(citation_url, httr::timeout(5))
      
      # Metadata JSON
      metadata_url <- sprintf("https://citation.doi.org/metadata?doi=%s", doi_input)
      metadata_res <- httr::GET(metadata_url, httr::timeout(5))
      
      output$doi_result <- renderText({
        if (httr::status_code(citation_res) == 200) {
          citation_text <- httr::content(citation_res, as = "text", encoding = "UTF-8")
          data_meta$last_doi_citation <- citation_text
          citation_text
        } else {
          paste0("Error while retrieving citation: ", httr::status_code(citation_res))
        }
      })
      
      # Store metadata silently
      if (httr::status_code(metadata_res) == 200) {
        meta_data <- tryCatch({
          jsonlite::fromJSON(httr::content(metadata_res, as = "text", encoding = "UTF-8"))
        }, error = function(e) NULL)
        data_meta$last_doi_metadata <- meta_data
      } else {
        data_meta$last_doi_metadata <- NULL
      }
    })
    
    # Add publication using metadata
    observeEvent(input$add_publication, {
      meta <- data_meta$last_doi_metadata
      req(meta)
      
      # Safe extraction with fallback to empty string
      get_safe <- function(x, default = "") {
        if (!is.null(x) && length(x) > 0) x else default
      }
      # str(meta)
      # print(str(meta$author))
      # str(meta$DOI)
      # Only try extracting author if `meta` is a list and has `author`
      author_last <- tryCatch({
        if (is.list(meta) && !is.null(meta$author) && length(meta$author) > 0) {
          get_safe(meta$author$family[[1]])
        } else {
          ""
        }
      }, error = function(e) "")
      
      new_pub <- data.frame(
        first_author_last_name = author_last,
        title = get_safe(meta$title),
        publication_year = as.character(get_safe(meta$issued$`date-parts`[[1]][1])),
        journal = get_safe(meta$`container-title`),
        doi = as.character(get_safe(meta$DOI)),
        stringsAsFactors = FALSE
      )
      
      if (is.null(data_meta$tbl7)) {
        data_meta$tbl7 <- new_pub
      } else {
        data_meta$tbl7 <- dplyr::bind_rows(data_meta$tbl7, new_pub)
      }
    })
    
    #### dpublication ####
    dpublication <- reactiveVal()
    data_meta <- reactiveValues(tbl7 = NULL, last_doi_citation = NULL)
    
    # Read and store publication metadata from uploaded Excel file
    observe({
      req(input$meta_file)
      publication_meta_info <- openxlsx::readWorkbook(WB_meta(), sheet = "publication", startRow = 1, colNames = TRUE)[-(1:6), ] %>%
        dplyr::tibble()
      dpublication(publication_meta_info)
    })
    
    # Sync dpublication into data_meta
    observeEvent(dpublication(), {
      data_meta$tbl7 <- dpublication()
    })
    

    # Render editable publication table
    output$tbl7 <- rhandsontable::renderRHandsontable({
      req(data_meta$tbl7, column_configs)
      column_configs <- column_configs()
      
      rhandsontable::rhandsontable(
        data_meta$tbl7,
        rowHeaders = NULL, contextMenu = TRUE, stretchH = 'all'
      ) %>%
        hot_col_wrapper('first_author_last_name', column_configs$tbl7$first_author_last_name) %>%
        hot_col_wrapper('title', column_configs$tbl7$title) %>%
        hot_col_wrapper('publication_year', column_configs$tbl7$publication_year) %>%
        hot_col_wrapper('journal', column_configs$tbl7$journal) %>%
        hot_col_wrapper('doi', column_configs$tbl7$doi)
    })
    
    
    
    
    
  } # end of server function
  
  
  shinyApp(ui, server)



}
