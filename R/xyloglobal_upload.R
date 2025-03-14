#' GloboXylo Shiny App: Upload Observation and Metadata Data
#'
#' This Shiny application facilitates the upload of observation and metadata files,
#' visualizes the data using maps and plots, and includes validation checks for data consistency.
#'
#' @import shiny bslib readxl writexl openxlsx leaflet plotly dplyr reactable
#' @importFrom shinyjs useShinyjs
#' @export
#'
#' @examples
#' \dontrun{
#' xyloglobal_upload()
#' }
xyloglobal_upload <- function() {
  ui <- shiny::fluidPage(
    
    theme = bslib::bs_theme(primary = "#006268", font_scale = 0.8, bootswatch = "yeti"),
    shinyjs::useShinyjs(),
    shiny::titlePanel("GloboXylo: Contributing Data"),
    
    navset_card_tab(id = 'tabs',
                         
                         # TAB 1: Upload Observation Data -----------------------------------------
                         nav_panel(title = "Upload observation data",
                                         
                                         shiny::fluidRow(
                                           # Left Side: Upload Section (With Background Color)
                                           shiny::column(3, class = "bg-light p-3 border-end",  
                                                         bslib::card(
                                                           bslib::card_header('Open observation data template'),
                                                           bslib::card_body(
                                                             fillable = FALSE,
                                                             shiny::p("Click the button to open the template. Fill it in, save it, then browse and load the file."),
                                                             shiny::actionButton("open_obs_temp", "Click to open data template!", class = "btn btn-primary"),
                                                             shiny::textOutput("obs_file_copied")
                                                           )
                                                         ),
                                                         
                                                         bslib::card(
                                                           bslib::card_header('Load the file you just saved!'),
                                                           shiny::fileInput("obs_file", "", accept = c(".xlsx"), multiple = FALSE),
                                                           shiny::selectInput("site_filter", "Select Site", choices = NULL, selected = NULL, selectize = TRUE),                                                            style = "height: 300px"
                                                         ),
                                                         # Add a ReactTable of key info below the cards
                                                         bslib::card(
                                                           bslib::card_header("Key Information Table"),
                                                           bslib::card_body(
                                                             reactable::reactableOutput("key_info_table")
                                                           )
                                                         )
                                           ),
                                           
                                           # Right Side: Map & Plotly + Add Observations Table here
                                           shiny::column(9,  
                                                         bslib::card(
                                                           bslib::card_header("Geolocation Map"),
                                                           bslib::card_body(
                                                             leaflet::leafletOutput("mymap", height = "400px")
                                                           )
                                                         ),
                                                         
                                                         bslib::card(
                                                           bslib::card_header("Data Coverage Overview"),
                                                           bslib::card_body(
                                                             plotly::plotlyOutput("data_coverage_plot", height = "300px")
                                                           )
                                                         ),
                                                         
                                                         # Move Observations performed section here
                                                         bslib::card(
                                                           bslib::card_header("Observations performed [number of radial files]"),
                                                           bslib::card_body(
                                                             reactable::reactableOutput("obs_table")
                                                           )
                                                         )
                                           )
                                         ),
                                         
                                         shiny::br(),
                                         
                                         # Adding validation checkboxes and next button
                                         shiny::fluidRow(
                                           shiny::column(12,
                                                         bslib::card(
                                                           bslib::card_header("Data Validation"),
                                                           bslib::card_body(
                                                             shiny::checkboxInput("validate_location", "Validate Location", value = FALSE),
                                                             shiny::checkboxInput("validate_data_coverage", "Validate Data Coverage", value = FALSE),
                                                             shiny::checkboxInput("validate_observation", "Validate observation list", value = FALSE),
                                                             shiny::textOutput("validation_status"),
                                                             shiny::actionButton('next_btn', 'Next', icon = shiny::icon('angle-double-right'), class = "btn btn-primary", disabled = TRUE)
                                                           )
                                                         )
                                           )
                                         )
                         ),
                         
                         # TAB 2: Upload Metadata -----------------------------------------------
                         nav_panel("Upload metadata",
                                         
                                         shiny::fluidRow(
                                           # Left side
                                           shiny::column(3, class = "bg-light p-3 border-end",  
                                                         bslib::card(
                                                           bslib::card_header('Open metadata template'),
                                                           bslib::card_body(
                                                             fillable = FALSE,
                                                             shiny::p("Click to open the template, prefilled with data from the observations file."),
                                                             shiny::p("Complete it, save it, then browse and load the file."),
                                                             shiny::p("Note: It may take a few seconds for the template to open.", style = "color: red;"),
                                                             shiny::actionButton("open_meta_temp", "Click to open meta template!", class = "btn btn-primary"),
                                                             shiny::textOutput("meta_file_copied")
                                                           )
                                                         ),
                                                         
                                                         bslib::card(
                                                           bslib::card_header('Load the file you just saved!'),
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
                                                         bslib::card(
                                                           bslib::card_header("Metadata Overview"),
                                                           bslib::card_body(
                                                             plotly::plotlyOutput("hierarchical_structure", height = "400px")
                                                           ),
                                                           bslib::card_body(
                                                             reactable::reactableOutput("meta_table")  # ReactTable placeholder
                                                           )
                                                         ),
                                                         bslib::card(
                                                           bslib::card_header("Format_validation report"),
                                                           bslib::card_body(
                                                             reactable::reactableOutput("validation_table"),  
                                                             shiny::uiOutput("validation_message") 
                                                           )
                                                         )
                                           ),
                                         ),
                                         
                                         shiny::br(),
                                         
                                         shiny::fluidRow(
                                           shiny::column(12,
                                                         bslib::card(
                                                           class = "border border-0 text-center",
                                                           shiny::actionButton('submit_btn', 'Submit', disabled = TRUE, icon = shiny::icon('angle-double-right'), class = "btn btn-primary")
                                                         )
                                           )
                                         )
                         )
    )
  )
  

  server <- function(input, output, session) {
    
    # TAB 1: ---------------------------------------------------------------------
    
    # Create and open copy of the obs template file
    tempdir_path <- tempdir()
    
    # upload TEMPLATE for OBSERVATION FILE 
    observeEvent(input$open_obs_temp, {
      template_path <- system.file("extdata", "Datasetname_xylo_data_yyyy-mm-dd.xlsx", package = "xyloR")
      obs_path_temporary <- file.path(tempdir_path, paste0("Datasetname_xylo_data_", Sys.Date(), ".xlsx"))
      obs_template <- openxlsx::loadWorkbook(template_path) # load the template
      openxlsx::saveWorkbook(obs_template, obs_path_temporary, overwrite = TRUE) # save in tempdir
      
      # Open xlsx based on OS
      tryCatch({
        if (.Platform$OS.type == "windows") {
          system2("cmd", c("/c", "start", shQuote(obs_path_temporary)), wait = FALSE)
        } else if (Sys.info()["sysname"] == "Darwin") {
          system(paste("open", shQuote(obs_path_temporary)))
        } else {
          system(paste("xdg-open", shQuote(obs_path_temporary)))
        }
      },
      error = function(e) {
        shiny::showNotification("File could not be opened!", type = "error")
      })
    })
    
    # print the tempdir() path just in case
    output$obs_file_copied <- renderText({
      req(input$open_obs_temp)
      paste0("File opened in temporary dir ", tempdir_path,
             " . Clicking the button again will overwrite the file, so be sure to save it to a different location!")
    })
    
    # upload FILLED OBSERVATION FILE and RENDER INFORMATION
    
    # get possible sites in file to fill selectInput$site_filter dropdown
    observe({
      req(input$obs_file)  # Ensure a file is uploaded
      
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)  # Load the workbook
      
      # Read site information from the "obs_data_info" sheet
      site_info <- openxlsx::readWorkbook(wb, sheet = "obs_data_info", startRow = 6, colNames = FALSE) %>% 
        setNames(c("site_label", "latitude", "longitude", "elevation"))
      
      # Extract unique site labels and update the dropdown
      updateSelectInput(session, "site_filter", choices = unique(site_info$site_label))
    })
    
    # Reactive xylo_obs data
    xylo_obs <- reactive({
      req(input$obs_file)
      req(input$site_filter)
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)
      df <- openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 1)[-(1:6), ] %>% 
        dplyr::tibble() %>%
        dplyr::mutate(sample_date = as.Date(as.numeric(sample_date), origin = "1899-12-30")) %>%
        dplyr::filter(!is.na(sample_date))
      
      # Filter by site_code if selected
      df <- df %>% dplyr::filter(site_label == input$site_filter)
      return(df)
    })
    
    # Render ReacTable with key info when file is uploaded
    output$key_info_table <- renderReactable({
      req(input$obs_file)  # Ensure the file is uploaded
      req(input$site_filter)  # Ensure the site is selected
      
      # Load and process data from the uploaded file
      df <- xylo_obs()
      # filtered Site info
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)
      site_info <- openxlsx::readWorkbook(wb, sheet = "obs_data_info", startRow = 6, colNames = FALSE) %>% 
        setNames(c("site_label", "latitude", "longitude", "elevation")) %>%
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
      site <- unique(df$site_label)
      from <- as.Date(range(df$sample_date)[1], origin = "1899-12-30")
      to <- as.Date(range(df$sample_date)[2], origin = "1899-12-30")
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
      
      # Render the table with reactable
      reactable::reactable(key_info, pagination = TRUE, defaultPageSize = 13)
    })
    
    # Render ReacTable with key info when file is uploaded
    output$obs_table <- renderReactable({
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
      
      # Render the table with reactable
      reactable::reactable(grouped)
    })
    
    
    # Render Leaflet map when file is uploaded
    output$mymap <- renderLeaflet({
      req(input$obs_file)
      req(input$site_filter)
      wb <- openxlsx::loadWorkbook(input$obs_file$datapath)  # Read the OBS file
      site_info <- openxlsx::readWorkbook(wb, sheet = "obs_data_info", startRow = 6, colNames = FALSE) %>% 
        setNames(c("site_label", "latitude", "longitude", "elevation")) %>% 
        dplyr::filter(site_label == input$site_filter)
      lng <- site_info %>% dplyr::select(longitude) %>% dplyr::pull() %>% as.numeric()
      lat <- site_info %>% dplyr::select(latitude) %>% dplyr::pull() %>% as.numeric()
      
      # Create leaflet map
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(lng = lng, lat = lat, popup = paste0("Site: ", site_info$site_label))
    })
    
    # Plotly plot: scatter plot of tree sample collection dates (Tab 1)
    output$data_coverage_plot <- renderPlotly({
      req(input$obs_file)
      req(input$site_filter)
      df <- xylo_obs()
      df$ColorFactor <- as.factor(df$sample_id)  # Color factor for distinct colors
      
      # Plot the data
      plot_ly(df, x = ~sample_date, y = ~tree_label, type = 'scatter', mode = 'markers',
              color = ~ColorFactor,  # Color by Sample_id
              marker = list(size = 10, opacity = 0.7)) %>%
        layout(title = "Tree Sample Collection Dates",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Tree Label", categoryorder = "category ascending"))
    })
    
    observe({
      validate_location <- input$validate_location
      validate_data_coverage <- input$validate_data_coverage
      validate_observation <- input$validate_observation  # Correct the reference to the checkbox ID
      
      # Enable the next button only if all checkboxes are TRUE
      shinyjs::toggleState(id = "next_btn", condition = validate_location && validate_data_coverage && validate_observation)
      
      # Update validation status
      if (validate_location && validate_data_coverage && validate_observation) {
        output$validation_status <- renderText("Validation complete! You can proceed.")
      } else {
        output$validation_status <- renderText("Please validate location, data coverage, and observation list before proceeding.")
      }
    })
    
    observeEvent(input$next_btn, {
      updateTabsetPanel(session, "inTabset", selected = "metadata")
    })
    
    # TAB 2: ---------------------------------------------------------------------
    
    xylo_file <- shiny::reactive({
      openxlsx::loadWorkbook(shiny::req(input$obs_file$datapath))
    })
    
    # upload METADATA FILE TEMPLATE and CREATE METADATAFILE
    shiny::observeEvent(input$open_meta_temp, {
      # Show the progress bar
      shiny::withProgress(message = 'Processing metadata...', value = 0, {
        
        # Loading the template
        template_path <- system.file("extdata", "Datasetname_xylo_meta_yyyy-mm-dd.xlsx", package = "xyloR")
        meta_path_temporary <- file.path(tempdir_path, paste0("Datasetname_xylo_meta_", Sys.Date(), ".xlsx"))
        
        # Update progress
        shiny::setProgress(value = 0.2, detail = "Loading the template...")
        meta_template <- openxlsx::loadWorkbook(template_path)  # load the template
        
        # Perform additional processing
        shiny::req(input$obs_file)  # Ensure file is uploaded
        shiny::setProgress(value = 0.5, detail = "Processing the template...")
        meta_template <- xyloR::create_xylo_metadata(input$obs_file$datapath, template_path)
        
        # Save the filled-in template
        shiny::setProgress(value = 0.8, detail = "Saving the file...")
        openxlsx::saveWorkbook(meta_template, meta_path_temporary, overwrite = TRUE)  # save in tempdir
        
        # Open the file based on OS
        shiny::setProgress(value = 1, detail = "Opening the file...")
        tryCatch({
          if (.Platform$OS.type == "windows") {
            system2("cmd", c("/c", "start", shQuote(meta_path_temporary)), wait = FALSE)
          } else if (Sys.info()["sysname"] == "Darwin") {
            system(paste("open", shQuote(meta_path_temporary)))
          } else {
            system(paste("xdg-open", shQuote(meta_path_temporary)))
          }
        },
        error = function(e) {
          shiny::showNotification("File could not be opened!", type = "error")
        })
      })
    })
    
    # print the tempdir() path just in case
    output$meta_file_copied <- shiny::renderText({
      shiny::req(input$open_meta_temp)
      paste0("File opened in temporary dir ", tempdir_path,
             " . Clicking the button again will overwrite the file, so be sure to save it to a different location!")
    })
    
    # upload FILLED METADATA FILE and check format validation
    validation_results <- shiny::reactiveVal(NULL)  # Store validation results
    
    shiny::observeEvent(input$meta_file, {
      shiny::req(input$meta_file)
      
      # Run the metadata validation
      tbl_validation <- xyloR::meta_format_validation(input$meta_file$datapath)
      
      # Store the results
      validation_results(tbl_validation)
    })
    
    # Render validation table or success message
    output$validation_message <- shiny::renderUI({
      tbl <- validation_results()
      
      if (is.null(tbl) || nrow(tbl) == 0) {
        shiny::tagList(
          shiny::tags$b("Congratulations! Your files are ready to be submitted."),
          shiny::tags$p("You can now click on the Submit button.")
        )
      } else {
        NULL  # Show nothing if there are validation issues
      }
    })
    
    # Render Plotly sunburst plot with hierarchical data
    output$hierarchical_structure  <- plotly::renderPlotly({
      shiny::req(input$meta_file$datapath)
      
      # Load meta file
      meta_file <- shiny::req(input$meta_file$datapath)
      sheet_names <- setdiff(readxl::excel_sheets(meta_file), c("instructions", "DropList", "ListOfVariables"))
      sheet_data <- setNames(lapply(sheet_names, function(sheet) readxl::read_excel(meta_file, sheet = sheet)[-1:-6,]), sheet_names)
      
      # group all samples per year, tree, plot, site, and network from sheet_data into a single data frame and count the number of samples per group
      df_joined <- dplyr::left_join(sheet_data[["sample"]], sheet_data[["tree"]], by = "tree_label", relationship = "many-to-many") %>%
        dplyr::left_join(sheet_data[["site"]], by = "site_label", relationship = "many-to-many") %>% 
        dplyr::group_by(network_label, site_label, plot_label, tree_label, year = lubridate::year(sample_date), sample_id) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(site_label = paste0(network_label, "__", site_label),
                      plot_label = paste0(site_label, "__", plot_label),
                      tree_label = paste0(plot_label, "__", tree_label),
                      year_label = paste0(tree_label, "__", year),
                      sample_label = paste0(year_label, "__", sample_id)) # Ensures uniqueness
      
      # Step 1: Aggregate at the tree level (sum of n)
      df_tree <- df_joined %>%
        dplyr::group_by(tree_label, plot_label) %>%
        dplyr::summarise(value = sum(n), .groups = "drop") %>%
        dplyr::rename(id = tree_label, parent = plot_label)
      
      # Step 2: Aggregate at the plot level (count number of unique trees per plot)
      df_plot <- df_joined %>%
        dplyr::distinct(plot_label, site_label, tree_label) %>%
        dplyr::group_by(plot_label, site_label) %>%
        dplyr::summarise(value = dplyr::n(), .groups = "drop") %>%
        dplyr::rename(id = plot_label, parent = site_label) # Extract the site from plot label
      
      # Step 3: Aggregate at the site level (count number of unique plots per site)
      df_site <- df_joined %>%
        dplyr::distinct(site_label, network_label) %>%
        dplyr::group_by(site_label, network_label) %>%
        dplyr::summarise(value = dplyr::n(), .groups = "drop") %>%
        dplyr::rename(id = site_label, parent = network_label)   # Assign network label
      
      # Step 4: Combine all levels into final hierarchy
      df_hierarchy <- dplyr::bind_rows(df_tree, df_plot, df_site) %>%
        dplyr::distinct(id, parent, value) %>%
        dplyr::arrange(parent, id) %>%
        dplyr::mutate(
          label = sub(".*__", "", id),  # Extract last part after "_"
          text = paste0(label, " (", value, ")")  # Format as "Label (Value)"
        )
      
      # Create the sunburst plot
      sunburst_plot <- plotly::plot_ly(
        data = df_hierarchy, 
        ids = ~id, 
        labels = ~text, 
        parents = ~parent, 
        values = ~value, 
        type = "sunburst"
      )
      
      sunburst_plot
    })
    
    # Render summary table
    output$meta_table <- reactable::renderReactable({
      shiny::req(input$meta_file$datapath)
      
      # Load meta file
      meta_file <- shiny::req(input$meta_file$datapath)
      
      # Read all sheets except "instructions", "DropList", "ListOfVariables"
      sheet_names <- setdiff(readxl::excel_sheets(meta_file), c("instructions", "DropList", "ListOfVariables"))
      sheet_data <- setNames(lapply(sheet_names, function(sheet) 
        readxl::read_excel(meta_file, sheet = sheet)[-1:-6,]), sheet_names)
      
      # Join sample, tree, and site data and group by relevant columns
      df_joined <- dplyr::left_join(sheet_data[["sample"]], sheet_data[["tree"]], by = "tree_label") %>%
        dplyr::left_join(sheet_data[["site"]], by = "site_label") %>%
        dplyr::group_by(network_label, site_label, plot_label, tree_label, year = lubridate::year(sample_date), sample_id) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")  # Ensure correct summarisation
      
      # Render the Reactable table
      reactable::reactable(df_joined,
                           searchable = TRUE,
                           striped = TRUE,
                           highlight = TRUE,
                           filterable = TRUE,
                           style = list(
                             height = "400px",  # Adjust height as needed
                             width = "100%"     # Width set to 100% of the container
                           ))
    })
    
    # Render VALIDATION table
    output$validation_table <- reactable::renderReactable({
      shiny::req(validation_results(), nrow(validation_results()) > 0)  # Show table only if there are errors
      
      reactable::reactable(validation_results(), 
                           searchable = TRUE,
                           striped = TRUE,
                           highlight = TRUE,
                           filterable = TRUE,
                           columns = list(
                             Sheet = reactable::colDef(name = "Sheet"),
                             Column = reactable::colDef(name = "Column"),
                             Issue = reactable::colDef(name = "Issue", minWidth = 300)
                           ))
    })
    
    # Enable submit button only if validation is successful
    shiny::observe({
      tbl <- validation_results()
      shinyjs::toggleState(id = "submit_btn", condition = !is.null(tbl) && nrow(tbl) == 0)
    })
    
    # Show success message when the submit button is clicked
    shiny::observeEvent(input$submit_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Submission Successful",
        "Your metadata file has been successfully submitted!",
        easyClose = TRUE
      ))
    })
    
    
  }
  
shinyApp(ui, server)

}
