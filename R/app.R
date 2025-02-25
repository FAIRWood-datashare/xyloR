library(shiny) 
library(shinyjs)
library(bslib)
library(readxl)
library(writexl)
library(openxlsx)
library(leaflet)
library(plotly)
library(dplyr)
library(DT)
library(reactable)

ui <- fluidPage(
  
  theme = bs_theme(primary = "#006268", font_scale = 0.8, bootswatch = "yeti"),
  useShinyjs(),  
  titlePanel("XyloGlobo: Contributing Data"),
  
  navset_card_tab(id = 'tabs',
                  
                  # 📌 TAB 1: Upload Observation Data -----------------------------------------
                  nav_panel(title = "Upload observation data",
                            
                            fluidRow(
                              # Left Side: Upload Section (With Background Color)
                              column(4, class = "bg-light p-3 border-end",  
                                     card(
                                       card_header('Open observation data template'),
                                       card_body(
                                         fillable = FALSE,
                                         p("Click the button to open the template. Fill it in, save it, then browse and load the file."),
                                         actionButton("open_obs_temp", "Click to open data template!", class = "btn btn-primary"),
                                         textOutput("obs_file_copied")
                                       )
                                     ),
                                     
                                     card(
                                       card_header('Load the file you just saved!'),
                                       fileInput("obs_file", "", accept = c(".xlsx"))
                                     ),
                                     # Add a ReactTable of key info below the cards
                                     card(
                                       card_header("Key Information Table"),
                                       card_body(
                                         reactableOutput("key_info_table")
                                       )
                                     )
                              ),
                              
                              # Right Side: Map & Plotly + Add Observations Table here
                              column(8,  
                                     card(
                                       card_header("Geolocation Map"),
                                       card_body(
                                         leafletOutput("mymap", height = "400px")
                                       )
                                     ),
                                     
                                     card(
                                       card_header("Data Coverage Overview"),
                                       card_body(
                                         plotlyOutput("data_coverage_plot", height = "300px")
                                       )
                                     ),
                                     
                                     # Move Observations performed section here
                                     card(
                                       card_header("Observations performed [number of radial files]"),
                                       card_body(
                                         reactableOutput("obs_table")
                                       )
                                     )
                              )
                            ),
                            
                            br(),
                            
                            # Adding validation checkboxes and next button
                            fluidRow(
                              column(12,
                                     card(
                                       card_header("Data Validation"),
                                       card_body(
                                         checkboxInput("validate_location", "Validate Location", value = FALSE),
                                         checkboxInput("validate_data_coverage", "Validate Data Coverage", value = FALSE),
                                         checkboxInput("validate_observation", "Validate observation list", value = FALSE),
                                         textOutput("validation_status"),
                                         actionButton('next_btn', 'Next', icon = icon('angle-double-right'), class = "btn btn-primary", disabled = TRUE)
                                       )
                                     )
                              )
                            )
                  ),
                  
                  # 📌 TAB 2: Upload Metadata -----------------------------------------------
                  nav_panel("Upload metadata",
                            
                            fluidRow(
                              column(4, class = "bg-light p-3 border-end",  
                                     card(
                                       card_header('Open metadata template'),
                                       card_body(
                                         fillable = FALSE,
                                         p("Click to open the template, prefilled with data from the observations file."),
                                         p("Complete it, save it, then browse and load the file."),
                                         p("Note: It may take a few seconds for the template to open.", style = "color: red;"),
                                         actionButton("open_meta_temp", "Click to open meta template!", class = "btn btn-primary"),
                                         textOutput("meta_file_copied")
                                       )
                                     ),
                                     
                                     card(
                                       card_header('Load the file you just saved!'),
                                       fileInput("meta_file", "", accept = c(".xlsx"))
                                     )
                              )
                            ),
                            
                            br(),
                            
                            fluidRow(
                              column(12,
                                     card(
                                       class = "border border-0 text-center",
                                       actionButton('submit_btn', 'Submit', icon = icon('angle-double-right'), class = "btn btn-primary")
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
  
  observeEvent(input$open_obs_temp, {
    template_path <- system.file("extdata", "Xylo_file.xltm", package = "xyloR")
    obs_path_temporary <- file.path(tempdir_path, "Xylo_file_copy.xlsx")
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
      showNotification("File could not be opened!", type = "error")
    })
  })
  
  # print the tempdir() path just in case
  output$obs_file_copied <- renderText({
    req(input$open_obs_temp)
    paste0("File opened in temporary dir ", tempdir_path,
           " . Clicking the button again will overwrite the file, so be sure to save it to a different location!")
  })
  
  
  # Render ReacTable with key info when file is uploaded
  output$key_info_table <- renderReactable({
    req(input$obs_file)  # Ensure the file is uploaded
    
    # Load and process data from the uploaded file
    wb <- openxlsx::loadWorkbook(input$obs_file$datapath)
    df <- openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 5)
    
    # Extract key information
    owner_lastname <- as.character(openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", rows = 2, cols = 2, colNames = FALSE))
    owner_fistname <- as.character(openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", rows = 1, cols = 2, colNames = FALSE))
    owner_email <- as.character(openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", rows = 3, cols = 2, colNames = FALSE))
    contact_lastname <- as.character(openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", rows = 2, cols = 4, colNames = FALSE))
    contact_fistname <- as.character(openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", rows = 1, cols = 4, colNames = FALSE))
    contact_email <- as.character(openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", rows = 3, cols = 4, colNames = FALSE))
    network <- unique(df$Network)
    site <- unique(df$Site_code)
    from <- as.Date(range(df$Date)[1], origin = "1899-12-30")
    to <- as.Date(range(df$Date)[2], origin = "1899-12-30")
    n_trees <- length(unique(df$Tree_label))
    n_dates <- length(unique(df$Date))
    n_samples <- nrow(df)
    
    # Prepare the key information table
    key_info <- t(data.frame(
      "Owner" = paste(owner_lastname, owner_fistname, sep = ", "),
      "Owner Email" = owner_email,
      "Contact" = paste(contact_lastname, contact_fistname, sep = ", "),
      "Contact Email" = contact_email,
      "Network" = paste(network, collapse = ", "),
      "Site" = paste(site, collapse = ", "),
      "Date From" = format(from, "%Y-%m-%d"),
      "Date To" = format(to, "%Y-%m-%d"),
      "n_Trees" = n_trees,
      "n_Dates" = n_dates,
      "n_Samples" = n_samples
    ))
    colnames(key_info) <- c("Key Info")
    
    # Render the table with reactable
    reactable(key_info, pagination = TRUE, defaultPageSize = 11)
  })
 
  # Render ReacTable with key info when file is uploaded
  output$obs_table <- renderReactable({
      req(input$obs_file)  # Ensure the file is uploaded
      
      # Load and process data from the uploaded file
    wb <- openxlsx::loadWorkbook(input$obs_file$datapath)
    df <- openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", startRow = 5)
      
      # Extract obs information
      obs_list <- colnames(df[, -c(1:5, ncol(df))])
      print(obs_list)
      
      # Extract count/width categories
      count_vars <- grep("count", obs_list, value = TRUE)
      width_vars <- grep("width", obs_list, value = TRUE)
      
      # Extract X/P categories
      X_vars <- grep("^X", obs_list, value = TRUE)
      P_vars <- grep("^P", obs_list, value = TRUE)
      
      # Extract C/E/W/M/Py categories
      C_vars <- grep("^C", obs_list, value = TRUE)
      E_vars <- grep("E", obs_list, value = TRUE)
      W_vars <- grep("W", obs_list, value = TRUE)
      M_vars <- grep("M", obs_list, value = TRUE)
      Py_vars <- grep("pY", obs_list, value = TRUE)
      
      # Group the results based on occurrences in the subset
      grouped <- data.frame(
        Category = c("Count", "Width"),
        # X = c(length(intersect(X_vars, count_vars)), length(intersect(X_vars, width_vars))),
        # P = c(length(intersect(P_vars, count_vars)), length(intersect(P_vars, width_vars))),
        C = c(length(intersect(C_vars, count_vars)), length(intersect(C_vars, width_vars))),
        E = c(length(intersect(E_vars, count_vars)), length(intersect(E_vars, width_vars))),
        W = c(length(intersect(W_vars, count_vars)), length(intersect(W_vars, width_vars))),
        M = c(length(intersect(M_vars, count_vars)), length(intersect(M_vars, width_vars))),
        Py = c(length(intersect(Py_vars, count_vars)), length(intersect(Py_vars, width_vars)))
      )
      
      # Render the table with reactable
      reactable(grouped)
    })
  
  
  # Render Leaflet map when file is uploaded
  output$mymap <- renderLeaflet({
    req(input$obs_file)
    wb <- openxlsx::loadWorkbook(input$obs_file$datapath)  # Read the OBS file
    lng <- as.numeric(openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", rows = 2, cols = 6, colNames = FALSE))
    lat <- as.numeric(openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", rows = 1, cols = 6, colNames = FALSE))
    site_code <- as.character(openxlsx::readWorkbook(wb, sheet = "Xylo_obs_data", rows = 7, cols = 2, colNames = FALSE))
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap tiles
      setView(lng = lng, lat= lat, zoom = 10) %>%  # Center the map on the coordinates
      addMarkers(lng = lng, lat = lat, popup = site_code, label = site_code)  # Add a marker
  })
  
  # Render Plotly plot when file is uploaded
  output$data_coverage_plot <- renderPlotly({
    req(input$obs_file)
    df <- openxlsx::readWorkbook(input$obs_file$datapath, sheet = "Xylo_obs_data", startRow = 5)
    # Convert Date column to Date type if needed
    df$Date <- as.Date(df$Date, origin = "1899-12-30")
    
    # Create dynamic size and color mappings based on Sample_id
    df$ColorFactor <- as.factor(df$Sample_id)  # Color factor for distinct colors
    
    # Plot the data
    plot_ly(df, x = ~Date, y = ~Tree_label, type = 'scatter', mode = 'markers',
            color = ~ColorFactor,  # Color by Sample_id
            marker = list(size = 10, opacity = 0.7)) %>%
      layout(title = "Tree Sample Collection Dates",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Tree Label", categoryorder = "category ascending"))
  })
  
  # Validate the location and data coverage checkboxes
  observe({
    validate_location <- input$validate_location
    validate_data_coverage <- input$validate_data_coverage
    validate_observation <- input$validate_observation  # Correct the reference to the checkbox ID
    
    # Enable the next button only if all checkboxes are TRUE
    toggleState(id = "next_btn", condition = validate_location && validate_data_coverage && validate_observation)
    
    # Update validation status
    if (validate_location && validate_data_coverage && validate_observation) {
      output$validation_status <- renderText("Validation complete! You can proceed.")
    } else {
      output$validation_status <- renderText("Please validate location, data coverage, and observation list before proceeding.")
    }
  })
  
  
  # functionality: switch to next tab when validation is complete
  observeEvent(input$next_btn, {
    nav_select(id = 'tabs', selected = "Upload metadata")
  })
  
  # TAB 2: ---------------------------------------------------------------------
  
  xylo_file <- reactive({
    openxlsx::loadWorkbook(input$obs_file$datapath)
  })
  
  observeEvent(input$open_meta_temp, {
    # Show the progress bar
    withProgress(message = 'Processing metadata...', value = 0, {
      
      # Loading the template
      template_path <- system.file("extdata", "XX_XX_XXX_meta.xltm", package = "xyloR")
      meta_path_temporary <- file.path(tempdir_path, "Xylo_metadata_copy.xlsx")
      
      # Update progress
      setProgress(value = 0.2, detail = "Loading the template...")
      meta_template <- openxlsx::loadWorkbook(template_path)  # load the template
      
      # Perform additional processing
      setProgress(value = 0.5, detail = "Processing the template...")
      meta_template <- create_xylo_metadata(xylo_file(), meta_template)
      
      # Save the filled-in template
      setProgress(value = 0.8, detail = "Saving the file...")
      openxlsx::saveWorkbook(meta_template, meta_path_temporary, overwrite = TRUE)  # save in tempdir
      
      # Open the file based on OS
      setProgress(value = 1, detail = "Opening the file...")
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
        showNotification("File could not be opened!", type = "error")
      })
    })
  })
  
  # print the tempdir() path just in case
  output$meta_file_copied <- renderText({
    req(input$open_meta_temp)
    paste0("File opened in temporary dir ", tempdir_path,
           " . Clicking the button again will overwrite the file, so be sure to save it to a different location!")
  })
  
  # Enable the button only when input file is loaded
  observe({
    toggleState(id = "submit_btn", condition = !is.null(input$meta_file))
  })
  # TODO: what happens when they click submit?
  # observeEvent(input$submit_btn, {
  #   ...
  # })
}

shinyApp(ui, server)
