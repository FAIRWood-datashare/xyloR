library(shiny)
library(shinyjs)
library(bslib)
library(readxl)
library(writexl)
library(openxlsx)

# Define example function
example_fun <- function(data) {
  # Example function: adds a new column with doubled values (assuming numeric data in first column)
  if (is.numeric(data[[1]])) {
    data$Doubled <- data[[1]] * 2
  }
  return(data)
}



ui <- fluidPage(
  
  theme = bs_theme(primary = "#006268", #, secondary = "#00919A"
                   font_scale = 0.8, bootswatch = "yeti"),
  
  useShinyjs(),  # Include shinyjs
  
  titlePanel("XyloGlobo: Contributing Data"),
  
  navset_card_tab(id = 'tabs',
    
    # TAB 1: -------------------------------------------------------------------
    nav_panel(title = "Upload observation data",
              
             card(
               card_header('Open observation data template'),
               card_body(
                 fillable=FALSE,
                 
                 p('Clicking the button will open the template. Fill it in
                  and save under your preferred location. Then use the next 
                  step to browse and load the filled in data file.'),
                 
                 actionButton("open_obs_temp", "Click to open data template!",
                              class="btn btn-primary"),
                 
                 textOutput("obs_file_copied")
                 ),
               ),
               
             card(
               card_header('Load the file you just saved!'),
               fileInput("obs_file", "", accept = c(".xlsx")),
             ),
               
             card(
               class="border border-0",
               card_body(
                 fillable = FALSE,
                 actionButton('next_btn', 'Next', icon = icon('angle-double-right'),
                              class="btn btn-primary"))
             )
    ),
  
    # TAB 2: -------------------------------------------------------------------
    nav_panel("Upload metadata",
              
              card(
                card_header('Open meta data template'),
                card_body(
                  fillable=FALSE,
                  
                  p('Clicking the button will open the template, prefilled with
                  the data already available from the observations file. 
                  Complete it and save under your preferred location. Then use the next 
                  step to browse and load the filled-in metadata file.'),
                  
                  p('Note that it may take a few seconds for the template to
                    be prefilled and opened.', style = "color: red;"),
                  
                  actionButton("open_meta_temp", "Click to open meta template!",
                               class="btn btn-primary"),
                  
                  textOutput("meta_file_copied")
                ),
              ),
              
              card(
                card_header('Load the file you just saved!'),
                fileInput("meta_file", "", accept = c(".xlsx")),
              ),
              
              card(
                class="border border-0",
                card_body(
                  fillable = FALSE,
                  actionButton('submit_btn', 'Submit', icon = icon('angle-double-right'),
                               class="btn btn-primary"))
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
           " . Clicking the button again will overwrite the file, so be sure
           to save it to a different location!")
  })
  
  # Enable next button only when input file is loaded
  observe({
    toggleState(id = "next_btn", condition = !is.null(input$obs_file))
  })
  # functionality: switch to next tab
  observeEvent(input$next_btn, {
    nav_select(id = 'tabs', selected = "Upload metadata")
  })
    
  # TAB 2: ---------------------------------------------------------------------

  xylo_file <- reactive({
    openxlsx::loadWorkbook(input$obs_file$datapath)
    })
  
  observeEvent(input$open_meta_temp, {
    template_path <- system.file("extdata", "XX_XX_XXX_meta.xltm", package = "xyloR")
    meta_path_temporary <- file.path(tempdir_path, "Xylo_metadata_copy.xlsx")
    meta_template <- openxlsx::loadWorkbook(template_path) # load the template
    
    # do something to the meta_template
    meta_template <- create_xylo_metadata(xylo_file(), meta_template)
    
    # save filled in meta_template in tempdir
    openxlsx::saveWorkbook(meta_template, meta_path_temporary, overwrite = TRUE) # save in tempdir
    
    # Open xlsx based on OS
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
  
  # print the tempdir() path just in case
  output$meta_file_copied <- renderText({
    req(input$open_meta_temp)
    paste0("File opened in temporary dir ", tempdir_path,
           " . Clicking the button again will overwrite the file, so be sure
           to save it to a different location!")
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
