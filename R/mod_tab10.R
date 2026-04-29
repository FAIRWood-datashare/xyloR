
#' @export
#' 
mod_tab10_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Authors",
    value = "tab9",
    
    fluidRow(
      
      # =====================================================
      # LEFT: AUTHOR TABLE
      # =====================================================
      column(
        8,
        
        bslib::card(
          bslib::card_header("9.1 Author enrichment (ORCID layer)"),
          
          bslib::card_body(
            
            rhandsontable::rHandsontableOutput(ns("author_hot")),
            
            tags$hr(),
            
            actionButton(
              ns("apply_authors"),
              "Apply enrichment",
              class = "btn btn-primary w-100"
            )
          )
        )
      ),
      
      # =====================================================
      # RIGHT: ENRICHMENT STATUS
      # =====================================================
      column(
        4,
        
        bslib::card(
          bslib::card_header("9.2 ORCID validation"),
          
          bslib::card_body(
            
            uiOutput(ns("author_status")),
            tags$hr(),
            DT::DTOutput(ns("author_issues"))
          )
        )
      )
    )
  )
}


mod_tab10_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    author_data <- shiny::reactiveVal()
    
    observe({
      req(ctx$engine())
      
      author_data(ctx$data$authors$enriched)
    })
    
    output$author_hot <- rhandsontable::renderRHandsontable({
      req(author_data())
      rhandsontable::rhandsontable(author_data())
    })
    
    observeEvent(input$author_hot, {
      
      updated <- rhandsontable::hot_to_r(input$author_hot)
      author_data(updated)
    })
    
    observeEvent(input$apply_authors, {
      
      req(author_data())
      ctx$data$authors$enriched <- author_data()
      
      showNotification("Author enrichment updated", type = "message")
    })
    
    output$author_status <- shiny::renderUI({
      
      req(ctx$engine())
      
      v <- ctx$engine()$validation$authors
      
      if (v$valid) {
        tags$div(class = "alert alert-success", "✔ Author enrichment complete")
      } else {
        tags$div(
          class = "alert alert-warning",
          paste("Missing ORCID:", paste(v$missing_orcid, collapse = ", "))
        )
      }
    })
    
    output$author_issues <- DT::renderDT({
      
      req(ctx$engine())
      
      v <- ctx$engine()$validation$authors
      
      data.frame(
        issue = if (!v$valid) v$issues else "No issues",
        unresolved = v$n_unresolved
      )
    })
  })
}