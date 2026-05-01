

#' @export
#' 
mod_tab11_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Publications",
    value = "tab10",
    
    fluidRow(
      
      # =====================================================
      # LEFT: PUBLICATION TABLE
      # =====================================================
      column(
        8,
        
        bslib::card(
          bslib::card_header("10.1 Publication enrichment (DOI layer)"),
          
          bslib::card_body(
            
            rhandsontable::rHandsontableOutput(ns("pub_hot")),
            
            tags$hr(),
            
            actionButton(
              ns("apply_pub"),
              "Apply enrichment",
              class = "btn btn-primary w-100"
            )
          )
        )
      ),
      
      # =====================================================
      # RIGHT: VALIDATION PANEL
      # =====================================================
      column(
        4,
        
        bslib::card(
          bslib::card_header("10.2 DOI validation"),
          
          bslib::card_body(
            
            uiOutput(ns("pub_status")),
            tags$hr(),
            DT::DTOutput(ns("pub_issues"))
          )
        )
      )
    )
  )
}

mod_tab11_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    pub_data <- reactiveVal()
    
    # =====================================================
    # INIT FROM DATA STORE
    # =====================================================
    observe({
      
      req(ctx$data$publications$enriched)
      
      pub_data(ctx$data$publications$enriched)
    })
    
    # =====================================================
    # TABLE RENDER
    # =====================================================
    output$pub_hot <- rhandsontable::renderRHandsontable({
      
      req(pub_data())
      
      rhandsontable::rhandsontable(pub_data())
    })
    
    # =====================================================
    # CAPTURE EDITS (SAFE ISOLATED UPDATE)
    # =====================================================
    observeEvent(input$pub_hot, {
      
      updated <- isolate(
        rhandsontable::hot_to_r(input$pub_hot)
      )
      
      pub_data(updated)
    })
    
    # =====================================================
    # APPLY → GLOBAL STORE (NO ENGINE INVALIDATION)
    # =====================================================
    observeEvent(input$apply_pub, {
      
      req(pub_data())
      
      ctx$data$publications$enriched <- pub_data()
      
      showNotification(
        "Publication enrichment updated",
        type = "message"
      )
    })
    
    # =====================================================
    # STATUS UI (SAFE ENGINE READ)
    # =====================================================
    output$pub_status <- shiny::renderUI({
      
      eng <- ctx$get_engine()
      
      if (is.null(eng)) {
        
        return(tags$div(
          class = "alert alert-warning",
          "⚠ Engine not available (run ingestion/QA)"
        ))
      }
      
      v <- eng$validation$publications
      
      missing <- if (!is.null(v$missing_doi)) {
        paste(v$missing_doi, collapse = ", ")
      } else {
        "None"
      }
      
      if (isTRUE(v$valid)) {
        
        tags$div(
          class = "alert alert-success",
          "✔ Publication metadata complete"
        )
        
      } else {
        
        tags$div(
          class = "alert alert-warning",
          paste("Missing DOI:", missing)
        )
      }
    })
    
    # =====================================================
    # ISSUE TABLE (SAFE)
    # =====================================================
    output$pub_issues <- DT::renderDT({
      
      eng <- ctx$get_engine()
      req(!is.null(eng))
      
      v <- eng$validation$publications
      
      data.frame(
        issue = if (!v$valid) v$issues else "No issues detected",
        unresolved = v$n_unresolved
      )
    })
    
  })
}