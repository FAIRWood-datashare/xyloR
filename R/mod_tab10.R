

#' @export
#' 
mod_tab10_ui <- function(id) {
  
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

mod_tab10_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 🧠 LOCAL BUFFER
    # =====================================================
    pub_data <- reactiveVal()
    
    # =====================================================
    # INIT FROM GLOBAL ENRICHMENT STORE (NOT ENGINE)
    # =====================================================
    observe({
      
      req(ctx$data$publications$enriched)
      
      pub_data(ctx$data$publications$enriched)
    })
    
    # =====================================================
    # 📊 RENDER TABLE
    # =====================================================
    output$pub_hot <- rhandsontable::renderRHandsontable({
      
      req(pub_data())
      
      rhandsontable::rhandsontable(pub_data())
    })
    
    # =====================================================
    # ✍️ LOCAL EDIT BUFFER
    # =====================================================
    observeEvent(input$pub_hot, {
      
      updated <- isolate(
        rhandsontable::hot_to_r(input$pub_hot)
      )
      
      pub_data(updated)
    })
    
    # =====================================================
    # 🌐 APPLY → GLOBAL STORE
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
    # 🧠 ENGINE-DRIVEN STATUS
    # =====================================================
    output$pub_status <- shiny::renderUI({
      
      req(ctx$engine())
      
      v <- ctx$engine()$validation$publications
      
      if (v$valid) {
        
        tags$div(
          class = "alert alert-success",
          "✔ Publication metadata complete"
        )
        
      } else {
        
        tags$div(
          class = "alert alert-warning",
          paste(
            "Missing DOI:",
            paste(v$missing_doi, collapse = ", ")
          )
        )
      }
    })
    
    # =====================================================
    # 📋 ISSUE TABLE
    # =====================================================
    output$pub_issues <- DT::renderDT({
      
      req(ctx$engine())
      
      v <- ctx$engine()$validation$publications
      
      data.frame(
        issue = if (!v$valid) v$issues else "No issues detected",
        unresolved = v$n_unresolved
      )
    })
  })
}