

#' @export
#' 
mod_tab8_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Sample",
    value = "tab8",
    
    fluidRow(
      
      # =====================================================
      # LEFT: SAMPLE TABLE
      # =====================================================
      column(
        8,
        
        bslib::card(
          bslib::card_header("8.1 Sample layer (derived from TREE)"),
          
          bslib::card_body(
            
            rhandsontable::rHandsontableOutput(ns("sample_hot")),
            
            tags$hr(),
            
            actionButton(
              ns("apply_sample"),
              "Apply changes",
              class = "btn btn-primary w-100"
            )
          )
        )
      ),
      
      # =====================================================
      # RIGHT: FINAL VALIDATION
      # =====================================================
      column(
        4,
        
        bslib::card(
          bslib::card_header("8.2 Final validation (engine)"),
          
          bslib::card_body(
            
            uiOutput(ns("sample_status")),
            tags$hr(),
            DT::DTOutput(ns("sample_issues"))
          )
        )
      )
    )
  )
}


mod_tab8_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 🧠 LOCAL BUFFER
    # =====================================================
    sample_data <- reactiveVal(NULL)
    
    # =====================================================
    # INIT FROM ENGINE (SAFE + SINGLE ACCESS)
    # =====================================================
    observe({
      
      eng <- ctx$engine()
      req(eng)
      
      sample_data(eng$derived$sample)
    })
    
    # =====================================================
    # 📊 RENDER TABLE
    # =====================================================
    output$sample_hot <- rhandsontable::renderRHandsontable({
      
      req(sample_data())
      
      rhandsontable::rhandsontable(
        sample_data(),
        stretchH = "all",
        rowHeaders = TRUE,
        useTypes = TRUE
      )
    })
    
    # =====================================================
    # ✍️ CAPTURE EDITS
    # =====================================================
    observeEvent(input$sample_hot, {
      
      updated <- rhandsontable::hot_to_r(input$sample_hot)
      sample_data(updated)
    })
    
    # =====================================================
    # 💾 APPLY → GLOBAL STATE
    # =====================================================
    observeEvent(input$apply_sample, {
      
      req(sample_data())
      
      ctx$data$sample$working_copy <- sample_data()
      
      showNotification(
        "Sample layer updated",
        type = "message"
      )
    })
    
    # =====================================================
    # 🧠 ENGINE-DRIVEN STATUS (SAFE ACCESS)
    # =====================================================
    output$sample_status <- shiny::renderUI({
      
      eng <- ctx$engine()
      req(eng)
      
      v <- eng$validation$sample
      
      if (isTRUE(v$valid)) {
        
        tags$div(
          class = "alert alert-success",
          "✔ Sample valid (export-ready)"
        )
        
      } else {
        
        tags$div(
          class = "alert alert-danger",
          paste(
            "Sample issues:",
            paste(v$issues, collapse = ", ")
          )
        )
      }
    })
    
    # =====================================================
    # 📋 FINAL VALIDATION TABLE (SAFE ACCESS)
    # =====================================================
    output$sample_issues <- DT::renderDT({
      
      eng <- ctx$engine()
      req(eng)
      
      v <- eng$validation$sample
      
      data.frame(
        issue = if (!v$valid) v$issues else "No issues detected",
        status = v$valid
      )
    })
  })
}