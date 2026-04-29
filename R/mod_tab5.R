

#' @export
#' 
mod_tab5_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Observations",
    value = "tab5",
    
    fluidRow(
      
      # =====================================================
      # LEFT: OBSERVATION GRID
      # =====================================================
      column(
        8,
        
        bslib::card(
          bslib::card_header("5.1 Observation data"),
          
          bslib::card_body(
            
            rhandsontable::rHandsontableOutput(ns("obs_hot")),
            
            tags$hr(),
            
            actionButton(
              ns("apply_obs"),
              "Apply changes",
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
          bslib::card_header("5.2 Schema validation"),
          
          bslib::card_body(
            
            uiOutput(ns("obs_status")),
            
            tags$hr(),
            
            DT::DTOutput(ns("obs_issues"))
          )
        )
      )
    )
  )
}


mod_tab5_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 🧠 LOCAL BUFFER
    # =====================================================
    obs_data <- reactiveVal()
    
    # INIT FROM GLOBAL (SAFE)
    observe({
      
      if (is.null(ctx$data$obs$working_copy)) return()
      
      obs_data(ctx$data$obs$working_copy)
    })
    
    # =====================================================
    # 📊 TABLE RENDER
    # =====================================================
    output$obs_hot <- rhandsontable::renderRHandsontable({
      
      req(obs_data())
      
      rhandsontable::rhandsontable(
        obs_data(),
        stretchH = "all",
        rowHeaders = TRUE,
        useTypes = TRUE
      )
    })
    
    # =====================================================
    # ✍️ EDIT BUFFER (LOCAL ONLY)
    # =====================================================
    observeEvent(input$obs_hot, {
      
      updated <- isolate(
        rhandsontable::hot_to_r(input$obs_hot)
      )
      
      obs_data(updated)
    })
    
    # =====================================================
    # 💾 APPLY → GLOBAL STATE
    # =====================================================
    observeEvent(input$apply_obs, {
      
      req(obs_data())
      
      ctx$data$obs$working_copy <- obs_data()
      
      showNotification("Observation updated", type = "message")
    })
    
    # =====================================================
    # 🟢 ENGINE STATUS (READ ONLY)
    # =====================================================
    output$obs_status <- renderUI({
      
      req(ctx$engine())
      
      v <- ctx$engine()$validation$obs
      
      if (v$valid) {
        tags$div(class = "alert alert-success",
                 "✔ Observation schema valid")
      } else {
        tags$div(
          class = "alert alert-danger",
          paste("Missing:", paste(v$missing_cols, collapse = ", "))
        )
      }
    })
    
    # =====================================================
    # 📋 ISSUE TABLE
    # =====================================================
    output$obs_issues <- DT::renderDT({
      
      req(ctx$engine())
      
      v <- ctx$engine()$validation$obs
      
      data.frame(
        issue = if (!v$valid) {
          paste("Missing column:", v$missing_cols)
        } else {
          "No issues detected"
        },
        rows = v$n_rows
      )
    })
  })
}