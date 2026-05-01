

#' @export
#' 
mod_tab7_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Site",
    value = "tab6",
    
    fluidRow(
      
      # =====================================================
      # LEFT: SITE TABLE
      # =====================================================
      column(
        8,
        
        bslib::card(
          bslib::card_header("6.1 Site layer (derived from engine)"),
          
          bslib::card_body(
            
            rhandsontable::rHandsontableOutput(ns("site_hot")),
            
            tags$hr(),
            
            actionButton(
              ns("apply_site"),
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
          bslib::card_header("6.2 Site validation (engine)"),
          
          bslib::card_body(
            
            uiOutput(ns("site_status")),
            tags$hr(),
            DT::DTOutput(ns("site_issues"))
          )
        )
      )
    )
  )
}


mod_tab7_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    # =====================================================
    # 🧠 LOCAL BUFFER
    # =====================================================
    site_data <- reactiveVal()
    
    # =====================================================
    # 🔄 INIT FROM GLOBAL STATE
    # =====================================================
    observe({
      
      req(ctx$data$site$working_copy)
      
      site_data(ctx$data$site$working_copy)
    })
    
    # =====================================================
    # 📊 TABLE RENDER
    # =====================================================
    output$site_hot <- rhandsontable::renderRHandsontable({
      
      req(site_data())
      
      rhandsontable::rhandsontable(
        site_data(),
        stretchH = "all",
        rowHeaders = TRUE,
        useTypes = TRUE
      )
    })
    
    # =====================================================
    # ✍️ LOCAL EDIT BUFFER
    # =====================================================
    observeEvent(input$site_hot, {
      
      updated <- isolate(
        rhandsontable::hot_to_r(input$site_hot)
      )
      
      site_data(updated)
    })
    
    # =====================================================
    # 💾 APPLY → GLOBAL STATE + ENGINE INVALIDATION
    # =====================================================
    observeEvent(input$apply_site, {
      
      req(site_data())
      
      ctx$data$site$working_copy <- site_data()
      
      ctx$invalidate_engine()
      ctx$update_ready()
      
      showNotification(
        "Site layer updated → engine invalidated",
        type = "message"
      )
    })
    
    # =====================================================
    # 🧠 SINGLE ENGINE RESOLVER (NEW STANDARD)
    # =====================================================
    get_engine <- reactive({
      
      eng <- ctx$get_engine()
      req(!is.null(eng))
      
      eng
    })
    
    # =====================================================
    # 🧠 STATUS UI (SIMPLIFIED + SAFE)
    # =====================================================
    output$site_status <- shiny::renderUI({
      
      if (!ctx$has_engine()) {
        
        tags$div(
          class = "alert alert-warning",
          "⚠ Engine not available (run ingestion/QA)"
        )
        
      } else {
        
        v <- get_engine()$validation$site
        
        if (isTRUE(v$valid)) {
          
          tags$div(
            class = "alert alert-success",
            "✔ Site layer valid"
          )
          
        } else {
          
          tags$div(
            class = "alert alert-danger",
            paste("Site issues:", paste(v$issues, collapse = ", "))
          )
        }
      }
    })
    
    # =====================================================
    # 📋 ISSUE TABLE (CONSISTENT ACCESS)
    # =====================================================
    output$site_issues <- DT::renderDT({
      
      req(ctx$has_engine())
      
      v <- get_engine()$validation$site
      
      data.frame(
        issue = if (!v$valid) v$issues else "No issues detected",
        n_sites = v$n_sites
      )
    })
    
  })
}