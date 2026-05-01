

#' @export
#' 
mod_tab8_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Tree",
    value = "tab7",
    
    fluidRow(
      
      # =====================================================
      # LEFT: TREE TABLE
      # =====================================================
      column(
        8,
        
        bslib::card(
          bslib::card_header("7.1 Tree layer (derived from SITE)"),
          
          bslib::card_body(
            
            rhandsontable::rHandsontableOutput(ns("tree_hot")),
            
            tags$hr(),
            
            actionButton(
              ns("apply_tree"),
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
          bslib::card_header("7.2 Tree validation (engine)"),
          
          bslib::card_body(
            
            uiOutput(ns("tree_status")),
            tags$hr(),
            DT::DTOutput(ns("tree_issues"))
          )
        )
      )
    )
  )
}


mod_tab8_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    # =====================================================
    # 🧠 LOCAL BUFFER
    # =====================================================
    tree_data <- reactiveVal()
    
    # =====================================================
    # 🔄 INIT FROM GLOBAL STATE
    # =====================================================
    observe({
      
      req(ctx$data$tree$working_copy)
      
      tree_data(ctx$data$tree$working_copy)
    })
    
    # =====================================================
    # 📊 TABLE RENDER
    # =====================================================
    output$tree_hot <- rhandsontable::renderRHandsontable({
      
      req(tree_data())
      
      rhandsontable::rhandsontable(
        tree_data(),
        stretchH = "all",
        rowHeaders = TRUE,
        useTypes = TRUE
      )
    })
    
    # =====================================================
    # ✍️ LOCAL EDIT BUFFER
    # =====================================================
    observeEvent(input$tree_hot, {
      
      updated <- isolate(
        rhandsontable::hot_to_r(input$tree_hot)
      )
      
      tree_data(updated)
    })
    
    # =====================================================
    # 💾 APPLY → GLOBAL STATE + ENGINE INVALIDATION
    # =====================================================
    observeEvent(input$apply_tree, {
      
      req(tree_data())
      
      ctx$data$tree$working_copy <- tree_data()
      
      ctx$invalidate_engine()
      ctx$update_ready()
      
      showNotification(
        "Tree layer updated → engine invalidated",
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
    # 🧠 STATUS UI
    # =====================================================
    output$tree_status <- shiny::renderUI({
      
      if (!ctx$has_engine()) {
        
        tags$div(
          class = "alert alert-warning",
          "⚠ Engine not available (run ingestion/QA)"
        )
        
      } else {
        
        v <- get_engine()$validation$tree
        
        if (isTRUE(v$valid)) {
          
          tags$div(
            class = "alert alert-success",
            "✔ Tree structure valid"
          )
          
        } else {
          
          tags$div(
            class = "alert alert-danger",
            paste("Tree issues:", paste(v$issues, collapse = ", "))
          )
        }
      }
    })
    
    # =====================================================
    # 📋 ISSUE TABLE
    # =====================================================
    output$tree_issues <- DT::renderDT({
      
      req(ctx$get_engine())
      
      v <- get_engine()$validation$tree
      
      data.frame(
        issue = if (!v$valid) v$issues else "No issues detected",
        n_edges = v$n_edges
      )
    })
    
  })
}