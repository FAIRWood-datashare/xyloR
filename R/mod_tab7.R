

#' @export
#' 
mod_tab7_ui <- function(id) {
  
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


mod_tab7_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 🧠 LOCAL BUFFER
    # =====================================================
    tree_data <- reactiveVal()
    
    # =====================================================
    # INIT FROM GLOBAL STATE (NOT ENGINE)
    # =====================================================
    observe({
      
      req(ctx$data$tree$working_copy)
      
      tree_data(ctx$data$tree$working_copy)
    })
    
    # =====================================================
    # 📊 RENDER HANDSONTABLE
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
    # 💾 APPLY → GLOBAL STATE
    # =====================================================
    observeEvent(input$apply_tree, {
      
      req(tree_data())
      
      ctx$data$tree$working_copy <- tree_data()
      
      showNotification(
        "Tree layer updated",
        type = "message"
      )
    })
    
    # =====================================================
    # 🧠 ENGINE STATUS (READ ONLY)
    # =====================================================
    output$tree_status <- shiny::renderUI({
      
      req(ctx$engine())
      
      v <- ctx$engine()$validation$tree
      
      if (v$valid) {
        
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
    })
    
    # =====================================================
    # 📋 ISSUE TABLE
    # =====================================================
    output$tree_issues <- DT::renderDT({
      
      req(ctx$engine())
      
      v <- ctx$engine()$validation$tree
      
      data.frame(
        issue = if (!v$valid) v$issues else "No issues detected",
        n_edges = v$n_edges
      )
    })
  })
}