

#' @export
#' 
mod_tab5_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidPage(
    
    shiny::h3("Engine Control Center"),
    
    shiny::fluidRow(
      
      shiny::column(
        4,
        shiny::tags$div(
          style = "padding:10px; border:1px solid #444; border-radius:6px;",
          
          shiny::h4("System Status"),
          
          shiny::uiOutput(ns("system_status")),
          
          shiny::hr(),
          
          shiny::h5("Readiness"),
          shiny::verbatimTextOutput(ns("ready_state"))
        )
      ),
      
      shiny::column(
        8,
        
        shiny::tags$div(
          style = "padding:10px; border:1px solid #444; border-radius:6px;",
          
          shiny::h4("Active Issues"),
          
          DT::dataTableOutput(ns("issue_table")),
          
          shiny::hr(),
          
          shiny::h4("Engine Summary"),
          
          DT::dataTableOutput(ns("engine_summary"))
        )
      )
    )
  )
}

mod_tab5_server <- function(id, ctx) {
  
  moduleServer(id, function(input, output, session) {
    
    # =====================================================
    # 🧠 READINESS STATE
    # =====================================================
    output$ready_state <- renderPrint({
      ctx$ready()
    })
    
    # =====================================================
    # 🟢 SYSTEM STATUS
    # =====================================================
    output$system_status <- renderUI({
      
      r <- ctx$ready()
      
      tagList(
        tags$div(paste("Data:", r$data)),
        tags$div(paste("Metadata:", r$metadata)),
        tags$div(paste("Engine:", r$engine))
      )
    })
    
    # =====================================================
    # ⚙️ ENGINE ACCESS (SAFE)
    # =====================================================
    engine <- reactive({
      req(ctx$snapshot())
      ctx$snapshot()
    })
    
    # =====================================================
    # 📊 ENGINE SUMMARY
    # =====================================================
    output$engine_summary <- DT::renderDataTable({
      
      eng <- engine()
      
      data.frame(
        component = c("obs", "site", "tree", "sample"),
        status = c(
          !is.null(eng$validation$obs),
          !is.null(eng$validation$site),
          !is.null(eng$validation$tree),
          !is.null(eng$validation$sample)
        )
      )
    })
    
    # =====================================================
    # 🚨 ISSUE TABLE (CORE FEATURE)
    # =====================================================
    output$issue_table <- DT::renderDataTable({
      
      eng <- engine()
      
      issues <- dplyr::bind_rows(
        normalize_validation(eng$validation$obs, "obs"),
        normalize_validation(eng$validation$site, "site"),
        normalize_validation(eng$validation$tree, "tree"),
        normalize_validation(eng$validation$sample, "sample")
      )
      
      issues
    }, selection = "single")
    
    # =====================================================
    # 🧭 CLICK → NAVIGATION (KEY FEATURE)
    # =====================================================
    observeEvent(input$issue_table_rows_selected, {
      
      eng <- engine()
      
      issues <- dplyr::bind_rows(
        normalize_validation(eng$validation$obs, "obs"),
        normalize_validation(eng$validation$site, "site"),
        normalize_validation(eng$validation$tree, "tree"),
        normalize_validation(eng$validation$sample, "sample")
      )
      
      row <- input$issue_table_rows_selected
      
      req(row)
      
      selected <- issues[row, ]
      
      # =================================================
      # ROUTING LOGIC (CLICK → TAB)
      # =================================================
      target_tab <- switch(
        selected$domain,
        obs = "tab3",
        site = "tab3",
        tree = "tab3",
        sample = "tab3",
        "tab3"
      )
      
      ctx$v2$stage <- target_tab
    })
    
  })
}