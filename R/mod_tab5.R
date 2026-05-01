

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
      
      list(
        dataset   = ctx$state$dataset_ready,
        ingestion = ctx$state$ingestion_ready,
        qa        = ctx$state$qa_ready,
        meta      = ctx$state$meta_ready,
        export    = ctx$state$export_ready
      )
    })
    
    # =====================================================
    # 🟢 SYSTEM STATUS (ENGINE FIXED)
    # =====================================================
    output$system_status <- renderUI({
      
      tagList(
        tags$div(paste("Dataset:",   ctx$state$dataset_ready)),
        tags$div(paste("Ingestion:", ctx$state$ingestion_ready)),
        tags$div(paste("QA:",        ctx$state$qa_ready)),
        tags$div(paste("Meta:",      ctx$state$meta_ready)),
        tags$div(paste("Engine:",    ctx$has_engine()))
      )
    })
    
    # =====================================================
    # 📊 ENGINE SUMMARY (SAFE SSOT ACCESS)
    # =====================================================
    output$engine_summary <- DT::renderDataTable({
      
      eng <- ctx$get_engine()
      req(!is.null(eng))
      
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
    # 🚨 ISSUE TABLE
    # =====================================================
    output$issue_table <- DT::renderDataTable({
      
      eng <- ctx$get_engine()
      req(!is.null(eng))
      
      issues <- dplyr::bind_rows(
        normalize_validation(eng$validation$obs, "obs"),
        normalize_validation(eng$validation$site, "site"),
        normalize_validation(eng$validation$tree, "tree"),
        normalize_validation(eng$validation$sample, "sample")
      )
      
      if (nrow(issues) == 0) {
        return(data.frame(message = "No issues found"))
      }
      
      issues
    }, selection = "single")
    
    # =====================================================
    # 🧭 NAVIGATION
    # =====================================================
    observeEvent(input$issue_table_rows_selected, {
      
      eng <- ctx$get_engine()
      req(!is.null(eng))
      
      issues <- dplyr::bind_rows(
        normalize_validation(eng$validation$obs, "obs"),
        normalize_validation(eng$validation$site, "site"),
        normalize_validation(eng$validation$tree, "tree"),
        normalize_validation(eng$validation$sample, "sample")
      )
      
      row <- input$issue_table_rows_selected
      req(row)
      
      selected <- issues[row, ]
      
      target_tab <- switch(
        selected$domain,
        obs    = "tab3",
        site   = "tab3",
        tree   = "tab3",
        sample = "tab3",
        "tab3"
      )
      
      ctx$state$stage <- target_tab
    })
    
  })
}