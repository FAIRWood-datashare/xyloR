

#' @export
#' 
mod_tab6_ui <- function(id) {
  
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


mod_tab6_server <- function(id, ctx, session) {
  
  moduleServer(id, function(input, output, session) {
    
    # =====================================================
    # 🧠 LOCAL BUFFER
    # =====================================================
    dsample <- reactiveVal(NULL)
    
    # =====================================================
    # 📥 INIT
    # =====================================================
    observe({
      
      req(ctx$files$wb_meta)
      
      df <- openxlsx::readWorkbook(
        ctx$files$wb_meta,
        sheet = "sample",
        startRow = 1,
        colNames = TRUE
      )[-(1:6), ] |>
        tibble::as_tibble()
      
      dsample(df)
    })
    
    # =====================================================
    # 📊 RENDER
    # =====================================================
    output$tbl5 <- rhandsontable::renderRHandsontable({
      
      req(dsample())
      
      rhandsontable::rhandsontable(
        dsample(),
        rowHeaders = NULL,
        contextMenu = TRUE,
        stretchH = "all"
      )
    })
    
    # =====================================================
    # ✍️ EDIT BUFFER ONLY
    # =====================================================
    observeEvent(input$tbl5, {
      
      req(input$tbl5)
      dsample(rhandsontable::hot_to_r(input$tbl5))
    })
    
    # =====================================================
    # 💾 SAVE (ONLY PLACE THAT TOUCHES GLOBAL STATE)
    # =====================================================
    observeEvent(input$save_sample, {
      
      req(dsample())
      
      ctx$data$sample$working_copy <- dsample()
      
      ctx$invalidate_engine()
      ctx$update_ready()
      
      save_and_validate(
        data_reactive = dsample(),
        sheet_name    = "sample",
        wb_reactive   = reactive(ctx$files$wb_meta),
        temp_folder   = ctx$files$temp_folder
      )
      
      showNotification("Sample saved", type = "message")
    })
  })
}