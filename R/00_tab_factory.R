

#' @export
#' 
make_tab_module <- function(tab_id,
                            init_fn = function(input, ctx) {},
                            view_fn,
                            signal_fn,
                            ui_fn) {
  
  function(id, ctx, session) {
    
    moduleServer(id, function(input, output, session) {
      
      ns <- session$ns
      
      # =====================================================
      # 1. INIT (SIDE EFFECTS ONLY)
      # =====================================================
      observe({
        init_fn(input, ctx)
      })
      
      # =====================================================
      # 2. VIEW (PURE REACTIVE CONTRACT)
      # =====================================================
      view <- view_fn(ctx)
      
      observeEvent(input$save_obs, {
        
        req(input$tbl1, input$tbl2)
        
        ctx$data$tbl1 <- rhandsontable::hot_to_r(input$tbl1)
        ctx$data$draft_obs <- rhandsontable::hot_to_r(input$tbl2)
        
        message("💾 Tab3 saved safely")
      })
      
      # =====================================================
      # 3. SIGNAL (FSM OUTPUT ONLY)
      # =====================================================
      observe({
        ctx$signals[[paste0(tab_id, "_done")]] <-
          signal_fn(ctx, view)
      })
      
      # =====================================================
      # 4. UI LAYER
      # =====================================================
      ui_fn(output, input, ctx, ns, view)
      
      # =====================================================
      # 5. NO NAVIGATION LOGIC
      # =====================================================
      observeEvent(input$next_btn, {
        shiny::showNotification(
          "Navigation handled by FSM engine",
          type = "message"
        )
      })
      
      invisible(NULL)
    })
  }
}