

#' @export
#' 
make_tab_module <- function(tab_id,
                            init_fn = function(input, ctx) {},
                            view_fn,
                            signal_fn,
                            ui_fn,
                            action_fn = function(input, output, ctx, view) {}) {
  
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
      
      # =====================================================
      # 3. ACTION LAYER (🔥 NEW — THIS IS YOUR FIX)
      # =====================================================
      action_fn(input, output, ctx, view)
      
      # =====================================================
      # 4. SIGNAL (FSM OUTPUT ONLY)
      # =====================================================
      observe({
        ctx$signals[[paste0(tab_id, "_done")]] <-
          signal_fn(ctx, view)
      })
      
      # =====================================================
      # 5. UI LAYER
      # =====================================================
      ui_fn(output, input, ctx, ns, view)
      
      # =====================================================
      # 6. NO NAVIGATION LOGIC
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

make_fsm_tab <- function(
    tab_id,
    init_fn = function(input, ctx) {},
    view_fn,
    signal_fn,
    ui_fn
) {
  
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
      # 2. VIEW (PURE STATE DERIVATION)
      # =====================================================
      view <- view_fn(ctx)
      
      # =====================================================
      # 3. SIGNAL (FSM OUTPUT ONLY)
      # =====================================================
      observe({
        ctx$signals[[paste0(tab_id, "_done")]] <-
          signal_fn(ctx, view)
      })
      
      # =====================================================
      # 4. UI
      # =====================================================
      ui_fn(output, input, ctx, ns, view)
      
      invisible(NULL)
    })
  }
}