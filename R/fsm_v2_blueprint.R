

make_tab_module <- function(tab_id,
                            init_fn,
                            view_fn,
                            signal_fn,
                            ui_fn) {
  
  function(id, ctx, session) {
    
    moduleServer(id, function(input, output, session) {
      
      ns <- session$ns
      
      # =====================================================
      # 1. INIT (OPTIONAL SIDE EFFECTS)
      # =====================================================
      observe({
        init_fn(input, ctx)
      })
      
      # =====================================================
      # 2. VIEW (PURE REACTIVE FUNCTIONS ONLY)
      # =====================================================
      view <- view_fn(ctx)
      
      # =====================================================
      # 3. SIGNAL (FSM INPUT ONLY)
      # =====================================================
      observe({
        ctx$signals[[paste0(tab_id, "_done")]] <- signal_fn(ctx, view)
      })
      
      # =====================================================
      # 4. UI
      # =====================================================
      ui_fn(output, input, ctx, ns, view)
      
      # =====================================================
      # 5. NO NAVIGATION LOGIC
      # =====================================================
      observeEvent(input$next_btn, {
        shiny::showNotification(
          "Navigation handled by FSM v2 engine",
          type = "message"
        )
      })
      
      invisible(NULL)
    })
  }
}

##############
# mod_tab4_server <- make_tab_module(
#   tab_id = "tab4",
#   
#   # =====================================================
#   # INIT
#   # =====================================================
#   init_fn = function(input, ctx) {
#     # optional file load, defaults, etc
#   },
#   
#   # =====================================================
#   # VIEW
#   # =====================================================
#   view_fn = function(ctx) {
#     
#     list(
#       ok_flag = reactive({
#         TRUE  # replace with real logic
#       }),
#       
#       summary = reactive({
#         NULL  # replace with computed view state
#       })
#     )
#   },
#   
#   # =====================================================
#   # SIGNAL
#   # =====================================================
#   signal_fn = function(ctx, view) {
#     isTRUE(view$ok_flag())
#   },
#   
#   # =====================================================
#   # UI
#   # =====================================================
#   ui_fn = function(output, input, ctx, ns, view) {
#     
#     output$summary <- DT::renderDT({
#       req(view$summary())
#       view$summary()
#     })
#   }
# )


# ###################
# 🟦 TAB5–TAB8 (IDENTICAL PATTERN)
# 
# You ONLY change:
#   
#   tab_id
# view logic
# signal condition
# UI outputs
# TAB5
# mod_tab5_server <- make_tab_module(
#   tab_id = "tab5",
#   init_fn = function(input, ctx) {},
#   view_fn = function(ctx) list(ok_flag = reactive(TRUE)),
#   signal_fn = function(ctx, view) TRUE,
#   ui_fn = function(output, input, ctx, ns, view) {}
# )