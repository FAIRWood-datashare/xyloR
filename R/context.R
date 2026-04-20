


# =========================================================
# APP CONTEXT FACTORY
# =========================================================
# This replaces out_tab1/out_tab2/out_tab3/out_tab4
# with a single unified state object (ctx)
# =========================================================
#' @export
#' 
create_app_context <- function() {
  
  ctx <- new.env(parent = emptyenv())
  
  # =====================================================
  # LEGACY STATE (KEEP FOR NOW – TRANSITION PHASE)
  # =====================================================
  ctx$state <- shiny::reactiveValues(
    
    tab1 = list(
      inputdata = list(
        valid = FALSE,
        confirmed = FALSE
      ),
      file = list(
        uploaded = FALSE
      ),
      validation = list(
        all_valid = FALSE
      ),
      done = FALSE,
      nav_ready = FALSE
    ),
    
    tab2 = list(
      metadata = list(
        valid = FALSE,
        confirmed = FALSE
      ),
      file = list(
        uploaded = FALSE
      ),
      validation = list(
        all_valid = FALSE
      ),
      done = FALSE,
      nav_ready = FALSE
    ),
    
    tab3 = list(
      done = FALSE,
      nav_ready = FALSE
    )
  )
  
  # =====================================================
  # FSM 
  # =====================================================
  ctx$fsm <- shiny::reactiveValues(
    state = "TAB1",
    flags = list(
      tab1_complete = FALSE,
      tab2_complete = FALSE,
      tab3_complete = FALSE
    ),
    events = list(
      go_next = FALSE
    )
  )
  
  # =====================================================
  # FSM TICK (EVENT TRIGGER MECHANISM)
  # =====================================================
  ctx$fsm_tick <- shiny::reactiveVal(0)
  ctx$fsm_trigger <- shiny::reactiveVal(0)
  
  # =====================================================
  # DATA + FILE STORAGE
  # =====================================================
  ctx$data  <- new.env(parent = emptyenv())
  ctx$files <- new.env(parent = emptyenv())
  
  ctx
}