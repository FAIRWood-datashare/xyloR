


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
  # FSM STATE (NAVIGATION ONLY)
  # =====================================================
  ctx$fsm <- shiny::reactiveValues(
    state = "tab1"
  )
  
  # =====================================================
  # SIGNALS (FSM INPUT ONLY)
  # =====================================================
  ctx$signals <- shiny::reactiveValues(
    tab1_done = FALSE,
    tab2_done = FALSE,
    tab3_done = FALSE
  )
  
  # =====================================================
  # DATA LAYER (SOURCE OF TRUTH ONLY)
  # =====================================================
  ctx$data <- shiny::reactiveValues(
    obs_truth = NULL,
    meta = NULL,
    draft_obs = NULL,
    tbl1 = NULL,
    site_info = NULL
  )
  
  # =====================================================
  # VIEW LAYER (STRUCTURED, NOT FREE-FORM)
  # =====================================================
  ctx$view <- shiny::reactiveValues(
    
    # TAB1 VIEW STATE
    tab1 = NULL,
    
    # TAB2 VIEW STATE
    tab2 = NULL,
    
    # TAB3 VIEW STATE
    tab3 = NULL
  )
  
  # =====================================================
  # FILES LAYER
  # =====================================================
  ctx$files <- shiny::reactiveValues(
    obs_file = NULL,
    meta_file = NULL,
    temp_folder = NULL
  )
  
  # =====================================================
  # DEBUG
  # =====================================================
  ctx$debug <- shiny::reactiveValues(
    last_transition_from = NULL,
    last_transition_to = NULL,
    last_trigger = NULL,
    timestamp = NULL
  )
  
  ctx
}