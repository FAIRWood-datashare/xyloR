


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
  # DATA LAYER (NO FLOW LOGIC)
  # =====================================================
  ctx$data <- shiny::reactiveValues(
    obs_truth = NULL,
    meta = NULL,
    draft_obs = NULL,
    tbl1 = NULL,
    site_info = NULL,
    validation_global = NULL
  )
  
  # =====================================================
  # FILES
  # =====================================================
  ctx$files <- shiny::reactiveValues(
    obs_file = NULL,
    meta_file = NULL,
    temp_folder = NULL
  )
  
  ctx
}