


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
  # 1. FSM (NAVIGATION ONLY)
  # =====================================================
  ctx$fsm <- reactiveValues(
    state = "tab1"
  )
  
  ctx$fsm_trigger <- shiny::reactiveVal(0)
  
  # =====================================================
  # 2. DATA LAYER (SINGLE SOURCE OF TRUTH)
  # =====================================================
  ctx$data <- shiny::reactiveValues(
    
    # RAW DATA
    raw_obs  = NULL,
    raw_meta = NULL,
    
    # CORE DATASETS
    obs_truth = NULL,
    tbl1      = NULL,
    
    # EDIT BUFFERS
    draft_obs  = NULL,
    draft_meta = NULL,
    
    # VALIDATION OUTPUT
    validation_global = NULL,
    
    # SITE INFO (shared)
    site_info = NULL,
    
    # COLUMN CONFIGS (shared UI config, NOT state)
    column_configs = NULL,
    
    # TAB1 FLOW FLAGS
    tab1_ui_valid  = FALSE,
    tab1_validated = FALSE,
    tab1_checks_ok = FALSE,
    tab1_complete  = FALSE
  )
  
  # =====================================================
  # 3. FILE STORAGE — must be reactiveValues so observers
  #    (boot, upload) fire when files are assigned
  # =====================================================
  ctx$files <- shiny::reactiveValues(
    obs_file  = NULL,
    meta_file = NULL
  )
  
  ctx
}