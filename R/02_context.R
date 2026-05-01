


# =========================================================
# APP CONTEXT FACTORY
# =========================================================
create_app_context <- function() {
  
  ctx <- new.env(parent = emptyenv())
  
  # =====================================================
  # 📚 EXPORT REGISTRY
  # =====================================================
  ctx$registry <- list()
  ctx$registry$exports <- list()
  
  # =====================================================
  # 📦 DATA LAYERS (RAW INPUT STATE)
  # =====================================================
  ctx$data <- shiny::reactiveValues(
    obs_raw     = NULL,
    obs_truth   = NULL,
    meta        = NULL,
    draft_obs   = NULL,
    tbl1        = NULL,
    site_info   = NULL,
    tree_info   = NULL,
    sample_info = NULL
  )
  
  # =====================================================
  # 🧭 VIEW STATE (optional UI state)
  # =====================================================
  ctx$view <- shiny::reactiveValues(
    tab1 = NULL,
    tab2 = NULL,
    tab3 = NULL
  )
  
  # =====================================================
  # 📁 FILE HANDLING
  # =====================================================
  ctx$files <- shiny::reactiveValues(
    obs_file    = NULL,
    meta_file   = NULL,
    temp_folder = NULL
  )
  
  # =====================================================
  # 🧪 VALIDATION STATE
  # =====================================================
  ctx$validation <- shiny::reactiveValues(
    global   = NULL,
    last_run = NULL
  )
  
  # =====================================================
  # 🎯 SINGLE SOURCE OF TRUTH: APP STATE
  # =====================================================
  ctx$state <- shiny::reactiveValues(
    stage           = "tab1",
    dataset_ready   = FALSE,
    ingestion_ready = FALSE,
    qa_ready        = FALSE,
    meta_ready      = FALSE,
    export_ready    = FALSE,
    
    engine          = NULL   # ✅ NEW SSOT
  )
  
  ctx$engine_get <- function() {
    ctx$get_engine()
  }
  
  ctx$engine_ready <- function() {
    !is.null(ctx$get_engine())
  }
  
  # =====================================================
  # 🐞 DEBUG / TRACE
  # =====================================================
  ctx$debug <- shiny::reactiveValues(
    last_transition_from = NULL,
    last_transition_to   = NULL,
    last_trigger         = NULL,
    timestamp            = NULL
  )
  
  # =====================================================
  # ✍️ EDIT CONTROL
  # =====================================================
  ctx$edit <- shiny::reactiveValues(
    pending_changes = list(),
    lock            = FALSE
  )
  
  # =====================================================
  # 🧾 FORM INPUTS
  # =====================================================
  ctx$form <- shiny::reactiveValues(
    dataset_name    = NULL,
    version         = NULL,
    description     = NULL,
    embargo         = NULL,
    obs_file        = NULL,
    metadata        = NULL,
    dataset_version = NULL
  )
  
  # =====================================================
  # 🧠 CACHE
  # =====================================================
  ctx$cache <- shiny::reactiveValues(
    orcid = list(),
    doi   = list()
  )
  
  # =====================================================
  # 🔄 READINESS FUNCTION (SAFE + PURE)
  # =====================================================
  ctx$update_ready <- function() {
    
    obs  <- ctx$data$obs_raw
    site <- ctx$data$site_info
    
    isTRUE(!is.null(obs) && !is.null(site))
  }
  
  return(ctx)
}