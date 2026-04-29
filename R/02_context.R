


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
  # 📚 EXPORT REGISTRY (APPEND-ONLY LOG)
  # =====================================================
  ctx$registry <- list()
  ctx$registry$exports <- list()
  
  # =====================================================
  # 📦 DATA LAYERS
  # =====================================================
  ctx$data <- shiny::reactiveValues(
    obs_raw = NULL,
    obs_truth = NULL,
    meta = NULL,
    draft_obs = NULL,
    tbl1 = NULL,
    site_info = NULL
  )
  
  # =====================================================
  # 🧭 VIEW STATE
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
    obs_file = NULL,
    meta_file = NULL,
    temp_folder = NULL
  )
  
  # =====================================================
  # 🧪 VALIDATION STATE
  # =====================================================
  ctx$validation <- shiny::reactiveValues(
    global = NULL,
    last_run = NULL
  )
  
  # =====================================================
  # 🧭 APP STATE FLAGS
  # =====================================================
  ctx$state <- shiny::reactiveValues(
    export_ready = FALSE,
    tab1_ready = FALSE,
    tab2_ready = FALSE,
    tab3_ready = FALSE
  )
  
  # =====================================================
  # 🐞 DEBUG / TRACE
  # =====================================================
  ctx$debug <- shiny::reactiveValues(
    last_transition_from = NULL,
    last_transition_to = NULL,
    last_trigger = NULL,
    timestamp = NULL
  )
  
  # =====================================================
  # ✍️ EDIT CONTROL
  # =====================================================
  ctx$edit <- shiny::reactiveValues(
    pending_changes = list(),
    lock = FALSE
  )
  
  # =====================================================
  # 🧾 FORM INPUTS
  # =====================================================
  ctx$form <- shiny::reactiveValues(
    
    dataset_name = NULL,
    version = NULL,
    description = NULL,
    embargo = NULL,
    
    obs_file = NULL,
    
    metadata = NULL
  )
  
  # =====================================================
  # 🧠 ENRICHMENT CACHE (ORCID / DOI)  ← NEW
  # =====================================================
  ctx$cache <- shiny::reactiveValues(
    orcid = list(),
    doi = list()
  )
  
  # =====================================================
  # ⚙️ ENGINE CACHE (CORE STATE)
  # =====================================================
  ctx$engine_cache <- shiny::reactiveVal(NULL)
  
  # =====================================================
  # 📸 STATE SNAPSHOT (EXPORT FREEZE) ← NEW
  # =====================================================
  ctx$snapshot <- shiny::reactiveVal(NULL)
  
  return(ctx)
}