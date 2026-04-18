


# =========================================================
# APP CONTEXT FACTORY
# =========================================================
# This replaces out_tab1/out_tab2/out_tab3/out_tab4
# with a single unified state object (ctx)
# =========================================================
#' @export
#' 
create_app_context <- function() {
  
  list(
    
    # =====================================================
    # GLOBAL APP STATE ENGINE
    # =====================================================
    state = list(
      
      tab = list(
        current = "tab1",
        allowed_transition = TRUE
      ),
      
      tab1 = list(
        metadata_valid = FALSE,
        file_uploaded = FALSE,
        obs_ready = FALSE,
        ready_to_continue = FALSE
      ),
      
      tab2 = list(
        metadata_uploaded = FALSE,
        metadata_valid = FALSE,
        ready_to_continue = FALSE
      )
    ),
    
    # =====================================================
    # DATA STORE (PURE DATA ONLY)
    # =====================================================
    data = list(
      obs = NULL,
      meta = NULL
    ),
    
    # =====================================================
    # FILE STORE (RAW INPUT ONLY)
    # =====================================================
    files = list(
      obs_file = NULL,
      meta_file = NULL
    ),
    
    # =====================================================
    # EVENTS (OPTIONAL LOGGING LAYER)
    # =====================================================
    events = list()
  )
}