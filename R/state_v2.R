
# state_v2.R
# =========================================================
# DEPRECATED — compute_v2_state() is no longer called.
# create_minimal_state() is retained for reference only.
# All state is now managed via ctx$state (see 02_context.R).
# =========================================================

#' @export
#'
create_minimal_state <- function() {
  

  shiny::reactiveValues(

    # Navigation (now ctx$state$stage)
    stage = "tab1",

    # Form inputs
    dataset_name = NULL,
    version      = NULL,
    description  = NULL,
    embargo      = NULL,

    obs_uploaded = FALSE,
    qa_checked   = FALSE,
    meta_uploaded = FALSE,

    # Derived flags (now ctx$state$*_ready)
    dataset_valid   = FALSE,
    ingestion_ready = FALSE,
    qa_ready        = FALSE,
    meta_ready      = FALSE,
    export_ready    = FALSE
  )
}

# compute_v2_state() removed — was only used to feed ctx$v2,
# which no longer exists. Logic migrated to individual modules
# writing directly to ctx$state$*_ready.