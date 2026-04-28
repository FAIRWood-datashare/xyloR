


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
  
  ctx$fsm <- shiny::reactiveValues(
    state = "tab1",
    focus_tab = NULL,
    focus_field = NULL,
    focus_row = NULL,
    focus_id = NULL
  )
  
  ctx$signals <- shiny::reactiveValues(
    tab1_done = FALSE,
    tab2_done = FALSE,
    tab3_done = FALSE
  )
  
  ctx$data <- shiny::reactiveValues(
    obs_raw = NULL,        # ADD
    obs_truth = NULL,
    meta = NULL,
    draft_obs = NULL,
    tbl1 = NULL,
    site_info = NULL
  )
  
  ctx$view <- shiny::reactiveValues(
    tab1 = NULL,
    tab2 = NULL,
    tab3 = NULL
  )
  
  ctx$files <- shiny::reactiveValues(
    obs_file = NULL,
    meta_file = NULL,
    temp_folder = NULL
  )
  
  ctx$validation <- shiny::reactiveValues(
    global = NULL,
    last_run = NULL
  )
  
  ctx$state <- shiny::reactiveValues(
    export_ready = FALSE,
    tab1_ready = FALSE,
    tab2_ready = FALSE,
    tab3_ready = FALSE
  )
  
  ctx$debug <- shiny::reactiveValues(
    last_transition_from = NULL,
    last_transition_to = NULL,
    last_trigger = NULL,
    timestamp = NULL
  )
  
  ctx$edit <- shiny::reactiveValues(
    pending_changes = list(),
    lock = FALSE
  )
  
  ctx$form <- shiny::reactiveValues(
    
    dataset_name = NULL,
    version = NULL,
    description = NULL,
    embargo = NULL,
    
    obs_file = NULL,
    
    metadata = NULL
  )
  
  # ctx$ui <- shiny::reactiveValues(
  #   
  #   tab1 = list(
  #     header_valid = FALSE,
  #     submit_valid = FALSE
  #   ),
  #   
  #   tab2 = list(
  #     header_valid = FALSE
  #   ),
  #   
  #   tab3 = list(
  #     header_valid = FALSE
  #   )
  # )
  
  ctx
}