


# =========================================================
# APP CONTEXT FACTORY
# =========================================================
# This replaces out_tab1/out_tab2/out_tab3/out_tab4
# with a single unified state object (ctx)
# =========================================================
#' @export
#' 
create_app_context <- function() {
  
  ctx <- shiny::reactiveValues(
    
    state = list(
      active_tab = NULL,
      dataset_name = NULL,
      description = NULL,
      embargo = NULL,
      version = NULL,
      initialized = FALSE
    ),
    
    files = list(
      wb_meta = NULL,
      wb_data = NULL,
      temp_folder = NULL,
      project_path = NULL,
      obs_file = NULL,
      meta_template = NULL
    ),
    
    data = list(),
    validation = list(),
    api = list(),
    ui = list(),
    
    config = list(
      skip_rows_excel = NULL,
      header_row_excel = NULL,
      factory_mode = TRUE
    )
  )
  
  # =========================================================
  # STEP 1 FIX: stable runtime ID for debugging
  # =========================================================
  ctx$.id <- paste0(
    "ctx_",
    paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
  )
  
  # =========================================================
  # 🧠 NEW: GLOBAL PIPELINE CONTROLLER (ADD THIS)
  # =========================================================
  ctx$pipeline <- shiny::reactiveValues(
    
    tab1 = "empty",
    tab2 = "empty",
    tab3 = "empty",
    tab4 = "empty",
    tab5 = "empty",
    tab6 = "empty",
    tab7 = "empty",
    tab8 = "empty",
    
    ready = FALSE
  )
  
  return(ctx)
}