


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
  
  ctx$state <- shiny::reactiveValues(
    
    # RAW INPUT
    dataset_name = NULL,
    version = NULL,
    description = NULL,
    
    # DERIVED STATE
    metadata_valid = FALSE,
    button_enabled = FALSE,
    
    # CONFIRMED STATE
    metadata_confirmed = FALSE
  )
  
  ctx$data <- new.env(parent = emptyenv())
  ctx$files <- new.env(parent = emptyenv())
  
  ctx
}