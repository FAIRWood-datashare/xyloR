


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
  # STATE (reactiveValues = correct choice)
  # =====================================================
  ctx$state <- shiny::reactiveValues(
    
    current_tab = "tab1",
    
    tab1 = list(
      inputdata = list(
        valid = FALSE,
        confirmed = FALSE
      ),
      file = list(
        uploaded = FALSE
      ),
      validation = list(
        all_valid = FALSE
      ),
      done = FALSE,
      nav_ready = FALSE
    ),
    
    tab2 = list(
      metadata = list(
        valid = FALSE,
        confirmed = FALSE
      ),
      file = list(
        uploaded = FALSE
      ),
      validation = list(
        all_valid = FALSE
      ),
      done = FALSE,
      nav_ready = FALSE
    ),
    
    tab3 = list(
      done = FALSE,
      nav_ready = FALSE
    )
  )
  
  # DATA + FILE STORAGE
  ctx$data  <- new.env(parent = emptyenv())
  ctx$files <- new.env(parent = emptyenv())
  
  ctx
}