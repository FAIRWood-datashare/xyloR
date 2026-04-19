


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
  
  ctx$state <- reactiveValues(
    
    tab1 = list(
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
      done = FALSE
    ),
    
    tab2 = list(
      done = FALSE
    ),
    
    tab3 = list(
      done = FALSE
    )
    
    # you can extend later
  )
  
  ctx$data <- new.env(parent = emptyenv())
  ctx$files <- new.env(parent = emptyenv())
  
  ctx
}