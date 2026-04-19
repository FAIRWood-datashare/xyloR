

# =========================================================
# XYLOR NAVIGATION ENGINE (PHASE 1 CORE)
# =========================================================

navigation_engine <- function(ctx, session) {
  
  observe({
    
    compute_state(ctx)
    
    if (isTRUE(ctx$state$tab1$done)) {
      
      bslib::nav_select(
        id = "tabs",
        selected = "tab2"
      )
    }
    
  })
}