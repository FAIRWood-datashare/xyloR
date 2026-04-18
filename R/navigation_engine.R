

# =========================================================
# XYLOR NAVIGATION ENGINE (PHASE 1 CORE)
# =========================================================

navigation_engine <- function(ctx, session) {
  
  observe({
    
    # -----------------------------------------------------
    # TAB FLOW RULES (STRICT ORDERED LOGIC)
    # -----------------------------------------------------
    
    # TAB 1 → TAB 2
    if (isTRUE(ctx$state$tab1$ready_to_continue)) {
      
      message("➡️ NAV: tab1 → tab2")
      
      bslib::nav_select(
        id = "tabs",
        selected = "tab2",
        session = session
      )
      
      return()
    }
    
    # TAB 2 → TAB 3
    if (isTRUE(ctx$state$tab2$ready_to_continue)) {
      
      message("➡️ NAV: tab2 → tab3")
      
      bslib::nav_select(
        id = "tabs",
        selected = "tab3",
        session = session
      )
      
      return()
    }
    
  })
}