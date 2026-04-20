

# =========================================================
# XYLOR NAVIGATION ENGINE
# =========================================================

navigation_engine <- function(ctx, session) {
  
  observe({
    
    invalidateLater(300, session)
    
    current_tab <- ctx$state$current_tab
    
    # =====================================================
    # TAB 1 → TAB 2
    # =====================================================
    if (isTRUE(ctx$state$tab1$nav_ready) && current_tab != "tab2") {
      
      ctx$state$current_tab <- "tab2"
      
      message("➡️ NAV: TAB1 → TAB2")
      
      bslib::nav_select(
        id = "tabs",
        selected = "tab2",
        session = session
      )
      
      ctx$state$tab1$nav_ready <- FALSE
    }
    
    # =====================================================
    # TAB 2 → TAB 3
    # =====================================================
    if (isTRUE(ctx$state$tab2$nav_ready) && current_tab != "tab3") {
      
      ctx$state$current_tab <- "tab3"
      
      message("➡️ NAV: TAB2 → TAB3")
      
      bslib::nav_select(
        id = "tabs",
        selected = "tab3",
        session = session
      )
      
      ctx$state$tab2$nav_ready <- FALSE
    }
  })
}