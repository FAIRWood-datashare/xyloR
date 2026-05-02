xyloR <- function() {
  shiny::addResourcePath("www", file.path(getwd(), "www"))
  
  ui <- shiny::fluidPage(
    
    shinyjs::useShinyjs(),
    
    theme = bslib::bs_theme(
      bootswatch = "darkly",
      primary = "#375A7F"
    ),
    
    # =====================================================
    # EXTERNAL LIBS
    # =====================================================
    htmltools::tags$head(
      htmltools::tags$script(src = "https://unpkg.com/@popperjs/core@2"),
      htmltools::tags$script(src = "https://unpkg.com/tippy.js@6")
    ),
    
    # =====================================================
    # CSS
    # =====================================================
    htmltools::tags$link(rel = "stylesheet", href = "custom_styles.css"),
    
    # =====================================================
    # UI CONTROLS (UNDO / REDO / RESET)
    # =====================================================
    tags$div(
      style = "
      position: fixed;
      top: 10px;
      right: 10px;
      z-index: 9999;
      display: flex;
      gap: 6px;
    ",
      
      tags$button(
        "Undo",
        onclick = "AppUI.undo()",
        class = "btn btn-sm btn-secondary"
      ),
      
      tags$button(
        "Redo",
        onclick = "AppUI.redo()",
        class = "btn btn-sm btn-secondary"
      ),
      
      tags$button(
        "Reset",
        onclick = "AppUI.reset()",
        class = "btn btn-sm btn-danger"
      )
    ),
    
    # =====================================================
    # JS LOADER (FIXED PATHS FOR SHINY)
    # =====================================================
    tags$script(HTML("
console.log('🚀 Loading JS modules');

function load(src) {
  return new Promise((res, rej) => {
    const s = document.createElement('script');
    s.src = src;
    s.onload = res;
    s.onerror = () => rej(src);
    document.head.appendChild(s);
  });
}

async function boot() {

  const files = [
    '/www/core/state.js',
    '/www/core/engine.js',
    '/www/core/history.js',
    '/www/core/cmd.js',
    '/www/core/render.js',
    '/www/app.js'
  ];

  for (const f of files) {
    await load(f);
  }

  console.log('✅ JS loaded');

  // =========================
  // SAFE INIT ORDER (FIXED)
  // =========================

  window.AppState?.init?.();
  window.AppRender?.init?.();

  // timeline must start AFTER history + state exist
  window.AppTimeline?.init?.();

  window.AppRender?.render?.();

  // =========================
  // UI BRIDGE (SAFE + STABLE)
  // =========================
  window.AppUI = {
    undo: () => window.App?.undo?.(),
    redo: () => window.App?.redo?.(),
    reset: () => {
      window.AppHistory?.reset?.(window.AppState?.get?.());
      console.log('history reset via UI');
    }
  };

}

boot();
")),
    
    # =====================================================
    # SHINY ↔ JS BRIDGE
    # =====================================================
    tags$script(HTML("
    Shiny.addCustomMessageHandler('cmdk-event', function(msg) {

      if (msg.type === 'navigate') {
        Shiny.setInputValue('js_nav', msg.payload, {priority: 'event'});
      }

      if (msg.type === 'invalidate') {
        Shiny.setInputValue('js_invalidate', msg.payload, {priority: 'event'});
      }

    });
  ")),
    
    # =====================================================
    # TITLE
    # =====================================================
    shiny::titlePanel("GloboXylo Data Collector"),
    
    # =====================================================
    # ROOT DOM
    # =====================================================
    tags$div(id = "main"),
    tags$div(id = "command-root"),
    
    tags$div(
      id = "timeline-inspector",
      style = "
    position: fixed;
    left: 10px;
    bottom: 10px;
    width: 260px;
    max-height: 300px;
    overflow: auto;
    background: rgba(0,0,0,0.75);
    color: white;
    padding: 10px;
    font-size: 12px;
    border-radius: 6px;
    z-index: 9999;
  "
    ),
    
    # =====================================================
    # TABS
    # =====================================================
    bslib::navset_card_tab(
      id = "tabs",
      
      bslib::nav_panel("Tab1",  value = "tab1",  mod_tab1_ui("tab1")),
      bslib::nav_panel("Tab2",  value = "tab2",  mod_tab2_ui("tab2")),
      bslib::nav_panel("Tab3",  value = "tab3",  mod_tab3_ui("tab3")),
      bslib::nav_panel("Tab4",  value = "tab4",  mod_tab4_ui("tab4")),
      bslib::nav_panel("Tab5",  value = "tab5",  mod_tab5_ui("tab5")),
      bslib::nav_panel("Tab6",  value = "tab6",  mod_tab6_ui("tab6")),
      bslib::nav_panel("Tab7",  value = "tab7",  mod_tab7_ui("tab7")),
      bslib::nav_panel("Tab8",  value = "tab8",  mod_tab8_ui("tab8")),
      bslib::nav_panel("Tab9",  value = "tab9",  mod_tab9_ui("tab9")),
      bslib::nav_panel("Tab10", value = "tab10", mod_tab10_ui("tab10")),
      bslib::nav_panel("Tab11", value = "tab11", mod_tab11_ui("tab11")),
      
      bslib::nav_panel("Debug", value = "debug", mod_debug_ui("debug"))
    )
  )
  
  server <- function(input, output, session) {
    
    # =====================================================
    # CONTEXT
    # =====================================================
    ctx <- create_app_context()
    
    # =====================================================
    # ENGINE API
    # =====================================================
    ctx$get_engine <- function() ctx$state$engine
    ctx$set_engine <- function(engine) ctx$state$engine <- engine
    
    ctx$invalidate_engine <- function(trigger = NULL) {
      ctx$state$engine <- NULL
      ctx$state$engine_tick <- Sys.time()
      
      ctx$debug$last_invalidation <- list(
        time = Sys.time(),
        trigger = trigger
      )
    }
    
    ctx$rebuild_engine <- function() {
      
      req(ctx$data$obs_raw)
      req(ctx$data$site_info)
      
      candidate <- update_state_engine(
        ctx$data$obs_raw,
        ctx$data$site_info,
        ctx$data$tree_info,
        ctx$data$sample_info
      )
      
      if (!validate_engine_structure(candidate)) return()
      
      ctx$set_engine(candidate)
    }
    
    # =====================================================
    # AUTO REBUILD
    # =====================================================
    shiny::observe({
      ctx$rebuild_engine()
    })
    
    # =====================================================
    # NAV SYNC (R → JS optional future use)
    # =====================================================
    shiny::observe({
      req(ctx$state$stage)
      
      bslib::nav_select(
        id = "tabs",
        selected = ctx$state$stage,
        session = session
      )
    })
    
    # =====================================================
    # 🔥 JS → R BRIDGE (THIS IS THE KEY FIX)
    # =====================================================
    shiny::observeEvent(input$js_nav, {
      
      req(input$js_nav)
      
      ctx$state$stage <- input$js_nav
      
      bslib::nav_select(
        id = "tabs",
        selected = input$js_nav,
        session = session
      )
    })
    
    shiny::observeEvent(input$js_invalidate, {
      
      req(input$js_invalidate)
      
      ctx$invalidate_engine(input$js_invalidate)
      ctx$rebuild_engine()
    })
    
    # =====================================================
    # HISTORY
    # =====================================================
    ctx$debug$history <- shiny::reactiveVal(data.frame(
      from = character(),
      to = character(),
      trigger = character(),
      timestamp = as.POSIXct(character())
    ))
    
    # =====================================================
    # MODULES
    # =====================================================
    mod_tab1_server("tab1", ctx, session)
    mod_tab2_server("tab2", ctx)
    mod_tab3_server("tab3", ctx)
    mod_tab4_server("tab4", ctx)
    mod_tab5_server("tab5", ctx)
    mod_tab6_server("tab6", ctx)
    mod_tab7_server("tab7", ctx)
    mod_tab8_server("tab8", ctx)
    mod_tab9_server("tab9", ctx)
    mod_tab10_server("tab10", ctx)
    mod_tab11_server("tab11", ctx)
    mod_debug_server("debug", ctx)
    
    # =====================================================
    # DEBUG OUTPUTS
    # =====================================================
    output$debug_stage <- shiny::renderText({
      paste0("stage: ", ctx$state$stage)
    })
    
    output$history_table <- DT::renderDataTable({
      DT::datatable(ctx$debug$history(), options = list(pageLength = 5))
    })
    
    output$graph <- visNetwork::renderVisNetwork({
      
      tabs <- paste0("tab", 1:4)
      
      nodes <- data.frame(
        id = tabs,
        label = tabs,
        color = ifelse(tabs == ctx$state$stage, "#00C853", "#2C3E50"),
        shape = "box"
      )
      
      edges <- data.frame(
        from = tabs[1:3],
        to   = tabs[2:4],
        arrows = "to"
      )
      
      visNetwork::visNetwork(nodes, edges)
    })
  }
  
  shiny::shinyApp(ui, server)
}