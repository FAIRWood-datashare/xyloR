#' mod_tab3 UI Function
#'
#' @description A shiny module for the "Observation" tab.
#'
#' @param id A string that serves as the module namespace identifier.
#' @return A `shiny.tag.list` containing the UI elements of the module.
#'
#' @import shiny
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#'
mod_tab3_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Export",
    value = "tab3",
    
    shiny::fluidRow(
      
      # =====================================================
      # LEFT PANEL (EXPORT CONTROL)
      # =====================================================
      shiny::column(
        3,
        class = "bg-light p-2 border-end",
        
        bslib::card(
          bslib::card_header("3.1 Export Status"),
          bslib::card_body(
            shiny::textOutput(ns("export_status"))
          )
        ),
        
        bslib::card(
          bslib::card_header("3.2 Actions"),
          bslib::card_body(
            shiny::uiOutput(ns("export_button")),
            shiny::downloadButton(ns("download_log"), "Log")
          )
        ),
        
        bslib::card(
          bslib::card_header("Validation Summary"),
          bslib::card_body(
            shiny::uiOutput(ns("final_validation"))
          )
        )
      ),
      
      # =====================================================
      # RIGHT PANEL (SUMMARY VIEW)
      # =====================================================
      shiny::column(
        9,
        
        bslib::card(
          bslib::card_header("Final Structure Overview"),
          bslib::card_body(
            plotly::plotlyOutput(ns("final_hierarchy"))
          )
        ),
        
        bslib::card(
          bslib::card_header("Export Preview"),
          bslib::card_body(
            DT::DTOutput(ns("export_preview"))
          )
        )
      )
    )
  )
}

#' mod_tab3 Server Function
#'
#' @description Server logic for the "Observation" tab module.
#'
#' @param id A string that serves as the module namespace identifier.
#' @param ctx The centralized app context (environment).
#' @param session The parent Shiny session.
#'
#' @return A list with WB, WB_meta, data_in, column_configs (for downstream tabs).
#' @export
#'
#' @import shiny
#' @importFrom shinyjs enable disable
#' @importFrom openxlsx loadWorkbook readWorkbook saveWorkbook
#' @importFrom bslib nav_panel card card_header card_body
#' @importFrom htmltools tagList div
#' @importFrom bsicons bs_icon
#'
mod_tab3_server <- make_tab_module(
  tab_id = "tab3",
  
  init_fn = function(input, ctx) {
    
    # ONLY initialization (no observers)
    
    if (!is.null(ctx$data$obs_truth) && is.null(ctx$data$draft_obs)) {
      ctx$data$draft_obs <- ctx$data$obs_truth
      
      # 🔥 FREEZE VIEW SNAPSHOT
      ctx$view$tbl2 <- ctx$data$draft_obs
    }
    
    if (!is.null(ctx$data$site_info) && is.null(ctx$data$tbl1)) {
      ctx$data$tbl1 <- ctx$data$site_info
      
      # 🔥 FREEZE VIEW SNAPSHOT
      ctx$view$tbl1 <- ctx$data$tbl1
    }
  },
  
  view_fn = function(ctx) {
    
    list(
      tbl1 = reactive(ctx$view$tbl1),
      tbl2 = reactive(ctx$view$tbl2),
      
      is_synced = reactive({
        req(ctx$data$obs_truth, ctx$data$draft_obs)
        identical(ctx$data$obs_truth, ctx$data$draft_obs)
      }),
      
      tab3_ready = reactive({
        req(ctx$data$obs_truth, ctx$data$draft_obs)
        identical(ctx$data$obs_truth, ctx$data$draft_obs)
      })
    )
  },
  
  signal_fn = function(ctx, view) {
    isTRUE(view$tab3_ready())
  },
  
  ui_fn = function(output, input, ctx, ns, view) {
    
    # =====================================================
    # RENDER ONLY FROM VIEW (CRITICAL FIX)
    # =====================================================
    output$tbl1 <- rhandsontable::renderRHandsontable({
      req(view$tbl1())
      rhandsontable::rhandsontable(view$tbl1())
    })
    
    output$tbl2 <- rhandsontable::renderRHandsontable({
      req(view$tbl2())
      rhandsontable::rhandsontable(view$tbl2())
    })
    
    output$sync_status <- shiny::renderUI({
      
      if (isTRUE(view$tab3_ready())) {
        shiny::tags$div(
          class = "alert alert-success",
          "✔ Tables are synchronized"
        )
      } else {
        shiny::tags$div(
          class = "alert alert-warning",
          "⚠ Unsaved changes detected"
        )
      }
    })
    
    # =====================================================
    # ONLY WRITE POINT (NO VIEW TOUCHING ELSEWHERE)
    # =====================================================
    observeEvent(input$save_obs, {
      
      req(input$tbl1, input$tbl2)
      
      ctx$data$tbl1 <- rhandsontable::hot_to_r(input$tbl1)
      ctx$data$draft_obs <- rhandsontable::hot_to_r(input$tbl2)
      
      # 🔥 UPDATE VIEW ONLY HERE (NO REACTIVITY CHAINS)
      ctx$view$tbl1 <- ctx$data$tbl1
      ctx$view$tbl2 <- ctx$data$draft_obs
      
      message("💾 SAFE SAVE COMMIT")
    })
  }
)