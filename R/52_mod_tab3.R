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
    title = "Observations",
    value = "tab3",
    
    shiny::fluidRow(
      
      # LEFT PANEL
      shiny::column(
        2,
        class = "bg-light p-2 border-end",
        
        bslib::card(
          bslib::card_body(
            shiny::actionButton(
              ns("save_obs"),
              "Save",
              class = "btn-primary"
            )
          )
        )
      ),
      
      # MAIN PANEL
      shiny::column(
        10,
        
        bslib::card(
          bslib::card_header("Site info"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl1"))
          )
        ),
        
        bslib::card(
          bslib::card_header("Observations"),
          bslib::card_body(
            rhandsontable::rHandsontableOutput(ns("tbl2"))
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
mod_tab3_server <- function(id, ctx, session) {
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      
      req(ctx$data$obs_truth)
      
      if (is.null(ctx$data$draft_obs)) {
        ctx$data$draft_obs <- ctx$data$obs_truth
        message("TAB3 init draft_obs")
      }
      
      if (is.null(ctx$data$tbl1) && !is.null(ctx$data$site_info)) {
        ctx$data$tbl1 <- ctx$data$site_info
        message("TAB3 init tbl1")
      }
    })
    
    observeEvent(input$tbl1, {
      ctx$data$tbl1 <- rhandsontable::hot_to_r(input$tbl1)
    })
    
    observeEvent(input$tbl2, {
      ctx$data$draft_obs <- rhandsontable::hot_to_r(input$tbl2)
    })
    
    observeEvent(input$save_obs, {
      
      ctx$data$obs_truth <- ctx$data$draft_obs
      
      message("TAB3 saved")
    })
    
    observe({
      
      req(ctx$data$obs_truth, ctx$data$draft_obs)
      
      ctx$signals$tab3_done <- identical(
        ctx$data$obs_truth,
        ctx$data$draft_obs
      )
      
      message("TAB3 signal:", ctx$signals$tab3_done)
    })
    
    invisible(NULL)
  })
}

