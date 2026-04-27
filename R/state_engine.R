

# state_engine.R
#' 
compute_export_state <- function(ctx) {
  
  blockers <- ctx$validation$global$blockers
  
  ctx$state$export_ready <-
    is.null(blockers) || length(blockers) == 0
  
  ctx$state$tab1_ready <-
    !is.null(ctx$data$obs_raw)
  
  ctx$state$tab2_ready <-
    !is.null(ctx$data$meta)
  
  ctx
}