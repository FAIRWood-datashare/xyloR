

# state_engine.R
#' 
compute_export_state <- function(ctx) {
  
  blockers <- ctx$validation$global$blockers
  
  ctx$state$export_ready <-
    is.null(blockers) || length(blockers) == 0
  
  # Canonical names (replaces tab1_ready / tab2_ready / tab3_ready)
  ctx$state$dataset_ready   <- !is.null(ctx$data$obs_raw)
  ctx$state$ingestion_ready <- !is.null(ctx$data$meta)
  
  ctx
}