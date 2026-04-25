
#' @export
fsm_contract <- function(data_ok, ui_ok, engine_ok) {
  data_ok && ui_ok && engine_ok
}

tab1_contract <- function(ctx, tab1) {
  
  obs <- ctx$data$obs_truth
  
  data_ok <- is.data.frame(obs) && nrow(obs) > 0
  
  ui_ok <- isTRUE(tab1$input_valid %||% FALSE) &&
    isTRUE(tab1$file_uploaded %||% FALSE)
  
  engine_ok <- TRUE
  
  isTRUE(data_ok && ui_ok && engine_ok)
}

tab2_contract <- function(ctx) {
  
  obs  <- ctx$data$obs_truth
  meta <- ctx$data$meta
  
  data_ok <- is.data.frame(obs) &&
    nrow(obs) > 0 &&
    is.data.frame(meta) &&
    nrow(meta) > 0
  
  engine_ok <- is.data.frame(ctx$validation_global) &&
    nrow(ctx$validation_global) == 0
  
  fsm_contract(
    data_ok = data_ok,
    ui_ok = TRUE,
    engine_ok = engine_ok
  )
}