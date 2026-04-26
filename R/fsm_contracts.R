
#' @export
#' 
make_contract <- function(data_ok, ui_ok, engine_ok) {
  
  list(
    ready = isTRUE(data_ok && ui_ok && engine_ok),
    data_ok = isTRUE(data_ok),
    ui_ok = isTRUE(ui_ok),
    engine_ok = isTRUE(engine_ok)
  )
}

tab1_contract <- function(ctx) {
  
  obs <- ctx$data$obs_truth
  
  data_ok <- is.data.frame(obs) && nrow(obs) > 0
  ui_ok <- TRUE
  
  engine_ok <- isTRUE(ctx$signals$tab1_done)
  
  make_contract(data_ok, ui_ok, engine_ok)
}

tab2_contract <- function(ctx) {
  
  obs  <- ctx$data$obs_truth
  meta <- ctx$data$meta
  
  data_ok <- is.data.frame(obs) &&
    nrow(obs) > 0 &&
    is.list(meta) &&
    !is.null(meta)
  
  ui_ok <- TRUE
  
  engine_ok <- isTRUE(ctx$signals$tab2_done)
  
  make_contract(data_ok, ui_ok, engine_ok)
}

