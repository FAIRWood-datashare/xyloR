
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


fsm_contract <- function(data_ok, ui_ok, engine_ok) {
  list(
    ready = isTRUE(data_ok && ui_ok && engine_ok),
    data_ok = data_ok,
    ui_ok = ui_ok,
    engine_ok = engine_ok
  )
}


tab1_contract <- function(ctx) {
  
  obs <- ctx$data$obs_truth
  
  data_ok <- is.data.frame(obs) && nrow(obs) > 0
  
  ui_ok <- isTRUE(ctx$data$tab1_ui_valid %||% FALSE) &&
    isTRUE(ctx$data$tab1_validated %||% FALSE) &&
    isTRUE(ctx$data$tab1_checks_ok %||% FALSE) &&
    isTRUE(ctx$data$tab1_complete %||% FALSE)
  
  engine_ok <- TRUE
  
  make_contract(data_ok, ui_ok, engine_ok)
}


tab2_contract <- function(ctx) {
  
  obs  <- ctx$data$obs_truth
  meta <- ctx$data$meta
  
  data_ok <- is.data.frame(obs) &&
    nrow(obs) > 0 &&
    is.list(meta) &&
    !is.null(meta$site)
  
  val <- ctx$data$validation_global %||% data.frame()
  
  engine_ok <- isTRUE(ctx$data$tab2_validation_done)
  
  make_contract(
    data_ok = data_ok,
    ui_ok = TRUE,
    engine_ok = engine_ok
  )
}
