
# =========================================================
# XYLOR STATE ENGINE (FINAL ARCHITECTURE CORE)
# =========================================================

update_state_engine <- function(ctx) {
  
  name <- ctx$state$dataset_name %||% ""
  version <- ctx$state$version
  desc <- ctx$state$description %||% ""
  
  valid <- nzchar(name) &&
    nchar(name) >= 3 &&
    nchar(name) <= 8 &&
    grepl("^[A-Z0-9]+$", name) &&
    !is.na(version) &&
    version >= 1 && version <= 99 &&
    nzchar(trimws(desc)) &&
    nchar(trimws(desc)) >= 50
  
  ctx$state$metadata_valid <- valid
  ctx$state$button_enabled <- valid
}


compute_state <- function(ctx) {
  
  ctx$state$tab1$done <-
    isTRUE(ctx$state$tab1$confirmed)
  
  ctx$state$tab2$ready <-
    isTRUE(ctx$state$tab1$done)
  
  ctx
}