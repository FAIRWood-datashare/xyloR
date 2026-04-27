


# validation_cross_tab.R
#' 
validate_cross_tab <- function(ctx) {
  
  errors <- list()
  warnings <- list()
  blockers <- list()
  
  obs  <- ctx$data$obs_clean
  meta <- ctx$data$meta
  
  # -------------------------
  # STRUCTURE CHECK
  # -------------------------
  if (is.null(obs)) {
    blockers <- c(blockers, list(list(
      type = "missing_obs",
      message = "OBS data missing",
      tab = "tab1",
      severity = "blocker",
      hint = "Upload observation file"
    )))
  }
  
  if (is.null(meta)) {
    blockers <- c(blockers, list(list(
      type = "missing_meta",
      message = "META data missing",
      tab = "tab2",
      severity = "blocker",
      hint = "Upload metadata file"
    )))
  }
  
  # -------------------------
  # ALIGNMENT CHECK (ROW-AWARE)
  # -------------------------
  if (!is.null(obs) && !is.null(meta)) {
    
    # ---- SITE CHECK (ROW LEVEL)
    bad_site_rows <- which(!obs$site_id %in% meta$site_id)
    
    if (length(bad_site_rows) > 0) {
      
      errors <- c(errors, lapply(bad_site_rows, function(i) {
        list(
          type = "site_mismatch",
          message = "Site mismatch between OBS and META",
          tab = "tab2",
          field = "site_id",
          row = i,
          severity = "error",
          hint = "Add missing site_id entries in META"
        )
      }))
    }
    
    # ---- TREE CHECK (ROW LEVEL)
    bad_tree_rows <- which(!obs$tree_id %in% meta$tree_id)
    
    if (length(bad_tree_rows) > 0) {
      
      errors <- c(errors, lapply(bad_tree_rows, function(i) {
        list(
          type = "tree_mapping",
          message = "Tree mapping incomplete",
          tab = "tab2",
          field = "tree_id",
          row = i,
          severity = "error",
          hint = "Ensure all tree_id values exist in META"
        )
      }))
    }
  }
  
  # -------------------------
  # META COMPLETENESS
  # -------------------------
  if (!is.null(meta)) {
    
    required_fields <- c("site_id", "tree_id", "person_id")
    missing <- setdiff(required_fields, names(meta))
    
    if (length(missing) > 0) {
      errors <- c(errors, list(list(
        type = "missing_fields",
        message = paste("Missing meta fields:", paste(missing, collapse = ", ")),
        tab = "tab2",
        severity = "error",
        hint = "Add missing columns to META template"
      )))
    }
  }
  
  # -------------------------
  # STORE RESULT
  # -------------------------
  ctx$validation$global <- list(
    errors = errors,
    warnings = warnings,
    blockers = blockers,
    ok = (length(blockers) == 0 && length(errors) == 0)
  )
  
  ctx
}