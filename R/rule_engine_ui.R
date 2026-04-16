


apply_rules_to_table <- function(ht, sheet_name, rules) {
  
  sheet_rules <- rules[[sheet_name]]$columns
  
  for (col_name in names(sheet_rules)) {
    
    cfg <- sheet_rules[[col_name]]
    
    # -------------------------
    # CHARACTER
    # -------------------------
    if (is.null(cfg$type) || cfg$type == "character") {
      
      ht <- rhandsontable::hot_col(
        ht,
        col = col_name,
        readOnly = isTRUE(cfg$read_only)
      )
    }
    
    # -------------------------
    # NUMERIC
    # -------------------------
    if (cfg$type == "numeric") {
      
      validator_js <- htmlwidgets::JS(
        "function (value, callback) {
          if (value === null || value === '' || value === undefined) {
            callback(true);
          } else {
            callback(!isNaN(value));
          }
        }"
      )
      
      ht <- rhandsontable::hot_col(
        ht,
        col = col_name,
        type = "numeric",
        validator = validator_js,
        allowInvalid = TRUE,
        readOnly = isTRUE(cfg$read_only)
      )
    }
    
    # -------------------------
    # DROPDOWN
    # -------------------------
    if (cfg$type == "dropdown") {
      
      if (is.null(cfg$options)) {
        # temporary fallback (we'll connect domains later)
        options <- character(0)
      } else {
        options <- cfg$options
      }
      
      ht <- rhandsontable::hot_col(
        ht,
        col = col_name,
        type = "dropdown",
        source = options,
        readOnly = isTRUE(cfg$read_only)
      )
    }
    
    # -------------------------
    # DATE
    # -------------------------
    if (cfg$type == "date") {
      
      ht <- rhandsontable::hot_col(
        ht,
        col = col_name,
        type = "date",
        dateFormat = "YYYY-MM-DD",
        readOnly = isTRUE(cfg$read_only)
      )
    }
  }
  
  ht
}