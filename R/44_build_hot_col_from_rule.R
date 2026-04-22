

#' @export
build_hot_col_from_rule <- function(ht, col_name, rule) {
  
  # -------------------------
  # CHARACTER
  # -------------------------
  if (rule$type == "character") {
    
    ht <- rhandsontable::hot_col(
      ht,
      col_name,
      readOnly = isTRUE(rule$readOnly),
      renderer = renderer_char(
        required = rule$mandatory,
        min_length = NA,
        max_length = rule$max_length,
        regex_pattern = rule$regex
      )
    )
  }
  
  # -------------------------
  # NUMERIC
  # -------------------------
  if (rule$type == "numeric") {
    
    ht <- rhandsontable::hot_col(
      ht,
      col_name,
      type = "numeric",
      readOnly = isTRUE(rule$readOnly),
      renderer = renderer_num(
        required = rule$mandatory,
        min_val = rule$min_val,
        max_val = rule$max_val
      )
    )
  }
  
  # -------------------------
  # DROPDOWN
  # -------------------------
  if (!is.null(rule$options)) {
    
    ht <- rhandsontable::hot_col(
      ht,
      col_name,
      type = "dropdown",
      source = rule$options,
      readOnly = isTRUE(rule$readOnly),
      renderer = renderer_drop(
        required = rule$mandatory,
        options = rule$options
      )
    )
  }
  
  # -------------------------
  # DATE
  # -------------------------
  if (rule$type == "date") {
    
    ht <- rhandsontable::hot_col(
      ht,
      col_name,
      type = "date",
      dateFormat = "YYYY-MM-DD",
      renderer = renderer_date(rule$mandatory)
    )
  }
  
  ht
}