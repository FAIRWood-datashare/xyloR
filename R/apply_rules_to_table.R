

#' @export
apply_rules_to_table <- function(ht, sheet_name, rules) {
  
  sheet_rules <- rules %>%
    dplyr::filter(table == sheet_name)
  
  for (i in seq_len(nrow(sheet_rules))) {
    
    rule <- sheet_rules[i, ]
    col  <- rule$name
    
    ht <- build_hot_col_from_rule(ht, col, rule)
  }
  
  ht
}