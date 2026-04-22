

#' @export
build_rules <- function(sheet_listvariables, sheet_droplist) {
  
  rules <- sheet_listvariables %>%
    dplyr::transmute(
      table = Table,
      name  = Name,
      
      type = dplyr::case_when(
        grepl("varchar|text", `cell constraints`, ignore.case = TRUE) ~ "character",
        grepl("int|decimal|numeric", `cell constraints`, ignore.case = TRUE) ~ "numeric",
        grepl("date", `cell constraints`, ignore.case = TRUE) ~ "date",
        grepl("bool|logical", `cell constraints`, ignore.case = TRUE) ~ "logical",
        TRUE ~ "character"
      ),
      
      mandatory = !is.na(Mandatory) &
        grepl("mandatory", Mandatory, ignore.case = TRUE),
      
      # numeric range parsing
      min_val = suppressWarnings(as.numeric(stringr::str_match(`cell constraints`, "Range:\\s*([0-9.]+)")[,2])),
      max_val = suppressWarnings(as.numeric(stringr::str_match(`cell constraints`, "to\\s*([0-9.]+)")[,2])),
      
      # length parsing
      max_length = suppressWarnings(as.numeric(stringr::str_match(`cell constraints`, "max length:\\s*(\\d+)")[,2])),
      
      regex = ifelse(grepl("Regex:", `cell constraints`),
                     stringr::str_remove(`cell constraints`, ".*Regex:\\s*"),
                     NA),
      
      source = `data origin`
    )
  
  # attach dropdown values
  rules$options <- lapply(rules$name, function(col) {
    if (!is.null(sheet_droplist[[col]])) {
      unique(na.omit(sheet_droplist[[col]]))
    } else {
      NULL
    }
  })
  
  rules
}
