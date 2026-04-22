

#' @export
build_validation_rules <- function(sheet_listvariables) {
  
  sheet_listvariables %>%
    dplyr::transmute(
      table = Table,
      name  = Name,
      
      # --- type inference ---
      type = dplyr::case_when(
        grepl("varchar", `cell constraints`, ignore.case = TRUE) ~ "character",
        grepl("int|decimal|numeric", `cell constraints`, ignore.case = TRUE) ~ "numeric",
        grepl("date", `cell constraints`, ignore.case = TRUE) ~ "date",
        grepl("bool|logical", `cell constraints`, ignore.case = TRUE) ~ "logical",
        TRUE ~ "character"
      ),
      
      # --- constraints (parsed ONCE here) ---
      mandatory = !is.na(Mandatory) &
        grepl("mandatory", Mandatory, ignore.case = TRUE),
      
      max_length = suppressWarnings(
        as.numeric(gsub("\\D", "", `cell constraints`))
      ),
      
      domain = Domain,
      
      source = `data origin`
    )
}