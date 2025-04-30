#' Get country codes
#' 
#' @export
#' 
get_country_codes <- function(){
  file_path <- system.file("extdata", "country_ISO3166-1-alpha-2_20241205.csv", package = "xyloR")
  iso_countries <- read.csv(file_path, stringsAsFactors = FALSE, na.strings=c(""))
  country_list <- setNames(iso_countries$Code, 
                           paste(iso_countries$Name, "  (", 
                                 iso_countries$Code, ")", sep = ""))
  return(country_list)
}

#' Helper to create Authors Input UI
#' 
#' @param author_nr nummer of the author
#' @import shiny
#' @export
#' 
author_input <- function(author_nr){
  tagList(
    span(p(paste0('Author ', author_nr)),style="font-weight:bold; color: #006268"),
    splitLayout(
      textInput(paste0('autname_', author_nr), NULL, "", placeholder = "Last name, first name"),
      textInput(paste0('autmail_', author_nr), NULL, "", placeholder = "name@example.com"),
      textInput(paste0('autaff_', author_nr), NULL, "", placeholder = "ROR of Affiliation")
    )
  )
}

# max_char_limit <- function(value, limit) {
#   if (nchar(value) > limit) paste0("Input exceeds ", limit, " characters.")
# }