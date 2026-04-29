
#' @export
#' 
# =====================================================
# DOI SERVICE
# =====================================================

get_doi_metadata <- function(doi) {
  
  res <- httr::GET(
    sprintf("https://citation.doi.org/metadata?doi=%s", URLencode(doi)),
    httr::timeout(5)
  )
  
  if (httr::status_code(res) != 200) return(NULL)
  
  tryCatch(
    jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
}

get_doi_citation <- function(doi) {
  
  res <- httr::GET(
    sprintf(
      "https://citation.doi.org/format?doi=%s&style=apa&lang=en-US",
      URLencode(doi)
    ),
    httr::timeout(5)
  )
  
  if (httr::status_code(res) != 200) return(NULL)
  
  httr::content(res, as = "text", encoding = "UTF-8")
}

format_doi_metadata <- function(meta) {
  
  if (is.null(meta) || (!is.list(meta) && !is.data.frame(meta))) {
    return(NULL)
  }
  
  authors <- meta$author
  
  list(
    title = if (!is.null(meta$title)) meta$title else "",
    first_author = if (!is.null(authors) && length(authors) > 0)
      authors$family[1] else "",
    journal = if (!is.null(meta$`container-title`))
      meta$`container-title` else "",
    year = if (!is.null(meta$issued))
      meta$issued$`date-parts`[1] else "",
    doi = if (!is.null(meta$DOI)) meta$DOI else ""
  )
}

# =====================================================
# ORCID SERVICE
# =====================================================

search_orcid <- function(query) {
  
  res <- httr::GET(
    paste0(
      "https://pub.orcid.org/v3.0/csv-search/",
      query,
      "&fl=family-name,given-names,email,orcid,current-institution-affiliation-name,other-names",
      "&rows=50"
    ),
    httr::timeout(5)
  )
  
  if (httr::status_code(res) != 200) return(NULL)
  
  df <- read.table(
    text = rawToChar(res$content),
    sep = ",",
    header = TRUE,
    stringsAsFactors = FALSE
  )
  
  if (nrow(df) == 0) return(NULL)
  
  df
}

format_orcid_results <- function(df) {
  
  df %>%
    dplyr::rename(
      last_name = family.name,
      first_name = given.names,
      orcid_id = orcid,
      org_name = current.institution.affiliation.name,
      other_names = other.names
    ) %>%
    dplyr::mutate(
      orcid_link = sprintf(
        "<a href='https://orcid.org/%s' target='_blank'>%s</a>",
        orcid_id, orcid_id
      )
    )
}