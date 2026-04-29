

#' @export
#'
external_metadata_module <- function(external) {
  
  # -------------------------
  # DOI STATE
  # -------------------------
  doi_metadata <- shiny::reactiveVal(NULL)
  doi_citation <- shiny::reactiveVal(NULL)
  
  fetch_doi <- function(doi) {
    
    meta <- get_doi_metadata(doi)
    cit  <- get_doi_citation(doi)
    
    doi_metadata(meta)
    doi_citation(cit)
  }
  
  # -------------------------
  # ORCID STATE
  # -------------------------
  orcid_results <- shiny::reactiveVal(NULL)
  
  fetch_orcid <- function(orcid = NULL, given = NULL, family = NULL) {
    
    df <- search_orcid(orcid, given, family)
    
    orcid_results(
      if (is.null(df)) NULL else format_orcid_results(df)
    )
  }
  
  # -------------------------
  # ROR STATE
  # -------------------------
  ror_results <- shiny::reactiveVal(NULL)
  
  fetch_ror <- function(query, country_code) {
    # keep your existing logic
  }
  
  # -------------------------
  # RETURN API (STABLE CONTRACT)
  # -------------------------
  list(
    
    doi = list(
      fetch    = fetch_doi,
      metadata = doi_metadata,
      citation = doi_citation
    ),
    
    orcid = list(
      fetch   = fetch_orcid,
      results = orcid_results
    ),
    
    ror = list(
      fetch   = fetch_ror,
      results = ror_results
    )
  )
}