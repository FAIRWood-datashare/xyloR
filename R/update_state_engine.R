

#' @export
#' 
# =====================================================
# 🧠 XYLOR V2 — UPDATE STATE ENGINE
# =====================================================
# Single source of truth for:
# - validation
# - derived structures
# - navigation flags
# - issue aggregation
# - summary stats
# =====================================================

update_state_engine <- function(obs, site = NULL, tree = NULL, sample = NULL) {
  
  # =====================================================
  # 1. OBS VALIDATION (raw integrity)
  # =====================================================
  if (is.null(obs) || nrow(obs) == 0) {
    return(list(
      
      validation = list(
        obs = list(valid = FALSE, issues = "OBS_NOT_INITIALIZED")
      ),
      
      flags = list(system_ok = FALSE),
      
      # =====================================================
      # ✅ STEP 3 — STATUS BLOCK (EARLY EXIT)
      # =====================================================
      status = list(
        ready = FALSE,
        missing = c("obs")
      )
    ))
  }
  
  # =====================================================
  # 2. SITE DERIVATION (NEW LAYER)
  # =====================================================
  site <- derive_site_from_obs(obs)
  
  site_valid <- validate_site(site, obs)
  
  # =====================================================
  # 3. TREE DERIVATION (FROM SITE, NOT OBS)
  # =====================================================
  tree <- derive_tree_from_site(site)
  
  tree_valid <- validate_tree(tree, site)
  
  # =====================================================
  # 4. SAMPLE DERIVATION (FROM TREE ONLY)
  # =====================================================
  sample <- derive_sample_from_tree(tree)
  
  sample_valid <- validate_sample(sample, tree)
  
  # =====================================================
  # 5. GLOBAL CONSOLIDATION
  # =====================================================
  list(
    
    raw = list(
      obs = obs
    ),
    
    derived = list(
      site = site,
      tree = tree,
      sample = sample,
      enrichment = list(
        authors = enrich_authors(sample),
        enrich_publications(sample)
      )
    ),
    
    export_ready = TRUE,
    
    validation = list(
      obs    = normalize_validation(obs_valid, "obs"),
      site   = normalize_validation(site_valid, "site"),
      tree   = normalize_validation(tree_valid, "tree"),
      sample = normalize_validation(sample_valid, "sample")
    ),
    
    flags = list(
      obs_ok = obs_valid$valid,
      site_ok = site_valid$valid,
      tree_ok = tree_valid$valid,
      sample_ok = sample_valid$valid,
      
      system_ok =
        obs_valid$valid &&
        site_valid$valid &&
        tree_valid$valid &&
        sample_valid$valid
    ),
    
    # =====================================================
    # ✅ STEP 3 — STATUS BLOCK (FULL STATE)
    # =====================================================
    status = list(
      ready = is.data.frame(obs) && nrow(obs) > 0,
      
      missing = names(which(c(
        obs    = is.null(obs),
        site   = is.null(site),
        tree   = is.null(tree),
        sample = is.null(sample)
      )))
    )
  )
}

normalize_validation <- function(x, domain) {
  
  # NULL case
  if (is.null(x)) {
    return(data.frame(
      domain = domain,
      id = NA_character_,
      severity = "ok",
      field = NA_character_,
      message = "No issues detected"
    ))
  }
  
  # FUNCTION (bug case)
  if (is.function(x)) {
    return(data.frame(
      domain = domain,
      id = NA_character_,
      severity = "error",
      field = NA_character_,
      message = "Invalid validation object: function"
    ))
  }
  
  # 🔥 LIST CASE (MOST IMPORTANT FOR YOU)
  if (is.list(x) && !is.data.frame(x)) {
    
    errors   <- x$errors %||% character(0)
    warnings <- x$warnings %||% character(0)
    
    # -------------------------------------------------
    # 🔥 NORMALIZE EMPTY CASES SAFELY
    # -------------------------------------------------
    if (length(errors) == 0 && length(warnings) == 0) {
      return(data.frame(
        domain = domain,
        id = NA_character_,
        severity = "ok",
        field = NA_character_,
        message = "No issues detected",
        stringsAsFactors = FALSE
      ))
    }
    
    # -------------------------------------------------
    # 🔥 BUILD SAFE VECTOR LENGTH
    # -------------------------------------------------
    messages <- c(errors, warnings)
    severity <- c(
      rep("error", length(errors)),
      rep("warning", length(warnings))
    )
    
    # safety guard (VERY IMPORTANT)
    n <- length(messages)
    
    df <- data.frame(
      domain = rep(domain, n),
      id = rep(NA_character_, n),
      severity = if (n > 0) severity else character(0),
      field = rep(NA_character_, n),
      message = if (n > 0) messages else character(0),
      stringsAsFactors = FALSE
    )
    
    return(df)
  }
  
  # DATA FRAME (already structured)
  if (is.data.frame(x)) {
    x$domain <- domain
    
    if (!"severity" %in% names(x)) x$severity <- "warning"
    if (!"message" %in% names(x)) x$message <- "unspecified issue"
    
    return(x)
  }
  
  # FALLBACK
  data.frame(
    domain = domain,
    id = NA_character_,
    severity = "error",
    field = NA_character_,
    message = "Unknown validation type"
  )
}

