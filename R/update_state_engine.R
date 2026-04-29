

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

safe_build_obs <- function(x) {
  x %||% list()
}

safe_build_site <- function(x) {
  x %||% list()
}

safe_build_tree <- function(x) {
  x %||% list()
}

safe_build_sample <- function(x) {
  x %||% list()
}


# =====================================================
# 🔒 SAFE VALIDATORS (MINIMAL IMPLEMENTATION)
# =====================================================

safe_validate_obs <- function(x) {
  list(valid = TRUE, issues = character(0))
}

safe_validate_site <- function(x) {
  list(valid = TRUE, issues = character(0))
}

safe_validate_tree <- function(x) {
  list(valid = TRUE, issues = character(0))
}

safe_validate_sample <- function(x) {
  list(valid = TRUE, issues = character(0))
}

safe_validate_authors <- function(engine) {
  list(valid = TRUE, issues = character(0))
}

update_state_engine <- function(obs, site, tree, sample) {
  
  # =====================================================
  # 🧯 1. SAFE INPUT NORMALIZATION
  # =====================================================
  obs    <- obs    %||% list()
  site   <- site   %||% list()
  tree   <- tree   %||% list()
  sample <- sample %||% list()
  
  # =====================================================
  # 🧱 2. ENGINE SKELETON (STRICT CONTRACT)
  # =====================================================
  engine <- list(
    derived = list(
      obs = NULL,
      site = NULL,
      tree = NULL,
      sample = NULL
    ),
    
    validation = list(
      obs = list(valid = TRUE, issues = character(0)),
      site = list(valid = TRUE, issues = character(0)),
      tree = list(valid = TRUE, issues = character(0)),
      sample = list(valid = TRUE, issues = character(0)),
      authors = list(valid = TRUE, issues = character(0))
    ),
    
    meta = list(
      timestamp = Sys.time(),
      version = "v2-engine"
    )
  )
  
  # =====================================================
  # 🧪 3. DERIVED STATE (SAFE WRAPPERS)
  # =====================================================
  engine$derived$obs    <- safe_build_obs(obs)
  engine$derived$site   <- safe_build_site(site)
  engine$derived$tree   <- safe_build_tree(tree)
  engine$derived$sample <- safe_build_sample(sample)
  
  # =====================================================
  # 🔍 4. VALIDATION (DEFENSIVE)
  # =====================================================
  engine$validation$obs    <- safe_validate_obs(engine$derived$obs)
  engine$validation$site   <- safe_validate_site(engine$derived$site)
  engine$validation$tree   <- safe_validate_tree(engine$derived$tree)
  engine$validation$sample <- safe_validate_sample(engine$derived$sample)
  
  # Authors (example cross-domain validation)
  engine$validation$authors <- safe_validate_authors(engine)
  
  # =====================================================
  # 🧯 5. FINAL SANITIZATION (CRITICAL SAFETY LAYER)
  # =====================================================
  engine <- sanitize_engine(engine)
  
  return(engine)
}

validate_engine_structure <- function(engine) {
  
  if (is.null(engine)) return(FALSE)
  
  if (is.null(engine$derived)) return(FALSE)
  if (is.null(engine$validation)) return(FALSE)
  
  TRUE
}

safe_validate <- function(valid = TRUE, issues = character(0)) {
  
  issues <- issues %||% character(0)
  
  if (length(issues) == 0) {
    return(list(valid = TRUE, issues = character(0)))
  }
  
  list(
    valid = valid,
    issues = issues
  )
}

sanitize_engine <- function(engine) {
  
  # Ensure structure exists
  if (is.null(engine$derived)) engine$derived <- list()
  if (is.null(engine$validation)) engine$validation <- list()
  if (is.null(engine$meta)) engine$meta <- list()
  
  # Force all validation entries to valid format
  for (nm in names(engine$validation)) {
    
    v <- engine$validation[[nm]]
    
    if (is.null(v)) {
      engine$validation[[nm]] <- list(valid = TRUE, issues = character(0))
    }
    
    if (is.logical(v) && length(v) == 1) {
      engine$validation[[nm]] <- list(valid = v, issues = character(0))
    }
    
    if (is.list(v) && is.null(v$issues)) {
      v$issues <- character(0)
      engine$validation[[nm]] <- v
    }
  }
  
  # Ensure meta safety
  if (is.null(engine$meta$timestamp)) {
    engine$meta$timestamp <- Sys.time()
  }
  
  engine
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

