

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
  obs_valid <- validate_obs(obs)
  
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
      obs = obs_valid,
      site = site_valid,
      tree = tree_valid,
      sample = sample_valid
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
    )
  )
}