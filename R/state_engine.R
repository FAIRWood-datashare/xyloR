

# =========================================================
# XYLOR STATE ENGINE (PHASE 1 CORE)
# =========================================================

# ---------------------------------------------------------
# SAFE STATE WRITER
# ---------------------------------------------------------
set_state <- function(ctx, path, value) {
  
  # Example path: "tab1.obs_ready"
  parts <- strsplit(path, "\\.")[[1]]
  
  ref <- ctx$state
  
  # Navigate down the nested list
  for (i in seq_len(length(parts) - 1)) {
    ref <- ref[[parts[i]]]
  }
  
  # Assign value
  ref[[parts[length(parts)]]] <- value
  
  # Return ctx (IMPORTANT: still immutable-style usage)
  ctx
}