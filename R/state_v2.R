
#' @export
#' 
create_minimal_state <- function() {
  
  shiny::reactiveValues(
    
    # =====================================================
    # NAVIGATION STATE (ONLY CONTROL FLOW)
    # =====================================================
    stage = "tab1",
    
    # =====================================================
    # INPUT STATE (raw form inputs only)
    # =====================================================
    dataset_name = NULL,
    version = NULL,
    description = NULL,
    embargo = NULL,
    
    obs_uploaded = FALSE,
    qa_checked = FALSE,
    meta_uploaded = FALSE,
    
    # =====================================================
    # DERIVED FLAGS (written ONLY by reactor)
    # =====================================================
    dataset_valid = FALSE,
    ingestion_ready = FALSE,
    qa_ready = FALSE,
    meta_ready = FALSE,
    export_ready = FALSE
  )
}

# =====================================================
# V2 STATE ENGINE (single source of truth helper)
# =====================================================

compute_v2_state <- function(ctx) {
  
  dataset_valid <-
    !is.null(ctx$v2$dataset_name) &&
    nchar(ctx$v2$dataset_name) >= 3 &&
    nchar(ctx$v2$dataset_name) <= 8 &&
    grepl("^[A-Z0-9]+$", ctx$v2$dataset_name) &&
    !is.null(ctx$v2$version) &&
    ctx$v2$version >= 1 && ctx$v2$version <= 99 &&
    !is.null(ctx$v2$description) &&
    nchar(trimws(ctx$v2$description)) >= 50 &&
    !is.null(ctx$v2$embargo) &&
    as.Date(ctx$v2$embargo) >= Sys.Date()
  
  ingestion_ready <- isTRUE(ctx$v2$obs_uploaded)
  qa_ready        <- isTRUE(ctx$v2$qa_checked)
  meta_ready      <- isTRUE(ctx$v2$meta_uploaded)
  
  list(
    dataset_valid = dataset_valid,
    ingestion_ready = ingestion_ready,
    qa_ready = qa_ready,
    meta_ready = meta_ready,
    export_ready = dataset_valid && ingestion_ready && qa_ready && meta_ready
  )
}