
#' @export
#' 
create_minimal_state <- function() {
  shiny::reactiveValues(
    
    # =====================================================
    # 🧭 GLOBAL NAVIGATION STATE
    # =====================================================
    stage = "tab1",   # tab1 → tab2 → tab3 → tab4
    
    # =====================================================
    # 🟦 STATE 1: DATASET DEFINITION (Tab1)
    # =====================================================
    dataset_valid = FALSE,
    dataset_ready = FALSE,
    
    # =====================================================
    # 🟩 STATE 2: DATA INGESTION (Tab2)
    # =====================================================
    obs_uploaded = FALSE,
    ingestion_valid = FALSE,
    
    # =====================================================
    # 🟨 STATE 3: OBSERVATION QA (Tab3)
    # =====================================================
    obs_verified = FALSE,
    qa_ready = FALSE,
    qa_issues = NULL,
    
    # =====================================================
    # 🟧 STATE 4: METADATA CONSTRUCTION (Tab4)
    # =====================================================
    meta_mode = NULL,         # "upload" | "in_app"
    meta_ready = FALSE,
    meta_valid = FALSE,
    
    # final export gate
    export_ready = FALSE
  )
}