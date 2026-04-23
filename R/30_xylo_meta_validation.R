

#' @export
xylo_meta_validation <- function(meta) {
  
  # =====================================================
  # 1. HANDLE NULL
  # =====================================================
  if (is.null(meta)) {
    return(data.frame(
      type = "error",
      source = "meta",
      message = "Meta is NULL"
    ))
  }
  
  # =====================================================
  # 2. HANDLE WRONG INPUT TYPE (FILE PATH CASE)
  # =====================================================
  if (is.character(meta) && length(meta) == 1) {
    
    return(data.frame(
      type = "error",
      source = "meta",
      message = "Meta is a file path. Expected cleaned metadata object (data.frame/list)."
    ))
  }
  
  # =====================================================
  # 3. TYPE SAFETY (ONLY ACCEPT CLEAN OBJECTS)
  # =====================================================
  if (!is.data.frame(meta) && !is.list(meta)) {
    return(data.frame(
      type = "error",
      source = "meta",
      message = paste0(
        "Invalid meta type: ",
        class(meta),
        ". Expected data.frame or list."
      )
    ))
  }
  
  # =====================================================
  # 4. ENSURE SAFE EMPTY OUTPUT STRUCTURE EXISTS
  # =====================================================
  safe_error <- function(msg) {
    data.frame(
      type = "error",
      source = "meta",
      message = msg,
      stringsAsFactors = FALSE
    )
  }
  
  # =====================================================
  # 5. CORE VALIDATION
  # =====================================================
  tryCatch({
    
    result <- validate_metadata_core(meta)
    
    # enforce structure safety (prevents your UI crash)
    if (is.null(result) || !is.data.frame(result)) {
      return(safe_error("Validation returned invalid result (not a data.frame)"))
    }
    
    # ensure required columns exist
    required_cols <- c("type", "source", "message")
    missing_cols <- setdiff(required_cols, names(result))
    
    if (length(missing_cols) > 0) {
      return(safe_error(
        paste0("Validation missing columns: ", paste(missing_cols, collapse = ", "))
      ))
    }
    
    result
    
  }, error = function(e) {
    
    safe_error(e$message)
    
  })
}
