# helpers.R

standard_module_output <- function(data = NULL, api = NULL, ui = NULL, meta = NULL) {
  list(
    data = data,
    api  = api,
    ui   = ui,
    meta = meta
  )
}

# Generic CRUD helpers for Shiny + rhandsontable
crud_load_excel <- function(wb, sheet, skip_rows = 6) {
  df <- openxlsx::readWorkbook(
    wb,
    sheet = sheet,
    startRow = 1,
    colNames = TRUE
  )
  
  df <- df[-(1:skip_rows), , drop = FALSE]
  
  tibble::as_tibble(df)
}

crud_add_update <- function(data, new_row, edit_mode, selected_row) {
  new_row <- as.data.frame(new_row, stringsAsFactors = FALSE)
  
  if (isTRUE(edit_mode) && !is.null(selected_row)) {
    data[selected_row, names(new_row)] <- new_row[1, ]
  } else {
    data <- dplyr::bind_rows(data, new_row)
  }
  
  tibble::as_tibble(data)
}

crud_delete <- function(data, selected_row) {
  if (is.null(selected_row)) return(data)
  if (selected_row > nrow(data)) return(data)
  
  data <- data[-selected_row, , drop = FALSE]
  data
}

crud_reorder <- function(data, order_col) {
  if (!order_col %in% names(data)) return(data)
  
  data[[order_col]] <- suppressWarnings(as.numeric(data[[order_col]]))
  
  data <- data[order(data[[order_col]], na.last = TRUE), , drop = FALSE]
  
  data[[order_col]] <- seq_len(nrow(data))
  
  data
}

# Card 1.1
# Function to update card header class based on validation result
update_card_header_class <- function(is_valid) {
  if (is_valid) {
    shinyjs::addClass("card_header1_1", "bg-success")
    shinyjs::removeClass("card_header1_1", "bg-danger")
  } else {
    shinyjs::addClass("card_header1_1", "bg-danger")
    shinyjs::removeClass("card_header1_1", "bg-success")
  }
}


# Card 1.2
# Helper function to update card header to 'success'
update_card_header_success <- function() {
  shinyjs::addClass(id = "card_header1_2", class = "bg-success")  # Green header for success
  shinyjs::removeClass(id = "card_header1_2", class = "bg-warning")  # Remove warning (yellow)
}


# Card 4
# Helper function to parse sample date
parse_sample_dates <- function(sample_date) {
  
  # Always work as character first (prevents coercion bugs)
  x <- trimws(as.character(sample_date))
  
  # Empty safety
  x[x %in% c("", "NA", "N/A")] <- NA
  
  # -----------------------------
  # 1. Excel numeric detection
  # -----------------------------
  is_excel_numeric <- suppressWarnings(!is.na(as.numeric(x))) & !is.na(x)
  
  out <- rep(as.Date(NA), length(x))
  
  if (any(is_excel_numeric, na.rm = TRUE)) {
    out[is_excel_numeric] <- as.Date(
      as.numeric(x[is_excel_numeric]),
      origin = "1899-12-30"
    )
  }
  
  # -----------------------------
  # 2. Everything else (robust parsing)
  # -----------------------------
  other <- !is_excel_numeric & !is.na(x)
  
  if (any(other)) {
    parsed <- suppressWarnings(
      lubridate::parse_date_time(
        x[other],
        orders = c("ymd", "dmy", "mdy"),
        quiet = TRUE
      )
    )
    
    out[other] <- as.Date(parsed)
  }
  
  return(out)
}