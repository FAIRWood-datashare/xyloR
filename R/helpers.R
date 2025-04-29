# helpers.R



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
  # If sample_date is numeric (Excel serial date format)
  if (all(!is.na(as.numeric(sample_date)))) {
    return(as.Date(as.numeric(sample_date), origin = "1899-12-30"))
  }
  
  # Otherwise, handle as character strings and parse using lubridate
  return(lubridate::parse_date_time(sample_date, orders = c("mdy", "dmy", "ymd")))
}