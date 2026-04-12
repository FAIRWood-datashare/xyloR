

#' @export
build_metadata_hierarchy <- function(meta_file_path) {
  
  sheet_names <- setdiff(
    readxl::excel_sheets(meta_file_path),
    c("instructions", "DropList", "ListOfVariables")
  )
  
  sheet_data <- setNames(
    lapply(sheet_names, function(sheet) {
      readxl::read_excel(meta_file_path, sheet)[-c(1:6), ]
    }),
    sheet_names
  )
  
  # ensure date format
  sheet_data[["sample"]]$sample_date <- as.Date(
    as.numeric(sheet_data[["sample"]]$sample_date),
    origin = "1899-12-30"
  )
  
  # -----------------------------
  # JOIN DATA
  # -----------------------------
  df_joined <- dplyr::left_join(
    sheet_data[["sample"]],
    sheet_data[["tree"]],
    by = "tree_label"
  ) %>%
    dplyr::left_join(
      sheet_data[["site"]],
      by = "site_label"
    ) %>%
    dplyr::mutate(
      
      plot_label_clean = dplyr::coalesce(plot_label.x, plot_label.y),
      year = lubridate::year(sample_date),
      network_label = dplyr::coalesce(network_label, site_label),
      
      site_label_full = ifelse(
        site_label == network_label | is.na(site_label),
        paste0(network_label, "_site"),
        paste0(network_label, "__", site_label)
      ),
      
      plot_label_full = ifelse(
        is.na(plot_label_clean) | plot_label_clean == site_label,
        paste0(site_label_full, "_plot"),
        paste0(site_label_full, "__", plot_label_clean)
      ),
      
      tree_label_full = paste0(plot_label_full, "__", tree_label),
      
      year_label   = paste0(tree_label_full, "__", year),
      sample_label = paste0(year_label, "__", sample_id)
    )
  
  # -----------------------------
  # BUILD HIERARCHY LEVELS
  # -----------------------------
  
  df_tree <- df_joined %>%
    dplyr::count(tree_label_full, plot_label_full, name = "value") %>%
    dplyr::rename(id = tree_label_full, parent = plot_label_full)
  
  df_plot <- df_joined %>%
    dplyr::distinct(plot_label_full, site_label_full) %>%
    dplyr::count(plot_label_full, site_label_full, name = "value") %>%
    dplyr::rename(id = plot_label_full, parent = site_label_full)
  
  df_site <- df_joined %>%
    dplyr::distinct(site_label_full, network_label) %>%
    dplyr::count(site_label_full, network_label, name = "value") %>%
    dplyr::rename(id = site_label_full, parent = network_label)
  
  df_network <- df_joined %>%
    dplyr::distinct(network_label) %>%
    dplyr::mutate(
      id = network_label,
      parent = "",
      value = 1
    )
  
  # -----------------------------
  # FINAL HIERARCHY
  # -----------------------------
  
  df_hierarchy <- dplyr::bind_rows(df_network, df_site, df_plot, df_tree) %>%
    dplyr::distinct(id, parent, value) %>%
    dplyr::arrange(parent, id) %>%
    dplyr::mutate(
      label = sub(".*__", "", id),
      text  = paste0(label, " (", value, ")")
    )
  
  return(df_hierarchy)
}