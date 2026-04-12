

create_exchange_zip <- function(
    obs_file,
    meta_file,
    output_zip,
    temp_dir,
    dataset_name,
    version = NULL,
    embargo = NULL,
    description = NULL
) {
  
  # 1. run transformation
  to_exchange_files(
    obs_file,
    meta_file,
    dir = temp_dir,
    dataset_name = dataset_name,
    version = version,
    embargo = embargo,
    description = description
  )
  
  # 2. collect files
  files_to_zip <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
  files_to_zip <- gsub("//", "/", files_to_zip)
  
  main_files <- files_to_zip[
    basename(files_to_zip) %in% c("tree.csv", "variables.csv", "zone_etude.csv") |
      grepl("xylo_data|xylo_meta", basename(files_to_zip))
  ]
  
  exchange_files <- setdiff(files_to_zip, main_files)
  
  # 3. create intermediate archive
  tmp_exchange <- file.path(temp_dir, "Exchange.zip")
  zip::zipr(zipfile = tmp_exchange, files = exchange_files)
  
  # 4. final zip
  zip::zipr(zipfile = output_zip, files = c(main_files, tmp_exchange))
  
  # 5. cleanup
  file.remove(tmp_exchange)
  
  return(output_zip)
}