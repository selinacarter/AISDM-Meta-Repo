######------- Data Cleaning Functions --------#######

# Convert possible "\N" values to proper numeric NA values
clean_numeric <- function(x) {
  suppressWarnings(as.numeric(na_if(as.character(x), "\\N")))
}


###### ----- Aggregate CSVs ------ ######

aggregate_csvs <- function(path_parts,
                           output_name = "aggregated.csv",
                           dedupe = TRUE,
                           lat_col = NULL,
                           lon_col = NULL,
                           prefix = "",
                           impute_counts = FALSE,
                           calculate_county_change = FALSE,
                           county_group_cols = c("county_geoid", "county_name", "county_state", "ds")) {
  
  data_dir <- do.call(here::here, as.list(path_parts))
  
  cat("Reading from:", data_dir, "\n")
  
  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir)
  }
  
  csv_files <- list.files(
    path = data_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  if (length(csv_files) == 0) {
    stop("No CSV files found in: ", data_dir)
  }
  
  cat("Found", length(csv_files), "files\n")
  
  combined_data <- lapply(csv_files, function(f) {
    read_csv(f, show_col_types = FALSE) |>
      mutate(source_file = basename(f))
  }) |>
    bind_rows()
  
  if (dedupe) {
    combined_data <- combined_data |> distinct()
  }
  
  
  # Extract date and time from file names
  combined_data <- combined_data |>
    mutate(
      ds = as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", source_file)),
      hour = sub(".*_(\\d{4}-\\d{2}-\\d{2})_(\\d{4}).*", "\\2", source_file)
    )
  
  # Impute count columns if requested
  if (impute_counts) {
    cat("Imputing n_baseline and n_crisis...\n")
    
    combined_data <- impute_count_values(combined_data)
  }
  
  # Aggregate to county-date level and calculate new percent_change if requested
  if (calculate_county_change) {
    cat("Calculating county-level percent_change from n_baseline and n_crisis...\n")
    
    combined_data <- calculate_county_percent_change(
      df = combined_data,
      group_cols = county_group_cols
    )
  }
  
  output_dir <- here::here("Outputs")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_file <- file.path(output_dir, output_name)
  
  write_csv(combined_data, output_file)
  
  cat("Saved to:", output_file, "\n")
  cat("Rows:", nrow(combined_data), "\n")
  
  return(combined_data)
}


analyze_dataset <- function(df) {
  list(
    missing = df |>
      summarise(across(everything(), ~mean(is.na(.)))),

    numeric_summary = df |>
      summarise(across(where(is.numeric),
                       list(mean = mean, sd = sd),
                       na.rm = TRUE)),

    n_rows = nrow(df),
    n_cols = ncol(df)
  )
}


###### ------- Movement Between Places w/o Aggregation ----#####

mp_data_bing <- aggregate_csvs(
  path_parts = c("Movement Between Places During Crisis"),
  output_name = "tk_movement_between_places_bing.csv"
)



###### ------- Facebook Population w/o Aggregation ----#####

fb_data_bing <- aggregate_csvs(
  path_parts = c("Facebook Population During Crisis - Bing Tiles"),
  output_name = "facebook_pop_bing.csv",
  lat_col = "latitude",
  lon_col = "longitude",
  prefix = "",
  impute_counts = FALSE
)

