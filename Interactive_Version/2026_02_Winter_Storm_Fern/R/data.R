library(arrow)
library(dplyr)
library(lubridate)
library(sf)
library(stringr)
library(quadkeyr)


# ============================================================
# Paths
# ============================================================

OUTPUTS_DIR <- "outputs"
DATA_DIR <- "data"
CACHE_DIR <- "cache"


if (!dir.exists(CACHE_DIR)) {
  dir.create(
    CACHE_DIR,
    recursive = TRUE
  )
}


# ============================================================
# Datetime
#
# Original data:
# America/Los_Angeles -> America/New_York
# ============================================================

make_datetime <- function(ds, hour) {
  
  hour_numeric <- suppressWarnings(
    as.integer(
      as.character(hour)
    )
  )
  
  hour_chr <- sprintf(
    "%04d",
    hour_numeric
  )
  
  datetime_string <- paste(
    as.character(ds),
    hour_chr
  )
  
  datetime_la <- as.POSIXct(
    datetime_string,
    format = "%Y-%m-%d %H%M",
    tz = "America/Los_Angeles"
  )
  
  lubridate::with_tz(
    datetime_la,
    tzone = "America/New_York"
  )
}


# ============================================================
# County Geometry
#
# County polygons are simplified once for faster Leaflet
# rendering.
# ============================================================

prepare_county_geometry <- function(counties) {
  
  if (!"county_geoid" %in% names(counties)) {
    
    possible_geoid <- c(
      "GEOID",
      "geoid",
      "GEOID20",
      "GEOID10"
    )
    
    available <- possible_geoid[
      possible_geoid %in% names(counties)
    ]
    
    if (length(available) == 0) {
      
      stop(
        paste0(
          "County geometry does not contain `county_geoid` ",
          "or a recognizable GEOID column."
        )
      )
    }
    
    counties$county_geoid <-
      counties[[available[1]]]
  }
  
  
  counties <- counties |>
    mutate(
      county_geoid = stringr::str_pad(
        as.character(county_geoid),
        width = 5,
        side = "left",
        pad = "0"
      )
    )
  
  
  # ----------------------------------------------------------
  # Simplify in a projected CRS
  #
  # This greatly reduces browser polygon size without visibly
  # changing the county map at this zoom level.
  # ----------------------------------------------------------
  
  counties |>
    sf::st_transform(
      3857
    ) |>
    sf::st_simplify(
      dTolerance = 500,
      preserveTopology = TRUE
    ) |>
    sf::st_transform(
      4326
    )
}


# ============================================================
# Build / Load TRUE Bing Tile Geometry
#
# Polygon geometry comes directly from quadkey.
#
# The conversion is cached because the same quadkeys are used
# every time the application starts.
# ============================================================

build_bing_tile_geometry <- function(scatter_df) {
  
  if (!"quadkey" %in% names(scatter_df)) {
    
    stop(
      paste0(
        "`quadkey` is missing from ",
        "outputs/tk_facebook_pop_bing.parquet. ",
        "Rerun the updated cleaning file first."
      )
    )
  }
  
  
  quadkeys <- scatter_df |>
    transmute(
      quadkey = as.character(
        quadkey
      )
    ) |>
    filter(
      !is.na(quadkey),
      nzchar(quadkey)
    ) |>
    distinct(
      quadkey
    ) |>
    arrange(
      quadkey
    )
  
  
  if (nrow(quadkeys) == 0) {
    
    stop(
      "No valid quadkeys were found."
    )
  }
  
  
  cache_file <- file.path(
    CACHE_DIR,
    "bing_tile_geometry.rds"
  )
  
  
  # ----------------------------------------------------------
  # Try cached geometry
  # ----------------------------------------------------------
  
  if (file.exists(cache_file)) {
    
    cached <- tryCatch(
      readRDS(
        cache_file
      ),
      error = function(e) NULL
    )
    
    
    if (
      !is.null(cached) &&
      is.list(cached) &&
      "quadkeys" %in% names(cached) &&
      "geometry" %in% names(cached) &&
      identical(
        cached$quadkeys,
        quadkeys$quadkey
      )
    ) {
      
      message(
        "Loading cached Bing tile geometry..."
      )
      
      return(
        cached$geometry
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # Build true Bing polygons
  # ----------------------------------------------------------
  
  message(
    paste0(
      "Building Bing geometry for ",
      nrow(quadkeys),
      " distinct quadkeys..."
    )
  )
  
  
  geometry <- quadkey_df_to_polygon(
    quadkeys
  )
  
  
  geometry <- sf::st_as_sf(
    geometry
  ) |>
    select(
      quadkey,
      geometry
    ) |>
    sf::st_transform(
      4326
    )
  
  
  # ----------------------------------------------------------
  # Cache geometry
  # ----------------------------------------------------------
  
  saveRDS(
    list(
      quadkeys = quadkeys$quadkey,
      geometry = geometry
    ),
    cache_file
  )
  
  
  geometry
}


# ============================================================
# Load Static Data
# ============================================================

load_static_data <- function() {
  
  # ----------------------------------------------------------
  # County-level data
  # ----------------------------------------------------------
  
  county_path <- file.path(
    OUTPUTS_DIR,
    "tk_facebook_pop_aggregated_bing.parquet"
  )
  
  
  county_df <- arrow::read_parquet(
    county_path
  ) |>
    as.data.frame()
  
  
  # ----------------------------------------------------------
  # Bing tile-level data
  # ----------------------------------------------------------
  
  scatter_path <- file.path(
    OUTPUTS_DIR,
    "tk_facebook_pop_bing.parquet"
  )
  
  
  scatter_df <- arrow::read_parquet(
    scatter_path
  ) |>
    as.data.frame()
  
  
  # ----------------------------------------------------------
  # County geometry
  # ----------------------------------------------------------
  
  counties <- sf::st_read(
    file.path(
      DATA_DIR,
      "counties.geojson"
    ),
    quiet = TRUE
  )
  
  
  # ==========================================================
  # Validate County Data
  # ==========================================================
  
  required_county_columns <- c(
    "county_geoid",
    "county_name_acs",
    "county_state",
    "percent_change",
    "ds",
    "hour",
    "n_crisis",
    "n_baseline"
  )
  
  
  missing_county <- setdiff(
    required_county_columns,
    names(county_df)
  )
  
  
  if (length(missing_county) > 0) {
    
    stop(
      paste(
        "County parquet is missing:",
        paste(
          missing_county,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ==========================================================
  # Validate Bing Data
  # ==========================================================
  
  required_scatter_columns <- c(
    "quadkey",
    "county_geoid",
    "county_name_acs",
    "county_state",
    "percent_change",
    "ds",
    "hour",
    "latitude",
    "longitude",
    "n_crisis",
    "n_baseline"
  )
  
  
  missing_scatter <- setdiff(
    required_scatter_columns,
    names(scatter_df)
  )
  
  
  if (length(missing_scatter) > 0) {
    
    stop(
      paste(
        "Bing parquet is missing:",
        paste(
          missing_scatter,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ==========================================================
  # Datetime
  # ==========================================================
  
  county_df$datetime <- make_datetime(
    county_df$ds,
    county_df$hour
  )
  
  
  scatter_df$datetime <- make_datetime(
    scatter_df$ds,
    scatter_df$hour
  )
  
  
  county_df <- county_df |>
    filter(
      !is.na(datetime)
    )
  
  
  scatter_df <- scatter_df |>
    filter(
      !is.na(datetime)
    )
  
  
  # ==========================================================
  # GEOID
  # ==========================================================
  
  county_df$county_geoid <- stringr::str_pad(
    as.character(
      county_df$county_geoid
    ),
    width = 5,
    side = "left",
    pad = "0"
  )
  
  
  scatter_df$county_geoid <- stringr::str_pad(
    as.character(
      scatter_df$county_geoid
    ),
    width = 5,
    side = "left",
    pad = "0"
  )
  
  
  # ==========================================================
  # Quadkey
  # ==========================================================
  
  scatter_df$quadkey <- as.character(
    scatter_df$quadkey
  )
  
  
  # ==========================================================
  # Characters
  # ==========================================================
  
  county_df$county_name_acs <- as.character(
    county_df$county_name_acs
  )
  
  county_df$county_state <- as.character(
    county_df$county_state
  )
  
  scatter_df$county_name_acs <- as.character(
    scatter_df$county_name_acs
  )
  
  scatter_df$county_state <- as.character(
    scatter_df$county_state
  )
  
  
  # ==========================================================
  # Numerics
  # ==========================================================
  
  numeric_county_columns <- intersect(
    c(
      "percent_change",
      "n_crisis",
      "n_baseline",
      "total_population",
      "median_income",
      "poverty_rate",
      "pct_age_65_plus",
      "pct_no_vehicle"
    ),
    names(county_df)
  )
  
  
  county_df[
    numeric_county_columns
  ] <- lapply(
    county_df[
      numeric_county_columns
    ],
    as.numeric
  )
  
  
  numeric_scatter_columns <- intersect(
    c(
      "percent_change",
      "latitude",
      "longitude",
      "n_crisis",
      "n_baseline",
      "total_population",
      "median_income",
      "poverty_rate",
      "pct_age_65_plus",
      "pct_no_vehicle"
    ),
    names(scatter_df)
  )
  
  
  scatter_df[
    numeric_scatter_columns
  ] <- lapply(
    scatter_df[
      numeric_scatter_columns
    ],
    as.numeric
  )
  
  
  # ==========================================================
  # Geometry
  # ==========================================================
  
  counties <- prepare_county_geometry(
    counties
  )
  
  
  bing_tiles <- build_bing_tile_geometry(
    scatter_df
  )
  
  
  # ==========================================================
  # Sort
  # ==========================================================
  
  county_df <- county_df |>
    arrange(
      datetime,
      county_state,
      county_name_acs
    )
  
  
  scatter_df <- scatter_df |>
    arrange(
      datetime,
      county_state,
      county_name_acs,
      quadkey
    )
  
  
  # ==========================================================
  # Dates / Counties
  # ==========================================================
  
  available_dates <- sort(
    unique(
      as.Date(
        county_df$datetime,
        tz = "America/New_York"
      )
    )
  )
  
  
  scatter_dates <- sort(
    unique(
      as.Date(
        scatter_df$datetime,
        tz = "America/New_York"
      )
    )
  )
  
  
  available_counties <- sort(
    unique(
      na.omit(
        county_df$county_name_acs
      )
    )
  )
  
  
  scatter_counties <- sort(
    unique(
      na.omit(
        scatter_df$county_name_acs
      )
    )
  )
  
  
  # ==========================================================
  # Color Domains
  # ==========================================================
  
  map_domain <- range(
    county_df$percent_change,
    na.rm = TRUE
  )
  
  
  scatter_domain <- range(
    scatter_df$percent_change,
    na.rm = TRUE
  )
  
  
  # ==========================================================
  # County Time-Series Start
  # ==========================================================
  
  time_series_start <- as.POSIXct(
    "2026-02-01 00:00:00",
    tz = "America/New_York"
  )
  
  
  # ==========================================================
  # Return
  # ==========================================================
  
  list(
    
    county_df = county_df,
    
    scatter_df = scatter_df,
    
    counties = counties,
    
    bing_tiles = bing_tiles,
    
    available_dates = available_dates,
    
    scatter_dates = scatter_dates,
    
    available_counties = available_counties,
    
    scatter_counties = scatter_counties,
    
    map_domain = map_domain,
    
    scatter_domain = scatter_domain,
    
    time_series_start = time_series_start
  )
}