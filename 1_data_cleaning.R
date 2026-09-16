library(tidyverse)
library(sf)
library(lubridate)
library(gganimate)
library(ggforce)
library(ggspatial)
library(maptiles)
library(quadkeyr)
library(prettymapr)
library(patchwork)
library(here)
library(rosm)


######------- Data Cleaning Functions --------#######
here::i_am("1_data_cleaning.R")

# Convert possible "\N" values to proper numeric NA values.
# NOTE: for the CSV load path this is now handled at read time via the `na=`
# argument in aggregate_csvs() (see #3); kept here for ad-hoc column cleaning.
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
                           calculate_county_change = FALSE,
                           county_group_cols = c("county_geoid", "county_name", "county_state", "ds")) {
  
  data_dir <- do.call(here::here, as.list(path_parts))
  
  #cat("Reading from:", data_dir, "\n")
  
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
  
  #cat("Found", length(csv_files), "files\n")
  
  # (#3) Treat Meta's "\N" null token (and blanks) as NA at parse time. Otherwise
  # any numeric column containing a "\N" is read as character and silently breaks
  # downstream min()/mean()/median()/fill scales.
  combined_data <- lapply(csv_files, function(f) {
    read_csv(
      f,
      col_types = cols(
        quadkey = col_character(),
        start_quadkey = col_character(),
        end_quadkey = col_character(),
        .default = col_guess()
      ),
      show_col_types = FALSE,
      na = c("", "NA", "\\N")
    ) |>
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


# ============================================================================
# Pipeline steps  —  sourcing this file only DEFINES these; nothing heavy runs
# until you call them. Each step reads its cached .Rds if present, otherwise
# rebuilds from the raw CSVs and saves the cache (self-healing). Each RETURNS its
# object so the caller (a Colab notebook cell, or the report's setup chunk)
# assigns the global the plotting functions expect: fb_data_bing, mp_data_bing,
# moved, tiles_3857, fires. Pass re_run = TRUE to force a rebuild (e.g. after
# adding new CSVs).
# ============================================================================

# Where the .Rds caches live. Default "." = working dir (your current caches).
# To regenerate a fresh set WITHOUT overwriting the current ones, point a step at
# another folder via cache_dir, e.g.:
#   run_dir <- format(Sys.time(), "rds_%Y%m%d_%H%M%S")   # timestamped folder
#   fb_data_bing <- load_population(re_run = TRUE, cache_dir = run_dir)
rds_cache_dir <- "."

# Resolve <dir>/<file>, creating <dir> if needed; returns the full cache path.
cache_path <- function(file, dir = rds_cache_dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, file)
}

load_population <- function(re_run = re_run_cleaning, cache_dir = rds_cache_dir,
                            cache = "fb_data_bing.Rds") {
  path <- cache_path(cache, cache_dir)
  if (re_run || !file.exists(path)) {
    message("Building population data from CSVs -> ", path)
    #Selina: no drop_na(n_difference) — an explicit NA is useful (it means < 11 users).
    fb <- aggregate_csvs(
      path_parts = c("fb_pop_crisis_bing_tiles"),
      lat_col = "latitude",
      lon_col = "longitude"
    )
    saveRDS(fb, path)
    fb
  } else {
    message("Loading cached ", path)
    readRDS(path)
  }
}

load_movement <- function(re_run = re_run_cleaning, cache_dir = rds_cache_dir,
                          cache = "mp_data_bing.Rds") {
  path <- cache_path(cache, cache_dir)
  if (re_run || !file.exists(path)) {
    message("Building movement data from CSVs -> ", path)
    mp <- aggregate_csvs(path_parts = c("fb_move_crisis_bing_tiles"))
    saveRDS(mp, path)
    mp
  } else {
    message("Loading cached ", path)
    readRDS(path)
  }
}

build_moved <- function(mp_data_bing, re_run = re_run_cleaning, cache_dir = rds_cache_dir,
                        cache = "moved.Rds") {
  path <- cache_path(cache, cache_dir)
  if (re_run || !file.exists(path)) {
    message("Building `moved` from movement data -> ", path)
    moved <- mp_data_bing |>
      #Selina: filter is "OR" (not "AND") — keep a move if EITHER lon OR lat changed,
      #so pure East/West or North/South moves are not incorrectly dropped.
      filter(start_longitude != end_longitude | start_latitude != end_latitude) |>
      #Selina: keep explicit NA (it means < 11 users) — no drop_na here.
      rename(`Difference between baseline and crisis` = n_difference) |>
      rename(`# Users During Crisis` = n_crisis)
    saveRDS(moved, path)
    moved
  } else {
    message("Loading cached ", path)
    readRDS(path)
  }
}



###### ------- Facebook Population w/o Aggregation ----#####




# Local cache dir for basemap tiles so repeated renders reuse them instead of
# re-downloading — and so a warmed cache lets the report render offline (#5).
# The report's four maps all share one lon/lat extent, so only the first plot
# hits the network; the rest read this cache. (The old global zoom-6 `osm`
# download was removed — every plot fetches a basemap for its own extent.)
tile_cache_dir <- "maptiles_cache"
if (!dir.exists(tile_cache_dir)) {
  dir.create(tile_cache_dir, recursive = TRUE, showWarnings = FALSE)
}

# ---------------------------------------------------------------------------
# fit_zoom() — cap basemap zoom to a tile budget (OOM guard).
# WHY: a data-driven extent (e.g. movement_plot3(direction="all") framing a
#      long-haul flow) can span the whole region; at a high zoom that is thousands
#      of tiles, which stitches into a huge raster and crashes the runtime. This
#      lowers the zoom until the estimated tile count is within max_tiles.
#' @param lon/@param lat num c(min,max) of the extent (degrees).
#' @param zoom int requested basemap zoom.
#' @param max_tiles int approx tile-count ceiling (default 250).
#' @return int a zoom <= requested; messages if it had to reduce.
# ---------------------------------------------------------------------------
fit_zoom <- function(lon, lat, zoom, max_tiles = 250) {
  dlon <- abs(diff(range(lon))); dlat <- abs(diff(range(lat)))
  z <- zoom
  while (z > 1) {
    deg_per_tile <- 360 / (2^z)
    tx <- max(1, ceiling(dlon / deg_per_tile))
    ty <- max(1, ceiling(dlat / deg_per_tile))   # rough but conservative for a budget
    if (tx * ty <= max_tiles) break
    z <- z - 1
  }
  if (z < zoom)
    message("fit_zoom(): large extent (~", round(dlon, 2), " x ", round(dlat, 2),
            " deg); reduced basemap zoom ", zoom, " -> ", z, " to stay within ~",
            max_tiles, " tiles.")
  z
}
# Tile-polygon step: quadkey squares → EPSG:3857, with the two renames the plots
# use. Rebuild if forced or the cache is missing. If you rebuilt fb_data_bing
# (e.g. new CSVs), pass re_run = TRUE here too so the tiles stay in sync.
build_tiles <- function(fb_data_bing, re_run = re_run_cleaning, cache_dir = rds_cache_dir,
                        cache = "tiles.Rds") {
  path <- cache_path(cache, cache_dir)
  if (re_run || !file.exists(path)) {
    # PERF: a tile's polygon depends ONLY on its quadkey, not the time window, so
    # polygonise the DISTINCT quadkeys once and join back to every row. With ~10
    # windows that is ~10x less geometry work (more if quadkey_df_to_polygon is
    # superlinear) - this is the fix for build_tiles() stalling for tens of minutes
    # on the full multi-window dataset (~200k rows / ~20k distinct quadkeys).
    qk <- dplyr::distinct(fb_data_bing, quadkey)
    qk <- qk[!is.na(qk$quadkey) & nzchar(qk$quadkey), , drop = FALSE]   # guard junk keys
    message("Building tile polygons for ", nrow(qk), " distinct quadkeys ",
            "(", nrow(fb_data_bing), " rows total) -> ", path)
    geom  <- quadkey_df_to_polygon(qk)          # sf: one polygon per quadkey
    geom  <- geom["quadkey"]                     # keep quadkey + geometry only (avoid col clashes)
    tiles <- sf::st_as_sf(dplyr::left_join(geom, fb_data_bing, by = "quadkey"))  # sf, all rows + attributes
    saveRDS(tiles, path)
  } else {
    message("Loading cached ", path)
    tiles <- readRDS(path)
  }
  sf::st_transform(tiles, 3857) |>
    dplyr::rename(`Difference between baseline and crisis` = n_difference) |>
    dplyr::rename(`# Users During Crisis` = n_crisis)
}

format_time <- function(ds, hour, tzone) {
  datetime <- ymd_hm(
    paste(ds, hour),
    tz = "America/Los_Angeles"
  )
  
  datetime_local <- with_tz(
    datetime,
    tzone = tzone
  )
  
  formatted <- format(
    datetime_local,
    "%B %d, %Y %I%p"
  )
  
  
  formatted <- gsub(" 0", " ", formatted)
  formatted <- gsub("AM", "am", formatted)
  formatted <- gsub("PM", "pm", formatted)
  
  formatted
}

# Earliest & latest time windows actually present in the data, plus pretty labels.
# Lives here (not in the .qmd) so the notebook and the report share one source.
compute_windows <- function(fb_data_bing, tzone) {
  aw <- fb_data_bing |>
    dplyr::distinct(ds, hour) |>
    dplyr::arrange(ds, hour)
  stopifnot("No population data loaded." = nrow(aw) > 0)
  fw <- dplyr::slice(aw, 1)
  lw <- dplyr::slice(aw, dplyr::n())
  list(
    first_ds     = as.character(fw$ds), first_hour  = fw$hour,
    latest_ds    = as.character(lw$ds), latest_hour = lw$hour,
    first_label  = format_time(as.character(fw$ds), fw$hour, tzone = tzone),
    latest_label = format_time(as.character(lw$ds), lw$hour, tzone = tzone)
  )
}

####----- LOADING DATA -----

fb_data_bing <- load_population()
tiles_3857   <- build_tiles(fb_data_bing)
w <- compute_windows(fb_data_bing, tzone)
attach(w)
mp_data_bing <- load_movement()
moved <- build_moved(mp_data_bing)
