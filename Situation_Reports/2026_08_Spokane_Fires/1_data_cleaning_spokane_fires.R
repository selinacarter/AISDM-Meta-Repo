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

#setwd("C:/Users/selin/Dropbox/CMU/Meta_AI_good/fire")
re_run_cleaning = TRUE


######------- Data Cleaning Functions --------#######
here::i_am("1_data_cleaning_spokane_fires.R")

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
    read_csv(f, show_col_types = FALSE, na = c("", "NA", "\\N")) |>
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
      path_parts = c("Facebook Population During Crisis - Bing Tiles"),
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
    mp <- aggregate_csvs(path_parts = c("Movement Between Places During Crisis"))
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

format_time <- function(ds, hour){
  df <- data.frame(ds, hour)
  
  df$datetime_cet <- ymd_hm(
    paste(df$ds, df$hour),
    tz = "America/Los_Angeles"
  )
  
  # Format
  df$formatted <- format(
    df$datetime_cet,
    "%B %d, %Y %I%p %Z"
  )
  
  # Remove leading zero from hour and lowercase AM/PM
  df$formatted <- gsub(" 0", " ", df$formatted)
  df$formatted <- gsub("AM", "am", df$formatted)
  df$formatted <- gsub("PM", "pm", df$formatted)
  
  df$formatted
}
# Wildfire-perimeters step. Builds the WFIGS query bbox from a buffer around
# Spokane, downloads the *current* perimeters, and caches them (+ fetch time) to
# fires.Rds for reproducibility. Network failure falls back to the cache, then to
# an empty layer so a plot/report still renders. Pass re_run = TRUE to refresh.
fetch_fires <- function(re_run = re_run_cleaning, cache_dir = rds_cache_dir,
                        cache = "fires.Rds",
                        center_lon = -117.4260, center_lat = 47.6588,
                        buffer_m = 100000) {
  path <- cache_path(cache, cache_dir)
  if (!re_run && file.exists(path)) {
    message("Loading cached ", path)
    return(readRDS(path))
  }

  spokane <- st_as_sf(
    data.frame(lon = center_lon, lat = center_lat),
    coords = c("lon", "lat"), crs = 4326
  )
  # buffer_m-metre buffer around Spokane, back to lon/lat for the query bbox
  extent <- st_buffer(st_transform(spokane, 3857), buffer_m) |> st_transform(4326)
  bbox <- st_bbox(extent)

  url <- paste0(
    "https://services3.arcgis.com/T4QMspbfLg3qTGWY/ArcGIS/rest/services/",
    "WFIGS_Interagency_Perimeters_Current/FeatureServer/0/query?",
    "where=1%3D1",
    "&geometry=",
    bbox["xmin"], ",", bbox["ymin"], ",", bbox["xmax"], ",", bbox["ymax"],
    "&geometryType=esriGeometryEnvelope",
    "&inSR=4326",
    "&spatialRel=esriSpatialRelIntersects",
    "&outFields=*",
    "&returnGeometry=true",
    "&f=geojson"
  )

  message("Downloading wildfire perimeters from WFIGS -> ", path)
  tryCatch(
    {
      f <- st_read(url, quiet = TRUE)
      attr(f, "fetched_at") <- Sys.time()
      saveRDS(f, path)
      f
    },
    error = function(e) {
      if (file.exists(path)) {
        warning("Fire-perimeter download failed (", conditionMessage(e),
                "); using cached ", path, ".")
        readRDS(path)
      } else {
        warning("Fire-perimeter download failed (", conditionMessage(e),
                ") and no cache exists; continuing with no fire perimeters.")
        st_sf(geometry = st_sfc(crs = 4326))
      }
    }
  )
}

# Human-readable timestamp of the perimeter snapshot, for the report to cite.
fires_label <- function(fires) {
  t <- attr(fires, "fetched_at")
  if (inherits(t, "POSIXct") && length(t) == 1 && !is.na(t)) {
    format(t, "%B %d, %Y %I:%M %p %Z")
  } else {
    "a cached snapshot (fetch time unavailable)"
  }
}

# Earliest & latest time windows actually present in the data, plus pretty labels.
# Lives here (not in the .qmd) so the notebook and the report share one source.
compute_windows <- function(fb_data_bing) {
  aw <- fb_data_bing |>
    dplyr::distinct(ds, hour) |>
    dplyr::arrange(ds, hour)
  stopifnot("No population data loaded." = nrow(aw) > 0)
  fw <- dplyr::slice(aw, 1)
  lw <- dplyr::slice(aw, dplyr::n())
  list(
    first_ds     = as.character(fw$ds), first_hour  = fw$hour,
    latest_ds    = as.character(lw$ds), latest_hour = lw$hour,
    first_label  = format_time(as.character(fw$ds), fw$hour),
    latest_label = format_time(as.character(lw$ds), lw$hour)
  )
}

# ============================================================================
# Data sanity check + old-vs-new comparison
# ----------------------------------------------------------------------------
# sanity_check(df)                 -> readable per-dataset health report
# compare_datasets(old, new)       -> rigorous keyed diff of two data frames
# compare_caches(new_dir)          -> run compare_datasets on every .Rds pair
# All are pure (no side effects); each also returns its findings invisibly.
# ============================================================================

# Measure columns we summarise / diff (only those actually present are used).
known_measures <- c(
  "n_baseline", "n_crisis", "n_difference",
  "density_baseline", "density_crisis", "percent_change", "z_score", "length_km",
  "# Users During Crisis", "Difference between baseline and crisis"
)

# Natural key for the Meta datasets: a tile (or O->D tile pair) at a time window.
guess_keys <- function(df) {
  nm <- names(df)
  for (k in list(c("quadkey", "ds", "hour"),
                 c("start_quadkey", "end_quadkey", "ds", "hour"))) {
    if (all(k %in% nm)) return(k)
  }
  character(0)
}

sanity_check <- function(df, name = deparse(substitute(df))) {
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  df <- as.data.frame(df)
  cat("\n==================== SANITY CHECK:", name, "====================\n")
  cat(sprintf("rows: %s   cols: %d\n", format(nrow(df), big.mark = ","), ncol(df)))

  cls <- vapply(df, function(x) class(x)[1], character(1))

  # 1) count columns must be numeric (catches the "\\N" parse bug from #3)
  count_like <- intersect(c("n_baseline", "n_crisis", "n_difference"), names(df))
  bad_class  <- count_like[cls[count_like] %in% c("character", "factor")]
  if (length(bad_class)) {
    cat("  [FAIL] count columns not numeric (\\N parse bug?):",
        paste(bad_class, collapse = ", "), "\n")
  } else if (length(count_like)) {
    cat("  [OK]   count columns are numeric\n")
  }

  # 2) leftover literal "\\N" tokens in any character column
  chr <- names(df)[cls == "character"]
  n_bs <- if (length(chr)) sum(vapply(chr,
            function(c) sum(df[[c]] == "\\N", na.rm = TRUE), numeric(1))) else 0
  cat(sprintf("  [%s] literal \"\\N\" tokens: %d\n", ifelse(n_bs == 0, "OK", "WARN"), n_bs))

  # 3) exact duplicate rows
  ndup <- sum(duplicated(df))
  cat(sprintf("  [%s] duplicate rows: %s\n",
              ifelse(ndup == 0, "OK", "WARN"), format(ndup, big.mark = ",")))

  # 4) time windows present
  if (all(c("ds", "hour") %in% names(df))) {
    win <- df |> dplyr::distinct(ds, hour) |> dplyr::arrange(ds, hour)
    cat("  windows:", paste(sprintf("%s %s", win$ds, win$hour), collapse = " | "), "\n")
  }

  # 5) coordinate ranges (valid lat/lon?)
  for (co in intersect(c("latitude", "longitude", "start_latitude", "start_longitude",
                         "end_latitude", "end_longitude"), names(df))) {
    rng <- range(df[[co]], na.rm = TRUE)
    ok  <- if (grepl("lat", co)) rng[1] >= -90  && rng[2] <= 90
           else                  rng[1] >= -180 && rng[2] <= 180
    cat(sprintf("  [%s] %-16s [% .4f, % .4f]\n", ifelse(ok, "OK", "FAIL"), co, rng[1], rng[2]))
  }

  # 6) per-measure summary + missingness + impossible negatives
  ms <- intersect(known_measures, names(df))
  summ <- NULL
  if (length(ms)) {
    cat("  measures (na% / min / median / mean / max / neg):\n")
    summ <- do.call(rbind, lapply(ms, function(m) {
      x <- suppressWarnings(as.numeric(df[[m]]))
      neg <- if (m %in% c("n_baseline", "n_crisis", "# Users During Crisis"))
               sum(x < 0, na.rm = TRUE) else NA_integer_
      data.frame(col = m, na_pct = round(mean(is.na(x)) * 100, 1),
                 min = round(min(x, na.rm = TRUE), 2), median = round(median(x, na.rm = TRUE), 2),
                 mean = round(mean(x, na.rm = TRUE), 2), max = round(max(x, na.rm = TRUE), 2),
                 neg = neg)
    }))
    print(summ, row.names = FALSE)
    bad_neg <- summ$col[!is.na(summ$neg) & summ$neg > 0]
    if (length(bad_neg))
      cat("  [FAIL] negative counts where impossible:", paste(bad_neg, collapse = ", "), "\n")
  }
  cat("=================================================================\n")
  invisible(list(name = name, nrow = nrow(df), ncol = ncol(df), classes = cls,
                 dup_rows = ndup, backslashN = n_bs, measures = summ))
}

# Rigorous two-version diff. Compares schema, classes, windows, per-measure
# summaries, and (keyed on the natural id) which rows are added/removed/changed.
compare_datasets <- function(old, new, keys = NULL, name = "", tol = 1e-6) {
  if (inherits(old, "sf")) old <- sf::st_drop_geometry(old)
  if (inherits(new, "sf")) new <- sf::st_drop_geometry(new)
  old <- as.data.frame(old); new <- as.data.frame(new)

  cat("\n#################### COMPARE:", name, "####################\n")
  cat(sprintf("rows  old=%s  new=%s  (delta %+d)\n",
              format(nrow(old), big.mark = ","), format(nrow(new), big.mark = ","),
              nrow(new) - nrow(old)))
  cat(sprintf("cols  old=%d  new=%d\n", ncol(old), ncol(new)))

  only_old <- setdiff(names(old), names(new))
  only_new <- setdiff(names(new), names(old))
  shared   <- intersect(names(old), names(new))
  if (length(only_old)) cat("  [WARN] cols only in OLD:", paste(only_old, collapse = ", "), "\n")
  if (length(only_new)) cat("  [WARN] cols only in NEW:", paste(only_new, collapse = ", "), "\n")

  cls_change <- shared[vapply(shared, function(c)
                       class(old[[c]])[1] != class(new[[c]])[1], logical(1))]
  if (length(cls_change)) {
    cat("  [WARN] class changed:\n")
    for (c in cls_change)
      cat(sprintf("     %-32s %s -> %s\n", c, class(old[[c]])[1], class(new[[c]])[1]))
  } else {
    cat("  [OK]   shared columns keep their class\n")
  }

  if (all(c("ds", "hour") %in% shared)) {
    wo <- unique(paste(old$ds, old$hour)); wn <- unique(paste(new$ds, new$hour))
    if (setequal(wo, wn)) {
      cat("  [OK]   same time windows\n")
    } else {
      cat("  [WARN] window set differs — only OLD:", paste(setdiff(wo, wn), collapse = " | "),
          "| only NEW:", paste(setdiff(wn, wo), collapse = " | "), "\n")
    }
  }

  ms <- intersect(known_measures, shared)
  if (length(ms)) {
    cat("  measure deltas (new - old):\n")
    print(do.call(rbind, lapply(ms, function(m) {
      xo <- suppressWarnings(as.numeric(old[[m]])); xn <- suppressWarnings(as.numeric(new[[m]]))
      data.frame(col = m, na_old = sum(is.na(xo)), na_new = sum(is.na(xn)),
                 d_mean = round(mean(xn, na.rm = TRUE) - mean(xo, na.rm = TRUE), 4),
                 d_median = round(median(xn, na.rm = TRUE) - median(xo, na.rm = TRUE), 4),
                 d_min = round(min(xn, na.rm = TRUE) - min(xo, na.rm = TRUE), 4),
                 d_max = round(max(xn, na.rm = TRUE) - max(xo, na.rm = TRUE), 4))
    })), row.names = FALSE)
  }

  # keyed value diff
  if (is.null(keys)) keys <- guess_keys(old)
  keys <- intersect(keys, shared)
  verdict <- "IDENTICAL (within tol)"
  n_only_old <- n_only_new <- n_rows_changed <- 0L
  if (length(keys) == 0) {
    cat("  [WARN] no key columns resolved - skipping keyed value diff\n")
    verdict <- "UNKEYED (see summaries above)"
  } else if (anyDuplicated(old[keys]) || anyDuplicated(new[keys])) {
    cat("  [WARN] keys not unique (", paste(keys, collapse = "+"),
        ") - skipping keyed value diff\n")
    verdict <- "UNKEYED (duplicate keys)"
  } else {
    cat("  keyed diff on:", paste(keys, collapse = " + "), "\n")
    o <- old |> dplyr::mutate(dplyr::across(dplyr::all_of(keys), as.character))
    n <- new |> dplyr::mutate(dplyr::across(dplyr::all_of(keys), as.character))
    n_only_old <- nrow(dplyr::anti_join(o, n, by = keys))
    n_only_new <- nrow(dplyr::anti_join(n, o, by = keys))
    both <- dplyr::inner_join(
      dplyr::select(o, dplyr::all_of(keys), dplyr::any_of(ms)),
      dplyr::select(n, dplyr::all_of(keys), dplyr::any_of(ms)),
      by = keys, suffix = c(".old", ".new")
    )
    cat(sprintf("     rows only in OLD: %s\n", format(n_only_old, big.mark = ",")))
    cat(sprintf("     rows only in NEW: %s\n", format(n_only_new, big.mark = ",")))
    cat(sprintf("     rows in BOTH:     %s\n", format(nrow(both), big.mark = ",")))
    changed_any <- rep(FALSE, nrow(both))
    for (m in ms) {
      a <- suppressWarnings(as.numeric(both[[paste0(m, ".old")]]))
      b <- suppressWarnings(as.numeric(both[[paste0(m, ".new")]]))
      # equal if both NA, or both present and within a relative tolerance
      d <- !((is.na(a) & is.na(b)) |
             (!is.na(a) & !is.na(b) & abs(a - b) <= tol * pmax(1, abs(a), abs(b))))
      cat(sprintf("     %-40s changed: %s\n", m, format(sum(d), big.mark = ",")))
      changed_any <- changed_any | d
    }
    n_rows_changed <- sum(changed_any)
    cat(sprintf("     rows with ANY changed measure: %s\n", format(n_rows_changed, big.mark = ",")))
    if (n_only_old > 0 || n_only_new > 0) verdict <- "ROWS DIFFER"
    else if (n_rows_changed > 0)          verdict <- "VALUES DIFFER"
  }
  if (length(cls_change)) verdict <- paste(verdict, "+ CLASS CHANGE")
  cat("  VERDICT:", verdict, "\n")
  cat("############################################################\n")
  invisible(list(name = name, only_old = only_old, only_new = only_new,
                 class_changes = cls_change, keys = keys,
                 only_in_old = n_only_old, only_in_new = n_only_new,
                 rows_changed = n_rows_changed, verdict = verdict))
}

# Convenience: compare every regenerated .Rds in new_dir against old_dir (default
# the originals in the working dir). Geometry of the sf caches isn't compared
# (it's deterministic from quadkey) — only their attribute tables.
compare_caches <- function(new_dir, old_dir = ".", tol = 1e-6,
                           files = c("fb_data_bing.Rds", "mp_data_bing.Rds",
                                     "moved.Rds", "tiles.Rds")) {
  out <- list()
  for (f in files) {
    op <- file.path(old_dir, f); np <- file.path(new_dir, f)
    if (!file.exists(op) || !file.exists(np)) {
      cat("\n[skip]", f, "- not present in both", old_dir, "and", new_dir, "\n"); next
    }
    out[[f]] <- compare_datasets(readRDS(op), readRDS(np), name = f, tol = tol)
  }
  cat("\n===== SUMMARY =====\n")
  for (f in names(out)) cat(sprintf("  %-20s %s\n", f, out[[f]]$verdict))
  invisible(out)
}

# -----------------------------------------------------------------------------
# Population map (merged) — one implementation for BOTH the crisis-vs-baseline
# difference map and the raw crisis-count map (#6). `metric` selects the coloured
# column and the colour scale; everything else (window filter, empty-window guard,
# per-extent basemap, fire perimeters, theme) is shared.
# -----------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# population_plot() — one population tile map for three metrics (see report §1/§1a).
# WHY: EMs need to see where population is anomalous. "difference"/"crisis" are the
#      original views (unchanged); "zscore" (Task 1) colours by Meta's standardized
#      change, pre-clipped to +/-4, so a single +1,186 outlier can't wash out the
#      +/-50 signal the difference map suffers from.
#' @param plot_ds    chr  window date "YYYY-MM-DD" (from tiles_3857$ds).
#' @param plot_hour  chr  window "0000"/"0800"/"1600" (population has 3/day).
#' @param metric     chr  "difference" (crisis-baseline, diverging), "crisis" (raw, log),
#'                        or "zscore" (standardized change, RdBu, fixed +/-4). Default "difference".
#' @param title      lgl  add a title (default TRUE); plot_title overrides the auto time label.
#' @param lon_limits num  c(min,max) longitude to zoom/clip (NULL = whole-data extent).
#' @param lat_limits num  c(min,max) latitude  to zoom/clip.
#' @param zoom       int  basemap tile zoom (higher = sharper; default 9).
#' @param show_evac  lgl  (Task 4) overlay evacuation-zone outlines if TRUE and a layer exists.
#' @param evac       sf   optional evac zones; NULL -> use global `evac_zones` if present.
#' @return ggplot object (previewable in a Colab cell; embeddable in the .qmd).
#' @sideeffects reads globals `tiles_3857`, `fires` (and `evac_zones` if show_evac);
#'              may fetch/cache a basemap to maptiles_cache/.
#' @examples population_plot(first_ds, first_hour, metric = "zscore",
#'                           lon_limits = c(-118.5,-115.5), lat_limits = c(47,49))
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# clip_sf_to_box() — crop an sf overlay (fire perimeters, evac zones) to the map's
# display box so it CANNOT expand the coord_sf frame past xlim/ylim. This is the fix
# for the tiles being squeezed into a corner: a fire layer drawn after coord_sf pulled
# the panel out to the fires' own extent. Clipping overlays to the map extent is
# standard cartographic practice and makes the coord limits authoritative.
#' @param layer sf overlay to clip (returned unchanged if NULL/empty or no box given).
#' @param xlim/@param ylim num c(min,max) in EPSG:3857 (the coord box); NULL -> no clip.
#' @return sf in EPSG:3857 clipped to the box (or the input unchanged if skipped/failed).
# ---------------------------------------------------------------------------
clip_sf_to_box <- function(layer, xlim, ylim) {
  if (is.null(layer) || !inherits(layer, "sf") || nrow(layer) == 0) return(layer)
  if (is.null(xlim) || is.null(ylim)) return(layer)
  bb <- sf::st_bbox(c(xmin = min(xlim), xmax = max(xlim),
                      ymin = min(ylim), ymax = max(ylim)), crs = sf::st_crs(3857))
  tryCatch(suppressWarnings(sf::st_crop(sf::st_transform(layer, 3857), bb)),
           error = function(e) layer)
}

# ---------------------------------------------------------------------------
# spokane_waypoints() — a handful of town anchors for the orientation overlay.
# WHY: viewers place the tiles faster with 2-4 familiar labels than with a graticule.
#      Kept short on purpose (busy labels fight the tiles); points outside a given
#      frame are clipped away by population_plot(), so a tight focus stays clean.
#' @return tibble(name, lon, lat) in WGS84. Override by passing your own to
#'         population_plot(waypoints = ...) / render_series(waypoints = ...).
# ---------------------------------------------------------------------------
spokane_waypoints <- function() {
  tibble::tibble(
    name = c("Spokane", "Spokane Valley", "Cheney", "Airway Heights", "Liberty Lake", "Deer Park"),
    lon  = c(-117.4260,      -117.2394,   -117.5758,      -117.5933,      -117.0894,    -117.4769),
    lat  = c(  47.6588,        47.6733,     47.4874,         47.6449,        47.6746,      47.9546)
  )
}

# ---------------------------------------------------------------------------
# get_wa_counties() — cached county polygons (tigris) for the county-line overlay.
# WHY: population_plot(show_counties=TRUE) reads a global `counties`; this builds it
#      once and caches to rds so Colab doesn't re-download. Degrades to NULL (no lines)
#      if tigris/network is unavailable, so a map still renders.
#' @return sf of counties (cb generalized) or NULL. Assign before plotting:
#'         counties <- get_wa_counties()
# ---------------------------------------------------------------------------
get_wa_counties <- function(state = "WA", year = 2022,
                            cache = "counties_wa.Rds", cache_dir = rds_cache_dir) {
  p <- cache_path(cache, cache_dir)
  if (file.exists(p)) return(readRDS(p))
  co <- tryCatch({
    if (!requireNamespace("tigris", quietly = TRUE))
      stop("tigris not installed - run install.packages('tigris')")
    options(tigris_use_cache = TRUE)
    tigris::counties(state = state, cb = TRUE, year = year)
  }, error = function(e) { warning("get_wa_counties(): ", conditionMessage(e)); NULL })
  if (!is.null(co)) saveRDS(co, p)
  co
}

# ---------------------------------------------------------------------------
# study_county_lines() — INTERIOR county division(s) among the study counties.
# WHY: drawing full county polygons paints the group's OUTER envelope too, and that
#      outer edge slices through the ~8 km buffer of edge tiles (2_geofilter keeps a
#      buffer past the county line) — the "line cutting through tiles" artifact. The
#      shared border BETWEEN the study counties is the informative, non-busy line, so
#      we keep only that.
#' @param co     sf county polygons (needs a NAME column; e.g. get_wa_counties()).
#' @param names  chr study-county names to divide (default Spokane + Stevens).
#' @return sf of LINESTRINGs (the shared border[s]) or NULL if it can't be built.
# ---------------------------------------------------------------------------
study_county_lines <- function(co, names = c("Spokane", "Stevens")) {
  if (is.null(co) || !inherits(co, "sf") || !("NAME" %in% base::names(co))) return(NULL)
  two <- co[co$NAME %in% names, , drop = FALSE]
  if (nrow(two) < 2) return(NULL)
  # Work in EPSG:3857 (planar GEOS, tolerant) and derive INNER lines as: every county edge
  # MINUS the group's outer envelope. Robust to generalized cb=TRUE polygons whose shared
  # edges don't perfectly coincide — the small buffer bridges those slivers. (st_intersection
  # of two generalized polygons often returns empty, which is why the pairwise approach failed.)
  two   <- suppressWarnings(sf::st_make_valid(sf::st_transform(two, 3857)))
  inner <- tryCatch({
    bnd   <- suppressWarnings(sf::st_union(sf::st_boundary(two)))   # all county edges
    outer <- suppressWarnings(sf::st_boundary(sf::st_union(two)))   # the group's outer ring
    suppressWarnings(sf::st_difference(bnd, sf::st_buffer(outer, 60)))
  }, error = function(e) NULL)
  if (is.null(inner)) return(NULL)
  g <- tryCatch(suppressWarnings(sf::st_collection_extract(sf::st_geometry(inner), "LINESTRING")),
                error = function(e) sf::st_geometry(inner))
  if (length(g) == 0) return(NULL)
  sf::st_sf(geometry = g)
}

population_plot <- function(
    plot_ds,
    plot_hour,
    metric = c("difference", "crisis", "zscore"),
    title = TRUE,
    plot_title = NULL,
    lon_limits = NULL,   # c(min_lon, max_lon)
    lat_limits = NULL,   # c(min_lat, max_lat)
    zoom = 9,            # (#7) basemap detail; higher = sharper (~1 tile at 6, ~25 at 9)
    show_evac = FALSE,   # (Task 4) draw evacuation-zone outlines if TRUE and a layer exists
    evac = NULL,         # optional sf of evac zones; NULL -> use global `evac_zones` if present
    show_aoi = TRUE,     # draw the Spokane+Stevens crop-boundary neatline (global `aoi` from 2_geofilter_spokane.R)
    aoi = NULL,          # optional sf AOI outline; NULL -> use global `aoi` if present
    fill_limits = NULL,  # force the fill scale limits (difference/crisis) so two panels can SHARE
                         # one scale + one combined legend; NULL = per-window data-driven (zscore fixed ±4)
    # (Orientation overlays) opt-in, subtle, and kept OUT of the colour legend so the
    # map doesn't get busy. Counties read the global `counties`; waypoints default to the
    # Spokane metro anchors (spokane_waypoints()). Both are clipped to the display box.
    show_counties  = FALSE,
    counties       = NULL,
    study_counties = c("Spokane", "Stevens"),  # interior division drawn for show_counties
    county_lines   = c("interior", "full"),    # "interior": just the shared border (no tile-slicing
                                               # outer edge); "full": the whole study-county outline
    show_waypoints = FALSE,
    waypoints      = NULL,
    show_scalebar  = FALSE                      # Mercator-corrected bar (needs lon/lat limits)
) {
  metric <- match.arg(metric)
  # (Task 1) metric selects the coloured column: difference & crisis unchanged;
  # zscore uses Meta's clipped standardized change (pre-winsorized to +/-4).
  fill_col <- switch(metric,
    difference = "Difference between baseline and crisis",
    crisis     = "# Users During Crisis",
    zscore     = "z_score")

  # Defensive: z_score must have survived quadkey_df_to_polygon into tiles_3857.
  if (metric == "zscore" && !("z_score" %in% names(tiles_3857))) {
    stop("population_plot(metric='zscore'): column `z_score` not in tiles_3857. ",
         "Rebuild with build_tiles(fb_data_bing, re_run = TRUE); if still missing, ",
         "quadkey_df_to_polygon dropped it - join z_score back from fb_data_bing by quadkey.")
  }

  lims <- range(tiles_3857[[fill_col]], na.rm = TRUE)

  first_time_pop <- tiles_3857 |>
    filter(ds == plot_ds, hour == plot_hour)

  # (#2) Fail loudly if the requested window doesn't exist, instead of drawing
  # an empty map or taking min()/max() of nothing.
  if (nrow(first_time_pop) == 0) {
    stop("population_plot(metric='", metric, "'): no data for ds=", plot_ds,
         ", hour=", plot_hour, ". Available windows: ",
         paste(sort(unique(paste(tiles_3857$ds, tiles_3857$hour))), collapse = ", "))
  }

  # Default: whole-data extent, no coord zoom
  xlim <- NULL
  ylim <- NULL
  osm_source <- first_time_pop   # basemap footprint

  if (!is.null(lon_limits) && !is.null(lat_limits)) {
    first_time_pop <- first_time_pop |>
      filter(lon_limits[1] <= longitude, longitude <= lon_limits[2]) |>
      filter(lat_limits[1] <= latitude,  latitude <= lat_limits[2])

    if (nrow(first_time_pop) == 0) {
      stop("population_plot(metric='", metric, "'): no tiles within the requested ",
           "lon/lat limits for ds=", plot_ds, ", hour=", plot_hour, ".")
    }

    lims <- range(first_time_pop[[fill_col]], na.rm = TRUE)

    # basemap footprint from the requested window
    osm_source <- st_as_sf(
      data.frame(lon = lon_limits, lat = lat_limits),
      coords = c("lon", "lat"),
      crs = 4326
    )

    # Convert lon/lat limits to EPSG:3857 for coord_sf()
    bbox_3857 <- st_bbox(
      c(xmin = lon_limits[1], xmax = lon_limits[2],
        ymin = lat_limits[1], ymax = lat_limits[2]),
      crs = st_crs(4326)
    ) |>
      st_as_sfc() |>
      st_transform(3857) |>
      st_bbox()

    # unname() so coord_sf gets plain numerics (named limit vectors can be mishandled).
    xlim <- unname(c(bbox_3857["xmin"], bbox_3857["xmax"]))
    ylim <- unname(c(bbox_3857["ymin"], bbox_3857["ymax"]))
  }

  # (#5/#7) Basemap for this extent, cached on disk and tolerant of network
  # failure — a failed download degrades to "no basemap" instead of erroring.
  osm <- tryCatch(
    get_tiles(osm_source, provider = "CartoDB.Voyager", crop = TRUE,
              zoom = zoom, cachedir = tile_cache_dir),
    error = function(e) {
      warning("Basemap download failed (", conditionMessage(e),
              "); drawing without a basemap.")
      NULL
    }
  )

  # Shared-scale override: when fill_limits is supplied (difference/crisis), use it so
  # a first/latest pair share ONE scale and ONE combined legend. zscore
  # is already fixed at ±4. NB: values outside the shared limits are dropped by ggplot
  # unless the scale squishes them — the difference/crisis scales below clamp via limits.
  if (!is.null(fill_limits)) lims <- fill_limits

  # Short legend titles: the long defaults ("Difference between baseline and
  # crisis", "# Users During Crisis") were clipped at the panel edge in the 2-up
  # layout. Paired with title.position="top" below, these now sit above the bar.
  fill_scale <- if (metric == "difference") {
    scale_fill_gradient2(low = "blue", mid = "grey", high = "red",
                         limits = lims, midpoint = 0,
                         name = "Users (crisis − baseline)")
  } else if (metric == "crisis") {
    scale_fill_gradient(trans = "log10", low = "blue", high = "red",
                        limits = lims, labels = scales::label_comma(),
                        name = "Users")
  } else {  # zscore: diverging RdBu reversed, fixed +/-4, squish out-of-bounds (Task 1, §3)
    scale_fill_distiller(palette = "RdBu", direction = -1,
                         limits = c(-4, 4), oob = scales::squish,
                         na.value = "grey60",
                         name = "z-score (crisis vs. baseline)")
  }

  p1 <- ggplot()
  if (!is.null(osm)) p1 <- p1 + layer_spatial(osm)
  p1 <- p1 +
    geom_sf(
      data = first_time_pop,
      aes(fill = .data[[fill_col]]),
      color = "white",
      linewidth = 0.1,
      alpha = 1
    ) +
    fill_scale +
    coord_sf(
      crs = st_crs(3857),
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    # Fire perimeters as a mapped colour so they get their own legend entry.
    # clip_sf_to_box() crops them to the display box so they can't expand the frame.
    geom_sf(
      data = clip_sf_to_box(fires, xlim, ylim),
      aes(color = "Fire Perimeter"),
      fill = NA,
      linewidth = 0.5
    )

  # (Task 4) Optional evacuation-zone outlines. They SHARE the one outline colour
  # legend with the fire perimeter (no extra scale / dependency). Only drawn when a
  # non-empty evac layer with a `level` field is available; default show_evac = FALSE
  # leaves Figs 2-5 byte-identical.
  outline_values <- c("Fire Perimeter" = "darkred")
  ez <- if (isTRUE(show_evac)) { if (!is.null(evac)) evac else get0("evac_zones") } else NULL
  if (!is.null(ez) && inherits(ez, "sf") && nrow(ez) > 0 && "level" %in% names(ez)) {
    ez <- dplyr::mutate(ez, evac_label = paste("Evac Level", level))
    p1 <- p1 + geom_sf(data = clip_sf_to_box(ez, xlim, ylim),
                       aes(color = evac_label), fill = NA, linewidth = 0.7)
    outline_values <- c(outline_values,
                        "Evac Level 1" = "gold", "Evac Level 2" = "orange", "Evac Level 3" = "red")
  }

  # (AOI) Spokane+Stevens crop-boundary neatline so every population map shows the
  # extent the data were clipped to. Literal colour (not mapped) => stays OUT of the
  # fire/evac colour legend. Reads global `aoi` unless one is passed; clipped to the
  # display box so it never expands the frame.
  ao <- if (isTRUE(show_aoi)) { if (!is.null(aoi)) aoi else get0("aoi") } else NULL
  if (!is.null(ao) && inherits(ao, "sf") && nrow(ao) > 0) {
    p1 <- p1 + geom_sf(data = clip_sf_to_box(ao, xlim, ylim), fill = NA,
                       color = "grey20", linetype = "dashed", linewidth = 0.4)
  }

  # (Orientation) Thin county boundaries so a viewer can place the tiles at a glance.
  # Literal colour => stays out of the fire/evac legend; clipped to the box. NOTE: the
  # `counties` PARAMETER shadows the global of the same name, so a bare get0("counties")
  # returns this function's NULL arg — read the GLOBAL env explicitly instead.
  co <- if (isTRUE(show_counties)) {
    if (!is.null(counties)) counties else get0("counties", envir = globalenv(), inherits = TRUE)
  } else NULL
  if (!is.null(co) && inherits(co, "sf") && nrow(co) > 0) {
    county_lines <- match.arg(county_lines)
    # "interior": the shared border BETWEEN the study counties only (no outer envelope, so
    # nothing slices the buffered edge tiles). "full": the whole study-county outline. Both
    # derive from the STUDY-county subset — NEVER the raw statewide layer, which would blow out
    # the frame. Crop LINES (st_boundary), not polygons: cropping a polygon adds frame-box edges
    # (a stray rectangle); cropping lines just trims the real borders at the frame.
    study_outline <- function() {
      sub <- if ("NAME" %in% names(co)) co[co$NAME %in% study_counties, , drop = FALSE] else co
      clip_sf_to_box(suppressWarnings(sf::st_boundary(sub)), xlim, ylim)
    }
    cl <- if (county_lines == "full") study_outline() else {
      inner <- tryCatch(clip_sf_to_box(study_county_lines(co, study_counties), xlim, ylim),
                        error = function(e) NULL)
      if (is.null(inner)) study_outline() else inner   # only fall back if it can't be built
    }
    if (!is.null(cl) && inherits(cl, "sf") && nrow(cl) > 0)
      p1 <- p1 + geom_sf(data = cl, fill = NA, color = "grey15", linewidth = 0.5)
  }

  # (Orientation) A few labelled town waypoints for a mental anchor. Drawn as sf
  # (geom_sf/geom_sf_text) so coord_sf transforms them exactly like the fire/AOI overlays
  # — a plain geom_point with numeric coords fights coord_sf's CRS and wrecks the frame.
  # Out-of-frame anchors are dropped up front (in lon/lat) so a tight focus stays clean.
  # Literal colours => the markers/labels never enter the fire/evac legend.
  wp <- if (isTRUE(show_waypoints)) { if (!is.null(waypoints)) waypoints else spokane_waypoints() } else NULL
  if (!is.null(wp) && (inherits(wp, "sf") || is.data.frame(wp)) && nrow(wp) > 0) {
    if (!inherits(wp, "sf") && !is.null(lon_limits) && !is.null(lat_limits)) {
      wp <- wp[wp$lon >= min(lon_limits) & wp$lon <= max(lon_limits) &
               wp$lat >= min(lat_limits) & wp$lat <= max(lat_limits), , drop = FALSE]
    }
    if (nrow(wp) > 0) {
      wp_sf <- if (inherits(wp, "sf")) wp else sf::st_as_sf(wp, coords = c("lon", "lat"), crs = 4326)
      wp_sf <- clip_sf_to_box(wp_sf, xlim, ylim)   # belt-and-suspenders; also lands it in EPSG:3857
      if (!is.null(wp_sf) && nrow(wp_sf) > 0) {
        p1 <- p1 +
          geom_sf(data = wp_sf, inherit.aes = FALSE, shape = 21, fill = "grey15",
                  color = "white", size = 1.6, stroke = 0.3) +
          geom_sf_text(data = wp_sf, aes(label = name), inherit.aes = FALSE,
                       size = 2.6, fontface = "bold", color = "grey10", vjust = -0.9)
      }
    }
  }

  # (Orientation) Mercator-corrected distance scale bar, built as sf so coord_sf places it
  # exactly like the waypoints (the annotate()-based helper mis-placed under coord_sf here).
  # Web-Mercator inflates map units by sec(latitude), so the bar's map length is
  # km_ground * 1000 / cos(lat) to keep the label truthful (~48% correction at Spokane).
  if (isTRUE(show_scalebar) && !is.null(xlim) && !is.null(ylim)) {
    .sx <- diff(range(xlim)); .sy <- diff(range(ylim))
    .x0 <- min(xlim) + 0.05 * .sx
    .y0 <- min(ylim) + 0.06 * .sy
    .lat0   <- (2 * atan(exp(.y0 / 6378137)) - pi / 2) * 180 / pi   # inverse Mercator of y0
    .cosphi <- cos(.lat0 * pi / 180)
    .nice_m <- c(1, 2, 5, 10, 20, 25, 50, 100) * 1000
    .km     <- .nice_m[which.min(abs(.nice_m - 0.25 * .sx * .cosphi))] / 1000
    .barmap <- .km * 1000 / .cosphi
    .bar <- sf::st_sf(geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(.x0, .y0, .x0 + .barmap, .y0), ncol = 2, byrow = TRUE)),
      crs = 3857))
    .lab <- sf::st_sf(name = paste0(.km, " km"),
      geometry = sf::st_sfc(sf::st_point(c(.x0 + .barmap / 2, .y0)), crs = 3857))
    p1 <- p1 +
      geom_sf(data = .bar, inherit.aes = FALSE, color = "grey15", linewidth = 1) +
      geom_sf_text(data = .lab, aes(label = name), inherit.aes = FALSE,
                   color = "grey15", size = 3, vjust = -0.6)
  }

  p1 <- p1 +
    scale_color_manual(values = outline_values, name = NULL, drop = TRUE) +
    guides(
      # title.position = "top" centers the title ABOVE the bar so it no longer
      # clips at the left edge of a narrow (2-up) panel.
      fill = guide_colorbar(direction = "horizontal", order = 1, title.position = "top"),
      color = guide_legend(direction = "horizontal", order = 2)
    )

  if (title) {
    if (is.null(plot_title)) {
      p1 + ggtitle(format_time(plot_ds, plot_hour))
    } else {
      p1 + ggtitle(plot_title)
    }
  } else {
    p1
  }
}

# Thin wrappers preserve the original call sites / API (#6). The ~250 lines of
# duplicated body are now the single implementation above.
population_plot_n_difference <- function(plot_ds, plot_hour, ...) {
  population_plot(plot_ds, plot_hour, metric = "difference", ...) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",         # stack the fill colorbar and the outline legend
      legend.direction = "horizontal", # each legend laid out horizontally
      legend.key.width = unit(1.4, "cm")
    )
}

population_plot_n_crisis <- function(plot_ds, plot_hour, ...) {
  population_plot(plot_ds, plot_hour, metric = "crisis", ...) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.position = "right",
      legend.box = "vertical",
      legend.key.width = unit(2.5, "cm")
    ) +
    guides(
      fill = guide_legend(direction = "vertical"),
      color = guide_legend(direction = "vertical")
    )
}

# population_plot_z_score() — wrapper for the standardized-change map (Task 1).
# WHY: shorthand so the report/notebook can call the z-score view like the others.
#' @inheritParams population_plot
#' @return ggplot object.
#' @examples population_plot_z_score(latest_ds, latest_hour, lon_limits=c(-118.5,-115.5), lat_limits=c(47,49))
population_plot_z_score <- function(plot_ds, plot_hour, ...) {
  population_plot(plot_ds, plot_hour, metric = "zscore", ...)
}

movement_plot <- function(
    plot_ds,
    plot_hour,
    plot_title,
    lon_limits = NULL,   # c(min_lon, max_lon)
    lat_limits = NULL,   # c(min_lat, max_lat)
    n_limit = NULL,
    zoom = 9             # (#7) basemap detail for this plot's extent
) {
  # (#4) Both limits are required — they are fed straight into between(), which
  # errors on NULL. Guard with a clear message instead of a cryptic crash.
  if (is.null(lon_limits) || is.null(lat_limits)) {
    stop("movement_plot() requires both lon_limits and lat_limits, e.g. ",
         "lon_limits = c(-118.5, -115.5), lat_limits = c(47, 49).")
  }

  time_moved <- moved |>
    filter(
      ds == plot_ds,
      hour == plot_hour
    ) |>
    filter(
      (between(start_latitude, lat_limits[1], lat_limits[2]) & between(start_longitude, lon_limits[1], lon_limits[2])) |
        (between(end_latitude, lat_limits[1], lat_limits[2]) & between(end_longitude, lon_limits[1], lon_limits[2]))
    )

  # (#2) Fail loudly rather than taking min()/max() of an empty set.
  if (nrow(time_moved) == 0) {
    stop("movement_plot(): no movement for ds=", plot_ds, ", hour=", plot_hour,
         " within the requested limits. Available windows: ",
         paste(sort(unique(paste(moved$ds, moved$hour))), collapse = ", "))
  }

  # plot limits
  plot_lat <- c(
    min(c(time_moved$start_latitude, time_moved$end_latitude)) - 0.5,
    max(c(time_moved$start_latitude, time_moved$end_latitude)) + 0.5
  )
  plot_long <- c(
    min(c(time_moved$start_longitude, time_moved$end_longitude)) - 0.5,
    max(c(time_moved$start_longitude, time_moved$end_longitude)) + 0.5
  )

  # (#5/#7) Basemap for THIS plot's extent, cached on disk and tolerant of
  # network failure (a failed download degrades to "no basemap").
  osm <- tryCatch(
    get_tiles(
      st_as_sf(
        data.frame(
          lon = c(plot_long[1], plot_long[2]),
          lat = c(plot_lat[1], plot_lat[2])
        ),
        coords = c("lon", "lat"),
        crs = 4326
      ),
      provider = "CartoDB.Voyager",
      crop = TRUE,
      zoom = zoom,
      cachedir = tile_cache_dir
    ),
    error = function(e) {
      warning("Basemap download failed (", conditionMessage(e),
              "); drawing without a basemap.")
      NULL
    }
  )

  p <- ggplot()
  if (!is.null(osm)) p <- p + layer_spatial(osm)
  p +
    geom_link(
      data = time_moved,
      aes(
        x = start_longitude,
        y = start_latitude,
        xend = end_longitude,
        yend = end_latitude,
        color = `# Users During Crisis`
      ),
      alpha = 0.5,
      arrow = arrow(length = unit(0.03, "npc")),
      linewidth = 1
    ) +
    # (#4) Arrows use the `color` aesthetic, so this must be a COLOUR scale —
    # scale_fill_gradient() silently did nothing here.
    scale_color_gradient(
      trans = "log10",
      low = "blue",
      high = "red",
      labels = scales::label_comma()
    ) +
    coord_sf(
      xlim = plot_long,
      ylim = plot_lat,
      expand = FALSE
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    ggtitle(plot_title)
}

# ---------------------------------------------------------------------------
# population_timeseries() — evacuation curve: users in fire-adjacent tiles across
# every 8-hour window (see report §3).
# WHY: two map snapshots can't show whether people actually left when the Level-3
#      order dropped; one line of fire-adjacent population over time makes the
#      evacuation (or its absence) legible, using only data already loaded.
#' @param fb_data_bing df  population tiles (needs latitude, longitude, ds, hour, n_crisis, n_baseline).
#' @param fires        sf  wildfire perimeters (fetch_fires()); may be empty.
#' @param buffer_km    num tiles within this buffer of the fire count as "adjacent" (default 15).
#' @param mode         chr one of: "split" (RECOMMENDED for egress) draws gross influx (tiles above
#'                        baseline) and gross exodus (tiles below baseline) as separate bands plus the
#'                        net line, so an exodus stays visible even when the net is positive; "anomaly"
#'                        draws only the net (crisis - baseline) line; "levels" draws the raw crisis vs
#'                        baseline sawtooth (reference). All modes are complete-case (see @return).
#' @param buffer_crs   int projected CRS for the buffer so buffer_km is TRUE ground distance
#'                        (default 32611 = UTM 11N, Spokane). Buffering in EPSG:3857 overstates
#'                        distance by ~1/cos(lat) (~1.48x at 47.6N): a "15 km" 3857 buffer is only
#'                        ~10 km on the ground. Using UTM makes "within N km" honest.
#' @return ggplot. NOTE: the per-tile change is n_crisis - n_baseline computed per row, which is NA
#'         whenever EITHER side was privacy-suppressed (<10) — so every window sums the SAME
#'         complete-case tiles and the earlier na.rm asymmetry (one side counted, the other dropped)
#'         is gone. Use a small buffer (~3-5 km) to isolate the fire edge from the metro.
#' @sideeffects none (pure).
#' @examples population_timeseries(fb_data_bing, fires, buffer_km = 5, mode = "split")
# ---------------------------------------------------------------------------
population_timeseries <- function(fb_data_bing, fires, buffer_km = 15,
                                  mode = c("split", "anomaly", "levels"), buffer_crs = 32611) {
  mode <- match.arg(mode)
  stopifnot(all(c("latitude","longitude","ds","hour","n_crisis","n_baseline") %in% names(fb_data_bing)))

  # tile centroids projected to an equidistant CRS (UTM 11N by default) so the
  # buffer test uses TRUE ground metres, not web-Mercator metres (see @param buffer_crs).
  pts <- sf::st_transform(
    sf::st_as_sf(fb_data_bing, coords = c("longitude","latitude"), crs = 4326, remove = FALSE),
    buffer_crs)

  adj <- fb_data_bing[FALSE, ]                     # empty, correct schema
  if (!is.null(fires) && inherits(fires, "sf") && nrow(fires) > 0) {
    buf <- sf::st_union(sf::st_buffer(sf::st_transform(fires, buffer_crs), buffer_km * 1000))
    adj <- fb_data_bing[lengths(sf::st_intersects(pts, buf)) > 0, ]
  }
  # Fallback so the curve is never empty: expanded fire bbox, else all tiles.
  if (nrow(adj) == 0) {
    warning("population_timeseries(): no tiles intersect the fire buffer; using an expanded bbox.")
    if (!is.null(fires) && inherits(fires, "sf") && nrow(fires) > 0) {
      bb  <- sf::st_bbox(sf::st_transform(fires, 4326)); pad <- buffer_km / 111  # ~deg per km
      adj <- dplyr::filter(fb_data_bing,
                           dplyr::between(longitude, bb["xmin"]-pad, bb["xmax"]+pad),
                           dplyr::between(latitude,  bb["ymin"]-pad, bb["ymax"]+pad))
    } else adj <- fb_data_bing
  }
  if (nrow(adj) == 0) stop("population_timeseries(): no data to plot.")

  # Per-tile change = n_crisis - n_baseline, computed per ROW. It is NA whenever
  # EITHER side was suppressed (<10), so summing it is complete-case: every window
  # aggregates the same both-sides-present tiles (fixes the na.rm asymmetry where a
  # crisis-only tile inflated the net and a baseline-only tile deflated it). Splitting
  # the per-tile change at 0 gives gross influx vs gross exodus, which do NOT cancel.
  ts <- adj |>
    dplyr::mutate(datetime = lubridate::ymd_hm(paste(ds, hour), tz = "America/Los_Angeles"),
                  diff = n_crisis - n_baseline) |>
    dplyr::group_by(datetime) |>
    dplyr::summarise(
      n_tiles  = sum(!is.na(diff)),
      crisis   = sum(n_crisis,   na.rm = TRUE),
      baseline = sum(n_baseline, na.rm = TRUE),
      net      = sum(diff,          na.rm = TRUE),   # influx + exodus
      influx   = sum(pmax(diff, 0), na.rm = TRUE),   # gross gain, tiles above baseline (>= 0)
      exodus   = sum(pmin(diff, 0), na.rm = TRUE),   # gross loss, tiles below baseline (<= 0)
      .groups = "drop") |>
    dplyr::arrange(datetime)

  if (mode == "levels") {
    # Raw levels: two near-identical sawtooths dominated by the day/night cycle.
    return(
      ggplot(ts, aes(x = datetime)) +
        geom_line(aes(y = baseline, linetype = "Pre-crisis baseline"), color = "grey40", linewidth = 0.8) +
        geom_line(aes(y = crisis,   linetype = "Crisis"),              color = "firebrick", linewidth = 1) +
        geom_point(aes(y = crisis), color = "firebrick", size = 1.5) +
        scale_linetype_manual(values = c("Crisis" = "solid", "Pre-crisis baseline" = "dashed"), name = NULL) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(x = NULL, y = "Users in fire-adjacent tiles",
             title = sprintf("Population near the fires (within %g km) by 8-hour window", buffer_km)) +
        theme_minimal() +
        theme(legend.position = "bottom")
    )
  }

  if (mode == "anomaly") {
    # Net (crisis - baseline) per window (complete-case). Cancels the day/night cycle,
    # but influx and exodus net against each other — a metro-wide influx can hide a
    # fire-edge exodus. Use "split" to keep them separate.
    return(
      ggplot(ts, aes(x = datetime, y = net)) +
        geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
        geom_line(color = "grey20", linewidth = 1) +
        geom_point(aes(color = net < 0), size = 1.8, show.legend = FALSE) +
        scale_color_manual(values = c(`TRUE` = "steelblue", `FALSE` = "firebrick")) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(x = NULL, y = "Net users vs. baseline (crisis − baseline)",
             title = sprintf("Net population anomaly near the fires (within %g km), by 8-hour window", buffer_km)) +
        theme_minimal() +
        theme(legend.position = "bottom")
    )
  }

  # mode == "split" (recommended): gross influx (red, above 0) and gross exodus
  # (blue, below 0) drawn separately, with the net line on top. Because influx and
  # exodus are NOT summed together, an exodus at the fire edge stays visible even when
  # the metro influx keeps the net positive.
  ggplot(ts, aes(x = datetime)) +
    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
    geom_area(aes(y = influx, fill = "Influx (tiles above baseline)"), alpha = 0.35) +
    geom_area(aes(y = exodus, fill = "Exodus (tiles below baseline)"), alpha = 0.35) +
    geom_line(aes(y = net, colour = "Net (influx + exodus)"), linewidth = 1) +
    geom_point(aes(y = net), colour = "grey20", size = 1.3) +
    scale_fill_manual(values = c("Influx (tiles above baseline)" = "firebrick",
                                 "Exodus (tiles below baseline)" = "steelblue"), name = NULL) +
    scale_colour_manual(values = c("Net (influx + exodus)" = "grey20"), name = NULL) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = NULL, y = "Users vs. baseline (crisis − baseline)",
         title = sprintf("Fire-adjacent gross influx vs. exodus (within %g km), by 8-hour window", buffer_km)) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# ---------------------------------------------------------------------------
# population_net_by_buffer() — localization view: the NET line only (no bands),
# overlaid at several buffer radii (report §3 companion to population_timeseries).
# WHY: one panel that shows the exodus is concentrated at the fire edge — the net
#      deepens as the ring tightens and washes toward zero as it widens into the
#      metro. Far less ink than stacking several split charts side by side, and all
#      radii share ONE y-axis so the comparison is valid. Reuses the same
#      complete-case net (crisis - baseline) and UTM buffer as population_timeseries().
#' @param fb_data_bing df population tiles (needs latitude, longitude, ds, hour, n_crisis, n_baseline).
#' @param fires        sf wildfire perimeters (fetch_fires()); if empty, all tiles are used.
#' @param buffers      num radii in km to overlay (default c(3, 5, 8)).
#' @param buffer_crs   int projected CRS for the buffers (default 32611 = UTM 11N; true ground metres).
#' @return ggplot: one net line per buffer radius, with a zero reference.
#' @sideeffects none (pure).
#' @examples population_net_by_buffer(fb_data_bing, fires, buffers = c(3, 5, 8))
# ---------------------------------------------------------------------------
population_net_by_buffer <- function(fb_data_bing, fires, buffers = c(3, 5, 8),
                                     buffer_crs = 32611) {
  stopifnot(all(c("latitude","longitude","ds","hour","n_crisis","n_baseline") %in% names(fb_data_bing)))
  buffers <- sort(unique(buffers))
  lvls <- paste0(buffers, " km")
  pts <- sf::st_transform(
    sf::st_as_sf(fb_data_bing, coords = c("longitude","latitude"), crs = 4326, remove = FALSE),
    buffer_crs)

  # net = sum(crisis - baseline) over complete-case tiles inside each buffer, per window.
  one_buffer <- function(km) {
    if (!is.null(fires) && inherits(fires, "sf") && nrow(fires) > 0) {
      buf <- sf::st_union(sf::st_buffer(sf::st_transform(fires, buffer_crs), km * 1000))
      adj <- fb_data_bing[lengths(sf::st_intersects(pts, buf)) > 0, ]
    } else adj <- fb_data_bing
    if (nrow(adj) == 0) return(NULL)
    adj |>
      dplyr::mutate(datetime = lubridate::ymd_hm(paste(ds, hour), tz = "America/Los_Angeles"),
                    diff = n_crisis - n_baseline) |>
      dplyr::group_by(datetime) |>
      dplyr::summarise(net = sum(diff, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(buffer = factor(paste0(km, " km"), levels = lvls))
  }

  ts <- dplyr::bind_rows(lapply(buffers, one_buffer))
  if (is.null(ts) || nrow(ts) == 0) stop("population_net_by_buffer(): no data to plot.")

  # Ordered viridis ramp so tighter rings read as one end of the scale; all share one axis.
  ggplot(ts, aes(x = datetime, y = net, colour = buffer)) +
    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.2) +
    scale_colour_viridis_d(option = "viridis", end = 0.9, name = "Buffer radius") +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = NULL, y = "Net users vs. baseline (crisis − baseline)",
         title = "Net fire-adjacent population by buffer radius, by 8-hour window") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# ---------------------------------------------------------------------------
# movement_plot2() — legible evacuation-flow map (report §4). ADDITIVE: the
# original movement_plot() is left untouched.
# WHY: movement_plot() overplots into a hairball on a data-driven extent; this
#      hard-clips to the operational window, drops tiny flows, encodes volume by
#      width+alpha (Iter 1), and can split egress vs intake + colour by z_score
#      (Iter 2), so an EM can read who is leaving the threatened area and where to.
#' @param plot_ds       chr date "YYYY-MM-DD" of the movement window (from moved$ds).
#' @param plot_hour     chr "0800" or "1600" ONLY (movement has 2 transitions/day).
#' @param focus_lon     num c(min,max) longitude — HARD clip (default Spokane metro).
#' @param focus_lat     num c(min,max) latitude  — HARD clip.
#' @param min_users     num drop flows below this many crisis users (default 25).
#' @param top_n         int keep only the N largest flows (NULL = all).
#' @param min_length_km num/@param max_length_km num optional flow-length band (NULL = off).
#' @param direction     chr "all" (default) / "egress" (origin near fire) / "intake" (dest near fire).
#' @param color_by      chr "none" (Iter 1, single colour) / "zscore" (Iter 2, RdBu +/-4) / "users" (mako).
#' @param buffer_km     num fire buffer for egress/intake (default 15).
#' @param curve         lgl geom_curve (TRUE) vs geom_segment (FALSE).
#' @param zoom          int basemap zoom (default 10).
#' @param plot_title    chr optional title (default auto).
#' @return ggplot object.
#' @sideeffects reads globals `moved` and `fires`; may fetch/cache a basemap.
#' @examples movement_plot2("2026-08-03","0800", direction="egress", color_by="zscore")
# ---------------------------------------------------------------------------
movement_plot2 <- function(
    plot_ds, plot_hour,
    focus_lon = c(-117.9, -117.0),   # default Spokane-metro operational extent
    focus_lat = c(47.4, 47.9),
    min_users = 25,
    top_n = NULL,
    min_length_km = NULL, max_length_km = NULL,
    direction = c("all", "egress", "intake"),
    color_by  = c("none", "zscore", "users"),
    buffer_km = 15,
    curve = TRUE,
    zoom = 10,
    show_aoi = TRUE,     # draw the crop boundary when it falls within the focus extent
    aoi = NULL,          # optional sf AOI outline; NULL -> use global `aoi`
    plot_title = NULL) {

  direction <- match.arg(direction)
  color_by  <- match.arg(color_by)
  if (!plot_hour %in% c("0800", "1600"))
    stop("movement_plot2(): movement has only 0800/1600 windows; got '", plot_hour, "'.")

  users_col <- "# Users During Crisis"
  flows <- dplyr::filter(moved, ds == plot_ds, hour == plot_hour)
  if (nrow(flows) == 0)
    stop("movement_plot2(): no movement for ds=", plot_ds, ", hour=", plot_hour,
         ". Available: ", paste(sort(unique(paste(moved$ds, moved$hour))), collapse = ", "))

  # (robustness) guarantee the friendly users column exists (rename n_crisis if a
  # stale/raw `moved` is in use), and check z_score before the zscore colour path.
  flows <- ensure_users_col(flows, "moved")
  if (color_by == "zscore" && !("z_score" %in% names(flows)))
    stop("movement_plot2(color_by='zscore'): `z_score` not in `moved`. Rebuild with ",
         "build_moved(mp_data_bing, re_run = TRUE), or use color_by = 'none'/'users'.")

  # magnitude / length filters remove the sub-threshold churn that makes the starburst
  flows <- dplyr::filter(flows, !is.na(.data[[users_col]]), .data[[users_col]] >= min_users)
  if (!is.null(min_length_km)) flows <- dplyr::filter(flows, length_km >= min_length_km)
  if (!is.null(max_length_km)) flows <- dplyr::filter(flows, length_km <= max_length_km)

  # Iteration 2: egress/intake split — is the flow's ORIGIN (egress) or DEST (intake)
  # inside the buffered fire polygon?
  if (direction != "all") {
    if (is.null(fires) || !inherits(fires, "sf") || nrow(fires) == 0) {
      warning("movement_plot2(): no fire layer — cannot split egress/intake; using all flows.")
    } else {
      # Buffer in UTM 11N (true metres), not EPSG:3857 whose metres are inflated
      # ~1/cos(lat) (~1.48x here) — so buffer_km is honest ground distance.
      buf <- sf::st_transform(
        sf::st_union(sf::st_buffer(sf::st_transform(fires, 32611), buffer_km * 1000)), 4326)
      spt <- sf::st_as_sf(flows, coords = c("start_longitude","start_latitude"), crs = 4326, remove = FALSE)
      ept <- sf::st_as_sf(flows, coords = c("end_longitude","end_latitude"),     crs = 4326, remove = FALSE)
      keep <- if (direction == "egress") lengths(sf::st_intersects(spt, buf)) > 0
              else                        lengths(sf::st_intersects(ept, buf)) > 0
      flows <- flows[keep, ]
    }
  }
  if (nrow(flows) == 0)
    stop("movement_plot2(): no flows left after filtering (min_users=", min_users,
         ", direction=", direction, "). Loosen the filters.")

  if (!is.null(top_n))
    flows <- dplyr::slice_max(flows, order_by = .data[[users_col]], n = top_n, with_ties = FALSE)

  # Project endpoints to 3857 so everything (flows, basemap, fires) shares one CRS,
  # exactly like population_plot() — avoids coord_sf lon/lat-vs-metres ambiguity.
  s <- sf::st_coordinates(sf::st_transform(
        sf::st_as_sf(flows, coords = c("start_longitude","start_latitude"), crs = 4326), 3857))
  e <- sf::st_coordinates(sf::st_transform(
        sf::st_as_sf(flows, coords = c("end_longitude","end_latitude"),     crs = 4326), 3857))
  flows$sx <- s[,1]; flows$sy <- s[,2]; flows$ex <- e[,1]; flows$ey <- e[,2]

  # focus bbox in 3857 for the hard clip
  bb <- sf::st_bbox(c(xmin = focus_lon[1], xmax = focus_lon[2],
                      ymin = focus_lat[1], ymax = focus_lat[2]), crs = sf::st_crs(4326)) |>
    sf::st_as_sfc() |> sf::st_transform(3857) |> sf::st_bbox()

  # cap zoom so a wide/ballooned focus can't request thousands of tiles (OOM guard)
  zoom <- fit_zoom(focus_lon, focus_lat, zoom)
  # muted Positron basemap so dark flow lines pop
  osm <- tryCatch(
    get_tiles(sf::st_as_sf(data.frame(lon = focus_lon, lat = focus_lat),
                           coords = c("lon","lat"), crs = 4326),
              provider = "CartoDB.Positron", crop = TRUE, zoom = zoom, cachedir = tile_cache_dir),
    error = function(e) { warning("Basemap download failed (", conditionMessage(e),
                                  "); drawing without a basemap."); NULL })

  # width+alpha carry volume (preattentive); colour is optional (Iter 2)
  flow_aes <- switch(color_by,
    none   = aes(x = sx, y = sy, xend = ex, yend = ey,
                 linewidth = .data[[users_col]], alpha = .data[[users_col]]),
    zscore = aes(x = sx, y = sy, xend = ex, yend = ey,
                 linewidth = .data[[users_col]], alpha = .data[[users_col]], colour = z_score),
    users  = aes(x = sx, y = sy, xend = ex, yend = ey,
                 linewidth = .data[[users_col]], alpha = .data[[users_col]], colour = .data[[users_col]]))

  geom_fun   <- if (curve) geom_curve else geom_segment
  extra      <- if (color_by == "none") list(colour = "grey20") else list()   # fixed colour for Iter 1
  arrow_spec <- arrow(length = unit(0.12, "cm"), type = "closed")             # small heads, not starbursts
  flow_layer <- do.call(geom_fun, c(list(mapping = flow_aes, data = flows,
                                          arrow = arrow_spec, lineend = "round"),
                                     if (curve) list(curvature = 0.2) else list(), extra))

  p <- ggplot()
  if (!is.null(osm)) p <- p + layer_spatial(osm)
  p <- p + flow_layer +
    scale_linewidth(range = c(0.2, 2.5), name = "Users moving") +
    scale_alpha(range = c(0.1, 0.85), guide = "none")
  if (color_by == "zscore")
    p <- p + scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-4, 4),
                                   oob = scales::squish, name = "Flow z-score")
  if (color_by == "users")
    p <- p + scale_color_viridis_c(option = "mako", direction = -1,
                                   name = "Users moving", labels = scales::label_comma())
  if (!is.null(fires) && inherits(fires, "sf") && nrow(fires) > 0)
    p <- p + geom_sf(data = clip_sf_to_box(fires, c(bb["xmin"], bb["xmax"]),
                                           c(bb["ymin"], bb["ymax"])),
                     fill = NA, color = "darkred", linewidth = 0.5)

  # (AOI) crop-boundary neatline; only visible when the focus extent reaches the
  # county edges (clipped away on a tight metro zoom). Reads global `aoi` unless passed.
  ao <- if (isTRUE(show_aoi)) { if (!is.null(aoi)) aoi else get0("aoi") } else NULL
  if (!is.null(ao) && inherits(ao, "sf") && nrow(ao) > 0)
    p <- p + geom_sf(data = clip_sf_to_box(ao, c(bb["xmin"], bb["xmax"]),
                                           c(bb["ymin"], bb["ymax"])),
                     fill = NA, color = "grey20", linetype = "dashed", linewidth = 0.4)

  ttl <- if (!is.null(plot_title)) plot_title else
    sprintf("Movement %s %s — %s", plot_ds, plot_hour, direction)
  p +
    coord_sf(crs = sf::st_crs(3857),
             xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]),
             expand = FALSE) +                                                # HARD clip
    labs(title = ttl) +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), legend.position = "bottom")
}

# ---------------------------------------------------------------------------
# aggregate_flows() — Iteration 3 SCAFFOLD (report §4 notes). Collapse O-D vectors
# to a coarser grid so dozens of parallel arrows become a few thick, labeled flows.
# WHY: the most legible EM product aggregates to named areas; grid-rounding is the
#      first step. NOTE: never sum inbound+outbound for net flux — sub-10-user
#      vectors are dropped upstream, so nets are biased.
#' @param flows df filtered movement rows (as inside movement_plot2).
#' @param round_deg num grid size in degrees to snap origins/destinations (default 0.05, ~5 km).
#' @return df aggregated O-D with summed users and a count of collapsed vectors.
#' @sideeffects none.
#' @examples aggregate_flows(dplyr::filter(moved, ds == "2026-08-03", hour == "0800"))
# ---------------------------------------------------------------------------
aggregate_flows <- function(flows, round_deg = 0.05) {
  users_col <- "# Users During Crisis"
  flows <- ensure_users_col(flows, "flows")   # rename n_crisis -> friendly name if needed
  flows |>
    dplyr::mutate(o_lon = round(start_longitude / round_deg) * round_deg,
                  o_lat = round(start_latitude  / round_deg) * round_deg,
                  d_lon = round(end_longitude   / round_deg) * round_deg,
                  d_lat = round(end_latitude    / round_deg) * round_deg) |>
    dplyr::group_by(o_lon, o_lat, d_lon, d_lat) |>
    dplyr::summarise(users = sum(.data[[users_col]], na.rm = TRUE),
                     n_vectors = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(users))
}
# Iteration 3 further ideas (not implemented — need extra deps / design decisions):
#  * aggregate to named neighborhoods / evacuation zones rather than a raw grid;
#  * edge bundling for a regional view (requires ggraph/edgebundle);
#  * small multiples across pivotal windows for the static PDF (keep animation in the gif).

# ---------------------------------------------------------------------------
# prepare_flows() — the filtered movement rows for one window (same filtering
# movement_plot2() does internally), exposed so callers can reuse the exact flows
# for framing (flow_extent), aggregation, or inspection.
# WHY: DRY - one definition of "which flows count" for extent + plotting.
#' @param plot_ds/@param plot_hour chr window ("0800"/"1600" only).
#' @param direction chr "all"/"egress"/"intake" (egress/intake need the fire layer).
#' @param min_users num drop flows below this (default 25).
#' @param top_n int keep only the N largest (NULL = all).
#' @param min_length_km/@param max_length_km num optional flow-length band.
#' @param buffer_km num fire buffer for egress/intake (default 15).
#' @return data.frame of moved rows after filtering (with `# Users During Crisis`).
#' @sideeffects reads globals `moved` and `fires`.
#' @examples prepare_flows(mv_ds, mv_hour, direction = "egress", min_users = 15)
# ---------------------------------------------------------------------------
prepare_flows <- function(plot_ds, plot_hour, direction = c("all", "egress", "intake"),
                          min_users = 25, top_n = NULL,
                          min_length_km = NULL, max_length_km = NULL, buffer_km = 15) {
  direction <- match.arg(direction)
  if (!plot_hour %in% c("0800", "1600"))
    stop("prepare_flows(): movement has only 0800/1600 windows; got '", plot_hour, "'.")
  users_col <- "# Users During Crisis"
  flows <- ensure_users_col(dplyr::filter(moved, ds == plot_ds, hour == plot_hour), "moved")
  flows <- dplyr::filter(flows, !is.na(.data[[users_col]]), .data[[users_col]] >= min_users)
  if (!is.null(min_length_km)) flows <- dplyr::filter(flows, length_km >= min_length_km)
  if (!is.null(max_length_km)) flows <- dplyr::filter(flows, length_km <= max_length_km)
  if (direction != "all") {
    if (is.null(fires) || !inherits(fires, "sf") || nrow(fires) == 0) {
      warning("prepare_flows(): no fire layer - cannot split egress/intake; using all flows.")
    } else {
      # Buffer in UTM 11N (true metres), not EPSG:3857 whose metres are inflated
      # ~1/cos(lat) (~1.48x here) — so buffer_km is honest ground distance.
      buf <- sf::st_transform(
        sf::st_union(sf::st_buffer(sf::st_transform(fires, 32611), buffer_km * 1000)), 4326)
      spt <- sf::st_as_sf(flows, coords = c("start_longitude","start_latitude"), crs = 4326, remove = FALSE)
      ept <- sf::st_as_sf(flows, coords = c("end_longitude","end_latitude"),     crs = 4326, remove = FALSE)
      keep <- if (direction == "egress") lengths(sf::st_intersects(spt, buf)) > 0
              else                        lengths(sf::st_intersects(ept, buf)) > 0
      flows <- flows[keep, ]
    }
  }
  if (!is.null(top_n) && nrow(flows) > 0)
    flows <- dplyr::slice_max(flows, order_by = .data[[users_col]], n = top_n, with_ties = FALSE)
  flows
}

# ---------------------------------------------------------------------------
# flow_extent() — data-driven map box around a set of flows (+ margin).
# WHY: frame a movement map to where the flows actually are, instead of a fixed box.
#' @param flows     df flows from prepare_flows() (needs start/end lon/lat).
#' @param margin_km num padding around the flow endpoints (default 5).
#' @return list(lon = c(min,max), lat = c(min,max)) - same shape as study_extent().
#' @sideeffects none.
#' @examples flow_extent(prepare_flows(mv_ds, mv_hour, direction = "egress", min_users = 15))
# ---------------------------------------------------------------------------
flow_extent <- function(flows, margin_km = 5) {
  if (nrow(flows) == 0) stop("flow_extent(): no flows to frame (loosen the filters).")
  lon <- c(flows$start_longitude, flows$end_longitude)
  lat <- c(flows$start_latitude,  flows$end_latitude)
  lat_mid <- mean(range(lat, na.rm = TRUE))
  dlat <- margin_km / 111
  dlon <- margin_km / (111 * cos(lat_mid * pi / 180))
  list(lon = c(min(lon, na.rm = TRUE) - dlon, max(lon, na.rm = TRUE) + dlon),
       lat = c(min(lat, na.rm = TRUE) - dlat, max(lat, na.rm = TRUE) + dlat))
}

# ---------------------------------------------------------------------------
# add_scalebar_3857() — Mercator-corrected distance scale bar for a 3857 plot.
# WHY: web-Mercator (EPSG:3857) inflates map units by sec(latitude); at Spokane
#      (~47.6 deg N) a bar sized in raw map units overstates ground distance ~48%.
#      This sizes the bar as km_ground * 1000 / cos(lat) so its label is truthful.
#' @param bb        bbox-like with xmin/xmax/ymin/ymax in EPSG:3857 (the clip box).
#' @param km        num ground length of the bar; NULL -> a "nice" ~1/4-width value.
#' @param pad       num inset from the lower-left corner (fraction of span, default 0.04).
#' @param text_size num label size.
#' @return list of ggplot layers (add to a plot with `+`).
#' @sideeffects none.
#' @examples p + add_scalebar_3857(bbox_3857, km = 10)
# ---------------------------------------------------------------------------
add_scalebar_3857 <- function(bb, km = NULL, pad = 0.04, text_size = 3) {
  span_x <- bb[["xmax"]] - bb[["xmin"]]
  span_y <- bb[["ymax"]] - bb[["ymin"]]
  x0 <- bb[["xmin"]] + pad * span_x
  y0 <- bb[["ymin"]] + pad * span_y
  # latitude at the bar (inverse web-Mercator of its y) for the sec() correction
  lat    <- (2 * atan(exp(y0 / 6378137)) - pi / 2) * 180 / pi
  cosphi <- cos(lat * pi / 180)
  # pick a round ground distance (~1/4 of the map width) unless one was supplied
  if (is.null(km)) {
    target_m <- 0.25 * span_x * cosphi
    nice_m   <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 500) * 1000
    km       <- nice_m[which.min(abs(nice_m - target_m))] / 1000
  }
  bar_map <- km * 1000 / cosphi          # bar length in 3857 map units
  tick    <- 0.012 * span_y
  list(
    annotate("segment", x = x0, xend = x0 + bar_map, y = y0, yend = y0,
             linewidth = 1.1, color = "grey15"),
    annotate("segment", x = x0,           xend = x0,           y = y0, yend = y0 + tick, color = "grey15"),
    annotate("segment", x = x0 + bar_map, xend = x0 + bar_map, y = y0, yend = y0 + tick, color = "grey15"),
    annotate("text", x = x0 + bar_map / 2, y = y0 + 3 * tick,
             label = paste0(km, " km"), size = text_size, color = "grey15")
  )
}

# ---------------------------------------------------------------------------
# movement_plot3() — legible v3 evacuation-flow map (report §6e). ADDITIVE:
# movement_plot() and movement_plot2() are left untouched.
# WHY: v1/v2 have three problems this fixes. (1) CONTRAST - a dark casing/halo is
#      drawn under every flow so pale RdBu / faint arrows stay visible on the
#      near-white basemap, which is itself muted by a translucent white scrim
#      (basemap_fade), and flows use a high constant opacity (no vanishing at the
#      old alpha=0.1 floor). (2) ZOOM/ARROW SIZE - fit="flows" frames to the
#      retained flows (flow_extent(), padded, with a minimum-span floor) so the
#      arrows fill the frame. (3) SCALE - a Mercator-corrected scale bar
#      (add_scalebar_3857()) plus a north arrow. Reuses prepare_flows()/
#      flow_extent()/fit_zoom()/ensure_users_col() so "which flows count" has one
#      definition shared with the framing preview and the animation.
#' @inheritParams prepare_flows
#' @param color_by   chr "zscore" (RdBu +/-4) / "users" (rocket, dark=more) / "none".
#' @param fit        chr "flows" (data-driven frame, default) or "fixed" (focus_lon/lat box).
#' @param focus_lon/@param focus_lat num the fit="fixed" box (default Spokane metro).
#' @param pad_km     num padding around the flows for fit="flows" (default 6).
#' @param min_span_km num widen the flows box to at least this span per axis (default 12).
#' @param curve      lgl geom_curve (TRUE) vs geom_segment (FALSE).
#' @param halo       lgl draw the dark casing under the flows (default TRUE).
#' @param halo_color chr casing colour ("grey20" for a light basemap; "white" for a dark one).
#' @param halo_mm    num casing width added under the thinnest flow (default 1).
#' @param basemap_fade num 0..0.6 white scrim over the basemap (higher = arrows pop; default 0.35).
#' @param linewidth_range num c(min,max) flow line widths (default c(0.4, 3.2)).
#' @param arrow_cm   num arrow-head length in cm (default 0.18).
#' @param zoom       int basemap tile detail (default 11; capped by fit_zoom()).
#' @param scalebar/@param north lgl draw the scale bar / north arrow (default TRUE).
#' @param plot_title chr optional title (default auto).
#' @return ggplot object.
#' @sideeffects reads globals `moved` and `fires`; may fetch/cache a basemap.
#' @examples movement_plot3(mv_ds, mv_hour, direction = "egress", color_by = "zscore", min_users = 15)
# ---------------------------------------------------------------------------
movement_plot3 <- function(plot_ds, plot_hour,
                           direction = c("all", "egress", "intake"),
                           color_by  = c("zscore", "users", "none"),
                           min_users = 25, top_n = NULL,
                           min_length_km = NULL, max_length_km = NULL,
                           buffer_km = 15,
                           fit = c("flows", "fixed"),
                           focus_lon = c(-117.9, -117.0), focus_lat = c(47.4, 47.9),
                           pad_km = 6, min_span_km = 12,
                           curve = TRUE,
                           halo = TRUE, halo_color = "grey20", halo_mm = 1,
                           basemap_fade = 0.35,
                           linewidth_range = c(0.4, 3.2), arrow_cm = 0.18,
                           users_limits = NULL,   # fix the "users moving" scale limits so
                                                  # small-multiple panels share ONE width legend
                           zoom = 11, scalebar = TRUE, north = TRUE,
                           plot_title = NULL) {
  direction <- match.arg(direction)
  color_by  <- match.arg(color_by)
  fit       <- match.arg(fit)
  users_col <- "# Users During Crisis"

  # 1) Flows for this window (one definition of "which flows count", via prepare_flows).
  flows <- prepare_flows(plot_ds, plot_hour, direction = direction, min_users = min_users,
                         top_n = top_n, min_length_km = min_length_km,
                         max_length_km = max_length_km, buffer_km = buffer_km)
  if (nrow(flows) == 0)
    stop("movement_plot3(): no flows after filtering (min_users=", min_users,
         ", direction=", direction, "). Loosen the filters.")
  if (color_by == "zscore" && !("z_score" %in% names(flows)))
    stop("movement_plot3(color_by='zscore'): `z_score` not in `moved`. Rebuild with ",
         "build_moved(mp_data_bing, re_run = TRUE), or use color_by = 'none'/'users'.")

  # 2) Framing: a fixed box, or data-driven from the flows with a minimum-span
  #    floor so a couple of short flows can't produce a tiny, over-zoomed frame.
  if (fit == "fixed") {
    ext <- list(lon = sort(focus_lon), lat = sort(focus_lat))
  } else {
    ext <- flow_extent(flows, margin_km = pad_km)
    per_lat <- 111; per_lon <- 111 * cos(mean(ext$lat) * pi / 180)   # km per degree
    grow <- function(v, per) {
      short <- min_span_km - diff(range(v)) * per
      if (short > 0) { d <- (short / 2) / per; c(min(v) - d, max(v) + d) } else v
    }
    ext$lon <- grow(ext$lon, per_lon); ext$lat <- grow(ext$lat, per_lat)
  }

  # 3) Project endpoints to 3857 so flows, basemap, and fires share one CRS
  #    (mirrors population_plot()/movement_plot2() — no lon/lat-vs-metres ambiguity).
  to_3857 <- function(lon, lat)
    sf::st_coordinates(sf::st_transform(
      sf::st_as_sf(flows, coords = c(lon, lat), crs = 4326), 3857))
  s <- to_3857("start_longitude", "start_latitude")
  e <- to_3857("end_longitude",   "end_latitude")
  flows$sx <- s[, 1]; flows$sy <- s[, 2]; flows$ex <- e[, 1]; flows$ey <- e[, 2]

  bb <- sf::st_bbox(c(xmin = ext$lon[1], xmax = ext$lon[2],
                      ymin = ext$lat[1], ymax = ext$lat[2]), crs = sf::st_crs(4326)) |>
    sf::st_as_sfc() |> sf::st_transform(3857) |> sf::st_bbox()

  # 4) Muted basemap for this extent (zoom capped so a wide box can't OOM).
  zoom <- fit_zoom(ext$lon, ext$lat, zoom)
  osm <- tryCatch(
    get_tiles(sf::st_as_sf(data.frame(lon = ext$lon, lat = ext$lat),
                           coords = c("lon", "lat"), crs = 4326),
              provider = "CartoDB.Positron", crop = TRUE, zoom = zoom, cachedir = tile_cache_dir),
    error = function(e) { warning("Basemap download failed (", conditionMessage(e),
                                  "); drawing without a basemap."); NULL })

  # 5) Flow marks: width ∝ users (preattentive), high constant opacity, optional
  #    colour. The casing is sized to the THIN end so it outlines thin/pale flows
  #    yet stays hidden under thick ones — this keeps ONE linewidth scale (and its
  #    "Users moving" legend) instead of needing ggnewscale for a wider casing.
  geom_fun   <- if (curve) geom_curve else geom_segment
  curve_args <- if (curve) list(curvature = 0.2) else list()
  arrow_spec <- arrow(length = unit(arrow_cm, "cm"), type = "closed")
  base_aes   <- aes(x = sx, y = sy, xend = ex, yend = ey)
  flow_aes   <- switch(color_by,
    none   = aes(x = sx, y = sy, xend = ex, yend = ey, linewidth = .data[[users_col]]),
    zscore = aes(x = sx, y = sy, xend = ex, yend = ey, linewidth = .data[[users_col]], colour = z_score),
    users  = aes(x = sx, y = sy, xend = ex, yend = ey, linewidth = .data[[users_col]], colour = .data[[users_col]]))

  p <- ggplot()
  if (!is.null(osm)) {
    p <- p + layer_spatial(osm)
    if (basemap_fade > 0)   # translucent white scrim so dark flows pop
      p <- p + annotate("rect", xmin = bb[["xmin"]], xmax = bb[["xmax"]],
                        ymin = bb[["ymin"]], ymax = bb[["ymax"]],
                        fill = "white", alpha = basemap_fade)
  }
  if (halo)
    p <- p + do.call(geom_fun, c(list(mapping = base_aes, data = flows,
                                       linewidth = min(linewidth_range) + halo_mm,
                                       colour = halo_color, alpha = 0.55,
                                       arrow = arrow_spec, lineend = "round"), curve_args))
  none_col <- if (color_by == "none") list(colour = "grey20") else list()
  p <- p + do.call(geom_fun, c(list(mapping = flow_aes, data = flows,
                                     alpha = 0.9, arrow = arrow_spec, lineend = "round"),
                                curve_args, none_col)) +
    # colour = users duplicates the width encoding, so hide the width legend there.
    # users_limits (when set) fixes the scale so small-multiple panels share ONE legend.
    scale_linewidth(range = linewidth_range, limits = users_limits, name = "Users moving",
                    guide = if (color_by == "users") "none" else "legend")
  if (color_by == "zscore")
    p <- p + scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-4, 4),
                                   oob = scales::squish, name = "Flow z-score")
  if (color_by == "users")
    p <- p + scale_color_viridis_c(option = "rocket", direction = -1,
                                   name = "Users moving", labels = scales::label_comma())
  if (!is.null(fires) && inherits(fires, "sf") && nrow(fires) > 0)
    p <- p + geom_sf(data = clip_sf_to_box(fires, c(bb[["xmin"]], bb[["xmax"]]),
                                           c(bb[["ymin"]], bb[["ymax"]])),
                     fill = NA, color = "darkred", linewidth = 0.5)

  # 6) Scale bar + north arrow, both drawn in 3857 data coordinates.
  if (scalebar) p <- p + add_scalebar_3857(bb)
  if (north) {
    nx  <- bb[["xmax"]] - 0.06 * (bb[["xmax"]] - bb[["xmin"]])
    ny0 <- bb[["ymax"]] - 0.16 * (bb[["ymax"]] - bb[["ymin"]])
    ny1 <- bb[["ymax"]] - 0.07 * (bb[["ymax"]] - bb[["ymin"]])
    p <- p +
      annotate("segment", x = nx, xend = nx, y = ny0, yend = ny1, color = "grey15",
               linewidth = 0.8, arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
      annotate("text", x = nx, y = ny1 + 0.02 * (bb[["ymax"]] - bb[["ymin"]]),
               label = "N", fontface = 2, size = 3.2, color = "grey15")
  }

  ttl <- if (!is.null(plot_title)) plot_title else
    sprintf("Movement %s %s — %s", plot_ds, plot_hour, direction)
  p +
    coord_sf(crs = sf::st_crs(3857),
             xlim = c(bb[["xmin"]], bb[["xmax"]]), ylim = c(bb[["ymin"]], bb[["ymax"]]),
             expand = FALSE) +                                          # HARD clip
    labs(title = ttl) +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), legend.position = "bottom")
}

# ---------------------------------------------------------------------------
# movement_plot_by_distance() — the egress flow map split into 3 panels by flow
# LENGTH (short / medium / long), sharing one fixed extent and one legend (report §4).
# WHY: a single window overplots into a hairball. Banding the flows
#      by distance separates local shuffling from cross-town and out-of-area moves, so
#      the arrows in each panel are far easier to read. All panels share the same box
#      and the z-score / users legend is collected once (patchwork guides = "collect").
#' @inheritParams prepare_flows
#' @param breaks_km num length(2) cut points (km) between short|medium|long; NULL = data tertiles.
#' @param focus_lon/@param focus_lat num the shared fixed extent (default Spokane metro).
#' @param color_by chr "zscore" (default) / "users" / "none" (passed to movement_plot3).
#' @param zoom int basemap zoom for each panel (default 11).
#' @param ... further args forwarded to movement_plot3() (halo, basemap_fade, ...).
#' @return a patchwork of 3 ggplots (short/medium/long) with one shared legend.
#' @sideeffects reads globals `moved` and `fires`; may fetch/cache basemaps.
#' @examples movement_plot_by_distance(mv_ds, mv_hour, direction = "egress", min_users = 15)
# ---------------------------------------------------------------------------
movement_plot_by_distance <- function(plot_ds, plot_hour,
                                      direction = "egress", min_users = 15, buffer_km = 15,
                                      breaks_km = NULL,
                                      focus_lon = c(-117.7, -117.0), focus_lat = c(47.45, 47.85),
                                      color_by = "zscore", zoom = 11, ...) {
  users_col <- "# Users During Crisis"
  # One definition of "which flows count", then band by length.
  flows <- prepare_flows(plot_ds, plot_hour, direction = direction,
                         min_users = min_users, buffer_km = buffer_km)
  if (nrow(flows) == 0) stop("movement_plot_by_distance(): no flows after filtering.")
  if (!("length_km" %in% names(flows)))
    stop("movement_plot_by_distance(): `length_km` not in movement data; cannot band by distance.")

  # Shared width-scale limits (global user range) so the 3 panels share ONE "Users moving"
  # legend instead of one per panel.
  ulim <- range(flows[[users_col]], na.rm = TRUE)

  # Cut points: supplied, else data tertiles so each panel holds ~1/3 of the flows.
  if (is.null(breaks_km))
    breaks_km <- unname(stats::quantile(flows$length_km, c(1/3, 2/3), na.rm = TRUE))
  b <- sort(unique(round(breaks_km, 1)))
  if (length(b) < 2) b <- c(b[1], b[1] + 1)              # degenerate-data guard

  bands <- list(
    list(lab = sprintf("Short (< %.0f km)",   b[1]),        lo = NULL, hi = b[1]),
    list(lab = sprintf("Medium (%.0f–%.0f km)", b[1], b[2]), lo = b[1], hi = b[2]),
    list(lab = sprintf("Long (> %.0f km)",    b[2]),        lo = b[2], hi = NULL))

  # Fixed extent so the three panels are directly comparable; a band with no flows
  # degrades to a titled blank panel instead of aborting the whole figure.
  mk <- function(bd) tryCatch(
    movement_plot3(plot_ds, plot_hour, direction = direction, color_by = color_by,
                   min_users = min_users, buffer_km = buffer_km,
                   min_length_km = bd$lo, max_length_km = bd$hi,
                   fit = "fixed", focus_lon = focus_lon, focus_lat = focus_lat,
                   users_limits = ulim, zoom = zoom, north = FALSE, plot_title = bd$lab, ...),
    error = function(e)
      ggplot() + labs(title = paste0(bd$lab, " — no flows")) + theme_void())

  patchwork::wrap_plots(lapply(bands, mk), ncol = 3, guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}

# TODO(Samsara): set EVAC_ZONES_URL to the Spokane County / SREC evacuation-zone
# ArcGIS FeatureServer query endpoint (.../query?where=1%3D1&outFields=*&f=geojson).
# Left NA on purpose so the report renders WITHOUT a guessed URL (Task 4 rule).
EVAC_ZONES_URL <- NA_character_

# normalize_evac_levels() — best-effort mapping of a raw evac layer to level 1/2/3.
# WHY: source schemas vary; population_plot()'s overlay expects a `level` field.
#' @param ez sf raw evacuation zones from ArcGIS.
#' @return sf with an integer `level` column (1/2/3; NA where unmappable).
#' @sideeffects none. NOTE: revisit the field/keyword mapping once the real schema is known.
normalize_evac_levels <- function(ez) {
  if (!inherits(ez, "sf") || nrow(ez) == 0) return(ez)
  nm <- names(ez); lvl <- rep(NA_integer_, nrow(ez))
  num_col <- nm[grepl("level|priority", nm, ignore.case = TRUE)][1]         # 1) numeric level-ish col
  if (!is.na(num_col) && is.numeric(ez[[num_col]])) lvl <- as.integer(ez[[num_col]])
  if (all(is.na(lvl))) {                                                     # 2) else parse text status
    txt_col <- nm[grepl("status|level|zone|stage", nm, ignore.case = TRUE)][1]
    if (!is.na(txt_col)) {
      s <- toupper(as.character(ez[[txt_col]]))
      lvl[grepl("3|GO NOW|GO|LEAVE", s)] <- 3L
      lvl[grepl("2|SET|BE READY", s)]    <- 2L
      lvl[grepl("1|READY|WARN", s)]      <- 1L
    }
  }
  ez$level <- lvl
  ez
}

# ---------------------------------------------------------------------------
# fetch_evac_zones() — evacuation-zone polygons, mirroring fetch_fires() (Task 4).
# WHY: overlaying Go/Set/Ready zones answers "are people still inside the Go-Now
#      zones?" Degrades gracefully: with no URL it returns an empty layer so maps
#      still render — never blocks on an unverified endpoint.
#' @param re_run    lgl force re-download (default re_run_cleaning).
#' @param cache     chr cache filename (default "evac_zones.Rds").
#' @param cache_dir chr directory for the cache (default rds_cache_dir).
#' @param url       chr ArcGIS geojson query URL (default EVAC_ZONES_URL; NA = disabled).
#' @return sf with a `level` column (empty sf if disabled/failed).
#' @sideeffects network (ArcGIS) + writes cache; falls back to cache then empty sf.
#' @examples evac_zones <- fetch_evac_zones()
# ---------------------------------------------------------------------------
fetch_evac_zones <- function(re_run = re_run_cleaning, cache = "evac_zones.Rds",
                             cache_dir = rds_cache_dir, url = EVAC_ZONES_URL) {
  path <- cache_path(cache, cache_dir)
  if (!re_run && file.exists(path)) { message("Loading cached ", path); return(readRDS(path)) }
  if (is.null(url) || is.na(url) || !nzchar(url)) {
    warning("EVAC_ZONES_URL not set - returning empty evac layer; maps render without the overlay. ",
            "Set EVAC_ZONES_URL (see TODO) to enable.")
    return(sf::st_sf(level = integer(0), geometry = sf::st_sfc(crs = 4326)))
  }
  message("Downloading evacuation zones -> ", path)
  tryCatch({
    ez <- normalize_evac_levels(sf::st_read(url, quiet = TRUE))
    saveRDS(ez, path); ez
  }, error = function(e) {
    if (file.exists(path)) {
      warning("Evac download failed (", conditionMessage(e), "); using cached ", path, ".")
      readRDS(path)
    } else {
      warning("Evac download failed (", conditionMessage(e), ") and no cache; empty layer.")
      sf::st_sf(level = integer(0), geometry = sf::st_sfc(crs = 4326))
    }
  })
}

# save_plot() — write a ggplot to outputs/ as a standalone PNG product (§11).
# WHY: each figure should also be a shareable file, not only a PDF page.
#' @param p ggplot; @param name chr filename; @param dir chr (default "outputs").
#' @param width/@param height/@param dpi numeric. @return (invisibly) the path.
#' @sideeffects writes a PNG under dir/. @examples save_plot(population_plot_z_score(latest_ds, latest_hour), "zscore_latest.png")
save_plot <- function(p, name, dir = "outputs", width = 8, height = 8, dpi = 300) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, name)
  ggplot2::ggsave(path, plot = p, width = width, height = height, dpi = dpi)
  message("saved ", path); invisible(path)
}

# ---------------------------------------------------------------------------
# study_extent() — pick a legible population-map bounding box by method.
# WHY: the default report extent (~224x222 km) renders ~2.4 km tiles too small to
#      read. This returns a tighter lon/lat box so maps are legible, with three
#      framings an EM can compare. Feed the result into population_plot*'s
#      lon_limits/lat_limits.
#' @param method chr one of:
#'   "auto_fires" (default) - bbox of the fire perimeters padded by margin_km (always includes the fires);
#'   "fixed"      - a fixed Spokane box (fixed_lon/fixed_lat);
#'   "movement"   - reuse movement_plot2()'s operational box (movement_lon/movement_lat).
#' @param fires    sf  fire perimeters (needed for "auto_fires"; falls back to "fixed" if empty).
#' @param margin_km num padding around the fire bbox for "auto_fires" (default 15).
#' @param fixed_lon/@param fixed_lat num the "fixed" box (default ~75x89 km around Spokane).
#' @param movement_lon/@param movement_lat num the "movement" box (matches movement_plot2 defaults).
#' @return list(lon = c(min,max), lat = c(min,max), method = chr).
#' @sideeffects none.
#' @examples ex <- study_extent("auto_fires", fires = fires)
#'           population_plot_z_score(latest_ds, latest_hour, lon_limits = ex$lon, lat_limits = ex$lat)
# ---------------------------------------------------------------------------
study_extent <- function(method = c("auto_fires", "fixed", "movement"),
                         fires = NULL, margin_km = 15,
                         fixed_lon = c(-118.0, -117.0), fixed_lat = c(47.3, 48.1),
                         movement_lon = c(-117.9, -117.0), movement_lat = c(47.4, 47.9)) {
  method <- match.arg(method)
  if (method == "movement") return(list(lon = movement_lon, lat = movement_lat, method = method))
  if (method == "fixed")    return(list(lon = fixed_lon,    lat = fixed_lat,    method = method))
  # "auto_fires": pad the fire-perimeter bbox by margin_km. Degrees-per-km differs
  # by axis (lat is constant ~111 km/deg; lon shrinks by cos(latitude)).
  if (is.null(fires) || !inherits(fires, "sf") || nrow(fires) == 0) {
    warning("study_extent('auto_fires'): no fire layer; falling back to the fixed Spokane box.")
    return(list(lon = fixed_lon, lat = fixed_lat, method = "fixed(fallback)"))
  }
  bb      <- sf::st_bbox(sf::st_transform(fires, 4326))
  lat_mid <- mean(c(bb[["ymin"]], bb[["ymax"]]))
  dlat    <- margin_km / 111
  dlon    <- margin_km / (111 * cos(lat_mid * pi / 180))
  list(lon = unname(c(bb[["xmin"]] - dlon, bb[["xmax"]] + dlon)),
       lat = unname(c(bb[["ymin"]] - dlat, bb[["ymax"]] + dlat)),
       method = method)
}

# ---------------------------------------------------------------------------
# focus_on_fires() — frame on ONE cluster of perimeters (e.g. the incident near
# Spokane), not all of `fires`.
# WHY: fetch_fires() returns EVERY WFIGS perimeter within ~100 km of Spokane, so
#      study_extent("auto_fires") frames the whole spread (incl. the Lake Roosevelt
#      fires ~55 km NW). This keeps only perimeters within `within_km` of a center
#      point and pads their bbox — so every map centers on the fires you care about.
#      Returns the SAME shape as study_extent(), so it drops straight into any
#      plot's lon_limits/lat_limits and into plot_extent().
#' @param fires     sf  fire perimeters (the global `fires` from fetch_fires()).
#' @param center    num c(lon, lat) to focus around; default = the Spokane incident
#'                  center (same point fetch_fires() queries around).
#' @param within_km num keep perimeters whose nearest point is within this many km
#'                  of `center` (default 20; raise to include more, lower to tighten).
#' @param margin_km num padding around the kept-fires bbox (default 8).
#' @return list(lon = c(min,max), lat = c(min,max), method = "focus_fires",
#'         fires = <the kept perimeters, as sf>). Feed $lon/$lat to any plot.
#' @sideeffects none.
#' @examples focus <- focus_on_fires(fires, within_km = 20)
#'           plot_extent(focus, fires)                       # preview the frame
#'           population_plot_z_score(w$latest_ds, w$latest_hour,
#'                                   lon_limits = focus$lon, lat_limits = focus$lat)
# ---------------------------------------------------------------------------
focus_on_fires <- function(fires,
                           center = c(lon = -117.4260, lat = 47.6588),
                           within_km = 20, margin_km = 8) {
  stopifnot("focus_on_fires(): `fires` must be a non-empty sf" =
              inherits(fires, "sf") && nrow(fires) > 0)
  # accept center as c(lon=, lat=) OR positional c(lon, lat)
  clon <- if (!is.null(names(center)) && "lon" %in% names(center)) center[["lon"]] else center[[1]]
  clat <- if (!is.null(names(center)) && "lat" %in% names(center)) center[["lat"]] else center[[2]]

  # WFIGS perimeters are often topologically invalid (duplicate vertices). In
  # lon/lat sf uses s2, which REJECTS those ("Loop N is not valid"). So work in a
  # projected CRS (distance then runs on GEOS, which is tolerant) and st_make_valid()
  # first. UTM (auto-picked zone) keeps planar metres ~= true ground metres, so
  # `within_km` stays real kilometres.
  utm_epsg <- (if (clat >= 0) 32600 else 32700) + (floor((clon + 180) / 6) + 1)
  f  <- sf::st_make_valid(sf::st_transform(fires, utm_epsg))
  pt <- sf::st_transform(sf::st_sfc(sf::st_point(c(clon, clat)), crs = 4326), utm_epsg)
  d_km <- as.numeric(sf::st_distance(f, pt)) / 1000     # GEOS planar km ~= true km
  keep <- sf::st_transform(f[d_km <= within_km, , drop = FALSE], 4326)  # back to lon/lat for bbox/plot
  if (nrow(keep) == 0)
    stop("focus_on_fires(): no perimeters within ", within_km, " km of (", clon, ", ",
         clat, "). Widen `within_km` or move `center`. Nearest is ",
         round(min(d_km), 1), " km.")

  # pad the kept-fires bbox by margin_km (deg-per-km differs by axis: lat ~111 km/deg,
  # lon shrinks by cos(latitude)).
  bb      <- sf::st_bbox(keep)
  lat_mid <- mean(c(bb[["ymin"]], bb[["ymax"]]))
  dlat    <- margin_km / 111
  dlon    <- margin_km / (111 * cos(lat_mid * pi / 180))
  list(lon    = unname(c(bb[["xmin"]] - dlon, bb[["xmax"]] + dlon)),
       lat    = unname(c(bb[["ymin"]] - dlat, bb[["ymax"]] + dlat)),
       method = "focus_fires",
       fires  = keep)
}

# ---------------------------------------------------------------------------
# ensure_users_col() — guarantee a `# Users During Crisis` column on movement rows.
# WHY: movement_plot2()/aggregate_flows() use the friendly name that build_moved()
#      assigns, but a stale/raw `moved` (e.g. a moved.Rds built before that rename)
#      may still carry the raw `n_crisis`. Use the friendly name if present; else
#      rename n_crisis to it; else stop with a clear message.
#' @param df   data.frame/sf movement rows.
#' @param what chr label used in the error message (default "movement data").
#' @return df with a `# Users During Crisis` column.
#' @sideeffects none.
#' @examples ensure_users_col(dplyr::filter(moved, ds == "2026-08-03", hour == "0800"))
# ---------------------------------------------------------------------------
ensure_users_col <- function(df, what = "movement data") {
  target <- "# Users During Crisis"
  if (target %in% names(df)) return(df)
  if ("n_crisis" %in% names(df)) {
    message("ensure_users_col(): renaming n_crisis -> `# Users During Crisis` in ", what,
            " (stale/raw movement data).")
    return(dplyr::rename(df, `# Users During Crisis` = n_crisis))
  }
  stop("ensure_users_col(): no users column in ", what,
       " - expected `# Users During Crisis` or `n_crisis`. Present: ",
       paste(names(df), collapse = ", "))
}

# Canonical wide "Spokane area" box (the report's original extent). The notebook
# sets a runtime global `spokane_extent` from this and reuses it everywhere;
# tighten via study_extent(). Shape matches study_extent(): list(lon, lat).
SPOKANE_EXTENT <- list(lon = c(-118.5, -115.5), lat = c(47, 49))

# ---------------------------------------------------------------------------
# plot_extent() — draw a map-extent box on a basemap (context / sanity check).
# WHY: makes an abstract lon/lat box concrete - see exactly what area a map will
#      cover, and how it sits relative to the fires, before rendering/saving.
#' @param extent     list(lon=c(min,max), lat=c(min,max)) - e.g. spokane_extent or study_extent(...).
#' @param fires      sf optional fire perimeters drawn (red) for context.
#' @param pad_factor num context shown around the box (0.6 = 60% padding each side).
#' @param zoom       int basemap zoom for the context view (default 8).
#' @return ggplot object.
#' @sideeffects may fetch/cache a basemap.
#' @examples plot_extent(spokane_extent, fires)
# ---------------------------------------------------------------------------
plot_extent <- function(extent, fires = NULL, pad_factor = 0.6, zoom = 8) {
  stopifnot(is.list(extent), !is.null(extent$lon), !is.null(extent$lat))
  padlon <- diff(range(extent$lon)) * pad_factor
  padlat <- diff(range(extent$lat)) * pad_factor
  view <- c(xmin = min(extent$lon) - padlon, xmax = max(extent$lon) + padlon,
            ymin = min(extent$lat) - padlat, ymax = max(extent$lat) + padlat)
  # the box itself, as an sf rectangle
  rect <- sf::st_as_sfc(sf::st_bbox(c(xmin = min(extent$lon), xmax = max(extent$lon),
                                      ymin = min(extent$lat), ymax = max(extent$lat)),
                                    crs = sf::st_crs(4326)))
  # everything in EPSG:3857 (like the maps) to avoid coord_sf lon/lat-vs-metres issues
  view_bb <- sf::st_bbox(sf::st_transform(
    sf::st_as_sfc(sf::st_bbox(view, crs = sf::st_crs(4326))), 3857))
  zoom <- fit_zoom(c(view[["xmin"]], view[["xmax"]]), c(view[["ymin"]], view[["ymax"]]), zoom)  # OOM guard
  osm <- tryCatch(
    get_tiles(sf::st_as_sf(data.frame(lon = c(view[["xmin"]], view[["xmax"]]),
                                      lat = c(view[["ymin"]], view[["ymax"]])),
                           coords = c("lon", "lat"), crs = 4326),
              provider = "CartoDB.Positron", crop = TRUE, zoom = zoom, cachedir = tile_cache_dir),
    error = function(e) { warning("Basemap download failed; drawing box without it."); NULL })
  p <- ggplot()
  if (!is.null(osm)) p <- p + layer_spatial(osm)
  if (!is.null(fires) && inherits(fires, "sf") && nrow(fires) > 0)
    p <- p + geom_sf(data = fires, fill = NA, color = "darkred", linewidth = 0.4)
  p +
    geom_sf(data = rect, fill = "blue", alpha = 0.12, color = "blue", linewidth = 1) +
    coord_sf(crs = sf::st_crs(3857),
             xlim = c(view_bb[["xmin"]], view_bb[["xmax"]]),
             ylim = c(view_bb[["ymin"]], view_bb[["ymax"]]), expand = FALSE) +
    labs(title = sprintf("Map extent: lon [%.3f, %.3f], lat [%.3f, %.3f]",
                         min(extent$lon), max(extent$lon), min(extent$lat), max(extent$lat))) +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
}

# ---------------------------------------------------------------------------
# save_pop_map() — build a population map at a chosen EXTENT + SIZE/RESOLUTION and
# save it as a PNG (also returns the plot so it previews in a Colab cell).
# WHY: one call that exposes the levers that matter for legibility - extent (how
#      zoomed in), zoom (basemap tile detail), and width/height/dpi (output pixels
#      = inches x dpi). See the notebook notes on pairing these.
#' @param metric    chr "difference" / "crisis" / "zscore".
#' @param ds        chr window date "YYYY-MM-DD"; @param hour chr "0000"/"0800"/"1600".
#' @param file      chr output filename (written under dir).
#' @param extent    list(lon,lat) - e.g. spokane_extent or study_extent("auto_fires", fires).
#' @param zoom      int basemap tile detail (pair with extent; see notebook notes).
#' @param show_evac lgl overlay evac zones if available (default FALSE).
#' @param width     num inches; @param height num inches; @param dpi int -> px = inches * dpi.
#' @param dir       chr output directory (default "outputs").
#' @return the ggplot (also saved to dir/file as a side effect).
#' @sideeffects writes a PNG; reads globals tiles_3857/fires; may fetch/cache a basemap.
#' @examples save_pop_map("zscore", latest_ds, latest_hour, "z.png",
#'                        extent = study_extent("auto_fires", fires), zoom = 12, width = 10, dpi = 300)
# ---------------------------------------------------------------------------
save_pop_map <- function(metric, ds, hour, file,
                         extent = SPOKANE_EXTENT, zoom = 11, show_evac = FALSE,
                         width = 8, height = 8, dpi = 300, dir = "outputs") {
  stopifnot(is.list(extent), !is.null(extent$lon), !is.null(extent$lat))
  p <- population_plot(ds, hour, metric = metric,
                       lon_limits = extent$lon, lat_limits = extent$lat,
                       zoom = zoom, show_evac = show_evac)
  save_plot(p, file, dir = dir, width = width, height = height, dpi = dpi)
  p
}

# ---------------------------------------------------------------------------
# all_windows() — every distinct (ds, hour) time frame in a dataset, chronological.
# WHY: enumerate all time frames so animations/loops cover the full record.
#' @param df data.frame/sf with `ds` and `hour` columns (e.g. tiles_3857 or moved).
#' @return tibble of distinct ds (chr) + hour, sorted.
#' @sideeffects none.
#' @examples all_windows(tiles_3857); all_windows(moved)
# ---------------------------------------------------------------------------
all_windows <- function(df) {
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  df |>
    dplyr::distinct(ds, hour) |>
    dplyr::mutate(ds = as.character(ds)) |>
    dplyr::arrange(ds, hour)
}

# ---------------------------------------------------------------------------
# animate_population() — GIF cycling every population window (see report maps).
# WHY: one animation shows evacuation/return over time, not just two snapshots.
#      metric = "zscore" is the default because its scale is fixed at +/-4, so the
#      colours mean the same thing in every frame (difference/crisis rescale per
#      window, which makes a misleading animation).
#' @param metric   chr "zscore" (recommended) / "difference" / "crisis".
#' @param extent   list(lon,lat) map box (default SPOKANE_EXTENT; or study_extent(...)).
#' @param zoom     int basemap tile detail (pair with extent; default 9 for the wide box).
#' @param out      chr output GIF path (default outputs/pop_<metric>.gif).
#' @param fps      num frames per second (default 2); width/height in PIXELS.
#' @param show_evac lgl overlay evac zones if available.
#' @return the GIF path (written to disk).
#' @sideeffects writes a .gif; reads globals tiles_3857/fires; fetches/caches basemaps.
#' @examples animate_population("zscore", extent = spokane_extent, zoom = 9)
# ---------------------------------------------------------------------------
animate_population <- function(metric = "zscore", extent = SPOKANE_EXTENT, zoom = 9,
                               out = NULL, fps = 2, width = 1400, height = 1400,
                               show_evac = FALSE) {
  if (!requireNamespace("gifski", quietly = TRUE))
    stop("animate_population(): install 'gifski' first (install.packages('gifski')).")
  if (is.null(out)) out <- file.path("outputs", paste0("pop_", metric, ".gif"))
  if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
  wins <- all_windows(tiles_3857)
  message("animate_population(): ", nrow(wins), " frames -> ", out)
  gifski::save_gif(
    for (i in seq_len(nrow(wins))) {
      # tryCatch so a sparse/empty window becomes a titled blank frame, not a crash
      p <- tryCatch(
        population_plot(wins$ds[i], wins$hour[i], metric = metric,
                        lon_limits = extent$lon, lat_limits = extent$lat,
                        zoom = zoom, show_evac = show_evac),
        error = function(e) ggplot() +
          labs(title = paste(wins$ds[i], wins$hour[i], "- no data")) + theme_void())
      print(p)
    },
    gif_file = out, width = width, height = height, delay = 1 / fps, progress = TRUE)
  message("saved ", out); out
}

# ---------------------------------------------------------------------------
# animate_movement() — GIF cycling every movement window (0800/1600 only).
# WHY: shows how evacuation flows evolve window to window. color_by = "zscore"
#      keeps the colour scale fixed (+/-4) across frames.
#' @param direction chr "all"/"egress"/"intake" (see movement_plot2).
#' @param color_by  chr "zscore"/"users"/"none".
#' @param focus_lon/@param focus_lat num hard-clip box (default Spokane metro).
#' @param min_users num drop flows below this (default 15). @param zoom int (default 11).
#' @param out chr GIF path. @param fps num fps. @param width/@param height px.
#' @return the GIF path.
#' @sideeffects writes a .gif; reads globals moved/fires; fetches/caches basemaps.
#' @examples animate_movement(direction = "egress", color_by = "zscore")
# ---------------------------------------------------------------------------
animate_movement <- function(direction = "all", color_by = "zscore",
                             focus_lon = c(-117.9, -117.0), focus_lat = c(47.4, 47.9),
                             min_users = 15, zoom = 11, out = "outputs/movement.gif",
                             fps = 2, width = 1500, height = 1300) {
  if (!requireNamespace("gifski", quietly = TRUE))
    stop("animate_movement(): install 'gifski' first (install.packages('gifski')).")
  if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
  wins <- dplyr::filter(all_windows(moved), hour %in% c("0800", "1600"))
  message("animate_movement(): ", nrow(wins), " frames -> ", out)
  gifski::save_gif(
    for (i in seq_len(nrow(wins))) {
      p <- tryCatch(
        movement_plot2(wins$ds[i], wins$hour[i],
                       focus_lon = focus_lon, focus_lat = focus_lat,
                       direction = direction, color_by = color_by,
                       min_users = min_users, zoom = zoom,
                       plot_title = paste("Movement", wins$ds[i], wins$hour[i],
                                          if (direction == "all") "" else direction)),
        error = function(e) ggplot() +
          labs(title = paste("Movement", wins$ds[i], wins$hour[i], "- no flows")) + theme_void())
      print(p)
    },
    gif_file = out, width = width, height = height, delay = 1 / fps, progress = TRUE)
  message("saved ", out); out
}

# ---------------------------------------------------------------------------
# animate_movement3() — GIF cycling every 0800/1600 window with the v3 styling.
# WHY: shows how evacuation flows evolve window to window. Uses fit = "fixed" on
#      purpose — a data-driven box would jump frame to frame, so animations pin the
#      extent while single figures use fit = "flows". color_by = "zscore" keeps the
#      colour scale fixed (+/-4) so a colour means the same thing in every frame.
#' @inheritParams movement_plot3
#' @param out chr GIF path (default outputs/movement_v3.gif).
#' @param fps num frames per second (default 2); width/height in PIXELS.
#' @param ... further args forwarded to movement_plot3() (halo, basemap_fade, ...).
#' @return the GIF path.
#' @sideeffects writes a .gif; reads globals moved/fires; fetches/caches basemaps.
#' @examples animate_movement3(direction = "egress", color_by = "zscore")
# ---------------------------------------------------------------------------
animate_movement3 <- function(direction = "all", color_by = "zscore",
                              focus_lon = c(-117.9, -117.0), focus_lat = c(47.4, 47.9),
                              min_users = 15, zoom = 11, out = "outputs/movement_v3.gif",
                              fps = 2, width = 1500, height = 1300, ...) {
  if (!requireNamespace("gifski", quietly = TRUE))
    stop("animate_movement3(): install 'gifski' first (install.packages('gifski')).")
  if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
  wins <- dplyr::filter(all_windows(moved), hour %in% c("0800", "1600"))
  message("animate_movement3(): ", nrow(wins), " frames -> ", out)
  gifski::save_gif(
    for (i in seq_len(nrow(wins))) {
      # fit = "fixed" pins the frame; tryCatch turns an empty window into a titled
      # blank frame instead of aborting the whole GIF.
      p <- tryCatch(
        movement_plot3(wins$ds[i], wins$hour[i], direction = direction, color_by = color_by,
                       fit = "fixed", focus_lon = focus_lon, focus_lat = focus_lat,
                       min_users = min_users, zoom = zoom,
                       plot_title = paste("Movement", wins$ds[i], wins$hour[i],
                                          if (direction == "all") "" else direction), ...),
        error = function(e) ggplot() +
          labs(title = paste("Movement", wins$ds[i], wins$hour[i], "- no flows")) + theme_void())
      print(p)
    },
    gif_file = out, width = width, height = height, delay = 1 / fps, progress = TRUE)
  message("saved ", out); out
}


# Quick manual smoke test (set test_it <- TRUE to run interactively).
# NOTE: run the pipeline steps first so the globals exist, e.g.:
#   fb_data_bing <- load_population(); tiles_3857 <- build_tiles(fb_data_bing)
#   fires <- fetch_fires(); w <- compute_windows(fb_data_bing)
# then use w$first_ds / w$first_hour (a window that exists in the data).
# ============================================================================
# EVAC-OVER-TIME + batch rendering + contact sheet
# WHY: a begin/end pair can't show evacuation orders LOOSENING — you need the whole
#      arc. These render a metric across EVERY time window into a fresh per-run
#      folder, build one glanceable contact sheet, and (report_evac_figs) auto-pick
#      the handful of frames worth putting in the report — so you stop choosing.
# ============================================================================

# Resolve a legible lon/lat frame: an explicit `extent`, else the notebook's
# `spokane_extent`, else the wide default. Feed focus_on_fires(fires) here to
# center every frame on the incident.
.default_extent <- function(extent = NULL) {
  if (!is.null(extent)) return(extent)
  if (exists("spokane_extent", inherits = TRUE)) return(get("spokane_extent"))
  SPOKANE_EXTENT
}

# ---------------------------------------------------------------------------
# render_series() — render ONE map type across MANY windows -> a fresh per-run dir.
# WHY: the "make a ton of plots at once, saved to a dir per run" workhorse; the
#      default (what="zscore") IS the evac-over-time series (standardized change,
#      comparable across days), framed on `extent` and saved chronologically.
#' @param what   chr population metric ("zscore"/"difference"/"crisis") or movement
#'               ("egress"/"intake"/"allflows"). Movement uses only 0800/1600 windows.
#' @param extent list(lon=c(min,max), lat=c(min,max)); default .default_extent().
#' @param windows data.frame(ds,hour) to render; NULL = every window present.
#' @param show_evac lgl overlay evac zones on population maps (if a layer exists).
#' @param out_root chr parent dir; tag chr optional run-name prefix.
#' @param montage lgl also build a contact sheet of the run (needs `magick`).
#' @return invisibly list(dir, manifest); writes PNGs + manifest.csv to the run dir.
#' @sideeffects creates outputs/<tag_>what_<timestamp>/ and writes images.
#' @examples render_series("zscore", extent = focus_on_fires(fires))
# ---------------------------------------------------------------------------
render_series <- function(what = c("zscore","difference","crisis","egress","intake","allflows"),
                          extent = NULL, windows = NULL, show_evac = TRUE,
                          out_root = "outputs", tag = "", zoom = 9,
                          width = 9, height = 9, dpi = 200, montage = TRUE,
                          show_counties = FALSE, counties = NULL,
                          county_lines = c("interior", "full"),
                          show_waypoints = FALSE, waypoints = NULL,
                          show_scalebar = FALSE) {
  what  <- match.arg(what)
  is_mv <- what %in% c("egress","intake","allflows")
  ext   <- .default_extent(extent)

  if (is.null(windows)) {
    if (is_mv) {
      stopifnot("render_series(): `moved` not loaded." = exists("moved"))
      windows <- dplyr::arrange(dplyr::distinct(moved, ds, hour), ds, hour)
      windows <- windows[windows$hour %in% c("0800","1600"), , drop = FALSE]   # movement cadence
    } else {
      src <- if (exists("tiles_3857")) sf::st_drop_geometry(tiles_3857) else
             if (exists("fb_data_bing")) fb_data_bing else
             stop("render_series(): need tiles_3857 or fb_data_bing loaded.")
      windows <- dplyr::arrange(dplyr::distinct(src, ds, hour), ds, hour)
    }
  }
  stamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  run_dir <- file.path(out_root, sprintf("%s%s_%s", if (nzchar(tag)) paste0(tag, "_") else "", what, stamp))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  message("render_series(", what, "): ", nrow(windows), " frames -> ", run_dir)

  rows <- list()
  for (i in seq_len(nrow(windows))) {
    ds <- as.character(windows$ds[i]); hr <- as.character(windows$hour[i])
    p <- tryCatch(
      if (is_mv) movement_plot2(ds, hr,
                   direction = switch(what, egress = "egress", intake = "intake", allflows = "all"),
                   color_by = "zscore", focus_lon = ext$lon, focus_lat = ext$lat)
      else       population_plot(ds, hr, metric = what, lon_limits = ext$lon, lat_limits = ext$lat,
                   show_evac = show_evac, zoom = zoom,
                   show_counties = show_counties, counties = counties,
                   county_lines = county_lines,
                   show_waypoints = show_waypoints, waypoints = waypoints,
                   show_scalebar = show_scalebar),
      error = function(e) { message("  skip ", ds, " ", hr, ": ", conditionMessage(e)); NULL })
    if (is.null(p)) next
    fn <- sprintf("%02d_%s_%s_%s.png", i, what, ds, hr)   # NN_ prefix keeps chronological order
    ggplot2::ggsave(file.path(run_dir, fn), p, width = width, height = height, dpi = dpi)
    rows[[length(rows) + 1]] <- data.frame(idx = i, file = fn, ds = ds, hour = hr, stringsAsFactors = FALSE)
    message("  saved ", fn)
  }
  man <- if (length(rows)) do.call(rbind, rows) else data.frame()
  utils::write.csv(man, file.path(run_dir, "manifest.csv"), row.names = FALSE)
  if (montage && nrow(man)) try(contact_sheet(run_dir), silent = TRUE)
  message("DONE: ", nrow(man), " images in ", run_dir)
  invisible(list(dir = run_dir, manifest = man))
}

# ---------------------------------------------------------------------------
# contact_sheet() — one glanceable grid of every PNG in a dir (pick fast).
# WHY: kills the "50 images, which do I use?" problem — see them all at once.
#      Uses base `grid` + the tiny `png` package (libpng, always present) — NO
#      ImageMagick, so no `libMagick++.so` system-dependency headaches.
#' @param dir chr folder of PNGs; cols int grid columns; out chr output path;
#'        thumb int per-cell pixel size.
#' @return invisibly the sheet path.
# ---------------------------------------------------------------------------
contact_sheet <- function(dir, cols = 4, out = file.path(dir, "_contact_sheet.png"), thumb = 400) {
  imgs <- sort(list.files(dir, pattern = "\\.png$", full.names = TRUE))
  imgs <- imgs[!grepl("_contact_sheet", imgs)]
  if (!length(imgs)) { message("contact_sheet(): no PNGs in ", dir); return(invisible(NULL)) }
  if (!requireNamespace("png", quietly = TRUE))
    install.packages("png", repos = "https://cloud.r-project.org")
  n <- length(imgs); rows <- ceiling(n / cols)
  grDevices::png(out, width = cols * thumb, height = rows * thumb, res = 72)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(rows, cols)))
  for (i in seq_along(imgs)) {
    r  <- ((i - 1) %/% cols) + 1L; cc <- ((i - 1) %% cols) + 1L
    img <- tryCatch(png::readPNG(imgs[i]), error = function(e) NULL)
    grid::pushViewport(grid::viewport(layout.pos.row = r, layout.pos.col = cc))
    if (!is.null(img)) grid::grid.raster(img, interpolate = FALSE)
    grid::grid.text(basename(imgs[i]), y = grid::unit(1, "npc") - grid::unit(1.5, "mm"),
                    gp = grid::gpar(cex = 0.5), just = "top")
    grid::popViewport()
  }
  message("contact sheet -> ", out, " (", n, " frames)")
  invisible(out)
}

# ---------------------------------------------------------------------------
# report_evac_figs() — the "just finish" button: the report's evac figure set.
# WHY: removes the choosing. Auto-picks EARLY / PEAK-displacement / LATEST windows
#      (peak = the window with the deepest net population deficit near the fires)
#      + the trend curve, and writes exactly those 4 with clean names. Use these.
#' @param extent  list(lon,lat); default .default_extent(). out chr output dir.
#' @param show_evac lgl overlay evac zones if a layer exists.
#' @return invisibly the picked windows; writes 4 PNGs (+ a contact sheet) to `out`.
#' @examples report_evac_figs(extent = focus_on_fires(fires))
# ---------------------------------------------------------------------------
report_evac_figs <- function(extent = NULL, out = "outputs/report_evac", show_evac = TRUE) {
  stopifnot("need fb_data_bing" = exists("fb_data_bing"),
            "need tiles_3857"   = exists("tiles_3857"),
            "need fires"        = exists("fires"))
  ext <- .default_extent(extent)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  w <- dplyr::arrange(dplyr::distinct(fb_data_bing, ds, hour), ds, hour)
  # peak displacement = window with the most-negative net (crisis - baseline) in-frame
  net <- vapply(seq_len(nrow(w)), function(i) {
    d <- dplyr::filter(fb_data_bing, ds == w$ds[i], hour == w$hour[i],
                       dplyr::between(longitude, ext$lon[1], ext$lon[2]),
                       dplyr::between(latitude,  ext$lat[1], ext$lat[2]))
    sum(pmin(0, d$n_crisis - d$n_baseline), na.rm = TRUE)   # depth of the population "hole"
  }, numeric(1))
  pick <- list(early = w[1, ], peak = w[which.min(net), ], latest = w[nrow(w), ])

  for (nm in names(pick)) {
    ds <- as.character(pick[[nm]]$ds); hr <- as.character(pick[[nm]]$hour)
    p  <- population_plot(ds, hr, metric = "zscore", lon_limits = ext$lon, lat_limits = ext$lat,
                          show_evac = show_evac)
    ggplot2::ggsave(file.path(out, sprintf("fig_evac_%s_%s_%s.png", nm, ds, hr)),
                    p, width = 8, height = 8, dpi = 300)
  }
  tc <- population_timeseries(fb_data_bing, fires, buffer_km = 15, mode = "anomaly")
  ggplot2::ggsave(file.path(out, "fig_evac_trend.png"), tc, width = 9, height = 5, dpi = 300)

  message("report evac set -> ", out, "  (early=", pick$early$ds, "; peak=", pick$peak$ds, " ",
          pick$peak$hour, "; latest=", pick$latest$ds, ")")
  try(contact_sheet(out, cols = 2), silent = TRUE)
  invisible(pick)
}

test_it <- FALSE
if (test_it) {
  fb_data_bing <- load_population()
  tiles_3857   <- build_tiles(fb_data_bing)
  fires        <- fetch_fires()
  w            <- compute_windows(fb_data_bing)
  population_plot_n_crisis(
    w$first_ds, w$first_hour,
    lon_limits = c(-118.5, -115.5),
    lat_limits = c(47, 49)
  )
}


