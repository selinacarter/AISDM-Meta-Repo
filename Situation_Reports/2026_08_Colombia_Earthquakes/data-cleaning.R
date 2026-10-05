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
here::i_am("data-cleaning.R")

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

format_time <- function(ds, hour) {
  datetime <- ymd_hm(
    paste(ds, hour),
    tz = "America/Los_Angeles"
  )
  
  datetime_colombia <- with_tz(
    datetime,
    tzone = "America/Bogota"
  )
  
  formatted <- format(
    datetime_colombia,
    "%B %d, %Y %I%p"
  )
  
  
  formatted <- gsub(" 0", " ", formatted)
  formatted <- gsub("AM", "am", formatted)
  formatted <- gsub("PM", "pm", formatted)
  
  formatted
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


shared_limits <- function(col, symmetric = FALSE,
                          ds1 = first_ds, hour1 = first_hour,
                          ds2 = latest_ds, hour2 = latest_hour) {
  if (!col %in% names(tiles_3857)) return(NULL)
  v <- tiles_3857 |>
    dplyr::filter((ds == ds1 & hour == hour1) | (ds == ds2 & hour == hour2),
                  dplyr::between(longitude, lon_limits[1], lon_limits[2]),
                  dplyr::between(latitude,  lat_limits[1], lat_limits[2])) |>
    dplyr::pull(.data[[col]])
  v <- v[is.finite(v)]
  if (!length(v)) return(NULL)
  r <- range(v, na.rm = TRUE)
  if (symmetric) { m <- max(abs(r)); c(-m, m) } else r
}

shared_eq_limits <- function(
    lon_limits,
    lat_limits,
    start_date = Sys.Date() - 17,
    end_date = Sys.Date(),
    min_magnitude = 2.5
) {
  usgs_url <- paste0(
    "https://earthquake.usgs.gov/fdsnws/event/1/query?",
    "format=geojson",
    "&starttime=", start_date,
    "&endtime=", end_date,
    "&minlatitude=", lat_limits[1],
    "&maxlatitude=", lat_limits[2],
    "&minlongitude=", lon_limits[1],
    "&maxlongitude=", lon_limits[2],
    "&minmagnitude=", min_magnitude
  )
  
  earthquakes <- st_read(usgs_url, quiet = TRUE)
  
  if (!nrow(earthquakes)) return(NULL)
  
  range(earthquakes$mag, na.rm = TRUE)
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
    fill_limits = NULL,  # force the fill scale limits (difference/crisis) so two panels can SHARE
    eq_limits = NULL,
    labels = NULL,
    label_angles = NULL
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

  if (!is.null(lon_limits)){
        first_time_pop = first_time_pop |>
          filter(lon_limits[1] <= longitude,  longitude <= lon_limits[2]) |>
          filter(lat_limits[1] <= latitude,  latitude <= lat_limits[2])

        lims <- range(first_time_pop[[fill_col]], na.rm = TRUE)

        pts <- rbind(
          data.frame(lon = lon_limits[1], lat = lat_limits[1]),
          data.frame(lon = lon_limits[2], lat = lat_limits[2])
        )
        pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
        

        # Download OpenStreetMap tiles
        osm <- get_tiles(
          pts_sf,
          provider = "CartoDB.Voyager",
          crop = TRUE,
          zoom = zoom
        )
        
        cutoff1 <- as.POSIXct(
          paste0("2026-08-09", " ", substr("0000", 1, 2), ":", substr("0000", 3, 4)),
          format = "%Y-%m-%d %H:%M",
          tz = "America/Los_Angeles"
        )
        
        cutoff2 <- as.POSIXct(
          paste0(plot_ds, " ", substr(plot_hour, 1, 2), ":", substr(plot_hour, 3, 4)),
          format = "%Y-%m-%d %H:%M",
          tz = "America/Los_Angeles"
        )

        earthquakes <- earthquakes |>
          filter(
            datetime_pst >= cutoff1,
            datetime_pst <= cutoff2
          )

      }


      # Default: no zoom
      xlim <- NULL
      ylim <- NULL

      # Convert lon/lat limits to EPSG:3857
      if (!is.null(lon_limits) && !is.null(lat_limits)) {

        bbox_ll <- st_as_sfc(
          st_bbox(
            c(
              xmin = lon_limits[1],
              xmax = lon_limits[2],
              ymin = lat_limits[1],
              ymax = lat_limits[2]
            ),
            crs = st_crs(4326)
          )
        )

        bbox_3857 <- st_transform(bbox_ll, 3857) |>
          st_bbox()

        xlim <- c(bbox_3857["xmin"], bbox_3857["xmax"])
        ylim <- c(bbox_3857["ymin"], bbox_3857["ymax"])
      }

      map_bbox <- st_as_sfc(
        st_bbox(
          c(
            xmin = lon_limits[1],
            xmax = lon_limits[2],
            ymin = lat_limits[1],
            ymax = lat_limits[2]
          ),
          crs = 4326
        )
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
                         name = "Users (crisis - baseline)")
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
  
  p1 <- ggplot() +
  layer_spatial(osm) +
    geom_sf(
      data = first_time_pop,
      aes(fill = .data[[fill_col]]),
      color = "white",
      linewidth = 0.1,
      alpha = 1
    ) +
    # Earthquakes
    geom_sf(
      data = earthquakes,
      aes(color = mag),
      size = 5,
      alpha = 0.8
    ) +
    geom_sf_text(
      data = earthquakes,
      aes(label = eq_order),
      color = "black",
      fontface = "bold",
      size = 3
    ) +
    scale_color_gradient(
      low = "gold",
      high = "red",
      limits = eq_limits,
      breaks = pretty(eq_limits, n = 5),
      oob = scales::squish,
      name = "Magnitude"
    ) + 
    fill_scale +
    coord_sf(
      crs = st_crs(3857),
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    )
  
  p1 <- p1 +
    guides(
      # title.position = "top" centers the title ABOVE the bar so it no longer
      # clips at the left edge of a narrow (2-up) panel.
      fill = guide_colorbar(direction = "horizontal", order = 1, title.position = "top"),
      color = guide_legend(direction = "horizontal", order = 2)
    )
  
  if (!is.null(labels)) {
    
    # `labels` is already in EPSG:3857
    coords <- st_coordinates(labels)
    
    label_xy <- labels |>
      st_drop_geometry() |>
      mutate(
        x = coords[, 1],
        y = coords[, 2]
      )
    
    # ------------------------------------------------------------
    # City-specific arrow angles
    #
    # label_angles should be a named vector, e.g.
    #
    # label_angles <- c(
    #   "Los Angeles" = 20,
    #   "San Diego" = -35,
    #   "San Francisco" = 90
    # )
    #
    # Angle convention:
    #   0   = straight up
    #   90  = right
    #   -90 = left
    #   180 = straight down
    # ------------------------------------------------------------
    
    if (is.null(label_angles)) {
      label_angles <- setNames(
        rep(0, nrow(label_xy)),
        label_xy$label
      )
    }
    
    # Match angle to each city
    label_xy$angle <- unname(label_angles[label_xy$label])
    
    # Default to straight up if a city does not have an angle
    label_xy$angle[is.na(label_xy$angle)] <- 0
    
    # Arrow/label distance
    arrow_length <- 80000
    
    # Convert angle from degrees to radians.
    #
    # 0 degrees = up
    # Positive = clockwise/right
    # Negative = counter-clockwise/left
    angle_rad <- label_xy$angle * pi / 180
    
    # Calculate label position from city position
    label_xy <- label_xy |>
      mutate(
        label_x = x + arrow_length * sin(angle_rad),
        label_y = y + arrow_length * cos(angle_rad)
      )
    
    # ------------------------------------------------------------
    # Arrow
    # ------------------------------------------------------------
    
    p1 <- p1 +
      geom_segment(
        data = label_xy,
        aes(
          x = label_x,
          y = label_y,
          xend = x,
          yend = y
        ),
        color = "black",
        linewidth = 1.2,
        arrow = grid::arrow(
          length = grid::unit(0.35, "cm"),
          type = "closed"
        )
      ) +
      
      # ----------------------------------------------------------
    # City label
    # ----------------------------------------------------------
    
    geom_label(
      data = label_xy,
      aes(
        x = label_x,
        y = label_y,
        label = label
      ),
      color = "black",
      fill = "white",
      size = 4,
      fontface = "bold",
      label.size = 0.3
    )
  }
  
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
      legend.position = "bottom",
      legend.box = "vertical",         # stack the fill colorbar and the outline legend
      legend.direction = "horizontal", # each legend laid out horizontally
      legend.key.width = unit(1.4, "cm")
    )
}

####----- LOADING DATA -----

fb_data_bing <- load_population()
tiles_3857   <- build_tiles(fb_data_bing)
w <- compute_windows(fb_data_bing)
attach(w)
mp_data_bing <- load_movement()
moved <- build_moved(mp_data_bing)
