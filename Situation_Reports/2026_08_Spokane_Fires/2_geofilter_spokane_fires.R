# ============================================================================
# 2_geofilter_spokane.R
# Clip the BROAD-scope Meta "…During Crisis" exports (population tiles + movement
# flows) to a Spokane + Stevens area of interest (AOI), then cache filtered,
# DROP-IN .Rds for the report.
#
# WHY: the broad export spans Boise -> Portland -> the Canadian border
#      (~18k tiles per 8-hour window); only ~6-8% sit in/around the two incident
#      counties, so plotting the whole thing gives zoomed-out, janky figures.
#      This builds ONE AOI polygon and clips both datasets to it.
#
# COST: a few seconds on Colab. The point-in-polygon test runs on DISTINCT tile
#       centroids only (a tile's location is fixed across time windows), so it
#       tests ~20k points against one polygon — not every row.
#
# OUT (written to OUT_DIR): fb_data_bing.Rds, mp_data_bing.Rds, moved.Rds, aoi.Rds
#   — same schema/globals your plotting functions expect. Sourcing this file also
#   ASSIGNS fb_data_bing / mp_data_bing / moved / aoi as globals, and RE-USES the
#   cached .Rds on a re-run (set RE_RUN <- TRUE to force a rebuild).
#   Then rebuild the tile polygons once (needs quadkeyr, from 1_data_cleaning*.R):
#       fb <- readRDS(file.path(OUT_DIR, "fb_data_bing.Rds"))
#       tiles_3857 <- build_tiles(fb, re_run = TRUE)
# ============================================================================

suppressPackageStartupMessages({ library(sf); library(dplyr); library(readr) })

# ---- paths (EDIT for Colab) -------------------------------------------------
ROOT     <- "/content"   # folder that CONTAINS the Meta exports

# FB_POP_DATA_PATH <- "Facebook Population During Crisis - Bing Tiles"
# FB_MOVE_DATA_PATH <- "Movement Between Places During Crisis"
FB_POP_DATA_PATH <- "fb_pop_crisis_bing_tiles"
FB_MOVE_DATA_PATH <- "fb_move_crisis_bing_tiles"

POP_DIR  <- file.path(ROOT, "/", FB_POP_DATA_PATH)
MOVE_DIR <- file.path(ROOT, "/", FB_MOVE_DATA_PATH)
OUT_DIR  <- file.path(ROOT, "rds_spokane")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Cache guard: re-uses rds_spokane/*.Rds if present, so the CSV re-read + crop only
# run the first time (matches the re_run pattern in 1_data_cleaning*.R — it even
# inherits that file's `re_run_cleaning` flag if you sourced it first). Force a
# rebuild after editing the paths or build_aoi() args: RE_RUN <- TRUE (or delete
# rds_spokane/).
RE_RUN <- if (exists("re_run_cleaning")) isTRUE(re_run_cleaning) else FALSE

# ---- 1. Area of interest ----------------------------------------------------
# Default AOI = Spokane + Stevens counties (the incident's named counties),
# dissolved and buffered by `margin_km` so border tiles and cross-border
# evacuation flows are kept, and the frame matches the county evac-lookup map.
# If tigris/network is unavailable it AUTO-falls back to `bbox`; set
# use_counties = FALSE to force the plain bounding box.
#   - widen the frame (e.g. to pull in Post Falls / Coeur d'Alene population):
#     bump margin_km to ~20.
build_aoi <- function(use_counties = TRUE, margin_km = 8,
                      counties = c("Spokane", "Stevens"), state = "WA",
                      bbox = c(xmin = -118.60, xmax = -116.80,   # counties + margin;
                               ymin = 47.20,  ymax = 49.05)) {   # ~map extent
  make_bbox <- function() st_as_sfc(st_bbox(bbox, crs = 4326))
  g <- if (!use_counties) make_bbox() else tryCatch({
    co <- tigris::counties(state = state, cb = TRUE, year = 2022, progress_bar = FALSE)
    co <- co[co$NAME %in% counties, ]
    stopifnot("county names not matched" = nrow(co) == length(counties))
    st_transform(st_buffer(st_union(st_transform(co, 3857)), margin_km * 1000), 4326)
  }, error = function(e) {
    message("tigris unavailable (", conditionMessage(e), "); using bbox AOI.")
    make_bbox()
  })
  st_sf(geometry = st_make_valid(g))
}

# DISTINCT (lon,lat) tile centroids that fall inside the AOI. Cheap vectorised
# bbox pre-filter first, precise polygon test only on the survivors.
inside_tiles <- function(lon, lat, aoi) {
  d  <- dplyr::distinct(data.frame(lon = lon, lat = lat))
  bb <- st_bbox(aoi)
  d  <- dplyr::filter(d, dplyr::between(lon, bb["xmin"], bb["xmax"]),
                         dplyr::between(lat, bb["ymin"], bb["ymax"]))
  if (nrow(d) == 0) return(d)
  pts <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  d[lengths(st_intersects(pts, aoi)) > 0, c("lon", "lat"), drop = FALSE]
}

# Population: keep tiles whose centroid is inside the AOI.
crop_population <- function(df, aoi) {
  keep <- inside_tiles(df$longitude, df$latitude, aoi)
  dplyr::semi_join(df, dplyr::rename(keep, longitude = lon, latitude = lat),
                   by = c("longitude", "latitude"))
}

# Movement: keep a flow if EITHER endpoint is inside the AOI, so evacuation
# egress (origin in-county, destination just outside) is preserved. Downstream
# plots hard-clip the drawing extent, so keeping long egress flows is safe.
crop_movement <- function(df, aoi) {
  ends <- rbind(data.frame(lon = df$start_longitude, lat = df$start_latitude),
                data.frame(lon = df$end_longitude,   lat = df$end_latitude))
  ins  <- inside_tiles(ends$lon, ends$lat, aoi)
  ins  <- paste(ins$lon, ins$lat)                       # membership set (same-session exact)
  dplyr::filter(df, paste(start_longitude, start_latitude) %in% ins |
                    paste(end_longitude,   end_latitude)   %in% ins)
}

# ---- 2. Reader (mirrors aggregate_csvs: \N -> NA, dedupe, ds/hour from name) -
read_meta_dir <- function(dir) {
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  stopifnot("no CSVs found in dir" = length(files) > 0)
  dplyr::bind_rows(lapply(files, function(f)
    readr::read_csv(f, show_col_types = FALSE, na = c("", "NA", "\\N")) |>
      dplyr::mutate(source_file = basename(f)))) |>
    dplyr::distinct() |>
    dplyr::mutate(ds   = as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", source_file)),
                  hour = sub(".*_(\\d{4}-\\d{2}-\\d{2})_(\\d{4}).*", "\\2", source_file))
}

# ---- 3. Run -----------------------------------------------------------------
# Build the object, or load its cache when present (unless RE_RUN). Mirrors the
# self-healing load_*/build_* helpers in 1_data_cleaning*.R.
cache_or_build <- function(file, build) {
  path <- file.path(OUT_DIR, file)
  if (!RE_RUN && file.exists(path)) { message("cached  ", path); return(readRDS(path)) }
  obj <- build(); saveRDS(obj, path); message("wrote   ", path); obj
}

aoi <- cache_or_build("aoi.Rds", function() build_aoi())   # changed build_aoi() args? RE_RUN <- TRUE

fb_data_bing <- cache_or_build("fb_data_bing.Rds", function() {
  raw <- read_meta_dir(POP_DIR); out <- crop_population(raw, aoi)
  message(sprintf("  population kept %d / %d rows (%.1f%%)",
                  nrow(out), nrow(raw), 100 * nrow(out) / nrow(raw))); out
})

mp_data_bing <- cache_or_build("mp_data_bing.Rds", function() {
  raw <- read_meta_dir(MOVE_DIR); out <- crop_movement(raw, aoi)
  message(sprintf("  movement kept %d / %d rows (%.1f%%)",
                  nrow(out), nrow(raw), 100 * nrow(out) / nrow(raw))); out
})

# `moved`: drop zero-length self-flows + apply the report's column renames
# (same as build_moved() in 1_data_cleaning*.R) so it is a drop-in replacement.
moved <- cache_or_build("moved.Rds", function()
  mp_data_bing |>
    dplyr::filter(start_longitude != end_longitude | start_latitude != end_latitude) |>
    dplyr::rename(`Difference between baseline and crisis` = n_difference,
                  `# Users During Crisis` = n_crisis))

message("ready: fb_data_bing, mp_data_bing, moved, aoi  (cache dir: ", OUT_DIR, ")")

# ---- 4. Optional: eyeball the crop on Colab ---------------------------------
# library(ggplot2)
# pts <- st_as_sf(dplyr::distinct(fb_data_bing, longitude, latitude),
#                 coords = c("longitude", "latitude"), crs = 4326)
# ggplot() +
#   geom_sf(data = aoi, fill = NA, color = "red", linewidth = 0.6) +
#   geom_sf(data = pts, size = 0.3, alpha = 0.4) +
#   labs(title = "Kept population tiles within the Spokane + Stevens AOI") +
#   theme_minimal()
