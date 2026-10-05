# ============================================================================
# 3_network_coverage_spokane_fires.R
# Meta "Network Coverage" (cellular connectivity) pipeline + a triangulation
# layer that sanity-checks whether the Population / Movement signals are skewed
# by the loss (or gain) of cellular coverage rather than by people actually
# moving.
#
# WHY (ESF-2 / comms-restoration awareness AND signal-integrity QA):
#   1. STANDALONE PRODUCT (Stages 1-5): where has cellular connectivity dropped
#      versus its 30-day baseline, and how confident are we? Meta's own questions
#      ("where did we not observe connectivity", "how certain are we there's been
#      a drop"). Emits maps + a time series an EM can act on.
#   2. TRIANGULATION (Stage 6, the reason this file exists for THIS project):
#      a tile emptying in the Population map can mean people LEFT (real signal) OR
#      the tower/power DROPPED (artifact). This file joins the independent
#      connectivity read to Population/Movement at the exact Bing-tile hierarchy
#      and quantifies how much of the observed population decline / evacuation
#      egress is CO-LOCATED with an outage -> an upper bound on coverage-induced
#      skew. Small share => the mobility signal is trustworthy here; large share
#      => caveat it.
#
# STATISTICAL FRAMING (encoded as behaviour, not just prose):
#   Active/Undetected are derived from the SAME Location-Services pings as the
#   Population count, so "no pings -> undetected" is mechanically correlated with
#   "population dropped". Using `undetected` to *explain* a population drop is
#   therefore partly circular. `p_connectivity` conditions on expected users +
#   day-of-week, so `outage_prob = 1 - p_connectivity` is the LEAST-confounded
#   outage estimator -> treated as primary; `undetected` is the candidate set.
#
# RELATIONSHIP TO THE OTHER FILES (modular, decoupled):
#   - Mirrors 1_data_cleaning_spokane_fires.R conventions: define-only on source;
#     each heavy step reads/writes a self-healing .Rds via cache_path()/RE_RUN,
#     RETURNS its object, guards loudly, has a doc header + a sanity check.
#   - Reuses 2_geofilter_spokane_fires.R's AOI so the connectivity footprint is
#     clipped to the SAME extent the Population/Movement data were clipped to
#     (critical for an apples-to-apples triangulation).
#   - Stages 1-5 run start-to-finish WITHOUT the population pipeline loaded.
#     Stage 6 + the fire overlay are OPTIONAL: they activate only when
#     fb_data_bing / moved / fires are available (passed in or found as globals).
#
# DEVIATION FROM claude_code_prompt_network_pipeline.md (read before reviewing):
#   * That prompt scoped OUT Stage 6 and assumed a `YYYYMMDD` filename date. The
#     real files use dashed dates (`YYYY-MM-DD`) and the project's live ask is
#     exactly the triangulation the prompt descoped. Both reconciled here; see the
#     [CHECK_ANALYSIS] block and the CHANGELOG at the foot of this file.
#
# OUT (written to CACHE_DIR): nc_daily.Rds, nc_tiles.Rds, nc_status.Rds,
#   nc_change.Rds, nc_l14.Rds (+ optional triangulation tables). Sourcing this
#   file only DEFINES functions; nothing heavy runs until you call load_nc().
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(sf); library(ggplot2)
  # Soft deps (each guarded at its call site so the file still SOURCES without them):
  #   quadkeyr  -> reuse quadkey_df_to_polygon() (already used by build_tiles())
  #   igraph    -> connected-component denoise; falls back to "all singletons"
  #   maptiles  -> basemap; falls back to no basemap
  #   viridis   -> sequential outage palette; falls back to a base gradient
})

# ============================================================================
# 1. CONFIG  (named params, not magic numbers)
# ============================================================================

# Bing tile levels. NC ships level-16 (~600 m); Population/Movement are level-14
# (~2.4 km). A level-16 quadkey's first 14 chars ARE its containing level-14
# tile -> that is the exact hierarchical join key used in Stage 6.
Z_NC  <- 16L
Z_POP <- 14L

# Where the three category folders live. EDIT for Colab (matches 2_geofilter's
# `ROOT <- "/content"` convention). Locally these sit under Documents/claude/data
# i.e. the sibling `data/` folder next to this file's working dir (spokane_fires/).
NC_ROOT <- if (dir.exists("/content")) "/content" else
           file.path(dirname(getwd()), "data")   # local fallback: ../data
NC_DIRS <- list(                                # category -> subfolder (folder is
  active     = "network_coverage_active",       # the RELIABLE category signal;
  undetected = "network_coverage_undetected",   # filename tokens are not, see §1
  prob       = "network_coverage_probability")  # of the CHANGELOG)

# Expected value column per category (used to VALIDATE + to skip mislabeled files).
NC_VALCOL <- c(active = "coverage", undetected = "no_coverage", prob = "p_connectivity")

# Analysis thresholds -------------------------------------------------------
tau_outage      <- 0.5   # outage_prob >= this on an undetected tile => "likely outage"
k_cluster       <- 3L    # min contiguous undetected L16 tiles for a "real" outage cluster
persist_days     <- 2L   # >= this many consecutive undetected days => CONFIRMED (vs candidate)
frac_undetected_hi <- 0.5 # L14 tile >= this share of OBSERVED L16 tiles undetected => compromised
pop_drop_z      <- -2    # population z_score <= this => a material population drop (Stage 6)

# Cache / re-run. Inherit the population pipeline's flags when sourced alongside
# it; otherwise define our own so this file is self-contained.
if (!exists("re_run_cleaning")) re_run_cleaning <- FALSE
if (!exists("rds_cache_dir"))   rds_cache_dir   <- "."
NC_RE_RUN <- isTRUE(re_run_cleaning)
CACHE_DIR <- rds_cache_dir

# Resolve <dir>/<file>, creating <dir> if needed (reuse the population helper if
# this file was sourced after 1_data_cleaning*.R; else define an identical one).
if (!exists("cache_path")) {
  cache_path <- function(file, dir = rds_cache_dir) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file.path(dir, file)
  }
}

# Colorblind-safe palettes (Okabe-Ito). Keep status/quadrant colours stable so
# every map in the notebook reads the same way.
NC_PAL_STATUS <- c(COVERED = "#009E73", UNDETECTED = "#D55E00",
                   UNCERTAIN = "#CC79A7", NO_BASELINE = "grey70")
NC_PAL_CHANGE <- c(NEW_UNDETECTED = "#D55E00", RECOVERED = "#009E73",
                   PERSISTENT_OUTAGE = "#7A0177", STABLE = "grey80")
NC_PAL_QUAD   <- c(GENUINE_DROP = "#0072B2", CONFOUNDED = "#D55E00",
                   STABLE_COVERED = "grey85", MASKED_OR_RESILIENT = "#CC79A7",
                   UNKNOWN_COVERAGE = "grey55")

# ---------------------------------------------------------------------------
# .nc_pad_quadkey() — restore leading zeros a numeric CSV parse may have dropped.
# WHY: Bing quadkeys are strings of digits 0-3 and leading zeros are SIGNIFICANT
#      ("02123..."), but readr/Excel can read the population/movement quadkey
#      column as a NUMBER and silently drop them ("2123..."). Our NC quadkeys are
#      derived as strings (always correct); this guards the OTHER side of the
#      Stage-6 join. Only leading zeros are ever lost (interior zeros are
#      significant digits and survive), so a left-pad to the target width is exact.
#' @param qk character/numeric vector; @param width integer target length (Z_POP=14).
#' @return character quadkeys, short all-[0-3] values left-padded with "0".
# ---------------------------------------------------------------------------
.nc_pad_quadkey <- function(qk, width = Z_POP) {
  qk <- as.character(qk)
  short <- !is.na(qk) & nchar(qk) < width & grepl("^[0-3]*$", qk)
  qk[short] <- stringr::str_pad(qk[short], width, side = "left", pad = "0")
  qk
}

# ============================================================================
# STAGE 2 (defined early — Stage 1's join needs the quadkey helper)
#   Tile identity + geometry.  NC ships lon/lat but no quadkey, so we derive it.
# ============================================================================

# ---------------------------------------------------------------------------
# nc_quadkey_from_lonlat() — Bing/slippy-tile quadkey from a tile-centre lon/lat.
# WHY: NC has no quadkey, but the quadkey is the EXACT tile key we must join on
#      (float lon/lat equality across categories/levels is unsafe). Implemented
#      with explicit bit-math (not a package call) so it is dependency-free and
#      unit-testable; verified to reproduce Meta's level-14 population quadkeys
#      exactly (see nc_quadkey_consistency_check() and the test file).
#' @param lon,lat numeric vectors, tile-centre coordinates in degrees (EPSG:4326).
#' @param zoom    integer Bing level (default Z_NC = 16).
#' @return character vector of quadkeys, length == length(lon). LEADING ZEROS
#'         PRESERVED (returned as strings) — required for the level-14 join key.
#' @sideeffects none.
#' @examples nc_quadkey_from_lonlat(-117.454833984375, 47.672785151405336, 14)
#'           #> "02123102031312"
# ---------------------------------------------------------------------------
nc_quadkey_from_lonlat <- function(lon, lat, zoom = Z_NC) {
  stopifnot(length(lon) == length(lat), zoom >= 1L)
  # Clamp to the Web Mercator valid range Bing uses (avoids Inf at the poles).
  lat <- pmin(pmax(lat, -85.05112878), 85.05112878)
  lon <- pmin(pmax(lon, -180), 180 - 1e-9)
  sin_lat <- sin(lat * pi / 180)
  x <- (lon + 180) / 360
  # Forward Web Mercator y in [0,1] (identical to the atanh/gudermannian form).
  y <- 0.5 - log((1 + sin_lat) / (1 - sin_lat)) / (4 * pi)
  n <- 2^zoom
  tx <- pmin(pmax(floor(x * n), 0), n - 1)   # tile X index, clamped to [0, n-1]
  ty <- pmin(pmax(floor(y * n), 0), n - 1)   # tile Y index
  tx <- as.integer(tx); ty <- as.integer(ty)
  # Build the quadkey MSB-first: digit = (x-bit) + 2*(y-bit) at each level.
  digits <- vector("list", zoom)
  for (i in seq_len(zoom)) {
    mask <- bitwShiftL(1L, zoom - i)          # bit for this level (MSB first)
    dx <- as.integer(bitwAnd(tx, mask) > 0L)
    dy <- as.integer(bitwAnd(ty, mask) > 0L)
    digits[[i]] <- dx + 2L * dy               # 0..3, as an integer vector
  }
  do.call(paste0, digits)                     # elementwise concat -> quadkey strings
}

# ---------------------------------------------------------------------------
# nc_quadkey_bounds() — inverse: quadkey -> tile lon/lat bounding box (+ centre).
# WHY: (a) an honest per-tile area needs the real square (Stage 4 uses st_area),
#      (b) a dependency-free polygon fallback if quadkeyr is absent, (c) the
#      round-trip sanity check in Stage 2 / the tests.
#' @param qk character vector of quadkeys, ALL the same length (== zoom).
#' @return data.frame(quadkey, xmin, xmax, ymin, ymax, lon_c, lat_c) in degrees.
#' @sideeffects none.
# ---------------------------------------------------------------------------
nc_quadkey_bounds <- function(qk) {
  stopifnot(length(qk) > 0)
  z <- nchar(qk[1]); stopifnot("mixed quadkey lengths" = all(nchar(qk) == z))
  ch <- do.call(rbind, strsplit(qk, "", fixed = TRUE))   # n x z character matrix
  tx <- integer(length(qk)); ty <- integer(length(qk))
  for (j in seq_len(z)) {                                  # MSB (j=1) first
    d  <- as.integer(ch[, j])
    tx <- bitwOr(bitwShiftL(tx, 1L), bitwAnd(d, 1L))
    ty <- bitwOr(bitwShiftL(ty, 1L), as.integer(bitwAnd(d, 2L) > 0L))
  }
  n <- 2^z
  lon_of <- function(t) t / n * 360 - 180
  lat_of <- function(t) atan(sinh(pi * (1 - 2 * t / n))) * 180 / pi   # inverse Mercator
  lon1 <- lon_of(tx);       lon2 <- lon_of(tx + 1)
  lat1 <- lat_of(ty);       lat2 <- lat_of(ty + 1)        # ty increases southward
  data.frame(
    quadkey = qk,
    xmin = pmin(lon1, lon2), xmax = pmax(lon1, lon2),
    ymin = pmin(lat1, lat2), ymax = pmax(lat1, lat2),
    lon_c = lon_of(tx + 0.5), lat_c = lat_of(ty + 0.5),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# nc_polys_from_quadkey() — L16 quadkeys -> sf polygons (EPSG:4326).
# WHY: reuse the project's TRUSTED quadkey_df_to_polygon() (same routine
#      build_tiles() uses for population tiles, so NC + population geometry are
#      built identically); degrade to the dependency-free bbox squares if it is
#      unavailable or errors on level-16 keys — never silently return nothing.
#' @param qk character vector of DISTINCT quadkeys.
#' @return sf with columns (quadkey, geometry), CRS 4326, one square per quadkey.
# ---------------------------------------------------------------------------
nc_polys_from_quadkey <- function(qk) {
  qk <- unique(qk[!is.na(qk) & nzchar(qk)])
  stopifnot("no quadkeys to polygonise" = length(qk) > 0)
  out <- tryCatch({
    if (!requireNamespace("quadkeyr", quietly = TRUE))
      stop("quadkeyr not installed")
    g <- quadkeyr::quadkey_df_to_polygon(data.frame(quadkey = qk))
    g <- g["quadkey"]                                   # keep key + geometry only
    sf::st_transform(g, 4326)
  }, error = function(e) {
    message("nc_polys_from_quadkey(): quadkeyr path unavailable (",
            conditionMessage(e), "); using dependency-free bbox squares.")
    b <- nc_quadkey_bounds(qk)
    geom <- sf::st_sfc(lapply(seq_len(nrow(b)), function(i) {
      sf::st_polygon(list(matrix(c(
        b$xmin[i], b$ymin[i], b$xmax[i], b$ymin[i],
        b$xmax[i], b$ymax[i], b$xmin[i], b$ymax[i],
        b$xmin[i], b$ymin[i]), ncol = 2, byrow = TRUE)))
    }), crs = 4326)
    sf::st_sf(quadkey = b$quadkey, geometry = geom)
  })
  out
}

# ============================================================================
# STAGE 1. INGEST & HARMONIZE the three categories -> nc_daily
# ============================================================================

# ---------------------------------------------------------------------------
# parse_nc_filename() — pull ds/hhmm/dataset_id/(token) from a NC file name.
# WHY: NC has no `ds`/`hour` columns; they live in the file name. Handles BOTH
#      shapes present in the data: `nc_<cat>_<id>_<YYYY-MM-DD>_<HHMM>.csv` (renamed)
#      and `<id>_<YYYY-MM-DD>_<HHMM>.csv` (raw download). The DASHED date matches
#      aggregate_csvs()/read_meta_dir() — the prompt's `\d{8}` would match nothing.
#' @param path character file path (or basename).
#' @return tibble(1x): source_file, token (active|undetected|prob|NA),
#'         dataset_id (chr|NA), ds (Date), hhmm (chr).
# ---------------------------------------------------------------------------
parse_nc_filename <- function(path) {
  f  <- basename(path)
  ds <- suppressWarnings(as.Date(str_match(f, "(\\d{4}-\\d{2}-\\d{2})")[, 2]))
  hh <- str_match(f, "\\d{4}-\\d{2}-\\d{2}_(\\d{4})")[, 2]
  tk <- str_match(f, "^nc_(active|undetected|prob)_")[, 2]      # NA for raw files
  id <- str_match(f, "(?:^nc_[a-z]+_)?(\\d{6,})_\\d{4}-\\d{2}-\\d{2}")[, 2]
  tibble::tibble(source_file = f, token = tk, dataset_id = id, ds = ds, hhmm = hh)
}

# ---------------------------------------------------------------------------
# read_one_nc_csv() — read a single NC csv and VALIDATE it against `category`.
# WHY: category is taken from the DIRECTORY (reliable), not the file name — the
#      data contains a mislabeled file (an undetected export sitting in the
#      probability folder). We keep a file only if it carries the value column
#      the folder promises; otherwise we WARN and skip it (never mis-ingest).
#' @param path      character path to one nc_*.csv.
#' @param category  chr one of "active","undetected","prob".
#' @return tibble(value_col, country, lon, lat, ds, dataset_id, source_file) or
#'         NULL (with a warning) if the expected value column is absent.
# ---------------------------------------------------------------------------
read_one_nc_csv <- function(path, category) {
  vcol <- NC_VALCOL[[category]]
  df <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA", "\\N"))
  if (!vcol %in% names(df)) {
    warning("read_one_nc_csv(): '", basename(path), "' in the ", category,
            " folder lacks '", vcol, "' (has: ", paste(names(df), collapse = ", "),
            ") — skipping (likely a mislabeled export).")
    return(NULL)
  }
  need <- c("lon", "lat")
  if (!all(need %in% names(df)))
    stop("read_one_nc_csv(): '", basename(path), "' missing lon/lat.")
  meta <- parse_nc_filename(path)
  df |>
    dplyr::transmute(
      !!vcol := .data[[vcol]],
      country = if ("country" %in% names(df)) as.character(country) else NA_character_,
      lon = as.numeric(lon), lat = as.numeric(lat),
      ds  = meta$ds, dataset_id = meta$dataset_id, source_file = meta$source_file)
}

# ---------------------------------------------------------------------------
# aggregate_nc_csvs() — read + bind + validate ONE category's folder.
# WHY: the per-category tibble that Stage-1's cross-category join consumes.
#' @param category chr one of names(NC_DIRS).
#' @param root     chr folder containing the category subfolders (default NC_ROOT).
#' @return tibble with the category's value column coerced/validated + ds/lon/lat.
#'         active/undetected coerced to integer and asserted == 1; prob coerced to
#'         double and asserted within [0,1].
#' @sideeffects reads CSVs from disk; warns on skipped/mislabeled files.
# ---------------------------------------------------------------------------
aggregate_nc_csvs <- function(category, root = NC_ROOT) {
  stopifnot(category %in% names(NC_DIRS))
  dir   <- file.path(root, NC_DIRS[[category]])
  if (!dir.exists(dir)) stop("aggregate_nc_csvs(): missing folder ", dir)
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) stop("aggregate_nc_csvs(): no CSVs in ", dir)

  parts <- Filter(Negate(is.null), lapply(files, read_one_nc_csv, category = category))
  if (!length(parts))
    stop("aggregate_nc_csvs(): no usable files in ", dir, " (all lacked '",
         NC_VALCOL[[category]], "').")
  out  <- dplyr::bind_rows(parts) |> dplyr::distinct()
  vcol <- NC_VALCOL[[category]]

  # Validate + coerce the value column per category (fail loudly on garbage).
  if (category == "prob") {
    out[[vcol]] <- as.numeric(out[[vcol]])
    rng <- range(out[[vcol]], na.rm = TRUE)
    if (is.finite(rng[1]) && (rng[1] < -1e-9 || rng[2] > 1 + 1e-9))
      stop("aggregate_nc_csvs(prob): p_connectivity outside [0,1]: [",
           rng[1], ", ", rng[2], "]")
  } else {
    out[[vcol]] <- as.integer(round(as.numeric(out[[vcol]])))
    bad <- out[[vcol]][!is.na(out[[vcol]])]
    if (length(bad) && any(bad != 1L))
      warning("aggregate_nc_csvs(", category, "): '", vcol,
              "' has values other than 1 (Meta ships only 1s here).")
  }
  if (all(is.na(out$ds)))
    warning("aggregate_nc_csvs(", category, "): no dates parsed from file names.")
  out
}

# ---------------------------------------------------------------------------
# load_nc() — build the harmonised daily table nc_daily (Stage 1 product).
# WHY: one row per (tile-day) carrying all three connectivity reads, keyed by the
#      exact level-16 quadkey. Cross-category join is on (quadkey, ds) — NOT raw
#      lon/lat floats — because the quadkey is the tile's exact identity and is
#      bit-stable across the three independent category exports.
#' @param root       chr folder with the category subfolders (default NC_ROOT).
#' @param re_run     logical force rebuild (default NC_RE_RUN).
#' @param cache_dir  chr .Rds cache dir (default CACHE_DIR).
#' @return tibble nc_daily: quadkey(chr), ds(Date), lon, lat, country,
#'         active(int|NA), undetected(int|NA), p_connectivity(dbl|NA), dataset ids.
#' @sideeffects reads CSVs; writes/reads cache_dir/nc_daily.Rds; warns if the
#'         three categories disagree on the set of dates present.
#' @examples nc_daily <- load_nc()
# ---------------------------------------------------------------------------
load_nc <- function(root = NC_ROOT, re_run = NC_RE_RUN, cache_dir = CACHE_DIR) {
  path <- cache_path("nc_daily.Rds", cache_dir)
  if (!re_run && file.exists(path)) { message("Loading cached ", path); return(readRDS(path)) }
  message("Building nc_daily from CSVs in ", root, " -> ", path)

  cats <- lapply(names(NC_DIRS), function(cat) {
    df <- aggregate_nc_csvs(cat, root)
    df$quadkey <- nc_quadkey_from_lonlat(df$lon, df$lat, Z_NC)   # derive tile identity
    df
  })
  names(cats) <- names(NC_DIRS)

  # Warn if the categories cover different date sets (expected here: the raw
  # 08-07 export exists for active/undetected but not prob — see CHANGELOG).
  dsets <- lapply(cats, function(d) sort(unique(as.character(d$ds))))
  if (length(unique(dsets)) > 1)
    warning("load_nc(): categories disagree on dates. ",
            paste(sprintf("%s={%s}", names(dsets), vapply(dsets, paste, "",
                   collapse = ",")), collapse = "  "))

  # Reduce each category to (quadkey, ds) + its value (+ carried lon/lat/country),
  # then full-join. Coalesce lon/lat/country across categories (a quadkey implies
  # one location, so these agree by construction; coalesce is just to fill NAs).
  slim <- function(d, vcol) dplyr::distinct(dplyr::select(
    d, quadkey, ds, lon, lat, country, dataset_id, dplyr::all_of(vcol)))
  a <- slim(cats$active,     "coverage")       |> dplyr::rename(active = coverage, id_active = dataset_id)
  u <- slim(cats$undetected, "no_coverage")    |> dplyr::rename(undetected = no_coverage, id_undet = dataset_id)
  p <- slim(cats$prob,       "p_connectivity") |> dplyr::rename(id_prob = dataset_id)

  join_keep <- function(x, y) dplyr::full_join(x, y, by = c("quadkey", "ds"),
                                               suffix = c("", ".y")) |>
    dplyr::mutate(lon = dplyr::coalesce(lon, .data[["lon.y"]]),
                  lat = dplyr::coalesce(lat, .data[["lat.y"]]),
                  country = dplyr::coalesce(country, .data[["country.y"]])) |>
    dplyr::select(-dplyr::ends_with(".y"))

  # active/undetected are tri-state after the full join: 1L (observed in that
  # category's export) or NA (not observed) — we deliberately DO NOT coerce NA to
  # 0, so "not in the active export" stays distinct from "actively 0".
  nc_daily <- join_keep(a, u) |> join_keep(p) |>
    dplyr::mutate(quadkey = as.character(quadkey)) |>
    dplyr::select(quadkey, ds, lon, lat, country,
                  active, undetected, p_connectivity,
                  dplyr::any_of(c("id_active", "id_undet", "id_prob")))

  saveRDS(nc_daily, path)
  nc_daily
}

# ============================================================================
# STAGE 2 (continued). Tile polygons + AOI clip
# ============================================================================

# ---------------------------------------------------------------------------
# nc_load_aoi() — the clip polygon, SHARED with the population/movement crop.
# WHY: triangulation is only valid if NC is clipped to the SAME extent as
#      Population/Movement. Prefer the exact aoi from 2_geofilter (global `aoi`
#      or aoi.Rds); fall back to a bbox only if that is unavailable.
#' @param aoi       optional sf AOI; NULL -> global `aoi` -> aoi.Rds -> bbox.
#' @param bbox      numeric fallback c(xmin,xmax,ymin,ymax) (default = 2_geofilter's).
#' @return sf (one polygon), CRS 4326.
# ---------------------------------------------------------------------------
nc_load_aoi <- function(aoi = NULL, cache_dir = CACHE_DIR,
                        bbox = c(xmin = -118.60, xmax = -116.80,
                                 ymin = 47.20,  ymax = 49.05)) {
  a <- aoi
  if (is.null(a)) a <- get0("aoi", ifnotfound = NULL)
  if (is.null(a)) { p <- cache_path("aoi.Rds", cache_dir)
                    if (file.exists(p)) a <- readRDS(p) }
  if (is.null(a) || !inherits(a, "sf") || nrow(a) == 0) {
    message("nc_load_aoi(): no shared AOI found; using bbox fallback ",
            "(matches 2_geofilter). Load 2_geofilter first for an exact match.")
    a <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(bbox, crs = 4326)))
  }
  sf::st_transform(a, 4326)
}

# ---------------------------------------------------------------------------
# build_nc_tiles() — DISTINCT level-16 quadkeys -> sf polygons (EPSG:3857),
# clipped to the AOI, with an honest per-tile area (km^2) via st_area.
# WHY: geometry depends only on the quadkey (not the day), so polygonise once and
#      reuse — the same PERF pattern as build_tiles(). Clipping here means every
#      downstream stage already works on the Spokane extent.
#' @param nc_daily  tibble from load_nc().
#' @param aoi       optional sf AOI (default nc_load_aoi()).
#' @return sf(quadkey, area_km2, geometry), CRS 3857, AOI-clipped, one row/tile.
#' @sideeffects writes/reads cache_dir/nc_tiles.Rds.
# ---------------------------------------------------------------------------
build_nc_tiles <- function(nc_daily, aoi = NULL, re_run = NC_RE_RUN, cache_dir = CACHE_DIR) {
  path <- cache_path("nc_tiles.Rds", cache_dir)
  if (!re_run && file.exists(path)) { message("Loading cached ", path); return(readRDS(path)) }

  # PERF: clip to the AOI on tile CENTROIDS *before* polygonising, so we build only
  # the ~Spokane-area tiles (a few thousand) instead of every tile in the whole
  # crisis bbox (~10-16k) — polygonising the full set via quadkeyr takes minutes.
  # Uses the lon/lat already in nc_daily; a cheap bbox filter precedes the precise
  # point-in-polygon test. All spatial ops run in EPSG:3857 (GEOS), which sidesteps
  # s2's strict validity checks on the county AOI polygon.
  td <- dplyr::distinct(nc_daily, quadkey, lon, lat)
  td <- td[!is.na(td$quadkey) & nzchar(td$quadkey) & !is.na(td$lon) & !is.na(td$lat), ]
  a4326 <- nc_load_aoi(aoi, cache_dir)
  bb    <- sf::st_bbox(a4326)
  td <- td[dplyr::between(td$lon, bb[["xmin"]], bb[["xmax"]]) &
           dplyr::between(td$lat, bb[["ymin"]], bb[["ymax"]]), ]
  if (!nrow(td)) stop("build_nc_tiles(): no NC tiles fall in the AOI bbox.")
  a3857  <- sf::st_make_valid(sf::st_transform(a4326, 3857))          # GEOS make_valid (tolerant)
  pts    <- sf::st_transform(sf::st_as_sf(td, coords = c("lon", "lat"), crs = 4326), 3857)
  inside <- lengths(sf::st_intersects(pts, a3857)) > 0               # centroid inside AOI (GEOS)
  qk     <- td$quadkey[inside]
  if (!length(qk)) stop("build_nc_tiles(): no NC tile centroids inside the AOI polygon.")
  message("Building ", length(qk), " level-16 tile polygons (AOI-clipped) -> ", path)

  polys <- nc_polys_from_quadkey(qk)                                 # EPSG:4326, AOI tiles only
  # Area on 4326 (geodesic) = TRUE ground area; a 3857 transform would inflate it
  # by ~sec^2(lat) (~2.2x at Spokane's ~47.5N), so compute it BEFORE transforming.
  polys$area_km2 <- as.numeric(sf::st_area(polys)) / 1e6
  tiles <- sf::st_transform(polys, 3857)                             # match population maps
  message("  kept ", nrow(tiles), " tiles inside the AOI.")
  saveRDS(tiles, path)
  tiles
}

# ============================================================================
# STAGE 3. Per-tile-per-day connectivity STATE -> nc_status
# ============================================================================

# ---------------------------------------------------------------------------
# nc_classify() — assign each (quadkey, ds) an ordered connectivity status.
# WHY: turns the three raw reads into ONE interpretable state + a continuous
#      outage score. `p_connectivity` is primary (least confounded); `undetected`
#      is the candidate set. NA p on an undetected tile => "uncertain", NOT 0.
#' @param nc_daily tibble from load_nc().
#' @param tiles    optional sf from build_nc_tiles(); if given, rows are filtered
#'                 to tiles inside the AOI and NA are honest (unobserved).
#' @param tau      numeric outage-prob threshold (default tau_outage).
#' @return tibble nc_status keyed by (quadkey, ds): status (ordered factor
#'         COVERED<UNDETECTED<UNCERTAIN<NO_BASELINE), outage_prob (dbl|NA),
#'         likely_outage (lgl), lon, lat.
# ---------------------------------------------------------------------------
nc_classify <- function(nc_daily, tiles = NULL, tau = tau_outage) {
  df <- nc_daily
  if (!is.null(tiles)) df <- dplyr::filter(df, quadkey %in% tiles$quadkey)

  df <- df |>
    dplyr::mutate(
      is_active = !is.na(active)     & active == 1L,
      is_undet  = !is.na(undetected) & undetected == 1L,
      # outage_prob is only defined where we have a probability read; on active
      # tiles it is 0 BY CONSTRUCTION (they pinged today) — keep that explicit.
      outage_prob = dplyr::case_when(
        is_active               ~ 0,
        !is.na(p_connectivity)  ~ 1 - p_connectivity,
        TRUE                    ~ NA_real_),
      status = dplyr::case_when(
        is_active & is_undet    ~ "CONFLICT",      # should be empty; flagged in QA
        is_active               ~ "COVERED",
        is_undet & is.na(p_connectivity) ~ "UNCERTAIN",   # undetected but no prob
        is_undet                ~ "UNDETECTED",
        !is.na(p_connectivity)  ~ "UNCERTAIN",      # prob-only tile (not in either set)
        TRUE                    ~ "NO_BASELINE"),
      likely_outage = is_undet & !is.na(outage_prob) & outage_prob >= tau)

  # Ordered factor so maps/legends sort sensibly. CONFLICT kept visible (not
  # dropped) so the QA step can surface any active∩undetected contradiction.
  lev <- c("COVERED", "UNDETECTED", "UNCERTAIN", "NO_BASELINE", "CONFLICT")
  df |>
    dplyr::transmute(quadkey, ds, lon, lat,
                     status = factor(status, levels = lev, ordered = TRUE),
                     outage_prob, likely_outage,
                     is_active, is_undet, p_connectivity)
}

# ============================================================================
# STAGE 4. Temporal change / emerging outages -> nc_change + time series
# ============================================================================

# ---------------------------------------------------------------------------
# nc_change() — per tile, detect NEW_UNDETECTED / RECOVERED transitions and the
# persistence run-length of consecutive undetected days.
# WHY: a tile undetected for >= persist_days with high outage_prob is a CONFIRMED
#      outage; a 1-day flip is a CANDIDATE. EMs prioritise confirmed, sustained
#      outages over transient noise.
#' @param nc_status tibble from nc_classify().
#' @param persist   integer confirmed-run threshold (default persist_days).
#' @return tibble: adds prev_status, change (factor), undetected_run (int),
#'         confirmed_outage (lgl); one row per (quadkey, ds).
# ---------------------------------------------------------------------------
nc_change <- function(nc_status, persist = persist_days) {
  undet <- function(s) !is.na(s) & s %in% c("UNDETECTED", "UNCERTAIN")
  nc_status |>
    dplyr::arrange(quadkey, ds) |>
    dplyr::group_by(quadkey) |>
    dplyr::mutate(
      prev_status = dplyr::lag(as.character(status)),
      cur_undet   = undet(as.character(status)),
      prev_undet  = undet(prev_status),
      change = dplyr::case_when(
        !cur_undet & prev_undet          ~ "RECOVERED",
        cur_undet  & !prev_undet %in% TRUE ~ "NEW_UNDETECTED",  # includes first-seen undetected
        cur_undet  & prev_undet          ~ "PERSISTENT_OUTAGE",
        TRUE                             ~ "STABLE"),
      # run-length of the current consecutive-undetected streak
      undetected_run = {
        r <- integer(length(cur_undet)); run <- 0L
        for (i in seq_along(cur_undet)) { run <- if (isTRUE(cur_undet[i])) run + 1L else 0L; r[i] <- run }
        r
      },
      confirmed_outage = undetected_run >= persist &
                         (is.na(outage_prob) | outage_prob >= tau_outage)) |>
    dplyr::ungroup() |>
    dplyr::mutate(change = factor(change,
        levels = c("NEW_UNDETECTED", "RECOVERED", "PERSISTENT_OUTAGE", "STABLE")))
}

# ---------------------------------------------------------------------------
# nc_timeseries_df() — daily rollup for the event time series.
# WHY: one legible curve of the outage footprint over the event for the report.
#' @param nc_status tibble from nc_classify().
#' @param tiles     optional sf from build_nc_tiles() for area (km^2); else count only.
#' @return tibble per ds: n_undetected, n_covered, area_undetected_km2,
#'         mean_outage_prob, n_likely_outage.
# ---------------------------------------------------------------------------
nc_timeseries_df <- function(nc_status, tiles = NULL, change = NULL) {
  area <- if (!is.null(tiles))
    dplyr::select(sf::st_drop_geometry(tiles), quadkey, area_km2) else NULL
  base <- nc_status
  if (!is.null(area)) base <- dplyr::left_join(base, area, by = "quadkey")
  out <- base |>
    dplyr::group_by(ds) |>
    dplyr::summarise(
      n_covered    = sum(status == "COVERED", na.rm = TRUE),
      n_undetected = sum(status %in% c("UNDETECTED", "UNCERTAIN"), na.rm = TRUE),
      area_undetected_km2 = if (!is.null(area))
        sum(area_km2[status %in% c("UNDETECTED", "UNCERTAIN")], na.rm = TRUE) else NA_real_,
      mean_outage_prob = mean(outage_prob[is_undet], na.rm = TRUE),
      n_likely_outage  = sum(likely_outage, na.rm = TRUE),
      .groups = "drop")
  if (!is.null(change)) {
    ch <- change |> dplyr::group_by(ds) |>
      dplyr::summarise(n_new_undetected = sum(change == "NEW_UNDETECTED", na.rm = TRUE),
                       n_recovered      = sum(change == "RECOVERED", na.rm = TRUE),
                       .groups = "drop")
    out <- dplyr::left_join(out, ch, by = "ds")
  }
  out
}

# ============================================================================
# STAGE 5. Spatial denoising (rigor vs. Meta's cell-ID artifacts)
# ============================================================================

# ---------------------------------------------------------------------------
# nc_denoise() — separate REAL clustered outages from isolated cell-ID artifacts.
# WHY: a lone undetected tile is more likely a cell-ID quirk than a genuine
#      outage; a contiguous cluster is credible. We DOWN-WEIGHT singletons
#      (label them low-confidence) — never delete — and report the count so
#      "clean" never silently means "truncated".
#' @param nc_status tibble from nc_classify() (a single ds, or all — grouped by ds).
#' @param tiles     sf from build_nc_tiles() (needs geometry for adjacency).
#' @param k         integer min cluster size to call an outage "real" (default k_cluster).
#' @param method    "cluster" (st_touches + connected components) or "focal" (raster majority).
#' @return tibble nc_status + cluster_id, cluster_size, outage_confidence
#'         ("high" for clusters >= k, "low" for singletons/small), for undetected tiles.
#' @sideeffects messages the singleton (down-weighted) count per ds.
# ---------------------------------------------------------------------------
nc_denoise <- function(nc_status, tiles, k = k_cluster, method = c("cluster", "focal")) {
  method <- match.arg(method)
  if (method == "focal")
    message("nc_denoise(): 'focal' is a stub for the sparse synthetic data; ",
            "using 'cluster'. (Rasterize+terra::focal is the dense-grid path.)")

  # IDEMPOTENT: drop any denoise columns from a prior pass so re-running (e.g. via
  # nc_sweep_k, or on an already-denoised nc_status) doesn't create cluster_size.x/.y
  # in the join below and blow up the case_when with a 0-length column.
  nc_status <- dplyr::select(nc_status,
                 -dplyr::any_of(c("cluster_id", "cluster_size", "outage_confidence")))

  geom <- tiles["quadkey"]
  out_parts <- lapply(split(nc_status, nc_status$ds), function(day) {
    ud <- day[day$is_undet %in% TRUE, , drop = FALSE]
    if (nrow(ud) == 0) { day$cluster_id <- NA_integer_; day$cluster_size <- 0L
                         day$outage_confidence <- NA_character_; return(day) }
    g <- dplyr::inner_join(geom, dplyr::select(ud, quadkey), by = "quadkey")
    # Connected components over edge-adjacency of the undetected L16 squares.
    comp <- tryCatch({
      if (!requireNamespace("igraph", quietly = TRUE)) stop("igraph absent")
      # unclass() strips the sgbp class so igraph sees a plain list-of-indices;
      # vertex count = length(nb) = nrow(g), so isolated tiles still get a vertex.
      nb  <- unclass(sf::st_touches(g))                 # sparse adjacency list
      ig  <- igraph::graph_from_adj_list(nb, mode = "all")
      igraph::components(ig)$membership
    }, error = function(e) {
      message("nc_denoise(): igraph/adjacency unavailable (", conditionMessage(e),
              "); treating every undetected tile as a singleton.")
      seq_len(nrow(g))                                  # graceful: all singletons
    })
    sz <- as.integer(table(comp)[as.character(comp)])
    map <- tibble::tibble(quadkey = g$quadkey, cluster_id = as.integer(comp),
                          cluster_size = sz)
    day <- dplyr::left_join(day, map, by = "quadkey")
    day$outage_confidence <- dplyr::case_when(
      !day$is_undet          ~ NA_character_,
      day$cluster_size >= k  ~ "high",
      TRUE                   ~ "low")
    n_low <- sum(day$outage_confidence == "low", na.rm = TRUE)
    if (n_low > 0)
      message(sprintf("  nc_denoise[%s]: %d undetected tiles DOWN-WEIGHTED as low-confidence singletons/small clusters (< %d).",
                      as.character(day$ds[1]), n_low, k))
    day
  })
  dplyr::bind_rows(out_parts)
}

# ============================================================================
# STAGE 6 (OPTIONAL). TRIANGULATION vs Population & Movement
#   Runs only when fb_data_bing / moved are available. This is the layer the
#   project asked for: is the mobility signal skewed by coverage loss?
# ============================================================================

# ---------------------------------------------------------------------------
# nc_rollup_to_l14() — aggregate the level-16 connectivity state up to the
# level-14 population/movement tile (parent quadkey = first 14 chars).
# WHY: Population/Movement live at level 14; to attach a coverage state to them
#      we summarise the (up to 16) level-16 children observed in each L14 tile.
#      Coverage is assessed over OBSERVED children only — an L14 tile with no NC
#      observation is UNKNOWN, never assumed covered.
#' @param nc_status tibble from nc_classify() (optionally nc_denoise()'d).
#' @return tibble per (qk14, ds): n_l16, n_active, n_undet, frac_undetected,
#'         mean_outage_prob, max_outage_prob, any_likely_outage,
#'         any_confirmed (if a denoised/changed input is passed), compromised (lgl).
# ---------------------------------------------------------------------------
nc_rollup_to_l14 <- function(nc_status, frac_hi = frac_undetected_hi, tau = tau_outage) {
  conf <- if ("outage_confidence" %in% names(nc_status)) nc_status$outage_confidence else NA
  nc_status |>
    dplyr::mutate(qk14 = stringr::str_sub(quadkey, 1, Z_POP),
                  high_conf_outage = is_undet &
                    (if (all(is.na(conf))) TRUE else outage_confidence %in% "high")) |>
    dplyr::group_by(qk14, ds) |>
    dplyr::summarise(
      n_l16            = dplyr::n(),
      n_active         = sum(is_active, na.rm = TRUE),
      n_undet          = sum(is_undet, na.rm = TRUE),
      frac_undetected  = n_undet / n_l16,
      mean_outage_prob = mean(outage_prob[is_undet], na.rm = TRUE),
      max_outage_prob  = suppressWarnings(max(outage_prob[is_undet], na.rm = TRUE)),
      any_likely_outage = any(likely_outage, na.rm = TRUE),
      any_high_conf     = any(high_conf_outage, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::mutate(
      mean_outage_prob = ifelse(is.nan(mean_outage_prob), NA_real_, mean_outage_prob),
      max_outage_prob  = ifelse(is.finite(max_outage_prob), max_outage_prob, NA_real_),
      # "compromised" = a credible outage footprint in this L14 tile: either a
      # high share of children undetected, or a high mean outage probability.
      compromised = (frac_undetected >= frac_hi) |
                    (!is.na(mean_outage_prob) & mean_outage_prob >= tau))
}

# ---------------------------------------------------------------------------
# triangulate_population() — the 2x2 that answers "is the population drop real?"
# WHY: classify every population tile-day on two INDEPENDENT axes — did the
#      population fall (Meta z_score), and is coverage compromised (NC). The
#      CONFOUNDED cell is where a population drop co-occurs with an outage: the
#      drop may be an artifact, not evacuation. GENUINE_DROP is the reassuring
#      cell (drop with coverage intact). Everything is attached at the exact
#      L14 quadkey/ds join.
#' @param fb_data_bing tibble/sf population (needs quadkey, ds, z_score,
#'                      n_difference/percent_change). sf ok (geometry dropped).
#' @param nc_l14       tibble from nc_rollup_to_l14().
#' @param z_drop       numeric z_score threshold for a material drop (default pop_drop_z).
#' @return tibble: population rows + coverage cols + `quadrant` factor
#'         (GENUINE_DROP / CONFOUNDED / STABLE_COVERED / MASKED_OR_RESILIENT /
#'         UNKNOWN_COVERAGE).
# ---------------------------------------------------------------------------
triangulate_population <- function(fb_data_bing, nc_l14, z_drop = pop_drop_z) {
  pop <- fb_data_bing
  if (inherits(pop, "sf")) pop <- sf::st_drop_geometry(pop)
  need <- c("quadkey", "ds")
  if (!all(need %in% names(pop)))
    stop("triangulate_population(): population needs columns quadkey + ds.")
  pop$quadkey <- .nc_pad_quadkey(pop$quadkey, Z_POP)   # restore any dropped leading zeros
  # z_score is Meta's winsorized standardized change (robust); fall back to
  # percent_change if z_score is absent.
  has_z <- "z_score" %in% names(pop)
  pop <- dplyr::mutate(pop, ds = as.Date(ds),
                       drop_metric = if (has_z) z_score else percent_change / 100)
  z_thr <- if (has_z) z_drop else (z_drop * 0.2)   # z<=-2, or ~ -40% if only pct

  joined <- dplyr::left_join(pop, nc_l14, by = c("quadkey" = "qk14", "ds"))
  joined |>
    dplyr::mutate(
      pop_drop      = !is.na(drop_metric) & drop_metric <= z_thr,
      has_coverage_read = !is.na(n_l16),
      quadrant = factor(dplyr::case_when(
        !has_coverage_read        ~ "UNKNOWN_COVERAGE",
        pop_drop  &  compromised  ~ "CONFOUNDED",
        pop_drop  & !compromised  ~ "GENUINE_DROP",
        !pop_drop &  compromised  ~ "MASKED_OR_RESILIENT",
        TRUE                      ~ "STABLE_COVERED"),
        levels = names(NC_PAL_QUAD)))
}

# ---------------------------------------------------------------------------
# triangulate_movement() — flag flows whose endpoints lost coverage.
# WHY: a flow's ORIGIN losing coverage suppresses observed outflow (users go dark
#      and are not assigned to any vector) -> evacuation egress is UNDER-counted;
#      a DESTINATION losing coverage under-counts intake. We attach the L14
#      coverage state to both endpoints and mark the direction of the likely bias.
#' @param moved  tibble/sf movement (needs start_quadkey, end_quadkey, ds).
#' @param nc_l14 tibble from nc_rollup_to_l14().
#' @return tibble: movement rows + origin_/dest_ coverage cols + `flow_flag`
#'         (ORIGIN_OUTAGE / DEST_OUTAGE / BOTH_OUTAGE / CLEAR / UNKNOWN).
# ---------------------------------------------------------------------------
triangulate_movement <- function(moved, nc_l14) {
  mv <- moved
  if (inherits(mv, "sf")) mv <- sf::st_drop_geometry(mv)
  need <- c("start_quadkey", "end_quadkey", "ds")
  if (!all(need %in% names(mv)))
    stop("triangulate_movement(): movement needs start_quadkey + end_quadkey + ds.")
  mv$start_quadkey <- .nc_pad_quadkey(mv$start_quadkey, Z_POP)   # restore dropped leading zeros
  mv$end_quadkey   <- .nc_pad_quadkey(mv$end_quadkey,   Z_POP)
  mv <- dplyr::mutate(mv, ds = as.Date(ds))
  o <- nc_l14 |> dplyr::select(qk14, ds, o_compromised = compromised,
                               o_mean_outage = mean_outage_prob, o_n_l16 = n_l16)
  d <- nc_l14 |> dplyr::select(qk14, ds, d_compromised = compromised,
                               d_mean_outage = mean_outage_prob, d_n_l16 = n_l16)
  mv |>
    dplyr::left_join(o, by = c("start_quadkey" = "qk14", "ds")) |>
    dplyr::left_join(d, by = c("end_quadkey"   = "qk14", "ds")) |>
    dplyr::mutate(
      o_read = !is.na(o_n_l16), d_read = !is.na(d_n_l16),
      flow_flag = factor(dplyr::case_when(
        !o_read & !d_read                                   ~ "UNKNOWN",
        (o_compromised %in% TRUE) & (d_compromised %in% TRUE) ~ "BOTH_OUTAGE",
        o_compromised %in% TRUE                              ~ "ORIGIN_OUTAGE",
        d_compromised %in% TRUE                              ~ "DEST_OUTAGE",
        TRUE                                                ~ "CLEAR"),
        levels = c("ORIGIN_OUTAGE", "DEST_OUTAGE", "BOTH_OUTAGE", "CLEAR", "UNKNOWN")))
}

# ---------------------------------------------------------------------------
# nc_skew_summary() — the HEADLINE number for the report.
# WHY: quantifies how much of the observed population DECLINE mass is co-located
#      with a credible outage (the CONFOUNDED share) vs. drops with intact
#      coverage (GENUINE). A small confounded share => the population/movement
#      signal is NOT substantially skewed by coverage loss; a large share =>
#      caveat the mobility read for those tiles/days.
#' @param tri_pop tibble from triangulate_population() (must carry n_difference).
#' @return tibble per ds (+ an "ALL" row): n_drop_tiles, n_confounded_tiles
#'         (DISTINCT tiles that day), decline_mass, confounded_decline_mass
#'         (summed over 8-hour windows -> person-window units), confounded_share.
#' @details Population is 8-hourly and the coverage state is daily, so a tile
#'   appears in up to 3 rows/day sharing one coverage flag. Tile COUNTS are
#'   de-duplicated (n_distinct); decline MASS is summed over windows. The share
#'   is a within-consistent ratio (num & denom summed identically), so it is
#'   robust to the windowing.
# ---------------------------------------------------------------------------
nc_skew_summary <- function(tri_pop) {
  mass_col <- if ("n_difference" %in% names(tri_pop)) "n_difference" else
              if ("Difference between baseline and crisis" %in% names(tri_pop))
                "Difference between baseline and crisis" else NA_character_
  if (is.na(mass_col))
    stop("nc_skew_summary(): need n_difference (or the renamed column) for decline mass.")
  per <- tri_pop |>
    dplyr::mutate(decline = pmax(0, -as.numeric(.data[[mass_col]])),  # magnitude of loss
                  is_conf = quadrant == "CONFOUNDED",
                  is_drop = quadrant %in% c("CONFOUNDED", "GENUINE_DROP")) |>
    dplyr::group_by(ds) |>
    dplyr::summarise(
      n_drop_tiles            = dplyr::n_distinct(quadkey[is_drop]),  # distinct tiles
      n_confounded_tiles      = dplyr::n_distinct(quadkey[is_conf]),
      decline_mass            = sum(decline[is_drop], na.rm = TRUE),  # person-windows
      confounded_decline_mass = sum(decline[is_conf], na.rm = TRUE),
      .groups = "drop") |>
    dplyr::mutate(confounded_share = ifelse(decline_mass > 0,
                    confounded_decline_mass / decline_mass, NA_real_))
  allr <- per |>
    dplyr::summarise(ds = as.Date(NA),
                     dplyr::across(c(n_drop_tiles, n_confounded_tiles,
                                     decline_mass, confounded_decline_mass), sum),
                     .groups = "drop") |>
    dplyr::mutate(confounded_share = ifelse(decline_mass > 0,
                    confounded_decline_mass / decline_mass, NA_real_))
  dplyr::bind_rows(per, allr)
}

# ---------------------------------------------------------------------------
# nc_movement_skew_summary() — the MOVEMENT analogue of nc_skew_summary().
# WHY: a flow whose ORIGIN lost coverage is under-counted — users go dark and are
#      never assigned to a vector — so evacuation EGRESS reads smaller than it is.
#      We weight each flow by its user volume and report two shares:
#        (a) suspect_volume_share = share of OBSERVED movement volume that touches
#            an outage at either endpoint (how much of the flow signal is suspect);
#        (b) origin_confounded_share = the direct parallel to the population
#            metric — share of the flow DECLINE (vs pre-crisis baseline) whose
#            ORIGIN is compromised — an upper bound on coverage-suppressed egress.
#      Origin drives egress under-counting, so the decline share keys on the
#      origin; the volume share flags either endpoint (dest outage under-counts
#      intake).
#' @param tri_mv tibble from triangulate_movement(); carries the user count and
#'        the baseline difference under EITHER the raw (n_crisis / n_difference)
#'        or the build_moved()-renamed (`# Users During Crisis` /
#'        `Difference between baseline and crisis`) column names.
#' @return tibble per ds (+ an "ALL" row): flow_mass, mass_origin_outage,
#'         mass_dest_outage, mass_any_outage, suspect_volume_share, decline_mass,
#'         origin_confounded_decline_mass, origin_confounded_share.
#' @examples nc_movement_skew_summary(triangulate_movement(moved, nc_l14))
# ---------------------------------------------------------------------------
nc_movement_skew_summary <- function(tri_mv) {
  users_col <- if ("# Users During Crisis" %in% names(tri_mv)) "# Users During Crisis" else
               if ("n_crisis" %in% names(tri_mv)) "n_crisis" else NA_character_
  diff_col  <- if ("Difference between baseline and crisis" %in% names(tri_mv))
                 "Difference between baseline and crisis" else
               if ("n_difference" %in% names(tri_mv)) "n_difference" else NA_character_
  if (is.na(users_col))
    stop("nc_movement_skew_summary(): need `# Users During Crisis` or n_crisis for flow volume.")
  x <- tri_mv |>
    dplyr::mutate(
      w_users   = as.numeric(.data[[users_col]]),
      w_decline = if (is.na(diff_col)) NA_real_ else pmax(0, -as.numeric(.data[[diff_col]])),
      o_out     = o_compromised %in% TRUE,     # origin coverage compromised that day
      d_out     = d_compromised %in% TRUE,     # destination compromised
      any_out   = o_out | d_out)
  agg <- function(df) dplyr::summarise(df,
      flow_mass          = sum(w_users, na.rm = TRUE),
      mass_origin_outage = sum(w_users[o_out], na.rm = TRUE),
      mass_dest_outage   = sum(w_users[d_out], na.rm = TRUE),
      mass_any_outage    = sum(w_users[any_out], na.rm = TRUE),
      decline_mass       = sum(w_decline, na.rm = TRUE),
      origin_confounded_decline_mass = sum(w_decline[o_out], na.rm = TRUE),
      .groups = "drop")
  shares <- function(df) dplyr::mutate(df,
      suspect_volume_share    = ifelse(flow_mass > 0, mass_any_outage / flow_mass, NA_real_),
      origin_confounded_share = ifelse(decline_mass > 0,
                                  origin_confounded_decline_mass / decline_mass, NA_real_))
  per  <- x |> dplyr::group_by(ds) |> agg() |> shares()
  allr <- x |> agg() |> dplyr::mutate(ds = as.Date(NA)) |> shares()
  dplyr::bind_rows(per, allr)
}

# ============================================================================
# MAPS  (match the population plot style; each RETURNS a ggplot)
# ============================================================================

# Reuse the population pipeline's basemap-cache dir + clip helper when present;
# else define minimal equivalents so maps work standalone.
if (!exists("tile_cache_dir")) tile_cache_dir <- "maptiles_cache"
if (!exists("clip_sf_to_box")) {
  clip_sf_to_box <- function(layer, xlim, ylim) {
    if (is.null(layer) || !inherits(layer, "sf") || nrow(layer) == 0) return(layer)
    if (is.null(xlim) || is.null(ylim)) return(layer)
    bb <- sf::st_bbox(c(xmin = min(xlim), xmax = max(xlim),
                        ymin = min(ylim), ymax = max(ylim)), crs = sf::st_crs(3857))
    tryCatch(suppressWarnings(sf::st_crop(sf::st_transform(layer, 3857), bb)),
             error = function(e) layer)
  }
}

# Internal: attach the day's status/attrs to the tile polygons + basemap, in the
# report's EPSG:3857 / theme_minimal / bottom-legend idiom. fires is OPTIONAL.
.nc_base_map <- function(layer, fill_aes, scale, plot_ds, fires = NULL, zoom = 9,
                         title = NULL) {
  if (nrow(layer) == 0)
    stop(".nc_base_map(): nothing to draw for ds=", plot_ds)
  osm <- tryCatch(
    maptiles::get_tiles(layer, provider = "CartoDB.Voyager", crop = TRUE,
                        zoom = zoom, cachedir = tile_cache_dir),
    error = function(e) { warning("Basemap failed (", conditionMessage(e),
                                  "); drawing without it."); NULL })
  p <- ggplot()
  if (!is.null(osm)) p <- p + ggspatial::layer_spatial(osm)
  p <- p + geom_sf(data = layer, fill_aes, color = "white", linewidth = 0.08) + scale
  if (!is.null(fires) && inherits(fires, "sf") && nrow(fires) > 0) {
    bb <- sf::st_bbox(layer)
    p <- p + geom_sf(data = clip_sf_to_box(fires, c(bb["xmin"], bb["xmax"]),
                                                  c(bb["ymin"], bb["ymax"])),
                     aes(color = "Fire Perimeter"), fill = NA, linewidth = 0.5) +
             scale_color_manual(values = c("Fire Perimeter" = "black"), name = NULL)
  }
  p + coord_sf(crs = sf::st_crs(3857), expand = FALSE) +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), legend.position = "bottom",
          legend.key.width = unit(1.2, "cm")) +
    ggtitle(title %||% paste("Network coverage —", plot_ds))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# Internal: join one day's per-tile attributes onto the tile polygons.
.nc_day_layer <- function(nc_status, tiles, plot_ds) {
  d <- dplyr::filter(nc_status, as.character(ds) == as.character(plot_ds))
  if (nrow(d) == 0)
    stop("no NC data for ds=", plot_ds, ". Available: ",
         paste(sort(unique(as.character(nc_status$ds))), collapse = ", "))
  dplyr::inner_join(tiles, d, by = "quadkey")
}

#' nc_coverage_map() — categorical COVERED / UNDETECTED / UNCERTAIN / NO_BASELINE.
#' @return ggplot. @examples nc_coverage_map(nc_status, nc_tiles, "2026-08-06", fires=fires)
nc_coverage_map <- function(nc_status, tiles, plot_ds, fires = NULL, zoom = 9) {
  layer <- .nc_day_layer(nc_status, tiles, plot_ds)
  .nc_base_map(layer, aes(fill = status),
               scale_fill_manual(values = NC_PAL_STATUS, drop = FALSE, name = "Connectivity"),
               plot_ds, fires, zoom,
               title = paste("Cellular connectivity —", plot_ds))
}

#' nc_outage_prob_map() — continuous outage_prob over undetected tiles.
#' @return ggplot. NA outage_prob drawn a distinct grey.
nc_outage_prob_map <- function(nc_status, tiles, plot_ds, fires = NULL, zoom = 9) {
  layer <- .nc_day_layer(nc_status, tiles, plot_ds) |>
    dplyr::filter(is_undet %in% TRUE)
  sc <- tryCatch(
    viridis::scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1),
                                  na.value = "grey80", name = "Outage probability"),
    error = function(e) scale_fill_gradient(low = "#fee5d9", high = "#a50f15",
                                  limits = c(0, 1), na.value = "grey80",
                                  name = "Outage probability"))
  .nc_base_map(layer, aes(fill = outage_prob), sc, plot_ds, fires, zoom,
               title = paste("Outage probability (undetected tiles) —", plot_ds))
}

#' nc_change_map() — NEW_UNDETECTED / RECOVERED / PERSISTENT on a given day.
#' @return ggplot. @param nc_change tibble from nc_change().
nc_change_map <- function(nc_change, tiles, plot_ds, fires = NULL, zoom = 9) {
  layer <- .nc_day_layer(nc_change, tiles, plot_ds)
  .nc_base_map(layer, aes(fill = change),
               scale_fill_manual(values = NC_PAL_CHANGE, drop = FALSE, name = "Change vs. prior day"),
               plot_ds, fires, zoom,
               title = paste("Connectivity change —", plot_ds))
}

#' nc_timeseries_plot() — undetected area/count + mean outage prob over the event.
#' @return ggplot. @param ts tibble from nc_timeseries_df().
nc_timeseries_plot <- function(ts) {
  has_area <- any(!is.na(ts$area_undetected_km2))
  yleft <- if (has_area) ts$area_undetected_km2 else ts$n_undetected
  ylab  <- if (has_area) "Undetected area (km²)" else "Undetected tiles (n)"
  ggplot(ts, aes(ds)) +
    geom_col(aes(y = yleft, fill = "Undetected footprint"), alpha = 0.6) +
    geom_line(aes(y = mean_outage_prob * max(yleft, na.rm = TRUE),
                  color = "Mean outage prob"), linewidth = 1) +
    scale_y_continuous(name = ylab,
      sec.axis = sec_axis(~ . / max(yleft, na.rm = TRUE), name = "Mean outage probability")) +
    scale_fill_manual(values = c("Undetected footprint" = "#D55E00"), name = NULL) +
    scale_color_manual(values = c("Mean outage prob" = "#7A0177"), name = NULL) +
    labs(x = NULL, title = "Cellular outage footprint over the event") +
    theme_minimal() + theme(legend.position = "bottom")
}

#' triangulation_map() — the 2x2 quadrant per population tile on a given day.
#' WHY: shows WHERE population drops are trustworthy (GENUINE_DROP) vs suspect
#'      (CONFOUNDED). Needs the population tile polygons (tiles_3857 from
#'      build_tiles()) so it draws at level 14.
#' @param tri_pop tibble from triangulate_population().
#' @param pop_tiles sf level-14 polygons with a `quadkey` column (build_tiles()).
#' @return ggplot.
triangulation_map <- function(tri_pop, pop_tiles, plot_ds, plot_hour = NULL,
                              fires = NULL, zoom = 9) {
  d <- dplyr::filter(tri_pop, as.character(ds) == as.character(plot_ds))
  if (!is.null(plot_hour) && "hour" %in% names(d)) d <- dplyr::filter(d, hour == plot_hour)
  if (nrow(d) == 0) stop("triangulation_map(): no rows for ds=", plot_ds,
                         if (!is.null(plot_hour)) paste0(", hour=", plot_hour))
  d <- dplyr::distinct(d, quadkey, .keep_all = TRUE)          # one polygon per tile
  layer <- dplyr::inner_join(pop_tiles["quadkey"], d, by = "quadkey")
  .nc_base_map(layer, aes(fill = quadrant),
               scale_fill_manual(values = NC_PAL_QUAD, drop = FALSE,
                                 name = "Population-drop × coverage"),
               plot_ds, fires, zoom,
               title = paste("Is the population change coverage-confounded? —", plot_ds))
}

# ============================================================================
# QA / SANITY  (reuse the existing suite's philosophy)
# ============================================================================

# ---------------------------------------------------------------------------
# nc_sanity_check() — readable per-dataset health report for the NC pipeline.
# WHY: same role as sanity_check() in 1_data_cleaning*: catch parse/logic bugs
#      before they reach a map. Adds NC-specific invariants (value ranges,
#      active∩undetected≈∅, level-16 tile size, singleton count).
#' @param nc_daily tibble from load_nc(); @param nc_status optional from nc_classify().
#' @return invisibly, a list of the computed findings.
# ---------------------------------------------------------------------------
nc_sanity_check <- function(nc_daily, nc_status = NULL, tiles = NULL, name = "nc") {
  cat("\n==================== NC SANITY CHECK:", name, "====================\n")
  cat(sprintf("rows: %s   distinct tiles: %s   dates: %s\n",
      format(nrow(nc_daily), big.mark = ","),
      format(dplyr::n_distinct(nc_daily$quadkey), big.mark = ","),
      paste(sort(unique(as.character(nc_daily$ds))), collapse = ", ")))

  # value ranges
  if ("p_connectivity" %in% names(nc_daily)) {
    pr <- range(nc_daily$p_connectivity, na.rm = TRUE)
    cat(sprintf("  [%s] p_connectivity in [%.3f, %.3f] (expect [0,1])\n",
                ifelse(pr[1] >= -1e-9 && pr[2] <= 1 + 1e-9, "OK", "FAIL"), pr[1], pr[2]))
  }
  for (cc in intersect(c("active", "undetected"), names(nc_daily))) {
    vals <- unique(stats::na.omit(nc_daily[[cc]]))
    cat(sprintf("  [%s] %-11s values = {%s} (expect {1})\n",
                ifelse(all(vals == 1L), "OK", "WARN"), cc, paste(vals, collapse = ",")))
  }
  # quadkey length == 16
  ln <- unique(nchar(nc_daily$quadkey))
  cat(sprintf("  [%s] quadkey length = {%s} (expect %d)\n",
              ifelse(identical(ln, Z_NC) || all(ln == Z_NC), "OK", "FAIL"),
              paste(ln, collapse = ","), Z_NC))
  # coordinate sanity
  cat(sprintf("  [%s] lon in [%.3f, %.3f], lat in [%.3f, %.3f]\n",
      ifelse(min(nc_daily$lon, na.rm = TRUE) >= -180 && max(nc_daily$lon, na.rm = TRUE) <= 180 &&
             min(nc_daily$lat, na.rm = TRUE) >= -90  && max(nc_daily$lat, na.rm = TRUE) <= 90, "OK", "FAIL"),
      min(nc_daily$lon, na.rm = TRUE), max(nc_daily$lon, na.rm = TRUE),
      min(nc_daily$lat, na.rm = TRUE), max(nc_daily$lat, na.rm = TRUE)))

  # active ∩ undetected per day ≈ ∅ (the core mutual-exclusivity invariant)
  conflict <- nc_daily |>
    dplyr::filter(!is.na(active) & active == 1L, !is.na(undetected) & undetected == 1L)
  cat(sprintf("  [%s] active ∩ undetected (same tile-day): %d rows (expect 0)\n",
              ifelse(nrow(conflict) == 0, "OK", "FAIL"), nrow(conflict)))

  if (!is.null(tiles)) {
    ar <- range(tiles$area_km2, na.rm = TRUE)
    # level-16 is ~600 m/side near the equator (~0.36 km²); at Spokane's ~47.5N
    # the ground tile is smaller (~0.15–0.2 km²). Flag only if wildly off.
    cat(sprintf("  [%s] level-16 tile area km² in [%.3f, %.3f] (expect ~0.1–0.4)\n",
                ifelse(ar[1] > 0.05 && ar[2] < 1, "OK", "WARN"), ar[1], ar[2]))
  }
  if (!is.null(nc_status)) {
    tab <- table(nc_status$status)
    cat("  status counts: ", paste(sprintf("%s=%d", names(tab), tab), collapse = "  "), "\n")
    if ("outage_confidence" %in% names(nc_status))
      cat(sprintf("  singleton (low-confidence) undetected tiles: %d\n",
                  sum(nc_status$outage_confidence == "low", na.rm = TRUE)))
  }
  cat("=================================================================\n")
  invisible(list(name = name, conflicts = nrow(conflict), qk_len = ln))
}

# ---------------------------------------------------------------------------
# compare_nc_days() — analog of compare_datasets() for two NC dates.
# WHY: what changed between two days — tiles that went undetected, recovered, or
#      flipped — with a one-line verdict.
#' @param nc_status tibble from nc_classify(); @param d1,d2 chr dates.
#' @return invisibly a findings list; prints added/removed/flipped counts.
# ---------------------------------------------------------------------------
compare_nc_days <- function(nc_status, d1, d2) {
  s <- function(dd) nc_status |> dplyr::filter(as.character(ds) == dd) |>
                    dplyr::select(quadkey, status)
  a <- s(d1); b <- s(d2)
  cat("\n#################### COMPARE NC:", d1, "->", d2, "####################\n")
  cat(sprintf("tiles  %s=%d  %s=%d\n", d1, nrow(a), d2, nrow(b)))
  j <- dplyr::full_join(a, b, by = "quadkey", suffix = c(".1", ".2"))
  new_undet <- sum(j$status.1 %in% "COVERED" & j$status.2 %in% c("UNDETECTED", "UNCERTAIN"), na.rm = TRUE)
  recovered <- sum(j$status.1 %in% c("UNDETECTED", "UNCERTAIN") & j$status.2 %in% "COVERED", na.rm = TRUE)
  appeared  <- sum(is.na(j$status.1) & !is.na(j$status.2))
  vanished  <- sum(!is.na(j$status.1) & is.na(j$status.2))
  cat(sprintf("  NEW undetected: %d   recovered: %d   tiles appeared: %d   vanished: %d\n",
              new_undet, recovered, appeared, vanished))
  cat(sprintf("  VERDICT: %s\n", if (new_undet > recovered) "coverage DEGRADING"
              else if (recovered > new_undet) "coverage RECOVERING" else "roughly stable"))
  cat("############################################################\n")
  invisible(list(new_undetected = new_undet, recovered = recovered))
}

# ---------------------------------------------------------------------------
# nc_quadkey_consistency_check() — THE cross-dataset logic check the project
# asked for: does our lon/lat->quadkey reproduce Meta's own quadkeys?
# WHY: proves the NC level-16 keys will nest correctly under the population
#      level-14 tiles. We recompute the population's OWN level-14 quadkey from its
#      tile-centre lon/lat and require an EXACT string match. If this passes, the
#      hierarchical join (str_sub(qk16,1,14)) is trustworthy.
#' @param fb_data_bing population tibble with quadkey + latitude + longitude.
#' @param n integer sample size (default: all rows).
#' @return invisibly the mismatch rate; stops loudly if any mismatch.
# ---------------------------------------------------------------------------
nc_quadkey_consistency_check <- function(fb_data_bing, n = NULL) {
  pop <- fb_data_bing
  if (inherits(pop, "sf")) pop <- sf::st_drop_geometry(pop)
  stopifnot(all(c("quadkey", "latitude", "longitude") %in% names(pop)))
  pop <- dplyr::distinct(pop, quadkey, latitude, longitude)
  if (!is.null(n) && n < nrow(pop)) pop <- pop[seq_len(n), ]
  # Restore leading zeros a numeric CSV parse may have dropped. Pad to the fixed
  # population level (Z_POP=14): every Spokane quadkey starts with "0", so a
  # UNIFORM loss would leave max(nchar)=13 and a max-width pad would be a no-op —
  # padding to the known width recovers it correctly.
  pop$quadkey <- .nc_pad_quadkey(pop$quadkey, Z_POP)
  lvl <- unique(nchar(pop$quadkey))
  stopifnot("population quadkeys are not a single level" = length(lvl) == 1)
  mine <- nc_quadkey_from_lonlat(pop$longitude, pop$latitude, zoom = lvl)
  ok <- mine == pop$quadkey
  cat(sprintf("\nnc_quadkey_consistency_check: %d/%d population level-%d quadkeys reproduced exactly (%.1f%%)\n",
              sum(ok), length(ok), lvl, 100 * mean(ok)))
  if (!all(ok)) {
    bad <- utils::head(data.frame(theirs = pop$quadkey[!ok], mine = mine[!ok],
                                  lon = pop$longitude[!ok], lat = pop$latitude[!ok]), 5)
    print(bad)
    stop("nc_quadkey_consistency_check(): quadkey math does NOT match Meta — do ",
         "not trust the L16/L14 join until this is resolved.")
  }
  invisible(1 - mean(ok))
}

# ============================================================================
# COLAB PLOTTING SUGGESTIONS — param sweeps (see also the notebook)
#   These RETURN named lists of ggplots so a Colab cell can print or ggsave them
#   in a loop. Each is cheap; the heavy pipeline objects are built once upstream.
# ============================================================================

# Sweep a map over EVERY available date (the "small multiples" the report wants).
# @examples ms <- nc_sweep_dates(nc_coverage_map, nc_status, nc_tiles, fires=fires)
#           for (nm in names(ms)) print(ms[[nm]])
nc_sweep_dates <- function(map_fn, x, tiles, fires = NULL, zoom = 9) {
  ds_all <- sort(unique(as.character(x$ds)))
  stats::setNames(lapply(ds_all, function(dd)
    tryCatch(map_fn(x, tiles, dd, fires = fires, zoom = zoom),
             error = function(e) { message("skip ", dd, ": ", conditionMessage(e)); NULL })),
    paste0("ds_", ds_all))
}

# Sweep the outage threshold tau to see how the "likely outage" footprint reacts
# (sensitivity analysis — is the confounded share robust to tau?).
# @examples sw <- nc_sweep_tau(nc_daily, tiles, fb_data_bing, taus = c(.3,.5,.7))
nc_sweep_tau <- function(nc_daily, tiles, fb_data_bing = NULL,
                         taus = c(0.3, 0.5, 0.7), z_drop = pop_drop_z) {
  lapply(stats::setNames(taus, paste0("tau_", taus)), function(tt) {
    st  <- nc_classify(nc_daily, tiles, tau = tt)
    l14 <- nc_rollup_to_l14(st, tau = tt)
    if (is.null(fb_data_bing)) return(l14)
    tri <- triangulate_population(fb_data_bing, l14, z_drop = z_drop)
    nc_skew_summary(tri)                       # headline share per ds at this tau
  })
}

# Sweep the cluster size k for the denoiser (how many outages survive as "real").
nc_sweep_k <- function(nc_status, tiles, ks = c(2L, 3L, 5L)) {
  lapply(stats::setNames(ks, paste0("k_", ks)), function(kk) {
    dn <- nc_denoise(nc_status, tiles, k = kk)
    dn |> dplyr::filter(is_undet %in% TRUE) |>
      dplyr::count(ds, outage_confidence)
  })
}

# Save every plot in a named list to outputs/ (reuse save_plot() if 1_ is loaded).
nc_save_all <- function(plots, prefix = "nc", dir = "outputs", width = 8, height = 8, dpi = 300) {
  saver <- if (exists("save_plot")) save_plot else function(p, name, dir, ...) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(file.path(dir, name), p, width = width, height = height, dpi = dpi) }
  for (nm in names(plots)) if (!is.null(plots[[nm]]))
    saver(plots[[nm]], sprintf("%s_%s.png", prefix, nm), dir = dir)
  invisible(dir)
}

# ----------------------------------------------------------------------------
# COLAB CELL RECIPE (copy each block into its own cell; mirrors colab_Spokane*).
# ----------------------------------------------------------------------------
# # Cell 1 — install (once):
# #   system("apt-get -qq install -y libudunits2-dev libgdal-dev")   # sf/terra system libs
# #   install.packages(c("sf","dplyr","tidyr","readr","stringr","ggplot2",
# #                      "quadkeyr","igraph","maptiles","ggspatial","viridis"))
# # Cell 2 — defs:      source("3_network_coverage_spokane_fires.R")
# # Cell 3 — (optional) source("2_geofilter_spokane_fires.R")   # so global `aoi` matches
# # Cell 4 — pipeline (one heavy step per cell if you prefer):
# #   nc_daily  <- load_nc(); nc_tiles <- build_nc_tiles(nc_daily)
# #   nc_status <- nc_denoise(nc_classify(nc_daily, nc_tiles), nc_tiles)
# #   nc_chg    <- nc_change(nc_status); nc_ts <- nc_timeseries_df(nc_status, nc_tiles, nc_chg)
# # Cell 5 — QA:        nc_sanity_check(nc_daily, nc_status, nc_tiles)
# # Cell 6 — MANY-PARAM MAP SWEEP (small multiples over every date):
# #   maps <- nc_sweep_dates(nc_coverage_map, nc_status, nc_tiles, fires = get0("fires"))
# #   for (nm in names(maps)) print(maps[[nm]]);  nc_save_all(maps, "coverage")
# #   pmaps <- nc_sweep_dates(nc_outage_prob_map, nc_status, nc_tiles); nc_save_all(pmaps, "outageprob")
# # Cell 7 — SENSITIVITY SWEEP (does the confounded share survive tau & k choices?):
# #   nc_sweep_tau(nc_daily, nc_tiles, get0("fb_data_bing"), taus = c(.3,.4,.5,.6,.7))
# #   nc_sweep_k(nc_status, nc_tiles, ks = c(2L,3L,4L,5L))
# # Cell 8 — TRIANGULATION (needs fb_data_bing/moved + tiles_3857 from 1_ & 2_):
# #   nc_quadkey_consistency_check(fb_data_bing)          # gate: must pass
# #   nc_l14 <- nc_rollup_to_l14(nc_status)
# #   tri <- triangulate_population(fb_data_bing, nc_l14); print(nc_skew_summary(tri))
# #   for (dd in sort(unique(as.character(tri$ds))))
# #     print(triangulation_map(tri, tiles_3857, dd, fires = get0("fires")))
# # Cell 9 — MOVEMENT skew (mass-weighted egress suppression):
# #   tri_mv <- triangulate_movement(moved, nc_l14)
# #   print(nc_movement_skew_summary(tri_mv))          # suspect volume + origin-confounded shares
# ============================================================================

# ============================================================================
# END-TO-END EXAMPLE  (define-only on source; runs only when test_it is TRUE)
# ============================================================================
test_it <- FALSE
if (test_it) {
  # --- Stages 1-5 (standalone product) ---------------------------------------
  nc_daily  <- load_nc()                                # Stage 1
  nc_tiles  <- build_nc_tiles(nc_daily)                 # Stage 2 (AOI-clipped)
  nc_status <- nc_classify(nc_daily, nc_tiles)          # Stage 3
  nc_chg    <- nc_change(nc_status)                     # Stage 4
  nc_status <- nc_denoise(nc_status, nc_tiles)          # Stage 5 (adds confidence)
  nc_ts     <- nc_timeseries_df(nc_status, nc_tiles, nc_chg)

  nc_sanity_check(nc_daily, nc_status, nc_tiles)        # QA
  print(nc_coverage_map(nc_status, nc_tiles, sort(unique(as.character(nc_status$ds)))[1]))
  print(nc_timeseries_plot(nc_ts))

  # --- Stage 6 (triangulation) — only if the population/movement objects exist -
  if (exists("fb_data_bing")) {
    nc_quadkey_consistency_check(fb_data_bing)          # MUST pass before trusting the join
    nc_l14  <- nc_rollup_to_l14(nc_status)
    tri_pop <- triangulate_population(fb_data_bing, nc_l14)
    print(nc_skew_summary(tri_pop))
    if (exists("tiles_3857"))
      print(triangulation_map(tri_pop, tiles_3857,
                              sort(unique(as.character(tri_pop$ds)))[1]))
    if (exists("moved")) {
      tri_mv <- triangulate_movement(moved, nc_l14)
      print(dplyr::count(tri_mv, ds, flow_flag))
      print(nc_movement_skew_summary(tri_mv))   # mass-weighted movement skew
    }
  }
}

# ============================================================================
# [CHECK_ANALYSIS]  (interpretive; numbers must be recomputed from YOUR data)
# ----------------------------------------------------------------------------
# The triangulation answers one question for the EM reader: are the Population
# and Movement signals in this event driven by people moving, or by cellular
# coverage dropping? Read nc_skew_summary(tri_pop)$confounded_share:
#   * share ≈ 0  -> population declines are NOT co-located with credible outages;
#                   the mobility signal is trustworthy — report it as evacuation.
#   * share large-> a material fraction of the "decline" sits under an outage;
#                   caveat those tiles/days (the drop may be phones going dark).
# CAVEATS to state in prose (do not delete):
#   1. `undetected` shares its ping source with the population count, so it is a
#      biased explainer; the p_connectivity-based `outage_prob` / `compromised`
#      flag is the primary evidence. <!-- TODO(author): cite Meta NC doc §Probability -->
#   2. NC is DAILY; Population/Movement are 8-hourly — a daily coverage state is
#      applied to all three windows of that day (a coarsening, stated as such).
#   3. Temporal overlap here is thin (NC ~3 days vs the event window); treat the
#      confounded share as indicative, not definitive. [PROSE PLACEHOLDER — overlap]
#   4. Level-14 tiles with no observed level-16 NC child are UNKNOWN_COVERAGE, not
#      "covered" — they are excluded from the skew denominator, not counted clean.
# ============================================================================
