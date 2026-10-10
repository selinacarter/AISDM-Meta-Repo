# # ==============================================================================
# # 3_hurricane_functions.R
# #
# # Reusable hurricane overlay for population_plot().
# #
# # Adds:
# #   * cached shaded-relief terrain
# #   * cached rivers and major roads with Overpass failover
# #   * observed NHC/CPHC best track up to the map time
# #   * forecast track in a DIFFERENT style (red dashed + diamonds)
# #   * optional NHC forecast cone KMZ
# #   * city markers + pointer labels
# #
# # The script can be sourced directly (defines functions only), or sourced from
# # inside population_plot(disaster_type = "hurricane"), where it auto-applies.
# # ==============================================================================
# 
# suppressPackageStartupMessages({
#   require(sf)
#   require(terra)
#   require(elevatr)
#   require(osmdata)
#   require(ggplot2)
#   require(lubridate)
#   require(jsonlite)
# })
# 
# # ------------------------------------------------------------------------------
# # NOAA / NHC ATCF helpers adapted from hurricane_tracker.R
# # ------------------------------------------------------------------------------
# 
# NHC_JSON  <- "https://www.nhc.noaa.gov/CurrentStorms.json"
# ATCF_BTK  <- "https://ftp.nhc.noaa.gov/atcf/btk/"
# ATCF_ARCH <- "https://ftp.nhc.noaa.gov/atcf/archive/"
# ATCF_AIDS <- "https://ftp.nhc.noaa.gov/atcf/aid_public/"
# 
# 
# # Saffir-Simpson / tropical-cyclone intensity categories.
# # Same breakpoints and display colors used in hurricane_tracker.R.
# SS_LEVELS <- c(
#   "Tropical Depression",
#   "Tropical Storm",
#   "Category 1",
#   "Category 2",
#   "Category 3",
#   "Category 4",
#   "Category 5"
# )
# 
# SS_COLORS <- c(
#   "Tropical Depression" = "#5EBAFF",
#   "Tropical Storm"      = "#00FAF4",
#   "Category 1"          = "#FFF795",
#   "Category 2"          = "#FFD821",
#   "Category 3"          = "#FF8F20",
#   "Category 4"          = "#FF6060",
#   "Category 5"          = "#C464D9"
# )
# 
# saffir_simpson <- function(wind_kt) {
#   cut(
#     wind_kt,
#     breaks = c(
#       -Inf,
#       34,
#       64,
#       83,
#       96,
#       113,
#       137,
#       Inf
#     ),
#     labels = SS_LEVELS,
#     right = FALSE
#   )
# }
# 
# read_lines_any <- function(src) {
#   con <- if (grepl("\\.gz$", src)) {
#     gzcon(
#       if (grepl("^https?://", src)) {
#         url(src, "rb")
#       } else {
#         file(src, "rb")
#       }
#     )
#   } else if (grepl("^https?://", src)) {
#     url(src)
#   } else {
#     file(src)
#   }
#   
#   on.exit(close(con))
#   readLines(con, warn = FALSE)
# }
# 
# safe_lines <- function(src) {
#   tryCatch(
#     read_lines_any(src),
#     error = function(e) NULL
#   )
# }
# 
# atcf_coord <- function(x) {
#   x <- trimws(x)
#   val <- suppressWarnings(
#     as.numeric(substr(x, 1, nchar(x) - 1))
#   ) / 10
#   hem <- substr(x, nchar(x), nchar(x))
#   ifelse(hem %in% c("S", "W"), -val, val)
# }
# 
# parse_atcf <- function(lines) {
#   if (is.null(lines)) return(NULL)
#   
#   lines <- lines[nzchar(trimws(lines))]
#   if (!length(lines)) return(NULL)
#   
#   f <- strsplit(lines, ",")
#   get <- function(i) {
#     vapply(
#       f,
#       function(r) {
#         if (length(r) >= i) trimws(r[i]) else NA_character_
#       },
#       ""
#     )
#   }
#   
#   data.frame(
#     basin   = get(1),
#     number  = get(2),
#     time    = as.POSIXct(get(3), format = "%Y%m%d%H", tz = "UTC"),
#     tech    = get(5),
#     tau     = suppressWarnings(as.integer(get(6))),
#     lat     = atcf_coord(get(7)),
#     lon     = atcf_coord(get(8)),
#     wind_kt = suppressWarnings(as.numeric(get(9))),
#     pres_mb = suppressWarnings(as.numeric(get(10))),
#     type    = get(11),
#     name    = toupper(get(28)),
#     stringsAsFactors = FALSE
#   )
# }
# 
# get_storm_track <- function(storm_id, src = NULL) {
#   id <- tolower(storm_id)
#   yr <- substr(id, 5, 8)
#   
#   srcs <- if (!is.null(src)) {
#     src
#   } else {
#     c(
#       paste0(ATCF_BTK, "b", id, ".dat"),
#       paste0(ATCF_ARCH, yr, "/b", id, ".dat.gz"),
#       paste0(ATCF_ARCH, yr, "/b", id, ".dat")
#     )
#   }
#   
#   d <- NULL
#   
#   for (u in srcs) {
#     d <- parse_atcf(safe_lines(u))
#     if (!is.null(d) && nrow(d)) break
#   }
#   
#   if (is.null(d) || !nrow(d)) {
#     stop("No best-track data found for ", storm_id)
#   }
#   
#   # ATCF repeats a valid time for wind-radius records. Keep one center fix.
#   d <- d[!is.na(d$time) & !is.na(d$lat) & !is.na(d$lon), ]
#   d <- d[order(d$time, -d$wind_kt), ]
#   d <- d[!duplicated(d$time), ]
#   
#   d$category <- saffir_simpson(
#     d$wind_kt
#   )
#   
#   rownames(d) <- NULL
#   d
# }
# 
# get_forecast_for_time <- function(
#     storm_id,
#     cutoff_utc,
#     src = NULL
# ) {
#   id <- tolower(storm_id)
#   
#   yr <- substr(id, 5, 8)
#   
#   srcs <- if (!is.null(src)) {
#     src
#   } else {
#     c(
#       # Active/current aid deck
#       paste0(ATCF_AIDS, "a", id, ".dat.gz"),
#       paste0(ATCF_AIDS, "a", id, ".dat"),
#       # Historical archive: needed for retrospective reports
#       paste0(ATCF_ARCH, yr, "/a", id, ".dat.gz"),
#       paste0(ATCF_ARCH, yr, "/a", id, ".dat")
#     )
#   }
#   
#   d <- NULL
#   
#   for (u in srcs) {
#     d <- parse_atcf(safe_lines(u))
#     if (!is.null(d) && nrow(d)) break
#   }
#   
#   if (is.null(d) || !nrow(d)) {
#     return(NULL)
#   }
#   
#   d <- d[
#     d$tech == "OFCL" &
#       !is.na(d$lat) &
#       !is.na(d$lon) &
#       !is.na(d$time),
#     ,
#     drop = FALSE
#   ]
#   
#   if (!nrow(d)) return(NULL)
#   
#   eligible <- d[d$time <= cutoff_utc, , drop = FALSE]
#   if (!nrow(eligible)) return(NULL)
#   
#   init <- max(eligible$time, na.rm = TRUE)
#   
#   out <- eligible[
#     eligible$time == init,
#     ,
#     drop = FALSE
#   ]
#   
#   out <- out[order(out$tau, -out$wind_kt), ]
#   out <- out[!duplicated(out$tau), ]
#   out$valid_time <- out$time + out$tau * 3600
#   
#   out$category <- saffir_simpson(
#     out$wind_kt
#   )
#   
#   out
# }
# 
# # ------------------------------------------------------------------------------
# # KMZ helper for archived NHC cone / track files.
# # ------------------------------------------------------------------------------
# 
# read_nhc_kmz <- function(path) {
#   if (is.null(path) || !nzchar(path) || !file.exists(path)) {
#     return(NULL)
#   }
#   
#   td <- tempfile("nhc_kmz_")
#   dir.create(td)
#   
#   on.exit(
#     unlink(td, recursive = TRUE, force = TRUE),
#     add = TRUE
#   )
#   
#   utils::unzip(path, exdir = td)
#   
#   kml <- list.files(
#     td,
#     pattern = "\\.kml$",
#     recursive = TRUE,
#     full.names = TRUE
#   )
#   
#   if (!length(kml)) {
#     warning("No KML found inside ", path, call. = FALSE)
#     return(NULL)
#   }
#   
#   tryCatch(
#     sf::st_read(kml[1], quiet = TRUE),
#     error = function(e) {
#       warning(
#         "Could not read NHC KMZ ",
#         basename(path),
#         ": ",
#         conditionMessage(e),
#         call. = FALSE
#       )
#       NULL
#     }
#   )
# }
# 
# # ------------------------------------------------------------------------------
# # Time helpers
# # ------------------------------------------------------------------------------
# 
# meta_window_utc <- function(
#     plot_ds,
#     plot_hour,
#     source_tz = "America/Los_Angeles"
# ) {
#   lubridate::ymd_hm(
#     paste(plot_ds, plot_hour),
#     tz = source_tz
#   ) |>
#     lubridate::with_tz("UTC")
# }
# 
# # ------------------------------------------------------------------------------
# # Cached map-feature helper
# # ------------------------------------------------------------------------------
# 
# hurricane_osm_lines_cached <- function(
#     map_bbox,
#     key,
#     values,
#     tag,
#     cache_dir
# ) {
#   if (is.null(map_bbox)) return(NULL)
#   
#   dir.create(
#     cache_dir,
#     recursive = TRUE,
#     showWarnings = FALSE
#   )
#   
#   bb_tag <- paste(
#     round(unname(map_bbox), 2),
#     collapse = "_"
#   )
#   
#   f <- file.path(
#     cache_dir,
#     paste0(tag, "_", bb_tag, ".rds")
#   )
#   
#   if (file.exists(f)) {
#     return(readRDS(f))
#   }
#   
#   servers <- c(
#     "https://overpass-api.de/api/interpreter",
#     "https://overpass.kumi.systems/api/interpreter",
#     "https://overpass.private.coffee/api/interpreter"
#   )
#   
#   out <- NULL
#   
#   for (srv in rep(servers, 3)) {
#     if (!is.null(out)) break
#     
#     message(
#       "Downloading OSM ",
#       tag,
#       " from ",
#       sub("https://([^/]+)/.*", "\\1", srv),
#       " ..."
#     )
#     
#     res <- tryCatch({
#       osmdata::set_overpass_url(srv)
#       
#       q <- osmdata::opq(
#         bbox = unname(map_bbox),
#         timeout = 180
#       ) |>
#         osmdata::add_osm_feature(
#           key = key,
#           value = values
#         )
#       
#       x <- osmdata::osmdata_sf(q)$osm_lines
#       
#       if (is.null(x) || nrow(x) == 0) {
#         "empty"
#       } else {
#         sf::st_transform(
#           x[, key, drop = FALSE],
#           3857
#         )
#       }
#       
#     }, error = function(e) {
#       message(
#         "   failed: ",
#         substr(conditionMessage(e), 1, 120)
#       )
#       NULL
#     })
#     
#     if (inherits(res, "sf")) {
#       out <- res
#       break
#     }
#     
#     if (identical(res, "empty")) {
#       message("   server returned 0 features")
#     }
#     
#     Sys.sleep(3)
#   }
#   
#   if (is.null(out)) {
#     message(
#       "*** ",
#       tag,
#       " could NOT be downloaded; layer omitted. Re-run later."
#     )
#   } else {
#     saveRDS(out, f)
#   }
#   
#   out
# }
# 
# # ------------------------------------------------------------------------------
# # Terrain helper
# #
# # Downloads/caches elevation once, then derives a hillshade surface using
# # slope + aspect. This replaces the contour-line layer so the shape of the
# # land remains visible without competing with the Bing population polygons.
# # ------------------------------------------------------------------------------
# 
# hurricane_terrain <- function(
#     map_bbox,
#     zoom,
#     contour_by = NULL,   # retained for backward compatibility; no longer used
#     cache_dir,
#     shade_angle = 40,
#     shade_direction = 315
# ) {
#   if (is.null(map_bbox)) return(NULL)
#   
#   dir.create(
#     cache_dir,
#     recursive = TRUE,
#     showWarnings = FALSE
#   )
#   
#   bb_tag <- paste(
#     round(unname(map_bbox), 2),
#     collapse = "_"
#   )
#   
#   elevation_file <- file.path(
#     cache_dir,
#     paste0(
#       "elevation_z",
#       zoom,
#       "_",
#       bb_tag,
#       ".tif"
#     )
#   )
#   
#   if (!file.exists(elevation_file)) {
#     message("Downloading elevation (first time only) ...")
#     
#     loc <- data.frame(
#       x = unname(
#         c(
#           map_bbox["xmin"],
#           map_bbox["xmax"]
#         )
#       ),
#       y = unname(
#         c(
#           map_bbox["ymin"],
#           map_bbox["ymax"]
#         )
#       )
#     )
#     
#     r0 <- terra::rast(
#       elevatr::get_elev_raster(
#         loc,
#         prj = 4326,
#         z = zoom,
#         src = "aws",
#         clip = "bbox",
#         verbose = FALSE
#       )
#     )
#     
#     names(r0) <- "elevation"
#     
#     terra::writeRaster(
#       r0,
#       elevation_file,
#       overwrite = TRUE
#     )
#   }
#   
#   elev <- terra::project(
#     terra::rast(elevation_file),
#     "EPSG:3857"
#   )
#   
#   names(elev) <- "elevation"
#   
#   # Keep plotting reasonably fast for larger/statewide extents.
#   if (terra::ncell(elev) > 2e5) {
#     elev <- terra::aggregate(
#       elev,
#       fact = ceiling(
#         sqrt(
#           terra::ncell(elev) / 2e5
#         )
#       )
#     )
#   }
#   
#   # Hillshade from slope and aspect.
#   slope <- terra::terrain(
#     elev,
#     v = "slope",
#     unit = "radians"
#   )
#   
#   aspect <- terra::terrain(
#     elev,
#     v = "aspect",
#     unit = "radians"
#   )
#   
#   shade <- terra::shade(
#     slope,
#     aspect,
#     angle = shade_angle,
#     direction = shade_direction
#   )
#   
#   names(shade) <- "shade"
#   
#   shade_df <- terra::as.data.frame(
#     shade,
#     xy = TRUE,
#     na.rm = TRUE
#   )
#   
#   list(
#     elevation = elev,
#     shade = shade,
#     shade_df = shade_df
#   )
# }
# 
# # ------------------------------------------------------------------------------
# # City pointer helper
# # ------------------------------------------------------------------------------
# 
# hurricane_city_layers <- function(
#     p,
#     cities,
#     default_dx_m = 25000,
#     default_dy_m = 20000
# ) {
#   if (is.null(cities) || !nrow(cities)) {
#     return(p)
#   }
#   
#   required <- c("place", "lon", "lat")
#   missing_cols <- setdiff(required, names(cities))
#   
#   if (length(missing_cols)) {
#     stop(
#       "`hurricane_config$cities` is missing: ",
#       paste(missing_cols, collapse = ", ")
#     )
#   }
#   
#   if (!"label_dx_m" %in% names(cities)) {
#     cities$label_dx_m <- default_dx_m
#   }
#   
#   if (!"label_dy_m" %in% names(cities)) {
#     cities$label_dy_m <- default_dy_m
#   }
#   
#   pts <- sf::st_as_sf(
#     cities,
#     coords = c("lon", "lat"),
#     crs = 4326
#   ) |>
#     sf::st_transform(3857)
#   
#   xy <- sf::st_coordinates(pts)
#   
#   lab <- sf::st_drop_geometry(pts)
#   lab$x <- xy[, 1]
#   lab$y <- xy[, 2]
#   lab$label_x <- lab$x + lab$label_dx_m
#   lab$label_y <- lab$y + lab$label_dy_m
#   
#   p +
#     ggplot2::geom_sf(
#       data = pts,
#       shape = 21,
#       fill = "black",
#       colour = "white",
#       size = 2,
#       stroke = 0.5,
#       inherit.aes = FALSE
#     ) +
#     ggplot2::geom_segment(
#       data = lab,
#       ggplot2::aes(
#         x = label_x,
#         y = label_y,
#         xend = x,
#         yend = y
#       ),
#       colour = "black",
#       linewidth = 0.5,
#       arrow = grid::arrow(
#         length = grid::unit(0.15, "cm"),
#         type = "closed"
#       ),
#       inherit.aes = FALSE
#     ) +
#     ggplot2::geom_label(
#       data = lab,
#       ggplot2::aes(
#         x = label_x,
#         y = label_y,
#         label = place
#       ),
#       size = 3,
#       fontface = "bold",
#       fill = "white",
#       colour = "black",
#       linewidth = 0.25,
#       inherit.aes = FALSE
#     )
# }
# 
# # ------------------------------------------------------------------------------
# # Main overlay
# # ------------------------------------------------------------------------------
# 
# add_hurricane_layers <- function(
#     p1,
#     map_bbox,
#     osm = NULL,
#     pop_sf = NULL,
#     fill_col = NULL,
#     plot_ds = NULL,
#     plot_hour = NULL,
#     tzone = "UTC",
#     config = NULL
# ) {
#   if (is.null(config)) {
#     config <- list()
#   }
#   
#   get_cfg <- function(name, default = NULL) {
#     if (!is.null(config[[name]])) {
#       config[[name]]
#     } else {
#       default
#     }
#   }
#   
#   cache_dir <- get_cfg(
#     "cache_dir",
#     "map_cache/hurricane"
#   )
#   
#   terrain_zoom <- get_cfg(
#     "terrain_zoom",
#     9
#   )
#   
#   contour_by <- get_cfg(
#     "contour_by",
#     500
#   )
#   
#   # Shaded-relief controls.
#   shade_angle <- get_cfg(
#     "shade_angle",
#     40
#   )
#   
#   shade_direction <- get_cfg(
#     "shade_direction",
#     315
#   )
#   
#   shade_alpha <- get_cfg(
#     "shade_alpha",
#     0.50
#   )
#   
#   pop_alpha <- get_cfg(
#     "pop_alpha",
#     0.82
#   )
#   
#   roads <- get_cfg(
#     "roads",
#     "major"
#   )
#   
#   rivers <- get_cfg(
#     "rivers",
#     "major"
#   )
#   
#   storm_id <- get_cfg(
#     "storm_id",
#     NULL
#   )
#   
#   track_src <- get_cfg(
#     "track_src",
#     NULL
#   )
#   
#   forecast_src <- get_cfg(
#     "forecast_src",
#     NULL
#   )
#   
#   forecast_track_kmz <- get_cfg(
#     "forecast_track_kmz",
#     NULL
#   )
#   
#   forecast_cone_kmz <- get_cfg(
#     "forecast_cone_kmz",
#     NULL
#   )
#   
#   source_tz <- get_cfg(
#     "source_tz",
#     "America/Los_Angeles"
#   )
#   
#   cities <- get_cfg(
#     "cities",
#     NULL
#   )
#   
#   show_storm_legend <- get_cfg(
#     "show_storm_legend",
#     TRUE
#   )
#   
#   show_path_legend <- get_cfg(
#     "show_path_legend",
#     TRUE
#   )
#   
#   show_context_legend <- get_cfg(
#     "show_context_legend",
#     TRUE
#   )
#   
#   map_time_utc <- if (
#     !is.null(plot_ds) &&
#     !is.null(plot_hour)
#   ) {
#     meta_window_utc(
#       plot_ds,
#       plot_hour,
#       source_tz = source_tz
#     )
#   } else {
#     as.POSIXct(
#       Sys.time(),
#       tz = "UTC"
#     )
#   }
#   
#   legend_values <- character(0)
#   
#   # --------------------------------------------------------------------------
#   # Save the Bing population layer and temporarily remove it.
#   #
#   # population_plot() enters this function with the Bing polygons as the most
#   # recently-added layer. We restore that layer near the end so the final order
#   # is:
#   #
#   #   basemap -> shaded terrain -> rivers/roads -> storm paths/cone
#   #   -> BING POPULATION -> city/current-center annotations
#   #
#   # This makes the Bing layer the second-to-top visual group while retaining
#   # enough transparency to read the contextual layers underneath.
#   # --------------------------------------------------------------------------
#   
#   pop_layer <- NULL
#   
#   if (
#     !is.null(pop_sf) &&
#     length(p1$layers) > 0
#   ) {
#     pop_layer <- p1$layers[[length(p1$layers)]]
#     pop_layer$aes_params$alpha <- pop_alpha
#     pop_layer$aes_params$colour <- "grey85"
#     pop_layer$aes_params$linewidth <- 0.05
#     
#     p1$layers <- p1$layers[-length(p1$layers)]
#   }
#   
#   # Current storm-center marker is stored and drawn AFTER the Bing layer.
#   current_center <- NULL
#   observed_points_sf <- NULL
#   observed_line_sf <- NULL
#   forecast_points_sf <- NULL
#   forecast_line_sf <- NULL
#   
#   # --------------------------------------------------------------------------
#   # SHADED TERRAIN
#   #
#   # The elevation DEM is converted to hillshade rather than contour lines.
#   # The relief sits directly above the CARTO basemap and below roads, rivers,
#   # hurricane tracks, and the Bing population layer.
#   # --------------------------------------------------------------------------
#   
#   if (!is.null(map_bbox)) {
#     terrain <- tryCatch(
#       hurricane_terrain(
#         map_bbox = map_bbox,
#         zoom = terrain_zoom,
#         contour_by = contour_by,
#         cache_dir = cache_dir,
#         shade_angle = shade_angle,
#         shade_direction = shade_direction
#       ),
#       error = function(e) {
#         warning(
#           "Hurricane terrain failed: ",
#           conditionMessage(e),
#           call. = FALSE
#         )
#         NULL
#       }
#     )
#     
#     if (!is.null(terrain)) {
#       shade_layer <- ggplot2::geom_raster(
#         data = terrain$shade_df,
#         ggplot2::aes(
#           x = x,
#           y = y,
#           alpha = shade
#         ),
#         fill = "grey15",
#         inherit.aes = FALSE
#       )
#       
#       # Put shaded relief immediately above the basemap.
#       p1$layers <- append(
#         p1$layers,
#         list(shade_layer),
#         after = if (!is.null(osm)) 1L else 0L
#       )
#       
#       p1 <- p1 +
#         ggplot2::scale_alpha_continuous(
#           range = c(0.03, shade_alpha),
#           guide = "none"
#         )
#     }
#   }
#   
#   # --------------------------------------------------------------------------
#   # Rivers
#   # --------------------------------------------------------------------------
#   
#   if (
#     !is.null(map_bbox) &&
#     rivers != "none"
#   ) {
#     waterways <- hurricane_osm_lines_cached(
#       map_bbox = map_bbox,
#       key = "waterway",
#       values = if (rivers == "all") {
#         c("river", "stream")
#       } else {
#         "river"
#       },
#       tag = paste0("water_", rivers),
#       cache_dir = cache_dir
#     )
#     
#     if (!is.null(waterways)) {
#       r <- waterways[
#         waterways$waterway == "river",
#         ,
#         drop = FALSE
#       ]
#       
#       if (nrow(r) > 0) {
#         if (show_context_legend) {
#           p1 <- p1 +
#             ggplot2::geom_sf(
#               data = r,
#               ggplot2::aes(colour = "Rivers"),
#               linewidth = 0.3,
#               alpha = 0.55,
#               inherit.aes = FALSE,
#               show.legend = "line"
#             )
#           legend_values["Rivers"] <- "dodgerblue3"
#         } else {
#           p1 <- p1 +
#             ggplot2::geom_sf(
#               data = r,
#               colour = "dodgerblue3",
#               linewidth = 0.3,
#               alpha = 0.55,
#               inherit.aes = FALSE,
#               show.legend = FALSE
#             )
#         }
#       }
#       
#       if (rivers == "all") {
#         s <- waterways[
#           waterways$waterway == "stream",
#           ,
#           drop = FALSE
#         ]
#         
#         if (nrow(s) > 0) {
#           if (show_context_legend) {
#             p1 <- p1 +
#               ggplot2::geom_sf(
#                 data = s,
#                 ggplot2::aes(colour = "Streams"),
#                 linewidth = 0.12,
#                 alpha = 0.45,
#                 inherit.aes = FALSE,
#                 show.legend = "line"
#               )
#             legend_values["Streams"] <- "deepskyblue3"
#           } else {
#             p1 <- p1 +
#               ggplot2::geom_sf(
#                 data = s,
#                 colour = "deepskyblue3",
#                 linewidth = 0.12,
#                 alpha = 0.45,
#                 inherit.aes = FALSE,
#                 show.legend = FALSE
#               )
#           }
#         }
#       }
#     }
#   }
#   
#   # --------------------------------------------------------------------------
#   # Major roads
#   #
#   # Roads are downloaded/cached here, but intentionally drawn AFTER the Bing
#   # population polygons so the road network remains visible on top.
#   # --------------------------------------------------------------------------
#   
#   road_major_sf <- NULL
#   road_secondary_sf <- NULL
#   
#   if (
#     !is.null(map_bbox) &&
#     roads != "none"
#   ) {
#     major <- c(
#       "motorway",
#       "motorway_link",
#       "trunk",
#       "trunk_link",
#       "primary",
#       "primary_link"
#     )
#     
#     secondary <- c(
#       "secondary",
#       "secondary_link"
#     )
#     
#     road_sf <- hurricane_osm_lines_cached(
#       map_bbox = map_bbox,
#       key = "highway",
#       values = if (roads == "secondary") {
#         c(major, secondary)
#       } else {
#         major
#       },
#       tag = paste0("roads_", roads),
#       cache_dir = cache_dir
#     )
#     
#     if (!is.null(road_sf)) {
#       road_major_sf <- road_sf[
#         road_sf$highway %in% major,
#         ,
#         drop = FALSE
#       ]
#       
#       if (roads == "secondary") {
#         road_secondary_sf <- road_sf[
#           road_sf$highway %in% secondary,
#           ,
#           drop = FALSE
#         ]
#       }
#     }
#   }
#   
#   # --------------------------------------------------------------------------
#   # Observed best track through the CURRENT MAP TIME.
#   # --------------------------------------------------------------------------
#   
#   if (!is.null(storm_id)) {
#     track <- tryCatch(
#       get_storm_track(
#         storm_id,
#         src = track_src
#       ),
#       error = function(e) {
#         warning(
#           "Best track unavailable: ",
#           conditionMessage(e),
#           call. = FALSE
#         )
#         NULL
#       }
#     )
#     
#     if (!is.null(track)) {
#       observed <- track[
#         track$time <= map_time_utc,
#         ,
#         drop = FALSE
#       ]
#       
#       if (nrow(observed) > 0) {
#         obs_sf <- sf::st_as_sf(
#           observed,
#           coords = c("lon", "lat"),
#           crs = 4326,
#           remove = FALSE
#         ) |>
#           sf::st_transform(3857)
#         
#         observed_points_sf <- obs_sf
#         
#         if (nrow(obs_sf) >= 2) {
#           obs_line <- obs_sf |>
#             dplyr::arrange(time) |>
#             dplyr::summarise(
#               geometry = sf::st_combine(geometry)
#             ) |>
#             sf::st_cast("LINESTRING")
#           
#           observed_line_sf <- obs_line
#           
#           p1 <- p1 +
#             ggplot2::geom_sf(
#               data = obs_line,
#               ggplot2::aes(colour = "Observed best track"),
#               linewidth = 0.8,
#               inherit.aes = FALSE,
#               show.legend = "line"
#             )
#           
#         }
#         
#         current_center <- obs_sf |>
#           dplyr::slice_max(
#             time,
#             n = 1,
#             with_ties = FALSE
#           )
#       }
#       
#       # ----------------------------------------------------------------------
#       # Forecast path.
#       #
#       # Priority:
#       #   1) archived/current TRACK KMZ if supplied
#       #   2) OFCL ATCF forecast at or before the map time
#       #
#       # Forecast is deliberately RED + DASHED, unlike the observed black track.
#       # ----------------------------------------------------------------------
#       
#       fcst_kmz <- read_nhc_kmz(
#         forecast_track_kmz
#       )
#       
#       if (!is.null(fcst_kmz)) {
#         fcst_kmz <- sf::st_transform(
#           fcst_kmz,
#           3857
#         )
#         
#         forecast_line_sf <- fcst_kmz
#         
#       } else {
#         forecast <- get_forecast_for_time(
#           storm_id = storm_id,
#           cutoff_utc = map_time_utc,
#           src = forecast_src
#         )
#         
#         if (!is.null(forecast) && nrow(forecast) > 0) {
#           fcst_sf <- sf::st_as_sf(
#             forecast,
#             coords = c("lon", "lat"),
#             crs = 4326,
#             remove = FALSE
#           ) |>
#             sf::st_transform(3857)
#           
#           start_sf <- NULL
#           
#           if (
#             exists("current_center", inherits = FALSE) &&
#             !is.null(current_center) &&
#             nrow(current_center) > 0
#           ) {
#             start_sf <- current_center
#           }
#           
#           line_pts <- if (!is.null(start_sf)) {
#             rbind(
#               start_sf["geometry"],
#               fcst_sf["geometry"]
#             )
#           } else {
#             fcst_sf["geometry"]
#           }
#           
#           if (nrow(line_pts) >= 2) {
#             fcst_line <- line_pts |>
#               dplyr::summarise(
#                 geometry = sf::st_combine(geometry)
#               ) |>
#               sf::st_cast("LINESTRING")
#             
#             forecast_line_sf <- fcst_line
#             forecast_points_sf <- fcst_sf
#           }
#         }
#       }
#     }
#   }
#   
#   # --------------------------------------------------------------------------
#   # Optional NHC uncertainty cone.
#   # --------------------------------------------------------------------------
#   
#   cone <- read_nhc_kmz(
#     forecast_cone_kmz
#   )
#   
#   if (!is.null(cone)) {
#     cone <- sf::st_transform(
#       cone,
#       3857
#     )
#     
#     p1 <- p1 +
#       ggplot2::geom_sf(
#         data = cone,
#         fill = "skyblue2",
#         colour = "steelblue4",
#         alpha = 0.16,
#         linewidth = 0.35,
#         inherit.aes = FALSE
#       )
#   }
#   
#   # --------------------------------------------------------------------------
#   # Restore Bing population polygons.
#   # --------------------------------------------------------------------------
#   
#   if (!is.null(pop_layer)) {
#     p1$layers <- append(
#       p1$layers,
#       list(pop_layer),
#       after = length(p1$layers)
#     )
#   }
#   
#   # --------------------------------------------------------------------------
#   # Draw roads ABOVE the Bing tiles.
#   # --------------------------------------------------------------------------
#   
#   if (
#     !is.null(road_major_sf) &&
#     nrow(road_major_sf) > 0
#   ) {
#     p1 <- p1 +
#       ggplot2::geom_sf(
#         data = road_major_sf,
#         colour = "black",
#         linewidth = 0.45,
#         alpha = 0.82,
#         inherit.aes = FALSE
#       )
#   }
#   
#   if (
#     !is.null(road_secondary_sf) &&
#     nrow(road_secondary_sf) > 0
#   ) {
#     p1 <- p1 +
#       ggplot2::geom_sf(
#         data = road_secondary_sf,
#         colour = "grey35",
#         linewidth = 0.25,
#         alpha = 0.72,
#         inherit.aes = FALSE
#       )
#   }
#   
#   # --------------------------------------------------------------------------
#   # TRACKER-STYLE HURRICANE PATH
#   #
#   # Match hurricane_tracker.R:
#   #   observed track  = solid grey/black path + circular category dots
#   #   forecast track  = red dashed path + diamond category dots
#   #   dot fill        = Saffir-Simpson / storm type
#   #   dot size        = max sustained wind (kt)
#   #
#   # Because the population plot already uses a fill scale, start a fresh fill
#   # scale here for the hurricane-category dots.
#   # --------------------------------------------------------------------------
#   
#   if (
#     !is.null(observed_line_sf) ||
#     !is.null(observed_points_sf) ||
#     !is.null(forecast_line_sf) ||
#     !is.null(forecast_points_sf)
#   ) {
#     if (!requireNamespace("ggnewscale", quietly = TRUE)) {
#       stop(
#         "Package `ggnewscale` is required for hurricane intensity dots. ",
#         "Install it once with install.packages('ggnewscale')."
#       )
#     }
#     
#     p1 <- p1 +
#       ggnewscale::new_scale_fill()
#   }
#   
#   if (
#     !is.null(observed_line_sf) &&
#     nrow(observed_line_sf) > 0
#   ) {
#     if (show_path_legend) {
#       p1 <- p1 +
#         ggplot2::geom_sf(
#           data = observed_line_sf,
#           ggplot2::aes(colour = "Observed best track"),
#           linewidth = 0.8,
#           linetype = "solid",
#           inherit.aes = FALSE,
#           show.legend = TRUE
#         )
#       legend_values["Observed best track"] <- "grey20"
#     } else {
#       p1 <- p1 +
#         ggplot2::geom_sf(
#           data = observed_line_sf,
#           colour = "grey20",
#           linewidth = 0.8,
#           linetype = "solid",
#           inherit.aes = FALSE,
#           show.legend = FALSE
#         )
#     }
#   }
#   
#   if (
#     !is.null(observed_points_sf) &&
#     nrow(observed_points_sf) > 0
#   ) {
#     p1 <- p1 +
#       ggplot2::geom_sf(
#         data = observed_points_sf,
#         ggplot2::aes(
#           fill = category,
#           size = wind_kt
#         ),
#         shape = 21,
#         colour = "grey20",
#         stroke = 0.35,
#         inherit.aes = FALSE
#       )
#   }
#   
#   if (
#     !is.null(forecast_line_sf) &&
#     nrow(forecast_line_sf) > 0
#   ) {
#     if (show_path_legend) {
#       p1 <- p1 +
#         ggplot2::geom_sf(
#           data = forecast_line_sf,
#           ggplot2::aes(colour = "Official forecast track"),
#           linewidth = 0.9,
#           linetype = "dashed",
#           inherit.aes = FALSE,
#           show.legend = TRUE
#         )
#       legend_values["Official forecast track"] <- "red3"
#     } else {
#       p1 <- p1 +
#         ggplot2::geom_sf(
#           data = forecast_line_sf,
#           colour = "red3",
#           linewidth = 0.9,
#           linetype = "dashed",
#           inherit.aes = FALSE,
#           show.legend = FALSE
#         )
#     }
#   }
#   
#   if (
#     !is.null(forecast_points_sf) &&
#     nrow(forecast_points_sf) > 0
#   ) {
#     p1 <- p1 +
#       ggplot2::geom_sf(
#         data = forecast_points_sf,
#         ggplot2::aes(
#           fill = category,
#           size = wind_kt
#         ),
#         shape = 23,
#         colour = "red3",
#         stroke = 0.7,
#         inherit.aes = FALSE
#       )
#   }
#   
#   if (
#     !is.null(observed_points_sf) ||
#     !is.null(forecast_points_sf)
#   ) {
#     if (show_storm_legend) {
#       p1 <- p1 +
#         ggplot2::scale_fill_manual(
#           values = SS_COLORS,
#           limits = SS_LEVELS,
#           drop = FALSE,
#           name = "Storm intensity",
#           guide = ggplot2::guide_legend(
#             override.aes = list(
#               shape = 21,
#               size = 4
#             ),
#             order = 1,
#             position = "right"
#           )
#         ) +
#         ggplot2::scale_size_continuous(
#           range = c(1.8, 5.5),
#           name = "Max wind (kt)",
#           guide = ggplot2::guide_legend(
#             order = 2,
#             position = "right"
#           )
#         )
#     } else {
#       p1 <- p1 +
#         ggplot2::scale_fill_manual(
#           values = SS_COLORS,
#           limits = SS_LEVELS,
#           drop = FALSE,
#           guide = "none"
#         ) +
#         ggplot2::scale_size_continuous(
#           range = c(1.8, 5.5),
#           guide = "none"
#         )
#     }
#   }
#   
#   # --------------------------------------------------------------------------
#   # City pointers remain the top annotation group.
#   # --------------------------------------------------------------------------
#   
#   p1 <- hurricane_city_layers(
#     p1,
#     cities = cities
#   )
#   
#   # --------------------------------------------------------------------------
#   # Combined feature legend.
#   # --------------------------------------------------------------------------
#   
#   if (length(legend_values) > 0) {
#     p1 <- p1 +
#       ggplot2::scale_colour_manual(
#         name = NULL,
#         values = legend_values,
#         breaks = names(legend_values),
#         guide = ggplot2::guide_legend(
#           direction = "horizontal",
#           order = 4,
#           position = "bottom",
#           override.aes = list(
#             linewidth = 0.9
#           )
#         )
#       )
#   }
#   
#   p1
# }
# 
# # ------------------------------------------------------------------------------
# # Auto-apply when sourced inside population_plot()
# # ------------------------------------------------------------------------------
# 
# if (
#   exists("p1", inherits = FALSE) &&
#   exists("map_bbox", inherits = FALSE)
# ) {
#   cfg <- if (
#     exists("disaster_config", inherits = FALSE) &&
#     !is.null(disaster_config)
#   ) {
#     disaster_config
#   } else if (exists("hurricane_config", inherits = TRUE)) {
#     get("hurricane_config", inherits = TRUE)
#   } else {
#     list()
#   }
#   
#   p1 <- add_hurricane_layers(
#     p1 = p1,
#     map_bbox = map_bbox,
#     osm = if (exists("osm", inherits = FALSE)) osm else NULL,
#     pop_sf = if (exists("first_time_pop", inherits = FALSE)) first_time_pop else NULL,
#     fill_col = if (exists("fill_col", inherits = FALSE)) fill_col else NULL,
#     plot_ds = if (exists("plot_ds", inherits = FALSE)) plot_ds else NULL,
#     plot_hour = if (exists("plot_hour", inherits = FALSE)) plot_hour else NULL,
#     tzone = if (exists("tzone", inherits = FALSE)) tzone else "UTC",
#     config = cfg
#   )
# }
# ==============================================================================
# 3_hurricane_functions.R
#
# Reusable hurricane overlay for population_plot().
#
# Adds:
#   * cached shaded-relief terrain
#   * cached rivers and major roads with Overpass failover
#   * observed NHC/CPHC best track up to the map time
#   * forecast track in a DIFFERENT style (red dashed + diamonds)
#   * optional NHC forecast cone KMZ
#   * city markers + pointer labels
#
# The script can be sourced directly (defines functions only), or sourced from
# inside population_plot(disaster_type = "hurricane"), where it auto-applies.
# ==============================================================================

suppressPackageStartupMessages({
  require(sf)
  require(terra)
  require(elevatr)
  require(osmdata)
  require(ggplot2)
  require(lubridate)
  require(jsonlite)
})

# ------------------------------------------------------------------------------
# NOAA / NHC ATCF helpers adapted from hurricane_tracker.R
# ------------------------------------------------------------------------------

NHC_JSON  <- "https://www.nhc.noaa.gov/CurrentStorms.json"
ATCF_BTK  <- "https://ftp.nhc.noaa.gov/atcf/btk/"
ATCF_ARCH <- "https://ftp.nhc.noaa.gov/atcf/archive/"
ATCF_AIDS <- "https://ftp.nhc.noaa.gov/atcf/aid_public/"


# Saffir-Simpson / tropical-cyclone intensity categories.
# Same breakpoints and display colors used in hurricane_tracker.R.
SS_LEVELS <- c(
  "Tropical Depression",
  "Tropical Storm",
  "Category 1",
  "Category 2",
  "Category 3",
  "Category 4",
  "Category 5"
)

SS_COLORS <- c(
  "Tropical Depression" = "#5EBAFF",
  "Tropical Storm"      = "#00FAF4",
  "Category 1"          = "#FFF795",
  "Category 2"          = "#FFD821",
  "Category 3"          = "#FF8F20",
  "Category 4"          = "#FF6060",
  "Category 5"          = "#C464D9"
)

saffir_simpson <- function(wind_kt) {
  cut(
    wind_kt,
    breaks = c(
      -Inf,
      34,
      64,
      83,
      96,
      113,
      137,
      Inf
    ),
    labels = SS_LEVELS,
    right = FALSE
  )
}

read_lines_any <- function(src) {
  con <- if (grepl("\\.gz$", src)) {
    gzcon(
      if (grepl("^https?://", src)) {
        url(src, "rb")
      } else {
        file(src, "rb")
      }
    )
  } else if (grepl("^https?://", src)) {
    url(src)
  } else {
    file(src)
  }
  
  on.exit(close(con))
  readLines(con, warn = FALSE)
}

safe_lines <- function(src) {
  tryCatch(
    read_lines_any(src),
    error = function(e) NULL
  )
}

atcf_coord <- function(x) {
  x <- trimws(x)
  val <- suppressWarnings(
    as.numeric(substr(x, 1, nchar(x) - 1))
  ) / 10
  hem <- substr(x, nchar(x), nchar(x))
  ifelse(hem %in% c("S", "W"), -val, val)
}

parse_atcf <- function(lines) {
  if (is.null(lines)) return(NULL)
  
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(NULL)
  
  f <- strsplit(lines, ",")
  get <- function(i) {
    vapply(
      f,
      function(r) {
        if (length(r) >= i) trimws(r[i]) else NA_character_
      },
      ""
    )
  }
  
  data.frame(
    basin   = get(1),
    number  = get(2),
    time    = as.POSIXct(get(3), format = "%Y%m%d%H", tz = "UTC"),
    tech    = get(5),
    tau     = suppressWarnings(as.integer(get(6))),
    lat     = atcf_coord(get(7)),
    lon     = atcf_coord(get(8)),
    wind_kt = suppressWarnings(as.numeric(get(9))),
    pres_mb = suppressWarnings(as.numeric(get(10))),
    type    = get(11),
    name    = toupper(get(28)),
    stringsAsFactors = FALSE
  )
}

get_storm_track <- function(storm_id, src = NULL) {
  id <- tolower(storm_id)
  yr <- substr(id, 5, 8)
  
  srcs <- if (!is.null(src)) {
    src
  } else {
    c(
      paste0(ATCF_BTK, "b", id, ".dat"),
      paste0(ATCF_ARCH, yr, "/b", id, ".dat.gz"),
      paste0(ATCF_ARCH, yr, "/b", id, ".dat")
    )
  }
  
  d <- NULL
  
  for (u in srcs) {
    d <- parse_atcf(safe_lines(u))
    if (!is.null(d) && nrow(d)) break
  }
  
  if (is.null(d) || !nrow(d)) {
    stop("No best-track data found for ", storm_id)
  }
  
  # ATCF repeats a valid time for wind-radius records. Keep one center fix.
  d <- d[!is.na(d$time) & !is.na(d$lat) & !is.na(d$lon), ]
  d <- d[order(d$time, -d$wind_kt), ]
  d <- d[!duplicated(d$time), ]
  
  d$category <- saffir_simpson(
    d$wind_kt
  )
  
  rownames(d) <- NULL
  d
}

get_forecast_for_time <- function(
    storm_id,
    cutoff_utc,
    src = NULL
) {
  id <- tolower(storm_id)
  
  yr <- substr(id, 5, 8)
  
  srcs <- if (!is.null(src)) {
    src
  } else {
    c(
      # Active/current aid deck
      paste0(ATCF_AIDS, "a", id, ".dat.gz"),
      paste0(ATCF_AIDS, "a", id, ".dat"),
      # Historical archive: needed for retrospective reports
      paste0(ATCF_ARCH, yr, "/a", id, ".dat.gz"),
      paste0(ATCF_ARCH, yr, "/a", id, ".dat")
    )
  }
  
  d <- NULL
  
  for (u in srcs) {
    d <- parse_atcf(safe_lines(u))
    if (!is.null(d) && nrow(d)) break
  }
  
  if (is.null(d) || !nrow(d)) {
    return(NULL)
  }
  
  d <- d[
    d$tech == "OFCL" &
      !is.na(d$lat) &
      !is.na(d$lon) &
      !is.na(d$time),
    ,
    drop = FALSE
  ]
  
  if (!nrow(d)) return(NULL)
  
  eligible <- d[d$time <= cutoff_utc, , drop = FALSE]
  if (!nrow(eligible)) return(NULL)
  
  init <- max(eligible$time, na.rm = TRUE)
  
  out <- eligible[
    eligible$time == init,
    ,
    drop = FALSE
  ]
  
  out <- out[order(out$tau, -out$wind_kt), ]
  out <- out[!duplicated(out$tau), ]
  out$valid_time <- out$time + out$tau * 3600
  
  out$category <- saffir_simpson(
    out$wind_kt
  )
  
  out
}

# ------------------------------------------------------------------------------
# KMZ helper for archived NHC cone / track files.
# ------------------------------------------------------------------------------

read_nhc_kmz <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  
  td <- tempfile("nhc_kmz_")
  dir.create(td)
  
  on.exit(
    unlink(td, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  utils::unzip(path, exdir = td)
  
  kml <- list.files(
    td,
    pattern = "\\.kml$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (!length(kml)) {
    warning("No KML found inside ", path, call. = FALSE)
    return(NULL)
  }
  
  tryCatch(
    sf::st_read(kml[1], quiet = TRUE),
    error = function(e) {
      warning(
        "Could not read NHC KMZ ",
        basename(path),
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
}

# ------------------------------------------------------------------------------
# Time helpers
# ------------------------------------------------------------------------------

meta_window_utc <- function(
    plot_ds,
    plot_hour,
    source_tz = "America/Los_Angeles"
) {
  lubridate::ymd_hm(
    paste(plot_ds, plot_hour),
    tz = source_tz
  ) |>
    lubridate::with_tz("UTC")
}

# ------------------------------------------------------------------------------
# Cached map-feature helper
# ------------------------------------------------------------------------------

hurricane_osm_lines_cached <- function(
    map_bbox,
    key,
    values,
    tag,
    cache_dir
) {
  if (is.null(map_bbox)) return(NULL)
  
  dir.create(
    cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  bb_tag <- paste(
    round(unname(map_bbox), 2),
    collapse = "_"
  )
  
  f <- file.path(
    cache_dir,
    paste0(tag, "_", bb_tag, ".rds")
  )
  
  if (file.exists(f)) {
    return(readRDS(f))
  }
  
  servers <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://overpass.private.coffee/api/interpreter"
  )
  
  out <- NULL
  
  for (srv in rep(servers, 3)) {
    if (!is.null(out)) break
    
    message(
      "Downloading OSM ",
      tag,
      " from ",
      sub("https://([^/]+)/.*", "\\1", srv),
      " ..."
    )
    
    res <- tryCatch({
      osmdata::set_overpass_url(srv)
      
      q <- osmdata::opq(
        bbox = unname(map_bbox),
        timeout = 180
      ) |>
        osmdata::add_osm_feature(
          key = key,
          value = values
        )
      
      x <- osmdata::osmdata_sf(q)$osm_lines
      
      if (is.null(x) || nrow(x) == 0) {
        "empty"
      } else {
        sf::st_transform(
          x[, key, drop = FALSE],
          3857
        )
      }
      
    }, error = function(e) {
      message(
        "   failed: ",
        substr(conditionMessage(e), 1, 120)
      )
      NULL
    })
    
    if (inherits(res, "sf")) {
      out <- res
      break
    }
    
    if (identical(res, "empty")) {
      message("   server returned 0 features")
    }
    
    Sys.sleep(3)
  }
  
  if (is.null(out)) {
    message(
      "*** ",
      tag,
      " could NOT be downloaded; layer omitted. Re-run later."
    )
  } else {
    saveRDS(out, f)
  }
  
  out
}

# ------------------------------------------------------------------------------
# Terrain helper
#
# Downloads/caches elevation once, then derives a hillshade surface using
# slope + aspect. This replaces the contour-line layer so the shape of the
# land remains visible without competing with the Bing population polygons.
# ------------------------------------------------------------------------------

hurricane_terrain <- function(
    map_bbox,
    zoom,
    contour_by = NULL,   # retained for backward compatibility; no longer used
    cache_dir,
    shade_angle = 40,
    shade_direction = 315
) {
  if (is.null(map_bbox)) return(NULL)
  
  dir.create(
    cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  bb_tag <- paste(
    round(unname(map_bbox), 2),
    collapse = "_"
  )
  
  elevation_file <- file.path(
    cache_dir,
    paste0(
      "elevation_z",
      zoom,
      "_",
      bb_tag,
      ".tif"
    )
  )
  
  if (!file.exists(elevation_file)) {
    message("Downloading elevation (first time only) ...")
    
    loc <- data.frame(
      x = unname(
        c(
          map_bbox["xmin"],
          map_bbox["xmax"]
        )
      ),
      y = unname(
        c(
          map_bbox["ymin"],
          map_bbox["ymax"]
        )
      )
    )
    
    r0 <- terra::rast(
      elevatr::get_elev_raster(
        loc,
        prj = 4326,
        z = zoom,
        src = "aws",
        clip = "bbox",
        verbose = FALSE
      )
    )
    
    names(r0) <- "elevation"
    
    terra::writeRaster(
      r0,
      elevation_file,
      overwrite = TRUE
    )
  }
  
  elev <- terra::project(
    terra::rast(elevation_file),
    "EPSG:3857"
  )
  
  names(elev) <- "elevation"
  
  # Keep plotting reasonably fast for larger/statewide extents.
  if (terra::ncell(elev) > 2e5) {
    elev <- terra::aggregate(
      elev,
      fact = ceiling(
        sqrt(
          terra::ncell(elev) / 2e5
        )
      )
    )
  }
  
  # Hillshade from slope and aspect.
  slope <- terra::terrain(
    elev,
    v = "slope",
    unit = "radians"
  )
  
  aspect <- terra::terrain(
    elev,
    v = "aspect",
    unit = "radians"
  )
  
  shade <- terra::shade(
    slope,
    aspect,
    angle = shade_angle,
    direction = shade_direction
  )
  
  names(shade) <- "shade"
  
  shade_df <- terra::as.data.frame(
    shade,
    xy = TRUE,
    na.rm = TRUE
  )
  
  list(
    elevation = elev,
    shade = shade,
    shade_df = shade_df
  )
}

# ------------------------------------------------------------------------------
# City pointer helper
# ------------------------------------------------------------------------------

hurricane_city_layers <- function(
    p,
    cities,
    default_dx_m = 25000,
    default_dy_m = 20000
) {
  if (is.null(cities) || !nrow(cities)) {
    return(p)
  }
  
  required <- c("place", "lon", "lat")
  missing_cols <- setdiff(required, names(cities))
  
  if (length(missing_cols)) {
    stop(
      "`hurricane_config$cities` is missing: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!"label_dx_m" %in% names(cities)) {
    cities$label_dx_m <- default_dx_m
  }
  
  if (!"label_dy_m" %in% names(cities)) {
    cities$label_dy_m <- default_dy_m
  }
  
  pts <- sf::st_as_sf(
    cities,
    coords = c("lon", "lat"),
    crs = 4326
  ) |>
    sf::st_transform(3857)
  
  xy <- sf::st_coordinates(pts)
  
  lab <- sf::st_drop_geometry(pts)
  lab$x <- xy[, 1]
  lab$y <- xy[, 2]
  lab$label_x <- lab$x + lab$label_dx_m
  lab$label_y <- lab$y + lab$label_dy_m
  
  p +
    ggplot2::geom_sf(
      data = pts,
      shape = 21,
      fill = "black",
      colour = "white",
      size = 2,
      stroke = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = lab,
      ggplot2::aes(
        x = label_x,
        y = label_y,
        xend = x,
        yend = y
      ),
      colour = "black",
      linewidth = 0.5,
      arrow = grid::arrow(
        length = grid::unit(0.15, "cm"),
        type = "closed"
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_label(
      data = lab,
      ggplot2::aes(
        x = label_x,
        y = label_y,
        label = place
      ),
      size = 3,
      fontface = "bold",
      fill = "white",
      colour = "black",
      linewidth = 0.25,
      inherit.aes = FALSE
    )
}

# ------------------------------------------------------------------------------
# Main overlay
# ------------------------------------------------------------------------------

add_hurricane_layers <- function(
    p1,
    map_bbox,
    osm = NULL,
    pop_sf = NULL,
    fill_col = NULL,
    plot_ds = NULL,
    plot_hour = NULL,
    tzone = "UTC",
    config = NULL
) {
  if (is.null(config)) {
    config <- list()
  }
  
  get_cfg <- function(name, default = NULL) {
    if (!is.null(config[[name]])) {
      config[[name]]
    } else {
      default
    }
  }
  
  cache_dir <- get_cfg(
    "cache_dir",
    "map_cache/hurricane"
  )
  
  terrain_zoom <- get_cfg(
    "terrain_zoom",
    9
  )
  
  contour_by <- get_cfg(
    "contour_by",
    500
  )
  
  # Shaded-relief controls.
  shade_angle <- get_cfg(
    "shade_angle",
    40
  )
  
  shade_direction <- get_cfg(
    "shade_direction",
    315
  )
  
  shade_alpha <- get_cfg(
    "shade_alpha",
    0.50
  )
  
  pop_alpha <- get_cfg(
    "pop_alpha",
    0.82
  )
  
  roads <- get_cfg(
    "roads",
    "major"
  )
  
  rivers <- get_cfg(
    "rivers",
    "major"
  )
  
  storm_id <- get_cfg(
    "storm_id",
    NULL
  )
  
  track_src <- get_cfg(
    "track_src",
    NULL
  )
  
  forecast_src <- get_cfg(
    "forecast_src",
    NULL
  )
  
  forecast_track_kmz <- get_cfg(
    "forecast_track_kmz",
    NULL
  )
  
  forecast_cone_kmz <- get_cfg(
    "forecast_cone_kmz",
    NULL
  )
  
  source_tz <- get_cfg(
    "source_tz",
    "America/Los_Angeles"
  )
  
  cities <- get_cfg(
    "cities",
    NULL
  )
  
  show_storm_legend <- get_cfg(
    "show_storm_legend",
    TRUE
  )
  
  show_path_legend <- get_cfg(
    "show_path_legend",
    TRUE
  )
  
  show_context_legend <- get_cfg(
    "show_context_legend",
    TRUE
  )
  
  map_time_utc <- if (
    !is.null(plot_ds) &&
    !is.null(plot_hour)
  ) {
    meta_window_utc(
      plot_ds,
      plot_hour,
      source_tz = source_tz
    )
  } else {
    as.POSIXct(
      Sys.time(),
      tz = "UTC"
    )
  }
  
  legend_values <- character(0)
  
  # --------------------------------------------------------------------------
  # Save the Bing population layer and temporarily remove it.
  #
  # population_plot() enters this function with the Bing polygons as the most
  # recently-added layer. We restore that layer near the end so the final order
  # is:
  #
  #   basemap -> shaded terrain -> rivers/roads -> storm paths/cone
  #   -> BING POPULATION -> city/current-center annotations
  #
  # This makes the Bing layer the second-to-top visual group while retaining
  # enough transparency to read the contextual layers underneath.
  # --------------------------------------------------------------------------
  
  pop_layer <- NULL
  
  if (
    !is.null(pop_sf) &&
    length(p1$layers) > 0
  ) {
    pop_layer <- p1$layers[[length(p1$layers)]]
    pop_layer$aes_params$alpha <- pop_alpha
    pop_layer$aes_params$colour <- "grey85"
    pop_layer$aes_params$linewidth <- 0.05
    
    p1$layers <- p1$layers[-length(p1$layers)]
  }
  
  # Current storm-center marker is stored and drawn AFTER the Bing layer.
  current_center <- NULL
  observed_points_sf <- NULL
  observed_line_sf <- NULL
  forecast_points_sf <- NULL
  forecast_line_sf <- NULL
  
  # --------------------------------------------------------------------------
  # SHADED TERRAIN
  #
  # The elevation DEM is converted to hillshade rather than contour lines.
  # The relief sits directly above the CARTO basemap and below roads, rivers,
  # hurricane tracks, and the Bing population layer.
  # --------------------------------------------------------------------------
  
  if (!is.null(map_bbox)) {
    terrain <- tryCatch(
      hurricane_terrain(
        map_bbox = map_bbox,
        zoom = terrain_zoom,
        contour_by = contour_by,
        cache_dir = cache_dir,
        shade_angle = shade_angle,
        shade_direction = shade_direction
      ),
      error = function(e) {
        warning(
          "Hurricane terrain failed: ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
    
    if (!is.null(terrain)) {
      shade_layer <- ggplot2::geom_raster(
        data = terrain$shade_df,
        ggplot2::aes(
          x = x,
          y = y,
          alpha = shade
        ),
        fill = "grey15",
        inherit.aes = FALSE
      )
      
      # Put shaded relief immediately above the basemap.
      p1$layers <- append(
        p1$layers,
        list(shade_layer),
        after = if (!is.null(osm)) 1L else 0L
      )
      
      p1 <- p1 +
        ggplot2::scale_alpha_continuous(
          range = c(0.03, shade_alpha),
          guide = "none"
        )
    }
  }
  
  # --------------------------------------------------------------------------
  # Rivers
  # --------------------------------------------------------------------------
  
  if (
    !is.null(map_bbox) &&
    rivers != "none"
  ) {
    waterways <- hurricane_osm_lines_cached(
      map_bbox = map_bbox,
      key = "waterway",
      values = if (rivers == "all") {
        c("river", "stream")
      } else {
        "river"
      },
      tag = paste0("water_", rivers),
      cache_dir = cache_dir
    )
    
    if (!is.null(waterways)) {
      r <- waterways[
        waterways$waterway == "river",
        ,
        drop = FALSE
      ]
      
      if (nrow(r) > 0) {
        if (show_context_legend) {
          p1 <- p1 +
            ggplot2::geom_sf(
              data = r,
              ggplot2::aes(colour = "Rivers"),
              linewidth = 0.3,
              alpha = 0.55,
              inherit.aes = FALSE,
              show.legend = "line"
            )
          legend_values["Rivers"] <- "dodgerblue3"
        } else {
          p1 <- p1 +
            ggplot2::geom_sf(
              data = r,
              colour = "dodgerblue3",
              linewidth = 0.3,
              alpha = 0.55,
              inherit.aes = FALSE,
              show.legend = FALSE
            )
        }
      }
      
      if (rivers == "all") {
        s <- waterways[
          waterways$waterway == "stream",
          ,
          drop = FALSE
        ]
        
        if (nrow(s) > 0) {
          if (show_context_legend) {
            p1 <- p1 +
              ggplot2::geom_sf(
                data = s,
                ggplot2::aes(colour = "Streams"),
                linewidth = 0.12,
                alpha = 0.45,
                inherit.aes = FALSE,
                show.legend = "line"
              )
            legend_values["Streams"] <- "deepskyblue3"
          } else {
            p1 <- p1 +
              ggplot2::geom_sf(
                data = s,
                colour = "deepskyblue3",
                linewidth = 0.12,
                alpha = 0.45,
                inherit.aes = FALSE,
                show.legend = FALSE
              )
          }
        }
      }
    }
  }
  
  # --------------------------------------------------------------------------
  # Major roads
  #
  # Roads are downloaded/cached here, but intentionally drawn AFTER the Bing
  # population polygons so the road network remains visible on top.
  # --------------------------------------------------------------------------
  
  road_major_sf <- NULL
  road_secondary_sf <- NULL
  
  if (
    !is.null(map_bbox) &&
    roads != "none"
  ) {
    major <- c(
      "motorway",
      "motorway_link",
      "trunk",
      "trunk_link",
      "primary",
      "primary_link"
    )
    
    secondary <- c(
      "secondary",
      "secondary_link"
    )
    
    road_sf <- hurricane_osm_lines_cached(
      map_bbox = map_bbox,
      key = "highway",
      values = if (roads == "secondary") {
        c(major, secondary)
      } else {
        major
      },
      tag = paste0("roads_", roads),
      cache_dir = cache_dir
    )
    
    if (!is.null(road_sf)) {
      road_major_sf <- road_sf[
        road_sf$highway %in% major,
        ,
        drop = FALSE
      ]
      
      if (roads == "secondary") {
        road_secondary_sf <- road_sf[
          road_sf$highway %in% secondary,
          ,
          drop = FALSE
        ]
      }
    }
  }
  
  # --------------------------------------------------------------------------
  # Observed best track through the CURRENT MAP TIME.
  # --------------------------------------------------------------------------
  
  if (!is.null(storm_id)) {
    track <- tryCatch(
      get_storm_track(
        storm_id,
        src = track_src
      ),
      error = function(e) {
        warning(
          "Best track unavailable: ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
    
    if (!is.null(track)) {
      observed <- track[
        track$time <= map_time_utc,
        ,
        drop = FALSE
      ]
      
      if (nrow(observed) > 0) {
        obs_sf <- sf::st_as_sf(
          observed,
          coords = c("lon", "lat"),
          crs = 4326,
          remove = FALSE
        ) |>
          sf::st_transform(3857)
        
        observed_points_sf <- obs_sf
        
        if (nrow(obs_sf) >= 2) {
          obs_line <- obs_sf |>
            dplyr::arrange(time) |>
            dplyr::summarise(
              geometry = sf::st_combine(geometry)
            ) |>
            sf::st_cast("LINESTRING")
          
          # The line is drawn once, in the tracker-style block further down.
          # (A duplicate layer here used to add a stray "colour" legend to
          # panels that were meant to show only the population colorbar.)
          observed_line_sf <- obs_line
          
        }
        
        current_center <- obs_sf |>
          dplyr::slice_max(
            time,
            n = 1,
            with_ties = FALSE
          )
      }
      
      # ----------------------------------------------------------------------
      # Forecast path.
      #
      # Priority:
      #   1) archived/current TRACK KMZ if supplied
      #   2) OFCL ATCF forecast at or before the map time
      #
      # Forecast is deliberately RED + DASHED, unlike the observed black track.
      # ----------------------------------------------------------------------
      
      fcst_kmz <- read_nhc_kmz(
        forecast_track_kmz
      )
      
      if (!is.null(fcst_kmz)) {
        fcst_kmz <- sf::st_transform(
          fcst_kmz,
          3857
        )
        
        forecast_line_sf <- fcst_kmz
        
      } else {
        forecast <- get_forecast_for_time(
          storm_id = storm_id,
          cutoff_utc = map_time_utc,
          src = forecast_src
        )
        
        if (!is.null(forecast) && nrow(forecast) > 0) {
          fcst_sf <- sf::st_as_sf(
            forecast,
            coords = c("lon", "lat"),
            crs = 4326,
            remove = FALSE
          ) |>
            sf::st_transform(3857)
          
          start_sf <- NULL
          
          if (
            exists("current_center", inherits = FALSE) &&
            !is.null(current_center) &&
            nrow(current_center) > 0
          ) {
            start_sf <- current_center
          }
          
          line_pts <- if (!is.null(start_sf)) {
            rbind(
              start_sf["geometry"],
              fcst_sf["geometry"]
            )
          } else {
            fcst_sf["geometry"]
          }
          
          if (nrow(line_pts) >= 2) {
            fcst_line <- line_pts |>
              dplyr::summarise(
                geometry = sf::st_combine(geometry)
              ) |>
              sf::st_cast("LINESTRING")
            
            forecast_line_sf <- fcst_line
            forecast_points_sf <- fcst_sf
          }
        }
      }
    }
  }
  
  # --------------------------------------------------------------------------
  # Optional NHC uncertainty cone.
  # --------------------------------------------------------------------------
  
  cone <- read_nhc_kmz(
    forecast_cone_kmz
  )
  
  if (!is.null(cone)) {
    cone <- sf::st_transform(
      cone,
      3857
    )
    
    p1 <- p1 +
      ggplot2::geom_sf(
        data = cone,
        fill = "skyblue2",
        colour = "steelblue4",
        alpha = 0.16,
        linewidth = 0.35,
        inherit.aes = FALSE
      )
  }
  
  # --------------------------------------------------------------------------
  # Restore Bing population polygons.
  # --------------------------------------------------------------------------
  
  if (!is.null(pop_layer)) {
    p1$layers <- append(
      p1$layers,
      list(pop_layer),
      after = length(p1$layers)
    )
  }
  
  # --------------------------------------------------------------------------
  # Draw roads ABOVE the Bing tiles.
  # --------------------------------------------------------------------------
  
  if (
    !is.null(road_major_sf) &&
    nrow(road_major_sf) > 0
  ) {
    p1 <- p1 +
      ggplot2::geom_sf(
        data = road_major_sf,
        colour = "black",
        linewidth = 0.45,
        alpha = 0.82,
        inherit.aes = FALSE
      )
  }
  
  if (
    !is.null(road_secondary_sf) &&
    nrow(road_secondary_sf) > 0
  ) {
    p1 <- p1 +
      ggplot2::geom_sf(
        data = road_secondary_sf,
        colour = "grey35",
        linewidth = 0.25,
        alpha = 0.72,
        inherit.aes = FALSE
      )
  }
  
  # --------------------------------------------------------------------------
  # TRACKER-STYLE HURRICANE PATH
  #
  # Match hurricane_tracker.R:
  #   observed track  = solid grey/black path + circular category dots
  #   forecast track  = red dashed path + diamond category dots
  #   dot fill        = Saffir-Simpson / storm type
  #   dot size        = max sustained wind (kt)
  #
  # Because the population plot already uses a fill scale, start a fresh fill
  # scale here for the hurricane-category dots.
  # --------------------------------------------------------------------------
  
  if (
    !is.null(observed_line_sf) ||
    !is.null(observed_points_sf) ||
    !is.null(forecast_line_sf) ||
    !is.null(forecast_points_sf)
  ) {
    if (!requireNamespace("ggnewscale", quietly = TRUE)) {
      stop(
        "Package `ggnewscale` is required for hurricane intensity dots. ",
        "Install it once with install.packages('ggnewscale')."
      )
    }
    
    p1 <- p1 +
      ggnewscale::new_scale_fill()
  }
  
  if (
    !is.null(observed_line_sf) &&
    nrow(observed_line_sf) > 0
  ) {
    if (show_path_legend) {
      p1 <- p1 +
        ggplot2::geom_sf(
          data = observed_line_sf,
          ggplot2::aes(colour = "Observed best track"),
          linewidth = 0.8,
          linetype = "solid",
          inherit.aes = FALSE,
          # NA = appear only in the guides whose aesthetic this layer maps
          # (colour). TRUE forced the line into the storm-intensity and
          # max-wind keys, which is what drew lines through those symbols.
          show.legend = NA
        )
      legend_values["Observed best track"] <- "grey20"
    } else {
      p1 <- p1 +
        ggplot2::geom_sf(
          data = observed_line_sf,
          colour = "grey20",
          linewidth = 0.8,
          linetype = "solid",
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
  }
  
  if (
    !is.null(observed_points_sf) &&
    nrow(observed_points_sf) > 0
  ) {
    p1 <- p1 +
      ggplot2::geom_sf(
        data = observed_points_sf,
        ggplot2::aes(
          fill = category,
          size = wind_kt
        ),
        shape = 21,
        colour = "grey20",
        stroke = 0.35,
        inherit.aes = FALSE
      )
  }
  
  if (
    !is.null(forecast_line_sf) &&
    nrow(forecast_line_sf) > 0
  ) {
    if (show_path_legend) {
      p1 <- p1 +
        ggplot2::geom_sf(
          data = forecast_line_sf,
          ggplot2::aes(colour = "Official forecast track"),
          linewidth = 0.9,
          linetype = "dashed",
          inherit.aes = FALSE,
          show.legend = NA
        )
      legend_values["Official forecast track"] <- "red3"
    } else {
      p1 <- p1 +
        ggplot2::geom_sf(
          data = forecast_line_sf,
          colour = "red3",
          linewidth = 0.9,
          linetype = "dashed",
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
  }
  
  if (
    !is.null(forecast_points_sf) &&
    nrow(forecast_points_sf) > 0
  ) {
    p1 <- p1 +
      ggplot2::geom_sf(
        data = forecast_points_sf,
        ggplot2::aes(
          fill = category,
          size = wind_kt
        ),
        shape = 23,
        colour = "red3",
        stroke = 0.7,
        inherit.aes = FALSE
      )
  }
  
  if (
    !is.null(observed_points_sf) ||
    !is.null(forecast_points_sf)
  ) {
    if (show_storm_legend) {
      p1 <- p1 +
        ggplot2::scale_fill_manual(
          values = SS_COLORS,
          limits = SS_LEVELS,
          drop = FALSE,
          name = "Storm intensity",
          # Right-hand legend column: vertical, compact keys. `direction` is
          # set explicitly because the population_plot_n_* wrappers set
          # legend.direction = "horizontal" / legend.key.width = 1.4 cm for
          # the bottom colorbar, which would otherwise lay these out as wide
          # horizontal rows and squeeze the map.
          guide = ggplot2::guide_legend(
            direction = "vertical",
            override.aes = list(
              shape = 21,
              size = 3.5
            ),
            order = 1,
            position = "right",
            theme = ggplot2::theme(
              legend.key.width = grid::unit(0.5, "cm"),
              legend.key.height = grid::unit(0.45, "cm")
            )
          )
        ) +
        ggplot2::scale_size_continuous(
          range = c(1.8, 5.5),
          name = "Max wind (kt)",
          guide = ggplot2::guide_legend(
            direction = "vertical",
            override.aes = list(
              fill = "grey85",
              colour = "grey20"
            ),
            order = 2,
            position = "right",
            theme = ggplot2::theme(
              legend.key.width = grid::unit(0.5, "cm")
            )
          )
        )
    } else {
      p1 <- p1 +
        ggplot2::scale_fill_manual(
          values = SS_COLORS,
          limits = SS_LEVELS,
          drop = FALSE,
          guide = "none"
        ) +
        ggplot2::scale_size_continuous(
          range = c(1.8, 5.5),
          guide = "none"
        )
    }
  }
  
  # --------------------------------------------------------------------------
  # City pointers remain the top annotation group.
  # --------------------------------------------------------------------------
  
  p1 <- hurricane_city_layers(
    p1,
    cities = cities
  )
  
  # --------------------------------------------------------------------------
  # Combined feature legend.
  # --------------------------------------------------------------------------
  
  if (length(legend_values) > 0) {
    p1 <- p1 +
      ggplot2::scale_colour_manual(
        name = NULL,
        values = legend_values,
        breaks = names(legend_values),
        # Track/feature key joins the storm legends in the right-hand column
        # (so the population colorbar is the only legend under the map).
        guide = ggplot2::guide_legend(
          direction = "vertical",
          order = 3,
          position = "right",
          override.aes = list(
            linewidth = 0.9,
            linetype = ifelse(
              names(legend_values) == "Official forecast track",
              "dashed",
              "solid"
            )
          ),
          theme = ggplot2::theme(
            legend.key.width = grid::unit(1, "cm")
          )
        )
      )
  }
  
  p1
}

# ------------------------------------------------------------------------------
# Auto-apply when sourced inside population_plot()
# ------------------------------------------------------------------------------

if (
  exists("p1", inherits = FALSE) &&
  exists("map_bbox", inherits = FALSE)
) {
  cfg <- if (
    exists("disaster_config", inherits = FALSE) &&
    !is.null(disaster_config)
  ) {
    disaster_config
  } else if (exists("hurricane_config", inherits = TRUE)) {
    get("hurricane_config", inherits = TRUE)
  } else {
    list()
  }
  
  p1 <- add_hurricane_layers(
    p1 = p1,
    map_bbox = map_bbox,
    osm = if (exists("osm", inherits = FALSE)) osm else NULL,
    pop_sf = if (exists("first_time_pop", inherits = FALSE)) first_time_pop else NULL,
    fill_col = if (exists("fill_col", inherits = FALSE)) fill_col else NULL,
    plot_ds = if (exists("plot_ds", inherits = FALSE)) plot_ds else NULL,
    plot_hour = if (exists("plot_hour", inherits = FALSE)) plot_hour else NULL,
    tzone = if (exists("tzone", inherits = FALSE)) tzone else "UTC",
    config = cfg
  )
}