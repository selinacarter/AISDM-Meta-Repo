
require(sf)
require(terra)
require(elevatr)
require(osmdata)
require(ggplot2)

# ---- SETTINGS (edit here) ----------------------------------------------------
flood_zoom       <- 8            # elevation detail: 8 ~ 500 m; 9-10 for small boxes
flood_contour_by <- 500          # metres
flood_rivers     <- "major"      # "none" | "major" (rivers) | "all" (+ streams)
flood_roads      <- "major"      # "none" | "major" | "secondary"
flood_pop_alpha  <- 0.7          # <1 lets terrain show through the cells
flood_fill_sigma <- 100          # smaller = small red/blue values stand out more
flood_poi_lon    <- 85.5             # glacier outburst site
flood_poi_lat    <- 28.3
# Label offsets from their point, in metres (dx: + east / - west, dy: + north / - south)
flood_glacier_dx   <- -62000   # label centre this far WEST of the cross (clears the marker)
flood_glacier_dy   <-      0
flood_kathmandu_dx <-      0
flood_kathmandu_dy <- -30000
flood_label_size   <- 3.6        # text size; smaller = fits easier
kathmandu_arrow_col <- "gold"    # try "yellow", "gold", "#FFD700"
flood_start_date    <- "26 August 2026"   # shown under each panel title (NA = hidden)
flood_show_arrows   <- FALSE         # FALSE: no arrows, so nothing covers the red cells
flood_arrow_lw      <- 0.6       # arrow line width (was 1.2)
flood_arrow_head_cm <- 0.25      # arrow head length in cm (was 0.35)
flood_dot_size     <- 1        # Kathmandu dot size (was 2.5)
# ------------------------------------------------------------------------------

add_flood_layers <- function(p1, map_bbox, osm = NULL,
                             zoom = flood_zoom, contour_by = flood_contour_by,
                             rivers = flood_rivers, roads = flood_roads,
                             pop_alpha = flood_pop_alpha) {
  
  cache_dir <- "map_cache"
  dir.create(cache_dir, showWarnings = FALSE)
  
  osm_lines_cached <- function(key, values, tag) {
    f <- file.path(cache_dir, paste0(tag, "_", bb_tag, ".rds"))
    if (file.exists(f)) return(readRDS(f))
    
    # Try several Overpass servers; the default one often times out or rate-limits.
    servers <- c("https://overpass-api.de/api/interpreter",
                 "https://overpass.kumi.systems/api/interpreter",
                 "https://overpass.private.coffee/api/interpreter")
    out <- NULL
    for (srv in rep(servers, 3)) {          # up to 3 rounds over the servers
      if (!is.null(out)) break
      message("Downloading OSM ", tag, " from ", sub("https://([^/]+)/.*", "\\1", srv), " ...")
      res <- tryCatch({
        osmdata::set_overpass_url(srv)
        q <- osmdata::opq(bbox = unname(map_bbox), timeout = 180) |>
          osmdata::add_osm_feature(key = key, value = values)
        x <- osmdata::osmdata_sf(q)$osm_lines
        if (is.null(x) || nrow(x) == 0) "empty" else sf::st_transform(x[, key], 3857)
      }, error = function(e) {
        message("   failed: ", substr(conditionMessage(e), 1, 120)); NULL
      })
      if (inherits(res, "sf")) { out <- res; break }
      if (identical(res, "empty")) { message("   server returned 0 features"); }
      Sys.sleep(3)                            # brief pause before the next attempt
    }
    if (is.null(out)) {
      message("*** ", tag, " could NOT be downloaded -> this layer will be missing. ",
              "Re-run later (failures are not cached).")
    } else {
      saveRDS(out, f)
    }
    out
  }
  
  leg <- character(0)   # legend entries: label = colour
  if (!is.null(map_bbox)) {   # NULL when population_plot() got no lon/lat limits
    bb_tag <- paste(round(unname(map_bbox), 2), collapse = "_")
    
    # 0. let terrain show through the population cells (polygon layer is last so far)
    p1$layers[[length(p1$layers)]]$aes_params$alpha <- pop_alpha
    p1$layers[[length(p1$layers)]]$aes_params$colour    <- "grey85"
    p1$layers[[length(p1$layers)]]$aes_params$linewidth <- 0.05
    
    # 1. ELEVATION: gradient (under cells) + contours (over cells)
    tryCatch({
      f <- file.path(cache_dir, paste0("elev_z", zoom, "_", bb_tag, ".tif"))
      if (!file.exists(f)) {
        message("Downloading elevation (first time only) ...")
        loc <- data.frame(x = unname(c(map_bbox["xmin"], map_bbox["xmax"])),
                          y = unname(c(map_bbox["ymin"], map_bbox["ymax"])))
        r0 <- terra::rast(elevatr::get_elev_raster(loc, prj = 4326, z = zoom,
                                                   src = "aws", clip = "bbox",
                                                   verbose = FALSE))
        names(r0) <- "elevation"
        terra::writeRaster(r0, f, overwrite = TRUE)
      }
      elev <- terra::project(terra::rast(f), "EPSG:3857")
      names(elev) <- "elevation"
      if (terra::ncell(elev) > 2e5)
        elev <- terra::aggregate(elev, fact = ceiling(sqrt(terra::ncell(elev) / 2e5)))
      elev_df <- terra::as.data.frame(elev, xy = TRUE, na.rm = TRUE)
      
      grad_layer <- ggplot2::geom_raster(
        data = elev_df, ggplot2::aes(x = x, y = y, alpha = elevation),
        fill = "grey15", inherit.aes = FALSE)
      p1$layers <- append(p1$layers, list(grad_layer),
                          after = if (!is.null(osm)) 1L else 0L)
      p1 <- p1 + ggplot2::scale_alpha_continuous(range = c(0, 0.5), guide = "none")
      
      rng    <- range(elev_df$elevation, na.rm = TRUE)
      levels <- seq(floor(rng[1] / contour_by) * contour_by,
                    ceiling(rng[2] / contour_by) * contour_by, by = contour_by)
      ct       <- sf::st_as_sf(terra::as.contour(elev, levels = levels))
      ct_index <- ct[ct$level %% 1000 == 0, ]
      p1 <- p1 +
        ggplot2::geom_sf(data = ct, ggplot2::aes(colour = "Contours"), linewidth = 0.12,
                         alpha = 0.6, inherit.aes = FALSE, show.legend = "line") +
        ggplot2::geom_sf(data = ct_index, color = "grey20", linewidth = 0.3,
                         alpha = 0.8, inherit.aes = FALSE)
      leg <- c(leg, "Contours" = "grey30")
    }, error = function(e) warning("Elevation failed: ", conditionMessage(e), call. = FALSE))
    
    # 2. RIVERS / STREAMS
    if (rivers != "none") {
      w <- osm_lines_cached("waterway",
                            if (rivers == "major") "river" else c("river", "stream"),
                            paste0("water_", rivers))
      if (!is.null(w)) {
        p1 <- p1 + ggplot2::geom_sf(data = w[w$waterway == "river", ],
                                    ggplot2::aes(colour = "Rivers"), linewidth = 0.6,
                                    alpha = 0.9, inherit.aes = FALSE, show.legend = "line")
        leg <- c(leg, "Rivers" = "dodgerblue3")
        if (rivers == "all") {
          p1 <- p1 + ggplot2::geom_sf(data = w[w$waterway == "stream", ],
                                      ggplot2::aes(colour = "Streams"), linewidth = 0.2,
                                      alpha = 0.7, inherit.aes = FALSE, show.legend = "line")
          leg <- c(leg, "Streams" = "deepskyblue3")
        }
      }
    }
    
    # 3. ROADS
    if (roads != "none") {
      major <- c("motorway", "motorway_link", "trunk", "trunk_link",
                 "primary", "primary_link")
      minor <- c("secondary", "secondary_link")
      rd <- osm_lines_cached("highway",
                             if (roads == "major") major else c(major, minor),
                             paste0("roads_", roads))
      if (!is.null(rd)) {
        p1 <- p1 + ggplot2::geom_sf(data = rd[rd$highway %in% major, ],
                                    ggplot2::aes(colour = "Major roads"), linewidth = 0.7,
                                    alpha = 0.85, inherit.aes = FALSE, show.legend = "line")
        leg <- c(leg, "Major roads" = "black")
        if (roads == "secondary") {
          p1 <- p1 + ggplot2::geom_sf(data = rd[rd$highway %in% minor, ],
                                      ggplot2::aes(colour = "Secondary roads"), linewidth = 0.3,
                                      alpha = 0.8, inherit.aes = FALSE, show.legend = "line")
          leg <- c(leg, "Secondary roads" = "grey35")
        }
      }
    }
  }
  
  # legend for contours / rivers / roads (bottom, next to the colour bar)
  if (length(leg) > 0)
    p1 <- p1 + ggplot2::scale_colour_manual(name = "Map features", values = leg,
                                            breaks = names(leg))
  
  # 4. GLACIER OUTBREAK MARKER
  flood_poi <- data.frame(lon = flood_poi_lon, lat = flood_poi_lat) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(3857)
  p1 + ggplot2::geom_sf(data = flood_poi, color = "darkorange", shape = 4,
                        size = 5, stroke = 2, inherit.aes = FALSE)
}

# ---- Auto-apply ONLY when sourced from inside population_plot() --------------
if (exists("p1", inherits = FALSE) && exists("map_bbox", inherits = FALSE)) {
  p1 <- add_flood_layers(p1, map_bbox,
                         osm = if (exists("osm", inherits = FALSE)) osm else NULL)
  
  # Colours: blue -> WHITE -> red, on a pseudo-log scale so small positive
  # (red) values are visible instead of being swamped by the 2000 maximum.
  # Overrides population_plot()'s local `fill_scale` before it is added.
  if (exists("metric", inherits = FALSE) && metric == "difference" &&
      exists("lims", inherits = FALSE)) {
    m <- max(abs(lims), na.rm = TRUE)
    fill_scale <- ggplot2::scale_fill_gradientn(
      # decrease: yellow (extreme) -> white | increase: white -> orange -> red (extreme)
      colours = c("#ffe600", "#fff7b0", "white", "#ff8a65", "red"),
      values  = c(0, 0.25, 0.5, 0.75, 1),
      limits  = c(-m, m), oob = scales::squish,
      trans   = scales::pseudo_log_trans(sigma = flood_fill_sigma),
      breaks  = c(-1000, -100, 0, 100, 1000),
      name    = "Users (crisis - baseline)")
  }
  
  if (!is.na(flood_start_date))
    p1 <- p1 + ggplot2::labs(subtitle = paste0("Disaster date: ", flood_start_date))
  
  message("[3_flood_functions.R] loaded | arrow lw = ", flood_arrow_lw,
          " | head = ", flood_arrow_head_cm, " cm")
  
  # ---- Keep glacier site in frame + draw labels/arrows ourselves -------------
  to_m <- function(lon, lat) sf::st_coordinates(sf::st_transform(sf::st_as_sf(
    data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326), 3857))
  
  if (exists("xlim", inherits = FALSE) && !is.null(xlim) &&
      exists("ylim", inherits = FALSE) && !is.null(ylim)) {
    
    pts <- data.frame(
      name  = c("Kathmandu", "Glacier Outbreak"),
      text  = c("Kathmandu", "Glacier\nOutbreak"),      # 2 lines = narrower box
      lon   = c(85.3240, flood_poi_lon),
      lat   = c(27.7172, flood_poi_lat),
      dx    = c(flood_kathmandu_dx, flood_glacier_dx),
      dy    = c(flood_kathmandu_dy, flood_glacier_dy),
      col   = c(kathmandu_arrow_col, "black")
    )
    xy <- to_m(pts$lon, pts$lat)
    pts$x <- xy[, 1]; pts$y <- xy[, 2]
    
    # widen the window so the glacier site is inside (pad ~0.1 deg)
    pad  <- 11000
    xlim <- range(xlim, pts$x[2] - pad, pts$x[2] + pad)
    ylim <- range(ylim, pts$y[2] - pad, pts$y[2] + pad)
    
    # label centres, clamped so the whole text box stays inside the map
    pts$lx <- pmin(pmax(pts$x + pts$dx, xlim[1] + 55000), xlim[2] - 55000)
    pts$ly <- pmin(pmax(pts$y + pts$dy, ylim[1] + 25000), ylim[2] - 25000)
    
    # population_plot() would draw these itself (fixed black arrows, fixed
    # 80 km placement), so remove them from `labels`.
    if (exists("labels", inherits = FALSE) && !is.null(labels)) {
      labels <- labels[!labels$label %in% pts$name, ]
      message("[3_flood_functions.R] labels left for population_plot(): ", nrow(labels),
              "  (should be 0 -> its thick black arrows are off)")
    }
    
    arr <- grid::arrow(length = grid::unit(flood_arrow_head_cm, "cm"), type = "closed")
    if (flood_show_arrows) {
      p1 <- p1 +
        ggplot2::annotate("segment", x = pts$lx, y = pts$ly, xend = pts$x, yend = pts$y,
                          colour = "black", linewidth = flood_arrow_lw * 2, arrow = arr) +
        ggplot2::annotate("segment", x = pts$lx, y = pts$ly, xend = pts$x, yend = pts$y,
                          colour = pts$col, linewidth = flood_arrow_lw, arrow = arr)
    } else {
      # no arrows: put the label box right next to its point, and mark Kathmandu
      pts$lx <- pts$x + c(flood_kathmandu_dx, flood_glacier_dx)
      pts$ly <- pts$y + c(flood_kathmandu_dy, flood_glacier_dy)
      pts$lx <- pmin(pmax(pts$lx, xlim[1] + 55000), xlim[2] - 55000)
      pts$ly <- pmin(pmax(pts$ly, ylim[1] + 25000), ylim[2] - 25000)
      p1 <- p1 + ggplot2::annotate("point", x = pts$x[1], y = pts$y[1],
                                   shape = 21, fill = "black", colour = "white",
                                   size = 2.5, stroke = 0.8)
    }
    p1 <- p1 +
      ggplot2::annotate("label", x = pts$lx, y = pts$ly, label = pts$text,
                        colour = "black", fill = "white", alpha = 0.85, size = flood_label_size,
                        fontface = "bold", label.size = 0.3, lineheight = 0.9)
  }
}