
# ==============================================================================
# 3_flood_functions.R
#
# Defines add_flood_layers(). Safe to source/run directly (it only defines a
# function). When sourced from inside population_plot() (disaster_type = "flood",
# local = TRUE) the last lines apply it to that function's `p1` automatically.
#
# Adds: elevation gradient, contours, rivers, streams, roads, glacier marker.
# Downloads are cached in ./map_cache, so re-runs and the 2nd panel are fast.
# ==============================================================================
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
flood_glacier_gap  <- 24000  # gap (m) between the Glacier label box and the cross = length of its arrow;
# the arrow tip ends exactly at the RIGHT edge of the label
flood_glacier_dy   <-      0
flood_kathmandu_dx <-  70000   # label centre EAST of the dot (top-right)
flood_kathmandu_dy <-  40000   # ... and NORTH of it
# Visibility of the user shading: cells with |users change| >= flood_hl_min are
# re-drawn ON TOP of contours and rivers (below roads), nearly opaque.
flood_hl_min       <- 25         # users; lower = more cells on top, higher = only hotspots
flood_hl_alpha     <- 0.95
flood_river_lw     <- 0.35       # rivers: thinner and fainter (was 0.6 / 0.9)
flood_river_alpha  <- 0.55
flood_bharatpur_lon <- 84.4333   # Bharatpur, Nepal (27.6833 N, 84.4333 E)
flood_bharatpur_lat <- 27.6833
flood_bharatpur_nudge_x <- -20000  # shifts the Bharatpur label LEFT (negative) / right (positive), metres;
# stops at the map edge
flood_bharatpur_dx  <- 30000     # label offset from the dot (+ east)
flood_bharatpur_dy  <- -22000    # (- south)
# Thin arrow from the glacier outburst site towards Kathmandu
flood_flow_arrow    <- FALSE   # glacier -> Kathmandu arrow removed
flood_flow_arrow_lw <- 0.4       # thin
flood_flow_arrow_col <- "black"
# Town labels (Kathmandu, Bharatpur): larger text, auto-placed OFF the roads, with a thin arrow
flood_label_avoid_roads <- TRUE  # search for a spot whose box does not sit on a road
flood_arrow_col       <- "#FFD400"  # arrows: yellow (drawn over a thin black outline so they stay visible)
flood_town_arrow_lw   <- 0.5      # thin arrows: site -> label
flood_arrow_head_tuck <- 0.97     # <1 pushes the arrow tip INTO the label box edge (tip touches text); 1 = estimated edge
flood_kathmandu_angles <- c(10, 65)  # Kathmandu label must sit in this compass window, degrees
# counter-clockwise from EAST (0 = east, 90 = north) => north-east
flood_panel_width_in  <- getOption("flood_panel_width_in", 1.9)      # width of ONE map panel in the plot as you view it, in inches (calibrated
# from your screenshots). Label boxes are measured in real text units
# and converted with this; if a resized plot pane makes labels look
# bigger/smaller relative to the map, scale it (smaller = bigger boxes)
flood_box_scale       <- 1.0     # raise (1.2) if boxes still touch roads: assumes a bigger box
flood_hetauda_lon  <- 85.029716  # Hetauda, Nepal (27.429071 N, 85.029716 E)
flood_hetauda_lat  <- 27.429071
flood_hetauda_angles <- c(-70, 10) # label window for Hetauda, degrees from EAST (0 = east, -90 = south)
flood_dot_size     <- 1.0        # Kathmandu dot size (was 2.5)
flood_label_size   <- getOption("flood_label_size", 3.2)        # ONE size for Kathmandu, Bharatpur and Glacier Outbreak labels
kathmandu_arrow_col <- "gold"    # try "yellow", "gold", "#FFD700"
flood_start_date    <- "26 August 2026"   # shown under each panel title (NA = hidden)
flood_show_arrows   <- FALSE         # FALSE: no arrows, so nothing covers the red cells
flood_arrow_lw      <- 0.6       # arrow line width (was 1.2)
flood_arrow_head_cm <- 0.25      # arrow head length in cm (was 0.35)
# ------------------------------------------------------------------------------

add_flood_layers <- function(p1, map_bbox, osm = NULL,
                             zoom = flood_zoom, contour_by = flood_contour_by,
                             rivers = flood_rivers, roads = flood_roads,
                             pop_alpha = flood_pop_alpha,
                             pop_sf = NULL, fill_col = NULL, hl_min = flood_hl_min) {
  
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
                                    ggplot2::aes(colour = "Rivers"), linewidth = flood_river_lw,
                                    alpha = flood_river_alpha, inherit.aes = FALSE, show.legend = "line")
        leg <- c(leg, "Rivers" = "dodgerblue3")
        if (rivers == "all") {
          p1 <- p1 + ggplot2::geom_sf(data = w[w$waterway == "stream", ],
                                      ggplot2::aes(colour = "Streams"), linewidth = 0.15,
                                      alpha = flood_river_alpha * 0.8, inherit.aes = FALSE, show.legend = "line")
          leg <- c(leg, "Streams" = "deepskyblue3")
        }
      }
    }
    
    # 2b. USER SHADING ON TOP of contours + rivers (roads are added after this)
    if (!is.null(pop_sf) && !is.null(fill_col) && fill_col %in% names(pop_sf)) {
      v  <- pop_sf[[fill_col]]
      hi <- pop_sf[!is.na(v) & abs(v) >= hl_min, ]
      if (nrow(hi) > 0)
        p1 <- p1 + ggplot2::geom_sf(data = hi,
                                    ggplot2::aes(fill = .data[[fill_col]]),
                                    colour = NA, alpha = flood_hl_alpha,
                                    inherit.aes = FALSE)
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
                                    ggplot2::aes(colour = "Major roads"), linewidth = 0.55,
                                    alpha = 0.75, inherit.aes = FALSE, show.legend = "line")
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
  p1 <- add_flood_layers(
    p1, map_bbox,
    osm      = if (exists("osm", inherits = FALSE)) osm else NULL,
    pop_sf   = if (exists("first_time_pop", inherits = FALSE)) first_time_pop else NULL,
    fill_col = if (exists("fill_col", inherits = FALSE)) fill_col else NULL)
  
  # Colours: blue -> WHITE -> red, on a pseudo-log scale so small positive
  # (red) values are visible instead of being swamped by the 2000 maximum.
  # Overrides population_plot()'s local `fill_scale` before it is added.
  if (exists("metric", inherits = FALSE) && metric == "difference" &&
      exists("lims", inherits = FALSE)) {
    m <- max(abs(lims), na.rm = TRUE)
    fill_scale <- ggplot2::scale_fill_gradientn(
      # decrease: yellow (extreme) -> white | increase: white -> orange -> red (extreme)
      # decrease: red (extreme) -> white | increase: white -> greens -> dark jungle green (extreme)
      colours = c("red", "#ff9a8a", "white", "#74c69d", "#2d6a4f", "#1A2421"),
      values  = c(0, 0.25, 0.5, 0.62, 0.8, 1),
      limits  = c(-m, m), oob = scales::squish,
      trans   = scales::pseudo_log_trans(sigma = flood_fill_sigma),
      breaks  = c(-1000, -100, 0, 100, 1000),
      name    = "Users (crisis - baseline)")
  }
  
  if (!is.na(flood_start_date))
    p1 <- p1 + ggplot2::labs(subtitle = paste0("Disaster date: ", flood_start_date))
  
  message("[3_flood_functions.R] loaded | label size = ", flood_label_size,
          " | dot size = ", flood_dot_size, " | Kathmandu offset (m) = ",
          flood_kathmandu_dx, ",", flood_kathmandu_dy)
  
  # ---- Keep glacier site in frame + draw labels/arrows ourselves -------------
  to_m <- function(lon, lat) sf::st_coordinates(sf::st_transform(sf::st_as_sf(
    data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326), 3857))
  
  if (exists("xlim", inherits = FALSE) && !is.null(xlim) &&
      exists("ylim", inherits = FALSE) && !is.null(ylim)) {
    
    pts <- data.frame(
      name = c("Kathmandu", "Glacier Outbreak", "Bharatpur", "Hetauda"),
      text = c("Kathmandu", "Glacier\nOutbreak", "Bharatpur", "Hetauda"),   # 2 lines = narrower box
      lon  = c(85.3240, flood_poi_lon, flood_bharatpur_lon, flood_hetauda_lon),
      lat  = c(27.7172, flood_poi_lat, flood_bharatpur_lat, flood_hetauda_lat),
      size = rep(flood_label_size, 4)
    )
    xy <- to_m(pts$lon, pts$lat)
    pts$x <- xy[, 1]; pts$y <- xy[, 2]
    
    # widen the window so the glacier site (row 2) is inside (pad ~0.1 deg)
    pad  <- 11000
    xlim <- range(xlim, pts$x[2] - pad, pts$x[2] + pad)
    ylim <- range(ylim, pts$y[2] - pad, pts$y[2] + pad)
    xr <- diff(xlim); yr <- diff(ylim)
    
    # label box size from the REAL text width (bold, same font size as the label), converted
    # to map metres. Much more accurate than counting characters.
    m_per_in <- xr / flood_panel_width_in
    txt_in <- function(txt, size) {
      ln <- strsplit(txt, "\n")[[1]]
      fs <- size * ggplot2::.pt
      m <- tryCatch({
        tg <- grid::textGrob(paste(ln, collapse = "\n"),
                             gp = grid::gpar(fontsize = fs, fontface = "bold", lineheight = 0.9))
        pad <- 2 * 0.25 * fs * 0.9 / 72.27 + 0.012          # label.padding + border, inches
        c(w = grid::convertWidth(grid::grobWidth(tg), "in", valueOnly = TRUE) + pad,
          h = grid::convertHeight(grid::grobHeight(tg), "in", valueOnly = TRUE) + pad)
      }, error = function(e) c(w = NA_real_, h = NA_real_))
      if (anyNA(m) || any(m <= 0))                          # no graphics device: character-count estimate
        m <- c(w = 0.0128 * size * max(nchar(ln)) * flood_panel_width_in,
               h = 0.0203 * size * length(ln)      * flood_panel_width_in)
      m
    }
    box_vis <- function(txt, size) {        # half-size of the visible box (arrow tips, spacing)
      d <- txt_in(txt, size)
      c(w = unname(0.5 * d["w"] * m_per_in), h = unname(0.5 * d["h"] * m_per_in))
    }
    box_half <- function(txt, size) {       # same + small clearance (used to keep boxes off roads)
      v <- box_vis(txt, size)
      c(w = unname(v["w"]) * 1.05 * flood_box_scale, h = unname(v["h"]) * 1.08 * flood_box_scale)
    }
    
    # roads (from the cache written above) used to keep label boxes off them
    roads_geom <- NULL
    if (flood_label_avoid_roads && flood_roads != "none" && !is.null(map_bbox)) {
      rf <- file.path("map_cache", paste0("roads_", flood_roads, "_",
                                          paste(round(unname(map_bbox), 2), collapse = "_"), ".rds"))
      if (file.exists(rf)) roads_geom <- sf::st_geometry(readRDS(rf))
    }
    
    # ---- glacier label: fixed spot just west of the cross --------------------
    hg <- box_half(pts$text[2], pts$size[2])
    hgv <- box_vis(pts$text[2], pts$size[2])
    gx <- min(max(pts$x[2] - hgv["w"] - flood_glacier_gap, xlim[1] + hg["w"]), xlim[2] - hg["w"])
    gy <- min(max(pts$y[2] + flood_glacier_dy, ylim[1] + hg["h"]), ylim[2] - hg["h"])
    
    # thin arrow glacier -> Kathmandu (also treated as an obstacle for label boxes)
    g <- c(pts$x[2], pts$y[2]); k <- c(pts$x[1], pts$y[1])
    u <- (k - g) / sqrt(sum((k - g)^2))
    fa0 <- g + u * 14000; fa1 <- k - u * 9000
    flow_pts <- if (flood_flow_arrow) cbind(seq(fa0[1], fa1[1], length.out = 40),
                                            seq(fa0[2], fa1[2], length.out = 40)) else cbind(-1e12, -1e12)
    
    # obstacles: c(xmin, xmax, ymin, ymax)
    obstacles <- list(
      c(gx - hg["w"], gx + hg["w"], gy - hg["h"], gy + hg["h"]),               # glacier label
      c(g[1] - 16000, g[1] + 16000, g[2] - 16000, g[2] + 16000),               # glacier cross
      c(pts$x[1] - 7000, pts$x[1] + 7000, pts$y[1] - 7000, pts$y[1] + 7000),   # Kathmandu dot
      c(pts$x[3] - 7000, pts$x[3] + 7000, pts$y[3] - 7000, pts$y[3] + 7000),   # Bharatpur dot
      c(pts$x[4] - 7000, pts$x[4] + 7000, pts$y[4] - 7000, pts$y[4] + 7000)    # Hetauda dot
    )
    
    # cost of a candidate box = road length under it (Inf if it leaves the map or hits an obstacle)
    box_cost <- function(x0, x1, y0, y1) {
      if (x0 < xlim[1] || x1 > xlim[2] || y0 < ylim[1] || y1 > ylim[2]) return(Inf)
      if (any(vapply(obstacles, function(o) x0 < o[2] && x1 > o[1] && y0 < o[4] && y1 > o[3],
                     logical(1)))) return(Inf)
      if (any(flow_pts[, 1] > x0 & flow_pts[, 1] < x1 & flow_pts[, 2] > y0 & flow_pts[, 2] < y1)) return(Inf)
      road_len <- 0
      if (!is.null(roads_geom)) {
        box <- sf::st_as_sfc(sf::st_bbox(c(xmin = unname(x0), ymin = unname(y0),
                                           xmax = unname(x1), ymax = unname(y1)),
                                         crs = sf::st_crs(3857)))
        hit <- lengths(sf::st_intersects(roads_geom, box)) > 0
        if (any(hit)) road_len <- tryCatch(
          sum(as.numeric(sf::st_length(suppressWarnings(
            sf::st_intersection(roads_geom[hit], box))))),
          error = function(e) 20000 * sum(hit))
      }
      road_len
    }
    
    # ---- label in a compass window, least road underneath --------------------
    place_label <- function(px, py, hw, hh, prefer_deg, fallback, angles = seq(0, 345, by = 15)) {
      hw <- unname(hw); hh <- unname(hh)    # names would leak into st_bbox() (xmin.w -> NA)
      best <- NULL; best_cost <- Inf
      for (r in c(25000, 35000, 45000, 55000, 70000, 85000)) {
        for (a in angles) {
          cx <- px + r * cos(a * pi / 180); cy <- py + r * sin(a * pi / 180)
          rc <- box_cost(cx - hw, cx + hw, cy - hh, cy + hh)
          if (!is.finite(rc)) next
          adiff <- abs(((a - prefer_deg + 180) %% 360) - 180)
          cost  <- rc + 0.3 * r + 150 * adiff
          if (cost < best_cost) { best_cost <- cost; best <- c(cx, cy) }
        }
      }
      if (is.null(best)) fallback else best
    }
    
    # ---- label straight BELOW the point (box slides sideways only if the map edge forces it)
    place_south <- function(px, py, hw, hh, hv, hwv, fallback) {
      hw <- unname(hw); hh <- unname(hh); hv <- unname(hv); hwv <- unname(hwv)
      cx <- min(max(px + flood_bharatpur_nudge_x, xlim[1] + hwv), xlim[2] - hwv)   # as far left as the map allows
      best <- NULL; best_cost <- Inf
      for (gap in seq(10000, 110000, by = 3000)) {
        cy <- py - gap - hv
        rc <- box_cost(cx - hwv, cx + hwv, cy - hh, cy + hh)
        if (!is.finite(rc)) next
        cost <- rc + 0.3 * gap
        if (cost < best_cost) { best_cost <- cost; best <- c(cx, cy) }
      }
      if (is.null(best)) fallback else best
    }
    
    hk <- box_half(pts$text[1], pts$size[1])
    hb <- box_half(pts$text[3], pts$size[3])
    kang <- seq(flood_kathmandu_angles[1], flood_kathmandu_angles[2], by = 5)
    kfb  <- c(min(max(pts$x[1] + flood_kathmandu_dx, xlim[1] + hk["w"]), xlim[2] - hk["w"]),
              min(max(pts$y[1] + flood_kathmandu_dy, ylim[1] + hk["h"]), ylim[2] - hk["h"]))
    kc <- place_label(pts$x[1], pts$y[1], hk["w"], hk["h"],
                      prefer_deg = mean(flood_kathmandu_angles), fallback = kfb, angles = kang)
    obstacles[[length(obstacles) + 1]] <- c(kc[1] - hk["w"], kc[1] + hk["w"], kc[2] - hk["h"], kc[2] + hk["h"])
    hh4 <- box_half(pts$text[4], pts$size[4])
    hang <- seq(flood_hetauda_angles[1], flood_hetauda_angles[2], by = 5)
    hc <- place_label(pts$x[4], pts$y[4], hh4["w"], hh4["h"], prefer_deg = mean(flood_hetauda_angles),
                      fallback = c(min(pts$x[4] + 60000, xlim[2] - hh4["w"]), pts$y[4] - 15000), angles = hang)
    obstacles[[length(obstacles) + 1]] <- c(hc[1] - hh4["w"], hc[1] + hh4["w"], hc[2] - hh4["h"], hc[2] + hh4["h"])
    hbv <- box_vis(pts$text[3], pts$size[3])
    bc <- place_south(pts$x[3], pts$y[3], hb["w"], hb["h"], hbv["h"], hbv["w"],
                      fallback = c(pts$x[3], pts$y[3] - 60000))
    
    pts$lx <- c(kc[1], gx, bc[1], hc[1])
    pts$ly <- c(kc[2], gy, bc[2], hc[2])
    
    # population_plot() would draw these itself (fixed thick black arrows), so
    # remove them from `labels`; we draw labels, dots and arrows here.
    if (exists("labels", inherits = FALSE) && !is.null(labels))
      labels <- labels[!labels$label %in% pts$name, ]
    
    # dots on the three towns
    p1 <- p1 + ggplot2::annotate("point", x = pts$x[c(1, 3, 4)], y = pts$y[c(1, 3, 4)],
                                 shape = 21, fill = "black", colour = "white",
                                 size = flood_dot_size, stroke = 0.5)
    
    # thin arrow: glacier site -> Kathmandu
    if (flood_flow_arrow)
      p1 <- p1 + ggplot2::annotate("segment", x = fa0[1], y = fa0[2], xend = fa1[1], yend = fa1[2],
                                   colour = flood_flow_arrow_col, linewidth = flood_flow_arrow_lw,
                                   arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed"))
    
    # thin yellow arrows (black outline underneath), site -> label box, end to end
    draw_arrow <- function(x, y, xend, yend) {
      list(
        ggplot2::annotate("segment", x = x, y = y, xend = xend, yend = yend,
                          colour = "black", linewidth = flood_town_arrow_lw * 2.2,
                          arrow = grid::arrow(length = grid::unit(0.20, "cm"), type = "closed")),
        ggplot2::annotate("segment", x = x, y = y, xend = xend, yend = yend,
                          colour = flood_arrow_col, linewidth = flood_town_arrow_lw,
                          arrow = grid::arrow(length = grid::unit(0.17, "cm"), type = "closed")))
    }
    site_arrow <- function(i, hv, start_off, tuck = flood_arrow_head_tuck) {
      p0 <- c(pts$x[i], pts$y[i]); cc <- c(pts$lx[i], pts$ly[i]); d <- cc - p0
      dist <- sqrt(sum(d^2)); if (dist < 1) return(NULL)
      s_exit <- min(hv["w"] / max(abs(d[1]), 1e-9), hv["h"] / max(abs(d[2]), 1e-9))
      if (s_exit >= 1) return(NULL)                      # site is inside the box
      e  <- cc - d * s_exit * tuck                       # tip at the visible box edge
      st <- p0 + d / dist * start_off                    # start right at the site
      draw_arrow(st[1], st[2], e[1], e[2])
    }
    # Bharatpur: arrow points straight DOWN (south) to the top edge of its label box
    south_arrow <- function() {
      st_y <- pts$y[3] - 3000
      e_y  <- pts$ly[3] + hbv["h"] * flood_arrow_head_tuck
      if (st_y <= e_y) return(NULL)
      draw_arrow(pts$x[3], st_y, pts$x[3], e_y)
    }
    hkv <- box_vis(pts$text[1], pts$size[1]); hhv <- box_vis(pts$text[4], pts$size[4])
    for (a in list(site_arrow(1, hkv, 3000), site_arrow(2, hgv, 3000), south_arrow(), site_arrow(4, hhv, 3000)))
      if (!is.null(a)) p1 <- p1 + a
    
    p1 <- p1 +
      ggplot2::annotate("label", x = pts$lx, y = pts$ly, label = pts$text,
                        colour = "black", fill = "white", size = pts$size,
                        fontface = "bold", label.size = 0.3, lineheight = 0.9)
  }
}