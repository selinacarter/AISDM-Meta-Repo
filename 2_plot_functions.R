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

population_plot <- function(
    plot_ds,
    plot_hour,
    tzone,
    metric = c("difference", "crisis", "zscore"),
    title = TRUE,
    plot_title = NULL,
    lon_limits = NULL,   # c(min_lon, max_lon)
    lat_limits = NULL,   # c(min_lat, max_lat)
    zoom = 9,             # basemap detail
    fill_limits = NULL,  # force the fill scale limits
    labels = NULL,
    label_angles = NULL,
    highway_detail = c("none", "major", "secondary", "all"),
    disaster_type = NULL # "fire", "earthquake"
) {
  
  metric <- match.arg(metric)
  highway_detail <- match.arg(highway_detail)
  # If using match.arg inside, handle NULL safely like this:
  if (!is.null(disaster_type)) {
    disaster_type <- match.arg(disaster_type, c("fire", "earthquake"))
  }
  # ------------------------------------------------------------
  # Select coloured column
  # ------------------------------------------------------------
  
  fill_col <- switch(
    metric,
    difference = "Difference between baseline and crisis",
    crisis     = "# Users During Crisis",
    zscore     = "z_score"
  )
  
  # ------------------------------------------------------------
  # Defensive check for z_score
  # ------------------------------------------------------------
  
  if (metric == "zscore" && !("z_score" %in% names(tiles_3857))) {
    stop(
      "population_plot(metric='zscore'): column `z_score` not in tiles_3857. ",
      "Rebuild with build_tiles(fb_data_bing, re_run = TRUE); if still missing, ",
      "quadkey_df_to_polygon dropped it - join z_score back from fb_data_bing by quadkey."
    )
  }
  
  # ------------------------------------------------------------
  # Get requested population window
  # ------------------------------------------------------------
  
  first_time_pop <- tiles_3857 |>
    dplyr::filter(
      ds == plot_ds,
      hour == plot_hour
    )
  
  # Fail loudly if requested window does not exist
  if (nrow(first_time_pop) == 0) {
    stop(
      "population_plot(metric='", metric,
      "'): no data for ds=", plot_ds,
      ", hour=", plot_hour,
      ". Available windows: ",
      paste(
        sort(unique(paste(tiles_3857$ds, tiles_3857$hour))),
        collapse = ", "
      )
    )
  }
  
  # ------------------------------------------------------------
  # Fill scale limits
  # ------------------------------------------------------------
  
  lims <- range(
    tiles_3857[[fill_col]],
    na.rm = TRUE
  )
  
  # ------------------------------------------------------------
  # Default coordinate limits
  # ------------------------------------------------------------
  
  xlim <- NULL
  ylim <- NULL
  
  # ------------------------------------------------------------
  # Spatial filtering and basemap
  # ------------------------------------------------------------
  
  osm <- NULL
  
  if (!is.null(lon_limits) && !is.null(lat_limits)) {
    
    # Restrict population data to requested bounding box
    first_time_pop <- first_time_pop |>
      dplyr::filter(
        longitude >= lon_limits[1],
        longitude <= lon_limits[2],
        latitude >= lat_limits[1],
        latitude <= lat_limits[2]
      )
    
    # Recalculate limits from visible data
    if (nrow(first_time_pop) > 0) {
      lims <- range(
        first_time_pop[[fill_col]],
        na.rm = TRUE
      )
    }
    
    # ----------------------------------------------------------
    # Basemap bounding box
    # ----------------------------------------------------------
    
    pts <- rbind(
      data.frame(
        lon = lon_limits[1],
        lat = lat_limits[1]
      ),
      data.frame(
        lon = lon_limits[2],
        lat = lat_limits[2]
      )
    )
    
    pts_sf <- sf::st_as_sf(
      pts,
      coords = c("lon", "lat"),
      crs = 4326
    )
    
    # Download OpenStreetMap / CartoDB basemap
    osm <- get_tiles(
      pts_sf,
      provider = "CartoDB.Voyager",
      crop = TRUE,
      zoom = zoom
    )
    
    # ----------------------------------------------------------
    # Convert bounding box to EPSG:3857
    # ----------------------------------------------------------
    
    bbox_ll <- sf::st_as_sfc(
      sf::st_bbox(
        c(
          xmin = lon_limits[1],
          xmax = lon_limits[2],
          ymin = lat_limits[1],
          ymax = lat_limits[2]
        ),
        crs = sf::st_crs(4326)
      )
    )
    
    bbox_3857 <- sf::st_transform(
      bbox_ll,
      3857
    ) |>
      sf::st_bbox()
    
    xlim <- c(
      bbox_3857["xmin"],
      bbox_3857["xmax"]
    )
    
    ylim <- c(
      bbox_3857["ymin"],
      bbox_3857["ymax"]
    )
    
    # Bounding box used by OSM highway query
    map_bbox <- c(
      xmin = lon_limits[1],
      ymin = lat_limits[1],
      xmax = lon_limits[2],
      ymax = lat_limits[2]
    )
    
  } else {
    
    # ----------------------------------------------------------
    # No explicit spatial limits
    #
    # We don't download a highway layer in this case because
    # querying the entire tiles_3857 extent through OSM can be
    # unnecessarily expensive.
    # ----------------------------------------------------------
    
    map_bbox <- NULL
  }
  
  # ------------------------------------------------------------
  # Highway layer
  # ------------------------------------------------------------
  
  highway_sf <- NULL
  
  if (
    highway_detail != "none" &&
    !is.null(map_bbox)
  ) {
    
    # Define OSM highway classes by desired granularity
    highway_classes <- switch(
      highway_detail,
      
      # Major highways:
      # Freeways, trunks, primary roads
      major = c(
        "motorway",
        "motorway_link",
        "trunk",
        "trunk_link",
        "primary",
        "primary_link"
      ),
      
      # Add secondary roads
      secondary = c(
        "motorway",
        "motorway_link",
        "trunk",
        "trunk_link",
        "primary",
        "primary_link",
        "secondary",
        "secondary_link"
      ),
      
      # Include tertiary and local/unclassified roads
      all = c(
        "motorway",
        "motorway_link",
        "trunk",
        "trunk_link",
        "primary",
        "primary_link",
        "secondary",
        "secondary_link",
        "tertiary",
        "tertiary_link",
        "unclassified",
        "residential"
      )
    )
    
    # Query OpenStreetMap
    highway_query <- osmdata::opq(
      bbox = map_bbox
    ) |>
      osmdata::add_osm_feature(
        key = "highway",
        value = highway_classes
      )
    
    highway_sf <- tryCatch(
      
      osmdata::osmdata_sf(
        highway_query
      )$osm_lines,
      
      error = function(e) {
        warning(
          "Could not download highway data from OpenStreetMap: ",
          conditionMessage(e)
        )
        NULL
      }
    )
    
    # ----------------------------------------------------------
    # Transform highways to Web Mercator
    # ----------------------------------------------------------
    
    if (
      !is.null(highway_sf) &&
      nrow(highway_sf) > 0
    ) {
      
      highway_sf <- sf::st_transform(
        highway_sf,
        3857
      )
      
      # Remove empty geometries
      highway_sf <- highway_sf[
        !sf::st_is_empty(highway_sf),
        ,
        drop = FALSE
      ]
    }
  }
  
  # ------------------------------------------------------------
  # Shared-scale override
  # ------------------------------------------------------------
  
  if (!is.null(fill_limits)) {
    lims <- fill_limits
  }
  
  # ------------------------------------------------------------
  # Fill scale
  # ------------------------------------------------------------
  
  fill_scale <- if (metric == "difference") {
    
    ggplot2::scale_fill_gradient2(
      low = "blue",
      mid = "grey",
      high = "red",
      limits = lims,
      midpoint = 0,
      name = "Users (crisis - baseline)"
    )
    
  } else if (metric == "crisis") {
    
    ggplot2::scale_fill_gradient(
      trans = "log10",
      low = "blue",
      high = "red",
      limits = lims,
      labels = scales::label_comma(),
      name = "Users"
    )
    
  } else {
    
    # z-score
    ggplot2::scale_fill_distiller(
      palette = "RdBu",
      direction = -1,
      limits = c(-4, 4),
      oob = scales::squish,
      na.value = "grey60",
      name = "z-score (crisis vs. baseline)"
    )
  }
  
  # ------------------------------------------------------------
  # Base population map
  # ------------------------------------------------------------
  
  p1 <- ggplot2::ggplot()
  
  # Basemap
  if (!is.null(osm)) {
    
    p1 <- p1 +
      layer_spatial(osm)
  }
  
  # Population polygons
  p1 <- p1 +
    ggplot2::geom_sf(
      data = first_time_pop,
      ggplot2::aes(
        fill = .data[[fill_col]]
      ),
      color = "white",
      linewidth = 0.1,
      alpha = 1
    )
  # ------------------------------------------------------------
  # Disaster-specific layer
  #
  # Added disaster-specific data (e.g. fire polygons or earthquake epicenters)
  # ------------------------------------------------------------
  
  
  if (!is.null(disaster_type)) {
    
    source(paste0("3_", disaster_type, "_functions.R"))
  }
  
  # ------------------------------------------------------------
  # Highway layer
  #
  # Added AFTER population polygons so highways appear on top.
  # ------------------------------------------------------------
  
  if (
    !is.null(highway_sf) &&
    nrow(highway_sf) > 0
  ) {
    
    # Different visual hierarchy depending on road type
    p1 <- p1 +
      ggplot2::geom_sf(
        data = highway_sf,
        ggplot2::aes(
          linewidth = highway
        ),
        color = "black",
        alpha = 0.85,
        inherit.aes = FALSE
      ) +
      
      ggplot2::scale_linewidth_manual(
        values = c(
          motorway       = 1.2,
          motorway_link  = 0.7,
          trunk          = 1.0,
          trunk_link     = 0.6,
          primary        = 0.8,
          primary_link   = 0.5,
          secondary      = 0.6,
          secondary_link = 0.4,
          tertiary       = 0.35,
          tertiary_link  = 0.25,
          unclassified   = 0.25,
          residential    = 0.20
        ),
        guide = "none"
      )
  }
  
  # ------------------------------------------------------------
  # Fill scale + coordinate system
  # ------------------------------------------------------------
  
  p1 <- p1 +
    fill_scale +
    
    ggplot2::coord_sf(
      crs = sf::st_crs(3857),
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    )
  
  # ------------------------------------------------------------
  # Legends
  # ------------------------------------------------------------
  
  p1 <- p1 +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        direction = "horizontal",
        order = 1,
        title.position = "top"
      ),
      color = ggplot2::guide_legend(
        direction = "horizontal",
        order = 2
      )
    )
  
  # ------------------------------------------------------------
  # City labels and arrows
  # ------------------------------------------------------------
  
  if (!is.null(labels)) {
    
    # `labels` is already in EPSG:3857
    coords <- sf::st_coordinates(labels)
    
    label_xy <- labels |>
      sf::st_drop_geometry() |>
      dplyr::mutate(
        x = coords[, 1],
        y = coords[, 2]
      )
    
    # ----------------------------------------------------------
    # City-specific arrow angles
    #
    # Example:
    #
    # label_angles <- c(
    #   "Los Angeles" = 20,
    #   "San Diego" = -35,
    #   "San Francisco" = 90
    # )
    #
    # Convention:
    #   0   = straight up
    #   90  = right
    #   -90 = left
    #   180 = straight down
    # ----------------------------------------------------------
    
    if (is.null(label_angles)) {
      
      label_angles <- stats::setNames(
        rep(0, nrow(label_xy)),
        label_xy$label
      )
    }
    
    # Match angle to each city
    label_xy$angle <- unname(
      label_angles[label_xy$label]
    )
    
    # Default to straight up
    label_xy$angle[
      is.na(label_xy$angle)
    ] <- 0
    
    # ----------------------------------------------------------
    # Arrow/label distance
    # ----------------------------------------------------------
    
    arrow_length <- 80000
    
    # Degrees -> radians
    angle_rad <- label_xy$angle * pi / 180
    
    # Calculate label position
    label_xy <- label_xy |>
      dplyr::mutate(
        label_x = x + arrow_length * sin(angle_rad),
        label_y = y + arrow_length * cos(angle_rad)
      )
    
    # ----------------------------------------------------------
    # Arrows
    # ----------------------------------------------------------
    
    p1 <- p1 +
      
      ggplot2::geom_segment(
        data = label_xy,
        ggplot2::aes(
          x = label_x,
          y = label_y,
          xend = x,
          yend = y
        ),
        color = "black",
        linewidth = 1.2,
        arrow = grid::arrow(
          length = grid::unit(
            0.35,
            "cm"
          ),
          type = "closed"
        ),
        inherit.aes = FALSE
      ) +
      
      # --------------------------------------------------------
    # City labels
    # --------------------------------------------------------
    
    ggplot2::geom_label(
      data = label_xy,
      ggplot2::aes(
        x = label_x,
        y = label_y,
        label = label
      ),
      color = "black",
      fill = "white",
      size = 4,
      fontface = "bold",
      label.size = 0.3,
      inherit.aes = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # Title
  # ------------------------------------------------------------
  
  if (title) {
    
    if (is.null(plot_title)) {
      
      p1 +
        ggplot2::ggtitle(
          format_time(
            plot_ds,
            plot_hour,
            tzone = tzone
          )
        )
      
    } else {
      
      p1 +
        ggplot2::ggtitle(
          plot_title
        )
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
