make_percent_change_map <- function(file_name,
                                    map_date = NULL,
                                    date_col = "ds",
                                    value_col = "percent_change",
                                    output_plot_name = NULL,
                                    title_prefix = "Population Density Change on",
                                    bbox_buffer = 0.25) {
  
  # Read file from root Outputs folder
  df <- read_csv(
    here("Outputs", file_name),
    show_col_types = FALSE
  )
  
  # Make sure the date column is treated as a Date
  df <- df |>
    mutate("{date_col}" := as.Date(.data[[date_col]]))
  
  # If no date is supplied, use the latest date by default
  if (is.null(map_date)) {
    map_date <- max(df[[date_col]], na.rm = TRUE)
  } else {
    map_date <- as.Date(map_date)
  }
  
  # Summarize to county level for the selected date
  selected_county <- df |>
    filter(.data[[date_col]] == map_date) |>
    group_by(county_geoid, county_name, county_state) |>
    summarise(
      mean_percent_change = mean(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(selected_county) == 0) {
    stop("No data found for selected date: ", map_date)
  }
  
  # Join county summaries to county polygons
  map_data <- counties_sf |>
    inner_join(selected_county, by = "county_geoid") |>
    st_transform(4326)
  
  # Build state boundary layer from states represented in the data
  state_lines <- tigris::states(cb = TRUE, year = 2023) |>
    st_transform(4326) |>
    filter(NAME %in% unique(map_data$county_state))
  
  # Build bounding box around relevant counties
  bbox <- st_bbox(state_lines)
  
  xlim <- c(
    as.numeric(bbox["xmin"]) - bbox_buffer,
    as.numeric(bbox["xmax"]) + bbox_buffer
  )
  
  ylim <- c(
    as.numeric(bbox["ymin"]) - bbox_buffer,
    as.numeric(bbox["ymax"]) + bbox_buffer
  )
  
  # Build map using sf only
  p <- ggplot() +
    geom_sf(
      data = map_data,
      aes(fill = mean_percent_change),
      color = "black",
      linewidth = 0.25
    ) +
    geom_sf(
      data = state_lines,
      fill = NA,
      color = "black",
      linewidth = 0.7
    ) +
    scale_fill_gradient2(
      labels = label_number(suffix = "%", accuracy = 1)
    ) +
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    labs(
      title = paste(title_prefix, map_date),
      fill = "% Change vs Baseline"
    ) +
    theme_minimal()
  
  # Save plot if requested
  if (!is.null(output_plot_name)) {
    ggsave(
      filename = here("Outputs", output_plot_name),
      plot = p,
      width = 10,
      height = 7
    )
  }
  
  return(p)
}



# Not working because of earlier runtime error do not delete
make_percent_change_map(
  file_name = "movement_between_places_aggregated.csv"
)

make_percent_change_map(
  file_name = "tk_facebook_pop_aggregated_bing.csv",
  bbox_buffer = 1
)

make_percent_change_map(
  file_name = "tk_movement_between_places_aggregated_admin.csv"
)

# Not working because of earlier runtime error do not delete
make_percent_change_map(
  file_name = "tk_movement_between_places_aggregated_bing.csv"
)

make_demographics_table(
  file_name = "tk_facebook_pop_aggregated_bing.csv",
  map_date = "2026-02-01",
  n_counties = 10
)


make_percent_change_map <- function(file_name,
                                    map_date = NULL,
                                    date_col = "ds",
                                    value_col = "percent_change",
                                    output_plot_name = NULL,
                                    title_prefix = "Population Density Change on",
                                    bbox_buffer = 0.25,
                                    maptype = "stamen_toner_lite",
                                    zoom = 7) {
  
  # Read file from root Outputs folder
  df <- readr::read_csv(
    here::here("Outputs", file_name),
    show_col_types = FALSE
  )
  
  # Make sure the date column is treated as a Date
  df <- df |>
    dplyr::mutate("{date_col}" := as.Date(.data[[date_col]]))
  
  # If no date is supplied, use the latest date by default
  if (is.null(map_date)) {
    map_date <- max(df[[date_col]], na.rm = TRUE)
  } else {
    map_date <- as.Date(map_date)
  }
  
  # Summarize to county level for the selected date
  selected_county <- df |>
    dplyr::filter(.data[[date_col]] == map_date) |>
    dplyr::group_by(county_geoid, county_name, county_state) |>
    dplyr::summarise(
      mean_percent_change = mean(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(selected_county) == 0) {
    stop("No data found for selected date: ", map_date)
  }
  
  # Join selected data to county boundaries
  map_counties_sf <- counties_sf |>
    dplyr::inner_join(selected_county, by = "county_geoid")
  
  # Keep only the state boundaries represented in the data
  map_states_sf <- states_sf |>
    dplyr::filter(state_name %in% unique(map_counties_sf$county_state))
  
  # Use state boundaries to set the full map extent
  
  bbox <- sf::st_bbox(map_states_sf)
  
  bbox_map <- c(
    left   = as.numeric(unname(bbox["xmin"])) - bbox_buffer,
    bottom = as.numeric(unname(bbox["ymin"])) - bbox_buffer,
    right  = as.numeric(unname(bbox["xmax"])) + bbox_buffer,
    top    = as.numeric(unname(bbox["ymax"])) + bbox_buffer
  )
  
  names(bbox_map) <- c("left", "bottom", "right", "top")
  
  base_map <- ggmap::get_stadiamap(
    bbox = bbox_map,
    zoom = zoom,
    maptype = maptype
  )
  
  # Download ggmap/Stadia basemap
  base_map <- ggmap::get_stadiamap(
    bbox = bbox_map,
    zoom = zoom,
    maptype = maptype
  )
  
  sf_to_polygon_df <- function(sf_object, id_cols) {
    coords <- st_coordinates(sf_object)
    
    sf_object |>
      st_drop_geometry() |>
      mutate(.row_id = row_number()) |>
      right_join(
        as.data.frame(coords) |>
          mutate(.row_id = L1),
        by = ".row_id"
      ) |>
      mutate(
        group_id = interaction(.row_id, L1, L2, drop = TRUE)
      )
  }
  
  # Convert county and state boundaries to regular data frames
  county_poly <- sf_to_polygon_df(map_counties_sf, id_cols = "county_geoid")
  
  state_poly <- sf_to_polygon_df(map_states_sf, id_cols = "state_name")
  
  # Build map using ggmap + regular polygon layers
  p <- ggmap::ggmap(base_map) +
    ggplot2::geom_polygon(
      data = county_poly,
      aes(
        x = X,
        y = Y,
        group = group_id,
        fill = mean_percent_change
      ),
      color = "white",
      linewidth = 0.25,
      alpha = 0.75,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_path(
      data = state_poly,
      aes(
        x = X,
        y = Y,
        group = group_id
      ),
      color = "black",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_gradient2(
      labels = scales::label_number(suffix = "%", accuracy = 1)
    ) +
    ggplot2::labs(
      title = paste(title_prefix, map_date),
      fill = "% Change vs Baseline"
    ) +
    ggplot2::theme_minimal()
  
  # Save plot if requested
  if (!is.null(output_plot_name)) {
    ggplot2::ggsave(
      filename = here::here("Outputs", output_plot_name),
      plot = p,
      width = 10,
      height = 7
    )
  }
  
  return(p)
}

make_percent_change_map(
  file_name = "tk_facebook_pop_aggregated_bing.csv",
  zoom = 7,
  bbox_buffer = 0.5
)

#-----------Investigation of mean v median---------------
# Might not even use but want thoughts on which to use, not many outliers present but there will be a lot of 0s pulling mean down
# Facetted plotting function using median percent change
plot_county_time_series_faceted_median <- function(time_data, plot_title) {
  ggplot(time_data, aes(x = ds, y = median_percent_change, group = county_name)) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1) +
    facet_wrap(~ county_name, scales = "free_y") +
    scale_y_continuous(
      labels = scales::label_number(suffix = "%", accuracy = 1)
    ) +
    labs(
      title = plot_title,
      subtitle = "County-level median percent change from baseline",
      x = NULL,
      y = "Median % Change vs Baseline"
    ) +
    theme_minimal()
}

high_time <- make_county_time_data(high_counties)

plot_county_time_series_faceted_median(
  high_time,
  "% Population Density Change Over Time:\nHighest-Change Counties"
)


average_time <- make_county_time_data(average_counties)

plot_county_time_series_faceted_median(
  average_time,
  "% Population Density Change Over Time:\nAverage-Change Counties"
)


low_time <- make_county_time_data(low_counties)

plot_county_time_series_faceted_median(
  low_time,
  "% Population Density Change Over Time:\nLowest-Change Counties"
)