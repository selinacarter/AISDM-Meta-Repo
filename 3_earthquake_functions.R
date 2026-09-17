fetch_earthquakes <- function(
    lon_limits,
    lat_limits,
    start_date,
    end_date,
    min_magnitude
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
  
  eq <- sf::st_read(usgs_url, quiet = TRUE)
  
  # Process only if rows are returned
  if (nrow(eq) > 0) {
    eq <- eq |>
      sf::st_transform(3857) |>
      dplyr::mutate(
        datetime = as.POSIXct(
          time / 1000,
          origin = "1970-01-01",
          tz = "UTC"
        ),
        datetime_pst = format(
          datetime,
          tz = "America/Los_Angeles"
        )
      ) |>
      dplyr::arrange(datetime) |>
      dplyr::mutate(eq_order = LETTERS[row_number()])
  }
  return (eq)
}

disaster_limits <- function(
    lon_limits,
    lat_limits,
    start_date,
    end_date,
    min_magnitude = 2.5
) {
  earthquakes <- fetch_earthquakes(lon_limits,
                                   lat_limits,
                                   start_date,
                                   end_date,
                                   min_magnitude)
  
  range(earthquakes$mag, na.rm = TRUE)
}

earthquakes <- fetch_earthquakes(lon_limits,
                                 lat_limits,
                                 start_date,
                                 end_date,
                                 min_magnitude)
if (is.null(disaster_limits)){
  disaster_limits <- range(earthquakes$mag, na.rm = TRUE) 
}

p1 <- p1 +
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
    limits = disaster_limits,
    breaks = pretty(disaster_limits, n = 5),
    oob = scales::squish,
    name = "Magnitude"
  ) + 
  coord_sf(
    crs = st_crs(3857),
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  )