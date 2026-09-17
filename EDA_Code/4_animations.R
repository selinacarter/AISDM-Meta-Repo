population_plot_animate <- function(
    metric = c("crisis", "difference"),
    tzone,
    ds_values = NULL,
    hour_values = NULL,
    ...,
    fps = 10,
    duration = NULL,
    filename = NULL,
    renderer = gganimate::gifski_renderer()
) {
  
  metric <- match.arg(metric)
  
  plot_fun <- switch(
    metric,
    crisis = population_plot_n_crisis,
    difference = population_plot_n_difference
  )
  
  time_windows <- tiles_3857 |>
    dplyr::distinct(ds, hour) |>
    dplyr::arrange(ds, hour)
  
  if (!is.null(ds_values)) {
    time_windows <- time_windows |>
      dplyr::filter(ds %in% ds_values)
  }
  
  if (!is.null(hour_values)) {
    time_windows <- time_windows |>
      dplyr::filter(hour %in% hour_values)
  }
  
  if (nrow(time_windows) == 0) {
    stop("No matching ds/hour combinations found in `tiles_3857`.")
  }
  
  time_windows <- time_windows |>
    dplyr::mutate(
      frame = dplyr::row_number(),
      frame_label = paste(ds, hour)
    )
  
  animation_data <- tiles_3857 |>
    dplyr::inner_join(
      time_windows,
      by = c("ds", "hour")
    )
  
  first_ds <- time_windows$ds[1]
  first_hour <- time_windows$hour[1]
  
  p <- plot_fun(
    plot_ds = first_ds,
    plot_hour = first_hour,
    tzone = tzone,
    ...
  )
  
  population_layer <- which(
    vapply(
      p$layers,
      function(layer) {
        dat <- layer$data
        
        !is.null(dat) &&
          is.data.frame(dat) &&
          "ds" %in% names(dat) &&
          "hour" %in% names(dat)
      },
      logical(1)
    )
  )
  
  if (length(population_layer) == 0) {
    stop("Could not identify the population layer in the plot.")
  }
  
  population_layer <- population_layer[1]
  
  p$layers[[population_layer]]$data <- animation_data
  
  p <- p +
    gganimate::transition_manual(frame) +
    ggplot2::labs(
      subtitle = "{frame_label}"
    )
  
  animation <- gganimate::animate(
    p,
    fps = fps,
    duration = duration,
    renderer = renderer
  )
  
  # Save GIF if filename is supplied
  if (!is.null(filename)) {
    gganimate::anim_save(
      filename = filename,
      animation = animation
    )
  }
  
  invisible(animation)
}
