# ==============================================================================
# 4_run_hurricane.R
#
# Reproducible hurricane analysis driver.
#
# Event-specific settings live in:
#   0_event_config.R
#
# This script:
#   1. Loads event configuration
#   2. Loads cleaning + plotting functions
#   3. Validates expected population windows
#   4. Builds detailed population maps
#   5. Builds hurricane overview map
#   6. Builds city-level population time series
#   7. Saves final figures
# ==============================================================================


# ==============================================================================
# PACKAGES
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(lubridate)
  library(patchwork)
  library(here)
  library(terra)
  library(elevatr)
  library(osmdata)
})


# ==============================================================================
# PROJECT LOCATION
# ==============================================================================

here::i_am(
  "Situation_Reports/2026_10_Florida_Gulf_Hurricane/4_run_hurricane.R"
)


# ==============================================================================
# LOAD EVENT CONFIG
# ==============================================================================

source(
  here::here(
    "Situation_Reports",
    "2026_10_Florida_Gulf_Hurricane",
    "0_event_config.R"
  )
)


# ==============================================================================
# LOAD DATA + REUSABLE FUNCTIONS
# ==============================================================================

source(
  here::here(
    "EDA_Code",
    "1_data_cleaning.R"
  )
)

source(
  here::here(
    "EDA_Code",
    "2_plot_functions.R"
  )
)


# ==============================================================================
# VALIDATE EXPECTED POPULATION WINDOWS
# ==============================================================================

expected_windows <- tibble(
  window_time = seq(
    ymd_hm(
      paste(
        event_start_ds,
        event_start_hour
      ),
      tz = data_tz
    ),
    ymd_hm(
      paste(
        event_end_ds,
        event_end_hour
      ),
      tz = data_tz
    ),
    by = "8 hours"
  )
) |>
  transmute(
    ds = as.Date(window_time),
    hour = format(
      window_time,
      "%H%M"
    )
  )


available_windows <- fb_data_bing |>
  distinct(
    ds,
    hour
  ) |>
  mutate(
    ds = as.Date(ds),
    hour = as.character(hour)
  ) |>
  arrange(
    ds,
    hour
  )


missing_windows <- anti_join(
  expected_windows,
  available_windows,
  by = c(
    "ds",
    "hour"
  )
)


if (nrow(missing_windows) > 0) {
  warning(
    nrow(missing_windows),
    " expected 8-hour population windows are missing."
  )
}


# ==============================================================================
# EVENT TIME BOUNDS
# ==============================================================================

event_start_time <- ymd_hm(
  paste(
    event_start_ds,
    event_start_hour
  ),
  tz = data_tz
) |>
  with_tz(
    tzone
  )


event_end_time <- ymd_hm(
  paste(
    event_end_ds,
    event_end_hour
  ),
  tz = data_tz
) |>
  with_tz(
    tzone
  )


event_tz_abbr <- format(
  event_start_time,
  "%Z"
)


# ==============================================================================
# SHARED POPULATION-DIFFERENCE SCALE
# ==============================================================================

diff_fill_limits <- shared_limits(
  col = "Difference between baseline and crisis",
  symmetric = TRUE,
  
  ds1 = event_start_ds,
  hour1 = event_start_hour,
  
  ds2 = event_end_ds,
  hour2 = event_end_hour,
  
  lon_limits_use = impact_lon,
  lat_limits_use = impact_lat
)


# ==============================================================================
# REUSABLE HURRICANE PANEL
# ==============================================================================

hurricane_panel <- function(
    ds,
    hr,
    metric = c(
      "difference",
      "crisis"
    ),
    lon_limits = impact_lon,
    lat_limits = impact_lat,
    zoom = 8,
    config = hurricane_config,
    fill_limits = diff_fill_limits
) {
  
  metric <- match.arg(
    metric
  )
  
  
  plot_function <- if (
    metric == "difference"
  ) {
    population_plot_n_difference
  } else {
    population_plot_n_crisis
  }
  
  
  args <- list(
    plot_ds = ds,
    plot_hour = hr,
    tzone = tzone,
    
    lon_limits = lon_limits,
    lat_limits = lat_limits,
    
    zoom = zoom,
    
    # Roads are supplied by the hurricane layer.
    highway_detail = "none",
    
    disaster_type = "hurricane",
    disaster_config = config,
    
    title = TRUE
  )
  
  
  if (
    metric == "difference" &&
    !is.null(fill_limits)
  ) {
    
    args$fill_limits <- fill_limits
    
  }
  
  
  do.call(
    plot_function,
    args
  )
}


# ==============================================================================
# FIGURE 2:
# POPULATION DIFFERENCE THROUGH THE EVENT
# ==============================================================================

message(
  "Building first population panel..."
)

p_first <- hurricane_panel(
  event_start_ds,
  event_start_hour
)


message(
  "Building midpoint population panel..."
)

p_mid <- hurricane_panel(
  event_mid_ds,
  event_mid_hour
)


message(
  "Building final population panel..."
)

p_latest <- hurricane_panel(
  event_end_ds,
  event_end_hour
)


fig_difference <- (
  p_first |
    p_mid |
    p_latest
) +
  patchwork::plot_layout(
    guides = "collect"
  ) &
  
  # Detailed figure should ONLY show the population colorbar.
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      title.position = "top",
      order = 1,
      barwidth = grid::unit(
        7,
        "cm"
      ),
      barheight = grid::unit(
        0.4,
        "cm"
      )
    ),
    colour = "none",
    linetype = "none"
  ) &
  
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "vertical",
    
    legend.text = ggplot2::element_text(
      size = 8
    ),
    
    legend.title = ggplot2::element_text(
      size = 9
    ),
    
    plot.margin = ggplot2::margin(
      5,
      12,
      5,
      12
    ),
    
    text = ggplot2::element_text(
      size = 9
    )
  )


print(
  fig_difference
)


# ==============================================================================
# FIGURE 1:
# BROADER HURRICANE PATH CONTEXT
# ==============================================================================

message(
  "Building hurricane overview panel..."
)


context_fill_limits <- shared_limits(
  col = "Difference between baseline and crisis",
  symmetric = TRUE,
  
  ds1 = statewide_path_ds,
  hour1 = statewide_path_hour,
  
  ds2 = statewide_path_ds,
  hour2 = statewide_path_hour,
  
  lon_limits_use = context_lon,
  lat_limits_use = context_lat
)


p_hurricane_path <- hurricane_panel(
  ds = statewide_path_ds,
  hr = statewide_path_hour,
  
  metric = "difference",
  
  lon_limits = context_lon,
  lat_limits = context_lat,
  
  zoom = 6,
  
  config = hurricane_overview_config,
  
  fill_limits = context_fill_limits
)


fig_hurricane_path <- p_hurricane_path +
  
  labs(
    subtitle = paste0(
      "Regional context: ",
      format_time(
        statewide_path_ds,
        statewide_path_hour,
        tzone = tzone,
        source_tz = data_tz
      ),
      " | solid black = observed track; red dashed = official forecast"
    )
  ) +
  
  theme(
    legend.box = "vertical",
    legend.spacing.y = grid::unit(
      0.12,
      "cm"
    )
  )


print(
  fig_hurricane_path
)


# ==============================================================================
# CITY SELECTION BOXES
# ==============================================================================

city_boxes <- make_site_boxes(
  cities,
  width_miles = site_box_miles
)


# ==============================================================================
# CITY POPULATION TIME SERIES
# ==============================================================================

city_ts <- site_population_timeseries(
  fb_data = fb_data_bing,
  sites = cities,
  width_miles = site_box_miles,
  
  source_tz = data_tz,
  output_tz = tzone
) |>
  
  filter(
    window_time >= event_start_time,
    window_time <= event_end_time
  )


# ==============================================================================
# FIGURE 3:
# CITY POPULATION CHANGE
# ==============================================================================

fig_city_ts <- ggplot(
  city_ts,
  aes(
    x = window_time,
    y = net_users
  )
) +
  
  geom_hline(
    yintercept = 0,
    colour = "grey60"
  ) +
  
  geom_line(
    colour = "darkgreen",
    linewidth = 0.55
  ) +
  
  geom_point(
    colour = "darkgreen",
    size = 0.9
  ) +
  
  facet_wrap(
    ~ place,
    ncol = 2,
    scales = "free_y"
  ) +
  
  labs(
    x = paste0(
      "Window start (",
      event_tz_abbr,
      ")"
    ),
    
    y = "Net users vs. baseline",
    
    title = paste0(
      "Population change in ",
      site_box_miles,
      " × ",
      site_box_miles,
      " mile city regions"
    )
  ) +
  
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  
  theme_minimal(
    base_size = 9
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


print(
  fig_city_ts
)


# ==============================================================================
# CITY SUMMARY FOR FIRST WINDOW
# ==============================================================================

first_city <- city_ts |>
  filter(
    as.character(ds) == event_start_ds,
    hour == event_start_hour
  )


places <- cities |>
  left_join(
    first_city |>
      select(
        place,
        net_users,
        crisis_users,
        tiles_with_data
      ),
    by = "place"
  )


# ==============================================================================
# SAVE OUTPUTS
# ==============================================================================

output_dir <- file.path(
  event_path,
  "outputs"
)


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Clean storm name for filenames
storm_slug <- storm_name |>
  tolower() |>
  gsub(
    "[^a-z0-9]+",
    "_",
    x = _
  ) |>
  gsub(
    "^_|_$",
    "",
    x = _
  )


# ------------------------------------------------------------------------------
# Figure 1: hurricane overview
# ------------------------------------------------------------------------------

ggplot2::ggsave(
  filename = file.path(
    output_dir,
    paste0(
      storm_slug,
      "_hurricane_path.png"
    )
  ),
  
  plot = fig_hurricane_path,
  
  width = 11,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------------------------
# Figure 2: population sequence
# ------------------------------------------------------------------------------

ggplot2::ggsave(
  filename = file.path(
    output_dir,
    paste0(
      storm_slug,
      "_population_difference.png"
    )
  ),
  
  plot = fig_difference,
  
  width = 14,
  height = 8.5,
  dpi = 300
)


# ------------------------------------------------------------------------------
# Figure 3: city population time series
# ------------------------------------------------------------------------------

ggplot2::ggsave(
  filename = file.path(
    output_dir,
    paste0(
      storm_slug,
      "_city_timeseries.png"
    )
  ),
  
  plot = fig_city_ts,
  
  width = 10,
  height = 7,
  dpi = 300
)


# ==============================================================================
# COMPLETE
# ==============================================================================

message(
  "Saved hurricane figures to: ",
  normalizePath(
    output_dir
  )
)