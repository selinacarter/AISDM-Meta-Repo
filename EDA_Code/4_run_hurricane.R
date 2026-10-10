# ==============================================================================
# 4_run_hurricane.R
#
# Reproducible hurricane driver.
#
# For future hurricanes, the main edits should be confined to the CONFIG block:
# event directory, storm ID/name, event times, map extents, cities, and (when
# desired) local archived NHC products.
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

# Declare file location for here::here() relative to your project root
here::i_am("4_run_hurricane.R") 
# (Use "EDA_Code/4_run_hurricane.R" if your .Rproj is in the parent directory)

# ==============================================================================
# CONFIG
# ==============================================================================
event_dir        <- "The Hurricane in Hawaii"
tzone            <- "Pacific/Honolulu"
data_tz          <- "America/Los_Angeles" # Meta's timestamp clock
re_run_cleaning  <- TRUE                  # TRUE on first run to build the .Rds caches

storm_name       <- "Lala"
storm_id         <- "CP012026"

# ==============================================================================
# RUN CLEANING PIPELINE
# ==============================================================================
source("1_data_cleaning.R")
storm_name <- "Lala"
storm_id <- "CP012026"

# ------------------------------------------------------------------------------
# Population-data window
#
# Data are available every 8 hours from 2026-08-15 00:00 through
# 2026-08-29 16:00. The exact midpoint is 2026-08-22 08:00 HST.
# ------------------------------------------------------------------------------

event_start_ds <- "2026-08-15"
event_start_hour <- "0000"

event_mid_ds <- "2026-08-22"
event_mid_hour <- "0800"

# Statewide storm-path context figure requested for early in the event.
statewide_path_ds <- "2026-08-16"
statewide_path_hour <- "0800"

event_end_ds <- "2026-08-29"
event_end_hour <- "1600"

# ------------------------------------------------------------------------------
# Map extents
# ------------------------------------------------------------------------------

big_island_lon <- c(-156.2, -154.6)
big_island_lat <- c(18.8, 20.4)

# Statewide context view: enough room to see the approach/path around Hawaiʻi.
hawaii_lon <- c(-161.0, -153.0)
hawaii_lat <- c(17.5, 23.5)

# Exact city selection size
site_box_miles <- 12

# ------------------------------------------------------------------------------
# Cities of interest on Hawaiʻi Island
# Waikoloa Village replaces Waimea because stronger population movement was observed there.
# ------------------------------------------------------------------------------

cities <- tribble(
  ~place,          ~role,                           ~lon,      ~lat,     ~label_dx_m, ~label_dy_m,
  "Hilo",          "East Hawaiʻi population hub",   -155.0885, 19.7074,   25000,        18000,
  "Kailua-Kona",   "West Hawaiʻi population hub",   -155.9969, 19.6400,   32000,       -18000,
  "Waikoloa Village", "Northwest Hawaiʻi community", -155.7900, 19.9400,   18000,        22000,
  "Pāhoa",         "Puna district community",        -154.9517, 19.4975,   25000,       -18000
)

# ------------------------------------------------------------------------------
# Optional local NHC inputs
#
# Leave NULL to let 3_hurricane_functions.R try the NOAA ATCF current-season
# and historical archive URLs. A local a-deck is still preferable when you want
# a fully reproducible retrospective forecast history with no network dependency.
# ------------------------------------------------------------------------------

best_track_src <- NULL
forecast_src <- NULL

# A single TRACK/CONE KMZ represents one advisory. Keep these NULL for the
# multi-window report unless you intentionally want that same advisory shown.
forecast_track_kmz <- NULL
forecast_cone_kmz <- NULL

# ------------------------------------------------------------------------------
# Detailed Big Island hurricane style
# ------------------------------------------------------------------------------

hurricane_config <- list(
  storm_name = storm_name,
  storm_id = storm_id,
  track_src = best_track_src,
  forecast_src = forecast_src,
  forecast_track_kmz = forecast_track_kmz,
  forecast_cone_kmz = forecast_cone_kmz,
  cities = cities,
  
  # Same DEM/contour approach as the Nepal flood plots.
  terrain_zoom = 9,
  contour_by = 500,
  
  roads = "major",
  rivers = "major",
  
  # Bing population layer is restored second-to-top by 3_hurricane_functions.R.
  pop_alpha = 0.82,
  
  # Detailed Big Island figures keep only the population colorbar.
  show_storm_legend = FALSE,
  show_path_legend = FALSE,
  show_context_legend = FALSE,
  
  source_tz = data_tz,
  
  cache_dir = file.path(
    event_path,
    "map_cache",
    "hurricane"
  )
)

# Statewide midpoint map: simplify contextual layers so the realized/forecast
# hurricane paths are easier to read at the wider scale.
hurricane_overview_config <- modifyList(
  hurricane_config,
  list(
    cities = NULL,
    terrain_zoom = 7,
    contour_by = 1000,
    roads = "none",
    rivers = "none",
    pop_alpha = 0.82,
    show_storm_legend = TRUE,
    show_path_legend = TRUE,
    show_context_legend = FALSE
  )
)

# ==============================================================================
# LOAD DATA + FUNCTIONS
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
# VALIDATE EXPECTED 8-HOUR WINDOWS
# ==============================================================================

expected_windows <- tibble(
  window_time = seq(
    ymd_hm(
      paste(event_start_ds, event_start_hour),
      tz = data_tz
    ),
    ymd_hm(
      paste(event_end_ds, event_end_hour),
      tz = data_tz
    ),
    by = "8 hours"
  )
) |>
  transmute(
    ds = as.Date(window_time),
    hour = format(window_time, "%H%M")
  )

available_windows <- fb_data_bing |>
  distinct(ds, hour) |>
  mutate(
    ds = as.Date(ds),
    hour = as.character(hour)
  ) |>
  arrange(ds, hour)

missing_windows <- anti_join(
  expected_windows,
  available_windows,
  by = c("ds", "hour")
)

if (nrow(missing_windows) > 0) {
  warning(
    nrow(missing_windows),
    " expected 8-hour population windows are missing."
  )
}

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
  lon_limits_use = big_island_lon,
  lat_limits_use = big_island_lat
)

# ==============================================================================
# PANEL HELPERS
# ==============================================================================

hurricane_panel <- function(
    ds,
    hr,
    metric = c("difference", "crisis"),
    lon_limits = big_island_lon,
    lat_limits = big_island_lat,
    zoom = 8,
    config = hurricane_config,
    fill_limits = diff_fill_limits
) {
  metric <- match.arg(metric)
  
  f <- if (metric == "difference") {
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
    
    # Roads/rivers come from the hurricane layer so they share its cache and
    # Nepal-style terrain workflow.
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
    f,
    args
  )
}

# ==============================================================================
# BIG ISLAND FIRST / MID / LATEST MAPS
# ==============================================================================

message("Building Big Island first panel ...")
p_first <- hurricane_panel(
  event_start_ds,
  event_start_hour
)

message("Building Big Island midpoint panel ...")
p_mid <- hurricane_panel(
  event_mid_ds,
  event_mid_hour
)

message("Building Big Island latest panel ...")
p_latest <- hurricane_panel(
  event_end_ds,
  event_end_hour
)

fig_difference <- (
  p_first |
    p_latest
) +
  patchwork::plot_layout(
    guides = "collect"
  ) &
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      title.position = "top",
      order = 1,
      barwidth = grid::unit(7, "cm"),
      barheight = grid::unit(0.4, "cm")
    ),
    colour = ggplot2::guide_legend(
      title.position = "left",
      order = 2,
      nrow = 2,
      keywidth = grid::unit(1.1, "cm")
    )
  ) &
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = ggplot2::element_text(size = 8),
    legend.title = ggplot2::element_text(size = 9),
    plot.margin = ggplot2::margin(5, 12, 5, 12),
    text = ggplot2::element_text(size = 9)
  )

print(fig_difference)

# ==============================================================================
# WHOLE-HAWAIʻI STORM-PATH MAP — 2026-08-16 08:00 HST
#
# Statewide context at 2026-08-16 08:00 HST.
# The black path is realized/best-track history through that time.
# The red dashed path is the official forecast available at/before that time.
# ==============================================================================

message("Building statewide 2026-08-16 08:00 hurricane-path panel ...")

statewide_fill_limits <- shared_limits(
  col = "Difference between baseline and crisis",
  symmetric = TRUE,
  ds1 = statewide_path_ds,
  hour1 = statewide_path_hour,
  ds2 = statewide_path_ds,
  hour2 = statewide_path_hour,
  lon_limits_use = hawaii_lon,
  lat_limits_use = hawaii_lat
)

p_hawaii_path <- hurricane_panel(
  ds = statewide_path_ds,
  hr = statewide_path_hour,
  metric = "difference",
  lon_limits = hawaii_lon,
  lat_limits = hawaii_lat,
  zoom = 6,
  config = hurricane_overview_config,
  fill_limits = statewide_fill_limits
)

fig_hawaii_path <- p_hawaii_path +
  labs(
    subtitle = paste0(
      "Statewide context: ",
      format_time(
        statewide_path_ds,
        statewide_path_hour,
        tzone = tzone,
        source_tz = data_tz
      ),
      " | solid black = realized track; red dashed = official forecast"
    )
  ) +
  theme(
    legend.box = "vertical"
  )

print(fig_hawaii_path)

# ==============================================================================
# EXACT 12 x 12 MILE CITY BOXES + TIME SERIES
# ==============================================================================

city_boxes <- make_site_boxes(
  cities,
  width_miles = site_box_miles
)

city_ts <- site_population_timeseries(
  fb_data = fb_data_bing,
  sites = cities,
  width_miles = site_box_miles,
  source_tz = data_tz,
  output_tz = tzone
) |>
  filter(
    window_time >= ymd_hm(
      paste(event_start_ds, event_start_hour),
      tz = tzone
    ),
    window_time <= ymd_hm(
      paste(event_end_ds, event_end_hour),
      tz = tzone
    )
  )

fig_city_ts <- ggplot(
  city_ts,
  aes(
    window_time,
    net_users
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
    x = "Window start (HST)",
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

print(fig_city_ts)

# ==============================================================================
# SAVE DRAFT OUTPUTS
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

ggplot2::ggsave(
  filename = file.path(
    output_dir,
    "big_island_hurricane_difference.png"
  ),
  plot = fig_difference,
  width = 14,
  height = 8.5,
  dpi = 200
)

ggplot2::ggsave(
  filename = file.path(
    output_dir,
    "hawaii_2026-08-16_0800_hurricane_path.png"
  ),
  plot = fig_hawaii_path,
  width = 11,
  height = 8,
  dpi = 200
)

ggplot2::ggsave(
  filename = file.path(
    output_dir,
    "big_island_city_timeseries.png"
  ),
  plot = fig_city_ts,
  width = 10,
  height = 7,
  dpi = 200
)

message(
  "Saved hurricane figures to: ",
  normalizePath(output_dir)
)
