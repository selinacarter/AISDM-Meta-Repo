# ==============================================================================
# EVENT CONFIG
# ==============================================================================

event_dir <- c(
  "Situation_Reports",
  "2026_10_Florida_Gulf_Hurricane"
)

event_path <- do.call(
  here::here,
  as.list(event_dir)
)

tzone <- "America/Chicago"
data_tz <- "America/Chicago"
re_run_cleaning <- FALSE

storm_name <- "Isaias"
storm_id <- "AL092026"

# Event windows
event_start_ds <- "2026-10-10"
event_start_hour <- "0000"

event_mid_ds <- "2026-10-10"
event_mid_hour <- "0800"

statewide_path_ds <- "2026-10-10"
statewide_path_hour <- "0800"

event_end_ds <- "2026-10-10"
event_end_hour <- "1600"

# Map extents
impact_lon <- c(-88.4, -85.4)
impact_lat <- c(29.8, 31.2)

context_lon <- c(-89.5, -83.5)
context_lat <- c(28.5, 33.5)

site_box_miles <- 9

cities <- tribble(
  ~place,              ~role,                                 ~lon,      ~lat,     ~label_dx_m, ~label_dy_m,
  "Gulf Shores",       "Coastal Alabama community",           -87.7008,  30.2460,   -25000,       22000,
  "Pensacola",         "Western Florida Panhandle hub",       -87.2194,  30.4073,   -25000,       22000,
  "Destin",            "Near-landfall coastal community",     -86.4571,  30.3891,    15000,       22000,
  "Panama City Beach", "Eastern Panhandle coastal community", -85.8889,  30.2303,    0,      -18000,
  "Tuscaloosa",        "Inland Alabama population hub",       -87.5692,  33.2098,    18000,      -18000
)

best_track_src <- NULL
forecast_src <- NULL
forecast_track_kmz <- NULL
forecast_cone_kmz <- NULL

hurricane_config <- list(
  storm_name = storm_name,
  storm_id = storm_id,
  track_src = best_track_src,
  forecast_src = forecast_src,
  forecast_track_kmz = forecast_track_kmz,
  forecast_cone_kmz = forecast_cone_kmz,
  cities = cities,
  terrain_zoom = 9,
  contour_by = 500,
  roads = "major",
  rivers = "major",
  pop_alpha = 0.82,
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

hurricane_overview_config <- modifyList(
  hurricane_config,
  list(
    cities = NULL,
    terrain_zoom = 7,
    contour_by = 1000,
    roads = "none",
    rivers = "none",
    show_storm_legend = TRUE,
    show_path_legend = TRUE,
    show_context_legend = FALSE
  )
)