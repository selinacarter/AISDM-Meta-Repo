library(tidyverse)
library(sf)
library(lubridate)
library(gganimate)
library(ggforce)
library(ggspatial)
library(maptiles)
library(quadkeyr)
library(prettymapr)
library(patchwork)
library(here)
#setwd("C:/Users/selin/Dropbox/CMU/Meta_AI_good/fire")
re_run_cleaning = FALSE


######------- Data Cleaning Functions --------#######
here::i_am("1_data_cleaning_Selina.R")

# Convert possible "\N" values to proper numeric NA values
clean_numeric <- function(x) {
  suppressWarnings(as.numeric(na_if(as.character(x), "\\N")))
}


###### ----- Aggregate CSVs ------ ######

aggregate_csvs <- function(path_parts,
                           output_name = "aggregated.csv",
                           dedupe = TRUE,
                           lat_col = NULL,
                           lon_col = NULL,
                           prefix = "",
                           calculate_county_change = FALSE,
                           county_group_cols = c("county_geoid", "county_name", "county_state", "ds")) {
  
  data_dir <- do.call(here::here, as.list(path_parts))
  
  #cat("Reading from:", data_dir, "\n")
  
  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir)
  }
  
  csv_files <- list.files(
    path = data_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  if (length(csv_files) == 0) {
    stop("No CSV files found in: ", data_dir)
  }
  
  #cat("Found", length(csv_files), "files\n")
  
  combined_data <- lapply(csv_files, function(f) {
    read_csv(f, show_col_types = FALSE) |>
      mutate(source_file = basename(f))
  }) |>
    bind_rows()
  
  if (dedupe) {
    combined_data <- combined_data |> distinct()
  }
  
  
  # Extract date and time from file names
  combined_data <- combined_data |>
    mutate(
      ds = as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", source_file)),
      hour = sub(".*_(\\d{4}-\\d{2}-\\d{2})_(\\d{4}).*", "\\2", source_file)
    )
  
  return(combined_data)
}


analyze_dataset <- function(df) {
  list(
    missing = df |>
      summarise(across(everything(), ~mean(is.na(.)))),
    
    numeric_summary = df |>
      summarise(across(where(is.numeric),
                       list(mean = mean, sd = sd),
                       na.rm = TRUE)),
    
    n_rows = nrow(df),
    n_cols = ncol(df)
  )
}


###### ------- Movement Between Places w/o Aggregation ----#####


if (re_run_cleaning) {
  mp_data_bing <- aggregate_csvs(
    path_parts = c("Movement Between Places During Crisis"),
    output_name = "tk_movement_between_places_bing.csv"
  )
  
  saveRDS(mp_data_bing, "mp_data_bing.Rds")
  
  fb_data_bing <- aggregate_csvs(
    path_parts = c("Facebook Population During Crisis - Bing Tiles"),
    output_name = "facebook_pop_bing.csv",
    lat_col = "latitude",
    lon_col = "longitude",
    prefix = ""
  ) #Selina: removed drop_na(n_crisis), since NA might be useful info
  # |>  drop_na(n_crisis)
  
  saveRDS(fb_data_bing, "fb_data_bing.Rds")
  
  moved <- mp_data_bing |>
    #Selina: Changed the filter to be "OR" (not "AND"): we want either start_long!=end_long OR start_lat!=end_lat. If you do "AND", you are incorrectly removing direct East/West and North/South movements.
    filter(start_longitude != end_longitude |  start_latitude != end_latitude) |>
    #Selina: I commented out the #drop_na. We might find the explicit NA useful, as it means there are < 11 people.
    #drop_na(n_crisis) |>
    rename(`# Users During Crisis` = n_crisis) |>
    rename(`Difference between baseline and crisis` = n_difference)
  
  
  pts <- rbind(
    data.frame(lon = min(fb_data_bing$longitude), lat = min(fb_data_bing$latitude)),
    data.frame(lon = max(fb_data_bing$longitude), lat = max(fb_data_bing$latitude))
  )
  
  saveRDS(moved, "moved.Rds")
  
} else{
  mp_data_bing=readRDS("mp_data_bing.Rds")
  fb_data_bing=readRDS("fb_data_bing.Rds")
  moved=readRDS("moved.Rds")
  pts <- rbind(
    data.frame(lon = min(fb_data_bing$longitude), lat = min(fb_data_bing$latitude)),
    data.frame(lon = max(fb_data_bing$longitude), lat = max(fb_data_bing$latitude))
  )
  
  
}


###### ------- Facebook Population w/o Aggregation ----#####




pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

# Download OpenStreetMap tiles
osm <- get_tiles(
  pts_sf,
  provider = "OpenStreetMap",
  crop = TRUE,
  zoom = 6
)
if (re_run_cleaning) {
  tiles <- quadkey_df_to_polygon(fb_data_bing)
} else {
  tiles=readRDS("tiles.Rds")
}

tiles_3857 <- st_transform(tiles, 3857) |>
  rename(`# Users During Crisis` = n_crisis) |>
  rename(`Difference between baseline and crisis` = n_difference)

format_time <- function(ds, hour){
  df <- data.frame(ds, hour)
  
  df$datetime_cet <- ymd_hm(
    paste(df$ds, df$hour),
    tz = "America/Los_Angeles"
  )
  
  # Format
  df$formatted <- format(
    df$datetime_cet,
    "%B %d, %Y %I%p %Z"
  )
  
  # Remove leading zero from hour and lowercase AM/PM
  df$formatted <- gsub(" 0", " ", df$formatted)
  df$formatted <- gsub("AM", "am", df$formatted)
  df$formatted <- gsub("PM", "pm", df$formatted)
  
  df$formatted
}

population_plot <- function(
    plot_ds,
    plot_hour,
    title = TRUE,
    plot_title = NULL,
    lon_limits = NULL,   # c(min_lon, max_lon)
    lat_limits = NULL    # c(min_lat, max_lat)
) {
  
  lims <- c(
    min(tiles_3857$`# Users`, na.rm = TRUE),
    max(tiles_3857$`# Users`, na.rm = TRUE)
  )
  
  first_time_pop <- tiles_3857 |>
    filter(ds == plot_ds, hour == plot_hour)
  
  # Default: no zoom
  xlim <- NULL
  ylim <- NULL
  
  # Convert lon/lat limits to EPSG:3857
  if (!is.null(lon_limits) && !is.null(lat_limits)) {
    
    bbox_ll <- st_as_sfc(
      st_bbox(
        c(
          xmin = lon_limits[1],
          xmax = lon_limits[2],
          ymin = lat_limits[1],
          ymax = lat_limits[2]
        ),
        crs = st_crs(4326)
      )
    )
    
    bbox_3857 <- st_transform(bbox_ll, 3857) |>
      st_bbox()
    
    xlim <- c(bbox_3857["xmin"], bbox_3857["xmax"])
    ylim <- c(bbox_3857["ymin"], bbox_3857["ymax"])
  }
  
  p1 <- ggplot() +
    layer_spatial(osm) +
    geom_sf(
      data = first_time_pop,
      aes(fill = `# Users`),
      color = "white",
      linewidth = 0.1,
      alpha = 1
    ) +
    scale_fill_gradient(
      trans = "log10",
      low = "blue",
      high = "red",
      limits = lims,
      labels = scales::label_comma()
    ) +
    coord_sf(
      crs = st_crs(3857),
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    geom_sf(
      data = fires,
      fill = "red",
      alpha = 0.35,
      color = "darkred",
      linewidth = 1
    ) +
    
    # Spokane marker
    geom_sf(
      data = spokane,
      color = "blue",
      size = 3
    ) +
    
    
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
  
  if (title) {
    if (is.null(plot_title)) {
      p1 + ggtitle(format_time(plot_ds, plot_hour))
    } else {
      p1 + ggtitle(plot_title)
    }
  } else {
    p1
  }
}


#Selina: add all this
library(rosm)


# -------------------------
# Spokane area fires
# -------------------------

spokane <- st_as_sf(
  data.frame(
    lon = -117.4260,
    lat = 47.6588
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

# 100 km buffer around Spokane
extent <- st_buffer(
  st_transform(spokane, 3857),
  100000
) |>
  st_transform(4326)


# -------------------------
# Download wildfire polygons
# -------------------------

bbox <- st_bbox(extent)

url <- paste0(
  "https://services3.arcgis.com/T4QMspbfLg3qTGWY/ArcGIS/rest/services/",
  "WFIGS_Interagency_Perimeters_Current/FeatureServer/0/query?",
  "where=1%3D1",
  "&geometry=",
  bbox["xmin"], ",",
  bbox["ymin"], ",",
  bbox["xmax"], ",",
  bbox["ymax"],
  "&geometryType=esriGeometryEnvelope",
  "&inSR=4326",
  "&spatialRel=esriSpatialRelIntersects",
  "&outFields=*",
  "&returnGeometry=true",
  "&f=geojson"
)

fires <- st_read(url, quiet = TRUE)

population_plot_n_crisis = function(
    plot_ds,
    plot_hour,
    title = TRUE,
    plot_title = NULL,
    lon_limits = NULL,   # c(min_lon, max_lon)
    lat_limits = NULL    # c(min_lat, max_lat)
) {
  
  lims <- c(
    min(tiles_3857$`# Users During Crisis`, na.rm = TRUE),
    max(tiles_3857$`# Users During Crisis`, na.rm = TRUE)
  )
  
  first_time_pop <- tiles_3857 |>
    filter(ds == plot_ds, hour == plot_hour)
  
  if (!is.null(lon_limits)){
    first_time_pop = first_time_pop |>
      filter(lon_limits[1] <= longitude,  longitude <= lon_limits[2]) |>
      filter(lat_limits[1] <= latitude,  latitude <= lat_limits[2])
    
    lims <- c(
      min(first_time_pop$`# Users During Crisis`, na.rm = TRUE),
      max(first_time_pop$`# Users During Crisis`, na.rm = TRUE)
    )
    
    pts <- rbind(
      data.frame(lon = lon_limits[1], lat = lat_limits[1]),
      data.frame(lon = lon_limits[2], lat = lat_limits[2])
    )
    pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
    
    # Download OpenStreetMap tiles
    osm <- get_tiles(
      pts_sf,
      provider = "CartoDB.Voyager",
      crop = TRUE,
      zoom = 6
    )
    
    
    
    
  }
  
  
  # Default: no zoom
  xlim <- NULL
  ylim <- NULL
  
  # Convert lon/lat limits to EPSG:3857
  if (!is.null(lon_limits) && !is.null(lat_limits)) {
    
    bbox_ll <- st_as_sfc(
      st_bbox(
        c(
          xmin = lon_limits[1],
          xmax = lon_limits[2],
          ymin = lat_limits[1],
          ymax = lat_limits[2]
        ),
        crs = st_crs(4326)
      )
    )
    
    bbox_3857 <- st_transform(bbox_ll, 3857) |>
      st_bbox()
    
    xlim <- c(bbox_3857["xmin"], bbox_3857["xmax"])
    ylim <- c(bbox_3857["ymin"], bbox_3857["ymax"])
  }
  
  p1 <- ggplot() +
    layer_spatial(osm) +
    geom_sf(
      data = first_time_pop,
      aes(fill = `# Users During Crisis`),
      color = "white",
      linewidth = 0.1,
      alpha = 1
    ) +
    scale_fill_gradient(
      trans = "log10",
      low = "blue",
      high = "red",
      limits = lims,
      labels = scales::label_comma()
    ) +
    coord_sf(
      crs = st_crs(3857),
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    geom_sf(
      data = fires,
      aes(color = "Fire Perimeter"),
      fill = NA,
      linewidth = 0.5
    ) +
    scale_color_manual(
      values = c("Fire Perimeter" = "darkred"),
      name = NULL
    ) +
    # Spokane marker
    #geom_sf(
    #  data = spokane,
    #  color = "blue",
    #  size = 3
    #) +
    theme_minimal()+
    theme(
      legend.position = "bottom",
      legend.box = "vertical",      # stack legends vertically
      legend.direction = "horizontal", # each legend is horizontal
      legend.key.width = unit(2, "cm")
    ) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        order = 1
      ),
      color = guide_legend(
        direction = "horizontal",
        order = 2
      )
    )
  
  if (title) {
    if (is.null(plot_title)) {
      p1 + ggtitle(format_time(plot_ds, plot_hour))
    } else {
      p1 + ggtitle(plot_title)
    }
  } else {
    p1
  }
}

population_plot_n_difference = function(
    plot_ds,
    plot_hour,
    title = TRUE,
    plot_title = NULL,
    lon_limits = NULL,   # c(min_lon, max_lon)
    lat_limits = NULL    # c(min_lat, max_lat)
) {
  
  lims <- c(
    min(tiles_3857$`Difference between baseline and crisis`, na.rm = TRUE),
    max(tiles_3857$`Difference between baseline and crisis`, na.rm = TRUE)
  )
  
  first_time_pop <- tiles_3857 |>
    filter(ds == plot_ds, hour == plot_hour)
  
  if (!is.null(lon_limits)){
    first_time_pop = first_time_pop |>
      filter(lon_limits[1] <= longitude,  longitude <= lon_limits[2]) |>
      filter(lat_limits[1] <= latitude,  latitude <= lat_limits[2])
    
    lims <- c(
      min(first_time_pop$`Difference between baseline and crisis`, na.rm = TRUE),
      max(first_time_pop$`Difference between baseline and crisis`, na.rm = TRUE)
    )
    
    pts <- rbind(
      data.frame(lon = lon_limits[1], lat = lat_limits[1]),
      data.frame(lon = lon_limits[2], lat = lat_limits[2])
    )
    pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
    
    # Download OpenStreetMap tiles
    osm <- get_tiles(
      pts_sf,
      provider = "CartoDB.Voyager",
      crop = TRUE,
      zoom = 6
    )
    
    
    
    
  }
  
  
  # Default: no zoom
  xlim <- NULL
  ylim <- NULL
  
  # Convert lon/lat limits to EPSG:3857
  if (!is.null(lon_limits) && !is.null(lat_limits)) {
    
    bbox_ll <- st_as_sfc(
      st_bbox(
        c(
          xmin = lon_limits[1],
          xmax = lon_limits[2],
          ymin = lat_limits[1],
          ymax = lat_limits[2]
        ),
        crs = st_crs(4326)
      )
    )
    
    bbox_3857 <- st_transform(bbox_ll, 3857) |>
      st_bbox()
    
    xlim <- c(bbox_3857["xmin"], bbox_3857["xmax"])
    ylim <- c(bbox_3857["ymin"], bbox_3857["ymax"])
  }
  
  p1 <- ggplot() +
    layer_spatial(osm) +
    geom_sf(
      data = first_time_pop,
      aes(fill = `Difference between baseline and crisis`),
      color = "white",
      linewidth = 0.1,
      alpha = 1
    ) +
    scale_fill_gradient(
      trans = "log10",
      low = "blue",
      high = "red",
      limits = lims,
      labels = scales::label_comma()
    ) +
    coord_sf(
      crs = st_crs(3857),
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    geom_sf(
      data = fires,
      aes(color = "Fire Perimeter"),
      fill = NA,
      linewidth = 0.5
    ) +
    scale_color_manual(
      values = c("Fire Perimeter" = "darkred"),
      name = NULL
    ) +
    # Spokane marker
    #geom_sf(
    #  data = spokane,
    #  color = "blue",
    #  size = 3
    #) +
    theme_minimal()+
    theme(
      legend.position = "bottom",
      legend.box = "vertical",      # stack legends vertically
      legend.direction = "horizontal", # each legend is horizontal
      legend.key.width = unit(2, "cm")
    ) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        order = 1
      ),
      color = guide_legend(
        direction = "horizontal",
        order = 2
      )
    )
  
  if (title) {
    if (is.null(plot_title)) {
      p1 + ggtitle(format_time(plot_ds, plot_hour))
    } else {
      p1 + ggtitle(plot_title)
    }
  } else {
    p1
  }
}

test_it = F
if (test_it){
  population_plot_n_crisis(
    "2026-08-03", "0000",
    lon_limits = c(-118.5, -115.5),
    lat_limits = c(47, 49)
  )  
}