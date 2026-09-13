# Wildfire-perimeters step. Builds the WFIGS query bbox from a buffer around
# Spokane, downloads the *current* perimeters, and caches them (+ fetch time) to
# fires.Rds for reproducibility. Network failure falls back to the cache, then to
# an empty layer so a plot/report still renders. Pass re_run = TRUE to refresh.
fetch_fires <- function(re_run = re_run_cleaning, cache_dir = rds_cache_dir,
                        cache = "fires.Rds",
                        center_lon = -117.4260, center_lat = 47.6588,
                        buffer_m = 100000) {
  path <- cache_path(cache, cache_dir)
  if (!re_run && file.exists(path)) {
    message("Loading cached ", path)
    return(readRDS(path))
  }
  
  spokane <- st_as_sf(
    data.frame(lon = center_lon, lat = center_lat),
    coords = c("lon", "lat"), crs = 4326
  )
  # buffer_m-metre buffer around Spokane, back to lon/lat for the query bbox
  extent <- st_buffer(st_transform(spokane, 3857), buffer_m) |> st_transform(4326)
  bbox <- st_bbox(extent)
  
  url <- paste0(
    "https://services3.arcgis.com/T4QMspbfLg3qTGWY/ArcGIS/rest/services/",
    "WFIGS_Interagency_Perimeters_Current/FeatureServer/0/query?",
    "where=1%3D1",
    "&geometry=",
    bbox["xmin"], ",", bbox["ymin"], ",", bbox["xmax"], ",", bbox["ymax"],
    "&geometryType=esriGeometryEnvelope",
    "&inSR=4326",
    "&spatialRel=esriSpatialRelIntersects",
    "&outFields=*",
    "&returnGeometry=true",
    "&f=geojson"
  )
  
  message("Downloading wildfire perimeters from WFIGS -> ", path)
  tryCatch(
    {
      f <- st_read(url, quiet = TRUE)
      attr(f, "fetched_at") <- Sys.time()
      saveRDS(f, path)
      f
    },
    error = function(e) {
      if (file.exists(path)) {
        warning("Fire-perimeter download failed (", conditionMessage(e),
                "); using cached ", path, ".")
        readRDS(path)
      } else {
        warning("Fire-perimeter download failed (", conditionMessage(e),
                ") and no cache exists; continuing with no fire perimeters.")
        st_sf(geometry = st_sfc(crs = 4326))
      }
    }
  )
}

# Human-readable timestamp of the perimeter snapshot, for the report to cite.
fires_label <- function(fires) {
  t <- attr(fires, "fetched_at")
  if (inherits(t, "POSIXct") && length(t) == 1 && !is.na(t)) {
    format(t, "%B %d, %Y %I:%M %p %Z")
  } else {
    "a cached snapshot (fetch time unavailable)"
  }
}

p1 <- p1 |>
  # Fire perimeters as a mapped colour so they get their own legend entry.
  # clip_sf_to_box() crops them to the display box so they can't expand the frame.
  geom_sf(
    data = clip_sf_to_box(fires, xlim, ylim),
    aes(color = "Fire Perimeter"),
    fill = NA,
    linewidth = 0.5
  )