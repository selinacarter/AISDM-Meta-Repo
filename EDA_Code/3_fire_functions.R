fetch_fires <- function(
    start_date,
    end_date,
    lon_limits,
    lat_limits,
    re_run = re_run_cleaning,
    cache_dir = rds_cache_dir,
    cache = "fires.Rds",
    date_field = "attr_FireDiscoveryDateTime",
    layer_url = paste0(
      "https://services3.arcgis.com/T4QMspbfLg3qTGWY/",
      "ArcGIS/rest/services/",
      "WFIGS_Interagency_Perimeters/",
      "FeatureServer/0/query"
    )
) {
  
  # ------------------------------------------------------------
  # Cache
  # ------------------------------------------------------------
  
  path <- cache_path(cache, cache_dir)
  
  if (!re_run && file.exists(path)) {
    message("Loading cached ", path)
    return(readRDS(path))
  }
  
  
  # ------------------------------------------------------------
  # Check longitude / latitude limits
  # ------------------------------------------------------------
  
  if (length(lon_limits) != 2 || length(lat_limits) != 2) {
    stop(
      "lon_limits and lat_limits must each contain exactly ",
      "two values."
    )
  }
  
  if (lon_limits[1] >= lon_limits[2]) {
    stop(
      "lon_limits must be c(min_longitude, max_longitude)."
    )
  }
  
  if (lat_limits[1] >= lat_limits[2]) {
    stop(
      "lat_limits must be c(min_latitude, max_latitude)."
    )
  }
  
  
  # ------------------------------------------------------------
  # Convert dates
  # ------------------------------------------------------------
  
  start_date <- as.POSIXct(
    start_date,
    tz = "UTC"
  )
  
  end_date <- as.POSIXct(
    end_date,
    tz = "UTC"
  )
  
  if (is.na(start_date) || is.na(end_date)) {
    stop(
      "start_date and end_date must be valid dates."
    )
  }
  
  if (start_date > end_date) {
    stop(
      "start_date must be before end_date."
    )
  }
  
  
  # ------------------------------------------------------------
  # Bounding box
  # ------------------------------------------------------------
  
  bbox <- paste(
    lon_limits[1],
    lat_limits[1],
    lon_limits[2],
    lat_limits[2],
    sep = ","
  )
  
  
  # ------------------------------------------------------------
  # Date query
  # ------------------------------------------------------------
  
  where <- paste0(
    date_field,
    " >= DATE '",
    format(start_date, "%Y-%m-%d %H:%M:%S"),
    "' AND ",
    date_field,
    " <= DATE '",
    format(end_date, "%Y-%m-%d %H:%M:%S"),
    "'"
  )
  
  
  # ------------------------------------------------------------
  # Build query URL
  # ------------------------------------------------------------
  
  url <- paste0(
    layer_url,
    "?",
    "where=",
    URLencode(where, reserved = TRUE),
    "&geometry=",
    URLencode(bbox, reserved = TRUE),
    "&geometryType=esriGeometryEnvelope",
    "&inSR=4326",
    "&spatialRel=esriSpatialRelIntersects",
    "&outFields=*",
    "&returnGeometry=true",
    "&f=geojson"
  )
  
  
  # ------------------------------------------------------------
  # Download
  # ------------------------------------------------------------
  
  message(
    "Downloading wildfire perimeters from WFIGS..."
  )
  
  message(
    "Dates: ",
    format(start_date, "%Y-%m-%d"),
    " to ",
    format(end_date, "%Y-%m-%d")
  )
  
  message(
    "Longitude: ",
    paste(lon_limits, collapse = " to ")
  )
  
  message(
    "Latitude: ",
    paste(lat_limits, collapse = " to ")
  )
  
  
  # ------------------------------------------------------------
  # Download with cache fallback
  # ------------------------------------------------------------
  
  tryCatch(
    
    {
      f <- st_read(
        url,
        quiet = TRUE
      )
      f <- st_transform(f, 3857)
      
      message(
        "Downloaded ",
        nrow(f),
        " fire perimeter(s)."
      )
      
      
      # --------------------------------------------------------
      # Metadata
      # --------------------------------------------------------
      
      attr(f, "fetched_at") <- Sys.time()
      attr(f, "query_start") <- start_date
      attr(f, "query_end") <- end_date
      attr(f, "query_lon_limits") <- lon_limits
      attr(f, "query_lat_limits") <- lat_limits
      attr(f, "query_date_field") <- date_field
      attr(f, "query_url") <- url
      
      
      # --------------------------------------------------------
      # Save
      # --------------------------------------------------------
      
      saveRDS(
        f,
        path
      )
      
      f
    },
    
    
    error = function(e) {
      
      # --------------------------------------------------------
      # Cache fallback
      # --------------------------------------------------------
      
      if (file.exists(path)) {
        
        warning(
          "Fire-perimeter download failed (",
          conditionMessage(e),
          "); using cached ",
          path,
          "."
        )
        
        readRDS(path)
        
      } else {
        
        warning(
          "Fire-perimeter download failed (",
          conditionMessage(e),
          ") and no cache exists; ",
          "continuing with no fire perimeters."
        )
        
        st_sf(
          geometry = st_sfc(
            crs = 3857
          )
        )
      }
    }
  )
}


fires <- fetch_fires(
  start_date = start_date,
  end_date   = end_date,
  lon_limits = lon_limits,
  lat_limits = lat_limits,
  re_run = TRUE
)

p1 <- p1 +
  geom_sf(
    data = fires,
    aes(color = "Fire Perimeter"),
    fill = NA,
    linewidth = 0.5
  )