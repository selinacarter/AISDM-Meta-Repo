library(ggplot2)
library(dplyr)
library(sf)
library(gganimate)
library(ggforce)
library(ggspatial)
library(maptiles)
library(quadkeyr)
library(prettymapr)

moved <- mp_data_bing |>
  filter(start_longitude != end_longitude,
         start_latitude != end_latitude) |>
  rename(`# Users` = n_crisis)
##### ------ ARROWS w/ MAP-----


pts <- rbind(
  data.frame(lon = min(fb_data_bing$longitude), lat = min(fb_data_bing$latitude)),
  data.frame(lon = max(fb_data_bing$longitude), lat = max(fb_data_bing$latitude))
)

pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

# Download OpenStreetMap tiles
osm <- get_tiles(
  pts_sf,
  provider = "OpenStreetMap",
  crop = TRUE,
  zoom = 6
)

p <- ggplot() +
  layer_spatial(osm) +
  geom_link(
    data = moved,
    aes(
      x = start_longitude,
      y = start_latitude,
      xend = end_longitude,
      yend = end_latitude,
      color = `# Users`,
      #colour = after_stat(index)
    ),
    alpha = 0.5,
    arrow = arrow(length = unit(0.03, "npc")),
    linewidth = 1
  ) +
  coord_sf() +
  theme_minimal()+
  labs(title = "Time: {current_frame}") +
  transition_manual(format(date_time, "%Y-%m-%d %H:%M"))
anim <- animate(p, width = 800, height = 600, fps = 5)
anim_save("spokane_fires_movement.gif", anim)




##### ------ GRADIENT ARROWS -----

tiles <- quadkey_df_to_polygon(fb_data_bing)


tiles_3857 <- st_transform(tiles, 3857) |>
  drop_na(n_crisis) |>
  rename(`# Users` = n_crisis)

p <- ggplot() +
  layer_spatial(osm) +
  geom_sf(
    data = tiles_3857,
    aes(fill = `# Users`),
    color = "white",
    linewidth = 0.1,
    alpha = 0.8
  ) +
  scale_fill_gradient(low = "yellow", high = "red")+
  coord_sf(crs = st_crs(3857)) +
  theme_minimal() +
  labs(title = "Time: {current_frame}") +
  transition_manual(format(date_time, "%Y-%m-%d %H:%M"))

anim <- animate(p, width = 800, height = 600, fps = 5)
anim_save("spokane_fires_population.gif", anim)