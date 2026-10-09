
library(tidyverse)
library(sf)
library(patchwork)

# 1. Map limits (top extended so the glacier site at 28.3N is inside)
lon_limits <- range(fb_data_bing$longitude, na.rm = TRUE)
lat_limits <- range(fb_data_bing$latitude,  na.rm = TRUE)
lat_limits[2] <- max(lat_limits[2], 28.45)

# 2. Shared colour limits
diff_fill_limits <- shared_limits(
  col       = "Difference between baseline and crisis",
  symmetric = TRUE,
  ds1 = w[["first_ds"]],  hour1 = w[["first_hour"]],
  ds2 = w[["latest_ds"]], hour2 = w[["latest_hour"]]
)

# 3. Labels (Kathmandu / Glacier are re-drawn by 3_flood_functions.R, but the
#    labels object must still exist and contain those names)
nepal_labels <- data.frame(
  label = c("Kathmandu", "Glacier Outbreak"),
  lon   = c(85.3240,     85.5000),
  lat   = c(27.7172,     28.3000)
) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  sf::st_transform(3857)

nepal_label_angles <- c("Kathmandu" = -45, "Glacier Outbreak" = -90)

# 4. The two panels
message("Building left panel (first run downloads + caches map layers) ...")
p_diff_left <- population_plot_n_difference(
  plot_ds = w[["first_ds"]], plot_hour = w[["first_hour"]], tzone = tzone,
  lon_limits = lon_limits, lat_limits = lat_limits, zoom = 6,
  fill_limits = diff_fill_limits,
  labels = nepal_labels, label_angles = nepal_label_angles,
  highway_detail = "none", disaster_type = "flood"
)

message("Building right panel ...")
p_diff_right <- population_plot_n_difference(
  plot_ds = w[["latest_ds"]], plot_hour = w[["latest_hour"]], tzone = tzone,
  lon_limits = lon_limits, lat_limits = lat_limits, zoom = 6,
  fill_limits = diff_fill_limits,
  labels = nepal_labels, label_angles = nepal_label_angles,
  highway_detail = "none", disaster_type = "flood"
)

# 5. Combine, SHOW and SAVE
fig_difference <- (p_diff_left | p_diff_right) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::guides(
    fill   = ggplot2::guide_colorbar(title.position = "top", order = 1,
                                     barwidth = grid::unit(7, "cm"),
                                     barheight = grid::unit(0.4, "cm")),
    colour = ggplot2::guide_legend(title.position = "left", order = 2, nrow = 1,
                                   keywidth = grid::unit(1.2, "cm"))
  ) &
  ggplot2::theme(legend.position = "bottom",
                 legend.box = "vertical",                    # colour bar above, map features below
                 legend.text = ggplot2::element_text(size = 8),
                 legend.title = ggplot2::element_text(size = 9),
                 plot.margin = ggplot2::margin(5, 15, 5, 15),
                 text = ggplot2::element_text(size = 9))

print(fig_difference)
ggplot2::ggsave("nepal_flood_difference.png", fig_difference,
                width = 14, height = 8.5, dpi = 200)         # wider so nothing is clipped
message("Saved: ", normalizePath("nepal_flood_difference.png"))
