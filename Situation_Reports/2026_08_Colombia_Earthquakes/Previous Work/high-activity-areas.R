re_run_cleaning <- FALSE
source("data-cleaning.R", local = env)

lon_limits = c(-77, -75)
lat_limits = c(4,7)

fb_data_bing |>
  filter(ds == first_ds,
         hour == "0800",
         between(latitude, lat_limits[1], lat_limits[2]),
         between(longitude, lon_limits[1], lon_limits[2])) |>
  View()


x_point <- st_as_sf(
  data.frame(longitude = c(-75.68481, -75.57495, -76.65161, -75.22339), 
             latitude = c(4.817312, 6.238855, 5.670651, 4.444997),
             label = c("Dosquebradas, Risaralda", "Medellin, Antioquia", "???", "Ibague, Tolima")),
  coords = c("longitude", "latitude"),
  crs = 4326
) |>
  st_transform(3857)

population_plot_n_difference(first_ds, "0800", 
                             eq_limits = .eq_limits,
                             lon_limits = lon_limits, 
                             lat_limits = lat_limits, 
                             zoom = pop_zoom) +
  geom_sf_text(
    data = x_point,
    aes(label = label),
    #shape = 4,
    color = "black",
    size = 2,
    stroke = 1.5
  )
