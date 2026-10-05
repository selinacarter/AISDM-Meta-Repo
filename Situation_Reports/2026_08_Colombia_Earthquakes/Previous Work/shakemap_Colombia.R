library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
run_once = FALSE #Set to TRUE first time you run so that the zip files download. Then turn it off.
# Colombia bounding box
setwd("C:/Users/selin/Dropbox/CMU/Meta_AI_good/Colombia") #Change to your path




if (run_once){
  download.file(
    "https://earthquake.usgs.gov/pdl/products/urn:usgs-product:us:shakemap:us6000tjl2:1786452072000/contents/download/shape.zip",
    "shakemap_shape.zip"
  )
  
  unzip("shakemap_shape.zip", exdir = "shakemap")
  
}

shp_files <- list.files(
  "shakemap",
  pattern = "\\.shp$",
  full.names = TRUE
)

shakemaps <- lapply(shp_files, st_read)
names(shakemaps) <- tools::file_path_sans_ext(
  basename(shp_files)
)

#PSA1P0 (often written as PSA1.0 or SA(1.0)) stands for 1.0-second Peak Pseudospectral Acceleration. It measures the maximum shaking intensity an earthquake causes to a simple physical structure that takes 1.0 second to complete one swing back and forth.
ggplot(shakemaps$psa1p0) +
  geom_sf(aes(color = PARAMVALUE)) +
  theme_minimal() +
  labs(
    title = "Earthquake Maximum Shaking Intensity",
    fill = "PSA1.0"
  )


#Note: MMI stands for the Modified Mercalli Intensity scale. It measures the local severity and shaking intensity of an earthquake based on observed effects on people, buildings, and the landscape, ranging from Roman numeral I (not felt) to XII (total destruction)
ggplot(mmi) +
  geom_sf(aes(fill = PARAMVALUE)) +
  theme_minimal() +
  labs(
    title = "Earthquake Shaking Intensity",
    fill = "MMI"
  )

#Note: PGV stands for Peak Ground Velocity. It measures the maximum speed (rate of ground movement) that the earth's surface reaches as seismic waves pass by
ggplot(shakemaps$pgv) +
  geom_sf(aes(fill = PARAMVALUE)) +
  theme_minimal() +
  labs(
    title = "Earthquake Peak Ground Velocity",
    fill = "PGV"
  )



#In an earthquake, PGA stands for Peak Ground Acceleration. It measures how hard the earth shakes horizontally or vertically at a specific geographic location, expressed as a fraction of Earth's gravity (g) or in units like m/s².
ggplot(shakemaps$pga) +
  geom_sf(aes(fill = PARAMVALUE)) +
  theme_minimal() +
  labs(
    title = "Earthquake Peak Ground Acceleration.",
    fill = "PGA"
  )

ggplot(shakemaps$psa0p3) +
  geom_sf(aes(fill = PARAMVALUE, alpha = .2)) +
  theme_minimal() +
  labs(
    title = "Earthquake Peak Ground Acceleration.",
    fill = "PGA"
  )




