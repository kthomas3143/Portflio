# insert data 
HDFC_addresses <- read.csv("~/Downloads/UHAB - Sheet1 (2) 2.csv")

# turn data into shapefile
library(sf)
library(dplyr)
HDFC_shapefile <- HDFC_addresses %>% 
  st_as_sf(coords = c("Latitude", "Longitude"), crs = 4326)

HDFC_shapefile %>% 
  st_as_sf(coords = c("Longitude", "Latitude")) %>% 
  st_sf(crs = 4326)

st_write(HDFC_shapefile, "HDFC_shapefile.shp")
if (st_crs(HDFC_shapefile) != st_crs(census_boroughs)) {
  HDFC_shapefile <- st_transform(HDFC_shapefile, st_crs(census_2020))
}

census_2020 <- st_read("~/Downloads/nyct2020_24c/nyct2020.shp")
census_2020 <- st_transform(census_2020, crs = st_crs("+proj=longlat +datum=WGS84"))

# combine shapefile with other shapefile

matched_data <- st_join(census_2020, HDFC_shapefile, join = st_intersection)

intersection_data <- st_intersection(HDFC_shapefile, census_2020)

result <- st_join(HDFC_shapefile, census_2020)
library(mapview)
mapview(HDFC_shapefile)


colnames(HDFC_shapefile)
colnames(census_2020)
