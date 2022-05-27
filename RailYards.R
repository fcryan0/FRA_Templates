# Script for Identifying named rail yards within a study area

library(tidyverse)
library(sf)
library(tmap)
library(tigris)

tmap_mode("view")
studyArea <- states(cb = TRUE) %>% filter(STUSPS == "NE")
crs <- 
  crsuggest::suggest_crs(studyArea) %>% slice_head(n = 1) %>% pull(crs_code) %>% as.numeric()
studyArea <- studyArea %>% st_transform(crs)

# Download data (SKIP IF ALREADY DOWNLOADED) -----------------------------------------------------------

# Downlaad from: https://data-usdot.opendata.arcgis.com/datasets/usdot::north-american-rail-network-lines/about
railLines <- read_sf("https://geo.dot.gov/mapping/rest/services/NTAD/North_American_Rail_Network_Lines/MapServer/0/query?outFields=*&where=1%3D1&f=geojson")
railLinesSA <- railLines %>% st_transform(crs) %>% st_intersection(studyArea)


tm_shape(railLinesSA %>% select(NET)) + tm_lines(col = "NET", lwd = 5, popup.vars = TRUE)

railYards <- 
  railLinesSA %>%
  filter(
    NET == ("Y"),
    !is.na(YARDNAME)
    )

tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(railYards) + tm_lines(col = "YARDNAME", lwd = 5, popup.vars = TRUE)

test <- railYards %>% count(YARDNAME)


