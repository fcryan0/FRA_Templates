library(tidyverse)
library(httr)
library(sf)
library(tmap)
library(tigris)
tmap_mode("view")

basemap <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) + tm_tiles("OpenRailwayMap", alpha = 0.2)

acc <-
  read_csv("CrossingData/Acc2021toNov.csv") %>%
  group_by(GXID) %>% summarize(Crashes2021 = n()) %>%
  mutate(CrossingID = GXID)

xings <- 
  read_csv("CurrentInventory/PublishedCrossingData-01-31-2022.csv") %>%
  filter(
    !is.na(Latitude), #filter xings without lat/long
    PosXing == 1, #filter only at-grade xings
    ReasonID != 16 #filter xings that have been closed
  ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  left_join(acc) 


basemap +
  tm_shape(xings %>% filter(Crashes2021 > 0)) + tm_dots(col = "darkred", alpha = 0.8, size = 0.03)

# Crashes per Crossing
temp <- xings %>% 
  st_drop_geometry() %>%
  group_by(StateName) %>% 
  summarize(
    crossings = n(), 
    crashes = sum(Crashes2021, na.rm = TRUE)
    ) %>%
  mutate(rate = crashes / crossings * 1000) %>%
  arrange(desc(rate))


# 2021 accident summary ---------------------------------------------------

accDetails <-
  read_csv("CrossingData/Acc2021toNov.csv")

accDetails %>% group_by(STATE) %>% summarize(count = n()) %>% arrange(desc(count))
