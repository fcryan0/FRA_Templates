library(tidyverse)
library(httr)
library(sf)
library(tmap)
library(tigris)
tmap_mode("view")

basemap <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) + tm_tiles("OpenRailwayMap", alpha = 0.5)

dataRaw <- 
  GET("https://www.fra.dot.gov/blockedcrossings/api/incidents?page=1&pageSize=500000") %>% 
  content() %>% bind_rows() %>%
  rename(CrossingID = crossingID)

xings <- 
  read_csv("CurrentInventory/PublishedCrossingData-01-31-2022.csv") %>%
  filter(
    !is.na(Latitude), #filter xings without lat/long
    PosXing == 1, #filter only at-grade xings
    ReasonID != 16 #filter xings that have been closed
    ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 

blockedXings <- dataRaw %>% left_join(xings)


blockedXings %>% count(CrossingID) %>% arrange(desc(n))
blockedXings %>% count(reason)
blockedXings %>% count(duration)


# Blocked Crossings: National Count of Reports -------------------------------------

blockedXingsReportCount <- 
  blockedXings %>% count(CrossingID) %>%
  left_join(xings %>% select(CrossingID)) %>% st_as_sf()
blockedXingsReportCount

basemap +
  tm_shape(blockedXingsReportCount) + tm_dots(alpha = 0.5, col = "red", size = "n", scale = 2)

# Blocked Crossings: State Count of Reports -------------------------------------

state <- "ND"
stateSF <- states(cb = TRUE) %>% filter(STUSPS == state) %>% st_transform(4326)

blockedXingsReportCount <- 
  blockedXings %>% count(CrossingID) %>%
  left_join(xings %>% select(CrossingID)) %>% st_as_sf() %>%
  filter(lengths(st_intersects(., stateSF)) > 0)
blockedXingsReportCount %>% summarize(complaints = sum(n))

basemap +
  tm_shape(stateSF) + tm_borders() +
  tm_shape(blockedXingsReportCount) + tm_dots(alpha = 0.5, col = "n", size = "n", scale = 1, palette = "-Spectral", title = "FRA Complaint Records")
