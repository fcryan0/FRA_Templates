library(tidyverse)
library(lubridate)
library(sf)
library(tmap)
library(tmaptools)
library(tigris)


#### Read/Modify Data ####

# Read in all Current US crossing data
gcisCurrent <- read_csv("PublishedCrossingData-05-31-2021.csv",
                        col_types = cols(
                          MilePost = col_double())) %>%
  filter(
    PosXing == 1, # Filter for at-grade crossings only
    ReasonID != 16, # Filter out "reason for change = closure" (more reliable thatn "CrossingClosed")
    #TypeXing == 3, # Filter for Private(2)/Public(3)
    #XPurpose == 1 # Filter for Highway(1)/PedPath(2)/PedStation(3)
  ) %>%
  mutate(
    Type = case_when(
      TypeXing == 3 & XPurpose == 1 ~ "PubHwy",
      TypeXing == 2  ~ "Priv",
      TypeXing == 3 & XPurpose  > 1 ~ "PubPed",
    ),
    WarnDev = case_when(
      GateConf == 3 ~ "FourQuad",
      Gates > 0 & Channel %in% 1:4 ~ "GatesWithMedian",
      Gates > 0 ~ "Gates",
      FlashPai > 0 & Gates == 0 ~ "FlashingOnly",
      Gates == 0 & FlashPai == 0 & StopStd > 0 ~ "StopSign",
      Gates == 0 & FlashPai == 0 & StopStd == 0 & XBuck > 0 ~ "Crossbuck"
    ),
    QZ = case_when(
      Whistban == 1 ~ "QZ",
      SpselIDs == 11 ~ "Has CWT",
      Whistban == 0 ~ "No CWT",
      is.na(Whistban) ~ "No CWT"
    )
  )



#### Mapping ####

State <- states() %>% filter(STUSPS == "WI") %>% st_transform(4326)

shpXings <- gcisCurrent[!is.na(gcisCurrent$Latitude),] %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  select(
    CrossingID,
    Railroad,
    CityName,
    CountyName,
    StateName,
    Street,
    Highway,
    Type,
    WarnDev, 
    QZ
  ) %>%
  st_intersection(State)

shpXingsWaupaca <- shpXings %>% filter (CountyName == "WAUPACA")

tmap_mode("view")

tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) + 
  tm_shape(shpXingsWaupaca) +
  tm_dots(col = "QZ", size = 0.1, palette = "Set2", 
          popup.vars = c(
            "CrossingID", 
            "Street",
            "WarnDev")
          ) +
  tm_text("Street", just = "left", ymod = 0.3)

tm_basemap("CartoDB.Positron") + 
  tm_shape(shpXings) +
  tm_dots(col = "QZ", size = 0.1, palette = "Set2")







#### OLD ####

# Read in all Accident/Incident data
file_namesAcc <- dir(pattern = "^Acc") # List of all files starting with "Acc"
gcisAccHist <- do.call(rbind, lapply(file_namesAcc, read_csv))

# Tally crashes by Xing ID within set timeframe
accCount3yr <- gcisAccHist %>%
  filter(YEAR %in% c(17:19)) %>%
  group_by(GXID) %>%
  rename("CrossingID" = GXID) %>%
  summarize (Crashes3yr = n())

accCount5yr <- gcisAccHist %>%
  filter(YEAR %in% c(15:19)) %>%
  group_by(GXID) %>%
  rename("CrossingID" = GXID) %>%
  summarize (Crashes5yr = n())

# Join crash count to gcisCurrent
gcisCurrent <-  gcisCurrent %>%
  left_join(accCount3yr, by = "CrossingID") %>%
  left_join(accCount5yr, by = "CrossingID") 

gcisCurrent$Crashes3yr[is.na(gcisCurrent$Crashes3yr)] <- 0
gcisCurrent$Crashes5yr[is.na(gcisCurrent$Crashes5yr)] <- 0

# Check

byState <- gcisCurrent %>%
  filter(Crashes3yr > 0 | Crashes5yr > 1) %>%
  group_by(StateName) %>%
  summarize(crashes = sum(Crashes5yr),Xings = n()) 

gcisCurrentFilter <- gcisCurrent %>%
  filter(Crashes3yr > 0 | Crashes5yr > 1)




tmap_mode("view")

tm_basemap("CartoDB.Positron") + 
  tm_shape(shpXings) +
  tm_dots(col = "darkred", size = 0.05)
