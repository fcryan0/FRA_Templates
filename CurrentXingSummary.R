library(tidyverse)
library(tigris)
library(lubridate)
library(sf)
library(tmap)
library(tmaptools)
library(httr)
library(rmapshaper)
library(ggspatial)

#### Set State  ####

state <- "NM"
stateFIPS <- str_extract(lookup_code(state), '(?<=\').*?(?=\')')
fips <- tigris::fips_codes %>% mutate(GEOID = paste0(state_code, county_code))

stateShp <- states(cb = TRUE) %>% filter(STUSPS == state) %>% st_transform(4326)

#### Read/Modify Data ####

# Read in all Current US crossing data
gcisCurrent <- read_csv("CurrentInventory/PublishedCrossingData-01-31-2022.csv",
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
      Whistban == 0 ~ "Horns Sound",
      is.na(Whistban) ~ "Horns Sound"
    )
  )

temp <- gcisCurrent %>%
  filter(
    AwhornChk == 1
  )

# Read in all Accident/Incident data
file_namesAcc <- dir(pattern = "^Acc", recursive = TRUE) # List of all files starting with "Acc"
gcisAccHist <- do.call(rbind, lapply(file_namesAcc, read_csv)) %>% 
  mutate(GEOID = paste0(STATE, CNTYCD))


#### Summary Tables ####

# Tally Crossings per State
CrossingsPerState <- gcisCurrent %>%
  group_by(StateName, Type) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Type, values_from = count) %>%
  mutate(totXings = sum(Priv, PubHwy, PubPed))

write_csv(CrossingsPerState, "CrossingsPerState.csv")

# D6 RRs
#D6counties <- c("Dodge", "Fillmore", "Freeborn", "Goodhue", "Houston", "Mower", "Olmsted", "Rice", "Steele", "Wabasha", "Winona")
#counties <- counties(state = "MN", cb = TRUE, progress_bar = FALSE) %>% 
#  filter(NAME %in% D6counties) %>% mutate(D6 = TRUE) %>% st_transform(4326)

#shpXings <- gcisCurrent[!is.na(gcisCurrent$Latitude),] %>%
#  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
#  filter(lengths(st_intersects(., counties)) > 0) 

#tm_shape(shpXings) + tm_dots(col = "Railroad")

#CrossingByRR <- shpXings %>%
#  group_by(SepRr1) %>% summarize(count = n())

# Tally crashes by Xing ID within set timeframe
accCount <- gcisAccHist %>%
  filter(YEAR %in% 16:20) %>%
  group_by(GXID) %>%
  rename("CrossingID" = GXID) %>%
  summarize (Crashes5yr = n())

# Join crash count to gcisCurrent
gcisCurrent <-  gcisCurrent %>%
  left_join(accCount, by = "CrossingID")

gcisCurrent$Crashes5yr[is.na(gcisCurrent$Crashes5yr)] <- 0


# Create Table of Public Crossings by State
StatePubXingCount <- gcisCurrent %>%
  filter(TypeXing == 3) %>%
  group_by(StateName) %>%
  summarize(count = n())

#Create summary table of crossing types for single state
TypeSumm <- gcisCurrent %>%
  filter(
    StateName == state
  ) %>%
  group_by(
    Type
  ) %>%
  summarize(
    count = n(),
    crashCount = sum(Crashes5yr)
  )

write_csv(TypeSumm, "TypeSumm.csv")

# Create summary table of public crossing warning device type
WarnSumm <- gcisCurrent %>%
  filter(
    StateName == state,
    TypeXing == 3
  ) %>%
  group_by(WarnDev) %>%
  summarize(
    count = n(),
    crashCount = sum(Crashes5yr)
  )

write_csv(WarnSumm, "WarnSumm.csv")  

#Tally Crashes (2015-2019, by vehicle position)
AccPosSumm <- gcisAccHist %>%
  filter(
    YEAR %in% 15:19,
    STATE == stateFIPS
  ) %>%
  group_by(
    POSITION
  ) %>%
  summarize(
    count = n(),
    fatal = sum(TOTKLD)
  )


#Tally Crashes (2015-2019, by County in state)
AccSumm <- gcisAccHist %>%
  filter(
    YEAR %in% 16:20,
    STATE == stateFIPS
  ) %>%
  group_by(
    GEOID
  ) %>%
  summarize(
    crash5yr = n(),
    fatal = sum(TOTKLD)
  ) %>%
  left_join(fips %>% select(GEOID, county))


#### Mapping ####

#Get County Geometry
ctyGeom <- counties(state, cb = TRUE) %>% st_transform(4326)

bb <- st_bbox(ctyGeom)
st_crs(bb)  

shp <- left_join(ctyGeom, AccSumm)

shpXings <- gcisCurrent[!is.na(gcisCurrent$Latitude),] %>%
  arrange(QZ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  filter(StateName == state) 
 

# Read in NA Rail Line Data; clipped to state bounding box

railLines <- read_sf("shapefiles/North_American_Rail_Lines.shp") %>% st_transform(4326)

railLinesTemp <- railLines %>% filter(lengths(st_intersects(., stateShp)) > 0)

railLinesIN <- railLinesTemp %>% st_intersection(stateShp) %>% summarize()

#st_crs(railLines)
#st_r

plot(railLinesIN)

tmap_mode("view")

tm_basemap( 
  c( 
    "CartoDB.Positron", 
    "OpenStreetMap.Mapnik", 
    "Esri.WorldImagery" 
  )) + 
tm_shape(shp) +
  tm_polygons("crash5yr",
    style = "jenks",
    n = 5,
    #breaks = c(1, 2, 5, 10, 50, 100),
    palette = "RdPu",
    colorNA = "light gray",
    textNA = "No Crashes",
    border.col = "white",
    lwd = 1,
    border.alpha = 0.5,
    alpha = 0.75,
    title = "Crashes by County") +
tm_shape(railLinesIN) +
  tm_lines(
    alpha = 0.5,
    col = "black") +
tm_legend(legend.outside = TRUE) +
tm_add_legend("line", col = "black", labels = "Rail Lines") +
tm_layout(
    frame = FALSE,
    inner.margins = c(0, 0.12, 0, 0)
  ) +
  tm_view(view.legend.position = c("right", "top")) +
  tm_view(view.legend.position = c("right", "bottom"))


#tm_basemap( 
#  c( 
#    "CartoDB.Positron", 
#    "OpenStreetMap.Mapnik", 
#    "Esri.WorldImagery" 
#  )) + 
#tm_shape(shpXings %>% filter(Crashes5yr > 0)) + tm_dots(col = "Crashes5yr", size = 0.5)

#tmap_save(m, "test.png")      




