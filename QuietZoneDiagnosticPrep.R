# Script for preparing quiet zone diagnostic materials

library(tidyverse)
library(sf)
library(tmap)
library(basemaps)


# Define Study Area Characteristics ---------------------------------------

# Hampshire, IL
crossingIDs <- c(
  "372269F",
  "372270A"
)

    # # Le Claire, IA
    # crossingIDs <- c(
    #   "865597L",
    #   "865599A",
    #   "865600S",
    #   "865601Y",
    #   "865602F",
    #   "865603M",
    #   "865604U",
    #   "865605B",
    #   "865606H",
    #   "865609D",
    #   "865610X",
    #   "865612L",
    #   "865614A",
    #   "865615G",
    #   "865616N"
    #   )




# Download FRA Data -------------------------------------------------------

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

gcisSA <- gcisCurrent %>% filter(CrossingID %in% crossingIDs)

shpXings <- gcisSA %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

tmap_mode("view")
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
tm_shape(shpXings) + tm_dots(col = "Type", popup.vars = TRUE)

# Adjust points
st_geometry(shpXings[shpXings$CrossingID == "372269F", ]) <- 
  st_geometry(st_as_sf(st_sfc(st_point(c(-88.53013247337978, 42.09918240681966)), crs = 4326)))


tm_shape(shpXings) + tm_dots()

# Create Maps for Diagnostic Review Forms ---------------------------------

# Buffer for inset maps
shpXingsBuff <- 
  shpXings %>% 
  st_transform(crsuggest::suggest_crs(shpXings) %>% slice_head(n = 1) %>% pull(crs_code) %>% as.numeric()) %>%
  st_buffer(500)

# Buffer for individual maps
shpXingsBuff2 <- 
  shpXings %>% 
  st_transform(crsuggest::suggest_crs(shpXings) %>% slice_head(n = 1) %>% pull(crs_code) %>% as.numeric()) %>%
  st_buffer(200)

basetile <- basemap(ext = shpXingsBuff, map_service = "osm", map_type = "streets")
#basetileLabels <- basemap(ext = counties %>% st_buffer(5280*1), map_service = "carto", map_type = "light_only_labels")

tmap_mode("plot")

# Loop to create to inset maps
x <- 1
for(x in 1:nrow(shpXings)) {
  tm <- tm_shape(basetile) + tm_rgb() +
    tm_shape(shpXingsBuff) + tm_borders(alpha = 0) + #needed because the shpXings bounding box does not change with the adjusted lat/long coordinates
    tm_shape(shpXings) + tm_dots(col = "navy", size = 0.5) +
    tm_shape(shpXings[x,]) + tm_symbols(border.col = "red", size = 3, border.lwd = 2, alpha = 0) 
  tmap_save(tm, filename = paste0("QuietZonePrep/Plot/Crossing", shpXings[x,] %>% pull(CrossingID), "_Inset.png"), 
            width = 3, height = 2, dpi = 300, units = "in", outer.margins = 0.01)  
}

for(x in 1:nrow(shpXings)) {
  basetileSat <- basemap(ext = shpXingsBuff2[x,], map_service = "esri", map_type = "world_imagery")
  basetileSatLabs <- basemap(ext = shpXingsBuff2[x,], map_service = "carto", map_type = "voyager_only_labels")
  tm <- tm_shape(basetileSat) + tm_rgb() +
    tm_shape(basetileSatLabs) + tm_rgb() 
  tmap_save(tm, filename = paste0("QuietZonePrep/Plot/Crossing", shpXings[x,] %>% pull(CrossingID), ".png"), 
              width = 7, height = 7, dpi = 300, units = "in", outer.margins = 0.01)
}



# Overview Map with Inset -------------------------------------------------

tm_shape(basetile) + tm_rgb() +
  tm_shape(shpXingsBuff) + tm_borders(alpha = 0) + #needed because the shpXings bounding box does not change with the adjusted lat/long coordinates
  tm_shape(shpXings) + tm_dots(col = "navy", size = 0.3) + tm_text("CrossingID", size = 0.8, ymod = -0.5)
tmap_save(filename = "QuietZonePrep/Plot/Report_Corridor.png", 
          width = 6.5, height = 5, dpi = 300, units = "in", outer.margins = 0.01)

shpXingsBuff3 <- 
  shpXings %>% 
  st_transform(crsuggest::suggest_crs(shpXings) %>% slice_head(n = 1) %>% pull(crs_code) %>% as.numeric()) %>%
  st_buffer(1000000)
basetileInset <- 
  basemap(ext = shpXingsBuff3, map_service = "osm", map_type = "streets")
  
tmap_mode("plot")
tm_shape(basetileInset) + tm_rgb(alpha = 0.8) +
  tm_shape(shpXingsBuff3) + tm_borders(alpha = 0) + #needed because the shpXings bounding box does not change with the adjusted lat/long coordinates
  tm_shape(shpXings %>% summarize() %>% st_centroid() %>% mutate(Name = "Hampshire")) + 
    tm_dots(col = "darkred", size = 0.1) + tm_text("Name", size = 0.5, ymod = 0.5, shadow = TRUE)
tmap_save(filename = "QuietZonePrep/Plot/Report_Corridor_Inset.png", 
          width = 3, height = 3, dpi = 300, units = "in", outer.margins = 0.01)
                               