library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)


#### Import ####

# Read in all US crossing data
gcisAll <- read_csv("PublishedCrossingData-09-30-2020.csv",
    col_types = cols(
      MilePost = col_double()))

railroadShp <- st_read("shapefiles", layer = "North_American_Rail_lines") %>%
  st_transform(crs = 4269)

#### Filter ####

# Filter as necessary
data <- gcisAll %>%
  filter(
    PosXing == 1, # Filter for at-grade crossings only
    ReasonID != 16, # Filter out "reason for change = closure" (more reliable than "CrossingClosed")
    CityName == "MANKATO",
    StateName == "MN",
    RrDiv == "GREAT LAKES",
    MilePost < 89,
    !CrossingID %in% c(
      "185398F",
      "193450D",
      "193449J",
      "193452S",
      "193450D")) %>%
  select(
    CrossingID,
    CityName,
    Street,
    Highway,
    Aadt,
    AadtYear,
    HwySpeed,
    HwySpeedps,
    LastUpdated,
    Railroad,
    RrDiv,
    RrSubDiv,
    Branch,
    TypeXing,
    XPurpose,
    Latitude,
    Longitude,
    DayThru,
    NghtThru,
    TotalSwt,
    TotalLtr,
    WeekTrnMov,
    YearTrnMov,
    MaxTtSpd,
    MinSpd,
    MaxSpd,
    MainTrk,
    SidingTrk,
    YardTrk,
    TransitTrk,
    IndustryTrk,
    OthrTrk,
    Gates,
    FlashPai,
    MilePost,
    SpselIDs) %>%
  arrange(desc(MilePost)) %>%
  mutate(
    TotTrains = DayThru + NghtThru + TotalSwt + TotalLtr,
    Type = case_when(
      TypeXing == 3 & XPurpose == 1 ~ "Public Highway",
      TypeXing == 2  ~ "Private",
      TypeXing == 3 & XPurpose  > 1 ~ "Public Pedestrian",
      )
    )

#### Mapping ####

# Create sf from crossing table
dataShp <- data %>%
  st_as_sf(coords = c("Longitude", 'Latitude'), crs = 4269) %>%
  st_transform(crs = 4269)

#Set bbox for rail crop to limmit size
bb <- st_bbox(dataShp)
scaleDeg <- 0.5
bb[1] <- bb[1] - scaleDeg
bb[2] <- bb[2] - scaleDeg
bb[3] <- bb[3] + scaleDeg
bb[4] <- bb[4] + scaleDeg
railroadShpCrop <- railroadShp%>%
  st_crop(bb)

railroadShpCrop = railroadShpCrop %>%
  filter(RROWNER1 %in% c("UP", "CPRS")) %>%
  mutate(Railroad = case_when(
    RROWNER1 == "UP" ~ "Union Pacific",
    RROWNER1 == "CPRS" ~ "Canadian Pacific"
    )
  )

#Write/Read fixed the issue of all railroads showing up in legend.
st_write(railroadShpCrop, dsn = "shapefiles", layer = "MankatoRail", driver = "ESRI Shapefile", delete_layer = TRUE)
railroadMankato <- st_read("shapefiles", layer = "MankatoRail")

#Set interactive viewing map bbox
bbXings <- st_bbox(dataShp)

tmap_mode("view")

tm_basemap("CartoDB") +
  tm_shape(railroadMankato, bbox = bbXings) +
    tm_lines(
      col = "Railroad", 
      lwd = 2, 
      palette = c("#A50026", "#2166AC")) +
  tm_shape(dataShp) +
    tm_dots(
      size = .2,
      col = "Type",
      palette = "Dark2",
      popup.vars = c(
        "Street: " = "Street"),
      title = "Railroad Crossing Types")  +
  tm_view(dot.size.fixed = TRUE)


#### OLD ####

ggplot(data = data, aes(x=Longitude, y = Latitude, label = CrossingID), color = "red") +
  geom_point() +
  geom_label_repel()


data(World, metro)
tm_basemap(leaflet::providers$OpenWeatherMap) +
  tm_shape(metro, bbox = "India") + tm_dots(col = "red", group = "Metropolitan areas") 


#+
  tm_tiles(paste0("http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
                  "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"), group = "Labels")

  
  tmap_mode("plot")
  
  data(World)
  data(metro)
  
  # legend bubble size (10, 20, 30, 40 million) are
  # - are normlized by upper limit (40e6),
  # - square rooted (see argument perceptual of tm_symbols), and 
  # - scaled by 2:
  bubble_sizes <- ((c(10, 20, 30, 40) * 1e6) / 40e6) ^ 0.5 * 2 
  
  tm_shape(World) + 
    tm_polygons() + 
    tm_shape(metro) +
    tm_symbols(col='pop2020', 
               breaks = c(0, 15, 25, 35, 40) * 1e6,
               n=4,
               palette = 'YlOrRd',
               size='pop2020',
               sizes.legend = c(10, 20, 30, 40) * 1e6,
               size.lim = c(0, 40e6),
               scale = 2,
               legend.size.show = FALSE,    # comment this line to see the original size legend
               legend.col.show = FALSE,     # comment this line to see the original color legend
               legend.size.is.portrait = TRUE) + 
    tm_add_legend('symbol', 
                  col = RColorBrewer::brewer.pal(4, "YlOrRd"),
                  border.col = "grey40",
                  size = bubble_sizes,
                  labels = c('0-15 mln','15-25 mln','25-35 mln','35-40 mln'),
                  title="Population Estimate")
  
  ## End(Not run)