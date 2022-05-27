library(tidyverse)
library(httr)
library(sf)
library(jsonlite)
library(OData)

token <- "c38e8c42dca973c5512de3f9bf76afc7"

url <- paste0("https://safetydata.fra.dot.gov/MasterWebService/PublicApi/frads/v1/odata/gcis/Crossings?token=", token)

test <- read_sf(request)

request <- GET(url)
response <- content(request, as = "text", encoding = "UTF-8")
data <- fromJSON(response) %>% data.frame()



test <- entitySets(url)
test <- metadata("https://safetydata.fra.dot.gov/MasterWebService/PublicApi/frads/v1/odata/gcis/$metadata")
test <- retrieveData(url)

#### Create Analysis ####
bodyA <- list(
  insight_login_email = "chris.ryan@hdrinc.com",
  analysis_name = "ZA-APITest",
  analysis_type = "Zone_Activity_Analysis",
  travel_mode_type = "All_Vehicles",
  oz_sets = list(list(name = "TestZones5")), #double list required to get square brackets in JSON
  date_ranges = list(list( #double list required to get square brackets in JSON
    start_date = "06/01/2020",
    end_date = "07/15/2020")),
  day_types = "Average Day|17,Average Weekday|14,Average Weekend Day|67",
  day_parts = "All Day|0023,Early AM|0005,Peak AM|0609,Mid-Day|1014,Peak PM|1518,Late PM|1923",
  trip_attributes = FALSE,
  traveler_attributes = FALSE,
  output_type = "volume",
  enable_visualization = TRUE
)

bodyAJSON <- jsonlite::toJSON(bodyA, auto_unbox = TRUE)

urlPostA <- paste0("https://insight.streetlightdata.com/api/v2/analyses", "?key=", StlKey)

# Call to create Zone Analysis
POST(urlPostA, content_type_json(), body = bodyAJSON)