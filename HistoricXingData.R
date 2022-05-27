# Script used to analyze historic FRA inventory and crash data


library(tidyverse)
library(lubridate)
library(scales)
library(tigris)
library(sf)
library(tmap)
library(scales)
tmap_mode("view")



    # # Read in all Historic US crossing data
    # file_names <- list.files("CrossingData", pattern = "^CrossingData", full.names = TRUE) 
    # gcisHist <- map_dfr(file_names, ~read_csv(., col_types = cols(.default = "c")))
    # 
    # # Read in all Crash Data (1980-2020)
    # file_names <- list.files("CrossingData", pattern = "^Acc", full.names = TRUE)
    # accHist <- map_dfr(file_names, ~read_csv(., col_types = cols(.default = "c")))
    # 
    # save(gcisHist, accHist, file = "CrossingData/_CombinedData.RData")
load("CrossingData/_CombinedData.RData")

state <- "NM"

# Minnesota Colors
color1 <- "#003865"
color2 <- "#A4BCC2"

# Ohio Colors
# color1 <- "#0067a7"
# color2 <- "#20a5de"

# Ohio Colors
# color1 <- "#700017"
# color2 <- "rosybrown3"

stateSF <- states(cb = TRUE) %>% filter(STUSPS == state)
tm_shape(stateSF) + tm_borders()
stateFIPS <- stateSF %>% pull(GEOID)

accHist2 <- accHist %>% 
  filter(PUBLIC == "Y") %>% 
  mutate(
    Year = case_when(
      as.numeric(IYR) <= 20 ~ as.numeric(IYR) + 2000,
      TRUE ~ as.numeric(IYR) + 1900
      ),
    TOTKLD = as.numeric(TOTKLD),
    TOTINJ = as.numeric(TOTINJ),
    Severity = case_when(
      TOTKLD > 0 ~ "Fatal",
      TOTINJ > 0 ~ "Injury", 
      TRUE ~ "Property Damage"
      )
    ) 

accHistAnnTot <- 
  accHist2 %>% group_by(Year) %>% 
  summarize(Count = n())

accHistState <- 
  accHist2 %>% filter(STATE == stateFIPS) %>% 
  group_by(Year) %>% summarize(Count = n())

accHistStateCty <- 
  accHist2 %>% filter(STATE == stateFIPS, str_detect(COUNTY, "LOUIS")) %>% 
  group_by(Year) %>% summarize(Count = n())

accHistStateSev <- 
  accHist2 %>% filter(STATE == stateFIPS) %>% 
  group_by(Year, Severity) %>% summarize(Count = n())

accHistStateCtySev <- 
  accHist2 %>% filter(STATE == stateFIPS, str_detect(COUNTY, "LOUIS")) %>% 
  group_by(Year, Severity) %>% summarize(Count = n())

# Check to compare user type for fatal crashes 
accHistStateFatalUser <- 
  accHist2 %>% filter(STATE == stateFIPS, Year >= 2016) %>%
  group_by(TYPVEH) %>% summarize(n = n())

# Total Crashes by Year by Severity
accHist2 %>% 
  ggplot(aes(x = Year, fill = Severity)) + geom_bar() +
  scale_fill_manual(values = c("darkred", "darkorange3", "darkslategrey")) +
  scale_y_continuous(labels = comma) +
  ylab("Accident/Incident Count") +
  theme(legend.position = "top")
ggsave(filename = "Output/National Crashes by Severity.jpg", width = 10, height = 6, device = "jpeg", dpi = 300, units = "in")
plotly::ggplotly()
temp <- accHist2 %>% count(Year)

# Total Crashes by Year
accHist2 %>% 
  ggplot(aes(x = Year)) + geom_bar() +
  scale_fill_manual(values = c("darkred", "darkorange3", "darkslategrey")) +
  scale_y_continuous(labels = comma) +
  ylab("Accident/Incident Count")
plotly::ggplotly()

# Total Crashes by Year (State)
accHistStateSev %>%
  ggplot(aes(x = Year, y = Count, fill = Severity)) + geom_col() +
  scale_fill_manual(values = c("darkred", "darkorange3", "darkslategrey")) +
  scale_y_continuous(labels = comma) +
  ylab("Accident/Incident Count") +
  theme(legend.position = "top")
ggsave(filename = "Output/State Crashes by Severity.jpg", width = 10, height = 6, device = "jpeg", dpi = 300, units = "in")
plotly::ggplotly()


# Total Crashes by Year (County)
accHistStateCtySev %>%
  ggplot(aes(x = Year, y = Count, fill = Severity)) + geom_col() +
  scale_fill_manual(values = c("darkred", "darkorange3", "darkslategrey")) +
  scale_y_continuous(labels = comma) +
  ylab("Accident/Incident Count") +
  theme(legend.position = "top")
ggsave(filename = "Output/State_County Crashes by Severity.jpg", width = 10, height = 6, device = "jpeg", dpi = 300, units = "in")
plotly::ggplotly()

# Filter for at-grade, public, highway crossings
gcisHistFilter <-  gcisHist %>%
  filter(
    PosXing == 1, # Filter for at-grade crossings only
    #StateName == state,
    TypeXing == 3, # Public Crossings
    XPurpose == 1, # Highway Crossings
  ) %>%
  select(
    CrossingID,
    ReasonID,
    RevisionDate,
    ReportYear,
    Gates,
    FlashPai,
    GateConf,
    StopStd,
    XBuck,
    StateName,
    CountyName
  ) %>%
  mutate(
    revDate = ymd(as.Date(RevisionDate, "%m/%d/%Y %H:%M:%S %p"))
  ) %>%
  arrange(
    CrossingID, revDate
  ) %>%
  mutate(
    revDateNext = case_when(
      is.na(lead(CrossingID)) ~ ymd("2020-01-01"),# Handle case where last row returns NA
      CrossingID != lead(CrossingID) ~ ymd("2021-01-01"), # For last crossing record, set future date
      CrossingID == lead(CrossingID) ~ lead(revDate) # Return the date of the next record change
    )
  )


results <- tibble(
  yearEnd = seq.Date(as.Date("1980-12-31"), as.Date("2020-12-31"), "years"),
  year = year(yearEnd),
  XingTot = 0,
  XingGate = 0,
  XingFlshOnly = 0,
  XingStopOnly = 0,
  XingXBuckOnly = 0
  )


# Loop to query table for each year
for(val in 1:nrow(results)) {
  
  # Tally all Xings
  results$XingTot[val] = sum(
    gcisHistFilter$revDate <= results$yearEnd[val]
    & gcisHistFilter$revDateNext > results$yearEnd[val]
    & gcisHistFilter$ReasonID != 16,
    na.rm = TRUE)
  
  # Tally all Gated Xings
  results$XingGate[val] = sum(
    gcisHistFilter$revDate <= results$yearEnd[val]
    & gcisHistFilter$revDateNext > results$yearEnd[val]
    & gcisHistFilter$ReasonID != 16
      & gcisHistFilter$Gates > 0,
    na.rm = TRUE)
  
  # Tally all Flashing Light Only Xings
  results$XingFlshOnly[val] = sum(
    gcisHistFilter$revDate <= results$yearEnd[val]
    & gcisHistFilter$revDateNext > results$yearEnd[val]
    & gcisHistFilter$ReasonID != 16
      & gcisHistFilter$Gates == 0
      & gcisHistFilter$FlashPai > 0,
    na.rm = TRUE)
  
  # Tally all Stop Sign Only Xings
  results$XingStopOnly[val] = sum(
    gcisHistFilter$revDate <= results$yearEnd[val]
    & gcisHistFilter$revDateNext > results$yearEnd[val]
    & gcisHistFilter$ReasonID != 16
      & gcisHistFilter$Gates == 0
      & gcisHistFilter$FlashPai == 0
      & gcisHistFilter$StopStd > 0,
    na.rm = TRUE)
  
  # Tally all Xbuck Only Xings
  results$XingXBuckOnly[val] = sum(
    gcisHistFilter$revDate <= results$yearEnd[val]
    & gcisHistFilter$revDateNext > results$yearEnd[val]
    & gcisHistFilter$ReasonID != 16
      & gcisHistFilter$Gates == 0
      & gcisHistFilter$FlashPai == 0
      & gcisHistFilter$StopStd == 0
      & gcisHistFilter$XBuck > 0,
    na.rm = TRUE)
}


resultsState <- tibble(
  yearEnd = seq.Date(as.Date("1980-12-31"), as.Date("2020-12-31"), "years"),
  year = year(yearEnd),
  XingTot = 0,
  XingGate = 0,
  XingFlshOnly = 0,
  XingStopOnly = 0,
  XingXBuckOnly = 0
)

# Loop to query table for each year (by state)
gcisHistFilterState <- gcisHistFilter %>% filter(StateName == state)

for(val in 1:nrow(results)) {
  
  # Tally all Xings
  resultsState$XingTot[val] = sum(
    gcisHistFilterState$revDate <= resultsState$yearEnd[val]
    & gcisHistFilterState$revDateNext > resultsState$yearEnd[val]
    & gcisHistFilterState$ReasonID != 16,
    na.rm = TRUE)
  
  # Tally all Gated Xings
  resultsState$XingGate[val] = sum(
    gcisHistFilterState$revDate <= resultsState$yearEnd[val]
    & gcisHistFilterState$revDateNext > resultsState$yearEnd[val]
    & gcisHistFilterState$ReasonID != 16
    & gcisHistFilterState$Gates > 0,
    na.rm = TRUE)
  
  # Tally all Flashing Light Only Xings
  resultsState$XingFlshOnly[val] = sum(
    gcisHistFilterState$revDate <= resultsState$yearEnd[val]
    & gcisHistFilterState$revDateNext > resultsState$yearEnd[val]
    & gcisHistFilterState$ReasonID != 16
    & gcisHistFilterState$Gates == 0
    & gcisHistFilterState$FlashPai > 0,
    na.rm = TRUE)
  
  # Tally all Stop Sign Only Xings
  resultsState$XingStopOnly[val] = sum(
    gcisHistFilterState$revDate <= resultsState$yearEnd[val]
    & gcisHistFilterState$revDateNext > resultsState$yearEnd[val]
    & gcisHistFilterState$ReasonID != 16
    & gcisHistFilterState$Gates == 0
    & gcisHistFilterState$FlashPai == 0
    & gcisHistFilterState$StopStd > 0,
    na.rm = TRUE)
  
  # Tally all Xbuck Only Xings
  resultsState$XingXBuckOnly[val] = sum(
    gcisHistFilterState$revDate <= resultsState$yearEnd[val]
    & gcisHistFilterState$revDateNext > resultsState$yearEnd[val]
    & gcisHistFilterState$ReasonID != 16
    & gcisHistFilterState$Gates == 0
    & gcisHistFilterState$FlashPai == 0
    & gcisHistFilterState$StopStd == 0
    & gcisHistFilterState$XBuck > 0,
    na.rm = TRUE)
}

# By State and By County
resultsStateCty <- tibble(
  yearEnd = seq.Date(as.Date("1980-12-31"), as.Date("2020-12-31"), "years"),
  year = year(yearEnd),
  XingTot = 0,
  XingGate = 0,
  XingFlshOnly = 0,
  XingStopOnly = 0,
  XingXBuckOnly = 0
)

# Loop to query table for each year (by state)
gcisHistFilterStateCty <- gcisHistFilter %>% filter(StateName == state, str_detect(CountyName, "LOUIS"))

for(val in 1:nrow(results)) {
  
  # Tally all Xings
  resultsStateCty$XingTot[val] = sum(
    gcisHistFilterStateCty$revDate <= resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$revDateNext > resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$ReasonID != 16,
    na.rm = TRUE)
  
  # Tally all Gated Xings
  resultsStateCty$XingGate[val] = sum(
    gcisHistFilterStateCty$revDate <= resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$revDateNext > resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$ReasonID != 16
    & gcisHistFilterStateCty$Gates > 0,
    na.rm = TRUE)
  
  # Tally all Flashing Light Only Xings
  resultsStateCty$XingFlshOnly[val] = sum(
    gcisHistFilterStateCty$revDate <= resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$revDateNext > resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$ReasonID != 16
    & gcisHistFilterStateCty$Gates == 0
    & gcisHistFilterStateCty$FlashPai > 0,
    na.rm = TRUE)
  
  # Tally all Stop Sign Only Xings
  resultsStateCty$XingStopOnly[val] = sum(
    gcisHistFilterStateCty$revDate <= resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$revDateNext > resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$ReasonID != 16
    & gcisHistFilterStateCty$Gates == 0
    & gcisHistFilterStateCty$FlashPai == 0
    & gcisHistFilterStateCty$StopStd > 0,
    na.rm = TRUE)
  
  # Tally all Xbuck Only Xings
  resultsStateCty$XingXBuckOnly[val] = sum(
    gcisHistFilterStateCty$revDate <= resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$revDateNext > resultsStateCty$yearEnd[val]
    & gcisHistFilterStateCty$ReasonID != 16
    & gcisHistFilterStateCty$Gates == 0
    & gcisHistFilterStateCty$FlashPai == 0
    & gcisHistFilterStateCty$StopStd == 0
    & gcisHistFilterStateCty$XBuck > 0,
    na.rm = TRUE)
}



results2 <- 
  results %>% select(Year = year, All = XingTot, Gated = XingGate) %>% 
  pivot_longer(cols = c("All", "Gated"), names_to = "Type", values_to = "Count")

resultsState2 <- 
  resultsState %>% select(Year = year, All = XingTot, Gated = XingGate) %>% 
  pivot_longer(cols = c("All", "Gated"), names_to = "Type", values_to = "Count")

resultsStateCty <- 
  resultsStateCty %>% select(Year = year, All = XingTot, Gated = XingGate) %>% 
  pivot_longer(cols = c("All", "Gated"), names_to = "Type", values_to = "Count")

coeff = 0.05

# National Data
ggplot() + 
  geom_col(data = results2, aes(x = Year, y = Count, fill = Type), position = "identity") +
  geom_line(data = accHistAnnTot, aes(x = Year, y = Count/coeff), color = "red", size = 1.5) +
  scale_fill_manual(values = c(color2, color1)) +
  scale_y_continuous(
    name = "Crossings", label = comma,
    sec.axis = sec_axis(~.*coeff, name = "Accidents/Incidents", label = comma)
  ) +
  theme(
    axis.text.y = element_text(color = color1),
    axis.title.y = element_text(color = color1),
    axis.text.y.right = element_text(color = "red"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "top"
  )
ggsave(filename = "Output/Crashes vs Crossings.jpg", 
       width = 10, height = 6, device = "jpeg", dpi = 300, units = "in")
#plotly::ggplotly()



# Filtered by State
coeff = 0.05
ggplot() + 
  geom_col(data = resultsState2, aes(x = Year, y = Count, fill = Type), position = "identity") +
  geom_line(data = accHistState, aes(x = Year, y = Count/coeff), color = "red", size = 1.5) +
  scale_fill_manual(values = c(color2, color1)) +
  scale_y_continuous(
    name = "Crossings", label = comma,
    sec.axis = sec_axis(~.*coeff, name = "Accidents/Incidents", label = comma)
  ) +
  theme(
    axis.text.y = element_text(color = color1),
    axis.title.y = element_text(color = color1),
    axis.text.y.right = element_text(color = "red"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "top"
  )
ggsave(filename = "Output/Crashes vs Crossings - State.jpg", 
       width = 10, height = 6, device = "jpeg", dpi = 300, units = "in")
#plotly::ggplotly()

# Filtered by State, County
coeff = 0.05
ggplot() + 
  geom_col(data = resultsStateCty, aes(x = Year, y = Count, fill = Type), position = "identity") +
  geom_line(data = accHistStateCty, aes(x = Year, y = Count/coeff), color = "red", size = 1.5) +
    geom_smooth(data = accHistStateCty, aes(x = Year, y = Count/coeff), method = "loess", se = FALSE, color = "deeppink", lty = 2, size = 1) +
  scale_fill_manual(values = c(color2, color1)) +
  scale_y_continuous(
    name = "Crossings", label = comma,
    sec.axis = sec_axis(~.*coeff, name = "Accidents/Incidents", label = comma)
  ) +
  theme(
    axis.text.y = element_text(color = color1),
    axis.title.y = element_text(color = color1),
    axis.text.y.right = element_text(color = "red"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "top"
  )
ggsave(filename = "Output/Crashes vs Crossings - State - SLC.jpg", 
       width = 10, height = 6, device = "jpeg", dpi = 300, units = "in")
plotly::ggplotly()


# Crash Type Summary ------------------------------------------------------

LU_Tables <- read_csv("OtherData/LU_Tables5.csv")
LU_Tables_Vars <- LU_Tables$Field %>% unique()

variable = "POSITION"
varName = "Position"
horizontal = FALSE
facet = TRUE

crashComparisonChart <- function(variable, horizontal = FALSE, varName, facet = FALSE) {
  data <- accHist %>%
    mutate(
      StateGroup = ifelse(STATE == stateFIPS, state, "Other States"),
      WarnDev = ifelse(str_detect(CROSSING, c("01|02|03")), "Active", "Passive"),
      Signals = case_when(
        str_detect(CROSSING, "01") ~ "1: Gates and Lights",
        str_detect(CROSSING, "02") ~ "2: Cantilever Flashing Lights Only",
        str_detect(CROSSING, "03") ~ "3: Mast-Mounted Flashing Lights Only",
        str_detect(CROSSING, "08") ~ "4: Stop Signs",
        str_detect(CROSSING, "07") ~ "5: Crossbucks",
        TRUE ~ "6: Other")
      ) %>%
    filter(
      !is.na(StateGroup), 
      IYR %in% 16:20,
      PUBLIC == "Y"
      ) %>% 
    {if(variable %in% LU_Tables_Vars) {.} %>% rename("join" = variable) %>%
        left_join(LU_Tables %>% filter(Field == variable), by = c("join" = "ID")) else {.} %>% 
        mutate(FullName = !!sym(variable))} %>%
    {if(facet) {.} %>% 
        group_by(FullName, StateGroup, WarnDev) %>% 
        summarize(FullName = first(FullName), StateGroup = first(StateGroup), WarnDev = first(WarnDev), count = n()) %>% 
        ungroup() %>% group_by(StateGroup, WarnDev) %>% mutate(proportion = count / sum(count)) else {.} %>% 
        group_by(FullName, StateGroup) %>% 
        summarize(FullName = first(FullName), StateGroup = first(StateGroup), count = n()) %>% 
        ungroup() %>% group_by(StateGroup) %>% mutate(proportion = count / sum(count))}
    
  data %>%
    ggplot(aes(x = FullName, y = proportion, fill = StateGroup)) + 
    geom_col(position = "dodge") +
    {if(horizontal) coord_flip()} +
    {if(facet) facet_wrap(~WarnDev)} +
    scale_fill_manual(values = c(color1, color2), name = "State") +
    xlab(varName) + ylab("Crash Proportion") + theme(legend.position = "top") 
    
  
  ggsave(filename = paste0("output/", variable, "-", facet, ".jpg"), units = "in", width = 8, height = 5, dpi = 300)
}

crashComparisonChart(variable = "MONTH", varName = "Month")
crashComparisonChart(variable = "MONTH", varName = "Month", facet = TRUE)
crashComparisonChart(variable = "TYPVEH", varName = "Vehicle Type", horizontal = TRUE)
crashComparisonChart(variable = "TYPVEH", varName = "Vehicle Type", facet = TRUE, horizontal = TRUE)

temp <- accHist %>% filter(IYR %in% 16:20, PUBLIC == "Y", TYPVEH == "J", STATE == stateFIPS) %>%
  mutate(narr = paste0(NARR1, NARR2, NARR3, NARR4, NARR5)) %>% select(narr)

crashComparisonChart(variable = "POSITION", varName = "Vehicle Position")
crashComparisonChart(variable = "POSITION", varName = "Vehicle Position", facet = TRUE)
crashComparisonChart(variable = "TYPACC", varName = "Type of Accident")
crashComparisonChart(variable = "TYPACC", varName = "Type of Accident", facet = TRUE)
crashComparisonChart(variable = "VISIBLTY", varName = "Visibility")
crashComparisonChart(variable = "VISIBLTY", varName = "Visibility", facet = TRUE, horizontal = TRUE)
crashComparisonChart(variable = "WEATHER", varName = "Weather")
crashComparisonChart(variable = "WEATHER", varName = "Weather", facet = TRUE, horizontal = TRUE)
crashComparisonChart(variable = "TRAIN2", varName = "Second Train")
crashComparisonChart(variable = "TYPTRK", varName = "Type of Track")
crashComparisonChart(variable = "MOTORIST", varName = "Motorist Action") 
crashComparisonChart(variable = "MOTORIST", varName = "Motorist Action", facet = TRUE, horizontal = TRUE) 
crashComparisonChart(variable = "VIEW", varName = "Primary Obstruction")
crashComparisonChart(variable = "Signals", varName = "Warning Device", horizontal = TRUE)


NatInventoryRaw <- 
  read_csv("CurrentInventory/PublishedCrossingData-11-30-2021.csv", col_types = cols(.default = "c")) %>%
  filter(
    PosXing == 1, # Filter for at-grade crossings only
    #StateName == state,
    TypeXing == 3, # Public Crossings
    #XPurpose == 1, # Highway Crossings
    ReasonID != 16 # Reason ID 16 (closure) is generally more reliable than CrossingClosed
    #CrossingClosed == "No"
  ) %>%
  mutate(
    AADT = as.numeric(Aadt),
    TrainADT = as.numeric(DayThru) + as.numeric(NghtThru),
    ExpInd = ifelse(is.na(AADT), 1, AADT) * ifelse(is.na(TrainADT), 1, TrainADT),
    WarnDev = case_when(
      as.numeric(Gates) > 0 ~ "1: Gates and Lights",
      as.numeric(FlashPai) > 0 ~ "2: Flashing Lights Only",
      as.numeric(StopStd) > 0 ~ "3: Stop Signs",
      as.numeric(XBuck) > 0 ~ "4: Crossbucks",
      TRUE ~ "5: Other"),
    StateGroup = ifelse(StateName == state, state, "Other States")
  ) %>%
  select(
    CrossingID,
    ReasonID,
    RevisionDate,
    ReportYear,
    Gates,
    FlashPai,
    GateConf,
    StopStd,
    XBuck,
    WarnDev,
    StateName, 
    StateGroup,
    AADT,
    TrainADT,
    ExpInd
    ) 

NatInventory <- 
  NatInventoryRaw  %>%
  group_by(WarnDev, StateGroup) %>% 
  summarize(WarnDev = first(WarnDev), StateGroup = first(StateGroup), count = n()) %>% 
  ungroup() %>% group_by(StateGroup) %>% mutate(proportion = count / sum(count))


NatInventory %>%
  ggplot(aes(x = WarnDev, y = proportion, fill = StateGroup)) + 
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(color1, color2), name = "State") +
  xlab("Warning Device") + ylab("Crossing Proportion") + theme(legend.position = "top") +
  coord_flip()
ggsave(filename = "output/WarnDevCountByState.jpg", units = "in", width = 8, height = 5, dpi = 300)

accHist2 %>% filter(GXID == "025617C", IYR %in% 16:20)


# National Comparisons ----------------------------------------------------

states <- states(cb = TRUE)


# Accidents by State: 80s
#AccByState <- 
  accHist %>% 
  filter(
    PUBLIC == "Y",
    IYR %in% 80:89
    ) %>%
  left_join(states %>% st_drop_geometry() %>% select(STATEFP, STUSPS), by = c("STATE" = "STATEFP")) %>%
  group_by(State = STUSPS) %>% 
  summarize(Crashes = n()) %>% 
  arrange(desc(Crashes)) %>% 
    mutate(rank = row_number()) %>% filter(State == state) %>% pull(rank)

# Accidents by State: 90s
#AccByState <- 
  accHist %>% 
  filter(
    PUBLIC == "Y",
    IYR %in% 90:99
  ) %>%
  left_join(states %>% st_drop_geometry() %>% select(STATEFP, STUSPS), by = c("STATE" = "STATEFP")) %>%
  group_by(State = STUSPS) %>% 
  summarize(Crashes = n()) %>% 
  arrange(desc(Crashes)) %>% 
    mutate(rank = row_number()) %>% filter(State == state) %>% pull(rank)

# Accidents by State: 00s
#AccByState <- 
  accHist %>% 
  filter(
    PUBLIC == "Y",
    IYR %in% c("00", 01:09)
  ) %>%
  left_join(states %>% st_drop_geometry() %>% select(STATEFP, STUSPS), by = c("STATE" = "STATEFP")) %>%
  group_by(State = STUSPS) %>% 
  summarize(Crashes = n()) %>% 
  arrange(desc(Crashes)) %>% 
    mutate(rank = row_number()) %>% filter(State == state) %>% pull(rank)

# Accidents by State: 10s
#AccByState <- 
  accHist %>% 
  filter(
    PUBLIC == "Y",
    IYR %in% 10:19
  ) %>%
  left_join(states %>% st_drop_geometry() %>% select(STATEFP, STUSPS), by = c("STATE" = "STATEFP")) %>%
  group_by(State = STUSPS) %>% 
  summarize(Crashes = n()) %>% 
  arrange(desc(Crashes)) %>% 
    mutate(rank = row_number()) %>% filter(State == state) %>% pull(rank)

  
# Crashes per state (public crossings)

acc5yr <- 
  accHist2 %>%
  filter(Year %in% 2016:2020) %>%
  group_by(CrossingID = GXID) %>% 
  summarize(acc5yr = n())

PerState <- 
  NatInventoryRaw %>% left_join(acc5yr) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  group_by(StateName) %>% 
  summarize(
    crashCount = sum(acc5yr),
    crossingCount = n(),
    expIndSum = sum(ExpInd)
    ) %>%
  mutate(
    crashPerCrossing = crashCount / crossingCount,
    crashPerExpInd = crashCount / expIndSum * 1000000
  )

# Crashes by State
PerState %>%
  ggplot(aes(
    x = fct_reorder(StateName, desc(crashCount)),
    y = crashCount,
    fill = factor(ifelse(StateName == state, state, "Other States"))
    )) +
  geom_col() +
  scale_fill_manual(values = c(color1, color2)) +
  theme(legend.position = "none") + xlab(element_blank()) + ylab("Crashes (2016-2020)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
ggsave(filename = "Output/StateComparison - Crashes.jpg", width = 8, height = 3, device = "jpeg", dpi = 300, units = "in")

# Crashes per Crossing by State
PerState %>%
  ggplot(aes(
    x = fct_reorder(StateName, desc(crashPerCrossing)),
    y = crashPerCrossing,
    fill = factor(ifelse(StateName == state, state, "Other States"))
  )) +
  geom_col() +
  scale_fill_manual(values = c(color1, color2)) +
  theme(legend.position = "none") + xlab(element_blank()) + ylab("Crashes per Crossing (2016-2020)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
ggsave(filename = "Output/StateComparison - Crashes per Crossing.jpg", width = 8, height = 3, device = "jpeg", dpi = 300, units = "in")

# Crashes per Exposure Index by State
PerState %>%
  ggplot(aes(
    x = fct_reorder(StateName, desc(crashPerExpInd)),
    y = crashPerExpInd,
    fill = factor(ifelse(StateName == state, state, "Other States"))
  )) +
  geom_col() +
  scale_fill_manual(values = c(color1, color2)) +
  theme(legend.position = "none") + xlab(element_blank()) + ylab("Crashes per Mil. Exp. (2016-2020)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
ggsave(filename = "Output/StateComparison - Crashes per ExpInd.jpg", width = 8, height = 3, device = "jpeg", dpi = 300, units = "in")



# Specific Evaluations ----------------------------------------------------

# Digging into the "other" category for vehicle type

typeOther <- 
  accHist %>% filter(STATE == stateFIPS, MOTORIST == 5) %>%
  mutate(across(NARR1:NARR5, ~replace_na(., ""))) %>% 
  transmute(narrCombo = paste0(NARR1, NARR2, NARR3, NARR4, NARR5)) %>%
  mutate(
    stuckStop = if_else(str_detect(narrCombo, "STOPPED | STUCK"), 1, 0),
    droveAround = if_else(str_detect(narrCombo, "AROUND"), 1, 0),
    snowmobile = if_else(str_detect(narrCombo, "SNOWMOBILE"), 1, 0),
    farm = if_else(str_detect(narrCombo, "FARM | TRACTOR"), 1, 0),
    unoccupied = if_else(str_detect(narrCombo, "UNOCCUPIED"), 1, 0),
    snowSlid = if_else(str_detect(narrCombo, "SLID | SNOW"), 1, 0)
  ) %>% 
  summarize(across(stuckStop:snowSlid, ~sum(.)))




# Tally of crossings with gates by State

GateCrossings <- 
  NatInventoryRaw %>%
  mutate(WarnDevSimp = if_else(WarnDev == "1: Gates and Lights", "Gates", "No Gates")) %>%
  group_by(StateName, WarnDevSimp) %>%
  summarize(crossings = n()) %>%
  pivot_wider(names_from = WarnDevSimp, values_from = crossings) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(percent = Gates / (Gates + `No Gates`))

GateCrossings %>%
  ggplot(aes(
    x = fct_reorder(StateName, desc(percent)),
    y = percent,
    fill = factor(ifelse(StateName == state, state, "Other States"))
    )) +
  geom_col() +
  scale_fill_manual(values = c(color1, color2)) +
  theme(legend.position = "none") + xlab(element_blank()) + ylab("Crossings with Gates")
ggsave(filename = "Output/StateComparison - Crossings with Gates.jpg", width = 15, height = 5, device = "jpeg", dpi = 300, units = "in")




# Pedestrian Focus --------------------------------------------------------

PedData <- 
  accHist2 %>%
  mutate(
    StateGroup = ifelse(STATE == stateFIPS, state, "Other States"),
    WarnDev = ifelse(str_detect(CROSSING, c("01|02|03")), "Active", "Passive"),
    Signals = case_when(
      str_detect(CROSSING, "01") ~ "1: Gates and Lights",
      str_detect(CROSSING, "02") ~ "2: Cantilever Flashing Lights Only",
      str_detect(CROSSING, "03") ~ "3: Mast-Mounted Flashing Lights Only",
      str_detect(CROSSING, "08") ~ "4: Stop Signs",
      str_detect(CROSSING, "07") ~ "5: Crossbucks",
      TRUE ~ "6: Other")
  ) %>%
  left_join(LU_Tables %>% filter(Field == "TYPVEH"), by = c("TYPVEH" = "ID")) %>%
  filter(
    FullName == "Pedestrian"#,
    #STATE == stateFIPS
    ) 

PedData %>% 
  group_by(Year, Severity) %>% summarize(count = n()) %>%
  ggplot(aes(x = Year, y = count, fill = Severity)) + geom_col() +
  scale_fill_manual(values = c("darkred", "darkorange3", "darkslategrey")) +
  scale_y_continuous(labels = comma) +
  ylab("Pedestrian Accidents/Incidents") +
  theme(legend.position = "top")
ggsave(filename = "output/National Pedestrian Crashes by Severity.jpg", width = 10, height = 6, device = "jpeg", dpi = 300, units = "in")

PedData %>% 
  filter(STATE == stateFIPS) %>%
  group_by(Year, Severity) %>% summarize(count = n()) %>%
  ggplot(aes(x = Year, y = count, fill = Severity)) + geom_col() +
  scale_fill_manual(values = c("darkred", "darkorange3", "darkslategrey")) +
  scale_y_continuous(labels = comma) +
  ylab("Pedestrian Accidents/Incidents") +
  theme(legend.position = "top")
ggsave(filename = "output/State Pedestrian Crashes by Severity.jpg", width = 10, height = 6, device = "jpeg", dpi = 300, units = "in")

PedData %>% filter(STATE == stateFIPS, Year %in% 2011:2020) %>% summarize(count = n()) / 10

PedData %>% group_by(Severity) %>% summarize(count = n())

PedDataSF <- PedData %>% right_join(NatInventoryRaw, by = c("GXID" = "CrossingID")) %>% st_as_sf()

tm_shape(PedDataSF) + tm_dots()
  


accHist2$TYPVEH %>% unique()

