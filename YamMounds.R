#####     YAMBOL COMPARISON WITH ELENOVO

install.packages('sf')
install.packages('leaflet')
install.packages("htmltab")

library('tidyverse')
library('sf')
library('leaflet')

### Load datasets from legacy verification (2010), literature(1980-2017),


# Mounds depicted in 1980s maps within Yambol
YamMapMnds <- read_sf("./data/Yam_LegacySubset2016.shp") # shapefile of all mounds in Yambol from 2010

YamMapMnds$utm35_E <- st_coordinates(YamMapMnds)[,1] #add coordinates back i the dataframe
YamMapMnds$utm35_N <- st_coordinates(YamMapMnds)[,2] #add coordinates back i the dataframe

st_crs(YamMapMnds)
YamMapMnds4326 <- st_transform(YamMapMnds, crs=4326)
YamMapMnds$Long <- st_coordinates(YamMapMnds4326)[,1] #add coordinates back i the dataframe
YamMapMnds$Lat <- st_coordinates(YamMapMnds4326)[,2] #add coordinates back i the dataframe

st_write(YamMapMnds,"/data/YambolMapMounds.shp", driver = "ESRI Shapefile") #FIX DOES NOT WORK



# A bit of local infrastructure and topography
Y_towns <- read_sf("D:/Users/MQ20149304/Desktop/TRAP_Oxbow/TopoData/Modern_settlements_Yambol_TRAP.shp")
Y_rivers <- read_sf("D:/Users/MQ20149304/Desktop/TRAP_Oxbow/TopoData/Rivers_Yambol_TRAP.shp")
Y_roads <- read_sf("D:/Users/MQ20149304/Desktop/TRAP_Oxbow/TopoData/Modern_roads_Yambol_TRAP.shp")

# Yambol surveys
ElhTeams <- read_sf("D:/Users/MQ20149304/Desktop/TRAP_Oxbow/YAM/YAM_units.shp")
plot(ElhTeams$geometry)

# Verified Mounds

### Golyam Manastir(2010) and Elenovo(2017)
GM10mnd <- read_csv("./data/YamLGV2010Mounds.csv")  # csv of mounds verified in the 2010 field

GM10mnd <- merge(GM10mnd, YamMapMnds, by.x="TRAPID", by.y="TRAPCode")
dim(GM10mnd)
GM10mnd4326 <- st_as_sf(GM10mnd, coords = c("Long", "Lat"), crs = 4326)
GM10mnd <- st_transform(GM10mnd4326, 32635)

EL17mnd <- read_csv("data/ElenovoMounds_cleaned.csv")
EL17mnd$Longitude <- as.numeric(EL17mnd$Longitude) # check that LatLong are numeric
which(is.na(EL17mnd$Longitude))

EL17mnd4326 <- st_as_sf(EL17mnd, coords = c("Longitude","Latitude"), crs = 4326)
EL17mnd <- st_transform(EL17mnd4326, 32635)
dim(EL17mnd)

####  PLOT

# Plot mounds and survey units 2010

plot(YamMapMnds$geometry, col = "orange", pch = 1, add = TRUE)
#plot(ElhTeams$geometry,add = TRUE, col ="brown")
plot(Y_rivers, add = TRUE, col = "blue")
plot(Y_roads, add =TRUE, col = "brown")
plot(Y_towns, add = TRUE, col = "salmon", alpha = 0) # ALPHA IS NOT MAKING IT TRANSPARENT
plot(GM10mnd, add = TRUE,pch = 2,  col = "purple")
plot(EL17mnd$geometry,pch = 2,  col = "hotpink")
# Mounds from all maps in SE Bulgaria

### FOR FURHTER WORK ONE MUST UPDATE THE 2010 LEGACY DB!!

# Excavated mounds in Yambol (based on AOR)

# Load Burial information (ie. Cultural dates)  from AOR from 1980-2017
YamLitChrono <- read_csv("D:/Users/MQ20149304/Documents/Professional/Projects/MQNS/Data/Literature/MndBurial.csv")
head(YamLitChrono)

# Load burial mound coordinates 
Yamnd <- read_csv("D:/Users/MQ20149304/Documents/Professional/Projects/MQNS/Data/Literature/LitMndCoordsnonas.csv")

## Filter mounds by Yambol region
head(Yamnd)
Yamnd <-  Yamnd %>% 
          filter(Region=='Yambol')
tally(Yamnd)

Yam_LitMounds <- merge(Yamnd, YamLitChrono, by.x='MoundID',by.y='Mound ID')
head(Yam_LitMounds[, c(1,5:6,37:38)])
tail(Yam_LitMounds)
dim(Yam_LitMounds)

# Drop NAs and create a spatial feature
#na.omit and drop_na are similar functions; rm.na is an argument for afunction

Y_ExcavAor <- st_as_sf(na.omit(Yam_LitMounds[, c(1,5:6,37:38)]), coords = c('Long','Lat'))
plot(Y_ExcavAor$geometry)

# Excavated Mounds from Izvestii
# TBD


################## PLAYING WITH ONLINE MAPS:::LEAFLET
library(tidyverse)
library(maptools)
library(leaflet)

leaflet() %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addCircleMarkers(data=YamMapMnds4326, radius = 1, group="Legacy") %>% 
  addCircleMarkers(data=Y_ExcavAor,group="LitMounds", radius = 2, 
             opacity = 0.5, color= "black", stroke = TRUE,
             fillOpacity = 0.5, weight=2, fillColor = "yellow",
             popup = paste0("MoundID: ", Y_ExcavAor$MoundID,
                            "<br> StartDate: ", Y_ExcavAor$StartDate,
                            "<br> EndDate: ", Y_ExcavAor$Enddate)) %>% 
addLayersControl(
  baseGroups = c("Topo","ESRI Aerial"),
  overlayGroups = c("LitMounds", "Legacy"),
  options = layersControlOptions(collapsed = T))

map

tally(drop_na(Yam_LitMounds[, c(1,5:6,37:38)])) # drop_na is same as na.omit
head(complete.cases(Yam_LitMounds[, c(1,5:6,37:38)])) # complete.cases good for specific columns
