### Spatial exploration of Conservation Halton
library(raster)
library(rgdal)
library(tidyverse)
library(sf)


## Load in biodiversity sites
# sites <- read.csv("data//biodiversityData//CHSiteAdjusted.csv") ## use corrected locations
# coordinates(sites) <- ~Lon+Lat
# proj4string(sites) <- "+proj=longlat +datum=WGS84 +no_defs"

sites <- readOGR(layer="CHSiteadjusted", dsn="data//biodiversityData")
sites <- st_as_sf(sites)

##  Load shapefile for lands
lands <- readOGR(layer="CHLands", dsn="data//CHProperties")
lands <- st_as_sf(lands)
trails <- readOGR(layer="Trails", dsn="data//CHProperties")
trails <- spTransform(trails, CRS="+proj=longlat +datum=WGS84 +no_defs") ## switch to lat lon
trails <- st_as_sf(trails)

## find polygon intersection with sites
out <- st_intersection(sites, lands)

## Properties with data
studyAreas <- lands[lands$Name %in% out$Name,]


#### Connect Mapbox polygons
# mapbox <- read.csv("data//Mapbox//mapboxJunJulyAug2020.csv")
# e <- as(raster::extent(78.46801, 78.83157, 19.53407, 19.74557), "SpatialPolygons")
# 
# outPoly <- data.frame()
# for(i in 1:nrow(mapbox)){
#   ### load mapbox polygon
#   bbox <- as.numeric(strsplit(mapbox[i,"bounds"], split=",")[[1]]) ## extract boundaries
#   e <- as(raster::extent(bbox[1],bbox[3],bbox[2],bbox[4]), "SpatialPolygons") ## convert to polygon
#   proj4string(e) <- "+proj=longlat +datum=WGS84 +no_defs" ## assign CRS
#   e <- st_as_sf(e) ## convert to SF data class
#   mapboxMatch <- st_intersection(studyAreas, e)
#   if(nrow(mapboxMatch)==0){
#     next
#   } else{
#   polyData <- mapbox[i,]
#   }
#   outPoly <- rbind(outPoly, polyData)
#   print(i)
# }


#### Used super computer  to find relevant polygons
polyFiles <- list.files("out//polygons", full.names = T) %>%  map_df(~read.csv(.))

### Function to convert bounds to polygon
makePolygonDF <- function(x){
  bbox <- as.numeric(strsplit(x, split=",")[[1]]) ## extract boundaries
  e <- as(raster::extent(bbox[1],bbox[3],bbox[2],bbox[4]), "SpatialPolygons") ## convert to polygon
  proj4string(e) <- "+proj=longlat +datum=WGS84 +no_defs" ## assign CRS
  e <- st_as_sf(e) ## convert to SF data class
  
}

## See overlap with conservation authority properties
polyMapbox <- lapply(polyFiles[,"bounds"], makePolygon)
allMapbox <- do.call(rbind, polyMapbox)
allMapbox <- cbind(allMapbox, polyFiles)
intersecMapboxCA <- st_intersection(allMapbox, lands)

## summary based on intersection
summaryMapbox <- intersecMapboxCA %>% 
                    group_by(Name) %>% 
                    summarize(activity = median(activity_index_total))
                    
plot(summaryMapbox)

Mapboxtiming <- intersecMapboxCA %>% 
  mutate(timeframe = ifelse(agg_time_period %in% 6:7, "peak-day",
                            ifelse(agg_time_period %in% c(4,8,9),"off-hours","closed"))) %>% 
  group_by(Name,timeframe) %>% 
  summarize(activity = median(activity_index_total))

######## Load in reservation data
reservations <- read.csv("data//ConservationHalton_ParkReservations.csv")

## select only parks with reservation data
StudyReservations <- reservations[reservations$Arrival.Location %in%  studyAreas$Name,]

## Split date column
StudyReservations <- StudyReservations %>% 
                        mutate(date = as.Date(Date)) %>% 
                        mutate(year = as.numeric(format(date, format = "%Y")),
                               month = as.numeric(format(date, format = "%m")),
                               day = as.numeric(format(date, format = "%d")))%>% 
                      mutate(time = as.POSIXct(Start.Time, format="%H:%M")) %>% 
                      mutate(hour =  as.numeric(format(time, "%H")))
             
### Summarize the reservation data for daily
parkAvgs <- StudyReservations %>% 
    filter(year == 2020) %>% 
    filter(month %in% 6:8) %>% 
    group_by(Arrival.Location) %>% 
    summarize(dailyAdults = sum(Adults+Seniors)/90,
              dailyYouths = sum(Children..5.14.+ Children..under.5.)/90)

### Summarize the reservation data for time periods
timing <- StudyReservations %>% 
  filter(year == 2020) %>% 
  filter(month %in% 6:8) %>% 
  mutate(timeframe = ifelse(hour %in% 12:16, "peak-day","off-hours")) %>% 
  group_by(Arrival.Location,timeframe) %>% 
  summarize(meanPeople = sum(People)/90) %>% 
  spread(timeframe, meanPeople)

parkPatterns <- left_join(parkAvgs, timing)

######## Load in trail patterns
trailsLands <- st_intersection(trails, studyAreas)

### Calculate length per CA area
trailSummary <- trailsLands %>% group_by(Name) %>% summarize(trailLength = sum(ShapeSTLen), 
                                             trailDens=trailLength/unique(SHAPE_Leng))


## Trail relationship with mapbox
joinData <- merge(data.frame(trailSummary), data.frame(summaryMapbox), by="Name", all=T)


ggplot(joinData, aes(x=trailLength, y=activity)) + geom_point() + ylim(0.015,0.045) + theme_classic() +
  geom_point(size=4) + xlim(-500,28000) + 
  geom_text(aes(label=Name),nudge_y = 0.002, nudge_x = 100) 
ggplot(joinData, aes(x=trailDens, y=activity)) + geom_point() + ylim(0.015,0.045) + theme_classic() +
  geom_point(size=4) + xlim(0,1.4) + 
  geom_text(aes(label=Name),nudge_y = 0.002, nudge_x = 0.05) 


## mapbox relationship with reservation data
parkPatternsLong <- parkPatterns %>% gather(timeframe, reservations, 2:5)

reservationMapbox <- Mapboxtiming %>% data.frame() %>% 
  select(Arrival.Location = Name, timeframe, activity) %>% 
  left_join(parkPatternsLong) %>% 
  left_join(studyAreas %>% select(Arrival.Location = Name, SHAPE_Area))


ggplot(reservationMapbox, aes(x=reservations/SHAPE_Area, y=activity, color=timeframe)) + 
  geom_point(size=4) + ylim(0.02,0.065) +
  geom_text(aes(label=Arrival.Location),nudge_y = 0.002, nudge_x = 0.2) +
 theme_classic()
