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
polyMapbox <- lapply(polyFiles[,"bounds"], makePolygonDF)
allMapbox <- do.call(rbind, polyMapbox)
allMapbox <- cbind(allMapbox, polyFiles)
intersecMapboxCA <- st_intersection(allMapbox, lands)

## export instersection for review
st_write(intersecMapboxCA, dsn="data//Mapbox", layer="MapboxCH", driver="ESRI Shapefile")

## summary based on intersection
summaryMapbox <- intersecMapboxCA %>% 
                    group_by(Name) %>% 
                    summarize(activity = sum(activity_index_total))
                    
plot(summaryMapbox)

Mapboxtiming <- intersecMapboxCA %>% 
  mutate(date=  as.Date(paste0(month,"-01"))) %>% 
  mutate(month = as.numeric(format(date, format = "%m"))) %>% 
  mutate(start_h2 =  agg_time_period*2) %>% 
  group_by(Name, month, start_h2) %>% 
  summarize(activity = sum(activity_index_total))


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
                      mutate(hour =  as.numeric(format(time, "%H"))) %>% 
                      mutate(h2window = cut(hour, breaks = c(8,10,12,14,16,18,20))) %>%  ## create two h windows to match Mapbox
                      mutate(start_h2 = as.numeric(h2window)*2+6)
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
parkPatternsLong <- StudyReservations %>% 
                      filter(month %in% 6:8) %>% 
                      group_by(Arrival.Location, start_h2, month) %>% 
                      summarize(nReserve=sum(People))
                        

reservationMapbox <- Mapboxtiming %>% data.frame() %>% 
  select(Arrival.Location = Name, month, start_h2, activity) %>% 
  left_join(parkPatternsLong) %>% 
  left_join(studyAreas %>% select(Arrival.Location = Name, SHAPE_Area)) %>% 
  filter(!is.na(nReserve))


ggplot(reservationMapbox, aes(x=nReserve, y=activity, color=Arrival.Location, shape=as.factor(month))) + 
  geom_point(size=4) + 
  geom_text(aes(label=as.character(start_h2)),nudge_y = 0.5, nudge_x = 100) +
 theme_classic()



######## Take a look at biodiversity data

birds <- read.csv("data//biodiversityData//CH_FBMP birds Feb 12 2021.csv", stringsAsFactors = F)

avgBird <- birds %>%
              group_by(Year, Site) %>% 
              filter(Year> 2009) %>% 
              mutate(Number = as.numeric(Number)) %>% 
              summarize(nRich = length(unique(Species.Name)), abd = sum(Number, na.rm=T), nInstances  = length(unique(Date))) %>% 
              mutate(adjAbd = abd/nInstances)

ggplot(avgBird %>%  filter(Site %in% Mapboxtiming$Name), aes(x=Year, y= nRich, color=Site)) + 
  geom_smooth(se=F)


