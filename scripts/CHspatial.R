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
polyFiles <- list.files("out//polygons", full.names = T) %>%  map_df(~read.csv(., stringsAsFactors = F))

### Function to convert bounds to polygon
makePolygonDF <- function(x){
  bbox <- as.numeric(strsplit(x, split=",")[[1]]) ## extract boundaries
  e <- as(raster::extent(bbox[1],bbox[3],bbox[2],bbox[4]), "SpatialPolygons") ## convert to polygon
  proj4string(e) <- "+proj=longlat +datum=WGS84 +no_defs" ## assign CRS
  e <- st_as_sf(e) ## convert to SF data class
  
}

## See overlap with conservation authority properties
polyMapbox <- lapply(polyFiles[,"bounds"], makePolygonDF)
allMapbox <- bind_rows(polyMapbox)
allMapbox <- cbind(allMapbox, polyFiles)
intersecMapboxCA <- st_intersection(allMapbox, lands)


## export instersection for review
# st_write(intersecMapboxCA, dsn="data//Mapbox", layer="MapboxCH", driver="ESRI Shapefile")

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
  summarize(activity = sum(activity_index_total), nPolys = length(activity_index_total))

## Save
mapboxTimingOut <- Mapboxtiming %>% data.frame() %>% dplyr::select(-geometry) 
write.csv(mapboxTimingOut, "out//data//MapboxTiming.csv", row.names=F)

MapboxDetails <-  studyAreas %>% select(Name, Area_Type, Managed, perimeter = SHAPE_Leng, area = SHAPE_Area) %>% 
      mutate(logArea = log(area)) %>% 
      st_join(Mapboxtiming)


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
                      mutate(time = as.POSIXct(Check.in.Time, format="%H:%M")) %>% 
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
                                             trailDens=trailLength/log(unique(SHAPE_Leng)),
                                             parkArea = unique(SHAPE_Area))


## Trail relationship with mapbox
joinData <- merge(data.frame(trailSummary), data.frame(summaryMapbox), by="Name", all=T)


plot1 <- ggplot(joinData, aes(x=trailLength, y=activity/log(parkArea), label=Name)) +theme_classic() +
  geom_point(size=4) + ylab("Mapbox Activity Density") + xlab("Sum Trail Length (m)") +
  geom_text(nudge_y = 0.5, nudge_x = 100) 
plot1

plot2 <- ggplot(joinData, aes(x=trailDens, y=activity/log(parkArea))) + geom_point() +  theme_classic() +
  geom_point(size=4) + 
  geom_text(aes(label=Name),nudge_y = 0.5, nudge_x = 0.05)  +
  xlab("Trail Density")+ ylab("Mapbox Activity Density") 
plot2

gridExtra::grid.arrange(plot1, plot2, ncol=2)

## mapbox relationship with reservation data
parkPatternsLong <- StudyReservations %>% 
                      filter(month %in% 6:8) %>% 
                      group_by(Arrival.Location, start_h2, month) %>% 
                      summarize(nReserve=sum(Adults+Seniors))
                        

reservationMapbox <- Mapboxtiming %>% data.frame() %>% 
  select(Arrival.Location = Name, month, start_h2, activity) %>% 
  left_join(parkPatternsLong) %>% 
  left_join(studyAreas %>% select(Arrival.Location = Name, SHAPE_Area)) %>% 
  filter(!is.na(nReserve))



ggplot(reservationMapbox, aes(x=nReserve, y=activity, color=as.factor(month),
                              label=as.character(start_h2))) + 
  geom_point(size=4) + 
  geom_text(nudge_y = 0.5) +
 theme_classic() + facet_wrap(~Arrival.Location, scales="free_x") +
  scale_colour_manual(values=RColorBrewer::brewer.pal(3, "Set2")) +
  ylab("Mapbox Activity") +  xlab("Number of Reservations")


######## Take a look at biodiversity data

birds <- read.csv("data//biodiversityData//CH_FBMP birds Feb 12 2021.csv", stringsAsFactors = F) %>% 
            select(Time:Comments)

avgBird <- birds %>%
              group_by(Year, Site) %>% 
              filter(Year> 2009) %>% 
              mutate(Number = as.numeric(Number)) %>% 
              summarize(nRich = length(unique(Species.Name)), abd = sum(Number, na.rm=T), nInstances  = length(unique(Date))) %>% 
              mutate(adjAbd = abd/nInstances)

ggplot(avgBird %>%  filter(Site %in% Mapboxtiming$Name), aes(x=Year, y= nRich, color=Site)) + 
  geom_smooth(se=F)

## save birds as spatial
birdGPS <- birds
coordinates(birdGPS) <- ~Easting+Northing
proj4string(birdGPS) <- "+proj=utm +zone=17 +datum=WGS84"
st_write(st_as_sf(birdGPS), dsn="data", layer="BirdLocations", driver="ESRI Shapefile")

avgActivity <- MapboxDetails %>% 
                rename(Name= Name.x) %>% 
                mutate(activityDens = activity/logArea) %>% 
                group_by(Name) %>% 
                summarize(dailyActivity = sum(activityDens)) %>% 
                data.frame()

### Average abundances
longtermBird <- avgBird %>% group_by(Name=Site) %>% 
              summarize(meanAbd = mean(abd), meanRich = mean(nRich), 
                        changeAbd = sum(diff(abd)), changeRich = sum(diff(nRich)) )


## Join with mapbox
birdMapbox <- summaryMapbox %>% left_join(longtermBird) %>% 
                gather(metric, value, 4:7)

ggplot(birdMapbox, aes(x=activity, y=value)) + geom_point()  + 
  facet_wrap(~metric, scales="free_y") + theme_classic() + geom_smooth(method="lm")
  
  
## check linear models
  birdMapbox %>%
    group_by(metric) %>%
    do(fit = broom::tidy(lm(value ~ activity,
                            data = .))) %>%
    unnest(fit)
  
  
