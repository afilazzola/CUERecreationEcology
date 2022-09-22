## EDA of Mapbox data


### libraries
library(raster)
library(rgdal)
library(tidyverse)
library(sf)

source("scripts//functions.r")


##  Load shapefile for lands
lands <- readOGR(layer="CHLands", dsn="data//CHProperties")
lands <- st_as_sf(lands)
trails <- readOGR(layer="Trails", dsn="data//CHProperties")
trails <- spTransform(trails, CRS="+proj=longlat +datum=WGS84 +no_defs") ## switch to lat lon
trails <- st_as_sf(trails)

#### Used super computer  to find relevant polygons (see MapboxMatch.r)
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

## export intersections for review
# st_write(intersecMapboxCA, dsn="data//Mapbox", layer="MapboxCH", driver="ESRI Shapefile")

## Extracting timing
Mapboxtiming <- intersecMapboxCA %>% 
  mutate(date=  as.Date(paste0(month,"-01"))) %>% 
  mutate(month = as.numeric(format(date, format = "%m"))) %>% 
  mutate(start_h2 =  agg_time_period*2) %>% 
  mutate(dayOfWeek = ifelse(agg_day_period == 0, "weekday", "weekend"))

### Drop pixels with higher overnight activity to account for road pollution and non-park activity
MapboxtimingParkCenter <- Mapboxtiming %>% 
  mutate(offHours = ifelse(start_h2 <= 4 | start_h2 == 22, "Y","N")) %>% 
  group_by(bounds, offHours) %>% 
  summarize(totalActivity = sum(activity_index_total)) %>% 
  filter(offHours == "Y")

## Adjust activity by area of pixel
MapboxtimingAdjusted <- Mapboxtiming %>%  
  mutate(area = as.vector(st_area(Mapboxtiming))) %>% 
  filter(!(Mapboxtiming$bounds %in% MapboxtimingParkCenter$bounds)) %>%   ## Drop non-park activity 
  mutate(activityIndex = round(activity_index_total, 3)) %>% 
  mutate(areaAdjActivity = activityIndex * area)

## Amount of activity removed
MapboxtimingRemoved <- Mapboxtiming %>% 
  mutate(OvernightFilter = ifelse(Mapboxtiming$bounds %in% MapboxtimingParkCenter$bounds, "Removed","Kept"))

ggplot(MapboxtimingRemoved, aes(x = reorder(Name, activity_index_total), y = activity_index_total, fill = OvernightFilter)) + 
  geom_boxplot() + coord_flip() + xlab("") + ylab("Mapbox Activity Index")

# st_write(MapboxtimingAdjusted, dsn="data//Mapbox", layer="MapboxAdjustedActivity", driver="ESRI Shapefile", append = F)


## Summarize data by Park
MapboxSummary <- MapboxtimingAdjusted %>% data.frame() %>% 
                  group_by(Name, month, start_h2, dayOfWeek) %>% 
                  summarize(totalActivity = sum(areaAdjActivity)) 
# lowestActivity <- min(MapboxSummary$totalActivity[MapboxSummary$totalActivity!=0])
# MapboxSummary <- MapboxSummary %>% 
#                   mutate(logActivity = log(totalActivity+lowestActivity)) ## adjust for right skew

## Summary statistics
MapboxStatistics <- MapboxSummary %>% 
                      mutate(accessibility = ifelse(start_h2 < 8 | start_h2 > 17, "closed", "open")) %>% 
                      group_by(Name, dayOfWeek, accessibility) %>% 
                      summarize(avgLogActivity = sum(totalActivity), 
                      IQRLogActivity = IQR(totalActivity))

## Refugia - raster pixels with no activity
uniquePolys <- MapboxtimingAdjusted %>%  distinct(Name, geometry)
mobileArea <- uniquePolys %>% 
                mutate(polyArea = st_area(.)) %>% 
                group_by(Name) %>% 
                summarize(totalHumanArea = sum(polyArea)) %>% 
                data.frame() %>%  dplyr::select(-geometry)
LandsMobileArea <- lands %>%  left_join(mobileArea) %>% 
                    mutate(HumanMobilePercent = (totalHumanArea/SHAPE_Area)*100)



#### Trail overlap with mapbox activity for CH lands

## First need to combine trails
trailsCombined <- st_combine(trails)

trailMapbox <- st_intersection(MapboxtimingAdjusted, trailsCombined)


## Amount of total activity along trails
trailData <- trailMapbox %>% 
              group_by(Name) %>%
              summarize(totalTrailActivity = sum(areaAdjActivity)) %>% 
                        data.frame()

## Get total trail activity to determine difference
rawTotalActivity <- MapboxtimingAdjusted %>% 
                        group_by(Name) %>% 
                        summarize(totalActivity = sum(areaAdjActivity)) %>% 
                        data.frame() 

## Join and determine difference to get off trail data
trailPatterns <- rawTotalActivity %>% 
  dplyr::select(-geometry) %>% 
  left_join(trailData) %>% 
  mutate(TrailActivityPercent= totalTrailActivity / totalActivity*100)



### Join all data and save output for exploration
trailPatternsSimplified <- trailPatterns %>% 
  dplyr::select(Name, TrailActivityPercent)
LandsMobileAreaSimplified <- LandsMobileArea %>% 
  data.frame() %>% 
  dplyr::select(-geometry, -OBJECTID, -totalHumanArea ) 
MapboxDataOut <- MapboxStatistics %>% 
        left_join(LandsMobileAreaSimplified) %>% 
        left_join(trailPatternsSimplified) %>% 
        mutate(PropertyAreakm2 = SHAPE_Area) %>% 
        mutate(activityDensityLog = avgLogActivity / PropertyAreakm2) %>% 
        data.frame()

write.csv(MapboxDataOut, "out//data//MapboxSummaryData.csv", row.names=F)




