## EDA of Mapbox data


### libraries
library(raster)
library(rgdal)
library(tidyverse)
library(sf)


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


## Extracting timing
Mapboxtiming <- intersecMapboxCA %>% 
  mutate(date=  as.Date(paste0(month,"-01"))) %>% 
  mutate(month = as.numeric(format(date, format = "%m"))) %>% 
  mutate(start_h2 =  agg_time_period*2) %>% 
  mutate(dayOfWeek = ifelse(agg_day_period == 0, "weekday", "weekend"))

## Summarize data by Park
MapboxSummary <- Mapboxtiming %>% data.frame() %>% 
                  group_by(Name, month, start_h2, dayOfWeek) %>% 
                  summarize(totalActivity = sum(activity_index_total))

## Refugia - raster pixels with no activity
uniquePolys <- Mapboxtiming %>%  distinct(Name, geometry)
mobileArea <- uniquePolys %>% 
                mutate(polyArea = st_area(.)) %>% 
                group_by(Name) %>% 
                summarize(totalHumanArea = sum(polyArea)) %>% 
                data.frame() %>%  dplyr::select(-geometry)
LandsMobileArea <- lands %>%  left_join(mobileArea) %>% 
                    mutate(HumanMobilePercent = totalHumanArea/SHAPE_Area*100)

## export instersection for review
# st_write(intersecMapboxCA, dsn="data//Mapbox", layer="MapboxCH", driver="ESRI Shapefile")
