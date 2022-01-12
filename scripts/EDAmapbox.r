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
