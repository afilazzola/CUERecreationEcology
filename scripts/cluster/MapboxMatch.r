### Spatial exploration of Conservation Halton
library(raster)
library(rgdal)
# library(tidyverse)
library(sf)


setwd("~/projects/def-sapna/afila/CUERecEcology")

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


## find polygon intersection with sites
out <- st_intersection(sites, lands)

## Properties with data
studyAreas <- lands[lands$Name %in% out$Name,]

### Function to convert bounds to polygon
makePolygon <- function(x){
  bbox <- as.numeric(strsplit(x, split=",")[[1]]) ## extract boundaries
  e <- as(raster::extent(bbox[1],bbox[3],bbox[2],bbox[4]), "SpatialPolygons") ## convert to polygon
  proj4string(e) <- "+proj=longlat +datum=WGS84 +no_defs" ## assign CRS
  e <- st_as_sf(e) ## convert to SF data class

}

## Load iteration
arrayIter <- as.numeric(commandArgs(trailingOnly = TRUE))
startIter <- 1+(131933*arrayIter)
batchRows <- 131933

#### Connect Mapbox polygons
dataColumns <- c("agg_day_period","agg_time_period","month","geography",
                 "bounds","xlat","xlon","activity_index_total")
mapbox <- read.csv("data//Mapbox//mapboxJunJulyAug2020.csv",
                   header=F,
                   skip=startIter,
                   nrows=131933,
                   col.names=dataColumns)


outPoly <- data.frame()
for(i in 1:nrow(mapbox)){
  tempPoly <- makePolygon(mapbox[i,"bounds"])
  mapboxMatch <- st_intersection(studyAreas, tempPoly)
  if(nrow(mapboxMatch)==0){
    next
  } else{
    polyData <- mapbox[i,]
  }
  outPoly <- rbind(outPoly, polyData)
}

write.csv(outPoly, paste0("out//MapboxArray",arrayIter,".csv"), row.names=FALSE)