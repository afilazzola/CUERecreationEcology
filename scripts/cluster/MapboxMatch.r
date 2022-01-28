### Spatial exploration of Conservation Halton
library(raster)
library(rgdal)
# library(tidyverse)
library(sf)


setwd("~/projects/def-sapna/afila/CUERecEcology")

## List lands
lands <- readOGR(layer="CHLands", dsn="data//CHProperties")
lands <- st_as_sf(lands)


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
                   nrows=batchRows,
                   col.names=dataColumns)


outPoly <- data.frame()
for(i in 1:nrow(mapbox)){
  tempPoly <- makePolygon(mapbox[i,"bounds"])
  mapboxMatch <- st_intersection(lands, tempPoly)
  if(nrow(mapboxMatch)==0){
    next
  } else{
    polyData <- mapbox[i,]
  }
  outPoly <- rbind(outPoly, polyData)
}

write.csv(outPoly, paste0("out//MapboxArray",arrayIter,".csv"), row.names=FALSE)