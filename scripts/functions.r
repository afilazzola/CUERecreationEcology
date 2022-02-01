#### Functions


### Function to Mapbox convert bounds to polygon
makePolygon <- function(x){
  bbox <- as.numeric(strsplit(x, split=",")[[1]]) ## extract boundaries
  e <- as(raster::extent(bbox[1],bbox[3],bbox[2],bbox[4]), "SpatialPolygons") ## convert to polygon
  proj4string(e) <- "+proj=longlat +datum=WGS84 +no_defs" ## assign CRS
  e <- st_as_sf(e) ## convert to SF data class

}



##### Find SF match for mapbox file

spatial_Mapbox_Find <- function(inpath = ".", MatchSites, ## matching spatial file
  mapboxFilepath, outpath, ## mapbox output
  startRow = 1, lengthrow = 100000, arrayIter = 1) {## "CHLands"

require(raster)
require(rgdal)
require(sf)

## List lands
lands <- readOGR(layer=MatchSites, dsn=inpath)
lands <- st_as_sf(lands)


## Load iteration
arrayIter <- as.numeric(commandArgs(trailingOnly = TRUE))
startIter <- startRow+(lengthrow*arrayIter)
batchRows <- lengthrow

#### Connect Mapbox polygons
dataColumns <- c("agg_day_period","agg_time_period","month","geography",
                 "bounds","xlat","xlon","activity_index_total")
mapbox <- read.csv(mapboxFilepath,
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

write.csv(outPoly, paste0(outpath,"/MapboxArray",arrayIter,".csv"), row.names=FALSE)
}


###### GBIF lookup of species name
require(rgbif)
getTaxaInfo <- function(genusVector) {
## loop through all the genus
genusDF <- data.frame()
for(i in genusVector){
  tryCatch({
    gbifDF <- name_lookup(i, limit=20)
    tempDF <- data.frame(Genus=i, kingdom = gbifDF$data$kingdom[1], 
                         phylum=gbifDF$data$phylum[1], 
                         order=gbifDF$data$order[1],
                         class=gbifDF$data$class[1],
                         family=gbifDF$data$family[1])
    genusDF <- rbind(genusDF, tempDF)
    
  }, error = function(e) e)
  
  progressBar <- which(genusVector == i) / length(genusVector)*100
  print(progressBar)
}
return(genusDF)
}



## Save plot
save_PDF <- function(plotName = "rplot1.pdf", setWidth = 8, setHeight = 6) {
pdf(plotName,         # File name
    width = setWidth, height = setHeight, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    useDingbats = F)          # Paper size
print(last_plot())
dev.off()  ## close Graphics
}

find_Native_Status <- function(speciesVector, nameType = "scientific") {
nativeList <- read.csv("data//biodiversityData//PlantNativeStatus.csv")

## drop "sp." from genus only
GenusOrSpecies <- tolower(gsub(" sp.", "", speciesVector))
GenusOrSpecies <- gsub(".", " ", GenusOrSpecies, fixed=T)
GenusOrSpecies <- trimws(GenusOrSpecies)

## search table
if(nameType == "scientific"){
searchList = tolower(nativeList$SCIENTIFIC_NAME)
} else{
searchList = tolower(nativeList$ENGLISH_COMMON_NAME)
}

matchNames <- c()
for(i in GenusOrSpecies){
matchName <-  nativeList[match(i, searchList), "EXOTIC_NATIVE"]
matchNames <- c(matchNames, matchName)
}
return(matchNames)
}
