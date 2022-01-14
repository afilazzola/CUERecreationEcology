### Convert JSON to dataframe

library(jsonlite)
library(tidyverse)
library(foreach)
library(leaflet)

## list JSON files
jsonFiles <- list.files("data//populartimes//JSON", full.names = T)


listOut <- foreach(i = 1:length(jsonFiles), .errorhandling="remove", .combine=rbind) %do% {

  ## Load JSON file
tableOut <- fromJSON(jsonFiles[i])

## Convert JSON file to data.frame
timesDF <- data.frame(tableOut$populartimes) ## put popular times in df
timeframe <- timesDF$data %>% do.call(rbind, .)  %>% t() %>% data.frame() ## transpose to day by hour
names(timeframe) <- timesDF$name
timeframe[,"hour"] <- 1:24

## Include CA site data
allData <- timeframe
allData[,"lat"] <- tableOut$coordinates$lat
allData[,"lon"] <- tableOut$coordinates$lng
allData[,"Site"] <- tableOut$name
allData[,"rating"] <- tableOut$rating
allData[,"rating_n"] <- tableOut$rating_n
allData
}

colours <- c("firebrick4","firebrick","firebrick3","firebrick1","coral2","chocolate1","darkorange","goldenrod2","goldenrod1","gold")


colours <- c("purple","firebrick","red","orange","gold","yellow")


visitTimes <- listOut %>% gather(Day, Popularity, 1:7) %>%  ## bring days into one column
  mutate(Busy=cut(Popularity, breaks=seq(-10,110,by=20), labels=rev(colours)), Busy = as.character(Busy))


ggplot(visitTimes %>% filter(hour == 1 & Day == "Monday"), aes(x=lat, y=lon, color=Popularity)) + geom_point(size=3)

weekDays <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

## Parallelize to make faster
library(foreach)
library(doParallel)
cl <- makeCluster(4, type="PSOCK")
clusterExport(cl, varlist=list("weekDays","visitTimes"),
              envir = environment())
registerDoParallel(cl)


foreach(i = 1:24, .packages=c("foreach","tidyverse","leaflet")) %do% {
  foreach(j = 1:7,.packages=c("foreach","tidyverse","leaflet")) %dopar%{

tempData <- visitTimes %>% filter(hour == i & Day == weekDays[j])
mapOut <- leaflet(tempData) %>% 
  addCircleMarkers(data = tempData, ~lon, ~lat, fillColor=tempData$Busy, fillOpacity=1, fill=T, color="black", opacity=1) %>%
  addTiles() %>% 
  addPopups(lng = -79.2, lat = 43.4,   paste0(weekDays[j]," H",i),  options = popupOptions(closeButton = FALSE))
mapOut
mapview::mapshot(mapOut, file = paste0("out//visualize//D",j,"H",i,".jpg"))
  }
}

stopCluster(cl)

## Combine into an animated GIF
library(magick)

listImages <- list.files("out//visualize//", full.names = T)
img_list <- lapply(listImages, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 10)

## save animated image
image_write(image = img_animated,
            path = "out//PopularCATimes.gif")


