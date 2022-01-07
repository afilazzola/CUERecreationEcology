### Wrangle the CHBIS dataset
library(raster)
library(rgdal)
library(tidyverse)
library(sf)

##  Load shapefile for lands
lands <- readOGR(layer="CHLands", dsn="data//CHProperties")
lands <- st_as_sf(lands)

### Load dataset and transform into lat lon
occData <- read.csv("data//biodiversityData//UrbanetDLA.csv") %>% filter(!(is.na(Easting) | is.na(Northing)))
coordinates(occData) <- ~Easting+Northing
proj4string(occData) <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"
occData <- spTransform(occData, CRS="+proj=longlat +datum=WGS84 +no_defs") ## switch to lat lon
occData <- crop(occData, extent(lands))
occData <- st_as_sf(occData)

### find intersection with CA properties
occLands <- st_intersection(occData, lands)

## Summarize records per CA property
totalOccLands <- occLands %>% group_by(ScientificName, CommonName, Name, SHAPE_Area, SHAPE_Leng, Year) %>% 
                  summarize(nOcc = length(ScientificName)) 
totalOccLandsDF <- totalOccLands %>% data.frame() %>% select(-geometry)




# write.csv(totalOccLandsDF, "out//data//SpeciesOccProperties.csv", row.names=FALSE)

## Load records per CA property
totalOccDF <- read.csv("out//data//SpeciesOccProperties.csv") %>% filter(Year > 2014)

## Get genus data
totalOccDF <- totalOccDF %>% separate(. , ScientificName, into=c("Genus","Species")) ## separate genus into separate column
uniqueGenus <- unique(totalOccDF$Genus)
genusDetails <- getTaxaInfo(uniqueGenus) ## search GBIF for species taxonomy details (searched by Genus)
totalOccDFTaxa <- merge(totalOccDF, genusDetails)

## calculate relative proportion of occurrences
propOccDF <-  totalOccDFTaxa %>% group_by(Name, kingdom, Year) %>% 
                mutate(propOcc = nOcc/sum(nOcc)*100)   %>% 
                mutate(speciesAbrv = abbreviate(Species, 6))

## Summary at site level
propertyChar <- propOccDF %>%  group_by(Name, SHAPE_Area) %>% summarize(nSpp = length(unique(Species)))

## Join with mapbox data
Mapboxtiming <- read.csv("out//data//MapboxTiming.csv")
MapboxTimingDensity <-  propertyChar %>% dplyr::select(Name, SHAPE_Area) %>% right_join(Mapboxtiming)

## Calculate mapbox metrics
MapboxMetrics <- MapboxTimingDensity %>% 
                  mutate(areakm2 = SHAPE_Area/100000, activityDensity = activity/areakm2) %>% 
                  mutate(timeFrame = ifelse(start_h2 %in% 10:16, "peakhours","offhours")) %>% 
                  group_by(Name, month, timeFrame) %>% 
                  summarize(sumActivity = sum(activityDensity), meanActivity = mean(activityDensity))

## Convert plant dataset into community matrix
plants <- propOccDF %>% filter(kingdom == "Plantae") %>% 
            dplyr::select(Name, speciesAbrv,  propOcc) %>% 
            filter(!is.na(speciesAbrv)) %>% 
            group_by(Name,  speciesAbrv) %>% 
            summarize(totalProp = sum(propOcc)) %>% 
            spread(speciesAbrv, totalProp, fill=0)

## convert mapbox data into community matrix for predictors
MapboxWide <- MapboxMetrics %>%  mutate(monthTime = paste0("Month",month, ".",timeFrame)) %>% 
                ungroup() %>% 
                dplyr::select(Name, monthTime, sumActivity) %>% 
                spread(monthTime, sumActivity, fill=0)

plantsMapbox <- MapboxWide %>% left_join(plants)

## ordination
library(vegan)

## Transform data
respData <- dplyr::select(plantsMapbox, abrtvs:vulgtm)
respDataNArmved <- respData[!is.na(rowSums(respData)),]
respDataTrans <- respDataNArmved[,colSums(respDataNArmved)>0]
respDataTrans <- decostand(respDataTrans, method="hell")

## Check for collinearity
library(usdm)
coLinear <- vifcor(data.frame(respDataTrans))
respDataTrans <- respDataTrans[coLinear@results$Variables]

dca1 <- decorana(respDataTrans)
dca1

## Adjust other data
predData <- dplyr::select(plantsMapbox, Month6.offhours:Month8.peakhours) %>% .[!is.na(rowSums(respData)),]

rda1 <- rda(X= respDataTrans, Y=predData)
summary(rda1)

plot(rda1)
anova(rda1)

pcaScoresMapbox <- cbind(predData, scores(rda1, choices=c(1,2), display="site"))

ggplot(pcaScoresMapbox, aes(x=Month6.peakhours, y= PC2)) + geom_point()
