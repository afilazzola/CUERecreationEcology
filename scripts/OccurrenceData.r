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
totalOccLands <- occLands %>% group_by(ScientificName, CommonName, Name, TSN, Type,
    SHAPE_Area, SHAPE_Leng, Year) %>% 
                  summarize(nOcc = length(ScientificName)) 
totalOccLandsDF <- totalOccLands %>% data.frame() %>% select(-geometry)




# write.csv(totalOccLandsDF, "out//data//SpeciesOccProperties.csv", row.names=FALSE)

## Load incidental records
totalOccDF <- read.csv("out//data//SpeciesOccProperties.csv") %>% filter(Year > 2014)
status <- read.csv("data//biodiversityData//invasionStatus.csv")
totalOccDF <- totalOccDF %>% 
    left_join(status) %>% 
    mutate(nativeStatus = ifelse(is.na(Introduced), "Native","Exotic"))

## load mapbox data
mapboxData <- read.csv("out//data//MapboxSummaryData.csv")

## Summarize relative abundances
relativeAbd <- totalOccDF %>% 
    mutate(Taxa = 
        case_when(Type %in% c("I","O","L") ~ "Insect",
            Type %in% c("B","M","F","H") ~ "Vertebrate",
            Type == "P" ~ "Plant")) %>% 
    group_by(Name, Taxa, nativeStatus) %>% 
    summarize(nSpecies = length(unique(ScientificName)), nObs = sum(nOcc))

propNativeProperty <- relativeAbd %>% 
    spread(nativeStatus, nObs, fill=0) %>% 
    mutate(lrrNative = log((Native+1)/(Exotic+1)))


siteTotal <- mapboxData %>% 
    group_by(Name) %>% 
    summarize(meanActivity = sum(avgLogActivity), 
        IQRactivity = IQR(avgLogActivity))

mapboxNative <- left_join(propNativeProperty,siteTotal) %>% 
    mutate(Taxa = as.factor(Taxa))

m1 <- lm(lrrNative ~ meanActivity + Taxa, data= mapboxNative)
summary(m1)
anova(m1)

ggplot(mapboxNative,
    aes(x=meanActivity, y = lrrNative)) + 
    facet_grid(~Taxa) +
    geom_jitter(width = 10) +  theme_classic() + 
     scale_colour_brewer(palette = "Set1") +
     geom_hline(yintercept = 0, lty=2) +
     geom_smooth(method = "lm", color="black") + 
     ylab("LRR Proportion Native") + xlab("Sum mobility activity data")
save_PDF("figs/IncidentalStatus.pdf", setWidth = 14)

ggplot(mapboxNative,
    aes(x=meanActivity, y = log(nSpecies+1), color=Taxa)) + 
    geom_point() +  theme_classic() + 
     scale_colour_brewer(palette = "Set1") +
     geom_hline(yintercept = 0, lty=2) +
     geom_smooth(method = "lm", color="black") + 
     ylab("LRR Proportion Native") + xlab("Total Adjusted Activity")

m1 <- MASS::glm.nb(nSpecies ~ meanActivity + Taxa, data= mapboxNative)
summary(m1)
anova(m1, test="Chisq")



 ggplot(mapboxNative,
    aes(x=meanActivity, y = log(nSpecies))) + 
    facet_grid(~Taxa) +
    geom_jitter(width = 10) +  theme_classic() + 
     scale_colour_brewer(palette = "Set1") +
     geom_hline(yintercept = 0, lty=2) +
     geom_smooth(method = "lm", color="black") + 
     ylab("LRR Proportion Native") + xlab("Sum mobility activity data")
save_PDF("figs/IncidentalRichness.pdf", setWidth = 14)
