## Clean biodiversity data for halton

## Libraries
library(tidyverse)
library(vegan)

source("scripts/functions.r")

## Site code info
siteCodes <- read.csv("data//biodiversityData//CHsitenames.csv")

## Species characteristics
occData <- read.csv("data//biodiversityData//UrbanetDLA.csv") %>% filter(!(is.na(Easting) | is.na(Northing)))


### Ground vegetation data
veg <- read.csv("data//biodiversityData//CH_groundvegetation Feb 11 2021.csv", header=F, stringsAsFactors = F)

## Create new category combining 
colnames(veg) <- apply(veg[1:3,], 2, paste0, collapse=";")
colnames(veg)[1:2] <- c("SpeciesName", "CommonName")
veg <- veg[-c(1:3),]

## Gather all the categories together and split into unique columns
vegLong <- veg %>% select(-"NA;NA;NA") %>% select(-"NA;NA;NA") %>% 
          gather(surveyInstance, value, 3:ncol(.))
vegLong <- vegLong %>% separate(surveyInstance, sep=";", c("Site","Year","Metric"))

## Find native status
uniqueVeg <- vegLong %>%
  distinct(SpeciesName, CommonName) %>% 
  filter(SpeciesName != "")
uniqueVeg[,"status"] <- find_Native_Status(uniqueVeg$SpeciesName, "scientific")


## Select only cover
vegCover <- vegLong %>% filter(Metric == "Cover (m2)")

## Summarize by site
vegCover <- vegCover %>% 
  separate(Site, sep=" ", c("Sitecode","PlotID")) %>% 
  left_join(siteCodes)
unique(vegCover$PlotID)

## Length Not NA to find number of species observed
lengthNA <- function(x) length(x[!is.na(x)])

vegSiteSummary <- vegCover %>% 
  group_by(PropertyName, PlotID, Year) %>%  ## calculate plot level averages
  mutate(value = as.numeric(value)) %>% 
  summarize(nSpecies = lengthNA(value),
            coverDens = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(PropertyName) %>% ## calculate site level patterns
  summarize(AbundanceAVG = mean(coverDens), RichnessAVG = mean(nSpecies)) %>% 
  mutate(community = "ground plants")
  

### Trees
trees <- read.csv("data//biodiversityData//CHTreeData.csv")
trees$Stem.Abundance <- as.numeric(gsub("#VALUE!", NA, trees$Stem.Abundance))


trees <- trees %>% separate(Site, into=c("Sitecode", "PlotID"), sep=" ")
treesReduced <- trees %>% 
  left_join(siteCodes) %>% 
  dplyr::select(Sitecode, PropertyName, PlotID, Year, Species.Richness,
                    Stem.Abundance, Total.Basal.Area) 
treeNames <- trees %>% 
  dplyr::select(American.Beech.basal.area:Yellow.Birch.basal.area) %>% 
  names() %>% 
  gsub(".basal.area", "", .) %>% 
  data.frame(CommonName = .)
treeNames[,"status"] <- find_Native_Status(treeNames$CommonName, "common")

treeSiteSummary <- treesReduced %>% 
                    group_by(PropertyName) %>% 
                    summarize(RichnessAVG = mean(Species.Richness, na.rm = T),
                    AbundanceAVG = mean(Stem.Abundance, na.rm = T),
                    BasalAreaAVG =  mean(Total.Basal.Area, na.rm = T))%>% 
                    mutate(community = "trees")


## Tree community data

### Bird data

birds <- read.csv("data//biodiversityData//CH_FBMP birds Feb 12 2021.csv", stringsAsFactors = F) %>% 
            select(Time:Comments)
uniqueBirds <- birds %>% distinct(Species.Name)

birdSummary<- birds %>%
              group_by(Site) %>% 
              filter(Year> 2009) %>% 
              mutate(Number = as.numeric(Number)) %>% 
              summarize(nRich = length(unique(Species.Name)), abd = sum(Number, na.rm=T), nInstances  = length(unique(Date))) %>% 
              mutate(RichnessAVG = nRich / nInstances, AbundanceAVG = abd/nInstances) %>% 
              mutate(community = "birds") %>% rename(PropertyName = Site)

birdComm <- birds %>%
              filter(Year> 2009) %>% 
              mutate(Number = as.numeric(Number)) %>% 
              mutate(species = abbreviate(Species.Name, 6)) %>% 
              group_by(Site, species) %>% 
              summarize(count = sum(Number, na.rm=T)) %>% 
              spread(species, count, fill=0)

## combine datasets

birdJoinData <- birdSummary %>% 
  dplyr::select(PropertyName, community, AbundanceAVG, RichnessAVG)
treeJoinData <- treeSiteSummary %>% 
  dplyr::select(PropertyName, community, AbundanceAVG, RichnessAVG, BasalAreaAVG)
plantJoinData <- vegSiteSummary %>% 
  dplyr::select(PropertyName, community, AbundanceAVG, RichnessAVG)

bioDataCombined <- plyr::rbind.fill(birdJoinData, treeJoinData, plantJoinData)

write.csv(bioDataCombined, "data//biodiversityData//summarizedSitelevelData.csv", row.names=F)
