## Clean biodiversity data for halton

## Libraries
library(tidyverse)
library(vegan)

## load data
veg <- read.csv("data//biodiversityData//CH_groundvegetation Feb 11 2021.csv", header=F, stringsAsFactors = F)

## Create new category combining 
colnames(veg) <- apply(veg[1:3,], 2, paste0, collapse=";")
colnames(veg)[1:2] <- c("SpeciesName", "CommonName")
veg <- veg[-c(1:3),]

## Gather all the categories together and split into unique columns
vegLong <- veg %>% select(-"NA;NA;NA") %>% select(-"NA;NA;NA") %>% 
          gather(surveyInstance, value, 3:ncol(.))
vegLong <- vegLong %>% separate(surveyInstance, sep=";", c("Site","Year","Metric"))

## Select only cover
vegCover <- vegLong %>% filter(Metric == "Cover (m2)")

## Summarize by site
vegCover <- vegCover %>% separate(Site, sep=" ", c("SiteName","QuadratID"))
unique(vegCover$QuadratID)
siteMeans <- vegCover %>% group_by(SiteName, Year, SpeciesName) %>% 
  mutate(value=as.numeric(value)) %>% 
  summarize(meanSpp = mean(value, na.rm=T)) %>% 
  filter(!is.na(meanSpp))

## Spread back into community matrix
vegWide <- siteMeans %>% mutate(abrSpp = abbreviate(SpeciesName, 6)) %>% 
  select(-SpeciesName) %>% 
  spread(abrSpp, meanSpp, fill=0)


## ordination
ordData <- decostand(vegWide[,3:ncol(vegWide)], method="hellinger")
rownames(ordData) <- paste(vegWide$SiteName, vegWide$Year, sep="-")
pca1 <- rda(ordData, Z=as.factor(vegWide$Year))

plot(pca1)
orditorp(pca1, display = "species", cex = 0.7, col = "darkorange3", air=0.5)
orditorp(pca1, display = "sites", cex = 0.7, col = "darkslateblue", air=0.1)

summary(pca1)





### Bird data
