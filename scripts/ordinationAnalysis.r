### Ordination analysis

## Libraries
library(tidyverse)
library(vegan)

source("scripts/functions.r")

## Site code info
siteCodes <- read.csv("data//biodiversityData//CHsitenames.csv")


### Trees
trees <- read.csv("data//biodiversityData//CHTreeData.csv")
trees$Stem.Abundance <- as.numeric(gsub("#VALUE!", NA, trees$Stem.Abundance))


trees <- trees %>% separate(Site, into=c("Sitecode", "PlotID"), sep=" ")
treesReduced <- trees %>% 
  left_join(siteCodes) %>% 
  dplyr::select(Sitecode, PropertyName, PlotID, Year, 
  American.Beech.basal.area:Yellow.Birch.basal.area) 
treeSiteAverages <- treesReduced %>% 
    group_by(PropertyName, Year) %>% 
    summarize_at(vars(American.Beech.basal.area:Yellow.Birch.basal.area),
        .funs = mean, na.rm = T) %>% 
    data.frame() 
treeSiteAverages[is.na(treeSiteAverages)] <- 0
names(treeSiteAverages) <- gsub(".basal.area", "", names(treeSiteAverages) )


## Set up biodiversity data
treeTransformed <- decostand(treeSiteAverages %>%
        dplyr::select(-PropertyName, -Year),
    method = "hellinger")
treeSiteAverages <- treeSiteAverages[rowSums(treeTransformed) != 0,]
treeTransformed <- treeTransformed[rowSums(treeTransformed) != 0,]

dca1 <- decorana(treeTransformed)

rda1 <- cca(treeTransformed, Z = as.factor(treeSiteAverages$Year))

## Mapbox data
mapbox <- read.csv("out//data//MapboxSummaryData.csv")

siteSummaries <- mapbox %>% 
    group_by(PropertyName = Name, dayOfWeek) %>% 
    summarize(totalActivity = sum(avgLogActivity),
        PercentUsed = unique(HumanMobilePercent)) %>% 
    spread(dayOfWeek, totalActivity) %>% 
    ungroup() %>% 
    filter(!is.na((weekend))) %>% 
    mutate(meanActivity = (weekday + weekend)/2, 
        dailyVariation = log(exp(weekend)/exp(weekday)))


treeTransformedMapbox <- treeSiteAverages %>% 
    left_join(siteSummaries) 
treeTransformed <- treeTransformed[!is.na(treeTransformedMapbox$PercentUsed),]
treeTransformedMapbox <- treeTransformedMapbox[!is.na(treeTransformedMapbox$PercentUsed),]
row.names(treeTransformed) <- paste0(
    abbreviate(treeTransformedMapbox$PropertyName, 6),
    "-",
    treeTransformedMapbox$Year)

rda1 <- cca(treeTransformed,
   Y = treeTransformedMapbox[,c("meanActivity","dailyVariation","PercentUsed")],
    Z = as.factor(treeTransformedMapbox$Year), 
    data = treeTransformedMapbox)

plot(rda1)
anova(rda1)
summary(rda1)
RsquareAdj(rda1)
pdf("save.pdf", useDingbats = F)


## Patterns in surveys
length(unique(treeTransformedMapbox$PropertyName))
min(treeTransformedMapbox$Year)
max(treeTransformedMapbox$Year)

rda1 <- cca(treeTransformed ~ Year,
    data = treeTransformedMapbox)