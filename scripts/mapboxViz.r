### Mapbox visualization of patterns for CH properties

## Libraries
library(tidyverse)
library(gridExtra)
library(raster)
library(sf)
library(rgdal)

## Data
mapbox <- read.csv("out//data//MapboxSummaryData.csv")

## Functions
source("scripts/functions.r")

plot1 <- ggplot(mapbox, aes(x=accessibility, y=avgLogActivity, fill=dayOfWeek)) +
    geom_boxplot() + theme_classic() +
    scale_fill_manual(values=c("#E69F00", "#56B4E9")) + 
    xlab("") + ylab("Average Mobile Activity (log-transformed)") + 
    theme(text = element_text(size = 16), legend.position = c(0.15, 0.9))
plot1

ggplot(mapbox, aes(x = reorder(Name, avgLogActivity), y=avgLogActivity)) +
 geom_boxplot() + coord_flip() + xlab("") + 
 ylab("Average Mobile Activity (log-transformed)") + theme_classic() +
 theme(text = element_text(size = 16))


ggplot(mapbox %>% distinct(Name, HumanMobilePercent), aes(x = reorder(Name, HumanMobilePercent), y = HumanMobilePercent)) +
 geom_bar(stat="identity") + coord_flip() + xlab("") + 
 ylab("Area with human activity (percent)") + theme_classic()


## Load reservation data to explore with mapbox

source("scripts//reservationClean.r")
head(parkReservationAvgs)

## Total in each park
totalParkRes <- parkReservationAvgs %>%
 dplyr::select(-start_h2) %>% 
 group_by(Name, dayOfWeek) %>% 
 summarize_all(sum)

mapboxParkReservations <- mapbox %>% 
                filter(accessibility == "open") %>% 
                right_join(totalParkRes)

## Test patterns in parks
m1 <- lm(avgLogActivity ~ dailyAdults * dayOfWeek, 
    data= mapboxParkReservations %>% filter(Name != "Mountsberg"))
anova(m1)
summary(m1)
lmOut <- effects::effect("dailyAdults:dayOfWeek ", m1, 
    xlevels = list(dailyAdults = seq(150,550,50))) %>% 
    data.frame()

plot2 <- ggplot(mapboxParkReservations %>% filter(Name != "Mountsberg"),
 aes(x = dailyAdults, y =avgLogActivity, color=dayOfWeek, label = Name)) +
 scale_color_manual(values=c("#E69F00", "#56B4E9")) + theme_classic() +
 geom_line(data=  lmOut ,aes(x  = dailyAdults, y= fit, label = NA), size=1.6) +
 geom_text() + xlim(100, 550) + xlab("Total daily reservations") + 
 ylab("Average Mobile Activity (log-transformed)")  +
 theme(text = element_text(size = 16), legend.position = c(0.15, 0.9)) 
plot2

 ### Trail density comparisons with mapbox
lands <- readOGR(layer="CHLands", dsn="data//CHProperties")
lands <- st_as_sf(lands)
trails <- readOGR(layer="Trails", dsn="data//CHProperties")
trails <- spTransform(trails, CRS="+proj=longlat +datum=WGS84 +no_defs") ## switch to lat lon
trails <- st_as_sf(trails)

trailParks<- st_intersection(trails, lands)

## summarize patterns in each park
trailStats <- trailParks %>% group_by(Name) %>% 
    summarize(trailLength = sum(ShapeSTLen)) 

## join with mapbox data
mapboxTrails <- mapbox %>% right_join(trailStats) %>% 
                    mutate(trailLengthkm2 = trailLength/1000,
                            trailDensity = (trailLength/1000) / (SHAPE_Area/1000000))

### Model patterns between trail use and human activity
m2 <- lm(avgLogActivity ~ trailDensity * dayOfWeek, 
    data =mapboxTrails %>%  filter(accessibility == "open") %>% 
        filter(!(Name %in% c("Robert Edmondson", "Shanahan")))) ## drop two smallest parks
summary(m2)
anova(m2)

lm2Out <- effects::effect("trailDensity", m2, 
    xlevels = list(trailDensity = -1:5)) %>% 
    data.frame()


plot3 <- ggplot(mapboxTrails %>% filter(accessibility == "open"),
    aes(x = trailDensity, y = avgLogActivity, color = dayOfWeek, label=Name)) +
geom_text() +
scale_color_manual(values=c("#E69F00", "#56B4E9")) + theme_classic() +
xlab("Trail Density (km/km2)") +
 ylab("Average Mobile Activity (log-transformed)") +
 xlim(-1,8) +
 theme(text = element_text(size = 16), legend.position = c(0.15, 0.9)) +
 geom_line(data=  lm2Out ,aes(x  = trailDensity, y= fit, label = NA), color = "grey60", size=1.2) 
plot3

ProportionActivityData <- mapboxTrails %>% 
    filter(accessibility == "open" & dayOfWeek == "weekend") %>% 
    mutate(humanMobileProp = HumanMobilePercent/100,
            trailDensity = (trailLength/1000) / (SHAPE_Area/1000000)) ## km / km2
m3 <- lm(humanMobileProp ~ trailDensity,
            data = ProportionActivityData)
car::Anova(m3, type=2)
summary(m3)

lm3Out <- effects::effect("trailDensity", m3, 
    xlevels = list(trailDensity = 0:8)) %>% 
    data.frame()


plot4 <- ggplot(ProportionActivityData,
    aes(x = trailDensity, y = humanMobileProp*100,  label = Name)) +
geom_text() +   theme(text = element_text(size = 16))  +
 theme_classic() +
xlab("Trail Density (km/km2)") +
 ylab("Percent area with any human activity") +
 xlim(0,8) + ylim(20,100) +
 theme(text = element_text(size = 16)) +
 geom_line(data=  lm3Out ,aes(x  = trailDensity, y= fit*100, label = NA), color = "grey60", size=1.2) 
plot4



#### Exploration of Mapbox data on its own
dayOfWeekTest <- mapbox %>% 
            dplyr::select(Name, dayOfWeek, accessibility, avgLogActivity) %>% 
            spread(dayOfWeek, avgLogActivity)
           

plot6 <- ggplot(dayOfWeekTest %>% filter(accessibility == "open"), aes(x=weekday, y = weekend)) +
    geom_point() + theme_classic() +
     ylab("Weekend activity") +
    xlab("Weekday activity") +
    theme(text = element_text(size = 16)) +
    geom_smooth(method="lm", color="grey60", se=F, size=1.2)
plot6

AccessibilityTest <- mapbox %>% 
            dplyr::select(Name, dayOfWeek, accessibility, avgLogActivity) %>% 
            spread(accessibility, avgLogActivity) 

plot7 <-  ggplot(AccessibilityTest, aes(x=open, y = closed, color = dayOfWeek)) +
    geom_point() + theme_classic() + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
    ylab("Closed hours activity") +
    xlab("Open hours activity") +
    theme(text = element_text(size = 16), legend.position = c(0.15, 0.9)) 
plot7

m4 <- lm(TrailActivityPercent ~ HumanMobilePercent, data=mapbox)
summary(m4)

## Comparison of on trail use to general human mobile

plot5 <- ggplot(mapbox, aes(x=TrailActivityPercent, y = HumanMobilePercent, label = Name)) + 
 theme_classic() +ylim(20,100) + xlim(-10,80) + 
xlab("Percent of activity directly on trails") + 
ylab("Percent area with any human activity") +  
 theme(text = element_text(size = 16)) +
 geom_smooth(method = "lm", color="grey60", se=F, size=1.2,fullrange=TRUE) +
 geom_text()
plot5

ggsave("figs//Figure2Trails.pdf", arrangeGrob(plot3, plot4, plot5, ncol=3), width=16, height=5.5)



## Activity of land management
parkType <- mapbox %>% group_by(Area_Type, Managed ) %>% 
    summarize(nSites = length(unique(Name)),
            PercentHumanActivity = mean(HumanMobilePercent),
            AvgSize = mean(PropertyAreakm2))
parkType
write.csv(parkType, "figs//Table1.csv", row.names=F)

## Save output plots
ggsave("figs/Figure1Activity.pdf", arrangeGrob(plot1, plot2), height=13)



### Biodiversity exploration

biodata <- read.csv("data//biodiversityData//summarizedSitelevelData.csv") %>% 
    rename(Name = PropertyName)

bioMapbox <- biodata %>% right_join(mapbox)


## Richness
ggplot(bioMapbox %>%  filter(accessibility == "open" & community != "NA" & dayOfWeek == "weekend" & Name != "Wildflower Woods"), 
    aes(x = activityDensityLog, y = RichnessAVG,  label=Name)) +
    facet_wrap(~community, scales="free") + geom_text(aes(color = Area_Type)) + 
    geom_smooth(method = "glm", color="black", method.args = list(family=gaussian(link = "log"))) +
    theme_classic() + theme(text = element_text(size = 16)) + 
    scale_colour_manual(values=c("#999999", "#E69F00", "#0c0d0e")) +
    ylab("Average annual species richness") + xlab("Mobile cell activity density") 
save_PDF("activityDensity.pdf", setWidth = 14)

activityModels <- bioMapbox %>%
    filter(accessibility == "open" & community != "NA" & dayOfWeek == "weekend" & Name != "Wildflower Woods") %>% 
    group_by(community) %>%
    do(fit = broom::tidy(glm(RichnessAVG ~ activityDensityLog, family=gaussian(link = "log"), 
                            data = .))) %>%
    unnest(fit)
activityModels
### No pattern with abundance

## Park usage
ggplot(bioMapbox %>%  filter(accessibility == "open" & community != "NA" & dayOfWeek == "weekend"), 
    aes(x = HumanMobilePercent, y = RichnessAVG, label=Name)) +
    facet_wrap(~community, scales="free") + geom_text(aes( color=Area_Type)) + 
   # geom_smooth(method = "lm",color="black") +
    scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    theme_classic() + theme(text = element_text(size = 16)) +
    ylab("Average annual abundance") + xlab("Park use (%)")
save_PDF("parkUse.pdf", setWidth = 14)

usageModels <- bioMapbox %>%
    filter(accessibility == "open" & community != "NA" & dayOfWeek == "weekend" & Name != "Wildflower Woods") %>% 
    group_by(community) %>%
    do(fit = broom::tidy(glm(RichnessAVG ~ HumanMobilePercent, family=gaussian(link = "log"),
                            data = .))) %>%
    unnest(fit)
usageModels

ggplot(bioMapbox %>%  filter(accessibility == "open" & community != "NA" & dayOfWeek == "weekend"), 
    aes(x = TrailActivityPercent, y = RichnessAVG, label=Name)) +
    facet_wrap(~community, scales="free") + geom_text() + 
    geom_smooth(method = "lm") +
    scale_color_manual(values=c("#E69F00", "#56B4E9")) + theme_classic()

    
trailUseModels <- bioMapbox %>%
    filter(accessibility == "open" & community != "NA" & dayOfWeek == "weekend" & Name != "Wildflower Woods") %>% 
    group_by(community) %>%
    do(fit = broom::tidy(lm(RichnessAVG ~ TrailActivityPercent,
                            data = .))) %>%
    unnest(fit)
trailUseModels 


#### IQR patterns

ggplot(bioMapbox %>%  filter(accessibility == "open" & community != "NA" & dayOfWeek == "weekend"), 
    aes(x = IQRLogActivity, y = RichnessAVG, label=Name)) +
    facet_wrap(~community, scales="free") + geom_text(aes( color=Area_Type)) +
    geom_smooth(method = "glm", color="black", method.args = list(family=gaussian(link = "log"))) +
    scale_color_manual(values=c("#999999","#E69F00", "#56B4E9")) + 
    theme_classic() + theme(text = element_text(size = 16)) +
    ylab("Average annual species richness") + xlab("Variability in cell activity (IQR)")
save_PDF("ActivityVariability.pdf", setWidth = 14)

IQRModels <- bioMapbox %>%
    filter(accessibility == "open" & community != "NA" & dayOfWeek == "weekend" & Name != "Wildflower Woods") %>% 
    group_by(community) %>%
    do(fit = broom::tidy(glm(RichnessAVG ~ IQRLogActivity, family=gaussian(link = "log"),
                            data = .))) %>%
    unnest(fit)
IQRModels


##### Compare activity with ELC 

#### Peak hours for mapbox

maxActivity <- MapboxtimingAdjusted %>% 
    group_by(bounds) %>% 
    summarize(peakActivity = max(areaAdjActivity)) %>% 
    select(-bounds)

# st_write(maxActivity,dsn="data//Mapbox", layer="test", driver="ESRI Shapefile")



## Load just peak activity from mapbox
MapboxtimingAdjusted <-  readOGR(dsn="data//Mapbox", layer="MapboxAdjustedActivity")
MapboxtimingAdjusted <- st_as_sf(MapboxtimingAdjusted)

## crop to lands
ELCmapbox <- st_intersection(MapboxtimingAdjusted, ELC)
ELCMapboxPatterns <- ELCmapbox %>% 
  mutate(propHumanActivity =  area/SHAPE_A) %>% 
  mutate(ELCarea = as.numeric(st_area(ELCmapbox))) %>% 
  group_by(Name, Class_Desc) %>% 
  summarize(totalActivity = mean(arAdjAc),
    totalHumanActivity = sum(propHumanActivity),
    totalELCused = sum(ELCarea)) %>% 
  left_join(ELCsitepatterns) %>% 
mutate(propELCused = totalELCused / totalELCArea)


plot1 <- ggplot(ELCMapboxPatterns, aes(x=Class_Desc, y= totalHumanActivity)) + geom_boxplot()+
  coord_flip() + theme_classic() + xlab("") + ylab("Percent of human activity")
plot2 <- ggplot(ELCMapboxPatterns, aes(x=Class_Desc, y= propELCused)) + geom_boxplot()+
  coord_flip() + theme_classic() + xlab("") + ylab("Percent of ELC class used")

## Does activity increase usage of any particular space
activityPatterns <- ELCMapboxPatterns  %>%
    group_by(Class_Desc) %>%
    do(fit = broom::tidy(lm(totalHumanActivity ~ totalActivity,
                            data = .))) %>%
    unnest(fit)
activityPatterns %>% filter(term == "totalActivity") %>% arrange(p.value)

ggsave("figs/ELCuse.pdf", 
    arrangeGrob(plot1, plot2), 
    height=10, width=8)
