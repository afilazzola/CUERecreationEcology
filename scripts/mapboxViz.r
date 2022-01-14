### Mapbox visualization of patterns for CH properties

## Libraries
library(tidyverse)

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
lmOut <- effects::effect("dailyAdults", m1, 
    xlevels = list(dailyAdults = seq(150,550,50))) %>% 
    data.frame()

plot2 <- ggplot(mapboxParkReservations %>% filter(Name != "Mountsberg"),
 aes(x = dailyAdults, y =avgLogActivity, color=dayOfWeek, label = Name)) +
 scale_color_manual(values=c("#E69F00", "#56B4E9")) + theme_classic() +
 geom_line(data=  lmOut ,aes(x  = dailyAdults, y= fit, label = NA), color = "grey40", size=1.6) +
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
                    mutate(trailLengthkm2 = trailLength/1000)

### Model patterns between trail use and human activity
m2 <- lm(avgLogActivity ~ poly(trailLengthkm2,2) * dayOfWeek, 
    data =mapboxTrails %>%  filter(accessibility == "open"))
summary(m2)
anova(m2)

lm2Out <- effects::effect("poly(trailLengthkm2,2)", m2, 
    xlevels = list(trailLengthkm2 = 0:26)) %>% 
    data.frame()


plot3 <- ggplot(mapboxTrails %>% filter(accessibility == "open"),
    aes(x = trailLengthkm2, y = avgLogActivity, color = dayOfWeek, label=Name)) +
geom_text() +
scale_color_manual(values=c("#E69F00", "#56B4E9")) + theme_classic() +
xlab("Total Length of Trails (km)") +
 ylab("Average Mobile Activity (log-transformed)") +
 xlim(-3,30) +
 theme(text = element_text(size = 16), legend.position = c(0.15, 0.9)) +
 geom_line(data=  lm2Out ,aes(x  = trailLengthkm2, y= fit, label = NA), color = "grey60", size=1.2) 
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

ggsave("WeekendPatterns.pdf", arrangeGrob(plot3, plot4, ncol=2), width=11, height=5.5)



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

AccessibilityTest <- mapbox %>% 
            dplyr::select(Name, dayOfWeek, accessibility, avgLogActivity) %>% 
            spread(accessibility, avgLogActivity) 

plot7 <-  ggplot(AccessibilityTest, aes(x=open, y = closed, color = dayOfWeek)) +
    geom_point() + theme_classic() + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
    ylab("Closed hours activity") +
    xlab("Open hours activity") +
    theme(text = element_text(size = 16), legend.position = c(0.15, 0.9)) 


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

## Activity of land management
parkType <- mapbox %>% group_by(Area_Type, Managed ) %>% 
    summarize(nSites = length(unique(Name)),
            PercentHumanActivity = mean(HumanMobilePercent),
            AvgSize = mean(PropertyAreakm2))
parkType
write.csv(parkType, "figs//Table1.csv", row.names=F)

## Save output plots
ggsave("figs/WeekendPatterns.pdf", arrangeGrob(plot1, plot2), height=13)
ggsave("figs/TrailPatterns.pdf", 
    arrangeGrob(plot3, plot4, plot5), 
    height=14, width=8)
ggsave("figs/AccessPatterns.pdf", 
    arrangeGrob(plot6, plot7), 
    height=10, width=8)
