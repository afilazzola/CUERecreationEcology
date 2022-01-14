### Mapbox metrics

library(tidyverse)
library(rgdal)


##  Load shapefile for lands
lands <- readOGR(layer="CHLands", dsn="data//CHProperties")
lands <- st_as_sf(lands)

## Load mapbox data
Mapboxtiming <- read.csv("out//data//MapboxTiming.csv") 

### determine hour timeframes
MapboxMetrics <- Mapboxtiming %>%
                      mutate(parkAvailability = ifelse(start_h2 >= 10 & start_h2 <= 16, "open","closed")) %>% 
                      tibble()

## Determine polygons with repeated observations
MapboxMetrics <- MapboxMetrics %>% 
                    group_by(dplyr::across(c(-start_h2,  -activity))) %>%  ##group by everything except time
                    mutate(dailyPolyFrequency = length(activity)) %>% 
                    left_join(data.frame(lands)) %>% 
                    mutate(areakm2 = SHAPE_Area/1000000)

### Calculate measures for comparisons with mapbox data
daily2HourWindow <- 12 ## 12, 2h windows
MapboxPatterns <- MapboxMetrics %>% group_by(Name, parkAvailability) %>% 
                    summarize(activityDensity = sum(activity)/areakm2,
                              activityDensityPerPolygon = sum(activity)/nPolys/areakm2,
                              polygonActivityHourly = sum(dailyPolyFrequency)/daily2HourWindow/areakm2,
                              polygonDensityHourly = nPolys/daily2HourWindow/areakm2)
                              



## Load reservation data as a check
reservations <- read.csv("data//ConservationHalton_ParkReservations.csv")

## Split date column
StudyReservations <- reservations %>% 
  mutate(date = as.Date(Date)) %>% 
  mutate(year = as.numeric(format(date, format = "%Y")),
         month = as.numeric(format(date, format = "%m")),
         day = as.numeric(format(date, format = "%d")))%>% 
  mutate(time = as.POSIXct(Check.in.Time, format="%H:%M")) %>% 
  mutate(hour =  as.numeric(format(time, "%H"))) %>% 
  mutate(h2window = cut(hour, breaks = c(6,8,10,12,14,16,18,20))) %>%  ## create two h windows to match Mapbox
  mutate(start_h2 = as.numeric(h2window)*2+6)


hist(StudyReservations$start_h2)


### Correlate with reservation data
parkAvgs <- StudyReservations %>% 
  filter(year == 2020) %>% 
  filter(month %in% 6:8) %>% 
  group_by(Name=Arrival.Location) %>% 
  summarize(totalAdults = sum(Adults+Seniors),
            totalYouths = sum(Children..5.14.+ Children..under.5.)) 


reservationMapbox <- MapboxPatterns %>% left_join(parkAvgs)

ggplot(reservationMapbox %>% filter(parkAvailability=="open"), 
       aes(x=totalAdults, y=activityDensity, label=Name)) + 
        geom_point() + geom_text()
