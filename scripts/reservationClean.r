### Summarize reservation data

## library
library(tidyverse)


######## Load in reservation data
reservations <- read.csv("data//ConservationHalton_ParkReservations.csv")


## Split date column
StudyReservations <- reservations %>% 
                        mutate(date = as.Date(Date)) %>% 
                        mutate(year = as.numeric(format(date, format = "%Y")),
                               month = as.numeric(format(date, format = "%m")),
                               day = as.numeric(format(date, format = "%d")))%>% 
                      mutate(time = as.POSIXct(Check.in.Time, format="%H:%M")) %>% 
                      mutate(hour =  as.numeric(format(time, "%H"))) %>% 
                      mutate(h2window = cut(hour, breaks = c(6,8,10,12,14,16,18,20,22))) %>%  ## create two h windows to match Mapbox
                      mutate(start_h2 = as.numeric(h2window)*2+4)

### Calculate same metrics as mapbox 
parkReservationAvgs <- StudyReservations %>% 
    filter(year == 2020) %>% 
    filter(month %in% 6:8) %>% 
    mutate(weekday = weekdays(date)) %>% 
    mutate(dayOfWeek = ifelse(weekday %in% c("Saturday","Sunday"), "weekend", "weekday")) %>% 
    mutate(Arrival.Location = replace(Arrival.Location,Arrival.Location %in%  
            c("Kelso Beach","Kelso Main Entrance","Kelso Summit"),"Kelso")) %>% 
    group_by(Name = Arrival.Location, dayOfWeek, start_h2) %>% 
    summarize(dailyAdults = sum(Adults+Seniors)/90,
              dailyYouths = sum(Children..5.14.+ Children..under.5.)/90)

