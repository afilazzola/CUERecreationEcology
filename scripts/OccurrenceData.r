### Wrangle the CHBIS dataset


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
