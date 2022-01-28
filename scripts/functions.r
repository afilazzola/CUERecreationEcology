#### Functions

###### GBIF lookup of species name
require(rgbif)
getTaxaInfo <- function(genusVector) {
## loop through all the genus
genusDF <- data.frame()
for(i in genusVector){
  tryCatch({
    gbifDF <- name_lookup(i, limit=20)
    tempDF <- data.frame(Genus=i, kingdom = gbifDF$data$kingdom[1], 
                         phylum=gbifDF$data$phylum[1], 
                         order=gbifDF$data$order[1],
                         class=gbifDF$data$class[1],
                         family=gbifDF$data$family[1])
    genusDF <- rbind(genusDF, tempDF)
    
  }, error = function(e) e)
  
  progressBar <- which(genusVector == i) / length(genusVector)*100
  print(progressBar)
}
return(genusDF)
}



## Save plot
save_PDF <- function(plotName = "rplot1.pdf", setWidth = 8, setHeight = 6) {
pdf(plotName,         # File name
    width = setWidth, height = setHeight, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    useDingbats = F)          # Paper size
print(last_plot())
dev.off()  ## close Graphics
}

find_Native_Status <- function(speciesVector, nameType = "scientific") {
nativeList <- read.csv("data//biodiversityData//PlantNativeStatus.csv")

## drop "sp." from genus only
GenusOrSpecies <- tolower(gsub(" sp.", "", speciesVector))
GenusOrSpecies <- gsub(".", " ", GenusOrSpecies, fixed=T)
GenusOrSpecies <- trimws(GenusOrSpecies)

## search table
if(nameType == "scientific"){
searchList = tolower(nativeList$SCIENTIFIC_NAME)
} else{
searchList = tolower(nativeList$ENGLISH_COMMON_NAME)
}

matchNames <- c()
for(i in GenusOrSpecies){
matchName <-  nativeList[match(i, searchList), "EXOTIC_NATIVE"]
matchNames <- c(matchNames, matchName)
}
return(matchNames)
}
