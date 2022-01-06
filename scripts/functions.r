#### Functions

###### GBIF lookup of species name
library(rgbif)
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
