### Mapbox visualization of patterns for CH properties

## Libraries
library(tidyverse)

## Data
mapbox <- read.csv("out//data//MapboxSummaryData.csv")

## Functions

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


ggplot(mapbox, aes(x=dayOfWeek, y=avgLogActivity, fill=accessibility)) +
    geom_boxplot() + theme_classic() +
    scale_fill_manual(values=c("#E69F00", "#56B4E9")) + 
    xlab("") + ylab("Average Mobile Activity (log-transformed)") + 
    theme(text = element_text(size = 16)) 
save_PDF(plotName = "meanPatterns.pdf")

ggplot(mapbox, aes(x = reorder(Name, avgLogActivity), y=avgLogActivity)) +
 geom_boxplot() + coord_flip() + xlab("") + 
 ylab("Average Mobile Activity (log-transformed)") + theme_classic() +
 theme(text = element_text(size = 16))
save_PDF(plotName = "PropertyActivity.pdf", setHeight = 10)

ggplot(mapbox %>% distinct(Name, HumanMobilePercent), aes(x = reorder(Name, HumanMobilePercent), y = HumanMobilePercent)) +
 geom_bar(stat="identity") + coord_flip() + xlab("") + 
 ylab("Area with human activity (percent)") + theme_classic()
 save_PDF(plotName = "HumanActivity.pdf", setHeight = 10)

