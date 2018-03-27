library(plyr)
library(dplyr)
library(rworldmap)
library(reshape2)

#read in data
declines <- read.csv(file = "data/richness_declines.csv", header = TRUE, sep = ",", na.strings=c("","NA"))

#aggregate data
declines.ave <- declines %>% 
    group_by(country, taxa, study) %>% 
    summarise(average = mean(percent_richness_change),
              median = median(percent_richness_change),
              max = max(percent_richness_change),
              min = min(percent_richness_change))

####################################################
#make syrphid map
####################################################

declines.syrphid <- as.data.frame(filter(declines.ave, taxa == "syrphid")) %>% droplevels()

map.syrphid <- joinCountryData2Map(declines.syrphid, joinCode="NAME", nameJoinColumn = "country",
                            nameCountryColumn = "distribution")

#average richness change
map.syrphid.ave <- mapCountryData(map.syrphid, nameColumnToPlot = "average", colourPalette="topo",
                       mapTitle = "Average syrphid richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.syrphid.ave, legendLabels="all", legendWidth=0.5))

#max richness change
map.syrphid.max <- mapCountryData(map.syrphid, nameColumnToPlot = "max", colourPalette="topo",
                                  mapTitle = "Maximum syrphid richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.syrphid.max, legendLabels="all", legendWidth=0.5))

#min richness change
map.syrphid.min <- mapCountryData(map.syrphid, nameColumnToPlot = "min", colourPalette="topo",
                                  mapTitle = "Minimum syrphid richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.syrphid.min, legendLabels="all", legendWidth=0.5))

####################################################
#make bee map
####################################################

declines.bee <- as.data.frame(filter(declines.ave, taxa == "bee")) %>% droplevels()

map.bee <- joinCountryData2Map(declines.bee, joinCode="NAME", nameJoinColumn = "country",
                                   nameCountryColumn = "distribution")

#average richness change
map.bee.ave <- mapCountryData(map.bee, nameColumnToPlot = "average", colourPalette="topo",
                                  mapTitle = "Average bee richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.bee.ave, legendLabels="all", legendWidth=0.5))

#max richness change
map.bee.max <- mapCountryData(map.bee, nameColumnToPlot = "max", colourPalette="topo",
                                  mapTitle = "Maximum bee richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.bee.max, legendLabels="all", legendWidth=0.5))

#min richness change
map.bee.min <- mapCountryData(map.bee, nameColumnToPlot = "min", colourPalette="topo",
                                  mapTitle = "Minimum bee richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.bee.min, legendLabels="all", legendWidth=0.5))

####################################################
#make bumblebee map
####################################################

declines.bumblebee <- as.data.frame(filter(declines.ave, taxa == "bumblebee")) %>% droplevels()

map.bumblebee <- joinCountryData2Map(declines.bumblebee, joinCode="NAME", nameJoinColumn = "country",
                               nameCountryColumn = "distribution")

#average richness change
map.bumblebee.ave <- mapCountryData(map.bumblebee, nameColumnToPlot = "average", colourPalette="topo",
                              mapTitle = "Average bumblebee richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.bumblebee.ave, legendLabels="all", legendWidth=0.5))

#max richness change
map.bumblebee.max <- mapCountryData(map.bumblebee, nameColumnToPlot = "max", colourPalette="topo",
                              mapTitle = "Maximum bumblebee richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.bumblebee.max, legendLabels="all", legendWidth=0.5))

#min richness change
map.bumblebee.min <- mapCountryData(map.bumblebee, nameColumnToPlot = "min", colourPalette="topo",
                              mapTitle = "Minimum bumblebee richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.bumblebee.min, legendLabels="all", legendWidth=0.5))

####################################################
#make butterfly map
####################################################

declines.butterfly <- as.data.frame(filter(declines.ave, taxa == "butterfly")) %>% droplevels()

map.butterfly <- joinCountryData2Map(declines.butterfly, joinCode="NAME", nameJoinColumn = "country",
                                     nameCountryColumn = "distribution")

#average richness change
map.butterfly.ave <- mapCountryData(map.butterfly, nameColumnToPlot = "average", colourPalette="topo",
                                    mapTitle = "Average butterfly richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.butterfly.ave, legendLabels="all", legendWidth=0.5))

#max richness change
map.butterfly.max <- mapCountryData(map.butterfly, nameColumnToPlot = "max", colourPalette="topo",
                                    mapTitle = "Maximum butterfly richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.butterfly.max, legendLabels="all", legendWidth=0.5))

#min richness change
map.butterfly.min <- mapCountryData(map.butterfly, nameColumnToPlot = "min", colourPalette="topo",
                                    mapTitle = "Minimum butterfly richness change", catMethod = "pretty", addLegend = F)
do.call(addMapLegend, c(map.butterfly.min, legendLabels="all", legendWidth=0.5))

####################################################
#END
####################################################