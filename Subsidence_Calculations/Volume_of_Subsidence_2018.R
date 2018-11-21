####################################################################################################################################
###                                           Volume Lost to Subsidence at CiPEHR                                                ###
###                                                       HGR 11/2018                                                           ###
####################################################################################################################################

# load libraries
library(raster)
library(sp)
library(rgdal)
library(tidyverse)

# load data
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks', full.names = TRUE)
elevation <- list(brick(filenames[1]), brick(filenames[2]), brick(filenames[3]))
blocks <- readOGR('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Blocks_Poly.shp')

# calculate subsidence between 2009 and 2018 and then calculate volume
subsidence <- list()
volume.raster <- list()
volume <- data.frame(block = c('A', 'B', 'C'))
for (i in 1:length(elevation)) {
  subsidence[[i]] <- elevation[[i]][[10]] - elevation[[i]][[1]]
  volume.raster[[i]] <- subsidence[[i]]*res(subsidence[[i]])[1]*res(subsidence[[i]])[2]
  volume$volume.lost[i] <- cellStats(volume.raster[[i]], stat = 'sum', na.rm = TRUE)
  plot(volume.raster[[i]])
  rm(i)
}

# write.csv(volume, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Volume_of_Subsidence_2018.csv')
