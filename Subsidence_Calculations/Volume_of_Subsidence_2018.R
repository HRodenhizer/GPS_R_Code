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

# plot
soil.loss <- ggplot(volume, aes(x = block, y = volume.lost*-1)) +
  geom_col(fill = '#990000') +
  scale_x_discrete(name = '') +
  scale_y_continuous(name = expression(Volume~Soil~Lost~(m^3)),
                     limits = c(0, 205)) +
  theme_few() +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x  = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Volume_Soil_Lost.jpg', soil.loss)
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Volume_Soil_Lost.pdf', soil.loss)
