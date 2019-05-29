##############################################################################################################
###                                Overview Map of GPS Survey                                              ###
###                                    Code by HGR 2/2019                                                  ###
##############################################################################################################

### Load Libraries ###########################################################################################
library(sf)
library(tidyverse)
library(sp)
library(raster)
library(ggmap)
library(ggthemes)
library(viridis)
##############################################################################################################

### Load Data ################################################################################################
points2009 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2009_SPCSAK4_corrected.shp')
points2018 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2018_SPCSAK4.shp')
emlpoints <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Sites.shp')
filenames <- c('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif/NEON_D19_HEAL_DP3_389000_7085000_DTM.tif',
               'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif/NEON_D19_HEAL_DP3_389000_7086000_DTM.tif',
               'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif/NEON_D19_HEAL_DP3_390000_7085000_DTM.tif',
               'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif/NEON_D19_HEAL_DP3_390000_7086000_DTM.tif')
emldtm <- raster::merge(raster(filenames[1]),
                        raster(filenames[2]),
                        raster(filenames[3]),
                        raster(filenames[4])) # only the four tiles right around eml
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif', full.names = TRUE)
# neondtm <- raster(filenames[1])
# for (i in 2:length(filenames)) {
#   neondtm <- raster::merge(neondtm, raster(filenames[i]))
# }
##############################################################################################################

### Select/clip to CiPEHR ####################################################################################
transects2009 <- points2009 %>%
  filter(type == 'fence' | type == 'trans')

transects2018 <- points2018 %>%
  mutate(Name = as.numeric(Name)) %>%
  filter(Name > 10000 & Name < 13000)

cipehrblocks <- emlpoints %>%
  filter(Exp == 'CiPEHR') %>%
  cbind.data.frame(st_coordinates(.))

plot(emldtm)
emlslope <- terrain(emldtm, opt = 'slope')
plot(emlslope)
emlaspect <- terrain(emldtm, opt = 'aspect')
plot(emlaspect)
emlhillshd <- hillShade(emlslope, emlaspect)
plot(emlhillshd)
##############################################################################################################

### Convert raster to dataframe for plotting with ggplot #####################################################
emlhillshd.df <- as.data.frame(emlhillshd, xy = TRUE)

### Plot block location over hillshade #######################################################################
# need to transform to have the same cs before this will work!
ggplot(emlhillshd.df, aes(x = x, y = y, fill = layer)) +
  geom_tile() +
  geom_point(data = cipehrblocks, aes(x = X, y = Y), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_gradient(low = '#FFFFFF', high = '#000000') +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_blank(),
        aspect.ratio = 1) +
  coord_fixed()
