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
library(RStoolbox)
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
filenames <- c('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2018/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_389000_7085000_image.tif',
               'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2018/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_389000_7086000_image.tif',
               'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2018/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7085000_image.tif',
               'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2018/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7086000_image.tif')
emlrgb <- raster::merge(brick(filenames[1]),
                        brick(filenames[2]),
                        brick(filenames[3]),
                        brick(filenames[4])) # only the four tiles right around eml
# filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif', full.names = TRUE)
# neondtm <- raster(filenames[1])
# for (i in 2:length(filenames)) {
#   neondtm <- raster::merge(neondtm, raster(filenames[i]))
# }
##############################################################################################################

### Select/clip to CiPEHR ####################################################################################
transects2009 <- points2009 %>%
  filter(type == 'fence' | type == 'trans') %>%
  st_transform(crs = st_crs(emldtm))

transects2018 <- points2018 %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name))) %>%
  filter(Name > 10000 & Name < 13000) %>%
  st_transform(crs = st_crs(emldtm))

cipehrblocks <- emlpoints %>%
  filter(Exp == 'CiPEHR') %>%
  st_transform(crs = st_crs(emldtm))

cipehr <- cipehrblocks %>%
  st_transform(crs = 4326) %>%
  cbind.data.frame(st_coordinates(.)) %>%
  group_by(Exp) %>%
  summarise(X = mean(X),
            Y = mean(Y))

cipehrblocks <- cipehrblocks %>%
  cbind.data.frame(st_coordinates(.))

plot(emldtm)
emlslope <- terrain(emldtm, opt = 'slope')
plot(emlslope)
emlaspect <- terrain(emldtm, opt = 'aspect')
plot(emlaspect)
emlhillshd <- hillShade(emlslope, emlaspect, direction = 45)
plot(emlhillshd)
rm(emlslope, emlaspect)
##############################################################################################################

### Convert raster to dataframe for plotting with ggplot #####################################################
emlhillshd.df <- as.data.frame(emlhillshd, xy = TRUE)
emlrgb <- reclassify(emlrgb, cbind(NA, -9999))
##############################################################################################################

### Plot block location over hillshade #######################################################################
# emlimage <- ggRGB(emlrgb, r = 1, g = 2, b = 3, stretch = 'lin')
# 
# emlimage +
#   geom_point(data = cipehrblocks, aes(x = X, y = Y), inherit.aes = FALSE) +
#   theme_few() +
#   scale_fill_gradient(low = '#FFFFFF', high = '#000000', guide = FALSE) +
#   theme(axis.title = element_blank(),
#         axis.text.x  = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         aspect.ratio = 1)

akcenter <- as.numeric(geocode('alaska, usa'))
akmap <- ggmap(get_googlemap(center = akcenter, zoom = 4, maptype = 'satellite'))

akmapgrob <- ggplotGrob(akmap +
  geom_point(data = cipehr, aes(x = X, y = Y), inherit.aes = FALSE, colour = '#cc3300') +
  theme_map() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0),"null"),
        panel.background = element_rect(fill = NULL)))

emlmap <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = layer)) +
  geom_tile() +
  geom_point(data = cipehrblocks, aes(x = X, y = Y), inherit.aes = FALSE, colour = '#cc3300') +
  geom_text(data = cipehrblocks, aes(x = X, y = Y, label = Block), inherit.aes = FALSE, colour = '#cc3300',  hjust = -0.2, vjust = -0.2) +
  theme_few() +
  scale_fill_gradient(low = '#000000', high = '#DDDDDD',
                      guide = FALSE) +
  scale_x_continuous(limits = c(389000, 391000),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(7085000, 7087000),
                     expand = c(0,0)) +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1) +
  coord_fixed()
emlmap

fullmap <- emlmap +
  annotation_custom(grob = akmapgrob, xmin = 389000, ymin = 7085000, xmax = 389500, ymax = 7085500)
fullmap
