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
library(ggpubr)
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
filenames <- c('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2018/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7085000_image.tif',
               'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2018/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7086000_image.tif')
emlrgb <- raster::merge(brick(filenames[1]),
                        brick(filenames[2])) # only the two tiles right around cipehr
# filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif', full.names = TRUE)
# neondtm <- raster(filenames[1])
# for (i in 2:length(filenames)) {
#   neondtm <- raster::merge(neondtm, raster(filenames[i]))
# }
rm(filenames)
##############################################################################################################

### Select/clip to CiPEHR ####################################################################################
spcsak4 <- st_crs(points2018)[[2]]

# transects as originally done
transects2009 <- points2009 %>%
  filter(type == 'fence' | type == 'trans') %>%
  cbind.data.frame(st_coordinates(.)) %>%
  st_as_sf() %>%
  mutate(block = ifelse(fence == 1 | fence == 2,
                        'A',
                        ifelse(fence == 3 | fence == 4,
                               'B',
                               'C'))) %>%
  dplyr::select(X, Y, block)

# transects as done with stakeout
transects2018 <- points2018 %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name))) %>%
  filter(Name > 10000 & Name < 13000) %>%
  cbind.data.frame(st_coordinates(.)) %>%
  st_as_sf()%>%
  mutate(block = ifelse(Name >= 10000 & Name < 11000,
                        'A',
                        ifelse(Name >= 11000 & Name < 12000,
                               'B',
                               'C'))) %>%
  dplyr::select(X, Y, block)

# start getting three block locations
cipehrblocks <- emlpoints %>%
  filter(Exp == 'CiPEHR') %>%
  st_transform(crs = st_crs(emldtm))

# overall cipehr location
cipehr <- cipehrblocks %>%
  st_transform(crs = 4326) %>%
  cbind.data.frame(st_coordinates(.)) %>%
  group_by(Exp) %>%
  summarise(X = mean(X),
            Y = mean(Y))

# final formatting for location of three blocks
cipehrblocks <- cipehrblocks %>%
  cbind.data.frame(st_coordinates(.))

# create hillshade of dtm
plot(emldtm)
emlslope <- terrain(emldtm, opt = 'slope')
plot(emlslope)
emlaspect <- terrain(emldtm, opt = 'aspect')
plot(emlaspect)
emlhillshd <- hillShade(emlslope, emlaspect, direction = 45)
plot(emlhillshd)
rm(emlslope, emlaspect)

# convert to dataframe for graphing
emlhillshd.df <- as.data.frame(emlhillshd, xy = TRUE)
rm(emlhillshd, emldtm)

# clip emlrgb to three separate rasters for each block and transform to SPCS AK 4 - this will take awhile!
emlrgb <- reclassify(emlrgb, cbind(NA, -9999)) %>%
  projectRaster(emlrgb, crs = spcsak4)

buffer2018 <- transects2018 %>%
  as.data.frame() %>%
  group_by(block) %>%
    summarise(xmin = min(X)-5,
            xmax = max(X)+5,
            ymin = min(Y)-5,
            ymax = max(Y)+5) %>%
  gather(key = xtype, value = X, xmin:xmax) %>%
  gather(key = ytype, value = Y, ymin:ymax) %>%
  dplyr::select(X, Y, block) %>%
  st_as_sf(coords = c('X', 'Y'), crs = spcsak4, remove = FALSE) %>%
  rbind.data.frame(transects2018) %>%
  arrange(block, X, Y)

blockimagery <- list()
for (i in 1:length(cipehrblocks$Block)) {
 blockimagery[[i]] <- crop(emlrgb, as(subset(buffer2018, block == c('A', 'B', 'C')[i]), 'Spatial'))
 rm(i)
}



# rgbnorm <- list()
# tran2018norm <- data.frame()
# 
# for (i in 1:length(blockimagery)) {
#   minx <- blockimagery[[i]]@extent@xmin
#   miny <- blockimagery[[i]]@extent@ymin
#   temp <- as.data.frame(blockimagery[[i]], xy = TRUE) %>%
#     mutate(x = round(x - minx),
#            y = round(y - miny),
#            block = c('A', 'B', 'C')[i]) %>%
#     rename(r = layer.1,
#            g = layer.2,
#            b = layer.3)
#   
#   rgbnorm[[i]] <- temp
#   temp <- transects2018 %>%
#     filter(Name >= 10000 + (i-1)*1000 & Name < 10000 + i*1000) %>%
#     cbind.data.frame(st_coordinates(.)) %>%
#     mutate(X = round(X - minx),
#            Y = round(Y - miny),
#            block = c('A', 'B', 'C')[i]) %>%
#     dplyr::select(X, Y, block)
#   tran2018norm <- rbind.data.frame(tran2018norm, temp)
#   rm(temp, i)
# }
##############################################################################################################

### Plot block location over hillshade #######################################################################
# better to have 2009 and 2018 separate or together? Is it worth using the imagery? It looks really busy.
emlimageA <- ggRGB(blockimagery[[1]], r = 1, g = 2, b = 3, stretch = 'lin')
emlimageB <- ggRGB(blockimagery[[2]], r = 1, g = 2, b = 3, stretch = 'lin')
emlimageC <- ggRGB(blockimagery[[3]], r = 1, g = 2, b = 3, stretch = 'lin')

A2009 <- emlimageA +
  geom_point(data = subset(transects2018, block == 'A'), aes(x = X, y = Y), inherit.aes = FALSE, size = 2, alpha = 0.8, color = 'black') +
  geom_point(data = subset(transects2009, block == 'A'), aes(x = X, y = Y), inherit.aes = FALSE, size = 2, alpha = 0.8, color = 'white') +
  ggtitle('A') +
  theme_map() +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1,
        plot.title = element_text(size = 12, hjust = 0.5))

B2009 <- emlimageB +
  geom_point(data = subset(transects2018, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 2, alpha = 0.8, color = 'black') +
  geom_point(data = subset(transects2009, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 2, alpha = 0.8, color = 'white') +
  ggtitle('B') +
  theme_map() +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1,
        plot.title = element_text(size = 12, hjust = 0.5))

C2009 <- emlimageC +
  geom_point(data = subset(transects2018, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 2, alpha = 0.8, color = 'black') +
  geom_point(data = subset(transects2009, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 2, alpha = 0.8, color = 'white') +
  ggtitle('C') +
  theme_map() +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1,
        plot.title = element_text(size = 12, hjust = 0.5))

figure <- ggarrange(A2009, B2009, C2009, ncol = 3, nrow = 1)
figure

A2018 <- emlimageA +
  geom_point(data = subset(transects2018, block == 'A'), aes(x = X, y = Y), inherit.aes = FALSE, size = 2) +
  theme_map() +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1)

B2018 <- emlimageB +
  geom_point(data = subset(transects2018, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 2) +
  theme_map() +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1)

C2018 <- emlimageC +
  geom_point(data = subset(transects2018, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 2) +
  theme_map() +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1)

figure <- ggarrange(A2009, B2009, C2009, A2018, B2018, C2018, ncol = 3, nrow = 2)
figure
# geom_point(data = subset(transects2018, block == 'A'), aes(x = X, y = Y), inherit.aes = FALSE, colour = 'blue', size = 3) +

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
                     expand = c(0,0),
                     name = '(m)') +
  scale_y_continuous(limits = c(7085000, 7087000),
                     expand = c(0,0),
                     name = '(m)') +
  theme(axis.title = element_text(size = 12),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1) +
  coord_fixed()
# emlmap

fullmap <- emlmap +
  annotation_custom(grob = akmapgrob, xmin = 389000, ymin = 7085000, xmax = 389500, ymax = 7085500)
fullmap
