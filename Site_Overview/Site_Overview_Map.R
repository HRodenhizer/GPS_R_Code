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
points2011 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2011_SPCSAK4_corrected.shp')
points2015 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2015_SPCSAK4_corrected.shp')
points2016 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2016_SPCSAK4.shp')
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

transects2011 <- points2011 %>%
  filter(type == 'fence' | type == 'a' | type == 'b' | type == 'c') %>%
  cbind.data.frame(st_coordinates(.)) %>%
  st_as_sf() %>%
  mutate(block = ifelse(fence == 1 | fence == 2,
                        'A',
                        ifelse(fence == 3 | fence == 4,
                               'B',
                               'C'))) %>%
  dplyr::select(X, Y, block)

# will have to filter geographically
transects2015 <- points2015 %>%
  filter(type == 'fence' | type == 'trans') %>%
  cbind.data.frame(st_coordinates(.)) %>%
  st_as_sf() %>%
  mutate(block = ifelse(fence == 1 | fence == 2,
                        'A',
                        ifelse(fence == 3 | fence == 4,
                               'B',
                               'C'))) %>%
  dplyr::select(X, Y, block)

transects2016 <- points2016 %>%
  st_zm() %>%
  filter(Type == 'fence' | Type == 'trans') %>%
  cbind.data.frame(st_coordinates(.)) %>%
  st_as_sf() %>%
  arrange(X, Y) %>%
  mutate(block = c(rep('A', 212), rep('B', 192), rep('C', 219))) %>%
  dplyr::select(X, Y, block)

# ifelse(nrow(st_crop(., filter(transects2018, block == 'A'))) > 1,
#        'A',
#        ifelse(nrow(st_crop(., filter(transects2018, block == 'B'))) > 1,
#               'B',
#               'C'))

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

# clip emlrgb to three separate rasters for each block and transform to SPCS AK 4 - this whole section will take awhile!
emlrgb <- reclassify(emlrgb, cbind(NA, -9999))
emlrgb <- projectRaster(emlrgb, crs = spcsak4)

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
  geom_point(data = subset(transects2016, block == 'A'), aes(x = X, y = Y, color = '2016'), inherit.aes = FALSE, size = 1) +
  geom_point(data = subset(transects2015, block == 'A'), aes(x = X, y = Y, color = '2015'), inherit.aes = FALSE, size = 1, alpha = 0.5) +
  geom_point(data = subset(transects2011, block == 'A'), aes(x = X, y = Y, color = '2011'), inherit.aes = FALSE, size = 1, alpha = 0.5) +
  geom_point(data = subset(transects2009, block == 'A'), aes(x = X, y = Y, color = '2009'), inherit.aes = FALSE, size = 1, alpha = 0.5) +
  scale_color_manual(values = c('2009' = 'white', '2011' = '#CCCCCC', '2015' = '#666666', '2016' = 'black')) +
  ggtitle('A') +
  theme_few() +
  scale_x_continuous(limits = c(537922, 537972),
                     breaks = c(537922, 537932, 537942, 537952, 537962, 537972),
                     labels = c(seq(0, 50, 10)),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(1100957, 1101007),
                     breaks = c(1100957, 1100967, 1100977, 1100987, 1100997, 1101007),
                     labels = c(seq(0, 50, 10)),
                     name = '2009 - 2016',
                     expand = c(0, 0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12), axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1,
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.title = element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.0025, 0),
        legend.text = element_text(size = 8),
        legend.margin = margin(t = 0, b = 2, r = 2),
        legend.key.height = unit(0.5, 'line'),
        plot.margin = unit(c(10, 0, 0, 0), "mm"))

B2009 <- emlimageB +
  geom_point(data = subset(transects2016, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, color = 'black') +
  geom_point(data = subset(transects2015, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = '#666666') +
  geom_point(data = subset(transects2011, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = '#CCCCCC') +
  geom_point(data = subset(transects2009, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = 'white') +
  ggtitle('B') +
  theme_few() +
  scale_x_continuous(limits = c(537983, 538028),
                     breaks = c(537983, 537993, 538003, 538013, 538023),
                     labels = c(seq(0, 40, 10)),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(1101090, 1101142),
                     breaks = c(1101090, 1101100, 1101110, 1101120, 1101130, 1101140),
                     labels = c(seq(0, 50, 10)),
                     name = '2009 - 2016',
                     expand = c(0, 0)) +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1,
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = unit(c(10, 0, 0, 0), "mm"))

C2009 <- emlimageC +
  geom_point(data = subset(transects2016, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, color = 'black') +
  geom_point(data = subset(transects2015, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = '#666666') +
  geom_point(data = subset(transects2011, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = '#CCCCCC') +
  geom_point(data = subset(transects2009, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = 'white') +
  ggtitle('C') +
  theme_few() +
  scale_x_continuous(limits = c(538103, 538149),
                     breaks = c(538103, 538113, 538123, 538133, 538143),
                     labels = c(seq(0, 40, 10)),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(1101012, 1101062),
                     breaks = c(1101012, 1101022, 1101032, 1101042, 1101052, 1101062),
                     labels = c(seq(0, 50, 10)),
                     name = '2009 - 2016',
                     expand = c(0, 0)) +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1,
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = unit(c(10, 0, 0, 0), "mm"))

# figure <- ggarrange(A2009, B2009, C2009, ncol = 3, nrow = 1)
# figure

A2018 <- emlimageA +
  geom_point(data = subset(transects2018, block == 'A'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1) +
  theme_few() +
  scale_x_continuous(limits = c(537922, 537972),
                     breaks = c(537922, 537932, 537942, 537952, 537962, 537972),
                     labels = c(seq(0, 50, 10)),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(1100957, 1101007),
                     breaks = c(1100957, 1100967, 1100977, 1100987, 1100997, 1101007),
                     labels = c(seq(0, 50, 10)),
                     name = '2017 - 2018',
                     expand = c(0, 0)) +
  ggtitle('') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1,
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = unit(c(0, 0, 10, 0), "mm"))

B2018 <- emlimageB +
  geom_point(data = subset(transects2018, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1) +
  theme_few() +
  scale_x_continuous(limits = c(537983, 538028),
                     breaks = c(537983, 537993, 538003, 538013, 538023),
                     labels = c(seq(0, 40, 10)),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(1101090, 1101142),
                     breaks = c(1101090, 1101100, 1101110, 1101120, 1101130, 1101140),
                     labels = c(seq(0, 50, 10)),
                     expand = c(0, 0)) +
  ggtitle('') +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1,
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = unit(c(0, 0, 10, 0), "mm"))

C2018 <- emlimageC +
  geom_point(data = subset(transects2018, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1) +
  theme_few() +
  scale_x_continuous(limits = c(538103, 538149),
                     breaks = c(538103, 538113, 538123, 538133, 538143),
                     labels = c(seq(0, 40, 10)),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(1101012, 1101062),
                     breaks = c(1101012, 1101022, 1101032, 1101042, 1101052, 1101062),
                     labels = c(seq(0, 50, 10)),
                     expand = c(0, 0)) +
  ggtitle('') +
  theme(axis.title = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        aspect.ratio = 1,
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = unit(c(0, 0, 10, 0), "mm"))

transects <- ggarrange(A2009, B2009, C2009, A2018, B2018, C2018, ncol = 3, nrow = 2)
transects
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/gps_transects.jpg', transects, width = 190, height = 120, units = 'mm')
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
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(7085000, 7087000),
                     expand = c(0,0)) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8),
        aspect.ratio = 1,
        plot.margin = unit(c(0, 10, 0, 5), "mm")) +
  coord_fixed()
# emlmap

fullmap <- emlmap +
  annotation_custom(grob = akmapgrob, xmin = 389000, ymin = 7085000, xmax = 389500, ymax = 7085500)
fullmap
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/eml_overview.jpg', fullmap, width = 190, height = 190, units = 'mm')
