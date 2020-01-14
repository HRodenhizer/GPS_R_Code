##############################################################################################################
###                                Overview Map of GPS Survey                                              ###
###                                    Code by HGR 2/2019                                                  ###
##############################################################################################################

### Load Libraries ###########################################################################################
library(sf)
library(sp)
library(raster)
library(ggmap)
library(ggthemes)
library(viridis)
library(RStoolbox)
library(ggpubr)
library(Rmisc)
library(tidyverse)
##############################################################################################################

### Load Data ################################################################################################
points2018 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2018_SPCSAK4.shp')
emlpoints <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Sites.shp')
cores09 <- c('ww1.2', 'ww1.3', 'ww2.1', 'ww2.4', 'ww3.2', 'ww3.4', 'ww4.1', 'ww4.3', 'ww5.2', 'ww5.3', 'ww6.2', 'ww6.4')
cores <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/water_wells.shp') %>%
  filter(Name %in% cores09)
fences <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Fences.shp')
emldtm <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2017.tif') %>%
  crop(y = extent(389000, 391000, 7085000, 7087000)) # the whole thing - needs to be clipped
# filenames <- c('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2018/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7085000_image.tif',
#                'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2018/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7086000_image.tif')
# emlrgb <- raster::merge(brick(filenames[1]),
#                         brick(filenames[2])) # only the two tiles right around cipehr
# rm(filenames)
##############################################################################################################

### Select/clip to CiPEHR ####################################################################################
### save crs to use for all data
spcsak4 <- st_crs(points2018)[[2]]


### create data frames used to clip and normalize all block data for graphing
# select only transects from point data and format
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

# create a data frame with transect corner points buffered to have equal areas among the 3 blocks
# used to clip the rgb data
coords_bbox <- transects2018 %>%
  as.data.frame() %>%
  group_by(block) %>%
  summarise(xmin = min(X),
            xmax = max(X),
            ymin = min(Y),
            ymax = max(Y)) %>%
  mutate(xdist = xmax - xmin,
         ydist = ymax - ymin,
         xmin = ifelse(xdist < 50, # adjust the min and max coordinates out to include an entire 50 meter distance in both x and y directions
                       xmin - (50 - xdist)/2,
                       NA),
         xmax = ifelse(xdist < 50,
                       xmax + (50 - xdist)/2,
                       NA),
         ymin = ifelse(ydist < 50,
                       ymin - (50 - ydist)/2,
                       NA),
         ymax = ifelse(ydist < 50,
                       ymax + (50 - ydist)/2,
                       NA)) %>%
  gather(key = 'x.min.max',
         value = 'x',
         xmin:xmax) %>%
  gather(key = 'y.min.max',
         value = 'y',
         ymin:ymax) %>%
  arrange(block, x, y) %>%
  select(-x.min.max, -y.min.max, - xdist, -ydist) %>%
  st_as_sf(coords = c('x', 'y'), crs = spcsak4, remove = FALSE)

# data frame with min coordinates
# used to normalize all block data
coords_norm <- coords_bbox %>%
  st_drop_geometry() %>%
  group_by(block) %>%
  summarise(xmin = min(x),
            ymin = min(y))


### create data frame with tower and block locations
# start getting three block locations
cipehrblocks <- emlpoints %>%
  filter(Exp == 'CiPEHR') %>%
  st_transform(crs = st_crs(emldtm))

# overall cipehr location
# used in overview map of ak
cipehr <- cipehrblocks %>%
  st_transform(crs = 4326) %>%
  cbind.data.frame(st_coordinates(.)) %>%
  group_by(Exp) %>%
  summarise(X = mean(X),
            Y = mean(Y))

# final formatting for location of three blocks
cipehrblocks <- cipehrblocks %>%
  cbind.data.frame(st_coordinates(.))

# cipehr sites and ec tower
# used to show site locations over hillshade of eml area
blocks_tower <- cipehrblocks %>%
  rbind.data.frame(data.frame(Id = 0,
                              Exp = 'EC Tower',
                              Block = 'Eddy Covariance Tower',
                              geometry = st_sfc(st_point(c(389400.000000000, 7085595.000000000))),
                              X = 389400.000000000,
                              Y = 7085595.000000000))


### create hillshade of eml area from dtm
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



# clip emlrgb to three separate rasters for each block and transform to SPCS AK 4
### this whole section will take awhile! ###
# the output has been saved as an r file and can be loaded in directly now!


# emlrgb <- reclassify(emlrgb, cbind(NA, -9999))
# emlrgb <- projectRaster(emlrgb, crs = spcsak4)
# 
# blockimagery <- list()
# for (i in 1:length(cipehrblocks$Block)) {
#  blockimagery[[i]] <- crop(emlrgb, as(subset(coords_bbox, block == c('A', 'B', 'C')[i]), 'Spatial'))
#  rm(i)
# }

# saveRDS(blockimagery, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/blockimagery.rds')
blockimagery <- readRDS('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/blockimagery.rds')

# convert blockimagery to dataframe, normalize, and convert back to raster
blockimagery_df_norm <- map2(blockimagery,
                            blocks,
                           ~ as(.x, 'SpatialPixelsDataFrame') %>%
                             as.data.frame() %>%
                             mutate(block = .y) %>%
                             left_join(coords_norm, by = c('block')) %>%
                             mutate(x = x - xmin,
                                    y = y - ymin) %>%
                             select(-xmin, -ymin))

blockimagery_norm <- list()
for (i in 1:length(blockimagery_df_norm)) {
  temp1 <- blockimagery_df_norm[[i]] %>%
    select(x, y, r = layer.1) %>%
    rasterFromXYZ()
  temp2 <- blockimagery_df_norm[[i]] %>%
    select(x, y, g = layer.2) %>%
    rasterFromXYZ()
  temp3 <- blockimagery_df_norm[[i]] %>%
    select(x, y, b = layer.3) %>%
    rasterFromXYZ()
  blockimagery_norm[[i]] <- brick(temp1, temp2, temp3)
  rm(temp1, temp2, temp3)
}
##############################################################################################################

### Plot block location over hillshade #######################################################################
# better to have 2009 and 2018 separate or together? Is it worth using the imagery? It looks really busy.
# emlimageA <- ggRGB(blockimagery[[1]], r = 1, g = 2, b = 3, stretch = 'lin', alpha = 0.7)
# emlimageB <- ggRGB(blockimagery[[2]], r = 1, g = 2, b = 3, stretch = 'lin', alpha = 0.7)
# emlimageC <- ggRGB(blockimagery[[3]], r = 1, g = 2, b = 3, stretch = 'lin', alpha = 0.7)

emlimages <- map(blockimagery_norm,
                 ~ggRGB(.x, r = 1, g = 2, b = 3, stretch = 'lin', alpha = 0.7))

# A2009 <- emlimageA +
#   geom_point(data = subset(transects2016, block == 'A'), aes(x = X, y = Y, color = '2016'), inherit.aes = FALSE, size = 1) +
#   geom_point(data = subset(transects2015, block == 'A'), aes(x = X, y = Y, color = '2015'), inherit.aes = FALSE, size = 1, alpha = 0.5) +
#   geom_point(data = subset(transects2011, block == 'A'), aes(x = X, y = Y, color = '2011'), inherit.aes = FALSE, size = 1, alpha = 0.5) +
#   geom_point(data = subset(transects2009, block == 'A'), aes(x = X, y = Y, color = '2009'), inherit.aes = FALSE, size = 1, alpha = 0.5) +
#   scale_color_manual(values = c('2009' = 'white', '2011' = '#CCCCCC', '2015' = '#666666', '2016' = 'black')) +
#   ggtitle('A') +
#   theme_few() +
#   scale_x_continuous(limits = c(537922, 537972),
#                      breaks = c(537922, 537932, 537942, 537952, 537962, 537972),
#                      labels = c(seq(0, 50, 10)),
#                      expand = c(0, 0)) +
#   scale_y_continuous(limits = c(1100957, 1101007),
#                      breaks = c(1100957, 1100967, 1100977, 1100987, 1100997, 1101007),
#                      labels = c(seq(0, 50, 10)),
#                      name = '2009 - 2016',
#                      expand = c(0, 0)) +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 12), axis.text.x  = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         aspect.ratio = 1,
#         plot.title = element_text(size = 12, hjust = 0.5),
#         legend.title = element_blank(),
#         legend.justification = c(0, 0),
#         legend.position = c(0.0025, 0),
#         legend.text = element_text(size = 8),
#         legend.margin = margin(t = 0, b = 2, r = 2),
#         legend.key.height = unit(0.5, 'line'),
#         plot.margin = unit(c(10, 0, 0, 0), "mm"))
# 
# B2009 <- emlimageB +
#   geom_point(data = subset(transects2016, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, color = 'black') +
#   geom_point(data = subset(transects2015, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = '#666666') +
#   geom_point(data = subset(transects2011, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = '#CCCCCC') +
#   geom_point(data = subset(transects2009, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = 'white') +
#   ggtitle('B') +
#   theme_few() +
#   scale_x_continuous(limits = c(537983, 538028),
#                      breaks = c(537983, 537993, 538003, 538013, 538023),
#                      labels = c(seq(0, 40, 10)),
#                      expand = c(0, 0)) +
#   scale_y_continuous(limits = c(1101090, 1101142),
#                      breaks = c(1101090, 1101100, 1101110, 1101120, 1101130, 1101140),
#                      labels = c(seq(0, 50, 10)),
#                      name = '2009 - 2016',
#                      expand = c(0, 0)) +
#   theme(axis.title = element_blank(),
#         axis.text.x  = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         aspect.ratio = 1,
#         plot.title = element_text(size = 12, hjust = 0.5),
#         plot.margin = unit(c(10, 0, 0, 0), "mm"))
# 
# C2009 <- emlimageC +
#   geom_point(data = subset(transects2016, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, color = 'black') +
#   geom_point(data = subset(transects2015, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = '#666666') +
#   geom_point(data = subset(transects2011, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = '#CCCCCC') +
#   geom_point(data = subset(transects2009, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1, alpha = 0.5, color = 'white') +
#   ggtitle('C') +
#   theme_few() +
#   scale_x_continuous(limits = c(538103, 538149),
#                      breaks = c(538103, 538113, 538123, 538133, 538143),
#                      labels = c(seq(0, 40, 10)),
#                      expand = c(0, 0)) +
#   scale_y_continuous(limits = c(1101012, 1101062),
#                      breaks = c(1101012, 1101022, 1101032, 1101042, 1101052, 1101062),
#                      labels = c(seq(0, 50, 10)),
#                      name = '2009 - 2016',
#                      expand = c(0, 0)) +
#   theme(axis.title = element_blank(),
#         axis.text.x  = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         aspect.ratio = 1,
#         plot.title = element_text(size = 12, hjust = 0.5),
#         plot.margin = unit(c(10, 0, 0, 0), "mm"))

# figure <- ggarrange(A2009, B2009, C2009, ncol = 3, nrow = 1)
# figure

# A2018 <- emlimageA +
#   geom_point(data = subset(transects2018, block == 'A'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1) +
#   theme_few() +
#   scale_x_continuous(limits = c(537922, 537972),
#                      breaks = c(537922, 537932, 537942, 537952, 537962, 537972),
#                      labels = c(seq(0, 50, 10)),
#                      expand = c(0, 0)) +
#   scale_y_continuous(limits = c(1100957, 1101007),
#                      breaks = c(1100957, 1100967, 1100977, 1100987, 1100997, 1101007),
#                      labels = c(seq(0, 50, 10)),
#                      name = '2017 - 2018',
#                      expand = c(0, 0)) +
#   ggtitle('') +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 12),
#         axis.text.x  = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         aspect.ratio = 1,
#         plot.title = element_text(size = 12, hjust = 0.5),
#         plot.margin = unit(c(0, 0, 10, 0), "mm"))
# 
# B2018 <- emlimageB +
#   geom_point(data = subset(transects2018, block == 'B'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1) +
#   theme_few() +
#   scale_x_continuous(limits = c(537983, 538028),
#                      breaks = c(537983, 537993, 538003, 538013, 538023),
#                      labels = c(seq(0, 40, 10)),
#                      expand = c(0, 0)) +
#   scale_y_continuous(limits = c(1101090, 1101142),
#                      breaks = c(1101090, 1101100, 1101110, 1101120, 1101130, 1101140),
#                      labels = c(seq(0, 50, 10)),
#                      expand = c(0, 0)) +
#   ggtitle('') +
#   theme(axis.title = element_blank(),
#         axis.text.x  = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         aspect.ratio = 1,
#         plot.title = element_text(size = 12, hjust = 0.5),
#         plot.margin = unit(c(0, 0, 10, 0), "mm"))
# 
# C2018 <- emlimageC +
#   geom_point(data = subset(transects2018, block == 'C'), aes(x = X, y = Y), inherit.aes = FALSE, size = 1) +
#   theme_few() +
#   scale_x_continuous(limits = c(538103, 538149),
#                      breaks = c(538103, 538113, 538123, 538133, 538143),
#                      labels = c(seq(0, 40, 10)),
#                      expand = c(0, 0)) +
#   scale_y_continuous(limits = c(1101012, 1101062),
#                      breaks = c(1101012, 1101022, 1101032, 1101042, 1101052, 1101062),
#                      labels = c(seq(0, 50, 10)),
#                      expand = c(0, 0)) +
#   ggtitle('') +
#   theme(axis.title = element_blank(),
#         axis.text.x  = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         aspect.ratio = 1,
#         plot.title = element_text(size = 12, hjust = 0.5),
#         plot.margin = unit(c(0, 0, 10, 0), "mm"))
# 
# transects <- ggarrange(A2009, B2009, C2009, A2018, B2018, C2018, ncol = 3, nrow = 2)
# transects
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/gps_transects.jpg', transects, width = 190, height = 120, units = 'mm')
# geom_point(data = subset(transects2018, block == 'A'), aes(x = X, y = Y), inherit.aes = FALSE, colour = 'blue', size = 3) +

# create an inset to show location in AK
akcenter <- c(-154.055, 64.603)
  # as.numeric(geocode('alaska, usa'))
akmap <- ggmap(get_googlemap(center = akcenter, zoom = 4, maptype = 'satellite', region = '.us'))
akmap

akmapgrob <- ggplotGrob(akmap +
  geom_point(data = cipehr, aes(x = X, y = Y), inherit.aes = FALSE, colour = '#cc3300') +
  theme_map() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0),"null"),
        panel.background = element_rect(fill = NULL)))

# create an inset to show the blocks
fences <- fences %>%
  mutate(fence = as.numeric(Id),
         block = ifelse(fence <= 2,
                        'A',
                        ifelse(fence >= 5,
                               'C',
                               'B')))

fences_list <- map(blocks,
                   ~filter(fences, block == .x))

cores <- cores %>%
  separate(Name, into = c('fence', 'plot')) %>%
  mutate(fence = as.numeric(str_sub(fence, start = 3)),
         block = ifelse(fence <= 2,
                        'A',
                        ifelse(fence >= 5,
                               'C',
                               'B')),
  treatment = ifelse(plot <= 2,
                     'Control',
                     'Warming')) %>%
  dplyr::select(block, fence, plot, treatment, Easting, Northing) %>%
  st_zm()

cores_list <- map(blocks,
                  ~filter(cores, block == .x))

# why aren't the treatment colors right? Also, why isn't datum working? How can I get it to display the graticules in the crs of the input data?
blocka <- emlimageA +
  geom_sf(data = filter(cores, block == 'A'), aes(color = treatment, fill = treatment), size = 5) +
  geom_sf(data = filter(fences, block == 'A'), size = 2) +
  geom_sf(data = trans_bbox[[1]], fill = NA, color = 'black', linetype = 'dashed', size = 1) +
  coord_sf(datum = spcsak4) +
  scale_colour_manual(values = c("#006699", "#990000"),
                      labels = c('Control', 'Warming'),
                      name = '') +
  scale_fill_manual(values = c("#006699", "#990000"),
                      labels = c('Control', 'Warming'),
                      name = '') +
  scale_x_continuous(name = '',
                     expand = c(0,0)) +
  scale_y_continuous(name = '',
                     expand = c(0,0)) +
  theme_few()
blocka

block_figures1 <- map2(emlimages,
                      cores_list,
                      ~.x +
                        geom_sf(data = .y, aes(color = treatment, fill = treatment), size = 5) +
                        coord_sf(datum = spcsak4) +
                        scale_colour_manual(values = c("#006699", "#990000"),
                                            labels = c('Control', 'Warming'),
                                            name = 'Soil Cores') +
                        scale_fill_manual(values = c("#006699", "#990000"),
                                          labels = c('Control', 'Warming'),
                                          name = 'Soil Cores') +
                        scale_x_continuous(name = 'Longitude (m)',
                                           expand = c(0,0)) +
                        scale_y_continuous(name = 'Latitude (m)',
                                           expand = c(0,0)) +
                        theme_few())

block_figures2 <- map2(block_figures1,
                       fences_list,
                       ~ .x +
                         geom_sf(data = .y, size = 2) +
                         coord_sf(datum = spcsak4))

block_figures3 <- map2(block_figures2,
                       trans_bbox,
                       ~ .x +
                         geom_sf(data = .y, fill = NA, color = 'black', linetype = 'dashed', size = 1) +
                         coord_sf(datum = spcsak4))

legends <- list(FALSE, FALSE, TRUE)
block_figures4 <- map2(block_figures3,
             legends,
            ~ if(.y) {.x + theme(legend.justification =c (1, 0),
                                 legend.position = c(1, 0),
                                 legend.background = element_rect(color="grey30", size=.5))} 
            else {.x + theme(legend.position = "none")})

titles <- list('Block A', 'Block B', 'Block C')
block_figures5 <- map2(block_figures4,
                       titles,
                       ~ .x + ggtitle(.y) + theme(plot.title = element_text(hjust = 0.5)))

block_figures <- Rmisc::multiplot(block_figures5[[1]],
                                  block_figures5[[2]],
                                  block_figures5[[3]],
                           cols = 3)

block_figure_grobs <- map(block_figures5,
                          ~ggplotGrob(.x))

# create main map with hillshade and block and tower locations
emlmap <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = layer)) +
  geom_tile() +
  geom_point(data = blocks_tower, aes(x = X, y = Y, colour = Exp), inherit.aes = FALSE) +
  geom_text(data = blocks_tower, aes(x = X, y = Y, label = Block), colour = 'black', inherit.aes = FALSE,  hjust = -0.2, vjust = -0.2) +
  theme_few() +
  scale_fill_gradient(low = '#000000', high = '#DDDDDD',
                      guide = FALSE) +
  scale_x_continuous(limits = c(389000, 391000),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(7085000, 7087000),
                     expand = c(0,0)) +
  scale_colour_manual(values = c('#cc3300', 'black'),
                      labels = c('CiPEHR', 'Tower'),
                      name = '')+
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8),
        aspect.ratio = 1,
        plot.margin = unit(c(0, 10, 0, 5), "mm")) +
  coord_fixed()

fullmap <- emlmap +
  annotation_custom(grob = akmapgrob, xmin = 389000, ymin = 7085000, xmax = 389500, ymax = 7085500) +
  annotation_custom(grob = block_figure_grobs[[1]], xmin = 389050, xmax = 389600, ymin = 7086450, ymax = 7086950)
fullmap
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/eml_overview.jpg', fullmap, width = 190, height = 190, units = 'mm')
