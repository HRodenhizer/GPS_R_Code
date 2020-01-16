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

### Select/clip to CiPEHR and normalize data to each block ###################################################
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
            ymax = max(Y))

# create a buffered polygon for clipping images
coords_buffer <- coords_bbox %>%
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
  arrange(block) %>%
  select(-x.min.max, -y.min.max, - xdist, -ydist) %>%
  st_as_sf(coords = c('x', 'y'), crs = spcsak4, remove = FALSE)

# data frame with min coordinates
# used to normalize all block data
coords_min <- coords_buffer %>%
  st_drop_geometry() %>%
  group_by(block) %>%
  summarise(xmin = min(x),
            ymin = min(y))

# create a data frame with the transect corner points normalized
coords_norm <- coords_bbox %>%
  gather(key = 'x.min.max',
         value = 'x',
         xmin:xmax) %>%
  gather(key = 'y.min.max',
         value = 'y',
         ymin:ymax) %>%
  arrange(block) %>%
  full_join(coords_min, by = c('block')) %>%
  mutate(x = round(x - xmin),
         y = round(y - ymin),
         order = rep(c(1, 2, 4, 3), 3)) %>%
  arrange(block, order) %>%
  select(block, x, y) %>%
  group_by(block) %>%
  rbind.data.frame(filter(., x == first(x) & y == first(y))) %>%
  arrange(block)

coords_list <- map(blocks,
                   ~ filter(coords_norm, block == .x))

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
blocks <- list('A', 'B', 'C')
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


### normalize fence and core data
# format the labels for the fences
fences_norm <- fences %>%
  mutate(fence = as.numeric(Id),
         block = ifelse(fence <= 2,
                        'A',
                        ifelse(fence >= 5,
                               'C',
                               'B')))

# extract the fence coordinates from the geometry column
fence_coords <- as.data.frame(st_coordinates(fences_norm)) %>%
  rename(Id = L1)

# join the coordinates to the fence dataset, join the min x and y values for each block, and set each SW corner of the block to the coords 0,0
fences_norm <- fences_norm %>%
  st_drop_geometry() %>%
  full_join(fence_coords, by = c('Id')) %>%
  full_join(coords_min, by = c('block')) %>%
  mutate(x = round(X - xmin),
         y = round(Y - ymin)) %>%
  select(block, fence, x, y)

# make a list with the fence information for each block
fences_list <- map(blocks,
                   ~filter(fences_norm, block == .x))

# format the core data
cores_norm <- cores %>%
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
  st_zm() %>%
  full_join(coords_min, by = c('block')) %>%
  mutate(x = round(Easting - xmin),
         y = round(Northing - ymin)) %>%
  select(block, fence, plot, treatment, x, y)

cores_list <- map(blocks,
                  ~filter(cores_norm, block == .x))
##############################################################################################################

### Plot block location over hillshade #######################################################################
emlimages <- map(blockimagery_norm,
                 ~ggRGB(.x, r = 1, g = 2, b = 3, stretch = 'lin', alpha = 0.7))


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
test <- emlimages[[1]] +
  geom_point(data = cores_list[[1]], aes(x = x, y = y, color = treatment, fill = treatment), size = 5) +
  geom_path(data = fences_list[[1]], aes(x = x, y = y, group = fence), size = 2) +
  geom_path(data = coords_list[[1]], aes(x = x, y = y, group = block), color = 'black', linetype = 'dashed', size = 1) +
  scale_colour_manual(values = c("#006699", "#990000"),
                      labels = c('Control', 'Warming'),
                      name = '') +
  scale_fill_manual(values = c("#006699", "#990000"),
                      labels = c('Control', 'Warming'),
                      name = '') +
  scale_x_continuous(name = 'Distance (m)',
                     breaks = seq(0, 50, 10),
                     labels = seq(0, 50, 10),
                     expand = c(0,0)) +
  scale_y_continuous(name = 'Distance (m)',
                     breaks = seq(0, 50, 10),
                     labels = seq(0, 50, 10),
                     expand = c(0,0)) +
  theme_few()
test

block_figures <- list()
for (i in 1:3) {
  legends <- list(FALSE, FALSE, TRUE)
  titles <- list('Block A', 'Block B', 'Block C')
  temp <- emlimages[[i]] +
    geom_point(data = cores_list[[i]], aes(x = x, y = y, color = treatment, fill = treatment), size = 3) +
    geom_path(data = fences_list[[i]], aes(x = x, y = y, group = fence), size = 1) +
    geom_path(data = coords_list[[i]], aes(x = x, y = y, group = block), color = 'black', linetype = 'dashed', size = 1) +
    scale_colour_manual(values = c("#006699", "#990000"),
                        labels = c('Control', 'Warming'),
                        name = 'Soil Cores') +
    scale_fill_manual(values = c("#006699", "#990000"),
                      labels = c('Control', 'Warming'),
                      name = 'Soil Cores') +
    scale_x_continuous(name = '',
                       limits = c(0, 50),
                       expand = c(0,0)) +
    scale_y_continuous(name = '',
                       limits = c(0, 50),
                       expand = c(0,0)) +
    theme_few() +
    ggtitle(paste(titles[i])) +
    theme(plot.title = element_text(hjust = 0.5,
                                    size = 10),
          text = element_text(size = 8))
  
  if (legends[[i]]) {
    block_figures[[i]] <- temp
  } else {
      block_figures[[i]] <- temp +
        theme(legend.position = "none")
  }
  rm(temp, legends, titles)
}

block_figures


block_figure_grobs <- map(block_figures,
                          ~ggplotGrob(.x))

# create main map with hillshade and block and tower locations
emlmap <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = layer)) +
  geom_tile() +
  geom_point(data = blocks_tower, aes(x = X, y = Y, colour = Exp), inherit.aes = FALSE, size = 3) +
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
  theme(text = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        aspect.ratio = 1,
        plot.margin = unit(c(0, 10, 0, 5), "mm")) +
  coord_fixed()

fullmap <- emlmap +
  annotation_custom(grob = akmapgrob, xmin = 389000, ymin = 7085000, xmax = 389500, ymax = 7085500) +
  annotation_custom(grob = block_figure_grobs[[1]], xmin = 389000, xmax = 389548, ymin = 7086390, ymax = 7087000) +
  annotation_custom(grob = block_figure_grobs[[2]], xmin = 389542, xmax = 390060, ymin = 7086390, ymax = 7087000) +
  annotation_custom(grob = block_figure_grobs[[3]], xmin = 390055, xmax = 391000, ymin = 7086390, ymax = 7087000)
fullmap
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/eml_overview_2.jpg', fullmap, width = 190, height = 160, units = 'mm')
