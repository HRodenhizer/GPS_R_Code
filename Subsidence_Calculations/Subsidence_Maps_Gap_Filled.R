############################################################################################################
###                            Elevation Maps for Gap Filled Years                                       ###
###                               Code written by HGR, Oct 2018                                          ###
### This code takes the corrected elvation surfaces created from GPS files and uses a cell by cell linear###
### model to predict the elevation in years without GPS data.                                            ###
############################################################################################################

### Load Packages ##########################################################################################
library(raster)
library(tidyverse)
library(rgdal)
library(sp)
library(sf)
library(ggthemes)
library(viridis)
############################################################################################################

### Load data ##############################################################################################
# find elevation files in directory
filenames <- list.files("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected", full.names = TRUE, pattern = '[[:alpha:]]{1}[[:digit:]]{4}[[:alpha:]]+\\.tif$')
# make a list of elevation raster stacks for each block
Elevation <- list(brick(stack(filenames[1:7])), brick(stack(filenames[8:14])), brick(stack(filenames[15:21])))
blocks <- readOGR('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Blocks_Poly.shp')
blocks11 <- readOGR('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Blocks_Poly_2011.shp')
############################################################################################################

### Write raster stacks of unfilled unclipped data #########################################################
# names <- paste("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/", c('A', 'B', 'C'), 'ElevStack_unfilled_unclipped.tif', sep = '')
# walk2(Elevation, names, ~writeRaster(.x, .y))
############################################################################################################

### Create empty rasters for missing years and fill values using a spline ##################################
Elevation_fill <- list()
for (i in 1:length(Elevation)){
  elevation <- Elevation[[i]] 
  empty_raster <- raster(nrows = nrow(elevation), 
                         ncols = ncol(elevation), 
                         ext=extent(elevation),  
                         crs=crs(elevation), 
                         resolution=res(elevation)) # create an empty raster with the correct extent, resolution, coordinate system, etc.
  values(empty_raster) <- NA # fill missing values with NA
  brick_missing_values <- brick(Elevation[[i]][[1]], #2009
                                empty_raster, #2010
                                Elevation[[i]][[2]], #2011
                                empty_raster, #2012
                                empty_raster, #2013
                                empty_raster, #2014
                                Elevation[[i]][[3]], #2015
                                Elevation[[i]][[4]], #2016
                                Elevation[[i]][[5]], #2017
                                Elevation[[i]][[6]], #2018
                                Elevation[[i]][[7]]) #2019
  Elevation_fill[[i]] <- approxNA(brick_missing_values)
  plot(Elevation_fill[[i]])
  rm(i, empty_raster, brick_missing_values) # remove the index value and intermediate steps
}
# because these are unclipped there will be some cells around the margins with bad filled data where a value
# was fit witout data on both sides (temporally)
############################################################################################################

### Write filled unclipped rasters #########################################################################
# names <- paste("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/", c('A', 'B', 'C'), 'ElevStack_filled_unclipped.tif', sep = '')
# walk2(Elevation_fill, names, ~writeRaster(.x, .y))
############################################################################################################

### clip filled rasters to areas with data for all years ###################################################
Elevation_fill_clipped <- map(Elevation_fill, ~mask(.x, blocks, xy = TRUE)) # why does this not mask when I use walk instead of map?
############################################################################################################

### Write filled clipped rasters #########################################################################
# names <- paste("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/", c('A', 'B', 'C'), 'ElevStack_filled_clipped.tif', sep = '')
# walk2(Elevation_fill_clipped, names, ~writeRaster(.x, .y))
############################################################################################################

### Calculate Latest Subsidence (CiPEHR) ###################################################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks', 
                        full.names = TRUE,
                        pattern = '[ABC]ElevStack_filled_clipped.tif')
Elevation_fill <- list(brick(filenames[which(str_detect(filenames, 'AElevStack'))]), 
                       brick(filenames[which(str_detect(filenames, 'BElevStack'))]), 
                       brick(filenames[which(str_detect(filenames, 'CElevStack'))]))
names(Elevation_fill) <- list('Block A', 'Block B', 'Block C')
names(Elevation_fill[[1]]) <- paste('A', seq(2009, 2019), sep = '_')
names(Elevation_fill[[2]]) <- paste('B', seq(2009, 2019), sep = '_')
names(Elevation_fill[[3]]) <- paste('C', seq(2009, 2019), sep = '_')

Fences <- readOGR('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Fences.shp')
Fences <- fortify(Fences) %>%
  mutate(fence = as.numeric(id) + 1,
         block = ifelse(fence <= 2,
                        'A',
                        ifelse(fence >= 5,
                               'C',
                               'B')))

sub <- list()
Fences_norm <- data.frame()
sub_df <- data.frame()
avg_sub <- data.frame()
for (i in 1:length(Elevation_fill)) {
  block.names <- c('A', 'B', 'C')
  sub[[i]] <- calc(Elevation_fill[[i]], function(x) {x - x[[1]]})
  temp_sub <- sub[[i]] %>%
    as.data.frame(xy = TRUE) %>%
    gather(key = key, value = subsidence, 3:13) %>%
    separate(key, into = c('block', 'year')) %>%
    mutate(year = as.numeric(year))
  min.coords <- temp_sub %>%
    summarize(x.min = min(x),
              y.min = min(y))
  temp_sub <- temp_sub %>%
    mutate(long.norm = round(x - min.coords$x.min),
           lat.norm = round(y - min.coords$y.min))
  sub_df <- sub_df %>%
    rbind.data.frame(temp_sub)
  temp_fences <- subset(Fences, block == unique(block)[i]) %>%
    mutate(long.norm = round(long - min.coords$x.min),
           lat.norm = round(lat - min.coords$y.min),
           dummy = '')
  Fences_norm <- Fences_norm %>%
    rbind.data.frame(temp_fences)
    avg_sub <- avg_sub %>%
      rbind.data.frame(data.frame(key = names(sub[[i]]), 
                                  avg.sub = cellStats(sub[[i]], mean, na.rm = TRUE)) %>%
                         separate(key, into = c('block', 'year')) %>%
                         mutate(year = as.numeric(year)))
      
  rm(i, temp_fences, temp_sub)
}

# writeRaster(sub[[1]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/ASubStack2009-2019.tif')
# writeRaster(sub[[2]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/BSubStack2009-2019.tif')
# writeRaster(sub[[3]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/CSubStack2009-2019.tif')
#############################################################################################################

### Calculate Latest Subsidence (DryPEHR) ###################################################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks', 
                        full.names = TRUE,
                        pattern = '[ABC]ElevStack_filled_unclipped.tif')
Elevation_fill_drypehr <- list(brick(filenames[which(str_detect(filenames, 'AElevStack'))]), 
                               brick(filenames[which(str_detect(filenames, 'BElevStack'))]), 
                               brick(filenames[which(str_detect(filenames, 'CElevStack'))]))
names(Elevation_fill_drypehr) <- list('Block A', 'Block B', 'Block C')
names(Elevation_fill_drypehr[[1]]) <- paste('A', seq(2009, 2019), sep = '_')
names(Elevation_fill_drypehr[[2]]) <- paste('B', seq(2009, 2019), sep = '_')
names(Elevation_fill_drypehr[[3]]) <- paste('C', seq(2009, 2019), sep = '_')

Elevation_fill_drypehr <- map(Elevation_fill_drypehr, ~subset(.x, 3:11))
Elevation_fill_drypehr_clipped <- map(Elevation_fill_drypehr, ~mask(.x, blocks11))

sub_drypehr <- list()
Fences_norm_drypehr <- data.frame()
sub_df_drypehr <- data.frame()
avg_sub_drypehr <- data.frame()
for (i in 1:length(Elevation_fill_drypehr_clipped)) {
  block.names <- c('A', 'B', 'C')
  sub_drypehr[[i]] <- calc(Elevation_fill_drypehr_clipped[[i]], function(x) {x - x[[1]]})
  temp_sub <- sub_drypehr[[i]] %>%
    as.data.frame(xy = TRUE) %>%
    gather(key = key, value = subsidence, 3:11) %>%
    separate(key, into = c('block', 'year')) %>%
    mutate(year = as.numeric(year))
  min.coords <- temp_sub %>%
    summarize(x.min = min(x),
              y.min = min(y))
  temp_sub <- temp_sub %>%
    mutate(long.norm = round(x - min.coords$x.min),
           lat.norm = round(y - min.coords$y.min))
  sub_df_drypehr <- sub_df_drypehr %>%
    rbind.data.frame(temp_sub)
  temp_fences <- subset(Fences, block == unique(block)[i]) %>%
    mutate(long.norm = round(long - min.coords$x.min),
           lat.norm = round(lat - min.coords$y.min),
           dummy = '')
  Fences_norm <- Fences_norm %>%
    rbind.data.frame(temp_fences)
  avg_sub_drypehr <- avg_sub_drypehr %>%
    rbind.data.frame(data.frame(key = names(sub[[i]]), 
                                avg.sub = cellStats(sub[[i]], mean, na.rm = TRUE)) %>%
                       separate(key, into = c('block', 'year')) %>%
                       mutate(year = as.numeric(year)))
  
  rm(i, temp_fences, temp_sub)
}

# writeRaster(sub_drypehr[[1]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/ASubStack2009-2019_drypehr.tif')
# writeRaster(sub_drypehr[[2]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/BSubStack2009-2019_drypehr.tif')
# writeRaster(sub_drypehr[[3]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/CSubStack2009-2019_drypehr.tif')
#############################################################################################################

### Graph subsidence (CiPEHR only) ##########################################################################
subsidence_map_all_years <- ggplot(sub_df, aes(x=long.norm, y=lat.norm, fill=subsidence)) +
  geom_tile(aes(height = 2, width = 2)) + # have to set height and width due to bug. See note/link above.
  geom_path(data = Fences_norm, aes(x = long.norm, y = lat.norm, group = group, color = dummy), inherit.aes = FALSE) +
  # geom_text(data = avg_sub, aes(x = 11, y = 37.5, label = paste('Avg Sub = ', round(avg.sub, 2), 'm')), inherit.aes = FALSE) +
  facet_grid(block ~ year) +
  theme_few() +
  scale_fill_viridis(expression(Delta*" Elevation (m)"),
                     limits = c(-1.0, 0.2),
                     na.value = 'transparent') +
  scale_color_manual('Snow Fence',
                     values = 'black') +
  scale_x_continuous(name = 'Distance (m)') +
  scale_y_continuous(name = 'Distance (m)') +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        title = element_text(size = 16)) +
  coord_fixed() +
  ggtitle('10 Years of Subsidence at CiPEHR')

subsidence_map_all_years

subsidence_map <- ggplot(filter(sub_df, year == 2019), aes(x=long.norm, y=lat.norm, fill=subsidence)) +
  geom_tile(aes(height = 2, width = 2)) + # have to set height and width due to bug. See note/link above.
  geom_path(data = Fences_norm, aes(x = long.norm, y = lat.norm, group = group, color = dummy), inherit.aes = FALSE) +
  # geom_text(data = avg_sub, aes(x = 11, y = 37.5, label = paste('Avg Sub = ', round(avg.sub, 2), 'm')), inherit.aes = FALSE) +
  facet_grid(. ~ block) +
  theme_few() +
  scale_fill_viridis(expression(Delta*" Elevation (cm)"),
                     limits = c(-1.0, 0.5),
                     breaks = c(-1.0, -0.5, 0, 0.5),
                     labels = c(-100, -50, 0, 50),
                     na.value = 'transparent') +
  scale_color_manual('Snow Fence',
                     values = 'black') +
  scale_x_continuous(name = 'Distance (m)') +
  scale_y_continuous(name = 'Distance (m)') +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        title = element_text(size = 16)) +
  coord_fixed() +
  ggtitle('10 Years of Subsidence at CiPEHR')

subsidence_map

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Ratio_Corrected_2018.jpg', width = 14, height = 5)
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Ratio_Corrected_2018.pdf', width = 14, height = 5)
############################################################################################################