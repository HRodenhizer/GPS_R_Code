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
############################################################################################################

### Load data ##############################################################################################
# find elevation files in directory
filenames <- list.files("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected", full.names = TRUE, pattern = '^.*.tif$')
# make a list of elevation raster stacks for each block
Elevation <- list(stack(filenames[1:6]), stack(filenames[7:12]), stack(filenames[13:18]))
# load extent data
blocks <- readOGR('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Blocks_Poly.shp')
############################################################################################################

### Create empty rasters for missing years and fill values using a spline ##################################
Elevation_fill <- list()
for (i in 1:length(Elevation)){
  elevation_mask <- mask(Elevation[[i]], blocks, xy = TRUE) # mask the raster to only include cells with values
  empty_raster <- raster(nrows = nrow(elevation_mask), 
                         ncols = ncol(elevation_mask), 
                         ext=extent(elevation_mask),  
                         crs=crs(elevation_mask), 
                         resolution=res(elevation_mask)) # create an empty raster with the correct extent, resolution, coordinate system, etc.
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
                                Elevation[[i]][[6]]) #2018
  Elevation_fill[[i]] <- mask(approxNA(brick_missing_values), blocks, xy = TRUE)
  plot(Elevation_fill[[i]])
  rm(i, elevation_mask, empty_raster, brick_missing_values) # remove the index value and intermediate steps
}
############################################################################################################

### Write raster stacks with all years #####################################################################
# writeRaster(Elevation_fill[[1]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/AElevationStack.tif')
# writeRaster(Elevation_fill[[2]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/BElevationStack.tif')
# writeRaster(Elevation_fill[[3]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/CElevationStack.tif')
############################################################################################################

### Graph Latest Subsidence ################################################################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks', full.names = TRUE)
Elevation_fill <- list(brick(filenames[1]), brick(filenames[2]), brick(filenames[3]))
Fences <- readOGR('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Fences.shp')
Fences <- fortify(Fences) %>%
  mutate(fence = as.numeric(id) + 1,
         block = ifelse(fence <= 2,
                        'A',
                        ifelse(fence >= 5,
                               'C',
                               'B')))


sub_2018 <- list()
Fences_norm <- data.frame()
sub_2018_df <- data.frame()
for (i in 1:length(Elevation_fill)) {
  blocks <- c('A', 'B', 'C')
  sub_2018[[i]] <- as(Elevation_fill[[i]][[10]] - Elevation_fill[[i]][[1]], "SpatialPixelsDataFrame") %>%
    as.data.frame() %>%
    rename(subsidence = layer) %>%
    mutate(year = 2018,
           block = blocks[i])
  min.coords <- sub_2018[[i]] %>%
    summarize(x.min = min(x),
              y.min = min(y))
  temp_sub <- sub_2018[[i]] %>%
    mutate(long.norm = round(x - min.coords$x.min),
           lat.norm = round(y - min.coords$y.min))
  sub_2018_df <- sub_2018_df %>%
    rbind.data.frame(temp_sub)
  temp_fences <- subset(Fences, block == unique(block)[i]) %>%
    mutate(long.norm = round(long - min.coords$x.min),
           lat.norm = round(lat - min.coords$y.min),
           dummy = '')
  Fences_norm <- Fences_norm %>%
    rbind.data.frame(temp_fences)
  rm(i, temp_fences, temp_sub)
}


subsidence_map <- ggplot(sub_2018_df, aes(x=long.norm, y=lat.norm, fill=subsidence)) +
  geom_tile(aes(height = 2, width = 2)) + # have to set height and width due to bug. See note/link above.
  geom_path(data = Fences_norm, aes(x=long.norm, y=lat.norm, group=group, color = dummy), inherit.aes = FALSE) +
  facet_grid(. ~ block) +
  coord_fixed() +
  theme_few() +
  scale_fill_viridis(expression(Delta*" Elevation (m)"),
                     limits = c(-1.0, 0.5),
                     direction = -1) +
  scale_color_manual('Snow Fence',
                     values = 'black') +
  scale_x_continuous(name = 'Distance (m)') +
  scale_y_continuous(name = 'Distance (m)') +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  ggtitle('10 Years of Subsidence at CiPEHR/DryPEHR')

subsidence_map

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Ratio_Corrected_2018.jpg')
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Ratio_Corrected_2018.pdf')
