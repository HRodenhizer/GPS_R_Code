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