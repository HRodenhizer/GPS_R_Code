########################################################################################
###                           DEM to microtopography                                 ###
###                              Code by HGR 2018                                    ###
########################################################################################

### Load libraries #####################################################################
library(tidyverse)
library(raster)
########################################################################################

#################### Import raster files################################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/',
                        pattern = '.*unclipped.tif',
                        full.names = TRUE)
elevation <- map(filenames, ~ brick(.x))
rm(filenames)
########################################################################################

### Calculate microtopography ##########################################################
# create a matrix of weights to calculate the mean in an approximate circle with radius 30 m
weights <- matrix(c(rep(0, 5), rep(1, 5),  rep(0, 5),
                    rep(0, 3), rep(1, 9),  rep(0, 3),
                    rep(0, 2), rep(1, 11), rep(0, 2),
                    rep(c(0, rep(1, 13), 0), 2),
                    rep(rep(1, 15), 5),
                    rep(c(0, rep(1, 13), 0), 2),
                    rep(0, 2), rep(1, 11), rep(0, 2),
                    rep(0, 3), rep(1, 9),  rep(0, 3),
                    rep(0, 5), rep(1, 5),  rep(0, 5)),
                    nrow = 15)

# resample elevation to 30 m average
avg_elev <- list(raster(), raster(), raster())
for (i in 1: length(elevation)) {
  for (k in 1:nlayers(elevation[[i]])) {
    temp_raster <- focal(elevation[[i]][[k]], weights, mean, na.rm = TRUE)
    avg_elev[[i]] <- brick(avg_elev[[i]], temp_raster) # this line isn't working
  }
}
########################################################################################