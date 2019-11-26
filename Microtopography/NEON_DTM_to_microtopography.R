########################################################################################
###                           DEM to microtopography                                 ###
###                              Code by HGR 2018                                    ###
########################################################################################

### Load libraries #####################################################################
library(raster)
library(tidyverse)
########################################################################################

#################### Import raster files################################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/',
                        pattern = '.*unclipped.tif',
                        full.names = TRUE)
elevation <- map(filenames, ~ brick(.x))
rm(filenames)
eml_dtm <- merge(raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif/NEON_D19_HEAL_DP3_390000_7085000_DTM.tif'),
                 raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif/NEON_D19_HEAL_DP3_390000_7086000_DTM.tif'))
########################################################################################

### determine and fix offset between neon dtm and site gps #############################
# project eml_dtm into correct cs
eml_dtm_spcsak4 <- list()
elev_diff <- list()
mean <- list()
for (i in 1:length(elevation)){
  eml_dtm_spcsak4[[i]] <- projectRaster(eml_dtm, elevation[[i]])
  elev_diff[[i]] <- elevation[[i]][[9]]-eml_dtm_spcsak4[[i]]
  mean[[i]] <- cellStats(elev_diff[[i]], 'mean')
}
offset <- mean(mean[[1]], mean[[2]], mean[[3]])

eml_dtm_spcsak4 <- projectRaster(eml_dtm, crs = crs(elevation[[1]])) + offset
########################################################################################

### Calculate microtopography ##########################################################
# create a matrix of weights to calculate the mean in a circle with radius 15 m
weights <- focalWeight(eml_dtm_spcsak4, 15, type = 'circle')

# resample elevation to 30 m average
avg_elev <- focal(eml_dtm_spcsak4, w = weights)

# calculate deviance from avg elevation
microtopography <- map(elevation, ~ .x[[9]]-resample(avg_elev, .x, method = 'bilinear'))

# plot microtopography at each block
walk(microtopography, ~plot(.x))

# save files
names <- map(list('/a_microtopography_2017.tif',
                  '/b_microtopography_2017.tif',
                  '/c_microtopography_2017.tif'),
             ~ paste('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Microtopography',
               .x,
               sep = ''))
walk2(microtopography, names, ~ writeRaster(.x, .y))
########################################################################################