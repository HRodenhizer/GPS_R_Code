##############################################################################################################
###                                Overview Map of GPS Survey                                              ###
###                                    Code by HGR 2/2019                                                  ###
##############################################################################################################

### Load Libraries ###########################################################################################
library(sf)
library(tidyverse)
library(sp)
library(raster)
##############################################################################################################

### Load Data ################################################################################################
points2009 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2009_SPCSAK4_corrected.shp')
points2018 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2018_SPCSAK4.shp')
emlpoints <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Sites.shp')
emldtm <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTM_All/eml_dtm')
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/DTMGtif', full.names = TRUE)
neondtm <- raster(filenames[1])
for (i in 2:length(filenames)) {
  neondtm <- raster::merge(neondtm, filenames[i])
}
##############################################################################################################

### Select/clip to CiPEHR ####################################################################################
transects2009 <- points2009 %>%
  filter(type == 'fence' | type == 'trans')

transects2018 <- points2018 %>%
  mutate(Name = as.numeric(Name)) %>%
  filter(Name > 10000 & Name < 13000)

cipehrblocks <- emlpoints %>%
  filter(Exp == 'CiPEHR')

plot(emldtm)
emlslope <- terrain(emldtm, opt = 'slope')
plot(emlslope)
emlaspect <- terrain(emldtm, opt = 'aspect')
plot(emlaspect)
emlhillshd <- hillShade(emlslope, emlaspect)
##############################################################################################################
plot(emldtm)
r <- raster(ncol=3, nrow=3)
r[] <- sqrt(1:ncell(r))
plot(r)
r[3:5] <- NA
test <- as.data.frame(r)
s <- stack(r, r*2)
as.data.frame(s)
as.data.frame(s, na.rm=TRUE)

### Convert raster to dataframe for plotting with ggplot #####################################################
emlhillshd.df <- as.data.frame(emlhillshd, xy = TRUE)
  as(emlshd, "SpatialPixelsDataFrame") %>% # convert to spatial pixels dataframe
  as.data.frame() %>% # convert to dataframe
  rename(value = 1)

### Plot block location over hillshade #######################################################################
