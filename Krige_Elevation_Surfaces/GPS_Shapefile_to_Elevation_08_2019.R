########################################################################################
###                GPS shapefile processing for elevation surface                    ###
###                           Code by HGR 2019                                       ###
########################################################################################

###Required packages to do this with tidyverse
library(sf)
library(viridis)
library(readxl)
library(sp)
library(rgdal)
library(gstat)
library(raster)
library(tidyverse)
library(ggthemes)

############## Load most recent gps file and 2017 elevation surface ###################
Points2018 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2018_SPCSAK4.shp")
Points2019 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2019_Aug_SPCSAK4.shp")
A2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2017K.tif')
B2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2017K.tif')
C2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2017K.tif')
A2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2017Var.tif')
B2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2017Var.tif')
C2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2017Var.tif')
#######################################################################################

# run this first time only
# transform to spcs ak 4 and adjust elevation manually because of export from Trimble Business Center in WGS84
points <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2019 GPS/summer_output/gps_2019_08.shp/Points.shp") %>%
  st_transform(st_crs(Points2018)) %>%
  mutate(Name = as.character(Name))

# if there were any point naming mishaps in the field, this is where to fix them - this will be different every time
names <- points$Name
new_names <- as.numeric(names) %>%
  replace(list = which(!is.na(names) & (names == 10442 | names == 10443)),
          values = c(11001, 11002))
new_names <- new_names %>%
  replace(list = which(is.na(new_names)),
          values = names[which(is.na(new_names))])
new_names <- new_names %>%
  replace(list = which(new_names == 'ck3g' | new_names == 'ck3h'),
          values = c('ck2h', 'ck1i'))

points <- points %>%
  mutate(Name = new_names)
# st_write(points, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_08_2019_SPCSAK4.shp")

##############################Select data by block##############################
#2018
Grids2019 <- Points2019 %>%
  mutate(Name = as.character(Name)) %>%
  mutate(Name = as.numeric(Name)) %>%
  filter(Name != is.na(Name) & Name < 13000) %>%
  arrange(Name) %>%
  mutate(block = as.factor(ifelse(Name >= 10000 & Name < 11000,
                                  'a',
                                  ifelse(Name >= 11000 & Name < 12000,
                                         'b',
                                         ifelse(Name >= 12000 & Name < 13000,
                                                'c',
                                                NA))))) %>%
  filter(!is.na(block))

# create spatial objects containing the points for each block (once for sf objects, once for sp)
for(j in 1:length(unique(Grids2019$block))) { # iterate for each block
  name <- paste(as.character(unique(Grids2019$block)[[j]]), '2019', sep = '') # create a unique name for each block's output
  name1 <- paste(as.character(unique(Grids2019$block)[[j]]), '2019sp', sep = '')
  tmp <- Grids2019 %>%
    filter(block == as.character(unique(Grids2019$block)[[j]])) # select only the data from block i
  assign(name, tmp) # name the sf object
  print(ggplot(tmp) +
          geom_sf(aes(colour = Elevation)) +
          scale_colour_viridis("Elevation") +
          ggtitle(name)) # plot to make sure the subset worked properly
  tmp <- tmp %>%
    st_zm(drop = TRUE, what = "ZM") %>% # get rid of Z and M geometries to convert to sp
    as('Spatial') # convert to sp
  assign(name1, tmp) # add sp object to list
  rm(j, name, name1, tmp) # remove unneccessary objects from environment
}
########################################################################################

##############################Calculate and fit variograms##############################
#a2019
a2019.vgm <- variogram(Elevation~Easting+Northing, a2019sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.03, "Exp", 10, 0, anis =  c(90, 0.8))
a2019.fit <- fit.variogram(a2019.vgm, model = vgm.anis)
plot(a2019.vgm, a2019.fit)

A2019grid <- A2017raster %>%
  as('SpatialPixelsDataFrame')

a2019k <-  krige(Elevation~1, a2019sp, A2019grid, a2019.fit)

ggplot(a2019) +
  geom_tile(data = as.data.frame(a2019k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('a2019')

# B2019
b2019.vgm <- variogram(Elevation~Easting+Northing, b2019sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.023, "Sph", 4, 0.002, anis =  c(0, 0.95))
b2019.fit <- fit.variogram(b2019.vgm, model = vgm.anis)
plot(b2019.vgm, b2019.fit)

B2019grid <- B2017raster %>%
  as('SpatialPixelsDataFrame')

b2019k <-  krige(Elevation~1, b2019sp, B2019grid, b2019.fit)

ggplot(b2019) +
  geom_tile(data = as.data.frame(b2019k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('b2019')

# C2019
c2019.vgm <- variogram(Elevation~Easting+Northing, c2019sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.025, "Sph", 10, 0.003, anis =  c(90, 0.9))
c2019.fit <- fit.variogram(c2019.vgm, model = vgm.anis)
plot(c2019.vgm, c2019.fit)

C2019grid <- C2017raster %>%
  as('SpatialPixelsDataFrame')

c2019k <-  krige(Elevation~1, c2019sp, C2019grid, c2019.fit)

ggplot(c2019) +
  geom_tile(data = as.data.frame(c2019k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('c2019')

#Convert to raster
A2019raster <- raster(a2019k)
B2019raster <- raster(b2019k)
C2019raster <- raster(c2019k)

# Raster of variance
A2019var <- raster(a2019k, layer = 2)
B2019var <- raster(b2019k, layer = 2)
C2019var <- raster(c2019k, layer = 2)

#Resample all rasters (elevation and variance) to 2017 grid
A2019re <- resample(A2019raster, A2017raster, method = "bilinear")
B2019re <- resample(B2019raster, B2017raster, method = "bilinear")
C2019re <- resample(C2019raster, C2017raster, method = "bilinear")
A2019varre <- resample(A2019var, A2017var, method = "bilinear")
B2019varre <- resample(B2019var, B2017var, method = "bilinear")
C2019varre <- resample(C2019var, C2017var, method = "bilinear")

# #Write tif files
# writeRaster(A2019re, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/A2019K.tif", format = "GTiff")
# writeRaster(B2019re, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/B2019K.tif", format = "GTiff")
# writeRaster(C2019re, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/C2019K.tif", format = "GTiff")
# writeRaster(A2019varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2019Var.tif", format = "GTiff")
# writeRaster(B2019varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2019Var.tif", format = "GTiff")
# writeRaster(C2019varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2019Var.tif", format = "GTiff")


# Next step: Update Elevation stack
# go to Subsidence_Maps_Gap_Filled.R