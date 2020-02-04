########################################################################################
###                GPS shapefile processing for elevation surface                    ###
###                 Code by HGR 2017                                                 ###
########################################################################################

###Required packages to do this with tidyverse
library(sf)
library(viridis)
library(ggplot2)
library(readxl)
library(sp)
library(rgdal)
library(gstat)
library(raster)
library(ggthemes)
library(tidyverse)

############## Load most recent gps file and 2017 elevation surface ###################
Points2018 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2018_SPCSAK4.shp")
A2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2017K.tif')
B2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2017K.tif')
C2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2017K.tif')
A2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2017Var.tif')
B2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2017Var.tif')
C2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2017Var.tif')
#######################################################################################

# had to correct elevation due to file being exported from Trimble Business Center in WGS84
# points <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2018 GPS/Post_processing_2018/All_Points_2018.shp/Points.shp") %>%
#   mutate(Elevation = Elevation-12.741669,
#          GlobalElli = GlobalElli-12.741669)
# st_write(points, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2018 GPS/Post_processing_2018/All_Points_2018.shp/Points_v2.shp")

##############################Select data by block##############################
#2018
Grids2018 <- Points2018 %>%
  mutate(Name = as.character(Name)) %>%
  mutate(Name = as.numeric(Name)) %>%
  filter(Name != is.na(Name)) %>%
  arrange(Name) %>%
  mutate(block = as.factor(ifelse(Name >= 10000 & Name < 11000,
                                  'a',
                                  ifelse(Name >= 11000 & Name < 12000,
                                         'b',
                                         'c'))))

# create spatial objects containing the points for each block (once for sf objects, once for sp)
for(j in 1:length(unique(Grids2018$block))) { # iterate for each block
  name <- paste(as.character(unique(Grids2018$block)[[j]]), '2018', sep = '') # create a unique name for each block's output
  name1 <- paste(as.character(unique(Grids2018$block)[[j]]), '2018sp', sep = '')
  tmp <- Grids2018 %>%
    filter(block == as.character(unique(Grids2018$block)[[j]])) # select only the data from block i
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

##############################Calculate and fit variograms##############################
#a2018
a2018.vgm <- variogram(Elevation~Easting+Northing, a2018sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.025, "Exp", 5, 0, anis =  c(90, 0.8))
a2018.fit <- fit.variogram(a2018.vgm, model = vgm.anis)
plot(a2018.vgm, a2018.fit)

A2018grid <- A2017raster %>%
  as('SpatialPixelsDataFrame')

a2018k <-  krige(Elevation~1, a2018sp, A2018grid, a2018.fit)

ggplot(a2018) +
  geom_tile(data = as.data.frame(a2018k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('a2018')

# B2018
b2018.vgm <- variogram(Elevation~Easting+Northing, b2018sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.023, "Exp", 4, 0.002, anis =  c(0, 0.9))
b2018.fit <- fit.variogram(b2018.vgm, model = vgm.anis)
plot(b2018.vgm, b2018.fit)

B2018grid <- B2017raster %>%
  as('SpatialPixelsDataFrame')

b2018k <-  krige(Elevation~1, b2018sp, B2018grid, b2018.fit)

ggplot(b2018) +
  geom_tile(data = as.data.frame(b2018k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('b2018')

# C2018
c2018.vgm <- variogram(Elevation~Easting+Northing, c2018sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.025, "Exp", 10, 0.003, anis =  c(90, 0.95))
c2018.fit <- fit.variogram(c2018.vgm, model = vgm.anis)
plot(c2018.vgm, c2018.fit)

C2018grid <- C2017raster %>%
  as('SpatialPixelsDataFrame')

c2018k <-  krige(Elevation~1, c2018sp, C2018grid, c2018.fit)

ggplot(c2018) +
  geom_tile(data = as.data.frame(c2018k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('c2018')

#Convert to raster
A2018raster <- raster(a2018k)
B2018raster <- raster(b2018k)
C2018raster <- raster(c2018k)

# Raster of variance
A2018var <- raster(a2018k, layer = 2)
B2018var <- raster(b2018k, layer = 2)
C2018var <- raster(c2018k, layer = 2)

#Resample all rasters (elevation and variance) to 2017 grid
A2018re <- resample(A2018raster, A2017raster, method = "bilinear")
B2018re <- resample(B2018raster, B2017raster, method = "bilinear")
C2018re <- resample(C2018raster, C2017raster, method = "bilinear")
A2018varre <- resample(A2018var, A2017var, method = "bilinear")
B2018varre <- resample(B2018var, B2017var, method = "bilinear")
C2018varre <- resample(C2018var, C2017var, method = "bilinear")

# #Write tif files
# writeRaster(A2018re, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2018K.tif", format = "GTiff")
# writeRaster(B2018re, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2018K.tif", format = "GTiff")
# writeRaster(C2018re, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2018K.tif", format = "GTiff")
# writeRaster(A2018varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2018Var.tif", format = "GTiff")
# writeRaster(B2018varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2018Var.tif", format = "GTiff")
# writeRaster(C2018varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2018Var.tif", format = "GTiff")
