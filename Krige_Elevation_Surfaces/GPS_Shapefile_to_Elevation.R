########################################################################################
###                GPS shapefile processing for elevation surface                    ###
###                 Code by HGR 2017                                                 ###
########################################################################################

#Required packages to run the following code
#library(sp)
#library(rgdal)
#library(raster)
#library(dplyr)
#library(stringr)

###Required packages to do this with tidyverse
library(sf)
library(tidyverse)
library(viridis)
library(rvest)
library(ggplot2)
library(readxl)
library(sp)
library(rgdal)
library(gstat)
library(raster)
library(ggmap)
library(rasterVis)
library(ggthemes)
library(ggsn)
library(gridExtra)
library(cowplot)

############## Load most recent gps file and 2017 elevation surface ###################
Points2015 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Old_uncorrected_2009_2015/All_Points_2015_SPCSAK4.shp")
A2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2017K.tif')
B2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2017K.tif')
C2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2017K.tif')
A2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2017Var.tif')
B2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2017Var.tif')
C2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2017Var.tif')
#######################################################################################

# correct elevation on 2015 shapefile using the elevation offset determined in C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/R_Code/Elevation_Corrections/Plot_Subsidence_Uncorrected_to_determine_corrections.R
Points2015 <- Points2015 %>%
  mutate(Elevation = Elevation + 2.07)

# write shapefile
# st_write(Points2015, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2015_SPCSAK4_corrected.shp")

##############################Select data by block##############################
#2015
A2015 <- Points2015 %>% filter(group_ == 'a')
B2015 <- Points2015 %>% filter(group_ == 'b')
C2015 <- Points2015 %>% filter(group_ == 'c')

##############Add coordinate columns (not part of geometry) to sf objects, so that when converted to sp that info will be available for kriging#######################
A2015sp <- A2015 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2015)
B2015sp <- B2015 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2015)
C2015sp <- C2015 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2015)

##############################Plot to make sure subset worked properly##############################
#2015
ggplot(A2015) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(B2015) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(C2015) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

##############################Interpolate a surface using kriging##############################
# To convert to sp, make sure the sf object only has x and y columns (z and m cause problems with conversion to sp and kriging as of 4/2018)
A2015sp <- st_zm(A2015sp, drop = TRUE, what = "ZM")
B2015sp <- st_zm(B2015sp, drop = TRUE, what = "ZM")
C2015sp <- st_zm(C2015sp, drop = TRUE, what = "ZM")

#First, convert to an sp object rather than sf
A2015sp <- as(A2015sp, 'Spatial')
B2015sp <- as(B2015sp, 'Spatial')
C2015sp <- as(C2015sp, 'Spatial')

##############################Calculate and fit variograms##############################
#A2015
A2015.vgm <- variogram(Elevation~X+Y, A2015sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.025, "Exp", 7, 0, anis =  c(90, 0.7))
A2015.fit <- fit.variogram(A2015.vgm, model = vgm.anis)
plot(A2015.vgm, vgm.anis)

A2015x <- seq(min(A2015sp@coords[,1]), max(A2015sp@coords[,1]), length.out = (max(A2015sp@coords[,1])-min(A2015sp@coords[,1]))/2)
A2015y <- seq(min(A2015sp@coords[,2]), max(A2015sp@coords[,2]), length.out = (max(A2015sp@coords[,2])-min(A2015sp@coords[,2]))/2)
A2015grid <- expand.grid(x = A2015x, y = A2015y)
gridded(A2015grid) = ~x+y
A2015grid@proj4string<-CRS(st_crs(Points2015)$proj4string)
plot(A2015grid)

A2015k <-  krige(Elevation~1, A2015sp, A2015grid, A2015.fit)
spplot(A2015k["var1.pred"], main = "ordinary kriging predictions")

ggplot(A2015) +
  geom_tile(data = as.data.frame(A2015k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

# B2015
B2015.vgm <- variogram(Elevation~X+Y, B2015sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.023, "Exp", 4, 0.002, anis =  c(0, 0.8))
B2015.fit <- fit.variogram(B2015.vgm, model = vgm.anis)
plot(B2015.vgm, vgm.anis)

B2015x <- seq(min(B2015sp@coords[,1]), max(B2015sp@coords[,1]), length.out = (max(B2015sp@coords[,1])-min(B2015sp@coords[,1]))/2)
B2015y <- seq(min(B2015sp@coords[,2]), max(B2015sp@coords[,2]), length.out = (max(B2015sp@coords[,2])-min(B2015sp@coords[,2]))/2)
B2015grid <- expand.grid(x = B2015x, y = B2015y)
gridded(B2015grid) = ~x+y
B2015grid@proj4string<-CRS(st_crs(Points2015)$proj4string)
plot(B2015grid)

B2015k <-  krige(Elevation~1, B2015sp, B2015grid, B2015.fit)
spplot(B2015k["var1.pred"], main = "ordinary kriging predictions")

ggplot(B2015) +
  geom_tile(data = as.data.frame(B2015k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

# C2015
C2015.vgm <- variogram(Elevation~X+Y, C2015sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.011, "Exp", 3, 0.002, anis =  c(90, 0.7))
C2015.fit <- fit.variogram(C2015.vgm, model = vgm.anis)
plot(C2015.vgm, vgm.anis)

C2015x <- seq(min(C2015sp@coords[,1]), max(C2015sp@coords[,1]), length.out = (max(C2015sp@coords[,1])-min(C2015sp@coords[,1]))/2)
C2015y <- seq(min(C2015sp@coords[,2]), max(C2015sp@coords[,2]), length.out = (max(C2015sp@coords[,2])-min(C2015sp@coords[,2]))/2)
C2015grid <- expand.grid(x = C2015x, y = C2015y)
gridded(C2015grid) = ~x+y
C2015grid@proj4string<-CRS(st_crs(Points2015)$proj4string)
plot(C2015grid)

C2015k <-  krige(Elevation~1, C2015sp, C2015grid, C2015.fit)
spplot(C2015k["var1.pred"], main = "ordinary kriging predictions")

ggplot(C2015) +
  geom_tile(data = as.data.frame(C2015k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#Convert to raster
A2015raster <- raster(A2015k)
B2015raster <- raster(B2015k)
C2015raster <- raster(C2015k)

# Raster of variance
A2015var <- raster(A2015k, layer = 2)
B2015var <- raster(B2015k, layer = 2)
C2015var <- raster(C2015k, layer = 2)

#Resample all rasters (elevation and variance) to 2017 grid
A2015re <- resample(A2015raster, A2017raster, method = "bilinear")
B2015re <- resample(B2015raster, B2017raster, method = "bilinear")
C2015re <- resample(C2015raster, C2017raster, method = "bilinear")
A2015varre <- resample(A2015var, A2017var, method = "bilinear")
B2015varre <- resample(B2015var, B2017var, method = "bilinear")
C2015varre <- resample(C2015var, C2017var, method = "bilinear")


#Apply correction to necessary years
A2015corrected <- A2015re+2.07
B2015corrected <- B2015re+2.07
C2015corrected <- C2015re+2.07

# #Write tif files
# writeRaster(A2015corrected, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/A2015RatioCorrected.tif", format = "GTiff")
# writeRaster(B2015corrected, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/B2015RatioCorrected.tif", format = "GTiff")
# writeRaster(C2015corrected, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/C2015RatioCorrected.tif", format = "GTiff")
# writeRaster(A2015varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2015Var.tif", format = "GTiff")
# writeRaster(B2015varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2015Var.tif", format = "GTiff")
# writeRaster(C2015varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2015Var.tif", format = "GTiff")
