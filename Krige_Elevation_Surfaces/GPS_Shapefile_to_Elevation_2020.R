########################################################################################
###                GPS shapefile processing for elevation surface                    ###
###                           Code by HGR 2020                                       ###
########################################################################################

### Required packages to do this with tidyverse ########################################
library(sf)
library(viridis)
library(readxl)
library(sp)
library(rgdal)
library(gstat)
library(raster)
library(tidyverse)
library(ggthemes)
########################################################################################

### Load most recent gps file and 2017 elevation surface ###############################
Points2019 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2019_Aug_SPCSAK4.shp")
Points2020 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2020_SPCSAK4.shp")
A2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2017K.tif')
crs(A2017raster) <- "+proj=tmerc +lat_0=54 +lon_0=-150 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
B2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2017K.tif')
crs(B2017raster) <- "+proj=tmerc +lat_0=54 +lon_0=-150 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
C2017raster <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2017K.tif')
crs(C2017raster) <- "+proj=tmerc +lat_0=54 +lon_0=-150 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
A2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2017Var.tif')
crs(A2017var) <- "+proj=tmerc +lat_0=54 +lon_0=-150 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
B2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2017Var.tif')
crs(B2017var) <- "+proj=tmerc +lat_0=54 +lon_0=-150 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
C2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2017Var.tif')
crs(C2017var) <- "+proj=tmerc +lat_0=54 +lon_0=-150 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
########################################################################################

# run this first time only
# transform to spcs ak 4 and adjust elevation manually because of export from Trimble Business Center in WGS84
points <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2020 GPS/processing/2020_gps_processing/2020_gps_processing.shp/Points.shp") %>%
  st_transform(st_crs(Points2019)) %>%
  mutate(Name = as.character(Name))

# if there were any point naming mishaps in the field, this is where to fix them - this will be different every time
names <- points$Name
new_names <- names %>%
  replace(list = which(names == 'grad24' |
                         names == 'grad23' |
                         names == 'grad22' |
                         names == 'grad21' |
                         names == 'grad20' |
                         names == 'grad19' |
                         names == 'grad18' |
                         names == 'grad17' |
                         names == 'grad16' |
                         names == 'grad15' |
                         names == 'grad14' |
                         names == 'grad13' |
                         names == 'grad12' |
                         names == 'grad11' |
                         names == 'grad10' |
                         names == 'grad9' |
                         names == 'grad8' |
                         names == 'grad7' |
                         names == 'grad6' |
                         names == 'grad5' |
                         names == 'grad4' |
                         names == 'grad3' |
                         names == 'grad2' |
                         names == 'grad1' |
                         names == 'grad36' |
                         names == 'grad35' |
                         names == 'grad34' |
                         names == 'grad33' |
                         names == 'grad32' |
                         names == 'grad31' |
                         names == 'grad30' |
                         names == 'grad29' |
                         names == 'grad28' |
                         names == 'grad27' |
                         names == 'grad26' |
                         names == 'grad25' |
                         names == 'hobo_fdbm_t' |
                         names == 'hobo_fdbm_s' |
                         names == '6_w_fdbm_t' |
                         names == '6_w_fdbm_s' |
                         names == '5_c_fdbm_t' |
                         names == '5_c_fdbm_s' |
                         names == '4_w_fdbm_t' |
                         names == '4_w_fdbm_s' |
                         names == '4_c_g_real' | 
                         names == '4_c_f_real' |
                         names == '4_c_g' | 
                         names == '4_c_f' |
                         names == '3_c_fdbm_t' |
                         names == '3_c_fdbm_s' |
                         names == '2_w_fdbm_t' |
                         names == '2_w_fdbm_s' | 
                         names == '1_c_fdbn_t' |
                         names == '1_c_fdbm' | 
                         names == '1_bs' | 
                         names == '1_b_n'),
          values = c('grad-24',
                     'grad-23',
                     'grad-22',
                     'grad-21',
                     'grad-20',
                     'grad-19',
                     'grad-18',
                     'grad-17',
                     'grad-16',
                     'grad-15',
                     'grad-14',
                     'grad-13',
                     'grad-12',
                     'grad-11',
                     'grad-10',
                     'grad-9',
                     'grad-8',
                     'grad-7',
                     'grad-6',
                     'grad-5',
                     'grad-4',
                     'grad-3',
                     'grad-2',
                     'grad-1',
                     'grad-36',
                     'grad-35',
                     'grad-34',
                     'grad-33',
                     'grad-32',
                     'grad-31',
                     'grad-30',
                     'grad-29',
                     'grad-28',
                     'grad-27',
                     'grad-26',
                     'grad-25',
                     'hobo_t',
                     'hobo_s',
                     '6_w_t',
                     '6_w_s',
                     '5_c_t',
                     '5_c_s',
                     '4_w_t',
                     '4_w_s',
                     '4_c_g',
                     '4_c_f',
                     '4_b_g',
                     '4_b_f',
                     '3_c_t',
                     '3_c_s',
                     '2_w_t',
                     '2_w_s',
                     '1_c_t',
                     '1_c_s',
                     '1_bs_ww', 
                     '1_bn_ww'))

points2 <- points %>%
  mutate(Name = str_replace_all(new_names, '_', '-'),
         Elevation = Elevation - 12.742,
         GlobalElli = GlobalElli - 12.742,
         numeric_names = as.numeric(Name),
         numeric_names = ifelse(str_detect(Name, 'grad'),
                                as.numeric(str_sub(Name, 6)),
                                ifelse(str_detect(Name, 'tk'),
                                       as.numeric(str_sub(Name, 4)),
                                       numeric_names)),
         type = factor(ifelse(str_detect(Name, 'tk'),
                              'thermokarst',
                              ifelse(str_detect(Name, 'base'),
                                     'base',
                                     ifelse(str_detect(Name, 'grad'),
                                            'gradient',
                                            ifelse(str_detect(Name, 'ck'),
                                                   'check',
                                                   ifelse(str_detect(Name, 'f'),
                                                          'plot',
                                                          ifelse(str_detect(Name, 'g'),
                                                                 'plot',
                                                                 ifelse(str_detect(Name, 'ww'),
                                                                        'waterwell',
                                                                        ifelse(str_detect(Name, 't') | str_detect(Name, 's'),
                                                                               'fdbm',
                                                                               ifelse(as.numeric(Name) > 10000,
                                                                                      'grid',
                                                                                      NA))))))))),
                       levels = c('base', 'check', 'grid', 'plot', 'waterwell', 'fdbm', 'gradient', 'thermokarst'))) %>%
  arrange(type, numeric_names, Name) %>%
  st_zm()
# st_write(points2, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2020_SPCSAK4.shp")

##############################Select data by block##############################
# 2020
Grids2020 <- Points2020 %>%
  rename(Elevation = Elevatn, Northing = Northng) %>%
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
for(j in 1:length(unique(Grids2020$block))) { # iterate for each block
  name <- paste(as.character(unique(Grids2020$block)[[j]]), '2020', sep = '') # create a unique name for each block's output
  name1 <- paste(as.character(unique(Grids2020$block)[[j]]), '2020sp', sep = '')
  tmp <- Grids2020 %>%
    filter(block == as.character(unique(Grids2020$block)[[j]])) # select only the data from block i
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
#a2020
a2020.vgm <- variogram(Elevation~Easting+Northing, a2020sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.03, "Exp", 10, 0, anis =  c(90, 0.8))
a2020.fit <- fit.variogram(a2020.vgm, model = vgm.anis)
plot(a2020.vgm, a2020.fit)

A2020grid <- A2017raster %>%
  as('SpatialPixelsDataFrame')

a2020k <-  krige(Elevation~1, a2020sp, newdata = A2020grid, model = a2020.fit)

ggplot(a2020) +
  geom_tile(data = as.data.frame(a2020k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('a2020')

# B2020
b2020.vgm <- variogram(Elevation~Easting+Northing, b2020sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.023, "Sph", 4, 0.002, anis =  c(0, 0.95))
b2020.fit <- fit.variogram(b2020.vgm, model = vgm.anis)
plot(b2020.vgm, b2020.fit)

B2020grid <- B2017raster %>%
  as('SpatialPixelsDataFrame')

b2020k <-  krige(Elevation~1, b2020sp, B2020grid, b2020.fit)

ggplot(b2020) +
  geom_tile(data = as.data.frame(b2020k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('b2020')

# C2020
c2020.vgm <- variogram(Elevation~Easting+Northing, c2020sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.025, "Sph", 10, 0.003, anis =  c(90, 0.9))
c2020.fit <- fit.variogram(c2020.vgm, model = vgm.anis)
plot(c2020.vgm, c2020.fit)

C2020grid <- C2017raster %>%
  as('SpatialPixelsDataFrame')

c2020k <-  krige(Elevation~1, c2020sp, C2020grid, c2020.fit)

ggplot(c2020) +
  geom_tile(data = as.data.frame(c2020k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('c2020')

#Convert to raster
A2020raster <- raster(a2020k)
B2020raster <- raster(b2020k)
C2020raster <- raster(c2020k)

# Raster of variance
A2020var <- raster(a2020k, layer = 2)
B2020var <- raster(b2020k, layer = 2)
C2020var <- raster(c2020k, layer = 2)

#Resample all rasters (elevation and variance) to 2017 grid
A2020re <- resample(A2020raster, A2017raster, method = "bilinear")
B2020re <- resample(B2020raster, B2017raster, method = "bilinear")
C2020re <- resample(C2020raster, C2017raster, method = "bilinear")
A2020varre <- resample(A2020var, A2017var, method = "bilinear")
B2020varre <- resample(B2020var, B2017var, method = "bilinear")
C2020varre <- resample(C2020var, C2017var, method = "bilinear")

# #Write tif files
# writeRaster(A2020re, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/A2020K.tif", format = "GTiff")
# writeRaster(B2020re, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/B2020K.tif", format = "GTiff")
# writeRaster(C2020re, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/C2020K.tif", format = "GTiff")
# writeRaster(A2020varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2020Var.tif", format = "GTiff")
# writeRaster(B2020varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2020Var.tif", format = "GTiff")
# writeRaster(C2020varre, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2020Var.tif", format = "GTiff")


# Next step: Update Elevation stack
# go to Subsidence_Maps_Gap_Filled.R