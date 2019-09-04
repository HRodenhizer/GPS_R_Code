######################################################################################################################
###             GPS points to elevation surfaces and subsidence from 2008-2019 for the EC Tower                    ###
###                                       Code by HGR 9/2019                                                       ###
######################################################################################################################

### Load Libraries ###################################################################################################
library(sf)
library(sp)
library(automap)
library(raster)
library(tidyverse)
######################################################################################################################

### Load Data ########################################################################################################
points2008 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/TowerFootprint_2008_2009/tower2008.shp')
points2017 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp')
points2019 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_08_2019_SPCSAK4.shp')
######################################################################################################################

### Standardize coordinate systems and select ec tower points only ###################################################
tower2008 <- points2008 %>%
  filter(Lat != 0) %>%
  st_transform(crs = crs(points2017)) %>%
  mutate(Easting = st_coordinates(.)[,1],
         Northing = st_coordinates(.)[,2],
         Elevation = st_coordinates(.)[,3]) %>%
  select(id, Easting, Northing, Elevation)

tower2017 <- points2017 %>%
  filter(as.numeric(as.character(Name)) >= 13000) %>%
  mutate(id = as.numeric(as.character(Name)) - 13000) %>%
  mutate(id = ifelse(id >= 218,
                     id + 5,
                     ifelse(id >= 140,
                            id + 4,
                            ifelse(id >= 73,
                                   id + 3,
                                   ifelse(id >= 24,
                                          id + 2,
                                          ifelse(id >= 14,
                                                 id + 1,
                                                 id)))))) %>%
  select(id, Easting, Northing, Elevation)

tower2019 <- points2019 %>%
  filter(as.numeric(as.character(Name)) >= 14000) %>%
  mutate(id = as.numeric(as.character(Name)) - 14000) %>%
  select(id, Easting, Northing, Elevation)
######################################################################################################################

### Create a list of files and krige elevation surfaces ##############################################################
tower <- list(
  tower2008,
  tower2017,
  tower2019
)

# create a list of sp objects to use in kriging
tower_sp <- map(
  tower,
  ~ .x %>%
    st_zm() %>% # remove z geometry (because it can't be handled during kriging)
    as('Spatial')
)

# create grid to krige over
x <- seq(min(tower_sp[[1]]@coords[,1]-10), max(tower_sp[[1]]@coords[,1]+10), length.out = (max(tower_sp[[1]]@coords[,1]+10)-min(tower_sp[[1]]@coords[,1]-10))/10)
y <- seq(min(tower_sp[[1]]@coords[,2]-10), max(tower_sp[[1]]@coords[,2]+10), length.out = (max(tower_sp[[1]]@coords[,2]+10)-min(tower_sp[[1]]@coords[,2]-10))/10)
grid <- expand.grid(x = x, y = y)
gridded(grid) = ~x+y
grid@proj4string<-CRS(st_crs(points2017)$proj4string)
# check that the grid looks right
plot(grid)
plot(tower_sp[[1]], add = TRUE, col = 'red')
plot(tower_sp[[2]], add = TRUE, col = 'blue')
plot(tower_sp[[3]], add = TRUE, col = 'green')

# krige surfaces
tower_model <- map(
  tower_sp,
  ~ autoKrige(Elevation ~ 1,
              .x,
              grid)
)

# convert elevation surfaces to raster
tower_raster <- map(
  tower_model,
  ~ brick(.x$krige_output)
)

plot(tower_raster[[1]])
plot(tower_raster[[2]])
plot(tower_raster[[3]])

######################################################################################################################