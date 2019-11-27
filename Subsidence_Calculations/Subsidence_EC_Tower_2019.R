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
library(readxl)
######################################################################################################################

### Load Data ########################################################################################################
points2008 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/TowerFootprint_2008_2009/tower2008.shp')
points2017 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp')
points2019 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2019_Aug_SPCSAK4.shp')
td2017 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 GPS/ALT_Measurements.xlsx')
td2019 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2019 GPS/ec_tower_alt_20190810.xlsx')
dtm2017 <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2017.tif')
######################################################################################################################

### Standardize coordinate systems and select ec tower points only ###################################################
# Verticle transformation of 2008 is complex
# -14.58 is the height of geoid 99. I think this value was applied to a height that was already orthometric during the original processing
# It seems like this could have happened if the the value entered for the base station was actually an orthometric height
# but was marked as an ellipsoidal height.
# Therefore, the following conversions need to be applied to the 2008 file:
# +14.58 to undo the extra conversion and get an orthometric height (in geoid 99)
# +1.81 to convert from geoid 99 to geoid 12B
# -2.0786 to fix the offset in the recorded base station height between 2008 and later years
# total offset is 14.3081
tower2008 <- points2008 %>%
  filter(Lat != 0) %>%
  st_transform(crs = crs(points2017)) %>%
  mutate(Easting = st_coordinates(.)[,1],
         Northing = st_coordinates(.)[,2],
         Elevation = st_coordinates(.)[,3] + 14.3081,
         ALT = dp_avg) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c('Easting', 'Northing', 'Elevation'), remove = FALSE) %>%
  select(id, ALT, Easting, Northing, Elevation)

tower2017 <- points2017 %>%
  filter(as.numeric(as.character(Name)) >= 13000) %>%
  left_join(mutate(td2017, Name = as.factor(Name)), by = c('Name')) %>%
  mutate(Name = as.numeric(as.character(Name)) - 13000) %>%
  mutate(id = ifelse(Name >= 218,
                     Name + 5,
                     ifelse(Name >= 140,
                            Name + 4,
                            ifelse(Name >= 73,
                                   Name + 3,
                                   ifelse(Name >= 24,
                                          Name + 2,
                                          ifelse(Name >= 14,
                                                 Name + 1,
                                                 Name)))))) %>%
  select(id, ALT, Easting, Northing, Elevation)

tower2019 <- points2019 %>%
  filter(as.numeric(as.character(Name)) >= 14000) %>%
  left_join(mutate(td2019, Name = as.factor(point + 14000)), by = c('Name')) %>%
  mutate(id = ifelse(as.numeric(as.character(Name)) < 14227,
                     as.numeric(as.character(Name)) - 14000,
                     as.numeric(as.character(Name)) - 13999),
         ALT = alt) %>%
  select(id, ALT, Easting, Northing, Elevation)

# compare height point by point
tower_summary <- tower2008 %>%
  mutate(year = 2008) %>%
  st_drop_geometry() %>%
  rbind.data.frame(mutate(st_drop_geometry(tower2017), year = 2017)) %>%
  rbind.data.frame(mutate(st_drop_geometry(tower2019), year = 2019)) %>%
  group_by(year) %>%
  summarise(mean.elev = mean(Elevation, na.rm = TRUE),
            se.elev = sd(Elevation)/sqrt(n()),
            lwr = mean.elev - qt(0.975, n() - 1)*se.elev,
            upr = mean.elev + qt(0.975, n() - 1)*se.elev)

diff1 <- tower_summary$mean.elev[2] - tower_summary$mean.elev[1]
diff2 <- tower_summary$mean.elev[3] - tower_summary$mean.elev[1]

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

# make a mask of the tower footprint
ec_mask <- raster(tower_model[[1]]$krige_output[2])
plot(ec_mask)
ec_mask[ec_mask > 0.045] <- NA
ec_mask[ec_mask <= 0.045] <- 1
plot(ec_mask)
plot(tower_sp[[1]], add = TRUE)
# convert elevation surfaces to raster
tower_raster <- map(
  tower_model,
  ~ brick(.x$krige_output) %>%
    mask(ec_mask)
)

plot(tower_raster[[1]][[1]]) # 2008
plot(tower_raster[[2]][[1]]) # 2017
plot(tower_raster[[3]]) # 2019

sub17 <- tower_raster[[2]][[1]] - tower_raster[[1]][[1]]
sub19 <- tower_raster[[3]][[1]] - tower_raster[[1]][[1]]
diff_17_19 <- tower_raster[[3]][[1]]-tower_raster[[2]][[1]]

plot(sub17)
plot(sub19)
plot(diff_17_19)
plot(tower_sp[[1]], add = TRUE, col = 'red')
plot(tower_sp[[2]], add = TRUE, col = 'blue')
plot(tower_sp[[3]], add = TRUE, col = 'green')

dtm_mask <- dtm2017 %>%
  projectRaster(ec_mask) %>%
  mask(ec_mask)

plot(dtm_mask)
plot(tower_sp[[2]], add = TRUE, col = 'red')

diff <- tower_raster[[2]][[1]] - dtm_mask # 2017 ec data is the second in the list, and dtm data from 2017
plot(diff)
test <- as.data.frame(diff)
boxplot(diff)
mean_diff <- mean(test$layer, na.rm = TRUE)
min_diff <- min(test$layer, na.rm = TRUE)
max_diff <- max(test$layer, na.rm = TRUE)

base_height <- raster::extract(dtm, filter(points2019, Name == 'base') %>% st_zm())
correction <- base_height - filter(points2019, Name == 'base')$Elevation
######################################################################################################################