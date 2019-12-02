######################################################################################################################
###             GPS points to elevation surfaces and subsidence from 2008-2019 for the EC Tower                    ###
###                                       Code by HGR 9/2019                                                       ###
######################################################################################################################

### Load Libraries ###################################################################################################
library(sf)
library(sp)
library(automap)
library(raster)
library(readxl)
library(tidyverse)
library(viridis)
######################################################################################################################

### Load Data ########################################################################################################
# points2008 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/TowerFootprint_2008_2009/tower2008.shp')
# # check that the shapefile loaded above is the same as the csv loaded below
# points2008_test <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/TowerFootprint_2008_2009/footprint08.csv")
# points2008_test <- st_as_sf(filter(points2008_test, Long != 0), coords = c('Long', 'Lat'), remove = FALSE)
# # check that the shapefile I previously converted to SPCS AK 4 is the same as the shapefile
# points2008_test2 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/TowerFootprint_2008_2009/Tower2008_SPCS/tower2008_SPCS.shp')
points2008 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/TowerFootprint_2008_2009/Tower2008_SPCS/tower2008_SPCS_redo_2019.shp')
st_crs(points2008) <- st_crs(points2017)
points2017 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp')
points2019 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2019_Aug_SPCSAK4.shp')
td2017 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 GPS/ALT_Measurements.xlsx')
td2019 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2019 GPS/ec_tower_alt_20190810.xlsx')
dtm2017 <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2017.tif')
st_crs(points2008) <- st_crs(points2017)
######################################################################################################################

### Test 2008 points in different coordinate systems #################################################################
points2008_spcs <- points2008 %>%
  filter(Long != 0) %>%
  st_zm() %>%
  st_transform(crs = st_crs(points2017)) %>%
  mutate(Lat = st_coordinates(.)[,1],
         Long = st_coordinates(.)[,2])
points2008_test2 <- points2008_test2 %>%
  filter(Long != 0) %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2])

# shapefile in WGS84 and csv are the same:
ggplot(filter(points2008, Long != 0), aes(x = Lat, y = Long)) +
  geom_point() +
  geom_point(data = points2008_test, aes(x = Lat, y = Long), color = 'red', inherit.aes = FALSE)

# shapefile in WGS84 (after conversion to SPCS AK4) and SPcS AK 4 are not the same(!):
ggplot(points2008_spcs, aes(x = Lat, y = Long)) +
  geom_point() +
  geom_point(data = points2008, aes(x = X, y = Y), color = 'red', inherit.aes = FALSE)

# export new file in SPCS AK4
# st_write(points2008_spcs, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/TowerFootprint_2008_2009/Tower2008_SPCS/tower2008_SPCS_redo_2019.shp")
#####################################################################################################################

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
  mutate(Easting = st_coordinates(.)[,1],
         Northing = st_coordinates(.)[,2],
         Elevation = Elev + 14.3081,
         ALT = dp_avg) %>%
  select(id, ALT, Easting, Northing, Elevation)

tower2017 <- points2017 %>%
  st_zm() %>%
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
  st_zm() %>%
  filter(as.numeric(as.character(Name)) >= 14000) %>%
  left_join(mutate(td2019, Name = as.factor(point + 14000)), by = c('Name')) %>%
  mutate(id = ifelse(as.numeric(as.character(Name)) < 14227,
                     as.numeric(as.character(Name)) - 14000,
                     as.numeric(as.character(Name)) - 13999),
         ALT = alt) %>%
  select(id, ALT, Easting, Northing, Elevation)

# compare height as average of point by point
tower_comparison <- tower2008 %>%
  mutate(year = 2008) %>%
  st_drop_geometry() %>%
  rbind.data.frame(mutate(st_drop_geometry(tower2017), year = 2017)) %>%
  rbind.data.frame(mutate(st_drop_geometry(tower2019), year = 2019))

tower_summary <- tower_comparison %>%
  group_by(year) %>%
  summarise(mean.elev = mean(Elevation, na.rm = TRUE),
            se.elev = sd(Elevation)/sqrt(n()),
            lwr = mean.elev - qt(0.975, n() - 1)*se.elev,
            upr = mean.elev + qt(0.975, n() - 1)*se.elev)

diff1 <- tower_summary$mean.elev[2] - tower_summary$mean.elev[1]
diff2 <- tower_summary$mean.elev[3] - tower_summary$mean.elev[1]

# compare heights point by point and plot
tower_comparison_2 <- tower_comparison %>%
  select(id, Elevation, year) %>%
  mutate(year = paste('year_', year, sep = '')) %>%
  spread(key = year, value = Elevation) %>%
  mutate(diff_17 = year_2017 - year_2008,
         diff_19 = year_2019 - year_2008,
         diff_17_norm = diff_17 - mean(diff_17, na.rm = TRUE),
         diff_19_norm = diff_19 - mean(diff_19, na.rm = TRUE)) %>%
  cbind.data.frame(tower_comparison[1:229, 3:4])

# look for patterns in height difference
ggplot(tower_comparison_2, aes(x = Easting, y = Northing, color = diff_17)) +
  geom_point() +
  scale_color_viridis()
ggplot(tower_comparison_2, aes(x = Easting, y = Northing, color = diff_19)) +
  geom_point() +
  scale_color_viridis()
ggplot(tower_comparison_2, aes(x = Easting, y = Northing, color = diff_17_norm)) +
  geom_point() +
  scale_color_viridis()
ggplot(tower_comparison_2, aes(x = Easting, y = Northing, color = diff_19_norm)) +
  geom_point() +
  scale_color_viridis()

# compare distance point by point
dist_17 <- filter(tower2008, id != 227) %>%
  st_distance(filter(tower2017, id != 227), by_element = TRUE)
dist_19 <- filter(tower2008, id != 227) %>%
  st_distance(filter(tower2019, id %in% tower2008$id), by_element = TRUE)
dist_17_19 <- filter(tower2017, id != 227) %>%
  st_distance(filter(tower2019, id %in% tower2008$id), by_element = TRUE)

tower_comparison_3 <- tower_comparison_2 %>%
  filter(id %in% tower2008$id & id != 227) %>%
  mutate(dist_17 = as.numeric(dist_17),
         dist_19 = as.numeric(dist_19),
         dist_17_19 = as.numeric(dist_17_19)) %>%
  st_as_sf(coords = c('Easting', 'Northing'), crs = st_crs(points2017))

# plot and look for patterns in distance between points
ggplot(tower_comparison_3, aes(x = Easting, y = Northing, color = dist_17)) +
  geom_point() +
  scale_color_viridis()
ggplot(tower_comparison_3, aes(x = Easting, y = Northing, color = dist_19)) +
  geom_point() +
  scale_color_viridis()
ggplot(tower_comparison_3, aes(x = Easting, y = Northing, color = dist_17_19)) +
  geom_point() +
  scale_color_viridis()
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
x <- seq(min(tower_sp[[1]]@coords[,1]-25), max(tower_sp[[1]]@coords[,1]+25), length.out = (max(tower_sp[[1]]@coords[,1]+25)-min(tower_sp[[1]]@coords[,1]-25))/25)
y <- seq(min(tower_sp[[1]]@coords[,2]-25), max(tower_sp[[1]]@coords[,2]+25), length.out = (max(tower_sp[[1]]@coords[,2]+25)-min(tower_sp[[1]]@coords[,2]-25))/25)
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
ec_mask[ec_mask > 0.043] <- NA
ec_mask[ec_mask <= 0.043] <- 1
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
plot(tower_raster[[3]][[1]]) # 2019

sub17 <- tower_raster[[2]][[1]] - tower_raster[[1]][[1]]
sub19 <- tower_raster[[3]][[1]] - tower_raster[[1]][[1]]
diff_17_19 <- tower_raster[[3]][[1]]-tower_raster[[2]][[1]]

# plot subsidence using only GPS data
plot(sub17)
plot(sub19)
plot(diff_17_19)
plot(tower_sp[[1]], add = TRUE, col = 'red')
plot(tower_sp[[2]], add = TRUE, col = 'blue')
plot(tower_sp[[3]], add = TRUE, col = 'green')

# resample NEON LiDAR to 25 m resolution
dtm_mask <- dtm2017 %>%
  projectRaster(ec_mask) %>%
  mask(ec_mask)

plot(dtm_mask)
plot(tower_sp[[2]], add = TRUE, col = 'red')

# calculate subsidence using resampled LiDAR in 2017, because the GPS points are missing areas of thermokarst
# still doesn't look that good... subsidence is not showing up in the places that it is most expected
sub17_LiDAR <- dtm_mask - tower_raster[[1]][[1]]
plot(sub17_LiDAR)

# difference between resampled NEON LiDAR data and GPS data from same year (how well do the datasets agree?)
diff <- tower_raster[[2]][[1]] - dtm_mask # 2017 ec data is the second in the list, and dtm data from 2017
plot(diff)
test <- as.data.frame(diff)
boxplot(diff)
mean_diff <- mean(test$layer, na.rm = TRUE)
min_diff <- min(test$layer, na.rm = TRUE)
max_diff <- max(test$layer, na.rm = TRUE)

# difference between base station height in NEON LiDAR and GPS
# The NEON LiDAR point is 15 cm lower
# A measurement on Sept. 19, 2019 showed that the survey marker is 25 cm out of the ground, which could be partially responsible for the disagreement
# The difference in resolution and accuracy of the LiDAR could also partially explain the difference.
base_height <- raster::extract(dtm2017, filter(points2019, Name == 'base') %>% st_zm())
correction <- base_height - filter(points2019, Name == 'base')$Elevation
######################################################################################################################


# can I estimate the amount of vertical difference between 2017 and 2019 based on the distance between points?
# in radians:
slope2017 <- mask(terrain(raster(tower_model[[2]][[1]]), opt = 'slope', neighbors = 4), ec_mask)
aspect2017 <- mask(terrain(raster(tower_model[[2]][[1]]), opt = 'aspect', neighbors = 4), ec_mask)

plot(tower_raster[[2]][[1]])
plot(slope2017)
plot(aspect2017)

# need to calculate direction between 2017 and 2019 points using tan-1((y1 - y)/(x1-x))
# then figure out how to caculate the slope in the right direction using the slope and aspect of the cell
# then calculate the height 
tower_comparison_4 <- tower_comparison_3 %>%
  mutate(slope_extract = raster::extract(slope2017, ., df = TRUE)$slope,
         aspect_extract = raster::extract(aspect2017, ., df = TRUE)$aspect)
