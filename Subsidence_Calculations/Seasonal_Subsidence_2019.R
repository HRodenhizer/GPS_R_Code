##############################################################################################################
###                            Seasonal Subsidence and Frost Heave 2019                                    ###
###                                      Code by HGR 6/2019                                                ###
##############################################################################################################

### Load libraries ###########################################################################################
library(sf)
library(raster)
library(tidyverse)
library(ggthemes)
library(viridis)
library(gridExtra)
library(RStoolbox)
library(automap)
library(gstat)
##############################################################################################################

### Load data ################################################################################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected',
                        pattern = '[ABC]201[89].*\\.tif',
                        full.names = TRUE)
elevation <- list(brick(stack(filenames[which(str_detect(filenames, 'A201'))])),
                  brick(stack(filenames[which(str_detect(filenames, 'B201'))])),
                  brick(stack(filenames[which(str_detect(filenames, 'C201'))])))
names(elevation) <- c('Block A', 'Block B', 'Block C')
names(elevation[[1]]) <- c('Aug2018', 'May2019', 'Aug2019')
names(elevation[[2]]) <- c('Aug2018', 'May2019', 'Aug2019')
names(elevation[[3]]) <- c('Aug2018', 'May2019', 'Aug2019')
Points2018 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2018_SPCSAK4.shp")
Points052019 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_May_2019_SPCSAK4.shp")
Points082019 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_08_2019_SPCSAK4.shp")
grids <- list(clearValues(raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2017K.tif')),
              clearValues(raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2017K.tif')),
              clearValues(raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2017K.tif')))
##############################################################################################################

### calculate elevation change ###############################################################################
elevation_start <- map(elevation, ~subset(.x, 1:2))
elevation_end <- map(elevation, ~subset(.x, 2:3))
elev_change <- map2(elevation_end, elevation_start, ~.x-.y)
walk(elev_change, ~plot(.x))
elev_change_df <- map2_dfr(elev_change,
                           c('a', 'b', 'c'),
                           ~as.data.frame(.x, xy = TRUE) %>%
                             gather(key = collection, value = dElev, May2019:Aug2019) %>%
                             mutate(block = .y))

# filenames <- paste('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/Seasonal_Sub/',
#                    titles,
#                    '18_19_frost_heave.tif',
#                    sep = '')
# map2(frost_heave, filenames, ~ writeRaster(.x, .y))
##############################################################################################################

### Calculate distances between gps points within the same cell at the two time points #######################
# select only the transect points
points18xy <- Points2018 %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name))) %>%
  filter(Name > 10000 & Name < 13000) %>%
  arrange(Name)

points0519xy <- Points052019 %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name))) %>%
  filter(Name > 10000 & Name < 13000) %>%
  arrange(Name)

points0819xy <- Points082019 %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name))) %>%
  filter(Name > 10000 & Name < 13000) %>%
  arrange(Name)

# calculate the distance between paired gps points
point_distances <- points0819xy %>%
  select(Name, Easting, Northing) %>%
  cbind.data.frame(data.frame(distance0519 = as.numeric(st_distance(points18xy, points0519xy, by_element = TRUE)))) %>%
  cbind.data.frame(data.frame(distance0819 = as.numeric(st_distance(points0519xy, points0819xy, by_element = TRUE)))) %>%
  st_as_sf() %>%
  mutate(block = ifelse(Name > 10000 & Name < 11000,
                        'a',
                        ifelse(Name > 11000 & Name < 12000,
                               'b',
                               'c'))) %>%
  gather(key = collection, value = distance, distance0519:distance0819) %>%
  mutate(collection = ifelse(collection == 'distance0519',
                             'May2019',
                             'Aug2019'))

distance_summary_stats <- point_distances %>%
  st_drop_geometry() %>%
  group_by(collection) %>% 
  summarise(mean = mean(distance, na.rm = TRUE),
            sd = sd(distance, na.rm = TRUE),
            max = max(distance, na.rm = TRUE),
            min = min(distance, na.rm = TRUE))

ggplot(point_distances, aes(x = block, y = distance, fill = collection)) +
  geom_boxplot()

# turn the distances into a list for using the map function
point_distances <- st_drop_geometry(point_distances)
distance_list <- list(list(filter(point_distances, block == 'a' & collection == 'May2019'),
                           filter(point_distances, block == 'a' & collection == 'Aug2019')),
                      list(filter(point_distances, block == 'b' & collection == 'May2019'),
                           filter(point_distances, block == 'b' & collection == 'Aug2019')),
                      list(filter(point_distances, block == 'c' & collection == 'May2019'),
                           filter(point_distances, block == 'c' & collection == 'Aug2019')))

# turn the distances into a raster
distance_raster <- list()
for (i in 1:length(distance_list)) {
  distance_raster[[i]] <- raster()
  for (k in 1:length(distance_list[[i]])) {
    print(paste(i, k, sep = '-'))
    temp <- rasterize(distance_list[[i]][[k]][,2:3], grids[[i]], field = distance_list[[i]][[k]][,6])
    print('after raster')
    distance_raster[[i]] <- stack(distance_raster[[i]], temp)
  }
  distance_raster[[i]] <- brick(distance_raster[[i]])
  names(distance_raster[[i]]) <- c('May2019', 'Aug2019')
  plot(distance_raster[[i]])
  rm(i, k)
}


# turn the distance raster into a data frame for joining with the elevation change raster and mapping
distance_df <- map2_dfr(distance_raster,
                        c('a', 'b', 'c'),
                        ~ fortify(.x) %>% 
                          gather(key = collection, value = point.dist, May2019:Aug2019) %>%
                          mutate(block = .y))
##############################################################################################################

### Join elevation change and point distance dataframes ######################################################
elev_change_df <- elev_change_df %>%
  full_join(distance_df, by = c('x', 'y', 'block', 'collection')) %>%
  group_by(block) %>%
  mutate(dElev.20 = ifelse(point.dist <= 0.2,
                           dElev,
                           NA),
         dElev.10 = ifelse(point.dist <= 0.1,
                           dElev,
                           NA),
         x.norm = round(x-min(x, na.rm = TRUE)),
         y.norm = round(y-min(y, na.rm = TRUE)),
         collection = factor(collection, levels = c('May2019', 'Aug2019'))) %>%
  select(x, y, x.norm, y.norm, block, collection, dElev, dElev.10, dElev.20, point.dist)
##############################################################################################################

### summarize the frost heave and seasonal subsidence ########################################################
summary1 <- elev_change_df %>%
  group_by(block, collection) %>%
  summarise(mean.d.elev = mean(dElev, na.rm = TRUE),
            mean.d.elev.20 = mean(dElev.20, na.rm = TRUE),
            mean.d.elev.10 = mean(dElev.10, na.rm = TRUE))

summary2 <- summary1 %>%
  group_by(collection) %>%
  summarise(mean.d.elev = mean(mean.d.elev, na.rm = TRUE),
            mean.d.elev.20 = mean(mean.d.elev.20, na.rm = TRUE),
            mean.d.elev.10 = mean(mean.d.elev.10, na.rm = TRUE))
##############################################################################################################

### map the seasonal elevation changes at CiPEHR ############################################################
facet_labels <- c(`a` = 'A',
                  `b` = 'B',
                  `c` = 'C',
                  `May2019` = 'August 2018 - May 2019',
                  `Aug2019` = 'May - August 2019')
ggplot(elev_change_df, aes(x = x.norm, y = y.norm, fill = dElev)) +
  geom_tile() +
  scale_x_continuous(name = '') +
  scale_y_continuous(name = '') +
  scale_fill_viridis(name = expression(Delta*" Elevation (m)")) +
  facet_grid(collection ~ block,
             labeller = as_labeller(facet_labels)) +
  theme_few() +
  coord_fixed()

ggplot(elev_change_df, aes(x = x.norm, y = y.norm, fill = dElev.20)) +
  geom_tile() +
  scale_x_continuous(name = '') +
  scale_y_continuous(name = '') +
  scale_fill_viridis(name = expression(Delta*" Elevation (m)")) +
  facet_grid(collection ~ block,
             labeller = as_labeller(facet_labels)) +
  theme_few() +
  coord_fixed()
ggplot(elev_change_df, aes(x = x.norm, y = y.norm, fill = dElev.10)) +
  geom_tile() +
  scale_x_continuous(name = '') +
  scale_y_continuous(name = '') +
  scale_fill_viridis(name = expression(Delta*" Elevation (m)")) +
  facet_grid(collection ~ block,
             labeller = as_labeller(facet_labels)) +
  theme_few() +
  coord_fixed()

# # map the frost heave with the distances between gps points for that cell overlaid
# titles <- list('A', 'B', 'C')
# walk2(frost_heave_df,
#      distance_list,
#      ~ print(ggplot(.x, aes(x = x, y = y, fill = d.elev)) +
#                geom_tile() +
#                geom_point(data = .y, aes(x = Easting, y = Northing, color = distance), inherit.aes = FALSE) +
#                geom_text(data = .y, aes(x = Easting, y = Northing, label = Name), size = 2, inherit.aes = FALSE) +
#                scale_color_viridis() +
#                ggtitle(.y$block)))
# 
# # map the frost heave excluding cells with gps points over 20 cm apart
# maps <- frost_heave_df %>%
#   walk2(titles,
#       ~ print(ggplot(.x, aes(x = x, y = y, fill = d.elev.2)) +
#                 geom_tile() +
#                 scale_fill_viridis(name = expression(paste(Delta, ' Elevation')),
#                                    limits = c(-0.05, 0.2)) +
#                 coord_fixed() +
#                 theme_few() +
#                 theme(axis.title = element_blank()) +
#                 ggtitle(paste(.y))))
# 
# fig <- grid.arrange(maps[[1]], maps[[2]], maps[[3]], ncol = 3)
# fig
# 
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/frost_heave_2018_2019.jpg', fig, width = 8, height = 3)
##############################################################################################################

### krige elevation surfaces for the ec tower and calculate elevation change #################################
tower <- list(Points052019%>%
                filter(as.numeric(as.character(Name)) >= 14000) %>%
                st_zm() %>%
                mutate(name2 = as.numeric(as.character(Name)) - 14000), 
              Points082019%>%
                filter(as.numeric(as.character(Name)) >= 14000) %>%
                st_zm() %>%
                mutate(name2 = as.numeric(as.character(Name)) - 14000))

tower_sp <- map(tower, ~ .x  %>%
                  as('Spatial'))


x <- seq(min(tower_sp[[2]]@coords[,1]-10), max(tower_sp[[2]]@coords[,1]+10), length.out = (max(tower_sp[[2]]@coords[,1]+10)-min(tower_sp[[2]]@coords[,1]-10))/10)
y <- seq(min(tower_sp[[2]]@coords[,2]-10), max(tower_sp[[2]]@coords[,2]+10), length.out = (max(tower_sp[[2]]@coords[,2]+10)-min(tower_sp[[2]]@coords[,2]-10))/10)
grid <- expand.grid(x = x, y = y)
gridded(grid) = ~x+y
grid@proj4string<-CRS(st_crs(Points052019)$proj4string)

# make sure the points from both collections fit in the grid
plot(grid)
plot(tower_sp[[1]], add = TRUE, col = 'red')
plot(grid)
plot(tower_sp[[2]], add = TRUE, col = 'red')

# krige the tower elevation surfaces
#may
may.vgm <- variogram(Elevation~Easting+Northing, tower_sp[[1]], alpha = c(0, 90)+45)
vgm.anis = vgm(0.5, "Exp", 100, 0, anis =  c(135, 0.7))
may.fit <- fit.variogram(may.vgm, model = vgm.anis)
plot(may.vgm, may.fit)

may_model <-  krige(Elevation~1, tower_sp[[1]], grid, may.fit)
may_raster <- raster(may_model)
may_var <- raster(may_model[2])
may_df <- may_raster %>%
  as.data.frame()

ggplot(tower[[1]]) +
  geom_tile(data = may_raster, aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('May')

#aug
aug.vgm <- variogram(Elevation~Easting+Northing, tower_sp[[2]], alpha = c(0, 90)+45)
vgm.anis = vgm(0.35, "Sph", 100, 0.5, anis =  c(135, 0.7))
aug.fit <- fit.variogram(aug.vgm, model = vgm.anis)
plot(aug.vgm, aug.fit)

aug_model <-  krige(Elevation~1, tower_sp[[2]], grid, aug.fit)
aug_raster <- raster(aug_model)
aug_var <- raster(aug_model[2])
aug_df <- aug_raster %>%
  as.data.frame()

ggplot(tower[[2]]) +
  geom_tile(data = aug_raster, aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('Aug')

# create a raster object to mask with
ec_mask <- aug_var
plot(ec_mask)
ec_mask[ec_mask > 0.07] <- NA
ec_mask[ec_mask <= 0.07] <- 1
plot(ec_mask)
plot(tower_sp[[1]], add = TRUE)

may_raster <- may_raster %>%
  mask(ec_mask)

aug_raster <- aug_raster %>%
  mask(ec_mask)

tower_surfaces <- map(tower_surfaces,
     ~ mask(.x, ec_mask))

walk(tower_surfaces,
     ~ plot(.x))

seasonal_sub_ec <- aug_raster - may_raster
plot(seasonal_sub_ec)
sub_subset <- seasonal_sub_ec
sub_subset[sub_subset > 0] <- NA
plot(sub_subset)

ggplot(tower[[1]], aes (x = Easting, y = Northing)) +
  geom_point() +
  geom_text(aes(label = name2), hjust = 1.1) +
  geom_point(data = tower[[2]], aes(x = Easting, y = Northing), inherit.aes = FALSE, color = 'red') +
  geom_text(data = tower[[2]], aes(x = Easting, y = Northing, label = name2), inherit.aes = FALSE, color = 'red')

may_ec_renumber <- tower[[1]] %>%
  mutate(name2 = ifelse(name2 <= 94,
                        name2,
                        ifelse(name2 >= 95 & name2 <= 96,
                               name2 + 1,
                               name2 + 2)))

ggplot(may_ec_renumber, aes (x = Easting, y = Northing)) +
  geom_point() +
  geom_text(aes(label = name2), hjust = 1.1) +
  geom_point(data = tower[[2]], aes(x = Easting, y = Northing), inherit.aes = FALSE, color = 'red') +
  geom_text(data = tower[[2]], aes(x = Easting, y = Northing, label = name2), inherit.aes = FALSE, color = 'red')

may_ec_subset <- may_ec_renumber %>%
  filter(name2 <= 228)

aug_ec_subset <- tower[[2]] %>%
  filter(name2 <= 94 | name2 == 96 | name2 == 97 | name2 >= 99)

ec_distances <- may_ec_subset %>%
  cbind.data.frame(data.frame(point.dist = st_distance(may_ec_subset, aug_ec_subset, by_element = TRUE))) %>%
  st_as_sf() %>%
  select(name2, Easting, Northing, point.dist)

ec_dist_sp <- ec_distances %>%
  as('Spatial')

ec_dist_model <- autoKrige(point.dist ~ 1, ec_dist_sp, grid)
ec_dist_surface <- ec_dist_model$krige_output %>%
  brick() %>%
  mask(ec_mask)

plot(ec_dist_surface)
##############################################################################################################