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
  select(x, y, block, collection, dElev, point.dist)
##############################################################################################################

# find the mean frost heave for each block for all cells, for all cells with gps points within 20 cm, 
# and for all cells with gps points within 10 cm
summary <- map_dfr(frost_heave_df, ~ summarise(.x, 
                                           mean.d.elev = mean(d.elev, na.rm = TRUE),
                                           mean.d.elev.2 = mean(d.elev.2, na.rm = TRUE),
                                           mean.d.elev.1 = mean(d.elev.1, na.rm = TRUE)))

# map the frost heave with the distances between gps points for that cell overlaid
titles <- list('A', 'B', 'C')
walk2(frost_heave_df,
     distance_list,
     ~ print(ggplot(.x, aes(x = x, y = y, fill = d.elev)) +
               geom_tile() +
               geom_point(data = .y, aes(x = Easting, y = Northing, color = distance), inherit.aes = FALSE) +
               geom_text(data = .y, aes(x = Easting, y = Northing, label = Name), size = 2, inherit.aes = FALSE) +
               scale_color_viridis() +
               ggtitle(.y$block)))

# map the frost heave excluding cells with gps points over 20 cm apart
maps <- frost_heave_df %>%
  walk2(titles,
      ~ print(ggplot(.x, aes(x = x, y = y, fill = d.elev.2)) +
                geom_tile() +
                scale_fill_viridis(name = expression(paste(Delta, ' Elevation')),
                                   limits = c(-0.05, 0.2)) +
                coord_fixed() +
                theme_few() +
                theme(axis.title = element_blank()) +
                ggtitle(paste(.y))))

fig <- grid.arrange(maps[[1]], maps[[2]], maps[[3]], ncol = 3)
fig

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/frost_heave_2018_2019.jpg', fig, width = 8, height = 3)
##############################################################################################################