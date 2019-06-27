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
##############################################################################################################

### Load data ################################################################################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation',
                        pattern = '[ABC]201[89]K.tif',
                        full.names = TRUE)
elevation <- list(brick(stack(filenames[1:2])),
                  brick(stack(filenames[3:4])),
                  brick(stack(filenames[5:6])))
Points2018 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2018_SPCSAK4.shp")
Points2019 <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_May_2019_SPCSAK4.shp")
grids <- list(clearValues(raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2017K.tif')),
              clearValues(raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2017K.tif')),
              clearValues(raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2017K.tif')))
##############################################################################################################

### calculate elevation change ###############################################################################
frost_heave <- map(elevation, ~.x[[2]] - .x[[1]])

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

points19xy <- Points2019 %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name))) %>%
  filter(Name > 10000 & Name < 13000) %>%
  arrange(Name)

# calculate the distance between paired gps points
point_distances <- points19xy %>%
  select(Name, Easting, Northing) %>%
  cbind.data.frame(data.frame(distance = as.numeric(st_distance(points18xy, points19xy, by_element = TRUE)))) %>%
  st_as_sf() %>%
  mutate(block = ifelse(Name > 10000 & Name < 11000,
                        'a',
                        ifelse(Name > 11000 & Name < 12000,
                               'b',
                               'c')))

mean_distance <- mean(point_distances$distance, na.rm = TRUE)
sd_distance <- sd(point_distances$distance, na.rm = TRUE)
max_distance <- max(point_distances$distance, na.rm = TRUE)
min_distance <- min(point_distances$distance, na.rm = TRUE)

ggplot(point_distances, aes(x = block, y = distance)) +
  geom_boxplot()

# turn the distances into a list for using the map function
distance_list <- list(filter(point_distances, block == 'a'),
                      filter(point_distances, block == 'b'),
                      filter(point_distances, block == 'c'))

# turn the distances into a raster and join with the frost heave raster
distance_raster <- map2(distance_list, grids, ~ rasterize(st_drop_geometry(.x)[,2:3], .y, field = st_drop_geometry(.x)[,4]))
frost_heave <- map2(frost_heave, distance_raster, ~ brick(stack(.x, .y)))

# turn the frost heave/distance raster into a data frame for mapping
frost_heave_df <- map(frost_heave, ~fortify(.x) %>% 
                        rename(d.elev = layer.1, point.dist = layer.2) %>%
                        mutate(d.elev.2 = ifelse(point.dist < 0.2,
                                                 d.elev,
                                                 NA),
                               d.elev.1 = ifelse(point.dist < 0.1,
                                                 d.elev,
                                                 NA)))

# find the mean frost heave for each block for all cells, for all cells with gps points within 20 cm, 
# and for all cells with gps points within 10 cm
summary <- map_dfr(frost_heave_df, ~ summarise(.x, 
                                           mean.d.elev = mean(d.elev, na.rm = TRUE),
                                           mean.d.elev.2 = mean(d.elev.2, na.rm = TRUE),
                                           mean.d.elev.1 = mean(d.elev.1, na.rm = TRUE)))

# map the frost heave with the distances between gps points for that cell overlaid
titles <- list('A', 'B', 'C')
map2(frost_heave_df,
     distance_list,
     ~ ggplot(.x, aes(x = long, y = lat, fill = d.elev)) +
       geom_tile() +
       geom_point(data = .y, aes(x = Easting, y = Northing, color = distance), inherit.aes = FALSE) +
       geom_text(data = .y, aes(x = Easting, y = Northing, label = Name), size = 2, inherit.aes = FALSE) +
       scale_color_viridis() +
       ggtitle(.y$block) )

# map the frost heave excluding cells with gps points over 20 cm apart
maps <- frost_heave_df %>%
  map2(titles,
      ~ ggplot(.x, aes(x = long, y = lat, fill = d.elev.2)) +
      geom_tile() +
      scale_fill_viridis(name = expression(paste(Delta, ' Elevation')),
                         limits = c(-0.05, 0.2)) +
      coord_fixed() +
      theme_few() +
      theme(axis.title = element_blank()) +
      ggtitle(paste(.y)))

fig <- grid.arrange(maps[[1]], maps[[2]], maps[[3]], ncol = 3)
fig

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/frost_heave_2018_2019.jpg', fig, width = 8, height = 3)
##############################################################################################################