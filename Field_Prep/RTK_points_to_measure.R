###############################################################################################################################
###                               Code to edit shapefile of RTK Points                                                      ###
###                               By HGR 5/2018                                                                             ###
###############################################################################################################################

# Load libraries
library(tidyverse)
library(sf)
library(readxl)

# Load data
# most of the points are in this file
RTK_Points <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Protocol/INCOMPLETE_EML_GPS_Points/EML_All_Points_To_Measure_Correct.shp") %>%
  st_zm() %>%
  filter(Type != 'Tower')
nad83_11 <- st_crs(RTK_Points) # this is SPCSAK4

# tower 2008
ec2008 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/TowerFootprint_2008_2009/tower2008.shp')
utm <- st_crs(ec2008)
ec2008 <- ec2008 %>%
  st_transform(crs = nad83_11) %>%
  st_zm() %>%
  cbind.data.frame(st_coordinates(.)) %>%
  st_as_sf() %>%
  mutate(Lat = X,
         Long = Y) %>%
  select(-X, -Y)

# need to incorporate the points which I added by hand in ArcMap to the the data frame (START HERE NEXT TIME)
ec2008_df <- ec2008 %>%
  select(id, flag, Lat, Long) %>%
  st_drop_geometry()

ec2008 <- ec2008_df %>%
  filter(flag == 0) %>%
  select(id, Lat, Long) %>%
  right_join(ec2008_df, by = 'id') %>%
  mutate(Lat = ifelse(is.na(Lat.y),
                      Lat.x,
                      Lat.y),
         Long = ifelse(is.na(Long.y),
                       Long.x,
                       Long.y)) %>%
  select(id, Lat, Long) %>%
  st_as_sf(coords = c('Lat', 'Long'),
           crs = nad83_11) %>%
  mutate(POINTNM = paste('ec', id, sep = ''),
         Measure_Type = 'Tower',
         Plot_Type = '') %>%
  select(POINTNM, Measure_Type, Plot_Type, Num = id) %>%
  arrange(Num)

plot(ec2008)

# AJ's points
AJ_points <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Protocol/RTK_Points/ForHeidi.xlsx')

missing_utm_points <- AJ_points %>%
  filter(is.na(north)) %>%
  st_as_sf(coords = c('long', 'lat'),
           crs = 4326) %>%
  st_transform(crs = utm) %>%
  st_coordinates()

AJ_points <- AJ_points %>%
  mutate(north = ifelse(is.na(north),
                        missing_utm_points[2],
                        north),
         east = ifelse(is.na(east),
                       missing_utm_points[1],
                       east),
         Measure_Type = 'Other',
         Plot_Type = '',
         Num = seq(1, 6)) %>%
  st_as_sf(coords = c('east', 'north'),
           crs = utm) %>%
  st_transform(crs = nad83_11) %>%
  select(POINTNM = name, Measure_Type, Plot_Type, Num)

# # A few points from Fay's 2008/2009 EC Tower survey need to be added (the transects of thaw depth at different scales)
# ec2008 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Protocol/eddy_tower_all_points/ec_all_points_2008.xlsx') %>%
#   st_as_sf(coords = c('east', 'north'), crs = 32606) %>%
#   st_transform(spcsak4) %>%
#   mutate(POINTNM = ifelse(site - as.integer(site) == 0,
#                           paste('Tower', as.integer(site), sep = '_'),
#                           paste('Tower', site, sep = '_')),
#          Type = 'Tower',
#          Grid_Num = NA) %>%
#   select(POINTNM, Type, Grid_Num)
# # st_write(ec2008, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Protocol/eddy_tower_all_points/ec_all_points_2008_spcsak4.shp')

# FDBM points need to be added
fdbm <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2016_SPCSAK4.shp') %>%
  st_transform(crs = nad83_11) %>%
  st_zm() %>%
  filter(Type == 'FDBM') %>%
  mutate(Num = NA,
         Measure_Type = 'Plot',
         Plot_Type = 'FDBM') %>%
  select(POINTNM = Name, Measure_Type, Plot_Type, Num)

# Order the plots, water wells, and FDBM
plotorder <- RTK_Points %>%
  rename(Num = Grid_Num) %>%
  filter(Type == 'Flux' | Type == 'Gas Well' | Type == 'Water Well' | Type == 'FDBM') %>%
  mutate(Measure_Type = 'Plot',
         Plot_Type = Type) %>%
  select(-Type) %>%
  rbind.data.frame(fdbm) %>%
  mutate(POINTNM = ifelse(Plot_Type == 'Flux' | Plot_Type == 'Gas Well',
                          paste(str_sub(str_replace(POINTNM, pattern = '\\.', replacement = '_'), start = 1, end = 3), 
                                '_',
                                str_sub(POINTNM, start = -1)),
                          ifelse(Plot_Type == 'Water Well',
                                 paste(str_sub(POINTNM, start = 3, end = 3), '_', str_sub(POINTNM, start = 4), '_ww', sep = ''),
                                 ifelse(POINTNM != 'hobo' & POINTNM != 'hobo_side',
                                        paste(str_sub(POINTNM, start = 1, end = 1), '_', str_sub(POINTNM, start = 2), sep = ''),
                                        ifelse(POINTNM == 'hobo',
                                               paste(9, '_', POINTNM, '_top', sep = ''),
                                               paste(9, '_', POINTNM)))))) %>%
  separate(POINTNM, into = c('fence', 'plot', 'type'), sep = '_', remove = FALSE) %>%
  mutate(plot = str_trim(plot),
         treatment = ifelse(plot == 'B' | plot == 'Bn' | plot == 'Bs',
                            'c',
                            ifelse(Plot_Type != 'FDBM' & (plot == 'C' | plot == 'D' | plot == 'Dn' | plot == 'Ds'),
                                   'w',
                                   ifelse((Plot_Type == 'Flux' | Plot_Type == 'Gas Well') & as.numeric(plot) <= 4,
                                          'c',
                                          ifelse((Plot_Type == 'Flux' | Plot_Type == 'Gas Well') & as.numeric(plot) >= 4,
                                                 'w',
                                                 ifelse(Plot_Type == 'Water Well' & as.numeric(plot) <= 2.5,
                                                        'c',
                                                        ifelse(Plot_Type == 'Water Well' & as.numeric(plot) >= 2.5,
                                                               'w',
                                                               ifelse(Plot_Type == 'FDBM' & plot == 'C',
                                                                      'c',
                                                                      'w'))))))),
         Plot_Other = ifelse(Plot_Type == 'Flux' | Plot_Type == 'Gas Well',
                             1,
                             2)) %>%
  arrange(fence, treatment, Plot_Other, plot, Plot_Type) %>%
  mutate(Num = seq(1, nrow(plotorder))) %>%
  select(POINTNM, Measure_Type, Plot_Type, Num)

# join all the data
RTK_Points_2 <- RTK_Points %>%
  filter(!(Type == 'Flux' | Type == 'Gas Well' | Type == 'Water Well' | Type == 'FDBM')) %>%
  mutate(POINTNM = ifelse(Type == 'grad',
                          paste(str_sub(POINTNM, start = 1, end = 4), '_', str_sub(POINTNM, start = 5), sep = ''),
                          as.character(POINTNM))) %>%
  separate(POINTNM, into = c('Type2', 'Num'), remove = FALSE) %>%
  mutate(Num = as.numeric(Num),
         Plot_Type = '') %>%
  select(POINTNM, Measure_Type = Type, Plot_Type, Num) %>%
  arrange(Measure_Type, Num) %>%
  rbind.data.frame(plotorder, ec2008, AJ_points) %>%
  mutate(Measure_Type = factor(Measure_Type, levels = c('A', 'B', 'C', 'Plot', 'grad', 'Tower'))) %>%
  arrange(Measure_Type) %>%
  mutate(ID = seq(1, nrow(RTK_Points_2))) %>%
  select(ID, POINTNM, Measure_Type, Plot_Type, Num)

plot(RTK_Points_2)
  
st_write(RTK_Points_2, dsn = 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Protocol/RTK_Points/RTK_Points_NAD83_201908.shp')
