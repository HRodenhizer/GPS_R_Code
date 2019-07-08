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
RTK_Points <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Protocol/INCOMPLETE_EML_GPS_Points/EML_GPS_Points.shp") %>%
  st_zm() %>%
  filter(Type != 'Tower')
nad83_11 <- st_crs(RTK_Points) # this is SPCSAK4

# tower 2008
ec2008 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/TowerFootprint_2008_2009/tower2008.shp') %>%
  st_transform(crs = nad83_11) %>%
  st_zm() %>%
  cbind.data.frame(st_coordinates(.)) %>%
  st_as_sf() %>%
  mutate(Lat = X,
         Long = Y) %>%
  select(-X, -Y)

# need to incorporate the points which I added by hand in ArcMap to the the data frame (START HERE NEXT TIME)
temp <- ec2008 %>%
  filter(flag == 0) %>%
  select(id, Lat, Long) %>%
  left_join(filter(ec2008, flag != 0), by = 'id') %>%
  mutate(Lat = ifelse(Lat == 0,
                      ))

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
  mutate(Grid_Num = NA) %>%
  select(POINTNM = Name, Type, Grid_Num)

plot(ec2008)

# Join shapefiles and reshape data
RTK_Points_2 <- RTK_Points %>%
  rbind.data.frame(ec2008, fdbm) %>%
  filter(POINTNM != is.na(POINTNM)) %>%
  mutate(POINTNM = ifelse(Type == 'grad',
                          paste(str_sub(POINTNM, start = 1, end = 4), str_sub(POINTNM, start = 5, end = -1), sep = '_'),
                          ifelse(Type == 'Water Well',
                                 paste(str_sub(POINTNM, start = 3, end = 3), '.', str_sub(POINTNM, start = 4, end = -1), str_sub(POINTNM, start = 1, end = 2), sep = ''),
                                 ifelse(Type == 'FDBM' & POINTNM != 'hobo' & POINTNM != 'hobo_side',
                                        paste(str_sub(POINTNM, start = 1, end = 1), '.', str_sub(POINTNM, start = 2, end = -1), sep = ''),
                                        as.character(POINTNM)))),
         ID = ifelse(Type == 'A',
                     Grid_Num + 10000,
                     ifelse(Type == 'B',
                            Grid_Num + 11000,
                            ifelse(Type == 'C',
                                   Grid_Num + 12000,
                                   ifelse(Type == 'Tower',
                                          as.numeric(str_sub(POINTNM, start = 7, end = -1)) + 14000,
                                          ifelse(Type == 'grad',
                                                 as.numeric(str_sub(POINTNM, start = 6, end = -1)) + 15000,
                                                 NA)))))) %>%
  mutate(POINTNM = ifelse(Type == 'Flux' | Type == 'Gas Well' | Type == 'Water Well' | Type == 'FDBM' & POINTNM != 'hobo' & POINTNM != 'hobo_side',
                          str_replace(POINTNM, pattern = fixed('.'), replacement = fixed('_')),
                          POINTNM),
         POINTNM = ifelse(Type == 'Flux' | Type == 'Gas Well',
                          paste(str_sub(POINTNM, start = 1, end = -2), str_sub(POINTNM, start = -1, end = -1), sep = '_'),
                          ifelse(Type == 'Water Well',
                                 paste(str_sub(POINTNM, start = 1, end = -3), str_sub(POINTNM, start = -2, end = -1), sep = '_'),
                                 ifelse(Type == 'FDBM' & POINTNM == 'hobo',
                                        'hobo_top',
                                        POINTNM))))
plotorder <- RTK_Points_2 %>%
  filter(Type == 'Flux' | Type == 'Gas Well' | Type == 'Water Well' | Type == 'FDBM') %>%
  separate(POINTNM, into = c('fence', 'plot', 'type'), sep = '_', remove = FALSE) %>%
  mutate(plot = ifelse(Type == 'Water Well',
                       paste('w', plot, sep = ''),
                       ifelse(Type == 'FDBM',
                              paste('fdbm', plot, sep = ''),
                              plot)),
         plot = factor(plot, levels = c('1', '2', '3', '4', 'B', 'wBn', 'wBs', 'w1', 'w2', 'w2.5', 'fdbmC', '5', '6', '7', '8', 'C', 'D', 'wDn', 'wDs', 'w3', 'w4', 'w4.5', 'fdbmW'))) %>%
  arrange(fence, plot) %>%
  mutate(ID = seq(13001, 13206)) %>%
  select(POINTNM, Type, Grid_Num, ID)

RTK_Points_3 <- RTK_Points_2 %>%
  filter(Type != 'Flux' & Type != 'Gas Well' & Type != 'Water Well' & Type != 'FDBM') %>%
  rbind.data.frame(plotorder) %>%
  arrange(ID) %>%
  mutate(Type = ifelse(Type == 'A',
                       'a',
                       ifelse(Type == 'B',
                              'b',
                              ifelse(Type == 'C',
                                     'c',
                                     ifelse(Type == 'FDBM',
                                            'fdbm',
                                            ifelse(Type == 'Flux',
                                                   'flux',
                                                   ifelse(Type == 'Gas Well',
                                                          'gw',
                                                          ifelse(Type == 'Tower',
                                                                 'tower',
                                                                 ifelse(Type == 'Water Well',
                                                                        'ww',
                                                                        as.character(Type)))))))))) %>%
  st_transform(crs = 4269)

# old renaming code that could be useful at some point
# ,
# Name = ifelse(Type == 'a' | Type == 'b' | Type == 'c' | Type == 'grad' | Type == 'tower',
#               str_c(Type, TID, sep = '_'),
#               ifelse(Type == 'flux' | Type == 'gw',
#                      str_replace(str_c(Type, str_sub(POINTNM, start = 1, end = 3), sep = '_'), pattern=fixed('.'), replacement=fixed('_')),
#                      ifelse(Type == 'fdbm',
#                             str_c(Type, POINTNM, sep = '_'),
#                             str_c(Type, str_sub(POINTNM, start = 3, end = 3), str_sub(POINTNM, start = 4), sep = '_'))))


# for some reason arcmap doesn't recognize the coordinate system when rtk_points_2 gets exported with the original cs.
# The last line of code above transforms to NAD83 (which gets recognized as a GRS80 in ArcMap, but needs to be defined (define projection in ArcMap)
# as NAD83, and then you can project to SPCS AK 4).

st_write(RTK_Points_3, dsn = 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Protocol/RTK_Points/RTK_Points_NAD83.shp')
