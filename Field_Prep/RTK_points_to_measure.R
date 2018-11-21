###############################################################################################################################
###                               Code to edit shapefile of RTK Points                                                      ###
###                               By HGR 5/2018                                                                             ###
###############################################################################################################################

# Load libraries
library(tidyverse)
library(sf)

# Load data - this data no longer exists. Start with RTK_Points if changes need to be made.
RTK_Points <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 Protocol/EML_All_Points_To_Measure_v3.shp")
st_crs(RTK_Points)

# Data reshaping
RTK_Points_2 <- RTK_Points %>%
  filter(POINTNM != is.na(POINTNM)) %>%
  arrange(Type, ID) %>%
  group_by(Type) %>%
  mutate(ID = ifelse(Type == 'FDBM',
                     ID+1775,
                     ID),
         TID = seq(1, n(), 1)) %>%
  ungroup() %>%
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
                                                                        as.character(Type))))))))),
         Name = ifelse(Type == 'a' | Type == 'b' | Type == 'c' | Type == 'grad' | Type == 'tower',
                       str_c(Type, TID, sep = '_'),
                       ifelse(Type == 'flux' | Type == 'gw',
                              str_replace(str_c(Type, str_sub(POINTNM, start = 1, end = 3), sep = '_'), pattern=fixed('.'), replacement=fixed('_')),
                              ifelse(Type == 'fdbm',
                                     str_c(Type, POINTNM, sep = '_'),
                                     str_c(Type, str_sub(POINTNM, start = 3, end = 3), str_sub(POINTNM, start = 4), sep = '_'))))) %>%
  arrange(ID) %>%
  select(-POINTNM, -UID) %>%
  st_transform(crs = 4269)

# for some reason arcmap doesn't recognize the coordinate system when rtk_points_2 gets exported with the original cs.
# The last line of code above transforms to NAD83 (which gets recognized as a GRS80 in ArcMap, but needs to be defined (define projection in ArcMap)
# as NAD83, and then you can project to SPCS AK 4).

st_write(RTK_Points_2, dsn = 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 Protocol/RTK_Points_NAD83.shp')
