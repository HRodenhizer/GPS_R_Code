#################################################################################################################
###                                 LTER Prep for GPS Data                                                    ###
###                                   Code by HGR 11/19                                                       ###
#################################################################################################################

# load libraries
library(sf)
library(tidyverse)

# load data files
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points',
                        pattern = '^All.*shp$',
                        full.names = TRUE)
# gps <- map(filenames, ~ st_read(.x))

# all the formats are different, so it doesn't actually makes sense to read them all into a list
gps2009 <- st_read(filenames[1])
gps2011 <- st_read(filenames[2])
gps2015 <- st_read(filenames[3])
gps2016 <- st_read(filenames[4])
gps2017 <- st_read(filenames[5])
gps2018 <- st_read(filenames[6])
gps2019_aug <- st_read(filenames[7])
gps2019_may <- st_read(filenames[8])

# separate out the transect data
trans2009 <- gps2009 %>%
  filter(type == 'trans' | type == 'fence')

trans2011 <- gps2011 %>%
  filter(type == 'fence' | type == 'a' | type == 'b' | type == 'c')

trans2015 <- gps2015 %>%
  filter(type == 'fence')

trans2016 <- gps2016 %>%
  filter(Type == 'trans')

trans2017 <- gps2017 %>%
  filter(as.numeric(as.character(Name)) >= 10000 & as.numeric(as.character(Name)) < 13000)

trans2018 <- gps2018 %>%
  filter(as.numeric(as.character(Name)) >= 10000 & as.numeric(as.character(Name)) < 13000)

trans2019_may <- gps2019_may %>%
  filter(as.numeric(as.character(Name)) >= 10000 & as.numeric(as.character(Name)) < 13000)

trans2019_aug <- gps2019_aug %>%
  filter(as.numeric(as.character(Name)) >= 10000 & as.numeric(as.character(Name)) < 13000)

# neaten and standardize columns
lter2009 <- trans2009 %>%
  mutate(year = 2009,
         block = ifelse(as.numeric(as.character(fence)) <= 2,
                        'a',
                        ifelse(as.numeric(as.character(fence)) >= 5,
                               'c',
                               'b')),
         point_number = 'need to figure this part out')
