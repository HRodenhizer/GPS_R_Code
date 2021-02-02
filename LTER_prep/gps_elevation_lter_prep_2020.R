#################################################################################################################
###                                 LTER Prep for GPS Elevation Data                                          ###
###                                        Code by HGR 11/19                                                  ###
#################################################################################################################

### load libraries ##############################################################################################
library(sf)
library(tidyverse)
library(viridis)
#################################################################################################################

### load data files #############################################################################################
filenames <- list.files('~/Documents/School/NAU/Schuur Lab/GPS/All_Points',
                        pattern = '^All.*2020_SPCSAK4\\.shp$',
                        full.names = TRUE)

# all the formats are different, so it doesn't actually makes sense to read them all into a list
gps2020 <- st_read(filenames[1]) %>%
  rename(Northing = Northng, Elevation = Elevatn)
#################################################################################################################

### separate out the transect data ##############################################################################
trans2020 <- gps2020 %>%
  filter(as.numeric(as.character(Name)) >= 10000 & as.numeric(as.character(Name)) < 13000)
#################################################################################################################

### 2020 ########################################################################################################
lter2020 <- trans2020 %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name)),
         year = 2020,
         block = ifelse(Name < 11000,
                        'a',
                        ifelse(Name < 12000,
                               'b',
                               'c'))) %>%
  arrange(Name) %>%
  select(year, block, id = Name, Easting, Northing, Elevation)

ggplot(filter(lter2020, block == 'a')) +
  geom_sf(aes(geometry = geometry, color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 10000))

ggplot(filter(lter2020, block == 'b')) +
  geom_sf(aes(geometry = geometry, color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 11000))

ggplot(filter(lter2020, block == 'c')) +
  geom_sf(aes(geometry = geometry, color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 12000))
#################################################################################################################

### Save Shapefiles #############################################################################################
st_write(lter2020,
         "/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/2020/EML_AK_CiPEHR_GPS_Elevation_2020.shp")
#################################################################################################################