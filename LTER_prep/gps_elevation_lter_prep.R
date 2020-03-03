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
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points',
                        pattern = '^All.*shp$',
                        full.names = TRUE)

# all the formats are different, so it doesn't actually makes sense to read them all into a list
gps2009 <- st_read(filenames[1])
gps2011 <- st_read(filenames[2])
gps2015 <- st_read(filenames[3])
gps2016 <- st_read(filenames[4])
gps2017 <- st_read(filenames[5])
gps2018 <- st_read(filenames[6])
gps2019_aug <- st_read(filenames[7])
gps2019_may <- st_read(filenames[8])
#################################################################################################################

### separate out the transect data ##############################################################################
trans2009 <- gps2009 %>%
  filter(type == 'trans')

trans2011 <- gps2011 %>%
  filter(type == 'a' | type == 'b' | type == 'c')

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
#################################################################################################################

### neaten and standardize columns
# why are point numbers getting added starting in the middle of the rows rather than in row 1?

### 2009 ########################################################################################################
lter2009 <- trans2009 %>%
  filter(id != 492) %>%
  arrange(fence) %>%
  mutate(year = 2009,
         count = seq(1, nrow(.)),
         block = ifelse(as.numeric(as.character(fence)) <= 2,
                        'a',
                        ifelse(as.numeric(as.character(fence)) >= 5,
                               'c',
                               'b')),
         point_number = ifelse(block == 'a',
                               c(seq(10001, 10075), seq(10082, 10111), 10076, seq(10112, 10121), 10077, seq(10122, 10131), 
                                 10078, seq(10132, 10141), 10079, seq(10142, 10149), 10080, 10081),
                               ifelse(block == 'b',
                                      c(seq(11013, 11019), 11002, seq(11020, 11029), 11001, seq(11030, 11039), 11004, 
                                        seq(11040, 11049), 11005, seq(11050, 11059), 11006, seq(11060, 11069), 11007, 
                                        seq(11070, 11072), 11008, 11009, 11077, 11078, seq(11083, 11091), 11076, 
                                        seq(11092, 11101), 11075, seq(11102, 11111), 11074, seq(11112, 11121), 11073, 
                                        seq(11122, 11130), 11082, seq(11131, 11140), 11081, seq(11141, 11150), 11080,
                                        seq(11151, 11153), 11079, 11001, seq(11010, 11012)),
                                      c(seq(12026, 12029), 12003, seq(12030, 12039), 12004, seq(12040, 12049), 12005,
                                        seq(12050, 12059), 12006, seq(12060, 12069), 12007, seq(12070, 12078), 12008, 12009,
                                        12079, 12088, seq(12089, 12097), 12080, seq(12098, 12107), 12081, seq(12106, 12115),
                                        12082, seq(12118, 12127), 12083, seq(12128, 12137), 12084, seq(12138, 12147), 12085,
                                        seq(12148, 12157), 12086, seq(12158, 12160), 12087, 12001, seq(12010, 12019), 12002,
                                        seq(12020, 12025)))),
         id = point_number,
         Easting = st_coordinates(.)[,1],
         Northing = st_coordinates(.)[,2]) %>%
  arrange(point_number) %>%
  select(year, block, id = point_number, Easting, Northing, Elevation)

ggplot(filter(lter2009, block == 'a'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 10000))

ggplot(filter(lter2009, block == 'b'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 11000))

ggplot(filter(lter2009, block == 'c'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 12000))
#################################################################################################################

### 2011 ########################################################################################################
lter2011 <- trans2011 %>%
  filter(id != 439 & id != 488 & id != 489 & id != 570 & id != 642) %>%
  arrange(id) %>%
  mutate(Easting = st_coordinates(.)[,1],
         Northing = st_coordinates(.)[,2],
         year = 2011,
         block = ifelse(id < 248,
                        'a',
                        ifelse(id < 454,
                               'b',
                               'c')),
         point_number = ifelse(block == 'a',
                               c(seq(10017, 10001), seq(10018, 10034), seq(10049, 10035), seq(10050, 10064), seq(10079, 10065),
                                 seq(10080, 10094), seq(10107, 10095), seq(10108, 10121), seq(10136, 10122), seq(10137, 10151),
                                 seq(10166, 10152), seq(10167, 10181), seq(10196, 10182), seq(10197, 10211)),
                               ifelse(block == 'b',
                                      c(seq(11200, 11205), seq(11015, 11001), seq(11016, 11030), seq(11045, 11031),
                                        seq(11046, 11060), seq(11075, 11061), seq(11076, 11090), seq(11101, 11091),
                                        seq(11102, 11115), seq(11130, 11116), seq(11131, 11145), seq(11160, 11146),
                                        seq(11161, 11175), seq(11190, 11176), seq(11191, 11199)),
                                      c(seq(12182, 12184), seq(12199, 12185), seq(12016, 12001), seq(12017, 12032), 
                                        seq(12047, 12032), seq(12049, 12064), seq(12080, 12065), seq(12081, 12096), 
                                        seq(12110, 12097), seq(12111, 12124), seq(12139, 12125), seq(12140, 12154), 
                                        seq(12169, 12155), seq(12170, 12181))))) %>%
  arrange(point_number) %>%
  select(year, block, id = point_number, Easting, Northing, Elevation)

ggplot(filter(lter2011, block == 'a'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 10000))

ggplot(filter(lter2011, block == 'b'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 11000))

ggplot(filter(lter2011, block == 'c'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis()#  +
  # geom_text(aes(label = id - 12000))
#################################################################################################################

### 2015 ########################################################################################################
lter2015 <- trans2015 %>%
  filter(id != 481 & !(id >= 538 & id <= 548) & !(id >= 549 & id <= 551)) %>%
  arrange(id) %>%
  mutate(Easting = st_coordinates(.)[,1],
         Northing = st_coordinates(.)[,2],
         year = 2015,
         block = ifelse(fence < 3,
                        'a',
                        ifelse(fence > 4,
                               'c',
                               'b')),
         point_number = ifelse(block == 'a',
                               c(seq(10011, 10025), seq(10040, 10026), seq(10041, 10055), seq(10070, 10056), seq(10071, 10085),
                                 seq(10100, 10086), seq(10101, 10114), seq(10006, 10001), seq(10010, 10007), seq(10128, 10115),
                                 seq(10129, 10143), seq(10158, 10144), seq(10159, 10173), seq(10188, 10174), seq(10189, 10203),
                                 seq(10218, 10204)),
                               ifelse(block == 'b',
                                      c(seq(11159, 11160), seq(11174, 11161), seq(11175, 11188), seq(11010, 11023),
                                        seq(11037, 11024), seq(11038, 11051), seq(11065, 11052), seq(11066, 11079),
                                        seq(11092, 11080), seq(11009, 11001), seq(11093, 11105), seq(11119, 11106),
                                        seq(11120, 11133), seq(11147, 11134), seq(11148, 11158)),
                                      c(seq(12072, 12063), seq(12080, 12096), seq(12113, 12097), seq(12114, 12129),
                                        seq(12011, 12001), seq(12143, 12130), seq(12144, 12158), seq(12173, 12159),
                                        seq(12174, 12188), seq(12203, 12189), seq(12204, 12218), seq(12232, 12219),
                                        seq(12012, 12028), seq(12045, 12029), seq(12046, 12062), seq(12079, 12073))))) %>%
  arrange(point_number) %>%
  select(year, block, id = point_number, Easting, Northing, Elevation)

ggplot(filter(lter2015, block == 'a'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 10000))

ggplot(filter(lter2015, block == 'b'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 11000))

ggplot(filter(lter2015, block == 'c'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 12000))
#################################################################################################################

### 2016 ########################################################################################################
lter2016 <- trans2016 %>%
  st_zm() %>%
  filter(Trans_numb != 288) %>%
  arrange(Trans_numb) %>%
  mutate(Easting = st_coordinates(.)[,1],
         Northing = st_coordinates(.)[,2],
         year = 2016,
         block = ifelse(Trans_numb < 213,
                        'a',
                        ifelse(Trans_numb < 405,
                               'b',
                               'c')),
         point_number = ifelse(block == 'a',
                               Trans_numb + 10000,
                               ifelse(block == 'b',
                                      c(seq(11164, 11170), seq(11191, 11178), seq(11014, 11009), seq(11020, 11015), 
                                        seq(11042, 11037), seq(11048, 11043), seq(11070, 11065), seq(11076, 11071), 
                                        seq(11096, 11093), seq(11092, 11085), seq(11084, 11077), seq(11064, 11057), 
                                        seq(11056, 11049), seq(11036, 11029), seq(11028, 11021), seq(11008, 11001), 
                                        seq(11107, 11097), seq(11121, 11108), seq(11135, 11122), seq(11149, 11136), 
                                        seq(11163, 11150), seq(11177, 11171)),
                                      c(seq(12050, 12035), seq(12068, 12052), seq(12085, 12069), seq(12100, 12086),
                                        seq(12114, 12101), seq(12128, 12115), seq(12143, 12129), seq(12158, 12144),
                                        seq(12174, 12159), seq(12189, 12175), seq(12204, 12190), seq(12219, 12205),
                                        seq(12017, 12001), seq(12034, 12018),
                                        12051)))) %>%
  arrange(point_number) %>%
  select(year, block, id = point_number, Easting, Northing, Elevation)

ggplot(filter(lter2016, block == 'a'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 10000))

ggplot(filter(lter2016, block == 'b'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 11000))

ggplot(filter(lter2016, block == 'c'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 12000))
#################################################################################################################

### 2017 ########################################################################################################
lter2017 <- trans2017 %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name)),
         year = 2017,
         block = ifelse(Name < 11000,
                        'a',
                        ifelse(Name < 12000,
                               'b',
                               'c'))) %>%
  arrange(Name) %>%
  select(year, block, id = Name, Easting, Northing, Elevation)

ggplot(filter(lter2017, block == 'a'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 10000))

ggplot(filter(lter2017, block == 'b'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 11000))

ggplot(filter(lter2017, block == 'c'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 12000))
#################################################################################################################

### 2018 ########################################################################################################
lter2018 <- trans2018 %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name)),
         year = 2018,
         block = ifelse(Name < 11000,
                        'a',
                        ifelse(Name < 12000,
                               'b',
                               'c'))) %>%
  arrange(Name) %>%
  select(year, block, id = Name, Easting, Northing, Elevation)

ggplot(filter(lter2018, block == 'a'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 10000))

ggplot(filter(lter2018, block == 'b'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 11000))

ggplot(filter(lter2018, block == 'c'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() #+
  # geom_text(aes(label = id - 12000))
#################################################################################################################

### 2019 May ####################################################################################################
lter2019_may <- trans2019_may %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name)),
         year = 2019,
         block = ifelse(Name < 11000,
                        'a',
                        ifelse(Name < 12000,
                               'b',
                               'c'))) %>%
  arrange(Name) %>%
  select(year, block, id = Name, Easting, Northing, Elevation)

ggplot(filter(lter2019_may, block == 'a'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 10000))

ggplot(filter(lter2019_may, block == 'b'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 11000))

ggplot(filter(lter2019_may, block == 'c'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 12000))
#################################################################################################################

### 2019 August #################################################################################################
lter2019 <- trans2019_aug %>%
  st_zm() %>%
  mutate(Name = as.numeric(as.character(Name)),
         year = 2019,
         block = ifelse(Name < 11000,
                        'a',
                        ifelse(Name < 12000,
                               'b',
                               'c'))) %>%
  arrange(Name) %>%
  select(year, block, id = Name, Easting, Northing, Elevation)

ggplot(filter(lter2019, block == 'a'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 10000))

ggplot(filter(lter2019, block == 'b'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 11000))

ggplot(filter(lter2019, block == 'c'), aes(x = Easting, y = Northing)) +
  geom_point(aes(color = id)) +
  scale_color_viridis() # +
  # geom_text(aes(label = id - 12000))
#################################################################################################################

### Save Shapefiles #############################################################################################
# st_write(lter2009, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/2009/EML_AK_CiPEHR_GPS_Elevation_2009.shp')
# st_write(lter2011, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/2011/EML_AK_CiPEHR_GPS_Elevation_2011.shp')
# st_write(lter2015, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/2015/EML_AK_CiPEHR_GPS_Elevation_2015.shp')
# st_write(lter2016, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/2016/EML_AK_CiPEHR_GPS_Elevation_2016.shp')
# st_write(lter2017, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/2017/EML_AK_CiPEHR_GPS_Elevation_2017.shp')
# st_write(lter2018, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/2018/EML_AK_CiPEHR_GPS_Elevation_2018.shp')
# st_write(lter2019, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/2019/EML_AK_CiPEHR_GPS_Elevation_2019.shp')
# st_write(lter2019_may, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/2019_may/EML_AK_CiPEHR_GPS_Elevation_2019_may.shp')
#################################################################################################################