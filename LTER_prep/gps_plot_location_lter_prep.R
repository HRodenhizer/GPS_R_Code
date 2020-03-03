#################################################################################################################
###                                 LTER Prep for GPS Plot Location Data                                      ###
###                                          Code by HGR 11/19                                                ###
#################################################################################################################

### load libraries ##############################################################################################
library(sf)
library(tidyverse)
library(viridis)
library(schoolmath)
#################################################################################################################

### load data ###################################################################################################
plots <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/plot_coordinates_from_2017.shp')
points2017 <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp')
#################################################################################################################

### split into experiments ######################################################################################
plots_cip <- plots %>%
  filter(exp == 'CiPEHR') %>%
  mutate(plot = as.numeric(plot),
         WW = ifelse(plot <= 4,
                     'C',
                     'WW'),
         SW = ifelse(is.even(plot),
                     'C',
                     'SW'),
         treatment = ifelse(WW == 'C' & SW == 'C',
                            'control',
                            ifelse(WW == 'C' & SW == 'SW',
                                   'air warming',
                                   ifelse(WW == 'WW' & SW == 'C',
                                          'soil warming',
                                          'air + soil warming'))),
         block = ifelse(fence <= 2,
                        'a',
                        ifelse(fence <= 4,
                               'b',
                               'c'))) %>%
  select(exp, block, fence, plot, WW, SW, treatment)

plots_dry <- plots %>%
  filter(exp == 'DryPEHR' | plot == '2') %>%
  mutate(exp = 'DryPEHR',
         block = ifelse(fence <= 2,
                        'a',
                        ifelse(fence <= 4,
                               'b',
                               'c')),
         plot = ifelse(plot == '2',
                       'a',
                       as.character(plot)),
         WW = ifelse(plot == 'a' | plot == 'b',
                     'C',
                     'WW'),
         SW = ifelse(WW == 'C',
                     'C',
                     'WW'),
         dry = ifelse(plot == 'a' | plot == 'c',
                      'C',
                      'D'),
         treatment = ifelse(WW == 'C' & dry == 'C',
                            'control',
                            ifelse(WW == 'WW' & dry == 'C',
                                   'warming',
                                   ifelse(WW == 'C' & dry == 'D',
                                          'drying',
                                          'warming + drying')))) %>%
  select(exp, block, fence, plot, WW, SW, dry, treatment)

plots_grad <- points2017 %>%
  filter(Type == 'grad') %>%
  st_zm() %>%
  mutate(exp = 'Gradient',
         plot = as.numeric(str_sub(as.character(Name), start = 5)),
         site = ifelse(plot <= 12,
                       'extensive',
                       ifelse(plot <= 24,
                              'moderate',
                              'minimal'))) %>%
  select(exp, site, plot)
#################################################################################################################

### save shapefiles #############################################################################################
# st_write(plots_cip, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/plot_locations/CiPEHR/EML_AK_CiPEHR_GPS_Plot_Locations.shp')
# st_write(plots_dry, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/plot_locations/DryPEHR/EML_AK_DryPEHR_GPS_Plot_Locations.shp')
# st_write(plots_grad, 'Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/LTER_files/plot_locations/Gradient/EML_AK_Gradient_GPS_Plot_Locations.shp')
#################################################################################################################