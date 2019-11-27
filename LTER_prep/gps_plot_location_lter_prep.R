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
plots <- st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/plot_coordinates_from_2017.shp")
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
