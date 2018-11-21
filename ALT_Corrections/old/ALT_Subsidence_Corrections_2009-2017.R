############################################################################################################
###                           ALT Subsidence Correction                                                  ###
###                            Code written by HGR, Feb 2018                                             ###
############################################################################################################

################################### Load packages necessary for analysis - run every time ###################################
library(plyr)
library(tidyverse)
library(sf)
library(readxl)
library(stringr)
library(raster)
library(ggthemes)
############################################################################################################

##################################### Add 2017 data to combined file - once only - DONE! ####################################
# This is a one time thing, don't repeat for future analyses, remove gas because there is only one measurement for the whole summer
Thaw2017dry <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Thaw Depth/processed/2017/Thaw Depth 2017.xlsx", sheet = 2)
Thaw2017 <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Thaw Depth/processed/2017/Thaw Depth 2017.xlsx")

Control <- Thaw2017 %>%
  slice(-c(337:384)) %>%
  filter(plot == 2 & id != 'offplottrans' & id != 'gas' & date != as.Date("2017-08-25")) %>%
  mutate(plot = 'a') %>%
  dplyr::select(-ww, -sw, -comment)

Thaw2017dry_final <- Thaw2017dry %>%
  slice(-c(127:144)) %>%
  filter(id == 'plot') %>%
  dplyr::select(-comment) %>%
  mutate(block = ifelse(fence == 1 | fence == 2,
                        'a',
                        ifelse(fence == 3 | fence == 4,
                               'b',
                               'c'))) %>%
  full_join(Control) %>%
  arrange(date, block, fence, plot) %>%
  dplyr::select(-ww, -sw)

write.csv(Thaw2017dry_final, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/2017_DryPEHR_Thaw_Depth_Formatted.csv")
############################################################################################################

##############Create cumulative file (all plots, all experiments, all years) of ALT - once only - DONE!######################
# Load ALT files
CiPEHRThaw <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Thaw Depth/processed/Cumulative CiPEHR Thaw Data.xlsx")
DryPEHRThaw <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Thaw Depth/processed/Cumulative DryPEHR Thaw Data.xlsx", sheet = 6)
GradientThaw <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/Gradient/Thaw Depth/processed/Cumulative Gradient Thaw Data.xlsx", sheet = 1)

# Prep CiPEHR and DryPEHR dataframes to join to do analysis all at once
CiPEHRThaw1 <- CiPEHRThaw %>%
  mutate(exp = 'CiPEHR',
         plot = as.character(plot),
         year = str_sub(date, 1, 4),
         date = as.Date(date),
         woy = as.integer(strftime(date, format="%j")) %/% 7) %>%
  dplyr::select(year, exp, fence, plot, id, td, date, woy) %>%
  arrange(year,id, fence, plot, date) %>%
  filter(woy == 36)

DryPEHRThaw1 <- DryPEHRThaw %>%
  mutate(exp = 'DryPEHR',
         year = str_sub(date, 1, 4),
         plot = ifelse(plot == 'A',
                       'a',
                       ifelse(plot == 'B',
                              'b',
                              ifelse(plot == 'C',
                                     'c',
                                     ifelse(plot == 'D',
                                            'd',
                                            plot)))),
         date = as.Date(date),
         woy = as.integer(strftime(date, format="%j")) %/% 7) %>%
  dplyr::select(year, exp, fence, plot, id, td, date, woy) %>%
  arrange(year,id, fence, plot, date) %>%
  group_by(year, fence, plot, id) %>%
  filter(woy == 36)

# Join CiPEHR and DryPEHR into one cumulative data frame
Thawdata <- CiPEHRThaw1 %>%
  rbind.data.frame(DryPEHRThaw1) %>%
  arrange(year, exp, id, fence, plot, date) %>%
  mutate(ALT = td) %>%
  dplyr::select(-td)

# write files to gps folder on my computer
# write.csv(Thawdata, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Thawdata_for_correction_2009_2017_v2.csv", row.names = FALSE)
# write.csv(GradientThaw1, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Gradient_Thawdata_for_correction_2009_2017.csv", row.names = FALSE)

############################################################################################################

######################### Load Data - Start here! ####################################
# Cumulative thaw data
ALTdata <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Thawdata_for_correction_2009_2017_v2.csv")
GradientALTdata <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Gradient_Thawdata_for_correction_2009_2017.csv")

# Subsidence rasters CiPEHR
A17_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2017SubClip.tif")
B17_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2017SubClip.tif")
C17_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2017SubClip.tif")
A16_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2016SubClip.tif")
B16_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2016SubClip.tif")
C16_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2016SubClip.tif")
A15_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2015SubClip.tif")
B15_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2015SubClip.tif")
C15_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2015SubClip.tif")
A11_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2011SubClip.tif")
B11_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2011SubClip.tif")
C11_Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2011SubClip.tif")

# CiPEHR - Stack the subsidence rasters which are calculated from 2009
AstackC <- stack(A11_Sub, A15_Sub, A16_Sub, A17_Sub)
BstackC <- stack(B11_Sub, B15_Sub, B16_Sub, B17_Sub)
CstackC <- stack(C11_Sub, C15_Sub, C16_Sub, C17_Sub)

# Subsidence rasters DryPEHR
A17_11Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2017_11SubClip.tif")
B17_11Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2017_11SubClip.tif")
C17_11Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2017_11SubClip.tif")
A16_11Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2016_11SubClip.tif")
B16_11Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2016_11SubClip.tif")
C16_11Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2016_11SubClip.tif")
A15_11Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2015_11SubClip.tif")
B15_11Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2015_11SubClip.tif")
C15_11Sub <- raster("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2015_11SubClip.tif")

# DryPEHR - Stack the subsidence rasters which are calculated from 2011
AstackD <- stack(A15_11Sub, A16_11Sub, A17_11Sub)
BstackD <- stack(B15_11Sub, B16_11Sub, B17_11Sub)
CstackD <- stack(C15_11Sub, C16_11Sub, C17_11Sub)

# 2017 shapefile for location of plots
Points2017 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp")
############################################################################################################

########################## Format points2017 and separate into exp for extraction ######################################
plotcoords <- Points2017 %>%
  dplyr::select(Name, Easting, Northing, Elevation, geometry, Type) %>%
  filter(Type == 'flux') %>%
  mutate(fence = as.integer(str_sub(Name, start = 1, end = 1)),
         plot = as.character(str_sub(Name, start = 3, end = 3)),
         exp = ifelse(plot == 'b' | plot == 'c' | plot == 'd',
                      'DryPEHR',
                      'CiPEHR')) %>%
  dplyr::select(-Type) %>%
  st_zm(drop = TRUE, what = "ZM")

# st_write(obj = plotcoords, dsn = "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/plot_coordinates_from_2017.shp", layer = "plot_coordinates_from_2017.shp")

plotcoordsAC <- plotcoords %>%
  filter(fence == 1 & exp == 'CiPEHR' | fence == 2 & exp == 'CiPEHR') %>%
  dplyr::select(Easting, Northing)

plotcoordsBC <- plotcoords %>%
  filter(fence == 3 & exp == 'CiPEHR' | fence == 4 & exp == 'CiPEHR') %>%
  dplyr::select(Easting, Northing)

plotcoordsCC <- plotcoords %>%
  filter(fence == 5 & exp == 'CiPEHR' | fence == 6 & exp == 'CiPEHR') %>%
  dplyr::select(Easting, Northing)

plotcoordsAD <- plotcoords %>%
  filter(fence == 1 & exp == 'DryPEHR' | fence == 2 & exp == 'DryPEHR') %>%
  dplyr::select(Easting, Northing)

plotcoordsBD <- plotcoords %>%
  filter(fence == 3 & exp == 'DryPEHR' | fence == 4 & exp == 'DryPEHR') %>%
  dplyr::select(Easting, Northing)

plotcoordsCD <- plotcoords %>%
  filter(fence == 5 & exp == 'DryPEHR' | fence == 6 & exp == 'DryPEHR') %>%
  dplyr::select(Easting, Northing)

ALTdata1 <- ALTdata %>%
  filter(id == 'plot') %>%
  mutate(exp = as.character(exp),
         plot = as.character(plot),
         block = ifelse(fence == 1 | fence == 2,
                        'A',
                        ifelse(fence == 3 | fence == 4,
                               'B',
                               'C')),
         treatment = ifelse(plot == '2' | plot == '4' | plot == 'a',
                            'Control',
                            ifelse(plot == '1' | plot == '3',
                                   'Air Warming',
                                   ifelse(plot == '5' | plot == '7',
                                          'Air + Soil Warming',
                                          ifelse(plot == '6' | plot == '8',
                                                 'Soil Warming',
                                                 ifelse(plot == 'b',
                                                        'Drying',
                                                        ifelse(plot == 'c',
                                                               'Warming',
                                                               'Drying + Warming'))))))) %>%
  group_by(year, exp, fence, plot) %>%
  filter(ALT == max(ALT)) %>%
  dplyr::select(-id, -date, -woy)

DryPEHRa <- ALTdata1 %>%
  ungroup() %>%
  filter(plot == '2' & year >= 2011) %>%
  mutate(plot = 'a',
         exp = 'DryPEHR')

ALTdata2 <- ALTdata1 %>%
  full_join(DryPEHRa, by = c("year", "exp", "fence", "plot", "ALT", "block", "treatment")) %>%
  arrange(year, exp, block, fence, plot)
############################################################################################################

######## Extract subsidence values from rasters using plot locations from plotcoords data frames and format to join with plotcoords again ########
### CiPEHR
AsubpointsC <- extract(AstackC, plotcoordsAC, layer = 1, nl = 4) %>%
  as.data.frame() %>%
  mutate(block = 'A',
         sub2011 = A2011SubClip,
         sub2015 = A2015SubClip,
         sub2016 = A2016SubClip,
         sub2017 = A2017SubClip) %>%
  dplyr::select(block, sub2011, sub2015, sub2016, sub2017) %>%
  cbind.data.frame(plotcoordsAC)

BsubpointsC <- extract(BstackC, plotcoordsBC, layer = 1, nl = 4) %>%
  as.data.frame() %>%
  mutate(block = 'B',
         sub2011 = B2011SubClip,
         sub2015 = B2015SubClip,
         sub2016 = B2016SubClip,
         sub2017 = B2017SubClip) %>%
  dplyr::select(block, sub2011, sub2015, sub2016, sub2017) %>%
  cbind.data.frame(plotcoordsBC)

CsubpointsC <- extract(CstackC, plotcoordsCC, layer = 1, nl = 4) %>%
  as.data.frame() %>%
  mutate(block = 'C',
         sub2011 = C2011SubClip,
         sub2015 = C2015SubClip,
         sub2016 = C2016SubClip,
         sub2017 = C2017SubClip) %>%
  dplyr::select(block, sub2011, sub2015, sub2016, sub2017) %>%
  cbind.data.frame(plotcoordsCC)

# join the cipehr subsidence data frames 
subpointsC <- AsubpointsC %>%
  rbind.data.frame(BsubpointsC, CsubpointsC) %>%
  left_join(plotcoords, by = c('Easting', 'Northing')) %>%
  gather(key = year,
         value = subsidence,
         sub2011:sub2017) %>%
  mutate(year = as.numeric(str_sub(year, start = 4)),
         treatment = ifelse(plot == 1 | plot == 3,
                            'Air Warming',
                            ifelse(plot == 2 | plot == 4,
                                   'Control',
                                   ifelse(plot == 5 | plot == 7,
                                          'Air + Soil Warming',
                                          'Soil Warming')))) %>%
  dplyr::select(-geometry.y) %>%
  rename(geometry = geometry.x)

### DryPEHR
AsubpointsD <- extract(AstackD, plotcoordsAD, layer = 1, nl = 4) %>%
  as.data.frame() %>%
  mutate(block = 'A',
         sub2015 = A2015_11SubClip,
         sub2016 = A2016_11SubClip,
         sub2017 = A2017_11SubClip) %>%
  dplyr::select(block, sub2015, sub2016, sub2017) %>%
  cbind.data.frame(plotcoordsAD)

BsubpointsD <- extract(BstackD, plotcoordsBD, layer = 1, nl = 4) %>%
  as.data.frame() %>%
  mutate(block = 'B',
         sub2015 = B2015_11SubClip,
         sub2016 = B2016_11SubClip,
         sub2017 = B2017_11SubClip) %>%
  dplyr::select(block, sub2015, sub2016, sub2017) %>%
  cbind.data.frame(plotcoordsBD)

CsubpointsD <- extract(CstackD, plotcoordsCD, layer = 1, nl = 4) %>%
  as.data.frame() %>%
  mutate(block = 'C',
         sub2015 = C2015_11SubClip,
         sub2016 = C2016_11SubClip,
         sub2017 = C2017_11SubClip) %>%
  dplyr::select(block, sub2015, sub2016, sub2017) %>%
  cbind.data.frame(plotcoordsCD)

# join drypehr subsidence data frames
subpointsD <- AsubpointsD %>%
  rbind.data.frame(BsubpointsD, CsubpointsD) %>%
  left_join(plotcoords, by = c('Easting', 'Northing')) %>%
  gather(key = year,
         value = subsidence,
         sub2015:sub2017) %>%
  mutate(year = as.numeric(str_sub(year, start = 4)),
         treatment = ifelse(plot == 'b',
                            'Drying',
                            ifelse(plot == 'c',
                                   'Warming',
                                   'Drying + Warming'))) %>%
  dplyr::select(-geometry.y) %>%
  rename(geometry = geometry.x)

# Make zero subsidence values for 2009 CiPEHR and 2011 DryPEHR
# Points2009 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2009_SPCSAK4.shp")
# 
# Points2009 <- Points2009 %>%
#   filter(type == 'flux' | type == 'gw') %>%
#   mutate(year = 2009,
#          exp = 'CiPEHR',
#          plot = as.numeric(plot),
#          block = ifelse(fence == 1 | fence == 2,
#                         'A',
#                         ifelse(fence == 3 | fence == 4,
#                                'B',
#                                'C')),
#          treatment = ifelse(plot == 1 | plot == 3,
#                             'Air Warming',
#                             ifelse(plot == 2 | plot == 4,
#                                    'Control',
#                                    ifelse(plot == 5 | plot == 7,
#                                           'Air + Soil Warming',
#                                           'Soil Warming'))),
#          subsidence = 0) %>%
#   dplyr::select(year, exp, block, fence, type, plot, treatment, subsidence)
# 
# Points2011 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2011_SPCSAK4.shp")
# 
# Points2011 <- Points2011 %>%
#   filter(type == 'flux' | type == 'gw') %>%
#   filter(transect == 'b' | transect == 'c' | transect == 'd') %>%
#   mutate(year = 2011,
#          exp = 'DryPEHR',
#          plot = transect,
#          block = ifelse(fence == 1 | fence == 2,
#                         'A',
#                         ifelse(fence == 3 | fence == 4,
#                                'B',
#                                'C')),
#          treatment = ifelse(plot == 'b',
#                             'Drying',
#                             ifelse(plot == 'c',
#                                    'Warming',
#                                    'Drying + Warming')),
#          subsidence = 0) %>%
#   dplyr::select(year, exp, block, fence, type, plot, treatment, subsidence)

Sub2009 <- ALTdata2 %>%
  ungroup() %>%
  filter(year == 2009) %>%
  dplyr::select(-ALT) %>%
  mutate(subsidence = 0) %>%
  left_join(plotcoords, by = c('fence', 'plot', 'exp')) %>%
  dplyr::select(year, exp, block, fence, plot, treatment, subsidence, geometry)


Sub2011 <- ALTdata2 %>%
  ungroup() %>%
  filter(year == 2011 & exp == 'DryPEHR') %>%
  dplyr::select(-ALT) %>%
  ungroup() %>%
  mutate(subsidence = 0,
         plot = ifelse(plot == 'a',
                       '2',
                       plot)) %>%
  left_join(plotcoords, by = c('fence', 'plot')) %>%
  mutate(plot = ifelse(plot == '2',
                       'a',
                       plot)) %>%
  dplyr::select(year, exp = exp.x, block, fence, plot, treatment, subsidence, geometry)

# Join cipehr, drypehr, and 2009. Make new time column setting first year to zero.
subpoints <- subpointsC %>%
  rbind.data.frame(subpointsD) %>%
  dplyr::select(year, exp, block, fence, plot, treatment, subsidence, geometry) %>%
  rbind.data.frame(Sub2009, Sub2011) %>%
  arrange(year, exp, block, fence, plot) %>%
  mutate(time = ifelse(exp == 'CiPEHR',
                       year - 2009,
                       year - 2011),
         year = as.integer(year),
         fence = as.integer(fence))

# write.csv(subpoints, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Subsidence_2009_2017.csv', row.names = FALSE)

############################################################################################################
  
######################### Linear Models ####################################################################
#Linear models - this one for each plot individually
submodel <- function(df) {fit <- lm(subsidence ~ time,  data=df)
return(cbind(
  reg.intercept = summary(fit)$coefficients[1],
  reg.slope = summary(fit)$coefficients[2],
  reg.r2 = summary(fit)$r.squared,
  reg.pvalue = anova(fit)$'Pr(>F)'[1]
))}

coefs_subsidence <- ddply(subpoints, .(fence, plot), submodel) %>%
  mutate(fence = as.integer(fence))

# write.csv(coefs_subsidence, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/CiPEHR_DryPEHR_Subsidence_Regression_Coefficients.csv', row.names = FALSE)
############################################################################################################

######################### Merge points2017 and regression coefficients and gap fill with regressions ###########################
subfill <- subpoints %>%
  dplyr::select(-time) %>%
  left_join(coefs_subsidence, by = c("fence", "plot")) %>%
  mutate(year = str_c('sub', year, sep = '')) %>%
  filter(plot != 'a') %>%
  spread(key = year, value = subsidence) %>%
  mutate(sub2010 = ifelse(exp == 'CiPEHR',
                          reg.intercept+reg.slope*1,
                          NA),
         sub2012 = ifelse(exp == 'CiPEHR',
                          reg.intercept+reg.slope*3,
                          reg.intercept+reg.slope*1),
         sub2013 = ifelse(exp == 'CiPEHR',
                          reg.intercept+reg.slope*4,
                          reg.intercept+reg.slope*2),
         sub2014 = ifelse(exp == 'CiPEHR',
                          reg.intercept+reg.slope*5,
                          reg.intercept+reg.slope*3)) %>%
  gather(key = year, value = subsidence, sub2009:sub2014) %>%
  mutate(year = as.integer(str_sub(year, start = 4)))

# write.csv(subfill, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Correction_Regressions_2009_2017.csv', row.names = FALSE)

# Add in drypehr a values from cipehr 2
DryPEHRa <- subfill %>%
  filter(plot == '2' & year >= 2011) %>%
  mutate(plot = 'a',
         exp = 'DryPEHR')

# join subsidence and alt data and make correction
ALTsub <- subfill %>%
  rbind.data.frame(DryPEHRa) %>%
  right_join(ALTdata1, by = c("year", "exp", "block", "fence", "plot", "treatment")) %>%
  mutate(ALT = ALT*-1,
         ALT.corrected = ifelse(reg.slope < 0,
                                ALT+subsidence*100,
                                ALT)) %>%
  dplyr::select(year, exp, block, fence, plot, treatment, reg.intercept, reg.slope, reg.r2, reg.pvalue, subsidence, ALT, ALT.corrected) %>%
  arrange(year, exp, block, fence, plot)

# Write file
# write.csv(ALTsub, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017.csv', row.names = FALSE)
############################################################################################################

ALTsub <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017.csv')
subpoints <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Subsidence_2009_2017.csv') %>%
  mutate(longitude = as.numeric(str_sub(longitude, start = 3)),
         latitude = as.numeric(str_sub(latitude, start = 1, end = -2))) %>%
  st_as_sf(coords = c('longitude', 'latitude'))

ALTsubgraph <- ALTsub %>%
  gather(key = Measurement_Type, value = ALT, ALT:ALT.corrected) %>%
  arrange(desc(Measurement_Type), year, exp, block, fence, plot) %>%
  filter(plot != 'a') %>%
  mutate(plot = factor(plot, levels = c('1', '2', '3', '4', 'b', '5', '6', '7', '8', 'c', 'd')))

subpoints2 <- subpoints %>%
  mutate(plot = factor(plot, levels = c('1', '2', '3', '4', 'b', '5', '6', '7', '8', 'c', 'd')),
         treatment = ifelse(treatment == 'Air Warming' | treatment == 'Control' | treatment == 'Drying',
                            'Control',
                            'Warming'))

# plots
g1 <- ggplot(subset(subpoints2, type == 'flux'), aes(x = year, y = subsidence)) +
  geom_point(aes(color = treatment)) +
  geom_smooth(method = 'lm', color = 'black') +
  facet_grid(fence~plot) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016),
                     name = '') +
  scale_y_continuous(name = 'Subsidence (m)') +
  ggtitle('Modeled Subsidence by Plot') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle = 60, vjust = 1, hjust = 1, size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12)) +
  scale_color_manual(name = 'Treatment',
                     breaks=c("Control", "Warming"),
                     values=c("#006666", "#CC3300"))

g1
# ggsave(plot = g1, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Figures/Subsidence_lm.jpg", height = 8, width = 12)

g2 <- ggplot(ALTsubgraph, aes(x = year, y = ALT, color = Measurement_Type)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000", "#ff0000"),
                     labels = c('ALT', 'Corrected ALT')) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016),
                     name = '') +
  scale_y_continuous(name = 'ALT (cm)') +
  facet_grid(fence ~ plot) +
  ggtitle('Comparison of Original and Corrected ALT') +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text.x  = element_text(angle = 60, vjust = 1, hjust = 1, size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
g2
# ggsave(plot = g2, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Figures/Corrected_ALT.jpg", height = 8, width = 12)
