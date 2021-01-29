###########################################################################################
### Plot level subsidence for uncorrected elevation data to determine correction values ###
###                             Code by HGR 9/2018                                      ###
###########################################################################################

#Required packages to run the following code
#library(sp)
#library(rgdal)
#library(raster)
#library(dplyr)
#library(stringr)

###Required packages to do this with tidyverse
library(plyr)
library(sf)
library(tidyverse)
library(raster)

##############################Load files##############################
# plot locations
plotcoords <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/plot_coordinates_from_2017.shp")

# ALT data
ALTdata <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Thawdata_for_correction_2009_2017_v2.csv")

# elevation surfaces
A2009K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2009K.tif")
B2009K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2009K.tif")
C2009K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2009K.tif")
A2011K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2011K.tif")
B2011K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2011K.tif")
C2011K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2011K.tif")
A2015K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2015K.tif")
B2015K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2015K.tif")
C2015K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2015K.tif")
A2016K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2016K.tif")
B2016K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2016K.tif")
C2016K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2016K.tif")
A2017K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2017K.tif")
B2017K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2017K.tif")
C2017K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2017K.tif")
A2018K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2018K.tif")
B2018K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2018K.tif")
C2018K <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2018K.tif")
###########################################################################################

########################## Format points2017 and separate into exp for extraction ######################################
DryPEHRa <- plotcoords %>%
  filter(plot == '2') %>%
  mutate(plot = 'a',
         exp = 'DryPEHR')

plotcoords <- plotcoords %>%
  rbind.data.frame(DryPEHRa) %>%
  mutate(plot = as.character(plot)) %>%
  dplyr::select(exp, fence, plot, Easting, Northing)

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
  arrange(plot) %>%
  dplyr::select(Easting, Northing)

plotcoordsBD <- plotcoords %>%
  filter(fence == 3 & exp == 'DryPEHR' | fence == 4 & exp == 'DryPEHR') %>%
  arrange(plot) %>%
  dplyr::select(Easting, Northing)

plotcoordsCD <- plotcoords %>%
  filter(fence == 5 & exp == 'DryPEHR' | fence == 6 & exp == 'DryPEHR') %>%
  arrange(plot) %>%
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
###########################################################################################

######################## calc uncorrected subsidence ##################
# Resample all rasters to 2017 grid
A2009re <- resample(A2009K, A2017K, method = "bilinear")
B2009re <- resample(B2009K, B2017K, method = "bilinear")
C2009re <- resample(C2009K, C2017K, method = "bilinear")
A2011re <- resample(A2011K, A2017K, method = "bilinear")
B2011re <- resample(B2011K, B2017K, method = "bilinear")
C2011re <- resample(C2011K, C2017K, method = "bilinear")
A2015re <- resample(A2015K, A2017K, method = "bilinear")
B2015re <- resample(B2015K, B2017K, method = "bilinear")
C2015re <- resample(C2015K, C2017K, method = "bilinear")
A2016re <- resample(A2016K, A2017K, method = "bilinear")
B2016re <- resample(B2016K, B2017K, method = "bilinear")
C2016re <- resample(C2016K, C2017K, method = "bilinear")
A2018re <- resample(A2018K, A2017K, method = "bilinear")
B2018re <- resample(B2018K, B2017K, method = "bilinear")
C2018re <- resample(C2018K, C2017K, method = "bilinear")

# Calculate subsidence - finally!
A18_09Sub <- A2018K-A2009re
B18_09Sub <- B2018K-B2009re
C18_09Sub <- C2018K-C2009re
A17_09Sub <- A2017K-A2009re
B17_09Sub <- B2017K-B2009re
C17_09Sub <- C2017K-C2009re
A16_09Sub <- A2016re-A2009re
B16_09Sub <- B2016re-B2009re
C16_09Sub <- C2016re-C2009re
A15_09Sub <- A2015re-A2009re
B15_09Sub <- B2015re-B2009re
C15_09Sub <- C2015re-C2009re
A11_09Sub <- A2011re-A2009re
B11_09Sub <- B2011re-B2009re
C11_09Sub <- C2011re-C2009re

A18_11Sub <- A2018K-A2011re
B18_11Sub <- B2018K-B2011re
C18_11Sub <- C2018K-C2011re
A17_11Sub <- A2017K-A2011re
B17_11Sub <- B2017K-B2011re
C17_11Sub <- C2017K-C2011re
A16_11Sub <- A2016re-A2011re
B16_11Sub <- B2016re-B2011re
C16_11Sub <- C2016re-C2011re
A15_11Sub <- A2015re-A2011re
B15_11Sub <- B2015re-B2011re
C15_11Sub <- C2015re-C2011re

# create stacks
AstackC <- stack(A11_09Sub, A15_09Sub, A16_09Sub, A17_09Sub, A18_09Sub)
BstackC <- stack(B11_09Sub, B15_09Sub, B16_09Sub, B17_09Sub, B18_09Sub)
CstackC <- stack(C11_09Sub, C15_09Sub, C16_09Sub, C17_09Sub, C18_09Sub)

AstackD <- stack(A15_11Sub, A16_11Sub, A17_11Sub, A18_11Sub)
BstackD <- stack(B15_11Sub, B16_11Sub, B17_11Sub, B18_11Sub)
CstackD <- stack(C15_11Sub, C16_11Sub, C17_11Sub, C18_11Sub)

###########################################################################################

######################### Extract subsidence/elevation values at plots ###########################
### CiPEHR
AsubpointsC <- raster::extract(AstackC, plotcoordsAC, layer = 1, nl = 5) %>%
  as.data.frame() %>%
  mutate(block = 'A',
         sub2011 = layer.1,
         sub2015 = layer.2,
         sub2016 = layer.3,
         sub2017 = layer.4,
         sub2018 = layer.5) %>%
  dplyr::select(block,sub2011, sub2015, sub2016, sub2017, sub2018) %>%
  cbind.data.frame(plotcoordsAC)

BsubpointsC <- raster::extract(BstackC, plotcoordsBC, layer = 1, nl = 5) %>%
  as.data.frame() %>%
  mutate(block = 'B',
         sub2011 = layer.1,
         sub2015 = layer.2,
         sub2016 = layer.3,
         sub2017 = layer.4,
         sub2018 = layer.5) %>%
  dplyr::select(block, sub2011, sub2015, sub2016, sub2017, sub2018) %>%
  cbind.data.frame(plotcoordsBC)

CsubpointsC <- raster::extract(CstackC, plotcoordsCC, layer = 1, nl = 5) %>%
  as.data.frame() %>%
  mutate(block = 'C',
         sub2011 = layer.1,
         sub2015 = layer.2,
         sub2016 = layer.3,
         sub2017 = layer.4,
         sub2018 = layer.5) %>%
  dplyr::select(block, sub2011, sub2015, sub2016, sub2017, sub2018) %>%
  cbind.data.frame(plotcoordsCC)

# join the cipehr subsidence data frames 
subpointsC <- AsubpointsC %>%
  rbind.data.frame(BsubpointsC, CsubpointsC) %>%
  left_join(subset(plotcoords, exp == 'CiPEHR'), by = c('Easting', 'Northing')) %>%
  gather(key = year,
         value = subsidence,
         sub2011:sub2018) %>%
  mutate(year = as.numeric(str_sub(year, start = 4)),
         treatment = ifelse(plot == 1 | plot == 3,
                            'Air Warming',
                            ifelse(plot == 2 | plot == 4,
                                   'Control',
                                   ifelse(plot == 5 | plot == 7,
                                          'Air + Soil Warming',
                                          'Soil Warming'))))

### DryPEHR
AsubpointsD <- raster::extract(AstackD, plotcoordsAD, layer = 1, nl = 4) %>%
  as.data.frame() %>%
  mutate(block = 'A',
         sub2015 = layer.1,
         sub2016 = layer.2,
         sub2017 = layer.3,
         sub2018 = layer.4) %>%
  dplyr::select(block, sub2015, sub2016, sub2017, sub2018) %>%
  cbind.data.frame(plotcoordsAD)

BsubpointsD <- raster::extract(BstackD, plotcoordsBD, layer = 1, nl = 4) %>%
  as.data.frame() %>%
  mutate(block = 'B',
         sub2015 = layer.1,
         sub2016 = layer.2,
         sub2017 = layer.3,
         sub2018 = layer.4) %>%
  dplyr::select(block, sub2015, sub2016, sub2017, sub2018) %>%
  cbind.data.frame(plotcoordsBD)

CsubpointsD <- raster::extract(CstackD, plotcoordsCD, layer = 1, nl = 5) %>%
  as.data.frame() %>%
  mutate(block = 'C',
         sub2015 = layer.1,
         sub2016 = layer.2,
         sub2017 = layer.3,
         sub2018 = layer.4) %>%
  dplyr::select(block, sub2015, sub2016, sub2017, sub2018) %>%
  cbind.data.frame(plotcoordsCD)


# join CiPEHR and DryPEHR subsidence and format
subpoints <- AsubpointsD %>%
  rbind.data.frame(BsubpointsD, CsubpointsD) %>%
  left_join(subset(plotcoords, exp == 'DryPEHR'), by = c('Easting', 'Northing')) %>%
  gather(key = year,
         value = subsidence,
         sub2015:sub2018) %>%
  mutate(year = as.numeric(str_sub(year, start = 4)),
         treatment = ifelse(plot == 'a',
                            'Control',
                            ifelse(plot == 'b',
                                   'Drying',
                                   ifelse(plot == 'c',
                                          'Warming',
                                          'Drying + Warming')))) %>%
  rbind.data.frame(subpointsC) %>%
  mutate(plot = as.character(plot)) %>%
  dplyr::select(year, exp, block, fence, plot, treatment, Easting, Northing, subsidence) %>%
  arrange(year, fence, plot)

# make a copy of plots to create 2009 and 2011 data
early.data <- subpoints %>%
  filter(year == 2011 & exp == 'CiPEHR' | year == 2015 & exp == 'DryPEHR') %>%
  mutate(year = ifelse(year == 2011,
                       2009,
                       2011),
         subsidence = 0)
###########################################################################################

######################## Join with ALT data ###############################################
sub.alt <- subpoints %>%
  rbind.data.frame(early.data) %>%
  full_join(ALTdata2, by = c('year', 'exp', 'block', 'fence', 'plot', 'treatment')) %>%
  dplyr:: select(-Easting, -Northing) %>%
  left_join(plotcoords, by = c('exp', 'fence', 'plot')) %>%
  arrange(exp, year, fence, plot)
###########################################################################################

############ Test Subsidence and ALT Values for equal ALT/Sub slope #######################
sub.alt.summary <- sub.alt %>%
  mutate(treatment2 = ifelse(treatment == 'Control' | treatment == 'Air Warming' | treatment == 'Drying',
                             'Control',
                             'Warming')) %>%
  group_by(year, treatment2) %>%
  summarise(ALT.uncorrected = mean(ALT, na.rm = TRUE),
            ALT.corrected = mean(ALT - subsidence, na.rm = TRUE),
            sub.mean = mean(subsidence, na.rm = TRUE)*100) %>%
  group_by(treatment2) %>%
  mutate(sub.test = ifelse(year == 2009| year == 2011,
                           sub.mean,
                           ifelse(year == 2015,
                                  sub.mean + 78,
                                  ifelse(year == 2016 | year == 2017,
                                         sub.mean - 129,
                                         NA))),
         ALT.test = ALT.uncorrected - sub.test) %>%
  ungroup()

# plot the test variables
# subsidence by alt (alt is not normalized, subsidence is geoid corrected)
g1 <- ggplot(sub.alt.summary, aes(x = ALT.test, y = sub.test, color = treatment2)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle("Relationship between test subsidence and test ALT")

# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_ALT_Ratio_Correction_lm.jpg")
# In this test, I was working directly with subsidence, which is relative to 2009, while when I do elevation corrections to raw GPS data
# it is relative to 2017.  This results in different correction values than in the code above. Raw elevation data look like below:
#                  __2017__ 
#
#  __2009__
#
#          __2015__
#
# But should look like this:
#
# __2009__
#
#
#         __2015__
#
#                 __2017__
# 
# This means that the correction for 2009 data is 129 cm (the negative value of the correction for 2017 above).
# The correction for the 2015 data is 129 cm + 78 cm (the correction in the code above) = 207 cm.

ALT_sub_ratio_elevation_corrections <- data.frame(year = c(2009, 2011, 2015, 2016, 2017, 2018),
                                                  `correction (cm)` = c(129, 129, 207, 0, 0, 0))

# write.csv(ALT_sub_ratio_elevation_corrections, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Elevation_Corrections_Sub_ALT_Ratio.csv')
