####################################################################################################################################
###                         Subsidence from soil cores/ what part of soil is compacting?                                         ###
###                                                HGR 10/2018                                                                   ###
####################################################################################################################################

### load libraries #################################################################################################################
library(raster)
library(readxl)
library(sf)
library(ggthemes)
library(lme4)
library(lmerTest)
library(viridis)
library(MuMIn)
library(merTools)
library(tidyverse)
library(R.utils)
####################################################################################################################################

### load data ######################################################################################################################
soil <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/Soil_09_17.csv')
### units of soil_09_17 ###
# depth.cat, depth0, depth1 (cm)
# moisture, ash, C, N (g/kg)
# bulk.density (g/cm^3)
# C/N (unitless)
# delta13C, delta15N (per mil)
# x.stock (stocks of soil, N, C, etc.), cu.x.stock (cumulative stocks) (kg/m^2)

# read in water well location data and subsidence data for 2013
water_wells <- read_sf('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/water_wells.shp') %>%
  filter(!(str_detect(Name, pattern = '^ww.*n$') | str_detect(Name, pattern = '^ww.*s$'))) %>%
  dplyr::select(Name, Easting, Northing) %>%
  mutate(year = ifelse(str_detect(Name, pattern = '^ww.*5$'),
                       2013,
                       ifelse(str_detect(Name, pattern = '^ww.*17$'),
                              2017,
                              2009)),
         fence = as.numeric(str_sub(Name, 3, 3)),
         block = ifelse(fence == 1 | fence == 2,
                        'a',
                        ifelse(fence == 3 | fence == 4,
                               'b',
                               'c')),
         well = str_sub(Name, 5)) %>%
  st_zm()
coords <- st_coordinates(water_wells)
water_wells <- water_wells %>%
  mutate(Easting = coords[,1],
         Northing = coords[,2])
# find elevation files in directory
filenames <- list.files("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks", full.names = TRUE, pattern = '^.*.tif$')
# make a list of elevation raster stacks for each block
Elevation <- list(brick(filenames[1]), brick(filenames[3]), brick(filenames[5]))
# Load ALT
ALTsub <- read.table('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.txt', header = TRUE, sep = '\t')

rm(filenames)
####################################################################################################################################

### functions necessary for following code #########################################################################################
avg.soil.prop <- Vectorize(function( depth0, depth1, new.depth0, new.depth1, bulk.density, soil.prop ){
  if( all( depth1 < new.depth0 )){    # if the core isn't deep enough to reach the starting depth of the desired layer
    return(NA)
  }
  height <- (depth1 - depth0)/100
  # calc avg bulk density of core between last and current goals
  if ( any(depth1 >= new.depth1) ) {
    index.lwr <- min(which(depth1 > new.depth0)) # layer where we reach the new starting depth
    index.upr <- min(which(depth1 >= new.depth1)) # layer where we reach the new ending depth
    prior.prop <- soil.prop[index.lwr:index.upr] # the values of the soil property that needs to be averaged
    
    # determine the needed height from each old soil layer
    heights <- height[index.lwr:index.upr]
    if (length(heights) == 1) {
      heights <- new.depth1 - new.depth0
    } else {
    heights[1] <- (depth1[index.lwr] - new.depth0)/100
    heights[length(heights)] <- (new.depth1 - depth0[index.upr])/100
    }
    
    # calculate weights based on the soil stock in the needed portion of each old soil layer
    weights <- bulk.density[index.lwr:index.upr]*heights
    prop <- sum(prior.prop * weights) / sum(weights)

  } else {
    
    index.lwr <- min(which(depth1 > new.depth0)) # layer where we reach the new starting depth
    index.upr <- max( which( depth1 >= new.depth0) ) # last layer we have
    prior.prop <- soil.prop[index.lwr:index.upr] # the values of the soil property that needs to be averaged
    
    # determine the needed height from each old soil layer
    heights <- height[index.lwr:index.upr]
    if (length(heights) == 1) {
      heights <- new.depth1 - new.depth0
    } else {
      heights[1] <- depth0[index.lwr] - new.depth0
      heights[length(heights)] <- new.depth1 - depth0[index.upr]
    }
    
    # calculate weights based on the soil stock in the needed portion of each old soil layer
    weights <- bulk.density[index.lwr:index.upr]*heights
    prop <- sum(prior.prop * weights) / sum(weights)
  }
  
  return(prop)
}, c('new.depth0', 'new.depth1'))
####################################################################################################################################

### determine volume of water to subsidence adjusted ALT in each year ##############################################################

# prep alt data for joining to soil core data
alt <- ALTsub %>%
  select(year, fence, plot, treatment, ALT.corrected) %>%
  mutate(treatment = ifelse(treatment == 'Control' | treatment == 'Air Warming',
                            'c',
                            'w')) %>%
  group_by(year, fence, treatment) %>%
  summarise(mean.ALT = mean(ALT.corrected, na.rm = TRUE)*-1) %>%
  ungroup() %>%
  mutate(alt.id = group_indices(., fence, treatment)) %>%
  arrange(fence, treatment, year) %>%
  filter(year == 2009 | year == 2013 | year == 2018)
  
new.depth0 <- alt$mean.ALT[c(TRUE, TRUE, FALSE)]
new.depth1 <- alt$mean.ALT[c(FALSE, TRUE, TRUE)]
new.alt.id <- alt$alt.id[c(FALSE, TRUE, TRUE)]
new.alt.year <- alt$year[c(FALSE, TRUE, TRUE)]

# select 2009, 2013, and 2017, average duplicate cores from the same fence/treatment, and join ALT data
soil_09 <- soil %>%
  filter(year == 2009) %>%
  select(-year, -plot) %>%
  filter(!is.na(moisture)) %>%
  group_by(block, fence, treatment)

# calculate average moisture in between 2009, 2013, and 2018 active layers
moisture <- soil_09 %>%
  do({ data.frame( moisture = avg.soil.prop(.$depth0, .$depth1, new.depth0, new.depth1, .$bulk.density, .$moisture))}) %>%
  ungroup()

soil_stock <- soil_09 %>%
  do({ data.frame( soil.stock = avg.soil.prop(.$depth0, .$depth1, new.depth0, new.depth1, .$bulk.density, .$soil.stock))}) %>%
  ungroup()

moisture_loss <- moisture %>%
  cbind.data.frame(select(soil_stock, soil.stock)) %>%
  group_by(block, fence, treatment) %>%
  mutate(depth0 = new.depth0,
         depth1 = new.depth1,
         alt.id = new.alt.id,
         alt.year = new.alt.year) %>%
  ungroup() %>%
  mutate(core.id = group_indices(., fence, treatment)) %>%
  filter(core.id == alt.id) %>%
  select(-core.id, -alt.id) %>%
  group_by(block, fence, treatment) %>%
  mutate(moisture.height = moisture*soil.stock/100^3, # output in m
         cumulative.moisture.height = cumsum(moisture.height))


####################################################################################################################################

