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
###

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
# Load ALT
ALTsub <- read.table('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.txt', 
                     header = TRUE, sep = '\t')
# find elevation files in directory
filenames <- list.files("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks", 
                        full.names = TRUE, pattern = '^.*.tif$')
# make a list of elevation raster stacks for each block
Elevation <- list(brick(filenames[1]), brick(filenames[3]), brick(filenames[5]))

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
      heights <- (new.depth1 - new.depth0)/10
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
      heights <- (new.depth1 - new.depth0)/100
    } else {
      heights[1] <- depth0[index.lwr] - new.depth0/100
      heights[length(heights)] <- new.depth1 - depth0[index.upr]/100
    }
    
    # calculate weights based on the soil stock in the needed portion of each old soil layer
    weights <- bulk.density[index.lwr:index.upr]*heights
    prop <- sum(prior.prop * weights) / sum(weights)
  }
  
  return(prop)
}, c('new.depth0', 'new.depth1'))

sum.soil.prop <- Vectorize(function( depth0, depth1, new.depth0, new.depth1, bulk.density, soil.prop ){
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
    weights <- heights
    if (length(heights) == 1) {
      weights <- (new.depth1 - new.depth0)/100
    } else {
      weights[1] <- (depth1[index.lwr] - new.depth0)/100
      weights[length(weights)] <- (new.depth1 - depth0[index.upr])/100
    }
    weights <- weights/heights
    
    # calculate weights based on the soil stock in the needed portion of each old soil layer
    prop <- sum(prior.prop * weights)
    
  } else {
    
    index.lwr <- min(which(depth1 > new.depth0)) # layer where we reach the new starting depth
    index.upr <- max( which( depth1 >= new.depth0) ) # last layer we have
    prior.prop <- soil.prop[index.lwr:index.upr] # the values of the soil property that needs to be averaged
    print(c(new.depth0, new.depth1))
    # determine the needed height from each old soil layer
    heights <- height[index.lwr:index.upr]
    weights <- heights
    print(paste('heights = ', heights, sep = ''))
    if (length(heights) == 1) {
      weights <- (new.depth1 - new.depth0)/100
    } else {
      weights[1] <- (depth1[index.lwr] - new.depth0)/100
      weights[length(weights)] <- (new.depth1 - depth0[index.upr])/100
    }
    print(paste('weights = ', weights, sep = ''))
    weights <- weights/heights
    print(paste('new.weights = ', weights, sep = ''))
    
    # calculate weights based on the soil stock in the needed portion of each old soil layer
    prop <- sum(prior.prop * weights)
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
  do({ data.frame( soil.stock = sum.soil.prop(.$depth0, .$depth1, new.depth0, new.depth1, .$bulk.density, .$soil.stock))}) %>%
  ungroup()

core.area <- 45.3646/10^4

moisture_loss <- moisture %>%
  cbind.data.frame(select(soil_stock, soil.stock)) %>%
  group_by(block, fence, treatment) %>%
  mutate(depth0 = new.depth0,
         depth1 = new.depth1,
         height = new.depth1 - new.depth0,
         alt.id = new.alt.id,
         alt.year = new.alt.year,
         bulk.density = soil.stock/(height/100)*10^-3) %>% # g/cm^3 output
  ungroup() %>%
  mutate(core.id = group_indices(., fence, treatment)) %>%
  filter(core.id == alt.id) %>%
  select(-core.id, -alt.id) %>%
  group_by(block, fence, treatment) %>%
  mutate(moisture.height = (moisture/1000*bulk.density*height/0.92)/(1 - moisture/1000)*10^-2, # output in m
         cumulative.moisture.height = cumsum(moisture.height)) %>%
  arrange(alt.year, fence, treatment)

ggplot(moisture_loss, aes(x = height/100, y = moisture.height)) +
  geom_point()

ggplot(moisture_loss, aes(x = moisture/1000, y = moisture.height)) +
  geom_point()

mean.sub <- moisture_loss %>%
  group_by(alt.year, treatment) %>%
  summarise(mean.ice.sub = mean(cumulative.moisture.height, na.rm = TRUE),
            se.ice.sub = sd(cumulative.moisture.height, na.rm = TRUE)/sqrt(n()))
####################################################################################################################################

### Compare potential subsidence to measured subsidence at that water well's location ##############################################
# water wells taken in 2009
ids <- c('1.2', '1.3', '2.1', '2.4', '3.2', '3.4', '4.1', '4.3', '5.2', '5.3', '6.2', '6.4')

ww2009 <- water_wells %>%
  mutate(well.id = paste(fence, well, sep = '.'),
         block = ifelse(block == 'a',
                        1,
                        ifelse(block == 'b',
                               2,
                               3))) %>%
  filter(well.id %in% ids) %>%
  mutate(treatment = ifelse(well <= 2.5,
                            'c',
                            'w'))

# extract subsidence at each of the 2009 water wells
blocks <- list(1, 2, 3)
sub13_18 <- map(Elevation, ~ brick(.x[[5]]-.x[[1]], .x[[10]]-.x[[1]]))
ww_sub <- map2_dfr(sub13_18, 
               blocks, 
               ~ raster::extract(.x, 
                                 as(filter(ww2009, block == .y), 
                                    'Spatial')) %>%
                 as.data.frame() %>%
                 rename(sub13 = layer.1,
                        sub18 = layer.2) %>%
                 mutate(block = .y)
               ) %>%
  mutate(well.id = ids) %>%
  gather(key = alt.year, value = sub, sub13:sub18) %>%
  mutate(alt.year = ifelse(alt.year == 'sub13',
                           2013,
                           2018))

# add location to the potential subsidence data frame and compare with gps subsidence
moisture_loss_2 <- moisture_loss %>%
  left_join(ww2009, by = c('block', 'fence', 'treatment')) %>%
  st_as_sf() %>%
  left_join(ww_sub, by = c('block', 'well.id', 'alt.year')) %>%
  st_as_sf() %>%
  mutate(diff = cumulative.moisture.height - sub*-1)

avg_moisture_loss <- moisture_loss_2 %>%
  st_drop_geometry() %>%
  group_by(alt.year, treatment) %>%
  summarise(difference = mean(diff, na.rm = TRUE),
            mean.ice.sub = mean(cumulative.moisture.height, na.rm = TRUE),
            se.ice.sub = sd(cumulative.moisture.height, na.rm = TRUE)/sqrt(n()),
            mean.gps.sub = mean(sub, na.rm = TRUE),
            se.ice.sub = sd(sub, na.rm = TRUE)/sqrt(n()))

model <- lm(sub*-1 ~ cumulative.moisture.height, data = moisture_loss_2)
summary(model)

ggplot(moisture_loss_2, aes(x = cumulative.moisture.height, y = sub*-1, colour = treatment, shape = as.factor(alt.year))) +
  geom_point() +
  geom_segment(x = min(moisture_loss_2$cumulative.moisture.height, na.rm = TRUE), 
               y = model$coefficients[1] + model$coefficients[2]*min(moisture_loss_2$cumulative.moisture.height, na.rm = TRUE),
               xend = max(moisture_loss_2$cumulative.moisture.height, na.rm = TRUE),
               yend = model$coefficients[1] + model$coefficients[2]*max(moisture_loss_2$cumulative.moisture.height, na.rm = TRUE),
               colour = 'black') +
  scale_x_continuous(name = 'Potential Subsidence from Ice Loss (m)') +
  scale_y_continuous(name = 'Measured Subsidence (m)') +
  annotate(geom = 'text', 
           x = 0.45, 
           y = -0.05, 
           label = paste('y = ', round(model$coefficients[1], 2), ' + ', round(model$coefficients[2], 2), 'x', sep = ''),
           size = 3) +
  scale_colour_manual(values = c("#006699", "#990000"),
                     labels = c('Control', 'Warming'),
                     name = '') +
  scale_shape_discrete(name = '') +
  theme_few() +
  theme(text = element_text(size = 8))
####################################################################################################################################