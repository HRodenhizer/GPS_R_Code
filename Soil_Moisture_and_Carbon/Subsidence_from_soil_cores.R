####################################################################################################################################
###                         Subsidence from soil cores/ what part of soil is compacting?                                         ###
###                                                HGR 10/2018                                                                   ###
####################################################################################################################################

### load libraries #################################################################################################################
library(plyr)
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
####################################################################################################################################

### load data ######################################################################################################################
soil_09_13 <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2009-2013 CiPEHR Processed SOIL DATA_César.xlsx",
                         sheet = 1) %>%
  separate(depth.cat, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  mutate(depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1))
# soil_17 <- read_excel('Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2017/2017 Soils_processing data sheet_3_8_18.xlsx')
# read in water well location data and subsidence data for 2013
water_wells <- read_sf('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/water_wells.shp') %>%
  filter(str_detect(Name, pattern = '^ww.*5$')) %>%
  select(Name, Easting, Northing) %>%
  mutate(fence = as.numeric(str_sub(Name, 3, 3)),
         block = ifelse(fence == 1 | fence == 2,
                        'a',
                        ifelse(fence == 3 | fence == 4,
                               'b',
                               'c')),
         well = str_sub(Name, 5))
# find elevation files in directory
filenames <- list.files("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks", full.names = TRUE, pattern = '^.*.tif$')
# make a list of elevation raster stacks for each block
Elevation <- list(brick(filenames[1]), brick(filenames[3]), brick(filenames[5]))
# Load ALT
ALTsub <- read.table('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.txt', header = TRUE, sep = '\t')
####################################################################################################################################


### functions necessary for following code #########################################################################################
get.height <- Vectorize(function( depth0, depth1, amount, goal ){
  A <- cumsum(amount)
  goal.lwr <- goal - 5
  if( all( A < goal.lwr | goal == 0)){    # if the core isn't deep enough to reach the desired quantity of ash - 5
    return(NA)
  }
  # calc height of core to goal ash content
  if ( any(A > goal)) { # deal with the situations in which the core reaches the total desired quantity of ash
    index.upr <- min( which( A >= goal ) ) # layer where we reach the desired quantity of ash
    start.ash <- A[index.upr] - amount[index.upr] # cumulative amount of ash to the layer below the layer where the goal is met
    need <- goal - start.ash # how much of the layer where the goal is met that is needed
    height <- (depth1[index.upr] - depth0[index.upr]) * (need / amount[index.upr]) # height of the layer where the goal is met that is needed
    tot.height <- depth0[index.upr] + height # add the core height to layer below layer where goal is met to the amount of current layer that is needed
  } else { # extrapolate depth for the situations in which we have passed the desired quantity of ash -5, but never reached the desired quantity of ash
    index.upr <- max( which( A >= goal.lwr) )
    layer.height <- depth1[index.upr] - depth0[index.upr]
    need <- goal - A[index.upr]
    height <- layer.height * (need/ amount[index.upr])
    tot.height <- depth1[index.upr] + height
  }
  
  return(tot.height)
}, 'goal')

avg.soil.prop <- Vectorize(function( depth0, depth1, amount, soil.prop, goal ){
  A <- cumsum(amount)
  goal.lwr <- goal - 5
  if( all( A < goal.lwr | goal == 0)){    # if the core isn't deep enough to reach the desired quantity of ash
    return(NA)
  }
  
  # calc avg bulk density of core between last and current goals
  if ( any( A > goal)) {
    index.lwr <- min(which(A > goal.lwr)) # layer where we reach the quantity of ash that we are starting from
    index.upr <- min(which(A >= goal)) # layer where we reach the desired quantity of ash
    start.ash <- A[index.upr] - amount[index.upr] # cumulative amount of ash to the layer below the layer where the goal is met
    need <- goal - start.ash # amount of ash of the layer where the goal is met that is needed
    prior.prop <- soil.prop[index.lwr:index.upr]
    weights <- amount[index.lwr:index.upr]
    if(length(prior.prop) == 1) {
      weights <- c(5)
    } else {
      weights[1] <- (A[index.lwr] - goal.lwr)
      weights[length(weights)] <- need
    }
    prop <- sum(prior.prop * weights) / 5
  } else {
    index.lwr <- min(which(A > goal.lwr)) # layer where we reach the quantity of ash that we are starting from
    index.upr <- max( which( A >= goal.lwr) ) # last layer we have
    need <- goal - A[index.upr] + amount[index.upr]
    prior.prop <- soil.prop[index.lwr:index.upr]
    weights <- amount[index.lwr:index.upr]
    if(length(prior.prop) == 1) {
      weights <- c(5)
    } else {
      weights[1] <- (A[index.lwr] - goal.lwr)
      weights[length(weights)] <- need
    }
    prop <- sum(prior.prop * weights) / 5
  }
  
  return(prop)
}, 'goal')
####################################################################################################################################

### calculate the height of cores with the same amount of ash mass in 2009 vs. 2013 ################################################
ash_mass_09_13 <- soil_09_13 %>%
  select(1:10) %>%
  filter(year == 2009 | year == 2013) %>%
  filter(moisture != is.na(moisture)) %>%
  group_by(year, block, fence, treatment) %>%
  mutate(soil.mass = bulk.density*(depth1 - depth0),
         soil.mass.tot = cumsum(soil.mass),
         ash.mass = ash*bulk.density*(depth1 - depth0)/1000,
         ash.mass.tot = cumsum(ash.mass))

# set 'goal' ash amounts by 5 and then add in the max shared ash amount as an additional goal
# (to be able to specifically compare the subsidence for the entire shared core depth without extrapolating to the next 5 grams of ash)
goal.max <- ash_mass_09_13 %>%
  group_by(year, block, fence, treatment) %>%
  filter(ash.mass.tot == max(ash.mass.tot, na.rm = TRUE)) %>%
  group_by(block, fence, treatment) %>%
  filter(ash.mass.tot == min(ash.mass.tot, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(block, fence, treatment) %>%
  select(year, block, fence, treatment, ash.mass.tot)

goal <- c(seq(0, 60, 5), goal.max$ash.mass.tot)

# extract depths to chosen uniform ash mass values: (5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55 + the total shared ash mass)
depth <- ash_mass_09_13 %>%
  do({ data.frame( depth1 = get.height(.$depth0, .$depth1, .$ash.mass, goal)) })

# average bulk density for each of the previously calculated depths
bd <- ash_mass_09_13 %>%
  do({ data.frame( bd = avg.soil.prop(.$depth0, .$depth1, .$ash.mass, .$bulk.density, goal))})

# average moisture for each of the previously calculated depths
moisture <- ash_mass_09_13 %>%
  do({ data.frame( moisture = avg.soil.prop(.$depth0, .$depth1, .$ash.mass, .$moisture, goal))})

# join depth, bd, and moisture dataframes into one
soil_09_13_ash <- depth %>%
  cbind.data.frame(bd = bd$bd, moisture = moisture$moisture) %>%
  mutate(ash.mass = rep(goal, 24),
         depth1 = ifelse(ash.mass == 0,
                         0,
                         depth1),
         group.id = group_indices(., block, fence, treatment),
         goal.id = rep(c(rep(0, 13), seq(1, 12)), 24)) %>%
  filter(!is.na(depth1)) %>%
  filter(ash.mass == round(ash.mass, 0) | group.id == goal.id) %>%
  select(year, block, fence, treatment, ash.mass, depth1, bd, moisture) %>%
  arrange(year, block, fence, treatment, ash.mass)

height <- soil_09_13_ash$depth1 %>%
  diff()
height <- height[height > 0]

soil_09_13_ash <- soil_09_13_ash %>%
  filter(!is.na(bd)) %>%
  cbind(height) %>%
  mutate(depth0 = depth1-height) %>%
  select(year, block, fence, treatment, ash.mass, depth0, depth1, height, bd, moisture)

# prep alt data for joining
alt <- ALTsub %>%
  select(year, fence, plot, treatment, ALT) %>%
  filter(year == 2009 | year == 2013) %>%
  mutate(treatment = ifelse(treatment == 'Control' | treatment == 'Air Warming',
                            'c',
                            'w')) %>%
  group_by(year, fence, treatment) %>%
  summarise(mean.ALT = mean(ALT, na.rm = TRUE),
            se.ALT = sd(ALT, na.rm = TRUE)/sqrt(n()))

# join alt with soil_09_13_ash
soil_ash_alt <- soil_09_13_ash %>%
  left_join(alt, by = c('year', 'fence', 'treatment')) %>%
  filter(-mean.ALT > depth0 & -mean.ALT < depth1 | fence == 3 & treatment == 'c' & year == 2009) %>%
  mutate(mean.ALT.ash = ifelse(-mean.ALT > depth0 & -mean.ALT < depth1,
                               ash.mass - 5 + 5*(-mean.ALT-depth0)/(depth1-depth0),
                               ash.mass + (-mean.ALT-depth1)*ash.mass/height))

# calculate subsidence, change in bd, and change in moisture in each layer
soil_09_ash <- soil_09_13_ash %>%
  filter(year == 2009) %>%
  select(block, fence, treatment, ash.mass, depth0.2009 = depth0, depth1.2009 = depth1, height.2009 = height, bd.2009 = bd, moisture.2009 = moisture)

soil_core_sub_ash <- soil_09_13_ash %>%
  filter(year == 2013) %>%
  select(block, fence, treatment, ash.mass, depth0.2013 = depth0, depth1.2013 = depth1, height.2013 = height, bd.2013 = bd, moisture.2013 = moisture) %>%
  full_join(soil_09_ash, by = c('block', 'fence', 'treatment', 'ash.mass')) %>%
  arrange(block, fence, treatment, ash.mass) %>%
  mutate(subsidence = height.2013 - height.2009,
         delta.bd = bd.2013 - bd.2009,
         delta.moisture = moisture.2013 - moisture.2009,
         type = 'ash') %>%
  select(type, block, fence, treatment, mass = ash.mass, subsidence, delta.bd, delta.moisture)
####################################################################################################################################

### calculate the height of cores with the same amount of soil mass in 2009 vs. 2013 ###############################################
goal.max <- ash_mass_09_13 %>%
  group_by(year, block, fence, treatment) %>%
  filter(soil.mass.tot == max(soil.mass.tot, na.rm = TRUE)) %>%
  group_by(block, fence, treatment) %>%
  filter(soil.mass.tot == min(soil.mass.tot, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(block, fence, treatment) %>%
  select(year, block, fence, treatment, soil.mass.tot)
goal <- c(seq(0, 65, 5), goal.max$soil.mass.tot)

# extract depths to chosen uniform soil mass values: (5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)
depth <- ash_mass_09_13 %>%
  do({ data.frame( depth1 = get.height(.$depth0, .$depth1, .$soil.mass, goal)) })

# average bulk density for each of the previously calculated depths
bd <- ash_mass_09_13 %>%
  do({ data.frame( bd = avg.soil.prop(.$depth0, .$depth1, .$soil.mass, .$bulk.density, goal))})

# average moisture for each of the previously calculated depths
moisture <- ash_mass_09_13 %>%
  do({ data.frame( moisture = avg.soil.prop(.$depth0, .$depth1, .$soil.mass, .$moisture, goal))})

# join depth, bd, and moisture dataframes into one
soil_09_13_soil <- depth %>%
  cbind.data.frame(bd = bd$bd, moisture = moisture$moisture) %>%
  mutate(soil.mass = rep(goal, 24),
         depth1 = ifelse(soil.mass == 0,
                         0,
                         depth1),
         group.id = group_indices(., block, fence, treatment),
         goal.id = rep(c(rep(0, 14), seq(1, 12)), 24)) %>%
  filter(!is.na(depth1)) %>%
  filter(soil.mass == round(soil.mass, 0) | group.id == goal.id) %>%
  select(year, block, fence, treatment, soil.mass, depth1, bd, moisture) %>%
  arrange(year, block, fence, treatment, soil.mass)


height <- soil_09_13_soil$depth1 %>%
  diff()
height <- height[height >= 0]

soil_09_13_soil <- soil_09_13_soil %>%
  filter(!is.na(bd)) %>%
  cbind(height) %>%
  mutate(depth0 = depth1-height) %>%
  select(year, block, fence, treatment, soil.mass, depth0, depth1, height, bd, moisture)

# join alt with soil_09_13_soil
soil_alt <- soil_09_13_soil %>%
  left_join(alt, by = c('year', 'fence', 'treatment')) %>%
  filter(-mean.ALT > depth0 & -mean.ALT < depth1) %>%
  mutate(mean.ALT.ash = ifelse(-mean.ALT > depth0 & -mean.ALT < depth1,
                               soil.mass - 5 + 5*(-mean.ALT-depth0)/(depth1-depth0),
                               soil.mass + (-mean.ALT-depth1)*soil.mass/height))

# calculate subsidence, change in bd, and change in moisture in each layer
soil_09_soil <- soil_09_13_soil %>%
  filter(year == 2009) %>%
  select(block, fence, treatment, soil.mass, depth0.2009 = depth0, depth1.2009 = depth1, height.2009 = height, bd.2009 = bd, moisture.2009 = moisture)

soil_core_sub_soil <- soil_09_13_soil %>%
  filter(year == 2013) %>%
  select(block, fence, treatment, soil.mass, depth0.2013 = depth0, depth1.2013 = depth1, height.2013 = height, bd.2013 = bd, moisture.2013 = moisture) %>%
  full_join(soil_09_soil, by = c('block', 'fence', 'treatment', 'soil.mass')) %>%
  arrange(block, fence, treatment, soil.mass) %>%
  mutate(subsidence = height.2013 - height.2009,
         delta.bd = bd.2013 - bd.2009,
         delta.moisture = moisture.2013 - moisture.2009,
         type = 'soil') %>%
  select(type, block, fence, treatment, mass = soil.mass, subsidence, delta.bd, delta.moisture)
####################################################################################################################################

### Compare soil normalized and ash normalized soil core subsidence by soil profile layer ################################################################
# this data frame has the subsidence, bd, and moisture change by soil profile layer for both ash and soil normalization methods
soil_core_sub <- soil_core_sub_ash %>%
  rbind.data.frame(soil_core_sub_soil)

# create vector of starting ash mass
start_ash <- soil_core_sub %>%
  filter(type == 'ash') %>%
  select(mass)

start_ash <- c(0, start_ash$mass)
start_ash <- diff(start_ash)
start_ash <- replace(start_ash, start_ash < 0, 5)

# this data frame can be used to test if there was more subsidence (or bd or moisture change) in the expanding active layer than previously thawed areas
soil_core_sub_alt <- soil_core_sub %>%
  filter(type == 'ash') %>%
  left_join(select(filter(soil_alt, year == 2009), block, fence, treatment, mean.ALT.ash), by = c('block', 'fence', 'treatment')) %>%
  rename(mean.ALT.ash.2009 = mean.ALT.ash) %>%
  left_join(select(filter(soil_alt, year == 2013), block, fence, treatment, mean.ALT.ash), by = c('block', 'fence', 'treatment')) %>%
  rename(mean.ALT.ash.2013 = mean.ALT.ash) %>%
  mutate(delta.mass = start_ash,
         mass0 = mass - delta.mass,
         depth.category = ifelse(mass <= mean.ALT.ash.2009 & mass < mean.ALT.ash.2013,
                                 'old.active.layer',
                                 ifelse(mass0 > mean.ALT.ash.2013,
                                        'permafrost',
                                        'new.active.layer')),
         depth.category = factor(depth.category, levels = c('old.active.layer', 'new.active.layer', 'permafrost'))) %>%
  group_by(type, block, fence, treatment, depth.category) %>%
  filter(!is.na(subsidence) & type == 'ash') %>%
  summarise(total.sub.per.grams.ash = sum(subsidence, na.rm = TRUE)/(max(mass)-min(mass0)),
            avg.delta.bd = mean(delta.bd, na.rm = TRUE),
            se.delta.bd = sd(delta.bd, na.rm = TRUE)/sqrt(n()),
            avg.delta.moisture = mean(delta.moisture, na.rm = TRUE),
            se.delta.moisture = sd(delta.moisture, na.rm = TRUE)/sqrt(n())) %>%
  mutate(total.sub.per.grams.ash = ifelse(total.sub.per.grams.ash == 0 & is.na(avg.delta.bd),
                                   NA,
                                   total.sub.per.grams.ash))

# difference in subsidence per depth in old vs. new active layer is not significant (retest with 2017 data and test lm over time)
t.test(subset(soil_core_sub_alt$total.sub.per.grams.ash, soil_core_sub_alt$depth.category == 'old.active.layer'),
       subset(soil_core_sub_alt$total.sub.per.grams.ash, soil_core_sub_alt$depth.category == 'new.active.layer'),
       alternative = 'greater')

# difference in change in bd in old vs. new active layer is significant (should do lm with change in bd over time when 2017 is added)
t.test(subset(soil_core_sub_alt$avg.delta.bd, soil_core_sub_alt$depth.category == 'old.active.layer'),
       subset(soil_core_sub_alt$avg.delta.bd, soil_core_sub_alt$depth.category == 'new.active.layer'),
       alternative = 'less')

# difference in change in moisture in old vs. new active layer is significant
t.test(subset(soil_core_sub_alt$avg.delta.moisture, soil_core_sub_alt$depth.category == 'old.active.layer'),
       subset(soil_core_sub_alt$avg.delta.moisture, soil_core_sub_alt$depth.category == 'new.active.layer'),
       alternative = 'greater')

# this data frame has the total subsidence across paired cores using both the ash and the soil normalization methods (should do lm with change in bd over time when 2017 is added)
soil_core_sub_summary <- soil_core_sub %>%
  group_by(type, block, fence, treatment) %>%
  filter(!is.na(subsidence)) %>%
  filter(mass != max(mass)) %>%
  summarise(total.subsidence = sum(subsidence, na.rm = TRUE),
            avg.delta.bd = mean(delta.bd, na.rm = TRUE),
            se.delta.bd = sd(delta.bd, na.rm = TRUE)/sqrt(n()),
            avg.delta.moisture = mean(delta.moisture, na.rm = TRUE),
            se.delta.moisture = sd(delta.moisture, na.rm = TRUE)/sqrt(n())) %>%
  mutate(total.subsidence = ifelse(total.subsidence == 0 & is.na(avg.delta.bd),
                                   NA,
                                   total.subsidence))

# separate out the ash normalized subsidence
soil_core_sub_sum_ash <- soil_core_sub_summary %>%
  filter(type == 'ash') %>%
  ungroup() %>%
  select(block, fence, treatment, subsidence.ash = total.subsidence)

# this data frame compares the subsidence due to bulk density changes only, carbon loss only, and total
soil_core_sub_summary_2 <- soil_core_sub_summary %>%
  filter(type == 'soil') %>%
  ungroup() %>%
  select(block, fence, treatment, subsidence.soil = total.subsidence) %>%
  full_join(soil_core_sub_sum_ash, by = c('block', 'fence', 'treatment')) %>%
  mutate(subsidence.c = subsidence.ash - subsidence.soil)
####################################################################################################################################

### Plots of soil profiles #############################################################################
# plots of change from 2009 to 2013
# ggplot(soil_core_sub, aes(x = subsidence, y = -ash.mass, color = treatment)) +
#   geom_point() +
#   geom_path() +
#   geom_hline(data = soil_ash_alt, aes(yintercept = -mean.ALT.ash, color = treatment, linetype = as.factor(year))) +
#   facet_grid(.~fence)
# 
# ggplot(soil_core_sub, aes(x = delta.bd, y = -ash.mass, color = treatment)) +
#   geom_point() +
#   geom_path() +
#   geom_hline(data = soil_ash_alt, aes(yintercept = -mean.ALT.ash, color = treatment, linetype = as.factor(year))) +
#   facet_grid(.~fence)
# 
# ggplot(soil_core_sub, aes(x = delta.moisture, y = -ash.mass, color = treatment)) +
#   geom_point() +
#   geom_path() +
#   geom_hline(data = soil_ash_alt, aes(yintercept = -mean.ALT.ash, color = treatment, linetype = as.factor(year))) +
#   facet_grid(.~fence)

# plots with 2009 and 2013 values compared in one facet
ggplot(soil_09_13_ash, aes(x = height, y = -ash.mass, color = as.factor(year))) +
  geom_point() +
  geom_path() +
  geom_hline(data = soil_ash_alt, aes(yintercept = -mean.ALT.ash, color = as.factor(year))) +
  facet_grid(fence~treatment)

ggplot(soil_09_13_ash, aes(x = bd, y = -ash.mass, color = as.factor(year))) +
  geom_point() +
  geom_path() +
  geom_hline(data = soil_ash_alt, aes(yintercept = -mean.ALT.ash, color = as.factor(year))) +
  facet_grid(fence~treatment)

ggplot(soil_09_13_ash, aes(x = moisture, y = -ash.mass, color = as.factor(year))) +
  geom_point() +
  geom_path() +
  geom_hline(data = soil_ash_alt, aes(yintercept = -mean.ALT.ash, color = as.factor(year))) +
  facet_grid(fence~treatment)
####################################################################################################################################

#### Compare subsidence calculated with and without carbon loss (ash vs. soil) #####################################################
carbon_loss_model <- lm(subsidence.c ~ subsidence.soil, soil_core_sub_summary_2)
summary(carbon_loss_model)
intercept <- carbon_loss_model$coefficients[1]
slope <- carbon_loss_model$coefficients[2]
soil_core_sub_conf <- predict(carbon_loss_model, newdata=soil_core_sub_summary_2, interval='confidence') %>%
  as.data.frame() %>%
  cbind.data.frame(soil_core_sub_summary_2)

sub_carbon_fig <- ggplot(soil_core_sub_summary_2, aes(x = subsidence.soil, y = subsidence.c)) +
  geom_ribbon(data = soil_core_sub_conf, aes(x = subsidence.soil, ymin = lwr, ymax = upr), fill = 'gray') +
  geom_segment(x = min(soil_core_sub_summary_2$subsidence.soil),
               y = slope*min(soil_core_sub_summary_2$subsidence.soil),
               xend = max(soil_core_sub_summary_2$subsidence.soil),
               yend = slope*max(soil_core_sub_summary_2$subsidence.soil),
               color = "black") +
  geom_point(aes(color = treatment)) +
  scale_color_manual(values = c("#006699", "#990000"),
                     labels = c('Control', 'Warming'),
                     name = '') +
  theme_few() +
  coord_fixed() +
  xlab("Subsidence (bulk density)") +
  ylab("Subsidence (C loss)") +
  annotate('text', x = 0, y = -15, 
           label = paste('y = ', round(intercept, 2), ' + ', round(slope, 2), 'x', sep = ''), 
           size = 3) +
  annotate('text', x = 1, y = -18, 
           label = 'p-value = 0.06', 
           size = 3) +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x  = element_text(size = 8),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8))
sub_carbon_fig
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Carbon_Contribution.jpg', sub_carbon_fig, height = 75, width = 115, units = 'mm')


# sub_carbon_boxplot <- ggplot(soil_core_sub_3, aes(x = dummy, y = subsidence)) +
#   geom_boxplot(aes(fill = sub_type), position = 'dodge2', color = 'black') +
#   scale_fill_manual(breaks = c('soil', 'ash'),
#                      values = c("#333333", "#ff3333"),
#                      labels = c('No Carbon Loss', 'With Carbon Loss'),
#                      name = '') +
#   scale_y_continuous(limits = c(-50, 20)) +
#   theme_few() +
#   ylab("Subsidence (cm)") +
#   theme(axis.title.x = element_blank(),
#         axis.text.x  = element_blank(),
#         axis.title.y = element_text(size = 12),
#         axis.text.y = element_text(size = 8),
#         axis.ticks.x = element_blank(),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 8),
#         legend.justification = c(0, 0),
#         legend.position = c(0.01, 0.01)) 
# sub_carbon_boxplot
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Carbon_Contribution_boxplot.jpg', sub_carbon_boxplot, height = 95, width = 115, units = 'mm')
####################################################################################################################################

### Compare soil core subsidence with gps subsidence at that location ##############################################################
Subsidence09_13 <- list()
for (i in 1:length(Elevation)) {
  Subsidence09_13[[i]] <- Elevation[[i]][[5]] - Elevation[[i]][[1]] # calculate subsidence from gap filled elevation raster stack
  Subsidence09_13[[i]]@file@name <- 'subsidence'
  plot(Subsidence09_13[[i]]) # plot to make sure it looks reasonable
  plot(as_Spatial(filter(st_zm(water_wells, what = 'ZM'), block == unique(block)[[i]])), add = TRUE)
  plot(Elevation[[i]])
  rm(i)
}

water_well_sub <- data.frame()
for (i in 1:length(Subsidence09_13)) {
  points <- water_wells %>%
    st_zm(what = 'ZM') %>% # remove Z and M geometry from water_wells
    filter(block == unique(block)[i]) %>% # select the correct block
    as_Spatial() # turn it into a SpatialPointsDataFrame
  temp <- raster::extract(Subsidence09_13[[i]], points, df = TRUE) %>% # extract subsidence
    select(-ID, GPS.sub = layer) %>% # rename the column with extracted subsidence and get rid of the ID created by extract
    mutate(GPS.sub = GPS.sub*100) %>% # convert from meters to centimeters
    cbind.data.frame(subset(water_wells, block == unique(block)[i])) # join extracted values to water well info
  water_well_sub <- water_well_sub %>%
    rbind.data.frame(temp) # join the water_well_sub dataframe for each block together
  rm(i, temp, points)
}

sub_comparison <- water_well_sub %>%
  mutate(block = as.numeric(as.factor(block)),
         treatment = ifelse(well == 2.5,
                            'c',
                            'w')) %>%
  select(block, fence, treatment, GPS.sub) %>%
  full_join(soil_core_sub_summary_2, by = c('block', 'fence', 'treatment')) %>%
  mutate(sub.diff.soil = GPS.sub - subsidence.soil,
         sub.diff.ash = GPS.sub - subsidence.ash)

mean.sub.diff.ash = mean(sub_comparison$sub.diff.ash)
sd.sub.diff.ash = sd(sub_comparison$sub.diff.ash)

# write.csv(sub_comparison, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/Soil_Core_vs_GPS_Subsidence.csv', row.names = FALSE)

sub_comparison_graph <- sub_comparison %>%
  gather(key = source, value = subsidence, GPS.sub:subsidence.ash)

g1 <- ggplot(sub_comparison_graph, aes(x = source, y = subsidence)) +
  geom_boxplot() +
  theme_few() +
  ggtitle('Comparison of 2009-2013 Subsidence Using GPS and Soil Cores') +
  annotate('text', x = 'subsidence.ash', y = 20, label = '*', size = 12)
g1
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/Soil_Core_vs_GPS_Subsidence.jpeg', g1)

t.test(sub_comparison$sub.diff.ash)
model <- lm(GPS.sub ~ subsidence.ash, data = sub_comparison)
sub_comparison <- sub_comparison %>%
  mutate(fit = model$coefficients[1] + model$coefficients[2]*subsidence.ash)

g2 <- ggplot(sub_comparison, aes(x = subsidence.ash, y = GPS.sub, colour = treatment)) +
  geom_point() +
  geom_line(aes(x = subsidence.ash, y = fit), inherit.aes = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_color_manual(values = c("#006699", "#990000"),
                     labels = c('Control', 'Warming'),
                     name = '') +
  scale_x_continuous(name = 'Soil Core Subsidence (cm)',
                     limits = c(-45, 25)) +
  scale_y_continuous(name = 'GPS Subsidence (cm)',
                     limits = c(-45, 25)) +
  ggtitle('Comparison of Subsidence Methods') +
  coord_fixed() +
  theme_few() +
  theme(legend.title=element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x  = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
g2

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Method_Comparison.jpg', g2, height = 4, width = 6.5)
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Method_Comparison.pdf', g2, height = 4, width = 6.5)
####################################################################################################################################

######################## DEFINE FUNCTIONS TO EXTRACT AND GRAPH CI #########################
#Extract the coefficients for the fixed effects from your model, make a dataframe with them called model
extract_ci <- function(x) {coefs<-fixef(x) 
modeldf<-as.data.frame(coefs)
#calculate confidence intervals; merge fixed effects and ci into one dataframe
ci <- confint(x,method="boot",boot.type="norm",level=0.95,nsim=1000)
modelci<-merge(ci,modeldf,by="row.names",all.x=F)
#rename colnames so that they make sense and remove symbols
colnames(modelci)<-c("term","min","max","coefs")
return (modelci)}

# graph CI
graph_ci <- function(ci,figtitle,model) {ggplot(ci,aes(x=names,y=coefs))+
    geom_errorbar(aes(ymin=min,ymax=max),width=0,size=1)+
    geom_point(aes(size=2))+
    labs (title = paste(figtitle, ", AIC:", round(AIC(model),2), sep =" ") , x = "Fixed effect", y = "Effect size and 95% CI") +
    guides(size=F,shape=F)+
    theme_bw()+
    theme(axis.text.x=element_text(size=18),
          axis.title.x=element_text(size=26),
          axis.title.y=element_text(size=26,vjust=1),
          axis.text.y=element_text(size=22),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank())+
    geom_hline(yintercept=0)+
    coord_flip() } 
#################################################################################

### Prep Data Frame for Mixed effects models #########
# prep ALTsub data
ALTsub_fence <- ALTsub %>%
  mutate(treatment = ifelse(treatment == 'Air Warming' | treatment == 'Control' | treatment == 'Drying',
                            'c',
                            'w'),
         subsidence = subsidence*100) %>%
  group_by(year, block, fence, treatment) %>%
  summarise(mean.subsidence = mean(subsidence, na.rm = TRUE),
            mean.ALT = mean(ALT.corrected, na.rm = TRUE),
            se.subsidence = sd(subsidence, na.rm = TRUE)/sqrt(n()),
            se.ALT = sd(ALT.corrected, na.rm = TRUE)/sqrt(n())) %>%
  ungroup() %>%
  dplyr::select(-block)

ash_profiles_model_ready <- soil_09_13_ash %>%
  left_join(ALTsub_fence, by = c('year', 'fence', 'treatment')) %>%
  ungroup() %>%
  mutate(time = year - 2009,
         block = as.factor(ifelse(fence == 1 | fence == 2,
                        1,
                        ifelse(fence == 3 | fence == 4,
                               2,
                               3))),
         fence2 = as.factor(ifelse(fence == 1 | fence == 3 | fence == 5,
                        1,
                        2)),
         treatment2 = as.factor(ifelse(treatment == 'c',
                             1,
                             2)),
         fencegroup = factor(block:fence2),
         wholeplot = factor(block:fence2:treatment2),
         moisture = as.numeric(moisture)/1000, # kg/kg rather than g/kg to make the scale similar to other predictor variables
         bulk.density = as.numeric(bd),
         mean.ALT = mean.ALT/100, # change to m for better scale
         moisture1 = 1/moisture) %>%
  select(year, fence, treatment, time, block, fence2, treatment2, fencegroup, wholeplot, ash.mass, bulk.density, moisture, moisture1, mean.subsidence, se.subsidence, mean.ALT, se.ALT)

sub_ash_model_ready <- ash_profiles_model_ready %>%
  group_by(year, fence, treatment, time, block, fence2, treatment2, fencegroup, wholeplot, mean.subsidence, se.subsidence, mean.ALT, se.ALT) %>%
  summarise(mean.moisture1 = mean(moisture1, na.rm = TRUE),
            se.moisture1 = sd(moisture1)/sqrt(n()),
            mean.bulk.density = mean(bulk.density, na.rm = TRUE),
            se.bulk.density = sd(bulk.density)/sqrt(n()),
            mean.moisture = mean(moisture, na.rm = TRUE),
            se.moisture = sd(moisture, na.rm = TRUE)/sqrt(n()))

plot_ash_profiles <- ash_profiles_model_ready %>%
  group_by(year, ash.mass, treatment) %>%
  summarise(mean.bulk.density = mean(bulk.density, na.rm = TRUE),
            mean.moisture = mean(moisture, na.rm = TRUE),
            se.bulk.density = sd(bulk.density, na.rm = TRUE)/sqrt(n()),
            se.moisture = sd(moisture, na.rm = TRUE)/sqrt(n()))

ggplot(plot_ash_profiles, aes(x = year, y = mean.bulk.density, colour = treatment)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean.bulk.density - se.bulk.density, ymax = mean.bulk.density + se.bulk.density)) +
  facet_grid(. ~ ash.mass) +
  scale_colour_manual(breaks = c('c', 'w'),
                      labels = c('Control', 'Warming'),
                      values = c('blue', 'red'))

ash_profiles_model_ready %>%
  dplyr::select(mean.subsidence, bulk.density, time, ash.mass, treatment2, mean.ALT, moisture, moisture1) %>%
  GGally::ggpairs(upper=list(continuous='points'),
                  lower=list(continuous='cor'))

# write.csv(ash_profiles_model_ready, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Soil_Cores_Model_Ready.csv', row.names = FALSE)
####################################################################################################################################

### Model bulk density #############################################################################################################
# Forward Selection starting with soil moisture
# model with 1/soil moisture only
model1 <- lmer(bulk.density ~ moisture1 + # use * or + depending on the interactions you want to specify
               (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
             data = ash_profiles_model_ready)
summary(model1)
# look at residuals
model1.resid <- resid(model1)
model1.fitted <- fitted(model1)
model1.sqrt <- sqrt(abs(resid(model1)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(model1.fitted, model1.resid, main='resid, model1')
plot(model1.fitted, model1.sqrt, main='sqrt resid, model1')
qqnorm(model1.resid, main = 'model1')
qqline(model1.resid)
par(mfrow=c(1,1))

# model with 1/soil moisture and ash.layer
model2 <- lmer(bulk.density ~ moisture1 + ash.layer + # use * or + depending on the interactions you want to specify
               (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
             data = ash_profiles_model_ready)

# model with 1/soil moisture and mean.ALT
model3 <- lmer(bulk.density ~ moisture1 + mean.ALT + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

# model with 1/soil moisture and treatment
model4 <- lmer(bulk.density ~ moisture1 + treatment2 + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

# model with 1/soil moisture and time
model5 <- lmer(bulk.density ~ moisture1 + time + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

anova(model1, model2, model3, model4, model5) # adding ash layer improves the model


# model with 1/soil moisture and ash.layer and interaction
model3 <- lmer(bulk.density ~ moisture1*ash.layer + # use * or + depending on the interactions you want to specify
                (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
              data = ash_profiles_model_ready)
anova(model2, model3) # adding the interaction does not improve the model enough

# model with 1/soil moisture, ash.layer, and ALT
model3 <- lmer(bulk.density ~ moisture1 + ash.layer + mean.ALT + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

# model with 1/soil moisture, ash.layer, and time
model4 <- lmer(bulk.density ~ moisture1 + ash.layer + time + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

# model with 1/soil moisture, ash.layer, and treatment
model5 <- lmer(bulk.density ~ moisture1 + ash.layer + treatment2 + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

anova(model2, model3, model4, model5) # model with only soil moisture and ash layer is the best

# re-run model2 with REML = TRUE
bd_m_a_model <- lmer(bulk.density ~ moisture1 + ash.layer + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = TRUE,
               data = ash_profiles_model_ready)
summary(bd_m_a_model)
car::vif(bd_m_a_model)
bd_ci <- extract_ci(bd_m_a_model)

# look at residuals
test.resid <- resid(bd_m_a_model)
test.fitted <- fitted(bd_m_a_model)
test.sqrt <- sqrt(abs(resid(bd_m_a_model)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(test.fitted, test.resid, main='resid, test')
plot(test.fitted, test.sqrt, main='sqrt resid, test')
qqnorm(test.resid, main = 'test')
qqline(test.resid)
par(mfrow=c(1,1))

# save the model
# saveRDS(bd_m_a_model, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/bd_model.rds')
# write.csv(bd_ci, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/bd_model_coefs.csv', row.names = FALSE)
####################################################################################################################################
bd_m_a_model <- readRDS('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/bd_model.rds')
bd_ci <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/bd_model_coefs.csv')
bd_model_r2 <- r.squaredGLMM(bd_m_a_model)
summary(bd_m_a_model)

bd_model_table <- data.frame(Response = c('Bulk Density', rep(NA, 5)),
                             `Full Model` = c('Soil Moisture', 'Ash Layer', 'ALT', 'Soil Warming', 'Year', 'Soil Moisture*Ash Layer'),
                             `Final Variables` = c('Intercept', 'Soil Moisture', 'Ash Layer', rep(NA, 3)),
                             Coeficient = c(bd_ci$coefs[1], bd_ci$coefs[3], bd_ci$coefs[2], rep(NA, 3)),
                             `Min CI` = c(bd_ci$min[1], bd_ci$min[3], bd_ci$min[2], rep(NA, 3)),
                             `Max CI` = c(bd_ci$max[1], bd_ci$max[3], bd_ci$max[2], rep(NA, 3)),
                             `R2 Marginal` = c(bd_model_r2[1], rep(NA, 5)),
                             `R2 Conditional` = c(bd_model_r2[2], rep(NA, 5)),
                             AIC = c(AIC(bd_m_a_model), rep(NA, 5)))
# write.csv(bd_model_table, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/BD_model_table.csv', row.names = FALSE)

### Model subsidence ###############################################################################################################
# I don't think I can actually include ALT, since it was used to determine subsidence - nothing is looking good.
# model with ALT, bulk density, and moisture
model1 <- lmer(mean.subsidence ~ treatment + mean.bulk.density + mean.moisture1 + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = sub_ash_model_ready)
summary(model1)
# look at residuals
model1.resid <- resid(model1)
model1.fitted <- fitted(model1)
model1.sqrt <- sqrt(abs(resid(model1)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(model1.fitted, model1.resid, main='resid, model1')
plot(model1.fitted, model1.sqrt, main='sqrt resid, model1')
qqnorm(model1.resid, main = 'model1')
qqline(model1.resid)
par(mfrow=c(1,1))

# try removing bulk.density
model2 <- lmer(mean.subsidence ~ mean.ALT + mean.moisture1 + # use * or + depending on the interactions you want to specify
                           (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
                         data = sub_ash_model_ready)
summary(model2)
anova(model1, model2) # simple model without ash layer is better

# try removing moisture1
model3 <- lmer(mean.subsidence ~ mean.ALT + mean.bulk.density + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = sub_ash_model_ready)
summary(model3)
anova(model1, model2, model3) # model3 is best

# try removing ALT
model4 <- lmer(mean.subsidence ~ mean.bulk.density + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = sub_ash_model_ready)

# try removing bulk density
model5 <- lmer(mean.subsidence ~ mean.ALT + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = sub_ash_model_ready)

anova(model3, model4, model5)

# re-run model2 with REML = TRUE
sub_environ_model <- lmer(mean.subsidence ~ mean.ALT + bulk.density + moisture1 + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = TRUE,
               data = sub_ash_model_ready)
summary(sub_environ_model)
sub_environ_ci <- extract_ci(sub_environ_model)
# save the model
# saveRDS(sub_environ_model, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/subsidence_environ_model.rds')
# write.csv(sub_environ_ci, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/subsidence_environ_model_coefs.csv', row.names = FALSE)
####################################################################################################################################
sub_environ_model <- readRDS('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/subsidence_environ_model.rds')
sub_environ_ci <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/subsidence_environ_model_coefs.csv')
bd_model_r2 <- r.squaredGLMM(bd_m_a_model)
summary(sub_environ_model)

### Model moisture ############################################################################################################
## Model with interactions between time and all the other predictor variables
mmodel1 <- lmer(moisture ~ time + ash.layer + treatment2 + mean.ALT + time:ash.layer + time:treatment2 + time:mean.ALT + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

# check for collinearity
car::vif(mmodel1) # again a lot - remove time and interactions and start over

## Model with ash.layer, treatment, and mean.ALT
mmodel1 <- lmer(moisture ~ ash.layer + treatment2 + mean.ALT + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

# check for collinearity
car::vif(mmodel1) # looks good, start simplifying model and testing

## Model with ash.layer and treatment
mmodel2 <- lmer(moisture ~ ash.layer + treatment2 + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

## Model with ash.layer and treatment
mmodel3 <- lmer(moisture ~ ash.layer + mean.ALT + # use * or + depending on the interactions you want to specify
                  (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
                data = ash_profiles_model_ready)

## Model with treatment and ALT
mmodel4 <- lmer(moisture ~ treatment2 + mean.ALT + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

AICc(mmodel1, mmodel2, mmodel3, mmodel4) # model 3 is best, compare with simpler models

## Model with ash.layer
mmodel5 <- lmer(moisture ~ ash.layer + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

## Model with ALT
mmodel6 <- lmer(moisture ~ mean.ALT + # use * or + depending on the interactions you want to specify
                 (1 | block/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = ash_profiles_model_ready)

AICc(mmodel3, mmodel5, mmodel6) # model 3 is best

summary(model3)

# check model assumptions of best fitting model: model3
hist(ash_profiles_model_ready$moisture)

# look at residuals
model3.resid <- resid(model3)
model3.fitted <- fitted(model3)
model3.sqrt <- sqrt(abs(resid(model3)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(model3.fitted, model3.resid, main='resid, model3')
plot(model3.fitted, model3.sqrt, main='sqrt resid, model3')
qqnorm(model3.resid, main = 'model3')
qqline(model3.resid)
par(mfrow=c(1,1))

# re-run best model with REML = TRUE
m.model <- lmer(moisture ~ ash.layer + mean.ALT + # use * or + depending on the interactions you want to specify
                   (1 | block/fencegroup/wholeplot) + (1|time), REML = TRUE,
                 data = ash_profiles_model_ready)

summary(m.model)

# calculate confidence intervals to look at fixed effects
m_model_ci <- extract_ci(m.model)

# save model output
# saveRDS(m.model, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/m_model.rds')
# write.csv(m_model_ci, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/m_model_coefficients.csv', row.names = FALSE)
####################################################################################################################################
# m_model <- readRDS('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/m_model.rds')
# m_model_ci <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/m_model_coefficients.csv')
# m_model_r2 <- r.squaredGLMM(m_model)
# summary(m_model)

### Graph models ###################################################################################################################
# bulk density
bd_m_a_ribbon <- expand.grid(moisture = seq(min(ash_profiles_model_ready$moisture, na.rm = TRUE), max(ash_profiles_model_ready$moisture, na.rm = TRUE), length.out = 50),
                              ash.layer = seq(5, 40, 5)) %>%
  mutate(moisture1 = moisture^-1,
         bd.lwr = bd_ci$min[1] + bd_ci$min[2]*ash.layer + bd_ci$min[3]*moisture1,
         bd.upr = bd_ci$max[1] + bd_ci$max[2]*ash.layer + bd_ci$max[3]*moisture1,
         bd.fit = bd_ci$coefs[1] + bd_ci$coefs[2]*ash.layer + bd_ci$coefs[3]*moisture1)
bd_m_a_r2 <- r.squaredGLMM(bd_m_a_model)

bulk.density.moisture <- ggplot(ash_profiles_model_ready, aes(x = moisture, y = bulk.density, colour = ash.layer)) +
  geom_point() +
  geom_line(data = bd_m_a_ribbon, aes(x = moisture, y = bd.fit, group = ash.layer, colour = ash.layer), inherit.aes = FALSE) +
  scale_color_gradient(name = 'Ash Normalized\nDepth (g ash)',
                       low = '#3399CC',
                       high = 'black',
                       guide = guide_legend(reverse = FALSE),
                       breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
  scale_x_continuous(name = expression(Soil~Moisture~(g~Water/g~Soil))) +
  scale_y_continuous(name = expression(Bulk~Density~(g/cm^3))) +
  theme_few() +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x  = element_text(size = 8),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8)) +
  coord_fixed(ratio = 0.8) + 
  annotate('text', x = 1.225, y = 1.9, label = paste('BD = ', round(bd_ci$coefs[1], 2), ' - ', round(bd_ci$coefs[2], 2)*-1, '(AND) + ', round(bd_ci$coefs[3], 2), '(1/SM)', sep = ''), size = 3) +
  annotate('text', x = 1.41, y = 1.8, label = paste0("~R^2~c==", round(as.numeric(bd_m_a_r2[2]), 2)), parse = TRUE, size = 3)

bulk.density.moisture

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Bulk_Denisty_Moisture_Mixed_Effects_power_2018_notitle.jpg', bulk.density.moisture, width = 180, height = 180, units = 'mm')
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Bulk_Density_Moisture_Mixed_Effects_power_2018_notitle.pdf', bulk.density.moisture, width = 180, height = 180, units = 'mm')

# subsidence
ggplot(ash_profiles_graph, aes(x = mean.ALT, y = mean.subsidence)) +
  geom_point()

subsidence <- ggplot(ash_profiles_model_ready, aes(x = mean.ALT, y = mean.subsidence, colour = bulk.density)) +
  geom_point() +
  geom_line(data = bd_m_a_ribbon, aes(x = moisture, y = bd.fit, group = ash.layer, colour = ash.layer), inherit.aes = FALSE) +
  scale_color_gradient(name = 'Ash Layer (g)',
                       low = '#3399CC',
                       high = 'black',
                       guide = guide_legend(reverse = FALSE)) +
  scale_x_continuous(name = expression(Ice~Content~('%'))) +
  scale_y_continuous(name = expression(Bulk~Density~(g/cm^3))) +
  theme_few() +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x  = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12)) +
  coord_fixed(ratio = 0.8) + 
  annotate('text', x = 1, y = 1.7, label = paste('BD = ', round(bd_ci$coefs[1], 2), ' + ', round(bd_ci$coefs[2], 2), '(ash.layer) + ', round(bd_ci$coefs[3], 2), '(1/SM)', sep = '')) +
  annotate('text', x = 1.325, y = 1.6, label = paste0("~R^2~c==", round(as.numeric(bd_m_a_r2[2]), 2)), parse = TRUE)

subsidence
##################################################################################################################################