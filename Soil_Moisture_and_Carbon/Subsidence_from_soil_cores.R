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
Elevation <- list(brick(filenames[1]), brick(filenames[2]), brick(filenames[3]))
# Load ALT
ALTsub <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.csv')
####################################################################################################################################

### Format 2017 data and combine with rest of data #################################################################################
# for (i in 1:nrow(soil_17)){
#   if (!is.na(soil_17$depth[i])) {
#     next # do nothing if there is a depth listed
#   } else {
#     soil_17$depth[i] <- soil_17$depth[i-1] # if no depth is listed, find the last depth and put that in
#   }
#   rm(i)
# }
# 
# soil_17 <- soil_17 %>%
#   filter(`sub-smp` == 'all' | `sub-smp` == 'CN/mstr') %>% # these rows contain the bulk density and moisture measurements I need
#   select(1:3, 5, 7, 18:19) %>% # these are the columns I want
#   mutate(year = 2017, # make some more columns of necessary information
#          block = ifelse(Fence <= 2,
#                         1,
#                         ifelse(Fence == 3 | Fence == 4,
#                                2,
#                                3)),
#          treatment = ifelse(plot <= 4,
#                             'c',
#                             'w')) %>%
#   select(year, block, fence = Fence, plot, treatment, depth.cat = depth, moisture = Moisture, bulk.density = `Bulk density`) %>% # rename and select to be consistent with earlier soil core data
#   group_by(year, block, fence, plot, treatment, depth.cat) %>%
#   summarise(moisture = max(moisture, na.rm = TRUE), # this essentially combines the two rows (one with bulk density, one with moisture) into one
#             bulk.density = max(bulk.density, na.rm = TRUE)) %>%
#   separate(depth.cat, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>% # make depth columns for the lower and upper depths of a core section
#   mutate(depth0 = as.numeric(depth0), # make sure the depths are numbers not text
#          depth1 = as.numeric(depth1)) %>%
#   arrange(year, block, fence, plot, depth0) # order the data in a way that makes sense
# 
# group <-  soil_17 %>%
#   group_by(year, fence, plot, treatment) %>%
#   group_indices('year', 'fence', 'plot', 'treatment')
# 
# soil_17 <- soil_17 %>%
#   cbind.data.frame(group) %>%
#   group_by(year, fence, plot, treatment)
# 
# depths <- c(0, seq(5, 95, 10), 100) # these are the standardized depths we want, but currently don't have
# 
# this next for loop is not working yet, but until there is ash data available for 2017, it doesn't really matter anyway
# soil_17_2 <- data.frame()
# for (i in 1:n_groups(soil_17)) {
#   temp <- subset(soil_17, group == i)
#   for (k in 1:nrow(temp)) {
#     print(paste('k =', k))
#     if (depths[k] == soil_17$depth0[k] & depths[k+1] == soil_17$depth1[k]) {
#       print('no change needed')
#       next
#     } else if (depths[k] != soil_17$depth0[k] & depths[k+1] == soil_17$depth1[k] | depths[k] != soil_17$depth0[k] & depths[k+1] < soil_17$depth1[k]) {# if only the upper depth is incorrect, or the upper depth is incorrect and the lower depth is too big
#       if (depths[k] > soil_17$depth0[k]) {# if the recorded depth is less than the standardized depth for the upper depth
#         print('option 1')
#         temp$depth0[k] <- depths[k]
#         temp$depth1[k] <- depths[k+1]
#         temp$depth.cat[k] <- paste(temp$depth0[k], temp$depth1[k], sep = '-')
#       } else {# if the recorded depth is greater than the standardized depth for the upper depth
#         print('option 2')
#         temp$moisture[k] <- ((temp$depth0[k] - depths[k]) * temp$moisture[k-1] + (temp$depth1[k] - temp$depth0[k]) * temp$moisture[k])/(depths[k+1] - depths[k]) # add the weighted moisture from the previous soil section
#         temp$bulk.density[k] <- ((temp$depth0[k] - depths[k]) * temp$bulk.density[k-1] + (temp$depth1[k] - temp$depth0[k]) * temp$bulk.density[k])/(depths[k+1] - depths[k]) # add the weighted bulk density from the previous soil section
#         temp$depth0[k] <- depths[k]
#         temp$depth1[k] <- depths[k+1]
#         temp$depth.cat[k] <- paste(temp$depth0[k], temp$depth1[k], sep = '-')
#       }
#     } else if (depths[k] == soil_17$depth0[k] & depths[k+1] != soil_17$depth1[k] | depths[k] > soil_17$depth0[k] & depths[k+1] != soil_17$depth1[k]) {# if only the lower depth is incorrect, or if the lower depth is incorrect and the upper depth is too small
#       if (depths[k+1] > soil_17$depth1[k]) {# if the recorded depth is less than the standardized depth for the lower depth
#         print('option 3')
#         temp$moisture[k] <- ((temp$depth1[k] - temp$depth0[k]) * temp$moisture[k] + (depths[k+1] - temp$depth1[k]) * temp$moisture[k])/(depths[k+1] - depths[k]) # add the weighted moisture from the next soil section
#         temp$bulk.density[k] <- ((temp$depth1[k] - temp$depth0[k]) * temp$bulk.density[k] + (depths[k+1] - temp$depth1[k]) * temp$bulk.density[k])/(depths[k+1] - depths[k]) # add the weighted bulk density from the next soil section
#         temp$depth0[k] <- depths[k]
#         temp$depth1[k] <- depths[k+1]
#         temp$depth.cat[k] <- paste(temp$depth0[k], temp$depth1[k], sep = '-')
#       } else {# if the recorded depth is greater than the standardized depth for the lower depth
#         print('option 4')
#         temp$depth0[k] <- depths[k]
#         temp$depth1[k] <- depths[k+1]
#         temp$depth.cat[k] <- paste(temp$depth0[k], temp$depth1[k], sep = '-')
#       }
#     } else {# if upper is too big and lower is too small
#       print('option 5')
#       temp$moisture[k] <- ((temp$depth0[k] - depths[k]) * temp$moisture[k-1] + (temp$depth1[k] - temp$depth0[k]) * temp$moisture[k] + (depths[k+1] - temp$depth1[k]) * temp$moisture[k])/(depths[k+1] - depths[k]) # add the weighted moisture from the next soil section
#       temp$bulk.density[k] <- ((temp$depth0[k] - depths[k]) * temp$bulk.density[k-1] + (temp$depth1[k] - temp$depth0[k]) * temp$bulk.density[k] + (depths[k+1] - temp$depth1[k]) * temp$bulk.density[k])/(depths[k+1] - depths[k]) # add the weighted bulk density from the next soil section
#       temp$depth0[k] <- depths[k]
#       temp$depth1[k] <- depths[k+1]
#       temp$depth.cat[k] <- paste(temp$depth0[k], temp$depth1[k], sep = '-')
#     }
#   }
#   soil_17_2 <- rbind.data.frame(soil_17_2, temp) %>%
#     distinct
# }
# 
####################################################################################################################################

### functions necessary for following code #########################################################################################
calc_ash_depth <- function(x) {
  if (x$year[1] == x$year.min.mass[1]) {
    depth <- x$max.depth[1]
  } else {
    running.mass <- 0
    n = nrow(x)
    for (i in 1:n) {
      if ((running.mass + x$ash.mass[i]) < x$ash.mass.total[i]) {
        running.mass <- running.mass + x$ash.mass[i]
        depth <- x$depth1[i]
      } else if (running.mass == x$ash.mass.total[i]) {
        depth <- depth
      } else {
        depth <- depth + (x$depth1[i] - x$depth0[i]) * ((x$ash.mass.total[i] - running.mass)/x$ash.mass[i])
        running.mass <- x$ash.mass.total[i]
      }
    }
  }
  data.frame(return(depth))
}

calc_soil_depth <- function(x) {
  if (x$year[1] == x$year.min.mass[1]) {
    depth <- x$max.depth[1]
  } else {
    running.mass <- 0
    n = nrow(x)
    for (i in 1:n) {
      if ((running.mass + x$soil.mass[i]) < x$soil.mass.total[i]) {
        running.mass <- running.mass + x$soil.mass[i]
        depth <- x$depth1[i]
      } else if (running.mass == x$soil.mass.total[i]) {
        depth <- depth
      } else {
        depth <- depth + (x$depth1[i] - x$depth0[i]) * ((x$soil.mass.total[i] - running.mass)/x$soil.mass[i])
        running.mass <- x$soil.mass.total[i]
      }
    }
  }
  data.frame(return(depth))
}
####################################################################################################################################

### calculate the height of cores with the same amount of soil mass in 2009 vs. 2013 ################################################
ash_mass_09_13 <- soil_09_13 %>%
  select(1:10) %>%
  filter(year == 2009 | year == 2013) %>%
  filter(moisture != is.na(moisture)) %>%
  group_by(year, block, fence, treatment) %>%
  mutate(soil.mass = bulk.density*(depth1 - depth0),
         ash.mass = ash*bulk.density*(depth1 - depth0)/1000,
         ash.mass.tot = cumsum(ash.mass))

ash_mass_total <- ash_mass_09_13 %>%
  group_by(year, block, fence, treatment) %>%
  summarise(soil.mass.total = sum(soil.mass, na.rm = TRUE),
            ash.mass.total = sum(ash.mass, na.rm = TRUE),
            max.depth = max(depth1)) %>%
  group_by(block, fence, treatment) %>%
  filter(ash.mass.total == min(ash.mass.total)) %>%
  arrange(block, fence, treatment) %>%
  rename(year.min.mass = year)

ash_mass_09_13_2 <- ash_mass_09_13 %>%
  left_join(ash_mass_total, by = c('block', 'fence', 'treatment')) %>%
  group_by(year, block, fence, treatment)

soil_core_sub <- ddply(ash_mass_09_13_2, c('year', 'block', 'fence', 'treatment'), calc_ash_depth) %>%
  mutate(year = paste('depth.', year, sep = '')) %>%
  spread(key = year, value = V1) %>%
  mutate(soil.core.sub.ash = depth.2013 - depth.2009) %>%
  select(-depth.2009, -depth.2013)

soil_core_sub_2 <- ddply(ash_mass_09_13_2, c('year', 'block', 'fence', 'treatment'), calc_soil_depth) %>%
  mutate(year = paste('depth.', year, sep = '')) %>%
  spread(key = year, value = V1) %>%
  mutate(soil.core.sub.soil = depth.2013 - depth.2009) %>%
  select(-depth.2009, -depth.2013) %>%
  full_join(soil_core_sub, by = c('block', 'fence', 'treatment'))

####################################################################################################################################

### compare soil core subsidence with gps subsidence at that location ##############################################################
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
  full_join(soil_core_sub_2, by = c('block', 'fence', 'treatment')) %>%
  mutate(sub.diff.soil = GPS.sub - soil.core.sub.soil,
         sub.diff.ash = GPS.sub - soil.core.sub.ash)

mean.sub.diff.ash = mean(sub_comparison$sub.diff.ash)
sd.sub.diff.ash = sd(sub_comparison$sub.diff.ash)

# write.csv(sub_comparison, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/Soil_Core_vs_GPS_Subsidence.csv', row.names = FALSE)

sub_comparison_graph <- sub_comparison %>%
  gather(key = source, value = subsidence, GPS.sub:soil.core.sub.ash)

g1 <- ggplot(sub_comparison_graph, aes(x = source, y = subsidence)) +
  geom_boxplot() +
  theme_few() +
  ggtitle('Comparison of 2009-2013 Subsidence Using GPS and Soil Cores') +
  annotate('text', x = 'soil.core.sub.ash', y = 20, label = '*', size = 12)
g1
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/Soil_Core_vs_GPS_Subsidence.jpeg', g1)

t.test(sub_comparison$sub.diff.ash)
model <- lm(GPS.sub ~ soil.core.sub.ash, data = sub_comparison)
sub_comparison <- sub_comparison %>%
  mutate(fit = model$coefficients[1] + model$coefficients[2]*soil.core.sub.ash)

g2 <- ggplot(sub_comparison, aes(x = soil.core.sub.ash, y = GPS.sub, colour = treatment)) +
  geom_point() +
  geom_line(aes(x = soil.core.sub.ash, y = fit), inherit.aes = FALSE) +
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

### How much does each depth increment contribute to subsidence? ###################################################################
# recalculate soil core metrics by layers determined by ash mass rather than depth
ash_mass_groups <- soil_09_13 %>%
  filter(moisture != is.na(moisture)) %>%
  group_by(year, fence, treatment) %>%
  group_indices('year', 'fence', 'treatment')

ash_mass <- soil_09_13 %>%
  select(1:10) %>%
  filter(moisture != is.na(moisture)) %>%
  group_by(year, block, fence, treatment) %>%
  mutate(soil.mass = bulk.density*(depth1 - depth0),
         ash.mass = ash*bulk.density*(depth1 - depth0)/1000,
         ash.mass.tot = cumsum(ash.mass)) %>%
  cbind.data.frame(ash_mass_groups) %>%
  rename(group = ash_mass_groups)

ash_profiles <- expand.grid(year = c(2009, 2010, 2011, 2013), 
                            fence = seq(1,6), treatment = c('c', 'w'), 
                            ash.layer = c(5, 10, 15, 20, 25, 30, 35, 40)) %>%
  arrange(year, fence, treatment, ash.layer) %>%
  mutate(moisture = '',
         bulk.density = '',
         group = rep(seq(1,48), times = 1, each = 8)) %>%
group_by(year, fence, treatment)

for (k in 1:max(ash_profiles$group)) {
  depth_subset <- ash_mass %>%
    filter(group == k)
  ash_subset <- ash_profiles %>%
    filter(group == k)
  max.ash.mass <- max(depth_subset$ash.mass.tot)
  for (i in 1:group_size(ash_subset)) {
    avg.bd <- 0
    avg.moisture <- 0
    ash.level.upr <- ash_subset$ash.layer[i]
    ash.level.lwr <- ash.level.upr - 5
    index.upr <- as.numeric(first(which(depth_subset$ash.mass.tot > ash.level.upr)))
    index.lwr <- as.numeric(first(which(depth_subset$ash.mass.tot > ash.level.lwr)))
    if (is.na(index.upr) & is.na(index.lwr)){
      weighted.bd <- NA
      weighted.moisture <- NA
    } else if (index.lwr == index.upr | is.na(index.upr)) {
      ash_profiles$bulk.density[(k-1)*8+i] <- depth_subset$bulk.density[index.lwr]
      ash_profiles$moisture[(k-1)*8+i] <- depth_subset$moisture[index.lwr]
    } else {
      for (j in index.lwr:index.upr) {
        if (depth_subset$ash.mass.tot[j] <= ash.level.upr) {
          weighted.bd <- ((depth_subset$ash.mass.tot[j] - ash.level.lwr)/5)*depth_subset$bulk.density[j]
          weighted.moisture <- ((depth_subset$ash.mass.tot[j] - ash.level.lwr)/5)*depth_subset$moisture[j]
        } else {
          weighted.bd <- ((ash.level.upr - depth_subset$ash.mass.tot[j-1])/5)*depth_subset$bulk.density[j]
          weighted.moisture <- ((ash.level.upr - depth_subset$ash.mass.tot[j-1])/5)*depth_subset$moisture[j]
        }
        avg.bd <- avg.bd + weighted.bd
        avg.moisture <- avg.moisture + weighted.moisture
      }
      ash_profiles$bulk.density[(k-1)*8+i] <- avg.bd
      ash_profiles$moisture[(k-1)*8+i] <- avg.moisture
    }
  }
  rm(i, j, k, avg.bd, avg.moisture, ash.level.lwr, ash.level.upr, index.lwr, index.upr, weighted.bd, weighted.moisture)
}
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

ash_profiles_model_ready <- ash_profiles %>%
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
         bulk.density = as.numeric(bulk.density),
         mean.ALT = mean.ALT/100, # change to m for better scale
         moisture1 = 1/moisture) %>%
  select(year, fence, treatment, time, block, fence2, treatment2, fencegroup, wholeplot, group, ash.layer, bulk.density, moisture, moisture1, mean.subsidence, se.subsidence, mean.ALT, se.ALT)

sub_ash_model_ready <- ash_profiles_model_ready %>%
  group_by(year, fence, treatment, group, time, block, fence2, treatment2, fencegroup, wholeplot, mean.subsidence, se.subsidence, mean.ALT, se.ALT) %>%
  summarise(mean.moisture1 = mean(moisture1, na.rm = TRUE),
            se.moisture1 = sd(moisture1)/sqrt(n()),
            mean.bulk.density = mean(bulk.density, na.rm = TRUE),
            se.bulk.density = sd(bulk.density)/sqrt(n()),
            mean.moisture = mean(moisture, na.rm = TRUE),
            se.moisture = sd(moisture, na.rm = TRUE)/sqrt(n()))

plot_ash_profiles <- ash_profiles_model_ready %>%
  group_by(year, ash.layer, treatment) %>%
  summarise(mean.bulk.density = mean(bulk.density, na.rm = TRUE),
            mean.moisture = mean(moisture, na.rm = TRUE),
            se.bulk.density = sd(bulk.density, na.rm = TRUE)/sqrt(n()),
            se.moisture = sd(moisture, na.rm = TRUE)/sqrt(n()))

ggplot(plot_ash_profiles, aes(x = year, y = mean.bulk.density, colour = treatment)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean.bulk.density - se.bulk.density, ymax = mean.bulk.density + se.bulk.density)) +
  facet_grid(. ~ ash.layer) +
  scale_colour_manual(breaks = c('c', 'w'),
                      labels = c('Control', 'Warming'),
                      values = c('blue', 'red'))

ash_profiles_model_ready %>%
  dplyr::select(mean.subsidence, bulk.density, time, ash.layer, treatment2, mean.ALT, moisture, moisture1) %>%
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
  scale_x_continuous(name = expression(Soil~Moisture~(gWater/gSoil))) +
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
  ggtitle('Bulk Density Responds to Soil Moisture and Depth') +
  coord_fixed(ratio = 0.8) + 
  annotate('text', x = 0.95, y = 1.9, label = paste('BD = ', round(bd_ci$coefs[1], 2), ' - ', round(bd_ci$coefs[2], 2)*-1, '(Ash Depth) + ', round(bd_ci$coefs[3], 2), '(1/SM)', sep = '')) +
  annotate('text', x = 1.325, y = 1.8, label = paste0("~R^2~c==", round(as.numeric(bd_m_a_r2[2]), 2)), parse = TRUE)

bulk.density.moisture

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Bulk_Denisty_Moisture_Mixed_Effects_power_2018.jpg', bulk.density.moisture, height = 6, width = 7.5)
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Bulk_Density_Moisture_Mixed_Effects_power_2018.pdf', bulk.density.moisture, height = 6, width = 7.5)

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