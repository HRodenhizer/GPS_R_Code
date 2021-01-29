############################################################################################################
###                  Soil Moisture to estimate possible subsidence                                       ###
###                  Code by HGR 3/2018                                                                  ###
############################################################################################################

### This was done before I realized that there is data which is better formatted for this process - use Soil_moisture_and_subsidence_v2 for a more streamlined process.

# load libraries
library(tidyverse)
library(readxl)

# part I - calcualte expected subsidence given moisture content
#load data
# soil <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/EML_AK_CiPEHR_SoilProperties_2009-2013_Data.csv")
soil2009 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/CiPEHR Soil Data 2009.xlsx', sheet = 3)
soil2017 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/2017 Soils_processing data sheet_3_8_18_filleddepthvalues.xlsx', sheet = 1)[-c(1061:1221),-c(22, 23)]
ALTsub <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017.csv')
ALTsub.geoid <- read.csv('file:///C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017_geoid_corrected.csv')

# get mean moisture by treatment
# moisture.df <- soil %>%
#   group_by(year, treatment, depth) %>%
#   mutate(vwc = moisture*bulk.density/1000) %>%
#   summarise(moisture = mean(moisture),
#             vwc = mean(vwc),
#             bulk.density = mean(bulk.density))
# 
# moisture.summary <- moisture.df %>%
#   group_by(treatment) %>%
#   summarise(mean.vwc = mean(vwc))
#   
# # plot
# ggplot(moisture.df, aes(x = year, y = vwc, colour = depth)) +
#   geom_line(aes(linetype = treatment))

# select necessary columns from 2009 and 2017 data and calculate ice mass and volume
soil2009.subset <- soil2009 %>%
  select(-bag, -Ht, -L, -W, -`foil/boat wt`, -`wet +foil`, -`dr+boat`) %>%
  filter(`sub-smp` == 'all' | `sub-smp` == 'CN/mstr') %>%
  mutate(wet = as.numeric(wet),
         ice_mass = wet - dry,
         ice_volume = ice_mass/0.9167)

soil2017.subset <- soil2017 %>%
  select(-bag, -Ht, -L, -W, -`foil/boat wt`, -`wet +foil`, -`dr+boat`) %>%
  filter(`sub-smp` == 'all' | `sub-smp` == 'CN/mstr') %>%
  mutate(ice_mass = wet - dry,
         ice_volume = ice_mass/0.9167) %>%
  filter(`well#` != '5-6.17' & depth != '55-65')

# Determine ratio of wet sub sample to total sample for each sample and calculate ice mass and volume of whole sample
ratio2009 <- soil2009.subset %>%
  select(Fence:`O-M`, depth:`sub-smp`, wet) %>%
  spread(key = `sub-smp`, value = wet) %>%
  mutate(ratio = all/`CN/mstr`) %>%
  gather(key = `sub-smp`, value = wet, all:`CN/mstr`)

ratio2017 <- soil2017.subset %>%
  select(Fence:`O-M`, depth:`sub-smp`, wet) %>%
  spread(key = `sub-smp`, value = wet) %>%
  mutate(ratio = all/`CN/mstr`) %>%
  gather(key = `sub-smp`, value = wet, all:`CN/mstr`)

# join ratio to rest of data
soil2009.join <- soil2009.subset %>%
  full_join(ratio2009, by = c("Fence", "plot", "well#", "O-M", "depth", "sub-smp", "wet"))
  

soil2017.join <- soil2017.subset %>%
  full_join(ratio2017, by = c("Fence", "plot", "well#", "O-M", "depth", "sub-smp", "wet"))

# Calculate ice volume in separate data frame
ice2009 <- soil2009.join %>%
  select(Fence:`O-M`, depth:`sub-smp`, ice_volume:ratio) %>%
  spread(key = `sub-smp`, value = ice_volume) %>%
  mutate(all = `CN/mstr`*ratio) %>%
  gather(key = `sub-smp`, value = ice_volume, all:`CN/mstr`)
  
ice2017 <- soil2017.join %>%
  select(Fence:`O-M`, depth:`sub-smp`, ice_volume:ratio) %>%
  spread(key = `sub-smp`, value = ice_volume) %>%
  mutate(all = `CN/mstr`*ratio) %>%
  gather(key = `sub-smp`, value = ice_volume, all:`CN/mstr`)

# Join ice volume to rest of data and calculate total ice volume of data
soil2009.join2 <- soil2009.join %>%
  select(-ice_volume) %>%
  full_join(ice2009, by = c("Fence", "plot", "well#", "O-M", "depth", "sub-smp", "ratio")) %>%
  mutate(ice_mass = ifelse(is.na(ice_mass),
                           ice_volume*0.9167,
                           ice_mass)) %>%
  filter(`sub-smp` == 'all') %>%
  group_by(Fence, plot, `well#`) %>%
  summarise(volume = sum(volume),
            ice_volume = sum(ice_volume)) %>%
  mutate(ice_fraction = ice_volume/volume) %>%
  rename(fence = Fence)

soil2017.join2 <- soil2017.join %>%
  select(-ice_volume) %>%
  full_join(ice2017, by = c("Fence", "plot", "well#", "O-M", "depth", "sub-smp", "ratio")) %>%
  mutate(ice_mass = ifelse(is.na(ice_mass),
                           ice_volume*0.9167,
                           ice_mass)) %>%
  filter(`sub-smp` == 'all') %>%
  group_by(Fence, plot, `well#`) %>%
  summarise(volume = sum(volume),
            ice_volume = sum(ice_volume))

# Spread ALT data to calculate change in ALT from 2009 - 2017
ALT <- ALTsub %>%
  dplyr::select(year, exp, block, fence, plot, treatment, ALT.corrected) %>%
  spread(key = year, value = ALT.corrected) %>%
  mutate(delta_ALT = `2017`-`2009`,
         plot = as.character(plot)) %>%
  right_join(soil2009.join2, by = c('fence', 'plot')) %>%
  mutate(exp_sub = ice_fraction*0.0833*delta_ALT)

ALT.geoid <- ALTsub.geoid %>%
  dplyr::select(year, exp, block, fence, plot, treatment, ALT.corrected) %>%
  spread(key = year, value = ALT.corrected) %>%
  mutate(delta_ALT = `2017`-`2009`,
         plot = as.character(plot)) %>%
  right_join(soil2009.join2, by = c('fence', 'plot')) %>%
  mutate(exp_sub = ice_fraction*0.0833*delta_ALT)

# write.csv(ALT.geoid, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Soil_Moisture_Subsidence_2009_2017_geoid_corrected.csv', row.names = FALSE)

# part II - soil moisture content only explains a small portion of subsidence, so this part incorporates changes in bulk density
soil_sum_09_13 <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2009-2013 CiPEHR Processed SOIL DATA_César.xlsx", sheet = 1)
soil2017_2 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/2017 Soils_processing data sheet_3_8_18_filleddepthvalues.xlsx', sheet = 2)[,-c(8:10)]

soil2017_3 <- soil2017_2 %>%
  select(-`B-C`) %>%
  mutate(year = 2017)
# soil2009.all <- soil2009.join %>%
#   filter(`sub-smp` == 'all') %>%
#   select(-`sub-smp`, -wet, -dry, -Moisture, -Rock, -Notes, -ice_mass, -ice_volume, -ratio)
# 
# # need to adjust so that depth is actual depth not depth within O or M layer
# soil2009.summary <- soil2009.join %>%
#   filter(`sub-smp` == 'CN/mstr') %>%
#   select(-`sub-smp`, -volume, -`Bulk density`, -Notes) %>%
#   full_join(soil2009.all, by = c("Fence", "plot", "well#", "O-M", "B-C", "depth")) %>%
#   mutate(treatment = ifelse(plot <= 4,
#                             'Control',
#                             'Warming'),
#          soil_layer = ifelse(`O-M` == 'M?',
#                              'M',
#                              `O-M`))
# 
# soil2009.depth <- soil2009.summary %>%
#   select(Fence, plot, `well#`, soil_layer, depth) %>%
#   separate(depth, c('depth_min', 'depth_max'), sep = '-', remove = FALSE) %>%
#   group_by(Fence, plot, `well#`, soil_layer) %>%
#   mutate(depth_min = as.numeric(depth_min),
#          depth_max = as.numeric(depth_max),
#          o_depth = ifelse(soil_layer == 'O',
#                           max(depth_max),
#                           NA)) %>%
#   group_by(Fence, plot, `well#`) %>%
#   mutate(o_depth = max(o_depth, na.rm = TRUE))
#   
#          depth_total = ifelse(soil_layer == 'O',
#                               soil_layer,
#                               max(soil_layer)+o_depth))
#   
#   group_by(soil_layer, treatment, depth) %>%
#   summarise(moisture = mean(Moisture, na.rm = TRUE),
#             ice_mass = mean(ice_mass, na.rm = TRUE),
#             ice_volume = mean(ice_volume, na.rm = TRUE),
#             bulk_density = mean(`Bulk density`, na.rm = TRUE)) %>%
#   arrange(treatment, desc(soil_layer), depth)

# ,
# total_depth = ifelse(`O-M` == 'O',
#                      `O-M`,
#                      str_c(str_sub(`O-M`, 1, 1) + ))