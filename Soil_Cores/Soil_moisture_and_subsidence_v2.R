############################################################################################################
###                  Soil Moisture to estimate possible subsidence                                       ###
###                  Code by HGR 3/2018                                                                  ###
############################################################################################################

# load libraries
library(tidyverse)
library(readxl)

# Load data
soil_09_13 <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2009-2013 CiPEHR Processed SOIL DATA_César.xlsx", sheet = 1) %>%
  separate(depth.cat, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  mutate(depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1))
soil2017 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/2017 Soils_processing data sheet_3_8_18_filleddepthvalues.xlsx', sheet = 2)[,-c(8:10)]
ALTsub.geoid <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017_geoid_corrected.csv')
ALTsub.control <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017.csv')

rawsoil2009 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/CiPEHR Soil Data 2009.xlsx', sheet = 3)
rawsoil2017 <- read_excel('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/2017 Soils_processing data sheet_3_8_18_filleddepthvalues.xlsx', sheet = 1)[-c(1061:1221),-c(22, 23)]

# Data formatting for joining (standardizing soil depths)
soil2017_2 <- soil2017 %>%
  select(-`B-C`) %>%
  mutate(year = 2017,
         treatment = ifelse(plot <= 4,
                            'c',
                            'w'),
         fence = Fence,
         block = ifelse(Fence <= 2,
                        1,
                        ifelse(Fence == 3 | Fence == 4,
                               2,
                               3)),
         depth = ifelse(depth == '45-55.5',
                        '45-55',
                        ifelse(depth == '55.5-65.5',
                               '55-65',
                               ifelse(depth == '65.5-68',
                                      '65-75',
                                      ifelse(depth == '45-54.5',
                                             '45-55',
                                             ifelse(depth == '54.5-64.5',
                                                    '55-65',
                                                    ifelse(depth == '65.5-75',
                                                           '65-75',
                                                           ifelse(depth == '95-102',
                                                                  '95-100',
                                                                  ifelse(depth == '55-65.5',
                                                                         '55-65',
                                                                         ifelse(depth == '65-75.5',
                                                                                '65-75',
                                                                                ifelse(depth == '75.5-85.5',
                                                                                       '75-85',
                                                                                       ifelse(depth == '65-69',
                                                                                              '65-75',
                                                                                              ifelse(depth == '54.5-65',
                                                                                                     '55-65',
                                                                                                     ifelse(depth == '75.5-85',
                                                                                                            '75-85',
                                                                                                            ifelse(depth == '75.5-80.5',
                                                                                                                   '75-85',
                                                                                                                   ifelse(depth == '55-56.5',
                                                                                                                          '55-65',
                                                                                                                          ifelse(depth == '35-44',
                                                                                                                                 '35-45',
                                                                                                                                 ifelse(depth == '44-53.5',
                                                                                                                                        '45-55',
                                                                                                                                        ifelse(depth == '53.5-58.5',
                                                                                                                                               '55-65',
                                                                                                                                               ifelse(depth == '65-76',
                                                                                                                                                      '65-75',
                                                                                                                                                      ifelse(depth == '45-52.5',
                                                                                                                                                             '45-55',
                                                                                                                                                             ifelse(depth == '55-62',
                                                                                                                                                                    '55-65',
                                                                                                                                                                    ifelse(depth == '75-82',
                                                                                                                                                                           '75-85',
                                                                                                                                                                           ifelse(depth == '65-72',
                                                                                                                                                                                  '65-75',
                                                                                                                                                                                  ifelse(depth == '72-85',
                                                                                                                                                                                         '75-85',
                                                                                                                                                                                         ifelse(depth == '85-97',
                                                                                                                                                                                                '85-95',
                                                                                                                                                                                                ifelse(depth == '97-99',
                                                                                                                                                                                                       '95-100',
                                                                                                                                                                                                       ifelse(depth == '65-66.5',
                                                                                                                                                                                                              '65-75',
                                                                                                                                                                                                              ifelse(depth == '65-73.5',
                                                                                                                                                                                                                            '65-75',
                                                                                                                                                                                                                            ifelse(depth == '0-4.5',
                                                                                                                                                                                                                                   '0-5',
                                                                                                                                                                                                                                   ifelse(depth == '4.5-15',
                                                                                                                                                                                                                                          '5-15',
                                                                                                                                                                                                                                          ifelse(depth == '45-47',
                                                                                                                                                                                                                                                 '45-55',
                                                                                                                                                                                                                                                 ifelse(depth == '55-63',
                                                                                                                                                                                                                                                        '55-65',
                                                                                                                                                                                                                                                        ifelse(depth == '25-37',
                                                                                                                                                                                                                                                               '25-35',
                                                                                                                                                                                                                                                               ifelse(depth == '37-45',
                                                                                                                                                                                                                                                                      '35-45',
                                                                                                                                                                                                                                                                      ifelse(depth == '55-66',
                                                                                                                                                                                                                                                                             '55-65',
                                                                                                                                                                                                                                                                             ifelse(depth == '66-75',
                                                                                                                                                                                                                                                                                    '65-75',
                                                                                                                                                                                                                                                                                    ifelse(depth == '55-62',
                                                                                                                                                                                                                                                                                           '55-65',
                                                                                                                                                                                                                                                                                           ifelse(depth == '75.5-87.5',
                                                                                                                                                                                                                                                                                                  '75-85',
                                                                                                                                                                                                                                                                                                  ifelse(depth == '5-10.5',
                                                                                                                                                                                                                                                                                                         '5-6.5',
                                                                                                                                                                                                                                                                                                         ifelse(depth == '6.5-15' & `well#` != '2-8.17' & `well#` != '3-6.17' ,
                                                                                                                                                                                                                                                                                                                '5-15',
                                                                                                                                                                                                                                                                                                                ifelse(depth == '75-80',
                                                                                                                                                                                                                                                                                                                       '75-85',
                                                                                                                                                                                                                                                                                                                       ifelse(depth == '44-55',
                                                                                                                                                                                                                                                                                                                              '45-55',
                                                                                                                                                                                                                                                                                                                              ifelse(depth == '55-60',
                                                                                                                                                                                                                                                                                                                                     '55-65',
                                                                                                                                                                                                                                                                                                                                     depth)))))))))))))))))))))))))))))))))))))))))))) %>%
  separate(depth, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  select(year, block, fence = Fence, `well#`, treatment, depth, depth0, depth1, moisture = Moisture, bulk.density = `Bulk density`) %>%
  mutate(score = ifelse(depth0 != 0 & depth0 != 5 & depth0 != 15 & depth0 != 25 & depth0 != 35 & depth0 != 45 & depth0 != 55 & depth0 != 65 & depth0 != 75 & depth0 != 85 & depth0 != 95,
                        1,
                        ifelse(depth1 != 5 & depth1 != 15 & depth1 != 25 & depth1 != 35 & depth1 != 45 & depth1 != 55 & depth1 != 65 & depth1 != 75 & depth1 != 85 & depth1 != 95 & depth1 != 100,
                               1,
                               0)),
         depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1),
         moisture = moisture*1000)

# summarize data which has two sets of moisture and bulk density values for one 10 cm chunk
soil2017_depth_normal <- soil2017_2 %>%
  filter(score == 1) %>%
  mutate(ddepth = depth1-depth0) %>%
  group_by(year, block, fence, `well#`)%>%
  summarise(moisture = sum(moisture*ddepth)/(max(depth1)-min(depth0)),
            bulk.density = sum(bulk.density*ddepth)/(max(depth1)-min(depth0)),
            depth0 = min(depth0),
            depth1 = max(depth1)) %>%
  mutate(treatment = ifelse(`well#` == '1-2.17' | `well#` == '1-4.17' | `well#` == '2-1.17' | `well#` == '2-3.17' | `well#` == '4-1.17' | `well#` == '5-3.17' | `well#` == '6-3.17',
                            'c',
                            'w'),
         depth = str_c(depth0, depth1, sep = '-')) %>%
  select(year, block, fence, `well#`, treatment, depth, depth0, depth1, moisture, bulk.density)

# create data frame with rows for all depths to 100 cm
`well#` <- c('1-1.17', '1-2.17', '1-4.17', '1-7.17', '1-8.17', '2-1.17',  '2-3.17',  '2-6.17', '2-8.17',  '3-2.17', '3-3.17', '3-6.17', '3-8.17', '4-1.17', '4-2.17', '4-5.17', '4-8.17', '5-1.17', '5-3.17', '5-6.17', '5-8.17', '6-1.17', '6-3.17', '6-5.17', '6-8.17')
depth <- c('0-5', '5-15', '15-25', '25-35', '35-45', '45-55', '55-65', '65-75', '75-85', '85-95', '95-100')
depths <- expand.grid(`well#` = `well#`, depth = depth) %>%
  as.data.frame() %>%
  mutate(`well#` = as.character(`well#`),
         depth = as.character(depth),
         year = 2017,
         fence = as.numeric(str_sub(`well#`, start = 1, end = 1)),
         block = ifelse(fence <= 2,
                        1,
                        ifelse(fence == 3 | fence == 4,
                               2,
                               3)),
         treatment = ifelse(str_sub(`well#`, start = 3, end = 3) <= 4,
                            'c',
                            'w')) %>%
  separate(depth, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  select(year, block, fence, `well#`, treatment, depth, depth0, depth1) %>%
  mutate(depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1)) %>%
  arrange(`well#`, depth)

# Join extra depths to the rest of 2017 data
soil2017_3 <- soil2017_2 %>%
  filter(score == 0) %>%
  select(year, block, fence, `well#`, treatment, depth, depth0, depth1, moisture, bulk.density) %>%
  rbind.data.frame(soil2017_depth_normal) %>%
  right_join(depths, by = c("year", "block", "fence", "well#", "treatment", "depth", "depth0", "depth1")) %>%
  select(-`well#`)


# Join summarized 09-13 data with 17 data
soil_09_17 <- soil_09_13 %>%
  select(-c(ash:cu.N.stock)) %>%
  rename(depth = depth.cat) %>%
  rbind.data.frame(soil2017_3) %>%
  group_by(year, block, fence, treatment, depth, depth0, depth1) %>%
  summarise(moisture = mean(moisture, na.rm = TRUE),
            bulk.density = mean(bulk.density, na.rm = TRUE)) %>%
  ungroup()

# prepare subsidence for joining with soil core data frame for graphing
soil_sub <- ALTsub.geoid %>%
  rename(ALT.geoid = ALT.corrected,
         subsidence.geoid = subsidence) %>%
  full_join(ALTsub.control, by = c('year', 'exp', 'block', 'fence', 'plot', 'treatment', 'ALT')) %>%
  rename(ALT.control = ALT.corrected,
         subsidence.control = subsidence) %>%
  filter(exp == 'CiPEHR') %>%
  mutate(treatment2 = ifelse(as.numeric(plot) <= 4,
                             'c',
                             'w')) %>%
  group_by(year, treatment2) %>%
  summarise(ALT = mean(ALT),
            ALT.control = mean(ALT.control),
            subsidence.control = mean(subsidence.control),
            ALT.geoid = mean(ALT.geoid),
            subsidence.geoid = mean(subsidence.geoid)) %>%
  rename(treatment = treatment2)
  

# take the mean of each year/treatment combination
soil_09_17_summary <- soil_09_17 %>%
  group_by(year, treatment, depth, depth0, depth1) %>%
  summarise(moisture = mean(moisture, na.rm = TRUE),
            bulk.density = mean(bulk.density, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year, treatment, depth0) %>%
  left_join(soil_sub, by = c('year', 'treatment')) %>%
  group_by(year, treatment, depth, moisture, bulk.density, ALT) %>%
  gather(key = depth.measurement,
         value = depth.layer,
         depth0:depth1) %>%
  mutate(depth.control = depth.layer-subsidence.control*100,
         depth.geoid = depth.layer-subsidence.geoid*100) %>%
  arrange(year, treatment, depth.layer, desc(depth.measurement)) %>%
  ungroup() %>%
  select(year, treatment, depth, depth.measurement, depth.layer, moisture, bulk.density, ALT, depth.control, ALT.control, subsidence.control, depth.geoid, ALT.geoid, subsidence.geoid)

# soil moisture average for control and warming
soil.moisture.avg <- soil_09_17_summary %>%
  summarise(mean(moisture, na.rm = TRUE)/1000)

# Depth profiles not corrected for subsidence
g1 <- ggplot(soil_09_17_summary, aes(x = moisture, y = -depth.layer, color = treatment)) +
  geom_path() +
  geom_hline(aes(yintercept = soil_09_17_summary$ALT, color = treatment)) +
  facet_grid(. ~ year) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Moisture Profiles not corrected for subsidence')

g2 <- ggplot(soil_09_17_summary, aes(x = bulk.density, y = -depth.layer, color = treatment)) +
  geom_path() +
  geom_hline(aes(yintercept = soil_09_17_summary$ALT, color = treatment)) +
  facet_grid(. ~ year) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Bulk Density Profiles not corrected for subsidence')


# Control Corrected Subsidence depth profiles
g3 <- ggplot(soil_09_17_summary, aes(x = moisture, y = -depth.control, color = treatment)) +
  geom_path() +
  geom_hline(aes(yintercept = soil_09_17_summary$ALT.control, color = treatment)) +
  facet_grid(. ~ year) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Moisture Profiles with control corrected subsidence')

g4 <- ggplot(soil_09_17_summary, aes(x = bulk.density, y = -depth.control, color = treatment)) +
  geom_path() +
  geom_hline(aes(yintercept = soil_09_17_summary$ALT.control, color = treatment)) +
  facet_grid(. ~ year) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Bulk Density Profiles with control corrected subsidence')

# Geoid Corrected Subsidence depth profiles
g5 <- ggplot(soil_09_17_summary, aes(x = moisture, y = -depth.geoid, color = treatment)) +
  geom_path() +
  geom_hline(aes(yintercept = soil_09_17_summary$ALT.geoid, color = treatment)) +
  facet_grid(. ~ year) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Moisture Profiles with Geoid corrected subsidence')

g6 <- ggplot(soil_09_17_summary, aes(x = bulk.density, y = -depth.geoid, color = treatment)) +
  geom_path() +
  geom_hline(aes(yintercept = soil_09_17_summary$ALT.geoid, color = treatment)) +
  facet_grid(. ~ year) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Bulk Density Profiles with Geoid corrected subsidence')

# ggsave(plot = g1, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Soil_Moisture_Profiles.jpg", height = 8, width = 10)
# ggsave(plot = g2, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Bulk_Density_Profiles.jpg", height = 8, width = 10)
# ggsave(plot = g3, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Soil_Moisture_Profiles_Control_Corrected.jpg", height = 8, width = 10)
# ggsave(plot = g4, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Bulk_Density_Profiles_Control_Corrected.jpg", height = 8, width = 10)
# ggsave(plot = g5, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Soil_Moisture_Profiles_Geoid_Corrected.jpg", height = 8, width = 10)
# ggsave(plot = g6, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Bulk_Density_Profiles_Geoid_Corrected.jpg", height = 8, width = 10)

# calculate total mass of cores
soil2009.subset <- rawsoil2009 %>%
  select(-bag, -Ht, -L, -W, -`foil/boat wt`, -`wet +foil`, -`dr+boat`) %>%
  filter(`sub-smp` == 'all' | `sub-smp` == 'CN/mstr') %>%
  mutate(wet = as.numeric(wet),
         ice_mass = wet - dry,
         ice_volume = ice_mass/0.9167,
         wet_dry_ratio = wet/dry) %>%
  group_by(Fence, plot, `well#`, depth) %>%
  mutate(wet_dry_ratio = max(wet_dry_ratio, na.rm = TRUE)) %>%
  filter(`sub-smp` == 'all') %>%
  separate(depth, c('depth0', 'depth1'), sep = '-') %>%
  ungroup() %>%
  mutate(dry = wet/wet_dry_ratio,
         plot = as.numeric(plot),
         treatment = ifelse(plot <= 4,
                            'c',
                            'w'),
         depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1)) %>%
  group_by(Fence, plot, treatment) %>%
  summarise(dry = sum(dry, na.rm = TRUE),
            depth = max(depth1)) %>%
  group_by(treatment) %>%
  summarise(dry = mean(dry, na.rm = TRUE),
            depth = mean(depth))

soil2017.subset <- rawsoil2017 %>%
  select(-bag, -Ht, -L, -W, -`foil/boat wt`, -`wet +foil`, -`dr+boat`) %>%
  filter(`sub-smp` == 'all' | `sub-smp` == 'CN/mstr') %>%
  mutate(ice_mass = wet - dry,
         ice_volume = ice_mass/0.9167,
         wet_dry_ratio = wet/dry) %>%
  filter(`well#` != '5-6.17' & depth != '55-65') %>%
  group_by(Fence, plot, `well#`, depth) %>%
  mutate(wet_dry_ratio = max(wet_dry_ratio, na.rm = TRUE)) %>%
  filter(`sub-smp` == 'all') %>%
  separate(depth, c('depth0', 'depth1'), sep = '-') %>%
  ungroup() %>%
  mutate(dry = wet/wet_dry_ratio,
         plot = as.numeric(plot),
         treatment = ifelse(plot <= 4,
                            'c',
                            'w'),
         depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1)) %>%
  group_by(Fence, plot, treatment) %>%
  summarise(dry = sum(dry, na.rm = TRUE),
            depth = max(depth1)) %>%
  group_by(treatment) %>%
  summarise(dry = mean(dry, na.rm = TRUE),
            depth = mean(depth))

