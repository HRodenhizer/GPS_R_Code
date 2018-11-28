####################################################################################################################################
###                               Soil cores to carbon content in active layer 2009-2013                                         ###
###                                                 HGR 7/2018                                                                   ###
####################################################################################################################################

# I need to select ALT in addition to ALT.corrected and carry that through to the end!
# load libraries
library(tidyverse)
library(readxl)
library(ggthemes)

# load data
soil_09_13 <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2009-2013 CiPEHR Processed SOIL DATA_César.xlsx",
                         sheet = 1) %>%
  separate(depth.cat, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  mutate(depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1))
ALTsub.geoid <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017_geoid_corrected.csv')
ALTsub.control <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017.csv')
ALTsub.ratio <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.csv')

# prep alt data - ALT.test contains the ALT corrected with the alt/sub corrected subsidence
ALTdata <- ALTsub.geoid %>%
  mutate(treatment = ifelse(treatment == 'Control' | treatment == 'Air Warming' | treatment == 'Drying',
                             'Control',
                             'Warming')) %>%
  group_by(year, treatment) %>%
  summarise(ALT.uncorrected = mean(ALT, na.rm = TRUE),
            ALT.mean = mean(ALT.corrected, na.rm = TRUE),
            sub.geoid = mean(subsidence, na.rm = TRUE)*100) %>%
  group_by(treatment) %>%
  mutate(ALT.geoid = ALT.mean*-1) %>%
  ungroup() %>%
  select(year, treatment, ALT.geoid)

# prep the ratio corrected alt data and join with the corrected alt data
ALTdata3 <- ALTsub.ratio %>%
  mutate(treatment2 = ifelse(treatment == 'Control' | treatment == 'Air Warming' | treatment == 'Drying',
                             'Control',
                             'Warming')) %>%
  group_by(year, treatment2) %>%
  summarise(ALT.mean = mean(ALT.corrected, na.rm = TRUE),
            sub.ratio = mean(subsidence, na.rm = TRUE)*100) %>%
  group_by(treatment2) %>%
  mutate(ALT.ratio = ALT.mean*-1) %>%
  ungroup() %>%
  select(year, treatment = treatment2, ALT.ratio, sub.ratio) %>%
  full_join(ALTdata, by = c('year', 'treatment'))

# prep the old alt data and join with the corrected alt data
ALTdata2 <- ALTsub.control %>%
  mutate(treatment2 = ifelse(treatment == 'Control' | treatment == 'Air Warming' | treatment == 'Drying',
                             'Control',
                             'Warming')) %>%
  group_by(year, treatment2) %>%
  summarise(ALT.mean = mean(ALT.corrected, na.rm = TRUE),
            sub.control = mean(subsidence, na.rm = TRUE)*100) %>%
  group_by(treatment2) %>%
  mutate(ALT.control = ALT.mean*-1) %>%
  ungroup() %>%
  select(year, treatment = treatment2, ALT.control) %>%
  full_join(ALTdata3, by = c('year', 'treatment'))


# select 2009 data, and calculate mean C content in percent (original C data from file in g/kg) and mean bulk density in g/cm3 by depth and treatment (control or warming)
# join to alt data, and fill in na values in soil carbon content at depth with closest measurement
carbon09 <- soil_09_13 %>%
  filter(year == 2009) %>%
  mutate(C = C/1000,
         treatment = ifelse(treatment == 'c',
                            'Control',
                            'Warming')) %>%
  group_by(year, treatment, depth.cat, depth0, depth1) %>%
  summarise(mean.C = mean(C, na.rm = TRUE),
            se.C = sd(C, na.rm = TRUE)/sqrt(n()),
            mean.bd = mean(bulk.density, na.rm = TRUE),
            se.bd = sd(bulk.density, na.rm = TRUE)/sqrt(n())) %>%
  arrange(treatment, depth0) %>%
  left_join(ALTdata2, by = c('year', 'treatment')) %>%
  rename(ALT.ratio.2009 = ALT.ratio, sub.ratio.2009 = sub.ratio) %>%
  select(-ALT.control, -ALT.geoid) %>%
  left_join(subset(ALTdata2, year == 2018), by = c('treatment')) %>%
  rename(year = year.x, ALT.ratio.2018 = ALT.ratio, sub.ratio.2018 = sub.ratio) %>%
  select(-year.y, -ALT.control, -ALT.geoid)

# calculate the difference in total c in active layer for 2009 and 2018
carbonchange <- carbon09 %>%
  group_by(treatment) %>%
  mutate(depth0 = depth0/100,
         depth1 = depth1/100,
         thickness = (depth1 - depth0),
         ALT.ratio.2009 = ALT.ratio.2009/100,
         ALT.ratio.2018 = ALT.ratio.2018/100,
         ALT.control.2009 = ALT.control.2009/100,
         ALT.control.2018 = ALT.control.2018/100,
         mean.C = ifelse(is.finite(mean.C),
                         mean.C,
                         nth(mean.C, 9)),
         mean.bd = ifelse(is.finite(mean.bd),
                         mean.C,
                         nth(mean.bd, 9)),
         avail.c.09.ratio = ifelse(ALT.ratio.2009 > depth1,
                                  mean.C*mean.bd*thickness*10^4, # calculate gC/m^2 lost (mean.C (%) * bd (g/cm^3) * depth (cm) * 10^4 (cm^2/m^2))
                                  ifelse(ALT.ratio.2009 > depth0 & ALT.ratio.2009 < depth1,
                                         mean.C*mean.bd*(ALT.ratio.2009-depth0)*10^4,
                                         NA)),
         avail.c.18.ratio = ifelse(ALT.ratio.2018 > depth1 & depth1 != 1,
                                  mean.C*mean.bd*thickness*10^4,
                                  ifelse(ALT.ratio.2018 > depth0 & ALT.ratio.2018 < depth1 | ALT.ratio.2018 > depth1 & depth1 == 1,
                                         mean.C*mean.bd*(ALT.ratio.2018-depth0)*10^4,
                                         NA)),
         avail.c.09.control = ifelse(ALT.control.2009 > depth1,
                                   mean.C*mean.bd*thickness*10^4, # calculate gC/m^2 lost (mean.C (%) * bd (g/cm^3) * depth (cm) * 10^4 (cm^2/m^2))
                                   ifelse(ALT.control.2009 > depth0 & ALT.control.2009 < depth1,
                                          mean.C*mean.bd*(ALT.control.2009-depth0)*10^4,
                                          NA)),
         avail.c.18.control = ifelse(ALT.control.2018 > depth1 & depth1 != 1,
                                   mean.C*mean.bd*thickness*10^4,
                                   ifelse(ALT.control.2018 > depth0 & ALT.control.2018 < depth1 | ALT.control.2018 > depth1 & depth1 == 1,
                                          mean.C*mean.bd*(ALT.control.2018-depth0)*10^4,
                                          NA)),
         Se.C.09.ratio = ifelse(!is.na(avail.c.09.ratio),
                          cumsum(se.C^2),
                          NA),
         Se.C.18.ratio = ifelse(!is.na(avail.c.18.ratio),
                         cumsum(se.C^2),
                         NA),
         Se.C.09.control = ifelse(!is.na(avail.c.09.control),
                                cumsum(se.C^2),
                                NA),
         Se.C.18.control = ifelse(!is.na(avail.c.18.control),
                                cumsum(se.C^2),
                                NA)) %>%
  summarise(tot.C.09.ratio = sum(avail.c.09.ratio, na.rm = TRUE),
            tot.C.18.ratio = sum(avail.c.18.ratio, na.rm = TRUE),
            tot.C.09.control = sum(avail.c.09.control, na.rm = TRUE),
            tot.C.18.control = sum(avail.c.18.control, na.rm = TRUE),
            Se.09.ratio = max(Se.C.09.ratio, na.rm = TRUE),
            Se.18.ratio = max(Se.C.18.ratio, na.rm = TRUE),
            Se.09.control = max(Se.C.09.control, na.rm = TRUE),
            Se.18.control = max(Se.C.18.control, na.rm = TRUE)) %>%
  mutate(diff.ratio = tot.C.18.ratio-tot.C.09.ratio,
         diff.control = )
  
# write.csv(carbonchange, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Carbon_Content_Change_2018.csv', row.names = FALSE)
carbonchange <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Carbon_Content_Change_2018.csv')

# plot
thawed.carbon <- ggplot(carbonchange, aes(x = treatment, y = diff)) +
  geom_col(fill = '#990000') +
  scale_x_discrete(name = '') +
  scale_y_continuous(name = expression(Thawed~Carbon~(gC/m^2))) +
  theme_few() +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x  = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

thawed.carbon

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Thawed_Carbon.jpg', thawed.carbon)
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Thawed_Carbon.pdf', thawed.carbon)
