####################################################################################################################################
###                               Soil cores to carbon content in active layer 2009-2013                                         ###
###                                                 HGR 7/2018                                                                   ###
####################################################################################################################################

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
ALTsub.ratio <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.csv')

# prep the ratio corrected alt data and join with the corrected alt data
ALTdata <- ALTsub.ratio %>%
  mutate(treatment2 = ifelse(treatment == 'Control' | treatment == 'Air Warming' | treatment == 'Drying',
                             'Control',
                             'Warming')) %>%
  group_by(year, treatment2) %>%
  summarise(ALT.raw = mean(ALT, na.rm = TRUE),
            ALT.ratio = mean(ALT.corrected, na.rm = TRUE),
            sub.ratio = mean(subsidence, na.rm = TRUE)*100,
            se.ALT.raw = sd(ALT, na.rm = TRUE)/sqrt(n()),
            se.ALT.ratio = sd(ALT.corrected, na.rm = TRUE)/sqrt(n()),
            se.sub = sd(subsidence, na.rm = TRUE)/sqrt(n())*100) %>%
  group_by(treatment2) %>%
  mutate(ALT.raw = ALT.raw*-1,
         ALT.ratio = ALT.ratio*-1) %>%
  ungroup() %>%
  select(year, treatment = treatment2, ALT.raw, ALT.ratio, sub.ratio, se.ALT.raw, se.ALT.ratio, se.sub)

carbon09 <- soil_09_13 %>%
  filter(year == 2009) %>%
  mutate(C = C/1000, # convert from g/kg to %
         treatment = ifelse(treatment == 'c',
                            'Control',
                            'Warming')) %>%
  group_by(year, treatment, depth.cat, depth0, depth1) %>%
  summarise(mean.C = mean(C, na.rm = TRUE),
            se.C = sd(C, na.rm = TRUE)/sqrt(n()),
            mean.bd = mean(bulk.density, na.rm = TRUE),
            se.bd = sd(bulk.density, na.rm = TRUE)/sqrt(n())) %>%
  ungroup() %>%
  arrange(treatment, depth0) %>%
  select(-year) %>%
  full_join(ALTdata, by = c('treatment')) %>%
  arrange(year, treatment)

# calculate the difference in total c in active layer for 2009 and 2018
thawedc <- carbon09 %>%
  filter(!is.na(mean.C)) %>%
  group_by(year, treatment) %>%
  mutate(depth0 = depth0/100,
         depth1 = ifelse(depth1 != 85,
                         depth1/100,
                         1.50),
         thickness = (depth1 - depth0),
         ALT.ratio = ALT.ratio/100,
         ALT.raw = ALT.raw/100,
         avail.c.raw = ifelse(ALT.raw < depth0,
                              NA,
                              ifelse(ALT.raw > depth1,
                                     mean.C*mean.bd*thickness*10^3, # calculate kgC/m^2 thawed (mean.C (%) * bd (g/cm^3) * depth (m) * 10^6 (cm^3/m^3) * 1/1000 (kg/g))
                                     mean.C*mean.bd*(ALT.raw-depth0)*10^3)),
         avail.c.ratio = ifelse(ALT.ratio < depth0,
                                NA,
                                ifelse(ALT.ratio > depth1,
                                       mean.C*mean.bd*thickness*10^3, # calculate kgC/m^2 thawed (mean.C (%) * bd (g/cm^3) * depth (m) * 10^6 (cm^3/m^3) * 1/1000 (kg/g))
                                       mean.C*mean.bd*(ALT.ratio-depth0)*10^3)),
         Se.raw = ifelse(ALT.raw < depth0,
                         NA,
                         ifelse(ALT.raw > depth1,
                                sqrt((se.C/mean.C)^2 + (se.bd/mean.bd)^2 + (se.ALT.raw*thickness/ALT.raw^2)^2),
                                sqrt((se.C/mean.C)^2 + (se.bd/mean.bd)^2 + (se.ALT.raw*(ALT.raw-depth0)/ALT.raw^2)^2))),
         Se.ratio = ifelse(ALT.ratio < depth0,
                           NA,
                           ifelse(ALT.ratio > depth1,
                                  sqrt((se.C/mean.C)^2 + (se.bd/mean.bd)^2 + (se.ALT.ratio*thickness/ALT.ratio^2)^2),
                                  sqrt((se.C/mean.C)^2 + (se.bd/mean.bd)^2 + (se.ALT.ratio*(ALT.ratio-depth0)/ALT.ratio^2)^2)))) %>%
  summarise(totC.raw = sum(avail.c.raw, na.rm = TRUE),
            totC.ratio = sum(avail.c.ratio, na.rm = TRUE),
            Se.raw = sqrt(sum(Se.raw^2, na.rm = TRUE)),
            Se.ratio = sqrt(sum(Se.ratio^2, na.rm = TRUE)))

carbonchange <- data.frame(treatment = c('Control', 'Warming'),
                           totC.09.ratio = c(thawedc$totC.ratio[1], thawedc$totC.ratio[2]),
                           totC.18.ratio = c(thawedc$totC.ratio[19], thawedc$totC.ratio[20]),
                           totC.09.raw = c(thawedc$totC.raw[1], thawedc$totC.raw[2]),
                           totC.18.raw = c(thawedc$totC.raw[19], thawedc$totC.raw[20]),
                           Se.09.ratio = c(thawedc$Se.ratio[1], thawedc$Se.ratio[2]),
                           Se.18.ratio = c(thawedc$Se.raw[1], thawedc$Se.raw[2]),
                           Se.09.raw = c(thawedc$Se.ratio[19], thawedc$Se.ratio[20]),
                           Se.18.raw = c(thawedc$Se.raw[19], thawedc$Se.raw[20])) %>%
  mutate(diff.ratio = totC.18.ratio - totC.09.ratio,
         diff.raw = totC.18.raw - totC.09.raw,
         se.ratio = sqrt(totC.18.ratio^2 + totC.09.ratio^2),
         se.raw = sqrt(totC.18.raw^2 + totC.09.raw^2))
  

avail_c <- carbonchange %>%
  gather(key = type, value = avail.c, diff.ratio:se.raw) %>%
  select(treatment, type, avail.c) %>%
  separate(type, into = c('measurement', 'sub.correction')) %>%
  spread(key = measurement, value = avail.c) %>%
  mutate(sub.correction = ifelse(sub.correction == 'raw',
                                 'Raw',
                                 'Subsidence Adjusted'))

# write.csv(thawedc, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Thawed_Carbon_w_sub.csv', row.names = FALSE)
# write.csv(carbonchange, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Carbon_Content_Change_2018.csv', row.names = FALSE)
carbonchange <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Carbon_Content_Change_2018.csv')

# plot
thawed.carbon <- ggplot(avail_c, aes(x = treatment, y = diff, colour = sub.correction)) +
  geom_errorbar(aes(ymin = diff - se, ymax = diff + se), width = 0.1) +
  geom_point() +
  scale_x_discrete(name = '') +
  scale_color_manual(name = '',
                     values = c("#ff0000", "#000000"),
                     labels = c('Thawed C', 'Subsidence Adjusted\nThawed C')) +
  scale_y_continuous(name = expression(Thawed~Carbon~(gC/m^2)),
                     limits = c(0, 450)) +
  ggtitle('Thawed Carbon After 10 Years of Warming') +
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

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Thawed_Carbon.jpg', thawed.carbon, height = 5, width = 7)
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Thawed_Carbon.pdf', thawed.carbon)
