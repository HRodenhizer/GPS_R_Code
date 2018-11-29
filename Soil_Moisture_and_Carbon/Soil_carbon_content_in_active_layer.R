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
  left_join(ALTdata, by = c('year', 'treatment')) %>%
  rename(ALT.raw.2009 = ALT.raw, ALT.ratio.2009 = ALT.ratio, sub.ratio.2009 = sub.ratio, se.ALT.raw.2009 = se.ALT.raw, se.ALT.ratio.2009 = se.ALT.ratio, se.sub.2009 = se.sub) %>%
  left_join(subset(ALTdata, year == 2018), by = c('treatment')) %>%
  rename(year = year.x, ALT.raw.2018 = ALT.raw, ALT.ratio.2018 = ALT.ratio, sub.ratio.2018 = sub.ratio, se.ALT.raw.2018 = se.ALT.raw, se.ALT.ratio.2018 = se.ALT.ratio, se.sub.2018 = se.sub) %>%
  select(-year.y)

# calculate the difference in total c in active layer for 2009 and 2018
carbonchange <- carbon09 %>%
  group_by(treatment) %>%
  mutate(depth0 = depth0/100,
         depth1 = depth1/100,
         thickness = (depth1 - depth0),
         ALT.ratio.2009 = ALT.ratio.2009/100,
         ALT.ratio.2018 = ALT.ratio.2018/100,
         ALT.raw.2009 = ALT.raw.2009/100,
         ALT.raw.2018 = ALT.raw.2018/100,
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
         avail.c.09.raw = ifelse(ALT.raw.2009 > depth1,
                                   mean.C*mean.bd*thickness*10^4, # calculate gC/m^2 lost (mean.C (%) * bd (g/cm^3) * depth (cm) * 10^4 (cm^2/m^2))
                                   ifelse(ALT.raw.2009 > depth0 & ALT.raw.2009 < depth1,
                                          mean.C*mean.bd*(ALT.raw.2009-depth0)*10^4,
                                          NA)),
         avail.c.18.raw = ifelse(ALT.raw.2018 > depth1 & depth1 != 1,
                                   mean.C*mean.bd*thickness*10^4,
                                   ifelse(ALT.raw.2018 > depth0 & ALT.raw.2018 < depth1 | ALT.raw.2018 > depth1 & depth1 == 1,
                                          mean.C*mean.bd*(ALT.raw.2018-depth0)*10^4,
                                          NA)),
         Se.2009.ratio = sqrt((se.C/mean.C)^2 + (se.bd/mean.bd)^2 + (se.ALT.ratio.2009*thickness/ALT.ratio.2009^2)^2),
         Se.2018.ratio = sqrt((se.C/mean.C)^2 + (se.bd/mean.bd)^2 + (se.ALT.ratio.2018*thickness/ALT.ratio.2018^2)^2),
         Se.2009.raw = sqrt((se.C/mean.C)^2 + (se.bd/mean.bd)^2 + (se.ALT.raw.2009*thickness/ALT.raw.2009^2)^2),
         Se.2018.raw = sqrt((se.C/mean.C)^2 + (se.bd/mean.bd)^2 + (se.ALT.raw.2018*thickness/ALT.raw.2018^2)^2)) %>%
  summarise(totC.09.ratio = sum(avail.c.09.ratio, na.rm = TRUE),
            totC.18.ratio = sum(avail.c.18.ratio, na.rm = TRUE),
            totC.09.raw = sum(avail.c.09.raw, na.rm = TRUE),
            totC.18.raw = sum(avail.c.18.raw, na.rm = TRUE),
            Se.2009.ratio = sqrt(sum(Se.2009.ratio^2, na.rm = TRUE)),
            Se.2018.ratio = sqrt(sum(Se.2018.ratio^2, na.rm = TRUE)),
            Se.2009.raw = sqrt(sum(Se.2009.raw^2, na.rm = TRUE)),
            Se.2018.raw = sqrt(sum(Se.2018.raw^2, na.rm = TRUE))) %>%
  mutate(diff.ratio = totC.18.ratio-totC.09.ratio,
         diff.raw = totC.18.raw-totC.09.raw,
         se.ratio = sqrt(Se.2009.ratio^2 + Se.2018.ratio^2),
         se.raw = sqrt(Se.2009.raw^2 + Se.2018.raw^2))

avail_c <- carbonchange %>%
  gather(key = type, value = avail.c, diff.ratio:se.raw) %>%
  select(treatment, type, avail.c) %>%
  separate(type, into = c('measurement', 'sub.correction')) %>%
  spread(key = measurement, value = avail.c) %>%
  mutate(sub.correction = ifelse(sub.correction == 'raw',
                                 'Raw',
                                 'Subsidence Adjusted'))


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
