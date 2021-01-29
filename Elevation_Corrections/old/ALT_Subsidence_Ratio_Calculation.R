############################################################################################################
###                           ALT Subsidence Ratio Calculation                                           ###
###                            Code written by HGR, May 2018                                             ###
############################################################################################################

################################### Load packages necessary for analysis ###################################
library(plyr)
library(tidyverse)
library(ggthemes)
library(raster)
############################################################################################################

################################# Load Data ################################################################
ALTsub.geoid <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017_geoid_corrected.csv')
ALTsub.control <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Subsidence_Corrected_2009_2017.csv')
Soil.moisture<- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Soil_Moisture_Subsidence_2009_2017_geoid_corrected.csv')
A2015Sub.raw <- raster()
############################################################################################################

ALTsub.geoid.summary <- ALTsub.geoid %>%
  mutate(treatment2 = ifelse(treatment == 'Control' | treatment == 'Air Warming',
                             'Control',
                             'Warming')) %>%
  group_by(year, treatment2) %>%
  summarise(ALT.uncorrected = mean(ALT),
            ALT.mean = mean(ALT.corrected),
            sub.mean = mean(subsidence)*100) %>%
  group_by(treatment2) %>%
  mutate(ALT.mean.positive = ALT.mean*-1,
         ALT.normalized = ALT.mean.positive-min(ALT.mean.positive),
         sub.ALT.ratio = sub.mean/ALT.mean*-1,
         sub.ALT.ratio.normalized = sub.mean/ALT.normalized,
         sub.test = ifelse(year == 2009 | year == 2011,
                           sub.mean,
                           ifelse(year == 2016 | year == 2017,
                                  sub.mean + 55,
                                  NA)),
         ALT.test = ifelse(year == 2009 | year == 2011,
                           ALT.mean.positive,
                           ifelse(year == 2016 | year == 2017,
                                  ALT.mean.positive - 55,
                                  NA))) %>%
  ungroup()

ALTsub.control.summary <- ALTsub.control %>%
  mutate(treatment2 = ifelse(treatment == 'Control' | treatment == 'Air Warming',
                             'Control',
                             'Warming')) %>%
  group_by(year, treatment2) %>%
  summarise(ALT.mean = mean(ALT.corrected),
            sub.mean = mean(subsidence)*100) %>%
  group_by(treatment2) %>%
  mutate(ALT.mean.positive = ALT.mean*-1,
         ALT.normalized = ALT.mean.positive-min(ALT.mean.positive),
         sub.ALT.ratio = sub.mean/ALT.mean*-1,
         sub.ALT.ratio.normalized = sub.mean/ALT.normalized)

Soil.moisture.summary <- Soil.moisture %>%
  select(treatment, ice_fraction) %>%
  mutate(treatment2 = ifelse(treatment == 'Control' | treatment == 'Air Warming',
                             'Control',
                             'Warming')) %>%
  group_by(treatment2) %>%
  summarise(ice.fraction.mean = mean(ice_fraction, na.rm = TRUE)) %>%
  mutate(slope = ice.fraction.mean*-0.0833)

#Linear models - relationship between subsidence and ALT
submodel <- function(df) {fit <- lm(sub.mean ~ ALT.normalized,  data=df)
return(cbind(
  reg.intercept = summary(fit)$coefficients[1],
  reg.slope = summary(fit)$coefficients[2],
  reg.r2 = summary(fit)$r.squared,
  reg.pvalue = anova(fit)$'Pr(>F)'[1]
))}

coefs_subsidence_geoid_normalized <- ddply(ALTsub.geoid.summary, .(treatment2), submodel)

coefs_subsidence_control_normalized <- ddply(ALTsub.control.summary, .(treatment2), submodel)

#Linear models - relationship between subsidence and ALT
submodel2 <- function(df) {fit <- lm(sub.mean ~ ALT.mean.positive,  data=df)
return(cbind(
  reg.intercept = summary(fit)$coefficients[1],
  reg.slope = summary(fit)$coefficients[2],
  reg.r2 = summary(fit)$r.squared,
  reg.pvalue = anova(fit)$'Pr(>F)'[1]
))}

coefs_subsidence_geoid <- ddply(ALTsub.geoid.summary, .(treatment2), submodel2)

coefs_subsidence_control <- ddply(ALTsub.control.summary, .(treatment2), submodel2)

#Linear model - relationship between subsidence and ALT assuming 12 cm subsidence in control by 2017
submodel3 <- function(df) {fit <- lm(sub.test ~ ALT.test,  data=df)
return(cbind(
  reg.intercept = summary(fit)$coefficients[1],
  reg.slope = summary(fit)$coefficients[2],
  reg.r2 = summary(fit)$r.squared,
  reg.pvalue = anova(fit)$'Pr(>F)'[1]
))}

coefs_subsidence_test <- ddply(ALTsub.geoid.summary, .(treatment2), submodel3)



# sub.alt ratio through time (alt is not normalized, subsidence is with geoid corrected data)
g1 <- ggplot(ALTsub.geoid.summary, aes(x = year, y = sub.ALT.ratio, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle("Observed geoid corrected subsidence for each unit of ALT")

# sub.alt ratio through time (alt is normalized, subsidence is with geoid corrected data)
g1.5 <- ggplot(ALTsub.geoid.summary, aes(x = year, y = sub.ALT.ratio.normalized, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle("Observed geoid corrected subsidence for each unit of ALT")

# sub.alt by alt (alt is normalized, subsidence is geoid corrected)
ggplot(ALTsub.geoid.summary, aes(x = ALT.normalized, y = sub.ALT.ratio.normalized, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle("Observed geoid corrected subsidence for each unit of ALT")

# subsidence by alt (alt is not normalized, subsidence is geoid corrected)
g2 <- ggplot(ALTsub.geoid.summary, aes(x = ALT.mean.positive, y = sub.mean, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  geom_abline(slope = -0.0483, intercept = 0) +
  annotate("text", x = 125, y = -12.5, label = "^ Expected slope of relationship given 58% ice") +
  ggtitle("Relationship between geoid corrected subsidence and ALT")

# subsidence by alt (alt is not normalized, subsidence is geoid corrected)
gsub.test <- ggplot(ALTsub.geoid.summary, aes(x = ALT.test, y = sub.test, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  geom_abline(slope = -0.0483, intercept = 0) +
  annotate("text", x = 100, y = -7, label = "^ Expected slope of relationship given 58% ice") +
  ggtitle("8 cm subsidence in control plots gives equivalent Sub/ALT ratio in C and W")

# subsidence by alt (alt is normalized, subsidence is geoid corrected)
g2.5 <- ggplot(ALTsub.geoid.summary, aes(x = ALT.normalized, y = sub.mean, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  geom_abline(slope = -0.0415, intercept = 0) +
  annotate("text", x = 75, y = -12.5, label = "^ Expected slope of relationship given ~50% ice") +
  ggtitle("Relationship between geoid corrected subsidence and ALT")

# subsidence by alt (alt is normalized, subsidence is geoid corrected, no gap-filled data included)
g2.75 <- ggplot(subset(ALTsub.geoid.summary, year == 2009 | year == 2011 | year == 2016 | year == 2017), aes(x = ALT.normalized, y = sub.mean, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  geom_abline(slope = -0.0415, intercept = 0) +
  annotate("text", x = 75, y = -12.5, label = "^ Expected slope of relationship given ~50% ice") +
  geom_smooth(method = lm) +
  ggtitle("Relationship between geoid corrected subsidence and ALT")

# sub.alt ratio through time (alt is not normalized, subsidence is control corrected)
g3 <- ggplot(ALTsub.control.summary, aes(x = year, y = sub.ALT.ratio, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle("Observed control corrected subsidence for each unit of ALT")

# sub.alt ratio through time (alt is normalized, subsidence is control corrected)
g3.5 <- ggplot(ALTsub.control.summary, aes(x = year, y = sub.ALT.ratio.normalized, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle("Observed control corrected subsidence for each unit of ALT")

# sub.alt ratio by alt (alt is normalized, subsidence is control corrected)
ggplot(ALTsub.control.summary, aes(x = ALT.normalized, y = sub.ALT.ratio.normalized, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle("Observed control corrected subsidence for each unit of ALT")

# subsidence by alt (alt is not normalized, subsidence is control corrected)
g4 <- ggplot(ALTsub.control.summary, aes(x = ALT.mean.positive, y = sub.mean, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  geom_abline(slope = -0.0483, intercept = 0) +
  annotate("text", x = 100, y = -7.5, label = "^ Expected slope of relationship given 58% ice") +
  ggtitle("Relationship between control corrected subsidence and ALT")

# subsidence by alt (alt is normalized, subsidence is control corrected)
g4.5 <- ggplot(ALTsub.control.summary, aes(x = ALT.normalized, y = sub.mean, color = treatment2)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  geom_abline(slope = -0.0415, intercept = 0) +
  annotate("text", x = 100, y = -7.5, label = "^ Expected slope of relationship given ~50% ice") +
  ggtitle("Relationship between control corrected subsidence and ALT")

# profile of subsidence and alt through time
g5 <- ggplot(ALTsub.geoid.summary, aes(x = year)) +
  facet_grid(. ~ treatment2) +
  geom_ribbon(aes(ymin = -200, ymax = ALT.mean), fill = "#3399CC") +
  geom_ribbon(aes(ymin = ALT.mean, ymax = sub.mean), fill = "#996633") +
  geom_path(aes(y = sub.mean), size = 1.5) +
  geom_path(aes(y = ALT.mean), size = 1.5) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016),
                     limits = c(2009, 2017),
                     expand = c(0,0),
                     name = '') +
  scale_y_continuous(limits = c(-200, 10),
                     expand = c(0,0),
                     name = 'Depth (cm)') +
  annotate("text", x = 2009.5, y = -175, label = "Permafrost", hjust = 0) +
  annotate("text", x = 2009.5, y = -30, label = "Active Layer", hjust = 0) +
  annotate("text", x = 2016.5, y = -30, label = "Subsidence", hjust = 1) +
  theme_few() +
  theme(strip.text.x = element_text(size = 12))


# ggplot(ALTsub.geoid.summary, aes(x = year)) +
#   facet_grid(. ~ treatment2) +
#   geom_ribbon(aes(ymin = -200, ymax = ALT.mean), fill = "blue") +
#   geom_ribbon(aes(ymin = ALT.mean, ymax = sub.mean), fill = "brown") +
#   geom_path(aes(y = sub.mean), size = 1.5) +
#   geom_path(aes(y = ALT.mean), size = 1.5) +
#   scale_x_continuous(breaks = c(2010, 2012, 2014, 2016),
#                      limits = c(2009, 2017),
#                      expand = c(0,0)) +
#   scale_y_continuous(limits = c(-200, 5),
#                      expand = c(0,0)) +
#   theme_few()

# ggsave(plot = g1, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_ALT_Ratio_Geoid.jpg", height = 8, width = 10)
# ggsave(plot = g2, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_ALT_Geoid.jpg", height = 8, width = 10)
# ggsave(plot = g3, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_ALT_Ratio_Control.jpg", height = 8, width = 10)
# ggsave(plot = g4, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_ALT_Control.jpg", height = 8, width = 10)
# ggsave(plot = g1.5, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_nALT_Ratio_Geoid.jpg", height = 8, width = 10)
# ggsave(plot = g2.5, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_nALT_Geoid.jpg", height = 8, width = 10)
# ggsave(plot = g3.5, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_nALT_Ratio_Control.jpg", height = 8, width = 10)
# ggsave(plot = g4.5, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_nALT_Control.jpg", height = 8, width = 10)
# ggsave(plot = g5, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Sub_ALT_Profile_Geoid.jpg", height = 8, width = 10)
# ggsave(plot = gsub.test, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoid Correction Verification Figures/Sub_ALT_Ratio_test_correction.jpg", height = 8, width = 10)
