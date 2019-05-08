############################################################################################################
###             ALT Subsidence Correction & Plot Level Mixed Effects Model                               ###
###                            Code written by HGR, Sep 2018                                             ###
############################################################################################################

################################### Load packages necessary for analysis - run every time ##################
library(sf)
library(readxl)
library(raster)
library(ggthemes)
library(data.table)
library(lubridate)
library(lme4)
library(ggfortify)
library(MuMIn)
library(merTools)
library(tidyverse)
############################################################################################################

### Load data ##############################################################################################
# CiPEHR - Stack the subsidence rasters which are calculated from 2009
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks', full.names = TRUE)
CiPEHR_brick <- list(brick(filenames[1]), 
                     brick(filenames[3]), 
                     brick(filenames[5]))
# DryPEHR - Stack the subsidence rasters which are calculated from 2011
DryPEHR_brick <- list(brick(filenames[2]), 
                      brick(filenames[4]), 
                      brick(filenames[6]))
# 2017 shapefile for location of plots
plotcoords <- read_sf("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/plot_coordinates_from_2017.shp") %>%
  mutate(block = ifelse(fence == 1 | fence == 2,
                        1,
                        ifelse(fence == 3 | fence == 4,
                               2,
                               3)))

plotcoords.drypehra <- plotcoords %>%
  filter(plot == 2) %>%
  mutate(plot = 'a',
         exp = 'DryPEHR')

plotcoords <- plotcoords %>%
  rbind.data.frame(plotcoords.drypehra) %>%
  arrange(exp, fence, plot)
# Load Thaw Data
ALTdata <- read_excel("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Cumulative CiPHER DryPEHR thaw depth new grouping.xlsx",
                      col_types = c('guess', 'guess', 'guess', 'text', 'guess', 'guess', 'guess', 'guess', 'guess', 'guess', 'guess'))

# Load Water Table Depth
filenames <- list.files("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/WTD", full.names = TRUE, pattern = '\\.csv$')
lst <- lapply(filenames, read.csv)
WTD <- rbindlist(lst, fill=T)
############################################################################################################

######## Extract subsidence values from rasters using plot locations from plotcoords data frames and format to join with plotcoords again ########
### CiPEHR
CiPEHR_extract <- data.frame()
for (i in 1: length(CiPEHR_brick)) {
  plotcoords.temp <- plotcoords %>%
    filter(block == i & exp == 'CiPEHR')
  CiPEHR_extract_temp <- raster::extract(CiPEHR_brick[[i]], plotcoords.temp, layer = 1, nl = nlayers(CiPEHR_brick[[i]]), df = TRUE) %>%
    dplyr::select(sub2010 = 2,
                  sub2011 = 3,
                  sub2012 = 4,
                  sub2013 = 5,
                  sub2014 = 6,
                  sub2015 = 7,
                  sub2016 = 8,
                  sub2017 = 9,
                  sub2018 = 10,
                  -ID) %>%
    mutate(sub2009 = 0) %>%
    cbind.data.frame(plotcoords.temp) %>%
    gather(key = year, value = subsidence, sub2010:sub2009) %>%
    mutate(year = as.numeric(str_sub(year, 4, 7)),
           time = year - 2009,
           treatment = ifelse(plot == 1 | plot == 3,
                              'Air Warming',
                              ifelse(plot == 2 | plot == 4,
                                     'Control',
                                     ifelse(plot == 5 | plot == 7,
                                            'Air + Soil Warming',
                                            'Soil Warming'))))
  CiPEHR_extract <- rbind.data.frame(CiPEHR_extract, CiPEHR_extract_temp)
  rm(i, plotcoords.temp, CiPEHR_extract_temp)
}

### DryPEHR
DryPEHR_extract <- data.frame()
for (i in 1: length(DryPEHR_brick)) {
  plotcoords.temp <- plotcoords %>%
    filter(block == i & exp == 'DryPEHR')
  DryPEHR_extract_temp <- raster::extract(DryPEHR_brick[[i]], plotcoords.temp, layer = 1, nl = nlayers(DryPEHR_brick[[i]]), df = TRUE) %>%
    dplyr::select(sub2012 = 2,
                  sub2013 = 3,
                  sub2014 = 4,
                  sub2015 = 5,
                  sub2016 = 6,
                  sub2017 = 7,
                  sub2018 = 8,
                  -ID) %>%
    mutate(sub2011 = 0) %>%
    cbind.data.frame(plotcoords.temp) %>%
    gather(key = year, value = subsidence, sub2012:sub2011) %>%
    mutate(year = as.numeric(str_sub(year, 4, 7)),
           time = year - 2011,
           treatment = ifelse(plot == 'a',
                              'Control',
                              ifelse(plot == 'b',
                                     'Drying',
                                     ifelse(plot == 'c',
                                            'Warming',
                                            'Drying + Warming'))))
  DryPEHR_extract <- rbind.data.frame(DryPEHR_extract, DryPEHR_extract_temp)
  rm(i, plotcoords.temp, DryPEHR_extract_temp)
}

# join CiPEHR and DryPEHR dataframes
Sub_extract <- CiPEHR_extract %>%
  rbind.data.frame(DryPEHR_extract) %>%
  arrange(year, exp, fence, plot) %>%
  st_as_sf() %>%
  group_by(fence, plot) %>%
  mutate(block = ifelse(block == 1,
                        'a',
                        ifelse(block == 2,
                               'b',
                               'c'))) %>%
  dplyr::select(year, exp, block, fence, plot, treatment, subsidence, time)

# write.table(Sub_extract, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/Subsidence_2009_2018_Ratio_Corrected.txt', sep = '\t', row.names = FALSE)

############################################################################################################
  
######################### Prep ALT data and join with sub data #############################################
# Duplicate plot 2 for drypehr a
ALTdata.dry <- ALTdata %>%
  filter(plot == 2) %>%
  mutate(plot = 'a')

# Select ALT from thaw data
ALTdata2 <- ALTdata %>%
  rbind.data.frame(ALTdata.dry) %>%
  mutate(year = year(date(date)),
         doy = yday(date(date)),
         week = floor(doy/7),
         exp = ifelse(plot == 'a' | plot == 'b' | plot == 'c' | plot == 'd',
                      'DryPEHR',
                      'CiPEHR'),
         treatment = ifelse(plot == 1 | plot == 3,
                            'Air Warming',
                            ifelse(plot == 2 | plot == 4,
                                   'Control',
                                   ifelse(plot == 5 | plot == 7,
                                          'Air + Soil Warming',
                                          ifelse(plot == 6 | plot == 8,
                                                 'Soil Warming',
                                                 ifelse(plot == 'a',
                                                        'Control',
                                                        ifelse(plot == 'b',
                                                               'Drying',
                                                               ifelse(plot == 'c',
                                                                      'Warming',
                                                                      'Drying + Warming')))))))) %>%
  filter(week == 36) %>%
  select(year, exp, block, fence, plot, treatment, ALT = td)
         
## Somehow 2016 DryPEHR data are getting duplicated - fix me!
ALTsub <- Sub_extract %>%
  left_join(ALTdata2, by = c("year", "exp", "block", "fence", "plot", "treatment")) %>%
  mutate(ALT = ALT*-1,
         ALT.corrected = ALT+subsidence*100) %>%
  dplyr::select(year, exp, block, fence, plot, treatment, subsidence, ALT, ALT.corrected) %>%
  arrange(year, exp, block, fence, plot)

# Write file
# write.table(ALTsub, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.txt', row.names = FALSE, sep = '\t')
############################################################################################################

################# Format WTD - only needs to be done once ##################################################
WTD2 <- WTD %>%
  mutate(exp = ifelse(is.na(Dry),
                      'CiPEHR',
                      'DryPEHR'),
         date = parse_date_time(Date, orders = c('m!*/d!/Y!', 'Y!/m!*/d!')),
         year = year(date),
         block = toupper(Block),
         WW = ifelse(toupper(WW) == 'W',
                     'WW',
                     toupper(WW)),
         treatment = ifelse(WW == 'C' & is.na(Dry) | WW == 'C' & Dry == 'c',
                            'Control',
                            ifelse(WW == 'WW' & is.na(Dry) | WW == 'WW' & Dry == 'c',
                                   'Warming',
                                   ifelse(WW == 'C' & Dry == 'd',
                                          'Drying',
                                          'Drying + Warming'))),
         WTD = as.numeric(as.character(WTD))) %>%
  dplyr::select(date, year, exp, block, fence = Fence, well = Well, treatment, WTD, TD)

# write.csv(WTD2, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/WTD/Compiled/WTD_2018_compiled.csv", row.names = FALSE)
############################################################################################################

##################### Load and Prep data for mixed effects model and graphing ##############################
ALTsub <- read.table('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.txt', sep = '\t', header = TRUE)
subpoints <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/Subsidence_2009_2018_Ratio_Corrected.csv') %>%
  st_as_sf(coords = c('X', 'Y'))
WTD <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/WTD/Compiled/WTD_2018_compiled.csv") %>%
  mutate(treatment = ifelse(treatment == 'Drying',
                            'Control',
                            ifelse(treatment == 'Drying + Warming',
                                   'Warming',
                                   as.character(treatment))))
thawedc <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Thawed_Carbon_w_sub.csv')

### Create a data frame of subsidence points to only reflect winter warming for mixed effects models and graphing subsidence
subpoints2 <- subpoints %>%
  mutate(plot = factor(plot, levels = c('1', '2', '3', '4','a', 'b', '5', '6', '7', '8', 'c', 'd')),
         treatment = ifelse(treatment == 'Air Warming' | treatment == 'Control' | treatment == 'Drying',
                            'Control',
                            'Warming'))

### Create a data frame to graph corrected and uncorrected ALT values on top of each other by plot
ALTsubgraph <- ALTsub %>%
  gather(key = Measurement_Type, value = ALT, ALT:ALT.corrected) %>%
  arrange(desc(Measurement_Type), year, exp, block, fence, plot) %>%
  mutate(plot = factor(plot, levels = c('1', '2', '3', '4','a', 'b', '5', '6', '7', '8', 'c', 'd')))

### Create a data frame with WTD, corrected, and uncorrected ALT for each fence and treatment per year
# join WTD and sub by fence and treatment
WTD_fence <- WTD %>%
  group_by(year, block, fence, treatment) %>%
  summarise(mean.WTD = mean(WTD, na.rm = TRUE)*-1,
            se.WTD = sd(WTD, na.rm = TRUE)/sqrt(n())) %>%
  ungroup() %>%
  mutate(block = tolower(block))

ALTsub_fence <- ALTsub %>%
  mutate(treatment = ifelse(treatment == 'Air Warming' | treatment == 'Control' | treatment == 'Drying',
                            'Control',
                            'Warming'),
         subsidence = subsidence*100) %>%
  group_by(year, block, fence, treatment) %>%
  summarise(mean.subsidence = mean(subsidence, na.rm = TRUE),
            mean.ALT = mean(ALT.corrected, na.rm = TRUE),
            se.subsidence = sd(subsidence, na.rm = TRUE)/sqrt(n()),
            se.ALT = sd(ALT.corrected, na.rm = TRUE)/sqrt(n())) %>%
  full_join(WTD_fence, by = c('year', 'block', 'fence', 'treatment'))

### Create a summarized data frame for graphing differences in corrected and uncorrected ALT between control and warming
ALTsubgraph2 <- ALTsubgraph %>%
  mutate(treatment = ifelse(treatment == 'Air Warming' | treatment == 'Control' | treatment == 'Drying',
                            'Control',
                            'Warming')) %>%
  group_by(year, treatment, Measurement_Type) %>%
  summarise(mean.subsidence = mean(subsidence, na.rm = TRUE)*100,
            mean.ALT = mean(ALT, na.rm = TRUE),
            se.subsidence = sd(subsidence, na.rm = TRUE)/sqrt(n()),
            se.ALT = sd(ALT, na.rm = TRUE)/sqrt(n())) %>%
  arrange(year, treatment, desc(Measurement_Type)) %>%
  mutate(sub.correction = ifelse(Measurement_Type == 'ALT',
                                 'Raw',
                                 'Subsidence Adjusted'))

### Create a data frame for graphing soil profiles with WTD
# Create a WTD data frame with averages for each treatment per year
WTD2 <- WTD %>%
  group_by(year, treatment) %>%
  summarise(mean.WTD = mean(WTD, na.rm = TRUE)*-1,
            se.WTD = sd(WTD, na.rm = TRUE)/sqrt(n()))

# a summarized dataframe with WTD as well to make the soil profile figures
ALTsub.summary <- ALTsub %>%
  mutate(treatment = ifelse(treatment == 'Air Warming' | treatment == 'Control' | treatment == 'Drying',
                            'Control',
                            'Warming')) %>%
  group_by(year, treatment) %>%
  summarise(mean.subsidence = mean(subsidence, na.rm = TRUE)*100,
            se.subsidence = sd(subsidence, na.rm = TRUE)/sqrt(n()),
            mean.ALT.corrected = mean(ALT.corrected, na.rm = TRUE),
            se.ALT.corrected = sd(ALT.corrected, na.rm = TRUE)/sqrt(n()),
            mean.ALT = mean(ALT, na.rm = TRUE),
            se.ALT = sd(ALT, na.rm = TRUE)/sqrt(n())) %>%
  mutate(percent.change = (mean.ALT.corrected/mean.ALT - 1)*100,
         se.percent.change = sqrt( (se.ALT.corrected/mean.ALT.corrected)^2 + (se.ALT/mean.ALT)^2 )*100) %>%
  full_join(WTD2, by = c('year', 'treatment'))

### Create a data frame for graphing the carbon change data  with ALT data
avail_c <- thawedc %>%
  gather(key = type, value = avail.c, totC.raw:Se.ratio) %>%
  separate(type, into = c('measurement', 'sub.correction')) %>%
  spread(key = measurement, value = avail.c) %>%
  mutate(sub.correction = ifelse(sub.correction == 'raw',
                                 'Raw',
                                 'Subsidence Adjusted'))

diff <- thawedc %>%
  mutate(diff = totC.ratio - totC.raw,
         Se.diff = sqrt(Se.ratio^2 + Se.raw^2))
############################################################################################################

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

############################## Mixed Effects Models ########################################################
subpointsC <- subpoints2 %>%
  cbind(subpoints$treatment) %>%
  rename(full.treatment = subpoints.treatment) %>%
  filter(exp == 'CiPEHR') %>%
  ungroup() %>%
  mutate(block2 = as.factor(ifelse(block == 'A',
                                   1,
                                   ifelse(block == 'B',
                                          2,
                                          3))),
         fence2 = as.factor(ifelse(fence == 1 | fence == 3 | fence == 5,
                                   1,
                                   2)),
         treatment2 = as.factor(ifelse(treatment == 'Control',
                                       1,
                                       2)),
         treatment3 = as.factor(ifelse(full.treatment == 'Control',
                                       1,
                                       ifelse(full.treatment == 'Air Warming',
                                              2,
                                              ifelse(full.treatment == 'Soil Warming',
                                                     3,
                                                     4)))),
         fencegroup = factor(block2:fence2),
         wholeplot = factor(block2:fence2:treatment2))

## Run the model with a different slope for each fence/treatment combination
# model2_ci <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Coefficients_Mixed_Effects.csv')


# ## Model with time and soil treatment, but no interaction
# model1 <- lmer(subsidence ~ 0 + time + treatment2 + 
#                  (1 | block2/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = subpointsC,
#                control=lmerControl(check.conv.singular="warning"))
# 
# summary(model1)
# 
# ## Model with time and all treatments, but no interaction
# model2 <- lmer(subsidence ~ 0 + time + treatment3 + 
#                  (1 | block2/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = subpointsC,
#                control=lmerControl(check.conv.singular="warning"))
# 
# summary(model2)
# 
# ## Model with different slopes for control and warming
# model3 <- lmer(subsidence ~ 0 + time + time*treatment2  + 
#                  (1 | block2/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = subpointsC,
#                control=lmerControl(check.conv.singular="warning"))
# 
# summary(model3)
# 
# 
# AICc(model3, model2, model1) # model 3 is best, and no additional variables to add since adding warming didn't improve model

## Model with time 
model1 <- lmer(subsidence ~ 0 + time +
                 (1 | block2/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = subpointsC,
               control=lmerControl(check.conv.singular="warning"))

summary(model1)

## Model with time and interaction between time and soil warming
model2 <- lmer(subsidence ~ 0 + time + time:treatment2 +
                 (1 | block2/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = subpointsC,
               control=lmerControl(check.conv.singular="warning"))

summary(model2)

## Model with time and interaction between time and all treatments
model3 <- lmer(subsidence ~ 0 + time + time:treatment3 +
                 (1 | block2/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = subpointsC,
               control=lmerControl(check.conv.singular="warning"))

summary(model3)

AICc(model3, model2, model1)


# check model residuals of model2
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

hist(subpointsC$subsidence)

# rerun best model with REML = TRUE
subsidence_model <- lmer(subsidence ~ 0 + time + time:treatment3  + 
                 (1 | block2/fencegroup/wholeplot) + (1|time), REML = TRUE,
               data = subpointsC,
               control=lmerControl(check.conv.singular="warning"))

summary(subsidence_model)
r.squaredGLMM(subsidence_model)

# save model
# saveRDS(subsidence_model, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/subsidence_model_nointercept.rds")

subsidence_model <- readRDS("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/subsidence_model_nointercept.rds")

# calculate confidence intervals to look at fixed effects
subsidence_model_ci <- extract_ci(subsidence_model)
# write.csv(subsidence_model_ci, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Coefficients_Mixed_Effects_nointercept.csv', row.names = FALSE)
subsidence_model_ci <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Coefficients_Mixed_Effects_nointercept.csv')

predInt <- predictInterval(subsidence_model, newdata = subpointsC, n.sims = 1000,
                           returnSims = TRUE, level = 0.95, ignore.fixed.terms = c(intercept, treatment3))

subpoints.fit <- subpointsC %>%
  cbind.data.frame(predInt) %>%
  dplyr::select(-geometry)
# write.csv(subpoints.fit, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Fit_2018.csv', row.names = FALSE)

############################################################################################################

################################### Graphs #################################################################
# data needed for graphs
model2 <- readRDS("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/subsidence_model.rds")
subpoints.fit <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Fit_2018.csv')
model2_ci <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Coefficients_Mixed_Effects.csv')
subpoints.ci <- data.frame(time = rep(seq(0, 9, length.out = 2), 2),
                           treatment = c(rep('Control', 2), rep('Warming', 2))) %>%
  mutate(lwr = ifelse(treatment == 'Control',
                      model2_ci$min[1] + model2_ci$min[2]*time,
                      model2_ci$min[1] + model2_ci$min[4] + (model2_ci$min[2] + model2_ci$min[3])*time),
         upr = ifelse(treatment == 'Control',
                      model2_ci$max[1] + model2_ci$max[2]*time,
                      model2_ci$max[1] + model2_ci$max[4] + (model2_ci$max[2] + model2_ci$max[3])*time),
         fit = ifelse(treatment == 'Control',
                      model2_ci$coefs[1] + model2_ci$coefs[2]*time,
                      model2_ci$coefs[1] + model2_ci$coefs[4] + (model2_ci$coefs[2] + model2_ci$coefs[3])*time))
model2_r2 <- r.squaredGLMM(model2)

# plots
# plot level subsidence
g1 <- ggplot(subpoints2, aes(x = year, y = subsidence)) +
  geom_point(aes(color = treatment)) +
  geom_smooth(method = 'lm', color = 'black') +
  facet_grid(fence~plot) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018),
                     name = '') +
  scale_y_continuous(name = 'Subsidence (m)') +
  ggtitle('Modeled Subsidence by Plot') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle = 60, vjust = 1.5, hjust = 1.5, size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12)) +
  scale_color_manual(name = 'Treatment',
                     breaks=c("Control", "Warming"),
                     values=c("#006666", "#CC3300"))

g1
# ggsave(plot = g1, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Figures/Plot_Subsidence_Ratio_Corrected_2018.jpg", height = 8, width = 12)
# ggsave(plot = g1, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Figures/Plot_Subsidence_Ratio_Corrected_2018.pdf", height = 8, width = 12)

# treatment level subsidence - How do you add confidence intervals for a mixed effects model?
mixed.model.graph <- ggplot(subpoints.fit, aes(x = time, y = subsidence, colour = treatment)) +
  geom_ribbon(data = subpoints.ci, aes(x = time, ymin = lwr, ymax = upr, group = treatment, fill = treatment), inherit.aes = FALSE, alpha = 0.3) +
  geom_point(alpha = 0.5) +
  geom_segment (mapping=aes(x = 0, 
                            y = model2_ci$coefs[1], 
                            xend = 9, 
                            yend = model2_ci$coefs[1] + model2_ci$coefs[2]*9), 
                colour = '#006699') +
  geom_segment (mapping=aes(x = 0, 
                            y = model2_ci$coefs[1] + model2_ci$coefs[4], 
                            xend = 9, 
                            yend = model2_ci$coefs[1] + model2_ci$coefs[4] + (model2_ci$coefs[2] + model2_ci$coefs[3])*9), 
                colour = '#990000') +
  scale_color_manual(values = c("#006699", "#990000"),
                     labels = c(paste('Control:  y = ', 
                                      round(model2_ci$coefs[1]*100, 2), 
                                      ' - ', 
                                      round(model2_ci$coefs[2]*-100, 2), 
                                      'x', 
                                      sep = ''), 
                                paste('Warming:  y = ', 
                                      round((model2_ci$coefs[1] + model2_ci$coefs[4])*100, 2), 
                                      ' - ', round((model2_ci$coefs[2] + model2_ci$coefs[3])*-100, 2), 
                                      'x', 
                                      sep = '')),
                     name = '') +
  scale_fill_manual(values = c("#006699", "#990000"),
                    labels = c(paste('Control:  y = ', 
                                     round(model2_ci$coefs[1]*100, 2), 
                                     ' - ', 
                                     round(model2_ci$coefs[2]*-100, 2), 
                                     'x', 
                                     sep = ''), 
                               paste('Warming:  y = ', 
                                     round((model2_ci$coefs[1] + model2_ci$coefs[4])*100, 2), 
                                     ' - ', round((model2_ci$coefs[2] + model2_ci$coefs[3])*-100, 2), 
                                     'x', 
                                     sep = '')),
                     name = '') +
  scale_x_continuous(breaks = seq(0, 9),
                     labels = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                     name = '') +
  scale_y_continuous(name = 'Subsidence (cm)',
                     breaks = seq(-0.9, 0.1, .1),
                     labels = c('', -80, '', -60, '', -40, '', -20, '', 0, '')) +
  theme_few() +
  theme(axis.text.x  = element_text(angle = 60, vjust = 1.5, hjust = 1.5, size = 8),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification=c(0, 0),
        legend.position=c(0.01, 0.01),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12)) +
  coord_fixed(ratio = 10) +
  annotate('text', x = 0.7, y = -0.7, label = paste0("~R[c]^2==", round(as.numeric(model2_r2[2]), 2)), parse = TRUE, size = 3)


mixed.model.graph
# ggsave(plot = mixed.model.graph, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Plot_Subsidence_Mixed_Effects_2018_notitle.jpg", width = 95, height = 115, units = 'mm')
# ggsave(plot = mixed.model.graph, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Plot_Subsidence_Mixed_Effects_2018_notitle.pdf", width = 95, height = 115, units = 'mm')

# ALT vs. subsidence adjusted ALT by plot
g2 <- ggplot(ALTsubgraph, aes(x = year, y = ALT, color = Measurement_Type)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000", "#ff0000"),
                     labels = c('ALT', 'Subsidence Adjusted ALT')) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018),
                     name = '') +
  scale_y_continuous(name = 'ALT (cm)') +
  facet_grid(fence ~ plot) +
  ggtitle('Comparison of Original and Subsidence Adjusted ALT') +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text.x  = element_text(angle = 60, vjust = 1.5, hjust = 1.5, size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
g2
# ggsave(plot = g2, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Figures/Subsidence_Adjusted_ALT_Ratio_Corrected_2018.jpg", height = 8, width = 12)
# ggsave(plot = g2, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Figures/Subsidence_Adjusted_ALT_Ratio_Corrected_2018.pdf", height = 8, width = 12)

# ALT vs. subsidence adjusted ALT by treatment
g3 <- ggplot(ALTsubgraph2, aes(x = year, y = mean.ALT, color = sub.correction)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean.ALT-se.ALT, ymax = mean.ALT+se.ALT), width = 0.5) +
  scale_color_manual(breaks = c('Raw', 'Subsidence Adjusted'),
                     values = c("#000000", "#ff0000"),
                     labels = c('ALT', 'Subsidence Adjusted\nALT')) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018),
                     name = '') +
  scale_y_continuous(name = 'ALT (cm)',
                     limits = c(-150, 0)) +
  facet_grid(. ~ treatment) +
  ggtitle('Original and Subsidence Adjusted ALT') +
  theme_few() +
  theme(legend.title=element_blank(),
        axis.text.x  = element_text(angle = 60, vjust = 1.5, hjust = 1.5, size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
g3
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur\ Lab/GPS/Figures/Subsidence_Adjusted_ALT_Ratio_Corrected_2018_summary.jpg', plot = g3, height = 6, width = 8.5)
# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur\ Lab/GPS/Figures/Subsidence_Adjusted_ALT_Ratio_Corrected_2018_summary.pdf", plot = g3, height = 6, width = 8.5)

# ALT vs. subsidence adjusted ALT by treatment
gtest <- ggplot(ALTsubgraph2, aes(x = year, y = mean.ALT, color = sub.correction)) +
  geom_col(data = diff, aes(x = year, y = diff/-1), color = 'grey35', inherit.aes = FALSE) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean.ALT-se.ALT, ymax = mean.ALT+se.ALT), width = 0.5) +
  geom_errorbar(data = diff, aes(x = year, ymin = (diff-Se.diff)/-1, ymax = (diff + Se.diff)/-1), inherit.aes = FALSE, width = 0.5, position = 'dodge') +
  scale_color_manual(breaks = c('Raw', 'Subsidence Adjusted'),
                     values = c("#000000", "#ff0000"),
                     labels = c('Raw ALT', 'ALT + Subsidence')) +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                     name = '') +
  scale_y_continuous(name = 'Subsidence Adjusted ALT (cm)',
                     limits = c(-150, 5),
                     breaks = c(-150, -125, -100, -75, -50, -25, 0),
                     labels = c(-150, '', -100, '', -50, '', 0),
                     sec.axis = sec_axis(~.*1, 
                                         name = expression(Delta~Thawed~Carbon~(kg~C/m^2)),
                                         labels = c(150, 100, 50, 0))) +
  facet_grid(. ~ treatment) +
  theme_few() +
  theme(legend.title=element_blank(),
        axis.text.x  = element_text(angle = 60, vjust = 1.5, hjust = 1.5, size = 8),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification=c(0,0),
        legend.position=c(0.01,0.01),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

gtest

gtest2 <- ggplot(ALTsubgraph2, aes(x = year, y = mean.ALT, color = sub.correction)) +
  geom_col(data = avail_c, aes(x = year, y = totC/-2, fill = sub.correction), inherit.aes = FALSE, position = 'dodge') +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean.ALT-se.ALT, ymax = mean.ALT+se.ALT), width = 0.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(data = avail_c, aes(x = year, ymin = (totC-Se)/-2, ymax = (totC + Se)/-2, group = sub.correction), inherit.aes = FALSE, width = 0.5, position = position_dodge(width = 1)) +
  scale_color_manual(name = 'Permafrost Thaw',
                     breaks = c('Raw', 'Subsidence Adjusted'),
                     values = c("#666666", "#000000"),
                     labels = c('ALT', 'S-ALT')) +
  scale_fill_manual(name = 'Thawed C',
                    breaks = c('Raw', 'Subsidence Adjusted'),
                     values = c("#666666", "#000000"),
                     labels = c('ALT', 'S-ALT')) +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                     name = '') +
  scale_y_continuous(name = 'Permafrost Thaw (cm)',
                     limits = c(-150, 5),
                     breaks = c(-150, -125, -100, -75, -50, -25, 0),
                     labels = c(-150, '', -100, '', -50, '', 0),
                     sec.axis = sec_axis(~.*2, 
                                         name = expression(Thawed~Carbon~(kg~C/m^2)),
                                         breaks = c(-300, -250, -200, -150, -100, -50, 0),
                                         labels = c(300, '', 200, '', 100, '', 0))) +
  facet_grid(. ~ treatment) +
  theme_few() +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  theme(axis.text.x  = element_text(angle = 60, vjust = 1.5, hjust = 1.5, size = 8),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 0),
        legend.position = c(0.01, 0.01),
        legend.box = 'horizontal',
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
gtest2

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur\ Lab/GPS/Figures/Subsidence_Permafrost_Thaw_2018_summary.jpg', plot = gtest2, height = 6, width = 9.5)
# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur\ Lab/GPS/Figures/Subsidence_Permafrost_Thaw_2018_summary.pdf", plot = gtest2, height = 6, width = 9.5)


# soil profile graphs
# colors needed
names <- c('Subsidence', 'Unsaturated Active Layer', 'Saturated Active Layer', 'Permafrost')
color <- c('Permafrost' = '#666666', 'Unsaturated Active Layer' = '#996633', 'Saturated Active Layer' = '#006699', 'Subsidence' = '#FFFFFF')
linetypes <- c('Permafrost' = 'solid', 'Unsaturated Active Layer' = 'solid', 'Saturated Active Layer' = 'solid', 'Subsidence' = 'dashed')

# Cross section of soil for control and warming
g4 <- ggplot(ALTsub.summary, aes(x = year)) +
  facet_grid(. ~ treatment) +
  geom_ribbon(aes(ymin = mean.subsidence, ymax = 0, fill = 'Subsidence', linetype = 'Subsidence'), 
              color = "black") +
  geom_ribbon(aes(ymin = -160, ymax = mean.ALT.corrected, fill = 'Permafrost', linetype = 'Permafrost'), 
              colour = 'black') +
  geom_ribbon(aes(ymin = mean.ALT, ymax = mean.subsidence, fill = 'Unsaturated Active Layer', linetype = 'Unsaturated Active Layer'), 
              colour = 'black') +
  geom_ribbon(aes(ymin = mean.ALT.corrected, ymax = mean.subsidence + mean.WTD, fill = 'Saturated Active Layer', linetype = 'Saturated Active Layer'), 
              colour = 'black') +
  geom_line(aes(y = mean.ALT), color = 'black', linetype = 3) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_fill_manual(name = '',
                    values = color,
                    breaks = names) +
  scale_linetype_manual(name = '',
                        values = linetypes,
                        breaks = names) +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                     labels = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                     limits = c(2009, 2018),
                     expand = c(0,0),
                     name = '') +
  scale_y_continuous(limits = c(-160, 5),
                     breaks = c(0, -25, -50, -75, -100, -125, -150),
                     labels = c(0, '', -50, '', -100, '', -150),
                     expand = c(0,0),
                     name = 'Depth (cm)') +
  theme_few() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        strip.text.x = element_text(size = 12, color = 'black'),
        axis.text.x  = element_text(angle = 60, vjust = 1.3, hjust = 1.5, size = 8),
        legend.justification=c(0,0),
        legend.position=c(0,0),
        legend.background = element_rect(color="grey30", size=.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        panel.spacing = unit(1, "lines"))

g4

# ggsave(plot = g4, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Sub_ALT_WTD_Profile_Ratio_Corrected_2018_notitle.jpg", height = 150, width = 190, units = 'mm')
# ggsave(plot = g4, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Sub_ALT_WTD_Profile_Ratio_Corrected_2018_notitle.pdf", height = 150, width = 190, units = 'mm')

# Cross section of soil by fence
g5 <- ggplot(ALTsub_fence, aes(x = year)) +
  facet_grid(treatment ~ fence) +
  geom_ribbon(aes(ymin = mean.subsidence, ymax = 0, fill = 'Subsidence')) +
  geom_ribbon(aes(ymin = -200, ymax = mean.ALT, fill = 'Permafrost'), 
              colour = 'black') +
  geom_ribbon(aes(ymin = mean.ALT, ymax = mean.subsidence, fill = 'Active Layer Thickness'), 
              colour = 'black') +
  geom_ribbon(aes(ymin = mean.ALT, ymax = mean.subsidence + mean.WTD, fill = 'Water Table Depth'), 
              colour = 'black') +
  geom_path(aes(y = mean.subsidence), size = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_fill_manual(name = '',
                    values = color,
                    breaks = color.names) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018),
                     limits = c(2009, 2018),
                     expand = c(0,0),
                     name = '') +
  scale_y_continuous(limits = c(-200, 10),
                     expand = c(0,0),
                     name = 'Depth (cm)') +
  theme_few() +
  theme(strip.text.x = element_text(size = 12),
        axis.text.x  = element_text(angle = 60, vjust = 1.5, hjust = 1.5, size = 12)) +
  coord_fixed(ratio = 0.1)

g5

# ggsave(plot = g5, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Soil_Profile_Ratio_Corrected_2018_by_fence.jpg")
# ggsave(plot = g5, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Soil_Profile_Ratio_Corrected_2018_by_fence.pdf")

g6 <- ggplot(ALTsub.summary, aes(x = mean.ALT.corrected*-1, y = mean.subsidence, colour = treatment)) +
  geom_point() +
  scale_color_manual(values = c("#006699", "#990000"),
                     labels = c('Control', 'Warming'),
                     name = '') +
  scale_x_continuous(name = 'ALT (cm)') +
  scale_y_continuous(name = 'Subsidence (cm)') +
  ggtitle('Relationship Between ALT and Subsidence') +
  theme_few() +
  theme(legend.title=element_blank(),
        axis.text.x  = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
g6

# ggsave(plot = g6, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/ALT_Subsidence_Relationship.jpg", height = 4, width = 7)
# ggsave(plot = g6, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/ALT_Subsidence_Relationship.pdf", height = 4, width = 7)
############################################################################################################