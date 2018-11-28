############################################################################################################
###             ALT Subsidence Correction & Plot Level Mixed Effects Model                               ###
###                            Code written by HGR, Sep 2018                                             ###
############################################################################################################

# need to add 2018 ALT to this dataset, also redo with filled elevation stack

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
CiPEHR_brick <- list(brick('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/AElevationStack.tif'), 
                     brick('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/BElevationStack.tif'), 
                     brick('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/CElevationStack.tif'))
# DryPEHR - Stack the subsidence rasters which are calculated from 2011
DryPEHR_brick <- list(stack(filenames[1:4]), stack(filenames[10:13]), stack(filenames[19:22]))
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
for (i in 1: length(CiPEHR_stack)) {
  plotcoords.temp <- plotcoords %>%
    filter(block == i & exp == 'CiPEHR')
  CiPEHR_extract_temp <- extract(CiPEHR_stack[[i]], plotcoords.temp, layer = 1, nl = nlayers(CiPEHR_stack[[i]]), df = TRUE) %>%
    dplyr::select(sub2011 = 2,
                  sub2015 = 3,
                  sub2016 = 4,
                  sub2017 = 5,
                  sub2018 = 6,
                  -ID) %>%
    mutate(sub2009 = 0,
           sub2010 = NA,
           sub2012 = NA,
           sub2013 = NA,
           sub2014 = NA) %>%
    cbind.data.frame(plotcoords.temp) %>%
    gather(key = year, value = subsidence, sub2011:sub2014) %>%
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
for (i in 1: length(DryPEHR_stack)) {
  plotcoords.temp <- plotcoords %>%
    filter(block == i & exp == 'DryPEHR')
  DryPEHR_extract_temp <- extract(DryPEHR_stack[[i]], plotcoords.temp, layer = 1, nl = nlayers(DryPEHR_stack[[i]]), df = TRUE) %>%
    dplyr::select(sub2015 = 2,
                  sub2016 = 3,
                  sub2017 = 4,
                  sub2018 = 5,
                  -ID) %>%
    mutate(sub2011 = 0,
           sub2012 = NA,
           sub2013 = NA,
           sub2014 = NA) %>%
    cbind.data.frame(plotcoords.temp) %>%
    gather(key = year, value = subsidence, sub2015:sub2014) %>%
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
                        'A',
                        ifelse(block == 2,
                               'B',
                               'C')),
         subsidence = ifelse(subsidence != is.na(subsidence),
                             subsidence,
                             predict.lm(subsidence ~ time))) %>%
  dplyr::select(year, exp, block, fence, plot, treatment, subsidence, time)

# write.table(Sub_extract, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/Subsidence_2009_2018_Ratio_Corrected.txt', sep = '\t', row.names = FALSE)

############################################################################################################
  
######################### Linear Models ####################################################################
#Linear models - this one for each plot individually
submodel <- function(df) {fit <- lm(subsidence ~ time,  data=df)
return(cbind.data.frame(
  reg.intercept = summary(fit)$coefficients[1],
  reg.slope = summary(fit)$coefficients[2],
  reg.r2 = summary(fit)$r.squared,
  reg.pvalue = anova(fit)$'Pr(>F)'[1]
))}

coefs_subsidence <- Sub_extract %>%
  group_by(fence, plot) %>%
  do(submodel(.)) %>%
  ungroup() %>%
  mutate(fence = as.integer(fence))

# write.csv(coefs_subsidence, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/CiPEHR_DryPEHR_Subsidence_Regression_Coefficients_Ratio_Corrected_2009_2018.csv', row.names = FALSE)
############################################################################################################

######################### Merge points2017 and regression coefficients and gap fill with regressions ###########################
require(graphics)

## Predictions
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

subfill <- Sub_extract %>%
  dplyr::select(-time) %>%
  left_join(coefs_subsidence, by = c("fence", "plot")) %>%
  mutate(year = str_c('sub', year, sep = '')) %>%
  spread(key = year, value = subsidence) %>%
  mutate(sub2010 = ifelse(exp == 'CiPEHR',
                          reg.intercept+reg.slope*1,
                          NA),
         sub2012 = ifelse(exp == 'CiPEHR',
                          reg.intercept+reg.slope*3,
                          reg.intercept+reg.slope*1),
         sub2013 = ifelse(exp == 'CiPEHR',
                          reg.intercept+reg.slope*4,
                          reg.intercept+reg.slope*2),
         sub2014 = ifelse(exp == 'CiPEHR',
                          reg.intercept+reg.slope*5,
                          reg.intercept+reg.slope*3)) %>%
  gather(key = year, value = subsidence, sub2009:sub2014) %>%
  mutate(year = as.integer(str_sub(year, start = 4))) %>%
  arrange(year, exp, block, fence, plot) %>%
  dplyr::select(year, exp, block, fence, plot, treatment, reg.intercept, reg.slope, reg.r2, reg.pvalue, subsidence, geometry)

subfill.coords <- st_coordinates(subfill$geometry)

subfill.excel <- subfill %>%
  dplyr::select(-geometry) %>%
  cbind.data.frame(subfill.coords)

# write.csv(subfill.excel, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Correction_Regressions_2009_2018.csv', row.names = FALSE)

# join subsidence and alt data and make correction

## Somehow 2016 DryPEHR data are getting duplicated - fix me!
ALTsub <- subfill %>%
  left_join(ALTdata2, by = c("year", "exp", "block", "fence", "plot", "treatment")) %>%
  mutate(ALT = ALT*-1,
         ALT.corrected = ifelse(reg.slope < 0,
                                ALT+subsidence*100,
                                ALT)) %>%
  dplyr::select(year, exp, block, fence, plot, treatment, reg.intercept, reg.slope, reg.r2, reg.pvalue, subsidence, ALT, ALT.corrected) %>%
  arrange(year, exp, block, fence, plot)

# Write file
# write.csv(ALTsub, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.csv', row.names = FALSE)
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
ALTsub <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.csv')
subpoints <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/Subsidence_2009_2018_Ratio_Corrected.csv') %>%
  st_as_sf(coords = c('X', 'Y'))
WTD <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/WTD/Compiled/WTD_2018_compiled.csv") %>%
  mutate(treatment = ifelse(treatment == 'Drying',
                            'Control',
                            ifelse(treatment == 'Drying + Warming',
                                   'Warming',
                                   as.character(treatment))))

# join WTD and sub by fence and treatment
WTD_fence <- WTD %>%
  group_by(year, block, fence, treatment) %>%
  summarise(mean.WTD = mean(WTD, na.rm = TRUE)*-1,
            se.WTD = sd(WTD, na.rm = TRUE)/sqrt(n()))

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

# average WTD by year and treatment
WTD2 <- WTD %>%
  group_by(year, treatment) %>%
  summarise(WTD = mean(WTD, na.rm = TRUE)*-1)

# gather ALT values that are and aren't corrected to be able to graph on top of each other
ALTsubgraph <- ALTsub %>%
  gather(key = Measurement_Type, value = ALT, ALT:ALT.corrected) %>%
  arrange(desc(Measurement_Type), year, exp, block, fence, plot) %>%
  mutate(plot = factor(plot, levels = c('1', '2', '3', '4','a', 'b', '5', '6', '7', '8', 'c', 'd')))

# redo treatment to only reflect winter warming
subpoints2 <- subpoints %>%
  mutate(plot = factor(plot, levels = c('1', '2', '3', '4','a', 'b', '5', '6', '7', '8', 'c', 'd')),
         treatment = ifelse(treatment == 'Air Warming' | treatment == 'Control' | treatment == 'Drying',
                            'Control',
                            'Warming'))

# a summarized dataframe for graphing differences in corrected and uncorrected ALT between control and warming
ALTsubgraph2 <- ALTsubgraph %>%
  mutate(treatment = ifelse(treatment == 'Air Warming' | treatment == 'Control' | treatment == 'Drying',
                            'Control',
                            'Warming')) %>%
  group_by(year, treatment, Measurement_Type) %>%
  summarise(mean.subsidence = mean(subsidence, na.rm = TRUE)*100,
            mean.ALT = mean(ALT, na.rm = TRUE),
            se.subsidence = sd(subsidence, na.rm = TRUE)/sqrt(n()),
            se.ALT = sd(ALT, na.rm = TRUE)/sqrt(n())) %>%
  arrange(year, treatment, desc(Measurement_Type))

# a summarized dataframe with WTD as well to make the soil profile figures
ALTsub.summary <- ALTsub %>%
  mutate(treatment = ifelse(treatment == 'Air Warming' | treatment == 'Control' | treatment == 'Drying',
                            'Control',
                            'Warming')) %>%
  group_by(year, treatment) %>%
  summarise(subsidence = mean(subsidence, na.rm = TRUE)*100,
            ALT = mean(ALT.corrected, na.rm = TRUE)) %>%
  full_join(WTD2, by = c('year', 'treatment'))
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
         fencegroup = factor(block2:fence2),
         wholeplot = factor(block2:fence2:treatment2))

## Run the model with a different slope for each fence/treatment combination
# model2_ci <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Coefficients_Mixed_Effects.csv')


## Model with time and treatment, but no interaction
model1 <- lmer(subsidence ~ time + treatment2 + 
                 (1 | block2/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = subpointsC,
               control=lmerControl(check.conv.singular="warning"))

summary(model1)

## Model with different slopes for control and warming
model2 <- lmer(subsidence ~ time*treatment2  + 
                 (1 | block2/fencegroup/wholeplot) + (1|time), REML = FALSE,
               data = subpointsC,
               control=lmerControl(check.conv.singular="warning"))

summary(model2)

AICc(model2, model1) # model 2 is better, stop

# check model residuals of model2
# look at residuals
model2.resid <- resid(model2)
model2.fitted <- fitted(model2)
model2.sqrt <- sqrt(abs(resid(model2)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(model2.fitted, model2.resid, main='resid, model2')
plot(model2.fitted, model2.sqrt, main='sqrt resid, model2')
qqnorm(model2.resid, main = 'model2')
qqline(model2.resid)
par(mfrow=c(1,1))

hist(subpointsC$subsidence)

# rerun best model with REML = TRUE
subsidence.model <- lmer(subsidence ~ time*treatment2  + 
                 (1 | block2/fencegroup/wholeplot) + (1|time), REML = TRUE,
               data = subpointsC,
               control=lmerControl(check.conv.singular="warning"))

summary(subsidence.model)

# save model
# saveRDS(subsidence.model, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/subsidence_model.rds")

# calculate confidence intervals to look at fixed effects
subsidence_model_ci <- extract_ci(subsidence.model)
# write.csv(model2_ci, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Coefficients_Mixed_Effects.csv', row.names = FALSE)
predInt <- predictInterval(subsidence.model, newdata = subpointsC, n.sims = 100,
                           returnSims = TRUE, level = 0.95)

subpoints.fit <- subpointsC %>%
  cbind.data.frame(predInt) %>%
  dplyr::select(-geometry)
# write.csv(subpoints.fit, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Fit_2018.csv', row.names = FALSE)
############################################################################################################

################################### Graphs #################################################################
# data needed for graphs
model2 <- readRDS("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/subsidence_model.rds")
subpoints.fit <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Fit_2018.csv')
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
model2_ci <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Subsidence_Coefficients_Mixed_Effects.csv')
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
  geom_abline(intercept = model2_ci$coefs[1], slope = model2_ci$coefs[2], colour = '#006699') +
  geom_abline(intercept = model2_ci$coefs[1] + model2_ci$coefs[4], slope = model2_ci$coefs[2] + model2_ci$coefs[3], colour = '#990000') +
  scale_color_manual(values = c("#006699", "#990000"),
                     labels = c('Control', 'Warming'),
                     name = '') +
  scale_fill_manual(values = c("#006699", "#990000"),
                     labels = c('Control', 'Warming'),
                     name = '') +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9),
                     labels = c(2010, 2012, 2014, 2016, 2018),
                     name = '') +
  scale_y_continuous(name = 'Subsidence (cm)',
                     breaks = c(-0.75, -0.50, -0.25, 0.00),
                     labels = c(-75, -50, -25, 0)) +
  ggtitle('Subsidence (Relative to 2009)') +
  theme_few() +
  theme(axis.text.x  = element_text(angle = 60, vjust = 1.5, hjust = 1.5, size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12)) +
  coord_fixed(ratio = 10) +
  annotate('text', x = 7, y = 0.25, label = paste('y = ', round(model2_ci$coefs[1]*100, 2), ' + ', round(model2_ci$coefs[2]*100, 2), 'x', sep = ''), colour = "#006699") +
  annotate('text', x = 1.7, y = -0.75, label = paste('y = ', round((model2_ci$coefs[1] + model2_ci$coefs[4])*100, 2), ' + ', round((model2_ci$coefs[2] + model2_ci$coefs[3])*100, 2), 'x', sep = ''), colour = "#990000") +
  annotate('text', x = 1.05, y = 0.25, label = paste0("~R^2~c==", round(as.numeric(model2_r2[2]), 2)), parse = TRUE)


mixed.model.graph
# ggsave(plot = mixed.model.graph, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Plot_Subsidence_Mixed_Effects_2018.jpg", width = 6, height = 6)
# ggsave(plot = mixed.model.graph, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Plot_Subsidence_Mixed_Effects_2018.pdf", width = 6, height = 6)

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
g3 <- ggplot(ALTsubgraph2, aes(x = year, y = mean.ALT, color = Measurement_Type)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean.ALT-se.ALT, ymax = mean.ALT+se.ALT), width = 0.5) +
  scale_color_manual(values = c("#000000", "#ff0000"),
                     labels = c('ALT', 'Subsidence\nAdjusted ALT')) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018),
                     name = '') +
  scale_y_continuous(name = 'ALT (cm)') +
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

# soil profile graphs
# colors needed
color.names <- c('Subsidence', 'Active Layer Thickness', 'Water Table Depth', 'Permafrost')
color <- c('Permafrost' = '#666666', 'Active Layer Thickness' = '#996633', 'Water Table Depth' = '#006699', 'Subsidence' = '#FFFFFF')

# Cross section of soil for control and warming
g4 <- ggplot(ALTsub.summary, aes(x = year)) +
  facet_grid(. ~ treatment) +
  geom_ribbon(aes(ymin = subsidence, ymax = 0, fill = 'Subsidence')) +
  geom_ribbon(aes(ymin = -175, ymax = ALT, fill = 'Permafrost'), 
              colour = 'black') +
  geom_ribbon(aes(ymin = ALT, ymax = subsidence, fill = 'Active Layer Thickness'), 
              colour = 'black') +
  geom_ribbon(aes(ymin = ALT, ymax = subsidence + WTD, fill = 'Water Table Depth'), 
              colour = 'black') +
  geom_path(aes(y = subsidence), size = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_fill_manual(name = '',
                    values = color,
                    breaks = color.names) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018),
                     limits = c(2009, 2018),
                     expand = c(0,0),
                     name = '') +
  scale_y_continuous(limits = c(-175, 10),
                     expand = c(0,0),
                     name = 'Depth (cm)') +
  theme_few() +
  theme(strip.text.x = element_text(size = 12),
        axis.text.x  = element_text(angle = 60, vjust = 1.5, hjust = 1.5, size = 12))

g4

# ggsave(plot = g4, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Sub_ALT_WTD_Profile_Ratio_Corrected_2018.jpg")
# ggsave(plot = g4, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Sub_ALT_WTD_Profile_Ratio_Corrected_2018.pdf")

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
cols <- c("LINE1"="#f04546","LINE2"="#3591d1","BAR"="#62c76b")
ggplot(data=data,aes(x=a)) + 
  geom_bar(stat="identity", aes(y=h, fill = "BAR"),colour="#333333")+ #green
  geom_line(aes(y=b,group=1, colour="LINE1"),size=1.0) +   #red
  geom_point(aes(y=b, colour="LINE1"),size=3) +           #red
  geom_errorbar(aes(ymin=d, ymax=e, colour="LINE1"), width=0.1, size=.8) + 
  geom_line(aes(y=c,group=1,colour="LINE2"),size=1.0) +   #blue 
  geom_point(aes(y=c,colour="LINE2"),size=3) +           #blue
  geom_errorbar(aes(ymin=f, ymax=g,colour="LINE2"), width=0.1, size=.8) + 
  scale_colour_manual(name="Error Bars",values=cols) + scale_fill_manual(name="Bar",values=cols) +
  ylab("Symptom severity") + xlab("PHQ-9 symptoms") +
  ylim(0,1.6) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, vjust=0.3))
############################################################################################################