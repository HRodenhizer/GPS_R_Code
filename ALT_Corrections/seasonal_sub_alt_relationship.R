##############################################################################################################
###                                  Seasonal Subsidence and ALT 2019                                      ###
###                                         Code by HGR 6/2019                                             ###
##############################################################################################################

### Load libraries ###########################################################################################
library(sf)
library(raster)
library(tidyverse)
library(ggthemes)
library(viridis)
library(merTools)
##############################################################################################################

### Load data ################################################################################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/Seasonal_Sub/', 
                        full.names = TRUE)
frost_heave <- map(filenames, ~ raster(.x))
rm(filenames)
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Microtopography/', 
                        full.names = TRUE)
microtopography <- map(filenames, ~ raster(.x))
plots <- st_read('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/plot_coordinates_from_2017.shp') %>%
  mutate(block = ifelse(fence <= 2,
                        'a',
                        ifelse(fence >= 5,
                               'c',
                               'b')))
ALTsub <- read.table('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.txt', header = TRUE, sep = '\t')
##############################################################################################################

### Extract seasonal subsidence/frost heave at plot locations ################################################
plot_list <- list(filter(plots, block == 'a'), filter(plots, block == 'b'), filter(plots, block == 'c'))
plot_sub_extract <- map2(frost_heave, plot_list,
                       ~ raster::extract(.x, .y, df = TRUE) %>% 
                         cbind.data.frame(.y) %>%
                         dplyr::select(exp, block, fence, plot, Name, frost.heave = 2, Easting, Northing, Elevation)) %>%
  bind_rows()

plot_micro_extract <- map2(microtopography, plot_list,
                           ~ raster::extract(.x, .y, df = TRUE) %>% 
                             cbind.data.frame(.y) %>%
                             dplyr::select(exp, block, fence, plot, Name, microtopography = 2, Easting, Northing, Elevation)) %>%
  bind_rows()

frost_heave_alt <- plot_sub_extract %>%
  left_join(plot_micro_extract, by = c('exp', 'block', 'fence', 'plot', 'Name', 'Easting', 'Northing', 'Elevation')) %>%
  left_join(dplyr::select(filter(ALTsub, year == 2018), -geometry), by = c('exp', 'block', 'fence', 'plot')) %>%
  left_join(plots, by = c('exp', 'block', 'fence', 'plot', 'Name', 'Easting', 'Northing', 'Elevation')) %>%
  st_as_sf() %>%
  mutate(full.treatment = treatment,
         treatment = ifelse(plot <= 4 | plot == 'a' | plot == 'b',
                             'Control',
                             'Warming'),
         block2 = as.factor(ifelse(block == 'a',
                                   1,
                                   ifelse(block == 'b',
                                          2,
                                          3))),
         fence2 = as.factor(ifelse(fence == 1 | fence == 3 | fence == 5,
                                   1,
                                   2)),
         treatment2.soil = as.factor(ifelse(treatment == 'Control',
                                       1,
                                       2)),
         treatment2.air.soil = as.factor(ifelse(full.treatment == 'Control' | full.treatment == 'Drying',
                                       1,
                                       ifelse(full.treatment == 'Air Warming',
                                              2,
                                              ifelse(full.treatment == 'Soil Warming',
                                                     3,
                                                     4)))),
         treatment2.cipehr.drypehr = as.factor(ifelse(full.treatment == 'Control',
                                                      1,
                                                      ifelse(full.treatment == 'Air Warming',
                                                             2,
                                                             ifelse(full.treatment == 'Soil Warming',
                                                                    3,
                                                                    ifelse(full.treatment == 'Air + Soil Warming',
                                                                           4,
                                                                           ifelse(full.treatment == 'Drying',
                                                                                  5,
                                                                                  6)))))),
         fencegroup = factor(block2:fence2),
         wholeplot = factor(block2:fence2:treatment2.cipehr.drypehr))
##############################################################################################################

### A few plots to see what the data look like ###############################################################
ggplot(frost_heave_alt, aes(x = frost.heave, y = ALT, color = full.treatment, label = Name)) +
  geom_point() +
  geom_text(aes(label = Name))

ggplot(frost_heave_alt, aes(x = subsidence, y = ALT, color = full.treatment, label = Name)) +
  geom_point() +
  geom_text(aes(label = Name))

ggplot(frost_heave_alt, aes(x = microtopography, y = ALT, color = full.treatment, label = Name)) +
  geom_point() +
  geom_text(aes(label = Name))

ggplot(frost_heave_alt, aes(x = as.factor(fence), y = subsidence, label = plot)) +
  geom_text(aes(label = plot))
##############################################################################################################

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
##############################################################################################################

### run mixed  effects model of frost.heave ###################################################################
# model of ALT by subsidence
model1 <- lmer(ALT ~ subsidence +
                 (1 | block2/fencegroup/wholeplot), REML = FALSE,
               data = frost_heave_alt,
               control=lmerControl(check.conv.singular="warning"))

summary(model1)

# model of ALT by frost heave
model2 <- lmer(ALT ~ frost.heave +
                 (1 | block2/fencegroup/wholeplot), REML = FALSE,
               data = frost_heave_alt,
               control=lmerControl(check.conv.singular="warning"))

summary(model2)

# model of ALT by microtopography
model3 <- lmer(ALT ~ microtopography +
                 (1 | block2/fencegroup/wholeplot), REML = FALSE,
               data = frost_heave_alt,
               control=lmerControl(check.conv.singular="warning"))

summary(model3)

AIC(model1, model2, model3)

# model of ALT by subsidence and frost heave
model4 <- lmer(ALT ~ subsidence + frost.heave +
                 (1 | block2/fencegroup/wholeplot), REML = FALSE,
               data = frost_heave_alt,
               control=lmerControl(check.conv.singular="warning"))

summary(model4)

# model of ALT by subsidence and microtopography
model5 <- lmer(ALT ~ subsidence + microtopography +
                 (1 | block2/fencegroup/wholeplot), REML = FALSE,
               data = frost_heave_alt,
               control=lmerControl(check.conv.singular="warning"))

summary(model5)

AIC(model1, model4, model5)

# model of ALT by subsidence and soil warming
model6 <- lmer(ALT ~ subsidence + treatment2.soil +
                 (1 | block2/fencegroup/wholeplot), REML = FALSE,
               data = frost_heave_alt,
               control=lmerControl(check.conv.singular="warning"))

summary(model6)

# model of ALT by subsidence and soil and air warming
model7 <- lmer(ALT ~ subsidence + treatment2.air.soil +
                 (1 | block2/fencegroup/wholeplot), REML = FALSE,
               data = frost_heave_alt,
               control=lmerControl(check.conv.singular="warning"))

summary(model7)

AIC(model1, model6, model7)

# model with subsidence, soil warming, and frost heave
model8 <- lmer(ALT ~ subsidence + treatment2.soil + frost.heave +
                 (1 | block2/fencegroup/wholeplot), REML = FALSE,
               data = frost_heave_alt,
               control=lmerControl(check.conv.singular="warning"))

summary(model8)

# model with subsidence, soil warming, and frost heave
model9 <- lmer(ALT ~ subsidence + treatment2.soil + microtopography +
                 (1 | block2/fencegroup/wholeplot), REML = FALSE,
               data = frost_heave_alt,
               control=lmerControl(check.conv.singular="warning"))

summary(model9)

AIC(model6, model8, model9)
