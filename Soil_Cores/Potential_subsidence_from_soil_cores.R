####################################################################################################################################
###                         Subsidence from soil cores/ what part of soil is compacting?                                         ###
###                                                HGR 10/2018                                                                   ###
####################################################################################################################################

### load libraries #################################################################################################################
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
library(R.utils)
####################################################################################################################################

### load data ######################################################################################################################
soil <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/Soil_09_17.csv')
### units of soil_09_17 ###
# depth.cat, depth0, depth1 (cm)
# moisture, ash, C, N (g/kg)
# bulk.density (g/cm^3)
# C/N (unitless)
# delta13C, delta15N (per mil)
# x.stock (stocks of soil, N, C, etc.), cu.x.stock (cumulative stocks) (kg/m^2)
###
rocks <- read_excel("Z:/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2009/CiPEHR Soil Data 2009_EP_CH_05.03.2012.xlsx",
                    sheet = 3) %>%
  filter(!is.na(Rock))

whc <- read_excel("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/water holding capacity.xlsx")

# read in water well location data and subsidence data for 2013
water_wells <- read_sf('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/water_wells.shp') %>%
  filter(!(str_detect(Name, pattern = '^ww.*n$') | str_detect(Name, pattern = '^ww.*s$'))) %>%
  dplyr::select(Name, Easting, Northing) %>%
  mutate(year = ifelse(str_detect(Name, pattern = '^ww.*5$'),
                       2013,
                       ifelse(str_detect(Name, pattern = '^ww.*17$'),
                              2017,
                              2009)),
         fence = as.numeric(str_sub(Name, 3, 3)),
         block = ifelse(fence == 1 | fence == 2,
                        'a',
                        ifelse(fence == 3 | fence == 4,
                               'b',
                               'c')),
         well = str_sub(Name, 5)) %>%
  st_zm()
coords <- st_coordinates(water_wells)
water_wells <- water_wells %>%
  mutate(Easting = coords[,1],
         Northing = coords[,2])
# Load ALT
ALTsub <- read.table('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.txt', 
                     header = TRUE, sep = '\t')
# find elevation files in directory
filenames <- list.files("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks", 
                        full.names = TRUE, pattern = '^.*.tif$')
# make a list of elevation raster stacks for each block
Elevation <- list(brick(filenames[1]), brick(filenames[4]), brick(filenames[7]))

rm(filenames)
####################################################################################################################################

### functions necessary for following code #########################################################################################
avg.soil.prop <- Vectorize(function( depth0, depth1, new.depth0, new.depth1, bulk.density, soil.prop ){
  if( all( depth1 < new.depth0 )){    # if the core isn't deep enough to reach the starting depth of the desired layer
    return(NA)
  }
  height <- (depth1 - depth0)/100
  # calc avg bulk density of core between last and current goals
  if ( any(depth1 >= new.depth1) ) {
    index.lwr <- min(which(depth1 > new.depth0)) # layer where we reach the new starting depth
    index.upr <- min(which(depth1 >= new.depth1)) # layer where we reach the new ending depth
    prior.prop <- soil.prop[index.lwr:index.upr] # the values of the soil property that needs to be averaged
    
    # determine the needed height from each old soil layer
    heights <- height[index.lwr:index.upr]
    if (length(heights) == 1) {
      heights <- (new.depth1 - new.depth0)/10
    } else {
    heights[1] <- (depth1[index.lwr] - new.depth0)/100
    heights[length(heights)] <- (new.depth1 - depth0[index.upr])/100
    }
    
    # calculate weights based on the soil stock in the needed portion of each old soil layer
    weights <- bulk.density[index.lwr:index.upr]*heights
    prop <- sum(prior.prop * weights) / sum(weights)

  } else {
    
    index.lwr <- min(which(depth1 > new.depth0)) # layer where we reach the new starting depth
    index.upr <- max( which( depth1 >= new.depth0) ) # last layer we have
    prior.prop <- soil.prop[index.lwr:index.upr] # the values of the soil property that needs to be averaged
    
    # determine the needed height from each old soil layer
    heights <- height[index.lwr:index.upr]
    if (length(heights) == 1) {
      heights <- (new.depth1 - new.depth0)/100
    } else {
      heights[1] <- depth0[index.lwr] - new.depth0/100
      heights[length(heights)] <- new.depth1 - depth0[index.upr]/100
    }
    
    # calculate weights based on the soil stock in the needed portion of each old soil layer
    weights <- bulk.density[index.lwr:index.upr]*heights
    prop <- sum(prior.prop * weights) / sum(weights)
  }
  
  return(prop)
}, c('new.depth0', 'new.depth1'))

sum.soil.prop <- Vectorize(function( depth0, depth1, new.depth0, new.depth1, bulk.density, soil.prop ){
  if( all( depth1 < new.depth0 )){    # if the core isn't deep enough to reach the starting depth of the desired layer
    return(NA)
  }
  height <- (depth1 - depth0)/100
  # calc avg bulk density of core between last and current goals
  if ( any(depth1 >= new.depth1) ) {
    index.lwr <- min(which(depth1 > new.depth0)) # layer where we reach the new starting depth
    index.upr <- min(which(depth1 >= new.depth1)) # layer where we reach the new ending depth
    prior.prop <- soil.prop[index.lwr:index.upr] # the values of the soil property that needs to be averaged
    
    # determine the needed height from each old soil layer
    heights <- height[index.lwr:index.upr]
    weights <- heights
    if (length(heights) == 1) {
      weights <- (new.depth1 - new.depth0)/100
    } else {
      weights[1] <- (depth1[index.lwr] - new.depth0)/100
      weights[length(weights)] <- (new.depth1 - depth0[index.upr])/100
    }
    weights <- weights/heights
    
    # calculate weights based on the soil stock in the needed portion of each old soil layer
    prop <- sum(prior.prop * weights)
    
  } else {
    
    index.lwr <- min(which(depth1 > new.depth0)) # layer where we reach the new starting depth
    index.upr <- max( which( depth1 >= new.depth0) ) # last layer we have
    prior.prop <- soil.prop[index.lwr:index.upr] # the values of the soil property that needs to be averaged
    print(c(new.depth0, new.depth1))
    # determine the needed height from each old soil layer
    heights <- height[index.lwr:index.upr]
    weights <- heights
    print(paste('heights = ', heights, sep = ''))
    if (length(heights) == 1) {
      weights <- (new.depth1 - new.depth0)/100
    } else {
      weights[1] <- (depth1[index.lwr] - new.depth0)/100
      weights[length(weights)] <- (new.depth1 - depth0[index.upr])/100
    }
    print(paste('weights = ', weights, sep = ''))
    weights <- weights/heights
    print(paste('new.weights = ', weights, sep = ''))
    
    # calculate weights based on the soil stock in the needed portion of each old soil layer
    prop <- sum(prior.prop * weights)
  }
  
  return(prop)
}, c('new.depth0', 'new.depth1'))
####################################################################################################################################

### determine volume of water to subsidence adjusted ALT in each year ##############################################################
# prep alt data for joining to soil core data
alt <- ALTsub %>%
  select(year, fence, plot, treatment, ALT.corrected) %>%
  mutate(treatment = ifelse(treatment == 'Control' | treatment == 'Air Warming',
                            'c',
                            'w')) %>%
  group_by(year, fence, treatment) %>%
  summarise(mean.ALT = mean(ALT.corrected, na.rm = TRUE)*-1) %>%
  ungroup() %>%
  mutate(alt.id = group_indices(., fence, treatment)) %>%
  arrange(fence, treatment, year) %>%
  filter(year == 2009 | year == 2013 | year == 2018)
  
new.depth0 <- alt$mean.ALT[c(TRUE, TRUE, FALSE)]
new.depth1 <- alt$mean.ALT[c(FALSE, TRUE, TRUE)]
new.alt.id <- alt$alt.id[c(FALSE, TRUE, TRUE)]
new.alt.year <- alt$year[c(FALSE, TRUE, TRUE)]

# prep rock data for joining to soil core data
rocks2 <-rocks %>%
  group_by(Fence, plot, depth) %>%
  summarise(rock = sum(Rock)) %>%
  separate(depth, into = c('depth0', 'depth1'), sep = '-') %>%
  mutate(treatment = ifelse(plot <= 4,
                            'c',
                            'w'),
         fence = Fence) %>%
  left_join(filter(alt, year == 2009), by = c('fence', 'treatment')) %>%
  rename(ALT.initial = mean.ALT) %>%
  left_join(filter(alt, year == 2013), by = c('fence', 'treatment')) %>%
  rename(ALT.2013 = mean.ALT) %>%
  left_join(filter(alt, year == 2018), by = c('fence', 'treatment')) %>%
  rename(ALT.end = mean.ALT) %>%
  ungroup() %>%
  select(fence, treatment, depth0, depth1, rock, ALT.initial, ALT.2013, ALT.end) %>%
  filter(depth1 > ALT.initial & depth1 < ALT.2013) %>%
  select(fence, treatment, rock) %>%
  group_by(fence, treatment) %>%
  summarise(rock = sum(rock)) %>%
  ungroup()

# select 2009, 2013, and 2017, average duplicate cores from the same fence/treatment, and join ALT data
soil_09 <- soil %>%
  filter(year == 2009) %>%
  select(-year, -plot) %>%
  filter(!is.na(moisture)) %>%
  group_by(block, fence, treatment)

# calculate average moisture in between 2009, 2013, and 2018 active layers
moisture <- soil_09 %>%
  do({ data.frame( moisture = avg.soil.prop(.$depth0, .$depth1, new.depth0, new.depth1, .$bulk.density, .$moisture))}) %>%
  ungroup()

soil_stock <- soil_09 %>%
  do({ data.frame( soil.stock = sum.soil.prop(.$depth0, .$depth1, new.depth0, new.depth1, .$bulk.density, .$soil.stock))}) %>%
  ungroup()

core.area <- 45.3646 # cm^2

ice_loss <- moisture %>%
  cbind.data.frame(select(soil_stock, soil.stock)) %>%
  group_by(block, fence, treatment) %>%
  mutate(depth0 = new.depth0,
         depth1 = new.depth1,
         height = new.depth1 - new.depth0,
         alt.id = new.alt.id,
         alt.year = new.alt.year,
         bulk.density = soil.stock/(height/100)*10^-3) %>% # g/cm^3 output
  ungroup() %>%
  mutate(core.id = group_indices(., fence, treatment)) %>%
  filter(core.id == alt.id) %>%
  select(-core.id, -alt.id) %>%
  group_by(block, fence, treatment) %>%
  mutate(ice.height = (moisture/1000*bulk.density*height/0.92)/(1 - moisture/1000)*10^-2, # output in m
         cumulative.ice.height = cumsum(ice.height)) %>%
  arrange(block, fence, treatment, alt.year) %>%
  left_join(rocks2, by = c('fence', 'treatment')) %>%
  mutate(rock = ifelse(is.na(rock),
                       0,
                       rock))

# test out adjusting for estimated pore ice content (pore ice doesn't take up volume that would be lost and result in subsidence upon thaw)
# using the estimate that soil volume is 50% pore space in silt loam (i.e. 50% of soil volume can be filled with water)
# also estimate bulk density if all volume of ice is removed to compare to theoretical bulk densities (rock = 2.65 g cm^-3, silt loam soil = 1.33 g cm^-3)
pore_ice_height <- ice_loss %>%
  mutate(core.volume = (depth1 - depth0)*core.area*10^-6, # m^3,
         ice.volume = ice.height*core.area*10^-4, # m^3
         soil.volume.measured = soil.stock*core.area/bulk.density*10^-7 + rock*10^-6, # m^3, including rocks - this gets us the core volume, except when there are rocks when it is larger than the core volume (this is an artifact of calculating bulk density with the total core area minus rocks)
         soil.volume = (core.volume - ice.volume)*2, # m^3, use this one, because the soil without ice does not take up the whole core area
         excess.ice.volume = ifelse(core.volume > soil.volume,
                                    core.volume - soil.volume,
                                    0), # m^3
         pore.ice.volume = ice.volume - excess.ice.volume, # m^3
         pore.ice.height = pore.ice.volume/(core.area*10^-4), # m
         excess.ice.height = excess.ice.volume/(core.area*10^-4), # m
         cumulative.excess.ice.height = cumsum(excess.ice.height), # m
         potential.sub = cumsum(excess.ice.height), # m
         bulk.density.2 = (bulk.density*core.volume)/(soil.volume)) # g cm^-3

mean(pore_ice_height$bulk.density, na.rm = TRUE)
mean(pore_ice_height$bulk.density.2, na.rm = TRUE)

# write.csv(ice_loss, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/ice_height.csv', row.names = FALSE)
# write.csv(pore_ice_height, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/excess_ice_height.csv', row.names = FALSE)

mean.sub <- pore_ice_height %>%
  group_by(alt.year, treatment) %>%
  summarise(mean.ice.sub = mean(cumulative.ice.height, na.rm = TRUE),
            se.ice.sub = sd(cumulative.ice.height, na.rm = TRUE)/sqrt(n()),
            mean.potential.sub = mean(potential.sub, na.rm = TRUE),
            se.potential.sub = sd(potential.sub, na.rm = TRUE)/sqrt(n()))

# figure out how water holding capacity compares to estimated pore space
# this results in even less excess ice than the calcualtion with the estimate of 50% pore space
# water holding capacity is known to be significantly less than the pore space, so this isn't a surprise
# FINAL CONCLUSION: DON'T USE THIS!
whc2 <- whc %>%
  select(fence, depth, whc = `100% WHC per g dry soil`) %>% # whc in g water per g soil at 100% whc
  separate(depth, into = c('depth0', 'depth1'), remove = FALSE) %>%
  filter(depth1 > min(alt$mean.ALT, na.rm = TRUE)) %>%
  mutate(count = 1) %>%
  group_by(depth0, depth1) %>%
  summarise(whc = mean(whc),
            count = sum(count)) %>%
  ungroup() %>%
  summarise(mean.whc = sum(whc*count)/sum(count))

whc_pore_ice_height <- pore_ice_height %>%
  mutate(soil.mass.measured = soil.stock*core.area*10^-1, # g
         pore.ice.volume.whc = as.numeric(whc2)*soil.mass.measured/10^6, # m^3
         excess.ice.volume.whc = ifelse(pore.ice.volume.whc < ice.volume,
                                        ice.volume - pore.ice.volume.whc,
                                        0)) # m^3
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
####################################################################################################################################

### Compare potential subsidence to measured subsidence at that water well's location ##############################################
# water wells taken in 2009
ids <- c('1.2', '1.3', '2.1', '2.4', '3.2', '3.4', '4.1', '4.3', '5.2', '5.3', '6.2', '6.4')

ww2009 <- water_wells %>%
  mutate(well.id = paste(fence, well, sep = '.'),
         block = ifelse(block == 'a',
                        1,
                        ifelse(block == 'b',
                               2,
                               3))) %>%
  filter(well.id %in% ids) %>%
  mutate(treatment = ifelse(well <= 2.5,
                            'c',
                            'w'))

# extract subsidence at each of the 2009 water wells
blocks <- list(1, 2, 3)
sub13_18 <- map(Elevation, ~ brick(.x[[5]]-.x[[1]], .x[[10]]-.x[[1]]))

ww_sub <- map2_dfr(sub13_18, 
               blocks, 
               ~ raster::extract(.x, 
                                 as(filter(ww2009, block == .y), 
                                    'Spatial')) %>%
                 as.data.frame() %>%
                 rename(sub13 = layer.1,
                        sub18 = layer.2) %>%
                 mutate(block = .y)
               ) %>%
  mutate(well.id = ids) %>%
  gather(key = alt.year, value = sub, sub13:sub18) %>%
  mutate(alt.year = ifelse(alt.year == 'sub13',
                           2013,
                           2018))

# add location to the potential subsidence data frame and compare with gps subsidence
# temporarilt switch input from moisture_height to pore_ice_height to test relationship between sub and ice height without pore ice input
ice_loss_2 <- pore_ice_height %>%
  left_join(ww2009, by = c('block', 'fence', 'treatment')) %>%
  st_as_sf() %>%
  left_join(ww_sub, by = c('block', 'well.id', 'alt.year')) %>%
  st_as_sf() %>%
  ungroup() %>%
  mutate(ice.height.diff = cumulative.ice.height - sub*-1,
         potential.sub.diff = potential.sub - sub*-1,
         time = alt.year-2009,
         block2 = as.factor(block),
         fence2 = as.factor(ifelse(fence == 1 | fence == 3 | fence == 5,
                                   1,
                                   2)),
         treatment2 = factor(ifelse(treatment == 'c',
                                       1,
                                       2)),
         fencegroup = factor(block2:fence2),
         wholeplot = factor(block2:fence2:treatment2))

# how does the relationship between ice height without pore ice and subsidence look?
ggplot(ice_loss_2, aes(x = potential.sub, y = sub)) +
  geom_point()

# relationship between total ice height and subsidence
ggplot(ice_loss_2, aes(x = cumulative.ice.height, y = sub)) +
  geom_point()

avg_ice_loss <- ice_loss_2 %>%
  st_drop_geometry() %>%
  group_by(alt.year, treatment) %>%
  summarise(mean.ice.height.diff = mean(ice.height.diff, na.rm = TRUE),
            mean.potential.sub.diff = mean(potential.sub.diff, na.rm = TRUE),
            mean.ice.height = mean(cumulative.ice.height, na.rm = TRUE),
            mean.potential.sub = mean(potential.sub, na.rm = TRUE),
            mean.gps.sub = mean(sub, na.rm = TRUE),
            se.ice.height.sub = sd(cumulative.ice.height, na.rm = TRUE)/sqrt(n()),
            se.potential.sub = sd(potential.sub, na.rm = TRUE)/sqrt(n()),
            se.gps.sub = sd(sub, na.rm = TRUE)/sqrt(n()))

# # model with total ice height
# model1 <- lmer(sub ~ cumulative.ice.height + treatment2 +
#                  (1 | block2/fencegroup/wholeplot) + (1|alt.year), REML = FALSE,
#                data = ice_loss_2,
#                control=lmerControl(check.conv.singular="warning"))
# 
# summary(model1)
# 
# model2 <- lmer(sub ~ cumulative.ice.height +
#                  (1 | block2/fencegroup/wholeplot) + (1|alt.year), REML = FALSE,
#                data = ice_loss_2,
#                control=lmerControl(check.conv.singular="warning"))
# 
# summary(model2)
# 
# AICc(model1, model2)
# 
# # check model residuals of model2
# # look at residuals
# model2.resid <- resid(model2)
# model2.fitted <- fitted(model2)
# model2.sqrt <- sqrt(abs(resid(model2)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model2.fitted, model2.resid, main='resid, model2')
# plot(model2.fitted, model2.sqrt, main='sqrt resid, model2')
# qqnorm(model2.resid, main = 'model2')
# qqline(model2.resid)
# par(mfrow=c(1,1))
# 
# hist(subpointsC$subsidence)
# 
# # re-run better model with REML = TRUE
# model <- lmer(sub ~ cumulative.ice.height +
#                 (1 | block2/fencegroup/wholeplot) + (1|time), REML = TRUE,
#               data = ice_loss_2,
#               control=lmerControl(check.conv.singular="warning"))
# summary(model)

# save model
# saveRDS(model, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/ice_loss_model.rds")

model <- readRDS("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/ice_loss_model.rds")
r.squared <- r.squaredGLMM(model)

# calculate confidence intervals to look at fixed effects
# model_ci <- extract_ci(model)
# write.csv(model_ci, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/ice_loss_Coefficients_Mixed_Effects.csv', row.names = FALSE)
model_ci <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/ice_loss_Coefficients_Mixed_Effects.csv')

# predInt <- predictInterval(model, newdata = ice_loss_2, n.sims = 1000,
#                            returnSims = TRUE, level = 0.95)
# 
# ice_loss_fit <- ice_loss_2 %>%
#   cbind.data.frame(predInt) %>%
#   dplyr::select(-geometry)
# write.csv(ice_loss_fit, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Ice_Loss_Fit_2018.csv', row.names = FALSE)
ice_loss_fit <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Subsidence_Analyses/2018/Ice_Loss_Fit_2018.csv')

# make confidence interval data frame for graphing
ConfData <- data.frame(cumulative.ice.height = seq(min(ice_loss_fit$cumulative.ice.height, na.rm = TRUE),
                                                        max(ice_loss_fit$cumulative.ice.height, na.rm = TRUE),
                                                        length.out = 10))
myStats <- function(model){
  out <- predict( model, newdata=ConfData, re.form=~0 )
  return(out)
}

bootObj <- bootMer(model, FUN=myStats, nsim = 1000)
ConfData <-  cbind( ConfData, confint( bootObj,  level=0.95 ))

colnames(ConfData) <- c('cumulative.ice.height', 'lwr', 'upr')

# # model with potential sub calculated with excess ice and frost heave in pore spaces
# model1 <- lmer(sub ~ potential.sub + treatment2 +
#                  (1 | block2/fencegroup/wholeplot) + (1|alt.year), REML = FALSE,
#                data = ice_loss_2,
#                control=lmerControl(check.conv.singular="warning"))
# 
# summary(model1)
# 
# model2 <- lmer(sub ~ potential.sub +
#                  (1 | block2/fencegroup/wholeplot) + (1|alt.year), REML = FALSE,
#                data = ice_loss_2,
#                control=lmerControl(check.conv.singular="warning"))
# 
# summary(model2)
# 
# model3 <- lmer(sub ~ 1 +
#                  (1 | block2/fencegroup/wholeplot) + (1|alt.year), REML = FALSE,
#                data = ice_loss_2,
#                control=lmerControl(check.conv.singular="warning"))
# 
# summary(model3)
# 
# # potential subsidence from excess ice and pore space frost heave does not predict observed subsidence
# AICc(model1, model2, model3)


# continue with total ice height
ice.loss <- ggplot(ice_loss_fit, aes(x = cumulative.ice.height, y = sub, colour = treatment)) +
  geom_ribbon(data = ConfData, aes(x = cumulative.ice.height, ymin = lwr, ymax = upr), inherit.aes = FALSE, alpha = 0.3) +
  geom_point() +
  geom_segment(aes(x = min(ice_loss_fit$cumulative.ice.height, na.rm = TRUE), 
                   y = model_ci$coefs[1] + model_ci$coefs[2]*min(ice_loss_fit$cumulative.ice.height, na.rm = TRUE), 
                   xend = max(ice_loss_fit$cumulative.ice.height, na.rm = TRUE), 
                   yend = model_ci$coefs[1] + model_ci$coefs[2]*max(ice_loss_fit$cumulative.ice.height, na.rm = TRUE)),
               colour = 'black') +
  scale_x_continuous(name = "Total Ice Height (cm)",
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5),
                     labels = c(10, 20, 30, 40, 50)) +
  scale_y_continuous(name = expression(Delta*" Elevation (cm)"),
                     breaks = c(-0.6, -0.4, -0.2, 0),
                     labels = c(-60, -40, -20, 0)) +
  annotate(geom = 'text', 
           x = 0.4, 
           y = 0.05, 
           label = paste('y = ', round(model_ci$coefs[1], 3), ' - ', round(model_ci$coefs[2], 3)*-1, 'x', sep = ''),
           size = 2.5) +
  annotate(geom = 'text',
           x = 0.46,
           y = 0.01,
           label = paste0("~R[c]^2==", round(r.squared[2], 2)), 
           parse = TRUE, 
           size = 2.5) +
  scale_colour_manual(values = c("#006699", "#990000"),
                      labels = c('Control', 'Warming'),
                      name = '') +
  scale_shape_discrete(name = '') +
  theme_few() +
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  coord_fixed()
ice.loss

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/ice_loss_potential_sub.jpg', ice.loss, width = 95, height = 100, units = 'mm')
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/ice_loss_potential_sub.pdf', ice.loss, width = 95, height = 100, units = 'mm')

ice_loss_model_table <- data.frame(Response = c('Subsidence', NA),
                             `Full Model` = c('Ice Height', 'Soil Warming'),
                             `Final Variables` = c('Intercept', 'Ice Height'),
                             Coeficient = c(model_ci$coefs[1], model_ci$coefs[2]),
                             `Min CI` = c(model_ci$min[1], model_ci$min[2]),
                             `Max CI` = c(model_ci$max[1], model_ci$max[2]),
                             `R2 Marginal` = c(r.squared[1], NA),
                             `R2 Conditional` = c(r.squared[2], NA),
                             AIC = c(AIC(model), NA))
# write.csv(ice_loss_model_table, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/ice_loss_model_table.csv', row.names = FALSE)

# plot of excess ice height vs. subsidence
excess.ice.loss <- ggplot(ice_loss_2, aes(x = cumulative.excess.ice.height, y = sub, colour = treatment)) +
  geom_point() +
  scale_x_continuous(name = "Excess Ice Height (cm)",
                     breaks = c(0.05, 0.1, 0.15, 0.2, 0.25),
                     labels = c(5, 10, 15, 20, 25)) +
  scale_y_continuous(name = expression(Delta*" Elevation (cm)"),
                     breaks = c(-0.6, -0.4, -0.2, 0),
                     labels = c(-60, -40, -20, 0)) +
  scale_colour_manual(values = c("#006699", "#990000"),
                      labels = c('Control', 'Warming'),
                      name = '') +
  scale_shape_discrete(name = '') +
  theme_few() +
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  coord_fixed()
excess.ice.loss

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/excess_ice_loss_potential_sub.jpg', excess.ice.loss, width = 95, height = 120, units = 'mm')
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/excess_ice_loss_potential_sub.pdf', excess.ice.loss, width = 95, height = 120, units = 'mm')
####################################################################################################################################

### Estimate subsidence from C loss ################################################################################################
C.core <- 1500 # g C/m^2 yr from Plaza et al. 2019
CO2.flux <- 755 # g C/m^2 yr from Plaza et al. 2019
CH4.flux <- 1.2 # g C/m^2 yr from Taylor et al. 2018
C.flux <- CO2.flux+CH4.flux
avg.bd <- soil %>%
  filter(year == 2009 & (depth.cat == '35-45' | depth.cat == '45-55')) %>%
  select(bulk.density) %>%
  summarise(mean.bd = mean(bulk.density, na.rm = TRUE)) %>%
  as.numeric()*10^6 # g/m^3

om.bd <- soil %>%
  filter(year == 2009 & (depth.cat == '5-15' | depth.cat == '15-25' | depth.cat == '25-35')) %>%
  select(bulk.density) %>%
  summarise(mean.bd = mean(bulk.density, na.rm = TRUE)) %>%
  as.numeric()*10^6

avg.C.stock <- soil %>%
  filter(year == 2009 & (depth.cat == '5-15' | depth.cat == '15-25' | depth.cat == '25-35')) %>%
  select(C) %>%
  summarise(mean.C = mean(C, na.rm = TRUE)) %>%
  as.numeric()*10^-3 # g/m^3

C.upr.bd.lwr.rate <- C.core/(avg.C.stock*avg.bd) # m/yr
C.lwr.bd.lwr.rate <- C.flux/(avg.C.stock*avg.bd) # m/yr
C.upr.bd.upr.rate <- C.core/(avg.C.stock*om.bd) # m/yr
C.lwr.bd.upr.rate <- C.flux/(avg.C.stock*om.bd) # m/yr
C.upr.bd.upr.9 <- C.upr.bd.upr.rate*9
C.lwr.bd.upr.9 <- C.lwr.bd.upr.rate*9
C.upr.bd.lwr.9 <- C.upr.bd.lwr.rate*9
C.lwr.bd.lwr.9 <- C.lwr.bd.lwr.rate*9

avg.sub.2018 <- ice_loss_2 %>%
  st_drop_geometry() %>%
  ungroup() %>%
  filter(alt.year == 2018) %>%
  select(sub) %>%
  summarise(mean.sub = mean(sub, na.rm = TRUE)*-1) %>%
  as.numeric()

C.upr.bd.lwr.percent <- C.upr.bd.lwr.9/avg.sub.2018
C.lwr.bd.lwr.percent <- C.lwr.bd.lwr.9/avg.sub.2018
C.upr.bd.upr.percent <- C.upr.bd.upr.9/avg.sub.2018
C.lwr.bd.upr.percent <- C.lwr.bd.upr.9/avg.sub.2018
####################################################################################################################################

### Figure of Soil Core Ice Height Calculation #####################################################################################
ice_height_calc_data <- data.frame(year = c(2009, 2009),
                                   x = c(0, 10),
                                   Soil.Surface = c(0, 0),
                                   Thaw.Penetration.2009 = c(-50, -50),
                                   Thaw.Penetration.2018 = c(-90, -90))

names <- c('Original Active Layer', 'Newly Thawed Permafrost', 'Permafrost')
color <- c('Permafrost' = '#666666', 'Original Active Layer' = '#996633', 'Newly Thawed Permafrost' = '#663322')

ggplot(ice_height_calc_data, aes(x = x)) +
  geom_ribbon(aes(ymin = -100, ymax = Thaw.Penetration.2018), fill = '#666666') +
  geom_ribbon(aes(ymin = Thaw.Penetration.2018, ymax = Thaw.Penetration.2009), fill = '#663322') +
  geom_ribbon(aes(ymin = Thaw.Penetration.2009, ymax = Soil.Surface), fill = '#996633') +
  geom_hline(aes(yintercept = Soil.Surface)) +
  geom_hline(aes(yintercept = Thaw.Penetration.2009)) +
  geom_hline(aes(yintercept = Thaw.Penetration.2018)) +
  annotate('text', x = 3, y = -70, label = 'Thawed', angle = 90, size = 2.5, hjust = 0.5, vjust = 0.5) +
  annotate('text', x = 7, y = -70, label = 'Permafrost', angle = 90, size = 2.5, hjust = 0.5, vjust = 0.5) +
  scale_x_continuous(name = '',
                     limits = c(0, 10),
                     labels = NULL,
                     expand = c(0,0)) +
  scale_y_continuous(position = 'right',
                     name = '',
                     limits = c(-100, 0),
                     breaks = c(-90, -50, 0),
                     labels = c('Final Thaw Penetration', 'Initial Thaw Penetration', 'Soil Surface'),
                     expand = c(0,0)) +
  scale_fill_manual(name = '',
                    values = color,
                    breaks = names) +
  ggtitle('2009 Soil Core') +
  theme_few() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 10),
        text = element_text(size = 10)) +
  coord_fixed()

# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Ice_Height_Calculation.jpg", height = 80, width = 45, units = 'mm')
# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Ice_Height_Calculation.pdf", height = 80, width = 45, units = 'mm')

soil_volume_calc_data <- data.frame(x = c(0, 10),
                                    top = c(0, 0),
                                    middle = c(-10, -10),
                                    bottom = c(-25, -25))

ggplot(soil_volume_calc_data, aes(x = x)) +
  geom_ribbon(aes(ymin = bottom, ymax = middle), fill = '#663322') +
  geom_ribbon(aes(ymin = middle, ymax = top), fill = '#0099cc') +
  geom_hline(aes(yintercept = top)) +
  geom_hline(aes(yintercept = middle)) +
  geom_hline(aes(yintercept = bottom)) +
  annotate('text', x = 5, y = -5, label = 'Excess Ice', size = 2.5, hjust = 0.5, vjust = 0.5) +
  annotate('text', x = 5, y = -16, label = 'Soil and', size = 2.5, hjust = 0.5, vjust = 0.5) +
  annotate('text', x = 5, y = -19, label = 'Pore Ice', size = 2.5, hjust = 0.5, vjust = 0.5) +
  scale_x_continuous(name = '',
                     limits = c(0, 10),
                     labels = NULL,
                     expand = c(0,0)) +
  scale_y_continuous(name = '',
                     limits = c(-25, 0),
                     expand = c(0,0),
                     labels = NULL) +
  theme_few() +
  theme(axis.ticks = element_blank(),
        text = element_text(size = 10)) +
  coord_fixed()

# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Soil_Volume_Calculation.jpg", height = 50, width = 30, units = 'mm')
# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Soil_Volume_Calculation.pdf", height = 50, width = 30, units = 'mm')

####################################################################################################################################
