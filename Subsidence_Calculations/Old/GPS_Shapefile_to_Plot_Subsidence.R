########################################################################################
###                GPS shapefile processing for plot level subsidence                ###
###                 Code by HGR 2017                                                 ###
########################################################################################

#Required packages to run the following code
#library(sp)
#library(rgdal)
#library(raster)
#library(dplyr)
#library(stringr)

###Required packages to do this with tidyverse
library(plyr)
library(sf)
library(tidyverse)
library(viridis)
library(rvest)
library(stringr)
library(readxl)
library(sp)
library(rgdal)
library(gstat)
library(raster)
library(Rmisc)
library(gridExtra)
library(ggthemes)
library(lubridate)
library(lme4)
library(MuMIn)

##############################Load files##############################
Points2009 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2009_SPCSAK4.shp")
Points2011 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2011_SPCSAK4.shp")
Points2015 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2015_SPCSAK4.shp")
Points2016 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2016_SPCSAK4.shp")
Points2017 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp")
#######################################################################

##########################Select flux and gaswell plots and correct elevation values (from GPS_Shapefile_to_Subsidence)#################################################
#Select flux and gaswell plots in all years
Plots2009 <- Points2009 %>%
  filter(type == 'gw' | type == 'flux') %>%
  mutate(Year = 2009,
         Elevation = Elevation+1.2) %>%
  dplyr::select(Year, Fence = fence, Plot = plot, Type = type, Elevation)

#Something is not right with the 2011 points (gw and flux plots right next to each other are appearing 4 meters off of each other)
# Plots2011 <- Points2011 %>%
#   filter(type == 'gw' | type == 'flux') %>%
#   mutate(Year = 2011,
#          Elevation = Elevation+1.2) %>%
#   dplyr::select(Year, Fence = fence, Plot = transect, Type = type, Elevation)

Plots2015 <- Points2015 %>%
  filter(type == 'gw' | type == 'flux') %>%
  mutate(Year = 2015,
         Elevation = Elevation+2.1) %>%
  dplyr::select(Year, Fence = fence, Plot = plot, Type = type, Elevation) %>%
  arrange(Fence, Plot)

Plots2016 <- Points2016 %>%
  filter(Type == 'gw' | Type == 'flux') %>%
  mutate(Year = 2016,
         Plot = as.character(Plot),
         Plot = ifelse(Plot == 'B',
                       'b',
                       ifelse(Plot == 'C',
                              'c',
                              ifelse(Plot == 'D',
                                     'd',
                                     Plot)))) %>%
  dplyr::select(Year, Fence, Plot, Type, Elevation)

Plots2017 <- Points2017 %>%
  filter(Type == 'gw' | Type == 'flux') %>%
  mutate(Fence = str_sub(Name, start = 1, end = 1),
         Plot = str_sub(Name, start = 3, end = 3),
         Year = 2017) %>%
  dplyr::select(Year, Fence, Plot, Type, Elevation)
#####################################################################################################

#Join all files into one
Plotsub <- Plots2009 %>%
  rbind.data.frame(Plots2015) %>%
  rbind.data.frame(Plots2016) %>%
  rbind.data.frame(Plots2017) %>%
  filter(Plot != 'b' & Plot != 'c' & Plot != 'd') %>%
  mutate(Treatment = ifelse(Plot == 1 | Plot == 2 | Plot == 3 | Plot == 4,
                            'Control',
                            'Warming'),
         Year = as.integer(Year),
         Block = ifelse(Fence == 1 | Fence == 2,
                        'A',
                        ifelse(Fence ==3 | Fence == 4,
                               'B',
                               'C')))
################################################Don't Run this################################################################
# #Summarise data for graphing
# Graphplots <- Plots %>%
#   group_by(Year, Fence, Treatment) %>%
#   summarise(Elevation = mean(Elevation))
# 
# #Separate out blocks
# GraphA <- Graphplots %>%
#   filter(Fence == 1 | Fence == 2)
# 
# GraphB <- Graphplots %>%
#   filter(Fence == 3 | Fence == 4)
# 
# GraphC <- Graphplots %>%
#   filter(Fence == 5 | Fence == 6)
# 
# 
# #Graph
# ggplot(Plots, aes(x = Year, y = Elevation)) +
#   geom_point() +
#   facet_grid(Plot~Fence)
# 
# ggplot(Graphplots, aes(x = Year, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   facet_grid(.~Fence)
# 
# #Graph by Block
# ggplot(GraphA, aes(x = Year, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   facet_grid(.~Fence)
# 
# ggplot(GraphB, aes(x = Year, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   facet_grid(.~Fence)
# 
# ggplot(GraphC, aes(x = Year, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   facet_grid(.~Fence)
##############################################################################################################################

##############################################Mixed effects model############################################################
# Prepare the data for stats
data2 <- Plotsub

# make unique variables for each nested unit for clarity, lme4 it will recognise the implicit nesting
data2$Block <- as.factor(data2$Block)
data2$Treatment <- as.factor(data2$Treatment)

# make model with fence coded 1-2 and plot coded 1-4, then create unique names for each fence, WW or plot
data2$fence_2 <- ifelse(data2$Fence == '1' | data2$Fence == '3' | data2$Fence == '5', "1", "2") 
data2$plot_1 <- '1'

data2$fence_2 <- as.factor(data2$fence_2)
# data2$Plot <- as.factor(data2$Plot)
data2$plot_1 <- as.factor(data2$plot_1)

# create variable for random structure
data2 <- within (data2, {
  fencegroup <- factor(Block:fence_2)
  wholeplot <- factor(Block:fence_2:Treatment)
  subplot4 <- factor(Block:fence_2:Treatment:plot_1)
})

ggplot(data2, aes(x = Year, y = Elevation, colour = Plot)) +
  facet_grid(.~wholeplot) +
  geom_point()



# center time on 0 so that 2009 is used as the reference year
baseyear <- min(data2$Year)
data2$time <- as.numeric(data2$Year - baseyear)

# make year a factor:
# categorical variable to put in random structure (time will be continuous as fixed effect),
# year effect will account for the offset from the overall trend throught time
data2$Year <- as.factor(data2$Year)

# reorder levels of time and treatment in the data to make sure that control in year 2009 is the reference
# for the intercept
print(levels(data2$Treatment))
print(levels(factor(data2$time)))

data2$Treatment <- relevel(data2$Treatment, ref="Control")
print(levels(data2$Treatment))

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

ggplot(data2, aes(x = time, y = Elevation, colour = subplot4)) +
         geom_point() +
         facet_grid(Block~fencegroup) +
  geom_smooth(method = lm)

ggplot(data2, aes(x = time, y = Elevation, colour = wholeplot)) +
  geom_point() +
  facet_grid(Block~fencegroup) +
  geom_smooth(method = lm)

## Run the model with same slope
# model1 <- lmer(Elevation ~ time*Treatment*fencegroup  + # use * or + depending on the interactions you want to specify
#                         (1 | Block/fencegroup/wholeplot) + (1|Year), REML = TRUE,
#                       data = data2,
#                       control=lmerControl(check.conv.singular="warning"))

# ##Model with different slopes
model2 <- lmer(Elevation ~ time*Treatment  + # use * or + depending on the interactions you want to specify
                 (1 | Block/fencegroup/wholeplot) + (1|Year), REML = TRUE,
               data = data2,
               control=lmerControl(check.conv.singular="warning"))

summary(model1)

# check model residuals of model1
# look at residuals
model1.resid <- resid(model1)
model1.fitted <- fitted(model1)
model1.sqrt <- sqrt(abs(resid(model1)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(model1.fitted, model1.resid, main='resid, model1')
plot(model1.fitted, model1.sqrt, main='sqrt resid, model1')
qqnorm(model1.resid, main = 'model1')
qqline(model1.resid)

par(mfrow=c(1,1))

model1.1 <- lmer(Elevation ~ time*wholeplot  + # use * or + depending on the interactions you want to specify
                 (1 | Block/fencegroup/wholeplot) + (1|Year), REML = FALSE,
               data = data2,
               control=lmerControl(check.conv.singular="warning"))

#test main effects
model1.2 <- update(model1, .~. -time:Treatment)

AICc(model1.2, model1.1)

# calculate confidence intervals to look at fixed effects
model1_ci <- extract_ci(model1)
model2_ci <- extract_ci(model2)

# write.csv(model1_ci, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 GPS/Analyses/Subsidence_Mixed_Effects_Treatment_by_fence.csv")
# write.csv(model2_ci, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 GPS/Analyses/Subsidence_Mixed_Effects_Treatment.csv")

#Import model output so you don't have to re-run it
# model1_ci <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 GPS/Analyses/Subsidence_Mixed_Effects_Treatment_by_fence.csv")
# model2_ci <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 GPS/Analyses/Subsidence_Mixed_Effects_Treatment.csv") %>%
# dplyr::select(-X)

# model1_ci$names <- as.character(model1_ci$term)
# 
# graph_ci(model1_ci, "Elevation", model1)
# 
# model1_ci$names<-c("Intercept","time", "warming * time","warming")
# #fix order of names so graph plots them in the same order regardless of the values
# model1_ci$names<-(factor(model1_ci$names,levels=c("Intercept","warming", "time", "warming * time")))
# 
# model1_ci <- model1_ci[order(model1_ci$names),]
# 
# graph_ci(model1_ci, "Log Biomass, random slope", model1)

#Graph the mixed effects model
#start by summarizing
graphdata <- data2 %>%
  group_by(Year, time, Treatment) %>%
  summarise(Mean.Elevation = mean(Elevation),
            SE.Elevation = sd(Elevation)/sqrt(n()))
control_label <- paste0('y=', round(model2_ci[1,4], 1), round(model2_ci[2,4], 2), 'x')
warming_label <- paste0('y=', round(model2_ci[1,4]+model2_ci[4,4], 1), round(model2_ci[2,4]+model2_ci[3,4], 2), 'x')


appender <- function(string, prefix = "Fence ") paste0(prefix, string)


ggplot(graphdata, aes(x = time, y = Mean.Elevation, colour = Treatment)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = Mean.Elevation-SE.Elevation, ymax = Mean.Elevation+SE.Elevation), width = 0.2) +
  scale_x_continuous(breaks = c(1, 3, 5, 7),
                     labels = c("2010","2012","2014","2016")) +
  scale_y_continuous(limits = c(663, 671)) +
  xlab(NULL) +
  ylab(label = 'Elevation') +
  scale_colour_manual(breaks=c("Control","Warming"),
                      values=c("#006666", "#CC3300")) +
  geom_abline(intercept=model2_ci[1,4], slope=model2_ci[2,4], colour="#006666") +
  geom_abline(intercept=model2_ci[1,2], slope=model2_ci[2,2], colour="#006666", linetype="dashed") +
  geom_abline(intercept=model2_ci[1,3], slope=model2_ci[2,3], colour="#006666", linetype="dashed") +
  geom_abline(intercept=model2_ci[1,4]+model2_ci[4,4], slope=model2_ci[2,4]+model2_ci[3,4], colour="#CC3300") +
  geom_abline(intercept=model2_ci[1,2]+model2_ci[4,2], slope=model2_ci[2,2]+model2_ci[3,2], colour="#CC3300", linetype="dashed") +
  geom_abline(intercept=model2_ci[1,3]+model2_ci[4,3], slope=model2_ci[2,3]+model2_ci[3,3], colour="#CC3300", linetype="dashed") +
  theme_few(base_size = 17) +
  annotate('text', x = 1, y = 668.5, label=control_label, colour = "#006666") +
  annotate('text', x = 1, y = 666, label=warming_label, colour = "#CC3300") +
  annotate('text', x = 2, y = 663, label='time by treatment*', colour = "black")


# ggsave("~/School/NAU/Schuur Lab/GPS/Images/Plot Level Subsidence Average 2.jpg", height = 14, width = 6)

###############################Graph by fence - add fence label! - add equations#####################################
# #Fence 1
# ggplot(subset(data2, Fence == 1), aes(x = time, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   scale_x_continuous(breaks = c(1, 3, 5, 7),
#                      labels = c("2010","2012","2014","2016")) +
#   scale_y_continuous(limits = c(668, 673)) +
#   xlab(label = NULL) +
#   scale_colour_manual(breaks=c("Control","Warming"),
#                       values=c("#006666", "#CC3300")) +
#   geom_abline(intercept=model1_ci[1,4], slope=model1_ci[7,4], colour="#006666") +
#   geom_abline(intercept=model1_ci[1,2], slope=model1_ci[7,2], colour="#006666", linetype="dashed") +
#   geom_abline(intercept=model1_ci[1,3], slope=model1_ci[7,3], colour="#006666", linetype="dashed") +
#   geom_abline(intercept=(model1_ci[1,4]+model1_ci[19,4]), slope=(model1_ci[7,4]+model1_ci[13,4]), colour="#CC3300") +
#   geom_abline(intercept=(model1_ci[1,2]+model1_ci[19,2]), slope=(model1_ci[7,2]+model1_ci[13,2]), colour="#CC3300", linetype="dashed") +
#   geom_abline(intercept=(model1_ci[1,3]+model1_ci[19,3]), slope=(model1_ci[7,3]+model1_ci[13,3]), colour="#CC3300", linetype="dashed") +
#   labs(title = "Fence 1") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_few()
# 
# #Fence 2
# ggplot(subset(data2, Fence == 2), aes(x = time, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   scale_x_continuous(breaks = c(1, 3, 5, 7),
#                      labels = c("2010","2012","2014","2016")) +
#   scale_y_continuous(limits = c(668, 673)) +
#   xlab(label = NULL) +
#   scale_colour_manual(breaks=c("Control","Warming"),
#                       values=c("#006666", "#CC3300")) +
#   geom_abline(intercept=(model1_ci[1,4]+model1_ci[2,4]), slope=(model1_ci[7,4]+model1_ci[8,4]), colour="#006666") +
#   geom_abline(intercept=(model1_ci[1,2]+model1_ci[2,2]), slope=(model1_ci[7,2]+model1_ci[8,2]), colour="#006666", linetype="dashed") +
#   geom_abline(intercept=(model1_ci[1,3]+model1_ci[2,3]), slope=(model1_ci[7,3]+model1_ci[8,3]), colour="#006666", linetype="dashed") +
#   geom_abline(intercept=(model1_ci[1,4]+model1_ci[2,4]+model1_ci[19,4]+model1_ci[20,4]), slope=(model1_ci[7,4]+model1_ci[8,4]+model1_ci[13,4]+model1_ci[14,4]), colour="#CC3300") +
#   geom_abline(intercept=(model1_ci[1,2]+model1_ci[2,2]+model1_ci[19,2]+model1_ci[20,2]), slope=(model1_ci[7,2]+model1_ci[8,2]+model1_ci[13,2]+model1_ci[14,2]), colour="#CC3300", linetype="dashed") +
#   geom_abline(intercept=(model1_ci[1,3]+model1_ci[2,3]+model1_ci[19,3]+model1_ci[20,3]), slope=(model1_ci[7,3]+model1_ci[8,3]+model1_ci[13,3]+model1_ci[14,3]), colour="#CC3300", linetype="dashed") +
#   theme_few()
# 
# #Fence 3 - fix coefs from here on!
# ggplot(subset(data2, Fence == 3), aes(x = time, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   scale_x_continuous(breaks = c(1, 3, 5, 7),
#                      labels = c("2010","2012","2014","2016")) +
#   scale_y_continuous(limits = c(669.5, 672)) +
#   xlab(label = NULL) +
#   scale_colour_manual(breaks=c("Control","Warming"),
#                       values=c("#006666", "#CC3300")) +
#   geom_abline(intercept=(model1_ci[1,4]+model1_ci[3,4]), slope=(model1_ci[7,4]+model1_ci[9,4]), colour="#006666") +
#   geom_abline(intercept=(model1_ci[1,2]+model1_ci[3,2]), slope=(model1_ci[7,2]+model1_ci[9,2]), colour="#006666", linetype="dashed") +
#   geom_abline(intercept=(model1_ci[1,3]+model1_ci[3,3]), slope=(model1_ci[7,3]+model1_ci[9,3]), colour="#006666", linetype="dashed") +
#   geom_abline(intercept=(model1_ci[1,4]+model1_ci[3,4]+model1_ci[19,4]+model1_ci[21,4]), slope=(model1_ci[7,4]+model1_ci[9,4]+model1_ci[13,4]+model1_ci[15,4]), colour="#CC3300") +
#   geom_abline(intercept=(model1_ci[1,2]+model1_ci[3,2]+model1_ci[19,2]+model1_ci[21,2]), slope=(model1_ci[7,2]+model1_ci[9,4]+model1_ci[13,4]+model1_ci[15,4]), colour="#CC3300", linetype="dashed") +
#   geom_abline(intercept=(model1_ci[1,3]+model1_ci[3,3]+model1_ci[19,3]+model1_ci[21,3]), slope=(model1_ci[7,3]+model1_ci[9,4]+model1_ci[13,4]+model1_ci[15,4]), colour="#CC3300", linetype="dashed") +
#   theme_few()
# 
# #Fence 4
# ggplot(subset(data2, Fence == 4), aes(x = time, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   scale_x_continuous(breaks = c(1, 3, 5, 7),
#                      labels = c("2010","2012","2014","2016")) +
#   scale_y_continuous(limits = c(669.5, 672)) +
#   xlab(label = NULL) +
#   scale_colour_manual(breaks=c("Control","Warming"),
#                       values=c("#006666", "#CC3300")) +
#   geom_abline(intercept=671.138, slope=-0.017, colour="#006666") +
#   geom_abline(intercept=670.716, slope=-0.037, colour="#006666", linetype="dashed") +
#   geom_abline(intercept=671.561, slope=0.003, colour="#006666", linetype="dashed") +
#   geom_abline(intercept=670.782, slope=-0.057, colour="#CC3300") +
#   geom_abline(intercept=670.023, slope=-0.095, colour="#CC3300", linetype="dashed") +
#   geom_abline(intercept=671.536, slope=-0.019, colour="#CC3300", linetype="dashed") +
#   theme_few()
# 
# #Fence 5
# ggplot(subset(data2, Fence == 5), aes(x = time, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   scale_x_continuous(breaks = c(1, 3, 5, 7),
#                      labels = c("2010","2012","2014","2016")) +
#   scale_y_continuous(limits = c(669.5, 672)) +
#   xlab(label = NULL) +
#   scale_colour_manual(breaks=c("Control","Warming"),
#                       values=c("#006666", "#CC3300")) +
#   geom_abline(intercept=671.138, slope=-0.017, colour="#006666") +
#   geom_abline(intercept=670.716, slope=-0.037, colour="#006666", linetype="dashed") +
#   geom_abline(intercept=671.561, slope=0.003, colour="#006666", linetype="dashed") +
#   geom_abline(intercept=670.782, slope=-0.057, colour="#CC3300") +
#   geom_abline(intercept=670.023, slope=-0.095, colour="#CC3300", linetype="dashed") +
#   geom_abline(intercept=671.536, slope=-0.019, colour="#CC3300", linetype="dashed") +
#   theme_few()
# 
# #Fence 6
# ggplot(subset(data2, Fence == 6), aes(x = time, y = Elevation, colour = Treatment)) +
#   geom_point() +
#   scale_x_continuous(breaks = c(1, 3, 5, 7),
#                      labels = c("2010","2012","2014","2016")) +
#   scale_y_continuous(limits = c(669.5, 672)) +
#   xlab(label = NULL) +
#   scale_colour_manual(breaks=c("Control","Warming"),
#                       values=c("#006666", "#CC3300")) +
#   geom_abline(intercept=671.138, slope=-0.017, colour="#006666") +
#   geom_abline(intercept=670.716, slope=-0.037, colour="#006666", linetype="dashed") +
#   geom_abline(intercept=671.561, slope=0.003, colour="#006666", linetype="dashed") +
#   geom_abline(intercept=670.782, slope=-0.057, colour="#CC3300") +
#   geom_abline(intercept=670.023, slope=-0.095, colour="#CC3300", linetype="dashed") +
#   geom_abline(intercept=671.536, slope=-0.019, colour="#CC3300", linetype="dashed") +
#   theme_few()
##################################################################################################

# Fig_TD <- ggplot(data.cip, aes(TD_std, KP.correct.meanI, colour=group, shape=group)) +
#   geom_point() +
#   scale_x_continuous(breaks = c(-0.865,-0.505,-0.145, 0.215, 0.575, 0.935,1.295),
#                      labels = c("0","20","40","60","80","100","120")) +
#   geom_abline(intercept=-25.86, slope=-4.01, colour="dodgerblue4") + # shallow dry
#   #geom_abline(intercept=-26.52, slope=-5.17, colour="dodgerblue4", linetype="dashed") + # shallow dry
#   #geom_abline(intercept=-25.19, slope=-2.93, colour="dodgerblue4", linetype="dashed") + # shallow dry
#   geom_abline(intercept=-24.98, slope=-1.64, colour="darkorange") + # deep dry
#   #geom_abline(intercept=-26.86, slope=-4.39, colour="darkorange", linetype="dashed") + # deep dry
#   #geom_abline(intercept=-23.12, slope=1.05, colour="darkorange", linetype="dashed") + # deep dry
#   geom_abline(intercept=-25.10,slope=-0.73, colour="firebrick") + # deep wet
#   #geom_abline(intercept=-26.77,slope=-3.22, colour="firebrick", linetype="dashed") + # deep wet
#  # geom_abline(intercept=-23.43,slope=1.76, colour="firebrick", linetype="dashed") + # deep wet
#   labs(x="Thaw depth (cm)",
#        y = expression(paste('Reco ',delta,''^13,'C-',CO[2],' ‰',sep=''))) +
#   theme(axis.text.y = element_blank(), #element_text(size=10,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
#         axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
#         # axis.title.x = element_blank(),
#         axis.title.y = element_blank(), #element_text(size=12),
#         axis.title.x = element_text(size=12),
#         axis.ticks.length=unit(-1.0,"mm"),
#         legend.key.size=unit(4,"mm"),
#         legend.key = element_rect(fill=NA, colour=NA),
#         legend.title = element_blank(),
#         legend.text = element_text(size=8),
#         # strip.text.x = element_text(size=14),
#         # strip.background = element_rect(colour="white",fill="white"),
#         panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
#         panel.border = element_rect(colour="black", fill=NA),
#         panel.grid = element_blank(),
#         plot.margin=unit(c(2,1,1,2),"mm"), #top,right,bottom,left)
#         legend.position=c(0.2,0.1),
#         legend.justification=c(0.5,0.4)) +
#   scale_shape_manual(values=c("shallow dry"=16, "deep dry" = 15,
#                               "deep wet"=17))+
#   scale_colour_manual(values=c("shallow dry"="dodgerblue4",
#                              "deep dry"="darkorange",
#                              "deep wet"="firebrick"))
# 
# Fig_TD 
#

##########################################################################################################################

###############################################Decided not to do it this way###################################################
ggplot(Plotsub, aes(x = Year, y = Elevation, colour = Treatment)) +
  geom_point() +
  geom_smooth(method = lm)

#Linear models - this one for each plot individually
submodel <- function(df) {fit <- lm(Elevation ~ Year,  data=df)
return(cbind(
  reg.intercept = summary(fit)$coefficients[1,1],
  reg.slope = summary(fit)$coefficients[2,1],
  reg.r2 = summary(fit)$r.squared,
  reg.pvalue = anova(fit)$'Pr(>F)'[1]
))}

coefs_plotsub <- ddply(Plotsub, .(Fence, Plot, Type), submodel)

ggplot(Plotsub, aes(x = Year, y = Elevation)) +
  stat_smooth(aes(colour = Type, fill = Type), method = 'lm') +
  geom_point(aes(colour = Type)) +
  facet_grid(Plot~Fence)


#Linear models - this one for each fence and treatment
coefs_plotsub2 <- ddply(Plotsub, .(Fence, Treatment), submodel) %>%
  mutate(Equation = paste0('y=', round(reg.intercept,2), round(reg.slope, 2), 'x R2: ', round(reg.r2, 2)))

Plotsubcoefs <- Plotsub %>%
  full_join(coefs_plotsub2, by = c('Fence', 'Treatment'))
  

# #Make thelabels the way I want them
appender <- function(string, prefix = "Fence ") paste0(prefix, string)
# eq <- function(prefix = "y=",
#                intercept = Plotsubcoefs$reg.intercept,
#                plus = "+",
#                slope = Plotsubcoefs$reg.slope,
#                x = 'x, R2: ',
#                R2 = Plotsubcoefs$reg.r2) paste0(prefix, string)

#Plot
ggplot(Plotsubcoefs, aes(x = Year, y = Elevation)) +
  stat_smooth(aes(colour = Treatment, fill = Treatment), method = 'lm') +
  geom_point(aes(colour = Treatment)) +
  facet_wrap(~Fence, dir = 'v', ncol = 3, scales = 'free_y', labeller = as_labeller(appender)) +
  scale_x_continuous(breaks=seq(2010, 2016, 2)) +
  scale_colour_manual(breaks=c("Control","Warming"),
                      values=c("#006666", "#CC3300")) +
  scale_fill_manual(breaks=c("Control","Warming"),
                    values=c("#006666", "#CC3300")) +
  theme_few()

#ggsave("~/School/NAU/Schuur Lab/GPS/Images/Plot Level Subsidence.jpg", height = 8, width = 12)
                                   
# #Plot
# p <- ggplot(Plotsubcoefs, aes(x = Year, y = Elevation)) +
#   stat_smooth(aes(colour = Treatment, fill = Treatment), method = 'lm') +
#   geom_point(aes(colour = Treatment)) +
#   facet_wrap(~Fence, dir = 'v', ncol = 3, scales = 'free_y', labeller = as_labeller(appender)) +
#   scale_x_continuous(breaks=seq(2010, 2016, 2)) +
#   scale_colour_manual(breaks=c("Control","Warming"),
#                       values=c("#006666", "#CC3300")) +
#   scale_fill_manual(breaks=c("Control","Warming"),
#                     values=c("#006666", "#CC3300")) +
#   theme_bw() +
#   annotate('text', x=2010, y=670, label=lm_eqn(lm(Elevation~Year, .(Fence, Treatment))),
#            colour="black", size=5,parse=T)
# 
# p1 = p + geom_text(data=coefs_plotsub2,aes(x = 2010, y = 670, label=Equation), parse = TRUE, inherit.aes=FALSE) +
#   facet_wrap(~Fence, dir = 'v', ncol = 3)
# p1

# #######
# # function to annotate graph with model formula
# lm_eqn = function(m) {
#   l <- list(a = format(coef(m)[1], digits = 2),
#             b = format(abs(coef(m)[2]), digits = 2),
#             r2 = format(summary(m)$r.squared, digits = 3));
#   
#   if (coef(m)[2] >= 0)  {
#     eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
#   } else {
#     eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
#   }
#   
#   as.character(as.expression(eq));                 
# }
# 
# # measured vs modeled 2014 parameters
# # measured vs modeled 2015 parameters
# # modeled 2014 vs 2015
# plots4a = function(.d) ggplot(.d, aes(x=flux.umol, y=NEE.pred.2015)) +
#   geom_point() +
#   geom_smooth(method="lm") +
#   scale_y_continuous(limits=c(-6,15)) +
#   scale_x_continuous(limits=c(-6,15)) +
#   labs(title=paste("Measured vs modeled 2015 param, Month=", .d$month,"Fence=", .d$fence, ", Plot", .d$plot, sep = " "),
#        x= "measured flux (umol CO2/m2/half hour)", y="modeled flux 2015(umol CO2/m2/half hour)") +
#   annotate("text", x=2, y=-2, label=lm_eqn(lm(NEE.pred.2015~flux.umol, .d)),
#            colour="black", size=5,parse=T)
# 
# l <- dlply(NEE_2015_2016_graph, .(month,fence,plot), plots4a)
# #ml <- do.call(marrangeGrob, c(l, list(nrow=4, ncol=1)))
# ml <- marrangeGrob(l, nrow=4, ncol=1)
# # ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
# ggsave("NEE_measured_VS_modeled_2015parameters_2016.pdf", ml, width=10, height=12)
#######################################################################################################################


#Extract subsidence and re-calculate ALT
# ###################Import raster files ########################################

A2015sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2015sub.tif")
B2015sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2015sub.tif")
C2015sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2015sub.tif")
A2016sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2016sub.tif")
B2016sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2016sub.tif")
C2016sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2016sub.tif")
A2017sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2017Sub.tif")
B2017sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2017Sub.tif")
C2017sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2017Sub.tif")

########################################Import thaw depth############################################################
Thaw2015 <- read_excel(path = "Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Thaw Depth/processed/2015/thaw_depth_2015.xlsx")
Thaw2016 <- read_excel(path = "Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Thaw Depth/processed/2016/Thaw Depth 2016.xlsx")
Thaw2017 <- read_excel(path = "Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Thaw Depth/processed/2017/Thaw Depth 2017.xlsx")

#Get the right data from the points and convert to sp
Plots2015.v2 <- Points2015 %>%
  filter(type == 'flux' | type == 'gw') %>%
  arrange(fence, plot)

Plots2015.v2 <- Plots2015.v2 %>% st_as_sf %>% st_coordinates() %>% cbind(Plots2015.v2)

coordinates(Plots2015.v2) = ~X+Y

Plots2016.v2 <- Points2016 %>%
  filter(Type == 'flux' | Type == 'gw')

coordinates(Plots2016.v2) = ~Easting+Northing

Plots2017.v2 <- Points2017 %>%
  filter(Type == 'flux' | Type == 'gw')

coordinates(Plots2017.v2) = ~Easting+Northing

#Put thaw data in correct format
Thaw2015.1 <- Thaw2015 %>%
  filter(fence != is.na(fence)) %>%
  filter(id != 'gas' & id != 'offplottrans') %>%
  mutate(realdate = parse_date_time(date,
                                    c("Y!-m!-d!")),
         doy = yday(realdate),
         year = 2015) %>%
  group_by(block, fence, plot) %>%
  filter(td == max(td)) %>%
  filter(doy == first(doy)) %>%
  arrange(fence, plot) %>%
  dplyr::select(year, block, fence, plot, doy, td)

Thaw2016.1 <- Thaw2016 %>%
  filter(fence != is.na(fence)) %>%
  filter(id != 'gas' & id != 'offplottrans') %>%
  mutate(realdate = parse_date_time(Date,
                                    c("Y!-m!-d!")),
         doy = yday(realdate),
         year = 2016) %>%
  group_by(block, fence, plot) %>%
  filter(td == max(td)) %>%
  filter(doy == first(doy)) %>%
  arrange(fence, plot) %>%
  dplyr::select(year, block, fence, plot, doy, td)

Thaw2017.1 <- Thaw2017 %>%
  filter(fence != is.na(fence)) %>%
  filter(id != 'gas' & id != 'offplottrans') %>%
  mutate(realdate = parse_date_time(date,
                                    c("Y!-m!-d!")),
         doy = yday(realdate),
         year = 2017) %>%
  group_by(block, fence, plot) %>%
  filter(td == max(td)) %>%
  filter(doy == first(doy)) %>%
  arrange(fence, plot) %>%
  dplyr::select(year, block, fence, plot, doy, td)
  
# Extract subsidence at each plot
A2015extract <- extract(A2015sub, Plots2015.v2)
B2015extract <- extract(B2015sub, Plots2015.v2)
C2015extract <- extract(C2015sub, Plots2015.v2)
A2016extract <- extract(A2016sub, Plots2016.v2)
B2016extract <- extract(B2016sub, Plots2016.v2)
C2016extract <- extract(C2016sub, Plots2016.v2)
A2017extract <- extract(A2017sub, Plots2017.v2)
B2017extract <- extract(B2017sub, Plots2017.v2)
C2017extract <- extract(C2017sub, Plots2017.v2)

# Add extracted values to thaw depth - 2015 is missing 1-1gw, 1-2gw, and 1-3gw
AThaw2015 <- A2015extract %>%
  sapply("[") %>%
  as.data.frame() %>%
  slice(1:41)

BThaw2015 <- B2015extract %>%
  sapply("[") %>%
  as.data.frame() %>%
  slice(42:85)

CThaw2015 <- C2015extract %>%
  sapply("[") %>%
  as.data.frame() %>%
  slice(86:129)

AThaw2016 <- A2016extract %>%
  sapply("[") %>%
  as.data.frame() %>%
  slice(1:44)

BThaw2016 <- B2016extract %>%
  sapply("[") %>%
  as.data.frame() %>%
  slice(45:88)

CThaw2016 <- C2016extract %>%
  sapply("[") %>%
  as.data.frame() %>%
  slice(89:132)

AThaw2017 <- A2017extract %>%
 sapply("[") %>%
  as.data.frame() %>%
  slice(1:44)

BThaw2017 <- B2017extract %>%
  sapply("[") %>%
  as.data.frame() %>%
  slice(45:88)

CThaw2017 <- C2017extract %>%
  sapply("[") %>%
  as.data.frame() %>%
  slice(89:132)


# Combine and make various dataframes needed for plotting
Thaw2015extract1 <- AThaw2015 %>%
  rbind.data.frame(BThaw2015) %>%
  rbind.data.frame(CThaw2015) %>%
  cbind.data.frame(Plots2015) %>%
  filter(Plot != 'b' & Plot != 'c' & Plot != 'd') %>%
  mutate(plot = as.character(Plot),
         plot = as.numeric(plot)) %>%
  dplyr::select(year = Year, fence = Fence, plot, Type, Sub = 1) %>%
  group_by(year, fence, plot) %>%
  summarise(Sub = mean(Sub, na.rm = TRUE)) %>%
  full_join(Thaw2015.1, by = c('year', 'fence', 'plot')) %>%
  mutate(ALT = round(td, 1),
         Sub = Sub*100,
         ALT.corrected = round(ifelse(Sub < 0,
                                      td-Sub,
                                      td), 1),
         Treatment = ifelse(plot == 1 | plot == 3,
                            'Air Warming',
                            ifelse(plot == 2 | plot == 4,
                                   'Control',
                                   ifelse(plot == 5 | plot == 7,
                                          'Air and Soil Warming',
                                          'Soil Warming')))) %>%
  dplyr::select(-td)

Thaw2016extract1 <- AThaw2016 %>%
  rbind.data.frame(BThaw2016) %>%
  rbind.data.frame(CThaw2016) %>%
  cbind.data.frame(Plots2016) %>%
  filter(Plot != 'b' & Plot != 'c' & Plot != 'd') %>%
  mutate(fence = as.numeric(Fence),
         plot = as.numeric(Plot)) %>%
  dplyr::select(year = Year, fence, plot, Type, Sub = 1) %>%
  group_by(year, fence, plot) %>%
  summarise(Sub = mean(Sub, na.rm = TRUE)) %>%
  full_join(Thaw2016.1, by = c('year', 'fence', 'plot')) %>%
  mutate(ALT = round(td, 1),
         Sub = Sub*100,
         ALT.corrected = round(ifelse(Sub < 0,
                                      td-Sub,
                                      td), 1),
         Treatment = ifelse(plot == 1 | plot == 3,
                            'Air Warming',
                            ifelse(plot == 2 | plot == 4,
                                   'Control',
                                   ifelse(plot == 5 | plot == 7,
                                          'Air and Soil Warming',
                                          'Soil Warming')))) %>%
  dplyr::select(-td)

Thaw2017extract1 <- AThaw2017 %>%
  rbind.data.frame(BThaw2017) %>%
  rbind.data.frame(CThaw2017) %>%
  cbind.data.frame(Plots2017) %>%
  filter(Plot != 'b' & Plot != 'c' & Plot != 'd') %>%
  mutate(fence = as.numeric(Fence),
         plot = as.numeric(Plot)) %>%
  dplyr::select(year = Year, fence, plot, Type, Sub = 1) %>%
  group_by(year, fence, plot) %>%
  summarise(Sub = mean(Sub, na.rm = TRUE)) %>%
  full_join(Thaw2017.1, by = c('year', 'fence', 'plot')) %>%
  mutate(ALT = round(td, 1),
         Sub = Sub*100,
         ALT.corrected = round(ifelse(Sub < 0,
                                td-Sub,
                                td), 1),
         Treatment = ifelse(plot == 1 | plot == 3,
                            'Air Warming',
                            ifelse(plot == 2 | plot == 4,
                                   'Control',
                                   ifelse(plot == 5 | plot == 7,
                                          'Air and Soil Warming',
                                          'Soil Warming')))) %>%
  dplyr::select(-td)

#Join all of the extracts together
Thawextract <- Thaw2015extract1 %>%
  full_join(Thaw2016extract1) %>%
  full_join(Thaw2017extract1)

#Put them in the correct format for various graphs
Thawcorrectionpercent <- Thawextract %>%
  mutate(ALT.increase.percent = round((ALT.corrected-ALT)*100/ALT, 2),
         Treatment = ifelse(Treatment == 'Control' | Treatment == 'Air Warming',
                            'Control',
                            'Warming')) %>%
  group_by(year, Treatment) %>%
  summarise(Mean.ALT.increase.percent = mean(ALT.increase.percent, na.rm = TRUE),
            SE.ALT.increase.percent = sd(ALT.increase.percent, na.rm = TRUE)/sqrt(n()))

Thawextract1 <- Thawextract %>%
  gather(key = Correction,
         value = ALT,
         ALT:ALT.corrected)

Thawextract.summary <- Thawextract1 %>%
  group_by(year, Treatment, Correction) %>%
  summarise(Mean.ALT = mean(ALT, na.rm = TRUE),
            SE.ALT = sd(ALT, na.rm = TRUE)/sqrt(n()))

Thawextract.summary2 <- Thawextract1 %>%
  group_by(year, plot, Treatment, Correction) %>%
  summarise(Mean.ALT = mean(ALT, na.rm = TRUE),
            SE.ALT = sd(ALT, na.rm = TRUE)/sqrt(n()))

Thawextract.summary3 <- Thawextract1 %>%
  mutate(Treatment = ifelse(Treatment == 'Control' | Treatment == 'Air Warming',
                            'Control',
                            'Warming'),
         ALT = -1*ALT) %>% 
  group_by(year, Treatment, Correction) %>%
  summarise(Mean.ALT = mean(ALT, na.rm = TRUE),
            SE.ALT = sd(ALT, na.rm = TRUE)/sqrt(n()))


Thawextract2 <- Thawextract1 %>%
  mutate(Treatment = ifelse(Treatment == 'Control' | Treatment == 'Air Warming',
                            'Control',
                            'Warming'),
         Group = paste0(year, Treatment, Correction))
  

#Graph correction by treatment all years
a <- ggplot(Thawextract.summary3, aes(x = Treatment)) +
  geom_point(aes(y = Mean.ALT, colour = Treatment, shape = Correction), size = 5) +
  geom_errorbar(aes(ymin = Mean.ALT-SE.ALT, ymax = Mean.ALT+SE.ALT, colour = Treatment), width = 0.2) +
  scale_y_continuous(name = 'Active Layer Thickness (cm)') +
  scale_colour_manual(breaks=c("Control", "Warming"),
                      values=c("#006666", "#CC3300")) +
  xlab(NULL) +
  facet_grid(.~year) +
  theme_few(base_size = 22)
a
# ggsave("~/School/NAU/Schuur Lab/GPS/Images/Corrected ALT 2.jpg", plot = a, height = 9, width = 10)

#AOV to test each difference in graph above
alt.aov <- aov(ALT ~ Group, Thawextract2)
summary(alt.aov)
coefficients(alt.aov)
TukeyHSD(alt.aov, "Group", conf.level = 0.95)




#Plot percent of ALT that correction changed it by all years
ggplot(Thawcorrectionpercent, aes(x = Treatment, colour = Treatment)) +
  geom_point(aes(y = Mean.ALT.increase.percent), size = 5) +
  geom_errorbar(aes(ymin = Mean.ALT.increase.percent-SE.ALT.increase.percent, ymax = Mean.ALT.increase.percent+SE.ALT.increase.percent), width = 0.2) +
  scale_y_continuous(name = 'Percent Increase') +
  scale_colour_manual(breaks=c("Control", "Warming"),
                      values=c("#006666", "#CC3300")) +
  xlab(NULL) +
  facet_grid(.~year) +
  theme_few(base_size = 22)

# ggsave("~/School/NAU/Schuur Lab/GPS/Images/Percent Increase.jpg", height = 9, width = 10)

#  Impact of correction by treatment
ggplot(Thaw2017extract.summary, aes(x = Treatment)) +
  geom_point(aes(y = Mean.ALT, colour = Treatment, shape = Correction), size = 5) +
  geom_errorbar(aes(ymin = Mean.ALT-SE.ALT, ymax = Mean.ALT+SE.ALT, colour = Treatment), width = 0.2) +
  scale_y_continuous(name = 'Active Layer Thickness (cm)') +
  scale_x_discrete(name = 'Treatment', limits = c('Control', 'Air Warming', 'Soil Warming', 'Air and Soil Warming')) +
  scale_colour_manual(breaks=c("Control","Air Warming", "Soil Warming", "Air and Soil Warming"),
                      values=c("#CC3300", "#33CC66", "#006666", "#FF9900")) +
  theme_few()
  
#ggsave("~/School/NAU/Schuur Lab/GPS/Images/Corrected ALT.jpg", height = 8, width = 6)

# Impact of correction by all plots
ggplot(Thaw2017extract, aes(x = plot)) +
  geom_point(aes(y = ALT, colour = Treatment, shape = Correction), size = 5) +
  scale_y_continuous(name = 'Active Layer Thickness (cm)') +
  scale_colour_manual(breaks=c("Control","Air Warming", "Soil Warming", "Air and Soil Warming"),
                      values=c("#CC3300", "#33CC66", "#006666", "#FF9900")) +
  scale_x_discrete(name = 'Plot', limits = c(1,2,3,4,5,6,7,8)) +
  facet_grid(~fence) +
  theme_few()

#ggsave("~/School/NAU/Schuur Lab/GPS/Images/Corrected ALT All Plots.jpg", height = 8, width = 12)

#Impact of correction by plot number

ggplot(Thaw2017extract.summary2, aes(x = plot)) +
  geom_point(aes(y = Mean.ALT, colour = Treatment, shape = Correction), size = 5) +
  geom_errorbar(aes(ymin = Mean.ALT-SE.ALT, ymax = Mean.ALT+SE.ALT, colour = Treatment), width = 0.2) +
  scale_y_continuous(name = 'Active Layer Thickness (cm)') +
  scale_colour_manual(breaks=c("Control","Air Warming", "Soil Warming", "Air and Soil Warming"),
                      values=c("#CC3300", "#33CC66", "#006666", "#FF9900")) +
  scale_x_discrete(name = 'Plot', limits = c(1,2,3,4,5,6,7,8)) +
  theme_few()

#ggsave("~/School/NAU/Schuur Lab/GPS/Images/Corrected ALT By Plot.jpg", height = 8, width = 10)

#Plot percent of ALT that correction changed it by
ggplot(Thaw2017correctionpercent, aes(x = plot)) +
  geom_point(aes(y = ALT.increase.percent, colour = Treatment), size = 5) +
  scale_y_continuous(name = 'Percent Increase') +
  scale_colour_manual(breaks=c("Control","Air Warming", "Soil Warming", "Air and Soil Warming"),
                      values=c("#CC3300", "#33CC66", "#006666", "#FF9900")) +
  scale_x_discrete(name = 'Plot', limits = c(1,2,3,4,5,6,7,8)) +
  facet_grid(~fence) +
  theme_few()

#ggsave("~/School/NAU/Schuur Lab/GPS/Images/Percent Increase.jpg", height = 8, width = 12)
