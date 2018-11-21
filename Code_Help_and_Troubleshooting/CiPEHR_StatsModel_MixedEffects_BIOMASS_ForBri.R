##########################################################################################
#         Random Effects Model used to test treatment and linear predictors              #
#   Developed with help of D. Sonderegger at NAU, and lab discussions                    #
# USE FOR: Biomass and 2009-2013 data set
#          Marguerite Mauritz Feb 2016                                                   #
#        (similar to Jan 2016, but including the analyses I really want)                 #
# ########################################################################################

# load required libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(nlme) # need nlme if doing repeated measures since only nlme allows autocorrelation matrices, lme4 is good for mixed models especially if they get more complicated
library(lme4)
library(lmerTest)
library(reshape)
library(dplyr)
# library(plyr)
library(scatterplot3d)
library(sjPlot)
# library(sjmisc)
library(arm)
library(MASS)
library(MuMIn) # contains AICc

# saving locations
statsoutput <- c("~/Desktop/SchuurLab/Projects/CiPEHR Seasonal Flux/StatsTables")

# import data
setwd("~/Desktop/SchuurLab/Projects/CiPEHR Seasonal Flux/data/stats")
flux.variables <- as.data.table(read.table("Flux_biomass_ald_wtd_gs_ngs_soil_hobo_chamb_2009_2013.csv",header=TRUE, sep=","))

# Prepare the data for stats
data2 <- flux.variables

# make unique variables for each nested unit for clarity, lme4 it will recognise the implicit nesting
data2$block <- as.factor(data2$block)

# make model with fence coded 1-2 and plot coded 1-4, then create unique names for each fence, WW or plot
data2$fence_2 <- ifelse(data2$fence == 1 | data2$fence == 3 | data2$fence == 5, "1", "2") 
data2$plot_4 <- ifelse(data2$plot == 1 | data2$plot == 5, "1",
                       ifelse(data2$plot == 2 | data2$plot == 6, "2",
                              ifelse(data2$plot == 3 | data2$plot == 7, "3","4")))


data2$fence_2 <- as.factor(data2$fence_2)
data2$plot <- as.factor(data2$plot)
data2$plot_4 <- as.factor(data2$plot_4)

# create variable for random structure
data2 <- within (data2, {
  wholeplot <- factor(block:fence_2:WW)
  # SWplot <- factor(block:fence_2:WW:SW) # this is in case I want to nest SW
  subplot4 <- factor(block:fence_2:WW:plot_4)
})

# stats lab consultation with Derek Sonderegger:
# For repeated measures
# make time a continuous variable to put in main effects (to test year effect) 
# and make year a categorical variable and include in random effects

# center time on 0 so that 2009 is used as the reference year
baseyear <- min(data2$year)
data2$time <- as.numeric(data2$year - baseyear)

# make year a factor:
# categorical variable to put in random structure (time will be continuous as fixed effect),
# year effect will account for the offset from the overall trend throught time
data2$year <- as.factor(data2$year)

# reorder levels of time and treatment in the data to make sure that control in year 2009 is the reference
# for the intercept
print(levels(data2$treatment))
print(levels(factor(data2$time)))

data2$treatment <- relevel(data2$treatment, ref="Control")
print(levels(data2$treatment))

# add labels for graphing
data2 <- data2[,labels:=factor(treatment,levels=c("Control","Air","Soil","Air & Soil"))]

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

# INITAL SET UP RUN, STOP HERE

# look at histograms for the variables
# total biomass
hist_totalbiomass <- ggplot(data2, aes(x=total_biomass, fill=WW))+geom_density(alpha=0.3)+facet_grid(.~SW)+labs(title="totalinoid Biomass")
hist_totalbiomass_log <- ggplot(data2, aes(x=log10(total_biomass+0.1), fill=WW))+geom_density(alpha=0.3)+facet_grid(.~SW)+labs(title="Log totalinoid Biomass")
grid.arrange(hist_totalbiomass, hist_totalbiomass_log)

# make figure to show what the model is testing: 
# how does soil warming affect the change over time?
# (pretend that air warming doesn't exist, and group only by soil warming)
# in data2 time is a continuous variable, year is a categorical variable 
ggplot(subset(data2), aes(time, total_biomass, colour=WW))+
  geom_point()+
  geom_smooth(method='lm') # adds linear regression line, this is NOT a mixed effects model line, don't use to display mixed effects model results

# see the response of each individual plot
ggplot(data2, aes(time, total_biomass, colour=subplot4, shape=WW))+
  geom_point()+
  geom_smooth(method='lm', alpha=0) # adds linear regression line, this is NOT a mixed effects model line, don't use to display mixed effects model results

# and on log scale
ggplot(subset(data2), aes(time, log10(total_biomass+0.1), colour=WW))+
  geom_point()+
  geom_smooth(method='lm') # adds linear regression line, this is NOT a mixed effects model line, don't use to display mixed effects model results


# define model to test a linear trend through time and treatment effect. 
# random effect is year, to account for year to year fluctuation, and nested plot
# these are random INTERCEPT models. 
model.biomass.log <- lmer(log10(total_biomass+0.1) ~ time*WW  + # use * or + depending on the interactions you want to specify
                             (1 | block/wholeplot/WW/subplot4) + (1|year), 
                           data = data2,
                          control=lmerControl(check.conv.singular="warning"))


model.biomass <- lmer(total_biomass ~ time*WW  + # use * or + depending on the interactions you want to specify
                            (1 | block/wholeplot/WW/subplot4) + (1|year), 
                          data = data2,
                          control=lmerControl(check.conv.singular="warning"))

# both models fail to converge. look at them. can I remove a random effect?
summary(model.biomass.log) # the variance in block, fence, WW = 0. try removing them one by one. 
summary(model.biomass) # the variance in block, fence, WW = 0. try removing them one by one.  

# rerun with different RE
model.biomass.log2 <- lmer(log10(total_biomass+0.1) ~ time*WW  + # use * or + depending on the interactions you want to specify
                            (1 | subplot4) + (1|year), 
                          data = data2,
                          control=lmerControl(check.conv.singular="warning"))

# try random slope model
model.biomass.log.slope <- lmer(log10(total_biomass+0.1) ~ time*WW  + # use * or + depending on the interactions you want to specify
                             (time|subplot4) + (1|year), 
                           data = data2,
                           control=lmerControl(check.conv.singular="warning"))
# compare the fit of the random intercept vs random slope model
# more complex model is justified if AIC is improved by >5 points
# if AIC of random slope model is 5 less, then use that. 
AICc(model.biomass.log2, model.biomass.log.slope)
# df       AIC
# df     AICc
# model.biomass.log2       7 -368.309
# model.biomass.log.slope  9 -398.307 # use random slope model!

model.biomass2 <- lmer(total_biomass ~ year*WW  + # use * or + depending on the interactions you want to specify
                        (1 | subplot4) + (1|year), 
                      data = data2,
                      control=lmerControl(check.conv.singular="warning"))

model.biomass2.slope <- lmer(total_biomass ~ year*WW  + # use * or + depending on the interactions you want to specify
                               (time|subplot4) + (1|year), 
                       data = data2,
                       control=lmerControl(check.conv.singular="warning"))

# check model residuals of log biomass
# look at residuals
model.log.resid <- resid(model.biomass.log.slope)
model.log.fitted <- fitted(model.biomass.log.slope)
model.log.sqrt <- sqrt(abs(resid(model.biomass.log.slope)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(model.log.fitted, model.log.resid, main='resid, model.biomass log')
plot(model.log.fitted, model.log.sqrt, main='sqrt resid, model.biomass log')
qqnorm(model.log.resid, main = 'model.biomass log')
qqline(model.log.resid)

par(mfrow=c(1,1))


# check model residuals of non-log
# look at residuals
model.resid <- resid(model.biomass2)
model.fitted <- fitted(model.biomass2)
model.sqrt <- sqrt(abs(resid(model.biomass2)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(model.fitted, model.resid, main='resid, model.biomass')
plot(model.fitted, model.sqrt, main='sqrt resid, model.biomass')
qqnorm(model.resid, main = 'model.biomass')
qqline(model.resid)

par(mfrow=c(1,1))

# log model residuals still have slightly unequal variance but are MUCH better. Keep log model
# calculate confidence intervals to look at fixed effects
biomass_log_ci <- extract_ci(model.biomass.log.slope)

biomass_log_ci$names <- as.character(biomass_log_ci$term)
  
graph_ci(biomass_log_ci, "Biomass", model.biomass.log.slope)

biomass_log_ci$names<-c("Intercept","time","soil * time","soil")
#fix order of names so graph plots them in the same order regardless of the values
biomass_log_ci$names<-(factor(biomass_log_ci$names,levels=c("Intercept","soil", "time", "soil * time")))

biomass_log_ci <- biomass_log_ci[order(biomass_log_ci$names),]

graph_ci(biomass_log_ci, "Log Biomass, random slope", model.biomass.log.slope)

# save the file with confidence intervals so the stats results are stored.
setwd(statsoutput)
# write.table(biomass_log_ci, "CI_Biomass_time.csv",sep=",", dec=".", row.names=FALSE)

