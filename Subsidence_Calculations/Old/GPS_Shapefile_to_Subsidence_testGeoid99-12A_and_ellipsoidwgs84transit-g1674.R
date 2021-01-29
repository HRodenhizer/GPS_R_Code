########################################################################################
###                GPS shapefile processing for subsidence                           ###
###                 Code by HGR 2017                                                 ###
########################################################################################

#Required packages to run the following code
#library(sp)
#library(rgdal)
#library(raster)
#library(dplyr)
#library(stringr)

###Required packages to do this with tidyverse
library(sf)
library(tidyverse)
library(viridis)
library(rvest)
library(ggplot2)
library(readxl)
library(sp)
library(rgdal)
library(gstat)
library(raster)
library(ggmap)
library(rasterVis)
library(ggthemes)
library(ggsn)
library(gridExtra)
library(cowplot)

##############################Load files##############################

#Points2017 <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/2017 GPS/Analyses"),
#                      layer = "All_Points_2017_SPCSAK4",
#                      stringsAsFactors = FALSE,
#                      drop_unsupported_fields = TRUE)

Points2009 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2009_SPCSAK4.shp")
Points2011 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2011_SPCSAK4.shp")
Points2015 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2015_SPCSAK4.shp")
Points2016 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2016_SPCSAK4.shp")
Points2017 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp")
Points2018 <- st_read("~/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2018_SPCSAK4.shp")


#Load ALT File
ALT2017 <- read_excel(path = "~/School/NAU/Schuur Lab/GPS/2017 GPS/ALT_Measurments.xlsx")

###Make name field in ALT character to match Points2017 name field
ALT2017 <- ALT2017 %>%
  mutate(Name = as.character(Name))

###Join ALT to Points2017 attributes
#Points2017@data <- Points2017@data %>%
#  inner_join(ALT, by = "Name") %>%
#  mutate(Name = as.numeric(Name))

Grids2017 <- Points2017 %>%
  mutate(Name = as.character(Name)) %>%
  inner_join(ALT2017, by = 'Name') %>%
  mutate(Name = as.numeric(Name))

Grids2018 <- Points2018 %>%
  mutate(Name = as.character(Name)) %>%
  mutate(Name = as.numeric(Name)) %>%
  filter(Name != is.na(Name)) %>%
  arrange(Name)

##############################Select data by block##############################
#Note that this turns the data into a dataframe rather than sf object
#A2017 <- Points2017[Points2017@data$Name > 10000 & Points2017@data$Name < 11000,] #why is this finding a lot of non-block A entries?
#A2017 <- subset(Points2017, Points2017$Name > 10000 & Points2017$Name < 11000)
#B2017 <- Points2017[Points2017$Name > 11000 & Points2017$Name < 12000,]
#C2017 <- Points2017[Points2017$Name > 12000 & Points2017$Name < 13000,]


#2009
A2009 <- Points2009 %>% filter(type == 'trans' & fence == 1 | type == 'trans' & fence == 2)
B2009 <- Points2009 %>% filter(type == 'trans' & fence == 3 | type == 'trans' & fence == 4)
C2009 <- Points2009 %>% filter(type == 'trans' & fence == 5 | type == 'trans' & fence == 6)


#2011
A2011 <- Points2011 %>% filter(type == 'a')
B2011 <- Points2011 %>% filter(type == 'b')
C2011 <- Points2011 %>% filter(type == 'c')


#2015
A2015 <- Points2015 %>% filter(group_ == 'a')
B2015 <- Points2015 %>% filter(group_ == 'b')
C2015 <- Points2015 %>% filter(group_ == 'c')


#2016
A2016 <- Points2016 %>% filter(FeatureCod == 'TS' | FeatureCod == 'TS1' | FeatureCod == 'TS2')
B2016 <- Points2016 %>% filter(FeatureCod == 'TS3' | FeatureCod == 'TS4')
C2016 <- Points2016 %>% filter(FeatureCod == 'TS5' | FeatureCod == 'TS6')


#2017
A2017 <- Grids2017 %>% filter(Name > 10000 & Name < 11000)
B2017 <- Grids2017 %>% filter(Name > 11000 & Name < 12000)
C2017 <- Grids2017 %>% filter(Name > 12000 & Name < 13000)
EC2017 <- Grids2017 %>% filter(Name > 13000 & Name < 14000)

#2018
A2018 <- Grids2018 %>% filter(Name > 10000 & Name < 11000)
B2018 <- Grids2018 %>% filter(Name > 11000 & Name < 12000)
C2018 <- Grids2018 %>% filter(Name > 12000 & Name < 13000)
EC2018 <- Grids2018 %>% filter(Name > 13000 & Name < 14000)

##############Add coordinate columns (not part of geometry) to sf objects, so that when converted to sp that info will be available for kriging#######################
A2009sp <- A2009 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2009)
B2009sp <- B2009 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2009)
C2009sp <- C2009 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2009)
A2011sp <- A2011 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2011)
B2011sp <- B2011 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2011)
C2011sp <- C2011 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2011)
A2015sp <- A2015 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2015)
B2015sp <- B2015 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2015)
C2015sp <- C2015 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2015)
A2016sp <- A2016 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2016)
B2016sp <- B2016 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2016)
C2016sp <- C2016 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2016)
A2017sp <- A2017 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2017)
B2017sp <- B2017 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2017)
C2017sp <- C2017 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2017)
EC2017sp <- C2017 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2017)
A2018sp <- A2018 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2018)
B2018sp <- B2018 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2018)
C2018sp <- C2018 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2018)

##############################Plot to make sure subset worked properly##############################
#plot(A2017$Easting, A2017$Northing)
#plot(B2017$Easting, B2017$Northing)
#plot(C2017$Easting, C2017$Northing)
#plot(EC2017$Easting, EC2017$Northing)


#2009
ggplot(A2009) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(B2009) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(C2009) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")


#2011
ggplot(A2011) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(B2011) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(C2011) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")


#2015
ggplot(A2015) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(B2015) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(C2015) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")


#2016
ggplot(A2016) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(B2016) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(C2016) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")


#2017
ggplot(A2017) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(B2017) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(C2017) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(EC2017) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#2018
ggplot(A2018) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(B2018) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

ggplot(C2018) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

##############################Interpolate a surface using kriging##############################
# To convert to sp, make sure the sf object only has x and y columns (z and m cause problems with conversion to sp and kriging as of 4/2018)
A2016sp <- st_zm(A2016sp, drop = TRUE, what = "ZM")
B2016sp <- st_zm(B2016sp, drop = TRUE, what = "ZM")
C2016sp <- st_zm(C2016sp, drop = TRUE, what = "ZM")
A2017sp <- st_zm(A2017sp, drop = TRUE, what = "ZM")
B2017sp <- st_zm(B2017sp, drop = TRUE, what = "ZM")
C2017sp <- st_zm(C2017sp, drop = TRUE, what = "ZM")
A2018sp <- st_zm(A2018sp, drop = TRUE, what = "ZM")
B2018sp <- st_zm(B2018sp, drop = TRUE, what = "ZM")
C2018sp <- st_zm(C2018sp, drop = TRUE, what = "ZM")

# Manual method to get rid of only z - however, kriging can't handle z, so I didn't end up using this method. Previous section is what I used.
# temp <- st_coordinates(A2016sp)[,1:3]
# temp <- temp %>%
#   as.data.frame() %>%
#   st_as_sf(coords = c("X", "Y", "Z")) %>%
#   st_geometry()
# st_geometry(A2016sp) <- temp
# st_crs(A2016sp) <- st_crs(A2016)
# 
# temp <- st_coordinates(B2016sp)[,1:3]
# temp <- temp %>%
#   as.data.frame() %>%
#   st_as_sf(coords = c("X", "Y", "Z")) %>%
#   st_geometry()
# st_geometry(B2016sp) <- temp
# st_crs(B2016sp) <- st_crs(B2016)
# 
# temp <- st_coordinates(C2016sp)[,1:3]
# temp <- temp %>%
#   as.data.frame() %>%
#   st_as_sf(coords = c("X", "Y", "Z")) %>%
#   st_geometry()
# st_geometry(C2016sp) <- temp
# st_crs(C2016sp) <- st_crs(C2016)
# 
# temp <- st_coordinates(A2017sp)[,1:3]
# temp <- temp %>%
#   as.data.frame() %>%
#   st_as_sf(coords = c("X", "Y", "Z")) %>%
#   st_geometry()
# st_geometry(A2017sp) <- temp
# st_crs(A2017sp) <- st_crs(A2017)
# 
# temp <- st_coordinates(B2017sp)[,1:3]
# temp <- temp %>%
#   as.data.frame() %>%
#   st_as_sf(coords = c("X", "Y", "Z")) %>%
#   st_geometry()
# st_geometry(B2017sp) <- temp
# st_crs(B2017sp) <- st_crs(B2017)
# 
# temp <- st_coordinates(C2017sp)[,1:3]
# temp <- temp %>%
#   as.data.frame() %>%
#   st_as_sf(coords = c("X", "Y", "Z")) %>%
#   st_geometry()
# st_geometry(C2017sp) <- temp
# st_crs(C2017sp) <- st_crs(C2017)

#First, convert to an sp object rather than sf
A2009sp <- as(A2009sp, 'Spatial')
B2009sp <- as(B2009sp, 'Spatial')
C2009sp <- as(C2009sp, 'Spatial')
A2011sp <- as(A2011sp, 'Spatial')
B2011sp <- as(B2011sp, 'Spatial')
C2011sp <- as(C2011sp, 'Spatial')
A2015sp <- as(A2015sp, 'Spatial')
B2015sp <- as(B2015sp, 'Spatial')
C2015sp <- as(C2015sp, 'Spatial')
A2016sp <- as(A2016sp, 'Spatial')
B2016sp <- as(B2016sp, 'Spatial')
C2016sp <- as(C2016sp, 'Spatial')
A2017sp <- as(A2017sp, 'Spatial')
B2017sp <- as(B2017sp, 'Spatial')
C2017sp <- as(C2017sp, 'Spatial')
A2018sp <- as(A2018sp, 'Spatial')
B2018sp <- as(B2018sp, 'Spatial')
C2018sp <- as(C2018sp, 'Spatial')



# #Graph points on google maps background - not working, maybe I'll figure this out later...
# A2017WGS84 <- spTransform(A2017sp, sp::CRS("+proj=longlat +datum=WGS84"))
# B2017WGS84 <- spTransform(B2017sp, sp::CRS("+proj=longlat +datum=WGS84"))
# C2017WGS84 <- spTransform(C2017sp, sp::CRS("+proj=longlat +datum=WGS84"))
# 
# qmap(location = c(-149.213, 63.885), zoom = 15, maptype = "satellite") +
#   geom_point(data = A2017WGS84, aes(x = X, y= Y, color = "Z"))


##############################Calculate and fit variograms##############################
#A2009
A2009.vgm <- variogram(Elevation~X+Y, A2009sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.0045, "Exp", 3, 0, anis =  c(0, 0.7))
A2009.fit <- fit.variogram(A2009.vgm, model = vgm.anis)
plot(A2009.vgm, vgm.anis)

A2009x <- seq(min(A2009sp@coords[,1]), max(A2009sp@coords[,1]), length.out = (max(A2009sp@coords[,1])-min(A2009sp@coords[,1]))/2)
A2009y <- seq(min(A2009sp@coords[,2]), max(A2009sp@coords[,2]), length.out = (max(A2009sp@coords[,2])-min(A2009sp@coords[,2]))/2)
A2009grid <- expand.grid(x = A2009x, y = A2009y)
gridded(A2009grid) = ~x+y
A2009grid@proj4string<-CRS(st_crs(Points2009)$proj4string)
plot(A2009grid)

A2009k <-  krige(Elevation~1, A2009sp, A2009grid, A2009.fit)
spplot(A2009k["var1.pred"], main = "ordinary kriging predictions")

ggplot(A2009) +
  geom_tile(data = as.data.frame(A2009k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")



#B2009
B2009.vgm <- variogram(Elevation~X+Y, B2009sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.009, "Exp", 10, 0.001, anis =  c(0, 0.5))
B2009.fit <- fit.variogram(B2009.vgm, model = vgm.anis)
plot(B2009.vgm, vgm.anis)

B2009x <- seq(min(B2009sp@coords[,1]), max(B2009sp@coords[,1]), length.out = (max(B2009sp@coords[,1])-min(B2009sp@coords[,1]))/2)
B2009y <- seq(min(B2009sp@coords[,2]), max(B2009sp@coords[,2]), length.out = (max(B2009sp@coords[,2])-min(B2009sp@coords[,2]))/2)
B2009grid <- expand.grid(x = B2009x, y = B2009y)
gridded(B2009grid) = ~x+y
B2009grid@proj4string<-CRS(st_crs(Points2009)$proj4string)
plot(B2009grid)

B2009k <-  krige(Elevation~1, B2009sp, B2009grid, B2009.fit)
spplot(B2009k["var1.pred"], main = "ordinary kriging predictions")

ggplot(B2009) +
  geom_tile(data = as.data.frame(B2009k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#C2009
# C2009.vgm <- variogram(Elevation~X+Y, C2009sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.005, "Exp", 7, 0.001, anis =  c(45, 0.3))
# C2009.fit <- fit.variogram(C2009.vgm, model = vgm.anis)
# plot(C2009.vgm, vgm.anis)

C2009.vgm <- variogram(Elevation~X+Y, C2009sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.004, "Exp", 3, 0.002, anis =  c(0, 0.9))
C2009.fit <- fit.variogram(C2009.vgm, model = vgm.anis)
plot(C2009.vgm, vgm.anis)

C2009x <- seq(min(C2009sp@coords[,1]), max(C2009sp@coords[,1]), length.out = (max(C2009sp@coords[,1])-min(C2009sp@coords[,1]))/2)
C2009y <- seq(min(C2009sp@coords[,2]), max(C2009sp@coords[,2]), length.out = (max(C2009sp@coords[,2])-min(C2009sp@coords[,2]))/2)
C2009grid <- expand.grid(x = C2009x, y = C2009y)
gridded(C2009grid) = ~x+y
C2009grid@proj4string<-CRS(st_crs(Points2009)$proj4string)
plot(C2009grid)

C2009k <-  krige(Elevation~1, C2009sp, C2009grid, C2009.fit)
spplot(C2009k["var1.pred"], main = "ordinary kriging predictions")

ggplot(C2009) +
  geom_tile(data = as.data.frame(C2009k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#A2011
# A2011.vgm <- variogram(Elevation~X+Y, A2011sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.009, "Exp", 10, 0, anis =  c(45, 0.3))
# A2011.fit <- fit.variogram(A2011.vgm, model = vgm.anis)
# plot(A2011.vgm, vgm.anis)

A2011.vgm <- variogram(Elevation~X+Y, A2011sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.007, "Exp", 4, 0.001, anis =  c(90, 0.9))
A2011.fit <- fit.variogram(A2011.vgm, model = vgm.anis)
plot(A2011.vgm, vgm.anis)

A2011x <- seq(min(A2011sp@coords[,1]), max(A2011sp@coords[,1]), length.out = (max(A2011sp@coords[,1])-min(A2011sp@coords[,1]))/2)
A2011y <- seq(min(A2011sp@coords[,2]), max(A2011sp@coords[,2]), length.out = (max(A2011sp@coords[,2])-min(A2011sp@coords[,2]))/2)
A2011grid <- expand.grid(x = A2011x, y = A2011y)
gridded(A2011grid) = ~x+y
A2011grid@proj4string<-CRS(st_crs(Points2011)$proj4string)
plot(A2011grid)

A2011k <-  krige(Elevation~1, A2011sp, A2011grid, A2011.fit)
spplot(A2011k["var1.pred"], main = "ordinary kriging predictions")

ggplot(A2011) +
  geom_tile(data = as.data.frame(A2011k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#B2011
# B2011.vgm <- variogram(Elevation~X+Y, B2011sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.006, "Exp", 10, 0, anis =  c(45, 0.3))
# B2011.fit <- fit.variogram(B2011.vgm, model = vgm.anis)
# plot(B2011.vgm, vgm.anis)

B2011.vgm <- variogram(Elevation~X+Y, B2011sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.005, "Exp", 6, 0.001, anis =  c(0, 0.7))
B2011.fit <- fit.variogram(B2011.vgm, model = vgm.anis)
plot(B2011.vgm, vgm.anis)

B2011x <- seq(min(B2011sp@coords[,1]), max(B2011sp@coords[,1]), length.out = (max(B2011sp@coords[,1])-min(B2011sp@coords[,1]))/2)
B2011y <- seq(min(B2011sp@coords[,2]), max(B2011sp@coords[,2]), length.out = (max(B2011sp@coords[,2])-min(B2011sp@coords[,2]))/2)
B2011grid <- expand.grid(x = B2011x, y = B2011y)
gridded(B2011grid) = ~x+y
B2011grid@proj4string<-CRS(st_crs(Points2011)$proj4string)
plot(B2011grid)

B2011k <-  krige(Elevation~1, B2011sp, B2011grid, B2011.fit)
spplot(B2011k["var1.pred"], main = "ordinary kriging predictions")

ggplot(B2011) +
  geom_tile(data = as.data.frame(B2011k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#C2011
# C2011.vgm <- variogram(Elevation~X+Y, C2011sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.006, "Exp", 5, 0, anis =  c(45, 0.3))
# C2011.fit <- fit.variogram(C2011.vgm, model = vgm.anis)
# plot(C2011.vgm, vgm.anis)

C2011.vgm <- variogram(Elevation~X+Y, C2011sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.004, "Exp", 4, 0.002, anis =  c(90, 0.9))
C2011.fit <- fit.variogram(C2011.vgm, model = vgm.anis)
plot(C2011.vgm, vgm.anis)

C2011x <- seq(min(C2011sp@coords[,1]), max(C2011sp@coords[,1]), length.out = (max(C2011sp@coords[,1])-min(C2011sp@coords[,1]))/2)
C2011y <- seq(min(C2011sp@coords[,2]), max(C2011sp@coords[,2]), length.out = (max(C2011sp@coords[,2])-min(C2011sp@coords[,2]))/2)
C2011grid <- expand.grid(x = C2011x, y = C2011y)
gridded(C2011grid) = ~x+y
C2011grid@proj4string<-CRS(st_crs(Points2011)$proj4string)
plot(C2011grid)

C2011k <-  krige(Elevation~1, C2011sp, C2011grid, C2011.fit)
spplot(C2011k["var1.pred"], main = "ordinary kriging predictions")

ggplot(C2011) +
  geom_tile(data = as.data.frame(C2011k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#A2015
# A2015.vgm <- variogram(Elevation~X+Y, A2015sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.025, "Exp", 10, 0, anis =  c(45, 0.3))
# A2015.fit <- fit.variogram(A2015.vgm, model = vgm.anis)
# plot(A2015.vgm, vgm.anis)

A2015.vgm <- variogram(Elevation~X+Y, A2015sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.025, "Exp", 7, 0, anis =  c(90, 0.7))
A2015.fit <- fit.variogram(A2015.vgm, model = vgm.anis)
plot(A2015.vgm, vgm.anis)

A2015x <- seq(min(A2015sp@coords[,1]), max(A2015sp@coords[,1]), length.out = (max(A2015sp@coords[,1])-min(A2015sp@coords[,1]))/2)
A2015y <- seq(min(A2015sp@coords[,2]), max(A2015sp@coords[,2]), length.out = (max(A2015sp@coords[,2])-min(A2015sp@coords[,2]))/2)
A2015grid <- expand.grid(x = A2015x, y = A2015y)
gridded(A2015grid) = ~x+y
A2015grid@proj4string<-CRS(st_crs(Points2015)$proj4string)
plot(A2015grid)

A2015k <-  krige(Elevation~1, A2015sp, A2015grid, A2015.fit)
spplot(A2015k["var1.pred"], main = "ordinary kriging predictions")

ggplot(A2015) +
  geom_tile(data = as.data.frame(A2015k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#B2015
# B2015.vgm <- variogram(Elevation~X+Y, B2015sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.03, "Exp", 10, 0, anis =  c(45, 0.3))
# B2015.fit <- fit.variogram(B2015.vgm, model = vgm.anis)
# plot(B2015.vgm, vgm.anis)

B2015.vgm <- variogram(Elevation~X+Y, B2015sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.023, "Exp", 4, 0.002, anis =  c(0, 0.8))
B2015.fit <- fit.variogram(B2015.vgm, model = vgm.anis)
plot(B2015.vgm, vgm.anis)

B2015x <- seq(min(B2015sp@coords[,1]), max(B2015sp@coords[,1]), length.out = (max(B2015sp@coords[,1])-min(B2015sp@coords[,1]))/2)
B2015y <- seq(min(B2015sp@coords[,2]), max(B2015sp@coords[,2]), length.out = (max(B2015sp@coords[,2])-min(B2015sp@coords[,2]))/2)
B2015grid <- expand.grid(x = B2015x, y = B2015y)
gridded(B2015grid) = ~x+y
B2015grid@proj4string<-CRS(st_crs(Points2015)$proj4string)
plot(B2015grid)

B2015k <-  krige(Elevation~1, B2015sp, B2015grid, B2015.fit)
spplot(B2015k["var1.pred"], main = "ordinary kriging predictions")

ggplot(B2015) +
  geom_tile(data = as.data.frame(B2015k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#C2015
# C2015.vgm <- variogram(Elevation~X+Y, C2015sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.015, "Exp", 8, 0, anis =  c(45, 0.3))
# C2015.fit <- fit.variogram(C2015.vgm, model = vgm.anis)
# plot(C2015.vgm, vgm.anis)

C2015.vgm <- variogram(Elevation~X+Y, C2015sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.011, "Exp", 3, 0.002, anis =  c(90, 0.7))
C2015.fit <- fit.variogram(C2015.vgm, model = vgm.anis)
plot(C2015.vgm, vgm.anis)


C2015x <- seq(min(C2015sp@coords[,1]), max(C2015sp@coords[,1]), length.out = (max(C2015sp@coords[,1])-min(C2015sp@coords[,1]))/2)
C2015y <- seq(min(C2015sp@coords[,2]), max(C2015sp@coords[,2]), length.out = (max(C2015sp@coords[,2])-min(C2015sp@coords[,2]))/2)
C2015grid <- expand.grid(x = C2015x, y = C2015y)
gridded(C2015grid) = ~x+y
C2015grid@proj4string<-CRS(st_crs(Points2015)$proj4string)
plot(C2015grid)

C2015k <-  krige(Elevation~1, C2015sp, C2015grid, C2015.fit)
spplot(C2015k["var1.pred"], main = "ordinary kriging predictions")

ggplot(C2015) +
  geom_tile(data = as.data.frame(C2015k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#A2016
# A2016.vgm <- variogram(Z~X+Y, A2016sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.03, "Exp", 10, 0, anis =  c(45, 0.3))
# A2016.fit <- fit.variogram(A2016.vgm, model = vgm.anis)
# plot(A2016.vgm, vgm.anis)

A2016.vgm <- variogram(Z~X+Y, A2016sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.025, "Exp", 4.5, 0, anis =  c(90, 0.7))
A2016.fit <- fit.variogram(A2016.vgm, model = vgm.anis)
plot(A2016.vgm, vgm.anis)

A2016x <- seq(min(A2016sp@coords[,1]), max(A2016sp@coords[,1]), length.out = (max(A2016sp@coords[,1])-min(A2016sp@coords[,1]))/2)
A2016y <- seq(min(A2016sp@coords[,2]), max(A2016sp@coords[,2]), length.out = (max(A2016sp@coords[,2])-min(A2016sp@coords[,2]))/2)
A2016grid <- expand.grid(x = A2016x, y = A2016y)
gridded(A2016grid) = ~x+y
A2016grid@proj4string<-CRS(st_crs(Points2016)$proj4string)
plot(A2016grid)

A2016k <-  krige(Elevation~1, A2016sp, A2016grid, A2016.fit)
spplot(A2016k["var1.pred"], main = "ordinary kriging predictions")

ggplot(A2016) +
  geom_tile(data = as.data.frame(A2016k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#B2016
# B2016.vgm <- variogram(Z~X+Y, B2016sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.04, "Exp", 10, 0, anis =  c(45, 0.3))
# B2016.fit <- fit.variogram(B2016.vgm, model = vgm.anis)
# plot(B2016.vgm)

B2016.vgm <- variogram(Z~X+Y, B2016sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.035, "Exp", 3.5, 0, anis =  c(90, 1))
B2016.fit <- fit.variogram(B2016.vgm, model = vgm.anis)
plot(B2016.vgm, vgm.anis)

B2016x <- seq(min(B2016sp@coords[,1]), max(B2016sp@coords[,1]), length.out = (max(B2016sp@coords[,1])-min(B2016sp@coords[,1]))/2)
B2016y <- seq(min(B2016sp@coords[,2]), max(B2016sp@coords[,2]), length.out = (max(B2016sp@coords[,2])-min(B2016sp@coords[,2]))/2)
B2016grid <- expand.grid(x = B2016x, y = B2016y)
gridded(B2016grid) = ~x+y
B2016grid@proj4string<-CRS(st_crs(Points2016)$proj4string)
plot(B2016grid)

B2016k <-  krige(Elevation~1, B2016sp, B2016grid, B2016.fit)
spplot(B2016k["var1.pred"], main = "ordinary kriging predictions")

ggplot(B2016) +
  geom_tile(data = as.data.frame(B2016k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#C2016
# C2016.vgm <- variogram(Z~X+Y, C2016sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.02, "Exp", 10, 0, anis =  c(45, 0.3))
# C2016.fit <- fit.variogram(C2016.vgm, model = vgm.anis)
# plot(C2016.vgm, vgm.anis)

C2016.vgm <- variogram(Z~X+Y, C2016sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.018, "Exp", 4, 0, anis =  c(90, 0.7))
C2016.fit <- fit.variogram(C2016.vgm, model = vgm.anis)
plot(C2016.vgm, vgm.anis)

C2016x <- seq(min(C2016sp@coords[,1]), max(C2016sp@coords[,1]), length.out = (max(C2016sp@coords[,1])-min(C2016sp@coords[,1]))/2)
C2016y <- seq(min(C2016sp@coords[,2]), max(C2016sp@coords[,2]), length.out = (max(C2016sp@coords[,2])-min(C2016sp@coords[,2]))/2)
C2016grid <- expand.grid(x = C2016x, y = C2016y)
gridded(C2016grid) = ~x+y
C2016grid@proj4string<-CRS(st_crs(Points2016)$proj4string)
plot(C2016grid)

C2016k <-  krige(Elevation~1, C2016sp, C2016grid, C2016.fit)
spplot(C2016k["var1.pred"], main = "ordinary kriging predictions")

ggplot(C2016) +
  geom_tile(data = as.data.frame(C2016k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#A2017
# A2017.vgm <- variogram(Z~X+Y, A2017sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.025, "Exp", 14, 0, anis =  c(45, 0.3))
# A2017.fit <- fit.variogram(A2017.vgm, model = vgm.anis)
# plot(A2017.vgm, vgm.anis)

A2017.vgm <- variogram(Z~X+Y, A2017sp, alpha = c(0:1)*90)
plot(A2017.vgm)
vgm.anis = vgm(0.025, "Exp", 8, 0, anis =  c(90, 0.7))
A2017.fit <- fit.variogram(A2017.vgm, model = vgm.anis)
plot(A2017.vgm, vgm.anis)
plot(A2017.vgm, A2017.fit)

A2017x <- seq(min(A2017sp@coords[,1]), max(A2017sp@coords[,1]), length.out = (max(A2017sp@coords[,1])-min(A2017sp@coords[,1]))/2)
A2017y <- seq(min(A2017sp@coords[,2]), max(A2017sp@coords[,2]), length.out = (max(A2017sp@coords[,2])-min(A2017sp@coords[,2]))/2)
A2017grid <- expand.grid(x = A2017x, y = A2017y)
gridded(A2017grid) = ~x+y
A2017grid@proj4string<-CRS(st_crs(Points2017)$proj4string)
plot(A2017grid)

A2017k <-  krige(Elevation~1, A2017sp, A2017grid, A2017.fit)
spplot(A2017k["var1.pred"], main = "ordinary kriging predictions")

ggplot() +
  geom_tile(data = as.data.frame(A2017k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  scale_fill_viridis("Elevation")

ggplot(A2017) +
  geom_tile(data = as.data.frame(A2017k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#B2017
# B2017.vgm <- variogram(Z~X+Y, B2017sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.03, "Exp", 10, 0, anis =  c(45, 0.3))
# B2017.fit <- fit.variogram(B2017.vgm, model = vgm.anis)
# plot(B2017.vgm, vgm.anis)

B2017.vgm <- variogram(Z~X+Y, B2017sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.03, "Exp", 5, 0, anis =  c(0, 0.95))
B2017.fit <- fit.variogram(B2017.vgm, model = vgm.anis)
plot(B2017.vgm, vgm.anis)

B2017x <- seq(min(B2017sp@coords[,1]), max(B2017sp@coords[,1]), length.out = (max(B2017sp@coords[,1])-min(B2017sp@coords[,1]))/2)
B2017y <- seq(min(B2017sp@coords[,2]), max(B2017sp@coords[,2]), length.out = (max(B2017sp@coords[,2])-min(B2017sp@coords[,2]))/2)
B2017grid <- expand.grid(x = B2017x, y = B2017y)
gridded(B2017grid) = ~x+y
B2017grid@proj4string<-CRS(st_crs(Points2017)$proj4string)
plot(B2017grid)

B2017k <-  krige(Elevation~1, B2017sp, B2017grid, B2017.fit)
spplot(B2017k["var1.pred"], main = "ordinary kriging predictions")

ggplot(B2017) +
  geom_tile(data = as.data.frame(B2017k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#C2017
# C2017.vgm <- variogram(Z~X+Y, C2017sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.02, "Exp", 12, 0, anis =  c(45, 0.3))
# C2017.fit <- fit.variogram(C2017.vgm, model = vgm.anis)
# plot(C2017.vgm, vgm.anis)

C2017.vgm <- variogram(Z~X+Y, C2017sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.02, "Exp", 5.5, 0, anis =  c(0, 0.95))
C2017.fit <- fit.variogram(C2017.vgm, model = vgm.anis)
plot(C2017.vgm, vgm.anis)

C2017x <- seq(min(C2017sp@coords[,1]), max(C2017sp@coords[,1]), length.out = (max(C2017sp@coords[,1])-min(C2017sp@coords[,1]))/2)
C2017y <- seq(min(C2017sp@coords[,2]), max(C2017sp@coords[,2]), length.out = (max(C2017sp@coords[,2])-min(C2017sp@coords[,2]))/2)
C2017grid <- expand.grid(x = C2017x, y = C2017y)
gridded(C2017grid) = ~x+y
C2017grid@proj4string<-CRS(st_crs(Points2017)$proj4string)
plot(C2017grid)

C2017k <-  krige(Elevation~1, C2017sp, C2017grid, C2017.fit)
spplot(C2017k["var1.pred"], main = "ordinary kriging predictions")

ggplot(C2017) +
  geom_tile(data = as.data.frame(C2017k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#A2018
# A2018.vgm <- variogram(Z~X+Y, A2018sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.025, "Exp", 14, 0, anis =  c(45, 0.3))
# A2018.fit <- fit.variogram(A2018.vgm, model = vgm.anis)
# plot(A2018.vgm, vgm.anis)

A2018.vgm <- variogram(Z~X+Y, A2018sp, alpha = c(0:1)*90)
plot(A2018.vgm)
vgm.anis = vgm(0.027, "Exp", 8, 0, anis =  c(90, 0.7))
A2018.fit <- fit.variogram(A2018.vgm, model = vgm.anis)
plot(A2018.vgm, vgm.anis)
plot(A2018.vgm, A2018.fit)

A2018x <- seq(min(A2018sp@coords[,1]), max(A2018sp@coords[,1]), length.out = (max(A2018sp@coords[,1])-min(A2018sp@coords[,1]))/2)
A2018y <- seq(min(A2018sp@coords[,2]), max(A2018sp@coords[,2]), length.out = (max(A2018sp@coords[,2])-min(A2018sp@coords[,2]))/2)
A2018grid <- expand.grid(x = A2018x, y = A2018y)
gridded(A2018grid) = ~x+y
A2018grid@proj4string<-CRS(st_crs(Points2018)$proj4string)
plot(A2018grid)

A2018k <-  krige(Elevation~1, A2018sp, A2018grid, A2018.fit)
spplot(A2018k["var1.pred"], main = "ordinary kriging predictions")

ggplot() +
  geom_tile(data = as.data.frame(A2018k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  scale_fill_viridis("Elevation")

ggplot(A2018) +
  geom_tile(data = as.data.frame(A2018k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#B2018
# B2018.vgm <- variogram(Z~X+Y, B2018sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.03, "Exp", 10, 0, anis =  c(45, 0.3))
# B2018.fit <- fit.variogram(B2018.vgm, model = vgm.anis)
# plot(B2018.vgm, vgm.anis)

B2018.vgm <- variogram(Z~X+Y, B2018sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.035, "Exp", 5, 0, anis =  c(0, 0.8))
B2018.fit <- fit.variogram(B2018.vgm, model = vgm.anis)
plot(B2018.vgm, vgm.anis)
plot(B2018.vgm, B2018.fit)


B2018x <- seq(min(B2018sp@coords[,1]), max(B2018sp@coords[,1]), length.out = (max(B2018sp@coords[,1])-min(B2018sp@coords[,1]))/2)
B2018y <- seq(min(B2018sp@coords[,2]), max(B2018sp@coords[,2]), length.out = (max(B2018sp@coords[,2])-min(B2018sp@coords[,2]))/2)
B2018grid <- expand.grid(x = B2018x, y = B2018y)
gridded(B2018grid) = ~x+y
B2018grid@proj4string<-CRS(st_crs(Points2018)$proj4string)
plot(B2018grid)

B2018k <-  krige(Elevation~1, B2018sp, B2018grid, B2018.fit)
spplot(B2018k["var1.pred"], main = "ordinary kriging predictions")

ggplot(B2018) +
  geom_tile(data = as.data.frame(B2018k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#C2018
# C2018.vgm <- variogram(Z~X+Y, C2018sp, alpha = c(0:3)*45)
# vgm.anis = vgm(0.02, "Exp", 12, 0, anis =  c(45, 0.3))
# C2018.fit <- fit.variogram(C2018.vgm, model = vgm.anis)
# plot(C2018.vgm, vgm.anis)

C2018.vgm <- variogram(Z~X+Y, C2018sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.025, "Exp", 5, 0, anis =  c(0, 0.95))
C2018.fit <- fit.variogram(C2018.vgm, model = vgm.anis)
plot(C2018.vgm, vgm.anis)
plot(C2018.vgm, C2018.fit)

C2018x <- seq(min(C2018sp@coords[,1]), max(C2018sp@coords[,1]), length.out = (max(C2018sp@coords[,1])-min(C2018sp@coords[,1]))/2)
C2018y <- seq(min(C2018sp@coords[,2]), max(C2018sp@coords[,2]), length.out = (max(C2018sp@coords[,2])-min(C2018sp@coords[,2]))/2)
C2018grid <- expand.grid(x = C2018x, y = C2018y)
gridded(C2018grid) = ~x+y
C2018grid@proj4string<-CRS(st_crs(Points2018)$proj4string)
plot(C2018grid)

C2018k <-  krige(Elevation~1, C2018sp, C2018grid, C2018.fit)
spplot(C2018k["var1.pred"], main = "ordinary kriging predictions")

ggplot(C2018) +
  geom_tile(data = as.data.frame(C2018k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")

#Convert to raster
A2009raster <- raster(A2009k)
B2009raster <- raster(B2009k)
C2009raster <- raster(C2009k)
A2011raster <- raster(A2011k)
B2011raster <- raster(B2011k)
C2011raster <- raster(C2011k)
A2015raster <- raster(A2015k)
B2015raster <- raster(B2015k)
C2015raster <- raster(C2015k)
A2016raster <- raster(A2016k)
B2016raster <- raster(B2016k)
C2016raster <- raster(C2016k)
A2017raster <- raster(A2017k)
B2017raster <- raster(B2017k)
C2017raster <- raster(C2017k)
A2018raster <- raster(A2018k)
B2018raster <- raster(B2018k)
C2018raster <- raster(C2018k)

# Raster of variance
A2009var <- raster(A2009k, layer = 2)
B2009var <- raster(B2009k, layer = 2)
C2009var <- raster(C2009k, layer = 2)
A2011var <- raster(A2011k, layer = 2)
B2011var <- raster(B2011k, layer = 2)
C2011var <- raster(C2011k, layer = 2)
A2015var <- raster(A2015k, layer = 2)
B2015var <- raster(B2015k, layer = 2)
C2015var <- raster(C2015k, layer = 2)
A2016var <- raster(A2016k, layer = 2)
B2016var <- raster(B2016k, layer = 2)
C2016var <- raster(C2016k, layer = 2)
A2017var <- raster(A2017k, layer = 2)
B2017var <- raster(B2017k, layer = 2)
C2017var <- raster(C2017k, layer = 2)
A2018var <- raster(A2018k, layer = 2)
B2018var <- raster(B2018k, layer = 2)
C2018var <- raster(C2018k, layer = 2)

# #Write tif files
# setwd("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces")
# writeRaster(A2009raster, "A2009K", format = "GTiff")
# writeRaster(B2009raster, "B2009K", format = "GTiff")
# writeRaster(C2009raster, "C2009K", format = "GTiff")
# writeRaster(A2011raster, "A2011K", format = "GTiff")
# writeRaster(B2011raster, "B2011K", format = "GTiff")
# writeRaster(C2011raster, "C2011K", format = "GTiff")
# writeRaster(A2015raster, "A2015K", format = "GTiff")
# writeRaster(B2015raster, "B2015K", format = "GTiff")
# writeRaster(C2015raster, "C2015K", format = "GTiff")
# writeRaster(A2016raster, "A2016K", format = "GTiff")
# writeRaster(B2016raster, "B2016K", format = "GTiff")
# writeRaster(C2016raster, "C2016K", format = "GTiff")
# writeRaster(A2017raster, "A2017K", format = "GTiff")
# writeRaster(B2017raster, "B2017K", format = "GTiff")
# writeRaster(C2017raster, "C2017K", format = "GTiff")
# writeRaster(A2018raster, "A2018K", format = "GTiff")
# writeRaster(B2018raster, "B2018K", format = "GTiff")
# writeRaster(C2018raster, "C2018K", format = "GTiff")
# writeRaster(A2009var, "A2009Var", format = "GTiff")
# writeRaster(B2009var, "B2009Var", format = "GTiff")
# writeRaster(C2009var, "C2009Var", format = "GTiff")
# writeRaster(A2011var, "A2011Var", format = "GTiff")
# writeRaster(B2011var, "B2011Var", format = "GTiff")
# writeRaster(C2011var, "C2011Var", format = "GTiff")
# writeRaster(A2015var, "A2015Var", format = "GTiff")
# writeRaster(B2015var, "B2015Var", format = "GTiff")
# writeRaster(C2015var, "C2015Var", format = "GTiff")
# writeRaster(A2016var, "A2016Var", format = "GTiff")
# writeRaster(B2016var, "B2016Var", format = "GTiff")
# writeRaster(C2016var, "C2016Var", format = "GTiff")
# writeRaster(A2017var, "A2017Var", format = "GTiff")
# writeRaster(B2017var, "B2017Var", format = "GTiff")
# writeRaster(C2017var, "C2017Var", format = "GTiff")
# writeRaster(A2018var, "A2018Var", format = "GTiff")
# writeRaster(B2018var, "B2018Var", format = "GTiff")
# writeRaster(C2018var, "C2018Var", format = "GTiff")

# ###################Import raster files (when you don't want to run the whole code)########################################
# A2009raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2009K.tif")
# B2009raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2009K.tif")
# C2009raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2009K.tif")
# A2011raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2011K.tif")
# B2011raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2011K.tif")
# C2011raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2011K.tif")
# A2015raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2015K.tif")
# B2015raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2015K.tif")
# C2015raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2015K.tif")
# A2016raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2016K.tif")
# B2016raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2016K.tif")
# C2016raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2016K.tif")
# A2017raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2017K.tif")
# B2017raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2017K.tif")
# C2017raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2017K.tif")
# A2018raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2018K.tif")
# B2018raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2018K.tif")
# C2018raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2018K.tif")

#Resample all rasters to 2017 grid
A2009re <- resample(A2009raster, A2017raster, method = "bilinear")
B2009re <- resample(B2009raster, B2017raster, method = "bilinear")
C2009re <- resample(C2009raster, C2017raster, method = "bilinear")
A2011re <- resample(A2011raster, A2017raster, method = "bilinear")
B2011re <- resample(B2011raster, B2017raster, method = "bilinear")
C2011re <- resample(C2011raster, C2017raster, method = "bilinear")
A2015re <- resample(A2015raster, A2017raster, method = "bilinear")
B2015re <- resample(B2015raster, B2017raster, method = "bilinear")
C2015re <- resample(C2015raster, C2017raster, method = "bilinear")
A2016re <- resample(A2016raster, A2017raster, method = "bilinear")
B2016re <- resample(B2016raster, B2017raster, method = "bilinear")
C2016re <- resample(C2016raster, C2017raster, method = "bilinear")
A2018re <- resample(A2018raster, A2017raster, method = "bilinear")
B2018re <- resample(B2018raster, B2017raster, method = "bilinear")
C2018re <- resample(C2018raster, C2017raster, method = "bilinear")

#Apply correction to necessary years
A2009corrected <- A2009re+1.8253
B2009corrected <- B2009re+1.8253
C2009corrected <- C2009re+1.8253
A2011corrected <- A2011re+1.8253
B2011corrected <- B2011re+1.8253
C2011corrected <- C2011re+1.8253
# A2015corrected <- A2015re+1.3656
# B2015corrected <- B2015re+1.3656
# C2015corrected <- C2015re+1.3656

# #Write tif files from geoid corrected elevation
# setwd("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces")
# writeRaster(A2009corrected, "A2009corrected", format = "GTiff")
# writeRaster(B2009corrected, "B2009corrected", format = "GTiff")
# writeRaster(C2009corrected, "C2009corrected", format = "GTiff")
# writeRaster(A2011corrected, "A2011corrected", format = "GTiff")
# writeRaster(B2011corrected, "B2011corrected", format = "GTiff")
# writeRaster(C2011corrected, "C2011corrected", format = "GTiff")

#Calculate subsidence - finally!
A17_09Sub <- A2017raster-A2009corrected
B17_09Sub <- B2017raster-B2009corrected
C17_09Sub <- C2017raster-C2009corrected
A16_09Sub <- A2016re-A2009corrected
B16_09Sub <- B2016re-B2009corrected
C16_09Sub <- C2016re-C2009corrected
# A15_09Sub <- A2015corrected-A2009corrected
# B15_09Sub <- B2015corrected-B2009corrected
# C15_09Sub <- C2015corrected-C2009corrected
A11_09Sub <- A2011corrected-A2009corrected
B11_09Sub <- B2011corrected-B2009corrected
C11_09Sub <- C2011corrected-C2009corrected

A17_11Sub <- A2017raster-A2011corrected
B17_11Sub <- B2017raster-B2011corrected
C17_11Sub <- C2017raster-C2011corrected
A16_11Sub <- A2016re-A2011corrected
B16_11Sub <- B2016re-B2011corrected
C16_11Sub <- C2016re-C2011corrected
# A15_11Sub <- A2015corrected-A2011corrected
# B15_11Sub <- B2015corrected-B2011corrected
# C15_11Sub <- C2015corrected-C2011corrected

#Test 2018 data
A18_17 <- A2018re-A2017raster
B18_17 <- B2018re-B2017raster
C18_17 <- C2018re-C2017raster
A18_16 <- A2018re-A2016re
B18_16 <- B2018re-B2016re
C18_16 <- C2018re-C2016re

#Plot the subsidence
plot(A11_09Sub)
plot(A15_09Sub)
plot(A16_09Sub)
plot(A17_09Sub)
plot(B11_09Sub)
plot(B15_09Sub)
plot(B16_09Sub)
plot(B17_09Sub)
plot(C11_09Sub)
plot(C15_09Sub)
plot(C16_09Sub)
plot(C17_09Sub)
plot(A18_17)
plot(B18_17)
plot(C18_17)
plot(A18_16)
plot(B18_16)
plot(C18_16)


# #Export Subsidence files - Better to wait until they have been clipped
# setwd("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces")
# writeRaster(A17_09Sub, "A2017Sub", format = "GTiff")
# writeRaster(B17_09Sub, "B2017Sub", format = "GTiff")
# writeRaster(C17_09Sub, "C2017Sub", format = "GTiff")
# writeRaster(A16_09Sub, "A2016Sub", format = "GTiff")
# writeRaster(B16_09Sub, "B2016Sub", format = "GTiff")
# writeRaster(C16_09Sub, "C2016Sub", format = "GTiff")
# writeRaster(A15_09Sub, "A2015Sub", format = "GTiff")
# writeRaster(B15_09Sub, "B2015Sub", format = "GTiff")
# writeRaster(C15_09Sub, "C2015Sub", format = "GTiff")
# writeRaster(A11_09Sub, "A2011Sub", format = "GTiff")
# writeRaster(B11_09Sub, "B2011Sub", format = "GTiff")
# writeRaster(C11_09Sub, "C2011Sub", format = "GTiff")

#Load subsidence files that haven't been clipped
# A17_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2017Sub.tif")
# B17_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2017Sub.tif")
# C17_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2017Sub.tif")
# A16_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2016Sub.tif")
# B16_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2016Sub.tif")
# C16_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2016Sub.tif")
# A15_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2015Sub.tif")
# B15_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2015Sub.tif")
# C15_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2015Sub.tif")
# A11_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2011Sub.tif")
# B11_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2011Sub.tif")
# C11_09Sub <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2011Sub.tif")

#Import shapefiles for mapping
Blocks <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/All_Points"),
                   layer = "Blocks_Poly",
                   stringsAsFactors = FALSE,
                   drop_unsupported_fields = TRUE)

Blocks11 <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/All_Points"),
                  layer = "Blocks_Poly_2011",
                  stringsAsFactors = FALSE,
                  drop_unsupported_fields = TRUE)

Fences <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/All_Points"),
                   layer = "Fences",
                   stringsAsFactors = FALSE,
                   drop_unsupported_fields = TRUE)

#Turn Blocks into separate rasters for each block
BlockA <- rasterize(Blocks, A2017raster)
BlockB <- rasterize(Blocks, B2017raster)
BlockC <- rasterize(Blocks, C2017raster)
BlockA11 <- rasterize(Blocks11, A2017raster)
BlockB11 <- rasterize(Blocks11, B2017raster)
BlockC11 <- rasterize(Blocks11, C2017raster)

#Mask the areas outside of blocks
A17_09Clip <- mask(A17_09Sub, BlockA)
B17_09Clip <- mask(B17_09Sub, BlockB)
C17_09Clip <- mask(C17_09Sub, BlockC)
A16_09Clip <- mask(A16_09Sub, BlockA)
B16_09Clip <- mask(B16_09Sub, BlockB)
C16_09Clip <- mask(C16_09Sub, BlockC)
# A15_09Clip <- mask(A15_09Sub, BlockA)
# B15_09Clip <- mask(B15_09Sub, BlockB)
# C15_09Clip <- mask(C15_09Sub, BlockC)
A11_09Clip <- mask(A11_09Sub, BlockA)
B11_09Clip <- mask(B11_09Sub, BlockB)
C11_09Clip <- mask(C11_09Sub, BlockC)
A17_11Clip <- mask(A17_11Sub, BlockA11)
B17_11Clip <- mask(B17_11Sub, BlockB11)
C17_11Clip <- mask(C17_11Sub, BlockC11)
A16_11Clip <- mask(A16_11Sub, BlockA11)
B16_11Clip <- mask(B16_11Sub, BlockB11)
C16_11Clip <- mask(C16_11Sub, BlockC11)
# A15_11Clip <- mask(A15_11Sub, BlockA11)
# B15_11Clip <- mask(B15_11Sub, BlockB11)
# C15_11Clip <- mask(C15_11Sub, BlockC11)

#Plot to make sure the clipping worked properly
plot(A17_09Clip)
plot(B17_09Clip)
plot(C17_09Clip)
plot(A16_09Clip)
plot(B16_09Clip)
plot(C16_09Clip)
# plot(A15_09Clip)
# plot(B15_09Clip)
# plot(C15_09Clip)
plot(A11_09Clip)
plot(B11_09Clip)
plot(C11_09Clip)
plot(A17_11Clip)

# # Export to subsidence tif file
# setwd("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped")
# writeRaster(A17_09Clip, "A2017SubClip_corrected", format = "GTiff")
# writeRaster(B17_09Clip, "B2017SubClip_corrected", format = "GTiff")
# writeRaster(C17_09Clip, "C2017SubClip_corrected", format = "GTiff")
# writeRaster(A16_09Clip, "A2016SubClip_corrected", format = "GTiff")
# writeRaster(B16_09Clip, "B2016SubClip_corrected", format = "GTiff")
# writeRaster(C16_09Clip, "C2016SubClip_corrected", format = "GTiff")
# writeRaster(A11_09Clip, "A2011SubClip_corrected", format = "GTiff")
# writeRaster(B11_09Clip, "B2011SubClip_corrected", format = "GTiff")
# writeRaster(C11_09Clip, "C2011SubClip_corrected", format = "GTiff")
# writeRaster(A17_11Clip, "A2017_11SubClip_corrected", format = "GTiff")
# writeRaster(B17_11Clip, "B2017_11SubClip_corrected", format = "GTiff")
# writeRaster(C17_11Clip, "C2017_11SubClip_corrected", format = "GTiff")
# writeRaster(A16_11Clip, "A2016_11SubClip_corrected", format = "GTiff")
# writeRaster(B16_11Clip, "B2016_11SubClip_corrected", format = "GTiff")
# writeRaster(C16_11Clip, "C2016_11SubClip_corrected", format = "GTiff")

# #Load subsidence files that have been clipped when not running whole script
# A17_09Clip <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2017SubClip_corrected.tif")
# B17_09Clip <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2017SubClip_corrected.tif")
# C17_09Clip <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2017SubClip_corrected.tif")
# A16_09Clip <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2016SubClip_corrected.tif")
# B16_09Clip <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2016SubClip_corrected.tif")
# C16_09Clip <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2016SubClip_corrected.tif")
# A11_09Clip <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/A2011SubClip_corrected.tif")
# B11_09Clip <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/B2011SubClip_corrected.tif")
# C11_09Clip <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/C2011SubClip_corrected.tif")

# Load variance files
# A09Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2009Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = A2009Var)
# B09Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2009Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = B2009Var)
# C09Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2009Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = C2009Var)
# A11Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2011Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = A2011Var)
# B11Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2011Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = B2011Var)
# C11Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2011Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = C2011Var)
# A16Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2016Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = A2016Var)
# B16Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2016Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = B2016Var)
# C16Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2016Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = C2016Var)
# A17Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/A2017Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = A2017Var)
# B17Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/B2017Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = B2017Var)
# C17Var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/C2017Var.tif") %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = C2017Var)

# Also load fences
# Fences <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/All_Points"),
#                   layer = "Fences",
#                   stringsAsFactors = FALSE,
#                   drop_unsupported_fields = TRUE)


# Use dataframe for graphing
A11_09_df <- as(A11_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame(A11_09_spdf) %>%
  rename(value = A2011SubClip_corrected) %>%
  mutate(year = 2011,
         block = 'A')

B11_09_df <- as(B11_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame(B11_09_spdf) %>%
  rename(value = B2011SubClip_corrected) %>%
  mutate(year = 2011,
         block = 'B')

C11_09_df <-  as(C11_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame(C11_09_spdf) %>%
  rename(value = C2011SubClip_corrected) %>%
  mutate(year = 2011,
         block = 'C')

A16_09_df <- as(A16_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame(A16_09_spdf) %>%
  rename(value = A2016SubClip_corrected) %>%
  mutate(year = 2016,
         block = 'A')

B16_09_df <- as(B16_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame(B16_09_spdf) %>%
  rename(value = B2016SubClip_corrected) %>%
  mutate(year = 2016,
         block = 'B')

C16_09_df <- as(C16_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame(C16_09_spdf) %>%
  rename(value = C2016SubClip_corrected) %>%
  mutate(year = 2016,
         block = 'C')

A17_09_df <- as(A17_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame(A17_09_spdf) %>%
  rename(value = A2017SubClip_corrected) %>%
  mutate(year = 2017,
         block = 'A')

B17_09_df <- as(B17_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame(B17_09_spdf) %>%
  rename(value = B2017SubClip_corrected) %>%
  mutate(year = 2017,
         block = 'B')

C17_09_df <- as(C17_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame(C17_09_spdf) %>%
  rename(value = C2017SubClip_corrected) %>%
  mutate(year = 2017,
         block = 'C')

# Join into one data frame for graphing
Subsidence.df <- A11_09_df %>%
  rbind.data.frame(B11_09_df, C11_09_df, A16_09_df, B16_09_df, C16_09_df, A17_09_df, B17_09_df, C17_09_df)

# Graph all in one go?
ggplot(Subsidence.df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  facet_grid(block ~ year, scales = 'free_x') +
  theme_few() +
  scale_fill_viridis("Subsidence",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(limits = c(min(Subsidence.df$x), max(Subsidence.df$x))) +
  scale_y_continuous(limits = c(min(Subsidence.df$y), max(Subsidence.df$y))) +
  north(symbol = 12, x.min = axmin, x.max = axmax, y.min = aymin, y.max = aymax, location = "topleft") +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22, angle = 0, vjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

# Values needed for north arrow
axmax <- max(A11_09_df$x)
axmin <- min(A11_09_df$x)
aymax <- max(A11_09_df$y)
aymin <- min(A11_09_df$y)

#data frame for scalebar = bounding box
bb <- data.frame(long = c(538107, 538142), lat = c(1101020.5, 1101058.5))

#Make a map
ggplot(A17_09Clip, aes(x,y, fill = Elevation)) +
  geom_raster()

# trying to figure out how to add a north arrow and scale bar
# and graph all in one window (use annotations for year and block labels to fix alignment issues in final plot - the labels look messier, though)
g2011a <- ggplot(A11_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(name = '2011',
                     limits = c(537928, 537963),
                     position = 'top') +
  scale_y_continuous(name = 'A',
                     limits = c(1100964.5, 1101002.5)) +
  north(symbol = 12, x.min = axmin, x.max = axmax, y.min = aymin, y.max = aymax, location = "topleft") +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22, angle = 0, vjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))
  
# scalebar(dist = 0.02, dd2km = FALSE, x.min = axmin, x.max = axmax, y.min = aymin, y.max = aymax) +

g2011b <- ggplot(B11_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(limits = c(537987.5, 538022.5)) +
  scale_y_continuous(name = 'B',
                     limits = c(1101098, 1101136)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 22, angle = 0, vjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

g2011c <- ggplot(C11_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(limits = c(538107, 538142)) +
  scale_y_continuous(name = 'C',
                     limits = c(1101018, 1101056)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 22, angle = 0, vjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

g2016a <- ggplot(A16_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(name = '2016',
                     limits = c(537928, 537963),
                     position = 'top') +
  scale_y_continuous(limits = c(1100964.5, 1101002.5)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

g2016b <- ggplot(B16_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(limits = c(537987.5, 538022.5)) +
  scale_y_continuous(limits = c(1101098, 1101136)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

g2016c <- ggplot(C16_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(limits = c(538107, 538142)) +
  scale_y_continuous(limits = c(1101018, 1101056)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

g2017a <- ggplot(A17_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(name = '2017',
                     limits = c(537928, 537963),
                     position = 'top') +
  scale_y_continuous(limits = c(1100964.5, 1101002.5)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,3,0,0), "cm"))

g2017b <- ggplot(B17_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(limits = c(537987.5, 538022.5)) +
  scale_y_continuous(limits = c(1101098, 1101136)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,3,0,0), "cm"))

g2017c <- ggplot(C17_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence (m)",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_x_continuous(limits = c(538107, 538142)) +
  scale_y_continuous(limits = c(1101018, 1101056)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.justification=c(0,1),
        legend.position=c(1.05,1.8),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.margin=unit(c(0,3,0,0), "cm"))

# scalebar(C17_09_df, dist = 0.01, height = 0.2, location = 'topleft', dd2km = FALSE)
# Have to save this from the plot window manually :(
grid.arrange(g2011a, g2016a, g2017a, g2011b, g2016b, g2017b, g2011c, g2016c, g2017c, ncol = 3)
plot_grid(g2011a, g2016a, g2017a, g2011b, g2016b, g2017b, g2011c, g2016c, g2017c, ncol = 3, align = 'hv')
# grid_arrange_shared_legend

# test with translucent variance over top
test1 <- ggplot(C17_09_df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  geom_tile(data = C17Var, aes(x = x, y = y, alpha = value), inherit.aes = FALSE, fill = 'black') +
  geom_path(data = Fences, aes(x=long, y=lat, group=group), inherit.aes = FALSE) +
  theme_few() +
  scale_fill_viridis("Subsidence (m)",
                     limits = c(-1.5, 0.5),
                     direction = -1) +
  scale_alpha_continuous("Variance",
                         limits = c(0, 0.005),
                         range = c(0, 1)) +
  scale_x_continuous(limits = c(538107, 538142)) +
  scale_y_continuous(limits = c(1101018, 1101056)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.justification=c(0,1),
        legend.position=c(1.05,1.3),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.margin=unit(c(0,0,0,0), "cm"))

# Graphs of variance
ggplot(A09Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(B09Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(C09Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(A11Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(B11Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(C11Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(A16Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(B16Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(C16Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(A17Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(B17Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

ggplot(C17Var, aes(x=x, y=y, fill=value)) +
  geom_tile()

# Semivariogram of subsidence
# load data
subpoints <- read.csv('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/Subsidence_2009_2017.csv')
plotcoords <-st_read("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/plot_coordinates_from_2017.shp")

#format data
subsidence2017 <- subpoints %>%
  mutate(fence = as.factor(fence)) %>%
  left_join(plotcoords, by = c('type', 'fence', 'plot', 'exp')) %>%
  filter(year == 2017)

subsidence2017sp <- subsidence2017 %>% st_as_sf() %>% st_coordinates() %>% cbind(subsidence2017)
coordinates(subsidence2017sp) <- ~X+Y

# semivariogram
sub.vgm <- variogram(subsidence~X+Y, subsidence2017sp)
plot(sub.vgm, range = c(0, 200))
vgm.anis = vgm(0.025, "Exp", 7, 0, anis =  c(90, 0.7))
A2015.fit <- fit.variogram(A2015.vgm, model = vgm.anis)
plot(A2015.vgm, vgm.anis)

A2015x <- seq(min(A2015sp@coords[,1]), max(A2015sp@coords[,1]), length.out = (max(A2015sp@coords[,1])-min(A2015sp@coords[,1]))/2)
A2015y <- seq(min(A2015sp@coords[,2]), max(A2015sp@coords[,2]), length.out = (max(A2015sp@coords[,2])-min(A2015sp@coords[,2]))/2)
A2015grid <- expand.grid(x = A2015x, y = A2015y)
gridded(A2015grid) = ~x+y
A2015grid@proj4string<-CRS(st_crs(Points2015)$proj4string)
plot(A2015grid)

A2015k <-  krige(Elevation~1, A2015sp, A2015grid, A2015.fit)
spplot(A2015k["var1.pred"], main = "ordinary kriging predictions")

ggplot(A2015) +
  geom_tile(data = as.data.frame(A2015k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation")