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

##############################Load files##############################

#Points2017 <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/2017 GPS/Analyses"),
#                      layer = "All_Points_2017_SPCSAK4",
#                      stringsAsFactors = FALSE,
#                      drop_unsupported_fields = TRUE)

Points2009 <- st_read("~/School/NAU/Schuur Lab/GPS/2009 GPS/All_Points_2009_SPCSAK4.shp")
Points2011 <- st_read("~/School/NAU/Schuur Lab/GPS/2011 GPS/All_Points_2011_SPCSAK4.shp")
Points2015 <- st_read("~/School/NAU/Schuur Lab/GPS/2015 GPS/All_Points_2015_SPCSAK4.shp")
Points2016 <- st_read("~/School/NAU/Schuur Lab/GPS/2016 GPS/All_Points_2016_SPCSAK4.shp")
Points2017 <- st_read("~/School/NAU/Schuur Lab/GPS/2017 GPS/Analyses/All_Points_2017_SPCSAK4.shp")

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


##############################Make copies of the points with different names##############################
#so that I can graph with the data.frame versions later, while kriging with the spatialpointsdataframe.

A2009sp <- A2009
B2009sp <- B2009
C2009sp <- C2009
A2011sp <- A2011
B2011sp <- B2011
C2011sp <- C2011
A2015sp <- A2015
B2015sp <- B2015
C2015sp <- C2015
A2016sp <- A2016
B2016sp <- B2016
C2016sp <- C2016
A2017sp <- A2017
B2017sp <- B2017
C2017sp <- C2017
EC2017sp <- EC2017

##############Turn the original dataframe versions back into sf and extract the coordinate columns#######################
A2009sp <- A2009 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2009sp)
B2009sp <- B2009 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2009sp)
C2009sp <- C2009 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2009sp)
A2011sp <- A2011 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2011sp)
B2011sp <- B2011 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2011sp)
C2011sp <- C2011 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2011sp)
A2015sp <- A2015 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2015sp)
B2015sp <- B2015 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2015sp)
C2015sp <- C2015 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2015sp)
A2016sp <- A2016 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2016sp)
B2016sp <- B2016 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2016sp)
C2016sp <- C2016 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2016sp)
A2017sp <- A2017 %>% st_as_sf() %>% st_coordinates() %>% cbind(A2017sp)
B2017sp <- B2017 %>% st_as_sf() %>% st_coordinates() %>% cbind(B2017sp)
C2017sp <- C2017 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2017sp)
EC2017sp <- C2017 %>% st_as_sf() %>% st_coordinates() %>% cbind(C2017sp)

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

##############################Interpolate a surface using kriging##############################
#First, convert to an sp object rather than sf
coordinates(A2009sp) = ~X+Y
coordinates(B2009sp) = ~X+Y
coordinates(C2009sp) = ~X+Y
coordinates(A2011sp) = ~X+Y
coordinates(B2011sp) = ~X+Y
coordinates(C2011sp) = ~X+Y
coordinates(A2015sp) = ~X+Y
coordinates(B2015sp) = ~X+Y
coordinates(C2015sp) = ~X+Y
coordinates(A2016sp) = ~X+Y
coordinates(B2016sp) = ~X+Y
coordinates(C2016sp) = ~X+Y
coordinates(A2017sp) = ~X+Y
coordinates(B2017sp) = ~X+Y
coordinates(C2017sp) = ~X+Y
coordinates(EC2017sp) = ~X+Y

#Apply coordinate system information to sp objects
A2009sp@proj4string<-CRS(st_crs(Points2009)$proj4string)
B2009sp@proj4string<-CRS(st_crs(Points2009)$proj4string)
C2009sp@proj4string<-CRS(st_crs(Points2009)$proj4string)
A2011sp@proj4string<-CRS(st_crs(Points2011)$proj4string)
B2011sp@proj4string<-CRS(st_crs(Points2011)$proj4string)
C2011sp@proj4string<-CRS(st_crs(Points2011)$proj4string)
A2015sp@proj4string<-CRS(st_crs(Points2015)$proj4string)
B2015sp@proj4string<-CRS(st_crs(Points2015)$proj4string)
C2015sp@proj4string<-CRS(st_crs(Points2015)$proj4string)
A2016sp@proj4string<-CRS(st_crs(Points2016)$proj4string)
B2016sp@proj4string<-CRS(st_crs(Points2016)$proj4string)
C2016sp@proj4string<-CRS(st_crs(Points2016)$proj4string)
A2017sp@proj4string<-CRS(st_crs(Points2017)$proj4string)
B2017sp@proj4string<-CRS(st_crs(Points2017)$proj4string)
C2017sp@proj4string<-CRS(st_crs(Points2017)$proj4string)
EC2017sp@proj4string<-CRS(st_crs(Points2017)$proj4string)

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



#Write tif files
setwd("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces")
writeRaster(A2009raster, "A2009K", format = "GTiff")
writeRaster(B2009raster, "B2009K", format = "GTiff")
writeRaster(C2009raster, "C2009K", format = "GTiff")
writeRaster(A2011raster, "A2011K", format = "GTiff")
writeRaster(B2011raster, "B2011K", format = "GTiff")
writeRaster(C2011raster, "C2011K", format = "GTiff")
writeRaster(A2015raster, "A2015K", format = "GTiff")
writeRaster(B2015raster, "B2015K", format = "GTiff")
writeRaster(C2015raster, "C2015K", format = "GTiff")
writeRaster(A2016raster, "A2016K", format = "GTiff")
writeRaster(B2016raster, "B2016K", format = "GTiff")
writeRaster(C2016raster, "C2016K", format = "GTiff")
writeRaster(A2017raster, "A2017K", format = "GTiff")
writeRaster(B2017raster, "B2017K", format = "GTiff")
writeRaster(C2017raster, "C2017K", format = "GTiff")

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

test1 <- as.data.frame(A2009re)

# #stack each block
# Astack <- stack(A2009re, A2011re, A2015re, A2016re, A2017raster)
# Bstack <- stack(B2009re, B2011re, B2015re, B2016re, B2017raster)
# Cstack <- stack(C2009re, C2011re, C2015re, C2016re, C2017raster)

#Band math to compare years from 2017
A17_09 <- A2017raster-A2009re
A17_11 <- A2017raster-A2011re
A17_15 <- A2017raster-A2015re
A17_16 <- A2017raster-A2016re
B17_09 <- B2017raster-B2009re
B17_11 <- B2017raster-B2011re
B17_15 <- B2017raster-B2015re
B17_16 <- B2017raster-B2016re
C17_09 <- C2017raster-C2009re
C17_11 <- C2017raster-C2011re
C17_15 <- C2017raster-C2015re
C17_16 <- C2017raster-C2016re

# #Band math to compare years from 2016 - tested, this makes no differences in corrections
# A16_09 <- A2016re-A2009re
# A16_11 <- A2016re-A2011re
# A16_15 <- A2016re-A2015re
# A16_17 <- A2016re-A2017raster
# B16_09 <- B2016re-B2009re
# B16_11 <- B2016re-B2011re
# B16_15 <- B2016re-B2015re
# B16_17 <- B2016re-B2017raster
# C16_09 <- C2016re-C2009re
# C16_11 <- C2016re-C2011re
# C16_15 <- C2016re-C2015re
# C16_17 <- C2016re-C2017raster

#Stack offset rasters from 2017
Aoffsets <- stack(A17_09, A17_11, A17_15, A17_16)
Boffsets <- stack(B17_09, B17_11, B17_15, B17_16)
Coffsets <- stack(C17_09, C17_11, C17_15, C17_16)

# #Stack offset rasters from 2016
# Aoffsets16 <- stack(A16_09, A16_11, A16_15, A16_17)
# Boffsets16 <- stack(B16_09, B16_11, B16_15, B16_17)
# Coffsets16 <- stack(C16_09, C16_11, C16_15, C16_17)

#Plot to determine view avg. offset
plot(A17_09)
plot(A17_11)
plot(A17_15)
plot(A17_16)
plot(B17_09)
plot(B17_11)
plot(B17_15)
plot(B17_16)
plot(C17_09)
plot(C17_11)
plot(C17_15)
plot(C17_16)

#Import control polygon to perform zonal statistics on offset rasters
Control <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/All_Points"),
                                         layer = "Control_Poly",
                                         stringsAsFactors = FALSE,
                                         drop_unsupported_fields = TRUE)



#Zonal Statistics and some reformatting of the resulting matrix
MeanOffsetA <- extract(Aoffsets, Control, fun = mean) %>%
  as.data.frame() %>%
  filter(V1 != is.na(V1))
MeanoffsetB <- extract(Boffsets, Control, fun = mean) %>%
  as.data.frame() %>%
  filter(V1 != is.na(V1))
MeanoffsetC <- extract(Coffsets, Control, fun = mean) %>%
  as.data.frame() %>%
  filter(V1 != is.na(V1))

# #Zonal Statistics and some reformatting of the resulting matrix from 2016 offsets
# MeanOffsetA16 <- extract(Aoffsets16, Control, fun = mean) %>%
#   as.data.frame() %>%
#   filter(V1 != is.na(V1))
# MeanoffsetB16 <- extract(Boffsets16, Control, fun = mean) %>%
#   as.data.frame() %>%
#   filter(V1 != is.na(V1))
# MeanoffsetC16 <- extract(Coffsets16, Control, fun = mean) %>%
#   as.data.frame() %>%
#   filter(V1 != is.na(V1))

#Combine correction values and rename columns/rows for clarity
# MeanOffset <- MeanOffsetA %>%
#   rbind(MeanoffsetB, MeanoffsetC) %>%
#   mutate(Block = c('A', 'B', 'C'),
#          Mean17_09 = round(V1, 1),
#          Mean17_11 = round(V2, 1),
#          Mean17_15 = round(V3, 1),
#          Mean17_16 = round(V4, 1)) %>%
#   dplyr::select(Block, Mean17_09, Mean17_11, Mean17_15, Mean17_16)

MeanOffset <- MeanOffsetA %>%
  rbind(MeanoffsetB, MeanoffsetC) %>%
  mutate(Block = c('A', 'B', 'C'),
         Mean17_09 = V1,
         Mean17_11 = V2,
         Mean17_15 = V3,
         Mean17_16 = V4, 1) %>%
  dplyr::select(Block, Mean17_09, Mean17_11, Mean17_15, Mean17_16)

MeanOffset <- MeanOffset %>%
  dplyr::select(-Block) %>%
  summarise_all(mean) %>%
  mutate(Mean17_09 = round(Mean17_09, 1),
         Mean17_11 = round(Mean17_11, 1),
         Mean17_15 = round(Mean17_15, 1),
         Mean17_16 = round(Mean17_16, 1))

# write.csv(MeanOffset, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 GPS/Analyses/Elevation_Corrections.csv")
# Meanoffset <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/2017 GPS/Analyses/Elevation_Corrections.csv")

# #From 2016
# MeanOffset16 <- MeanOffsetA16 %>%
#   rbind(MeanoffsetB, MeanoffsetC) %>%
#   mutate(Block = c('A', 'B', 'C'),
#          Mean16_09 = V1,
#          Mean16_11 = V2,
#          Mean16_15 = V3,
#          Mean16_17 = V4, 1) %>%
#   dplyr::select(Block, Mean16_09, Mean16_11, Mean16_15, Mean16_17)
# 
# MeanOffset16 <- MeanOffset16 %>%
#   dplyr::select(-Block) %>%
#   summarise_all(mean) %>%
#   mutate(Mean16_09 = round(Mean16_09, 1),
#          Mean16_11 = round(Mean16_11, 1),
#          Mean16_15 = round(Mean16_15, 1),
#          Mean16_17 = round(Mean16_17, 1))

#Apply correction to necessary years
A2009corrected <- A2009re+1.2
B2009corrected <- B2009re+1.2
C2009corrected <- C2009re+1.2
A2011corrected <- A2011re+1.2
B2011corrected <- B2011re+1.2
C2011corrected <- C2011re+1.2
A2015corrected <- A2015re+2.1
B2015corrected <- B2015re+2.1
C2015corrected <- C2015re+2.1

#Calculate subsidence - finally!
A17_09Sub <- A2017raster-A2009corrected
B17_09Sub <- B2017raster-B2009corrected
C17_09Sub <- C2017raster-C2009corrected
A16_09Sub <- A2016re-A2009corrected
B16_09Sub <- B2016re-B2009corrected
C16_09Sub <- C2016re-C2009corrected
A15_09Sub <- A2015corrected-A2009corrected
B15_09Sub <- B2015corrected-B2009corrected
C15_09Sub <- C2015corrected-C2009corrected
A11_09Sub <- A2011corrected-A2009corrected
B11_09Sub <- B2011corrected-B2009corrected
C11_09Sub <- C2011corrected-C2009corrected

A17_11Sub <- A2017raster-A2011corrected
B17_11Sub <- B2017raster-B2011corrected
C17_11Sub <- C2017raster-C2011corrected
A16_11Sub <- A2016re-A2011corrected
B16_11Sub <- B2016re-B2011corrected
C16_11Sub <- C2016re-C2011corrected
A15_11Sub <- A2015corrected-A2011corrected
B15_11Sub <- B2015corrected-B2011corrected
C15_11Sub <- C2015corrected-C2011corrected


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


test <- as.data.frame(A17_09)
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
A15_09Clip <- mask(A15_09Sub, BlockA)
B15_09Clip <- mask(B15_09Sub, BlockB)
C15_09Clip <- mask(C15_09Sub, BlockC)
A11_09Clip <- mask(A11_09Sub, BlockA)
B11_09Clip <- mask(B11_09Sub, BlockB)
C11_09Clip <- mask(C11_09Sub, BlockC)
A17_11Clip <- mask(A17_11Sub, BlockA11)
B17_11Clip <- mask(B17_11Sub, BlockB11)
C17_11Clip <- mask(C17_11Sub, BlockC11)
A16_11Clip <- mask(A16_11Sub, BlockA11)
B16_11Clip <- mask(B16_11Sub, BlockB11)
C16_11Clip <- mask(C16_11Sub, BlockC11)
A15_11Clip <- mask(A15_11Sub, BlockA11)
B15_11Clip <- mask(B15_11Sub, BlockB11)
C15_11Clip <- mask(C15_11Sub, BlockC11)

#Plot to make sure the clipping worked properly
plot(A17_09Clip)
plot(B17_09Clip)
plot(C17_09Clip)
plot(A16_09Clip)
plot(B16_09Clip)
plot(C16_09Clip)
plot(A15_09Clip)
plot(B15_09Clip)
plot(C15_09Clip)
plot(A11_09Clip)
plot(B11_09Clip)
plot(C11_09Clip)
plot(A17_11Clip)

# Export to subsidence tif file
# setwd("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped")
# writeRaster(A17_09Clip, "A2017SubClip", format = "GTiff")
# writeRaster(B17_09Clip, "B2017SubClip", format = "GTiff")
# writeRaster(C17_09Clip, "C2017SubClip", format = "GTiff")
# writeRaster(A16_09Clip, "A2016SubClip", format = "GTiff")
# writeRaster(B16_09Clip, "B2016SubClip", format = "GTiff")
# writeRaster(C16_09Clip, "C2016SubClip", format = "GTiff")
# writeRaster(A15_09Clip, "A2015SubClip", format = "GTiff")
# writeRaster(B15_09Clip, "B2015SubClip", format = "GTiff")
# writeRaster(C15_09Clip, "C2015SubClip", format = "GTiff")
# writeRaster(A11_09Clip, "A2011SubClip", format = "GTiff")
# writeRaster(B11_09Clip, "B2011SubClip", format = "GTiff")
# writeRaster(C11_09Clip, "C2011SubClip", format = "GTiff")
# writeRaster(A17_11Clip, "A2017_11SubClip", format = "GTiff")
# writeRaster(B17_11Clip, "B2017_11SubClip", format = "GTiff")
# writeRaster(C17_11Clip, "C2017_11SubClip", format = "GTiff")
# writeRaster(A16_11Clip, "A2016_11SubClip", format = "GTiff")
# writeRaster(B16_11Clip, "B2016_11SubClip", format = "GTiff")
# writeRaster(C16_11Clip, "C2016_11SubClip", format = "GTiff")
# writeRaster(A15_11Clip, "A2015_11SubClip", format = "GTiff")
# writeRaster(B15_11Clip, "B2015_11SubClip", format = "GTiff")
# writeRaster(C15_11Clip, "C2015_11SubClip", format = "GTiff")

#Make a map
ggplot(A17_09Clip, aes(x,y, fill = Elevation)) +
  geom_raster()
