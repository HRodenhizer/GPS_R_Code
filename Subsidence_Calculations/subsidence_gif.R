########################################################################################
###                Elevation Surfaces to Subsidence up through 2018                  ###
###                              Code by HGR 2017                                    ###
########################################################################################

# load libraries
library(raster)
# library(rgdal)
# library(ggthemes)
# library(viridis)
# library(ggsn)
# library(maptools)
# # library(cowplot)
# # library(plot3D)
# library(magick)
# library(sf)
library(rayshader)
library(tidyverse)

##################### Import raster files ##############################################
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks', 
                        full.names = TRUE,
                        pattern = 'ElevStack_filled_clip')
elevation_fill <- map(filenames,
                      ~ brick(.x))
# load extent data
# blocks <- readOGR('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Blocks_Poly.shp')
# blocks11 <- readOGR('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Blocks_Poly_2011.shp')
########################################################################################

### Make Elevation into matrices for gifs ##############################################
multiband_raster_to_matrix <- function(multiband_raster) {
  bands <- seq(1, nlayers(multiband_raster))
  output <- map(bands,
                ~ raster_to_matrix(multiband_raster[[.x]]))
  return(output)
}
elevation_matrix <- map(elevation_fill,
                        ~ multiband_raster_to_matrix(.x))
########################################################################################

### Make a gif of subsidence ##########################################################
loadzip = tempfile() 
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()

# start by plotting in 3D
elevation_matrix[[2]][[12]] %>%
  sphere_shade(sunangle = 45) %>%
  plot_3d(elevation_matrix[[2]][[12]], zscale = 100, fov = 0, theta = 0, windowsize = c(600, 600))
Sys.sleep(0.2)
render_snapshot()

elevation.3D <- list()
for (i in 1:length(elevation.matrix)){
  zlower <- min(elevation.matrix[[i]][[10]], na.rm = TRUE) - 0.3
  zupper <- max(elevation.matrix[[i]][[1]], na.rm = TRUE) + 0.3
  img <- image_graph(1000, 1000, res = 96)
  nlong <- as.numeric(dim(elevation.matrix[[i]][[1]])[2])
  nlat <- as.numeric(dim(elevation.matrix[[i]][[1]])[1])
  x <- matrix(rep(seq(0, nlong-1, by = 1), nlat), ncol = nlong, byrow = TRUE)
  y <- matrix(rep(seq(nlat-1, 0, by = -1), nlong), ncol = nlong)
  for(j in 1:length(elevation.matrix[[i]])){
    surf3D(x = x, 
           y = y, 
           z = elevation.matrix[[i]][[j]], 
           colvar = elevation.matrix[[i]][[j]], 
           col = viridis(1024, begin = 0, end = 1), 
           phi = 60, 
           theta = -70, 
           clim = c(zlower, zupper), 
           zlim = c(zlower, zupper), 
           colkey = FALSE)
    title(paste(2008+j), line = -10)
    image_crop(img, geometry_area(600, 600, 250, 200), repage = FALSE)
    if (j == 10){
      for (k in 1:10){
        surf3D(x = x, 
               y = y, 
               z = elevation.matrix[[i]][[j]], 
               colvar = elevation.matrix[[i]][[j]], 
               col = viridis(1024, begin = 0, end = 1), 
               phi = 60, 
               theta = -70, 
               clim = c(zlower, zupper), 
               zlim = c(zlower, zupper), 
               colkey = FALSE)
        title(paste(2008+j), line = -10)
        image_crop(img, geometry_area(600, 600, 250, 200), repage = FALSE)
      }
    }
  }
  dev.off()
  elevation.3D[[i]] <- image_animate(img, fps = 2)
}

print(elevation.3D[[1]])
print(elevation.3D[[2]])
print(elevation.3D[[3]])

# for (i in 1:length(elevation.3D)) {
#   image_write(elevation.3D[[i]], paste('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Figures/', c('A', 'B', 'C')[i], 'elevation_v2.gif', sep = ''))
# }
img <- image_graph(500, 500, res = 96)
zlower <- min(elevation.matrix[[1]][[10]], na.rm = TRUE) - 0.3
zupper <- max(elevation.matrix[[1]][[1]], na.rm = TRUE) + 0.3
nlong <- as.numeric(dim(elevation.matrix[[1]][[10]])[2])
nlat <- as.numeric(dim(elevation.matrix[[1]][[10]])[1])
x <- matrix(rep(seq(0, nlong-1, by = 1), nlat), ncol = nlong, byrow = TRUE)
y <- matrix(rep(seq(nlat-1, 0, by = -1), nlong), ncol = nlong)
surf3D(x = x, 
       y = y, 
       z = elevation.matrix[[1]][[10]], 
       colvar = elevation.matrix[[1]][[10]], 
       col = viridis(1024, begin = 0, end = 1), 
       phi = 60, 
       theta = -70, 
       clim = c(zlower, zupper), 
       zlim = c(zlower, zupper), 
       colkey = FALSE)
title(paste(2008+10), line = -5)
image_crop(img, geometry_area(300, 300, 125, 100), repage = FALSE)
dev.off()
#######################################################################################