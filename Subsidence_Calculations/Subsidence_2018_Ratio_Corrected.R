########################################################################################
###                Elevation Surfaces to Subsidence up through 2018                  ###
###                              Code by HGR 2017                                    ###
########################################################################################

# note that variance code predates the stacking of elevation and subsidence files and will need work to run

# load libraries
library(raster)
library(rgdal)
library(tidyverse)
library(ggthemes)
library(viridis)
library(ggsn)
library(maptools)
# library(cowplot)
library(plot3D)
library(magick)

##################### Import raster files ##############################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/', full.names = TRUE)
Elevation_fill <- list(brick(filenames[1]), brick(filenames[2]), brick(filenames[3]))
########################################################################################

### Make Elevation into matrices for gifs ##############################################
elevation.matrix <- list() # make an empty dataframe to fill with data from each block and year
for (i in 1:length(Elevation_fill)){ # repeat over each block
  elevation.matrix[[i]] <- list()
  for (k in 1:nlayers(Elevation_fill[[i]])){ # repeat over each year within each block
    elevation.matrix[[i]][[k]] <- as.matrix(Elevation_fill[[i]][[k]]) # convert to spatial pixels dataframe
  }
  rm(i, k)
}
########################################################################################

### Calculate subsidence ###############################################################
# CiPEHR
subsidenceC <- list()
for (i in 1:length(Elevation_fill)) {
  sub_stack <- stack()
  for (k in 2:nlayers(Elevation_fill[[i]])) {
   temp_raster <- Elevation_fill[[i]][[k]] - Elevation_fill[[i]][[1]]
   sub_stack <- stack(sub_stack, temp_raster)
  }
  subsidenceC[[i]] <- sub_stack
  names(subsidenceC[[i]]) <- paste('year', seq(2010, 2018), sep = '')
  rm(temp_raster, i, k, sub_stack)
}

# DryPEHR
subsidenceD <- list()
for (i in 1:length(Elevation_fill)) {
  sub_stack <- stack()
  for (k in 4:nlayers(Elevation_fill[[i]])) {
    temp_raster <- Elevation_fill[[i]][[k]] - Elevation_fill[[i]][[3]]
    sub_stack <- stack(sub_stack, temp_raster)
  }
  subsidenceD[[i]] <- sub_stack
  names(subsidenceD[[i]]) <- paste('year', seq(2012, 2018), sep = '')
  rm(temp_raster, i, k, sub_stack)
}

# Export to subsidence tif file
# CiPEHR
# writeRaster(subsidenceC[[1]], "C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/ASubStack", format = "GTiff")
# writeRaster(subsidenceC[[2]], "C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/BSubStack", format = "GTiff")
# writeRaster(subsidenceC[[3]], "C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/CSubStack", format = "GTiff")

# DryPEHR
# writeRaster(subsidenceD[[1]], "C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/ASubStack_drypehr", format = "GTiff")
# writeRaster(subsidenceD[[2]], "C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/BSubStack_drypehr", format = "GTiff")
# writeRaster(subsidenceD[[3]], "C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/CSubStack_drypehr", format = "GTiff")
########################################################################################

######################## read in subsidence data for graphing ##########################
filenames <- list.files('C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks', full.names = TRUE)
subsidenceC <- list(brick(filenames[1]), brick(filenames[3]), brick(filenames[5]))

Fences <- readOGR(dsn = path.expand("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Fences.shp"),
                  stringsAsFactors = FALSE,
                  drop_unsupported_fields = TRUE)

Fences <- fortify(Fences) %>%
  mutate(fence = as.numeric(id) + 1,
         block = ifelse(fence <= 2,
                        'A',
                        ifelse(fence >= 5,
                               'C',
                               'B')))

######################################################################################################

###### Convert subsidence to dataframe for graphing ############################################
Subsidence.df <- data.frame() # make an empty dataframe to fill with data from each block and year
for (i in 1:length(subsidenceC)){ # repeat over each block
  for (k in 1:nlayers(subsidenceC[[i]])){ # repeat over each year within each block
    temp <- as(subsidenceC[[i]][[k]], "SpatialPixelsDataFrame") %>% # convert to spatial pixels dataframe
      as.data.frame() %>% # convert to dataframe
      rename(Sub = 1) %>% # rename the subsidence column from whatever name is automatically taken from filename
      mutate(year = k + 2009, # add column with year
             block = c('A', 'B', 'C')[i]) # add column with block
    Subsidence.df <- rbind.data.frame(Subsidence.df, temp) # add the data from one block in one year above to the combined dataframe
  }
  rm(i, k, temp)
}

Subsidence.matrix <- list() # make an empty dataframe to fill with data from each block and year
for (i in 1:length(subsidenceC)){ # repeat over each block
  Subsidence.matrix[[i]] <- list()
  for (k in 1:nlayers(subsidenceC[[i]])){ # repeat over each year within each block
    Subsidence.matrix[[i]][[k]] <- as.matrix(subsidenceC[[i]][[k]]) # convert to spatial pixels dataframe
  }
  rm(i, k)
}
###############################################################################################

### In order to graph nicely, it is easiest to normalize the coordinates on each block to 0 in the southwest corner ######
min.coords <- Subsidence.df %>%
  group_by(block) %>%
  summarize(x.min = min(x),
            y.min = min(y))

Subsidence.df <- Subsidence.df %>%
  left_join(min.coords, by = 'block') %>%
  mutate(long.norm = round(x - x.min), # have to round due to bug in ggplot. Details here: https://stackoverflow.com/questions/18157975/combine-geom-tile-and-facet-grid-facet-wrap-and-remove-space-between-tiles-gg
         lat.norm = round(y - y.min)) %>%
  select(-x.min, -y.min)

Fences.norm <- Fences %>%
  left_join(min.coords, by = 'block') %>%
  mutate(long.norm = round(long - x.min),
         lat.norm = round(lat - y.min),
         dummy = '') %>%
  select(-x.min, -y.min)

# map with all years (including gap filled data)
sub_map_filled <- ggplot(Subsidence.df, aes(x=long.norm, y=lat.norm, fill=Sub)) +
  geom_tile(aes(height = 2, width = 2)) + # have to set height and width due to bug. See note/link above.
  geom_path(data = Fences.norm, aes(x=long.norm, y=lat.norm, group=group, color = dummy), inherit.aes = FALSE) +
  facet_grid(block ~ year) +
  coord_fixed() +
  theme_few() +
  scale_fill_viridis(expression(Delta*" Elevation (m)"),
                     limits = c(-1.0, 0.5),
                     direction = -1) +
  scale_color_manual('Snow Fence',
                     values = 'black') +
  scale_x_continuous(name = 'Distance (m)') +
  scale_y_continuous(name = 'Distance (m)') +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  ggtitle('Subsidence at CiPEHR/DryPEHR\n(Relative to 2009)')

sub_map_filled

# map with all years (including gap filled data)
sub_map <- ggplot(subset(Subsidence.df, year == 2011 | year >= 2015), aes(x=long.norm, y=lat.norm, fill=Sub)) +
  geom_tile(aes(height = 2, width = 2)) + # have to set height and width due to bug. See note/link above.
  geom_path(data = Fences.norm, aes(x=long.norm, y=lat.norm, group=group, color = dummy), inherit.aes = FALSE) +
  facet_grid(block ~ year) +
  coord_fixed() +
  theme_few() +
  scale_fill_viridis(expression(Delta*" Elevation (m)"),
                     limits = c(-1.0, 0.5),
                     direction = -1) +
  scale_color_manual('Snow Fence',
                     values = 'black') +
  scale_x_continuous(name = 'Distance (m)') +
  scale_y_continuous(name = 'Distance (m)') +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  ggtitle('Subsidence at CiPEHR/DryPEHR\n(Relative to 2009)')

sub_map

# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Ratio_Corrected.jpg', sub_map)
# ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Ratio_Corrected.pdf', sub_map)
###########################################################################################

# # create a variance data frame
# A2009var_df <- as(A2009var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = A2009Var) %>%
#   mutate(year = 2009,
#          block = 'A')
# 
# B2009var_df <- as(B2009var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = B2009Var) %>%
#   mutate(year = 2009,
#          block = 'B')
# 
# C2009var_df <-  as(C2009var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = C2009Var) %>%
#   mutate(year = 2009,
#          block = 'C')
# 
# A2011var_df <- as(A2011var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = A2011Var) %>%
#   mutate(year = 2011,
#          block = 'A')
# 
# B2011var_df <- as(B2011var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = B2011Var) %>%
#   mutate(year = 2011,
#          block = 'B')
# 
# C2011var_df <- as(C2011var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = C2011Var) %>%
#   mutate(year = 2011,
#          block = 'C')
# 
# A2015var_df <- as(A2015var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = A2015Var) %>%
#   mutate(year = 2015,
#          block = 'A')
# 
# B2015var_df <- as(B2015var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = B2015Var) %>%
#   mutate(year = 2015,
#          block = 'B')
# 
# C2015var_df <- as(C2015var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = C2015Var) %>%
#   mutate(year = 2015,
#          block = 'C')
# 
# A2016var_df <- as(A2016var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = A2016Var) %>%
#   mutate(year = 2016,
#          block = 'A')
# 
# B2016var_df <- as(B2016var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = B2016Var) %>%
#   mutate(year = 2016,
#          block = 'B')
# 
# C2016var_df <- as(C2016var_clip, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = C2016Var) %>%
#   mutate(year = 2016,
#          block = 'C')
# 
# A2017var_df <- as(A2017var, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = A2017Var) %>%
#   mutate(year = 2017,
#          block = 'A')
# 
# B2017var_df <- as(B2017var, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = B2017Var) %>%
#   mutate(year = 2017,
#          block = 'B')
# 
# C2017var_df <- as(C2017var, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = C2017Var) %>%
#   mutate(year = 2017,
#          block = 'C')
# 
# A2018var_df <- as(A2018var, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = A2018Var) %>%
#   mutate(year = 2018,
#          block = 'A')
# 
# B2018var_df <- as(B2018var, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = B2018Var) %>%
#   mutate(year = 2018,
#          block = 'B')
# 
# C2018var_df <- as(C2018var, "SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(var = C2018Var) %>%
#   mutate(year = 2018,
#          block = 'C')
# ##########################################
# 
# # Join into one data frame for graphing
# Variance.df <- A2009var_df %>%
#   rbind.data.frame(B2009var_df, C2009var_df,
#                    A2011var_df, B2011var_df, C2011var_df,
#                    A2015var_df, B2015var_df, C2015var_df,
#                    A2016var_df, B2016var_df, C2016var_df,
#                    A2017var_df, B2017var_df, C2017var_df,
#                    A2018var_df, B2018var_df, C2018var_df) %>%
#   rename(long = x, lat = y)
# 
# 
# Variance.df <- Variance.df %>%
#   left_join(min.coords, by = 'block') %>%
#   mutate(long.norm = round(long - x.min), # have to round due to bug in ggplot. Details here: https://stackoverflow.com/questions/18157975/combine-geom-tile-and-facet-grid-facet-wrap-and-remove-space-between-tiles-gg
#          lat.norm = round(lat - y.min)) %>%
#   select(-x.min, -y.min)
# 
# 
# ### need to clip 2009-2016 before plotting!
# # Graph all in one go!
# var_map <- ggplot(Variance.df, aes(x=long.norm, y=lat.norm, fill=var)) +
#   geom_tile(aes(height = 2, width = 2)) + # have to set height and width due to bug. See note/link above.
#   geom_path(data = Fences.norm, aes(x=long.norm, y=lat.norm, group=group, color = dummy), inherit.aes = FALSE) +
#   facet_grid(block ~ year) +
#   coord_fixed() +
#   theme_few() +
#   scale_fill_viridis("Variance",
#                      direction = -1) +
#   scale_color_manual('Snow Fence',
#                      values = 'black') +
#   scale_x_continuous(name = 'Distance (m)') +
#   scale_y_continuous(name = 'Distance (m)') +
#   theme(aspect.ratio = 1,
#         plot.title = element_text(hjust = 0.5),
#         axis.title.x = element_text(size = 10),
#         axis.title.y = element_text(size = 10)) +
#   ggtitle('Variance of the Elevation Surfaces')
# 
# var_map
# 
# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Variance.jpg", var_map)
# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Variance.pdf", var_map)
# 

# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Ratio_Corrected.jpg", sub_map)
# ggsave("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/Subsidence_Ratio_Corrected.pdf", sub_map)

###### I decided not to include a north arrow because it doesn't make any sense anway #################################
# adding the following causes problems...
  # north(Subsidence.df, symbol = 12, location = 'bottomleft') +
  # scalebar(Subsidence.df, dd2km = FALSE, dist = 0.01, st.size = 1, facet.var = c('block', 'year'), facet.lev = c('C', 2011))

# Add north arrow by making a new empty plot and adding arrow to it, then joining with other figure using cowplot
# make dummy data frame
# data <- data.frame(long = c(0, 1, 0, 1),
#                    lat = c(0, 1, 1, 0)) %>%
#   mutate(value = '')
# 
# north_arrow <- ggplot(data, aes(x = long, y = lat)) +
#   geom_tile(alpha = 0) +
#   scale_x_continuous(limits = c(0, 1)) +
#   scale_y_continuous(limits = c(0,1)) +
#   coord_fixed() +
#   theme_few() +
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         plot.margin = c(0,0,0,0), 'cm') +
#   north(data, symbol = 12, x.min = 0, x.max = 1, y.min = 0, y.max = 1, location = 'bottomleft', scale = 1)
# 
# north_arrow
# 
# map <- ggdraw() +
#   draw_plot(sub_map, 0, 0, 1, 1) +
#   draw_plot(north_arrow, 0.065, 0.11, 0.055, 0.055)
# 
# map
############################################################################################################################


### Make a gif of subsidence ###############################################################################################
# start by plotting in 3D
subsidence.3D <- list()
for (i in 1:length(Subsidence.matrix)){
  img <- image_graph(1000, 1000, res = 96)
  nlong <- (dim(Subsidence.matrix[[i]][[1]])[2]-1)*2
  nlat <- (dim(Subsidence.matrix[[i]][[1]])[1]-1)*2
  x <- matrix(rep(seq(0, nlong, by = 2), (nlat/2)+1), ncol = (nlong/2)+1, byrow = TRUE)
  y <- matrix(rep(seq(nlat, 0, by = -2), (nlong/2)+1), ncol = (nlong/2)+1)
  for(k in 1:length(Subsidence.matrix[[i]])){
    surf3D(x = x, 
           y = y, 
           z = Subsidence.matrix[[i]][[k]], 
           colvar = Subsidence.matrix[[i]][[k]], 
           col = viridis(1024, begin = 0, end = 1), 
           phi = 60, 
           theta = -70, 
           clim = c(-1, 0.5), 
           zlim = c(-1, 0.5), 
           colkey = FALSE)
  }
  dev.off()
  subsidence.3D[[i]] <- image_animate(img, fps = 2)
}

print(subsidence.3D[[1]])
print(subsidence.3D[[2]])
print(subsidence.3D[[3]])

# for (i in 1:length(subsidence.3D)) {
#   image_write(subsidence.3D[[i]], paste('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/', c('A', 'B', 'C')[i], 'subsidence.gif', sep = ''))
# }

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
           colkey = FALSE,
           main = paste(2008+j))
    if (j == 10){
      for (k in 1:10){
        surf3D(x = x, 
               y = y, 
               z = elevation.matrix[[i]][[10]], 
               colvar = elevation.matrix[[i]][[10]], 
               col = viridis(1024, begin = 0, end = 1), 
               phi = 60, 
               theta = -70, 
               clim = c(zlower, zupper), 
               zlim = c(zlower, zupper), 
               colkey = FALSE,
               main = '2018')
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
#   image_write(elevation.3D[[i]], paste('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Figures/', c('A', 'B', 'C')[i], 'elevation.gif', sep = ''))
# }
