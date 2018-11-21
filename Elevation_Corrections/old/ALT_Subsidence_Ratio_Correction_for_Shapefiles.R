########################################################################################################################
###                            Code for Elevation Corrections for GPS Shapefiles                                     ###
###                                          Code by HGR 9/18                                                        ###
########################################################################################################################

# packages needed for code
library(tidyverse)
library(sf)

# import shapefiles from all_points folder
lst <- list.files("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points", pattern = ".shp$", full.names = TRUE)
lst <- lst[-7]
shapefiles <- lapply(lst, read_sf)

# separate list of shapefiles
Points2009 <- shapefiles[[1]]
Points2011 <- shapefiles[[2]]

# Correct elevation values
Points2009corrected <- Points2009 %>%
  mutate(Elevation = Elevation + 1.2753)

Points2011corrected <- Points2011 %>%
  mutate(Elevation = Elevation + 1.2753)

# write_sf(Points2009corrected, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2009_SPCSAK4_corrected.shp")
# write_sf(Points2011corrected, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2011_SPCSAK4_corrected.shp")
