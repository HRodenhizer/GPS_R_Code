########################################################################################
###                Elevation Surfaces to Subsidence up through 2018                  ###
###                              Code by HGR 2017                                    ###
########################################################################################

# load libraries
library(raster)
library(rgdal)
library(tidyverse)
library(ggthemes)
library(viridis)
library(ggsn)
library(maptools)
library(cowplot)
library(rgdal)

##################### Import raster files ########################################
# A2009raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/A2009RatioCorrected.tif")
# B2009raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/B2009RatioCorrected.tif")
# C2009raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/C2009RatioCorrected.tif")
# A2011raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/A2011RatioCorrected.tif")
# B2011raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/B2011RatioCorrected.tif")
# C2011raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/C2011RatioCorrected.tif")
# A2015raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/A2015RatioCorrected.tif")
# B2015raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/B2015RatioCorrected.tif")
# C2015raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/C2015RatioCorrected.tif")
# A2016raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2016K.tif")
# B2016raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2016K.tif")
# C2016raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2016K.tif")
# A2017raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2017K.tif")
# B2017raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2017K.tif")
# C2017raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2017K.tif")
# A2018raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/A2018K.tif")
# B2018raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/B2018K.tif")
# C2018raster <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Uncorrected_kriged_elevation/C2018K.tif")
# A2009var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2009var.tif")
# B2009var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2009var.tif")
# C2009var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2009var.tif")
# A2011var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2011var.tif")
# B2011var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2011var.tif")
# C2011var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2011var.tif")
# A2015var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2015var.tif")
# B2015var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2015var.tif")
# C2015var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2015var.tif")
# A2016var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2016var.tif")
# B2016var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2016var.tif")
# C2016var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2016var.tif")
# A2017var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2017var.tif")
# B2017var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2017var.tif")
# C2017var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2017var.tif")
# A2018var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2018var.tif")
# B2018var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2018var.tif")
# C2018var <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2018var.tif")

filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/', full.names = TRUE)
Elevation_fill <- list(brick(filenames[1]), brick(filenames[2]), brick(filenames[3]))
########################################################################################

#Calculate subsidence - finally!
# CiPEHR
# A18_09Sub <- A2018raster-A2009raster
# B18_09Sub <- B2018raster-B2009raster
# C18_09Sub <- C2018raster-C2009raster
# A17_09Sub <- A2017raster-A2009raster
# B17_09Sub <- B2017raster-B2009raster
# C17_09Sub <- C2017raster-C2009raster
# A16_09Sub <- A2016raster-A2009raster
# B16_09Sub <- B2016raster-B2009raster
# C16_09Sub <- C2016raster-C2009raster
# A15_09Sub <- A2015raster-A2009raster
# B15_09Sub <- B2015raster-B2009raster
# C15_09Sub <- C2015raster-C2009raster
# A11_09Sub <- A2011raster-A2009raster
# B11_09Sub <- B2011raster-B2009raster
# C11_09Sub <- C2011raster-C2009raster

subsidence <- list()
sub_stack <- stack()
for (i in 1:length(Elevation_fill)) {
  for (k in 2:nlayers(Elevation_fill[i])) {
   temp_raster <- Elevation_fill[i][k] - Elevation_fill[i][1]
   sub_stack <- stack(sub_stack, temp_raster)
  }
  subsidence[i] <- sub_stack
  rm(temp_raster, sub_stack, i, k)
}

# DryPEHR
A18_11Sub <- A2018raster-A2011raster
B18_11Sub <- B2018raster-B2011raster
C18_11Sub <- C2018raster-C2011raster
A17_11Sub <- A2017raster-A2011raster
B17_11Sub <- B2017raster-B2011raster
C17_11Sub <- C2017raster-C2011raster
A16_11Sub <- A2016raster-A2011raster
B16_11Sub <- B2016raster-B2011raster
C16_11Sub <- C2016raster-C2011raster
A15_11Sub <- A2015raster-A2011raster
B15_11Sub <- B2015raster-B2011raster
C15_11Sub <- C2015raster-C2011raster

### Clip subsidence files to exclude areas not measured in
# 2009-2016 (CiPEHR - pre DryPEHR we didn't measure GPS far enough out to include future DryPEHR Plots)
# or not measured in 2011-2016 (DryPEHR) ###

#Import shapefiles for mapping
Blocks <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles"),
                  layer = "Blocks_Poly",
                  stringsAsFactors = FALSE,
                  drop_unsupported_fields = TRUE)

Blocks11 <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles"),
                    layer = "Blocks_Poly_2011",
                    stringsAsFactors = FALSE,
                    drop_unsupported_fields = TRUE)

Fences <- readOGR(dsn = path.expand("~/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles"),
                  layer = "Fences",
                  stringsAsFactors = FALSE,
                  drop_unsupported_fields = TRUE)

Fences <- fortify(Fences) %>%
  mutate(fence = as.numeric(id) + 1,
         block = ifelse(fence <= 2,
                        'A',
                        ifelse(fence >= 5,
                               'C',
                               'B')))

#Turn Blocks into separate rasters for each block
# CiPEHR
BlockA <- rasterize(Blocks, A2017raster)
BlockB <- rasterize(Blocks, B2017raster)
BlockC <- rasterize(Blocks, C2017raster)
# DryPEHR
BlockA11 <- rasterize(Blocks11, A2017raster)
BlockB11 <- rasterize(Blocks11, B2017raster)
BlockC11 <- rasterize(Blocks11, C2017raster)

# Mask the areas outside of blocks for subsidence
# CiPEHR
A18_09Clip <- mask(A18_09Sub, BlockA)
B18_09Clip <- mask(B18_09Sub, BlockB)
C18_09Clip <- mask(C18_09Sub, BlockC)
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
# DryPEHR
A18_11Clip <- mask(A18_11Sub, BlockA11)
B18_11Clip <- mask(B18_11Sub, BlockB11)
C18_11Clip <- mask(C18_11Sub, BlockC11)
A17_11Clip <- mask(A17_11Sub, BlockA11)
B17_11Clip <- mask(B17_11Sub, BlockB11)
C17_11Clip <- mask(C17_11Sub, BlockC11)
A16_11Clip <- mask(A16_11Sub, BlockA11)
B16_11Clip <- mask(B16_11Sub, BlockB11)
C16_11Clip <- mask(C16_11Sub, BlockC11)
A15_11Clip <- mask(A15_11Sub, BlockA11)
B15_11Clip <- mask(B15_11Sub, BlockB11)
C15_11Clip <- mask(C15_11Sub, BlockC11)

# mask variance
A2009var_clip <- mask(A2009var, BlockA)
B2009var_clip <- mask(B2009var, BlockB)
C2009var_clip <- mask(C2009var, BlockC)
A2011var_clip <- mask(A2011var, BlockA)
B2011var_clip <- mask(B2011var, BlockB)
C2011var_clip <- mask(C2011var, BlockC)
A2016var_clip <- mask(A2016var, BlockA)
B2016var_clip <- mask(B2016var, BlockB)
C2016var_clip <- mask(C2016var, BlockC)
A2015var_clip <- mask(A2015var, BlockA)
B2015var_clip <- mask(B2015var, BlockB)
C2015var_clip <- mask(C2015var, BlockC)

# # Export to subsidence tif file
# setwd("C://Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected")

# CiPEHR
# writeRaster(A18_09Clip, "A2018Sub_RatioCorrected", format = "GTiff")
# writeRaster(B18_09Clip, "B2018Sub_RatioCorrected", format = "GTiff")
# writeRaster(C18_09Clip, "C2018Sub_RatioCorrected", format = "GTiff")
# writeRaster(A17_09Clip, "A2017Sub_RatioCorrected", format = "GTiff")
# writeRaster(B17_09Clip, "B2017Sub_RatioCorrected", format = "GTiff")
# writeRaster(C17_09Clip, "C2017Sub_RatioCorrected", format = "GTiff")
# writeRaster(A16_09Clip, "A2016Sub_RatioCorrected", format = "GTiff")
# writeRaster(B16_09Clip, "B2016Sub_RatioCorrected", format = "GTiff")
# writeRaster(C16_09Clip, "C2016Sub_RatioCorrected", format = "GTiff")
# writeRaster(A15_09Clip, "A2015Sub_RatioCorrected", format = "GTiff")
# writeRaster(B15_09Clip, "B2015Sub_RatioCorrected", format = "GTiff")
# writeRaster(C15_09Clip, "C2015Sub_RatioCorrected", format = "GTiff")
# writeRaster(A11_09Clip, "A2011Sub_RatioCorrected", format = "GTiff")
# writeRaster(B11_09Clip, "B2011Sub_RatioCorrected", format = "GTiff")
# writeRaster(C11_09Clip, "C2011Sub_RatioCorrected", format = "GTiff")

# DryPEHR
# writeRaster(A18_11Clip, "A18_11Sub_ratio_corrected.tif")
# writeRaster(B18_11Clip, "B18_11Sub_ratio_corrected.tif")
# writeRaster(C18_11Clip, "C18_11Sub_ratio_corrected.tif")
# writeRaster(A17_11Clip, "A17_11Sub_ratio_corrected.tif")
# writeRaster(B17_11Clip, "B17_11Sub_ratio_corrected.tif")
# writeRaster(C17_11Clip, "C17_11Sub_ratio_corrected.tif")
# writeRaster(A16_11Clip, "A16_11Sub_ratio_corrected.tif")
# writeRaster(B16_11Clip, "B16_11Sub_ratio_corrected.tif")
# writeRaster(C16_11Clip, "C16_11Sub_ratio_corrected.tif")
# writeRaster(A15_11Clip, "A15_11Sub_ratio_corrected.tif")
# writeRaster(B15_11Clip, "B15_11Sub_ratio_corrected.tif")
# writeRaster(C15_11Clip, "C15_11Sub_ratio_corrected.tif")

# Variance
# writeRaster(A2009var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2009Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(B2009var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2009Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(C2009var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2009Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(A2011var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2011Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(B2011var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2011Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(C2011var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2011Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(A2015var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2015Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(B2015var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2015Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(C2015var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2015Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(A2016var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2016Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(B2016var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2016Var.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(C2016var_clip, "C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2016Var.tif", format = "GTiff", overwrite = TRUE)

######################## read in subsidence data for graphing ########################################
A11_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/A2011Sub_RatioCorrected.tif')
B11_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/B2011Sub_RatioCorrected.tif')
C11_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/C2011Sub_RatioCorrected.tif')
A15_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/A2015Sub_RatioCorrected.tif')
B15_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/B2015Sub_RatioCorrected.tif')
C15_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/C2015Sub_RatioCorrected.tif')
A16_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/A2016Sub_RatioCorrected.tif')
B16_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/B2016Sub_RatioCorrected.tif')
C16_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/C2016Sub_RatioCorrected.tif')
A17_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/A2017Sub_RatioCorrected.tif')
B17_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/B2017Sub_RatioCorrected.tif')
C17_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/C2017Sub_RatioCorrected.tif')
A18_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/A2018Sub_RatioCorrected.tif')
B18_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/B2018Sub_RatioCorrected.tif')
C18_09Clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/C2018Sub_RatioCorrected.tif')
A2009var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2009Var.tif')
B2009var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2009Var.tif')
C2009var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2009Var.tif')
A2011var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2011Var.tif')
B2011var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2011Var.tif')
C2011var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2011Var.tif')
A2015var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2015Var.tif')
B2015var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2015Var.tif')
C2015var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2015Var.tif')
A2016var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2016Var.tif')
B2016var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2016Var.tif')
C2016var_clip <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2016Var.tif')
A2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2017Var.tif')
B2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2017Var.tif')
C2017var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2017Var.tif')
A2018var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/A2018Var.tif')
B2018var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/B2018Var.tif')
C2018var <- raster('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/Variance/C2018Var.tif')
######################################################################################################

### Graph subsidence ###
###### Use dataframe for graphing ######
A11_09_df <- as(A11_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = A2011Sub_RatioCorrected) %>%
  mutate(year = 2011,
         block = 'A')

B11_09_df <- as(B11_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = B2011Sub_RatioCorrected) %>%
  mutate(year = 2011,
         block = 'B')

C11_09_df <-  as(C11_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = C2011Sub_RatioCorrected) %>%
  mutate(year = 2011,
         block = 'C')

A15_09_df <- as(A15_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = A2015Sub_RatioCorrected) %>%
  mutate(year = 2015,
         block = 'A')

B15_09_df <- as(B15_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = B2015Sub_RatioCorrected) %>%
  mutate(year = 2015,
         block = 'B')

C15_09_df <-  as(C15_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = C2015Sub_RatioCorrected) %>%
  mutate(year = 2015,
         block = 'C')

A16_09_df <- as(A16_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = A2016Sub_RatioCorrected) %>%
  mutate(year = 2016,
         block = 'A')

B16_09_df <- as(B16_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = B2016Sub_RatioCorrected) %>%
  mutate(year = 2016,
         block = 'B')

C16_09_df <- as(C16_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = C2016Sub_RatioCorrected) %>%
  mutate(year = 2016,
         block = 'C')

A17_09_df <- as(A17_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = A2017Sub_RatioCorrected) %>%
  mutate(year = 2017,
         block = 'A')

B17_09_df <- as(B17_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = B2017Sub_RatioCorrected) %>%
  mutate(year = 2017,
         block = 'B')

C17_09_df <- as(C17_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = C2017Sub_RatioCorrected) %>%
  mutate(year = 2017,
         block = 'C')

A18_09_df <- as(A18_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = A2018Sub_RatioCorrected) %>%
  mutate(year = 2018,
         block = 'A')

B18_09_df <- as(B18_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = B2018Sub_RatioCorrected) %>%
  mutate(year = 2018,
         block = 'B')

C18_09_df <- as(C18_09Clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(Sub = C2018Sub_RatioCorrected) %>%
  mutate(year = 2018,
         block = 'C')
##########################################

Subsidence.df <- A11_09_df %>%
  rbind.data.frame(B11_09_df, C11_09_df, 
                   A15_09_df, B15_09_df, C15_09_df, 
                   A16_09_df, B16_09_df, C16_09_df, 
                   A17_09_df, B17_09_df, C17_09_df,
                   A18_09_df, B18_09_df, C18_09_df)

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

sub_map <- ggplot(Subsidence.df, aes(x=long.norm, y=lat.norm, fill=Sub)) +
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

# create a variance data frame
A2009var_df <- as(A2009var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = A2009Var) %>%
  mutate(year = 2009,
         block = 'A')

B2009var_df <- as(B2009var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = B2009Var) %>%
  mutate(year = 2009,
         block = 'B')

C2009var_df <-  as(C2009var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = C2009Var) %>%
  mutate(year = 2009,
         block = 'C')

A2011var_df <- as(A2011var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = A2011Var) %>%
  mutate(year = 2011,
         block = 'A')

B2011var_df <- as(B2011var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = B2011Var) %>%
  mutate(year = 2011,
         block = 'B')

C2011var_df <- as(C2011var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = C2011Var) %>%
  mutate(year = 2011,
         block = 'C')

A2015var_df <- as(A2015var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = A2015Var) %>%
  mutate(year = 2015,
         block = 'A')

B2015var_df <- as(B2015var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = B2015Var) %>%
  mutate(year = 2015,
         block = 'B')

C2015var_df <- as(C2015var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = C2015Var) %>%
  mutate(year = 2015,
         block = 'C')

A2016var_df <- as(A2016var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = A2016Var) %>%
  mutate(year = 2016,
         block = 'A')

B2016var_df <- as(B2016var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = B2016Var) %>%
  mutate(year = 2016,
         block = 'B')

C2016var_df <- as(C2016var_clip, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = C2016Var) %>%
  mutate(year = 2016,
         block = 'C')

A2017var_df <- as(A2017var, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = A2017Var) %>%
  mutate(year = 2017,
         block = 'A')

B2017var_df <- as(B2017var, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = B2017Var) %>%
  mutate(year = 2017,
         block = 'B')

C2017var_df <- as(C2017var, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = C2017Var) %>%
  mutate(year = 2017,
         block = 'C')

A2018var_df <- as(A2018var, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = A2018Var) %>%
  mutate(year = 2018,
         block = 'A')

B2018var_df <- as(B2018var, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = B2018Var) %>%
  mutate(year = 2018,
         block = 'B')

C2018var_df <- as(C2018var, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(var = C2018Var) %>%
  mutate(year = 2018,
         block = 'C')
##########################################

# Join into one data frame for graphing
Variance.df <- A2009var_df %>%
  rbind.data.frame(B2009var_df, C2009var_df,
                   A2011var_df, B2011var_df, C2011var_df,
                   A2015var_df, B2015var_df, C2015var_df,
                   A2016var_df, B2016var_df, C2016var_df,
                   A2017var_df, B2017var_df, C2017var_df,
                   A2018var_df, B2018var_df, C2018var_df) %>%
  rename(long = x, lat = y)


Variance.df <- Variance.df %>%
  left_join(min.coords, by = 'block') %>%
  mutate(long.norm = round(long - x.min), # have to round due to bug in ggplot. Details here: https://stackoverflow.com/questions/18157975/combine-geom-tile-and-facet-grid-facet-wrap-and-remove-space-between-tiles-gg
         lat.norm = round(lat - y.min)) %>%
  select(-x.min, -y.min)


### need to clip 2009-2016 before plotting!
# Graph all in one go!
var_map <- ggplot(Variance.df, aes(x=long.norm, y=lat.norm, fill=var)) +
  geom_tile(aes(height = 2, width = 2)) + # have to set height and width due to bug. See note/link above.
  geom_path(data = Fences.norm, aes(x=long.norm, y=lat.norm, group=group, color = dummy), inherit.aes = FALSE) +
  facet_grid(block ~ year) +
  coord_fixed() +
  theme_few() +
  scale_fill_viridis("Variance",
                     direction = -1) +
  scale_color_manual('Snow Fence',
                     values = 'black') +
  scale_x_continuous(name = 'Distance (m)') +
  scale_y_continuous(name = 'Distance (m)') +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  ggtitle('Variance of the Elevation Surfaces')

var_map
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


# Calculate some stats
# Mean and variance by treatment