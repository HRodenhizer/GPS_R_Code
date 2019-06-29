################################################################################################################
###                                 Prep 2017 Soil Core Data for Analysis                                    ###
###                                           Code by HGR 6/2019                                             ###
################################################################################################################

### Load libraries #############################################################################################
library(tidyverse)
library(readxl)
################################################################################################################

### Load data ##################################################################################################
soil_09_13 <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2009-2013 CiPEHR Processed SOIL DATA_C�sar.xlsx",
                         sheet = 1) %>%
  separate(depth.cat, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  mutate(depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1))
soil_17 <- read_excel('Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2017/2017 Soils_processing data sheet_3_8_18.xlsx',
                      sheet = 2)
ash_17 <- read_excel('Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2017/Ash_Mass_Data_2017.xlsx')
# this will end up being a bunch of files
cn_17 <- read_excel('Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2017/2017 CiPHER soil cores _CN data raw_tray one.xlsx',
                    sheet = 2) %>%
  select(1:7) %>%
  rename(ID = `Final Result`)
################################################################################################################

### Join ash and cn to soil data ###############################################################################
# commented out sections will need to be incorporated once all data have been collected
# format ash data
ash_17_v2 <- ash_17 %>%
  mutate(ash = `Ash Mass (%)`*1000) %>% # g ash/g soil * 1000g/1kg = g ash/kg soil
  separate(depth, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  mutate(depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1)) %>%
  select(-c(5:9))

# join all the cn data together
# cn_17_v2 <- cn_17_1 %>% # might need to remove %C and replace with %C real for some or all of the files
#   rbind.data.frame(cn_17_2) # add other files here and average 4-2.17 25-35

# this layer got split into two for ash and CN analysis and needs to be averaged into one layer before joining with soil data
avg_ash_4_2_17 <- ash_17_v2 %>%
  filter(`well#` == '4-2.17' & depth == '25-32' | `well#` == '4-2.17' & depth == '32-35') %>%
  group_by(`well#`) %>%
  summarise(new.ash = sum(ash*(depth1-depth0)/10)) %>% # average by depth rather than stock (mass/soil core area) because bulk.density only available for 25-35 as a whole
  mutate(depth = '25-35',
         new.depth0 = 25,
         new.depth1 = 35)

# average this split layer prior to joining with soil data like the split ash layer above
# avg_cn_4_2_17 <- cn17_v2 %>%
#   filter(ID == S126 | ID == S214) %>%
#   mutate(depth0 = ifelse(ID == S126,
#                          25,
#                          32),
#          depth1 = ifelse(ID == S214,
#                          32,
#                          35)) %>%
#   summarise(`new.%C` = sum(`%C`*(depth1-depth0)/10),
#             `new.%N` = sum(`%N`*(depth1-depth0)/10),
#             new.d13C = sum(d13C*(depth1-depth0)/10),
#             new.d15N = sum(d15N*(depth1-depth0)/10)) %>% # average by depth rather than stock (mass/soil core area) because bulk.density only available for 25-35 as a whole
#   mutate(depth = '25-35',
#          new.depth0 = 25,
#          new.depth1 = 35)

# replace incorrect depth value in core 5-6
soil_17 <- soil_17 %>%
  mutate(depth = ifelse(Fence == 5 & plot == 6 & depth == '0-6.5' | Fence == 6 & plot == 8 & depth == '0-6.5',
                        '0-5',
                        depth))

# join soil, ash, cn data for 2017
soil_17_v2 <- soil_17 %>%
  select(-c(5, 9:11)) %>%
  separate(depth, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  mutate(depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1)) %>%
  full_join(filter(ash_17_v2, !(`well#` == '4-2.17' & depth == '25-32' | 
                                  `well#` == '4-2.17' & depth == '32-35')), 
            by = c('well#', 'depth', 'depth0', 'depth1')) %>%
  full_join(avg_ash_4_2_17, by = c('well#', 'depth')) %>%
  left_join(cn_17, by = 'ID') %>% # change to join with filter(cn_17_v2, !(ID == S126 | ID == S214))
  # full_join(avg_cn_4_2_17, by = c('ID', 'depth')) %>%
  mutate(depth0 = ifelse(!is.na(depth0),
                         depth0,
                         new.depth0),
         depth1 = ifelse(!is.na(depth1),
                         depth1,
                         new.depth1),
         stock = `Bulk density`*(depth1-depth0),
         moisture = Moisture*1000, # to match Cesar's units of g/kg
         ash = ifelse(!is.na(ash),
                      ash,
                      new.ash)) %>% # , (remove %>% and add in comma when all data are incorporated)
         # `%C` = ifelse(!is.na(`%C`),
         #                `%C`,
         #                `new.%C`),
         # `%N` = ifelse(!is.na(`%N`),
         #                `%N`,
         #                `new.%N`),
         # d13C = ifelse(!is.na(d13C),
         #                d13C,
         #                new.d13C),
         # d15N = ifelse(!is.na(d15N),
         #               d15N,
         #               new.d15N)) %>% # move the averaged data for 4-2.17 25-35 into the right columns
  select(-c(new.depth0, new.depth1, new.ash)) %>% # , `new.%C`, `new.%N`, new.d13C), new.d15N) %>% (remove %>% and add in the rest when all data are incorporated)
  group_by(Fence, `well#`) %>%
  arrange(Fence, `well#`, depth0) %>%
  mutate(year = 2017,
         block = ifelse(Fence == 1 | Fence == 2,
                        1,
                        ifelse(Fence == 3 | Fence == 4,
                               2,
                               3)),
         plot = as.numeric(str_sub(`well#`, 3, 3)),
         treatment = ifelse(plot <= 4,
                            'c',
                            'w')) %>%
  ungroup() %>%
  select(year, block, fence = Fence, plot, treatment, depth.cat = depth, depth0, depth1, stock, moisture, 
         bulk.density = `Bulk density`, ash, C = `%C`, N = `%N`, delta13C = d13C, delta15N = d15N)

# separate layers which were split into two (based on organic vs. mineral soil) and average
depth0_values <- c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95)
depth1_values <- c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 100)

split_layers <- soil_17_v2 %>%
  filter(!(depth0 %in% depth0_values) | 
           !(depth1 %in% depth1_values)) %>%
  filter(depth0 < 15) %>%
  group_by(year, block, fence, plot, treatment) %>%
  filter(length(depth0) > 1) %>%
  filter(!(fence == 3 & plot == 6 | fence == 6 & plot == 1)) %>%
  summarise(new.depth0 = first(depth0),
            new.depth1 = last(depth1),
            stock = sum(stock*(depth1 - depth0))/(last(depth1) - first(depth0)),
            moisture = sum(moisture*(stock))/(sum(stock)),
            bulk.density = sum(bulk.density*(stock))/(sum(stock)),
            ash = sum(ash*(stock))/(sum(stock)),
            C = sum(C*(stock))/(sum(stock)),
            N = sum(N*(stock))/(sum(stock)),
            delta13C = sum(delta13C*(stock))/(sum(stock)),
            delta15N = sum(delta15N*(stock))/(sum(stock))) %>%
  mutate(depth.cat = paste(new.depth0, new.depth1, sep = '-')) %>%
  select(year, block, fence, plot, treatment, depth.cat, depth0 = new.depth0, depth1 = new.depth1, stock, moisture, bulk.density, 
         ash, C, N, delta13C, delta15N)

# separate layers which have depths slightly offset from the desired depths
uneven_layers <- soil_17_v2 %>%
  filter(!(depth0 %in% depth0_values) | 
           !(depth1 %in% depth1_values)) %>%
  filter(depth1 > 15 | fence == 3 & plot == 6 & depth0 == 6.5 | fence == 4 & plot == 8 | fence == 5 & plot == 6 | fence == 6 & plot == 1 | fence == 6 & plot == 8) %>%
  mutate(new.depth.cat = NA,
         new.depth0 = NA,
         new.depth1 = NA,
         new.stock = NA,
         new.moisture = NA,
         new.bulk.density = NA,
         new.ash = NA,
         new.C = NA,
         new.N = NA,
         new.delta13C = NA,
         new.delta15N = NA)

for (i in 1:nrow(uneven_layers)) {
  
  current_fence <- uneven_layers$fence[i]
  current_plot <- uneven_layers$plot[i]
  current_depth0 <- uneven_layers$depth0[i]
  current_depth1 <- uneven_layers$depth1[i]
  idx <- which.min(abs(depth0_values-current_depth0))
  new_depth0 <- depth0_values[idx]
  new_depth1 <- depth1_values[idx]
  
  if (i == 1 | i == nrow(uneven_layers)) {
    
    uneven_layers$new.depth0[i] <- new_depth0
    uneven_layers$new.depth1[i] <- new_depth1
    uneven_layers$new.depth.cat[i] <- paste(new_depth0, new_depth1, sep = '-')
    
  } else {
    
    uneven_layers$new.depth0[i] <- new_depth0
    uneven_layers$new.depth1[i] <- new_depth1
    uneven_layers$new.depth.cat[i] <- paste(new_depth0, new_depth1, sep = '-')
    
    if (current_depth0 > depth0_values[idx] &
        current_depth1 < depth1_values[idx] &
        current_fence == uneven_layers$fence[i-1] &
        current_plot == uneven_layers$plot[i-1] &
        current_fence == uneven_layers$fence[i+1] &
        current_plot == uneven_layers$plot[i+1]) { # any rows in which depth0 is greater than and depth1 is less than the normalized depth and the rows on either side are of the same core
      
      # uneven_layers$new.moisture[i] <- (uneven_layers$moisture[i-1]*uneven_layers$stock[i-1]*(current_depth0-new_depth0) + 
      #   uneven_layers$moisture[i]*uneven_layers$stock[i]*(current_depth1 - current_depth0) +
      #   uneven_layers$moisture[i+1]*uneven_layers$stock[i+1]*(new_depth1-current_depth1))/
      #   (uneven_layers$stock[i-1]*(current_depth0-new_depth0) + uneven_layers$stock[i]*(current_depth1 - current_depth0) + uneven_layers$stock[i+1]*(new_depth1-current_depth1))
      print('The case where values need to be averaged with both the layer above and below the current layer is not yet supported.')
      
    } else if (current_depth0 > depth0_values[idx] &
               current_fence == uneven_layers$fence[i-1] &
               current_plot == uneven_layers$plot[i-1]) {
      
      # stock
      uneven_layers$new.stock[i] <- (uneven_layers$stock[i-1]*(current_depth0 - new_depth0) +
                                          uneven_layers$stock[i]*(new_depth1 - current_depth0))/
        (new_depth1 - new_depth0)
      
      # moisture
      uneven_layers$new.moisture[i] <- (uneven_layers$moisture[i-1]*uneven_layers$stock[i-1]*(current_depth0 - new_depth0) +
                                          uneven_layers$moisture[i]*uneven_layers$stock[i]*(new_depth1 - current_depth0))/
        (uneven_layers$stock[i-1]*(current_depth0 - new_depth0) + uneven_layers$stock[i]*(new_depth1 - current_depth0))
      
      # bulk density
      uneven_layers$new.bulk.density[i] <- (uneven_layers$bulk.density[i-1]*uneven_layers$stock[i-1]*(current_depth0 - new_depth0) +
                                          uneven_layers$bulk.density[i]*uneven_layers$stock[i]*(new_depth1 - current_depth0))/
        (uneven_layers$stock[i-1]*(current_depth0 - new_depth0) + uneven_layers$stock[i]*(new_depth1 - current_depth0))
      
      # ash
      uneven_layers$new.ash[i] <- (uneven_layers$ash[i-1]*uneven_layers$stock[i-1]*(current_depth0 - new_depth0) +
                                          uneven_layers$ash[i]*uneven_layers$stock[i]*(new_depth1 - current_depth0))/
        (uneven_layers$stock[i-1]*(current_depth0 - new_depth0) + uneven_layers$stock[i]*(new_depth1 - current_depth0))
      
      # C
      uneven_layers$new.C[i] <- (uneven_layers$C[i-1]*uneven_layers$stock[i-1]*(current_depth0 - new_depth0) +
                                          uneven_layers$C[i]*uneven_layers$stock[i]*(new_depth1 - current_depth0))/
        (uneven_layers$stock[i-1]*(current_depth0 - new_depth0) + uneven_layers$stock[i]*(new_depth1 - current_depth0))
      
      # N
      uneven_layers$new.N[i] <- (uneven_layers$N[i-1]*uneven_layers$stock[i-1]*(current_depth0 - new_depth0) +
                                          uneven_layers$N[i]*uneven_layers$stock[i]*(new_depth1 - current_depth0))/
        (uneven_layers$stock[i-1]*(current_depth0 - new_depth0) + uneven_layers$stock[i]*(new_depth1 - current_depth0))
      
      # delta13C
      uneven_layers$new.delta13C[i] <- (uneven_layers$delta13C[i-1]*uneven_layers$stock[i-1]*(current_depth0 - new_depth0) +
                                          uneven_layers$delta13C[i]*uneven_layers$stock[i]*(new_depth1 - current_depth0))/
        (uneven_layers$stock[i-1]*(current_depth0 - new_depth0) + uneven_layers$stock[i]*(new_depth1 - current_depth0))
      
      # delta15N
      uneven_layers$new.delta15N[i] <- (uneven_layers$delta15N[i-1]*uneven_layers$stock[i-1]*(current_depth0 - new_depth0) +
                                          uneven_layers$delta15N[i]*uneven_layers$stock[i]*(new_depth1 - current_depth0))/
        (uneven_layers$stock[i-1]*(current_depth0 - new_depth0) + uneven_layers$stock[i]*(new_depth1 - current_depth0))
      
    } else if (current_depth1 < depth1_values[idx] &
               current_fence == uneven_layers$fence[i+1] &
               current_plot == uneven_layers$plot[i+1]) {
      
      # stock
      uneven_layers$new.stock[i] <- (uneven_layers$stock[i]*(current_depth1 - new_depth0) +
                                       uneven_layers$stock[i+1]*(new_depth1 - current_depth1))/
        (new_depth1 - new_depth0)
      
      # moisture
      uneven_layers$new.moisture[i] <- (uneven_layers$moisture[i]*uneven_layers$stock[i]*(current_depth1 - new_depth0) +
                                          uneven_layers$moisture[i+1]*uneven_layers$stock[i+1]*(new_depth1 - current_depth1))/
        (uneven_layers$stock[i]*(current_depth1 - new_depth0) + uneven_layers$stock[i+1]*(new_depth1 - current_depth1))
      
      # bulk.density
      uneven_layers$new.bulk.density[i] <- (uneven_layers$bulk.density[i]*uneven_layers$stock[i]*(current_depth1 - new_depth0) +
                                          uneven_layers$bulk.density[i+1]*uneven_layers$stock[i+1]*(new_depth1 - current_depth1))/
        (uneven_layers$stock[i]*(current_depth1 - new_depth0) + uneven_layers$stock[i+1]*(new_depth1 - current_depth1))
      
      # ash
      uneven_layers$new.ash[i] <- (uneven_layers$ash[i]*uneven_layers$stock[i]*(current_depth1 - new_depth0) +
                                          uneven_layers$ash[i+1]*uneven_layers$stock[i+1]*(new_depth1 - current_depth1))/
        (uneven_layers$stock[i]*(current_depth1 - new_depth0) + uneven_layers$stock[i+1]*(new_depth1 - current_depth1))
      
      # C
      uneven_layers$new.C[i] <- (uneven_layers$C[i]*uneven_layers$stock[i]*(current_depth1 - new_depth0) +
                                          uneven_layers$C[i+1]*uneven_layers$stock[i+1]*(new_depth1 - current_depth1))/
        (uneven_layers$stock[i]*(current_depth1 - new_depth0) + uneven_layers$stock[i+1]*(new_depth1 - current_depth1))
      
      # N
      uneven_layers$new.N[i] <- (uneven_layers$N[i]*uneven_layers$stock[i]*(current_depth1 - new_depth0) +
                                          uneven_layers$N[i+1]*uneven_layers$stock[i+1]*(new_depth1 - current_depth1))/
        (uneven_layers$stock[i]*(current_depth1 - new_depth0) + uneven_layers$stock[i+1]*(new_depth1 - current_depth1))
      
      # delta13C
      uneven_layers$new.delta13C[i] <- (uneven_layers$delta13C[i]*uneven_layers$stock[i]*(current_depth1 - new_depth0) +
                                          uneven_layers$delta13C[i+1]*uneven_layers$stock[i+1]*(new_depth1 - current_depth1))/
        (uneven_layers$stock[i]*(current_depth1 - new_depth0) + uneven_layers$stock[i+1]*(new_depth1 - current_depth1))
      
      # delta15N
      uneven_layers$new.moisture[i] <- (uneven_layers$moisture[i]*uneven_layers$stock[i]*(current_depth1 - new_depth0) +
                                          uneven_layers$moisture[i+1]*uneven_layers$stock[i+1]*(new_depth1 - current_depth1))/
        (uneven_layers$stock[i]*(current_depth1 - new_depth0) + uneven_layers$stock[i+1]*(new_depth1 - current_depth1))
      
    }
    
  }
  
}

# tidy the normalized data
uneven_layers_v2 <- uneven_layers %>%
  mutate(new.stock = ifelse(is.na(new.stock),
                            stock,
                            new.stock),
         new.moisture = ifelse(is.na(new.moisture),
                               moisture,
                               new.moisture),
         new.bulk.density = ifelse(is.na(new.bulk.density),
                               bulk.density,
                               new.bulk.density),
         new.ash = ifelse(is.na(new.ash),
                               ash,
                          new.ash),
         new.C = ifelse(is.na(new.C),
                        C,
                        new.C),
         new.N = ifelse(is.na(new.N),
                        N,
                        new.N),
         new.delta13C = ifelse(is.na(new.delta13C),
                               delta13C,
                               new.delta13C),
         new.delta15N = ifelse(is.na(new.delta15N),
                               delta15N,
                               new.delta15N)) %>%
  select(year, block, fence, plot, treatment, depth.cat = new.depth.cat, depth0 = new.depth0, depth1 = new.depth1, 
         stock = new.stock, moisture = new.moisture, bulk.density = new.bulk.density, ash = new.ash, C = new.C, 
         N = new.N, delta13C = new.delta13C, delta15N = new.delta15N)

soil_17_template <- expand.grid(fence = seq(1, 6, 1),
                                plot = seq(1, 8, 1),
                                depth.cat = c('0-5', '5-15', '15-25', '25-35', '35-45', '45-55', '55-65', '65-75', '75-85', '85-95', '95-100')) %>%
  separate(depth.cat, into = c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  mutate(year = 2017,
         block = ifelse(fence <= 2,
                        1,
                        ifelse(fence >= 5,
                               3,
                               2)),
         depth.cat = as.character(depth.cat),
         depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1),
         treatment = ifelse(plot <= 4,
                            'c',
                            'w')) %>%
  select(year, block, fence, plot, treatment, depth.cat, depth0, depth1) %>%
  arrange(block, fence, plot, depth0)

# join soil_17_v2 with split layers and uneven layers and then insert into the template
soil_17_final <- soil_17_v2 %>%
  rbind.data.frame(split_layers, uneven_layers_v2) %>%
  right_join(soil_17_template, by = c('year', 'block', 'fence', 'plot', 'treatment', 'depth.cat', 'depth0', 'depth1')) %>%
  group_by(year, block, fence, plot, treatment) %>%
  filter(!all(is.na(stock))) %>%
  select(-stock) %>%
  mutate(CtoN = C/N,
         soil.stock = bulk.density*(depth1 - depth0)*10, # g/cm^3 * cm * 1 kg/1000 g * 10,000 cm^2/m^2 = kg/m^2
         ash.stock = ash*bulk.density*(depth1 - depth0)/100, # g ash/kg soil * g soil/cm^3 soil * cm * 1 kg/1000 g * 1 kg/1000 g * 10,000 cm^2/m^2 = kg/m^2
         C.stock = C*bulk.density*(depth1 - depth0)/100, # g C/kg soil * g soil/cm^3 soil * cm * 1 kg/1000 g * 1 kg/1000 g * 10,000 cm^2/m^2 = kg/m^2,
         N.stock = N*bulk.density*(depth1 - depth0)/100, # g N/kg soil * g soil/cm^3 soil * cm * 1 kg/1000 g * 1 kg/1000 g * 10,000 cm^2/m^2 = kg/m^2,
         cu.soil.stock = cumsum(soil.stock),
         cu.ash.stock = cumsum(ash.stock),
         cu.C.stock = cumsum(C.stock),
         cu.N.stock = cumsum(N.stock))

# join 09_13 and 17 data
soil_09_17 <- soil_09_13 %>%
  mutate(plot = '') %>%
  select(year, block, fence, plot, treatment, depth.cat, depth0, depth1, moisture, bulk.density, ash, C, N, CtoN, delta13C, 
         delta15N, soil.stock, ash.stock, C.stock , N.stock, cu.soil.stock, cu.ash.stock, cu.C.stock, cu.N.stock) %>%
  rbind.data.frame(soil_17_final)

# remember to save to server when final copy is ready
# write.csv(soil_09_17, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/Soil_09_17.csv', row.names = FALSE) 
################################################################################################################