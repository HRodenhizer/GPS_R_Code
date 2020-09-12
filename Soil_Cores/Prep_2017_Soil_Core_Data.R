################################################################################################################
###                                 Prep 2017 Soil Core Data for Analysis                                    ###
###                                           Code by HGR 6/2019                                             ###
################################################################################################################

### Load libraries #############################################################################################
library(tidyverse)
library(readxl)
################################################################################################################

### Load data ##################################################################################################
soil_09_13 <- read_excel("Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2009-2013 CiPEHR Processed SOIL DATA_César.xlsx",
                         sheet = 1) %>%
  separate(depth.cat, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  mutate(depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1))
# samples from S17 on are mislabeled and need to have 1 added to them
soil_17 <- read_excel('Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2017/2017 Soils_processing data sheet_3_8_18.xlsx',
                      sheet = 3) %>%
  filter(ID != 'S213') %>%
  mutate(ID = seq(1, nrow(.)), sep = '',
         ID = ifelse(ID >= 17,
                     paste('S', ID + 1, sep = ''),
                     paste('S', ID, sep = '')))
ash_17 <- read_excel('Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2017/Ash_Mass_Data_2017.xlsx')
# this will end up being a bunch of files
cn_17_1 <- read_excel('Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2017/190405_2017 CiPEHR soil cores.xlsx',
                    sheet = 2) %>%
  select(1:5) %>%
  select(ID = `Final Result`, `%N`, `%C`, d15N, d13C)
  

cn_17_2 <- read_excel('Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2017/190702_2017 CiPEHR soil cores Tray3.xlsx',
                      sheet = 2) %>%
  select(1:5) %>%
  rename(ID = `Final Result`)

cn_17_3 <- read_excel('Z:/Schuur Lab/New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2017/190705_2017 CiPEHR soil cores Tray4.xlsx',
                      sheet = 2) %>%
  select(1:5) %>%
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
cn_17_v2 <- cn_17_1 %>% # might need to remove %C and replace with %C real for some or all of the files
  rbind.data.frame(cn_17_2, cn_17_3) %>% # add other files here and average 4-2.17 25-35
  group_by(ID) %>%
  summarise(`%N` = mean(`%N`),
            `%C` = mean(`%C`),
            d15N = mean(d15N),
            d13C = mean(d13C)) %>%
  ungroup()

data <- which(str_detect(cn_17_v2$ID, pattern = '^S[:digit:]'))
cn_17_data <- cn_17_v2[data,]
cn_17_no_duplicates <- cn_17_data$ID %>%
  unique()
  

# this layer got split into two for ash and CN analysis and needs to be averaged into one layer before joining with soil data
avg_ash_4_2_17 <- ash_17_v2 %>%
  filter(`well#` == '4-2.17' & depth == '25-32' | `well#` == '4-2.17' & depth == '32-35') %>%
  group_by(`well#`) %>%
  summarise(new.ash = sum(ash*(depth1-depth0)/10)) %>% # average by depth rather than stock (mass/soil core area) because bulk.density only available for 25-35 as a whole
  mutate(depth = '25-35',
         new.depth0 = 25,
         new.depth1 = 35)

# average this split layer prior to joining with soil data like the split ash layer above
avg_cn_4_2_17 <- cn_17_v2 %>%
  filter(ID == 'S126' | ID == 'S214') %>%
  mutate(depth0 = ifelse(ID == 'S126',
                         25,
                         32),
         depth1 = ifelse(ID == 'S214',
                         32,
                         35),
         `well#` = '4-2.17') %>%
  group_by(`well#`) %>%
  summarise(`new.%C` = sum(`%C`*(depth1-depth0)/10),
            `new.%N` = sum(`%N`*(depth1-depth0)/10),
            new.d13C = sum(d13C*(depth1-depth0)/10),
            new.d15N = sum(d15N*(depth1-depth0)/10)) %>% # average by depth rather than stock (mass/soil core area) because bulk.density only available for 25-35 as a whole
  mutate(depth = '25-35') %>%
  ungroup()

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
  left_join(filter(cn_17_v2, !(ID == 'S126' | ID == 'S214')), by = 'ID') %>%
  full_join(avg_cn_4_2_17, by = c('well#', 'depth')) %>%
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
                      new.ash), # , (remove %>% and add in comma when all data are incorporated)
         `%C` = ifelse(!is.na(`%C`),
                        `%C`,
                        `new.%C`),
         `%N` = ifelse(!is.na(`%N`),
                        `%N`,
                        `new.%N`),
         d13C = ifelse(!is.na(d13C),
                        d13C,
                        new.d13C),
         d15N = ifelse(!is.na(d15N),
                       d15N,
                       new.d15N)) %>% # move the averaged data for 4-2.17 25-35 into the right columns
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
  filter(!(fence == 3 & plot == 6)) %>%
  summarise(new.depth0 = min(depth0),
            new.depth1 = max(depth1),
            new.stock = sum(stock*(depth1 - depth0))/(max(depth1) - min(depth0)),
            moisture = sum(moisture*(stock))/(sum(stock)),
            bulk.density = sum(bulk.density*(stock))/(sum(stock)),
            ash = sum(ash*(stock))/(sum(stock)),
            C = sum(C*(stock))/(sum(stock)),
            N = sum(N*(stock))/(sum(stock)),
            delta13C = sum(delta13C*(stock))/(sum(stock)),
            delta15N = sum(delta15N*(stock))/(sum(stock))) %>%
  mutate(depth.cat = paste(new.depth0, new.depth1, sep = '-')) %>%
  select(year, block, fence, plot, treatment, depth.cat, depth0 = new.depth0, depth1 = new.depth1, stock = new.stock, moisture, bulk.density, 
         ash, C, N, delta13C, delta15N)

#  | fence == 6 & plot == 1 <- this was filtering with 3-6, but I don't know why

# separate layers which have depths slightly offset from the desired depths
uneven_layers <- soil_17_v2 %>%
  filter(!(depth0 %in% depth0_values) | 
           !(depth1 %in% depth1_values)) %>%
  filter(depth1 > 15 | fence == 3 & plot == 6 & depth0 == 6.5 | fence == 5 & plot == 6 | fence == 6 & plot == 8) %>%
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

# standardize layer depths
for (i in 1:nrow(uneven_layers)) {
  
  current_fence <- uneven_layers$fence[i]
  current_plot <- uneven_layers$plot[i]
  current_depth0 <- uneven_layers$depth0[i] # actual depth of top of layer (i.e. a layer that doesn't line up with the standardized depths)
  current_depth1 <- uneven_layers$depth1[i] # actual depth of bottom of layer
  idx <- which.min(abs(depth0_values-current_depth0)) # depth0_values is a vector of the desired depths - this code finds the standardized depth which is closest to the current depth
  new_depth0 <- depth0_values[idx] # standardized depth which the output will be aligned to (top of layer)
  new_depth1 <- depth1_values[idx] # standardized depth which the output will be aligned to (bottom of layer)
  
  if (i == 1 | i == nrow(uneven_layers)) { # at the beginning or end of the dataset, we cannot do any calculations to adjust values, so we just adjust the depth to reflect the depth we want
    
    uneven_layers$new.depth0[i] <- new_depth0
    uneven_layers$new.depth1[i] <- new_depth1
    uneven_layers$new.depth.cat[i] <- paste(new_depth0, new_depth1, sep = '-')
    
  } else { # adjust the depth values and then go through an if statement to figure out how to deal with the averaging of values
    
    uneven_layers$new.depth0[i] <- new_depth0
    uneven_layers$new.depth1[i] <- new_depth1
    uneven_layers$new.depth.cat[i] <- paste(new_depth0, new_depth1, sep = '-')
    
    if (current_depth0 > depth0_values[idx] &
        current_depth1 < depth1_values[idx] &
        current_fence == uneven_layers$fence[i-1] &
        current_plot == uneven_layers$plot[i-1] &
        current_fence == uneven_layers$fence[i+1] &
        current_plot == uneven_layers$plot[i+1]) { # any rows in which depth0 is greater than and depth1 is less than the standardized depth and the rows on either side are of the same core
      
      # uneven_layers$new.moisture[i] <- (uneven_layers$moisture[i-1]*uneven_layers$stock[i-1]*(current_depth0-new_depth0) + 
      #   uneven_layers$moisture[i]*uneven_layers$stock[i]*(current_depth1 - current_depth0) +
      #   uneven_layers$moisture[i+1]*uneven_layers$stock[i+1]*(new_depth1-current_depth1))/
      #   (uneven_layers$stock[i-1]*(current_depth0-new_depth0) + uneven_layers$stock[i]*(current_depth1 - current_depth0) + uneven_layers$stock[i+1]*(new_depth1-current_depth1))
      print('The case where values need to be averaged with both the layer above and below the current layer is not yet supported.')
      
    } else if (current_depth0 > depth0_values[idx] &
               current_fence == uneven_layers$fence[i-1] &
               current_plot == uneven_layers$plot[i-1]) { # if the top of the core is lower than it should be and the previous row of data is in the same core
      
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
               current_plot == uneven_layers$plot[i+1]) { # if the bottom of the core is shallower than it should be and the following row of data is from the same core
      
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

# create a template with the proper entries to fill in with the data
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

# join soil_17_v2 with split layers (line 155) and uneven layers (line 199) and then insert into the template
soil_17_final <- soil_17_v2 %>%
  rbind.data.frame(split_layers, uneven_layers_v2) %>%
  right_join(soil_17_template, by = c('year', 'block', 'fence', 'plot', 'treatment', 'depth.cat', 'depth0', 'depth1')) %>%
  group_by(year, block, fence, plot, treatment) %>%
  filter(!all(is.na(stock))) %>%
  select(-stock) %>%
  mutate(C = C*10,
         N = N*10,
         CtoN = C/N,
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

# plot to check for anomalous values
soil_09_17_summary <- soil_09_17 %>%
  group_by(year, treatment) %>%
  summarise(mean.C = mean(C, na.rm = TRUE),
            se.C = sd(C, na.rm = TRUE)/sqrt(n()),
            mean.N = mean(N, na.rm = TRUE),
            se.N = sd(N, na.rm = TRUE)/sqrt(n()),
            mean.d13C = mean(delta13C, na.rm = TRUE),
            se.d13C = sd(delta13C, na.rm = TRUE)/sqrt(n()),
            mean.d15N = mean(delta15N, na.rm = TRUE),
            se.d15N = sd(delta15N, na.rm = TRUE)/sqrt(n()))
  
ggplot(filter(soil_09_17, C > 0), aes(x = as.factor(year), y = C)) +
  geom_point() +
  geom_text(data = filter(soil_09_17, C > 500), aes(label = paste(fence, plot, depth.cat, sep = '-')), position = position_jitter(width = 3), hjust = 1.5) +
  facet_grid(.~treatment)

view(filter(soil_09_17, C > 500))

ggplot(soil_09_17, aes(x = as.factor(year), y = N)) +
  geom_point() +
  facet_grid(.~treatment)

ggplot(soil_09_17, aes(x = as.factor(year), y = delta13C)) +
  geom_point() +
  geom_text(data = filter(soil_09_17, delta13C < -29), aes(label = paste(fence, plot, depth.cat, sep = '-')), position = position_jitter(width = 3), hjust = 1.5) +
  facet_grid(.~treatment)

ggplot(soil_09_17, aes(x = as.factor(year), y = delta15N)) +
  geom_point() +
  facet_grid(.~treatment)

# remember to save to server when final copy is ready
# write.csv(soil_09_17, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Soil_Cores/Soil_09_17.csv', row.names = FALSE) 
################################################################################################################