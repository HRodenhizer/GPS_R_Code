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
# format ash data
ash_17_v2 <- ash_17 %>%
  mutate(ash = `Ash Mass (g)`/`Dry Soil Mass (g)`*1000) %>% # g ash/g soil * 1000g/1kg = g ash/kg soil
  separate(depth, c('depth0', 'depth1'), sep = '-', remove = FALSE) %>%
  mutate(depth0 = as.numeric(depth0),
         depth1 = as.numeric(depth1)) %>%
  select(-c(5:9))

# this layer got split into two for some reason and needs to be averaged into one layer
temp <- ash_17_v2 %>%
  filter(`well#` == '4-2.17' & depth == '25-32' | `well#` == '4-2.17' & depth == '32-35') %>%
  group_by(`well#`) %>%
  summarise(new.ash = sum(ash*(depth1-depth0)/10)) %>%
  mutate(depth = '25-35',
         new.depth0 = 25,
         new.depth1 = 35)

# join all the cn data together before joining to soil data
cn_17_v2 <- cn_17_1 %>%
  rbind.data.frame(cn_17_2) # add other files here

# join soil, ash, cn data for 2017
soil_17_v2 <- soil_17 %>%
  select(-c(5, 9:11)) %>%
  full_join(filter(ash_17_v2, !(`well#` == '4-2.17' & depth == '25-32' | 
                                  `well#` == '4-2.17' & depth == '32-35')), 
            by = c('well#', 'depth')) %>%
  full_join(temp, by = c('well#', 'depth')) %>%
  mutate(depth0 = ifelse(!is.na(depth0),
                         depth0,
                         new.depth0),
         depth1 = ifelse(!is.na(depth1),
                         depth1,
                         new.depth1),
         ash = ifelse(!is.na(ash),
                      ash,
                      new.ash)) %>%
  select(-c(new.depth0, new.depth1, new.ash)) %>%
  left_join(cn_17, by = 'ID') %>% # change to join with cn_17_v2
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
                            'w'),
         CtoN = `%C`/`%N`,
         soil.stock = `Bulk density`*(depth1 - depth0)*10, # g/cm^3 * cm * 1 kg/1000 g * 10,000 cm^2/m^2 = kg/m^2
         ash.stock = ash*`Bulk density`*(depth1 - depth0)/100, # g ash/kg soil * g soil/cm^3 soil * cm * 1 kg/1000 g * 1 kg/1000 g * 10,000 cm^2/m^2 = kg/m^2
         C.stock = `%C`*`Bulk density`*(depth1 - depth0)/100, # g C/kg soil * g soil/cm^3 soil * cm * 1 kg/1000 g * 1 kg/1000 g * 10,000 cm^2/m^2 = kg/m^2,
         N.stock = `%N`*`Bulk density`*(depth1 - depth0)/100, # g N/kg soil * g soil/cm^3 soil * cm * 1 kg/1000 g * 1 kg/1000 g * 10,000 cm^2/m^2 = kg/m^2,
         cu.soil.stock = cumsum(soil.stock),
         cu.ash.stock = cumsum(ash.stock),
         cu.C.stock = cumsum(C.stock),
         cu.N.stock = cumsum(N.stock)) %>%
  ungroup() %>%
  select(year, block, fence = Fence, treatment, depth.cat = depth, depth0, depth1, moisture = Moisture, 
         bulk.density = `Bulk density`, ash, C = `%C`, N = `%N`, CtoN, delta13C = d13C, delta15N = d15N, 
         soil.stock, ash.stock, C.stock , N.stock, cu.soil.stock, cu.ash.stock, cu.C.stock, cu.N.stock)

rm(temp)

# join 09_13 and 17 data
soil_09_17 <- soil_09_13 %>%
  rbind.data.frame(soil_17_v2)
  
################################################################################################################