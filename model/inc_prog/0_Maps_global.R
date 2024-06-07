# This file defines the maps for kyoeiken analysis for the global scenario 

# Shiya ZHAO, 2021/12/13


# Lists ----
## scenario ----
lis_scenario <- c("No-Miti","1.5C", "2C","1.5C_EPC", "2C_EPC")

lis_scenario_noEPC <- c("No-Miti","1.5C", "2C")

lis_scenario_mitigation <- c("1.5C","2C","1.5C_EPC", "2C_EPC")

lis_ref <-  read_delim(paste0(dir_def, "scenarioctax.set"), delim = " ", col_names = FALSE, show_col_types = FALSE)[,1] %>% 
  dplyr::rename(ref = colnames(.)) 
lis_ref <- unique(lis_ref$ref) 

lis_MSGscenario <- c("SSP2_mitigation_1150_2025-2100_post", "SSP2_mitigation_650_2025-2100_post", "SSP2_BaU_R12_parent_post")
## year ----
lis_y <- as.character(seq(2010, 2100, 10))


## commodity ----
lis_I <-  read_delim(paste0(dir_def, "I_calib.set"), delim = " ", col_names = FALSE, show_col_types = FALSE)[,1] %>% 
  dplyr::rename(I = colnames(.)) 
lis_I <- unique(lis_I$I)

lis_I_abb <- c('Food&Beverages',
               'Alcohol&Tobacco',
               'Clothes&Footwear',
               'Housing&Water',
               'Energy',
               'Furnishing',
               'Health',
               'Transport',
               'Communication',
               'Recreation',
               'Education',
               'Restaurant&Hotel',
               'Miscellaneous')





## CGE region ----
lis_R_CGE <- c("WLC","JPN","CHN","INC","XSE","XSA","XOC","XE25","XER",
  "CIS","TUR","CAN","USA","BRA","XLM","XME","XNF","XAF")

lis_R_MSG <- read_delim(paste0(dir_def, "/message_region.set"), delim = " ", col_names = FALSE, show_col_types = FALSE)[,1] %>% 
  dplyr::rename("R12" = colnames(.))
lis_R_MSG <- unique(lis_R_MSG$R12)



## segment and thresholds ----


lis_TH <- c('1.9-threshold',
            '2.15-threshold',
            '3.65-threhold',
            '6.85-threhold',
            '3.2-threshold',
            '5.5-threshold')

# Maps ----
## commodity ----
MapI <- data.frame(I = c('Food and nonalcoholic beverages', 'Alcoholic beverages, tobacco, and narcotics', 'Clothing and footwear', 
                     # 'Housing, water, electricity, gas and other fuels',
                     'Housing and water',
                     "Energy",
                     # 'Solid', "Gas","LiquiC","Electricity","Bio", "Other",
                     'Furnishings, household equipment and maintenance', 'Health', 'Transport', 'Communication',
                     'Recreation and culture','Education','Restaurants and hotels','Miscellaneous goods and services'),
                   I_abb = c('Food&Beverages','Alcohol&Tobacco','Clothes&Footwear',
                             'Housing&Water',
                             'Energy',
                             'Furnishing', 'Health', 'Transport', 'Communication',
                             'Recreation', 'Education', 'Restaurant&Hotel', 'Miscellaneous'))




## scenario ----
MapScenario_tmp <- data.frame(PHIscenario = c('SSP2_Baseline',
                            'SSP2_1p5deg', 
                            'SSP2_2deg',
                            'SSP2_NDC',
                            'SSP2_1p5deg_redist', 
                            'SSP2_2deg_redist',
                            'SSP2_NDC_redist',
                            'historical'),
                          scenario = c("No-Miti",
                                       "1.5C",
                                       "2C",
                                       "NDC",
                                       "1.5C_EPC",
                                       "2C_EPC",
                                       "NDC_EPC",
                                       "historical"))

# unique(MapScenario$PHIscenario)
MapScenario <- readr::read_delim(paste0(dir_def,"/scenario_AIMHub.map"), delim = ".", 
                                                                col_names = c("PHIscenario","CGEscenario"),
                                                                show_col_types = F) %>% #view
  mutate(CGEscenario = gsub("\t", "", .$CGEscenario), 
         PHIscenario = gsub("\t", "", .$PHIscenario),
         CGEscenario = gsub(" ", "", .$CGEscenario), 
         PHIscenario = gsub(" ", "", .$PHIscenario)) %>% 
  full_join(MapScenario_tmp) %>% 
  filter(!is.na(scenario)) %>% 
  mutate(CGEscenario = case_when(scenario == "historical" ~ "historical", 
                                 scenario != "historical" ~ CGEscenario))

MapScenario_msg <- readr::read_delim(paste0(dir_def,"/scenario_MESSAGEix.map"), delim = ".", 
                                     col_names = c("PHIscenario", "MSGscenario"), 
                                     show_col_types = F) %>% #view
  right_join(MapScenario) %>% #view
  filter(MSGscenario %in% lis_MSGscenario) %>% 
  select(c("MSGscenario", "scenario"))

MapScenario <- MapScenario %>% 
  right_join(MapScenario_msg)

rm(MapScenario_tmp)

## region ---- 

Map_r_PHI2Hub <- readr::read_delim(paste0(dir_def,"/Regionmap_AIMHub.map"), delim = " ", 
                               col_names = c("R1"), 
                               show_col_types = F) %>%
  select(colnames(.)[apply(!is.na(.),2,sum)!=0][c(1,3)]) %>% 
  gdata::rename.vars(from = colnames(.), to = c("R", "R_CGE")) %>% 
  rbind(data.frame(R = "WLD", R_CGE = "WLD"))

Map_r_PHI2msg <- readr::read_delim(paste0(dir_def,"/Regionmap_MESSAGEix.map"), delim = ".", 
                               col_names = c("R1"), 
                               show_col_types = F) %>%
  gdata::rename.vars(from = colnames(.), to = c("R", "R_MSG")) 

Map_R17_R8 <- readr::read_delim(paste0(dir_def,"/region17_8.map"), delim = ".", 
                                   col_names = c("R_CGE", "R8"), 
                                   show_col_types = F) %>% 
  mutate(R_CGE = gsub(" ", "", R_CGE), 
         R8 = gsub(" ", "", R8)) %>% 
  mutate(R_CGE = gsub("\t", "", R_CGE), 
         R8 = gsub("\t", "", R8)) %>% 
  mutate(R8 = recode(.$R8, 
                     "AFR8" = "Africa",
                     "ASI8" = "Other Asia", 
                     "CHN8" = "China",
                     "EUR8" = "Europe",
                     "FSU8" = "Former Soviet Union",
                     "LAM8" = "Latin America",
                     "NAM8" = "North America",
                     "PAO8" = "Pacific OECD")) %>% 
  rbind(data.frame(R_CGE = "World", R8 = "World"))


Map_R12_R8 <- readr::read_delim(paste0(dir_def,"/region12_8.map"), delim = ".", 
                                col_names = c("R_MSG", "R8"), 
                                show_col_types = F) %>% 
  mutate(R_MSG = gsub(" ", "", R_MSG), 
         R8 = gsub(" ", "", R8)) %>% 
  mutate(R_MSG = substr(R_MSG, 1,3),
         R8 = gsub("\t", "", R8)) %>% 
  mutate(R8 = recode(.$R8, 
                     "AFR8" = "Africa",
                     "ASI8" = "Other Asia", 
                     "CHN8" = "China",
                     "EUR8" = "Europe",
                     "FSU8" = "Former Soviet Union",
                     "LAM8" = "Latin America",
                     "NAM8" = "North America",
                     "PAO8" = "Pacific OECD")) %>% 
  rbind(data.frame(R_MSG = "World", R8 = "World"))


lis_R8 <- unique(Map_R12_R8$R8) %>% as.character()

