# This file defines the maps for kyoeiken analysis for the global scenario 

# Shiya ZHAO, 2021/12/13


# Lists ----
## scenario ----
# lis_scenario_main <- c("No-Miti","1.5C", "2C","1.5C_EPC", "2C_EPC")


lis_Ref <- read_delim(paste0(dir_def_sce, "scenario.set"), delim = " ", col_names = FALSE, show_col_types = FALSE)[,1] %>% 
  dplyr::rename(scenario = colnames(.)) 

# lis_scenario_noEPC <- c("No-Miti","1.5C", "2C")

# lis_scenario_mitigation <- c("1.5C","2C","1.5C_EPC", "2C_EPC")

# lis_scenario_CGE <-  read_delim(paste0(dir_def, "AIMHub/scenario_AIMHub.map"), delim = ".", col_names = FALSE, show_col_types = FALSE)[,1] %>% 
#   dplyr::rename(scenarioCGE = colnames(.)) 
# lis_scenario_CGE <- unique(lis_scenario_CGE$scenarioCGE)

lis_scenario_CGE <-  read_delim(paste0(dir_def, "AIMHub/scenario_AIMHub.map"), delim = " ", 
                               col_names = c("CGEscenario"), 
                               show_col_types = F) %>% 
  mutate(CGEscenario = str_split_i(CGEscenario, "\\.",2)) %>% distinct() 

lis_scenario_CGE_main <- c("SSP2_1000C_CACN_NoCC", "SSP2_500C_CACN_NoCC", "SSP2_BaU_NoCC")

lis_MSGscenario <- c("SSP2_mitigation_1150_2025-2100_post", "SSP2_mitigation_650_2025-2100_post", "SSP2_BaU_R12_parent_post")
## year ----
lis_y <- as.character(seq(2010, 2100, 10))


## commodity ----
lis_I <-  read_delim(paste0(dir_def, "I_Calib.set"), delim = " ", col_names = FALSE, show_col_types = FALSE)[,1] %>% 
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



## CGE energy ----
lis_PrmEne_type <- c('Coal (without CCS)',
                     'Coal (with CCS)',
                     'Oil (without CCS)',
                     'Oil (with CCS)',
                     'Gas (without CCS)',
                     'Gas (with CCS)',
                     'Hydropower',
                     'Nuclear',
                     "Ocean",
                     'Solar',
                     'Wind',
                     'Geothermal',
                     'Bioenergy (without CCS)',
                     'Bioenergy (with CCS)',
                     "Secondary Energy Trade",
                     'Others')



lis_PowEne_type <- c('Coal (without CCS)',
                     'Coal (with CCS)',
                     'Oil (without CCS)',
                     'Oil (with CCS)',
                     'Gas (without CCS)',
                     'Gas (with CCS)',
                     'Hydropower',
                     'Nuclear',
                     'Solar',
                     'Wind',
                     'Geothermal',
                     'Bioenergy (without CCS)',
                     'Bioenergy (with CCS)',
                     'Others')

lis_FinEne_type <- c('Coal',#8
                     'Liquid',#6
                     'Gas',#2
                     'Electricity',#1
                     # 'Geothermal',#3
                     'Heat',#4
                     # 'Hydropower',#5
                     # 'Solids',#7
                     'Biomass')#10


# Primary energy variables
lis_VPrm_Ene <- c(  "Prm_Ene_Coa_wo_CCS",
                    "Prm_Ene_Coa_w_CCS",
                    "Prm_Ene_Oil_wo_CCS",
                    "Prm_Ene_Oil_w_CCS",
                    "Prm_Ene_Gas_wo_CCS",
                    "Prm_Ene_Gas_w_CCS",
                    "Prm_Ene_HyC",
                    "Prm_Ene_Nuc",
                    "Prm_Ene_Solar",
                    "Prm_Ene_Win",
                    "Prm_Ene_Geo",
                    "Prm_Ene_Bio_wo_CCS",
                    "Prm_Ene_Bio_w_CCS",
                    "Prm_Ene_Oth")


# Secondary energy variables
lis_VSec_Ene <- c('Sec_Ene_Ele',
                  'Sec_Ene_Gas',
                  'Sec_Ene_Heat',
                  'Sec_Ene_Solids',
                  'Sec_Ene_Liq')

lis_VSec_Power <- c('Sec_Ene_Ele_Nuc',
                    'Sec_Ene_Ele_Oth',
                    'Sec_Ene_Ele_Bio_w_CCS',
                    'Sec_Ene_Ele_Bio_wo_CCS',
                    'Sec_Ene_Ele_Coa_w_CCS',
                    'Sec_Ene_Ele_Coa_wo_CCS',
                    'Sec_Ene_Ele_Gas_w_CCS',
                    'Sec_Ene_Ele_Gas_wo_CCS',
                    'Sec_Ene_Ele_Geo',
                    'Sec_Ene_Ele_Hyd',
                    'Sec_Ene_Ele_Oil_w_CCS',
                    'Sec_Ene_Ele_Oil_wo_CCS',
                    'Sec_Ene_Ele_Solar',
                    'Sec_Ene_Ele_Win')


# Final energy variables
lis_VFin_Ene <- c("Fin_Ene_Res_Ele",
                  "Fin_Ene_Res_Gas",
                  "Fin_Ene_Res_Heat",
                  "Fin_Ene_Res_HyC",
                  "Fin_Ene_Res_Liq",
                  "Fin_Ene_Res_Oth",
                  "Fin_Ene_Res_SolidsCoa",
                  "Fin_Ene_Res_SolidsBio")



## MSG energy --------------------------------------------------------------

lis_VPrm_Ene_msg <- c("Primary Energy|Biomass|w/ CCS",
                      "Primary Energy|Biomass|w/o ",
                      "Primary Energy|Coal|w/ CCS",
                      "Primary Energy|Coal|w/o CCS",
                      "Primary Energy|Fossil|w/ CCS",
                      "Primary Energy|Fossil|w/o CCS",
                      "Primary Energy|Gas|w/ CCS",
                      "Primary Energy|Gas|w/o CCS",
                      "Primary Energy|Geothermal",
                      "Primary Energy|Hydro",
                      "Primary Energy|Nuclear",
                      "Primary Energy|Ocean",
                      "Primary Energy|Oil",
                      "Primary Energy|Other",
                      "Primary Energy|Secondary Energy Trade",
                      "Primary Energy|Solar",
                      "Primary Energy|WinC")


Map_Ene_Prm_msg <- data.frame(Variable = c("Primary Energy|Biomass|w/ CCS","Primary Energy|Biomass|w/o ",
                                           "Primary Energy|Coal|w/ CCS", "Primary Energy|Coal|w/o CCS",
                                           # "Primary Energy|Fossil|w/ CCS", "Primary Energy|Fossil|w/o CCS",
                                           "Primary Energy|Oil",
                                           "Primary Energy|Gas|w/ CCS", "Primary Energy|Gas|w/o CCS",
                                           "Primary Energy|Geothermal",
                                           "Primary Energy|Hydro",
                                           "Primary Energy|Nuclear",
                                           "Primary Energy|Ocean",
                                           "Primary Energy|Solar",
                                           "Primary Energy|WinC",
                                           "Primary Energy|Secondary Energy Trade",
                                           "Primary Energy|Other"),
                              PrmEne_type = c("Bioenergy (with CCS)", "Bioenergy (without CCS)",
                                              "Coal (with CCS)", "Coal (without CCS)",
                                              "Oil (with CCS)", 
                                              "Gas (with CCS)", "Gas (without CCS)",
                                              "Geothermal",
                                              "Hydropower", 
                                              "Nuclear",
                                              "Ocean",
                                              "Solar", 
                                              "WinC",
                                              "Secondary Energy Trade",
                                              "Other"))



# final
lis_VFin_Ene_msg <- c("Final Energy|Residential and Commercial|Electricity",
                      "Final Energy|Residential and Commercial|Gases",
                      "Final Energy|Residential and Commercial|Liquids|Biomass",
                      "Final Energy|Residential and Commercial|Liquids|Oil",	
                      "Final Energy|Residential and Commercial|Solids|Biomass",
                      "Final Energy|Residential and Commercial|Solids|Coal")



Map_Ene_Fin_msg <- data.frame(Variable = c("Final Energy|Residential and Commercial|Electricity",
                                           "Final Energy|Residential and Commercial|Gases",
                                           "Final Energy|Residential and Commercial|Liquids|Biomass",
                                           "Final Energy|Residential and Commercial|Liquids|Oil",	
                                           "Final Energy|Residential and Commercial|Solids|Biomass",
                                           "Final Energy|Residential and Commercial|Solids|Coal"),
                              FinEne_type = c("Electricity", 
                                              "Gas",
                                              "Bio_liq", 
                                              "LiquiC", 
                                              "Bio_pri",
                                              "SoliC"))

## CGE region ----
lis_R_CGE <- c("WLD","JPN","CHN","INC","XSE","XSA","XOC","XE25","XER",
  "CIS","TUR","CAN","USA","BRA","XLM","XME","XNF","XAF")

lis_R_msg <- read_delim(paste0(dir_def, "MESSAGEix/message_region.set"), delim = " ", col_names = FALSE, show_col_types = FALSE)[,1] %>% 
  dplyr::rename("R12" = colnames(.))
lis_R_msg <- unique(lis_R_msg$R12)


lis_R_phi <- readr::read_delim(paste0(dir_def,"AIMHub/Regionmap_AIMHub.map"), delim = " ", 
                                   col_names = c("R1"), 
                                   show_col_types = F) %>%
  dplyr::select(colnames(.)[apply(!is.na(.),2,sum)!=0][c(1,3)]) %>% 
  gdata::rename.vars(from = colnames(.), to = c("R", "R_CGE")) %>% 
  rbind(data.frame(R = "WLD", R_CGE = "WLD"))
lis_R_phi <-unique(lis_R_phi$R) %>% 
  gsub('\"', "",.)


## segment and thresholds ----
lis_DEC <- c('1','2','3','4','5','6','7','8','9','10')


# Maps ----
## commodity ----
MapI <- data.frame(I = c('Food and nonalcoholic beverages', 'Alcoholic beverages, tobacco, and narcotics', 'Clothing and footwear', 
                     'Housing and water',
                     "Energy",
                     'Furnishings, household equipment and maintenance', 'Health', 'Transport', 'Communication',
                     'Recreation and culture','Education','Restaurants and hotels','Miscellaneous goods and services'),
                   I_abb = c('Food&Beverages','Alcohol&Tobacco','Clothes&Footwear',
                             'Housing&Water',
                             'Energy',
                             'Furnishing', 'Health', 'Transport', 'Communication',
                             'Recreation', 'Education', 'Restaurant&Hotel', 'Miscellaneous'))


Map_Ene_Fin_res <- data.frame( VEMF = c("Fin_Ene_Res_Ele", "Fin_Ene_Res_Gas", "Fin_Ene_Res_Heat", 
                                        "Fin_Ene_Res_HyC", "Fin_Ene_Res_Oth", "Fin_Ene_Res_Liq",
                                        "Fin_Ene_Res_SolidsCoa", "Fin_Ene_Res_SolidsBio"),
                               FinEne_type = c('Electricity', "Gas", "Heat",
                                               "Hydrogen", "Other","LiquiC", "Coal","Biomass"))


MapFE <- data.frame(FinEne_type = c('Electricity',"Gas","Heat", "Hydrogen","Other","LiquiC","Coal", "Biomass"),
                    I_abb = c('ELE','GAS','ELE','ELE','ELE','LQD','SLD','BIO'))


Map_Ene_Pow_res <- data.frame(VEMF = c('Sec_Ene_Ele_Coa_wo_CCS', 'Sec_Ene_Ele_Coa_w_CCS',
                                       'Sec_Ene_Ele_Oil_wo_CCS','Sec_Ene_Ele_Oil_w_CCS',
                                       'Sec_Ene_Ele_Gas_wo_CCS','Sec_Ene_Ele_Gas_w_CCS',
                                       'Sec_Ene_Ele_Hyd','Sec_Ene_Ele_Nuc',
                                       'Sec_Ene_Ele_Solar','Sec_Ene_Ele_Win',
                                       'Sec_Ene_Ele_Geo','Sec_Ene_Ele_Bio_wo_CCS',
                                       'Sec_Ene_Ele_Bio_w_CCS','Sec_Ene_Ele_Oth'),
                              PowEne_type = c( "Coal (without CCS)", "Coal (with CCS)",
                                               "Oil (without CCS)","Oil (with CCS)",
                                               "Gas (without CCS)", "Gas (with CCS)",
                                               "Hydropower", "Nuclear",
                                               "Solar","WinC",
                                               "Geothermal","Bioenergy (without CCS)",
                                               "Bioenergy (with CCS)", "Others"))

Map_Ene_Prm_res <- data.frame(VEMF = c('Prm_Ene_Coa_wo_CCS', 'Prm_Ene_Coa_w_CCS',
                                       'Prm_Ene_Oil_wo_CCS', 'Prm_Ene_Oil_w_CCS',
                                       'Prm_Ene_Gas_wo_CCS',  'Prm_Ene_Gas_w_CCS',
                                       'Prm_Ene_Hyd', 'Prm_Ene_Nuc',
                                       'Prm_Ene_Solar', 'Prm_Ene_Win',
                                       'Prm_Ene_Geo', 'Prm_Ene_Bio_wo_CCS',
                                       'Prm_Ene_Bio_w_CCS', 'Prm_Ene_Oth'),
                              PrmEne_type = c("Coal (without CCS)", "Coal (with CCS)",
                                              "Oil (without CCS)", "Oil (with CCS)",
                                              "Gas (without CCS)", "Gas (with CCS)",
                                              "Hydropower", "Nuclear",
                                              "Solar", "WinC",
                                              "Geothermal","Bioenergy (without CCS)",
                                              "Bioenergy (with CCS)", "Others"))


## scenario ----

# mapping CGE scenarios with PHI scenarios and Messageix scenarios
# CGE to PHI scenarios
MapScenario_cge <- readr::read_delim(paste0(dir_def,"/AIMHub/scenario_AIMHub.map"), delim = ".", 
                                  col_names = c("Ref","CGEscenario"),
                                  show_col_types = F) %>% #view
  mutate(CGEscenario = gsub("\t", "", .$CGEscenario), 
         Ref = gsub("\t", "", .$Ref),
         CGEscenario = gsub(" ", "", .$CGEscenario), 
         Ref = gsub(" ", "", .$Ref))

# MSG to PHI scnearios
MapScenario_msg <- readr::read_delim(paste0(dir_def,"MESSAGEix/scenario_MESSAGEix.map"), delim = ".", 
                                     col_names = c("Ref", "MSGscenario"), 
                                     show_col_types = F) %>% filter(MSGscenario != "NPiREF")

# merge the two mapping dfs
MapScenario <- left_join(MapScenario_cge, MapScenario_msg, by = "Ref")





## region ---- 

Map_r_PHI2Hub <- readr::read_delim(paste0(dir_def,"AIMHub/Regionmap_AIMHub.map"), delim = " ", 
                               col_names = c("R1"), 
                               show_col_types = F) %>%
  dplyr::select(colnames(.)[apply(!is.na(.),2,sum)!=0][c(1,3)]) %>% 
  gdata::rename.vars(from = colnames(.), to = c("R", "R_CGE")) %>% 
  rbind(data.frame(R = "WLD", R_CGE = "WLD"))

Map_r_PHI2msg <- readr::read_delim(paste0(dir_def,"/MESSAGEix/Regionmap_MESSAGEix.map"), delim = ".", 
                               col_names = c("R1"), 
                               show_col_types = F) %>%
  gdata::rename.vars(from = colnames(.), to = c("R", "R_MSG")) 

Map_R17_R8 <- readr::read_delim(paste0(dir_def,"/message-hub/region17_8.map"), delim = ".", 
                                   col_names = c("R_CGE", "R8"), 
                                   show_col_types = F) %>% 
  mutate(R_CGE = gsub(" ", "", R_CGE), 
         R8 = gsub(" ", "", R8)) %>% 
  mutate(R_CGE = gsub("\t", "", R_CGE), 
         R8 = gsub("\t", "", R8)) %>% 
  mutate(R8 = recode(.$R8, 
                     "AFR8" = "Africa and Middle East",
                     "ASI8" = "Other Asia", 
                     "CHN8" = "China",
                     "EUR8" = "Europe",
                     "FSU8" = "Former Soviet Union",
                     "LAM8" = "Latin America",
                     "NAM8" = "North America",
                     "PAO8" = "Pacific OECD")) %>% 
  rbind(data.frame(R_CGE = "World", R8 = "World"))


Map_R12_R8 <- readr::read_delim(paste0(dir_def,"/message-hub/region12_8.map"), delim = ".", 
                                col_names = c("R_MSG", "R8"), 
                                show_col_types = F) %>% 
  mutate(R_MSG = gsub(" ", "", R_MSG), 
         R8 = gsub(" ", "", R8)) %>% 
  mutate(R_MSG = substr(R_MSG, 1,3),
         R8 = gsub("\t", "", R8)) %>% 
  mutate(R8 = recode(.$R8, 
                     "AFR8" = "Africa and Middle East",
                     "ASI8" = "Other Asia", 
                     "CHN8" = "China",
                     "EUR8" = "Europe",
                     "FSU8" = "Former Soviet Union",
                     "LAM8" = "Latin America",
                     "NAM8" = "North America",
                     "PAO8" = "Pacific OECD")) %>% 
  rbind(data.frame(R_MSG = "World", R8 = "World"))


lis_R8 <- unique(Map_R12_R8$R8) %>% as.character()
## others ----
MapDec <- data.frame(quantile = as.character(seq(0.1,1,0.1)),
                     DEC = as.character(seq(1,10,1))) 


