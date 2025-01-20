# This Rscript is developed for running a regression from the scenario enssemble
# Shiya ZHAO, 2024.01.26

require(viridisLite)
require(tidyverse)
require(gamstransfer)

# args <- commandArgs()
# print(as.character(args))
# prog_loc <- as.character(args[length(args)])
# setwd("E:/szhao/model/YSSP_MESSAGE_PHI/AIMPHI_YSSP/exe")
setwd("/Users/shiyazhao/Desktop/Model/AIMPHI/YSSP_AIMPHI/exe")

prog_loc <- "model"

source(paste0("../",prog_loc, "/inc_prog/igdx_GAMS_PATH.r"))
print(args)
print(prog_loc)


txt_TH <- "pop_2.15"

# 0. Define function, list ------------------------------------------------
# directory
dir.create(paste0(dir_output_YSSPpaper, "/fig/SI/regression/"))
dir.create(paste0(dir_output_YSSPpaper, "/csv/regression"))

# Function
F_scenario <- function(x){
  y <- x %>% mutate(scenario = gsub("SSP2_","",Ref)) %>% 
    mutate(policy = case_when(endsWith(scenario, "FC") ~ "Full Combo",
                              endsWith(scenario, "FC_redist") ~ "Full Combo",
                               !(endsWith(scenario, "FC") | endsWith(scenario, "FC_redist") ) ~ "Carbon tax")) %>% 
    mutate(scenario = gsub("_FC","",scenario))
  return(y)
}

F_scenario_rename <- function(x){
  y <- x %>% 
    mutate(Ref = case_when(Ref == "SSP2_2deg" ~ "SSP2_1000", Ref == "SSP2_1p5deg" ~ "SSP2_500", Ref == "SSP2_2deg_redist" ~ "SSP2_1000_redist", Ref == "SSP2_1p5deg_redist" ~ "SSP2_500_redist", 
                           !(Ref %in% c("SSP2_2deg", "SSP2_1p5deg", "SSP2_2deg_redist", "SSP2_1p5deg_redist")) ~ Ref))
  return(y)
}

F_CarbonB_factor <- function(x){
  y <- x %>% mutate(target = factor(target, levels = seq(400, 1500, 100) %>% as.character()))
  return(y)
  
}


F_TH <- function(x){
  y <- x %>% 
    left_join(data.frame(TH = c("pop_2.15" ,"pop_3.65","pop_6.85"), 
                         TH1 = c("2.15-threshold","3.65-threshold","6.85-threshold")))
  return(y)
  
}

# List
lis_y <- c(seq(2030, 2070, 10))

a <- Map_r_PHI2Hub %>% F_R_CGEfull()
lis_R_CGE <- unique(a$R_CGE) %>% as.character()
lis_R_CGE <- lis_R_CGE[1:17]
rm(a)

# list for plot
lis_R_CGE_PoV <- c("Sub-Saharan Africa", "India", "Rest of Asia","Southeast Asia")


# palette
# palette_CarbonB <- c("400" = , "500" = , "600" = , "700" = , )



# 0. load data ------------------------------------------------------------
dir_AnalysisExpenditure <- "../output/gdx/ConsumptionResults_reg/AnalysisExpenditure_AIMHub.gdx"
dir_PHIinput <- paste0("../",prog_loc,"/data/PHIinputdata/AIMHub.gdx")
dir_EDS <- paste0("../",prog_loc,"/data/Emission_DS/AIMHub.gdx")

# data from AIM/PHI output
m_input <- Container$new(dir_AnalysisExpenditure)

df_Gini <- m_input['Gini_exp']$records %>% gdata::rename.vars(colnames(.), c("Ref","R","Y","value_Gini")) %>% 
  mutate(unit_Gini_exp = "0-1") %>% filter(Y %in% lis_y)
df_Gini_R <- df_Gini %>% 
  left_join(df_Gini %>% filter(Ref == "SSP2_Baseline") %>% dplyr::rename("value_Gini_BaU" = "value_Gini") %>% select(-Ref)) %>% 
  mutate(change_rate_Gini = (value_Gini - value_Gini_BaU)/value_Gini_BaU, 
         change_Gini = value_Gini - value_Gini_BaU) %>% left_join(Map_r_PHI2Hub) %>% F_R_CGEfull()

df_PoV <- m_input['PoVExp']$records %>% gdata::rename.vars(colnames(.), c("Ref","R","Y","TH", "value_PoVExp")) %>% 
  mutate(unit_PoV = "persons") %>% filter(Y %in% lis_y) #, value_PoVExp > 1

df_PoV_R <- df_PoV %>% 
  left_join(df_PoV %>% filter(Ref == "SSP2_Baseline") %>% dplyr::rename("value_PoVExp_BaU" = "value_PoVExp") %>% select(-Ref) %>% distinct()) %>% 
  mutate(change_rate_PoVExp = (value_PoVExp - value_PoVExp_BaU)/value_PoVExp_BaU, change_PoVExp = value_PoVExp - value_PoVExp_BaU)


df_PoV_Rcge <- df_PoV_R %>% 
  select("Ref", "R", "Y", "TH", "value_PoVExp", "unit_PoV", "value_PoVExp_BaU") %>% 
  left_join(Map_r_PHI2Hub) %>% 
  filter(!is.na(R_CGE)) %>% 
  dplyr::group_by(Ref, R_CGE, Y, TH) %>% 
  reframe(value_PoVExp_Rcge = sum(value_PoVExp), 
          value_PoVExp_BaU_Rcge = sum(value_PoVExp_BaU),  
          change_rate_PoVExp_Rcge = (value_PoVExp_Rcge - value_PoVExp_BaU_Rcge)/value_PoVExp_BaU_Rcge, 
          change_PoVExp_Rcge = (value_PoVExp_Rcge - value_PoVExp_BaU_Rcge),
          unit_PoVExp = "persons") %>% 
  F_R_CGEfull()

# identify countries left out from the computation
a <- df_Gini_R %>% select("Ref","R","Y") %>% filter(Y == 2030) %>% distinct() %>% pivot_wider(names_from = "Ref", values_from = "Y") %>% filter(is.na(`SSP2_1100`))
a$R %>% openxlsx::write.xlsx(file = paste0(dir_ModelComparison, "/prog/regression/1_R_regression.xlsx"))
  

df_PoVGap_abs <- m_input['PoVgap_absExp']$records %>% gdata::rename.vars(colnames(.), c("Ref","R","Y", "TH","value_PoVGap_abs")) %>% 
  mutate(unit_PoVGap_abs = "$") %>% filter(Y %in% lis_y)

df_PoVGap_GDP <- m_input['PoVgap_GDPExp']$records %>% gdata::rename.vars(colnames(.), c("Ref","R","Y", "TH","value_PoVGap_GDP")) %>% 
  mutate(unit_PoVGap_GDP = "1") %>% filter(Y %in% lis_y)


# data from AIM/PHI input
m_PHIinput <- Container$new(dir_PHIinput)
df_PGHG <- m_PHIinput["PGHG"]$records %>% gdata::rename.vars(colnames(.), c("Ref", "Y","R","value_PGHG")) %>%
  mutate(unit_PGHG = "$") %>% filter(Y %in% lis_y)

df_PGHG_Rcge <- df_PGHG %>% left_join(Map_r_PHI2Hub) %>% F_R_CGEfull() %>% dplyr::group_by(Ref, Y, R_CGE) %>% reframe(value_PGHG = mean(value_PGHG), unit_PGHG= "$")

# tax revenue total
df_TxRev <- m_PHIinput["Tx_revenue"]$records %>% gdata::rename.vars(colnames(.), c("Ref", "Y","R","Type","value_Tx_revenue")) %>% 
  mutate(unit_Tx_revenue = "$")  %>% filter(Y %in% lis_y)

# tax revenue per capita
df_TxRev_capita <- m_PHIinput["Tx_revenue_capita"]$records %>% gdata::rename.vars(colnames(.),c("Ref", "Y","R","Type","value_Tx_revenue_capita")) %>% 
  mutate(unit_Tx_revenue = "$ per capita")  %>% filter(Y %in% lis_y)

# tax revenue per GDP (ratio)
df_TxRev_rate <- m_PHIinput["Tx_revenue_rate"]$records %>% gdata::rename.vars(colnames(.), c("Ref", "Y","R","value_Tx_revenue_rate")) %>% 
  mutate(unit_Tx_revenue_rate = "1") %>% filter(Y %in% lis_y)


# data from emission downscaling
m_EDS <- Container$new(dir_EDS)

df_EGHG_R <- m_EDS["EGHG"]$records %>% gdata::rename.vars(colnames(.), c("Ref", "Y","R", "Species", "Source","value_EGHG")) %>%
  mutate(unit_EGHG = "kt CO2eq") %>% filter(Y %in% lis_y, Species == "CO2") %>% left_join(Map_r_PHI2Hub) %>% F_R_CGEfull()
df_EGHG_R <- df_EGHG_R %>% left_join(df_EGHG_R %>% filter(Ref == "SSP2_Baseline") %>% dplyr::rename("value_EGHG_BaU" = "value_EGHG") %>% select(-Ref))%>% 
  mutate(EGHG_reductionrate = (value_EGHG-value_EGHG_BaU)/value_EGHG_BaU)

df_EGHG_Rcge1 <- m_EDS["GHG_load"]$records %>% gdata::rename.vars(colnames(.), c("Ref", "R_CGE","Y", "Species", "Source","value_EGHG")) %>%
  mutate(unit_EGHG = "kt CO2eq") %>% filter(Y %in% lis_y, Species == "CO2") %>% F_R_CGEfull() %>% filter(!is.na(R_CGE))
df_EGHG_Rcge <- df_EGHG_Rcge1 %>% 
  left_join(df_EGHG_Rcge1 %>% filter(Ref == "SSP2_Baseline") %>% dplyr::rename("value_EGHG_BaU" = "value_EGHG") %>% select(-Ref)) %>% 
  mutate(change_EGHG_Rcge = value_EGHG - value_EGHG_BaU, change_rate_EGHG_Rcge = change_EGHG_Rcge/value_EGHG_BaU)
rm(df_EGHG_Rcge1,a, df_Gini)


## 00 redistribution ---------
type_redist <- "Neutral"
# type_redist <- "EPC"

lis_redist <- c("Neutral", "EPC")

## 00 regression ---------
# type_reg <-  "PGHG"
type_reg <-  "EGHG_Rcge"

df_smry_tot_pov <- data.frame()
df_smry_tot_gini <- data.frame()
for(redist in seq_along(lis_redist)){
  # poverty
  type_redist <- lis_redist[redist]
  
  source(paste0(dir_ModelComparison, "/prog/regression/1_reg_poverty.R"))
  
  df_smry_tot_pov <- df_smry_tot_pov %>% rbind(df_smry %>% mutate(revenue = type_redist))
  rm(df_smry)
  print("1")
  
  
  # inequality
  source(paste0(dir_ModelComparison, "/prog/regression/2_reg_inequality.R"))
  
  df_smry_tot_gini <- df_smry_tot_gini %>% rbind(df_smry %>% mutate(revenue = type_redist))
  rm(df_smry)
  print("2")
  
}

save(df_smry_tot_pov, df_smry_tot_gini, file = paste0(dir_output_YSSPpaper, "/RData/reg_summary.RData"))
