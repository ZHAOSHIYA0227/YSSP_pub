library(tidyverse)
library(readxl)
require(sf)
require(gdxrrw)

dir.create("../../../exe")
setwd("../../../exe")
getwd()
# sw_mac <- "MacBook"
sw_mac <- "MacMini"


prog_loc <- "PovertyIncome"
dir_ModelComparison <- paste0("../public_repo/model/")


source(paste0(dir_ModelComparison, "/inc_prog/igdx_GAMS_PATH.R"))

# switch ----------

ModelComparison <-  T
pov_his <- F



# definition -----------------------------------------------------------------

dir_output_YSSPpaper <- paste0(dir_ModelComparison, "/output/YSSP_paper")
dir.create(dir_output_YSSPpaper)

lis_R_agg <- c("WLD", "R5OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")

lis_TH_od <- c( "pop_2.15","pop_3.65","pop_6.85")
lis_TH_loop <- c( "2.15-threshold","3.65-threshold","6.85-threshold")
lis_TH_no <- c(2.15*365, 3.65*365, 6.85*365)

lis_TH <- lis_TH_loop

txt_plot_TH <- "2.15-threshold"

lis_model <- c("AIM-Hub", "MESSAGEix")



# settings ----------------------------------------------------------------

source(paste0(dir_ModelComparison, "/inc_prog/0_Functions.R"))
source(paste0(dir_ModelComparison, "/inc_prog/0_Directories.R"))
source(paste0(dir_ModelComparison, "/inc_prog/0_PlotSettings.R"))
source(paste0(dir_ModelComparison, "/inc_prog/0_PlotWorldMap.R"))
source(paste0(dir_ModelComparison, "/inc_prog/0_Maps_global.R"))


# Comparing the distributions in the EPC redistribution scenarios -----------------------------------------------------------------

if(ModelComparison == T){
  # Running ModelComparison for OneEarth Reviewer -----------------------------------------------------------------
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp00_settings.R"))
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp0_ScenarioOverview.R"))
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp1_Poverty.R"))
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp1_PovertyRiskThreshold.R"))
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp1_2_Poverty_revenue.R"))
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp2_Inequality.R"))
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp5_Expenditure.R"))
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp8_Food_Poverty.R"))
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp0_reg_master.R"))
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp_data_1_poverty.R"))  
  
  source(paste0(dir_ModelComparison, "prog/Compare/cp9_Sensitivity.R"))  
}


