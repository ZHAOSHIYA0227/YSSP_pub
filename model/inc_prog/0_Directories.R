# This file stores the directory for PHI analysis
# The system directory are often different by user
# By Shiya Zhao, 2022/02/16



# Input directory ----
input_base <- paste0("../", prog_loc, "/data/inputdata_base.gdx")
dir_input <- paste0("../", prog_loc, "/data/PHIinputdata/")
input_cge <- paste0("../", prog_loc, "/data/PHIinputdata/AIMHub.gdx")
input_msg <- paste0("../", prog_loc, "/data/PHIinputdata/MESSAGEix.gdx")
demand <- "../output/gdx/ConsumptionResults/"

iamc_cge <- paste0("../", prog_loc, "/data/IAMCtemplate_emission/AIMHub.gdx")
iamc_msg <- paste0("../", prog_loc, "/data/IAMCtemplate_emission/MESSAGEix.gdx")

# AIM-Hub-PHI
cge_ana <- 	paste0("../", prog_loc, "/data/cgeoutput/analysis.gdx")
cge_var <- paste0("../", prog_loc, "/data/cgeoutput/global_17_iamc.gdx")

AIDADSCalib <- 	paste0("../output/gdx/AIDADSCalib/AIDADSCalibNationResults.gdx")
AnaExp <- "../output/gdx/ConsumptionResults/AnalysisExpenditure_AIMHub.gdx"
AnaInc <- "../output/gdx/IncomeResults/AnalysisIncome_AIMHub.gdx"
# pricechange <- "../output/gdx/ConsumptionResults.gdx"
demandcge <- paste0(demand, "ConsumptionResults_AIMHub.gdx")



#MESSAGEix-PHI
msg_iamc <- paste0("../", prog_loc, "/data/MESSAGE_output/message_global_12_iamc.gdx")
msg_var <- paste0("../", prog_loc, "/data/MESSAGE_output/message_global_12_iamc.gdx")
AnaIncmsg <- "../output/gdx/IncomeResults/AnalysisIncome_MESSAGEix.gdx"
AnaExpmsg <- "../output/gdx/ConsumptionResults/AnalysisExpenditure_MESSAGEix.gdx"
demandmsg <- paste0(demand, "ConsumptionResults_MESSAGEix.gdx")



# World development database 
wdi <- paste0("../", prog_loc, "/data/WDI/WDI.xlsx")

dir_inc_data <- paste0(dir_ModelComparison,"inc_data/")

# Define directory ----
dir_def <- paste0("../",prog_loc,"/define/")
dir_def_sce <- paste0("../",prog_loc,"/define/scenario/YSSP/")
dir_data <- paste0("../", prog_loc, "/data/")


# Output directory ----
dir_fig <- paste0(dir_ModelComparison,"output/fig/")
dir_csv <- paste0(dir_ModelComparison,"output/csv/")
paste0(dir_output_YSSPpaper, "/RData/") %>% dir.create()

dir_output <- paste0(dir_ModelComparison,"output/")
dir.create(dir_output)
dir.create(paste0(dir_output, "RData"))
dir.create(dir_fig)
dir.create(dir_csv)


