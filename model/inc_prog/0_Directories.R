# This file stores the directory for PHI analysis
# The system directory are often different by user
# By Shiya Zhao, 2022/02/16



# Input directory ----
input_base <- "../../data/inputdata_base.gdx"
input_cge <- "../../data/PHIinputdata/AIMHub.gdx"
input_msg <- "../../data/PHIinputdata/MESSAGEix.gdx"

# AIM-Hub-PHI

cge_var <- "../../data/cgeoutput/global_17_iamc.gdx"

AnaExp <- "../../output/gdx/ConsumptionResults/AnalysisExpenditure_AIMHub.gdx"
demandcge <- "../../output/gdx/ConsumptionResults/ConsumptionResults_AIMHub.gdx"



#MESSAGEix-PHI

msg_var <- "../../data/MESSAGE_output/message_global_12_iamc.gdx"

AnaExpmsg <- "../../output/gdx/ConsumptionResults/AnalysisExpenditure_MESSAGEix.gdx"
demandmsg <- "../../output/gdx/ConsumptionResults/ConsumptionResults_MESSAGEix.gdx"


# Define directory ----
dir_def <- "../define/"
dir_data <- "../../data/"


# Output directory ----
dir_csv <- "../../output/csv/"
dir_out <- "../../output/paper"
dir_output <- "../../output/"
dir.create(dir_output)
dir.create(paste0(dir_output, "RData"))
dir.create(dir_csv)


