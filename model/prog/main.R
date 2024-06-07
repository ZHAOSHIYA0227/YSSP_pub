library(tidyverse)
library(readxl)
require(sf)
require(gdxrrw)


source(paste0("../inc_prog/igdx_GAMS_PATH.r"))



# definition ----

source("../inc_prog/0_Functions.R")
source("../inc_prog/0_Directories.R")
source("../inc_prog/0_PlotSettings.R")
source("../inc_prog/0_PlotWorldMap.R")
source("../inc_prog/0_Maps_global.R")


dir.create(dir_out)
dir.create(paste0(dir_out, "/csv"))
dir.create(paste0(dir_out, "/SI/"))
lis_R_agg <- c("WLD", "R5OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")


lis_TH_od <- c( "pop_1.9","pop_3.2","pop_5.5")
lis_TH_loop <- c( "1.9-threshold","3.2-threshold","5.5-threshold")
lis_TH_no <- c(1.9*365, 3.2*365, 5.5*365)
lis_TH <- lis_TH_loop

txt_plot_TH <- "1.9-threshold"

# Running -----------------------------------------------------------------



source(paste0("../prog/main/0_ScenarioOverview.R"))

source(paste0("../prog/main/1_Poverty.R"))

source(paste0("../prog/main/1_2_Poverty_revenue.R"))

source(paste0("../prog/main/2_Inequality.R"))

source(paste0("../prog/main/3_DecileDataAnalysis.R"))

source(paste0("../prog/main/4_Food_Poverty.R"))

source(paste0("../prog/regression/0_reg_master.R"))
