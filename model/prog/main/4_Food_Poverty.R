# This Rscript calculates food poverty related indicators
# Shiya ZHAO, 2024.01.18

require(tidyverse)
require(gamstransfer)
require(gdata)
require(openxlsx)
require(ggpmisc)

# 0. load data ------

# CPI
df_CPI_fd <- read_csv(file = paste0("../../data/CPI/FOOD_OECD/DP_LIVE_18012024033551633.csv")) %>% 
  gdata::rename.vars(colnames(.), c("R", "Indicator", "Subject", "Measure", "Frequency", "Y", "CPI_fd_value", "Flag")) %>% 
  filter(Indicator == "CPI", Subject == "FOOD", Measure == "IDX2015", Frequency == "A") %>% 
  select(-c("Indicator", "Subject", "Measure", "Frequency", "Flag")) %>% filter(Y%in%c("2017", "2011")) %>% 
  pivot_wider(names_from = "Y", values_from = "CPI_fd_value") %>% 
  mutate(CPI_fd_conversion_1117 = `2011`/`2017`) %>%
  select(c(R, CPI_fd_conversion_1117)) %>% 
  filter(!is.na(CPI_fd_conversion_1117))

df_CPI <- readxl::read_excel(paste0("../../data/CPI/WB/P_Data_Extract_From_World_Development_Indicators.xlsx")) %>% 
  select(c("Country Code", "2017 [YR2017]", "2016 [YR2016]", "2018 [YR2018]", "2011 [YR2011]")) %>% 
  mutate(CPI_conversion_1117 = as.numeric(`2011 [YR2011]`)/ as.numeric(`2017 [YR2017]`))%>%
  dplyr::rename("R" = "Country Code") %>% 
  select(c("R", CPI_conversion_1117)) %>% filter(!is.na(CPI_conversion_1117)) %>% 
  filter(!is.na(CPI_conversion_1117))



# PPP
df_PPP <- readxl::read_excel(paste0("../../data/PPP/API_PA.NUS.PPP_DS2_en_excel_v2_6299587.xls"))[-(1:2),]
colnames(df_PPP) <- df_PPP[1,]
df_PPP <- df_PPP %>% 
  select("Country Code", "Indicator Name", "2011", "2017") %>% 
  gdata::rename.vars(colnames(.), c("R", "Indicator", "2011", "2017")) %>% 
  mutate(PPP_conversion = 1, PPP_conversion_unit = "2011/2017") %>% 
  select("R", "PPP_conversion") 


# Population
m_input <- Container$new(paste0("../../data/inputdata_base.gdx"))
df_Population <- m_input["Population"]$records %>% gdata::rename.vars(colnames(.), c("PHIscenario", "R", "Y", "POP")) %>% 
  filter(PHIscenario == "SSP2") %>% select(-PHIscenario) %>% mutate(Unit_POP = "1")

# Food poverty lines
# "Cost of an energy insufficient diet [CoCA]"                                                         
# "Cost of a nutrient inadequate diet [CoNA]"                                                          
# "Cost of a healthy diet [CoHD]"                                                                    
# "Cost of a healthy diet relative to the cost of insufficient energy from starchy staples [CoHD_CoCA]"
# "Affordability of an energy insufficient diet: ratio of cost to the food poverty line [CoCA_pov]"    
# "Affordability of a nutrient inadequate diet: ratio of cost to the food poverty line [CoNA_pov]"     
# "Affordability of a healthy diet: ratio of cost to the food poverty line [CoHD_pov]"               
# "Affordability of an energy insufficient diet: ratio of cost to food expenditures [CoCA_fexp]"       
# "Affordability of a nutrient inadequate diet: ratio of cost to food expenditures [CoNA_fexp]"        
# "Affordability of a healthy diet: ratio of cost to food expenditures [CoHD_fexp]"  


df_FPL <- readxl::read_excel(paste0("../../data/FoodPoverty/P_Data_Extract_From_Food_Prices_for_Nutrition.xlsx"), 
                              sheet = "Data") %>% mutate(Unit = "2017PPP$") %>% filter(Time == "2017") %>% 
  filter(`Classification Name` == "Food Prices for Nutrition 1.0", `Classification Code` == "FPN 1.0") %>% 
  pivot_longer(c(7:16), names_to = "Indicator", values_to = "value") %>% 
  select(-c("Classification Name", "Classification Code", "Time Code")) %>% 
  gdata::rename.vars(colnames(.), c("Country", "R", "Y", "Unit", "Indicator", "value")) %>% 
  left_join(df_CPI) %>% left_join(df_CPI_fd) %>% 
  mutate(CPI_conver = case_when(!is.na(CPI_conversion_1117) ~ CPI_conversion_1117,
                                    !is.na(CPI_fd_conversion_1117) ~ CPI_fd_conversion_1117,
                                    (is.na(CPI_conversion_1117) & is.na(CPI_fd_conversion_1117))~ median(df_CPI_fd$CPI_fd_conversion_1117)) %>% as.numeric(),
         value = as.numeric(value)) %>% 
  left_join(df_PPP) %>% 
  mutate(value = value * CPI_conver * PPP_conversion, Y = "2011", Unit = "2011PPP$") %>% 
  select("R", "Y", "Unit", "Indicator", "value") %>% 
  filter(Indicator %in% c("Cost of an energy sufficient diet [CoCA]", 
                          "Cost of a nutrient adequate diet [CoNA]", 
                          "Cost of a healthy diet [CoHD]")) %>% 
  mutate(Indicator = recode(.$Indicator, 
                            "Cost of an energy sufficient diet [CoCA]" = "CoCA", 
                            "Cost of a nutrient adequate diet [CoNA]" = "CoNA", 
                            "Cost of a healthy diet [CoHD]" = "CoHD")) %>% 
  pivot_wider(names_from = "Indicator", values_from = "value") %>% 
  mutate(FPL_19 = 1.9*0.52, FPL_215 = 2.15*0.52, FPL_32 = 3.2*0.52, FPL_55 = 5.5*0.52) %>% 
  pivot_longer(c("CoCA", "CoNA", "CoHD", "FPL_19", "FPL_215", "FPL_32", "FPL_55"),names_to = "Indicator", values_to = "value_ind_2010")



  


# 1. Food poverty index calculation ---------------------------------------

# Food expenditure

lis_tx_name <- c("AIMHub", "MESSAGEix")
df_FoodPov <- data.frame()
# tx_name <- 1
for(tx_name in 1:length(lis_tx_name)){
  dir_input <- paste0("../../output/gdx/ConsumptionResults/ConsumptionResults_",lis_tx_name[tx_name],".gdx")
  if(lis_tx_name[tx_name] == "AIMHub"){m <- "AIM-Hub"}else{m <- lis_tx_name[tx_name]}
  df_FoodPov_tmp <- F_FoodPov(df_FPL, dir_input) %>% mutate(model = m)

  df_FoodPov <- df_FoodPov %>% rbind(df_FoodPov_tmp)
}

df_FoodPov_p <- df_FoodPov %>% select(-value_indicator) %>% 
  left_join(Map_r_PHI2Hub) %>% 
  dplyr::rename("PHIscenario" = "Ref") %>% 
  left_join(MapScenario) %>% 
  select("model", "R", "R_CGE", "scenario", "Y", "Indicator", "value") %>% 
  F_reve_mutate() %>% 
  F_R_CGEfull() %>% 
  left_join(df_Population) %>% 
  mutate(Indicator = recode(.$Indicator, 
                            "CoCA" = "Energy insufficient diet", 
                            "CoNA" = "Nutrient inadequate diet", 
                            "CoHD" = "Unhealthy diet")) %>%   distinct() %>% 
  mutate(Indicator = factor(Indicator, levels = c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet")))

# 2. plot ------------------------------------------------------------------

## point: compared with historical data
df_FoodPov_his <- read_excel(paste0("../../data/FoodPoverty/P_Data_Extract_From_Food_Prices_for_Nutrition_population.xlsx")) %>% 
  filter(`Classification Name` == "Food Prices for Nutrition 1.0", `Classification Code` == "FPN 1.0") %>%  
  select("Country Code", "Time", "Percent of the population who cannot afford sufficient calories [CoCA_headcount]", "Percent of the population who cannot afford nutrient adequacy [CoNA_headcount]", "Percent of the population who cannot afford a healthy diet [CoHD_headcount]") %>% 
  gdata::rename.vars(colnames(.), c("R", "Y", "CoCA", "CoNA", "CoHD")) %>% mutate(CoCA = as.numeric(CoCA), CoNA = as.numeric(CoNA), CoHD = as.numeric(CoHD))

df_FoodPov_his <- df_FoodPov_his %>% 
  filter(Y %in% c("2017")) %>% select(-"CoHD", -"Y") %>% 
  left_join(df_FoodPov_his%>% select(-c("Y","CoCA", "CoNA")) %>% filter(!(CoHD == "..")) %>% dplyr::group_by(R) %>% dplyr::reframe(CoHD = mean((CoHD))) %>% filter(!(CoHD == 0))) %>% 
  pivot_longer(c("CoCA", "CoNA", "CoHD"), names_to = "Indicator", values_to = "value_his") %>% 
  left_join(df_FoodPov_p %>% filter(model == "AIM-Hub", scenario == "No-Miti", Y == "2020", !is.na(Indicator)) %>% select(model, R, Indicator, value) %>% mutate(value = 100*value) %>% pivot_wider(names_from = "Indicator", values_from = "value") %>% 
              dplyr::rename("CoCA" = "Energy insufficient diet", "CoNA" = "Nutrient inadequate diet", "CoHD" = "Unhealthy diet") %>% pivot_longer(c("CoCA", "CoNA", "CoHD"), names_to = "Indicator", values_to = "value_prj")) %>% 
  mutate(Indicator = factor(Indicator, levels = c("CoCA", "CoNA", "CoHD"))) %>% 
  left_join(Map_r_PHI2Hub) %>% 
  F_R_CGEfull()


## col: region, headcount ----
pdata <- df_FoodPov_p %>% filter(model == "AIM-Hub", Indicator %in% c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet")) %>% 
  dplyr::group_by(model, scenario, scenario_Name, revenue, R_CGE,  Y, Indicator) %>% 
  dplyr::reframe(value = sum(value * POP), POP = sum(POP)) 
   
write.xlsx(pdata, file = paste0(dir_out, "/csv/4_FoodPoverty_R_CGE.xlsx"))

p <- ggplot(pdata %>% filter(scenario_Name %in% c("1.5C", "No-Miti"), Y %in% c(2030, 2050, 2070))) +
  geom_col(aes(x = factor( recode(scenario, "1.5C" = "1.5C\n Neutral", "1.5C_EPC" = "1.5C\n Progressive", "No-Miti" = "No-Miti"),
                           levels = c("1.5C\n Neutral",  "1.5C_EPC" = "1.5C\n Progressive", "No-Miti" = "No-Miti")) , 
               y = value/1000000, fill = R_CGE, group = paste0(model, scenario,R_CGE, revenue)), width = 0.5) +
  facet_grid(Indicator ~ Y) +
  MyTheme +
  scale_fill_manual(values = palette_R_CGE) +
  guides(fill = guide_legend(title = "Region", ncol = 1)) +
  labs(x = "Revenue", y = "Food poverty headcount | million")
p 

ggsave(p, filename = paste0(dir_out, "/fig/4_FoodPoverty.pdf"), 
       width = 22, height = 17, units = "cm")



# select
## box: rate, Nutrient inadequate diet ----

pdata <- df_FoodPov_p %>% filter(model == "AIM-Hub", Indicator %in% c("Nutrient inadequate diet")) %>% 
  select(model, scenario, R, R_CGE, revenue, scenario_Name, Y, Indicator, value) %>% 
  left_join(df_FoodPov_p %>% filter(model == "AIM-Hub", Indicator %in% c("Nutrient inadequate diet"), scenario == "No-Miti") %>% 
              select(model, R, R_CGE, Y, Indicator, value) %>% dplyr::rename("value_BaU" = "value")) %>% 
  mutate(change = value-value_BaU)

p <- ggplot(pdata %>% filter(scenario_Name == "1.5C", Y %in% c(2030, 2050, 2070), R_CGE %in% c("India", "Sub-Saharan Africa", "Southeast Asia", "Rest of Asia"))) +
  geom_boxplot(aes(x = R_CGE, y = change*100, color = R_CGE, linetype = revenue)) +
  geom_hline(yintercept = 0, color = "grey50") +
  MyTheme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  labs(x = "Region", y = "Increase in food poverty rate | %", subtitle = "Nutrient inadequate diet") +
  scale_color_manual(values = palette_R_CGE) +
  guides(color = guide_legend(title = "Region"), linetype = guide_legend(title = "Revenue")) +
  facet_grid( ~ Y)
p

ggsave(p, filename = paste0(dir_out, "/fig/4_FoodPoverty_box_CoCNA.pdf"), 
       width = 20, height = 8, units = "cm")



## col: region, headcount, Nutrient inadequate diet ----
pdata <- df_FoodPov_p %>% filter(model == "AIM-Hub", Indicator %in% c("Nutrient inadequate diet")) %>% 
  dplyr::group_by(model, scenario, scenario_Name, revenue, R_CGE,  Y, Indicator) %>% 
  dplyr::reframe(value = sum(value * POP), POP = sum(POP)) 


p <- ggplot(pdata %>% filter(scenario_Name %in% c("1.5C", "No-Miti"), Y %in% c(2030, 2050, 2070))) +
  geom_col(aes(x = factor( recode(scenario, "1.5C" = "1.5C\n Neutral", "1.5C_EPC" = "1.5C\n Progressive", "No-Miti" = "No-Miti"),
                           levels = c("1.5C\n Neutral",  "1.5C_EPC" = "1.5C\n Progressive", "No-Miti" = "No-Miti")) ,
               y = value/1000000, fill = R_CGE, group = paste0(model, scenario,R_CGE, revenue)), width = 0.5) +
  facet_grid( ~ Y) + 
  MyTheme + theme(legend.key.size = unit(0.46, 'cm'), 
                  legend.key.height = unit(0.46, 'cm'),
                  legend.key.width = unit(0.46, 'cm'), 
                  legend.title = element_text(size=10),
                  legend.text = element_text(size=9)) +
  scale_fill_manual(values = palette_R_CGE) +
  guides(fill = guide_legend(title = "Region", ncol = 2)) +
  labs(x = "Scenario", y = "Food poverty headcount | million")
p 

ggsave(p, filename = paste0(dir_out, "/fig/4_FoodPoverty_CoNA.pdf"), 
       width = 24, height = 7, units = "cm")


## col: region, headcount, all ----
pdata <- df_FoodPov_p %>% filter(model == "AIM-Hub", Indicator %in% c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet")) %>% 
  dplyr::group_by(model, scenario, scenario_Name, revenue, R_CGE,  Y, Indicator) %>% 
  dplyr::reframe(value = sum(value * POP), POP = sum(POP)) 


p <- ggplot(pdata %>% filter(scenario_Name %in% c("1.5C", "No-Miti"), Y %in% c(2030))) +
  geom_col(aes(x = factor( recode(scenario, "1.5C" = "1.5C\n Neutral", "1.5C_EPC" = "1.5C\n Progressive", "No-Miti" = "No-Miti"),
                           levels = c("1.5C\n Neutral",  "1.5C_EPC" = "1.5C\n Progressive", "No-Miti" = "No-Miti")) , 
               y = value/1000000, fill = R_CGE, group = paste0(model, scenario,R_CGE, revenue)), width = 0.5) +
  facet_grid( ~ Indicator) +
  MyTheme + theme(legend.key.size = unit(0.46, 'cm'), 
                 legend.key.height = unit(0.46, 'cm'),
                 legend.key.width = unit(0.46, 'cm'), 
                 legend.title = element_text(size=10),
                 legend.text = element_text(size=9)) +
  scale_fill_manual(values = palette_R_CGE) +
  guides(fill = guide_legend(title = "Region", ncol = 2)) +
  labs(x = "Scenario", y = "Food poverty headcount | million")
p 

ggsave(p, filename = paste0(dir_out, "/fig/4_FoodPoverty_2030.pdf"), 
       width = 24, height = 7, units = "cm")



## box: change of FPL ---------------------------------------------------
df_FPLsc <- F_FoodPov(df_FPL, paste0("../../output/gdx/ConsumptionResults/ConsumptionResults_AIMHub.gdx"), output_FPL = T) 

pdata <- df_FPLsc %>% filter(Indicator %in% c("CoCA","CoNA", "CoHD"), Y %in% c(2020, 2030, 2050, 2070)) %>% 
  mutate(Indicator = recode(.$Indicator, 
                            "CoCA" = "Energy insufficient diet", 
                            "CoNA" = "Nutrient inadequate diet", 
                            "CoHD" = "Unhealthy diet")) %>% 
  dplyr::rename(PHIscenario = "Ref") %>% left_join(MapScenario) %>% 
  select(-c("PHIscenario", "CGEscenario", "MSGscenario")) %>% F_reve_mutate() %>% filter(revenue == "Neutral")


### global ----
p <- ggplot(pdata) +
  geom_boxplot(aes(x = Y, y = value_indicator, color = scenario_Name), width = .7, outlier.shape = NA) +
  geom_hline(yintercept = 1.9, color = "grey", linetype = "dashed") +
  MyTheme +
  facet_grid(~Indicator) +
  labs(x = "Year", y = "FPL | 2011PPP$ per capita per day") +
  guides(color = guide_legend(title = "Scenario")) +
  coord_cartesian(ylim = c(0, 7.5)) +
  scale_color_manual(values = palette_Sc) +
  scale_y_continuous(breaks = seq(0,7.5,2.5))

p

ggsave(p, filename = paste0(dir_out, "/fig/SI/4_FPL.pdf"), 
       width = 18, height = 8, units = "cm")


### regional ----
# a <- df_FoodPov %>% filter(R == "JPN", Indicator == "CoNA")

pdata <- df_FPLsc %>% filter(Indicator %in% c("CoNA"), Y %in% c(2010, 2030, 2050, 2070)) %>% 
  mutate(Indicator = recode(.$Indicator, 
                            "CoNA" = "Nutrient inadequate diet", 
                            )) %>% 
  dplyr::rename(PHIscenario = "Ref") %>% left_join(MapScenario) %>% 
  select(-c("PHIscenario", "CGEscenario", "MSGscenario")) %>% F_reve_mutate() %>% filter(revenue == "Neutral") %>% 
  left_join(Map_r_PHI2Hub) %>% F_R_CGEfull()

p <- ggplot(pdata) +
  geom_boxplot(aes(x = R_CGE, y = value_indicator, color = scenario_Name), width = .7, outlier.shape = NA) +
  geom_hline(yintercept = 1.9, color = "grey", linetype = "dashed") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  facet_wrap(~Y) +
  labs(x = "Region", y = "FPL | 2011PPP$ per capita per day") +
  guides(color = guide_legend(title = "Scenario")) +
  coord_cartesian(ylim = c(0, 7.5)) +
  scale_color_manual(values = palette_Sc) +
  scale_y_continuous(breaks = seq(0,7.5,2.5))

p

ggsave(p, filename = paste0(dir_out, "/fig/SI/4_FPL_regional.pdf"), 
       width = 24, height = 14, units = "cm")


 print("The END of main/4_Food_Poverty.R")
