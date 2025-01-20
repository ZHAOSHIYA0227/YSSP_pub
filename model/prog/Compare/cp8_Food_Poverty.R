# This Rscript calculates food poverty related indicators
# Shiya ZHAO, 2024.01.18

require(tidyverse)
require(gamstransfer)
require(gdata)
require(openxlsx)
require(ggpmisc)
require(gdxrrw)
require(foreach)
require(doParallel)
require(ggpubr)

lis_indicator <- c("Energy insufficient diet", "Nutrient inadequate diet", "Unhealthy diet" )
                   
lis_indicator_abb <- c("CoCA","CoNA","CoHD" )
# 00. switch ----
sw_FPL <- T
sw_FoodPov <- T

# adjust (T) or not adjust (F) based on comparison between national projections and statistics
sw_adj <- F

# use simplified income threshold (T, calculating an income threshold based on WB's proportion) 
# or not (F, comparing against AIM-PHI's food expenditure)
sw_inc <- T


# 0. load data ------

# CPI
df_CPI_fd <- read_csv(file = paste0(dir_ModelComparison, "/data/CPI/FOOD_OECD/DP_LIVE_18012024033551633.csv")) %>% 
  gdata::rename.vars(colnames(.), c("R", "Indicator", "Subject", "Measure", "Frequency", "Y", "CPI_fd_value", "Flag")) %>% 
  filter(Indicator == "CPI", Subject == "FOOD", Measure == "IDX2015", Frequency == "A") %>% 
  dplyr::select(-c("Indicator", "Subject", "Measure", "Frequency", "Flag")) %>% filter(Y%in%c("2017", "2011")) %>% 
  pivot_wider(names_from = "Y", values_from = "CPI_fd_value") %>% 
  # mutate(CPI_fd_conversion_1117 = `2011`/`2017`) %>%
  mutate(CPI_fd_conversion_1717 = 1) %>%
  dplyr::select(c(R, CPI_fd_conversion_1717)) %>% 
  filter(!is.na(CPI_fd_conversion_1717))

df_CPI <- readxl::read_excel(paste0(dir_ModelComparison, "/data/CPI/WB/P_Data_Extract_From_World_Development_Indicators.xlsx")) %>% 
  dplyr::select(c("Country Code", "2017 [YR2017]", "2016 [YR2016]", "2018 [YR2018]", "2011 [YR2011]")) %>% 
  # mutate(CPI_conversion_1117 = as.numeric(`2011 [YR2011]`)/ as.numeric(`2017 [YR2017]`))%>%
  mutate(CPI_conversion_1717 = 1)%>%
  dplyr::rename("R" = "Country Code") %>% 
  dplyr::select(c("R", CPI_conversion_1717)) %>% filter(!is.na(CPI_conversion_1717)) %>% 
  filter(!is.na(CPI_conversion_1717))



# PPP
df_PPP <- readxl::read_excel(paste0(dir_ModelComparison, "/data/PPP/API_PA.NUS.PPP_DS2_en_excel_v2_6299587.xls"))[-(1:2),]
colnames(df_PPP) <- df_PPP[1,]
df_PPP <- df_PPP %>% 
  dplyr::select("Country Code", "Indicator Name", "2011", "2017") %>% 
  gdata::rename.vars(colnames(.), c("R", "Indicator", "2011", "2017")) %>% 
  # mutate(PPP_conversion = 1, PPP_conversion_unit = "2011/2017") %>% 
  mutate(PPP_conversion = 1, PPP_conversion_unit = 1) %>% 
  dplyr::select("R", "PPP_conversion") 


# Population
m_input <- Container$new(paste0("../", prog_loc, "/data/inputdata_base.gdx"))
df_Population <- m_input["Population"]$records %>% gdata::rename.vars(colnames(.), c("Ref", "R", "Y", "POP")) %>% 
  filter(Ref == "SSP2") %>% dplyr::select(-Ref) %>% mutate(Unit_POP = "1")
rm(m_input)

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


df_FPL <- readxl::read_excel(paste0(dir_ModelComparison, "/data/FoodPoverty/P_Data_Extract_From_Food_Prices_for_Nutrition.xlsx"), 
                             sheet = "Data") %>% mutate(Unit = "2017PPP$") %>% filter(Time == "2017") %>% 
  filter(`Classification Name` == "Food Prices for Nutrition 1.0", `Classification Code` == "FPN 1.0") %>% 
  pivot_longer(c(7:16), names_to = "Indicator", values_to = "value") %>% 
  dplyr::select(-c("Classification Name", "Classification Code", "Time Code")) %>% 
  gdata::rename.vars(colnames(.), c("Country", "R", "Y", "Unit", "Indicator", "value")) %>% 
  left_join(df_CPI) %>% left_join(df_CPI_fd) %>% 
  mutate(CPI_conver = case_when(!is.na(CPI_conversion_1717) ~ CPI_conversion_1717,
                                !is.na(CPI_fd_conversion_1717) ~ CPI_fd_conversion_1717,
                                (is.na(CPI_conversion_1717) & is.na(CPI_fd_conversion_1717))~ median(df_CPI_fd$CPI_fd_conversion_1717)) %>% as.numeric(),
         value = as.numeric(value)) %>% 
  left_join(df_PPP) %>% 
  mutate(value = value * CPI_conver * PPP_conversion, Y = "2011", Unit = "2017PPP$") %>% 
  dplyr::select("R", "Y", "Unit", "Indicator", "value") %>% 
  filter(Indicator %in% c("Cost of an energy sufficient diet [CoCA]", 
                          "Cost of a nutrient adequate diet [CoNA]", 
                          "Cost of a healthy diet [CoHD]")) %>% 
  mutate(Indicator = recode(.$Indicator, 
                            "Cost of an energy sufficient diet [CoCA]" = "CoCA", 
                            "Cost of a nutrient adequate diet [CoNA]" = "CoNA", 
                            "Cost of a healthy diet [CoHD]" = "CoHD")) %>% 
  pivot_wider(names_from = "Indicator", values_from = "value") %>% 
  mutate(FPL_19 = 1.9*0.52, FPL_215 = 2.15*0.52, FPL_365 = 3.65*0.52, FPL_685 = 6.85*0.52) %>% 
  pivot_longer(c("CoCA", "CoNA", "CoHD", "FPL_19", "FPL_215", "FPL_365", "FPL_685"),names_to = "Indicator", values_to = "value_ind_2010")






# 1. Food poverty index calculation ---------------------------------------

# Food poverty index projection,
# unit: 1 (food poverty rate)
lis_tx_name <- c("AIMHub", "MESSAGEix")

if(sw_FPL == T){
  df_FPLsc <- data.frame()
}

if(sw_FoodPov == T){
  df_FoodPov <- data.frame()
}

# identify country groups if necessary
if(sw_inc == T){
  df_FPL_wd <- readxl::read_excel(paste0(dir_ModelComparison, "/data/WorldBank/OGHIST.xlsx"), sheet = "Country Analytical History")
  colnames(df_FPL_wd) <- df_FPL_wd[4,]
  colnames(df_FPL_wd)[1] <- "R"
  df_FPL_wd <- df_FPL_wd %>% dplyr::select(R, FY21)
  df_FPL_wd <- df_FPL_wd[-1:-10,]
  
  df_FPL_wd <- df_FPL_wd %>% mutate(adjust = case_when(FY21 == "L" ~ .63, FY21 == "LM" ~ .56, FY21 == "UM" ~ .46, FY21 == "H" ~ .46)) %>% dplyr::select(-FY21) %>% filter(!is.na(R))
}

for(tx_name in 1:length(lis_tx_name)){
  if(lis_tx_name[tx_name] == "AIMHub"){m <- "AIM-Hub"}else{m <- lis_tx_name[tx_name]}
  
  # load data
  dir_input <- paste0("../output/gdx/ConsumptionResults/ConsumptionResults_",lis_tx_name[tx_name],".gdx")
  df_Exp_fd <- dir_input %>% rgdx.param("ExpNational") %>% gdata::rename.vars(colnames(.), c("R", "Ref", "Seg", "Y", "I", "value")) %>%
    filter(I == "Food and nonalcoholic beverages") %>% mutate(value = value/365)
  
  df_PQ <- dir_input %>% rgdx.param("PQchange") %>%
    gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "R3", "I", "PQ")) %>%
    filter(I == "Food and nonalcoholic beverages")
  
  df_Mu_exp <- dir_input %>% rgdx.param("Mu_exp") %>% gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "para_Mu_exp")) 
  
  df_Sigma_exp <- dir_input %>% rgdx.param("Sigma_exp") %>% gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "para_Sigma_exp")) 
  
  lis_sc_loop <- unique(df_Exp_fd$Ref)
  
  # parallel calculation ---
  if(sw_FPL == T){
    # Food poverty lines
    time1 <- Sys.time()
    cluster <- makeCluster(4)
    registerDoParallel(cluster)
    df_FPLsc_tmp <- foreach(s = 1:length(lis_sc_loop[!grepl("Gini",lis_sc_loop)]), .combine =rbind,.packages = c('tidyverse', 'dplyr', 'stats') ) %dopar% { # .export = c("df_Exp_fd",  "df_PQ",  "df_Mu_exp",  "df_Sigma_exp"), 
      # s <- 1
      df_Exp_fd1 <- df_Exp_fd %>% filter(Ref %in% lis_sc_loop[s])
      
      df_PQ1 <- df_PQ %>% filter(Ref %in% lis_sc_loop[s])
      
      df_Mu_exp1 <- df_Mu_exp %>% filter(Ref %in% lis_sc_loop[s])
      
      df_Sigma_exp1 <- df_Sigma_exp %>% filter(Ref %in% lis_sc_loop[s])
      
      # cost indicator for food poverty line
      F_FoodPov(df_FPL, df_FPL_wd_in = df_FPL_wd, output_FPL = T,  
                df_Exp_fd_in=df_Exp_fd1,  df_PQ_in=df_PQ1,  df_Mu_exp_in=df_Mu_exp1,  
                df_Sigma_exp_in=df_Sigma_exp1, lis_ind = c("CoCA", "CoNA", "CoHD")) %>% 
        mutate(model = m) 
    }
    stopImplicitCluster()
    stopCluster(cluster)
    time2 <- Sys.time()
    print(time2-time1)
    
    # combine with for loop
    df_FPLsc <- df_FPLsc %>% rbind(df_FPLsc_tmp)
    
    rm(df_FPLsc_tmp)
  }
  
  
  # Food poverty ratio ---
  if(sw_FoodPov == T){
    time3 <- Sys.time()
    cluster <- makeCluster(4)
    registerDoParallel(cluster)
    df_FoodPov_tmp <- foreach(s = 1:length(lis_sc_loop), .combine =rbind, .packages = c('tidyverse', 'dplyr', 'stats') ) %dopar% {
      # s <- 1
      df_Exp_fd1 <- df_Exp_fd %>% filter(Ref %in% lis_sc_loop[s])
      
      df_PQ1 <- df_PQ %>% filter(Ref %in% lis_sc_loop[s])
      
      df_Mu_exp1 <- df_Mu_exp %>% filter(Ref %in% lis_sc_loop[s])
      
      df_Sigma_exp1 <- df_Sigma_exp %>% filter(Ref %in% lis_sc_loop[s])
      
      # food poverty rate
      F_FoodPov(df_FPL, df_FPL_wd_in = df_FPL_wd, output_FPL = F,  simplified_income = sw_inc,
                     df_Exp_fd_in=df_Exp_fd1,  df_PQ_in=df_PQ1,  df_Mu_exp_in=df_Mu_exp1,  
                df_Sigma_exp_in=df_Sigma_exp1, lis_ind = c("CoCA", "CoNA", "CoHD")) %>% 
       mutate(model = m)
      
     
    }
    stopImplicitCluster()
    stopCluster(cluster)
    time4 <- Sys.time()
    print(time4-time3)
    
    
    # combine with for loop
    df_FoodPov <- df_FoodPov %>% rbind(df_FoodPov_tmp)
    
    rm(df_FoodPov_tmp)
  }

  rm(dir_input,  df_Exp_fd,  df_PQ,  df_Mu_exp,  df_Sigma_exp)
}

# the income threshold of FPL
df_FPL_inc1 <- df_FPLsc %>% left_join(df_FPL_wd) %>% mutate(value_indicator_adj_inc = value_indicator*365/adjust) %>% 
  dplyr::select(model, R, Y, Ref, Indicator, I, value_indicator_adj_inc) %>% distinct()

unique(df_FoodPov$R) %>% length()


df_FoodPov_p_od <- df_FoodPov %>% dplyr::select(-value_indicator) %>% 
  left_join(Map_r_PHI2Hub) %>% 
  dplyr::select("model", "R", "R_CGE", "Ref", "Y", "Indicator", "value") %>% 
  left_join(df_Population) %>% 
  F_reve_mutate() %>% 
  F_R_CGEfull() %>% 
  mutate(Indicator = recode(.$Indicator, 
                            "CoCA" = "Energy insufficient diet", 
                            "CoNA" = "Nutrient inadequate diet", 
                            "CoHD" = "Unhealthy diet")) %>%   distinct() %>% 
  mutate(Indicator = factor(Indicator, levels = c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet")), unit = "1")


# export data -------------------------------------------------------------
# food poverty rate: df_FoodPov and df_FoodPov_p


# 2. plot: Food poverty ------------------------------------------------------------------

## 2.1 point: compared with historical data ------------------------------------------------------------------
df_FoodPov_his <- read_excel(paste0(dir_ModelComparison, "/data/FoodPoverty/P_Data_Extract_From_Food_Prices_for_Nutrition_population.xlsx")) %>% 
  filter(`Classification Name` == "Food Prices for Nutrition 1.0", `Classification Code` == "FPN 1.0") %>%  
  dplyr::select("Country Code", "Time", "Percent of the population who cannot afford sufficient calories [CoCA_headcount]", "Percent of the population who cannot afford nutrient adequacy [CoNA_headcount]", "Percent of the population who cannot afford a healthy diet [CoHD_headcount]") %>% 
  gdata::rename.vars(colnames(.), c("R", "Y", "CoCA", "CoNA", "CoHD")) %>% mutate(CoCA = as.numeric(CoCA), CoNA = as.numeric(CoNA), CoHD = as.numeric(CoHD))

df_FoodPov_his <- df_FoodPov_his %>% 
  filter(Y %in% c("2017")) %>% dplyr::select(-"CoHD", -"Y") %>% 
  left_join(df_FoodPov_his%>% dplyr::select(-c("Y","CoCA", "CoNA")) %>% filter(!(CoHD == "..")) %>% dplyr::group_by(R) %>% dplyr::reframe(CoHD = mean((CoHD))) %>% filter(!(CoHD == 0))) %>% 
  dplyr::rename("Energy insufficient diet" = "CoCA", "Nutrient inadequate diet" = "CoNA", "Unhealthy diet" = "CoHD") %>% 
  pivot_longer(c("Energy insufficient diet", "Nutrient inadequate diet", "Unhealthy diet"), names_to = "Indicator", values_to = "value_his") %>% 
  left_join(df_FoodPov_p_od %>% filter(model == "AIM-Hub", Y == "2020", target == "No-Miti", gini == "consistent", !is.na(Indicator)) %>% 
              dplyr::select(model, R, Indicator, value) %>% mutate(value = 100*value) %>% pivot_wider(names_from = "Indicator", values_from = "value") %>% 
              # dplyr::rename("CoCA" = "Energy insufficient diet", "CoNA" = "Nutrient inadequate diet", "CoHD" = "Unhealthy diet") %>% 
              pivot_longer(c("Energy insufficient diet", "Nutrient inadequate diet", "Unhealthy diet"), names_to = "Indicator", values_to = "value_prj")) %>% 
  mutate(Indicator = factor(Indicator, levels = c("Energy insufficient diet", "Nutrient inadequate diet", "Unhealthy diet"))) %>% 
  left_join(Map_r_PHI2Hub) %>% 
  F_R_CGEfull()


pdata <- df_FoodPov_his %>% left_join(Map_r_PHI2Hub) %>% filter(!is.na(R_CGE))


unique(pdata$R) %>% length()
p <- ggplot(pdata, mapping = aes(x = value_his, y = value_prj)) +
  geom_point(aes(color = R_CGE)) +
  geom_abline(intercept = 0, slope = 1, color = "grey50") +
  facet_wrap(~Indicator, ncol = 1) +
  labs(x = "World Bank statistics (%)", y = "Model estimation (%)", color = "Region") +
  MyTheme +
  theme(legend.position = "right") + guides(color = guide_legend(ncol = 1)) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(use_label(c("eq", "R2")), formula = y ~ x + 0) +
  scale_color_manual(values = palette_R_CGE) 
p
ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_Food_Poverty_Compare_historical_wld.pdf"), 
       width = 12, height = 21, units = "cm")

# p <- p +theme(legend.position = "right")
# p_leg <- get_legend(p) %>% as_ggplot()
# ggsave(p_leg, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_Food_Poverty_Compare_historical_wld_legend.pdf"), width = 5, height = 12, units = "cm")

rm(p)

colnames(pdata)
if(sw_adj == T){
  rsq_by_group <- pdata %>%
    group_by(Indicator) %>%
    group_modify(~{
      model <- lm(value_prj ~ value_his + 0, data = .)  # Fit linear model
      rsq <- summary(model)$r.squared  # Extract R-squared
      slope <- coef(model)[1]
      data.frame(R_squared = rsq, adj = slope)  # Return R-squared value
    })
  # our projections overestimates food poverty
  # CoCA: est = 0.715*obs, CoNA: est = 0.864*obs, CoHA: est = 1.03*obs
  # adjustment factor for better reproduction of the national observations
  df_adj <- rsq_by_group %>% dplyr::select(Indicator, adj)
  rm(rsq_by_group)
  df_FoodPov_p <- df_FoodPov_p_od %>% left_join(df_adj) %>% mutate(value = value/adj, scenario = paste0(target, "_", revenue)) 
}else{
  df_FoodPov_p <- df_FoodPov_p_od %>% mutate(scenario = paste0(target, "_", revenue), adj = "none") 
}

# adjust the projections

df_FoodPov_rate <- df_FoodPov_p %>% dplyr::select(-adj) %>% mutate(unit = "1")
save(df_FoodPov_rate, df_FoodPov_p, df_FPLsc, df_FPLsc, file = paste0(dir_output_YSSPpaper, "/RData/Food poverty.RData"))
rm(df_FoodPov_rate)

# global statistics after the adjustment (either adjusted or not)
head(df_FoodPov_his)
head(df_FoodPov_p)
pdata <- df_FoodPov_his %>% left_join(df_FoodPov_p %>% 
                                        filter(model == "AIM-Hub", gini == "consistent", Ref == "SSP2_Baseline", Y == "2020") %>% mutate(value_prj_adj = value*100) %>% 
                                        dplyr::select(R, R_CGE, Ref, Indicator, value_prj_adj))
unique(pdata$R) %>% length()
p <- ggplot(pdata, mapping = aes(x = value_his, y = value_prj_adj)) +
  geom_point(aes(color = R_CGE)) +
  geom_abline(intercept = 0, slope = 1, color = "grey50") +
  facet_wrap(~Indicator, ncol = 1) +
  labs(x = "World Bank statistics (%)", y = "Model estimation (%)", color = "Region") +
  MyTheme +
  theme(legend.position = "right") + guides(color = guide_legend(ncol = 1)) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(use_label(c("eq", "R2")), formula = y ~ x + 0) +
  scale_color_manual(values = palette_R_CGE) 
p
ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_Food_Poverty_Compare_historical_wld_adj.pdf"), width = 11, height = 18, units = "cm")

## 2.2 col: region, headcount ----
pdata <- df_FoodPov_p %>% filter(Indicator %in% c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet")) %>% 
  dplyr::group_by(model, scenario, target, revenue, gini, R_CGE,  Y, Indicator) %>% 
  dplyr::reframe(value = sum(value * POP), POP = sum(POP)) 

write.xlsx(pdata, file = paste0(dir_output_YSSPpaper, "/csv/8_FoodPoverty_R_CGE.xlsx"))

p <- ggplot(pdata %>% filter(target %in% c("1.5C", "No-Miti"), model == "AIM-Hub", Y %in% c(2030, 2050, 2070), gini == "consistent")) +
  geom_col(aes(x = factor( recode(scenario, "1.5C_Neutral" = "1.5C\n Neutral", "1.5C_EPC" = "1.5C\n EPC", "No-Miti_Neutral" = "No-Miti"),
                           levels = c("1.5C\n Neutral",  "1.5C_EPC" = "1.5C\n EPC", "No-Miti" = "No-Miti")) , 
               y = value/1000000, fill = R_CGE, group = paste0(model, scenario,R_CGE, revenue)), width = 0.5) +
  facet_grid(Indicator ~ Y) +
  MyTheme +
  scale_fill_manual(values = palette_R_CGE) +
  guides(fill = guide_legend(title = "Region", ncol = 1)) +
  labs(x = "Scenario", y = "Food poverty headcount (million)")
p 

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/8_FoodPoverty.pdf"), 
       width = 22, height = 17, units = "cm")
rm(p)


### model comparison ----
pdata1 <- pdata %>% filter(target %in% c("1.5C"),  Y %in% c(2030, 2040, 2050, 2070)) %>% dplyr::select(-POP) %>% 
  pivot_wider(names_from = "model", values_from = "value") %>% mutate(gini = case_when(gini == "consistent" ~ "SSP2", gini != "consistent" ~ gini))

p <- ggplot(pdata1,aes(x = `AIM-Hub`/1000000, 
                       y = `MESSAGEix`/1000000)) +
  geom_point(aes(color = R_CGE, group = paste0(gini,scenario,R_CGE), alpha = gini, shape = Y, size = Indicator)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  facet_wrap(~ revenue, ncol = 1, scales = "free_y") +
  MyTheme + theme(legend.position = "right") +
  scale_color_manual(values = palette_R_CGE) +
  scale_size_manual(values = c("Energy insufficient diet" = 2,"Nutrient inadequate diet" = 1.5, "Unhealthy diet" = 1)) +
  scale_alpha_manual(values = palette_alpha_gini_all) +
  guides(fill = guide_legend(title = "Region", ncol = 1)) +
  labs(x = "AIM-Hub (million)", y = "MESSAGEix (million)", color = "Region", alpha = "SSP", shape = "Year", size = "Indicator") +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(use_label(c("eq", "R2")), formula = y ~ x + 0) 

p
ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/8_FoodPoverty_model compare.pdf"), 
       width = 12, height = 13, units = "cm")
p_leg <- get_legend(p + theme(legend.position = "right")) %>% as_ggplot()

ggsave(p_leg, filename = paste0(dir_output_YSSPpaper, "/fig/8_FoodPoverty_model compare_legend.pdf"), 
       width = 5, height = 24, units = "cm")

 rm(p, p_leg)




## 2.3 col: rate ----
pdata <- df_FoodPov_p %>% filter(model == "AIM-Hub", Indicator %in% c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet")) %>% 
  dplyr::group_by(model, scenario, R_CGE, revenue, target, gini, Y, Indicator) %>% 
  dplyr::reframe(value = sum(value * POP)/sum(POP)) 

write.xlsx(pdata, file = paste0(dir_output_YSSPpaper, "/csv/8_FoodPoverty_R.xlsx"))

p <- ggplot(pdata %>% filter(target %in% c("1.5C", "No-Miti"), Y %in% c(2030, 2040, 2050, 2070), gini == "consistent")) +
  geom_col(aes(x = factor( recode(scenario, "1.5C_Neutral" = "1.5C\n Neutral", "1.5C_EPC" = "1.5C\n EPC", "No-Miti_Neutral" = "No-Miti"),
                           levels = c("1.5C\n Neutral",  "1.5C_EPC" = "1.5C\n EPC", "No-Miti" = "No-Miti")) , 
               y = value*100, fill = R_CGE, group = paste0(model, scenario, R_CGE, revenue)), width = 0.4, position = "dodge") +
  facet_grid(Indicator ~ Y) +
  MyTheme +
  scale_fill_manual(values = palette_R_CGE) +
  guides(fill = guide_legend(title = "Region", ncol = 1)) +
  labs(x = "Revenue", y = "Food poverty headcount (%)")
p 

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_FoodPoverty_share.pdf"), 
       width = 34, height = 17, units = "cm")
rm(p)

## 2.4 box: change in food poverty ----
df_FoodPov_change <- df_FoodPov_p %>% filter( Indicator %in% c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet")) %>% 
  dplyr::select(model, scenario, R, R_CGE, revenue, target, gini, Y, Indicator, value) %>% 
  left_join(df_FoodPov_p %>% filter(Indicator %in% c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet"), target == "No-Miti") %>% 
              dplyr::select(model, R, R_CGE, Y, Indicator, gini, value) %>% dplyr::rename("value_BaU" = "value")) %>% 
  mutate(change = value-value_BaU)

write.xlsx(df_FoodPov_change, file = paste0(dir_output_YSSPpaper, "/csv/8_FoodPoverty_rate_change.xlsx"))
unique(df_FoodPov_change$R) %>% length()
a <- df_FoodPov_change %>% filter(gini == "consistent", revenue == "Neutral", 
                                  Y %in% c("2030", "2050"), Indicator == "Nutrient inadequate diet", target == "1.5C") %>% 
  dplyr::group_by(model, scenario, R_CGE, revenue, target, gini, Y, Indicator) %>% reframe(median = median(change))

pdata <- df_FoodPov_change
p <- ggplot(pdata %>% filter(target == "1.5C", Y %in% c(2030, 2040, 2050, 2070), model == "AIM-Hub", 
                             R_CGE %in% c("Rest of South America", "Sub-Saharan Africa", "Southeast Asia", "Rest of Asia"))) +
  geom_boxplot(aes(x = R_CGE, y = change*100, color = R_CGE, linetype = revenue), outlier.alpha = .2, outlier.size = .5) +
  geom_hline(yintercept = 0, color = "grey50") +
  MyTheme +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(x = "Region", y = "Increase in food poverty rate (% point)") +
  scale_color_manual(values = palette_R_CGE) +
  guides(color = guide_legend(title = "Region"), linetype = guide_legend(title = "Revenue")) +
  facet_grid(Indicator ~ Y)
p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/8_FoodPoverty_box.pdf"), 
       width = 22, height = 18, units = "cm")



# dplyr::select
### 2.4.1 change, box: rate, Nutrient inadequate diet ----

pdata <- df_FoodPov_change %>% filter( Indicator %in% c("Nutrient inadequate diet"))


p <- ggplot(pdata %>% filter(target == "1.5C", Y %in% c(2030, 2040, 2050, 2070), R_CGE %in% c("Rest of South America", "Sub-Saharan Africa", "Southeast Asia", "Rest of Asia"))) +
  geom_boxplot(aes(x = R_CGE, y = change*100, color = R_CGE, linetype = model), outlier.alpha = .2, outlier.size = .5) +
  geom_hline(yintercept = 0, color = "grey50") +
  MyTheme +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  labs(x = "Region", y = "Increase in food poverty rate (% point)", subtitle = "Nutrient inadequate diet") +
  scale_color_manual(values = palette_R_CGE) +
  guides(color = guide_legend(title = "Region"), linetype = guide_legend(title = "Model")) +
  facet_grid(revenue ~ Y)
p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/8_FoodPoverty_box_CoNA_model.pdf"), 
       width = 20, height = 12, units = "cm")



## point: region, headcount, model, Nutrient inadequate diet ----
for(i in seq_along(lis_indicator)){
  pdata <- df_FoodPov_p %>% mutate(scenario = paste0(target, "_", revenue)) %>% 
    filter(Indicator %in% lis_indicator[i]) %>% # c("Nutrient inadequate diet")
    dplyr::group_by(model, scenario, target, revenue, gini, R_CGE,  Y, Indicator) %>% 
    dplyr::reframe(value = sum(value * POP), POP = sum(POP)) 
  
  
  p <- ggplot(pdata %>% filter(target %in% c("1.5C", "No-Miti"), Y %in% c(2030, 2050, 2070), gini == "consistent")) +
    geom_col(aes(x = factor( recode(scenario, "1.5C_Neutral" = "1.5C\n Neutral", "1.5C_EPC" = "1.5C\n EPC", "No-Miti_Neutral" = "No-Miti"),
                             levels = c("1.5C\n Neutral",  "1.5C_EPC" = "1.5C\n EPC", "No-Miti_Neutral" = "No-Miti")) ,
                 y = value/1000000, fill = R_CGE, group = paste0(model, scenario, R_CGE, revenue)), width = 0.5) +
    facet_grid(Y ~ model) + 
    MyTheme + theme(legend.key.size = unit(0.46, 'cm'), 
                    legend.key.height = unit(0.46, 'cm'),
                    legend.key.width = unit(0.46, 'cm'), 
                    legend.title = element_text(size=10),
                    legend.text = element_text(size=9)) +
    scale_fill_manual(values = palette_R_CGE) +
    guides(fill = guide_legend(title = "Region", ncol = 1)) +
    labs(x = "Scenario", y = "Food poverty headcount (million)")
  p 
  
  ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/8_FoodPoverty_col_",lis_indicator_abb[i],"_model.pdf"), 
         width = 13, height = 16.5, units = "cm")
}

## point plot ---------
pdata1 <- pdata %>% filter(target %in% c("1.5C", "No-Miti"), Y %in% c(2030, 2050, 2070)) %>% dplyr::select(-POP) %>% 
  pivot_wider(names_from = "model", values_from = "value")
unique(pdata1$gini)
p <- ggplot(pdata1,mapping = aes(x = `AIM-Hub`/1000000,
                                 y = `MESSAGEix`/1000000)) +
  geom_point(aes(alpha = gini, color = R_CGE, shape = Y)) +
  geom_abline(intercept = 0, slope = 1, color = "grey") +
  # facet_grid(model ~ Y) + 
  MyTheme + theme(legend.key.size = unit(0.46, 'cm'), 
                  legend.key.height = unit(0.46, 'cm'),
                  legend.key.width = unit(0.46, 'cm'), 
                  legend.title = element_text(size=10),
                  legend.text = element_text(size=9),
                  legend.position = "none") +
  scale_color_manual(values = palette_R_CGE) + scale_alpha_manual(values = palette_alpha_gini) +
  guides(color = guide_legend(title = "Region", ncol = 2)) +
  labs(x = "MESSAGEix (million)", y = "AIM-Hub (million)", shape = "Year", alpha = "SSP") +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(use_label(c("eq", "R2")), formula = y ~ x + 0) 
p 

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/8_FoodPoverty_point_CoNA_model.pdf"), 
       width = 10, height = 10, units = "cm")

p_leg <- get_legend(p + theme(legend.position = "right")) %>%  as_ggplot()
ggsave(p_leg, filename = paste0(dir_output_YSSPpaper, "/fig/8_FoodPoverty_point_CoNA_model_legend.pdf"), 
       width = 8, height = 12, units = "cm")
rm(p, pdata1, p_leg)



## col: region, headcount, all ----
pdata <- df_FoodPov_p %>% mutate(scenario = paste0(target, "_", revenue))%>% 
  filter(Indicator %in% c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet"), gini == "consistent") %>% 
  dplyr::group_by(model, scenario, target, gini, revenue, R_CGE,  Y, Indicator) %>% 
  dplyr::reframe(value = sum(value * POP), POP = sum(POP)) 


p <- ggplot(pdata %>% filter(target %in% c("1.5C", "No-Miti"), Y %in% c(2030))) +
  geom_col(aes(x = factor( recode(scenario, "1.5C_Neutral" = "1.5C\n Neutral", "1.5C_EPC" = "1.5C\n EPC", "No-Miti_Neutral" = "No-Miti"),
                           levels = c("1.5C\n Neutral",  "1.5C_EPC" = "1.5C\n EPC", "No-Miti_Neutral" = "No-Miti")) ,
               y = value/1000000, fill = R_CGE, group = paste0(model, scenario,R_CGE, revenue)), width = 0.5) +
  facet_grid(Indicator ~ model) +
  MyTheme + theme(legend.key.size = unit(0.46, 'cm'), 
                  legend.key.height = unit(0.46, 'cm'),
                  legend.key.width = unit(0.46, 'cm'), 
                  legend.title = element_text(size=10),
                  legend.text = element_text(size=9)) +
  scale_fill_manual(values = palette_R_CGE) +
  guides(fill = guide_legend(title = "Region", ncol = 1)) +
  labs(x = "Scenario", y = "Food poverty headcount (million)")
p 

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/8_FoodPoverty_2030.pdf"), 
       width = 14, height = 16.5,units = "cm")

# rm(df_FoodPov_change, df_FoodPov, df_FoodPov_his, df_FoodPov_p, df_FoodPov_tmp)
# rm(pdata, p551, p, p_1, p_21, p_211, p_5b, p_leg, p_551, PoV, PoV_WLD, df_Sigma_exp,df_Mu_exp, Gini, Gini_cge, Gini_msg, pdata, pdata1, pdata2)

# 3. plot: Food poverty lines and cost indicator ---------------------------------------------------

colnames(df_FPLsc)
unique(df_FPLsc$Ref)
unique(df_FPLsc$Y)
unique(df_FPLsc$Indicator)
unique(df_FPLsc$model)

## 3.1 Cost indicator ----

### 3.1.1 regional ----

pdata <- df_FPLsc %>% filter(Indicator %in% c("CoCA"), Y %in% c(2030, 2040, 2050, 2070)) %>%  # Indicator %in% c("CoNA"),
  F_reve_mutate() %>% filter(revenue == "Neutral") %>% 
  left_join(Map_r_PHI2Hub) %>% F_R_CGEfull()

p <- ggplot(pdata) +
  geom_boxplot(aes(x = R_CGE, y = value_indicator, color = target), width = .7, outliers = F) + # , linetype = model
  geom_hline(yintercept = 2.15, color = "grey", linetype = "dashed") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  facet_wrap(~Y) +
  labs(x = "Region", y = "cost (2017PPP$ per capita per day)") + # , linetype = "Model"
  guides(color = guide_legend(title = "Scenario")) +
  # coord_cartesian(ylim = c(0, 7.5)) +
  scale_color_manual(values = palette_Sc)# +

p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_costdiets_CoCA_regional.pdf"), 
       width = 24, height = 16, units = "cm")



# 2030
pdata <- df_FPLsc %>% filter(Indicator %in% c("CoCA", "CoNA", "CoHD"), 
  Y %in% c(2030)) %>% 
  mutate(Indicator = recode(.$Indicator, 
                            "CoCA" = "Energy insufficient diet", 
                            "CoNA" = "Nutrient inadequate diet", 
                            "CoHD" = "Unhealthy diet"
  )) %>% 
  # dplyr::rename(PHIscenario = "Ref") %>% left_join(MapScenario_main) %>% 
  # dplyr::select(-c("PHIscenario", "CGEscenario", "MSGscenario")) %>% 
  F_reve_mutate() %>% filter(revenue == "Neutral", !is.na(Indicator)) %>% #, target != "2C") %>% 
  left_join(Map_r_PHI2Hub) %>% F_R_CGEfull()

p <- ggplot(pdata) +
  geom_boxplot(aes(x = R_CGE, y = value_indicator, color = target, linetype = model), width = .7, outliers=F) +
  geom_hline(yintercept = 1.9, color = "grey", linetype = "dashed") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  facet_grid(Indicator~.) +
  labs(x = "Region", y = "Cost (2017PPP$ per capita per day)", linetype = "Model") +
  guides(color = guide_legend(title = "Scenario")) +
  # coord_cartesian(ylim = c(0, 7.5)) +
  scale_color_manual(values = palette_Sc) #+
# scale_y_continuous(breaks = seq(0,7.5,2.5))

p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_FPL_regional_2030.pdf"), 
       width = 20, height = 18, units = "cm")





## 3.2 income food poverty lines ----
### 3.2.1 global ----
pdata <- df_FPL_inc1 %>% filter(Indicator %in% c("CoCA","CoNA", "CoHD"), Y %in% c(2020, 2030, 2040, 2050, 2060, 2070)) %>% 
  mutate(Indicator = recode(.$Indicator, 
                            "CoCA" = "Energy insufficient diet", 
                            "CoNA" = "Nutrient inadequate diet", 
                            "CoHD" = "Unhealthy diet")) %>% 
  F_reve_mutate() %>% filter(revenue == "Neutral")
colnames(pdata)


p <- ggplot() +
  geom_boxplot(pdata, mapping = aes(x = Y, y = value_indicator_adj_inc/365, color = target), position = position_dodge(width = .8), width = .6, # , linetype = model
               width = .7, outliers = F) +
  geom_hline(data = data.frame(TH_name = c("2.15-threshold", "3.65-threshold", "6.85-threshold"), TH = c(2.15, 3.65, 6.85)),
             mapping = aes(yintercept = TH, linetype = TH_name), color = "grey") +
  MyTheme + theme(axis.text.x = element_text(angle = 30)) +
  facet_grid(~Indicator) +
  labs(x = "Year", y = "FPL (2017PPP$ per capita per day)") +
  guides(color = guide_legend(title = "Scenario"), linetype = guide_legend(title = "Poverty line")) +
  scale_color_manual(values = palette_Sc) 

p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_FPL_global.pdf"), 
       width = 20, height = 8, units = "cm")

#### global + model ----
# a <- pdata %>% filter(target %in% c("No-Miti", "1.5C"))
p <- ggplot() +
  geom_boxplot(pdata, # filter(target %in% c("No-Miti", "1.5C")) 
               mapping = aes(x = Y, y = value_indicator_adj_inc/365, color = model), # linetype = model, 
               position = position_dodge(width = .8), width = .6, 
               width = .7, outliers = F) +
  geom_hline(data = data.frame(TH_name = c("2.15-threshold", "3.65-threshold", "6.85-threshold"), TH = c(2.15, 3.65, 6.85)),
             mapping = aes(yintercept = TH, linetype = TH_name), color = "grey") +
  MyTheme + theme(axis.text.x = element_text(angle = 30)) +
  facet_grid(target~Indicator) +
  labs(x = "Year", y = "FPL (2017PPP$ per capita per day)") +
  guides(color = guide_legend(title = "Scenario"), linetype = guide_legend(title = "Poverty line")) +
  scale_color_manual(values = palette_model) 

p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_FPL_global_model.pdf"), 
       width = 20, height = 14, units = "cm")


### 3.3 regional ----
pdata <- df_FPL_inc1 %>% filter(Indicator %in% c("CoCA"), Y %in% c(2030, 2040, 2050, 2070)) %>%  # Indicator %in% c("CoNA"),
  F_reve_mutate() %>% filter(revenue == "Neutral") %>% 
  left_join(Map_r_PHI2Hub) %>% F_R_CGEfull()

p <- ggplot(pdata) +
  geom_boxplot(aes(x = R_CGE, y = value_indicator_adj_inc/365, color = target), width = .7, outliers = F) + # , linetype = model
  geom_hline(yintercept = 2.15, color = "grey", linetype = "dashed") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  facet_wrap(~Y) +
  labs(x = "Region", y = "FPL (2017PPP$ per capita per day)") + # , linetype = "Model"
  guides(color = guide_legend(title = "Scenario")) +
  # coord_cartesian(ylim = c(0, 7.5)) +
  scale_color_manual(values = palette_Sc)# +

p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_FPL_CoCA_regional.pdf"), 
       width = 24, height = 16, units = "cm")


#### regional and different indicators -----
pdata <- df_FPL_inc1 %>% filter(Indicator %in% c("CoCA", "CoNA", "CoHD"), 
                                Y %in% c(2030)) %>%  # Indicator %in% c("CoNA"),
  mutate(Indicator = recode(.$Indicator, 
                            "CoCA" = "Energy insufficient diet", 
                            "CoNA" = "Nutrient inadequate diet", 
                            "CoHD" = "Unhealthy diet")) %>% 
  F_reve_mutate() %>% filter(revenue == "Neutral") %>% 
  left_join(Map_r_PHI2Hub) %>% F_R_CGEfull() #%>% filter(target %in% c("No-Miti", "1.5C") )

p <- ggplot(pdata) +
  geom_boxplot(aes(x = R_CGE, y = value_indicator_adj_inc/365, color = target), width = .7, outliers = F) + # , linetype = model
  geom_hline(yintercept = 2.15, color = "grey", linetype = "dashed") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  facet_wrap(Indicator~., ncol = 1) +
  labs(x = "Region", y = "FPL (2017PPP$ per capita per day)") + # , linetype = "Model"
  guides(color = guide_legend(title = "Scenario")) +
  scale_color_manual(values = palette_Sc)# +

p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/8_FPL_standards_regional.pdf"), 
       width = 14, height = 14, units = "cm")


# rm(p, pdata, df_FPLsc, df_FoodPov_p_od, df_FoodPov_p)
# 4. See the data ----
b <- pdata <- df_FoodPov_p %>% filter(Indicator %in% c("Energy insufficient diet","Nutrient inadequate diet","Unhealthy diet")) %>% 
  dplyr::group_by(model, scenario, target, revenue, gini, R_CGE, R,  Y, Indicator) %>% 
  dplyr::reframe(value = sum(value * POP), POP = sum(POP))  %>% 
  filter(target %in% c("1.5C", "No-Miti"), model == "AIM-Hub", Y %in% c(2030, 2050, 2070), gini == "consistent")


print("The END of AIMHub_only/8_Food_Poverty.R")


