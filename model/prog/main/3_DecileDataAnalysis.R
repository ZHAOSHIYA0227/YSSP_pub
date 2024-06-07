# This Rscript deals with the decile data after imputation
# By Shiya ZHAO, 2023/11/01

# 1 calculate the inequality index 
#https://www.sciencedirect.com/science/article/pii/S0305750X18304212
# policy effect of a carbon tax on the lowest income group relative to the national average over GDP per capita 

require(tidyverse)
require(gamstransfer)
# 0. load data ------------------------------------------------------------
m_cge <- Container$new("../../output/gdx/Decile/AIMHub.gdx")
m_msg <- Container$new("../../output/gdx/Decile/MESSAGEix.gdx")

df_budget <- m_cge["Budget"]$records %>% mutate(model = "AIM-Hub") %>% 
  rbind(m_msg["Budget"]$records %>% mutate(model = "MESSAGEix"))
colnames(df_budget) <- str_split(colnames(df_budget), pattern = "_", simplify = T)[,1] 
df_budget <- df_budget %>% dplyr::rename("PHIscenario" = "Ref") %>% left_join(MapScenario) %>% left_join(Map_r_PHI2Hub) %>% 
  mutate(DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character, "ALL")))


df_Exp <- m_cge["Exp_I"]$records %>% mutate(model = "AIM-Hub") %>% 
  rbind(m_msg["Exp_I"]$records %>% mutate(model = "MESSAGEix"))
colnames(df_Exp) <- str_split(colnames(df_Exp), pattern = "_", simplify = T)[,1] 
df_Exp <- df_Exp %>% dplyr::rename("PHIscenario" = "Ref") %>% left_join(MapScenario) %>% left_join(Map_r_PHI2Hub) %>% left_join(MapI) %>% 
  mutate(I_abb= factor(I_abb, levels = lis_I_abb), DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character, "ALL"))) %>% 
  select("model", "R", "R_CGE", "Y", "scenario", "DEC", "I_abb", "value")

df_ExpShare <- m_cge["ExpShare_I"]$records %>% mutate(model = "AIM-Hub") %>% 
  rbind(m_msg["ExpShare_I"]$records %>% mutate(model = "MESSAGEix"))
colnames(df_ExpShare) <- str_split(colnames(df_ExpShare), pattern = "_", simplify = T)[,1] 
df_ExpShare <- df_ExpShare %>% dplyr::rename("PHIscenario" = "Ref") %>% left_join(MapScenario) %>% left_join(Map_r_PHI2Hub) %>% left_join(MapI)  %>% 
  mutate(I_abb= factor(I_abb, levels = lis_I_abb), DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character, "ALL"))) %>% 
  select("model", "R", "R_CGE", "Y", "scenario", "DEC", "I_abb", "value")


df_Con <- m_cge["Con_I"]$records %>% mutate(model = "AIM-Hub") %>% 
  rbind(m_msg["Con_I"]$records %>% mutate(model = "MESSAGEix"))
colnames(df_Con) <- str_split(colnames(df_Con), pattern = "_", simplify = T)[,1] 
df_Con <- df_Con %>% dplyr::rename("PHIscenario" = "Ref") %>% left_join(MapScenario) %>% left_join(Map_r_PHI2Hub) %>% left_join(MapI) %>% 
  mutate(I_abb= factor(I_abb, levels = lis_I_abb), DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character, "ALL")))  %>% 
  select("model", "R", "R_CGE", "Y", "scenario", "DEC", "I_abb", "value")

df_PQ <- m_cge["Price_I"]$records %>% mutate(model = "AIM-Hub") %>% 
  rbind(m_msg["Price_I"]$records %>% mutate(model = "MESSAGEix"))
colnames(df_PQ) <- str_split(colnames(df_PQ), pattern = "_", simplify = T)[,1] 
df_PQ <- df_PQ %>% dplyr::rename("PHIscenario" = "Ref") %>% left_join(MapScenario) %>% left_join(Map_r_PHI2Hub) %>% left_join(MapI) %>% 
  mutate(I_abb= factor(I_abb, levels = lis_I_abb))  %>% 
  select("model", "R", "R_CGE", "Y", "scenario",  "I_abb", "value")

# budget plot -------------------------------------------------------------------

pdata <- df_budget %>% 
  select(R, Y, model, scenario, DEC,  value) %>%
  distinct() %>% 
  filter( model == "AIM-Hub") %>%
  pivot_wider(names_from = "scenario", values_from = "value") %>% 
  pivot_longer(c("2C", "1.5C", "2C_EPC", "1.5C_EPC"), values_to = "value", names_to = "scenario") %>% 
  mutate(change = (value - `No-Miti`)/`No-Miti`) %>% F_reve_mutate()


pdata %>% filter(Y == "2030", DEC != "ALL", change < 1) %>% openxlsx::write.xlsx(file = paste0(dir_out, "/csv/3_ConsumptionLoss_tot.xlsx"))



lis_y_for <- c(seq(2030, 2070,20))
for(y in 1:length(lis_y_for)){
# y <- 1
  p <- ggplot(pdata %>% filter(Y == lis_y_for[y], DEC != "ALL", change < 1)) +
    geom_boxplot(aes(x = DEC, y = change * 100, 
                   group = paste0(DEC,scenario, Y), color = scenario_Name), width = .7, outliers = F, linewidth = 0.3) + 
    geom_hline(yintercept = 0, color = "grey50") +
    MyTheme +
    labs(x = "Decile", y = "Change in household expenditure | %", subtitle = lis_y_for[y]) +
    scale_color_manual(values = palette_Sc)+
    guides(color = guide_legend(title = "Scenario")) + 
    facet_wrap(~revenue, scales = "free", ncol = 1)

  p

  ggsave(p, filename = paste0(dir_out, "/fig/3_budget_box_", lis_y_for[y], ".pdf"), width = 10, height = 10, units = "cm")
  rm(p)

}

rm(pdata)

# EV plot -----------------------------------------------------------------
df_EV <- df_Con %>% left_join(df_PQ %>% filter(scenario == "No-Miti") %>% select(-scenario) %>% dplyr::rename("PQ_BaU" = "value")) %>% 
  left_join(df_Con %>% filter(scenario == "No-Miti") %>% select(-scenario) %>% dplyr::rename("Con_BaU" = "value")) %>% 
  dplyr::group_by(model, R, R_CGE, Y, scenario, DEC) %>% 
  dplyr::mutate(EVi = (value*PQ_BaU-Con_BaU*PQ_BaU)/sum(Con_BaU*PQ_BaU), EV = sum(EVi), Coni = (value*PQ_BaU-Con_BaU*PQ_BaU)/(Con_BaU*PQ_BaU)) %>% F_reve_mutate()
df_EV %>% filter(Y %in% c(2030), scenario %in% c("1.5C", "1.5C_EPC")) %>% select(model, R, R_CGE,Y,scenario, DEC, I_abb, EVi) %>% 
  openxlsx::write.xlsx(file = paste0(dir_out,"/csv/3_EVi_2030.xlsx"))
df_EV %>% filter(Y %in% c(2050), scenario %in% c("1.5C", "1.5C_EPC")) %>% select(model, R, R_CGE,Y,scenario, DEC, I_abb, EVi) %>% 
  openxlsx::write.xlsx(file = paste0(dir_out,"/csv/3_EVi_2050.xlsx"))
df_EV %>% filter(Y %in% c(2070), scenario %in% c("1.5C", "1.5C_EPC")) %>% select(model, R, R_CGE,Y,scenario, DEC, I_abb, EVi) %>% 
  openxlsx::write.xlsx(file = paste0(dir_out,"/csv/3_EVi_2070.xlsx"))

lis_y_for <- c(seq(2030, 2070,20))
for(y in 1:length(lis_y_for)){
  # y <- 1
pdata <- df_EV %>% select(-EVi) %>% distinct() %>% filter(scenario != "No-Miti", Y %in% lis_y_for[y], DEC != "ALL") %>% filter(EV < 1)


p <- ggplot(pdata) +
  geom_boxplot(aes(x = DEC, y = EV * 100, group = paste0(DEC,scenario), 
                          color = scenario_Name), 
               outliers = F) + 
  geom_hline(yintercept = 0, color = "grey50") +
  MyTheme +
  facet_wrap(~revenue, scales = "free", ncol = 4) +
  labs(x = "Decile", y = "Change in consumption | %", subtitle = lis_y_for[y]) +
  scale_color_manual(values = palette_Sc) + 
  # scale_y_continuous(breaks = pretty(pdata$EV*100, n = 5)) +
  guides(color = guide_legend(title = "Scenario"))

p

ggsave(p, filename = paste0(dir_out, "/fig/SI/3_EV_box_", lis_y_for[y], ".pdf"), 
       width = 18, height = 7, units = "cm")
       rm(p)
}




# 0_income index and stuff ------------------------------------------------
df_index <- df_budget %>% 
  # mutate(expenditure = share * budget) %>% 
  select(c("model", "R", "Y", "DEC",  "scenario", "value")) %>% 
  distinct() %>% 
  pivot_wider(names_from = "scenario", values_from = "value") %>% 
  pivot_longer(all_of(lis_scenario_mitigation), names_to = "scenario", values_to = "value") %>% 
  # select(c("model", "R", "Y", "DEC", "I_abb", "scenario", "No-Miti", "value")) %>% 
  mutate(DEC1 = recode(.$DEC, 
                       "1" = "low",
                       "2" = "low",
                       "3" = "low",
                       "4" = "low",
                       "7" = "high",
                       "8" = "high",
                       "9" = "high",
                       "10" = "high"
  )) %>% 
  filter(DEC1 %in% c("low", "high"), 
         Y %in% lis_y) %>% 
  dplyr::group_by(model, R, Y, DEC1,  scenario) %>% 
  reframe(change1 = (sum(value)-sum(`No-Miti`))/sum(`No-Miti`) *100) %>%
  pivot_wider(names_from = "DEC1", values_from = "change1") %>% 
  mutate(sign_low = `low`/abs(`low`), sign_high = `high`/abs(`high`)) %>% 
  filter(!is.na(low)) %>% 
  mutate(#mean = select(., `1`:`10`) %>% rowSums(na.rm = TRUE)/10,
    index = low/high,
    size = ((index-min(index)+1)/(max(index)-min(index)))) %>% 
  filter(index != max(index))


# plot of index -----------------------------------------------------------

pdata <- df_index #%>% filter(model == "MESSAGEix")

p <- ggplot(pdata) +
  geom_point(aes(x = low, y = high, color = model, shape = scenario,alpha = scenario), size = 0.7) +
  geom_vline(xintercept = 0, color = "grey70") +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  geom_abline(intercept = 0, slope = -1, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = palette_model) +
  scale_alpha_manual(values = c("2C" = 0.5, "1.5C" = 1)) +
  guides(color = guide_legend(title = "Model"), alpha = guide_legend(title = "Scenario"), shape = guide_legend(title = "Scenario")) +
  labs(y = "Change in budget higher-income population |%", x = "Change in lower-income population |%") +  
  ggthemes::theme_pander()

p

ggsave(p, filename = paste0(dir_out, "/fig/SI/3_change of income.pdf"), width = 12, height = 8, units = "cm")
  rm(p)

# boxplot-budget
csv_output <- df_budget %>% 
  # mutate(expenditure = share * budget) %>% 
  select(c("model", "R", "Y", "DEC",  "scenario", "value")) %>% 
  distinct() %>% 
  pivot_wider(values_from = "value", names_from = "scenario") %>% 
  pivot_longer(cols = c("2C", "1.5C"),values_to = "budget", names_to = "scenario") %>% 
  mutate(change = (budget - `No-Miti`)/`No-Miti`) %>% 
  group_by(model, scenario, Y, DEC) %>% 
  reframe(median = median(change)) 
openxlsx::write.xlsx(csv_output, file = paste0(dir_out, "/csv/Dec_budget.xlsx"))

rm(csv_output)

pdata <- df_budget %>% 
  select(c("model", "R", "Y", "DEC",  "scenario", "value")) %>% 
  distinct() %>% 
  pivot_wider(values_from = "value", names_from = "scenario") %>% 
  pivot_longer(cols = c("2C", "1.5C"),values_to = "budget", names_to = "scenario") %>% 
  mutate(change = (budget - `No-Miti`)/`No-Miti`) %>% 
  filter(model != "AIM-Hub", Y %in% c(2030, 2050, 2070), scenario == "1.5C")
p <- ggplot(pdata) +
  geom_boxplot(aes(x = DEC, y = change*100, color = Y), outlier.alpha = 0.5, outlier.size = 0.5, outlier.shape = NA, width = .7) +
  geom_vline(xintercept = 0, color = "grey70") +
  geom_hline(yintercept = 0, color = "grey70") +
  scale_color_manual(values = c("2030" = "#376888", "2050" = "#826b88", "2070" = "#de786a")) + 
  labs(y = "Percentage change in budget|1.5C|%", x = "Income deciles (1st-lowest,10th-highest)") + 
  ggthemes::theme_pander() +
  coord_cartesian(ylim = c(-10, 0)) +
  guides(color = guide_legend(title = "Year")) +
  theme(text = element_text(size = 14))

p

ggsave(p, filename = paste0(dir_out, "/fig/SI/3_change of income_msg.pdf"), 
       width = 18, height = 9, units = "cm")
  rm(p, pdata)

 # -------------------------------------------------------------------------




min(df_index$index)
max(df_index$index)

df_index_cge <- df_index %>% 
  filter(Y == "2050", 
         scenario == "1.5C", 
         model == "AIM-Hub"
  ) #%>% 
# select(c("R", "I_abb", "Index")) %>% 
# filter(!is.na(Index)) %>% 
# pivot_wider(names_from = "I_abb", values_from = "Index") 

## Figure 55 bubble charts ----
colnames(df_Exp)

df_Con %>% filter(R %in% c("JPN", "CHN"), I_abb %in% c("Energy"), Y == "2030")
df_indexi <- df_Con %>% 
  # mutate(expenditure = share * budget) %>%
  select(c("model", "R", "Y", "DEC", "I_abb",  "scenario", "value")) %>% 
  distinct() %>% 
  pivot_wider(names_from = "scenario", values_from = "value") %>% 
  pivot_longer(all_of(lis_scenario_mitigation), names_to = "scenario", values_to = "value") %>% 
  # select(c("model", "R", "Y", "DEC", "I_abb", "scenario", "No-Miti", "value")) %>% 
  mutate(DEC1 = recode(.$DEC, 
                       "1" = "low",
                       "2" = "low",
                       "3" = "low",
                       "4" = "low",
                       "7" = "high",
                       "8" = "high",
                       "9" = "high",
                       "10" = "high"
  )) %>% 
  filter(DEC1 %in% c("low", "high"), 
         Y %in% lis_y, Y != "2020") %>% 
  dplyr::group_by(model, R, Y, DEC1, I_abb, scenario) %>% 
  reframe(change1 = (sum(value)-sum(`No-Miti`))/sum(`No-Miti`) *100) %>% 
  pivot_wider(names_from = "DEC1", values_from = "change1") %>% 
  mutate(sign_low = `low`/abs(`low`), sign_high = `high`/abs(`high`)) %>% 
  filter(!is.na(low)) %>% 
  mutate(#mean = select(., `1`:`10`) %>% rowSums(na.rm = TRUE)/10,
    index = low/high,
    size = ((index-min(index)+1)/(max(index)-min(index)))) %>% 
  filter(index != max(index)) 


## Figure 551 bubble charts for EVi ----
pdata <- df_EV %>% select("R", "Y" ,"model","scenario" ,"I_abb", "DEC" ,"EVi") %>% 
  pivot_wider(values_from = "EVi", names_from = "DEC") %>% 
  left_join(Map_r_PHI2Hub) %>% 
  filter(scenario != "No-Miti", `1` != max(abs(`1`)),`10` != max(abs(`10`)), Y != "2020"
         ) %>% F_reve_mutate() %>% F_R_CGEfull()

lis_model <- c("AIM-Hub", "MESSAGEix")
lis_revenue <- unique(pdata$revenue) %>% as.character()
for(m in 1:length(lis_model)){
  for(rev in 1:length(lis_revenue)){
# rev <- 1  
p552 <- ggplot(pdata %>% filter(`1` > -1, revenue == lis_revenue[rev], model == lis_model[m])) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dashed")+
  geom_vline(xintercept = 0, color = "grey70", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "grey90") +
  geom_point(mapping = aes(x = `1`*100, y = `10`*100, 
                           shape = scenario_Name, #alpha = scenario,
                           color = R_CGE), alpha = 0.6, size = 1.7) +
  scale_y_continuous(guide = guide_axis(position = "left")) +
  scale_size(range = c(1, 10), name = "low/high") +
  scale_color_manual(values = palette_R_CGE) +
  labs(x = "Change in consumption|1st-decile|%", y = "Change in consumption|10th-decile|%", subtitle = paste0(lis_revenue[rev]," | ", lis_model[m])) +
  guides(size = "none", color = guide_legend(title = "Region", ncol = 1), shape = guide_legend(title = "Scenario"), 
         alpha = guide_legend(title = "Scenario")) +
  facet_wrap(I_abb ~ ., ncol = 5) +
  MyTheme + theme(legend.key.size = unit(0.5, 'cm'), 
                  legend.key.height = unit(0.5, 'cm'),
                  legend.key.width = unit(0.5, 'cm'), 
                  legend.title = element_text(size=10),
                  legend.text = element_text(size=9),
                  panel.spacing.x = unit(1.1,"lines"))

p552

if(lis_model[m] == "AIM-Hub"){
  m_m <- "AIMHub"
}else(m_m <- lis_model[m])

ggsave(p552, filename = paste0(dir_out,"/fig/SI/3_EVi",lis_revenue[rev],"_", m_m,".pdf"), 
       width = 26, height = 12, units = "cm")

       rm(p552)
  }
}
rm(pdata)



 ### model comparison ----
pdata <- df_EV %>% select("R", "Y" ,"model","scenario" ,"I_abb", "DEC" ,"EVi") %>% 
  pivot_wider(values_from = "EVi", names_from = "DEC") %>% 
  left_join(Map_r_PHI2Hub) %>% 
  filter(scenario != "No-Miti", `1` != max(abs(`1`)),`10` != max(abs(`10`)), Y != "2020"
  ) %>% F_reve_mutate() %>% F_R_CGEfull()

lis_revenue <- unique(pdata$revenue) %>% as.character()
for(rev in 1:length(lis_revenue)){
  p552 <- ggplot(pdata %>% filter(`1` > -1, revenue == lis_revenue[rev])) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed")+
    geom_vline(xintercept = 0, color = "grey70", linetype = "dashed") +
    geom_abline(slope = 1, intercept = 0, color = "grey90") +
    geom_point(mapping = aes(x = `1`, y = `10`, #size = abs(`1`/`10`),
                             shape = model, 
                             color = R_CGE), alpha = 0.5, size = 1.5) +
    scale_y_continuous(guide = guide_axis(position = "left")) +
    scale_size(range = c(1, 10), name = "low/high") +
    scale_color_manual(values = palette_R_CGE) +
    labs(x = "Change in consumption|1st-decile|%", y = "Change in consumption|10th-decile|%", 
         subtitle = paste0(lis_revenue[rev])) +
    guides(size = "none", color = guide_legend(title = "Region", ncol = 1), shape = guide_legend(title = "Model"), 
           alpha = guide_legend(title = "Model")) +
    facet_wrap(I_abb ~ ., ncol = 5) +
    MyTheme
  p552
  
  ggsave(p552, filename = paste0(dir_out,"/fig/SI/3_EVi",lis_revenue[rev],"_model.pdf"), 
         width = 28, height = 14, units = "cm")

        rm(p552)
}

rm(pdata)
#


rm(df_budget,m_cge, m_msg,df_Exp,df_ExpShare, df_Con, df_PQ, df_EV, df_index, df_indexi, df_index_cge)
print("The END of main/3_DecileDataAnalysis.R")
