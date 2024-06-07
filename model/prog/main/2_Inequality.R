# This Rscript is for YSSP final report
# This is the inequality assessment part

# Shiya ZHAO, 2023/07/17

## Loading packages ----
require(gdxrrw)
require(ggplot2)
require(tidyverse)
require(cowplot)
require(gdata)
require(grid)
require(openxlsx)
require(ggpubr)


Gini_msg <- rgdx.param(AnaExpmsg, "Gini_exp") %>%
  mutate(model = "MESSAGEix",
         Gini_exp = Gini_exp*100) %>% 
  gdata::rename.vars(from = "Ref", to ="PHIscenario") %>% 
  left_join(MapScenario) %>% 
  full_join(Map_r_PHI2Hub) %>% 
  filter(!is.na(scenario))  %>% 
  mutate(R_CGE = factor(R_CGE, levels = lis_R_CGE))

Gini_cge <- rgdx.param(AnaExp, "Gini_exp") %>%
  mutate(model = "AIM-Hub",
         Gini_exp = Gini_exp*100) %>% 
  gdata::rename.vars(from = "Ref", to ="PHIscenario") %>% 
  left_join(MapScenario) %>% 
  full_join(Map_r_PHI2Hub) %>% 
  filter(!is.na(scenario))  %>% 
  mutate(R_CGE = factor(R_CGE, levels = lis_R_CGE))


Gini <- Gini_msg %>% 
  rbind(Gini_cge) %>% 
  select("model", "scenario", "R", "R_CGE", "Y", "Gini_exp") %>% 
  F_reve_mutate()
openxlsx::write.xlsx(Gini, file = paste0(dir_csv, "Gini.xlsx"))

rm(Gini_msg, Gini_cge)

Gini %>% 
  dplyr::group_by(model, R_CGE, Y, scenario) %>% 
  dplyr::reframe(median = median(Gini_exp)) %>% 
  F_reve_mutate() %>% 
  openxlsx::write.xlsx(file = paste0(dir_csv, "Gini_R17_median.xlsx"))


# 1.1 Gini: Line plot baseline map ----

pdata0 <- Gini %>% 
  filter(Y %in% c("2030")) %>%
  select(-scenario_Name, -revenue) %>% 
  pivot_wider(names_from = "scenario", values_from = "Gini_exp") %>% 
  pivot_longer(c("2C", "1.5C","2C_EPC", "1.5C_EPC"), names_to = "scenario", values_to = "value") %>% 
  mutate(value = `value`-`No-Miti`, type_variable = "Additional") %>% 
  select(c("model", "R", "R_CGE", "Y", "scenario", "value")) %>% 
  filter(scenario == "1.5C", !(R %in% c("HKG", "MAC", "TWN")) ) %>% 
  dplyr::group_by(model, scenario, Y) %>% 
  arrange(scenario, Y, desc(value))
pdata0 <- pdata0[-c(11:nrow(pdata0)),]
lis_R_Gini <- unique(pdata0$R) 
lis_R_Gini <- lis_R_Gini[!lis_R_Gini =="HKG"]
rm(pdata0)



# 1.3 Gini change: Box plot ----------------------------------------------------------------
pdata <- Gini %>% left_join(Gini %>% filter(scenario == "No-Miti") %>% select(-c("scenario", "revenue", "scenario_Name")) %>% dplyr::rename("Gini_exp_BaU" = "Gini_exp")) %>% 
  mutate(change = Gini_exp-Gini_exp_BaU) %>% 
  filter(Y %in% c(2030, 2050)) %>% left_join(Map_r_PHI2Hub) %>% mutate(R_CGE = case_when(R == "IND" ~ "IND", R != "IND" ~ R_CGE)) %>% 
　F_R_CGEfull()

pdata %>% write.xlsx(paste0(dir_out, "/csv/2_Gini_change_box.xlsx"))


lis_revenue <- unique(pdata$revenue) %>% as.character()
for(rev in 1:length(lis_revenue)){
p_211 <- ggplot() +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_boxplot(data = pdata %>% filter(scenario != "No-Miti", revenue == lis_revenue[rev]) %>% filter(model == "AIM-Hub"),
               aes(x = Y, y = change, group = paste0(Y, scenario, model), color = scenario_Name)) + # , alpha=scenario1
  scale_color_manual(values = palette_Sc) +
  labs(
    x = "Year",
    y = "Gini coefficient | %point",
    subtitle = lis_revenue[rev]
  )+
  facet_wrap(~ R_CGE, ncol = 6) + # model
  guides(color = guide_legend(title = "Scenario")) + # , linetype = guide_legend("Revenue")
  MyTheme 
p_211

ggsave(p_211, filename = paste0(dir_out, "/fig/SI/2_box_plot_model_Gini coefficient change_", lis_revenue[rev],".pdf"),
       width = 32, height = 12, units = "cm")

rm(p_211)
}



## AIMHub maintext ----
pdata <- Gini %>% left_join(Gini %>% filter(scenario == "No-Miti") %>% select(-c("scenario", "revenue", "scenario_Name")) %>% dplyr::rename("Gini_exp_BaU" = "Gini_exp")) %>% 
  mutate(change = Gini_exp-Gini_exp_BaU) %>% 
  filter(Y %in% c(2030, 2050, 2070)) %>% left_join(Map_r_PHI2Hub) %>% mutate(R_CGE = case_when(R == "IND" ~ "IND", R != "IND" ~ R_CGE)) %>% 
  F_R_CGEfull()
pdata %>% dplyr::group_by(model, scenario,Y) %>% dplyr::reframe(median = median(change)) %>% write.xlsx(paste0(dir_out, "/csv/2_Gini_change_box_median.xlsx"))

p_211 <- ggplot() +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_boxplot(data = pdata %>% filter(scenario != "No-Miti") %>% filter(model == "AIM-Hub"),
               aes(x = Y, y = change, group = paste0(Y, scenario, model), color = scenario_Name), outlier.shape = NA, outliers = F) + # , alpha=scenario1
  scale_color_manual(values = palette_Sc) +
  labs(
    x = "Year",
    y = "Change in Gini | %point"
  )+
  facet_wrap(~ revenue, scales = "free", ncol = 1) + # model
  guides(color = guide_legend(title = "Scenario")) + # , linetype = guide_legend("Revenue")
  MyTheme 
p_211

ggsave(p_211, filename = paste0(dir_out, "/fig/2_box_plot_model_Gini coefficient change_all_AIMHub.pdf"),
       width = 9, height = 9, units = "cm")
rm(p_211, pdata)



### model compare ----
pdata <- Gini %>% left_join(Gini %>% filter(scenario == "No-Miti") %>% select(-c("scenario", "revenue", "scenario_Name")) %>% dplyr::rename("Gini_exp_BaU" = "Gini_exp")) %>% 
  mutate(change = Gini_exp-Gini_exp_BaU) %>% 
  filter(Y %in% c(2030, 2050, 2070)) %>% left_join(Map_r_PHI2Hub) %>% mutate(R_CGE = case_when(R == "IND" ~ "IND", R != "IND" ~ R_CGE)) %>% 
  F_R_CGEfull()
pdata %>% dplyr::group_by(model, scenario,Y) %>% dplyr::reframe(median = median(change)) %>% write.xlsx(paste0(dir_out, "/csv/2_Gini_change_box_median.xlsx"))


#### point plot ----
pdata1 <- pdata %>% filter(scenario != "No-Miti") %>% 
  filter(abs(change) > 0.1) %>% 
  select("model", "scenario","R", "R_CGE", "Y", "scenario_Name", "revenue", "change" ) %>% 
  pivot_wider(names_from = "model", values_from = "change")

p_211 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "grey80") +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_point(data = pdata1,
             aes(x = `AIM-Hub`, y = `MESSAGEix`, color = R_CGE, shape = Y, size = Y), alpha = 0.6) + 
  scale_color_manual(values = palette_R_CGE) +
  scale_size_manual(values = c("2030" = 2, "2050" = 1)) +
  labs(
    x = "Change of Gini in AIM-Hub | % point",
    y = "Change of Gini in MESSAGEix | % point"
  )+
  facet_wrap(~ revenue, scales = "free") + # model
  guides(color = guide_legend(title = "Scenario", ncol = 2), shape = guide_legend(title = "Year"), size = guide_legend(title = "Year")) + # , linetype = guide_legend("Revenue")
  MyTheme# +
  # theme(legend.key.size = unit(0.2, "cm"))
p_211

ggsave(p_211, filename = paste0(dir_out, "/fig/SI/2_box_plot_model_Gini coefficient change_all_model.pdf"),
       width = 24, height = 9, units = "cm")
rm(p_211, pdata)



# 2. Decile_Expenditure ---------------------------------------------------
# 2.0 load data ----
Exp_dec_cge <- rgdx.param(paste0("../../output/gdx/Decile/AIMHub.gdx"), "Budget") %>% mutate(model = "AIM-Hub")
Exp_dec_msg <- rgdx.param(paste0("../../output/gdx/Decile/MESSAGEix.gdx"), "Budget") %>% mutate(model = "MESSAGEix")
Exp_dec <- Exp_dec_msg %>% rbind(Exp_dec_cge) %>% dplyr::rename("PHIscenario" = "Ref") %>% left_join(MapScenario) %>% 
  select("model", "scenario", "R", "Y", "DEC","Budget") %>% 
  F_reve_mutate() %>% 
  mutate(DEC = gsub("D_","",DEC)) %>% 
  mutate(DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character(), "ALL")))


rm(Exp_dec_cge,Exp_dec_msg)


## 2.1 decile budget change ----
Exp_dec_BaU <- Exp_dec %>% filter(scenario == "No-Miti") %>% select(-scenario, -revenue, -scenario_Name) %>% dplyr::rename("Budget_BaU" = "Budget")
pdata <- Exp_dec %>% left_join(Exp_dec_BaU) %>% mutate(change = (Budget-Budget_BaU)/Budget_BaU) %>% 
  left_join(Map_r_PHI2Hub) 



### model compare ----
p <- ggplot(pdata %>% filter(DEC != "ALL", Y %in% c("2030", "2050"), scenario_Name == "1.5C", change < 1)) +
  geom_hline(yintercept = 0, color = "grey80") + 
  geom_boxplot(aes(x = DEC, y = change * 100, color = model, linetype = Y, group = paste0(Y, model, DEC)), outlier.shape = NA) +
  MyTheme +
  labs(x = "Decile", y = "Change in average income | %") +
  facet_wrap(revenue~., scales = "free") +
  scale_color_manual(values = palette_model) +
  guides(color = guide_legend(title = "Scenario"))
p

ggsave(p, filename = paste0("2_Expenditure_change_model.pdf"), path = paste0(dir_out, "/fig/SI/"),
       width = 26, height = 8, units = "cm")
rm(p, pdata)




# 2.2 Top-bottom index ----------------------------------------------------
pdata <- Exp_dec %>% left_join(Exp_dec_BaU) %>% mutate(change = (Budget-Budget_BaU)/Budget_BaU) %>% 
  left_join(Map_r_PHI2Hub) %>% select(-Budget_BaU, -Budget) %>% filter(scenario != "No-Miti", Y %in% c(seq(2030, 2070,10))) %>% pivot_wider(names_from = "DEC", values_from = "change") %>% 
  F_R_CGEfull()
  # dplyr::group_by(model, scenario, revenue, scenario_Name, R_CGE, Y, DEC) %>% 
  # dplyr::reframe(change = mean(change))
lis_model <- c("AIM-Hub", "MESSAGEix")
lis_revenue <- unique(pdata$revenue) %>% as.character()
for(m in lis_model){
  # for(rev in lis_revenue){
    # m <- "AIM-Hub"
    # rev <- "Neutral"
  p <- ggplot(pdata %>% filter( model == m)) + # revenue == rev,
    geom_point(aes(x = `1`*100, y = `10` * 100, size = `Y`, color = R_CGE, shape = scenario_Name), alpha = 0.6) +
    geom_hline(yintercept = 0, color = "grey80") +
    geom_vline(xintercept = 0, color = "grey80") +
    geom_abline(intercept = 0, slope = 1, color = "grey50", linetype = 2) +
    # geom_abline(intercept = 0, slope = -1, color = "grey50", linetype = 2) +
    labs(x = "Change in the lowest decile | %", y = "Change in the highest decile | %", subtitle = m) + 
    MyTheme + theme(legend.key.size = unit(0.42, 'cm'), 
                   legend.key.height = unit(0.42, 'cm'),
                   legend.key.width = unit(0.42, 'cm'), 
                   legend.title = element_text(size=10),
                   legend.text = element_text(size=9)) +
    guides(color = guide_legend(title = "Region", ncol = 2), 
           size = guide_legend(title = "Year"), 
           shape = guide_legend(title = "Scenario")) +
    scale_color_manual(values = palette_R_CGE) +
    scale_size_manual(values = c("2030" = 2, "2050" = 1.5, "2070" = 1)) +
    facet_wrap(~revenue, scales = "free")

  p
  
  if(m == "AIM-Hub"){m_m <- "AIMHub"}else{m_m <- m}
  
  ggsave(p, filename = paste0("2_Expenditure_change_highlow_", m_m, ".pdf"), path = paste0(dir_out, "/fig/"),
       width = 20, height = 7, units = "cm")
  

# }
}

p_legend <- get_legend(p) %>% as_ggplot()

ggsave(p_legend, filename = paste0("2_Expenditure_change_highlow_legend.pdf"), 
       path = paste0(dir_out, "/fig/"),
       width = 8, height = 10, units = "cm")

rm(p, p_legend, Gini, Exp_dec, Exp_dec_BaU, pdata, pdata1)



print("The END of main/2_Inequality.R")

