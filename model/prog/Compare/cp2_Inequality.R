# This Rscript is for YSSP final report
# This is the inequality assessment part

# This program is a summary of all the figures and plots in the paper 
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with Expenditure and its change

# Shiya ZHAO, 2021/06/17

## Loading packages ----
require(gdxrrw)
require(ggplot2)
require(tidyverse)
require(cowplot)
require(gdata)
require(grid)
require(openxlsx)
require(ggpubr)


F_change <- function(x){
  
  y <- x %>% mutate(scenario = paste0(target, "_", revenue)) %>% 
    select(-Ref, -target, -revenue) %>% 
    pivot_wider(names_from = "scenario", values_from = "Gini_exp") %>% 
    pivot_longer(c("2C_Neutral", "1.5C_Neutral","2C_EPC", "1.5C_EPC"), names_to = "scenario", values_to = "value") %>% 
    mutate(value = `value`-`No-Miti_Neutral`, type_variable = "Additional") %>% 
    select(c("model", "R", "R_CGE", "Y", "scenario", "gini", "value")) %>% 
    mutate(target = str_split_i(scenario, "_", 1),
           revenue = str_split_i(scenario, "_", 2)) 
  return(y)
}

# Gini_msg <- rgdx.param(AnaExpmsg, "Gini_exp") %>%
#   mutate(model = "MESSAGEix",
#          Gini_exp = Gini_exp*100) %>% 
#   # gdata::rename.vars(from = "Ref", to ="PHIscenario") %>% 
#   left_join(MapScenario) %>% 
#   full_join(Map_r_PHI2Hub) %>% 
#   filter(!is.na(Ref))  %>% 
#   mutate(R_CGE = factor(R_CGE, levels = lis_R_CGE))
# 
# Gini_cge <- rgdx.param(AnaExp, "Gini_exp") %>%
#   mutate(model = "AIM-Hub",
#          Gini_exp = Gini_exp*100) %>% 
#   # gdata::rename.vars(from = "Ref", to ="PHIscenario") %>% 
#   left_join(MapScenario) %>% 
#   full_join(Map_r_PHI2Hub) %>% 
#   filter(!is.na(Ref))  %>% 
#   mutate(R_CGE = factor(R_CGE, levels = lis_R_CGE))
# 
# Gini <- Gini_msg %>% 
#   rbind(Gini_cge) %>% 
#   select("model", "Ref", "R", "Y", "Gini_exp") %>% left_join(Map_r_PHI2Hub) %>% 
#   F_reve_mutate()


# update Gini with discrete calculation
sw_gini_disc <- F
Gini_tmp <- data.frame()
Gini <- data.frame()
for(m in seq_along(lis_model)){
  if(lis_model[m] == "AIM-Hub"){
    txt_m <- "AIMHub"
  }else{
    txt_m <- lis_model[m]
  }
  
  if(sw_gini_disc){
    gdx_gini <- paste0("../output/gdx/ConsumptionResults/Gini_",txt_m, ".gdx")
    Gini_tmp <- rgdx.param(gdx_gini, "Gini_disc") %>%
      mutate(model = lis_model[m],
             Gini_exp = Gini_disc*100) %>% select(-Gini_disc) %>%  
      # gdata::rename.vars(from = "Ref", to ="PHIscenario") %>% 
      left_join(MapScenario) %>% 
      full_join(Map_r_PHI2Hub) %>% 
      filter(!is.na(Ref))  %>%  F_reve_mutate() %>% 
      mutate(R_CGE = factor(R_CGE, levels = lis_R_CGE))
  }else{
    gdx_gini <- paste0("../output/gdx/ConsumptionResults/AnalysisExpenditure_",txt_m, ".gdx")
    Gini_tmp <- rgdx.param(gdx_gini, "Gini_exp") %>%
      mutate(model = lis_model[m],
             Gini_exp = Gini_exp*100) %>%  
      left_join(MapScenario) %>% 
      full_join(Map_r_PHI2Hub) %>% 
      filter(!is.na(Ref))  %>%  F_reve_mutate() %>% 
      mutate(R_CGE = factor(R_CGE, levels = lis_R_CGE))
    
  }  
  Gini <- Gini %>% rbind(Gini_tmp)
  
  rm(Gini_tmp)
}

colnames(Gini)
Gini <- Gini %>% select(-MSGscenario, -CGEscenario)

openxlsx::write.xlsx(Gini, file = paste0(dir_csv, "Gini.xlsx"))


Gini %>% 
  dplyr::group_by(model, R_CGE, Y, Ref) %>% 
  dplyr::reframe(median = median(Gini_exp)) %>% 
  F_reve_mutate() %>%
  openxlsx::write.xlsx(file = paste0(dir_csv, "Gini_R17_median.xlsx"))


# 1.1 Gini: Line plot baseline map ----
colnames(Gini)
pdata0 <- Gini %>% 
  filter(Y %in% c("2030"), gini == "consistent") %>%
  F_change() %>% 
  filter(grepl("1.5C",scenario)) # !(R %in% c("HKG", "MAC", "TWN")) 
colnames(pdata0)
pdata0 <- pdata0[-c(11:nrow(pdata0)),]
lis_R_Gini <- unique(pdata0$R) 
# lis_R_Gini <- lis_R_Gini[!lis_R_Gini =="HKG"]
rm(pdata0)


# plot
pdata <- Gini %>% filter(Y %in% seq(2020, 2100,10),
                         R %in% lis_R_Gini) %>% 
  F_change() %>% select(-scenario) %>% 
  mutate(v_color = target, v_group = paste0(target, "_", model))

p_21 <- F_plot_ribbon(pdata, txt_y = "(point)") + geom_hline(yintercept = 0, color = "grey70")+facet_grid(revenue~R) +
  scale_color_manual(values = palette_Sc) +
  scale_fill_manual(values = palette_Sc) +
  guides(color = guide_legend(title = "Scenario"), fill = guide_legend(title = "Scenario"), 
         linetype = guide_legend(title = "Model"), shape = guide_legend(title = "Model"))  +
  theme(axis.text.x = element_text(angle = 60))
p_21
ggsave(p_21, filename = paste0(dir_output_YSSPpaper, "/fig/SI/2_Gini coefficient change.pdf"),
       width = 28, height = 12, units = "cm")

rm(p_21)



# 1.2 Gini: Box plot ----------------------------------------------------------------
pdata <- Gini %>% filter(Y %in% c(2030, 2040, 2050, 2070), target %in% c("1.5C", "No-Miti")) %>% #filter(model == "AIM-Hub") %>% 
  left_join(Map_r_PHI2Hub) %>% mutate(R_CGE = case_when(R == "IND" ~ "IND", R != "IND" ~ R_CGE)) %>%
  F_R_CGEfull()

p_211 <- ggplot() +
  geom_boxplot(data = pdata,
            aes(x = Y, y = Gini_exp, group = paste0(Y, target, revenue, model), color = target, linetype = revenue)) +
  scale_color_manual(values = palette_Sc) +
  # scale_linetype_manual(values = c("AIM-Hub" = "longdash", "MESSAGEix" = "solid")) +
  labs(
    x = "Year",
    y = "Gini (point)"
  )+
  facet_wrap(~ R_CGE, ncol = 6) + # model
  # MyTheme +
  guides(color = guide_legend(title = "Scenario"), linetype = guide_legend("Revenue")) +
  MyTheme +
  theme(axis.line = element_line(color = "grey50")) 
p_211

ggsave(p_211, filename = paste0(dir_output_YSSPpaper, "/fig/SI/2_box_plot_2model_Gini coefficient_15C.pdf"),
       width = 34, height = 15, units = "cm")

rm(p_211, pdata)



# 1.3 Gini change: Box plot ----------------------------------------------------------------

pdata <- Gini %>% left_join(Gini %>% filter(target == "No-Miti") %>% select(-c("Ref", "revenue", "target")) %>% dplyr::rename("Gini_exp_BaU" = "Gini_exp")) %>% 
  mutate(change = Gini_exp-Gini_exp_BaU) %>% 
  filter(Y %in% c(2030, 2040, 2050, 2070)) %>% left_join(Map_r_PHI2Hub) %>% mutate(R_CGE = case_when(R == "IND" ~ "IND", R != "IND" ~ R_CGE)) %>% 
  F_R_CGEfull()

pdata %>% write.xlsx(paste0(dir_output_YSSPpaper, "/csv/2_Gini_change_box.xlsx"))

lis_revenue <- unique(pdata$revenue) %>% as.character()
for(rev in 1:length(lis_revenue)){
p_211 <- ggplot() +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_boxplot(data = pdata %>% filter(target != "No-Miti", revenue == lis_revenue[rev]),
               aes(x = paste0(Y), y = change, group = paste0(Y, target, revenue, model), color = target, linetype = model), outliers = FALSE) + # , alpha=scenario1
  scale_color_manual(values = palette_Sc) +
  labs(
    x = "Year",
    y = "Change in Gini (point)",
    subtitle = lis_revenue[rev]
  )+
  facet_wrap(~ R_CGE, ncol = 6) + # model
  guides(color = guide_legend(title = "Scenario",linetype = "Model")) + # , linetype = guide_legend("Revenue")
  MyTheme 
p_211

ggsave(p_211, filename = paste0(dir_output_YSSPpaper, "/fig/SI/2_box_plot_model_Gini coefficient change_", lis_revenue[rev],".pdf"),
       width = 32, height = 12, units = "cm")

rm(p_211)
}

p_211 <- ggplot() +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_boxplot(data = pdata %>% filter(target != "No-Miti") %>% mutate(dummy = "A"), # %>% filter(model == "AIM-Hub")
               aes(x = Y, y = change, group = paste0(Y, target, revenue, model), color = target, linetype = model), 
               outlier.alpha = .2, outlier.size = .2, width = .6, position = position_dodge(width = .8), outliers = F) + # , alpha=scenario1
  scale_color_manual(values = palette_Sc) +
  # scale_linetype_manual(values = palette_lin)
  labs(
    x = "Year",
    y = "Change in Gini (point)"
  )+
  facet_wrap(~revenue, scales = "free_y", ncol = 1) + # model
  guides(color = guide_legend(title = "Scenario"), linetype = guide_legend("Model")) + # , linetype = guide_legend("Revenue")
  MyTheme 
p_211

ggsave(p_211, filename = paste0(dir_output_YSSPpaper, "/fig/SI/2_box_plot_model_Gini coefficient change_all_",sw_gini_disc,".pdf"),
       width = 10, height = 10, units = "cm")
rm(p_211)


### model compare ----

#### boxplot -----
#### point plot ----
pdata1 <- pdata %>% filter(target != "No-Miti") %>% 
  filter(abs(change) > 0.1) %>% 
  select("model", "target", "revenue", "gini","R", "R_CGE", "Y", "change") %>% 
  pivot_wider(names_from = "model", values_from = "change")

p_211 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "grey80", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(data = pdata1,
             aes(x = `AIM-Hub`, y = `MESSAGEix`, color = R_CGE, shape = Y, size = target), alpha = 0.5) + 
  scale_color_manual(values = palette_R_CGE) +
  scale_size_manual(values = c("1.5C" = 2, "2C" = 1)) +
  # scale_size_manual(values = c("2030" = 2, "2050" = 1)) +
  labs(
    x = "Change of Gini in AIM-Hub (point)",
    y = "Change of Gini in MESSAGEix (point)"
  )+
  facet_wrap(~ revenue, scales = "free") + # model
  guides(color = guide_legend(title = "Scenario", ncol = 2), linetype = "Model", shape = guide_legend(title = "Year"), size = guide_legend(title = "Year")) + # , linetype = guide_legend("Revenue")
  MyTheme + theme(legend.position = "none", axis.text.x = element_text(hjust = .8))
p_211

ggsave(p_211, filename = paste0(dir_output_YSSPpaper, "/fig/SI/2_box_plot_model_Gini coefficient change_all.pdf"),
       width = 16, height = 8, units = "cm")

rm(p_211, pdata)



# 2. Decile_Expenditure ---------------------------------------------------
# 2.0 load data ----
Exp_dec_cge <- rgdx.param(paste0("../output/gdx/Decile_AIMHub.gdx"), "output_Budget") %>% gdata::rename.vars(colnames(.), c("R", "R3", "Ref", "DEC", "Y", "Budget")) %>% select(-R3) %>% mutate(model = "AIM-Hub")
Exp_dec_msg <- rgdx.param(paste0("../output/gdx/Decile_MESSAGEix.gdx"), "output_Budget") %>% gdata::rename.vars(colnames(.), c("R", "R3", "Ref", "DEC", "Y", "Budget")) %>% select(-R3)%>% mutate(model = "MESSAGEix") 
Exp_dec <- Exp_dec_msg %>% rbind(Exp_dec_cge) %>% # left_join(MapScenario_main) %>% 
  select("model", "Ref", "R", "Y", "DEC","Budget") %>% 
  F_reve_mutate() %>% # F_sce_main() %>% 
  mutate(DEC = gsub("D_","",DEC)) %>% 
  mutate(DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character(), "ALL")))


rm(Exp_dec_cge,Exp_dec_msg)


## 2.1 decile budget change ----
Exp_dec_BaU <- Exp_dec %>% filter(target == "No-Miti") %>% select(-Ref, -revenue, -target) %>% dplyr::rename("Budget_BaU" = "Budget")
pdata <- Exp_dec %>% left_join(Exp_dec_BaU) %>% mutate(change = (Budget-Budget_BaU)/Budget_BaU) %>% 
  left_join(Map_r_PHI2Hub) 

### Decile: boxplot ----
lis_y_for <- c(seq(2030, 2070,10))
lis_revenue <- unique(pdata$revenue) %>% as.character()
# m <- "AIM-Hub"
# y <- 1
# rev <- 1
for(m in lis_model){
  for(y in 1:length(lis_y_for)){
  for(rev in 1:length(lis_revenue)){
    p <- ggplot(pdata %>% filter(DEC != "ALL", revenue == lis_revenue[rev], Y == lis_y_for[y], target != "No-Miti", 
                                 model == m, change < 1)) +
      geom_hline(yintercept = 0, color = "grey80") + 
      geom_boxplot(aes(x = DEC, y = change * 100, color = target), outlier.size = 0.3, outlier.alpha = .5, outliers = FALSE) +
      MyTheme +
      labs(x = "Decile", y = "Change in average income (%)", subtitle = paste0(lis_y_for[y], " | ", lis_revenue[rev], " | ",m)) +
      facet_wrap(~R_CGE, ncol = 5) +
      scale_color_manual(values = palette_Sc) +
      guides(color = guide_legend(title = "Scenario"))
    p
    if(m == "AIM-Hub"){
    ggsave(p, filename = paste0("2_Expenditure_change_",lis_y_for[y], "_", lis_revenue[rev],"_AIMHub.pdf"), path = paste0(dir_output_YSSPpaper, "/fig/SI/"),
           width = 28, height = 18, units = "cm")}else{
    ggsave(p, filename = paste0("2_Expenditure_change_",lis_y_for[y], "_", lis_revenue[rev],"_", m, ".pdf"), path = paste0(dir_output_YSSPpaper, "/fig/SI/"),
           width = 28, height = 18, units = "cm")
           }
    
    rm(p)
}
}
}

### model compare ----
# 1.5C SI
p <- ggplot(pdata %>% filter(DEC != "ALL", Y %in% c("2030", "2050"), target %in% c("1.5C"), change < 1)) +
  geom_hline(yintercept = 0, color = "grey80") + 
  geom_boxplot(aes(x = DEC, y = change * 100, color = model, linetype = Y, group = paste0(Y, model, DEC)), outliers = F) +
  MyTheme +
  labs(x = "Decile", y = "Change in total consumption (%)", color = "Scenario", linetype = "Year") +
  facet_wrap(~revenue, scales = "free", ncol = 1) +
  scale_color_manual(values = palette_model) 
p

ggsave(p, filename = paste0("2_Expenditure_change_model_20302050_1p5.pdf"), path = paste0(dir_output_YSSPpaper, "/fig/SI/"),
       width = 12, height = 10, units = "cm")

 
 
 # 1.5C and 2C main
 a <- pdata %>% filter(DEC != "ALL", Y %in% c("2030"), target %in% c("1.5C", "2C"))
 p <- ggplot(pdata %>% filter(DEC != "ALL", Y %in% c("2030"), target %in% c("1.5C", "2C"))) +
   geom_hline(yintercept = 0, color = "grey80") + 
   geom_boxplot(aes(x = DEC, y = change * 100, color = target, linetype = model, group = paste0(target, model, DEC)), outliers = F) +
   MyTheme +
   labs(x = "Decile", y = "Change in total consumption (%)", color = "Target", linetype = "Model") +
   facet_wrap(~revenue, scales = "free_y", ncol = 1) +
   scale_color_manual(values = palette_Sc) 
 p
 
 ggsave(p, filename = paste0("2_Expenditure_change_model_2030_2C1p5.pdf"), path = paste0(dir_output_YSSPpaper, "/fig/"),
        width = 12, height = 10, units = "cm")
 rm(p, pdata)

### Decile: Average ----
 
Exp_dec_change <- Exp_dec %>% left_join(Exp_dec_BaU) %>% mutate(change = (Budget-Budget_BaU)/Budget_BaU) 

pdata <- Exp_dec_change %>% 
  left_join(Map_r_PHI2Hub) %>% dplyr::group_by(model, Ref, revenue, target, gini, R_CGE, Y, DEC) %>% 
  dplyr::reframe(change = median(change)) %>%
   filter(DEC != "ALL", target != "No-Miti") %>% 
  F_R_CGEfull()

lis_y_for <- c(seq(2030, 2070,20))
lis_revenue <- unique(pdata$revenue) %>% as.character()

for(m in lis_model){
  for(y in lis_y_for){
    # m <- "AIM-Hub"
    # y <- 2030
    # rev <- "Neutral"
    p <- ggplot(pdata %>% filter(Y == y,  model == m)) + # , revenue == rev
      geom_hline(yintercept = 0, color = "grey80") + 
      geom_line(aes(x = DEC, y = change * 100, color = R_CGE, group = paste0(Ref, R_CGE))) +
      MyTheme +
      labs(x = "Decile", y = "Change in average income (%)", subtitle = paste0(y, " | ", m)) +
      facet_grid(revenue~target) +
      scale_color_manual(values = palette_R_CGE) +
      guides(color = guide_legend(title = "Region", ncol = 1))
    p
    
    if(m == "AIM-Hub"){
      ggsave(p, filename = paste0("2_Expenditure_change_average_",y, "_AIMHub.pdf"), path = paste0(dir_output_YSSPpaper, "/fig/SI/"),
             width = 16, height = 12, units = "cm")}else{
      ggsave(p, filename = paste0("2_Expenditure_change_average_",y, "_", m, ".pdf"), path = paste0(dir_output_YSSPpaper, "/fig/SI/"),
             width = 16, height = 12, units = "cm")
             }
    
    rm(p)
  }
}



# 2.2 Top-bottom index ----------------------------------------------------
## top bottom index and plot
map_dec_tb <- data.frame(DEC = c(seq(1,4,1), seq(7,10,1)) %>% as.character(), TB = c(rep("Bottom", 4), rep("Top", 4)))
Exp_dec_change_topbottom <- Exp_dec %>% left_join(Exp_dec_BaU) %>% 
  left_join(map_dec_tb) %>% filter(!is.na(TB))%>% dplyr::group_by(model, Ref, R, Y, revenue, target, gini, TB) %>% 
  reframe(change = (sum(Budget)-sum(Budget_BaU))/sum(Budget_BaU)) %>% pivot_wider(values_from = "change", names_from = "TB")
rm(map_dec_tb)

# both models
pdata <- Exp_dec_change_topbottom %>% filter(Y %in% c(seq(2030, 2070,10)))

p <- ggplot(pdata %>% filter( target == "1.5C") %>% F_gini_rename()) + # revenue == rev,
  geom_point(aes(x = `Bottom`*100, y = `Top` * 100, size = `Y`, color = model, shape = model, alpha = gini)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_abline(intercept = 0, slope = 1, color = "grey50", linetype = 2) +
  labs(x = "Change in the bottom four deciles (%)", y = "Change in the top four decile (%)", color = "Model", size = "Year", shape = "Model", alpha = "SSP") + 
  MyTheme + theme(legend.key.size = unit(0.42, 'cm'), 
                  legend.key.height = unit(0.42, 'cm'),
                  legend.key.width = unit(0.42, 'cm'), 
                  legend.spacing = unit(.1, "cm"),
                  legend.title = element_text(size=10),
                  legend.text = element_text(size=9)) +
  scale_color_manual(values = palette_model) +
  scale_shape_manual(values = palette_shape_model) +
  scale_alpha_manual(values = palette_alpha_gini_all) +
  scale_size_manual(values = c("2030" = 2, "2040" = 1.6, "2050" = 1.3, "2060" = 1, "2070" = .7)) +
  facet_wrap(~revenue, scales = "free")

p



## 2.2.1. 1st and 10th deciles ---------------
pdata <- Exp_dec_change %>% 
  left_join(Map_r_PHI2Hub) %>% select(-Budget_BaU, -Budget) %>% 
  filter(target != "No-Miti", Y %in% c(seq(2030, 2070,10))) %>% pivot_wider(names_from = "DEC", values_from = "change") %>% 
  F_R_CGEfull()


lis_revenue <- unique(pdata$revenue) %>% as.character()
for(m in lis_model){
  # for(rev in lis_revenue){
    # m <- "AIM-Hub"
    # rev <- "Neutral"
  p <- ggplot(pdata %>% filter( model == m)) + # revenue == rev,
    geom_point(aes(x = `1`*100, y = `10` * 100, size = `Y`, color = R_CGE, shape = target), alpha = 0.6) +
    geom_hline(yintercept = 0, color = "grey80") +
    geom_vline(xintercept = 0, color = "grey80") +
    geom_abline(intercept = 0, slope = 1, color = "grey50", linetype = 2) +
    # geom_abline(intercept = 0, slope = -1, color = "grey50", linetype = 2) +
    labs(x = "Change in the lowest decile (%)", y = "Change in the highest decile (%)", subtitle = m) + 
    MyTheme + theme(legend.key.size = unit(0.42, 'cm'), 
                   legend.key.height = unit(0.42, 'cm'),
                   legend.key.width = unit(0.42, 'cm'), 
                   legend.title = element_text(size=10),
                   legend.text = element_text(size=9)) +
    guides(color = guide_legend(title = "Region", ncol = 2), 
           size = guide_legend(title = "Year"), 
           shape = guide_legend(title = "Scenario")) +
    scale_color_manual(values = palette_R_CGE) +
    scale_size_manual(values = c("2030" = 2, "2040" = 1.8, "2050" = 1.5, "2060" = 1.2, "2070" = 1)) +
    facet_wrap(~revenue, scales = "free")

  p
  
  if(m == "AIM-Hub"){m_m <- "AIMHub"}else{m_m <- m}
  
  ggsave(p, filename = paste0("2_Expenditure_change_highlow_", m_m, ".pdf"), path = paste0(dir_output_YSSPpaper, "/fig/"),
       width = 20, height = 7, units = "cm")
  

# }
}

p_legend <- get_legend(p) %>% as_ggplot()

ggsave(p_legend, filename = paste0("2_Expenditure_change_highlow_legend.pdf"), 
       path = paste0(dir_output_YSSPpaper, "/fig/"),
       width = 8, height = 10, units = "cm")


# both models, comparing models
p <- ggplot(pdata %>% filter( target == "1.5C", `1` < 2, gini == "consistent") %>% F_gini_rename()) + # revenue == rev,
  geom_point(aes(x = `1`*100, y = `10` * 100, size = `Y`, color = model, shape = model), alpha = .4) + # , alpha = gini
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_abline(intercept = 0, slope = 1, color = "grey50", linetype = 2) +
  labs(x = "Change in the lowest decile (%)", y = "Change in the highest decile (%)", color = "Model", size = "Year", shape = "Model") +  # , alpha = "SSP"
  MyTheme + theme(legend.key.size = unit(0.42, 'cm'), 
                  legend.key.height = unit(0.42, 'cm'),
                  legend.key.width = unit(0.42, 'cm'), 
                  legend.spacing = unit(.1, "cm"),
                  legend.title = element_text(size=10),
                  legend.text = element_text(size=9)) +
  scale_color_manual(values = palette_model) +
  scale_shape_manual(values = palette_shape_model) +
  # scale_alpha_manual(values = palette_alpha_gini_all) +
  scale_size_manual(values = c("2030" = 2, "2040" = 1.6, "2050" = 1.3, "2060" = 1, "2070" = .7)) +
  facet_wrap(~revenue, scales = "free")

p


ggsave(p, filename = paste0("2_Expenditure_change_highlow_cfmodel.pdf"), path = paste0(dir_output_YSSPpaper, "/fig/"),
       width = 16, height = 8, units = "cm")


 ## both models, comparing regions
p <- ggplot(pdata %>% filter( target == "1.5C")) + # revenue == rev,
  geom_point(aes(x = `1`*100, y = `10` * 100, size = `Y`, color = R_CGE, shape = model), alpha = 0.6) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_abline(intercept = 0, slope = 1, color = "grey50", linetype = 2) +
  # geom_abline(intercept = 0, slope = -1, color = "grey50", linetype = 2) +
  labs(x = "Change in the lowest decile (%)", y = "Change in the highest decile (%)") + 
  MyTheme + theme(legend.key.size = unit(0.42, 'cm'), 
                  legend.key.height = unit(0.42, 'cm'),
                  legend.key.width = unit(0.42, 'cm'), 
                  legend.title = element_text(size=10),
                  legend.text = element_text(size=9),
                  legend.position = "none") +
  guides(color = guide_legend(title = "Region", ncol = 2), 
         size = guide_legend(title = "Year"), 
         shape = guide_legend(title = "Model")) +
  scale_color_manual(values = palette_R_CGE) +
  scale_size_manual(values = c("2030" = 2, "2050" = 1.5, "2070" = 1)) +
  facet_wrap(~revenue, scales = "free")

p


ggsave(p, filename = paste0("2_Expenditure_change_highlow_cfregions.pdf"), path = paste0(dir_output_YSSPpaper, "/fig/"),
       width = 12, height = 7, units = "cm")

rm(p, p_legend, Gini, Exp_dec, Exp_dec_BaU, pdata, pdata1)



print("The END of AIMHub_only/2_Inequality.R")

