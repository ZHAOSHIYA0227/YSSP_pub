# This Rscript deals with the decile data after imputation
# By Shiya ZHAO, 2023/11/01

# 1 calculate the inequality index 
#https://www.sciencedirect.com/science/article/pii/S0305750X18304212
# policy effect of a carbon tax on the lowest income group relative to the national average over GDP per capita 



# 0. load data ------------------------------------------------------------
df_PQ <- F_load_dec(variable = "output_Price_I", lis_colname = c("R", "R3", "Ref", "I", "Y", "value", "model")) %>% 
  left_join(MapI) %>% mutate(I_abb= factor(I_abb, levels = lis_I_abb)) %>% 
  dplyr::select("model", "R", "R_CGE", "Y", "Ref",  "I_abb", "value") %>% F_reve_mutate() %>% mutate(scenario = paste0(target,"_",revenue))


# budget plot -------------------------------------------------------------------

pdata <- df_budget %>% 
  dplyr::select(R, Y, model, DEC, scenario, gini,  value) %>%
  distinct() %>% 
  # filter( model == "AIM-Hub") %>%
  pivot_wider(names_from = "scenario", values_from = "value") %>% 
  pivot_longer(c("2C_Neutral", "1.5C_Neutral", "2C_EPC", "1.5C_EPC"), values_to = "value", names_to = "scenario") %>% 
  mutate(change = (value - `No-Miti_Neutral`)/`No-Miti_Neutral`) %>% F_split_sce()


pdata %>% filter(Y == "2030", DEC != "ALL", change < 2) %>% openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper, "/csv/6_ConsumptionLoss_tot.xlsx"))
# save consumption loss

df_cnsloss_d <- pdata %>% dplyr::select(-scenario) %>% filter(change < 2)
save(df_cnsloss_d, file = paste0(dir_output_YSSPpaper, "/RData/6_consumptionloss.RData"))
rm(df_cnsloss_d)

lis_y_for <- c(seq(2020, 2100,10))
for(y in 1:length(lis_y_for)){

  pdata1 <- pdata %>% filter(Y == lis_y_for[y], DEC != "ALL", change < 1, gini == "consistent") %>% mutate(revenue = factor(revenue, levels = c("Neutral", "EPC")))
  p <- ggplot(pdata1) +
    geom_boxplot(aes(x = DEC, y = change * 100, 
                   group = paste0(DEC,scenario, Y, model), color = target, linetype = model), width = .7, outliers = F, linewidth = 0.3) + 
    geom_hline(yintercept = 0, color = "grey50") +
    MyTheme +
    labs(x = "Decile", y = "Change in household expenditure (%)", subtitle = lis_y_for[y]) +
    scale_color_manual(values = palette_Sc)+
    guides(color = guide_legend(title = "Scenario"), linetype = guide_legend(title = "Model")) + 
    facet_wrap(~revenue, scales = "free", ncol = 1)

  p

  ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/6_budget_box_", lis_y_for[y], ".pdf"), width = 12, height = 10, units = "cm")
  rm(p)

}

rm(pdata)

# EV plot -----------------------------------------------------------------
df_EV <- df_Con %>% 
  left_join(df_PQ %>% filter(scenario == "No-Miti_Neutral") %>% dplyr::select(-scenario, -target, -revenue, -Ref) %>% dplyr::rename("PQ_BaU" = "value")) %>% 
  left_join(df_Con %>% filter(scenario == "No-Miti_Neutral") %>% dplyr::select(-scenario, -target, -revenue) %>% dplyr::rename("Con_BaU" = "value")) %>% 
  dplyr::group_by(model, R, R_CGE, Y, scenario, gini, DEC) %>% 
  dplyr::mutate(EVi = (value*PQ_BaU-Con_BaU*PQ_BaU)/sum(Con_BaU*PQ_BaU), EV = sum(EVi), Coni = (value*PQ_BaU-Con_BaU*PQ_BaU)/(Con_BaU*PQ_BaU)) %>% ungroup()

save(df_EV, df_PQ, file = paste0(dir_output_YSSPpaper, "/RData/6_EVi.RData"))

df_EV %>% filter(Y %in% c(2030), target %in% c("1.5C", "2C")) %>% dplyr::select(model, R, R_CGE,Y,target, revenue, gini, DEC, I_abb, EVi) %>% 
  openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper,"/csv/6_EVi_2030.xlsx"))
df_EV %>% filter(Y %in% c(2050), target %in% c("1.5C", "2C")) %>% dplyr::select(model, R, R_CGE,Y,target, revenue, gini, DEC, I_abb, EVi) %>% 
  openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper,"/csv/6_EVi_2050.xlsx"))
df_EV %>% filter(Y %in% c(2070), target %in% c("1.5C", "2C")) %>% dplyr::select(model, R, R_CGE,Y,target, revenue, gini, DEC, I_abb, EVi) %>% 
  openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper,"/csv/6_EVi_2070.xlsx"))



lis_y_for <- c(seq(2020,2050,10), 2070, 2090)
for(y in 1:length(lis_y_for)){
  # y <- 1
pdata <- df_EV %>% dplyr::select(-EVi) %>% distinct() %>% filter(scenario != "No-Miti_Neutral", Y %in% lis_y_for[y], DEC != "ALL", gini == "consistent") %>% filter(EV < 1)


p <- ggplot(pdata) +
  geom_boxplot(aes(x = DEC, y = EV * 100, group = paste0(DEC,scenario, model), 
                          color = target, linetype = model), 
               outliers = F) + 
  geom_hline(yintercept = 0, color = "grey50") +
  MyTheme +
  facet_wrap(~revenue, scales = "free_y", ncol = 1) +
  labs(x = "Decile", y = "Change in consumption (%)", subtitle = lis_y_for[y]) +
  scale_color_manual(values = palette_Sc) + 
  # scale_y_continuous(breaks = pretty(pdata$EV*100, n = 5)) +
  guides(color = guide_legend(title = "Scenario"))

# p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/6_EV_box_", lis_y_for[y], ".pdf"), 
       width = 12, height = 10.4, units = "cm")
       rm(p)
}




# 0_income index and stuff ------------------------------------------------
df_index <- df_budget %>% 
  dplyr::select(c("model", "R", "Y", "DEC",  "scenario", "gini", "value")) %>% 
  distinct() %>% 
  pivot_wider(names_from = "scenario", values_from = "value") %>% 
  pivot_longer(c("2C_EPC", "2C_Neutral", "1.5C_EPC", "1.5C_Neutral"), names_to = "scenario", values_to = "value") %>% 
  # dplyr::select(c("model", "R", "Y", "DEC", "I_abb", "scenario", "No-Miti", "value")) %>% 
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
  dplyr::group_by(model, R, Y, DEC1,  scenario, gini) %>% 
  reframe(change1 = (sum(value)-sum(`No-Miti_Neutral`))/sum(`No-Miti_Neutral`) *100) %>%
  pivot_wider(names_from = "DEC1", values_from = "change1") %>% 
  mutate(sign_low = `low`/abs(`low`), sign_high = `high`/abs(`high`)) %>% 
  filter(!is.na(low)) %>% 
  mutate(#mean = dplyr::select(., `1`:`10`) %>% rowSums(na.rm = TRUE)/10,
    index = low/high,
    size = ((index-min(index)+1)/(max(index)-min(index)))) %>% 
  filter(index != max(index))


# plot of index -----------------------------------------------------------

pdata <- df_index  %>% F_split_sce() %>% filter(Y != 2020, low < 100)
a <- pdata %>% filter(revenue == "EPC")
b <- pdata %>% filter(revenue == "Neutral")

p <- ggplot(pdata) +
  geom_point(aes(x = low, y = high, color = model, shape = target,alpha = target), size = 0.7) +
  geom_vline(xintercept = 0, color = "grey70") +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  geom_abline(intercept = 0, slope = -1, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = palette_model) +
  scale_alpha_manual(values = c("2C" = 0.5, "1.5C" = 1)) +
  guides(color = guide_legend(title = "Model"), alpha = guide_legend(title = "Scenario"), shape = guide_legend(title = "Scenario")) +
  labs(y = "Change in budget higher-income population |%", x = "Change in lower-income population |%") +  
  facet_grid(Y~revenue) +
  ggthemes::theme_pander()

p

ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/6_change of income.pdf"), width = 12, height = 8, units = "cm")
  rm(p)

# boxplot-budget
csv_output <- df_budget %>% 
  # mutate(expenditure = share * budget) %>% 
  dplyr::select(c("model", "R", "Y", "DEC",  "scenario", "gini", "value")) %>% 
  distinct() %>% 
  pivot_wider(values_from = "value", names_from = "scenario") %>% 
  pivot_longer(cols = c("2C_Neutral", "1.5C_Neutral", "2C_EPC", "1.5C_EPC"),values_to = "budget", names_to = "scenario") %>% 
  mutate(change = (budget - `No-Miti_Neutral`)/`No-Miti_Neutral`) %>% 
  group_by(model, scenario, gini, Y, DEC) %>% 
  reframe(median = median(change)) 
openxlsx::write.xlsx(csv_output, file = paste0(dir_output_YSSPpaper, "/csv/Dec_budget.xlsx"))

rm(csv_output)

pdata <- df_budget %>% 
  dplyr::select(c("model", "R", "Y", "DEC",  "scenario", "gini", "value")) %>% 
  distinct() %>% 
  pivot_wider(values_from = "value", names_from = "scenario") %>% 
  pivot_longer(cols = c("2C_Neutral", "1.5C_Neutral", "2C_EPC", "1.5C_EPC"),values_to = "budget", names_to = "scenario") %>% 
  mutate(change = (budget - `No-Miti_Neutral`)/`No-Miti_Neutral`) %>% 
  F_split_sce() %>% 
  filter(Y %in% c(2030, 2050, 2070), target == "1.5C")
p <- ggplot(pdata) +
  geom_boxplot(aes(x = DEC, y = change*100, color = Y), outlier.alpha = 0.5, outlier.size = 0.5, outlier.shape = NA, width = .7) +
  geom_vline(xintercept = 0, color = "grey70") +
  geom_hline(yintercept = 0, color = "grey70") +
  scale_color_manual(values = c("2030" = "#376888", "2050" = "#826b88", "2070" = "#de786a")) + 
  labs(y = "Percentage change in budget|1.5C|%", x = "Income deciles (1st-lowest,10th-highest)") + 
  ggthemes::theme_pander() +
  coord_cartesian(ylim = c(-40, 20)) +
  facet_grid(revenue~model)+
  guides(color = guide_legend(title = "Year")) +
  theme(text = element_text(size = 14))

p
  
ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/6_change of income_msg.pdf"), 
       width = 20, height = 18, units = "cm")
  rm(p, pdata)

# -------------------------------------------------------------------------

##########################################################################################################################################################
##########################################################################################################################################################


min(df_index$index)
max(df_index$index)

df_index_cge <- df_index %>% 
  filter(Y == "2050", 
         scenario == "1.5C", 
         model == "AIM-Hub"
  ) #%>% 
# dplyr::select(c("R", "I_abb", "Index")) %>% 
# filter(!is.na(Index)) %>% 
# pivot_wider(names_from = "I_abb", values_from = "Index") 

## Figure 55 bubble charts ----
colnames(df_Exp)

df_Con %>% filter(R %in% c("JPN", "CHN"), I_abb %in% c("Energy"), Y == "2030")
df_indexi <- df_Con %>% 
  # mutate(expenditure = share * budget) %>%
  dplyr::select(c("model", "R", "Y", "DEC", "I_abb",  "scenario", "gini", "value")) %>% 
  distinct() %>% 
  pivot_wider(names_from = "scenario", values_from = "value") %>% 
  pivot_longer(colnames(.)[which(grepl("[0-9]C", colnames(.)))], names_to = "scenario", values_to = "value") %>% 
  # dplyr::select(c("model", "R", "Y", "DEC", "I_abb", "scenario", "No-Miti", "value")) %>% 
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
  dplyr::group_by(model, R, Y, DEC1, I_abb, scenario, gini) %>% 
  reframe(change1 = (sum(value)-sum(`No-Miti_Neutral`))/sum(`No-Miti_Neutral`) *100) %>% 
  pivot_wider(names_from = "DEC1", values_from = "change1") %>% 
  mutate(sign_low = `low`/abs(`low`), sign_high = `high`/abs(`high`)) %>% 
  filter(!is.na(low)) %>% 
  mutate(#mean = dplyr::select(., `1`:`10`) %>% rowSums(na.rm = TRUE)/10,
    index = low/high,
    size = ((index-min(index)+1)/(max(index)-min(index)))) %>% 
  filter(index != max(index)) %>% F_split_sce() %>% F_sce_main()

pdata <- df_indexi %>%
  mutate(Y1=as.numeric(as.character(Y))) %>% 
  filter(R %in% c("CHN", "JPN", "IND", "MDG", "ETH"), target == "1.5C")
# filter(Y == "2050") 


p55 <- ggplot(pdata) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dashed")+
  geom_vline(xintercept = 0, color = "grey70", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "grey90") +
  geom_point(mapping = aes(x = low, y = high, size = abs(index),
                           shape = model,
                           color = factor(I_abb, levels = lis_I_abb)), alpha = 0.7) +
  # scale_x_continuous(guide = guide_axis(position = "bottom")) + 
  scale_y_continuous(guide = guide_axis(position = "left")) +
  scale_size(range = c(1, 10), name = "low/high") +
  guides(size = "none", color = guide_legend(title = "Commodity")) +
  facet_grid(revenue~R) +
  MyTheme +
  scale_color_manual(values = palette_COM)  
# hrbrthemes::theme_ipsum()
# scale_x_discrete(limits = seq(2020, 2090, 10))

p55


rm(p55, pdata)

## Figure 551 bubble charts global ----
#### model comparison ----
pdata <- df_indexi %>%
  mutate(Y1=as.numeric(as.character(Y)))  %>% 
  filter(target == "1.5C") %>% 
  left_join(Map_r_PHI2Hub) %>% 
  filter(low != max(.$low), high != max(.$high),
         I_abb %in% c("Food&Beverages", "Energy")
         )%>% 
  F_R_CGEfull() %>% F_sce_main()
# filter(Y == "2050") 
max(pdata$high)
max(pdata$low)
p551 <- ggplot(pdata) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dashed")+
  geom_vline(xintercept = 0, color = "grey70", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "grey90") +
  geom_point(mapping = aes(x = low, y = high, size = abs(index),
                           shape = model,
                           color = R_CGE), alpha = 0.7) +
  # scale_x_continuous(guide = guide_axis(position = "bottom")) +
  labs(x = "Change in consumption in the lower deciles (1st-4th) (%)", y = "Change in consumption in the higher deciles (6st-10th) (%)") +
  scale_y_continuous(guide = guide_axis(position = "left")) +
  scale_size(range = c(1, 10), name = "low/high") +
  scale_color_manual(values = palette_R_CGE) +
  guides(size = "none", color = guide_legend(title = "Region"), shape = guide_legend(title = "Model")) +
  facet_grid(revenue~I_abb) +
  MyTheme 
# hrbrthemes::theme_ipsum()
# scale_x_discrete(limits = seq(2020, 2090, 10))

p551

ggsave(p551, filename = paste0(dir_output_YSSPpaper,"/fig/SI/6_Consumption loss index.pdf"), 
       width = 18, height = 14, units = "cm")

rm(p551, pdata)

## Figure 551 bubble charts for EVi ----
pdata <- df_EV %>% dplyr::select("R", "Y" ,"model","scenario", "gini" ,"I_abb", "DEC" ,"EVi") %>% 
  pivot_wider(values_from = "EVi", names_from = "DEC") %>% 
  left_join(Map_r_PHI2Hub) %>% 
  filter(scenario != "No-Miti", `1` != max(abs(`1`)),`10` != max(abs(`10`)), Y == "2030", gini == "consistent"
         ) %>% F_split_sce() %>% F_R_CGEfull() %>% F_gini_rename %>% filter(target == "1.5C")


lis_revenue <- unique(pdata$revenue) %>% as.character()
for(m in 1:length(lis_model)){
  for(rev in 1:length(lis_revenue)){
# rev <- 1
# m <- 1
p552 <- ggplot(pdata %>% filter(`1` > -1, revenue == lis_revenue[rev], model == lis_model[m])) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dashed")+
  geom_vline(xintercept = 0, color = "grey70", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "grey90") +
  geom_point(mapping = aes(x = `1`*100, y = `10`*100, 
                           shape = target,
                           color = R_CGE, #alpha = gini
                           ), alpha = .5, size = 1.7) +
  scale_y_continuous(guide = guide_axis(position = "left")) +
  scale_size(range = c(1, 10), name = "low/high") +
  scale_color_manual(values = palette_R_CGE) +
  # scale_alpha_manual(values = palette_alpha_gini_all) +
  labs(x = "Change in consumption in the 1st-decile (%)", y = "Change in consumption in the 10th-decile (%)", subtitle = paste0(lis_revenue[rev]," | ", lis_model[m])) +
  guides(size = "none", color = guide_legend(title = "Region", ncol = 1), shape = guide_legend(title = "Scenario")#, 
         # alpha = guide_legend(title = "SSP")
         ) +
  facet_wrap(I_abb ~ ., ncol = 5) +
  MyTheme + theme(legend.key.size = unit(0.5, 'cm'), 
                  legend.key.height = unit(0.5, 'cm'),
                  legend.key.width = unit(0.5, 'cm'), 
                  legend.title = element_text(size=10),
                  legend.text = element_text(size=9),
                  legend.position = "none",
                  panel.spacing.x = unit(1.1,"lines"))

p552

if(lis_model[m] == "AIM-Hub"){
  m_m <- "AIMHub"
}else(m_m <- lis_model[m])

ggsave(p552, filename = paste0(dir_output_YSSPpaper,"/fig/SI/6_EVi_",lis_revenue[rev],"_", m_m,".pdf"), 
       width = 22, height = 12, units = "cm")

p_leg <- get_legend(p552 + theme(legend.position = "right")) %>% ggpubr::as_ggplot()
ggsave(p_leg, filename = paste0(dir_output_YSSPpaper,"/fig/SI/6_EVi_",lis_revenue[rev],"_legend.pdf"), 
       width = 5, height = 18, units = "cm")

       rm(p552,p_leg)
  }
}




 ### model comparison ----

lis_revenue <- unique(pdata$revenue) %>% as.character()
for(rev in 1:length(lis_revenue)){
  p552 <- ggplot(pdata %>% filter(`1` > -1, revenue == lis_revenue[rev])) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed")+
    geom_vline(xintercept = 0, color = "grey70", linetype = "dashed") +
    geom_abline(slope = 1, intercept = 0, color = "grey90") +
    geom_point(mapping = aes(x = `1`*100, y = `10`*100, #size = abs(`1`/`10`),
                             shape = model, 
                             color = R_CGE, alpha = gini), size = 1.5) +
    scale_y_continuous(guide = guide_axis(position = "left")) +
    scale_size(range = c(1, 10), name = "low/high") +
    scale_color_manual(values = palette_R_CGE) +
    scale_alpha_manual(values = palette_alpha_gini_all) +
    scale_shape_manual(values = palette_shape_model) +
    labs(x = "Change in consumption in the 1st-decile (%)", y = "Change in consumption in the 10th-decile (%)", 
         subtitle = paste0(lis_revenue[rev])) +
    guides(size = "none", color = guide_legend(title = "Region", ncol = 1), shape = guide_legend(title = "Model"), 
           alpha = guide_legend(title = "SSP")) +
    facet_wrap(I_abb ~ ., ncol = 5) +
    MyTheme + theme(legend.position = "none")
  
  
  ggsave(p552, filename = paste0(dir_output_YSSPpaper,"/fig/SI/6_EVi_",lis_revenue[rev],"_model.pdf"), 
         width = 22, height = 14, units = "cm")

  p_leg <- get_legend(p552 + theme(legend.position = "right")) %>% ggpubr::as_ggplot()
  ggsave(p_leg, filename = paste0(dir_output_YSSPpaper,"/fig/SI/6_EVi_model_legend.pdf"), 
         width = 5, height = 18, units = "cm")
  
        rm(p552,p_leg)
}


  rm(pdata)
#   p551 <- ggplot(pdata) +
#   geom_line(mapping = aes(x =  as.numeric(as.character(Y)),
#                           y = index,
#                           group = paste0(model, "_", scenario, "_", R),
#                           color = scenario,
#                           linetype = model)) +
#   xlim(2020, 2070) +
#   facet_wrap(~ I_abb, scales = "free") +
#   MyTheme +
#   scale_color_manual(values = palette_Sc)
# # hrbrthemes::theme_ipsum()
# # scale_x_discrete(limits = seq(2020, 2090, 10))
# 
# p551
# 
# ## heatmaps ----
# df_index_cge_tmp <- df_index_cge
# pdata <- df_index_cge %>% 
#   dplyr::select(-"R") %>%
#   as.matrix()
# rownames(pdata) <- as.character(df_index_cge$R)
# 
# heatmap(pdata, scale = "row")
# 
# 
# dt <- as.matrix(mtcars)
# 
# 
# 
# 
# # Figure 56 Price in No-Miti
# pdata <- PQ %>% 
#   filter(#scenario == "No-Miti",
#     R %in% lis_R_concern,
#     I_abb %in% c("FNB", "ENE"))
# 
# # colnames(pdata)
# p_5e <- ggplot(data =  pdata,
#                mapping = aes(x = Y, y = PQchange,colour = scenario)) +
#   geom_abline(intercept = 0, slope = 0, colour = 'grey30') +
#   geom_abline(intercept = 1, slope = 0, colour = 'grey90') +
#   geom_line(mapping = aes(group =  paste0(scenario, model), linetype = model)) +
#   geom_point(shape = 1, size = 0.5) + # position = 'dodge'
#   labs(title = "Price Change", # Change in consumption compared to Baseline (2050)"
#        x = "cyl",
#        fill = "gear")+
#   xlab("Income deciles") +
#   ylab("Price") +
#   facet_wrap(I_abb~R, scales = 'free', ncol = 4) +
#   MyTheme  +
#   scale_color_manual(values = palette_Sc) +
#   guides(colour = guide_legend(title = 'scenario', row = 1, byrow = TRUE, size = 7))
# p_5e
# 
# ggsave(plot = p_5e, filename = paste0(dir_output, "fig/Figure 56 Price change No-Miti.pdf"), width = 20, height = 14, units = "cm")
# 

rm( df_index, df_indexi, df_index_cge)
print("The END of AIMHub_only/6_DecileDataAnalysis.R")

