# this script calculates the poverty headcount, poverty gap, and revenue redistribution
# Shiya ZHAO, 2024.01.12




# 0. load data ---------------------------------------------------------
# population
Population <- input_base %>%
  rgdx.param("Population") %>%
  mutate(POP = Population/1000000,
         UNIT = "Million people",
  ) %>%
  dplyr::group_by(Ref, Y, R) %>%
  dplyr::summarise(POP = sum(POP), .groups = "drop") %>%
  ungroup() %>%
  filter(Ref == "SSP2") %>%
  dplyr::select(-c("Ref"))
# unit = Million people


# GDP per capita ----
df_GDPCapcge <- paste0(dir_input, "AIMHub.gdx") %>% rgdx.param("GDPCap") %>% 
  filter(Y %in% seq(2010,2100,5)) %>% mutate(model = "AIM-Hub") 
df_GDPCapmsg <- paste0(dir_input, "MESSAGEix.gdx") %>% rgdx.param("GDPCap") %>% 
  filter(Y %in% seq(2010,2100,5)) %>% mutate(model = "MESSAGEix") 

df_GDP0 <- rbind(df_GDPCapcge, df_GDPCapmsg) %>% left_join(Population) %>% mutate(GDP = GDPCap*POP*1000000)
rm(df_GDPCapcge, df_GDPCapmsg)

df_GDP_R17 <- df_GDP0 %>% left_join(Population) %>% left_join(Map_r_PHI2Hub) %>% 
  filter(!is.na(GDPCap), !is.na(R_CGE), R!= "WLD") %>%  
  mutate(R =  paste0("R17", R_CGE)) %>% 
  dplyr::group_by(model, Ref, R, Y) %>% 
  dplyr::reframe(GDPCap = sum(GDPCap*POP)/sum(POP), GDP = sum(GDPCap*POP)*1000000, POP = sum(POP))

df_GDP <- df_GDP0 %>% rbind(df_GDP_R17)
rm(df_GDP0, df_GDP_R17)


## Poverty gap per GDP ----
# AIM-Hub
PoVGap_tmp <- rgdx.param(AnaExp, "PoVgap_GDPExp") %>% 
  filter(
    Y %in% seq(2010,2100,5)) %>% 
  mutate(Effect = 'Expenditure',
         model = "AIM-Hub") %>% 
  left_join(MapScenario)

# messageix
PoVGapmsg_tmp <- rgdx.param(AnaExpmsg, "PoVgap_GDPExp") %>% 
  filter(
    Y %in% seq(2010,2100,5)) %>%
  mutate(Effect = 'Expenditure',
         model = "MESSAGEix") %>% 
  left_join(MapScenario)



# Poverty gap absolute
# AIM-Hub
PoVGap_absExp_tmp <- rgdx.param(AnaExp, "PoVgap_absExp") %>% 
  filter(
    Y %in% seq(2010,2100,5)) %>% 
  mutate(Effect = 'Expenditure',
         model = "AIM-Hub") %>% 
  left_join(MapScenario)

# messageix
PoVGapmsg_absExp_tmp <- rgdx.param(AnaExpmsg, "PoVgap_absExp") %>% 
  filter(
    Y %in% seq(2010,2100,5)) %>%
  mutate(Effect = 'Expenditure',
         model = "MESSAGEix") %>% 
  left_join(MapScenario)

PoVGap <- PoVGap_tmp %>% rbind(PoVGapmsg_tmp) %>% dplyr::select("Ref", "R", "Y", "TH", "model", "PoVgap_GDPExp") %>% mutate(Unit_GDPExp = "1") %>% 
  left_join(PoVGap_absExp_tmp %>% rbind(PoVGapmsg_absExp_tmp) %>% 
              dplyr::select("Ref", "R", "Y", "TH", "model", "PoVgap_absExp") %>% mutate(Unit_absExp = "$")) %>% F_reve_mutate() %>% 
  F_TH()
unique(PoVGap$R)
rm(PoVGap_tmp, PoVGapmsg_tmp, PoVGap_absExp_tmp, PoVGapmsg_absExp_tmp)

## Poverty headcount----
load(paste0(dir_output_YSSPpaper, "/RData/Poverty.RData"))
PoV_WLD <- PoV %>% filter(R == "WLD") %>% mutate(Ref = paste0(target, "_", revenue, "_", gini)) %>% 
  dplyr::select(model, target, revenue, gini, Y, R, Effect, TH1, UNIT, value, POP, RoP) 


rm(PoVExpmsg_tmp, PoVExpcge_tmp)




## Carbon tax revenue ----

TX_HUB <- rgdx.param(paste0("../", prog_loc, "/data/PHIinputdata/AIMHub.gdx"), "Tx_revenue_capita") %>%
  mutate(Unit = "US$2010 per capita", model = "AIM-Hub") %>% 
  gdata::rename.vars(c("Tx_revenue_capita"), c("Rev_capita")) %>% mutate(Unit_Rev = Unit) %>% dplyr::select(-Unit)

TX_MSG <- rgdx.param(paste0("../", prog_loc, "/data/PHIinputdata/MESSAGEix.gdx"), "Tx_revenue_capita") %>%
  mutate(Unit = "US$2010 per capita", model = "MESSAGEix") %>% 
  gdata::rename.vars(c("Tx_revenue_capita"), c("Rev_capita")) %>% mutate(Unit_Rev = Unit) %>% dplyr::select(-Unit)

TX_tot_HUB <- rgdx.param(paste0("../", prog_loc, "/data/PHIinputdata/AIMHub.gdx"), "Tx_revenue") %>%
  mutate(Unit = "US$2010 per capita", model = "AIM-Hub") %>% 
  gdata::rename.vars(c("Tx_revenue"), c("Rev")) %>% mutate(Unit_Rev = Unit) %>% dplyr::select(-Unit)

TX_tot_MSG <- rgdx.param(paste0("../", prog_loc, "/data/PHIinputdata/MESSAGEix.gdx"), "Tx_revenue") %>%
  mutate(Unit = "US$2010 per capita", model = "MESSAGEix") %>% 
  gdata::rename.vars(c("Tx_revenue"), c("Rev")) %>% mutate(Unit_Rev = Unit) %>% dplyr::select(-Unit)

TX_rate_HUB <- rgdx.param(paste0("../", prog_loc, "/data/PHIinputdata/AIMHub.gdx"), "Tx_revenue_rate") %>%
  mutate(Unit = "1", model = "AIM-Hub") %>% 
  gdata::rename.vars(c("Tx_revenue_rate"), c("Rev_rate")) %>% mutate(Unit_rate = Unit) %>% dplyr::select(-Unit)

TX_rate_MSG <- rgdx.param(paste0("../", prog_loc, "/data/PHIinputdata/MESSAGEix.gdx"), "Tx_revenue_rate") %>%
  mutate(Unit = "1", model = "MESSAGEix") %>% 
  gdata::rename.vars(c("Tx_revenue_rate"), c("Rev_rate")) %>% mutate(Unit_rate = Unit) %>% dplyr::select(-Unit)

Tx_capita0 <- TX_HUB %>%  
  rbind(TX_MSG) %>% 
  left_join(TX_tot_HUB %>% rbind(TX_tot_MSG)) %>% 
  left_join(TX_rate_HUB %>% rbind(TX_rate_MSG)) %>%  
  left_join(MapScenario) %>% 
  filter(as.numeric(as.character(Y)) %in% seq(2020, 2100, 10),
         Type == "PPP") %>% 
  dplyr::select(-c("Type", "CGEscenario", "MSGscenario")) %>%
  left_join(Population) %>% 
  F_reve_mutate()

Tx_capita_R17 <- Tx_capita0 %>% left_join(Map_r_PHI2Hub) %>% mutate(R = paste0("R17", R_CGE)) %>% 
  filter(!is.na(POP), !is.na(R_CGE)) %>% 
  dplyr::group_by(model, Ref, R, target, gini, revenue, Y) %>% 
  dplyr::reframe(Rev_capita = sum(Rev_capita*POP)/sum(POP), Rev = sum(Rev)) 

Tx_capita_WLD <- Tx_capita0 %>% 
  filter(!is.na(POP), R!= "WLD") %>% 
  dplyr::group_by(model, Ref, target, gini, revenue, Y) %>% 
  dplyr::reframe(Rev_capita = sum(Rev_capita*POP)/sum(POP), Rev = sum(Rev), R = "WLD")

Tx_capita <- Tx_capita_WLD %>% rbind(Tx_capita_R17) %>% left_join(df_GDP) %>% 
  mutate(Rev_rate = Rev/GDP, Unit_rate = "1", Unit_Rev = "US$2010 per capita") %>% dplyr::select(-GDP, -GDPCap) %>% 
  rbind(Tx_capita0)

rm(TX_tot_HUB, TX_rate_HUB, TX_MSG, TX_HUB, Tx_capita_WLD, Tx_capita_R17, Tx_capita0, TX_rate_MSG, TX_tot_MSG)

# poverty and revenue
df_PoV_Rev <- PoV %>% 
  dplyr::select(model, target, revenue, gini, R, Y, Effect, TH1, value, POP) %>% #view
  filter(Effect == "Expenditure",
         Y %in% seq(2010, 2100,10)) %>% mutate(scenario = paste0(target, "_", revenue)) %>% dplyr::select(-target, -revenue) %>% 
  pivot_wider(values_from = "value", names_from = "scenario") %>% dplyr::rename("No-Miti" = "No-Miti_Neutral") %>% 
  pivot_longer(cols = c("1.5C_Neutral", "1.5C_EPC", "2C_Neutral", "2C_EPC"), values_to = "value", names_to = "scenario") %>% 
  mutate(`addition` = value-`No-Miti`) %>% 
  F_split_sce() %>% 
  left_join(Tx_capita %>% dplyr::select(-POP, -Ref)) %>% filter(R %in% lis_R_phi) %>% 
  mutate(Rev_capita = case_when(is.na(Rev_capita) ~ 0, 
                                !is.na(Rev_capita) ~ Rev_capita)) %>% 
  dplyr::select(-"Effect",-"scenario")

df_PoV_Rev[which(df_PoV_Rev$Y == 2020), which(colnames(df_PoV_Rev) == "addition")] <- 0 #%>% filter(TH1 == txt_plot_TH)

df_PoV_Rev_WLD <- df_PoV_Rev %>% filter(R == "WLD")

df_PoV_Rev_WLD[which(df_PoV_Rev_WLD$Y == 2020), which(colnames(df_PoV_Rev_WLD) == "addition")] <- 0 #%>% filter(TH1 == txt_plot_TH)
save(Tx_capita, PoVGap, df_PoV_Rev, file = paste0(dir_output_YSSPpaper, "/RData/Poverty_gap_tax_revenue.RData"))

## Decile budget in the baseline ----
Exp_dec <- data.frame()
Exp_dec_tmp <- data.frame()
for(m in seq_along(lis_model)){
  if(lis_model[m] == "AIM-Hub"){
    txt_m <- "AIMHub"
  }else{
    txt_m <- lis_model[m]
  }
  
  Exp_dec_tmp <- rgdx.param(paste0("../output/gdx/Decile_",txt_m,".gdx"), "output_Budget") %>% 
    gdata::rename.vars(colnames(.), c("R", "R3", "Ref", "DEC", "Y", "Budget")) %>% dplyr::select(-R3) %>% mutate(model = lis_model[m])
  
  Exp_dec <- Exp_dec_tmp %>% 
    dplyr::select("model", "Ref", "R", "Y", "DEC","Budget") %>% 
    F_reve_mutate() %>% mutate(DEC = gsub("D_","",DEC)) %>% 
    mutate(DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character(), "ALL"))) %>% rbind(Exp_dec)
  
  rm(Exp_dec_tmp)
}


## cnsloss ----
load(paste0(dir_output_YSSPpaper, "/RData/0_CNS_loss.RData"))


# 1. revenue v.s. decile consumptions ----
Exp_dec_BaU <- Exp_dec %>% filter(target == "No-Miti") %>% dplyr::select(-Ref, -revenue, -target) %>% dplyr::rename("Budget_BaU" = "Budget")

Tx_capita_tmp <- Tx_capita 
df_exp_rev <- Exp_dec_BaU %>% left_join(Tx_capita) %>% gdata::rename.vars(c("Rev_capita", "Rev_rate"), c("EPC ($/capita)", "Neutral (1)")) %>% 
  dplyr::select(-c("Unit_Rev", "Unit_rate", "revenue", "Ref")) %>% distinct() %>% 
  mutate(Percentage_rev = `EPC ($/capita)`/Budget_BaU * 100) %>% left_join(Map_r_PHI2Hub) %>% F_R_CGEfull()

# output in xlsx
load(paste0(dir_output_YSSPpaper, "/RData/1_GHG and PGHG.RData"))

df_exp_rev %>% filter(!is.na(Rev)) %>% 
  left_join(df_PGHG %>% gdata::rename.vars(c("unit", "value"), c("unit_PGHG", "PGHG")) %>% dplyr::select(-variable, -R, -revenue, -gini) %>% distinct()) %>% 
  mutate(idx = case_when(Percentage_rev > 100 ~ 1, Percentage_rev <= 100 ~ 0))%>% openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper, "/csv/1_2_expenditure and revenue per capita.xlsx"), sheetName = "OD")

## lowest decile ----
### lowest decile v.s. EPC ----
# scatter plot x: decile income, y: revenue (EPC)
pdata <- df_exp_rev %>% filter(DEC %in% c("1"), Y == "2030", target == "1.5C", gini == "consistent") %>% 
  mutate(label = case_when(Percentage_rev > 100 ~ 1, Percentage_rev <= 100 ~ 0))

p <- ggplot(pdata, mapping = aes(x = Budget_BaU, y = `EPC ($/capita)`)) +
  geom_abline(intercept = 0, slope = 1, color = "grey") +
  geom_point(mapping = aes( color = R_CGE)) +
  geom_text(data = pdata %>% filter(label == 1),  # dplyr::select points for labeling
            aes(label = R), 
            vjust = 1,  # Adjust vertical position
            color = "red",
            size = 2) +
  facet_wrap(~model) + MyTheme + labs(x = "Consumption of decile 1 in No-Miti ($/capita)", y = "Revenue per capita ($/capita)", color = "Region") +
  scale_color_manual(values = palette_R_CGE) + guides(color = guide_legend(ncol = 2))
p
ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Decile1_point_RevenueEPC_model.pdf"),
       p, height = 8, width = 24, units = "cm")

# EPC
# boxplot x: R_CGE, y: EPC revenue, facet: Y and 1-3 deciles

pdata <- df_exp_rev %>% filter(DEC %in% c("1"), Y %in% c("2030", "2040", "2050"), target == "1.5C", gini == "consistent", model =="AIM-Hub") %>% 
  # mutate(label = case_when(Percentage_rev > 100 ~ 1, Percentage_rev <= 100 ~ 0)) %>% 
  dplyr::group_by(model,R_CGE,Y, target, DEC) %>% mutate(Q1 = quantile(`Percentage_rev`, 0.25), 
                                                         Q3 = quantile(`Percentage_rev`, 0.75),
                                                         Upper = Q3 + 1.5*(Q3-Q1), 
                                                         Lower = Q1 - 1.5*(Q3-Q1), 
                                                         outliers = case_when(`Percentage_rev` >= Upper ~ `Percentage_rev`,
                                                                              Lower >= `Percentage_rev` ~ `Percentage_rev`,
                                                                              Lower < `Percentage_rev` & `Percentage_rev` < Upper ~ 0))

p <- ggplot(pdata, mapping = aes(x = R_CGE, y = Percentage_rev)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(Y~DEC, ncol = 3) + MyTheme + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) +
  facet_wrap(~Y) + labs(x = "Region", y = "Revenue per household consumption (%)") +
  geom_text(data = pdata %>% filter(outliers != 0),  # dplyr::select points for labeling
            aes(label = R),
            vjust = 1.5,  # Adjust vertical position
            color = "red",
            size = 2) +
  scale_color_manual(values = palette_R_CGE)
p

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_EPC_Decile1_RevenueInCNS_model.pdf"),
       p, height = 12, width = 40, units = "cm")

### lowest decile v.s. Neutral ----
# the percentage revenues in national total consumption
# boxplot x: R_CGE, y: percentage, facet: 
pdata <- df_exp_rev %>% filter(DEC %in% c("1"), Y %in% c(2030, 2040, 2050), target != "No-Miti", gini == "consistent", target == "1.5C", model == "AIM-Hub") %>% 
  mutate(label = case_when(Percentage_rev > 100 ~ 1, Percentage_rev <= 100 ~ 0)) %>% 
  dplyr::group_by(model,R_CGE,Y, target, DEC) %>% mutate(Q1 = quantile(`Neutral (1)`, 0.25), 
                                                    Q3 = quantile(`Neutral (1)`, 0.75), 
                                                    Upper = Q3 + 1.5*(Q3-Q1), 
                                                    Lower = Q1 - 1.5*(Q3-Q1), 
                                                    outliers = case_when(`Neutral (1)` >= Upper ~ `Neutral (1)`,
                                                                         Lower >= `Neutral (1)` ~ `Neutral (1)`,
                                                                         Lower < `Neutral (1)` & `Neutral (1)` < Upper ~ 0))

p <- ggplot(pdata, mapping = aes(x = R_CGE, y = `Neutral (1)` * 100)) +
  geom_boxplot() +
  facet_grid(~Y) + MyTheme + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) +
  geom_text(data = pdata %>% filter(outliers != 0),  # dplyr::select points for labeling
            aes(label = R),
            vjust = 1.5,  # Adjust vertical position
            color = "red",
            size = 2) + labs(x = "Region", y = "Revenue per national consumption (%)") +
  scale_color_manual(values = palette_R_CGE)
p

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_neutral_Revenue per cns_model.pdf"),
       p, height = 10, width = 30, units = "cm")


# comparing the national total consumption loss with the revenue recycled

pdata <- df_exp_rev %>% filter(DEC %in% c("1"), Y %in% c(2030, 2040, 2050), target != "No-Miti", gini == "consistent", target == "1.5C") %>% # , model == "AIM-Hub"
  left_join(CNS_loss) %>% dplyr::select("model", "R", "Y", "Neutral (1)", "target", "R_CGE", "value_cnsloss") %>% distinct() %>% filter( !is.na(value_cnsloss))
colnames(pdata)

p <- ggplot(pdata, mapping = aes(x = `Neutral (1)` * 100, y = `value_cnsloss`)) +
  geom_abline(intercept = 0, slope = 1, color = "grey") +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(mapping = aes( color = R_CGE, shape = model)) +
  facet_wrap(~Y) + MyTheme + 
  labs(x = "Revenue per national consumption (%)", y = "Consumption loss (%)", color = "Region", shape = "Model") +
  scale_color_manual(values = palette_R_CGE) + guides(color = guide_legend(ncol = 2))
p
ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_point_CNSloss_neutral.pdf"),
       p, height = 9, width = 28, units = "cm")

# boxplot; net income change without price intervention

p <- ggplot(pdata, mapping = aes(x = R_CGE, y = `Neutral (1)` * 100 -`value_cnsloss`)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_boxplot(mapping = aes()) +
  facet_wrap(~Y) + MyTheme + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  labs(x = "Region", y = "Net consumption change after redistribution (%)") +
  scale_color_manual(values = palette_R_CGE) + guides(color = guide_legend(ncol = 2))
p
ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_point_CNSloss_revenueNeutral.pdf"),
       p, height = 8, width = 35, units = "cm")


## highest decile ----
### highest decile v.s. EPC ----
# point plot
pdata <- df_exp_rev %>% filter(DEC %in% c("10"), Y == "2030", target == "1.5C", gini == "consistent") %>% 
  mutate(label = case_when(Percentage_rev > 100 ~ 1, Percentage_rev <= 100 ~ 0))

p <- ggplot(pdata, mapping = aes(x = Budget_BaU, y = `EPC ($/capita)`)) +
  geom_abline(intercept = 0, slope = 1, color = "grey") +
  geom_point(mapping = aes( color = R_CGE)) +
  geom_text(data = pdata %>% filter(label == 1),  # dplyr::select points for labeling
            aes(label = R), 
            vjust = 1,  # Adjust vertical position
            color = "red",
            size = 2) +
  facet_wrap(~model) + MyTheme + labs(x = "Consumption of decile 1 in No-Miti ($/capita)", y = "Revenue per capita ($/capita)") +
  scale_color_manual(values = palette_R_CGE) + guides(color = guide_legend(ncol = 2))
p
ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Decile10_point_RevenueEPC_model.pdf"),
       p, height = 8, width = 24, units = "cm")


# boxplot
p <- ggplot(pdata) +
  geom_boxplot(mapping = aes(x = R_CGE, y = `EPC ($/capita)`/Budget_BaU * 100, color = model)) +
  geom_hline(yintercept = 0, color = "grey") +
  # geom_point(mapping = aes( color = R_CGE)) +
  geom_text(data = pdata %>% filter(label == 1),  # dplyr::select points for labeling
            aes(label = R), 
            vjust = 1,  # Adjust vertical position
            color = "red",
            size = 2) +
  MyTheme + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(x = "Region", y = "Revenue/decile 10 consumption (%)", color = "Model") +
  scale_color_manual(values = palette_model) +
  guides(color = guide_legend(ncol = 1))
p
ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Decile10_box_RevenueEPC_model.pdf"),
       p, height = 11, width = 16, units = "cm")



# 2. revenue v.s. poverty reduction ----
## 





## revenue recycling ----
pdata <- df_PoV_Rev %>% 
  dplyr::select("model", "R", "Y", "TH1", "target", "revenue", "gini","addition","Rev_capita") %>% #view
  pivot_wider(names_from = "revenue", values_from = "addition") %>% 
  left_join(Map_r_PHI2Hub) %>% F_R_CGEfull() %>% 
  filter(Y %in% c(2030, 2050), !is.na(R_CGE), R_CGE != "WLD", gini == "consistent") 

### per capita ----
p_12 <- ggplot(pdata) +
  geom_boxplot(mapping = aes(x = R_CGE, y = Rev_capita, color = model), width = .8, outlier.alpha = .5, outlier.size = .5) +
  labs(y = "Carbon tax revenue per capita ($/capita)", 
       x = "Year") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  facet_grid(Y~target) +
  scale_linetype_manual(values = palette_linetype_revenue) +
  scale_color_manual(values = palette_model)  +
  scale_shape_manual(values = palette_shape_revenue)  +
  guides(color=guide_legend(title="Model"),
         linetype=guide_legend(title="Revenue"),
         shape = guide_legend(title = "Revenue"))
p_12


ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Revenue per capita_box_model.pdf"),
       p_12, height = 14, width = 26, units = "cm")
rm(p_12, pdata)


### revenue per GDP v.s. poverty gap per GDP (No-Miti) ----
a <- PoVGap %>% filter(target == "No-Miti") %>% 
  dplyr::select(-c(Ref, revenue, target,  TH))

# national level revenue per GDP
pdata <- Tx_capita %>% full_join(a, by = c("R", "Y", "model", "gini")) %>%  
  filter(!is.na(PoVgap_absExp), !grepl("R17", R), R!= "WLD") %>% 
  left_join(Map_r_PHI2Hub) %>% filter(!is.na(Ref)) %>% F_R_CGEfull() %>%
  dplyr::select("model", "Ref","Y","R_CGE","R","TH1","Rev_rate","Unit_rate","revenue","target","gini","PoVgap_GDPExp", "Unit_GDPExp") %>% 
  dplyr::rename("Revenue" = Rev_rate, "Poverty gap" = PoVgap_GDPExp) 
unique(pdata$R_CGE)
# boxplot
p_121 <- ggplot(pdata %>%
                  pivot_longer(cols = c("Revenue", "Poverty gap"), names_to = "type", values_to = "value") %>% 
                  filter(target == "1.5C",Y %in% c(2030, 2040, 2050), 
                         R_CGE %in% c("India", "Sub-Saharan Africa", "Rest of Asia"), TH1 == "2.15-threshold")) +
  geom_boxplot(mapping = aes(x = type, y = value*100, color = model), width = .8, 
               outliers = FALSE, outlier.alpha = .5, outlier.size = .5) +
  labs(y = "Carbon tax revenue per GDP (%)", 
       x = "Region") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  facet_grid(R_CGE~Y) +
  scale_linetype_manual(values = palette_linetype_revenue) +
  scale_color_manual(values = palette_model)  +
  scale_shape_manual(values = palette_shape_revenue)  +
  guides(color=guide_legend(title="Model"),
         linetype=guide_legend(title="Revenue"),
         shape = guide_legend(title = "Revenue"))
p_121


ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Revenue PoVGap per GDP_box_model.pdf"),
       p_121, height = 12, width = 10, units = "cm")
rm(p_121)


# point
p_121 <- ggplot(pdata %>% filter(target == "1.5C", 
                                 Y %in% c("2030", "2040", "2050", "2060"), 
                                 R_CGE %in% c("India", "Sub-Saharan Africa", "Rest of Asia"),
                                 TH1 == "2.15-threshold")) +
  geom_point(mapping = aes(x = `Poverty gap` * 100, y = `Revenue`*100, color = model, shape = Y), alpha = .5) +
  geom_abline(intercept = 0, slope = 1, color = "grey") +
  labs(y = "Carbon tax revenue per GDP (%)", 
       x = "Poverty gap per GDP (%)") +
  MyTheme + theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1)) +
  facet_wrap(~R_CGE, scale = "free") +
  scale_color_manual(values = palette_model)  +
  guides(color=guide_legend(title="Model"),
         shape = guide_legend(title = "Year"))
p_121


ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Revenue PoVGap per GDP_point_model.pdf"),
       p_121, height = 7, width = 20, units = "cm")
rm(p_121)



# line plot of region level revenue per GDP ----

pdata <- Tx_capita %>% full_join(a, by = c("R", "Y", "model", "gini")) %>% mutate(R_CGE = gsub("R17", "", R)) %>% 
  F_R_CGEfull() %>% 
  filter(!is.na(PoVgap_absExp), grepl("R17",R) | R == "WLD") %>% 
  dplyr::select("model", "Ref","Y","R_CGE","R","TH1","Rev_rate","Unit_rate","revenue","target","gini","PoVgap_GDPExp", "Unit_GDPExp") %>% 
  dplyr::rename("Revenue" = Rev_rate, "Poverty gap" = PoVgap_GDPExp) %>% 
  filter(target == "1.5C", R_CGE %in% c("India", "Sub-Saharan Africa", "Rest of Asia"), gini == "consistent", TH1 == "2.15-threshold", revenue == "Neutral")
  # pivot_longer(cols = c("Revenue", "Poverty gap"), names_to = "type", values_to = "value") 
p_121 <- ggplot() +
  geom_line(pdata, mapping = aes(x = Y, y = `Revenue`*100, group = paste0(model), color = model)) +
  geom_line(pdata %>% filter(model == "MESSAGEix"), mapping = aes(x = Y, y = `Poverty gap`*100, group = paste0(model)),color = "grey", linetype = "dashed") +
  # geom_boxplot(mapping = aes(x = R_CGE, y = PoVgap_GDPExp*100, color = model), width = .8, outlier.alpha = .5, outlier.size = .5, linetype = "dashed") +
  labs(y = "Carbon tax revenue per GDP (%)", 
       x = "Region") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  facet_grid(~R_CGE) +
  scale_color_manual(values = palette_model)  +
  guides(color=guide_legend(title="Model"),
         linetype=guide_legend(title="Type"),)
p_121


# tax revenue
pdata1 <- Tx_capita %>% filter(grepl("R17", R)) %>% mutate(R_CGE = gsub("R17", "", R)) %>% 
  F_R_CGEfull() %>%  filter(target == "1.5C", Y != "2020", R_CGE %in% c("India", "Sub-Saharan Africa", "Rest of Asia"), gini == "consistent", revenue == "Neutral") %>% 
  mutate(Y = factor(Y, levels = c(seq(2030, 2100, 10))) )
pdata2 <- PoVGap %>% filter(grepl("R17", R)) %>% mutate(R_CGE = gsub("R17", "", R)) %>% 
  F_R_CGEfull() %>% filter(target == "1.5C", Y != "2020", TH1 == "2.15-threshold", R_CGE %in% c("India", "Sub-Saharan Africa", "Rest of Asia"), gini == "consistent", revenue == "Neutral") %>% 
  dplyr::select(-c(Ref, revenue, target,  TH)) %>% mutate(Y = factor(Y, levels = c(seq(2030, 2100, 10))) )
colnames(pdata2)
p_121 <- ggplot() +
  geom_line(pdata1, mapping = aes(x = Y, y = `Rev_rate`*100, group = paste0(model), color = model)) +
  geom_line(pdata2, mapping = aes(x = Y, y = `PoVgap_GDPExp`*100, group = paste0(model), color = model),linetype = "dashed") +
  # geom_boxplot(mapping = aes(x = R_CGE, y = PoVgap_GDPExp*100, color = model), width = .8, outlier.alpha = .5, outlier.size = .5, linetype = "dashed") +
  labs(y = "Carbon tax revenue per GDP (%)", 
       x = "Region") +
  MyTheme + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  facet_grid(~R_CGE) +
  scale_color_manual(values = palette_model)  +
  guides(color=guide_legend(title="Model"),
         linetype=guide_legend(title="Type"),)
p_121

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Revenue PoVGap per GDP_line.pdf"),
       p_121, height = 8, width = 16, units = "cm")
rm(p_121)

## revenue recycling v.s. baseline poverty ----
colnames(PoV)
colnames(Tx_capita)
pdata <- Tx_capita  %>% dplyr::select(-POP) %>% 
  full_join(PoV %>% filter(Effect == "Expenditure", target == "No-Miti") %>% mutate(Unit_value = UNIT) %>% 
              dplyr::select(-c(target, revenue, Effect, POP,UNIT,gini)) %>% gdata::rename.vars(c("value", "RoP"), c("PoV_baseline", "RoP_baseline"))) %>% 
  filter(Y == 2030, !is.na(TH1), R != "WLD", !is.na(Ref), target == "1.5C") #%>% filter(is.na(target))
p_12 <- ggplot(pdata) +
  geom_point(mapping = aes(x = RoP_baseline, y = Rev_capita, color = target, shape = gini)) +
  labs(y = "Carbon tax revenue per capita ($/capita)", 
       x = "Poverty rate") +
  MyTheme +
  facet_wrap(~TH1, scales = "free", ncol = 3) +
  guides(color=guide_legend(title="Scenario"),
         # linetype=guide_legend(title="Revenue"),
         shape = guide_legend(title = "Distribution"))
p_12


ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_RoP_Revenue per capita.pdf"),
       p_12, height = 16, width = 34, units = "cm")
rm(p_12,pdata)


# 3. total revenue, poverty gap, and poverty headcount ----

# total revenue and total poverty gap
pdata <- Tx_capita %>% left_join(PoVGap) %>% 
  dplyr::select(Y, R, model, target, revenue, gini, TH1, Rev, Unit_Rev, PoVgap_absExp, Unit_absExp) %>% left_join(Map_r_PHI2Hub) %>% filter(target == "1.5C")
openxlsx::write.xlsx(pdata, file = paste0(dir_output_YSSPpaper,"/csv/1_2_TxRevenue_PoVGap.xlsx"))
rm(pdata)

# share in global poverty gap
y <- c(2030, 2040, 2050, 2060, 2070)
for(th in seq_along(lis_TH_loop)){
    # poverty gap
  pdata0 <- PoVGap %>% filter(Y %in% y,R!= "WLD", grepl("R17", R), target != "2C", gini == "consistent", TH1 == lis_TH_loop[th]) %>%
    dplyr::select("target", "gini", "revenue", "Y", "TH1", "model", "PoVgap_absExp", "Unit_absExp", "R") %>% 
    mutate(R_CGE = gsub("R17","",R), group = case_when(target == "No-Miti" ~ target, target != "No-Miti" ~ paste0(target, "_", revenue))) %>% F_R_CGEfull() %>% 
    mutate(group = factor(group, levels = c("No-Miti", "1.5C_Neutral", "1.5C_EPC")))
  
  p <- ggplot(pdata0) + geom_col(aes(x = Y, y = PoVgap_absExp, fill = R_CGE), position = "fill") +
    facet_wrap(group~model, ncol = 2) + MyTheme + scale_fill_manual(values = palette_R_CGE) + 
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
    labs(x = "Year", y = "Share in global poverty gap (%)", fill = "Region")
  p
  
  ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Share in global poverty gap_",lis_TH_loop[th],".pdf"),
         p, height = 16, width = 14, units = "cm")
  rm(p, pdata0)
  

}


# absolute global poverty gap

pdata0 <- PoVGap %>% filter(Y %in% y, R== "WLD", target != "2C", gini == "consistent") %>%
  dplyr::select("target", "gini", "revenue", "Y", "TH1", "model", "PoVgap_absExp", "Unit_absExp", "R") %>% 
  mutate(R_CGE = gsub("R17","",R), group = case_when(target == "No-Miti" ~ target, target != "No-Miti" ~ paste0(target, "_", revenue))) %>% F_R_CGEfull() %>% 
  mutate(group = factor(group, levels = c("No-Miti", "1.5C_Neutral", "1.5C_EPC")))

p <- ggplot(pdata0 %>% filter(Y %in% c(2030, 2040, 2050))) + 
  geom_col(aes(x = group, y = PoVgap_absExp/1000000000, fill = model), position = "dodge") +
  facet_grid(TH1~Y, scales = "free_y") + MyTheme + scale_fill_manual(values = palette_model) + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  labs(x = "Year", y = "Global poverty gap (Billion)", fill = "Model")
p

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Global poverty gap.pdf"),
       p, height = 14, width = 16, units = "cm")



## poverty gap v.s. total carbon tax revenue ----
colnames(Tx_capita)
colnames(PoVGap)
pdata <- Tx_capita %>% left_join(PoVGap) %>% filter(!is.na(Rev_rate), !is.na(TH1)) %>% 
  dplyr::select(Y, R, model, target, revenue, gini, TH1, Rev_rate, Unit_rate, PoVgap_GDPExp, Unit_GDPExp) %>% 
  mutate(group = case_when(R == "WLD" ~ "World", 
                           grepl("R17",R) ~ "Region",
                           !grepl("R17",R) & R != "WLD" ~ "Country")) %>% 
  left_join(Map_r_PHI2Hub) %>% mutate(R_CGE = case_when(group == "World" ~ "World",
                                                        group == "Region" ~ gsub("R17","",R),
                                                        group == "Country" ~ R_CGE
                                                        )) %>% F_R_CGEfull() %>% F_gini_rename()
pdata[which(pdata$R == "WLD"), which(colnames(pdata) == "R_CGE")] <- "World"

unique(pdata$R)
p <- ggplot(pdata %>% filter(target == "1.5C", Y %in% c(seq(2030, 2070, 10)))) + # , gini == "SSP2"
  geom_point(aes(y = Rev_rate, x = PoVgap_GDPExp, color = R_CGE, alpha = Y, shape = model, size = gini)) +
  geom_abline(intercept = 0, slope = 1, color = "grey") +
  facet_grid(group~TH1, scales = "free") + 
  scale_color_manual(values = palette_R_CGE) + 
  scale_alpha_manual(values = c("2030" = .5, "2040" = 0.3, "2050" = .2, "2060" = .1, "2070" = .07))+
  scale_size_manual(values = c("SSP2" = 1.5, "SSP1" = .8, "SSP3" = .8, "SSP4" = .8, "SSP5" = .8))+
  labs(x = "Poverty gap per GDP (%)", y = "Carbon tax revenue per GDP (%)", color = "Region", alpha = "Year", shape = "Model", size = "SSP") +
  MyTheme + theme(legend.position = "none")
p

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Poverty gap and revenue.pdf"),
       p, height = 12, width = 12, units = "cm")


p_leg <- ggpubr::get_legend(p + theme(legend.position = "right")) %>% ggpubr::as_ggplot()
ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_2_revenue/1_2_Poverty gap and revenue_legend.pdf"),
       p_leg, height = 24, width = 6, units = "cm")

rm(p, p_leg)



rm(df_PoV_Rev, df_PoV_Rev_WLD, PoV, PoV_WLD, PoVGap, Tx_capita, TX_rate_HUB, TX_rate_MSG, TX_tot_MSG, Population)
print("The END of AIMHub_only/1_2_Poverty_revenue.R")
