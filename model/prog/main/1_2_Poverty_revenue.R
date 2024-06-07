# this script calculates the poverty headcount, poverty gap, and revenue redistribution
# Shiya ZHAO, 2024.01.12



# 0. load data ---------------------------------------------------------
## Poverty gap per GDP ----
# Poverty gap per GDP
# AIM-Hub
PoVGap_tmp <- rgdx.param(AnaExp, "PoVgap_GDPExp") %>% 
  filter(#Ref %in% lis_ref ,
    Y %in% seq(2010,2100,5)) %>% 
  mutate(Effect = 'Expenditure',
         model = "AIM-Hub") %>% 
  gdata::rename.vars("Ref", "PHIscenario") %>%
  left_join(MapScenario)

# messageix
PoVGapmsg_tmp <- rgdx.param(AnaExpmsg, "PoVgap_GDPExp") %>% 
  filter(#Ref %in% lis_ref ,
    Y %in% seq(2010,2100,5)) %>%
  mutate(Effect = 'Expenditure',
         model = "MESSAGEix") %>% 
  gdata::rename.vars("Ref", "PHIscenario") %>%
  left_join(MapScenario)



# Poverty gap absolute
# AIM-Hub
PoVGap_absExp_tmp <- rgdx.param(AnaExp, "PoVgap_absExp") %>% 
  filter(#Ref %in% lis_ref ,
    Y %in% seq(2010,2100,5)) %>% 
  mutate(Effect = 'Expenditure',
         model = "AIM-Hub") %>% 
  gdata::rename.vars("Ref", "PHIscenario") %>%
  left_join(MapScenario)

# messageix
PoVGapmsg_absExp_tmp <- rgdx.param(AnaExpmsg, "PoVgap_absExp") %>% 
  filter(#Ref %in% lis_ref ,
    Y %in% seq(2010,2100,5)) %>%
  mutate(Effect = 'Expenditure',
         model = "MESSAGEix") %>% 
  gdata::rename.vars("Ref", "PHIscenario") %>%
  left_join(MapScenario)


PoVGap <- PoVGap_tmp %>% rbind(PoVGapmsg_tmp) %>% select("scenario", "R", "Y", "TH", "model", "PoVgap_GDPExp") %>% mutate(Unit_GDPExp = "1") %>% 
  left_join(PoVGap_absExp_tmp %>% rbind(PoVGapmsg_absExp_tmp) %>% 
              select("scenario", "R", "Y", "TH", "model", "PoVgap_absExp") %>% mutate(Unit_absExp = "$")) %>% 
  F_TH()

rm(PoVGap_tmp, PoVGapmsg_tmp, PoVGap_absExp_tmp, PoVGapmsg_absExp_tmp)



## Poverty headcount ----
# MESSAGEix output
PoVExpmsg_tmp <- rgdx.param(AnaExpmsg, "PoVExp") %>% 
  filter(#Ref %in% lis_ref ,
    Y %in% seq(2010,2100,5)) %>%
  mutate(Effect = 'Expenditure',
         model = "MESSAGEix") %>% 
  gdata::rename.vars("Ref", "PHIscenario") %>%
  left_join(MapScenario)


# AIM-Hub output
PoVExpcge_tmp <- rgdx.param(AnaExp, "PoVExp") %>% 
  filter(#Ref %in% lis_ref ,
    Y %in% seq(2010,2100,5)) %>%
  mutate(Effect = 'Expenditure',
         model = "AIM-Hub") %>% 
  gdata::rename.vars("Ref", "PHIscenario") %>%
  left_join(MapScenario)


# Combine the two data

Population <- input_base %>% 
  rgdx.param("Population") %>% 
  mutate(POP = Population/1000000,
         UNIT = "Million people",
         R = recode(R, 
                    "HKG" = "CHN",
                    "TWN" = "CHN")) %>%
  dplyr::group_by(Ref, Y, R) %>% 
  dplyr::summarise(POP = sum(POP), .groups = "drop") %>% 
  ungroup() %>% 
  filter(Ref == "SSP2") %>% 
  select(-c("Ref")) 
# unit = Million people

PoV <- PoVExpmsg_tmp %>% 
  rbind(PoVExpcge_tmp) %>% 
  rename.vars(from = 'PoVExp', to = 'PoV') %>%
  mutate(R = recode(R, 
                    "HKG" = "CHN",
                    "TWN" = "CHN")) %>%
  rename.vars(from = 'PoV', to = 'value') %>% 
  dplyr::group_by(model, scenario, Y, R, Effect, TH) %>% 
  dplyr::summarise(value = sum(value), .groups = "drop") %>%
  ungroup() %>% 
  F_TH() %>%
  filter(scenario %in% lis_scenario,
  )%>%
  mutate(value = value/1000000,
         UNIT = "Million people") %>%  # unit: million people
  left_join(Population) %>% 
  mutate(RoP = value/POP) %>% 
  filter(Y %in% c(seq(2010,2100,10))) %>% 
  F_reve_mutate() 


PoV_WLD <- PoV %>% 
  dplyr::group_by(model, scenario, revenue, scenario_Name, Y, Effect, TH1, UNIT) %>% 
  dplyr::reframe(value = sum(value), POP = sum(POP), RoP = value/POP) 

rm(PoVExpmsg_tmp, PoVExpcge_tmp)



## Carbon tax revenue ----

TX_HUB <- rgdx.param(paste0("../../data/PHIinputdata/AIMHub.gdx"), "Tx_revenue_capita") %>%
  mutate(Unit = "US$2010 per capita", model = "AIM-Hub") %>% 
  gdata::rename.vars(c("Tx_revenue_capita", "Ref"), c("Rev_capita", "PHIscenario")) %>% mutate(Unit_Rev = "$")

TX_MSG <- rgdx.param(paste0("../../data/PHIinputdata/MESSAGEix.gdx"), "Tx_revenue_capita") %>%
  mutate(Unit = "US$2010 per capita", model = "MESSAGEix") %>% 
  gdata::rename.vars(c("Tx_revenue_capita", "Ref"), c("Rev_capita", "PHIscenario")) %>% mutate(Unit_Rev = "$") 

TX_tot_HUB <- rgdx.param(paste0("../../data/PHIinputdata/AIMHub.gdx"), "Tx_revenue") %>%
  mutate(Unit = "US$2010 per capita", model = "AIM-Hub") %>% 
  gdata::rename.vars(c("Tx_revenue", "Ref"), c("Rev", "PHIscenario")) %>% mutate(Unit_Rev = "$")

TX_tot_MSG <- rgdx.param(paste0("../../data/PHIinputdata/MESSAGEix.gdx"), "Tx_revenue") %>%
  mutate(Unit = "US$2010 per capita", model = "MESSAGEix") %>% 
  gdata::rename.vars(c("Tx_revenue", "Ref"), c("Rev", "PHIscenario")) %>% mutate(Unit_Rev = "$")

TX_rate_HUB <- rgdx.param(paste0("../../data/PHIinputdata/AIMHub.gdx"), "Tx_revenue_rate") %>%
  mutate(Unit = "US$2010 per capita", model = "AIM-Hub") %>% 
  gdata::rename.vars(c("Tx_revenue_rate", "Ref"), c("Rev_rate", "PHIscenario")) %>% mutate(Unit_rate = "1")

TX_rate_MSG <- rgdx.param(paste0("../../data/PHIinputdata/MESSAGEix.gdx"), "Tx_revenue_rate") %>%
  mutate(Unit = "US$2010 per capita", model = "MESSAGEix") %>% 
  gdata::rename.vars(c("Tx_revenue_rate", "Ref"), c("Rev_rate", "PHIscenario")) %>% mutate(Unit_rate = "1")

Tx_capita <- TX_HUB %>%  
  rbind(TX_MSG) %>% 
  left_join(TX_tot_HUB %>% rbind(TX_tot_MSG)) %>% 
  left_join(TX_rate_HUB %>% rbind(TX_rate_MSG)) %>%  
  left_join(MapScenario) %>% 
  filter(as.numeric(as.character(Y)) %in% seq(2020, 2100, 10),
         Type == "PPP",
         scenario %in% lis_scenario) %>% 
  select(-c("Type", "PHIscenario", "CGEscenario", "MSGscenario")) %>%
  left_join(Population) %>% 
  F_reve_mutate()

Tx_capita_WLD <- Tx_capita %>% 
  group_by(model, scenario, Y) %>% 
  filter(!is.na(POP)) %>% 
  dplyr::summarise(Rev_capita = sum(Rev_capita*POP)/sum(POP),.groups = "drop")


rm(TX_tot_HUB, TX_rate_HUB, TX_MSG, TX_HUB)


df_PoV_Rev <- PoV %>% 
  select(model, scenario, R, Y, Effect, TH1, value, POP) %>% #view
  filter(Effect == "Expenditure",
         scenario %in% lis_scenario,
         Y %in% seq(2010, 2100,10)) %>% 
  pivot_wider(values_from = "value", names_from = "scenario") %>% # view
  pivot_longer(cols = c("1.5C", "1.5C_EPC", "2C", "2C_EPC"), values_to = "value", names_to = "scenario") %>% 
  mutate(`addition` = value-`No-Miti`,
         # addition=case_when(abs(addition) < 10 ~ 0,
         #                    abs(addition) >=10 ~ addition)
  ) %>% 
  F_reve_mutate() %>% 
  left_join(Tx_capita) %>% 
  mutate(Rev_capita = case_when(is.na(Rev_capita) ~ 0, 
                                !is.na(Rev_capita) ~ Rev_capita)) %>% 
  select(-"Effect")

df_PoV_Rev[which(df_PoV_Rev$Y == 2020), which(colnames(df_PoV_Rev) == "addition")] <- 0 #%>% filter(TH1 == "1.9-threshold")

df_PoV_Rev_WLD <- PoV_WLD %>% 
  select(model, scenario, Y, Effect, TH1, value, POP) %>% #view
  filter(Effect == "Expenditure",
         scenario %in% lis_scenario,
         Y %in% seq(2010, 2100,10)) %>% 
  pivot_wider(values_from = "value", names_from = "scenario") %>% # view
  pivot_longer(cols = c("1.5C", "1.5C_EPC", "2C", "2C_EPC"), values_to = "value", names_to = "scenario") %>% 
  mutate(`addition` = value-`No-Miti`,
         addition=case_when(abs(addition) < 1 ~ 0,
         abs(addition) >=1 ~ addition)
  ) %>% 
  filter(!is.na(`No-Miti`)) %>% 
  left_join(Tx_capita_WLD) %>% 
  mutate(Rev_capita = case_when(is.na(Rev_capita) ~ 0, 
                                !is.na(Rev_capita) ~ Rev_capita)) %>% 
  F_reve_mutate() %>% 
  select(-"Effect")

df_PoV_Rev_WLD[which(df_PoV_Rev_WLD$Y == 2020), which(colnames(df_PoV_Rev_WLD) == "addition")] <- 0 #%>% filter(TH1 == "1.9-threshold")



# 1. revenue v.s. poverty reduction ----

## revenue recycling ----
pdata <- df_PoV_Rev %>% 
  select("model", "R", "Y", "TH1", "scenario_Name", "revenue","addition","Rev_capita") %>% #view
  pivot_wider(names_from = "revenue", values_from = "addition") %>% 
  left_join(Map_r_PHI2Hub) %>% 
  filter(Y %in% c(2030, 2050, 2070), !is.na(R_CGE), R_CGE != "WLD") 

p_12 <- ggplot(pdata) +
  geom_boxplot(mapping = aes(x = R_CGE, y = Rev_capita, color = scenario_Name)) +
  labs(y = "Carbon tax revenue per capita| US$2010/capita", 
       x = "Year") +
  MyTheme +
  facet_wrap(Y~model, scales = "free", ncol = 2) +
  scale_linetype_manual(values = palette_linetype_revenue) +
  scale_color_manual(values = palette_Sc)  +
  scale_shape_manual(values = palette_shape_revenue)  +
  guides(color=guide_legend(title="Scenario"),
         linetype=guide_legend(title="Revenue"),
         shape = guide_legend(title = "Revenue"))
p_12


ggsave(filename = paste0(dir_out, "/fig/SI/1_2_Revenue per capita_box_model.pdf"),
       p_12, height = 16, width = 34, units = "cm")
rm(p_12,pdata)



# 2. total revenue, poverty gap, and poverty headcount ----

# total revenue and total poverty gap
pdata <- Tx_capita %>% left_join(PoVGap) %>% 
  select(Y, R, model, scenario, TH1, Rev, Unit_Rev, PoVgap_absExp, Unit_absExp) %>% left_join(Map_r_PHI2Hub) %>% filter(scenario == "1.5C")
openxlsx::write.xlsx(pdata, file = paste0(dir_out,"/csv/1_2_TxRevenue_PoVGap.xlsx"))
rm(pdata)

print("The END of main/1_2_Poverty_revenue.R")
