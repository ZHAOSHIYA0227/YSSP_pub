# This is the poverty assessment part
# Shiya ZHAO, 2023/07/12
require(ggpubr)
require(gamstransfer)
require(sf)


# 1. Poverty --------------------------------------------------------------

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
  filter(scenario %in% lis_scenario)%>% 
  mutate(value = value/1000000,
         UNIT = "Million people") %>%  # unit: million people
  left_join(Population) %>% 
  mutate(RoP = value/POP) %>% 
  filter(Y %in% c(seq(2010,2100,10))) %>% 
  F_reve_mutate() 

openxlsx::write.xlsx(PoV %>% filter(Effect == "Expenditure"), 
                     file = paste0(dir_out, "/csv/00_PoV.xlsx"))

PoV_WLD <- PoV %>% 
  filter(R %in% lis_R_agg)
  


openxlsx::write.xlsx(PoV_WLD %>% filter(Effect == "Expenditure"), 
                     file = paste0(dir_out, "/csv/00_PoV_WLD.xlsx"))

rm(PoVExpmsg_tmp, PoVIncmsg_tmp, PoVExpcge_tmp, PoVInccge_tmp)

# 1.0 Maps ----------------------------------------------------------------

# Data output: the policy impacts of climate policies -----------------
df <- PoV_WLD %>% 
  filter(Y %in% seq(2010, 2060, 10),
         R == "WLD",
         TH1 == "1.9-threshold",
         Effect == "Expenditure") %>%
  select("model", "scenario", "Y", "UNIT", "RoP") %>% 
  pivot_wider(names_from = "scenario", values_from = "RoP") 

df %>% 
  openxlsx::write.xlsx(file = paste0(dir_out, "/csv/1_poverty implication policy poverty rate.xlsx"))

# 1.1 Poverty rate Global ----


## col+line plot ----
### SI model compare ----
pdata1 <- PoV_WLD %>% 
  filter(Effect == "Expenditure",
         R == "WLD",
         scenario %in% lis_scenario,
         TH1 %in% c(lis_TH),
         Y %in% c(2020, 2030, 2050, 2070))
pdata <- pdata1 %>% 
  left_join(pdata1 %>% filter(scenario_Name == "No-Miti") %>% 
              dplyr::rename("RoP_BaU" = "RoP", "value_BaU" = "value") %>% select(c("model", "Y", "R","Effect", "TH", "RoP_BaU", "value_BaU"))) %>% 
  filter(scenario_Name %in% c("1.5C", "2C")) %>% 
  mutate(change_RoP = case_when(Y != 2020 ~ RoP - RoP_BaU, Y == 2020 ~0), 
         change_PoP = case_when(Y != 2020 ~ value-value_BaU, Y == 2020 ~0))
pdata %>% 
  openxlsx::write.xlsx(file = paste0(dir_out, "/csv/1_poverty_change_model.xlsx"))
rm(pdata1)

p_1 <- ggplot() + 
  geom_line(data = pdata, mapping = aes(x = Y, y = change_RoP *100,
                group = paste0(model, "_", scenario, "_", TH1, "_", revenue),
                linetype = model,
                color = scenario_Name)) +
  geom_point(data = pdata,
             aes(x = Y, y = change_RoP * 100, shape = model, color = scenario_Name),
             size = 1) +
  geom_hline(yintercept = 0, color = "grey70") +
  MyTheme +
  facet_grid(revenue~ TH1, scales = "free") +
  labs(x = "Year", y = "Change in poverty rate | % points") +
  scale_x_discrete(breaks=seq(2020, 2100, 10)) +
  scale_color_manual(values = palette_Sc)  +
  guides(color=guide_legend(title="Scenario"),
         linetype=guide_legend(title="Model"),
         shape = guide_legend(title = "Model"))

p_1
ggsave(filename = paste0(dir_out, "/fig/SI/1_Poverty rate_col_line_models.pdf"),
       p_1, height = 10, width = 22, units = "cm")
rm(p_1, pdata)


# AIM-Hub
pdata <- PoV_WLD %>% 
  filter(Effect == "Expenditure",
         model == "AIM-Hub",
         R == "WLD",
         scenario %in% lis_scenario,
         TH1 %in% c(lis_TH),
         Y %in% c(2020, 2030, 2050, 2070)) 

p_1 <- ggplot() + 
  geom_col(data = pdata %>% filter(scenario_Name == "No-Miti"),
           aes(x = Y, y = RoP * 100),fill = "grey", alpha = 0.6, width = 0.5) +
  geom_line(data = pdata %>% filter(scenario_Name %in% c("1.5C", "2C")),
            mapping = aes(x = Y, y = RoP *100,
                          group = paste0(model, "_", scenario, "_", TH1, "_", revenue),
                          linetype = revenue,
                          color = scenario_Name)) +
  geom_point(data = pdata %>% filter(scenario_Name %in% c("1.5C", "2C")),
             aes(x = Y, y = RoP * 100, shape = revenue, color = scenario_Name),
             size = 2) +
  MyTheme +
  facet_grid(~ TH1, scales = "free")  +
  labs(x = "Year", y = "Poverty rate | %") +
  scale_x_discrete(breaks=seq(2020, 2100, 10)) +
  scale_linetype_manual(values = palette_linetype_revenue) +
  scale_color_manual(values = palette_Sc)  +
  scale_shape_manual(values = palette_shape_revenue)  +
  guides(color=guide_legend(title="Scenario"),
         linetype=guide_legend(title="Revenue"),
         shape = guide_legend(title = "Revenue"))

p_1
ggsave(filename = paste0(dir_out, "/fig/1_Poverty rate_col_line.pdf"),
       p_1, height = 6, width = 20, units = "cm")




## area plot ---------------------------------------------------------------
pdata1 <- PoV %>% 
  filter(Effect == "Expenditure",
         !(R %in% lis_R_agg),
         # model == "AIM-Hub",
         scenario %in% lis_scenario,
         # TH1 %in% c(lis_TH),
         Y %in% c(2020, 2030, 2050,  2070))
# test the grouping
a <- pdata1 %>% dplyr::group_by(model, scenario, Y, Effect, TH) %>% reframe(POP = sum(POP)) %>% left_join(Population %>% filter(R == "WLD") %>% dplyr::rename("POP_WLD" = "POP"))
rm(a)

pdata2 <- pdata1 %>% filter(scenario == "No-Miti") %>% select(-c("scenario", "scenario_Name", "revenue")) %>% dplyr::rename("RoP_BaU" = "RoP", "value_BaU" = "value")

pdata <- pdata1 %>% left_join(pdata2) %>% left_join(Map_r_PHI2Hub) %>% 
  F_R_CGEfull() %>% 
  dplyr::group_by(model, scenario, Y, R_CGE, Effect, TH1) %>% 
  dplyr::reframe(value = sum(value), POP = sum(POP), RoP = value/POP, value_BaU = sum(value_BaU), RoP_BaU = value_BaU/POP) %>% 
  mutate(change_PoP = value - value_BaU, change_RoP = RoP - RoP_BaU) %>% 
  F_reve_mutate() %>% filter(TH1 == c("1.9-threshold", "3.2-threshold", "5.5-threshold"), scenario != "No-Miti", scenario_Name == "1.5C")

pdata[which(pdata$Y==2020), which(startsWith(colnames(pdata), "change_"))] <- 0
dir.create(paste0(dir_out, "/csv/"))
pdata1 %>% left_join(pdata2) %>% left_join(Map_r_PHI2Hub) %>% 
  F_R_CGEfull() %>% 
  dplyr::group_by(model, scenario, Y, R_CGE, Effect, TH1) %>% 
  dplyr::reframe(value = sum(value), POP = sum(POP), RoP = value/POP, value_BaU = sum(value_BaU), RoP_BaU = value_BaU/POP) %>% 
  mutate(change_PoP = value - value_BaU, change_RoP = RoP - RoP_BaU) %>% 
  F_reve_mutate() %>% openxlsx::write.xlsx(file = paste0(dir_out, "/csv/00_PoV_region.xlsx"))
rm(pdata1, pdata2)


### SI model compare ----
p <- ggplot(pdata %>% filter(Y %in% c(2030, 2050))) +
  geom_col(aes(x = paste0(Y,"\n", model), y = change_PoP, fill = (R_CGE)), position = "stack", width = 0.6) +
  geom_hline(yintercept = 0, color = "grey70") + 
  facet_wrap(~revenue, scales = "free") +
  MyTheme +
  labs(x = "Year", y = "Addition to poverty headcount | million", subtitle = paste0("1.5C | ", txt_plot_TH)) +
  guides(fill = guide_legend(title = "Region", ncol = 1)) +
  scale_fill_manual(values = palette_R_CGE)
p

ggsave(filename = paste0(dir_out, "/fig/SI/1_Poverty rate_area_model.pdf"),
       p, height = 12, width = 24, units = "cm")


# 1.2 Trace back population -----------------------------------------------

EV <- rgdx.param(demandcge, "EV") %>% 
  gdata::rename.vars(colnames(.), c("R", "PHIscenario", "Seg", "Y", "EV")) %>% 
  left_join(MapScenario) %>% 
  filter( scenario %in% lis_scenario,
          Y %in% lis_y) %>% 
  SegNum() %>% 
  select(R, scenario, Y, Upper, EV) %>% 
  mutate(BaU_income = Upper/(1+EV)) %>% 
  filter(BaU_income > 50) %>% 
  filter(Upper %in% c(690, 790, 1200, 1300, 2500, 2000)) %>% 
  mutate(model = "AIM-Hub",
         TH1 = case_when(Upper == 690 ~ "1.9-threshold", 
                         Upper == 790 ~ "2.15-threshold",
                         Upper == 1300 ~ "3.65-threshold",
                         Upper == 2500 ~ "6.85-threshold",
                         Upper == 1200 ~ "3.2-threshold",
                         Upper == 2000 ~ "5.5-threshold")) %>% 
  F_reve_mutate()



EV %>% 
  filter(R != "WLD") %>%
  left_join(Map_r_PHI2Hub) %>% F_R_CGEfull() %>% 
  mutate(value_plot = BaU_income/365) %>% openxlsx::write.xlsx(file = paste0(dir_out, "/csv/1_poverty_vulnerability_threshold.xlsx"))


rm(df, Population, PoV, PoV_WLD, EV)


print("The END of main/1_Poverty.R")
