# This Rscript is for YSSP final report
# This is the poverty assessment part

require(ggpubr)
require(gamstransfer)
require(sf)
# install.packages("R.utils")
# install.packages("collections")
# install.packages("/Library/Frameworks/GAMS.framework/Versions/42/Resources/apifiles/R/gamstransfer/source/gamstransfer_r.tar.gz",
# dependencies=TRUE)
# install.packages("/Library/Frameworks/GAMS.framework/Versions/42/Resources/apifiles/R/gamstransfer/binary/gamstransfer.tgz", type="binary")
# 1. Poverty --------------------------------------------------------------

# MESSAGEix output
PoVExpmsg_tmp <- rgdx.param(AnaExpmsg, "PoVExp") %>% 
  filter(Y %in% seq(2010,2100,5)) %>%
  mutate(Effect = 'Expenditure',
         model = "MESSAGEix") %>% 
  # gdata::rename.vars("Ref", "PHIscenario") %>%
  left_join(MapScenario)


PoVIncmsg_tmp <- rgdx.param(AnaIncmsg, "PoV") %>%
  filter(#Ref %in% lis_ref,
         Y %in% c(seq(2010,2100,5))) %>%
  mutate(Effect = 'Income',
         model = "MESSAGEix") %>%
  # gdata::rename.vars("Ref", "PHIscenario") %>% 
  left_join(MapScenario)


# AIM-Hub output
PoVExpcge_tmp <- rgdx.param(AnaExp, "PoVExp") %>% 
  filter(#Ref %in% lis_ref ,
         Y %in% seq(2010,2100,5)) %>%
  mutate(Effect = 'Expenditure',
         model = "AIM-Hub") %>% 
  # gdata::rename.vars("Ref", "PHIscenario") %>%
  left_join(MapScenario)


PoVInccge_tmp <- rgdx.param(AnaInc, "PoV") %>%
  filter(#Ref %in% lis_ref,
         Y %in% c(seq(2010,2100,5))) %>%
  mutate(Effect = 'Income',
         model = "AIM-Hub") %>%
  # gdata::rename.vars("Ref", "PHIscenario") %>% 
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
  rbind(PoVIncmsg_tmp) %>% 
  rbind(PoVInccge_tmp) %>% 
  mutate(R = recode(R, 
                    "HKG" = "CHN",
                    "TWN" = "CHN")) %>%
  rename.vars(from = 'PoV', to = 'value') %>% 
  dplyr::group_by(model, Ref, Y, R, Effect, TH) %>% 
  dplyr::summarise(value = sum(value), .groups = "drop") %>%
  ungroup() %>% 
  F_TH() %>%
  mutate(value = value/1000000,
         UNIT = "Million people") %>%  # unit: million people
  left_join(Population) %>% F_reve_mutate() %>% filter(!is.na(target)) %>% 
  mutate(RoP = value/POP) %>% 
  filter(Y %in% c(seq(2010,2100,10))) %>% select(-Ref)


openxlsx::write.xlsx(PoV %>% filter(Effect == "Expenditure") %>% left_join(Map_r_PHI2Hub), 
                     file = paste0(dir_output_YSSPpaper, "/csv/00_PoV.xlsx"))

PoV_WLD <- PoV %>% 
  filter(R %in% lis_R_agg)
  


openxlsx::write.xlsx(PoV_WLD %>% filter(Effect == "Expenditure"), 
                     file = paste0(dir_output_YSSPpaper, "/csv/00_PoV_WLD.xlsx"))

dir.create(paste0(dir_output_YSSPpaper, "/RData/"))
save(PoV, Population, file = paste0(dir_output_YSSPpaper, "/RData/Poverty.RData"))

rm(PoVExpmsg_tmp, PoVIncmsg_tmp, PoVExpcge_tmp, PoVInccge_tmp)

# 1.0 Maps ----------------------------------------------------------------

# Data output: the policy impacts of climate policies -----------------
df <- PoV_WLD %>% 
  filter(Y %in% seq(2010, 2070, 10),
         R == "WLD",
         TH1 == txt_plot_TH,
         # model == "MESSAGEix",
         Effect == "Expenditure") %>%
  select("model", "target", "revenue", "gini", "Y", "UNIT", "RoP") %>% 
  pivot_wider(names_from = "target", values_from = "RoP") 

df %>% 
  openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper, "/csv/1_poverty implication policy poverty rate.xlsx"))
rm(df)


# 1.1 Poverty rate Global ----


## line plot: multiple thresholds ----
pdata <- PoV_WLD %>% 
  filter(Effect == "Expenditure",
         R == "WLD",
         TH1 %in% c(lis_TH),target == "No-Miti",
         Y %in% seq(2020, 2100, 10)) %>% mutate(v_group = paste0(model, "_", target, "_", revenue, "_", TH1), v_color = TH1) %>% select(-value) %>% mutate(value = RoP*100) %>% select(-RoP) 


p_1 <- F_plot_ribbon(pdata, txt_y = "Poverty rate (%)") + 
  scale_color_manual(values = palette_TH) +
  scale_fill_manual(values = palette_TH) +
  scale_x_discrete(breaks = seq(2020, 2100,10)) + theme(axis.text.x = element_text(angle = 30)) +
  guides(color = guide_legend(title = "Poverty line"), fill = guide_legend(title = "Poverty line"))
p_1

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_Poverty rate_line_No-Miti.pdf"),
       p_1, height = 8, width = 10, units = "cm")

 rm(p_1, pdata)



### Model compare ----
pdata1 <- PoV_WLD %>% 
  filter(Effect == "Expenditure",
         R == "WLD",
         TH1 %in% c(lis_TH),
         Y %in% c(seq(2020,2100,10))) 

pdata2 <- pdata1 %>% 
  left_join(pdata1 %>% filter(target == "No-Miti") %>% 
              dplyr::rename("RoP_BaU" = "RoP", "value_BaU" = "value") %>% select(c("model", "Y", "R","Effect", "TH", "RoP_BaU", "value_BaU", "gini"))) %>% 
  filter(target %in% c("1.5C", "2C")) %>% 
  mutate(change_RoP = case_when(Y != 2020 ~ RoP - RoP_BaU, Y == 2020 ~0), 
         change_PoP = case_when(Y != 2020 ~ value-value_BaU, Y == 2020 ~0))
pdata2 %>% 
  openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper, "/csv/1_poverty_change_model.xlsx"))

pdata <- pdata2 %>% mutate(v_group = paste0(model, "_", target, "_", revenue)) %>% 
  select("model", "Y", "R", "Effect", "TH1", "change_RoP", "revenue", "target", "gini", "v_group") %>% 
  dplyr::rename("value" = "change_RoP") %>% mutate(value = value * 100, v_color = target)
  
p_1 <- F_plot_ribbon(pdata, txt_y = "Change of poverty rate (%)") + facet_grid(revenue~TH1) + 
  scale_color_manual(values = palette_Sc) +
  scale_fill_manual(values = palette_Sc) +
  geom_abline(intercept = 0, slope = 0, color = "grey") +
  scale_x_discrete(breaks = seq(2020, 2100,10)) + theme(axis.text.x = element_text(angle = 30)) +
  guides(color = guide_legend(title = "Target"), fill = guide_legend(title = "Target"))
p_1

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_Poverty rate_line_change_models.pdf"),
       p_1, height = 10, width = 22, units = "cm")
rm(p_1, pdata, pdata2)


# columns and lines
pdata_box <- pdata1 %>% filter(target == "No-Miti", Y %in% c(2020,2030, 2040, 2050, 2060, 2070)) 
colnames(pdata1)
pdata_line <- pdata1 %>% filter(target != "No-Miti", target != "2C", Y %in% c(2020,2030, 2040, 2050, 2060, 2070)) %>% select(-value) %>% dplyr::rename("value" = "RoP") %>% 
    mutate(v_group = paste0(model, "_", target, "_", revenue), value = value * 100, v_color = revenue)

# ribbon plot
p_1 <- F_plot_ribbon(pdata_line, txt_y = "Poverty rate (%)") + 
  geom_boxplot(pdata_box, mapping = aes(x = Y, y = RoP*100), alpha = .4, color = "grey40") +
  facet_wrap(~TH1, scales = "free") + 
  scale_color_manual(values = palette_color_revenue) +
  scale_fill_manual(values = palette_color_revenue) +
  geom_abline(intercept = 0, slope = 0, color = "grey") +
  scale_x_discrete(breaks = seq(2020, 2070,10)) +
  theme(axis.text.x = element_text(angle = 30)) +
  guides(color = guide_legend(title = "Redistribution"), 
         fill = guide_legend(title = "Redistribution"))
p_1

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/1_Poverty rate_ribbon_models.pdf"),
       p_1, height = 7, width = 20, units = "cm")


# line plot
rm(pdata_box)
pdata_box <- pdata1 %>% filter(target == "No-Miti", Y %in% c(2020,2030, 2040, 2050, 2060, 2070), gini == "consistent", model == "AIM-Hub") 
p_1 <- F_plot_ribbon(pdata_line, ribbon = F, txt_y = "Poverty rate (%)") + 
  geom_col(pdata_box, mapping = aes(x = Y, y = RoP*100), alpha = .4, width = .7, fill = "grey40") +
  facet_wrap(~TH1, scales = "free") + 
  scale_color_manual(values = palette_color_revenue) +
  scale_fill_manual(values = palette_color_revenue) +
  geom_abline(intercept = 0, slope = 0, color = "grey") +
  scale_x_discrete(breaks = seq(2020, 2070,10)) +
  theme(axis.text.x = element_text(angle = 30)) +
  guides(color = guide_legend(title = "Redistribution"), 
         fill = guide_legend(title = "Redistribution"))
p_1

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/1_Poverty rate_line_models.pdf"),
       p_1, height = 7, width = 20, units = "cm")

## area plot ---------------------------------------------------------------
pdata1 <- PoV %>% 
  filter(Effect == "Expenditure",
         !(R %in% c(lis_R_agg, "WLD", "World")), !grepl("R17", R),
         Y %in% c(seq(2020,2100,10))) #%>% F_sce_main()

# test the grouping
a <- pdata1 %>% dplyr::group_by(model, target, revenue, gini, Y, Effect, TH) %>% 
  reframe(POP = sum(POP)) %>% left_join(Population %>% filter(R == "WLD") %>% dplyr::rename("POP_WLD" = "POP"))
rm(a)

pdata2 <- pdata1 %>% filter(target == "No-Miti") %>% select(-c("target", "revenue")) %>% 
  dplyr::rename("RoP_BaU" = "RoP", "value_BaU" = "value")


pdata <- pdata1 %>% left_join(pdata2) %>% left_join(Map_r_PHI2Hub) %>% 
  F_R_CGEfull() %>% 
  dplyr::group_by(model, target, revenue, gini, Y, R_CGE, Effect, TH1) %>% 
  dplyr::reframe(value = sum(value), POP = sum(POP), RoP = value/POP, value_BaU = sum(value_BaU), RoP_BaU = value_BaU/POP) %>% 
  mutate(change_PoP = value - value_BaU, change_RoP = RoP - RoP_BaU) %>% 
  filter(TH1 == c(lis_TH), target != "No-Miti", target == "1.5C")

pdata[which(pdata$Y==2020), which(startsWith(colnames(pdata), "change_"))] <- 0
dir.create(paste0(dir_output_YSSPpaper, "/csv/"))
pdata1 %>% left_join(pdata2) %>% left_join(Map_r_PHI2Hub) %>% 
  F_R_CGEfull() %>% 
  dplyr::group_by(model,target, revenue, gini, Y, R_CGE, Effect, TH1) %>% 
  dplyr::reframe(value = sum(value), POP = sum(POP), RoP = value/POP, value_BaU = sum(value_BaU), RoP_BaU = value_BaU/POP) %>% 
  mutate(change_PoP = value - value_BaU, change_RoP = RoP - RoP_BaU) %>% 
  openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper, "/csv/00_PoV_region.xlsx"))
rm(pdata1, pdata2)

pdata <- pdata %>% filter(gini == "consistent", TH1 == "2.15-threshold")

# defining the plot function for simple column plot for change of population in poverty
F_plot_colR <- function(pdata){
  p <- ggplot(pdata) +
    geom_col(aes(x = as.character(Y) , y = change_PoP, fill = (R_CGE)), position = "stack", width = .9) +
    geom_hline(yintercept = 0, color = "grey70") + 
    facet_grid(revenue~model) +
    MyTheme + theme(legend.key.size = unit(0.42, 'cm'),
                    legend.key.height = unit(0.42, 'cm'), 
                    legend.key.width = unit(0.42, 'cm'),
                    legend.title = element_text(size=10), 
                    legend.text = element_text(size=9),
                    legend.position = "none") +
    labs(x = "Year", y = "Change in poverty headcount (Million)") +
    guides(fill = guide_legend(title = "Region", ncol = 1)) +
    scale_fill_manual(values = palette_R_CGE)
  p
  
}

unique(pdata$target)
unique(pdata$revenue)
unique(pdata$gini)
# Up: Neutral
p <- F_plot_colR(pdata %>% filter(revenue == "Neutral", Y %in% c(2020, 2030, 2040, 2050, 2060, 2070)))  + theme(axis.text.x = element_text(angle = 30))
p

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/1_Poverty rate_area_neutral.pdf"),
       p, height = 6, width = 14, units = "cm")

# Bottom: EPC
p <- F_plot_colR(pdata %>% filter(revenue == "EPC", Y %in% c(2020, 2030, 2040, 2050, 2060, 2070))) + theme(axis.text.x = element_text(angle = 30))
p

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/1_Poverty rate_area_EPC.pdf"),
       p, height = 6, width = 14, units = "cm")

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/1_Poverty rate_area_ylabel.pdf"),
       p, height = 8, width = 14, units = "cm")

 # Legend
p <- p + theme(legend.position = "right")
p_legend <- get_legend(p) %>% as_ggplot()

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/1_Poverty rate_area_legend.pdf"),
       p_legend, height = 8, width = 5, units = "cm")
rm(p, p_legend)


  ### SI model compare ----
p <- ggplot(pdata %>% filter(Y %in% c(2030, 2040, 2050, 2070))) +
  geom_col(aes(x = paste0(Y,"\n", model), y = change_PoP, fill = (R_CGE)), position = "stack", width = 0.6) +
  geom_hline(yintercept = 0, color = "grey70") + 
  facet_wrap(~revenue, scales = "free") +
  MyTheme +
  labs(x = "Year", y = "Addition to poverty headcount (million)", subtitle = paste0("1.5C | ", txt_plot_TH)) +
  guides(fill = guide_legend(title = "Region", ncol = 1)) +
  scale_fill_manual(values = palette_R_CGE)

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_Poverty headcount_col_model.pdf"),
       p, height = 12, width = 36, units = "cm")
rm(p)



print("The END of Compare/1_Poverty.R")
