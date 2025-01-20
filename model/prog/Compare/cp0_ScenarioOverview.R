# This Rscript is for YSSP final report
library(ggpubr)

# Does not involve the redistribution scenarios or the Gini variation scenarios

lis_y <- seq(2020,2070,10)

# 2.0 loading iamc_msg ----------------------------------------------------
# MESSAGEix
iamc_msg <- msg_var %>% 
  rgdx.param("message_iamc") %>%
  gdata::rename.vars(colnames(.), c("model","MSGscenario", "R_MSG", "variable", "unit", "Y", "value")) %>% 
  filter(model == "MESSAGEix-GLOBIOM 1.1-R12") %>%  
  mutate(model = "MESSAGEix") %>% 
  left_join(MapScenario_msg) %>% 
  left_join(Map_R12_R8) %>% F_reve_mutate() %>% 
  filter(!is.na(Ref), !is.na(R8)) %>% F_sce_main() %>% 
  select("model", "R_MSG", "R8", "Y", "target","revenue","gini","variable", "value", "unit") %>% 
  gdata::rename.vars("R_MSG", "R") 




# AIM-Hub
map_VUMAP <- cge_var %>% 
  rgdx.set("VUMAP") %>% 
  gdata::rename.vars(colnames(.), c("variable", "unit"))

iamc_cge_tmp <- cge_var %>% 
  rgdx.param("IAMC_template") %>% 
  mutate(model = "AIM-Hub")

iamc_cge <- iamc_cge_tmp %>% 
  gdata::rename.vars(colnames(.), c("CGEscenario", "R_CGE", "variable","Y", "value", "model")) %>% 
  left_join(MapScenario_cge) %>% 
  left_join(map_VUMAP) %>% 
  left_join(Map_R17_R8) %>% F_reve_mutate() %>% 
  filter(R_CGE %in% c(lis_R_CGE, "World")) %>% F_sce_main() %>% 
  select("model", "R_CGE", "R8", "Y", "target","revenue","gini","variable", "value", "unit") %>% 
  gdata::rename.vars("R_CGE", "R") 

rm(iamc_cge_tmp)
save(iamc_cge, iamc_msg, file = paste0(dir_output_YSSPpaper, "/RData/0_iamc_template.RData"))
load(paste0(dir_output_YSSPpaper, "/RData/0_iamc_template.RData"))
# 2.1 Emission trajectory -------------------------------------------------

GHG_msg <- iamc_msg %>% 
  filter(variable %in% c("Emissions|CO2"))  # c("Emissions|CO2", "Emissions|Kyoto Gases")
GHG_cge <- iamc_cge %>% 
  filter(variable %in% c("Emi_CO2"))    # c("Emissions|CO2", "Emissions|Kyoto Gases")

GHG <- GHG_msg %>% 
  rbind(GHG_cge) %>% 
  mutate(variable = "Emissions|CO2") %>% 
  group_by(model, R8,  Y, target, gini, revenue, variable, unit) %>% 
  dplyr::summarise(value = sum(value), .groups = "drop")

df_GHG_BaU_cge_2020 <- GHG %>% 
  filter( Y %in% c( "2020"), target == "No-Miti", model == "AIM-Hub") %>% 
  pivot_wider(values_from = "value", names_from = "Y") %>% 
  select("R8", "variable", "unit", "2020")
  
## plot ----------

pdata <- GHG %>% 
  filter(Y %in% lis_y) %>% 
  pivot_wider(values_from = "value", names_from = "Y") %>% 
  select(-c( "2020")) %>% 
  left_join(df_GHG_BaU_cge_2020) %>% 
  pivot_longer(as.character(lis_y), values_to = "value", names_to = "Y") %>% F_sce_main(.,EPC = F) %>% 
  filter(R8 %in% lis_R8, R8 != "WLD")

rm(df_GHG_BaU_cge_2020, GHG_msg, GHG_cge)

p_GHG_global <- ggplot(pdata) +
  geom_line(aes(x = Y, y = value/1000, group = paste0(model, "_", target),
                color = target, linetype = model)) +
  geom_abline(slope = 0, intercept = 0, color = "grey80") +
  facet_wrap(~ R8, scales = "free", ncol = 3) +
  labs(x = "Year", y = expression(paste(CO[2], "emissions (Gt/yr)"))) +
  guides(color = guide_legend(title = 'Scenario', ncol = 1),
         linetype = guide_legend(title = 'Model', ncol = 1)) +
  scale_color_manual(values = palette_Sc) +
  scale_x_discrete(breaks = seq(2020, 2100,20)) +
  MyTheme 
p_GHG_global 

ggsave(path = paste0(dir_output_YSSPpaper, "/fig/SI/"), 
       p_GHG_global, filename = "0_CO2_emissions.pdf",
       width = 22, height = 14, units = "cm")

 rm(p_GHG_global, pdata)

# 4. GDP ---------------------------------------------------------------------
df_Pop <- input_base %>% 
  rgdx.param("Population") %>% 
  filter(Ref == "SSP2") %>% 
  mutate(unit = "1") 

df_Pop8 <- input_base %>% 
  rgdx.param("Population") %>% 
  filter(Ref == "SSP2") %>% 
  mutate(unit = "1") %>% 
  left_join(Map_r_PHI2Hub) %>% 
  left_join(Map_R17_R8) %>% 
  filter(!is.na(R8)) %>% 
  group_by(R8, Y) %>% 
  reframe(Pop = sum(Population))
# unit=1


GDP_msg <- iamc_msg %>% 
  filter(variable %in% c("GDP|PPP")) %>% 
  mutate_at("value", ~replace_na(.,0)) %>% 
  select(c("model", "R", "R8", "Y", "target","revenue", "gini", "variable", "unit", "value")) %>% 
  mutate(model = "MESSAGEix")

unique(GDP_msg$R)
unique(GDP_msg$R8)

GDP_cge <- iamc_cge %>% 
  filter(variable %in% c("GDP_PPP")) %>% 
  mutate_at("value", ~replace_na(.,0)) %>% 
  select(c("model", "R", "R8", "Y", "target","revenue", "gini", "variable", "unit", "value")) %>% 
    mutate(model = "AIM-Hub")
 
df_GDP_cap <- GDP_cge %>% 
  rbind(GDP_msg) %>% 
  mutate(variable = "GDP|PPP") %>% 
  group_by(model, R8, Y, target, revenue, gini, variable, unit) %>% 
  reframe(value = sum(value)) %>% 
  left_join(df_Pop8) %>% 
  mutate(value = value/Pop*1000000, unit = "Thousand(2011$) per capita")
  

## plot GDP PPP ----------


pdata <- df_GDP_cap %>% filter(R8 != "WLD", R8 != "World",
                                Y %in% seq(2020, 2100, 10)) 


p <- ggplot() +
  # geom_line(data = pdata,
           # aes(x = Y, y = value, group = paste0(model, scenario), linetype = model, color = scenario)) +
  geom_col(data = pdata %>% filter(target == "No-Miti"),
            aes(x = Y, y = value, fill = model), position = "dodge") +
  labs(x = "Year", y = "GDP per capita （thousand $/yr）",
    # caption="Column: GDP per capita in the No-Miti scenario \nLine: GDP loss"
  )+
  facet_wrap(~R8, ncol = 4) +
  MyTheme +
  theme(axis.text.x = element_text(angle = 60)) +
  scale_color_manual(values = palette_Sc) +
  scale_fill_manual(values = palette_model) +
  guides(fill = guide_legend(title= "Model"))
p

ggsave(p,
       filename = "0_GDPPPP.pdf",
       path = paste0(dir_output_YSSPpaper, "/fig/SI"),
       width = 23, height = 10, units = "cm")


## plot GDP loss lineplot ----------


pdata <- df_GDP_cap %>%
  group_by(model, R8, Y, target, revenue, gini, variable, unit) %>%
  reframe(value = sum(value)) %>%
  filter(R8 != "WLD", R8 != "World",
         Y %in% lis_y) %>% F_sce_main(EPC = F) %>% 
  pivot_wider(values_from = "value", names_from = "target") %>%
  pivot_longer(cols = c("2C", "1.5C"), values_to = "value", names_to = "target") %>%
  mutate(change = (`value`-`No-Miti`)/`No-Miti`*100) 


p <- ggplot() +
  geom_line(data = pdata,
            aes(x = Y, y = change, group = paste0(model, target), linetype = model, color = target)) +
  geom_hline(yintercept = 0, color = "grey80") +
  labs(x = "Year", y = "GDP loss (%)")+
  facet_wrap(~R8, ncol = 3) +
  MyTheme +
  theme(axis.text.x = element_text(angle = 60)) +
  scale_color_manual(values = palette_Sc) +
  guides(linetype = guide_legend(title= "Model"), color = guide_legend(title = "Scenario"))
p

ggsave(p,
       filename = "0_GDPPPP_loss.pdf",
       path = paste0(dir_output_YSSPpaper, "/fig/SI"),
       width = 19, height = 15, units = "cm")



# 5. consumtpion--------------------------------------------------------------- 

load(paste0(dir_output_YSSPpaper, "/RData/0_iamc_template.RData"))
CNS_loss_tmp <- data.frame()
CNS_loss <- data.frame()
for(m in seq_along(lis_model)){
  # m <- 2
  if(lis_model[m] == "AIM-Hub"){
    txt_var <- "Pol_Cos_Cns_Los_rat"
    df_tmp <- iamc_cge
  }else if(lis_model[m] == "MESSAGEix"){
    txt_var <- "Consumption"
    df_tmp <- iamc_msg
  }
  
  # read the data 
  CNS_loss_tmp <- df_tmp %>% 
    filter(variable %in% txt_var) %>% 
    mutate_at("value", ~replace_na(.,0)) %>% 
    dplyr::select(c("model", "R", "R8", "Y", "target", "gini",  "unit", "value")) %>% distinct() %>% mutate(model = lis_model[m])
  
  if(lis_model[m] == "AIM-Hub"){
    CNS_loss_tmp <- CNS_loss_tmp %>% dplyr::rename("R_CGE"= "R", "unit_cnsloss" = "unit", "value_cnsloss" = "value") %>% full_join(Map_r_PHI2Hub) %>% dplyr::select(-R_CGE)
  }else if(lis_model[m] == "MESSAGEix"){
    CNS_loss_tmp <- CNS_loss_tmp %>% left_join(CNS_loss_tmp %>% filter(target == "No-Miti") %>% mutate(value_BaU = value) %>% dplyr::select(-target, -value)) %>% 
      mutate(value_cnsloss = (value_BaU-value)/value_BaU*100, unit_cnsloss = "%") %>% dplyr::rename("R_MSG"= "R")%>% 
      full_join(Map_r_PHI2msg) %>% dplyr::select("model", "R8", "Y", "target", "gini" , "unit_cnsloss" , "value_cnsloss", "R" )
  }
  
  CNS_loss <- CNS_loss %>% rbind(CNS_loss_tmp)
  
}
save(CNS_loss, file = paste0(dir_output_YSSPpaper, "/RData/0_CNS_loss.RData"))


## plot ----------

pdata <- CNS_loss %>% filter(!is.na(model), Y %in% lis_y)

p <- ggplot() +
  geom_line(data = pdata,
            aes(x = Y, y = value_cnsloss, group = paste0(model, target, R), linetype = model, color = target)) +
  geom_hline(yintercept = 0, color = "grey80") +
  labs(x = "Year", y = "GDP loss (%)")+
  facet_wrap(~R8, ncol = 4) +
  MyTheme +
  theme(axis.text.x = element_text(angle = 60)) +
  scale_color_manual(values = palette_Sc) +
  guides(linetype = guide_legend(title= "Model"), color = guide_legend(title = "Scenario"))
p

ggsave(p,
       filename = "0_CNS_loss.pdf",
       path = paste0(dir_output_YSSPpaper, "/fig/SI"),
       width = 26, height = 18, units = "cm")


# 6. Carbon price ---------------------------------------------------------------


PGHG_msg <- iamc_msg %>% 
  filter(variable %in% c("Price|Carbon")) %>% 
  filter(R == "World") %>% 
  select(c("model", "R", "Y",  "target","revenue", "gini", "variable", "unit", "value")) %>% 
  mutate(model = "MESSAGEix") %>% 
  distinct() %>% mutate_at("value", ~replace_na(.,0))

PGHG_cge <- iamc_cge %>% 
  filter(variable %in% c("Prc_Car")) %>% 
  filter(R == "World") %>% 
  select(c("model", "R", "Y",  "target","revenue", "gini", "variable", "unit", "value")) %>% 
  mutate_at("value", ~replace_na(.,0)) %>% 
  mutate(model = "AIM-Hub")

df_PGHG <- PGHG_cge %>% 
  rbind(PGHG_msg) %>% 
  mutate(variable = "Price|Carbon") %>% 
  pivot_wider(names_from = "Y", values_from = "value") %>% 
  mutate_at(c(6:8), ~replace_na(.,0)) %>%
  pivot_longer(c(as.character(lis_y)), names_to = "Y", values_to = "value") %>% 
  select("model", "R", "Y",  "target","revenue", "gini",  "variable", "unit", "value") %>% F_sce_main(EPC = F)

rm(PGHG_cge, PGHG_msg)

# timing of carbon neutrality
a1 <- GHG %>% filter(R8 == "World", model == "AIM-Hub", Y %in% c(2050, 2055), target == "1.5C") %>% 
  pivot_wider(names_from = "Y", values_from = "value") %>% 
  mutate(Y1 = .$`2050`/(.$`2050`-.$`2055`)) %>% mutate(a = Y1*5+2050) %>% select("model", "R8", "target", "variable","a")

a2 <- GHG %>% filter(R8 == "World", model == "AIM-Hub", Y %in% c(2065, 2070), target == "2C") %>% 
  pivot_wider(names_from = "Y", values_from = "value") %>% 
  mutate(Y1 = .$`2065`/(.$`2065`-.$`2070`)) %>% mutate(a = Y1*5+2065) %>% select("model", "R8", "target", "variable","a")

a3 <- GHG %>% filter(R8 == "World", model == "MESSAGEix", Y %in% c(2060, 2070), target == "1.5C") %>% 
  pivot_wider(names_from = "Y", values_from = "value") %>% 
  mutate(Y1 = .$`2060`/(.$`2060`-.$`2070`)) %>% mutate(a = Y1*10+2060) %>% select("model", "R8", "target", "variable","a")

a4 <- GHG %>% filter(R8 == "World", model == "MESSAGEix", Y %in% c(2070, 2080), target == "2C") %>% 
  pivot_wider(names_from = "Y", values_from = "value") %>% 
  mutate(Y1 = .$`2070`/(.$`2070`-.$`2080`)) %>% mutate(a = Y1*10+2070) %>% select("model", "R8", "target", "variable","a")

a <- a1 %>% rbind(a2) %>% rbind(a3) %>% rbind(a4) %>% mutate(a = round(a))
rm(a1, a2, a3, a4)

# plot --------
pdata <- df_PGHG %>%  mutate_at("value", ~replace_na(.,0)) %>% filter(Y %in% lis_y) 

p <- ggplot() +
  geom_line(pdata, mapping = aes(x = Y, y = value, color = target, linetype = model, group = paste0(model, target))) +
  geom_abline(intercept = 0, slope = 0, color = "grey") +
  # geom_point(a, mapping = aes(x = a, y = 0, color = scenario, shape = model)) +
  MyTheme +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(y = expression(paste("Carbon price ($/t ",CO[2], "eq)")), x = "Year")+
  scale_color_manual(values = palette_Sc) +
  guides(color = guide_legend(title="Scenario"), linetype = guide_legend(title="Model"))
p

ggsave(p, filename = "0_PGHG.pdf", path = paste0(dir_output_YSSPpaper, "/fig/SI"), 
       width = 9.5, height = 6.5, units = "cm")
  rm(p, pdata)


# 7. Emission downscaled --------------------------------------------------

GHG_DS_HUB <- rgdx.param(paste0("../", prog_loc, "/data/Emission_DS/AIMHub.gdx"), "EGHG") %>%
  mutate(Unit = "kt CO2eq") %>% 
  gdata::rename.vars(c("EGHG", "Ref"), c("EGHG_HUB", "Ref") )

GHG_DS_MSG <- rgdx.param(paste0("../", prog_loc, "/data/Emission_DS/MESSAGEix.gdx"), "EGHG") %>%
  mutate(Unit = "kt CO2eq") %>% 
  gdata::rename.vars(c("EGHG", "Ref"), c("EGHG_MSG", "Ref") )

GHG_DS <- GHG_DS_HUB %>% left_join(GHG_DS_MSG) %>% F_reve_mutate() %>% F_sce_main() %>% 
  filter(as.numeric(as.character(Y)) %in% lis_y,
         Species == "CO2") %>% 
  left_join(Map_r_PHI2Hub) %>% 
  mutate(R_CGE = ifelse(R == "WLD", "WLD", R_CGE)) %>% 
  F_R_CGEfull()

rm(GHG_DS_HUB, GHG_DS_MSG)
# kt CO2eq
# plot

pdata <- GHG_DS %>% 
  filter(R_CGE != "WLD", R != "WLD", Source == "tot_anthr") %>% 
  F_sce_main(EPC = F)
# c("No-Miti", "2C", "1.5C")

p <- ggplot(pdata) +
  geom_point(aes(x = EGHG_HUB/10^6, y = EGHG_MSG/10^6, color = R_CGE)) +
  geom_hline(yintercept = 0, color = "grey90") +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed") +
  MyTheme +
  facet_grid(~target) +
  theme(legend.position = "bottom") +
  labs(x = expression(paste("AIM-Hub ", CO[2]," emissions (Gt/yr)")),y = expression(paste("MESSAGEix ",CO[2]," emissions (Gt/yr)"))) +
  scale_color_manual(values = palette_R_CGE)
p

ggsave(p, path = paste0(dir_output_YSSPpaper, "/fig/SI/"), filename = "0_emissions_model.pdf",width = 30, height = 12, units = "cm")

rm(p, pdata)



# plot national emission pathways to see how the emission pathways differ

pdata <- GHG_DS %>% 
  filter(R_CGE %in% c("WLD", "USA", "CHN", "JPN", "IND", "BRA")) %>% #
  filter(Source == "tot_anthr") %>% F_sce_main() %>% 
  pivot_longer(cols = c("EGHG_HUB", "EGHG_MSG"),names_to = "model", values_to = "value") %>% 
  group_by(target, revenue, gini, model, Y, Species, Source, Unit, R_CGE) %>% 
  dplyr::summarise(value = sum(value), .groups = "drop")

pdata <- GHG_DS %>% 
  filter(R %in% c("KOR", "FRA", "DEU", "IDN", "MLS", "COG", "EGY", "ARG", "CHL","AUT", "MNG", "AFG")) %>% 
  filter(Source == "tot_anthr") %>% F_sce_main() %>% 
  pivot_longer(cols = c("EGHG_HUB", "EGHG_MSG"),names_to = "model", values_to = "value")

p <- ggplot(pdata %>% filter(target %in% c("No-Miti", "1.5C", "2C"))) +
  geom_line(aes(x = Y, y = value/10^6, group = paste0(model, target), 
                color = target, linetype = model)) +
  geom_hline(yintercept = 0, color = "grey90") +
  # geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed") +
  MyTheme +
  facet_wrap(~R, scales = "free") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 60)) +
  labs(x = "Year",y = expression(paste(CO[2]," emissions (Gt/y)"))) +
  scale_color_manual(values = palette_Sc)
p

rm(p, pdata)

# map
# aa <- df_Pop
# unique(pdata$R)
pdata0 <- GHG_DS %>%
  left_join(df_Pop %>% select(-"Ref")) %>% 
  select("target", "revenue", "gini", "Y", "R", "EGHG_HUB", "EGHG_MSG", "Unit", "Population", "Species", "Source") %>% 
  distinct() %>% filter(!is.na(Population)) %>% 
  pivot_longer(c("EGHG_HUB", "EGHG_MSG"), names_to = "model", values_to = "value_plot") %>% 
  mutate(model = recode(.$model, "EGHG_HUB" = "AIM-Hub", "EGHG_MSG" = "MESSAGEix"), value_plot = value_plot/Population*1000, Unit = "t CO2 per capita")

# s <- "FFI"
for(s in c("tot_anthr", "FFI")){
  if(s == "tot_anthr"){
    tx_caption <- expression(paste("Total anthropogenic ", CO[2], " emissions"))
  }else{
    tx_caption <- expression(paste("FFI ", CO[2], " emissions"))
  }
  
  for(m in c("AIM-Hub", "MESSAGEix")){
    if( m == "AIM-Hub"){
      tx_m <- "AIMHub"
    }else{
      tx_m <- "MESSAGEix"
    }
    # s <- "FFI"
    # m <- "MESSAGEix"
    pdata <- pdata0 %>% filter(Species == "CO2", Source == s) %>% F_sce_main(EPC = F) %>% 
      filter( Y %in% c("2030", "2050"), R != "WLD", model == m) %>% left_join(Map_r_PHI2Hub) %>% mutate(scenario = target)
    
    p <- F_plot_globalMap(data=pdata, lis_plot_y=c("2030", "2050"), lis_plot_scenario = c("No-Miti", "1.5C", "2C"),
                          by = c("R"),legendName = expression(paste(CO[2], " emissions (t/capita/yr)"))) +
      scale_fill_gradientn(limits = c(-10,50), #values = c(0, 10, 20, 30, 40, 50),
        colours = c("#8ece6d", "white", "#30b0e0", "#ffe691","#9696b2", "#a82229", "#441a21"))  + #"#30b0e0",,"#6d9b8e" "#f8b976", 
      facet_grid(scenario~Y) +
      labs(subtitle = tx_caption)
    
    p
    
    ggsave(p, path = paste0(dir_output_YSSPpaper, "/fig/SI"), filename =paste0("0_CO2emissions_map_", s, "_", tx_m, ".pdf"), 
           width = 24, height = 20, units = "cm")
    rm(p, pdata)
    }
  
}



# 8. Commodity prices -----------------------------------------------------
df_Price_tmp <- data.frame()
df_Price <- data.frame()
for(m in seq_along(lis_model)){
  if(lis_model[m] == "AIM-Hub"){
    txt_m <- "AIMHub"
  }else{
    txt_m <- lis_model[m] 
  }
    gdx_in <- paste0("../output/gdx/ConsumptionResults/ConsumptionResults_", txt_m,".gdx")
    df_Price_tmp <- rgdx.param(gdx_in, "PQchange") %>% 
      gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "R3", "I", "value")) %>% 
      F_reve_mutate() %>% 
      F_sce_main() %>% filter(revenue == "Neutral", Y %in% c(seq(2020, 2070, 10))) %>% 
      left_join(Map_r_PHI2Hub) %>% 
      select("R", "R_CGE", "Y", "target","revenue", "I","gini", "value") %>%
      mutate(model = lis_model[m]) %>%
      mutate_at("value", ~replace_na(.,0)) %>% 
      mutate_at("value", ~replace(., is.infinite(.), 0))
    
    df_Price <- df_Price %>% rbind(df_Price_tmp)
  
}


palette_color_y <- c("2030" = "#1d3557", "2040" = "#457b9d", "2050" = "#00b4d8", "2070" = "#90e0ef")
pdata <- df_Price %>% left_join(Map_R17_R8) %>% filter(Y %in% c("2030", "2040", "2050", "2070")) %>% 
  pivot_wider(names_from = "target", values_from = "value") %>% 
  pivot_longer(cols = c("1.5C", "2C"), names_to = "target", values_to = "value") %>%
  mutate(change = (`value`-`No-Miti`)/`No-Miti`*100) %>% left_join(MapI) %>% mutate(I_abb = factor(I_abb, levels = lis_I_abb))

p <- ggplot() +
  geom_boxplot(data = pdata %>% filter(target == "1.5C"), aes(x = I_abb, y = change, color = Y), outlier.alpha = .5) +
  # facet_grid(R8~.) +
  geom_hline(yintercept = 0, color = "grey") +
  labs(x = "Commodity", y = "Price change (%)", color = "Year") +
  scale_color_manual(values = palette_color_y) +
  MyTheme + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1), legend.position = "right") 
p

ggsave(p, path = paste0(dir_output_YSSPpaper, "/fig/SI"), filename = "0_PriceChange_global.pdf", 
       width = 18, height = 10, units = "cm")
# export to RData ----
df_GDP <- df_GDP_cap %>% mutate(value_tot = value * Pop, unit_value_tot = "Thousand(2011$)")
save(df_PGHG, GHG, df_GDP, GHG_DS, file = paste0(dir_output_YSSPpaper, "/RData/1_GHG and PGHG.RData"))




rm(pdata0, iamc_cge_tmp, map_VUMAP, iamc_msg,iamc_cge, df_Price, GHG, df_GDP_cap, df_PGHG, df_Pop, df_Pop8, GDP_cge, GDP_msg, GHG_DS, a)
print("The END of AIMHub_only/0_ScenarioOverview.R")


