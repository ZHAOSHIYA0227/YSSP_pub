# This Rscript is for scenario overview
library(ggpubr)

# 1.0 loading scenario data ----------------------------------------------------
# MESSAGEix
iamc_msg <- msg_var %>% 
  rgdx.param("message_iamc") %>%
  gdata::rename.vars(colnames(.), c("model","MSGscenario", "R_MSG", "variable", "unit", "Y", "value")) %>% 
  filter(model == "MESSAGEix-GLOBIOM 1.1-R12") %>%  
  mutate(model = "MESSAGEix") %>% 
  left_join(MapScenario) %>% 
  left_join(Map_R12_R8) %>% 
  filter(scenario %in% lis_scenario, !is.na(R8)) %>% 
  select("model", "R_MSG", "R8", "Y", "scenario","variable", "value", "unit") %>% 
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
  left_join(MapScenario) %>% 
  left_join(map_VUMAP) %>% 
  left_join(Map_R17_R8) %>% 
  filter(scenario %in% lis_scenario, R_CGE %in% c(lis_R_CGE, "World")) %>% 
  select("model","R_CGE", "R8", "Y", "scenario","variable", "value", "unit") %>% 
  gdata::rename.vars("R_CGE", "R") 



# 2. Price: Energy ----------------------------------------------------------------

Price_msg <- input_msg %>% 
  rgdx.param("PriceChange") %>% 
  mutate(model = "MESSAGEix")

Price_cge <- input_cge %>% 
  rgdx.param("PriceChange") %>% 
  mutate(model = "AIM-Hub")

df_Price <- Price_msg %>% 
  rbind(Price_cge) %>% 
  left_join(Map_r_PHI2Hub) %>% 
  dplyr::rename("PHIscenario" = "Ref") %>% 
  left_join(MapScenario) %>% 
  filter(!is.na(scenario)) %>% 
  select("model", "R", "R_CGE", "Y", "I", "scenario", "PriceChange") %>% 
  filter(!is.na(R_CGE)) %>% 
  mutate(R = case_when(R == "HKG" ~ "CHN", R == "MAC" ~ "CHN",
                       !(R %in% c("HKG", "MAC")) ~ R)) %>% 
  group_by(model, R, R_CGE, Y, I, scenario) %>% 
  reframe(PriceChange = median(PriceChange)) %>% 
  F_R_CGEfull()

rm(Price_msg, Price_cge)




# plot region Energy
lis_y <- seq(2020, 2100,10)
pdata <- df_Price %>% filter(I %in% c("Energy"), R != "WLD", scenario %in% lis_scenario_noEPC) %>%
  filter(Y %in% lis_y) %>% 
  group_by(model, R_CGE, Y, I, scenario) %>% 
  reframe(mean = mean(PriceChange), max = max(PriceChange), min = min(PriceChange)) %>%
  mutate(Y = as.character(Y))

p_price_ene_region <- ggplot() +
  geom_line(pdata, mapping = aes(x = Y, y = mean, color = scenario, linetype = model,
                group = paste0(model, "_", scenario))) +
  geom_ribbon(data = pdata %>% filter(model == "MESSAGEix"),
              aes(x = Y, ymin = min, ymax = max, group = paste0(scenario, "_", model),
                  fill = scenario),
              stat = "identity", alpha=0.2) +
  facet_wrap(~R_CGE, scales = "free", ncol = 6) +
  MyTheme  +
  labs(title = "Energy | Index (y2010=1)", y = "Price Index|y2010=1", x = "Year") +
  scale_x_discrete(breaks = seq(2020, 2100,20)) +
  scale_color_manual(values = palette_Sc) +
  scale_fill_manual(values = palette_Sc) +
  guides(color = guide_legend(title = "Scenario"), fill = guide_legend(title = "Scenario"), linetype = guide_legend(title = "Model"))
p_price_ene_region

ggsave(path = paste0(dir_out, "/fig/SI/"), 
       p_price_ene_region, filename = "0_price_region_ene.pdf",
       width = 36, height = 18, units = "cm")
rm(p_price_ene_region, pdata)

# plot region Agriculture
pdata <- df_Price %>% filter(I %in% c("Food and nonalcoholic beverages")) %>%
  filter(Y %in% lis_y, R != "WLD", scenario %in% lis_scenario_noEPC) %>% 
  group_by(model, R_CGE, Y, I, scenario) %>% 
  reframe(mean = mean(PriceChange), max = max(PriceChange), min = min(PriceChange))

p_price_agri_region <- ggplot() +
  geom_line(pdata, mapping = aes(x = Y, y = mean, color = scenario, linetype = model,
                                 group = paste0(model, "_", scenario))) +
  geom_ribbon(data = pdata %>% filter(model == "MESSAGEix"), 
              aes(x = Y, ymin = min, ymax = max, group = paste0(scenario, "_", model), 
                  fill = scenario), 
              stat = "identity", alpha=0.2) +
  facet_wrap(~R_CGE, scales = "free", ncol = 6) +
  MyTheme +
  labs(title = "Food and nonalcoholic beverages | Index (y2010=1)", y = "Price Index|y2010=1", x = "Year") +
  scale_x_discrete(breaks = seq(2020, 2100,20)) +
  scale_color_manual(values = palette_Sc) +
  scale_fill_manual(values = palette_Sc)  +
  guides(color = guide_legend(title = "Scenario"), fill = guide_legend(title = "Scenario"), linetype = guide_legend(title = "Model"))
p_price_agri_region

ggsave(path = paste0(dir_out, "/fig/SI/"), 
       p_price_agri_region, filename = "0_price_region_agri.pdf",
       width = 36, height = 18, units = "cm")
rm(p_price_agri_region, pdata)





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
  select(c("model", "R", "R8", "Y", "scenario", "variable", "unit", "value")) %>% 
  mutate(model = "MESSAGEix")


GDP_cge <- iamc_cge %>% 
  filter(variable %in% c("GDP_PPP")) %>% 
  mutate_at("value", ~replace_na(.,0)) %>% 
  select(c("model", "R", "R8", "Y", "scenario", "variable", "unit", "value")) %>% 
    mutate(model = "AIM-Hub")
 
df_GDP_cap <- GDP_cge %>% 
  rbind(GDP_msg) %>% 
  mutate(variable = "GDP|PPP") %>% 
  group_by(model, R8, Y, scenario, variable, unit) %>% 
  reframe(value = sum(value)) %>% 
  left_join(df_Pop8) %>% 
  mutate(value = value/Pop*1000000, unit = "Thousand(2011$) per capita")
  

# GDP per capita lineplot

pdata <- df_GDP_cap %>% filter(R8 != "WLD", R8 != "World",
                                Y %in% seq(2020, 2100, 10)) 


p <- ggplot() +
  # geom_line(data = pdata,
           # aes(x = Y, y = value, group = paste0(model, scenario), linetype = model, color = scenario)) +
  geom_col(data = pdata %>% filter(scenario == "No-Miti"),
            aes(x = Y, y = value, fill = model), position = "dodge") +
  labs(x = "Year", y = "GDP per capita | thousand US$2011/yr",
    # caption="Column: GDP per capita in the No-Miti scenario \nLine: GDP loss"
  )+
  facet_wrap(~R8, ncol = 4) +
  MyTheme +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = palette_Sc) +
  scale_fill_manual(values = palette_model) +
  guides(fill = guide_legend(title= "Model"))
p

ggsave(p,
       filename = "0_GDPPPP.pdf", 
       path = paste0(dir_out, "/fig/SI"),
       width = 26, height = 12, units = "cm")


# GDP loss lineplot

pdata <- df_GDP_cap %>%
  group_by(model, R8, Y, scenario, variable, unit) %>%
  reframe(value = sum(value)) %>%
  filter(R8 != "WLD", R8 != "World",
         Y %in% seq(2020, 2100, 10)) %>%
  pivot_wider(values_from = "value", names_from = "scenario") %>%
  pivot_longer(cols = c("2C", "1.5C"), values_to = "value", names_to = "scenario") %>%
  mutate(change = (`value`-`No-Miti`)/`No-Miti`*100) 


p <- ggplot() +
  geom_line(data = pdata,
            aes(x = Y, y = change, group = paste0(model, scenario), linetype = model, color = scenario)) +
  geom_hline(yintercept = 0, color = "grey80") +
  labs(x = "Year", y = "GDP loss | %")+
  facet_wrap(~R8, ncol = 4) +
  MyTheme +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = palette_Sc) +
  guides(linetype = guide_legend(title= "Model"), color = guide_legend(title = "Scenario"))
p

ggsave(p,
       filename = "0_GDPPPP_loss.pdf",
       path = paste0(dir_out, "/fig/SI"),
       width = 26, height = 12, units = "cm")


# 6. Emission downscaled --------------------------------------------------

GHG_DS_HUB <- rgdx.param(paste0("../../data/Emission_DS/AIMHub.gdx"), "EGHG") %>%
  mutate(Unit = "kt CO2eq") %>% 
  gdata::rename.vars(c("EGHG", "Ref"), c("EGHG_HUB", "PHIscenario") )

GHG_DS_MSG <- rgdx.param(paste0("../../data/Emission_DS/MESSAGEix.gdx"), "EGHG") %>%
  mutate(Unit = "kt CO2eq") %>% 
  gdata::rename.vars(c("EGHG", "Ref"), c("EGHG_MSG", "PHIscenario") )

GHG_DS <- GHG_DS_HUB %>% left_join(GHG_DS_MSG) %>% 
  left_join(MapScenario) %>% 
  filter(as.numeric(as.character(Y)) %in% seq(2010, 2100, 10),
         Species == "CO2",
         scenario %in% lis_scenario) %>% 
  left_join(Map_r_PHI2Hub) %>% 
  mutate(R_CGE = ifelse(R == "WLD", "WLD", R_CGE)) %>% 
  F_R_CGEfull()

rm(GHG_DS_HUB, GHG_DS_MSG)
# kt CO2eq


# map

pdata0 <- GHG_DS %>%
  left_join(df_Pop) %>% 
  select("scenario", "Y", "R", "EGHG_HUB", "EGHG_MSG", "Unit", "Population", "Species", "Source") %>% 
  distinct() %>% 
  pivot_longer(c("EGHG_HUB", "EGHG_MSG"), names_to = "model", values_to = "value_plot") %>% 
  mutate(model = recode(.$model, "EGHG_HUB" = "AIM-Hub", "EGHG_MSG" = "MESSAGEix"), value_plot = value_plot/Population*1000, Unit = "t CO2 per capita")



s <- "FFI"

tx_caption <- expression(paste("FFI ", CO[2], " emissions"))

for(m in c("AIM-Hub", "MESSAGEix")){
  if( m == "AIM-Hub"){
    tx_m <- "AIMHub"
  }else{
    tx_m <- "MESSAGEix"
  }

  pdata <- pdata0 %>% filter(Species == "CO2", Source == s) %>% 
    filter( Y %in% c("2030", "2050"), scenario %in% c("No-Miti", "1.5C", "2C"), R != "WLD", model == m) 

  p <- F_plot_globalMap(data=pdata, lis_plot_y=c("2030", "2050"), lis_plot_scenario = c("No-Miti", "1.5C", "2C"),
                        by = c("R"),legendName = expression(paste(CO[2], " emissions | tonne ", CO[2], "/yr/capita"))) +
    scale_fill_gradientn(limits = c(-10,50), #values = c(0, 10, 20, 30, 40, 50),
      colours = c("#8ece6d", "white", "#30b0e0", "#ffe691","#9696b2", "#a82229", "#441a21"))  + #"#30b0e0",,"#6d9b8e" "#f8b976", 
    facet_grid(scenario~Y) +
    labs(subtitle = tx_caption)

  ggsave(p, path = paste0(dir_out, "/fig/SI"), filename =paste0("0_CO2emissions_map_", s, "_", tx_m, ".pdf"), 
          width = 24, height = 20, units = "cm")
  rm(p, pdata)
}


print("The END of main/0_ScenarioOverview.R")


