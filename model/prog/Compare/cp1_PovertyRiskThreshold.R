# This script calculates teh poverty risk threshold (trace back the population)
# Shiya ZHAO, 2024.10.17

require(ggpubr)
require(gamstransfer)
require(openxlsx)

# 1.2 Trace back population -----------------------------------------------

# define a function for reading the EV data
# this calculation is not correct
# gdx_demand <- paste0(demand, "ConsumptionResults_AIMHub.gdx")
# gdx_in <- paste0(dir_input,"AIMHub.gdx")

F_read_ev <- function(gdx_demand, gdx_in, variable = "EV"){
  df_tx_rate <- rgdx.param(gdx_in, "Tx_revenue_rate") %>% 
    gdata::rename.vars(colnames(.), c("Ref", "Y", "R","tx_rate"))
    
  df_tx_adeq <- rgdx.param(gdx_in, "Tx_revenue_adeq") %>% 
    gdata::rename.vars(colnames(.), c("Ref", "Y", "R", "type", "tx_amount")) %>% filter(type == "PPP") %>% select( -type)
    
  
  df_cnsloss <- rgdx.param(gdx_in, "ConsumptionLoss") %>% 
    gdata::rename.vars(colnames(.), c("Ref", "R","Y", "cnsloss"))
  
  df <- rgdx.param(gdx_demand, variable) %>% 
    gdata::rename.vars(colnames(.), c("R", "Ref", "Seg", "Y", "EV")) %>% left_join(df_tx_adeq) %>% left_join(df_tx_rate) %>% left_join(df_cnsloss) %>% 
    F_reve_mutate() %>% 
    filter(Y %in% lis_y) %>% 
    SegNum() %>% 
    filter(Upper %in% c(690, 790, 1200, 1300, 2500, 2000)) %>% 
    select(R, target, revenue, gini, Y, Upper, EV, tx_rate, cnsloss, tx_amount) %>% 
    # mutate(BaU_income = Upper/((1+EV)*(1-cnsloss))) %>% 
    mutate(BaU_income = case_when(revenue == "Neutral" ~ Upper/((1+EV)*(1-cnsloss)), 
                                  !is.na(tx_amount) & revenue == "EPC" ~ (Upper/(1+EV) - tx_amount)/(1-cnsloss-tx_rate),
                                  is.na(tx_rate)  & revenue == "EPC"  ~ Upper/((1+EV)*(1-cnsloss)))) %>% 
    # filter(BaU_income > 50) %>% 
    mutate(TH1 = case_when(Upper == 690 ~ "1.9-threshold", 
                           Upper == 790 ~ "2.15-threshold",
                           Upper == 1300 ~ "3.65-threshold",
                           Upper == 2500 ~ "6.85-threshold",
                           Upper == 1200 ~ "3.2-threshold",
                           Upper == 2000 ~ "5.5-threshold")) %>% filter(TH1 %in% lis_TH)
  
  return(df)
}
# m <- "AIM-Hub"
EV <- data.frame()
EV_tmp <- data.frame()
for(m in lis_model){
  if(m == "AIM-Hub"){
    txt_m <- "AIMHub"
  }else{
    txt_m <- m
  }
  print(m)
  
  EV_tmp <- F_read_ev(gdx_demand = paste0(demand, "ConsumptionResults_", txt_m, ".gdx"), gdx_in = paste0(dir_input, txt_m, ".gdx")) %>% mutate(model = m)
  
  EV <- EV %>% rbind(EV_tmp)
  
}
rm(EV_tmp)



# plot 
pdata <- EV %>% filter(Y %in% c(2030, 2040, 2050, 2070, 2100)) %>% filter(TH1 %in% lis_TH, target == "1.5C") %>% mutate(BaU_income=BaU_income/365) 
pdata1 <- data.frame(TH1 = lis_TH, thre = c(2.15, 3.65, 6.85))

# neutral %>% filter(revenue == "Neutral")
p <- ggplot() +
  geom_boxplot(pdata, mapping = aes(x = Y, y = BaU_income, group = paste0(target,revenue,Y, model), color = model), outlier.alpha = .8, outlier.size = .5) +
  geom_hline(yintercept = 0, color = "grey", linetype = "solid") + 
  geom_hline(pdata1, mapping = aes(yintercept = thre), color = "grey",linetype = "dashed") + 
  facet_wrap(revenue ~ TH1, ncol = 3, scales = "free") +
  MyTheme +
  scale_color_manual(values = palette_model) +
  guides(color = guide_legend(title = "Model"))
p

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_PRT.pdf"),
       p, height = 18, width = 20, units = "cm")

rm(p, pdata, pdata1)

EV %>% 
  filter(R != "WLD") %>%
  left_join(Map_r_PHI2Hub) %>% F_R_CGEfull() %>% 
  mutate(value_plot = BaU_income/365) %>% openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper, "/csv/1_poverty_vulnerability_threshold.xlsx"))
df_PRT <- EV %>% mutate(PRT = BaU_income/365) %>% mutate(unit = "$ per capita per day")
save(df_PRT, file = paste0(dir_output_YSSPpaper, "/RData/1_PRT.RData"))
rm(df_PRT)


## map 2030 2050 ----
lis_y_loop <- c(2030, 2050)

# for(m in lis_model){
  for(y in lis_y_loop){
    for(th in 1:length(lis_TH_loop)){
      # y <- 2030
      # th <- 1
      pdata <- EV %>% 
        filter(R != "WLD") %>%
        mutate(value_plot = BaU_income/365) %>% 
        filter(Y == y,
               # model == m,
               target == "1.5C",
               gini == "consistent",
               TH1 == lis_TH_loop[th] # txt_plot_TH
        ) %>% #left_join(Map_r_PHI2Hub) %>% 
        mutate(scenario = paste0(target," | ",revenue), value_plot = case_when(value_plot > 0 ~ value_plot, value_plot <= 0 ~ 0)) %>% F_sce_main()
      
      a <- (lis_TH_no[th]/365-min(pdata$value_plot))/(max(pdata$value_plot)-min(pdata$value_plot))
      
      p_map <- F_plot_globalMap(data = pdata, 
                                lis_plot_y = y, 
                                lis_plot_scenario = unique(pdata$scenario), by = c("R"),
                                legendName = paste0("PRT (2017PPP$ per capita per day) ")) +
        theme(strip.text = element_text(size = 10), legend.position = "bottom", legend.key.width = unit(1, 'cm')) +
        facet_wrap(model~scenario, ncol = 2) +
        labs(subtitle = paste0( y," | ", lis_TH_loop[th])) + 
        scale_fill_gradientn(values = c(0, a/3, 2*a/3, a, (1-a)/4+a, 2*(1-a)/4+a, 1-(1-a)/4, 1),
                             colours = c("#30b0e0", "#8ece6d", "#a4c3b2","white", "#ffe691", "#f19239" ,"#a82229", "black"))
                             # colours = c("white","#30b0e0", "#8ece6d","#ccd5ae", "#ffe691", "#f19239" ,"#a82229", "black"))
      
      ggsave(p_map,
             filename = paste0(dir_output_YSSPpaper, "/fig/1_map_PRT_", y, "_", lis_TH_loop[th], ".pdf"),
             height = 20, width = 30, units = "cm")
      
      rm(a, pdata, p_map)
    }
  }



# same model, different years in a plot ----------

# for(m in lis_model){
  for(th in 1:length(lis_TH_loop)){
    y <- c(2030,2040,2050)
    pdata <- EV %>% 
      filter(R != "WLD") %>%
      mutate(value_plot = BaU_income/365) %>% 
      filter(Y %in% y,
             # model == m,
             target == "1.5C",
             gini == "consistent",
             TH1 == lis_TH_loop[th] # txt_plot_TH
      ) %>% #left_join(Map_r_PHI2Hub) %>% 
      mutate(scenario = paste0(target," | ",revenue, " | ", model), value_plot = case_when(value_plot > 0 ~ value_plot, value_plot <= 0 ~ 0)) %>% F_sce_main()
    
    a <- (lis_TH_no[th]/365-min(pdata$value_plot))/(max(pdata$value_plot)-min(pdata$value_plot))
    
    p_map <- F_plot_globalMap(data = pdata, 
                              lis_plot_y = y, 
                              lis_plot_scenario = unique(pdata$scenario), by = c("R"),
                              legendName = paste0("PRT (2017PPP$ per capita per day) ")) +
      theme(strip.text = element_text(size = 10), legend.position = "bottom", legend.key.width = unit(1, 'cm')) +
      facet_wrap(scenario~Y, ncol = 3) +
      # labs(subtitle = paste0( m," | ", lis_TH_loop[th])) + 
      labs(subtitle = paste0(lis_TH_loop[th])) + 
      scale_fill_gradientn(values = c(0, a/3, 2*a/3, a, (1-a)/4+a, 2*(1-a)/4+a, 1-(1-a)/4, 1),
                           colours = c("#30b0e0", "#8ece6d", "#a4c3b2","white", "#ffe691", "#f19239" ,"#a82229", "black"))
    # colours = c("white","#30b0e0", "#8ece6d","#ccd5ae", "#ffe691", "#f19239" ,"#a82229", "black"))
    
    ggsave(p_map,
           filename = paste0(dir_output_YSSPpaper, "/fig/1_map_PRT_", lis_TH_loop[th], ".pdf"),
           height = 29, width = 32, units = "cm")
    
    rm(a, pdata, p_map)
  }
# }







# map for PhD thesis defense (presentation slides) --------
dir.create(paste0(dir_output_YSSPpaper, "/fig/PhD/gif_PRT"))
lis_y_loop <- c("2030", "2040", "2050")
pdata0 <- EV %>% 
  filter(R != "WLD") %>%
  mutate(value_plot = BaU_income/365) %>% 
  filter(
         model == "AIM-Hub",
         revenue == "Neutral",
         target == "1.5C",
         gini == "consistent",
         TH1 == txt_plot_TH # txt_plot_TH
  ) %>% 
  mutate(scenario = paste0(target," | ",revenue), value_plot = case_when(value_plot > 0 ~ value_plot, value_plot <= 0 ~ 0)) %>% F_sce_main()

a <- (2.15-min(pdata0$value_plot))/(max(pdata0$value_plot)-min(pdata0$value_plot))

for(y in lis_y_loop){
    pdata <- pdata0 %>% filter(Y == y)
    
    p_map <- F_plot_globalMap(data = pdata, 
                              lis_plot_y = y, 
                              lis_plot_scenario = unique(pdata$scenario), by = c("R"),
                              legendName = paste0("PRT (2017PPP$ per capita per day) ")) +
      theme(strip.text = element_text(size = 10), legend.position = "none", legend.key.width = unit(1, 'cm')) +
      facet_wrap(~scenario, ncol = 2) +
      labs(title = paste0( txt_plot_TH," | AIM-Hub |", y)) + 
      scale_fill_gradientn(values = c(0, a/3, 2*a/3, a, (1-a)/4+a, 2*(1-a)/4+a, 1-(1-a)/4, 1),
                           colours = c("#30b0e0", "#8ece6d", "#a4c3b2","white", "#ffe691", "#f19239" ,"#a82229", "black"))
    # colours = c("white","#30b0e0", "#8ece6d","#ccd5ae", "#ffe691", "#f19239" ,"#a82229", "black"))
    
    ggsave(p_map,
           filename = paste0(dir_output_YSSPpaper, "/fig/PhD/gif_PRT/1_map_PRT_", txt_plot_TH, "_", y, ".png"),
           height = 10, width = 16, units = "cm")
    
    rm(pdata, p_map)

}

# legend
p_map <- F_plot_globalMap(data = pdata0, 
                          lis_plot_y = lis_y_loop, 
                          lis_plot_scenario = unique(pdata0$scenario), by = c("R"),
                          legendName = paste0("PRT (2017PPP$ per capita per day) ")) +
  theme(strip.text = element_text(size = 10), legend.position = "bottom", legend.key.width = unit(1, 'cm')) +
  facet_wrap(~Y, ncol = 3) +
  labs(title = paste0( txt_plot_TH," | AIM-Hub |")) + 
  scale_fill_gradientn(values = c(0, a/3, 2*a/3, a, (1-a)/4+a, 2*(1-a)/4+a, 1-(1-a)/4, 1),
                       colours = c("#30b0e0", "#8ece6d", "#a4c3b2","white", "#ffe691", "#f19239" ,"#a82229", "black"))
# colours = c("white","#30b0e0", "#8ece6d","#ccd5ae", "#ffe691", "#f19239" ,"#a82229", "black"))
ggsave(p_map,
       filename = paste0(dir_output_YSSPpaper, "/fig/PhD/gif_PRT/1_test.pdf"),
       height = 8, width = 30, units = "cm")


rm(a, pdata0)


# map for PhD thesis ----
dir.create(paste0(dir_output_YSSPpaper, "/fig/PhD"))
lis_y_loop <- c("2030", "2050")

for(y in lis_y_loop){
  pdata <- EV %>% 
    filter(R != "WLD") %>%
    mutate(value_plot = BaU_income/365) %>% 
    filter(Y %in% y,
           # model == m,
           target %in% c("1.5C"),
           TH1 == txt_plot_TH
    ) %>% left_join(Map_r_PHI2Hub) %>% mutate(scenario = paste0(target, " | ", revenue), value_plot = case_when(value_plot > 0 ~ value_plot, value_plot <= 0 ~ 0)) %>% 
    F_sce_main()
  
  a <- (2.15-min(pdata$value_plot))/(max(pdata$value_plot)-min(pdata$value_plot))
  
  unique(pdata$scenario)
  
  p_map <- F_plot_globalMap(data = pdata, 
                            lis_plot_y = lis_y_loop, 
                            lis_plot_scenario = unique(pdata$scenario), by = c("R"),
                            legendName = paste0("PRL\n(2017PPP$ per capita per day) ")) +
    theme(strip.text = element_text(size = 10), legend.position = "bottom", legend.key.width = unit(1, 'cm')) +
    facet_wrap(model~scenario, ncol = 2) +
    scale_fill_gradientn(values = c(0, a/3, 2*a/3, a, (1-a)/4+a, 2*(1-a)/4+a, 1-(1-a)/4, 1),
                         colours = c("#30b0e0", "#8ece6d", "#a4c3b2","white", "#ffe691", "#f19239" ,"#a82229", "black"))
                         # colours = c("white","#30b0e0", "#8ece6d","#ccd5ae", "#ffe691", "#f19239" ,"#a82229", "black"))
  p_map
  
  ggsave(p_map,
         filename = paste0(dir_output_YSSPpaper, "/fig/PhD/1_map_PRT_215TH_",y,".pdf"),
         height = 20, width = 30, units = "cm")
  
  rm(a, pdata, p_map)
}

# 
# ## map all ----
# 
# lis_y_loop <- c(seq(2030, 2100,10))
# for(m in lis_model){
#   for(y in lis_y_loop){
#     # y <- 2040
#     for(th in 1:length(lis_TH_loop)){
#       # th <- 3
#       pdata <- EV %>% 
#         filter(R != "WLD") %>%
#         mutate(value_plot = BaU_income/365) %>% 
#         filter(Y %in% y,
#                model == m,
#                target %in% c("1.5C", "2C"),
#                TH1 == lis_TH_loop[th] # txt_plot_TH
#         ) %>% left_join(Map_r_PHI2Hub) %>% mutate(scenario = paste0(target," | ",revenue), 
#                                                   value_plot = case_when(value_plot > 0 ~ value_plot, value_plot <= 0 ~ 0)) %>% F_sce_main()
#       
#       a <- (lis_TH_no[th]-min(pdata$value_plot))/(max(pdata$value_plot)-min(pdata$value_plot))
#       p_map <- F_plot_globalMap(data = pdata, 
#                                 lis_plot_y = c(y), 
#                                 lis_plot_scenario = unique(pdata$scenario), by = c("R"),
#                                 legendName = paste0("PRL\n(2017PPP$ per capita per day) ")) +
#         theme(strip.text = element_text(size = 10), legend.position = "bottom", legend.key.width = unit(1, 'cm')) +
#         facet_wrap(~scenario, ncol = 2) +
#         labs(subtitle = paste0(y, " | ", lis_TH_loop[th]," | ", m)) + 
#         scale_fill_gradientn(values = c(0, a/3, 2*a/3, a, (1-a)/4+a, 2*(1-a)/4+a, 1-(1-a)/4, 1),
#                              # limits = c(min(pdata$value_plot),max(pdata$value_plot)),
#                              colours = c("#30b0e0", "#8ece6d", "#a4c3b2","white", "#ffe691", "#f19239" ,"#a82229", "black"))
#       
#       ggsave(p_map,
#              filename = paste0(dir_output_YSSPpaper, "/fig/SI/1_map_Population group falling to poverty_", y, "_",lis_TH_loop[th],"_", m, ".pdf"),
#              height = 18, width = 30, units = "cm")
#       
#       rm(p_map, a, pdata)
#     } 
#   }
# }

rm(df, Population, PoV, PoV_WLD, EV)
