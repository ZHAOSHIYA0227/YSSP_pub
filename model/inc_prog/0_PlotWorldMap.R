   # This file tried to plot the poverty headcount, tax revenue, and distributional effects in world maps
# Shiya ZHAO, 2023/06/19

library(tidyverse)
library(maps)
library(ggplot2)
# 
# data=PoV_log
# lis_plot_y="2030"
# plotvalue = log(PoV$value/1000000)
# legendName = "log(PovertyHeadcount) \n Headcount: million"




F_plot_globalMap <- function(data=pdata, lis_plot_y="2030", lis_plot_scenario = c("1.5C"), by = c("R_CGE"),
                             legendName = "log(PovertyHeadcount) \n Headcount: million "){
  
  # df_world_map_tmp <- ggplot2::map_data("world") %>% 
  #   mutate(R = countrycode::countrycode(sourcevar=region, origin = "country.name.en", 
  #                                       destination = "iso3c")) %>% 
  #   mutate(R = recode(.$R, "CHN" = "TWN")) 
  # 
  # df_world_map <- df_world_map_tmp %>% 
  #   left_join(Map_r_PHI2Hub) %>% 
  #   filter(!is.na(R_CGE)) %>% 
  #   right_join(data, by = by, relationship = "many-to-many")
  # 
  # pdata_map <- df_world_map %>% 
  #   filter(Y %in% lis_plot_y, R != "WLD") 
  # 
  # p1 <- ggplot(pdata_map, aes(x = long, y = lat, group = group, fill =value_plot)) + 
  #   geom_polygon( color = "grey50", size = 0.1) +
  #   MyTheme +
  #   theme(axis.title = element_blank(),
  #         axis.ticks = element_blank(), 
  #         axis.line = element_blank(), 
  #         axis.text = element_blank(), 
  #         legend.position = "bottom") +
  #   labs(fill = legendName) +
  #   scale_x_continuous(breaks = NULL) +
  #   scale_y_continuous(breaks = NULL)

  ####

  
  outline <- st_read(paste0(dir_ModelComparison, "inc_data/chinasheng_world/country1.shp")) 
  df_world_map_tmp <- outline %>% 
    mutate(R = case_when(GMI_CNTRY == "MON" ~ "MNE",
                         GMI_CNTRY == "ROM" ~ "ROU",
                         GMI_CNTRY == "ZAR" ~ "COD",
                         !(GMI_CNTRY %in% c("MON","ROM", "ZAR")) ~ GMI_CNTRY)) 

  df_world_map <- df_world_map_tmp %>% 
    select("R", "GMI_CNTRY", "SOVEREIGN", "geometry") %>% 
    left_join(Map_r_PHI2Hub) %>% 
    mutate(R_CGE = case_when(is.na(R_CGE) ~ "none", !is.na(R_CGE) ~ R_CGE)) %>%
    full_join(data, by = by, relationship = "many-to-many") 
  
  
    pdata_map <- df_world_map %>%
      filter(R != "WLD") %>% 
      filter(Y %in% lis_plot_y)
    
    lis_plot_model <- unique(data$model) %>% as.character()
    
    pdata_map_tmp <- data.frame()
    for(y in 1:length(lis_plot_y)){
      for(scenario in 1:length(lis_plot_scenario)){
        for(model in 1:length(lis_plot_model)){
          pdata_map_tmp0 <- df_world_map %>%
            filter(R != "WLD") %>% 
            filter(is.na(Y))
          
          pdata_map_tmp0$Y <- lis_plot_y[y]
          pdata_map_tmp0$scenario <- lis_plot_scenario[scenario]
          pdata_map_tmp0$model <- lis_plot_model[model]
          
          pdata_map_tmp <- pdata_map_tmp %>%  rbind(pdata_map_tmp0)
        }
      }

    }
     
    colnames(pdata_map)
    colnames(pdata_map_tmp)
    pdata_map <- pdata_map %>% 
      rbind(pdata_map_tmp)
    p1 <- ggplot() + 
      geom_sf(data = pdata_map, mapping = aes(fill = value_plot), color = "grey50",  
              size = 0.1) +
      MyTheme +
      theme(axis.title.x = element_blank(),
            axis.ticks.x = element_blank(), 
            axis.line.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.line.y = element_blank(), 
            axis.text.y = element_blank(),
            legend.position = "bottom") +
      labs(fill = legendName) +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      facet_grid(~Y)
  
  return(p1)
}


