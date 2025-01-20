# This Rscript plots for the sensitivity analysis in the OneEarth paper
# By Shiya ZHAO, 2024.12.16

require(ggpubr)
# make directory ----------------------------------------------------------
paste0(dir_output_YSSPpaper, "/fig/SI/9_sensitivity/") %>% dir.create()
paste0(dir_output_YSSPpaper, "/fig/SI/9_sensitivity/line/") %>% dir.create()
paste0(dir_output_YSSPpaper, "/fig/SI/9_sensitivity/col/") %>% dir.create()

# loading data ------------------------------------------------------------

load(file = paste0(dir_output, "/RData/1_variable ranges and changes.RData"))


# variables ---------------------------------------------------------------

lis_var_loop <- c("Poverty rate", "Food poverty rate", "Gini", "EV", "EVi")


# plot the variables: line and ribbon -------------------------------------------------
df_line <- df_change %>% select(R, Y, TH, variable, revenue, target, min, max, median) %>% 
  mutate(v_color = target, v_group = paste0(target, revenue)) %>% 
  filter(R == "World") %>% 
  mutate(TH = case_when(TH == "pop_2.15" ~ "2.15-threshold", 
                        TH == "pop_3.65" ~ "3.65-threshold", 
                        TH == "pop_6.85" ~ "6.85-threshold",
                        TH == "point" ~ "Point", !grepl("pop_", TH) ~ TH))

for(var in seq_along(lis_var_loop)){

  pdata <- df_line %>% 
    filter(variable == c(lis_var_loop[var])) 
  
  if(lis_var_loop[var] %in% c("Poverty rate")){
    # (food) poverty rate --------------------------------------
    txt_y <- "Change (% point)"
    txt_x <- "Year"
    
    size_width <- 15.5
    size_height <- 6
    
    pdata <- pdata %>% 
      mutate(v_x = Y, v_facet = TH, median = median * 100, min = min * 100, max = max * 100)

  }else if(lis_var_loop[var] %in% c("Food poverty rate")){
    txt_y <- "Change (% point)"
    txt_x <- "Year"
    
    size_width <- 15.5
    size_height <- 6
    
    pdata <- pdata %>% 
      mutate(v_x = Y, v_facet = TH, median = median * 100, min = min * 100, max = max * 100)

  }else if(lis_var_loop[var] == "Gini"){
    # Gini --------------------------------------------------------------------
    txt_y <- "Change (point)"
    txt_x <- "Year"
    
    size_width <- 6
    
    size_height <- 6
    
    pdata <- pdata %>% 
      mutate(v_x = Y, v_facet = "Gini")

  }else if(lis_var_loop[var] %in% c("EV")){
    # consumption loss --------------------------------------------------------
    txt_y <- "Change (% point)"
    txt_x <- "Decile"
    
    size_width <- 10
    size_width <- 6
    size_height <- 6
    
    pdata <- pdata %>% filter(Y == "2030", TH != "ALL") %>% 
      mutate(v_x = factor(TH, levels = c(seq(1,10,1) %>% as.character())),
             v_facet = "Consumption")
    
  }else if(lis_var_loop[var] %in% c("EVi")){
    # consumption loss by commodity--------------------------------------------------------
    txt_y <- "Change (% point)"
    txt_x <- "Commodity"
    
    size_width <- 18
    size_height <- 9
    
    pdata <- pdata %>% filter(Y == "2030", grepl("1",TH)) %>% 
      mutate(v_x = factor(str_split_i(TH, "_", 2), levels = lis_I_abb), 
             v_group = paste0(str_split_i(TH, "_", 1),"_",revenue, "_", target),
             v_facet = case_when(str_split_i(TH, "_", 1) == "1" ~ "Decile 1", 
                                 str_split_i(TH, "_", 1) == "10" ~ "Decile 10"))
  }

  if(lis_var_loop[var]  != "EVi"){

    # not EVi, the dimension I_abb is not considered 
    p <-  ggplot(pdata) +
      geom_line( mapping = aes(x = v_x, y = median, color = v_color, linetype = revenue,
                               group = v_group)) +
      geom_ribbon( aes(x = v_x, ymin = min, ymax = max, group = v_group,fill = v_color),
                   stat = "identity", alpha=0.2) + 
      guides(color = guide_legend(title = "Scenario"), 
             fill = guide_legend(title = "Scenario"),
             linetype = guide_legend(title = "Redistribution")#, shape = guide_legend(title = "Model")
      ) + 
      MyTheme  + theme(legend.position = "none", axis.text.x = element_text(angle = 30)) +
      labs(y = txt_y, x =  txt_x ) +
  
      scale_color_manual(values = palette_Sc) +
      scale_fill_manual(values = palette_Sc) 
    
  }else{
      # in EVi, the dimension I_abb is considered 
    print(head(pdata))
    p <-  ggplot(pdata) +
      geom_line( mapping = aes(x = v_x, y = median, color = v_color, linetype = revenue,
                               group = v_group)) +
      geom_ribbon( aes(x = v_x, ymin = min, ymax = max, group = v_group,fill = v_color),
                   stat = "identity", alpha=0.2) +
      guides(color = guide_legend(title = "Scenario"), 
             fill = guide_legend(title = "Scenario"),
             linetype = guide_legend(title = "Redistribution")#, shape = guide_legend(title = "Model")
      ) + 
      MyTheme  + theme(legend.position = "none", axis.text.x = element_text(angle = 60)) +
      labs(y = txt_y, x =  txt_x ) +
      scale_color_manual(values = palette_Sc) +
      scale_fill_manual(values = palette_Sc) 
    p
  }
  
  
  if(lis_var_loop[var] %in% c("EV")){
    p <- p + facet_grid(~v_facet, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 0)) 
    # scale_x_discrete(breaks = c(seq(1,10,1) %>% as.character(), "ALL")) 
    
  }else if(lis_var_loop[var] %in% c("EVi")){
    p <- p + facet_grid(~v_facet, scales = "free_y") +
      theme(axis.text.x = element_text(hjust = 1, vjust = 1)) 

    # scale_x_discrete(breaks = c(seq(1,10,1) %>% as.character(), "ALL")) 
    
  }else if(lis_var_loop[var] %in% c("Gini")){
    p <- p + facet_grid(~v_facet, scales = "free_y") +
      theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
      scale_x_discrete(breaks = seq(2020, 2100,20)) 
    
    # scale_x_discrete(breaks = c(seq(1,10,1) %>% as.character(), "ALL")) 
    
  }else{
    p <- p + facet_grid(~TH, scales = "free_y") +
      scale_x_discrete(breaks = seq(2020, 2100,20)) 
  }
  
  
  p
  ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/9_sensitivity/line/",lis_var_loop[var],".pdf"), 
         width = size_width, height = size_height, units = "cm")
  
    if(lis_var_loop[var] %in% c("Poverty rate")){
      p_leg <- get_legend(p + theme(legend.position = "right")) %>% as_ggplot()
      ggsave(p_leg , filename = paste0(dir_output_YSSPpaper, "/fig/SI/9_sensitivity/line/legend.pdf"), 
           width = 3, height = 5.5, units = "cm")
      rm(p_leg)
    }
  rm(p, pdata, txt_y, txt_x, size_width, size_height)
}

rm(df_line)

#  plot the variables: columns --------------------------------------------
df_col <- df_change %>% 
  pivot_longer(cols = colnames(.)[which(grepl("_", colnames(.)))], 
               names_to = "Distribution", values_to = "value") %>% 
  mutate(SSP = case_when(grepl("consistent", Distribution) ~ "SSP2",
                         !grepl("consistent", Distribution) ~ str_split_i(Distribution, "_", 2)), 
         v_color = target, v_group = paste0(target, revenue),
         model = str_split_i(Distribution, "_", 1)) %>% select(-Distribution) %>% 
  filter(R == "World", target != "No-Miti") %>% 
  mutate(TH = case_when(TH == "pop_2.15" ~ "2.15-threshold", 
                        TH == "pop_3.65" ~ "3.65-threshold", 
                        TH == "pop_6.85" ~ "6.85-threshold",
                        TH == "point" ~ "Point", !grepl("pop_", TH) ~ TH))

for(var in seq_along(lis_var_loop)){
  # var <- 1
  pdata <- df_col %>% 
    filter(variable == c(lis_var_loop[var])) 
  
  if(lis_var_loop[var] %in% c("Poverty rate")){
    # Poverty rate --------------------------------------
    txt_y <- "Change (% point)"
    txt_x <- "Poverty lines"
    
    size_width <- 8
    size_height <- 8
    
    pdata <- pdata %>% filter(target == "1.5C", Y == "2030") %>% 
      mutate(v_x = TH, v_facet = revenue, value = value * 100, median = median * 100, min = min * 100, max = max * 100)
    
  }else if(lis_var_loop[var] %in% c("Food poverty rate")){
    txt_y <- "Change (% point)"
    txt_x <- "Dietary requirement"
    
    size_width <- 8
    size_height <- 8
    
    pdata <- pdata %>% filter(target == "1.5C", Y == "2030") %>% 
      mutate(v_x = TH, v_facet = revenue, value = value * 100, median = median * 100, min = min * 100, max = max * 100)
    
  }else if(lis_var_loop[var] == "Gini"){
    # Gini --------------------------------------------------------------------
    txt_y <- "Change (point)"
    txt_x <- "Year"
    
    size_width <- 12
    
    size_height <- 6
    
    pdata <- pdata %>% filter(Y %in% c("2020", "2030", "2040", "2050"), target == "1.5C") %>% 
      mutate(v_x = Y, v_facet = revenue, v_group = paste0(revenue))
    
  }else if(lis_var_loop[var] %in% c("EV")){
    # consumption loss --------------------------------------------------------
    txt_y <- "Change (% point)"
    txt_x <- "Decile"
    
    size_width <- 10
    size_width <- 14
    size_height <- 6
    
    pdata <- pdata %>% filter(Y == "2030", target == "1.5C", TH != "ALL") %>% 
      mutate(v_x = factor(TH, levels = c(seq(1,10,1) %>% as.character())),
             v_facet = revenue)
    
  }else if(lis_var_loop[var] %in% c("EVi")){
    # consumption loss by commodity--------------------------------------------------------
    txt_y <- "Change (% point)"
    txt_x <- "Commodity"
    
    size_width <- 16
    size_height <- 10
    
    pdata <- pdata %>% filter(Y == "2030", target == "1.5C", grepl("1",TH)) %>% 
      mutate(v_x = factor(str_split_i(TH, "_", 2), levels = lis_I_abb), 
             v_group = paste0(str_split_i(TH, "_", 1),"_",revenue, "_", target),
             v_facet = case_when(str_split_i(TH, "_", 1) == "1" ~ "Decile 1", 
                                 str_split_i(TH, "_", 1) == "10" ~ "Decile 10"))
  }
  
  
  if(lis_var_loop[var]  != "EVi"){
    
    # not EVi, the dimension I_abb is not considered 
    p <-  ggplot(pdata) + geom_hline(yintercept = 0, color = "grey90") +
      geom_col( mapping = aes(x = v_x, y = median, linetype = revenue,
                               group = v_group), width = .7, fill = "grey75", position = "dodge") +
      geom_point(aes(x = v_x, y = value, shape = SSP, color = model)) +
      geom_ribbon( aes(x = v_x, ymin = min, ymax = max, fill = v_color, group = v_group), # ,fill = v_color
        stat = "identity", alpha=0.2) +
      guides(color = guide_legend(title = "Model"),
             shape = guide_legend(title = "SSP")) + 
      MyTheme  + theme(legend.position = "none", axis.text.x = element_text(angle = 60)) +
      labs(y = txt_y, x =  txt_x ) +
      scale_color_manual(values = palette_model)+
      scale_fill_manual(values = palette_Sc)

  }else{
    # in EVi, the dimension I_abb is considered 
    print(head(pdata))
    p <-  ggplot(pdata) +geom_hline(yintercept = 0, color = "grey90") +
      geom_col( mapping = aes(x = v_x, y = median, linetype = revenue,
                              group = v_group), width = .7, fill = "grey75", position = "dodge") +
      geom_point(aes(x = v_x, y = value, shape = SSP, color = model)) +
      geom_ribbon( aes(x = v_x, ymin = min, ymax = max, group = v_group,fill = v_color), #
                   stat = "identity", alpha=0.2) +
      guides(color = guide_legend(title = "Model"), shape = guide_legend(title = "SSP")) +
      MyTheme  + theme(legend.position = "none", axis.text.x = element_text(angle = 60)) +
      labs(y = txt_y, x =  txt_x ) +
      scale_color_manual(values = palette_model) +
      scale_fill_manual(values = palette_Sc)
    p
  }
  
  
  if(lis_var_loop[var] %in% c("EV", "Gini")){
    p <- p + facet_grid(~v_facet, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 0)) 
    p
  }else if(lis_var_loop[var] %in% c("EVi")){
    p <- p + facet_grid(revenue~v_facet, scales = "free_y") +
      theme(axis.text.x = element_text(hjust = 1, vjust = 1)) 

    
  }else #if(lis_var_loop[var] %in% c("Gini")){
  #   p <- p + facet_grid(~v_facet, scales = "free_y") +
  #     theme(axis.text.x = element_text(angle = 0))
  #     
  #   
  # }else
    {
    p <- p + 
      facet_grid(~v_facet, scales = "free_y")  +
      theme(axis.text.x = element_text(hjust = 1, vjust = 1))
    }
  
  
  ggsave(p, filename = paste0(dir_output_YSSPpaper, "/fig/SI/9_sensitivity/col/",lis_var_loop[var],".pdf"), 
         width = size_width, height = size_height, units = "cm")
  
  if(lis_var_loop[var] %in% c("Poverty rate")){
    p_leg <- get_legend(p + theme(legend.position = "right")) %>% as_ggplot()
    ggsave(p_leg , filename = paste0(dir_output_YSSPpaper, "/fig/SI/9_sensitivity/col/legend.pdf"), 
           width = 3, height = 12, units = "cm")
  }
}


