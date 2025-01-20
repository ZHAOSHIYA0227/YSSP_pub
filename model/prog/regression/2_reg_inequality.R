# This Rscript calculates the response of inequality index to policy stringency
# Shiya ZHAO, 2024.01.28




# 0. Function -------------------------------------------------------------
F_plot_EGHG_Gini <- function(pdata, x_label = expression(paste(CO[2]," reduction rate (%)")) , 
                        y_label = "Change in Gini (point)"){
  
  plot <- ggplot(pdata) +
    geom_point(aes(x = -x_value, y = y_value, color = variable_color, shape = variable_shape)) +
    geom_smooth(aes(x = -x_value, y = y_value, group = paste0(variable_shape)),
                color = "grey50", alpha = 0.5, linewidth = 0.2,method = 'lm', formula = 'y ~ x') +
    geom_hline(yintercept = 0, color = "grey") +
    labs(x = x_label, y = y_label) +
    guides(color = guide_legend(title = expression(paste("Carbon Budget (Gt ", CO[2], ")")), ncol = 1)) +
    scale_color_manual(values = viridis(12)) +
    MyTheme
  plot
  
  return(plot)
}




# # 1. Regression: ---------------------------------------------------------------------
# # Gini to PGHG
# df <- df_PGHG_Rcge %>% left_join(df_Gini_R) %>% #view
#   filter(!is.na(value_Gini)) %>% F_scenario_rename() %>% F_scenario() %>% 
#   F_reve_mutate() %>% 
#   filter(abs(change_Gini) > 10^(-4)) %>% F_CarbonB_factor()
# x <-"value_PGHG"
# y <- "change_Gini"


# switch ----

if(type_reg == "PGHG"){
  # Regression: PGHG v.s. additional poverty headcount -------------------
  df <- df_PGHG_Rcge %>% left_join(df_Gini_R) %>% #view
    filter(!is.na(value_Gini)) %>% F_scenario_rename() %>% F_scenario() %>% 
    F_reve_mutate(all = T) %>% 
    filter(abs(change_Gini) > 10^(-5)) %>% F_CarbonB_factor()
  x <-"value_PGHG"
  y <- "change_Gini"
  
  
}else if(type_reg == "EGHG_Rcge"){
  # regression: Emissions v.s. additional poverty headcount ----
  df <- df_EGHG_Rcge %>% filter(Species == "CO2", Source == "tot_anthr") %>% left_join(df_Gini_R) %>% #view
    filter(!is.na(value_Gini)) %>% F_scenario_rename() %>% F_scenario() %>% 
    F_reve_mutate(all = T) %>% 
    filter(abs(change_Gini) > 10^(-5)) %>% F_CarbonB_factor()
  x <- "change_rate_EGHG_Rcge"
  y <- "change_Gini"
  
}else if(type_reg == "EGHG_R"){
  df <- df_EGHG_R %>% filter(Source == "tot_anthr") %>% left_join(df_PoV_R) %>%
    filter(!is.na(value_PoVExp)) %>% F_scenario_rename() %>% F_scenario() %>%
    F_reve_mutate(all = T)  %>% F_CarbonB_factor()
  x <- "EGHG_reduction_rate"
  y <- "change_Gini"
}

# Gini to PGHG
colnames(df)
df_plot <- df %>%
  mutate(x_value = .[[x]]*100 , y_value = .[[y]]*100) %>% filter(!(target %in% c("400", "1600"))) 



### 1.1.1 2030, group by policy ---------------------------------------------------------------------
# national 
pdata <- df_plot %>% 
  filter(Y %in% c(2030), revenue == type_redist) %>% 
  mutate(variable_color = target, variable_shape = policy)

# plot regression 2030
# carbon price in "SSP2_500_FC", "SSP2_600_FC" are the same in 2030 but differ afterwards
# The outlier in the plot is SSP2_400_FC
rm(p)
p <- F_plot_EGHG_Gini(pdata) + facet_wrap(~R_CGE, ncol = 6) + guides(shape = guide_legend(title = "Policy"))
p

ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/regression/2_Gini_PGHG_2030_national_",type_redist,".png"), 
       width = 32, height = 16, units = "cm")


# regional 
pdata <- df_plot %>% 
  dplyr::group_by(Ref, R_CGE, Y, Species, Source, policy, revenue, target, scenario) %>% 
  dplyr::reframe(x_value = (sum(value_EGHG)-sum(value_EGHG_BaU))/sum(value_EGHG_BaU), 
                 y_value = mean(change_Gini)) %>% 
  filter(Y %in% c(2030), revenue == type_redist) %>% 
  mutate(variable_color = target, variable_shape = policy)

# plot regression 2030
# carbon price in "SSP2_500_FC", "SSP2_600_FC" are the same in 2030 but differ afterwards
# The outlier in the plot is SSP2_400_FC

p <- F_plot_EGHG_Gini(pdata) + facet_wrap(~R_CGE, ncol = 6) + guides(shape = guide_legend(title = "Policy"))
p


ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/regression/2_Gini_PGHG_2030_regional_",type_redist,".png"), 
       width = 32, height = 16, units = "cm")
rm(p)


### 1.1.2 2.15, group by year, regional ---------------------------------------------------------------------
# colnames(df_Gini_R)

lis_pol <- c("Carbon tax","Full Combo")
for(pol in 1:length(lis_pol)){
 # national
 pdata <- df_plot %>% filter(Y %in% c(2030, 2050, 2070), revenue == type_redist, abs(y_value) > 0.0001, policy == lis_pol[pol]) %>% 
  mutate(variable_color = target, variable_shape = Y)

  # plot regression 2.15-threshold
  # carbon price in "SSP2_500_FC", "SSP2_600_FC" are the same in 2030 but differ afterwards
  # The outlier in the plot is SSP2_400_FC
  
  p <- F_plot_EGHG_Gini(pdata) + facet_wrap(~R_CGE, ncol = 6) + labs(subtilte = lis_pol[pol], shape = "Year") + guides(shape = guide_legend(title = "Year"))
  p
  
  # slopes are different across years
  
  ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/regression/2_Gini_PGHG_Year_national_",type_redist,"_",lis_pol[pol],".png"), 
         width = 32, height = 16, units = "cm")
  
  
  
  
  # regional. 
  pdata <- df_plot %>%   
    dplyr::group_by(Ref, R_CGE, Y, Species, Source, policy, revenue, target, scenario) %>% 
    dplyr::reframe(x_value = (sum(value_EGHG)-sum(value_EGHG_BaU))/sum(value_EGHG_BaU), 
                   y_value = mean(change_Gini)) %>% 
    filter(Y %in% c(2030, 2050, 2070), revenue == type_redist, abs(y_value) > 0.0001, policy == lis_pol[pol]) %>% 
    mutate(variable_color = target, variable_shape = Y)
  
  # plot regression 2.15-threshold
  # carbon price in "SSP2_500_FC", "SSP2_600_FC" are the same in 2030 but differ afterwards
  # The outlier in the plot is SSP2_400_FC
  
  p <- F_plot_EGHG_Gini(pdata) + facet_wrap(~R_CGE, ncol = 6) + labs(subtilte = lis_pol[pol]) + guides(shape = guide_legend(title = "Year"))
  p
  
  # slopes are different across years
  
  ggsave(filename = paste0(dir_output_YSSPpaper, "/fig/SI/regression/2_Gini_PGHG_Year_regional_",type_redist,"_",lis_pol[pol],".png"), 
         width = 32, height = 16, units = "cm")
}



# 1.2 Regression -------------
df_lm_input <- df %>% 
  filter(revenue == type_redist, abs(change_Gini) > 0.0001) 


lis_y_for <- seq(2030, 2070, 20)
df_smry <- data.frame()
df_smry_output <- data.frame()
# for(th in 1:length(lis_TH_for)){
  for(year in 1:length(lis_y_for)){
    for(rcge in 1:17){
      # rcge <-3
      # y <- 1
      pdata1 <- df_lm_input %>% filter(R_CGE == lis_R_CGE[rcge], Y %in% lis_y_for[year]) 
      if(nrow(pdata1) & length(unique(pdata1$policy)) > 1 ){
        df_lm <- lm(data = pdata1, pdata1[[y]]*100 ~ I(-pdata1[[x]]*100) + policy)
        smry_lm <- summary(df_lm)
        df_smry_tmp <- data.frame(R_CGE = lis_R_CGE[rcge],
                                  intercept = df_lm[["coefficients"]][1] %>% signif(digits = 3) ,
                                  slope_PGHG = df_lm[["coefficients"]][2] %>% signif(digits = 3) ,
                                  p_Full_Combo = df_lm[["coefficients"]][3] %>% signif(digits = 3) ,
                                  # TH = df_lm[["coefficients"]][3],
                                  r_sqr = smry_lm[["adj.r.squared"]]%>% signif(digits = 3),
                                  Y = lis_y_for[year],
                                  revenue = type_redist
        )
      }else if(nrow(pdata1) & length(unique(pdata1$policy)) == 1 ){
        df_lm <- lm(data = pdata1, pdata1[[y]]*100 ~ I(-pdata1[[x]]*100) )
        smry_lm <- summary(df_lm)
        df_smry_tmp <- data.frame(R_CGE = lis_R_CGE[rcge],
                                  intercept = df_lm[["coefficients"]][1] %>% signif(digits = 3) ,
                                  slope_PGHG = df_lm[["coefficients"]][2] %>% signif(digits = 3) ,
                                  p_Full_Combo = 0,
                                  # TH = df_lm[["coefficients"]][3],
                                  r_sqr = smry_lm[["adj.r.squared"]]%>% signif(digits = 3),
                                  Y = lis_y_for[year],
                                  revenue = type_redist
        )
      }else if(!(nrow(pdata1))){
        df_smry_tmp <- data.frame()
      }
      
      df_smry <- df_smry %>% rbind(df_smry_tmp)
    }
  }
# }


df_smry <- df_smry %>% mutate(intercept = case_when(abs(intercept) < 0.0001 ~ 0, abs(intercept) > 0.0001 ~ intercept), 
                   slope_PGHG = case_when(abs(slope_PGHG) < 0.0001 ~ 0, abs(slope_PGHG) > 0.0001 ~ slope_PGHG), 
                   p_Full_Combo = case_when(abs(p_Full_Combo) < 0.0001 ~ 0, abs(p_Full_Combo) > 0.0001 ~ p_Full_Combo), 
                   r_sqr = case_when(abs(r_sqr) < 0.0001 ~ 0, abs(r_sqr) > 0.0001 ~ r_sqr)) 
openxlsx::write.xlsx(df_smry, file = paste0(dir_output_YSSPpaper, "/csv/regression/2_Gini_PGHG_",type_reg, "_",type_redist,"_summary.xlsx"))


