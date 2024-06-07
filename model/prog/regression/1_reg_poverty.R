# This Rscript calculates the response of poverty headcount to policy stringency
# Shiya ZHAO, 2024.01.28


## 00 redistribution ---------
# type_redist <- "Neutral"
type_redist <- "Progressive"

## 00 regression ---------
# type_reg <-  "PGHG"
type_reg <-  "EGHG_Rcge"



# 0. Defining functions -----
F_plot_EGHG <- function(pdata, x_label = expression(paste(CO[2]," reduction rate | %")) , 
                        y_label = "Change in poverty headcount | million"){
  
  plot <- ggplot(pdata) +
    geom_point(aes(x = -x_value, y = y_value, color = variable_color, shape = variable_shape)) +
    geom_smooth(aes(x = -x_value, y = y_value, group = paste0(variable_shape)),
                color = "grey50", alpha = 0.5, linewidth = 0.2,method = 'lm', formula = 'y ~ x') +
    geom_hline(yintercept = 0, color = "grey") +
    labs(x = x_label, y = y_label) +
    guides(color = guide_legend(title = paste(expression("Carbon Budget | Gt ", CO[2])), ncol = 2)) +
    scale_color_manual(values = viridis(12)) +
    MyTheme
  plot
  return(plot)
}



# switch ----

if(type_reg == "PGHG"){
 # Regression: PGHG v.s. additional poverty headcount -------------------
df <- df_PGHG_Rcge %>% left_join(df_PoV_Rcge) %>%
  filter(!is.na(value_PoVExp_Rcge)) %>% F_scenario_rename() %>% F_scenario() %>% 
  F_reve_mutate()  %>% F_CarbonB_factor()
x <- "value_PGHG"
y <- "change_PoVExp_Rcge"

}else if(type_reg == "EGHG_Rcge"){
  # regression: Emissions v.s. additional poverty headcount ----
  df <- df_EGHG_Rcge %>% filter(Species == "CO2", Source == "tot_anthr") %>% left_join(df_PoV_Rcge) %>%
    filter(!is.na(value_PoVExp_Rcge)) %>% F_scenario_rename() %>% F_scenario() %>% 
    F_reve_mutate()  %>% F_CarbonB_factor()
  x <- "change_rate_EGHG_Rcge"
  y <- "change_PoVExp_Rcge"
  
}else if(type_reg == "EGHG_R"){
  df <- df_EGHG_R %>% filter(Source == "tot_anthr") %>% left_join(df_PoV_R) %>%
    filter(!is.na(value_PoVExp)) %>% F_scenario_rename() %>% F_scenario() %>%
    F_reve_mutate()  %>% F_CarbonB_factor()
  x <- "EGHG_reduction_rate"
  y <- "change_PoVExp"
}

df_plot <- df %>%
  mutate(x_value = .[[x]]*100 , y_value = .[[y]]/1000000) %>% filter(!(scenario_Name %in% c("400", "1600")), TH %in% lis_TH_od) %>% F_TH()
  


### 1.1.1 plot 2030 1.9-Threshold -----
rm(pdata)
# facet by year ----
lis_y_for <- c(2030, 2050, 2070)
for(year in 1:length(lis_y_for)){
pdata <- df_plot %>% 
  filter(TH == txt_TH, Y %in% lis_y_for, abs(change_PoVExp_Rcge) > 1, 
         revenue == type_redist,R_CGE %in% lis_R_CGE_PoV) %>% 
  mutate(variable_color = scenario_Name, variable_shape = policy)

# plot regression 2030
# carbon price in "SSP2_500_FC", "SSP2_600_FC" are the same in 2030 but differ afterwards
# The outlier in the plot is SSP2_400_FC

p <- F_plot_EGHG(pdata) + facet_grid(Y~R_CGE) + guides(shape = guide_legend(title = "Policy"))
p

ggsave(filename = paste0(dir_out, "/fig/SI/regression/1_PoV_2030_",type_redist, "_", txt_TH,".pdf"), 
       width = 22, height = 12, units = "cm")
}


# facet by year ----
lis_y_for <- c(2030, 2050, 2070)
for(year in 1:length(lis_y_for)){
pdata <- df_plot %>% 
  filter(TH == txt_TH, Y %in% lis_y_for, abs(change_PoVExp_Rcge) > 1, 
         revenue == type_redist,R_CGE %in% lis_R_CGE_PoV) %>% 
  mutate(variable_color = scenario_Name, variable_shape = policy)

# plot regression 2030
# carbon price in "SSP2_500_FC", "SSP2_600_FC" are the same in 2030 but differ afterwards
# The outlier in the plot is SSP2_400_FC

p <- F_plot_EGHG(pdata) + facet_grid(Y~R_CGE) + guides(shape = guide_legend(title = "Policy")) + 
  guides(color = guide_legend(title = paste(expression("Carbon Budget | Gt ", CO[2])), ncol = 1))
  
p

ggsave(filename = paste0(dir_out, "/fig/SI/regression/1_PoV_allyear_",type_redist, "_", txt_TH,".pdf"), 
       width = 22, height = 11, units = "cm")
}


### 1.1.2. plot 2.15-Threshold, years 2030, 2050, 2070-----------------------------------------------------------------

pdata <- df_plot %>%
  filter(TH == txt_TH, Y %in% c(2030, 2050, 2070), abs(change_PoVExp_Rcge) > 1, revenue == type_redist, R_CGE %in% lis_R_CGE_PoV) %>% 
  mutate(variable_color = scenario_Name, variable_shape = Y)


# plot regression 2.15-threshold
# carbon price in "SSP2_500_FC", "SSP2_600_FC" are the same in 2030 but differ afterwards
# The outlier in the plot is SSP2_400_FC
p <- F_plot_EGHG(pdata) + facet_grid(policy~R_CGE) + guides(shape = guide_legend(title = "Year"))
p

# slopes are different across years

ggsave(filename = paste0(dir_out, "/fig/SI/regression/1_PoV_",type_reg,"_",txt_TH,"_",type_redist,".pdf"), 
       width = 26, height = 12, units = "cm")



### 1.1.3. plot 2030, different thresholds-----------------------------------------------------------------
pdata <- df_plot %>% filter(Y %in% c(2030), abs(change_PoVExp_Rcge) > 1, revenue == type_redist, R_CGE %in% lis_R_CGE_PoV, TH %in% lis_TH_od) %>% 
  # F_TH() %>% 
  # left_join(data.frame(TH = lis_TH_od, TH1 = lis_TH))
  mutate(variable_color = scenario_Name, variable_shape = TH1)


# plot regression 2.15-threshold
# carbon price in "SSP2_500_FC", "SSP2_600_FC" are the same in 2030 but differ afterwards
# The outlier in the plot is SSP2_400_FC
p <- F_plot_EGHG(pdata) + facet_grid(policy~R_CGE) + guides(shape = guide_legend(title = "Poverty lines"))
p

# slopes are different across poverty lines (a little)

ggsave(filename = paste0(dir_out, "/fig/SI/regression/1_PoV_",type_reg,"_2030_povertylines_",type_redist,"_", txt_TH, ".pdf"), 
       width = 22, height = 10, units = "cm")



### 1.1.4 Regression --------
# linear regression
df_lm_input <- df %>% 
  filter(revenue == type_redist, abs(change_PoVExp_Rcge) > 1) 


lis_y_for <- seq(2030, 2070, 20)
df_smry <- data.frame()
df_smry_tmp <- data.frame()
rm(df_lm, smry_lm)
for(th in 1:length(lis_TH_od)){
  for(year in 1:length(lis_y_for)){
    for(rcge in 1:17){
      # th <- 1
      # year <- 1
      # rcge <- 4
      pdata1 <- df_lm_input %>% filter(R_CGE == lis_R_CGE[rcge], Y %in% lis_y_for[year], TH %in% lis_TH_od[th]) 
      if(nrow(pdata1) & length(unique(pdata1$policy)) > 1 ) {
        df_lm <- lm(data = pdata1, pdata1[[y]]/1000000 ~ I(-pdata1[[x]]*100) + policy)
        # df_lm <- lm(data = pdata1, change_PoVExp_Rcge/1000000 ~ I(-change_rate_EGHG_Rcge) + policy)
        smry_lm <- summary(df_lm)
        df_smry_tmp <- data.frame(R_CGE = lis_R_CGE[rcge],
                                  intercept = df_lm[["coefficients"]][1] %>% signif(digits = 3) ,
                                  slope_PGHG = df_lm[["coefficients"]][2] %>% signif(digits = 3) ,
                                  p_Full_Combo = df_lm[["coefficients"]][3] %>% signif(digits = 3) ,
                                  # TH = df_lm[["coefficients"]][3],
                                  r_sqr = smry_lm[["adj.r.squared"]] %>% signif(digits = 3) ,
                                  Y = lis_y_for[year], TH = lis_TH_od[th],
                                  revenue = type_redist
        )
      }else if (nrow(pdata1) & length(unique(pdata1$policy)) == 1 ){
        df_lm <- lm(data = pdata1, pdata1[[y]]/1000000 ~ I(-pdata1[[x]]*100))
        smry_lm <- summary(df_lm)
        df_smry_tmp <- data.frame(R_CGE = lis_R_CGE[rcge],
                                  intercept = df_lm[["coefficients"]][1] %>% signif(digits = 3) ,
                                  slope_PGHG = df_lm[["coefficients"]][2] %>% signif(digits = 3) ,
                                  p_Full_Combo = 0 %>% signif(digits = 3)  %>% signif(digits = 3) ,
                                  # TH = df_lm[["coefficients"]][3],
                                  r_sqr = smry_lm[["adj.r.squared"]] %>% signif(digits = 3) ,
                                  Y = lis_y_for[year], TH = lis_TH_od[th],
                                  revenue = type_redist
        )
      }else if (!nrow(pdata1)){
        df_smry_tmp <- data.frame()
        }
      
      df_smry <- df_smry %>% rbind(df_smry_tmp)
    }
  }
  # df_smry_output <- df_smry_output %>% rbind(df_smry)
}


df_smry <- df_smry %>% mutate(intercept = case_when(abs(intercept) < 0.0001 ~ 0, abs(intercept) > 0.0001 ~ intercept), 
                   slope_PGHG = case_when(abs(slope_PGHG) < 0.0001 ~ 0, abs(slope_PGHG) > 0.0001 ~ slope_PGHG), 
                   p_Full_Combo = case_when(abs(p_Full_Combo) < 0.0001 ~ 0, abs(p_Full_Combo) > 0.0001 ~ p_Full_Combo), 
                   r_sqr = case_when(abs(r_sqr) < 0.0001 ~ 0, abs(r_sqr) > 0.0001 ~ r_sqr)) %>% F_TH() 
openxlsx::write.xlsx(df_smry, file = paste0(dir_out, "/csv/regression/1_PoV_", type_reg,"_",x,"_",type_redist,"_summary.xlsx"))





# 
# # 2. National -------------------------------------------------------------
# 
# ## Regression: national Poverty vs Emissions ---------------------------------------------------------------
# df <- df_EGHG_R %>% filter(Source == "tot_anthr") %>% left_join(df_PoV_R) %>% 
#   filter(!is.na(value_PoVExp)) %>% F_scenario_rename() %>% F_scenario() %>% 
#   F_reve_mutate()  %>% F_CarbonB_factor() 
# x <- "EGHG_reductionrate"
# y <- "change_PoVExp"
# 
# 
# 
# ### 1.1.1 plot 2030 2.15-Threshold -----
# 
# pdata <- df %>% 
#   filter(TH == txt_TH, Y %in% c(2030), revenue == type_redist,R_CGE %in% lis_R_CGE_PoV) %>% 
#   filter(!is.na(scenario_Name))
# 
# # plot regression 2030
# # carbon price in "SSP2_500_FC", "SSP2_600_FC" are the same in 2030 but differ afterwards
# # The outlier in the plot is SSP2_400_FC
# p <- ggplot(pdata) +
#   geom_point(aes(x = -.data[[x]] * 100, y = .data[[y]]/1000000, color = scenario_Name, shape = policy)) +
#   geom_smooth(aes(x = -.data[[x]] * 100, y = .data[[y]]/1000000, group = paste0(policy)), color = "grey50", alpha = 0.5, linewidth = 0.2,method = 'lm', formula = 'y ~ x') +
#   geom_hline(yintercept = 0, color = "grey") +
#   facet_wrap(~R_CGE, scales = "free") +
#   labs(x = "Emissions reduction rate | %", y = "Change in poverty headcount | million") +
#   guides(color = guide_legend(title = paste(expression("Carbon Budget | Gt ", CO[2])), ncol = 2),
#          shape = guide_legend(title = "Policy")) +
#   scale_color_manual(values = viridis(12)) +
#   MyTheme
# p
# 
# ggsave(filename = paste0(dir_out, "/fig/SI/regression/1_PoV_PGHG_country_2030_",type_redist,".pdf"), 
#        width = 26, height = 8, units = "cm")
# 
# 
