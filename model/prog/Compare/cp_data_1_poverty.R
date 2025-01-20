require(gdxrrw)

# 0. function ----
F_read_var <- function(var = "PoVExp", directory = "../output/gdx/ConsumptionResults/AnalysisExpenditure_"){
  df <- data.frame()
  df_tmp <- data.frame()
  for(m in seq_along(lis_model)){
    if(lis_model[m] == "AIM-Hub"){
      txt_m <- "AIMHub"
    }else{
      txt_m <- lis_model[m]
    }
    
    gdx_in <- paste0(directory, txt_m, ".gdx")
    
    df_tmp <- rgdx.param(gdx_in, var) %>% 
      filter(Y %in% seq(2010,2100,5)) %>%
      mutate(model = lis_model[m]) 
    
    df <- rbind(df, df_tmp)
  }
  
  return(df)
}


# calculate the change from the baseline

F_change <- function(df, wide = T, multi_m = T){
  df1 <- df %>% filter(target == "No-Miti") %>% select(-target, -revenue) %>% dplyr::rename(value_BaU = value)
  df2 <- df %>% left_join(df1) %>% mutate(change = value-value_BaU)
  
  if(wide == T){
    if(multi_m == T){
      df3 <- df2 %>% mutate(group = paste0(model, "_", gini)) %>% select(-value_BaU, -value, -model, -gini)
      lis_group <- unique(df3$group)
      df4 <- df3  %>% pivot_wider(values_from = "change", names_from = "group") %>% 
        mutate(min = apply(.[which(colnames(.) %in% lis_group)], 1, min), 
               max = apply(.[which(colnames(.) %in% lis_group)], 1, max),
               median = apply(.[which(colnames(.) %in% lis_group)], 1, median))
      return(df4)
      
    }else{
      df3 <- df2 %>% select(-value_BaU, -value) %>% pivot_wider(values_from = "change", names_from = "gini") %>% 
        mutate(min = apply(.[which(colnames(.) %in% c("SSP1", "consistent", "SSP3", "SSP4", "SSP5"))], 1, min), 
               max = apply(.[which(colnames(.) %in% c("SSP1", "consistent", "SSP3", "SSP4", "SSP5"))], 1, max),
               median = apply(.[which(colnames(.) %in% c("SSP1", "consistent", "SSP3", "SSP4", "SSP5"))], 1, median) )
      return(df3)
    }
  }else{
    return(df2)
  }
  
}


# range of statistics

F_range <- function(df){
  df1 <- df %>% mutate(group = paste0(model, "_", gini)) %>% select(-model, -gini)
  lis_group <- unique(df1$group)
  df2 <- df1  %>% pivot_wider(values_from = "value", names_from = "group") %>% 
    mutate(min = apply(.[which(colnames(.) %in% lis_group)], 1, min), 
           max = apply(.[which(colnames(.) %in% lis_group)], 1, max),
           median = apply(.[which(colnames(.) %in% lis_group)], 1, median) )
}


# occurrences of poverty risk thresholds
# x <- df_PRT_day
# var_rev = lis_rev_loop[i]
F_occurence <- function(x, n = 20, var_gini = "consistent", var_model = "AIM-Hub", var_y = "2030", var_th = "2.15-threshold", var_rev ="Neutral", var_target = "1.5C"){
  y0 <- x %>% filter(gini %in% var_gini) %>% 
    filter(model %in% var_model, Y %in% var_y, TH %in% var_th, revenue %in% var_rev, target %in% var_target) %>% filter(!(R %in% c("MAC", "HKG"))) %>% 
    # dplyr::group_by(model, Y, R, TH, gini, revenue, target, variable) %>% 
    arrange(desc(value))
  y <- y0[1:n,] %>% left_join(Map_r_PHI2Hub) %>% #mutate(rank = row_number()) %>% #filter(!(R %in% c("MAC", "HKG"))) %>% 
    dplyr::group_by(R_CGE, model) %>% reframe(count = n())
  
  return(y)
}


# 1. Data ----
## 1.1 Poverty headcount ----
df_PoV <- F_read_var(var = "PoVExp", directory = "../output/gdx/ConsumptionResults/AnalysisExpenditure_")


## 1.2 Poverty rate ----
df_RoP <- F_read_var(var = "RoPExp", directory = "../output/gdx/ConsumptionResults/AnalysisExpenditure_")

## 1.3 Gini ----
df_Gini <- F_read_var(var = "Gini_exp", directory = "../output/gdx/ConsumptionResults/AnalysisExpenditure_")
unique(df_Gini$R)
# Gini range
# global median of the gini coefficients 
# colnames(df_Gini)
df_Gini_median <- df_Gini %>% dplyr::group_by(Ref,Y,model) %>% reframe(value = median(Gini_exp), R = "World") %>% 
  rbind(df_Gini %>% left_join(Map_r_PHI2Hub) %>% dplyr::group_by(Ref, R_CGE,Y,model) %>% 
          reframe(value = median(Gini_exp)) %>% mutate(R = paste0("R17",R_CGE)) %>% select(-R_CGE)) %>%  
  rbind(df_Gini %>% dplyr::rename(value = Gini_exp)) %>% mutate(value = value * 100)

## 1.4 Food poverty ----
# load data
load(paste0(dir_output_YSSPpaper, "/RData/Food poverty.RData"))

# data processing
df_FP_R <- df_FoodPov_p %>% mutate(rate0 = value, headcount0 = value*POP, variable = "Food poverty", TH = Indicator) %>% filter(!is.na(value)) %>% 
  select(-revenue, -target, -gini,-value,-Unit_POP, -R_CGE, -Indicator, -unit, -adj, -scenario)

df_FP <- df_FP_R %>% left_join(Map_r_PHI2Hub %>% filter(R_CGE != "WLD")) %>% 
  dplyr::group_by(model, Ref, Y, variable, TH, R_CGE) %>% reframe(headcount = sum(headcount0), rate = sum(headcount0)/sum(POP)) %>% mutate(R = paste0("R17",R_CGE)) %>% select(-R_CGE) %>%  
  rbind(df_FP_R %>% filter(R != "WLD") %>% dplyr::group_by(model, Ref, Y, variable, TH) %>% reframe(headcount = sum(headcount0), rate = sum(headcount0)/sum(POP)) %>% mutate(R = "World")) %>% 
  rbind(df_FP_R %>% dplyr::rename(rate = rate0, headcount = headcount0) %>% select(-POP))
colnames(df_FP_R)
# the two dataframes that we need
df_FP_headcount <- df_FP %>% mutate(variable = "Food poverty headcount", value = headcount) %>% select(-rate, -headcount)
df_FP_rate <- df_FP %>% mutate(variable = "Food poverty rate", value = rate) %>% select(-rate, -headcount)
rm(df_FP_R, df_FP)

# unique(Map_r_PHI2Hub$R_CGE)
unique(df_FP_headcount$R)
# length(unique(df_FP$R))
# length(unique(df_FPL$R))
# length(unique(df_FoodPov$R))
# colnames(df_FP_rate)
# colnames(df_PRT)


## 1.4 poverty risk threshold ----
# load data
load(paste0(dir_output_YSSPpaper, "/RData/1_PRT.RData"))
df_PRT_day <- df_PRT %>% dplyr::rename(value = PRT, TH = TH1) %>% select(model, Y, R, value, TH, gini, revenue, target) %>% mutate(variable = "PRT")
unique(df_PRT_day$model)

lis_rev_loop <- unique(df_PRT_day$revenue)
df_PRT_rank <- data.frame()
# lis_model
for(m in seq_along(lis_model)){
  for(rev in seq_along(lis_rev_loop)){
    df_PRT_rank_tmp <- df_PRT_day %>% F_occurence(var_rev = lis_rev_loop[rev], var_model = lis_model[m]) %>% mutate(Y = "2030", revenue = lis_rev_loop[rev])
    df_PRT_rank <- df_PRT_rank %>% rbind(df_PRT_rank_tmp)
    rm(df_PRT_rank_tmp)
  }
}
rm(lis_rev_loop, df_PRT_rank_tmp)
print(df_PRT_rank)

df_PRT_rank %>% F_R_CGEfull() %>% pivot_wider(names_from = "model", values_from = "count") %>% openxlsx::write.xlsx(file = paste0(dir_output_YSSPpaper, "/csv/1_PRLrank.xlsx"))


## 1.5 consumption loss ----
# load data 
load(paste0(dir_output_YSSPpaper, "/RData/6_consumptionloss.RData"))

# data processing
# percentage change 
df_cnsloss_R <- df_cnsloss_d %>% select(model,R,Y, gini, target, revenue, DEC, change) %>% mutate(change = change * 100, variable = "Budget loss") %>% dplyr::rename(TH = DEC, value = change)

# global
df_cnsloss_Rall <- df_cnsloss_R %>% left_join(Map_r_PHI2Hub %>% filter(R_CGE != "WLD")) %>% 
  dplyr::group_by(model, gini, target, revenue, Y, variable, TH, R_CGE) %>% reframe(value = median(value)) %>% mutate(R = paste0("R17",R_CGE)) %>% select(-R_CGE) %>%  
  rbind(df_cnsloss_R %>% filter(R != "WLD") %>% dplyr::group_by(model, gini, target, revenue, Y, variable, TH) %>% reframe(value = median(value)) %>% mutate(R = "World")) %>% 
  rbind(df_cnsloss_R)
rm(df_cnsloss_R)



## 1.6 consumption basket ----
# load data
load(file = paste0(dir_output_YSSPpaper, "/RData/6_EVi.RData"))
colnames(df_EV)
colnames(df_PoV)
df_EVi_R <- df_EV %>% ungroup() %>% select(model, revenue, target, gini, R, Y, DEC, I_abb, EVi) %>% 
  mutate(TH = paste0(DEC, "_", I_abb), variable = "EVi", value = EVi*100) %>% select(-DEC, -I_abb, -EVi) 


df_EV_R <- df_EV %>% ungroup() %>% select(model, revenue, target, gini, R, Y, DEC,  EV) %>% distinct() %>% 
  mutate(variable = "EV", value = EV*100, TH = DEC) %>% select(-DEC, -EV) 

df_EV_EVi_Rall <- rbind(df_EV_R, df_EVi_R)  %>% left_join(Map_r_PHI2Hub %>% filter(R_CGE != "WLD")) %>% 
  dplyr::group_by(model, gini, target, revenue, Y, variable, TH, R_CGE) %>% reframe(value = median(value)) %>% mutate(R = paste0("R17",R_CGE)) %>% select(-R_CGE) %>%  
  rbind(rbind(df_EV_R, df_EVi_R) %>% filter(R != "WLD") %>% dplyr::group_by(model, gini, target, revenue, Y, variable, TH) %>% reframe(value = median(value)) %>% mutate(R = "World")) %>% 
  rbind(df_EV_R) %>% rbind(df_EVi_R)

rm(df_EV_R, df_EVi_R, df_EV)
unique(df_EV_EVi_Rall$R) 

df_PQ_R <- df_PQ %>% select(model, R, Y, Ref, I_abb, value) %>% distinct() %>% dplyr::rename(TH = I_abb) %>% mutate(variable = "Price")

df_PQ_Rall <- df_PQ_R %>% left_join(Map_r_PHI2Hub %>% filter(R_CGE != "WLD")) %>% 
  dplyr::group_by(model, Ref, Y, variable, TH, R_CGE) %>% reframe(value = median(value)) %>% mutate(R = paste0("R17",R_CGE)) %>% select(-R_CGE) %>%  
  rbind(df_PQ_R %>% filter(R != "WLD") %>% dplyr::group_by(model, Ref, Y, variable, TH) %>% reframe(value = median(value)) %>% mutate(R = "World")) %>% 
  rbind(df_PQ_R)
rm(df_PQ, df_PQ_R)

## 1.7 Marginal policy effects ----
# load data
load(paste0(dir_output_YSSPpaper, "/RData/reg_summary.RData"))
df_mpe_tot <- df_smry_tot_pov %>% mutate(variable = "Poverty") %>% rbind(df_smry_tot_gini %>% mutate(variable = "Gini", TH1 = "point", TH = "point")) %>% 
  dplyr::rename(R = R_CGE) %>% select(-TH1) %>% mutate(type = "Marginal policy effect") %>% #pivot_longer(c("intercept", "slope_PGHG", "p_Full_Combo")) %>% 
  mutate(target = "dummy")


## 1.8 tax revenue -----
# load the data
load(paste0(dir_output_YSSPpaper, "/RData/Poverty_gap_tax_revenue.RData"))
df_txrev <- Tx_capita
a <- Tx_capita %>% left_join(Map_r_PHI2Hub) %>% filter(!is.na(R_CGE))

# combining dataframes
df <- df_PoV %>% mutate(variable = "Poverty Headcount") %>% dplyr::rename(value = PoVExp) %>%  
  rbind(df_RoP %>% mutate(variable = "Poverty rate") %>% dplyr::rename(value = RoPExp)) %>%
  rbind(df_Gini_median %>% mutate(variable = "Gini", TH = "point")) %>% 
  rbind(df_PQ_Rall) %>% 
  # rbind(df_Gini %>% mutate(variable = "Gini", TH = "point", value = Gini_exp * 100) %>% select(-Gini_exp)) %>%
  rbind(df_FP_headcount) %>% rbind(df_FP_rate) %>%
  F_reve_mutate() %>% select(-Ref) %>%
  rbind(df_EV_EVi_Rall) %>% rbind(df_PRT_day) %>% rbind(df_cnsloss_Rall) %>% 
  mutate(R = case_when(R == "WLD" ~ "World", R!="WLD" ~ R))


# unique(df$R)
# 2. change ----
a <- df_Gini_median %>% F_reve_mutate() %>% select(-Ref) %>% F_change()
df_change <- df %>%  F_change() # filter(!(variable %in% c( "EV","Consumption loss"))) %>%
rm(a)

df_change_2m <- df_change %>% select(-colnames(.)[which(grepl("_SSP", colnames(.)))])
colnames(df_change_2m)


openxlsx::write.xlsx(df_change, file = paste0(dir_output, "/csv/1_change in variables.xlsx"))

# 3. range ----
# colnames(df_Gini_median%>% mutate(variable = "Gini_median", TH = "point") %>% F_reve_mutate() %>% select(-Ref))

df_range <- df %>% 
  rbind(df_Gini_median%>% mutate(variable = "Gini_median", TH = "point") %>% F_reve_mutate() %>% select(-Ref)) %>% 
  F_range()
colnames(df_range)
unique(df_range$variable)

df_range_2m <- df_range %>% select(-colnames(.)[which(grepl("_SSP", colnames(.)))])
unique(df_range_2m$TH)

save(df_range, df_change, df_range_2m, df_change_2m, df_PRT_rank, file = paste0(dir_output, "/RData/1_variable ranges and changes.RData"))
openxlsx::write.xlsx(df_range, file = paste0(dir_csv, "1_variable ranges.xlsx"))

# Gini change range
# global median of the Gini change
df_Gini_change_median <- df_change %>% filter(variable == "Gini") %>% 
  pivot_longer(c("AIM-Hub_consistent", "AIM-Hub_SSP1", "AIM-Hub_SSP3", "AIM-Hub_SSP4", "AIM-Hub_SSP5",
                 "MESSAGEix_consistent", "MESSAGEix_SSP1", "MESSAGEix_SSP3", "MESSAGEix_SSP4", "MESSAGEix_SSP5" ),
               values_to = "change", names_to = "group") %>% select(-min, -max) %>% 
  dplyr::group_by(Y, TH, variable, revenue, target, group) %>% reframe(value = median(change)) %>% 
  mutate(model = str_split_i(group, "_",1), gini = str_split_i(group, "_",2)) %>% 
  F_range()


# regional median of the Gini change
df_Gini_change_median_Rcge <- df_change %>% filter(variable == "Gini") %>% 
  pivot_longer(c("AIM-Hub_consistent", "AIM-Hub_SSP1", "AIM-Hub_SSP3", "AIM-Hub_SSP4", "AIM-Hub_SSP5",
                 "MESSAGEix_consistent", "MESSAGEix_SSP1", "MESSAGEix_SSP3", "MESSAGEix_SSP4", "MESSAGEix_SSP5" ),
               values_to = "change", names_to = "group") %>% select(-min, -max) %>% left_join(Map_r_PHI2Hub) %>% 
  dplyr::group_by(Y, TH, R_CGE, variable, revenue, target, group) %>% reframe(value = median(change)) %>% 
  mutate(model = str_split_i(group, "_",1), gini = str_split_i(group, "_",2)) %>% 
  F_range()

rm(df_Gini_change_median_Rcge, df_change, df_Gini_change_median, df_range, df,
   df_PoV, df_RoP, df_Gini_median, df_PQ_Rall, df_Gini, df_FP_headcount,
   df_FP_rate, df_EV_EVi_Rall, df_PRT_day, df_cnsloss_Rall)



