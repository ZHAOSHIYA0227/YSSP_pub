# This file defines the functions for AIM/PHI analysis 
# In the first feature paper and my thesis

# Shiya ZHAO, 2021/12/13


# total decile
#

F_TH <- function(x){
  y <- x %>% left_join(data.frame(TH = lis_TH_od, TH1 = lis_TH)) %>% filter(!is.na(TH1))


}

F_R_CGEfull <- function(x){
  y <- x %>% 
  mutate(R_CGE = case_when(R_CGE=="XSA" ~ "Rest of Asia",
                           R_CGE=="XER" ~ "Rest of Europe",
                           R_CGE=="XNF" ~ "North Africa",
                           R_CGE=="XAF" ~ "Sub-Saharan Africa",
                           R_CGE=="XLM" ~ "Rest of South America",
                           R_CGE=="CIS" ~ "Former Soviet Union",
                           R_CGE=="XOC" ~ "Oceania",
                           R_CGE=="XE25" ~ "EU",
                           R_CGE=="XME" ~ "Middle East",
                           R_CGE=="BRA" ~ "Brazil",
                           R_CGE=="XSE" ~ "Southeast Asia",
                           R_CGE=="CAN" ~ "Canada",
                           R_CGE=="CHN" ~ "China",
                           R_CGE=="IND" ~ "India",
                           R_CGE=="JPN" ~ "Japan",
                           R_CGE=="TUR" ~ "Turkey",
                           R_CGE=="USA" ~ "USA"  )) 
  return(y)
}



# Mutate for scenario name and redistribution -----------------------------
F_reve_mutate <- function(x){
  y <- x %>% 
    mutate(revenue = str_split_i(scenario, "_", 2), 
         scenario_Name = str_split_i(scenario, "_", 1)) %>% 
    mutate(revenue = case_when(is.na(revenue) ~ "Neutral",
                               !is.na(revenue) ~ "Progressive")) 
  return(y)
}


# Segment in the expenditure model ----
SegNum <- function(a){   
  lis_Seg <- c('Fiftytok',
               'ktoTk',
               'TktoHk',
               'HktoM',
               'MtoTM')
  lis_Seg11 <- c(50,
                 1000,
                 10000,
                 100000,
                 1000000)
  lis_Seg3 <- c(10,
                100,
                1000,
                10000,
                100000)
  tmp2 <- data.frame()
  for(i in 1:length(lis_Seg)){
    tmp <- a[which(startsWith(as.character(a$Seg),lis_Seg[i])),]%>%
      mutate(Seg0 = lis_Seg[i], Seg1 = lis_Seg11[i])
    tmp1 <- mutate (tmp, 
                    Seg2 = as.numeric(substring(as.character(tmp$Seg),nchar(lis_Seg[i])+1)),
                    Upper = Seg1+Seg2*lis_Seg3[i],
                    Lower = Seg1+(Seg2-1)*lis_Seg3[i])
    tmp2 <- rbind(tmp2,tmp1)
  }
  tmp3 <- tmp2 #%>%
    # select(-c("Seg0","Seg1","Seg2"))
  return(tmp3)
}




# Food poverty ----
F_FoodPov <- function(df_FPL, dir_input, output_FPL=F, lis_ind = c("CoCA", "CoNA", "CoHD")){
  m_input <- Container$new(dir_input)
  
  # Food expenditure
  df_Exp_fd <- m_input["ExpNational"]$records %>% gdata::rename.vars(colnames(.), c("R", "Ref", "Seg", "Y", "I", "value")) %>% 
    filter(I == "Food and nonalcoholic beverages") %>% 
    mutate(value = value/365) 
  
  # Price change
  df_PQ <- m_input["PQchange"]$records %>% 
    gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "R3", "I", "PQ")) %>% 
    filter(I == "Food and nonalcoholic beverages") 
  
  df_PQ_2010 <- df_PQ %>% filter(Y == 2010) %>% select(-Y, -I) %>% dplyr::rename("PQ_2010" = "PQ")
  
  # Future FPL
  df_FPL_scenario <- df_PQ %>% left_join(df_PQ_2010) %>% select(-R3) %>% left_join(df_FPL %>% select(-Y)) %>% 
    mutate(value_indicator = value_ind_2010 * PQ/PQ_2010) %>% filter(Indicator %in% lis_ind) %>% 
    select("R", "Ref", "Y", "I", "Indicator", "Unit", "value_indicator") 
  
  # Mu_exp and Sigma_exp
  df_Mu_exp <- m_input["Mu_exp"]$records %>% gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "para_Mu_exp")) 
  
  df_Sigma_exp <- m_input["Sigma_exp"]$records %>% gdata::rename.vars(colnames(.), c("R", "Ref", "Y", "para_Sigma_exp")) 

  df_FPL_inc <- df_FPL_scenario %>% mutate(value_indicator_52_inc = value_indicator*365/.52) %>% filter(Indicator %in% lis_ind) %>% 
    select(R, Y, Ref, Indicator, value_indicator_52_inc) %>% distinct()
  
  df_Exp_fd_FPL <- df_Exp_fd %>% left_join(df_FPL_scenario)  %>% SegNum() %>% 
    mutate(FP = case_when(value <= value_indicator ~ 1, value > value_indicator ~ 0)) %>% 
    filter(FP == 1) %>% filter(Indicator %in% lis_ind) %>% 
    dplyr::group_by(R, Ref, Y, I, Indicator, value_indicator) %>% dplyr::reframe(Income = max(Upper)) %>% 
    right_join(df_FPL_inc) %>% mutate(Income = case_when(is.na(Income) ~ value_indicator_52_inc, !is.na(Income) ~ Income)) %>% filter(!is.na(Income)) %>% 
    left_join(df_Mu_exp) %>% left_join(df_Sigma_exp) %>% 
    filter(!is.na(para_Mu_exp)) %>% 
    mutate(value = plnorm(Income, meanlog = para_Mu_exp, sdlog = para_Sigma_exp)) %>% 
    select(R, Ref, Y, Indicator, Income, value, value_indicator)
  
  if(output_FPL==F){
    return(df_Exp_fd_FPL)
  }else{
    return(df_FPL_scenario %>% select(-I))
  }
}

