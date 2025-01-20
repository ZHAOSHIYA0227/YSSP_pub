# This file checks for the infeasible income segments for the expenditure model and decile calculation

# This file is called by tool/ModelComparison/prog/5_Expenditure.R


# Indicator  ----
df_PQ <- ExpNational %>% 
  # filter(I_abb == "FNB") %>% 
  select(c("model", "R", "Y", "scenario",  "I_abb", "PQchange")) %>%
  distinct(model, R, Y, scenario, I_abb, PQchange) 

df_ExpNational <- ExpNational %>% 
  filter(I_abb == "FNB") %>% 
  select(c("model", "R", "Y", "scenario", "DEC",  "value_seg")) %>% 
  pivot_wider(names_from = "DEC", values_from = "value_seg") %>% 
  mutate(no_na = apply(is.na(.),1,sum)) %>% 
  filter(no_na != 0)


df_ind_ExpNational <- df_ExpNational %>% 
  select(c("model", "R", "Y", "scenario", "no_na"))


# decile results ----
df_PoutputNational_msg <- rgdx.param(demandmsg, "PoutputNational") %>% 
  gdata::rename.vars(from = colnames(.), to = c("R", "PHIscenario", "Seg", "Y", "I", "value")) %>%
  filter( PHIscenario %in% lis_ref,
          Y %in% lis_y,
          I == "Education") %>%
  mutate(model = "MESSAGEix") 

df_PoutputNational_cge <- rgdx.param(demandcge, "PoutputNational") %>% 
  gdata::rename.vars(from = colnames(.), to = c("R", "PHIscenario", "Seg", "Y", "I", "value")) %>% 
  filter( PHIscenario %in% lis_ref,
          Y %in% lis_y,
          I == "Education") %>%
  mutate(model = "AIM-Hub") 


df_PoutputNational <- df_PoutputNational_cge %>% rbind(df_PoutputNational_msg) %>% 
  left_join(MapScenario_main) %>% 
  select(-c("PHIscenario", "CGEscenario")) %>% 
  right_join(df_ind_ExpNational, by = c("R", "Y", "scenario", "model")) 

# identify the q_values
Mu_exp_cge <- rgdx.param(demandcge, "Mu_exp") %>%
  rename.vars(colnames(.), c("R",'Ref','Y','Mu_exp')) %>%
  filter(Ref %in% lis_ref) %>% 
  mutate(model = "AIM-Hub")

Sigma_exp_cge <- rgdx.param(demandcge, "Sigma_exp") %>%
  rename.vars(colnames(.), c("R",'Ref','Y','Sigma_exp')) %>%
  filter(Ref %in% lis_ref) %>% 
  mutate(model = "AIM-Hub")


Mu_exp_msg <- rgdx.param(demandmsg, "Mu_exp") %>%
  rename.vars(colnames(.), c("R",'Ref','Y','Mu_exp')) %>%
  filter(Ref %in% lis_ref) %>% 
  mutate(model = "MESSAGEix")


Sigma_exp_msg <- rgdx.param(demandmsg, "Sigma_exp") %>%
  rename.vars(colnames(.), c("R",'Ref','Y','Sigma_exp')) %>%
  filter(Ref %in% lis_ref) %>% 
  mutate(model = "MESSAGEix")

Mu_exp <- Mu_exp_msg %>% 
  rbind(Mu_exp_cge)

Sigma_exp <- Sigma_exp_msg %>% 
  rbind(Sigma_exp_cge)


q <- seq(from = 0.1, to = 1, by = 0.1)

# the q_value for each scenario and each year
data_q_value <- Sigma_exp %>%
  left_join(Mu_exp) %>%
  dplyr::group_by(model,Ref,Y,R) %>%
  dplyr::reframe(Sigma_exp = Sigma_exp, Mu_exp = Mu_exp, 
                   q_value = c(qlnorm(q,Mu_exp,Sigma_exp)[-10],1000000)) %>%
  mutate(DEC = as.character(c(t(rep(q*10,nrow(.)/length(q)))))) %>% 
  pivot_wider(names_from = "DEC", values_from = "q_value") %>% 
  gdata::rename.vars(from = c(seq(1,10,1)), to = paste0("DEC_", c(seq(1,10,1))))



df_PoutputNational_seg_tmp <- df_PoutputNational %>% 
  SegNum() %>% 
  dplyr::group_by(model, R, Y, scenario) %>% 
  reframe(bottom= min(Lower)) %>% 
  left_join(df_ind_ExpNational) %>% 
  select(c("model", "R", "Y", "scenario", "bottom", "no_na"))
  
  
df_PoutputNational_seg <- data_q_value %>% 
  dplyr::rename("PHIscenario" = "Ref") %>% 
  left_join(MapScenario_main) %>% 
  select(-c("PHIscenario", "CGEscenario")) %>% 
  right_join(df_PoutputNational_seg_tmp, by = c("model", "Y", "R", "scenario")) %>% 
  select(c("model", "scenario", "Y", "R", "Sigma_exp", "Mu_exp", "bottom",paste0("DEC_", c(seq(1,10,1))), "no_na"))

openxlsx::write.xlsx(df_PoutputNational_seg, file = paste0(dir_output, "R13/csv/df_PoutputNational_seg.xlsx"))


## AIDADS parameter ----
par <- rgdx.param(AIDADSCalib, "ParaoutNational") %>% 
  gdata::rename.vars(from = colnames(.), to = c("R", "para", "I", "value")) %>%
  left_join(MapI) %>% 
  select(-c("I"))

df_par <- par %>% 
  pivot_wider(names_from = "I_abb", values_from = "value") %>% 
  mutate_at(c(3:ncol(.)), ~replace_na(.,0),0) %>% 
  left_join(df_PoutputNational_seg, relationship = "many-to-many") %>% 
  filter(!is.na(no_na))


par_gamma <- par %>% 
  filter(para == "gamma") 

# par_gamma_tmp <- par_gamma %>% 
#   pivot_wider(names_from = "I_abb", values_from = "value") %>% 
#   mutate_at(c(3:16), ~replace_na(.,0),0) %>% 
#   left_join(df_PoutputNational_seg, relationship = "many-to-many") %>% 
#   filter(!is.na(no_na))


par_gamma_PQ <- par_gamma %>% 
  left_join(df_PoutputNational_seg, relationship = "many-to-many") %>% 
  filter(!is.na(no_na)) %>% 
  left_join(df_PQ) %>% 
  select(c("model", "R", "scenario", "Y", "I_abb", "para", "no_na", "value", "PQchange"))


par_lowerbound <- par_gamma_PQ %>% 
  dplyr::group_by(model, R, scenario, Y, no_na) %>% 
  dplyr::reframe(subsistent = sum(value * PQchange) ) %>% 
  left_join(df_PoutputNational_seg)

openxlsx::write.xlsx(par_lowerbound, file = paste0(dir_output, "R13/csv/par_lowerbound.xlsx"))

