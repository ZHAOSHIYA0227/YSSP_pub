# This code calculated the average income level of each decile from the distribution function
# By Shiya ZHAO, 2023.08.03
library(stats)

# MESSAGEix
Demand_data <- demandmsg

Mu_exp <- rgdx.param(Demand_data, "Mu_exp") %>%
  gdata::rename.vars(colnames(.), c("R",'Ref','Y','Mu_exp')) %>%
  filter(Ref %in% lis_ref)


Sigma_exp <- rgdx.param(Demand_data, "Sigma_exp") %>%
  gdata::rename.vars(colnames(.), c("R",'Ref','Y','Sigma_exp')) %>%
  filter(Ref %in% lis_ref)


quantile_input <- seq(from = 0.1, to = 1, by = 0.1) 
q <- quantile_input

data_q_value1_msg <- Sigma_exp %>%
  left_join(Mu_exp) %>%
  group_by(Ref,Y,R) %>%
  dplyr::reframe(Sigma_exp = Sigma_exp, Mu_exp = Mu_exp, 
                 q_value = c(qlnorm(q,Mu_exp,Sigma_exp)[-length(q)],1000000)) %>%
  mutate(DEC = as.character(c(t(rep(q*length(q),nrow(.)/length(q))))),
         model = "MESSAGEix")

## AIM-Hub
Demand_data <- demandcge

Mu_exp <- rgdx.param(Demand_data, "Mu_exp") %>%
  gdata::rename.vars(colnames(.), c("R",'Ref','Y','Mu_exp')) %>%
  filter(Ref %in% lis_ref)


Sigma_exp <- rgdx.param(Demand_data, "Sigma_exp") %>%
  gdata::rename.vars(colnames(.), c("R",'Ref','Y','Sigma_exp')) %>%
  filter(Ref %in% lis_ref)


quantile_input <- seq(from = 0.1, to = 1, by = 0.1) 
q <- quantile_input

data_q_value1_cge <- Sigma_exp %>%
  left_join(Mu_exp) %>%
  group_by(Ref,Y,R) %>%
  dplyr::reframe(Sigma_exp = Sigma_exp, Mu_exp = Mu_exp, 
                 q_value = c(qlnorm(q,Mu_exp,Sigma_exp)[-length(q)],1000000)) %>%
  mutate(DEC = as.character(c(t(rep(q*length(q),nrow(.)/length(q))))),
         model = "AIM-Hub")


## integration

data_q_value1 <- data_q_value1_msg %>%
  rbind(data_q_value1_cge) 

data_q_value2 <- data_q_value1 %>% 
  dplyr::rename("q_value_low" = "q_value") %>% 
  mutate(DEC = case_when((DEC != length(q))~ as.character(as.numeric(DEC) + 1),
                          DEC == length(q) ~ NA)) %>% 
  filter(!is.na(DEC)) %>% 
  select(c("model","Ref" , "Y" , "R" ,"q_value_low" ,"DEC"))



#### Function definition for average income calculation 
F_1 <- function(x, Mu_exp, Sigma_exp){
  df <- plnorm(x, meanlog = Mu_exp, sdlog = Sigma_exp)
  return(df)
}

F_2 <- function(x,Mu_exp, Sigma_exp){
  df <- integrate(F_1, lower = 0, upper = x, Mu_exp, Sigma_exp)[["value"]]
}

v.F_2 <- Vectorize(F_2, vectorize.args=c('x', 'Mu_exp', 'Sigma_exp'))

####

data_q_value_tmp <- data_q_value1 %>% 
  left_join(data_q_value2, by = c("model", "Ref" , "Y" , "R" ,"DEC")) %>% 
  mutate_at(c(9),  ~replace(., is.na(.), 0.001)) %>% 
  mutate(a = plnorm(q_value, meanlog = Mu_exp, sdlog = Sigma_exp),
         b = plnorm(q_value_low, meanlog = Mu_exp, sdlog = Sigma_exp),
         denominator = plnorm(q_value, meanlog = Mu_exp, sdlog = Sigma_exp)-plnorm(q_value_low, meanlog = Mu_exp, sdlog = Sigma_exp) , 
         int1 = q_value * plnorm(q_value, meanlog = Mu_exp, sdlog = Sigma_exp) - q_value_low*plnorm(q_value_low, meanlog = Mu_exp, sdlog = Sigma_exp), 
         int2 = v.F_2(q_value, Mu_exp, Sigma_exp) - v.F_2(q_value_low, Mu_exp, Sigma_exp)) %>% 
  mutate( income_average = (int1 - int2)/denominator)

data_q_value <- data_q_value_tmp %>% 
  select(c("model", "Ref", "Y" , "R" , "DEC", "income_average")) %>%
  dplyr::rename("PHIscenario" = "Ref") %>% 
  left_join(MapScenario_main) %>% 
  select("model", "scenario", "R"  ,"Y" ,  "DEC" , "income_average")


pdata <- data_q_value %>% 
  filter(R %in% c("IND", "IDN", "CHN", "ZAF"),
         Y %in% c("2030", "2050"),
         scenario == "1.5D") 
  
# p <- ggplot(data = pdata %>% mutate(DEC = factor(.$DEC, levels = seq(1,10,1) %>% as.character()) )) +
#   geom_point(aes(x = DEC, y = income_average, color = scenario), size = 0.7) +
#   geom_line(aes(x = DEC, y = income_average, group = paste0(model, "_", scenario, "_"), color = scenario, linetype = model)) +
#   MyTheme +
#   facet_wrap(Y~R, scales = "free", ncol = 4)

p <- ggplot(data = pdata %>% mutate(DEC = factor(.$DEC, levels = seq(1,10,1) %>% as.character()) )) +
  geom_col(aes(x = DEC, y = income_average, fill = R), position = "dodge") +
  # geom_line(aes(x = DEC, y = income_average, group = paste0(model, "_", scenario, "_"), color = scenario, linetype = model)) +
  MyTheme +
  facet_wrap(~Y, scales = "free", ncol = 4)

p
ggsave(filename = paste0(dir_fig, "Figure incl2 average income in the 4 countries.png"), width = 20, height = 12, units = "cm")


###
csv_data_q_value <- data_q_value %>% 
  pivot_wider(names_from = "Y", values_from = "income_average") %>% 
  openxlsx::write.xlsx(file = paste0(dir_csv, "/incl2 income_average.xlsx"))

pdata <- data_q_value %>% 
  filter(R %in% c("IND", "IDN", "CHN", "ZAF"),
         Y %in% c("2030", "2050")) %>% 
  pivot_wider(names_from = "scenario", values_from = "income_average") %>% 
  pivot_longer(c("2D", "1.5D"), names_to = "scenario", values_to = "value") %>% 
  mutate(change_rate = (value-No-Miti)/No-Miti)

p <- ggplot(data = pdata %>% mutate(DEC = factor(.$DEC, levels = seq(1,10,1) %>% as.character()) )) +
  geom_point(aes(x = DEC, y = change_rate, color = scenario), size = 0.7) +
  geom_line(aes(x = DEC, y = change_rate, group = paste0(model, "_", scenario, "_"), color = scenario, linetype = model)) +
  MyTheme +
  facet_wrap(Y~R, scales = "free", ncol = 4)
  
p
ggsave(filename = paste0(dir_fig, "Figure incl2 average income changes in the 4 countries.png"), width = 20, height = 12, units = "cm")


