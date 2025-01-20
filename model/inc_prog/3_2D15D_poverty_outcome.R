# Checking where 2D is more significantly affecting poverty than 1.5D and why
# Bugs in the model?
# Mapping problem? (N)
# Price and income? 



# 0. Data -----------------------------------------------------------------

PoVExpmsg_tmp <- rgdx.param(AnaExpmsg, "PoVExp") %>%
  filter(Ref %in% lis_ref ,
         Y%in% c(seq(2010,2100,5)))%>%
  mutate(Effect = 'Expenditure',
         model = "MESSAGEix") %>%
  gdata::rename.vars("Ref", "PHIscenario") %>%
  left_join(MapScenario_main)
# PoVExpmsg_tmp %>% filter(R == "RUS")

PoVIncmsg_tmp <- rgdx.param(AnaIncmsg, "PoV") %>%
  filter(Ref %in% lis_ref,
         Y %in% c(seq(2010,2100,5))) %>%
  mutate(Effect = 'Income',
         model = "MESSAGEix") %>%
  gdata::rename.vars("Ref", "PHIscenario") %>% 
  left_join(MapScenario_main)


PoVmsg <- PoVExpmsg_tmp %>% 
  rename.vars(from = 'PoVExp', to = 'PoV') %>%
  rbind(PoVIncmsg_tmp)


# 1. Comparing ------------------------------------------------------------

PoVmsg1 <- PoVmsg %>% 
  select("R", "Y", "TH", "PoV", "Effect",
         "model", "scenario") %>% 
  pivot_wider(names_from = "scenario", values_from = "PoV") %>% 
  filter(TH == "pop_5.5",
         as.character(Y) %>% as.numeric() <= 2060) %>% 
  mutate(dif = `1.5D`-`2D`) %>% 
  filter(dif <= -1000) 

PoVmsg2 <- PoVmsg1 %>% 
  left_join(Map_r_PHI2msg) %>% 
  distinct(R, .keep_all = T) %>% 
  filter(!is.na(R_MSG), R_MSG != "WLD") %>% 
  dplyr::group_by(Y, TH, Effect, model, R_MSG)
# %>% 
  dplyr::reframe(count = unique(R) %>% length())



# 2. Take CHN for example ----------------------------------------------------
PoVmsg_CHN <- PoVmsg1 %>% 
  filter(R == "CHN")

PriceChange_CHN <- rgdx.param(input_msg, "PriceChange") %>% 
  gdata::rename.vars(from = c("Ref", "PriceChange"), to = c("PHIscenario", "value")) %>% 
  left_join(Map_r_PHI2msg) %>% 
  left_join(Map_R12_R7) %>% 
  mutate(model = "MESSAGEix") %>% 
  filter(Y %in% as.character(seq(2010, 2100, 10)), !is.na(R7), R_MSG == "CHN")%>% 
  left_join(MapScenario_main) %>% 
  select(c("model","scenario", "R", "R7",  "Y", "I", "value")) %>% 
  filter(Y == "2030",
         )

Incomeloss_CHN <- msg_iamc <- msg_var %>% 
  rgdx.param("message_iamc") %>%
  gdata::rename.vars(colnames(.), c("model","MSGscenario", "R", "variable", "unit", "Y", "value")) %>% 
  filter(model == "MESSAGEix-GLOBIOM 1.1-R12") %>% 
  filter(variable == "GDP|PPP") %>% 
  mutate_at("value", ~replace_na(.,0)) %>% 
  left_join(MapScenario_msg) %>% 
  mutate(GDP = value*1000) %>% 
  select(-c("model", "variable", "unit", "value", "MSGscenario")) %>% 
  mutate(model = "MESSAGEix")
# billion US$ 2010 per yr --> million US$ 2010 per yr to keep consistency with AIM-Hub


