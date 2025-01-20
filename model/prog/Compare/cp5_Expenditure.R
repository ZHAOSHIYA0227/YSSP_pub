# This program is a summary of all the figures and plots in the paper 
# "Poverty and inequality in China under the long-term global temperature target"
# This program deals with Expenditure and its change

# Shiya ZHAO, 2021/06/17

## Loading packages ----
require(gdxrrw)
require(ggplot2)
require(tidyverse)
require(cowplot)
require(gdata)
require(grid)
require(Rmisc)
require(scales)
require(hrbrthemes)
require(foreach)
require(doParallel)
require(parallel)
require(gamstransfer)

getwd()


# 0. load data ------------------------------------------------------------
m_cge <- Container$new("../output/gdx/Decile_AIMHub.gdx")
m_msg <- Container$new("../output/gdx/Decile_MESSAGEix.gdx")

df_budget <- F_load_dec(variable = "output_Budget", lis_colname = c("R", "R3", "Ref", "DEC", "Y", "value", "model"))  %>% 
  mutate(DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character, "ALL"))) %>% F_reve_mutate() %>% 
  dplyr::select(-Ref) %>% mutate(scenario = paste0(target,"_",revenue))

df_Exp <- F_load_dec(variable = "output_Exp_I", lis_colname = c("R", "R3", "Ref", "DEC", "I", "Y", "value", "model")) %>% 
  mutate(DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character, "ALL"))) %>% 
  left_join(MapI) %>% mutate(I_abb= factor(I_abb, levels = lis_I_abb)) %>% 
  dplyr::select("model", "R", "R_CGE", "Y", "Ref", "DEC", "I_abb", "value") %>% F_reve_mutate() %>% dplyr::select(-Ref) %>% mutate(scenario = paste0(target,"_",revenue))


df_ExpShare <- F_load_dec(variable = "output_ExpShare_I", lis_colname = c("R", "R3", "Ref", "DEC", "I", "Y", "value", "model")) %>% 
  mutate(DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character, "ALL"))) %>%
  left_join(MapI) %>% mutate(I_abb= factor(I_abb, levels = lis_I_abb)) %>% 
  dplyr::select("model", "R", "R_CGE", "Y", "Ref", "DEC", "I_abb", "value") %>% F_reve_mutate() %>% dplyr::select(-Ref) %>% mutate(scenario = paste0(target,"_",revenue))


df_Con <- F_load_dec(variable = "output_Con_I", lis_colname = c("R", "R3", "Ref", "DEC", "I", "Y", "value", "model")) %>% 
  mutate(DEC = factor(DEC, levels = c(seq(1,10,1) %>% as.character, "ALL"))) %>%
  left_join(MapI) %>% mutate(I_abb= factor(I_abb, levels = lis_I_abb)) %>% 
  dplyr::select("model", "R", "R_CGE", "Y", "Ref", "DEC", "I_abb", "value") %>% F_reve_mutate() %>% dplyr::select(-Ref) %>% mutate(scenario = paste0(target,"_",revenue))


# Expenditure share block ----
lis_y <- c("2020", "2030", "2050", "2070")


# 1_expenditure share in the baseyear ----

# for(y in 1:length(lis_y)){
#   # y <- 1
#   pdata <- df_Exp %>%
#     filter(Y %in% lis_y[y], scenario == 'No-Miti', DEC != "ALL") %>%
#     na.omit()
#   
#   p_5a  <- ggplot(data =  pdata %>% filter(R == "CHN"),
#                   mapping = aes(x = DEC, y = value,  # factor(quantile*10,levels = lis_DEC)
#                                 fill = I_abb)) + 
#     geom_col(position = 'fill',
#              show.legend  = TRUE) +
#     labs(title = lis_y[y], # Expenditure pattern
#       subtitle = '(Baseline, 2020)',
#       x = "Income decile",
#       y = "",
#       fill = "gear") +
#     MyTheme +
#     facet_wrap(model~R, ncol = 4) +
#     cowplot::panel_border(color = "grey85", size = 1, linetype = 1)+
#     guides(fill = guide_legend(title = 'Commodity', ncol = 1, byrow = TRUE), size = 7) +
#     scale_fill_manual(values = palette_COM) 
#   
#   
#   p_5a
#   ggsave(p_5a, filename = paste0(dir_output, "/fig/SI/5_country_ExpenditureShare/", lis_y[y], "_", lis_R[r]," No-Miti.pdf"),
#          width = 26, height = 14, units = "cm")
#   
# }


# 2_evolution of expenditure share ----
ExpNational2 <- df_ExpShare


## Plot ----
lis_R_concern <- unique(ExpNational2$R)

for(r in 1:length(lis_R_concern)){
  # r <- 7
  pdata <- filter(ExpNational2,
                  Y %in% lis_y, 
                  R == lis_R_concern[r],
                  target %in% c('No-Miti')) 

  p_5b  <- ggplot() + 
    geom_line(data = pdata %>% filter(DEC != "ALL", gini == "consistent"),mapping = aes(x = DEC, y = value,color = Y, group =paste0(Y, model),linetype = model)) +
    geom_ribbon(data = pdata %>% filter(DEC != "ALL") %>% mutate(pivot_name = paste0(gini, "_", model)) %>% dplyr::select(-gini, -model) %>% 
                  pivot_wider(values_from = "value", names_from = "pivot_name") %>% 
                  mutate(max = apply(.[,(ncol(.)-9):ncol(.)],1,max), 
                         min = apply(.[,(ncol(.)-9):ncol(.)],1,min)), 
                mapping = aes(x = DEC, ymin = min, ymax = max, fill = Y, group = paste0(Y)), alpha = .3) +
    geom_point(data = pdata %>% filter(DEC != "ALL", gini == "consistent"),mapping = aes(x = DEC, y = value,color = Y), shape = 1, size = 0.3) +
    geom_hline(data = pdata %>% filter(DEC == "ALL", gini == "consistent"),mapping = aes(yintercept = value, color = Y), linetype = "dashed") +
    labs(title = lis_R_concern[r], 
         x = "Income decile",
         fill = "Year", color = "Year", linetype = "Model")+
    xlab("Income decile") +
    ylab("Expenditure share (%)") +
    facet_wrap(~ I_abb, scales = "free", ncol = 5) + #fct_rev
    MyTheme 
  
  
  # p_5b
  dir.create(paste0(dir_output_YSSPpaper, "/fig/SI/5_ExpShare/"))
  ggsave(p_5b, filename = paste0(dir_output_YSSPpaper, "/fig/SI/5_ExpShare/5_", lis_R_concern[r], "_Expenditure share_BaU.pdf"),
         width = 28, height = 18, units = "cm")
  
}
print("END loop 1")
rm(ExpNational2)

save(df_budget, df_ExpShare, df_Exp,file = paste0(dir_output_YSSPpaper, "/Rdata/5_df_ExpShare.RData"))

# run Decile analysis
source(paste0(dir_ModelComparison, "prog/Compare/cp6_DecileDataAnalysis.R"))

rm(df_PQ, df_Con, df_EV)
rmarkdown::render(input = paste0(dir_ModelComparison, "/prog/Compare/cp5_7_EnergyShare.Rmd"))

rmarkdown::render(input = paste0(dir_ModelComparison, "/prog/Compare/cp5_8_FoodShare.Rmd"))
rm(df_budget, df_ExpShare, df_Exp, pdata, m_cge, m_msg, p_5b, df_Con)
print("The END of Compare/5_Expenditure.R")


