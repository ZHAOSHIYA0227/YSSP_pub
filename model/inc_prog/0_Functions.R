# This file defines the functions for AIM/PHI analysis 
# In the first feature paper and my thesis

# Shiya ZHAO, 2021/12/13

# renaming the gini assumptions
F_gini_rename <- function(x){
  y <- x %>% mutate(gini = case_when(gini == "consistent" ~ "SSP2", gini != "consistent" ~ gini))
  return(y)
}

# total decile
# split scenario to target and revenue
F_split_sce <- function(x){
  y <- x %>% mutate(target = str_split_i(scenario, "_", 1), revenue = str_split_i(scenario, "_", 2)) 
  return(y)
}

# for loading data from Decile_%model%.gdx
F_load_dec <- function(variable,lis_colname = c("R", "R3", "Ref", "DEC", "Y", "value", "model")){
  y <- m_cge[variable]$records %>% mutate(model = "AIM-Hub") %>% 
    rbind(m_msg[variable]$records %>% mutate(model = "MESSAGEix"))
  colnames(y) <- lis_colname
  y <- y %>% dplyr::select(-"R3") %>% #dplyr::rename("PHIscenario" = "Ref") %>% left_join(MapScenario) %>% 
    left_join(Map_r_PHI2Hub)
  return(y)
}


# ribbon and line plot
F_plot_ribbon <- function(pdata, ribbon = T, txt_y = "Million people"){
  
  pdata1 <- pdata %>% filter(gini == "consistent")
  
  p <- ggplot() +
    # geom_point(pdata, mapping = aes(x = Y, y = mean, color = scenario)) +  
    geom_line(pdata1, mapping = aes(x = Y, y = value, color = v_color, linetype = model,
                                    group = v_group)) +
    geom_point(pdata1, mapping = aes(x = Y, y = value, shape = model, color = v_color, group = v_group)) +
    MyTheme  +
    labs(y = txt_y, x = "Year") +
    scale_x_discrete(breaks = seq(2020, 2100,20)) +
    scale_shape_manual(values = palette_shape_model) +  
    # scale_color_manual(values = palette_Sc) +
    # scale_fill_manual(values = palette_Sc) +
    guides(# color = guide_legend(title = "Scenario"), fill = guide_legend(title = "Scenario"), 
      linetype = guide_legend(title = "Model"), shape = guide_legend(title = "Model"))
  
  if(ribbon == T){
    pdata2 <- pdata %>% pivot_wider(names_from = "gini", values_from = "value") %>% 
      mutate(max = apply(.[which(colnames(.) %in% c("SSP1", "consistent", "SSP3", "SSP4", "SSP5"))], 1, max),
             min = apply(.[which(colnames(.) %in% c("SSP1", "consistent", "SSP3", "SSP4", "SSP5"))], 1, min)) %>% dplyr::select(-c("consistent", "SSP1", "SSP3", "SSP4", "SSP5"))
      # mutate(min = min(SSP1, consistent, SSP3, SSP4, SSP5), max = max(SSP1, consistent, SSP3, SSP4, SSP5))
    
    p <- p + geom_ribbon(data = pdata2, aes(x = Y, ymin = min, ymax = max, group = v_group,fill = v_color), stat = "identity", alpha=0.2) 
  }
  

return(p) 

}


# poverty thresholds
F_TH <- function(x){
  y <- x %>% left_join(data.frame(TH = lis_TH_od, TH1 = lis_TH)) %>% filter(!is.na(TH1))
    
    
    # mutate(TH1 = recode(TH,
    #                   lis_TH_od[1] = lis_TH[1],
    #                   lis_TH_od[2] = lis_TH[2],
    #                   lis_TH_od[3] = lis_TH[3])) 
}

# change
F_change <- function(x){
  
  y <- x %>% mutate(scenario = paste0(target, "_", revenue)) %>% 
    dplyr::select(-target, -revenue) %>% 
    pivot_wider(names_from = "scenario", values_from = "value") %>% 
    pivot_longer(c("2C_Neutral", "1.5C_Neutral","2C_EPC", "1.5C_EPC"), names_to = "scenario", values_to = "value") %>% 
    mutate(change = `value`-`No-Miti_Neutral`, type_variable = "Additional") %>% 
    dplyr::select(c("model", "R", "Y", "scenario", "gini", "value", "change")) %>% 
    mutate(target = str_split_i(scenario, "_", 1),
           revenue = str_split_i(scenario, "_", 2)) 
  return(y)
}

# region full names
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
                           R_CGE=="USA" ~ "USA" ,
                           R_CGE=="WLD" ~ "World" )) 
  return(y)
}


# Decile calculation updated ----
F_quantile <- function(Consumption_data, OD, df_q = seq(0.1,1,0.1), StepLength=200, LengthThreshold=500, split_all = T, budget_highest=1000000){
  
  # df_FreqSegall <- rgdx.param(paste0("../output/gdx/demand_decomposition/country/JPN.gdx"), "FreqSegall") %>%
  #   gdata::rename.vars(from = colnames(.), to = c("scenario","Y","Seg","pop")) %>%
  #   mutate(R = "JPN") %>% 
  #   filter(scenario %in% lis_scenario_main,
  #          Y %in% lis_y) %>% 
  #   SegNum() %>% 
  #   dplyr::select(c("scenario", "Y", "Seg", "Upper")) %>% 
  #   gdata::rename.vars("Upper", "value") %>% 
  #   mutate(I = "total")
  
  lis_scenario_function <- unique(OD$scenario)
  lis_y_function <- unique(OD$Y)
  lis_I_function <- unique(OD$I)
  
  df_Mu_exp <- rgdx.param(Consumption_data, "Mu_exp") %>% 
    gdata::rename.vars(colnames(.),c("R", "scenario","Y","para_Mu_exp")) 
  
  df_Sigma_exp <- rgdx.param(Consumption_data, "Sigma_exp") %>%
    gdata::rename.vars(colnames(.),c("R", "scenario","Y","para_Sigma_exp")) 
  
  OD1 <- OD %>% left_join(df_Mu_exp) %>% left_join(df_Sigma_exp) 
  
  Seg_pop <- rgdx.param(Consumption_data, "FreqSegall")
  colnames(Seg_pop) <- c("R",'scenario','Y','Seg','pop')
  # Seg_pop <- filter(Seg_pop, Y %in% lis_y) %>%
  #   SegNum(budget_highest)
  
  data0 <- OD1 %>% 
    SegNum() %>% 
    left_join(Seg_pop) %>% 
    dplyr::select(c("scenario", "R", "Y", "Seg", "I", "value", "Upper", "Lower", "para_Mu_exp", "para_Sigma_exp", "pop")) %>% 
    mutate(DEC = case_when(Lower <= qlnorm(0.1, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 1,
                           qlnorm(0.1, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.2, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 2,
                           qlnorm(0.2, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.3, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 3,
                           qlnorm(0.3, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.4, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 4,
                           qlnorm(0.4, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.5, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 5,
                           qlnorm(0.5, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.6, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 6,
                           qlnorm(0.6, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.7, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 7,
                           qlnorm(0.7, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.8, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 8,
                           qlnorm(0.8, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower & Lower <= qlnorm(0.9, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) ~ 9,
                           qlnorm(0.9, meanlog = para_Mu_exp, sdlog = para_Sigma_exp) < Lower ~ 10),
           q_value = case_when(DEC == 1 ~ qlnorm(0.1, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 2 ~ qlnorm(0.2, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 3 ~ qlnorm(0.3, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 4 ~ qlnorm(0.4, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 5 ~ qlnorm(0.5, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 6 ~ qlnorm(0.6, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 7 ~ qlnorm(0.7, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 8 ~ qlnorm(0.8, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 9 ~ qlnorm(0.9, meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
                               DEC == 10 ~ 1000000),
           idx = case_when(Upper > q_value & Lower <= q_value ~ 1,
                           !(Upper > q_value & Lower <= q_value) ~ 0))
  
  # filtering and splitting the segments where the decile thresholds cut
  data1_tmp1 <- data0 %>%
    mutate(floor1 = ifelse(idx == 1, floor((q_value - Lower)/StepLength),floor((Upper - Lower)/StepLength)),
           floor2 = ifelse(idx == 1, floor((Upper - q_value)/StepLength), 0),
           ceiling1 = ifelse(idx == 1, ceiling((q_value - Lower)/StepLength), ceiling((Upper - Lower)/StepLength)),
           ceiling2 = ifelse(idx == 1, ceiling((Upper - q_value)/StepLength), 0)) %>%
    mutate(ExpShare = value/Upper) %>%
    transform(DEC = as.numeric(DEC))
  
  data1_ExpShare <- data1_tmp1 %>%
    dplyr::select(c("scenario", "R", "Y", "Seg", "I", "ExpShare")) %>% 
    # mutate(I = paste0(I, "_ExpShare")) %>% 
    pivot_wider(names_from = I, values_from = "ExpShare")
  
  if(split_all == TRUE){
    data1_tmp2 <- data1_tmp1 %>%
      # filter(Upper-Lower >= LengthThreshold) %>%
      dplyr::select(-"ExpShare") %>%
      spread(I, value) %>%
      dplyr::select(-all_of(lis_I_function))  
  }else{
    data1_tmp2 <- data1_tmp1 %>%
      filter(DEC != "10",idx == 1) %>% 
      filter(Upper-Lower >= LengthThreshold) %>%
      dplyr::select(-"ExpShare") %>%
      spread(I, value) %>%
      dplyr::select(-all_of(lis_I_function))
  }
  
  data1_tmp3 <- data1_tmp2 %>%
    dplyr::group_by(Y,scenario,R,DEC,Seg) %>%
    na.omit() %>%
    slice(rep(1:n(), first(ceiling1+ceiling2)))%>%
    # group_by(Y,scenario,DEC,Seg1,Seg2) %>%
    dplyr::mutate(idx1 = 1,
                  idxnstep = cumsum(idx1),
                  part = ifelse(idxnstep <= ceiling1,1,2),
                  nstep = ifelse(part == 1,idxnstep,idxnstep-ceiling1),
                  n = ifelse(part == 1, ceiling1, ceiling2)) %>%
    dplyr::select(-c(ceiling1,ceiling2,floor1,floor2)) 
  
  dat2 <- data1_tmp3 %>%
    mutate(UpperLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*nstep,
                                               ifelse(idx == 1, q_value,Upper)), 
                             ifelse(nstep < n, q_value+StepLength*nstep, Upper)), 
           LowerLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*(nstep-1),  Lower+StepLength*(n-1)),
                             ifelse(nstep < n, q_value+StepLength*(nstep-1), q_value+StepLength*(n-1))))
  
  data1 <- dat2 %>%
    ungroup() %>%
    mutate(DEC_new = DEC+part-1) %>%
    dplyr::select(-c("Upper", "Lower", "DEC")) %>%
    dplyr::rename(Upper = "UpperLim", Lower = "LowerLim", DEC = "DEC_new") %>%
    # colnames(data1)
    
    left_join(data1_ExpShare,relationship = "many-to-many") %>%
    pivot_longer(cols = all_of(lis_I_function), names_to = "I", values_to = "ExpShare") %>% 
    # filter(is.na(I))
    mutate(value_new = Upper*ExpShare, 
           density = dlnorm((Upper+Lower),meanlog = para_Mu_exp, sdlog = para_Sigma_exp), 
           pop_new = (Upper-Lower)*density) %>%
    dplyr::select(-c("pop","density","idx1","idxnstep","part","nstep","n")) %>%
    dplyr::rename(value = "value_new", pop = "pop_new")
  # view(data1) 
  
  if(split_all == TRUE){
    data2 <- data1 %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = para_Mu_exp, sdlog = para_Sigma_exp),
             pop = (Upper-Lower)*dlnorm(Upper,meanlog = para_Mu_exp, sdlog = para_Sigma_exp)) %>%
      dplyr::select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_function)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,10,1)))) %>%
      group_by(scenario,R,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new), 
                       .groups="drop")
  }else{
    data2 <- data1_tmp4 %>%
      filter(!(idx == 1 & DEC != "10")) %>% 
      dplyr::select(colnames(data1)) %>%
      rbind(data1) %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = para_Mu_exp, sdlog = para_Sigma_exp)) %>%
      dplyr::select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_function)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,10,1)))) %>%
      group_by(scenario,R,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new), 
                       .groups="drop")
  }
  
  return(data2)
}


# Mutate for scenario name and redistribution -----------------------------
# F_reve_mutate <- function(x){
#   y <- x %>% 
#     mutate(revenue = str_split_i(scenario, "_", 2), 
#          target = str_split_i(scenario, "_", 1)) %>% 
#     mutate(revenue = case_when(is.na(revenue) ~ "Neutral",
#                                !is.na(revenue) ~ "EPC")) 
#   return(y)
# }
# 
# with Gini variations
F_reve_mutate <- function(x, all = F){
  y <- x %>% 
    mutate(revenue = case_when(grepl("redist", Ref) ~ "EPC", !grepl("redist", Ref) ~ "Neutral"),
           target = case_when(grepl("Baseline", Ref) ~ "No-Miti", grepl("2deg", Ref) ~ "2C", grepl("1p5deg", Ref) ~ "1.5C", (!grepl("No-Miti", Ref) & !grepl("2deg", Ref) & !grepl("1p5deg", Ref)) ~ str_split_i(Ref, "_", 2)),
           gini = case_when(grepl("Gini1", Ref) ~ "SSP1", grepl("Gini3", Ref) ~ "SSP3", grepl("Gini4", Ref) ~ "SSP4", grepl("Gini5", Ref) ~ "SSP5", !grepl("Gini", Ref) ~ "consistent")
           ) %>% filter(!is.na(target)) %>% mutate(revenue = factor(revenue, levels = c("Neutral", "EPC")))
  if(all == F){
    y <- y %>% filter(target %in% c("1.5C", "2C", "No-Miti"))
  }
  
  return(y)
}


# selecting main scenarios
F_sce_main <- function(x, EPC = T){
  y <- x %>% filter(gini == "consistent", target %in% c("No-Miti", "2C", "1.5C"))
  
  if(EPC == T){
   return(y)
  }else{
    y <- y %>% filter(revenue != "EPC")
    return(y)
  }
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
    # dplyr::select(-c("Seg0","Seg1","Seg2"))
  return(tmp3)
}

# Plot output for grid ----
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}


# EPS output ----
kmg2.ggsave <- function (filename = default_name(plot), plot = last_plot(),
                         device = default_device(filename), path = NULL, scale = 1,
                         width = par("din")[1], height = par("din")[2], dpi = 300,
                         keep = plot$options$keep, drop = plot$options$drop, ...)
{
  # original
  # if (!inherits(plot, "ggplot"))
  #     stop("plot should be a ggplot2 plot")
  if (!inherits(plot, "recordedplot"))
    stop("plot should be a recordedplot")
  eps <- ps <- function(..., width, height) grDevices::postscript(...,
                                                                  width = width, height = height, onefile = FALSE, horizontal = FALSE,
                                                                  paper = "special")
  tex <- function(..., width, height) grDevices::pictex(...,
                                                        width = width, height = height)
  pdf <- function(..., version = "1.4") grDevices::pdf(...,
                                                       version = version)
  svg <- function(...) grDevices::svg(...)
  wmf <- function(..., width, height) grDevices::win.metafile(...,
                                                              width = width, height = height)
  png <- function(..., width, height) grDevices::png(..., width = width,
                                                     height = height, res = dpi, units = "in")
  jpg <- jpeg <- function(..., width, height) grDevices::jpeg(...,
                                                              width = width, height = height, res = dpi, units = "in")
  bmp <- function(..., width, height) grDevices::bmp(..., width = width,
                                                     height = height, res = dpi, units = "in")
  tiff <- function(..., width, height) grDevices::tiff(...,
                                                       width = width, height = height, res = dpi, units = "in")
  default_name <- function(plot) {
    paste(digest.ggplot(plot), ".pdf", sep = "")
  }
  default_device <- function(filename) {
    pieces <- strsplit(filename, "\\.")[[1]]
    ext <- tolower(pieces[length(pieces)])
    match.fun(ext)
  }
  if (missing(width) || missing(height)) {
    message("Saving ", prettyNum(width * scale, digits = 3),
            "\" x ", prettyNum(height * scale, digits = 3), "\" image")
  }
  width <- width * scale
  height <- height * scale
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  device(file = filename, width = width, height = height, ...)
  on.exit(capture.output(dev.off()))
  print(plot, keep = keep, drop = drop)
  invisible()
}

# Q_lognormal <- function(df_q,mu,sigma){
#   q1 <- qlnorm(df_q,meanlog = mu, sdlog = sigma)
#   return(q1)
# }


# Energy poverty ----
## This function could encounter conflicts due to loaded packages
## unload the "plyr" and "Rmisc" 
## by running : detach('package:plyr', unload = TRUE)
## and : detach('package:Rmisc', unload = TRUE)
## before using this function
# F_EnePov <- function(Demand_data, PVL = 2.15,lis_ref = c("SSP2_Baseline"),lis_y = c(2020, 2030, 2050, 2070),lis_Iene = c("Energy"), median = T){
F_EnePov <- function(Seg_pop, ExpNational_tmp, PVL = 2.15,lis_ref = c("SSP2_Baseline"),lis_y = c(2020, 2030, 2050, 2070),lis_Iene = c("Energy"), median = T){
  # lis_y <- c("2030", "2050")
  # Demand_data <- demandcge
  # lis_Iene <- "Energy"
  # lis_ref <- c("SSP2_Baseline", "SSP2_1p5deg", "SSP2_2deg")
  # PVL <- 2.15 # poverty line used for LIHC
  
  # Seg_pop <- rgdx.param(Demand_data, "FreqSegall") %>% 
  #   rename.vars(from = colnames(.), to = c("R", 'Ref','Y','Seg','pop')) %>%
  #   filter(Y %in% lis_y,Ref %in% lis_ref) %>%
  #   group_by(R,Ref,Y) %>%
  #   dplyr::reframe(pop = pop, Seg = Seg) %>% #popcum = cumsum(pop), 
  #   SegNum() 
  # 
  # ExpNational_tmp <- rgdx.param(Demand_data, "ExpNational") %>%
  #   rename.vars(from = colnames(.), to = c("R", 'Ref','Seg','Y',"I",'ExpNational')) %>%
  #   filter( Ref %in% lis_ref,
  #           Y %in% lis_y,
  #           # I %in% lis_Iene,
  #           # VC == "share"
  #   ) %>%
  #   dplyr::group_by(R, Ref, Seg, Y) %>% 
  #   dplyr::mutate(share = ExpNational/sum(ExpNational)) %>% 
  #   ungroup() %>% 
  #   filter(I %in% lis_Iene)
  # view(ExpNational_tmp)
  
  Ene_exp <- ExpNational_tmp %>%
    group_by(R, Ref,Seg,Y) %>%
    dplyr::summarise(ENEshare = sum(share), .groups = "drop")  %>%
    left_join(Seg_pop) %>%
    mutate(ENEabs = ENEshare*Upper) %>%
    dplyr::select(-c( "Seg0","Seg1","Seg2"))
  # the total share of disposable income spent on residential energy use
  # view(Ene_exp)
  
  
  Ene_share_med_tmp1 <- Ene_exp %>%
    group_by(R, Ref,Y) %>%
    dplyr::arrange(ENEshare, by_group = T) %>%
    dplyr::reframe(ENEshare = ENEshare, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5))# %>%
  # colnames(Ene_share_med)
  Ene_share_med <-  Ene_share_med_tmp1 %>%
    group_by(R, Ref,Y) %>%
    dplyr::reframe(idx = min(idx)) %>% # the one closest to 0.5 is deemed as segment with the median value
    left_join(Ene_share_med_tmp1) %>%# here we picked out the income segments whose share of energy expenditure equals the median
    SegNum() %>% 
    dplyr::select("R", "Ref","Y","ENEshare", "Upper") %>%
    group_by(R, Ref,Y) %>%
    dplyr::reframe(Upper= min(Upper)) %>% 
    left_join(Ene_share_med_tmp1 %>% SegNum() %>% dplyr::select(-Seg0,-Seg1, -Seg2, -Lower)) %>% 
    dplyr::select("R", "Ref","Y","ENEshare") %>%
    dplyr::rename(MedianShare = "ENEshare") 
  # view(Ene_share_med)
  
  
  Ene_abs_med_tmp1 <- Ene_exp %>%
    mutate(ENEabs = ENEshare*Upper) %>%
    group_by(R, Ref,Y) %>%
    dplyr::arrange(ENEabs, by_group = T) %>%
    dplyr::reframe(ENEabs = ENEabs, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5))# %>%
  # view(Ene_abs_med_tmp1)
  Ene_abs_med <- Ene_abs_med_tmp1 %>%
    group_by(R, Ref,Y) %>%
    dplyr::summarise(idx = min(idx), .groups = "drop") %>% # the one closest to 0.5 is deemed as segment with the median value
    left_join(Ene_abs_med_tmp1) %>%# here we picked out the income segments whose share of energy expenditure equals the median
    dplyr::select("R", "Ref","Y","ENEabs") %>%
    dplyr::rename(MedianAbs = "ENEabs") 
  # view(Ene_abs_med)
  
  EneMedian <- Ene_abs_med %>%
    full_join(Ene_share_med) %>%
    ungroup()

  
    # view(EnePov)
  if(median){
    # print("T")

    
    return(EneMedian)
  }else{
    # print("F")
    # colnames(Ene_abs_med)
    EnePov_2M <- Ene_exp %>%
      dplyr::select("R", "Ref","Seg","Y","ENEshare","pop") %>%
      left_join(Ene_share_med, by = c("R", "Ref", "Y")) %>% 
      mutate(Ind = ENEshare-2*MedianShare) %>%
      filter(Ind >= 0) %>%
      group_by(R, Ref,Y) %>%
      dplyr::summarize(TwoM = sum(pop), .groups = "drop")
    # view(EnePov_2M)
    
    EnePov_HEP <- Ene_exp %>%
      dplyr::select("R", "Ref","Seg","Y","ENEabs","pop") %>%
      left_join(Ene_abs_med) %>%
      mutate(Ind = ENEabs-0.5*MedianAbs) %>%
      filter(Ind <= 0) %>%
      group_by(R, Ref,Y) %>%
      dplyr::summarize(HEP = sum(pop), .groups = "drop")
    # view(EnePov_HEP)
    
    EnePov_LIHC <- Ene_exp %>%
      # dplyr::select("Ref","Seg","Y","ENEabs","pop") %>%
      left_join(Ene_share_med) %>%
      mutate(Exp_nonEne = Upper*(1-ENEshare),
             PovTH = PVL*365,
             Ind1 = ENEshare - MedianShare,
             Ind2 = Exp_nonEne - PovTH) %>%
      filter(Ind1 > 0, Ind2 <= 0) %>%
      group_by(R, Ref,Y) %>%
      dplyr::summarize(LIHC = sum(pop), .groups = "drop")
    
    EnePov <- EnePov_2M %>%
      full_join(EnePov_HEP) %>%
      full_join(EnePov_LIHC) %>%
      ungroup() %>% left_join(EneMedian)
    return(EnePov)
  }
  
}

# with regional/global aggregation
F_EnePov_global <- function(Demand_data,population,map_r_PHI2CGE,lis_R,lis_ref,lis_y,lis_Iene, median){
  # lis_y <- c("2020","2030", "2040", "2050")
  # Demand_data <- ConsumptionResults
  # lis_Iene <- lis_I_ene_full
  # lis_R <-c("CHN","IND","USA")
  # median <-  T
  lis_R <-  F
  if(lis_R == F){
    ExpNational_tmp <- Demand_data %>%
      rgdx.param("ExpNational") %>%
      rename.vars(from = colnames(.), to = c("R","Ref","VC","Seg","Y","I","ExpNational")) %>%
      filter( Ref %in% lis_ref,
              Y %in% lis_y,
              I %in% lis_Iene)%>%
      transform(Y = factor(Y, levels = lis_y))
  }else{
    ExpNational_tmp <- Demand_data %>%
      rgdx.param("ExpNational") %>%
      rename.vars(from = colnames(.), to = c("R","Ref","VC","Seg","Y","I","ExpNational")) %>%
      filter( Ref %in% lis_ref,
              Y %in% lis_y,
              I %in% lis_Iene,
              R %in% lis_R)  %>%
      transform(Y = factor(Y, levels = lis_y))
  }
  
  
  Seg_pop <- rgdx.param(Demand_data, "FreqSegall") %>%
    rename.vars(from = colnames(.), to = c("R",'Ref','Y','Seg','pop')) %>%
    filter(Y %in% lis_y,
           Ref %in% lis_ref) %>%
    SegNum() 
  # view(Seg_pop)
  
  
  
  Ene_exp_tmp <- ExpNational_tmp %>%
    filter( VC == "abs") %>%
    dplyr::select(-"VC") %>%
    group_by(R,Ref,Seg,Y) %>%
    dplyr::summarise(ENEabs = sum(ExpNational),
                     .groups = "drop")  %>%
    left_join(Seg_pop) %>%
    left_join(population) %>%
    mutate(population_seg = Population * pop,
           ENEabs_seg = ENEabs * population_seg) %>%
    left_join(map_r_PHI2CGE) %>%
    group_by(R_CGE, Ref, Seg, Y) %>%
    dplyr::summarise(population_seg_tot = sum(population_seg),
                     ENEabs_seg_avg = sum(ENEabs_seg)/sum(population_seg),
                     .groups = "drop") %>%
    dplyr::select(R_CGE,Ref,Seg,Y,population_seg_tot,ENEabs_seg_avg)
  # the total share of disposable income spent on residential energy use
  # view(Ene_exp_tmp)  
  
  population_PHI_17 <- Ene_exp_tmp %>%
    group_by(R_CGE, Ref, Y) %>%
    dplyr::summarise(population_tot = sum(population_seg_tot),
                     .groups = "drop")
  
  
  Ene_exp <- Ene_exp_tmp %>%
    SegNum() %>%
    left_join(population_PHI_17) %>%
    gdata::rename.vars(from = c("R_CGE", "ENEabs_seg_avg"),
                       to = c("R","ENEabs"))%>%
    mutate(pop = population_seg_tot/population_tot,
           ENEshare = ENEabs/Upper) %>%
    dplyr::select(R,Ref,Seg,Y,pop,ENEabs,ENEshare,Upper) 
 
  Ene_share_med_tmp1 <- Ene_exp %>%
    group_by(R,Ref,Y) %>%
    dplyr::arrange(ENEshare, by_group = T) %>%
    dplyr::summarise(ENEshare = ENEshare, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5))# %>%
  # # view(Ene_share_med_tmp1)
  
  Ene_share_med <-  Ene_exp %>%
    group_by(R,Ref,Y) %>%
    dplyr::arrange(ENEshare, by_group = T) %>%
    dplyr::summarise(ENEshare = ENEshare, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5)) %>%
    filter(idx == min(idx)) %>%
    dplyr::select("R","Ref","Y","ENEshare") %>%
    dplyr::rename(MedianShare = "ENEshare") 
  # view(Ene_share_med)
  
  
  Ene_abs_med <- Ene_exp %>%
    mutate(ENEabs = ENEshare*Upper) %>%
    group_by(R,Ref,Y) %>%
    dplyr::arrange(ENEabs, by_group = T) %>%
    dplyr::summarise(ENEabs = ENEabs, Seg = Seg, popcum = cumsum(pop), idx = abs(popcum-0.5)) %>%
    filter(idx == min(idx)) %>%
    dplyr::select("R","Ref","Y","ENEabs") %>%
    dplyr::rename(MedianAbs = "ENEabs") 
  # view(Ene_abs_med)
  
  
  
  EnePov_2M <- Ene_exp %>%
    dplyr::select("R","Ref","Seg","Y","ENEshare","pop") %>%
    left_join(Ene_share_med) %>%
    mutate(Ind = ENEshare-2*MedianShare) %>%
    filter(Ind >= 0) %>%
    group_by(R,Ref,Y) %>%
    dplyr::summarize(TwoM = sum(pop))
  # view(EnePov_2M)
  
  EnePov_HEP <- Ene_exp %>%
    dplyr::select("R","Ref","Seg","Y","ENEabs","pop") %>%
    left_join(Ene_abs_med) %>%
    mutate(Ind = ENEabs-0.5*MedianAbs) %>%
    filter(Ind <= 0) %>%
    group_by(R,Ref,Y) %>%
    dplyr::summarize(HEP = sum(pop))
  # view(EnePov_HEP)
  
  EnePov_LIHC <- Ene_exp %>%
    # dplyr::select("Ref","Seg","Y","ENEabs","pop") %>%
    left_join(Ene_share_med) %>%
    mutate(Exp_nonEne = Upper*(1-ENEshare),
           PovTH = 3.2*365,
           Ind1 = ENEshare - MedianShare,
           Ind2 = Exp_nonEne - PovTH) %>%
    filter(Ind1 > 0, Ind2 <= 0) %>%
    group_by(R, Ref,Y) %>%
    dplyr::summarize(LIHC = sum(pop))
  
  EneMedian <- Ene_abs_med %>%
    full_join(Ene_share_med) %>%
    ungroup()
  
  EnePov <- EnePov_2M %>%
    full_join(EnePov_HEP) %>%
    full_join(EnePov_LIHC) %>%
    ungroup()
  # view(EnePov)
  if(median == T){
    return(EneMedian)
  }else{
    return(EnePov)    
  }
  
}



# Food poverty ----
# dir_input <- paste0("../output/gdx/ConsumptionResults/ConsumptionResults_AIMHub.gdx")
# rm(a,aa,df_FoodPov_p, df_FoodPov, df_FPL, df_FPL_scenario, df_FPLsc, df1, df2, df3, df4, df_Exp_fd_in)
F_FoodPov <- function(df_FPL, df_FPL_wd_in = data.frame(Indicator = c("CoCA", "CoNA", "CoHD"), adjust = c(.63,.63,.63)), output_FPL=F, test = T, simplified_income = F, lis_ind = c("CoCA", "CoNA", "CoHD"), df_Exp_fd_in,  df_PQ_in,  df_Mu_exp_in,  df_Sigma_exp_in){

  
  df_PQ_2010 <- df_PQ_in %>% filter(Y == 2010) %>% dplyr::select(-Y, -I) %>% dplyr::rename("PQ_2010" = "PQ")
  
  # Future FPL
  df_FPL_scenario0 <- df_PQ_in %>% left_join(df_PQ_2010) %>% dplyr::select(-R3) %>% 
    full_join(df_FPL %>% dplyr::select(Indicator, Unit) %>% distinct() %>% mutate(I = "Food and nonalcoholic beverages")) %>% 
    left_join(df_FPL %>% dplyr::select(-Y) %>% filter(!is.na(Indicator))) %>% 
    mutate(value_indicator = case_when(Indicator %in% lis_ind ~ value_ind_2010 * PQ/PQ_2010,
                                       !(Indicator %in% lis_ind) ~ value_ind_2010), # do not scale the poverty line derived food poverty line by price. 
           idx = case_when(Indicator %in% lis_ind ~ 1, !(Indicator %in% lis_ind) ~ 0)) %>% 
    dplyr::select("R", "Ref", "Y", "I", "Indicator", "Unit", "value_indicator", "idx") %>% left_join(Map_r_PHI2Hub) 
  
  # imputing for countries where the three cost indicators are absent
  df_FPL_scenario <- df_FPL_scenario0 %>% filter(!is.na(value_indicator), !is.na(Indicator))  %>% dplyr::group_by(R_CGE, Ref, Y, I, Indicator, Unit, idx) %>% reframe(median = median(value_indicator)) %>% 
    right_join(df_FPL_scenario0) %>% mutate(value_indicator = case_when(is.na(value_indicator) ~ median, !is.na(value_indicator) ~ value_indicator)) %>% 
    dplyr::select(-median, -R_CGE, -idx)

    print(head(df_FPL_wd_in))


  if(output_FPL==F){
    df_FPL_inc <- df_FPL_scenario %>% left_join(df_FPL_wd_in) %>% mutate(value_indicator_adj_inc = value_indicator*365/adjust) %>% filter(Indicator %in% lis_ind) %>% 
      dplyr::select(R, Y, Ref, Indicator, I, value_indicator_adj_inc) %>% distinct()

    if(simplified_income == T){
      df_Exp_fd_FPL <- df_FPL_inc %>% mutate(Income = value_indicator_adj_inc) %>% filter(!is.na(Income)) %>%
        left_join(df_Mu_exp_in) %>% left_join(df_Sigma_exp_in) %>%
        filter(!is.na(para_Mu_exp)) %>%
        mutate(value = plnorm(Income, meanlog = para_Mu_exp, sdlog = para_Sigma_exp)) %>%
        left_join(df_FPL_scenario) %>% dplyr::select(R, Ref, Y, Indicator, Income, value, value_indicator)
      
      #df_Exp_fd_in %>% #SegNum() %>% #left_join(df_FPL_scenario)  %>% 
        # mutate(FP = case_when(value <= value_indicator ~ 1, value > value_indicator ~ 0)) %>%
        # filter(value <= value_indicator) %>% 
        # filter(Indicator %in% lis_ind) %>%
        # dplyr::group_by(R, Ref, Y, I, Indicator) %>% dplyr::reframe(Income = max(Upper)) %>%

      
    }else{
      df_Exp_fd_FPL <- df_Exp_fd_in %>% left_join(df_FPL_scenario)  %>% SegNum() %>%
          mutate(FP = case_when(value <= value_indicator ~ 1, value > value_indicator ~ 0)) %>%
          filter(value <= value_indicator) %>% filter(Indicator %in% lis_ind) %>%
          dplyr::group_by(R, Ref, Y, I, Indicator) %>% dplyr::reframe(Income = max(Upper)) %>%
          right_join(df_FPL_inc) %>% mutate(Income = case_when(is.na(Income) ~ value_indicator_adj_inc, 
                                                               !is.na(Income) ~ Income,
                                                               Income/value_indicator_adj_inc > 2 ~ value_indicator_adj_inc)) %>% filter(!is.na(Income)) %>%
          left_join(df_Mu_exp_in) %>% left_join(df_Sigma_exp_in) %>%
          filter(!is.na(para_Mu_exp)) %>%
          mutate(value = plnorm(Income, meanlog = para_Mu_exp, sdlog = para_Sigma_exp)) %>%
          left_join(df_FPL_scenario) %>% dplyr::select(R, Ref, Y, Indicator, Income, value, value_indicator)
    }
      
      return(df_Exp_fd_FPL)
  }else{
    # return(df_FPL_inc %>% mutate(value_indicator = value_indicator_adj_inc/365) %>% dplyr::select(-I, -value_indicator_adj_inc))
    return(df_FPL_scenario)
  }
}

# A general function for decile assignment ----

# F_DEC_tmp <- function(Demand_data,OD,lis_y, split_all){
#   StepLength <-  50
#   LengthThreshold <- 100
#   # OD <- PoutputNational
#   # Demand_data <- "../../../output/gdx/demand/country/CHN.gdx"
#   # split_all <- TRUE
#   # PoutputNational <- rgdx.param(CHN_demand, "PoutputNational") %>%
#   #   filter( Ref %in% lis_ref,
#   #           Y %in% lis_y) %>%
#   #   dplyr::rename(value = "PoutputNational") %>%
#   #   left_join(MapI) %>%
#   #   dplyr::select(-"I")
#   
#   lis_ref <- as.character(unique(OD$Ref))
#   lis_I_abb <- as.character(unique(OD$I_abb))
#   
#   
#   Seg_pop <- rgdx.param(Demand_data, "FreqSegall") %>%
#     rename.vars(colnames(.), c("R",'Ref','Y','Seg','pop')) %>%
#     SegNum()
#   
#   
#   Mu_exp <- rgdx.param(Demand_data, "Mu_exp") %>%
#     rename.vars(colnames(.), c("R",'Ref','Y','Mu_exp')) %>%
#     filter(Ref %in% lis_ref)
#   
#   
#   Sigma_exp <- rgdx.param(Demand_data, "Sigma_exp") %>%
#     rename.vars(colnames(.), c("R",'Ref','Y','Sigma_exp')) %>%
#     filter(Ref %in% lis_ref)
#   
#   
#   df_q <- seq(from = 0.1, to = 1, by = 0.1)
#   
#   # the q_value for each scenario and each year
#   data_q_value <- Sigma_exp %>%
#     left_join(Mu_exp) %>%
#     group_by(Ref,Y,R) %>%
#     dplyr::summarise(Sigma_exp = Sigma_exp, Mu_exp = Mu_exp, 
#                      q_value = c(qlnorm(df_q,Mu_exp,Sigma_exp)[-10],1000000),
#                      .groups = "drop") %>%
#     mutate(DEC = as.character(c(t(rep(df_q*10,nrow(.)/length(df_q)))))) 
#   
#   
#   # identifying the decile where each income segment belongs
#   data_tmp1 <- data_q_value  %>%
#     spread(DEC,q_value) %>%
#     left_join(Seg_pop)
#   data_tmp2 <- ifelse((data_tmp1[,which(colnames(data_tmp1) %in% as.character(df_q*10))]-data_tmp1$Lower) <0, 1,0) %>%
#     apply(.,1,sum) %>%
#     as.data.frame() %>%
#     dplyr::rename(DEC_tmp = colnames(.)) %>%
#     mutate(DEC = as.character(DEC_tmp +1))
#   data1_tmp3 <- data_tmp1%>%
#     cbind(data_tmp2) %>%
#     dplyr::select(-c(as.character(df_q*10),"DEC_tmp")) %>%
#     left_join(data_q_value) %>%
#     mutate(idx = ifelse((Lower <= q_value & q_value <= Upper),1,0)) %>% # idx == 1: the segment is cut by q_value
#     right_join(OD) %>%
#     spread(I_abb, value)
#   
#   # filtering and splitting the segments where the decile thresholds cut
#   data1_tmp4 <- data1_tmp3 %>%
#     mutate(floor1 = ifelse(idx == 1, floor((q_value - Lower)/StepLength),floor((Upper - Lower)/StepLength)),
#            floor2 = ifelse(idx == 1, floor((Upper - q_value)/StepLength), 0),
#            ceiling1 = ifelse(idx == 1, ceiling((q_value - Lower)/StepLength), ceiling((Upper - Lower)/StepLength)),
#            ceiling2 = ifelse(idx == 1, ceiling((Upper - q_value)/StepLength), 0)) %>%
#     gather(I, value, all_of(lis_I_abb))%>%
#     mutate(ExpShare = value/Upper) %>%
#     # dplyr::select(-"total") %>%
#     transform(DEC = as.numeric(DEC))
#   colnames(data1_tmp4)
#   
#   data1_ExpShare <- data1_tmp4 %>%
#     dplyr::select(c("Ref", "R", "Y", "Sigma_exp", "Mu_exp", "Seg", "I", "ExpShare"))
#   if(split_all == TRUE){
#     data1_tmp6 <- data1_tmp4 %>%
#       # filter(Upper-Lower >= LengthThreshold) %>%
#       dplyr::select(-"ExpShare") %>%
#       spread(I, value) %>%
#       dplyr::select(-all_of(lis_I_abb))  
#   }else{
#     data1_tmp6 <- data1_tmp4 %>%
#       filter(DEC != "10",idx == 1) %>% 
#       filter(Upper-Lower >= LengthThreshold) %>%
#       dplyr::select(-"ExpShare") %>%
#       spread(I, value) %>%
#       dplyr::select(-all_of(lis_I_abb))
#   }
#   
#   
#   data1_tmp7 <- data1_tmp6 %>%
#     dplyr::group_by(Y,Ref,R,DEC,Seg1,Seg2) %>%
#     slice(rep(1:n(), first(ceiling1+ceiling2)))%>%
#     # group_by(Y,Ref,DEC,Seg1,Seg2) %>%
#     dplyr::mutate(idx1 = 1,
#                   idxnstep = cumsum(idx1),
#                   part = ifelse(idxnstep <= ceiling1,1,2),
#                   nstep = ifelse(part == 1,idxnstep,idxnstep-ceiling1),
#                   n = ifelse(part == 1, ceiling1, ceiling2)) %>%
#     dplyr::select(-c(ceiling1,ceiling2,floor1,floor2)) 
#   
#   dat2 <- data1_tmp7 %>%
#     mutate(UpperLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*nstep,
#                                                ifelse(idx == 1, q_value,Upper)), # min(q_value,Upper)
#                              ifelse(nstep < n, q_value+StepLength*nstep, Upper)), 
#            LowerLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*(nstep-1),  Lower+StepLength*(n-1)),
#                              ifelse(nstep < n, q_value+StepLength*(nstep-1), q_value+StepLength*(n-1))))
#   
#   data1 <- dat2 %>%
#     ungroup() %>%
#     mutate(DEC_new = DEC+part-1) %>%
#     dplyr::select(-c("Upper", "Lower", "DEC")) %>%
#     dplyr::rename(Upper = "UpperLim", Lower = "LowerLim", DEC = "DEC_new") %>%
#     left_join(data1_ExpShare) %>%
#     mutate(value_new = Upper*ExpShare, 
#            density = dlnorm((Upper+Lower),meanlog = Mu_exp, sdlog = Sigma_exp), 
#            pop_new = (Upper-Lower)*density) %>%
#     dplyr::select(-c("pop","density","idx1","idxnstep","part","nstep","n")) %>%
#     dplyr::rename(value = "value_new", pop = "pop_new")
#   # view(data1) 
#   
#   if(split_all == TRUE){
#     data2 <- data1 %>%
#       mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = Mu_exp, sdlog = Sigma_exp),
#              pop = (Upper-Lower)*dlnorm(Upper,meanlog = Mu_exp, sdlog = Sigma_exp)) %>%
#       dplyr::select(-"idx") %>%
#       transform(I = factor(I, levels = lis_I_abb)) %>%
#       transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,10,1)))) %>%
#       # gather(I, value, all_of(lis_I_abb)) %>%
#       group_by(Ref,R,Y,DEC,I) %>%
#       dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new),#pop_old = sum(pop),
#                        .groups="drop")
#   }else{
#     data2 <- data1_tmp4 %>%
#       filter(!(idx == 1 & DEC != "10")) %>% 
#       dplyr::select(colnames(data1)) %>%
#       rbind(data1) %>%
#       mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = Mu_exp, sdlog = Sigma_exp)) %>%
#       dplyr::select(-"idx") %>%
#       transform(I = factor(I, levels = lis_I_abb)) %>%
#       transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,10,1)))) %>%
#       # gather(I, value, all_of(lis_I_abb)) %>%
#       group_by(Ref,R,Y,DEC,I) %>%
#       dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new),#pop_old = sum(pop),
#                        .groups="drop")
#   }
#   return(data2)
# }





F_DEC_tmp <- function(Demand_data,OD,lis_y,quantile_input = seq(from = 0.1, to = 1, by = 0.1), split_all){
  StepLength <-  50
  LengthThreshold <- 100
  
  # OD <- PoutputNational
  # Demand_data <- "../../../output/gdx/demand/country/CHN.gdx"
  split_all <- TRUE
  # PoutputNational <- rgdx.param(CHN_demand, "PoutputNational") %>%
  #   filter( Ref %in% lis_ref,
  #           Y %in% lis_y) %>%
  #   dplyr::rename(value = "PoutputNational") %>%
  #   left_join(MapI) %>%
  #   dplyr::select(-"I")
  
  # OD <- PoutputNational_cge
  # quantile_input <-  seq(from = 0.1, to = 1, by = 0.1)
  # quantile_input <- seq(from = 0.2, to = 1, by = 0.2)
  
  lis_ref <- as.character(unique(OD$Ref))
  lis_I_abb <- as.character(unique(OD$I_abb))
  
  
  Seg_pop <- rgdx.param(Demand_data, "FreqSegall") %>%
    rename.vars(colnames(.), c("R",'Ref','Y','Seg','pop')) %>%
    SegNum()
  
  
  df_Mu_exp <- rgdx.param(Demand_data, "Mu_exp") %>%
    rename.vars(colnames(.), c("R",'Ref','Y','Mu_exp')) %>%
    filter(Ref %in% lis_ref)
  
  
  df_Sigma_exp <- rgdx.param(Demand_data, "Sigma_exp") %>%
    rename.vars(colnames(.), c("R",'Ref','Y','Sigma_exp')) %>%
    filter(Ref %in% lis_ref)
  
  
  df_q <- c(quantile_input)
  
  # the q_value for each scenario and each year
  data_q_value <- df_Sigma_exp %>%
    left_join(df_Mu_exp) %>%
    filter( Y %in% lis_y) %>% 
    group_by(Ref,Y,R) %>%
    dplyr::reframe(Sigma_exp = Sigma_exp, Mu_exp = Mu_exp, 
                   q_value = c(qlnorm(df_q,Mu_exp,Sigma_exp)[-length(df_q)],1000000)) %>%
    mutate(DEC = as.character(c(t(rep(df_q*length(df_q),nrow(.)/length(df_q)))))) 
  
  
  # identifying the decile where each income segment belongs
  data_tmp1 <- data_q_value  %>%
    spread(DEC,q_value) %>%
    left_join(Seg_pop)
  data_tmp2 <- ifelse((data_tmp1[,which(colnames(data_tmp1) %in% as.character(df_q*length(df_q)))]-data_tmp1$Lower) <0, 1,0) %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    dplyr::rename(DEC_tmp = colnames(.)) %>%
    mutate(DEC = as.character(DEC_tmp +1))
  data1_tmp3 <- data_tmp1%>%
    cbind(data_tmp2) %>%
    dplyr::select(-c(as.character(df_q*length(df_q)),"DEC_tmp")) %>%
    left_join(data_q_value) %>%
    mutate(idx = ifelse((Lower <= q_value & q_value <= Upper),1,0)) %>% # idx == 1: the segment is cut by q_value
    right_join(OD) %>%
    filter(Y %in% lis_y) %>% 
    spread(I_abb, value)
  
  # filtering and splitting the segments where the decile thresholds cut
  data1_tmp4 <- data1_tmp3 %>%
    mutate(floor1 = ifelse(idx == 1, floor((q_value - Lower)/StepLength),floor((Upper - Lower)/StepLength)),
           floor2 = ifelse(idx == 1, floor((Upper - q_value)/StepLength), 0),
           ceiling1 = ifelse(idx == 1, ceiling((q_value - Lower)/StepLength), ceiling((Upper - Lower)/StepLength)),
           ceiling2 = ifelse(idx == 1, ceiling((Upper - q_value)/StepLength), 0)) %>%
    gather(I, value, all_of(lis_I_abb))%>%
    mutate(ExpShare = value/Upper) %>%
    # dplyr::select(-"total") %>%
    transform(DEC = as.numeric(DEC))
  # colnames(data1_tmp4)
  
  data1_ExpShare <- data1_tmp4 %>%
    dplyr::select(c("Ref", "R", "Y", "Sigma_exp", "Mu_exp", "Seg", "I", "ExpShare"))
  if(split_all == TRUE){
    data1_tmp6 <- data1_tmp4 %>%
      # filter(Upper-Lower >= LengthThreshold) %>%
      dplyr::select(-"ExpShare") %>%
      spread(I, value) %>%
      dplyr::select(-all_of(lis_I_abb))  
  }else{
    data1_tmp6 <- data1_tmp4 %>%
      filter(DEC != length(df_q),idx == 1) %>% 
      filter(Upper-Lower >= LengthThreshold) %>%
      dplyr::select(-"ExpShare") %>%
      spread(I, value) %>%
      dplyr::select(-all_of(lis_I_abb))
  }
  
  
  data1_tmp7 <- data1_tmp6 %>%
    dplyr::group_by(Y,Ref,R,DEC,Seg1,Seg2) %>% 
    slice(rep(1:n(), first(ceiling1+ceiling2))) %>% 
    # group_by(Y,Ref,DEC,Seg1,Seg2) %>%
    dplyr::mutate(idx1 = 1,
                  idxnstep = cumsum(idx1),
                  part = ifelse(idxnstep <= ceiling1,1,2),
                  nstep = ifelse(part == 1,idxnstep,idxnstep-ceiling1),
                  n = ifelse(part == 1, ceiling1, ceiling2)) %>%
    dplyr::select(-c(ceiling1,ceiling2,floor1,floor2)) 
  
  dat2 <- data1_tmp7 %>%
    mutate(UpperLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*nstep,
                                               ifelse(idx == 1, q_value,Upper)), # min(q_value,Upper)
                             ifelse(nstep < n, q_value+StepLength*nstep, Upper)), 
           LowerLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*(nstep-1),  Lower+StepLength*(n-1)),
                             ifelse(nstep < n, q_value+StepLength*(nstep-1), q_value+StepLength*(n-1))))
  
  data1 <- dat2 %>%
    ungroup() %>%
    mutate(DEC_new = DEC+part-1) %>%
    dplyr::select(-c("Upper", "Lower", "DEC")) %>%
    dplyr::rename(Upper = "UpperLim", Lower = "LowerLim", DEC = "DEC_new") %>%
    left_join(data1_ExpShare) %>%
    mutate(value_new = Upper*ExpShare, 
           density = dlnorm((Upper+Lower),meanlog = Mu_exp, sdlog = Sigma_exp), 
           pop_new = (Upper-Lower)*density) %>%
    dplyr::select(-c("pop","density","idx1","idxnstep","part","nstep","n")) %>%
    dplyr::rename(value = "value_new", pop = "pop_new")
  # view(data1) 
  
  if(split_all == TRUE){
    data2 <- data1 %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = Mu_exp, sdlog = Sigma_exp),
             pop = (Upper-Lower)*dlnorm(Upper,meanlog = Mu_exp, sdlog = Sigma_exp)) %>%
      dplyr::select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_abb)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,length(df_q),1)))) %>%
      # gather(I, value, all_of(lis_I_abb)) %>%
      group_by(Ref,R,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new),#pop_old = sum(pop),
                       .groups="drop")
  }else{
    data2 <- data1_tmp4 %>%
      filter(!(idx == 1 & DEC != "10")) %>% 
      dplyr::select(colnames(data1)) %>%
      rbind(data1) %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = Mu_exp, sdlog = Sigma_exp)) %>%
      dplyr::select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_abb)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,length(df_q),1)))) %>%
      # gather(I, value, all_of(lis_I_abb)) %>%
      group_by(Ref,R,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new),#pop_old = sum(pop),
                       .groups="drop")
  }
  return(data2)
}

# Nishiura-Matsui-Mori plots ----

F_p_area <- function(pdata,Value,ylab1,xlab1,palette_fill){
  plot <- ggplot() +
    geom_area(data=pdata,
              aes(x=as.numeric(as.character(Year)), 
                  y=Value, 
                  fill=Variable,group=Variable),
              stat="identity") + 
    ylab(ylab1) + xlab(xlab1) + 
    facet_wrap(~ SCENARIO,nrow=1) + 
    # MyThemeLine +
    theme(legend.position=c(0.5,1), text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12)) +
    guides(fill=guide_legend(ncol=5,title=NULL)) + 
    scale_fill_manual(values = palette_fill) +
    scale_x_continuous(breaks=seq(min(as.numeric(as.character(pdata$Year))),
                                  max(as.numeric(as.character(pdata$Year))),
                                  10))
  return(plot)
}


# plot <- fin.1+fin_ind.2+fin_bui.2+fin_tra.2+plot_layout(ncol=4,widths=c(4,1.1,1,1))


F_p_bar_1 <- function(pdata,Value,ylab1, xlab1, palette_fill){
  plot <- ggplot() + 
    geom_bar(data=pdata,
             aes(x=SCENARIO, y = Value, 
                 fill=Variable, group=Variable),
             stat="identity") + 
    ylab(ylab1) + 
    xlab(xlab1) + 
    guides(fill=guide_legend(title=NULL)) + 
    MyThemeLine +
    facet_wrap(~Year, ncol = length(unique(pdata$Year))) +
    theme(legend.position=, text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12)) + 
    scale_fill_manual(values=palette_fill,name="") +
    guides(fill=guide_legend(ncol=1,title=NULL))
  
  return(plot)
}


#-------------------------


plot.1 <- function(XX){
  plot <- ggplot() + 
    geom_area(data=XX,
              aes(x=Year, y=Value , fill=Variable, group=Variable), 
              stat="identity") + 
    guides(fill=guide_legend(title=NULL)) + 
    MyThemeLine +
    theme(legend.position=c(0.5,1), 
          text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12)) +
    guides(fill=guide_legend(ncol=5,title=NULL)) #+ 
    # scale_x_continuous(breaks=seq(miny,maxy,10))
  
  plot2 <- plot +
    facet_wrap(~ SCENARIO,nrow=1) + 
    # scale_fill_manual(values=colorpal) + 
    theme(legend.position='bottom')
  plot3 <- plot2
  return(plot3)
}

plot.2 <- function(XX){
  plot <- ggplot() + 
    geom_bar(data=XX,
             aes(x=SCENARIO, y = Value, fill=Variable, group=Variable), stat='identity') + 
    # ylab("") + 
    # xlab(xlab1) + 
    guides(fill=guide_legend(title=NULL)) + 
    MyThemeLine +
    theme(legend.position=, 
          text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12), 
          axis.text.y=element_blank(),
          axis.title.y = element_blank())+
    guides(fill=guide_legend(ncol=3,title=NULL))
  
  plot3 <- plot +
    facet_wrap(~ Year) #+ 
    # scale_fill_manual(values=colorpal)  

   return(plot3)
}

#linesplot function with dots
plot.3 <- function(XX){ 
  plot <- ggplot(XX,aes(x=Year,y=Value,colour=flag2,fill=flag)) +
    geom_line(size=0.5) +
    geom_point(aes(x=Year,y=Value,shape=flag),size=4,position=position_dodge(0)) +
    scale_shape_manual(values=c(19,4)) +
    ylab(ylab1) + xlab(xlab1) + guides(fill=guide_legend(title=NULL)) +
    MyThemeLine +
    theme(legend.position ="none", text=element_text(size=12),
          legend.title=element_blank(),
          axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=12)) +
    guides(fill=guide_legend(ncol=5,title=NULL)) + scale_x_continuous(breaks=seq(miny,maxy,10))
  plot2 <- plot + facet_wrap(~Variable, nrow=1) + #scale_fill_manual(values=colorpal) +
    theme(legend.position="bottom")
  plot3 <- plot2
  return(plot3)
}

#plot.2 ver2
plot.2s <- function(XX){
  plot <- ggplot() + 
    geom_bar(data=XX,aes(x=SCENARIO, y = Value, fill=Variable, group=Variable), stat='identity') + 
    ylab(ylab1) + xlab(xlab1) + guides(fill=guide_legend(title=NULL)) + 
    MyThemeLine +
    theme(legend.position=, text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = 12))+
    guides(fill=guide_legend(ncol=3,title=NULL))
  
  plot3 <- plot + scale_fill_manual(values=colorpal)  
  #    annotate("segment",x=miny,xend=maxy,y=0,yend=0,linetype="solid",color="grey") + theme(legend.position='bottom')
  return(plot3)
}

