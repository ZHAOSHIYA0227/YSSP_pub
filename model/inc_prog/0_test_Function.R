


OD <- PoutputNational_cge %>% 
  filter(R %in% c("IND", "ZAF"))
Demand_data <- demandcge
quantile_input <-  seq(from = 0.1, to = 1, by = 0.1)

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
  #   select(-"I")
  
  # OD <- PoutputNational_cge
  # quantile_input <-  seq(from = 0.1, to = 1, by = 0.1)
  # quantile_input <- seq(from = 0.2, to = 1, by = 0.2)
  
  lis_ref <- as.character(unique(OD$Ref))
  lis_I_abb <- as.character(unique(OD$I_abb))
  
  
  Seg_pop <- rgdx.param(Demand_data, "FreqSegall") %>%
    rename.vars(colnames(.), c("R",'Ref','Y','Seg','pop')) %>%
    SegNum()
  
  
  Mu_exp <- rgdx.param(Demand_data, "Mu_exp") %>%
    rename.vars(colnames(.), c("R",'Ref','Y','Mu_exp')) %>%
    filter(Ref %in% lis_ref)
  
  
  Sigma_exp <- rgdx.param(Demand_data, "Sigma_exp") %>%
    rename.vars(colnames(.), c("R",'Ref','Y','Sigma_exp')) %>%
    filter(Ref %in% lis_ref)
  
  
  q <- quantile_input
  
  # the q_value for each scenario and each year
  data_q_value <- Sigma_exp %>%
    left_join(Mu_exp) %>%
    group_by(Ref,Y,R) %>%
    dplyr::reframe(Sigma_exp = Sigma_exp, Mu_exp = Mu_exp, 
                     q_value = c(qlnorm(q,Mu_exp,Sigma_exp)[-length(q)],1000000)) %>%
    mutate(DEC = as.character(c(t(rep(q*length(q),nrow(.)/length(q)))))) 
  
  
  # identifying the decile where each income segment belongs
  data_tmp1 <- data_q_value  %>%
    spread(DEC,q_value) %>%
    left_join(Seg_pop)
  data_tmp2 <- ifelse((data_tmp1[,which(colnames(data_tmp1) %in% as.character(q*length(q)))]-data_tmp1$Lower) <0, 1,0) %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    dplyr::rename(DEC_tmp = colnames(.)) %>%
    mutate(DEC = as.character(DEC_tmp +1))
  data1_tmp3 <- data_tmp1%>%
    cbind(data_tmp2) %>%
    select(-c(as.character(q*length(q)),"DEC_tmp")) %>%
    left_join(data_q_value) %>%
    mutate(idx = ifelse((Lower <= q_value & q_value <= Upper),1,0)) %>% # idx == 1: the segment is cut by q_value
    right_join(OD) %>%
    spread(I_abb, value)
  
  # filtering and splitting the segments where the decile thresholds cut
  data1_tmp4 <- data1_tmp3 %>%
    mutate(floor1 = ifelse(idx == 1, floor((q_value - Lower)/StepLength),floor((Upper - Lower)/StepLength)),
           floor2 = ifelse(idx == 1, floor((Upper - q_value)/StepLength), 0),
           ceiling1 = ifelse(idx == 1, ceiling((q_value - Lower)/StepLength), ceiling((Upper - Lower)/StepLength)),
           ceiling2 = ifelse(idx == 1, ceiling((Upper - q_value)/StepLength), 0)) %>%
    gather(I, value, all_of(lis_I_abb))%>%
    mutate(ExpShare = value/Upper) %>%
    # select(-"total") %>%
    transform(DEC = as.numeric(DEC))
  # colnames(data1_tmp4)
  
  data1_ExpShare <- data1_tmp4 %>%
    select(c("Ref", "R", "Y", "Sigma_exp", "Mu_exp", "Seg", "I", "ExpShare"))
  if(split_all == TRUE){
    data1_tmp6 <- data1_tmp4 %>%
      # filter(Upper-Lower >= LengthThreshold) %>%
      select(-"ExpShare") %>%
      spread(I, value) %>%
      select(-all_of(lis_I_abb))  
  }else{
    data1_tmp6 <- data1_tmp4 %>%
      filter(DEC != length(q),idx == 1) %>% 
      filter(Upper-Lower >= LengthThreshold) %>%
      select(-"ExpShare") %>%
      spread(I, value) %>%
      select(-all_of(lis_I_abb))
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
    select(-c(ceiling1,ceiling2,floor1,floor2)) 
  
  dat2 <- data1_tmp7 %>%
    mutate(UpperLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*nstep,
                                               ifelse(idx == 1, q_value,Upper)), # min(q_value,Upper)
                             ifelse(nstep < n, q_value+StepLength*nstep, Upper)), 
           LowerLim = ifelse(part == 1, ifelse(nstep < n, Lower+StepLength*(nstep-1),  Lower+StepLength*(n-1)),
                             ifelse(nstep < n, q_value+StepLength*(nstep-1), q_value+StepLength*(n-1))))
  
  data1 <- dat2 %>%
    ungroup() %>%
    mutate(DEC_new = DEC+part-1) %>%
    select(-c("Upper", "Lower", "DEC")) %>%
    dplyr::rename(Upper = "UpperLim", Lower = "LowerLim", DEC = "DEC_new") %>%
    left_join(data1_ExpShare) %>%
    mutate(value_new = Upper*ExpShare, 
           density = dlnorm((Upper+Lower),meanlog = Mu_exp, sdlog = Sigma_exp), 
           pop_new = (Upper-Lower)*density) %>%
    select(-c("pop","density","idx1","idxnstep","part","nstep","n")) %>%
    dplyr::rename(value = "value_new", pop = "pop_new")
  # view(data1) 
  
  if(split_all == TRUE){
    data2 <- data1 %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = Mu_exp, sdlog = Sigma_exp),
             pop = (Upper-Lower)*dlnorm(Upper,meanlog = Mu_exp, sdlog = Sigma_exp)) %>%
      select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_abb)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,length(q),1)))) %>%
      # gather(I, value, all_of(lis_I_abb)) %>%
      group_by(Ref,R,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new),#pop_old = sum(pop),
                       .groups="drop")
  }else{
    data2 <- data1_tmp4 %>%
      filter(!(idx == 1 & DEC != "10")) %>% 
      select(colnames(data1)) %>%
      rbind(data1) %>%
      mutate(pop_new = (Upper-Lower)*dlnorm((Upper+Lower)/2,meanlog = Mu_exp, sdlog = Sigma_exp)) %>%
      select(-"idx") %>%
      transform(I = factor(I, levels = lis_I_abb)) %>%
      transform(DEC = factor(as.character(DEC), levels = as.character(seq(1,length(q),1)))) %>%
      # gather(I, value, all_of(lis_I_abb)) %>%
      group_by(Ref,R,Y,DEC,I) %>%
      dplyr::summarise(value_seg = sum(value*pop_new)/sum(pop_new),  pop = sum(pop_new),#pop_old = sum(pop),
                       .groups="drop")
  }
  return(data2)
}
