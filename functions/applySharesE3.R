applySharesE3 <- function(dtIo, dtIoName, dissVaFu){
  
  setDT(dtIo)
  
  dtIo[corrCpa, on = .(Set_i = FIGARO_Model), Set_i64 := FIGARO_64]
  dtIo[corrNace, on = .(Set_j = FIGARO_Model), Set_j64 := FIGARO_64]
  
  dtIoDis <- dtIo[, share := abs(value)/(sum(abs(value))),
                  by = .(ctr, m, Set_j64, Set_i64)] %>%
    group_by(ctr, m, Set_j64, Set_i64) %>%
    mutate(shareMis = 1/n()) %>% 
    ungroup() %>% 
    mutate(share = case_when(
      !is.finite(share) ~ shareMis,
      .default = share)) %>% 
    select(-value, -shareMis) %>% 
    setDT() %>% 
    changeLabelsFNAM(to = "FNAM")
  
  if(dissVaFu == TRUE){
    
    # Apply shares of D1 and B2A3G to D11, D12, B2, B3
    dtIoB1g <- NULL
    
    for(b1g in c("D11", "D12", "B2", "B3")){
      
      if(b1g %in% c("D11", "D12")){
        
        b1g64 <- "D1"
        
      } else if( b1g %in% c("B2", "B3")){
        
        b1g64 <- "B2A3G"
      }
      
      dtIoB1g <- rbindlist(list(dtIoB1g, dtIoDis[Set_i64 %in% b1g64, 
                                                 .(ctr, m, Set_j, Set_j64, 
                                                   Set_i64 = b1g, Set_i = b1g, share)]), 
                           fill = TRUE) %>% 
        mutate(ctr = m)
      
    }
    
    dtIoDis <- dtIoDis[!Set_i %in% c("D1", "B2A3G")]
    dtIoDis <- rbindlist(list(dtIoDis, dtIoB1g), fill = TRUE)
    
    if(Ed == "old"){
      
      # Apply shares of P51G to Ns
      dtIoP51g <- NULL
      
      for(capAcc in NsFnam){
        
        dtIoP51g <- rbindlist(list(dtIoP51g, dtIoDis[Set_j64 %in% "P51G",
                                                     .(ctr, m, Set_i, Set_i64,
                                                       Set_j64 = capAcc, Set_j = capAcc, share)]),
                              fill = TRUE)
        
      }
      
      dtIoDis <- dtIoDis[Set_j != "P51G"]
      dtIoDis <- rbindlist(list(dtIoDis, dtIoP51g), fill = TRUE)

    }
  } 
  
  if(dtIoName == "useE3"){
    
    dtAg <- useFnam
    
  } else{
    
    dtAg <- makeFnam
    
    # to convert from supply to make
    setnames(dtIoDis, c("Set_i", "Set_j", "Set_i64", "Set_j64"),
             c("Set_j", "Set_i", "Set_j64", "Set_i64"))
    
  }
  
  dtIoDis <- dtIoDis[dtAg, on = .(ctr, m, Set_i64 = Set_i, Set_j64 = Set_j), 
                     value := value] %>%
    .[, value := value*share] %>% 
    mutate(value = if_else(is.na(value), 0, value)) %>% 
    .[, !c("share")] %>% 
    setDF()
  
  return(dtIoDis)
  
}
