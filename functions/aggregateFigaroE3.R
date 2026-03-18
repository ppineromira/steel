aggregateFigaroE3 <- function(area, table){

  print(area)
  
  if(table != "FIGARO_E3_OUTPUT_SUPPLY_ROW_PROCESSED"){
   
    dtE3 <- tbl(conAMA, table) %>% 
      filter(COUNTERPART_AREA == area) %>% 
      collect() %>% 
      as.data.table() 
    
  } else if(table == "FIGARO_E3_OUTPUT_SUPPLY_ROW_PROCESSED"){
    
    dtE3 <- tbl(conAMA, table) %>% 
      filter(REF_AREA == area) %>% 
      collect() %>% 
      as.data.table()    
    
  }
  
  if(table %in% c("FIGARO_E3_SUPPLY", "FIGARO_E3_USE")){
    
    dtE3[as.data.table(corrE3Nace %>% 
                         bind_rows(corrE3Fd)), on = .(COL_PI = COL_PI_E_3), COL_PI := i.COL_PI]
    dtE3[as.data.table(corrE3Cpa %>% 
                         bind_rows(corrE3B1g)), on = .(ROW_PI = ROW_PI_E_3), ROW_PI := i.ROW_PI]
    
    dtE3 <- dtE3[, .(OBS_VALUE = sum(OBS_VALUE)), by = .(REF_AREA, COUNTERPART_AREA, COL_PI, ROW_PI)]
    
    } else if(table == "FIGARO_E3_OUTPUT_SUPPLY_COL_PROCESSED"){
    
    dtE3[as.data.table(corrE3Nace %>% 
                         bind_rows(corrE3Fd)), on = .(COL_PI = COL_PI_E_3), COL_PI := i.COL_PI]
    
      dtE3 <- dtE3[, .(OBS_VALUE = sum(OBS_VALUE)), by = .(COUNTERPART_AREA, COL_PI)]
      
    } else if(table == "FIGARO_E3_OUTPUT_SUPPLY_ROW_PROCESSED"){
      
      dtE3[as.data.table(corrE3Cpa %>% 
                           bind_rows(corrE3B1g)), on = .(ROW_PI = ROW_PI_E_3), ROW_PI := i.ROW_PI]
      
      dtE3 <- dtE3[, .(OBS_VALUE = sum(OBS_VALUE)), by = .(REF_AREA, ROW_PI)]
    }

  return(dtE3)
}
