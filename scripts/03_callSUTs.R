# SUTS FROM FNAM ----------------------------------------------------------

if(Ed == "25"){
  
  if(!file.exists("tmpFiles/makeFnamEd25.rds")){
    
    makeFnam <- tbl(conAMA, paste0("FIGARO_NAM_25ED_", tPeriod)) %>% 
      filter(SET_I %in% industry64fnam,
             SET_J %in% product64fnam) %>% 
      collect() %>%
      as.data.table() %>% 
      adjustLabelsFNAM()
    
    saveRDS(makeFnam, "tmpFiles/makeFnamEd25.rds")
    
  } else {
    
    makeFnam <- readRDS("tmpFiles/makeFnamEd25.rds")
  }
  
  if(!file.exists("tmpFiles/useFnamEd25.rds")){
    
    useFnam <- tbl(conAMA, paste0("FIGARO_NAM_25ED_", tPeriod)) %>% 
      filter(SET_I %in% c(product64fnam,  d21x31, b1gFnam),
             SET_J %in% c(industry64fnam, FinalDemandP3P5NoAgg, "B8_S11")) %>% 
      collect() %>%
      as.data.table() %>% 
      adjustLabelsFNAM()
    
    saveRDS(useFnam, "tmpFiles/useFnamEd25.rds")
    
  } else{
    
    useFnam <- readRDS("tmpFiles/useFnamEd25.rds")
  }
  
} else if(Ed == "old"){
  
  if(!file.exists("tmpFiles/makeFnamEdOld.rds")){
    
    # Load FNAM.gdx and process to FIGARO labels
    fnamList <- readGDX(loadFrom = "inputFiles/FNAM.gdx", symbols = "NAM")
    fnam <- fnamList$NAM$records %>% 
      convertFactorsToCharacter() %>% 
      changeIsoCodes(columnsToConvert = c("ctr", "m"), to = "iso2c")
    
    # Extract make matrix and aggregate
    makeFnam <- fnam %>% 
      filter(Set_i %in% industry64,
             Set_j %in% product64) %>% 
      changeCodesFNAM(to = "FNAM", c("Set_i", "Set_j", "ctr", "m")) %>% 
      as.data.table()
    
    saveRDS(makeFnam, "tmpFiles/makeFnamEdOld.rds")
    
    useFnam <- fnam %>% 
      changeOldFnamCodes(columnsToConvert = "Set_j", to = "FIGARO") %>% 
      filter(Set_i %in% c(product64, 
                          d21x31, b1gFnam),
             Set_j %in% c(industry64, FinalDemandP3P5NoAgg, "B8_S11", NsFnam)) %>% 
      changeCodesFNAM(to = "FNAM", c("Set_i", "Set_j", "ctr", "m")) %>% 
      #aggregateToP51G() %>% 
      as.data.table()
    
    saveRDS(useFnam, "tmpFiles/useFnamEdOld.rds")
    
  } else{
    
    makeFnam <- readRDS("tmpFiles/makeFnamEdOld.rds")
    
    useFnam <- readRDS("tmpFiles/useFnamEdOld.rds")
    
  }
  
}

