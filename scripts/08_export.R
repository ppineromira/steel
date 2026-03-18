# EXPORT ------------------------------------------------------------------
if(export == TRUE){
  
  if(Ed == "25"){
    
    # fnamRest <- tbl(conAMA, paste0("FIGARO_NAM_25ED_", tPeriod)) %>% 
    #   filter(!SET_I %in% c(product64fnam,  d21x31, b1gFnam, industry64fnam),
    #          !SET_J %in% c(industry64fnam, FinalDemandP3P5NoAgg, "B8_S11", product64fnam)) %>% 
    #   collect() %>%
    #   as.data.table() %>% 
    #   adjustLabelsFNAM() %>% 
    #   anti_join(useFnam %>% bind_rows(makeFnam), by = c("ctr",  "m",  "Set_i", "Set_j")) %>% 
    #   mutate(base = as.character(tPeriod))
    
  } else if(Ed == "old"){
    
    # Load FNAM.gdx and process to FIGARO labels
    fnamList <- readGDX(loadFrom = "inputFiles/FNAM.gdx", symbols = "NAM")
    fnamRest <- fnamList$NAM$records %>% 
      convertFactorsToCharacter() %>% 
      changeOldFnamCodes(columnsToConvert = "Set_j", to = "FIGARO") %>% 
      changeIsoCodes(columnsToConvert = c("ctr", "m"), to = "iso2c") %>% 
      changeCodesFNAM(to = "FNAM", c("Set_i", "Set_j", "ctr", "m")) %>% 
      anti_join(useFnam %>% bind_rows(makeFnam), by = c("ctr",  "m",  "Set_i", "Set_j")) %>% 
      filter((!Set_i %in% product64fnam) & (Set_j != "S2")) %>% 
      filter((!Set_j %in% industry64fnam) & (Set_i != "P7"))
    
  }
  
  fnamFinal <- makeFinal %>% 
    bind_rows(useFinal, s2Cpa, p7Nace, fnamRest) %>% 
    changeOldFnamCodes(c("Set_i", "Set_j"), to = "FNAM") %>% 
    changeCodesFNAM(c("Set_i", "Set_j"), to = "FIGARO") %>% 
    changeIsoCodes(columnsToConvert = c("ctr", "m"), to = "iso3c") %>% 
    mutate(ctr = if_else(ctr == "WRL_REST", "ROW", ctr),
           m = if_else(m == "WRL_REST", "ROW", m)) %>% 
    filter(value != 0)
  
  ctrs <- c("All", unique(fnamFinal$ctr))
  
  for(ctrGdx in ctrs){
    
    if(ctrGdx == "All"){
      
      fnamFinalCtr <- fnamFinal
      
      invCpaCtr <- invCpa
      
    } else if(ctrGdx != "All"){
      
      fnamFinalCtr <- fnamFinal %>% 
        filter(ctr == ctrGdx)
      
      invCpaCtr <- invCpa %>% 
        filter(ctr == ctrGdx)
      
    }
    
    dt = Container$new()
    
    # Create sets with proper names matching your data
    base_set = Set$new(dt, "base", records = unique(fnamFinalCtr$base), description = "base")
    ctr_set = Set$new(dt, "ctr", records = unique(fnamFinalCtr$ctr), description = "country")
    Set_i_set = Set$new(dt, "Set_i", records = unique(fnamFinalCtr$Set_i), description = "row")
    m_set = Set$new(dt, "m", records = unique(fnamFinalCtr$m), description = "importer")
    Set_j_set = Set$new(dt, "Set_j", records = unique(fnamFinalCtr$Set_j), description = "column")
    
    inv_Set_i_set = Set$new(dt, "inv_Set_i", records = unique(invCpaCtr$Set_i), description = "row")
    inv_Set_j_set = Set$new(dt, "inv_Set_j", records = unique(invCpaCtr$Set_j), description = "column")
    
    # Define the domain correctly - match the column order in your data
    # Assuming your data has columns in this order: ctr, Set_i, m, Set_j, base
    # Set the records - make sure your data matches the domain structure
    
    nam_vars <- c()
    
    if(ctrGdx == "All"){
      
      NAM = Parameter$new(dt, "NAM", c(base_set, ctr_set, Set_i_set, m_set, Set_j_set), description = "FIGARO National Accounting Matrix (FNAM)")
      
      NAM$setRecords(fnamFinalCtr)
      
      Investment_Matrix = Parameter$new(dt, "Investment_Matrix", c(base_set, ctr_set, inv_Set_i_set, inv_Set_j_set), 
                                        description = "Investment Matrix")
      
      Investment_Matrix$setRecords(invCpaCtr)
      
    } else if(ctrGdx != "All"){
      
      var_name <- paste0("NAM_", ctrGdx)
      
      assign(var_name, 
             Parameter$new(dt, 
                           var_name, 
                           c(base_set, ctr_set, Set_i_set, m_set, Set_j_set), 
                           description = "FIGARO National Accounting Matrix (FNAM)"))
      
      get(var_name)$setRecords(fnamFinalCtr)
      
      inv_var_name <- paste0("Investment_Matrix_", ctrGdx)
      
      assign(inv_var_name, 
             Parameter$new(dt, 
                           inv_var_name, 
                           c(base_set, ctr_set, inv_Set_i_set, inv_Set_j_set), 
                           description = "Investment Matrix"))
      
      get(inv_var_name)$setRecords(invCpaCtr)
      
    }
    
    if(ctrGdx == "All"){
      
      dt$write(paste0("outputFiles/FNAM.gdx"))
      
      fwrite(fnamFinalCtr, "outputFiles/FNAM.csv")
      
    } else if(ctrGdx != "All"){
      
      dt$write(paste0("outputFiles/FNAM_2017_", ctrGdx,".gdx"))
      
      fwrite(fnamFinalCtr, paste0("outputFiles/FNAM_2017_", ctrGdx,".csv"))
      
    }
    
  }
  
}
