# AGREGATE FIGARO-E3 ------------------------------------------------------
# Aggregate all non-needed rows and columns for the dissagregation excercise
if(!file.exists("tmpFiles/supplyE3.rds")){
  
  supplyE3 <- lapply(c(EuCountries, NonEuCountries, "FIGW1"),
                     aggregateFigaroE3, "FIGARO_E3_SUPPLY") %>% 
    rbindlist() %>% 
    changeCodesFNAM(to = "FNAM", c("REF_AREA", "COUNTERPART_AREA", "COL_PI", "ROW_PI"))
  
  names(supplyE3) <- snakecase::to_lower_camel_case(names(supplyE3))
  
  supplyE3 <- changeLabelsFNAM(supplyE3, to = "FNAM")
  
  saveRDS(supplyE3, "tmpFiles/supplyE3.rds")
  
} else {
  
  supplyE3 <- readRDS("tmpFiles/supplyE3.rds")
}

if(!file.exists("tmpFiles/useE3.rds")){
  
  useE3 <- lapply(c(EuCountries, NonEuCountries, "FIGW1"),
                  aggregateFigaroE3, "FIGARO_E3_USE") %>% 
    rbindlist() %>% 
    changeCodesFNAM(to = "FNAM", c("REF_AREA", "COUNTERPART_AREA", "COL_PI", "ROW_PI"))
  
  names(useE3) <- snakecase::to_lower_camel_case(names(useE3))
  
  useE3 <- changeLabelsFNAM(useE3, to = "FNAM") %>% 
    as.data.table() %>% 
    renameW2(to = "FNAM")
  
  saveRDS(useE3, "tmpFiles/useE3.rds")
  
} else{
  
  useE3 <- readRDS("tmpFiles/useE3.rds")
}