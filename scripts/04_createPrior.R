# CREATE PRIOR ------------------------------------------------------------
# Apply shares
makeDis <- supplyE3 %>% 
  filter(Set_j %in% disNace$FIGARO_Model | 
           Set_i %in% disCpa$FIGARO_Model)

if(Ed == "25"){
  
  makeDisCand <- lapply(setdiff(CandidateCountries, "TR"),
                        duplicateWRL_REST, makeDis, "supply") %>%
    rbindlist()
  
  makeDis <- makeDis %>% 
    bind_rows(makeDisCand)
  
}

makeDis <- makeDis %>% 
  applySharesE3(dtIoName = "supplyE3", dissVaFu = FALSE)

performTestE3Shares(makeFnam, makeDis, "make")
performTestShareUnmodified(makeFnam, makeDis, "make")

useDis <- useE3 %>% 
  filter(Set_j %in% disNace$FIGARO_Model | 
           Set_i %in% disCpa$FIGARO_Model) 

if(Ed == "25"){
  
  useDisCand <- lapply(setdiff(CandidateCountries, "TR"),
                       duplicateWRL_REST, useDis, "use") %>%
    rbindlist()
  
  useDis <- useDis %>% 
    bind_rows(useDisCand)
  
}

useDis <- useDis %>% 
  applySharesE3(dtIoName = "useE3", dissVaFu = TRUE) %>% 
  distinct()

performTestE3Shares(useFnam, useDis, "use")
performTestShareUnmodified(useFnam, useDis, "use")

# Prepare Prior
useUnb <- cartesianAndPopulate(useDis, table = "USE", populate = TRUE) %>% 
  createAreaAndPiLabel(area = "ctr", pi = "Set_i", newColumnName = "icuseRow") %>% 
  createAreaAndPiLabel(area = "m", pi = "Set_j", newColumnName = "icuseCol")

useUnb[useFnam, `:=` (value = i.value), on = .(ctr,  m, Set_i,  Set_j)]

make <- cartesianAndPopulate(makeDis, table = "MAKE", populate = TRUE) %>% 
  createAreaAndPiLabel(area = "ctr", pi = "Set_i", newColumnName = "icsupRow") %>% 
  createAreaAndPiLabel(area = "m", pi = "Set_j", newColumnName = "icsupCol")

make[makeFnam, `:=` (value = i.value), on = .(ctr,  m, Set_i,  Set_j)]

useUnbCast <- dcast(useUnb, icuseRow ~ icuseCol, value.var = "value", fill = 0)
rowNamesCast <- useUnbCast$icuseRow

usePrior <- useUnbCast[, icuseRow := NULL] %>% as.matrix()
rownames(usePrior) <- rowNamesCast

usePrior <- usePrior[c(allCpa, allVa), c(allNace, allFu)]

if(adjustNeg == TRUE){
  
  negIc <- extractNegativeEntries(usePrior[allCpa, allNace])
  
  usePrior[negIc$rowname, negIc$colname] <- 0
  
  negCons <- extractNegativeEntries( usePrior[allCpa, paste0(rep(c(EuCountries, NonEuCountries, "WRL_REST"), each = 4),
                                                             "_", rep(setdiff(fuFnam, c("P51G", "B8_S11")), times = 46))])
  
  usePrior[negCons$rowname, negCons$colname] <- 0
}
