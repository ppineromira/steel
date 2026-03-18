# OBTAIN BALANCE MATRIX ---------------------------------------------------
# Define Targets
cTargetDis <- produceTargets(target = "column", "supply")
cTargetDisVec <- cTargetDis$value
names(cTargetDisVec) <- cTargetDis$icuse

rTargetDis <- produceTargets(target = "row", "supply")
rTargetDisVec <- rTargetDis$value
names(rTargetDisVec) <- rTargetDis$icuse

## Allocate the difference (1500) across the Ctargets
dif <-  sum(cTargetDisVec) - sum(rTargetDisVec)
cTargetDisVec <- cTargetDisVec - (cTargetDisVec / sum(cTargetDisVec) * dif)

if(checkCondition == TRUE){
  
  cTest <- checkTargets(cTargetDis, rowSums(usePrior)) %>% 
    filter(alertC1 == "p" | alertC2 == "p" | alertC3 == "p" | alertC4 == "p")
  
  rTest <- checkTargets(rTargetDis, colSums(usePrior)) %>% 
    filter(alertC1 == "p" | alertC2 == "p" | alertC3 == "p" | alertC4 == "p")
  
  usePrior[, rTest$icuse] <- 0
  
  rTargetDisVec[rTest$icuse] <- 0
  
}

useBalMat <- applyRAS(usePrior, cTarget = cTargetDisVec, 
                      rTarget = rTargetDisVec,
                      tol = 0.001, maxIter = 1000)

useFinal <- longAndClasif(useBalMat)

makeRest <- anti_join(makeFnam, makeDis, by = c("ctr", 
                                                "m",  
                                                "Set_i" = "Set_i64", 
                                                "Set_j" = "Set_j64")) %>% 
  setDF()

makeFinal <- makeRest %>% 
  bind_rows(makeDis %>%
              mutate(base = as.character(tPeriod)) %>%
              select(base, ctr, Set_i, m, Set_j, value))