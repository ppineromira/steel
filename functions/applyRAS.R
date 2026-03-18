applyRAS <- function(ioTable, cTarget, rTarget, tol, maxIter){
  
  Zprior <- ioTable
  
  cError <- head(sort(abs(rowSums(Zprior) - cTarget), decreasing = T))[1]
  rError <- head(sort(abs(colSums(Zprior) - rTarget), decreasing = T))[1]
  
  loopError <- max(cError, rError)
  
  iterNumber <- 0
  loopError <- Inf
  loopErrorPrev <- loopError
#  while(loopError > tol & iterNumber < maxIter & loopErrorPrev >= loopError){  
while(loopError > tol & iterNumber < maxIter){
    
    loopErrorPrev <- loopError
    
    for(i in 1:nrow(Zprior)) Zprior[i,] <- allocNP(Zprior[i,], cTarget[i])
    for(j in 1:ncol(Zprior)) Zprior[,j] <- allocNP(Zprior[,j], rTarget[j])
    
    # Calculate errors
    cError <- head(sort(abs(rowSums(Zprior) - cTarget), decreasing = T))[1]
    rError <- head(sort(abs(colSums(Zprior) - rTarget), decreasing = T))[1]
    loopError <- max(cError, rError)
    
    # Print iteration error
    if(cError > rError){
      
      reportLine <- paste0("Iter ", iterNumber," max error column ", names(cError), " = ",
                           round(as.numeric(cError), 5))

    } else{

      reportLine <- paste0("Iter ", iterNumber," max error row ",
                           names(rError), " = ", round(as.numeric(rError), 3))
       
    }
    
    print(reportLine)
    
    iterNumber <- iterNumber + 1

  }
  
  return(Zprior)
  
}
