performTestShareUnmodified <- function(dtFnam, dtDis, ioComp) {
  
  if(ioComp %in% c("p1DissE3")){
    
    dtRest <- anti_join(dtFnam, dtDis, by = c("area", 
                                              "pi" = "pi64")) %>% 
      setDF()
    
  } else{
    
    dtRest <- anti_join(dtFnam, dtDis, by = c("ctr", 
                                              "m",  
                                              "Set_i" = "Set_i64", 
                                              "Set_j" = "Set_j64")) %>% 
      setDF()
  }
  
  # Calculate the sum of obsValue for useTest
  percUnmodified <- (sum(dtRest$value)/sum(dtFnam$value))*100
  
  cat("% unmodified:", percUnmodified, "\n")

}