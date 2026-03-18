performTestE3Shares <- function(dtFnam, dtDis, ioComp) {

  if(ioComp %in% c("p1DissE3")){

    dtTest <- semi_join(dtFnam, dtDis, by = c("area", 
                                              "pi" = "pi64")) %>% 
      setDF()
    
  } else{
    
    dtTest <- semi_join(dtFnam, dtDis, by = c("ctr", 
                                              "m", 
                                              "Set_i" = "Set_i64", 
                                              "Set_j" = "Set_j64")) %>% 
      setDF()
  }
  
  sumTest <- sum(dtTest$value)
  
  sumDis <- sum(dtDis$value)
  
  
  cat("Sum of obsValue in original:", sumTest, "\n")
  cat("Sum of obsValue in dissagregated:", sumDis, "\n")
  

}
