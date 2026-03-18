produceTargets <- function(target, version){
  
  TargetVa <- useUnb %>% 
    .[, .(value = sum(value)), by = .(ctr, Set_i)] %>% 
    .[Set_i %in% c(d21x31, b1gFnam)]
  
  TargetFu <- useUnb[Set_j %in% fuFnam] %>% 
    .[, .(value = sum(value)), by = .(m, Set_j)]
  
  if(version == "supply"){
    
    TargetCpa <- make[, .(value = sum(value)), by = .(m, Set_j)]
    
    TargetNace <- make[, .(value = sum(value)), by = .(ctr, Set_i)]
    
    if(target == "column"){
      
      TargetDis <- TargetCpa %>%
        rename(ctr = m, Set_i = Set_j) %>% 
        bind_rows(TargetVa) %>% 
        createAreaAndPiLabel("ctr", "Set_i", "icuse") %>% 
        arrange(match(icuse, c(allCpa, allVa)))
      
    } else if (target == "row"){
      
      TargetDis <- TargetNace %>% 
        rename(m = ctr, Set_j = Set_i) %>% 
        bind_rows(TargetFu) %>% 
        createAreaAndPiLabel("m", "Set_j", "icuse") %>% 
        arrange(match(icuse, c(allNace, allFu)))
      
    }
    
  }
  
  return(TargetDis)
  
}
