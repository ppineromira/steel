renameW2 <- function(dt, to){
  
  if(to == "FNAM"){

    # Update refArea using data.table's := operator
    dt[ctr == "W2", ctr := m]
    
  } else if(to == "FIGARO"){
    
    # Update refArea using data.table's := operator
   dt <- dt %>% 
      mutate(ctr := case_when(
        Set_i %in% c(d21x31, "D29X39", "D11", "D12", "B2", "B3") ~ "W2",
        TRUE ~ ctr
      ))
    
  }
  return(dt)
}