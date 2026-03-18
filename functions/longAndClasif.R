longAndClasif <- function(dtIo){
  
  dtLong <- reshape2::melt(dtIo, value.name = "value") %>% 
    mutate(ctr = ifelse(grepl("^WRL_REST_", Var1), "WRL_REST", sub("_.*", "", Var1)),
           Set_i = ifelse(grepl("^WRL_REST_", Var1), 
                          sub("^WRL_REST_", "", Var1),
                          sub("^[^_]*_", "", Var1)),
           m = ifelse(grepl("^WRL_REST_", Var2), "WRL_REST", sub("_.*", "", Var2)),
           Set_j = ifelse(grepl("^WRL_REST_", Var2), 
                          sub("^WRL_REST_", "", Var2),
                          sub("^[^_]*_", "", Var2)),
           base = as.character(tPeriod)) %>% 
    select(base, ctr, Set_i, m, Set_j, value)
  
  return(dtLong)
  
}
