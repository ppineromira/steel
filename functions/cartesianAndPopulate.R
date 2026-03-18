cartesianAndPopulate <- function(dt, table, populate){
  
  if(table == "P1nace"){
    
    cartesianPopulated <- CJ(area = c(EuCountries, NonEuCountries, "WRL_REST"),
                 pi = productModel,
                 sorted = FALSE)
    
    if(populate == TRUE){
      
      cartesianPopulated[dt, `:=` (value = i.value), on = .(area, pi)]
    }
    
  }
  
  if(table == "P1cpa"){

    cartesianPopulated <- CJ(area = c(EuCountries, NonEuCountries, "WRL_REST"),
                             pi = productModel,
                             sorted = FALSE)

    if(populate == TRUE){

      cartesianPopulated[dt, `:=` (value = i.value), on = .(area, pi)]
    }

  }
  
  if(table == "USE"){
    
    useRow <- CJ(ctr = c(EuCountries, NonEuCountries, "WRL_REST"),
                 Set_i = productModel,
                 sorted = FALSE)
    
    useCol <- CJ(m = c(EuCountries, NonEuCountries, "WRL_REST"),
                 Set_j = c(industryModel, fuFnam),
                 sorted = FALSE)
    
    useVa <- CJ(ctr = c(EuCountries, NonEuCountries, "WRL_REST"),
                Set_j = industryModel,
                Set_i = c(d21x31, b1gFnam),
                sorted = FALSE)
    useVa[, m := ctr]
    
    useFu <- CJ(ctr = c(EuCountries, NonEuCountries, "WRL_REST"),
                Set_j = fuFnam,
                Set_i = c(d21x31, b1gFnam),
                sorted = FALSE)
    useFu[,m := ctr]
    
    cartesianPopulated <- expand_grid(useCol, useRow) %>% 
      bind_rows(useVa, useFu) %>% 
      as.data.table()
    
   if(populate == TRUE){
     
     cartesianPopulated[dt, `:=` (value = i.value), on = .(ctr,  m, Set_i,  Set_j)]
     
   }
    
  } else if(table == "MAKE"){
    
    cartesianPopulated <- CJ(m = c(EuCountries, NonEuCountries, "WRL_REST"),
                   Set_j = productModel,
                   Set_i = industryModel,
                   sorted = FALSE)
    
    cartesianPopulated[,ctr := m]
    
    if(populate == TRUE){
      
      cartesianPopulated[dt, `:=` (value = i.value), on = .(ctr,  m, Set_i,  Set_j)]
    }
    
  }

  if(populate == TRUE){
    
    cartesianPopulated[is.na(value), value := 0] 
    
  }
  
  return(cartesianPopulated)
}