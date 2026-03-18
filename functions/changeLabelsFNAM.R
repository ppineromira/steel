changeLabelsFNAM <- function(dt, to){
  
  columnNames <- colnames(dt)
  
  columnMapping <- c(
    "base" = "timePeriod",
    "ctr" = "refArea",
    "Set_i" = "rowPi",
    "m" = "counterpartArea",
    "Set_j" = "colPi",
    "value" = "obsValue"
  )
  
  # Changes to FIGARO labels
  if(to == "FIGARO"){
    
    mapping <- columnMapping
    
  }
  
  else if (to == "FNAM"){
    
    mapping <- setNames(names(columnMapping), columnMapping)
    
  }
  
  commonCols <- intersect(names(mapping), columnNames)
  mapping <- mapping[commonCols]
  
  dt <- dt %>%
    rename_with(~ mapping[.], all_of(names(mapping)))  
  
  return(dt)  
  
}