adjustLabelsFNAM <- function(dt){
  
  columnMapping <- c(
    "BASE" = "base",
    "CTR" = "ctr",
    "SET_I" = "Set_i",
    "M" = "m",
    "SET_J" = "Set_j",
    "VALUE" = "value"
  )
  
  
  dt <- dt %>%
    rename_with(~ columnMapping[.], all_of(names(columnMapping)))  
  
  return(dt)  
  
}