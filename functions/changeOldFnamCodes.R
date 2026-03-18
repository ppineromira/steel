changeOldFnamCodes <- function(dt, columnsToConvert, to){
  
  setDT(dt)

  for (col in columnsToConvert) {
    
    if (col %in% names(dt)) {
    
      if(to == "FIGARO"){
        
        set(dt, i = which(dt[[col]] == "COFOG"), j = col, value = "P3_S13")
        set(dt, i = which(dt[[col]] == "COICOP"), j = col, value = "P3_S14")
        set(dt, i = which(dt[[col]] == "COPNI"), j = col, value = "P3_S15")
        set(dt, i = which(dt[[col]] == "B8S11"), j = col, value = "B8_S11")
        set(dt, i = which(dt[[col]] == "P33"), j = col, value = "OP_RES")
        
      } else if(to == "FNAM"){
        
        set(dt, i = which(dt[[col]] == "P3_S13"), j = col, value = "COFOG")
        set(dt, i = which(dt[[col]] == "P3_S14"), j = col, value = "COICOP")
        set(dt, i = which(dt[[col]] == "P3_S15"), j = col, value = "COPNI")
        set(dt, i = which(dt[[col]] == "B8_S11"), j = col, value = "B8S11")
        set(dt, i = which(dt[[col]] == "OP_RES"), j = col, value = "P33")
      }
    }
  }
  
  return(dt)
  
}