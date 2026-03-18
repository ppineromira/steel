changeCodesFNAM <- function(dt, columnsToConvert, to){
  
  setDT(dt)
  
  for (col in columnsToConvert) {
    
    if (col %in% names(dt)) {
      
      if(to == "FIGARO"){
        
        set(dt, i = which(dt[[col]] == "C10-C12"), j = col, value = "C10T12")
        set(dt, i = which(dt[[col]] == "C13-C15"), j = col, value = "C13T15")
        set(dt, i = which(dt[[col]] == "C31_C32"), j = col, value = "C31_32")
        set(dt, i = which(dt[[col]] == "E37-E39"), j = col, value = "E37T39")
        set(dt, i = which(dt[[col]] == "J59_J60"), j = col, value = "J59_60")
        set(dt, i = which(dt[[col]] == "J62_J63"), j = col, value = "J62_63")
        set(dt, i = which(dt[[col]] == "M69_M70"), j = col, value = "M69_70")
        set(dt, i = which(dt[[col]] == "M74_M75"), j = col, value = "M74_75")
        set(dt, i = which(dt[[col]] == "N80-N82"), j = col, value = "N80T82")
        set(dt, i = which(dt[[col]] == "Q87_Q88"), j = col, value = "Q87_88")
        set(dt, i = which(dt[[col]] == "R90-R92"), j = col, value = "R90T92")
        
        set(dt, i = which(dt[[col]] == "CPA_C10-12"), j = col, value = "CPA_C10T12")
        set(dt, i = which(dt[[col]] == "CPA_C13-15"), j = col, value = "CPA_C13T15")
        set(dt, i = which(dt[[col]] == "CPA_C31_32"), j = col, value = "CPA_C31_32")
        set(dt, i = which(dt[[col]] == "CPA_E37-39"), j = col, value = "CPA_E37T39")
        set(dt, i = which(dt[[col]] == "CPA_J59_60"), j = col, value = "CPA_J59_60")
        set(dt, i = which(dt[[col]] == "CPA_J62_63"), j = col, value = "CPA_J62_63")
        set(dt, i = which(dt[[col]] == "CPA_M69_70"), j = col, value = "CPA_M69_70")
        set(dt, i = which(dt[[col]] == "CPA_M74_75"), j = col, value = "CPA_M74_75")
        set(dt, i = which(dt[[col]] == "CPA_N80-82"), j = col, value = "CPA_N80T82")
        set(dt, i = which(dt[[col]] == "CPA_Q87_88"), j = col, value = "CPA_Q87_88")
        set(dt, i = which(dt[[col]] == "CPA_R90-92"), j = col, value = "CPA_R90T92")
        
        set(dt, i = which(dt[[col]] == "B8_S11"), j = col, value = "P5M")
        
        set(dt, i = which(dt[[col]] == "WRL_REST"), j = col, value = "FIGW1")
        
      } else if(to == "FNAM"){
        
        set(dt, i = which(dt[[col]] == "C10T12"), j = col, value = "C10-C12")
        set(dt, i = which(dt[[col]] == "C13T15"), j = col, value = "C13-C15" )
        set(dt, i = which(dt[[col]] == "C31_32"), j = col, value = "C31_C32" )
        set(dt, i = which(dt[[col]] == "E37T39"), j = col, value = "E37-E39")
        set(dt, i = which(dt[[col]] == "J59_60"), j = col, value = "J59_J60")
        set(dt, i = which(dt[[col]] == "J62_63"), j = col, value = "J62_J63")
        set(dt, i = which(dt[[col]] == "M69_70"), j = col, value = "M69_M70")
        set(dt, i = which(dt[[col]] == "M74_75"), j = col, value = "M74_M75")
        set(dt, i = which(dt[[col]] == "N80T82"), j = col, value = "N80-N82")
        set(dt, i = which(dt[[col]] == "Q87_88"), j = col, value = "Q87_Q88")
        set(dt, i = which(dt[[col]] == "R90T92"), j = col, value = "R90-R92")
        
        set(dt, i = which(dt[[col]] == "CPA_C10T12"), j = col, value = "CPA_C10-12")
        set(dt, i = which(dt[[col]] == "CPA_C13T15"), j = col, value = "CPA_C13-15" )
        set(dt, i = which(dt[[col]] == "CPA_C31_32"), j = col, value = "CPA_C31_32" )
        set(dt, i = which(dt[[col]] == "CPA_E37T39"), j = col, value = "CPA_E37-39")
        set(dt, i = which(dt[[col]] == "CPA_J59_60"), j = col, value = "CPA_J59_60")
        set(dt, i = which(dt[[col]] == "CPA_J62_63"), j = col, value = "CPA_J62_63")
        set(dt, i = which(dt[[col]] == "CPA_M69_70"), j = col, value = "CPA_M69_70")
        set(dt, i = which(dt[[col]] == "CPA_M74_75"), j = col, value = "CPA_M74_75")
        set(dt, i = which(dt[[col]] == "CPA_N80T82"), j = col, value = "CPA_N80-82")
        set(dt, i = which(dt[[col]] == "CPA_Q87_88"), j = col, value = "CPA_Q87_88")
        set(dt, i = which(dt[[col]] == "CPA_R90T92"), j = col, value = "CPA_R90-92")
        
        set(dt, i = which(dt[[col]] == "P5M"), j = col, value = "B8_S11")
        
        set(dt, i = which(dt[[col]] == "FIGW1"), j = col, value = "WRL_REST")
      }
    }
  }
  
  setDF(dt)
  
  return(dt)
  
}