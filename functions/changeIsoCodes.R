changeIsoCodes <- function(dt, columnsToConvert, to){
  
  for (column in columnsToConvert) {
     
    # Convert the country codes
    dt[[column]] <- countrycode(dt[[column]], origin = ifelse(to == "iso2c", "iso3c", "iso2c"),
                             destination = to, warn = FALSE,  nomatch = NULL)
    
    # Convert 'ROW' to 'FIGW1' and vice versa
    dt <- dt %>% 
      mutate(!!sym(column) := case_when(
        !!sym(column) == "ROW" ~ "FIGW1",
        !!sym(column) == "FIGW1" ~ "ROW",
        TRUE ~ !!sym(column)
      ))
    
      }
  
  return(dt)
}