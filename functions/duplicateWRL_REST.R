duplicateWRL_REST <- function(candidate, dt, table){
  
  if(table == "supply"){
    
    dtCandidate <- dt %>%
      filter(ctr == "WRL_REST" & m == "WRL_REST") %>%
      mutate(ctr = candidate) %>%
      bind_rows(dt %>%
                  filter(ctr == "WRL_REST" & m == "WRL_REST") %>%
                  mutate(m = candidate))
    
  } else if (table == "use"){
    
    # Trade with all countries and WRL_REST
    dtCandidateDomTrade <- dt %>% 
      filter(ctr == "WRL_REST" | m == "WRL_REST") %>% 
      mutate(ctr = if_else(ctr == "WRL_REST", candidate, ctr),
             m = if_else(m == "WRL_REST", candidate, m)) %>% 
      bind_rows( dt %>%
                   filter(ctr == "WRL_REST" & m == "WRL_REST") %>%
                   mutate(ctr = candidate)) %>% 
      bind_rows( dt %>%
                   filter(ctr == "WRL_REST" & m == "WRL_REST") %>%
                   mutate(m = candidate))
    
    # Intra Candidate Countries trade
    dtCandidateIntra <- NULL
    
    dtCandidateToWRL_REST <- dt %>%
      filter(ctr == "WRL_REST" & m == "WRL_REST") %>%
      mutate(ctr = candidate)
    
    for(oth in setdiff(CandidateCountries, c(candidate, "TR"))){
      
      dtCandidateIntra <- dtCandidateIntra %>%
        bind_rows(dtCandidateToWRL_REST %>% 
                mutate(m = if_else(m == "WRL_REST", oth, m)))
      
    }
    
    dtCandidate <- dtCandidateDomTrade %>% 
      bind_rows(dtCandidateIntra)
    
  }

    return(dtCandidate)
}