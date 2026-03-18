checkTargets <- function(targets, sums){
  
  check <- targets %>% 
    mutate(sum = sums) %>% 
    mutate(alertC1 = if_else((value == 0 | value < 0.001) & sum > 0, "p", "ok"),
           alertC2 = if_else((sum == 0 | sum < 0.001) & value > 0, "p", "ok"),
           alertC3 = if_else(is.finite(sum/value) > 10, "p", "ok"),
           alertC4 = if_else(is.finite(value/sum) > 10, "p", "ok"))
  
}