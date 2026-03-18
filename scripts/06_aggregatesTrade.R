# EXPORTS -----------------------------------------------------------------
s2Cpa <- useFinal %>% 
  filter(ctr != m) %>% 
  group_by(base, ctr, Set_i) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(Set_i %in% productModel) %>% 
  mutate(m = ctr, Set_j = "S2") %>% 
  select(base, ctr, Set_i, m, Set_j, value)

# IMPORTS -----------------------------------------------------------------
p7Nace <- useFinal %>% 
  filter(ctr != m) %>% 
  group_by(base, m, Set_j) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(Set_j %in% industryModel) %>% 
  mutate(ctr = m, Set_i = "P7") %>% 
  select(base, ctr, Set_i, m, Set_j, value)