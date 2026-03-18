make <- fnamFinal %>% 
  filter(Set_i %in% industry64fnam,
            Set_j %in% product64fnam)

use <- fnamFinal %>% 
  filter(Set_i %in% c(product64fnam,  d21x31, b1gFnam),
         Set_j %in% c(industry64fnam, FinalDemandP3P5NoAgg, "B8_S11"))


testX <- useFinal %>% 
  group_by(ctr, Set_i) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(Set_i %in% c(product64fnam)) %>% 
  left_join(makeFinal %>% 
              group_by(m, Set_j) %>% 
              summarise(value = sum(value)) %>% 
              ungroup() %>% 
              filter(Set_j %in% c(product64fnam)),
            by = c("ctr" = "m",
                   "Set_i" = "Set_j")) %>% 
  mutate(diff = value.x-value.y)

testX2 <- useFinal %>% 
  group_by(m, Set_j) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(Set_j %in% c(industry64fnam)) %>% 
  left_join(makeFinal %>% 
              group_by(ctr, Set_i) %>% 
              summarise(value = sum(value)) %>% 
              ungroup() %>% 
              filter(Set_i %in% c(industry64fnam)),
            by = c("m" = "ctr",
                   "Set_j" = "Set_i")) %>% 
  mutate(diff = value.x-value.y)

testX3 <- useFnam %>% 
  group_by(ctr, Set_i) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(Set_i %in% c(product64fnam)) %>% 
  left_join(makeFnam %>% 
              group_by(m, Set_j) %>% 
              summarise(value = sum(value)) %>% 
              ungroup() %>% 
              filter(Set_j %in% c(product64fnam)),
            by = c("ctr" = "m",
                   "Set_i" = "Set_j")) %>% 
  mutate(diff = value.x-value.y)
 

ctrSel <- "DEU"

gdpIncome <- fnamFinal %>% 
  filter(Set_i %in% c("D11", "D12", "B2", "B3", "D21X31"), m == ctrSel) %>% 
  group_by(m) %>% 
  summarise(value = sum(value))

gdpDemand <- fnamFinal %>% 
  filter(Set_j %in% c("COICOP", "COFOG", "COPNI", "B8S11", "S2"), m == ctrSel) %>% 
  group_by(m) %>% 
  summarise(tot = sum(value)) %>% 
  ungroup() %>% 
  cbind.data.frame(fnamFinal %>% 
                     filter(Set_i == "P7", m == ctrSel) %>% 
                     group_by(m) %>% 
                     summarise(imp = sum(value)) %>% 
                     ungroup()) %>% 
  select(-3) %>% 
  mutate(value = tot-imp)
