# INVESTMENT MATRIX -------------------------------------------------------
invMatList <- readGDX(loadFrom = "inputFiles/FNAM.gdx", symbols = "Investment_Matrix")
invMat <- invMatList$Investment_Matrix$records

p51g <- useFinal %>% 
  filter(Set_i %in% c(productModel, "D21X31"),
         Set_j %in% NsFnam) %>% 
  group_by(base, m, Set_i) %>% 
  summarise(p51g = sum(value)) %>% 
  ungroup() %>% 
  left_join(corrCpa, by = c("Set_i" = "FIGARO_Model"))

invMatShares <- invMat %>% 
  changeIsoCodes(columnsToConvert = "ctr", to = "iso2c") %>% 
  changeCodesFNAM(to = "FNAM", c("Set_i", "Set_j", "ctr", "m")) %>% 
  group_by(base, ctr, Set_i) %>% 
  mutate(share = if_else(is.finite(value/sum(value)),value/sum(value), 0)) %>% 
  ungroup() %>% 
  select(-value) 

xShares <- useFinal %>% 
  filter(Set_j %in% industryModel) %>% 
  group_by(base, m, Set_j) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  left_join(corrNace, by = c("Set_j" = "FIGARO_Model")) %>% 
  group_by(m, FIGARO_64) %>% 
  mutate(share = if_else(is.finite(value/sum(value)),value/sum(value), 0)) %>% 
  ungroup()  %>% 
  select(m, Set_j, share)

invCpa <- p51g %>%
  left_join(invMatShares, by = c("base", "m" = "ctr", "FIGARO_64" = "Set_i"), 
            relationship = "many-to-many") %>% 
  mutate(value = p51g * share) %>% 
  select(base, m, Set_i, Set_j, value) %>% 
  filter(!is.na(Set_j)) %>% 
  left_join(corrNace, by = c("Set_j" = "FIGARO_64"),
            relationship = "many-to-many") %>% 
  left_join(xShares, by = c("m", "FIGARO_Model" = "Set_j")) %>% 
  mutate(share*value) %>% 
  select(base, m, Set_i, m, FIGARO_Model, value) %>% 
  rename(ctr = m, Set_j = FIGARO_Model) %>% 
  changeOldFnamCodes(c("Set_i", "Set_j"), to = "FNAM") %>% 
  changeCodesFNAM(c("Set_i", "Set_j"), to = "FIGARO") %>% 
  changeIsoCodes(columnsToConvert = "ctr", to = "iso3c") %>% 
  mutate(ctr = if_else(ctr == "WRL_REST", "ROW", ctr)) %>% 
  filter(value != 0)
