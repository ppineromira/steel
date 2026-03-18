# CLASSIFICATIONS ---------------------------------------------------------
# EU countries
EuCountries <- tbl(conAMA, "EU_COUNTRIES") %>% 
  collect() %>%  pull(EU_COUNTRIES)

# Non EU countries (exc FIGW1)
FigaroNonEuCountries <- tbl(conAMA, "FIGARO_NON_EU_COUNTRIES") %>% 
  collect() %>%  pull(FIGARO_NON_EU_COUNTRIES)

CandidateCountries <- tbl(conAMA, "CANDIDATE_COUNTRIES") %>% 
  collect() %>%  pull(CANDIDATE_COUNTRIES)

if(Ed == "25"){
  
  NonEuCountries <- c(FigaroNonEuCountries, setdiff(CandidateCountries, FigaroNonEuCountries))
  
} else if(Ed == "old"){
  
  NonEuCountries <- FigaroNonEuCountries
  
}

# Final use
FinalDemandP3P5NoAgg <-tbl(conAMA, "FINALDEMAND_P3P5_NO_AGG") %>% 
  collect() %>% pull(FINAL_DEMAND_P_3_P_5_NO_AGG)

# Figaro Industries
industry64 <- tbl(conAMA, "INDUSTRY64") %>% 
  collect() %>% 
  pull(INDUSTRY_64)

# Figaro Products
product64 <- paste0("CPA_",
                    tbl(conAMA, "INDUSTRY64") %>% 
                      collect() %>% 
                      pull(INDUSTRY_64))

# Figaro gross value added
b1g <- tbl(conAMA, "B1G") %>% 
  collect() %>% 
  pull(B_1_G)

# Figaro taxes less subsidies on products
d21x31 <- tbl(conAMA, "D21X31_CODE") %>% 
  collect() %>% 
  pull(D_21_X_31_CODE)

# CLASSIFICATIONS FNAM ----------------------------------------------------
industry64fnam <- tbl(conAMA, "INDUSTRY64") %>% 
  collect() %>% 
  changeCodesFNAM(., "INDUSTRY_64", "FNAM") %>%
  pull(INDUSTRY_64)

product64fnam <- tbl(conAMA, "INDUSTRY64") %>% 
  collect() %>% 
  mutate(INDUSTRY_64 = paste0("CPA_", INDUSTRY_64)) %>% 
  changeCodesFNAM(., "INDUSTRY_64", "FNAM") %>%
  pull(INDUSTRY_64)

# FNAM capital accounts
NsFnam <- c("N111G",
            "N112G",
            "N1131G",
            "N1132G",
            "N115G",
            "N1171G",
            "N1179G",
            "N11OG" ) 

# FNAM gross value added
b1gFnam <- c("D29X39", "D11", "D12", "B2", "B3")

# FNAM Final use
if(Ed == "old"){
  
  fuFnam <- c("P3_S13", "P3_S14", "P3_S15", "B8_S11", NsFnam)
  
} else if(Ed == "25"){
  
  fuFnam <- c("P3_S13", "P3_S14", "P3_S15", "B8_S11", "P51G")
}

fuFnamE3 <- c("P3_S13", "P3_S14", "P3_S15", "B8_S11", "P51G")

# CLASSIFICATION MODEL ----------------------------------------------------
disCpa <- tibble(FIGARO_64 = "CPA_C24",
                 FIGARO_Model = c("CPA_C241", "CPA_C24Z"))

missingCpa <- setdiff(product64fnam, disCpa$FIGARO_64)

corrCpa <- disCpa %>% 
  bind_rows(tibble(FIGARO_64 = missingCpa ,
                   FIGARO_Model = missingCpa)) %>% 
  bind_rows(tibble(FIGARO_64 = c(b1g,"OP_NRES", "OP_RES", d21x31),
                   FIGARO_Model = c(b1g,"OP_NRES", "OP_RES", d21x31))) %>% 
  arrange(match(FIGARO_64, product64fnam))

productModel <- setdiff(corrCpa$FIGARO_Model, c(b1g,"OP_NRES", "OP_RES", d21x31))

disNace <- tibble(FIGARO_64 = "C24",
                  FIGARO_Model = c("C241", "C24Z"))
  
missingNace <- setdiff(industry64fnam, disNace$FIGARO_64)

corrNace <- disNace %>% 
  bind_rows(tibble(FIGARO_64 = missingNace,
                   FIGARO_Model = missingNace)) %>% 
  bind_rows(tibble(FIGARO_64 = fuFnamE3, 
                   FIGARO_Model = fuFnamE3)) %>% 
  arrange(match(FIGARO_64, industry64fnam))

industryModel <- setdiff(corrNace$FIGARO_Model, fuFnam)

if(Ed == "25"){
  
  totalCountries <- 50
  
} else if(Ed == "old"){
  
  totalCountries <- 46
}

allCpa <- paste0(rep(c(EuCountries, NonEuCountries, "WRL_REST"), each = length(productModel)),
                 "_", rep(productModel, times = totalCountries))

allNace <- paste0(rep(c(EuCountries, NonEuCountries, "WRL_REST"), each = length(industryModel)),
                  "_", rep(industryModel, times = totalCountries))

allFu <- paste0(rep(c(EuCountries, NonEuCountries, "WRL_REST"), each = length(fuFnam)),
                "_", rep(fuFnam, times = totalCountries))

allVa <- paste0(rep(c(EuCountries, NonEuCountries, "WRL_REST"), each = 6),
                "_", c(d21x31, b1gFnam))

# CORRESPONCE WITH FIGARO E3 ---------------------------------------------
# Nace correspondence between FIGARO and FIGARO-E3
corrE3Nace <- tbl(conAMA, "CORRESP_NACE_FIGARO_FIGAROE3") %>%
  collect() %>% 
  mutate(COL_PI = if_else(COL_PI != c("C24" ), COL_PI, COL_PI_E_3)) %>% 
  mutate(COL_PI = case_when(
    COL_PI_E_3 == "C24_A" ~ "C241",
    .default = COL_PI)) %>% 
  mutate(COL_PI = case_when(
    (substr(COL_PI_E_3,1,3)) == "C24" & (!COL_PI_E_3  %in% c("C24_A")) ~ "C24Z",
    .default = COL_PI)) # %>% 
  # mutate(COL_PI = case_when(
  #   COL_PI_E_3 == "E37T39_A" ~ "E37T39_AL",
  #   .default = COL_PI)) %>% 
  # mutate(COL_PI = case_when(
  #   (substr(COL_PI_E_3,1,6)) == "E37T39" & (!COL_PI_E_3  %in% c("E37T39_A")) ~ "E37T39Z",
  #   .default = COL_PI)) %>% 
  # mutate(COL_PI = case_when(
  #   COL_PI_E_3 == "D3511_A" ~ "D3511C",
  #   .default = COL_PI)) %>%
  # mutate(COL_PI = case_when(
  #   COL_PI_E_3 == "D3511_F" ~ "D3511P",
  #   .default = COL_PI)) %>%
  # mutate(COL_PI = case_when(
  #   COL_PI_E_3 == "D3511_B" ~ "D3511G",
  #   .default = COL_PI)) %>%
  # mutate(COL_PI = case_when(
  #   (substr(COL_PI_E_3,1,3)) == "D35" & (!COL_PI_E_3  %in% c("D3511_A",
  #                                                            "D3511_F",
  #                                                            "D3511_B",
  #                                                            "D3511_C",
  #                                                            "D3511_D",
  #                                                            "D3511_E",
  #                                                            "D3511_G",
  #                                                            "D3511_H")) ~ "D35Z",
  #   .default = COL_PI)) %>%
  # mutate(COL_PI = case_when(
  #   COL_PI_E_3 == "D3511_C" ~ "D3512N",
  #   .default = COL_PI)) %>%
  # mutate(COL_PI = case_when(
  #   COL_PI_E_3 == "D3511_D" ~ "D3512H",
  #   .default = COL_PI)) %>%
  # mutate(COL_PI = case_when(
  #   COL_PI_E_3 == "D3511_E" ~ "D3512W",
  #   .default = COL_PI)) %>%
  # mutate(COL_PI = case_when(
  #   COL_PI_E_3 == "D3511_G" ~ "D3512B",
  #   .default = COL_PI)) %>%
  # mutate(COL_PI = case_when(
  #   COL_PI_E_3 == "D3511_H" ~ "D3512S",
  #   .default = COL_PI))

# Cpa correspondence between FIGARO and FIGARO-E3
corrE3Cpa <- tbl(conAMA, "CORRESP_CPA_FIGARO_FIGAROE3") %>%
  collect() %>% 
  mutate(ROW_PI = if_else(!ROW_PI %in% c("CPA_C24"), ROW_PI, ROW_PI_E_3)) %>% 
  # mutate(ROW_PI = case_when(
  #   ROW_PI_E_3 == "CPA_B07_E" ~ "CPA_B_AL",
  #   .default = ROW_PI)) %>% 
  # mutate(ROW_PI = case_when(
  #   (substr(ROW_PI_E_3,1,5)) == "CPA_B" & (!ROW_PI_E_3  %in% c("CPA_B07_E")) ~ "CPA_BZ",
  #   .default = ROW_PI)) %>% 
  # mutate(ROW_PI = case_when(
  #   ROW_PI_E_3 == "CPA_E37T39_A" ~ "CPA_E37T39_AL",
  #   .default = ROW_PI)) %>% 
  # mutate(ROW_PI = case_when(
  #   (substr(ROW_PI_E_3,1,10)) == "CPA_E37T39" & (!ROW_PI_E_3  %in% c("CPA_E37T39_A")) ~ "CPA_E37T39Z",
  #   .default = ROW_PI)) %>% 
  mutate(ROW_PI = case_when(
    ROW_PI_E_3 == "CPA_C24_A" ~ "CPA_C241",
    .default = ROW_PI)) %>% 
  mutate(ROW_PI = case_when(
    (substr(ROW_PI_E_3,1,7)) == "CPA_C24" & (!ROW_PI_E_3  %in% c("CPA_C24_A")) ~ "CPA_C24Z",
    .default = ROW_PI)) 

corrE3Fd <- data.frame(COL_PI = FinalDemandP3P5NoAgg, 
                       COL_PI_E_3 = FinalDemandP3P5NoAgg)

corrE3B1g <- data.frame(ROW_PI = c("OP_RES", "OP_NRES", "D21X31", "D29X39", rep("D1", 3),
                                   rep("B2A3G", 4)),  
                        ROW_PI_E_3 = c("OP_RES",  "OP_NRES", "D21X31", "D29X39",
                                       "D1_LS",	"D1_MS",	"D1_HS",
                                       "B2A3G_COFC","B2A3G_RENL","B2A3G_ROYR","B2A3G_NOPS"))

