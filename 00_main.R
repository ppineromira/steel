# DISAGREGATION SUT -------------------------------------------------------
# Packages
pacman::p_load(tidyverse, data.table, arrow, ROracle, DBI, readxl, gamstransfer, countrycode)
options(dplyr.summarise.inform = FALSE)

# Functions
rFunctions <- list.files(path = "functions/", pattern = "\\.R$", full.names = TRUE)
lapply(rFunctions, source)

# Decide Edition
# Ed <- "25"
Ed <- "old"

# DB and proxy configuration
source("config/config.R")

# PROCESS -----------------------------------------------------------------
# Load classifications
source("scripts/dissagregation/01_classifications.R")

# Aggregate FIGARO-E3 when needed (rows and columns not dissagregated)
source("scripts/dissagregation/02_processE3.R")

# Call SUTs from FNAM (define year)
tPeriod <- 2017
source("scripts/dissagregation/03_callSUTs.R")

# Create prior
adjustNeg <- FALSE
source("scripts/dissagregation/04_createPrior.R")

# Balance dissagregated SUT
checkCondition <- FALSE 
source("scripts/dissagregation/05_balance.R")

# Estimate aggregates for trade
source("scripts/dissagregation/06_aggregatesTrade.R")

# Estimate investment matrix
source("scripts/dissagregation/07_investments.R")

export <- TRUE
source("scripts/dissagregation/08_export.R")

# Disconnect database
DBI::dbDisconnect(conAMA)
DBI::dbDisconnect(conFIG)