rm(list=ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)

# Load the data
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/bajasur/bajasur_updated/")

db <- read_dta("bajasur_ALL_SALVADOR.dta")

# db <- db %>%
#   select(uniqueid, section, year, winner, PAN, PRI, PRD_PT, PVEM, MRPS, PPS, PSN, PC,PAS,PRS,PRI_PVEM, PRD_PC,PT,PAN_PVEM, PRI_PMRPS,PRD_PT_PC,PMRPS,PANAL,PAN_PRS,PRD_PT_MC,MORENA,PH,PES,CI_1,PAN_PRD_PH_PRS,MC,BCSC,MORENA_PES)
# Select and remove unwanted variables
db <- db %>%
  select(-matches("^(coalition|.*incumbent|.*winner_counter|.*STATE|.*.winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

# Reorder columns
# Reorder columns
db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "BAJA CALIFORNIA SUR") %>%
  select(mun, state, uniqueid, section, year, everything()) 


write_dta(db,"/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/bajasur/bajasur_vote.dta")
 