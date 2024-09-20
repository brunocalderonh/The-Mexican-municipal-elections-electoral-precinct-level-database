rm(list=ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)

# Load the data
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/aguascalientes/Aguascalientes_updated/")

db <- read_dta("Aguascalientes_ALL_SALVADOR.dta")

db <- db %>%
  select(-matches("^(.*STATE|.*winner_counter|.*winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "AGUASCALIENTES") %>%
  select(mun, state, uniqueid, section, year, everything()) 

write_dta(db,"/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/aguascalientes/aguascalientes_vote.dta")
