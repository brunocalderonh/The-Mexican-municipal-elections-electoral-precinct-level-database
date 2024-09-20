rm(list=ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)

# Load the data
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/coahuila/coahuila_updated/")

db <- read_dta("coahuila_ALL_SALVADOR.dta")

db <- db %>%
  select(-matches("^(coalition|.*incumbent|.*winner_counter|.*STATE|.*.winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))


db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "COAHUILA") %>%
  select(mun, state, uniqueid, section, year, everything())


write_dta(db,"/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/coahuila/coahuila_vote.dta")
