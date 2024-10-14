rm(list=ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)

# Load the data
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/chihuahua/chihuahua_updated/")

db <- read_dta("chihuahua_ALL_SALVADOR.dta")
extra_correction <- read.csv("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/correct_extra_elec_final.csv")

extra_correction <- extra_correction %>%
  select(-X)

db <- db %>%
  select(-matches("^(coalition|.*incumbent|.*winner_counter|.*STATE|.*.winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

# Reorder columns
db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "CHIHUAHUA") %>%
  select(mun, state, uniqueid, section, year, everything())


db <- db %>%
  anti_join(extra_correction, by = c("section","mun","year", "uniqueid"))



write_dta(db,"/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/chihuahua/chihuahua_vote.dta")
