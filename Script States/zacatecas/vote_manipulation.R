rm(list=ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)

# Load the data
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/zacatecas/zacatecas_updated/")

db <- read_dta("zacatecas_ALL_SALVADOR.dta")
extra_correction <- read.csv("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/correct_extra_elec.csv")
extra_correction <- extra_correction %>%
  select(-X)


# Select and remove unwanted variables
db <- db %>%
  select(-matches("^(no_reg|.*nulo|.*coalition|.*incumbent|.*winner_counter|.*STATE|.*.winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

# Reorder columns
db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "ZACATECAS") %>%
  select(mun, state, uniqueid, section, year, everything())%>%
  mutate(across(where(is.character), ~ iconv(., from = "", to = "UTF-8")))

db <- db %>%
  anti_join(extra_correction, by = c("section","mun","year", "uniqueid"))

write_dta(db,"/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/zacatecas/zacatecas_vote.dta")
