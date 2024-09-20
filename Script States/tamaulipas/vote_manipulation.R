rm(list=ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)

# Load the data
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/tamaulipas/tamaulipas_updated/")

db <- read_dta("tamaulipas_ALL_SALVADOR.dta")

# Select and remove unwanted variables
db <- db %>%
  select(-matches("^(no_reg|.*nulo|.*coalition|.*incumbent|.*winner_counter|.*STATE|.*.winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

# Reorder columns
db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "TAMAULIPAS") %>%
  select(mun, state, uniqueid, section, year, everything())%>%
  mutate(across(where(is.character), ~ iconv(., from = "", to = "UTF-8")))


write_dta(db,"/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/tamaulipas/tamaulipas_vote.dta")
