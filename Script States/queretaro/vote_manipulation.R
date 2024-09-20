rm(list=ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)

# Load the data
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/queretaro/queretaro_updated/")

db <- read_dta("queretaro_ALL_SALVADOR.dta")

# Select and remove unwanted variables
db <- db %>%
  select(-matches("^(no_reg|.*nulo|.*coalition|.*incumbent|.*winner_counter|.*STATE|.*.winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

# Reorder columns
db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "QUERETARO") %>%
  select(mun, state, uniqueid, section, year, everything())%>%
  mutate(across(where(is.character), ~ iconv(., from = "", to = "UTF-8")))



write_xlsx(db,"/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/queretaro/queretaro_vote.xlsx")
