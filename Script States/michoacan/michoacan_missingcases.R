# Clear the environment
rm(list = ls())

# Load necessary libraries
library(readxl)
library(dplyr)

# Set working directory
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/")

db1 <- read_excel("michoacan/michoacan_collapsed.xlsx")
db2 <- read_excel("michoacan/michoacan_collapsed_edited_wrong_state.xlsx")


db1 <- db1 %>%
  left_join(db2 %>% select(uniqueid, year, state, mun, incumbent_party_magar, incumbent_candidate_magar, researched_incumbent, source_researched_incumbent),
            by = c("uniqueid", "year", "state", "mun", "incumbent_party_magar", "incumbent_candidate_magar")) %>%
  mutate(researched_incumbent = if_else(is.na(researched_incumbent.x), researched_incumbent.y, researched_incumbent.x),
         source_researched_incumbent = if_else(is.na(source_researched_incumbent.x), source_researched_incumbent.y, source_researched_incumbent.x)) %>%
  select(-researched_incumbent.x, -researched_incumbent.y, -source_researched_incumbent.x, -source_researched_incumbent.y)


db1 <- db1 %>%
  select(uniqueid,
         year,
         state, 
         mun, 
         incumbent_party_magar,
         incumbent_candidate_magar,
         incumbent_vote,
         researched_incumbent,
         source_researched_incumbent,
         incumbent_party_JL,
         incumbent_candidate_JL,
         incumbent_party_Horacio,
         incumbent_party_inafed,
         incumbent_candidate_inafed,
         state_year,
         state_incumbent_party,
         state_incumbent_candidate,
         PRI_vote,
         PRI_vote_party_component,
         everything())

write_xlsx(db1, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/michoacan/michoacan_collapsed_edited.xlsx")
