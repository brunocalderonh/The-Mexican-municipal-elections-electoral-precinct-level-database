rm(list=ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)
library(rstudioapi)

# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../../../"))

# Now set the path to the CSV file relative to the root of the repository
db <- read_csv("Data/vote data/aguascalientes_vote_all.csv")


db <- db %>%
  select(-matches("^(.*STATE|.*winner_counter|.*winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "AGUASCALIENTES") %>%
  select(mun, state, uniqueid, section, year, everything()) 

# Construct the path to save the CSV file in the desired directory
output_path <- file.path(script_dir, "../../Processed Data/aguascalientes/aguascalientes_vote.csv")

# Use write_csv to save the file
write_csv(db, output_path)