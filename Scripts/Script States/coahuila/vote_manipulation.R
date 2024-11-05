rm(list = ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)
library(rstudioapi)
library(readr)

# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../../../"))

# Now set the path to the CSV file relative to the root of the repository
db <- read_csv("Processed Data/coahuila/coahuila_process_raw_data.csv")
# Perform your data transformations
db <- db %>%
  select(-matches("^(.*STATE|.*winner_counter|.*winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

db <- db %>%
  rename(mun = municipality) %>%
  mutate(state = "COAHUILA") %>%
  select(mun, state, uniqueid, section, year, everything())

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/coahuila")
output_path <- file.path(output_dir, "coahuila_vote_manipulation.csv")

# Use write_csv to save the file
write_csv(db, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)