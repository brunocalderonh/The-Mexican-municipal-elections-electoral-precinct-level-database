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
db <- read_csv("Data/vote data/sinaloa_vote_all.csv")


db <- db %>%
  select(-matches("^(no_reg|.*nulo|.*coalition|.*incumbent|.*winner_counter|.*STATE|.*.winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

# Reorder columns
db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "SINALOA") %>%
  select(mun, state, uniqueid, section, year, everything())


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/sinaloa")
output_path <- file.path(output_dir, "sinaloa_vote.csv")

# Use write_csv to save the file
write_csv(db, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)