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
db <- read_csv("Processed Data/chiapas/chiapas_process_raw_data.csv")

extra_correction <- read.csv("Data/extraordinary elections/correct_extra_elec_final.csv")

extra_correction <- extra_correction %>%
  select(-X)

db <- db %>%
  select(-matches("^(coalition|.*incumbent|.*winner_counter|.*STATE|.*.winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

# Reorder columns
db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "CHIAPAS") %>%
  select(mun, state, uniqueid, section, year, everything()) 


db <- db %>%
  anti_join(extra_correction, by = c("section","mun","year", "uniqueid"))


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/chiapas")
output_path <- file.path(output_dir, "chiapas_vote_manipulation.csv")

# Use write_csv to save the file
write_csv(db, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)
