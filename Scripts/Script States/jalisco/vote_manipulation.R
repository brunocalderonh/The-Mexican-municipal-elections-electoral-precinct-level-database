rm(list=ls())

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
db <- read_csv("Processed Data/jalisco/jalisco_process_raw_data.csv", 
col_types = cols(ASC = col_double(), PVEM_MC = col_character()))


extra_same_yr <- read_csv("Data/extraordinary elections/diff_year_extra_elec.csv")
extra_flag <- read_csv("Data/extraordinary elections/diff_year_extra_elec_flag.csv")

extra_correction <- read.csv("Data/extraordinary elections/correct_extra_elec_final.csv")



extra_correction <- extra_correction %>%
  select(-X)


# Select and remove unwanted variables
db <- db %>%
  select(-matches("^(coalition|.*incumbent|.*winner_counter|.*STATE|.*.winner_|.*mun_|.*_winner.*|.*first.*|.*second.*|.*third.*|.*turnout.*|.*month.*)"))

# Reorder columns
db <- db %>%
  rename(mun = municipality ) %>%
  mutate(state = "JALISCO") %>%
  select(mun, state, uniqueid, section, year, everything())%>%
  mutate(across(where(is.character), ~ iconv(., from = "", to = "UTF-8")))

db <- db %>%
  anti_join(extra_correction, by = c("section","mun","year", "uniqueid"))
db <- db %>%
  anti_join(extra_same_yr , by = c("section","year", "uniqueid"))

# Add the `extra` column to flag matches
db <- db %>%
  left_join(extra_flag %>% select(section, year, uniqueid) %>% mutate(extra = 1), 
            by = c("section", "year", "uniqueid")) %>%
  mutate(extra = ifelse(is.na(extra), 0, extra))


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/jalisco")
output_path <- file.path(output_dir, "jalisco_vote_manipulation.csv")

# Use write_csv to save the file
write_csv(db, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)



# na_summary <- db %>%
#   select(-c(mun, state, uniqueid, section, valid, winner, listanominal, total, CI_1, CI_2, nulo, CI_3, CI_4, CI_5)) %>%
#   group_by(year) %>%
#   summarize(across(everything(), ~ sum(is.na(.)), .names = "na_{col}"))

