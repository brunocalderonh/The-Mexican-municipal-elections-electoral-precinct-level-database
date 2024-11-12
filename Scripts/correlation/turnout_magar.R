rm(list = ls())

library(tidyr)
library(dplyr)
library(readr)  # Use readr for read_csv
library(openxlsx)
library(purrr)
library(readxl)
library(rstudioapi)

# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../../"))

# Read the CSV files into individual data frames
db70 <- read_csv("Correlation Data/turnout_magar/magar_1970.csv")
db80 <- read_csv("Correlation Data/turnout_magar/magar_1980.csv")
db90 <- read_csv("Correlation Data/turnout_magar/magar_1990.csv")
db00 <- read_csv("Correlation Data/turnout_magar/magar_2000.csv")
db10 <- read_csv("Correlation Data/turnout_magar/magar_2010.csv")
db20 <- read_csv("Correlation Data/turnout_magar/magar_2020.csv")


# Function to convert columns starting with "l" to character and "v" to numeric
convert_columns <- function(df) {
  df %>% 
    # Convert columns starting with "l" followed by numbers to character
    mutate(across(starts_with("l"), as.character)) %>% 
    # Convert columns starting with "v" followed by numbers to numeric
    mutate(across(starts_with("v"), as.numeric))
}

# Apply the conversion function to all data frames
db70 <- convert_columns(db70)
db80 <- convert_columns(db80)
db90 <- convert_columns(db90)
db00 <- convert_columns(db00)
db10 <- convert_columns(db10)
db20 <- convert_columns(db20)

# Combine all databases into a single data frame called `db`
db <- bind_rows(db70, db80, db90, db00, db10, db20) %>% 
  rename(
    "uniqueid" = "inegi",
    "year" = "yr",
    "listanominal" = "lisnom",
    "total" = "efec"
  ) %>% 
  select(-emm,-ife,-edon,-date,-ncand,-ncoal,-dpre1997,-dextra) %>% 
  mutate(turnout = total/as.numeric(listanominal)) %>% 
  # mutate(total = rowSums(select(., starts_with("v")), na.rm = TRUE)) %>% 
  select( year, uniqueid, mun,turnout, total, listanominal,everything())

#####

# Pivot the vote and party columns to long format
db_party_votes <- db %>%
  # Ensure year, uniqueid, mun, turnout, total, listanominal are retained
  select(year, uniqueid, mun, turnout, total, listanominal, starts_with("v"), starts_with("l")) %>%
  
  # Pivot the vote columns to long format
  pivot_longer(
    cols = starts_with("v"),  # Match vote column names
    names_to = "vote_index", 
    names_prefix = "v", 
    values_to = "votes",
    values_transform = list(votes = as.numeric)
  ) %>%
  
  # Pivot the party columns to long format
  pivot_longer(
    cols = starts_with("l"),  # Match party column names
    names_to = "party_index", 
    names_prefix = "l", 
    values_to = "party"
  ) %>%
  
  # Ensure vote and party indices match (v01 matches l01, etc.)
  filter(vote_index == party_index) %>%
  
  # Remove invalid or missing party entries
  filter(party != "0")   

# Step 1: Split party coalitions into individual parties
db_party_votes <- db_party_votes %>%
  # Separate party coalitions into individual parties by "-"
  separate_rows(party, sep = "-")

# Step 2: Pivot wider and allocate votes to parties
db_party_wide <- db_party_votes %>%
  filter(party %in% c("pri", "morena", "prd", "pan")) %>%  # Only keep the relevant parties
  pivot_wider(
    names_from = party,        # Create a column for each party
    values_from = votes,       # Fill with vote values
    values_fill = 0            # Fill missing values with 0
  )

db_party_wide <- db_party_wide %>%
  # Step 3: Group by year, uniqueid, and mun to aggregate duplicate rows
  group_by(year, uniqueid, mun) %>%
  summarize(
    turnout = first(turnout),       # Take the first value of turnout
    total = first(total),           # Take the first value of total
    pri = sum(pri, na.rm = TRUE),     # Sum votes for pri
    pan = sum(pan, na.rm = TRUE),     # Sum votes for pan
    morena = sum(morena, na.rm = TRUE), # Sum votes for morena
    prd = sum(prd, na.rm = TRUE)        # Sum votes for prd
  ) %>%
  
  # Step 4: Ungroup to finalize the data frame
  ungroup()

# Perform a left join to bring in the listanominal column from the original db
db_party_wide <- db_party_wide %>%
  left_join(select(db, uniqueid, year, listanominal), by = c("uniqueid", "year"))

# Step 3: Combine rows with the same uniqueid and year, summing votes
db_final <- db_party_wide %>%
  group_by(uniqueid, year, total, mun, turnout, listanominal) %>%
  summarise(
    pri = sum(pri, na.rm = TRUE),       # Sum votes for 'pri'
    morena = sum(morena, na.rm = TRUE), # Sum votes for 'morena'
    prd = sum(prd, na.rm = TRUE),       # Sum votes for 'prd'
    pan = sum(pan, na.rm = TRUE),       # Sum votes for 'pan'
    .groups = "drop"
  ) %>%
  rename(magar_turnout = turnout) #%>%   # Rename turnout to magar_turnout
  #filter(magar_turnout <= 1)            # Filter for magar_turnout <= 1

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Correlation Data/generated_data")
output_path <- file.path(output_dir, "magar_turnout.csv")

# Use write_csv to save the file
write_csv(db_final, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)


