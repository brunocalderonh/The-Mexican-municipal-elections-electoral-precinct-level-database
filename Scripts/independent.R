rm(list = ls())

library(tidyr)
library(dplyr)
library(readr)  # Use readr for read_csv
library(openxlsx)
library(purrr)
library(readxl)
library(rstudioapi)
library(readr)
library(stringr)
library(ggplot2)


# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../"))

# Path to the .zip file
zip_file <- "Final Data/all_states_final.zip"

# Unzip the file to a temporary directory
temp_dir <- tempdir()  # Create a temporary directory
unzip(zip_file, exdir = temp_dir)  # Extract the contents to the temp directory

# Find the CSV file within the temp directory
unzipped_file <- file.path(temp_dir, "all_states_final.csv")

# Now read the unzipped CSV file from the temporary directory

#final db
db <- read_csv(unzipped_file)  # Use read_csv for CSV files


replace1 <- function(party_str) {
  if (!is.na(party_str)) {
    if (party_str == "INDEPENDIENTE") {
      party_str <- "INDEP"
    } 
  }
  return(party_str)  # Ensure the function returns the modified value
}

db <- db%>%
  mutate(state_incumbent_party = sapply(state_incumbent_party, replace1))

replace2 <- function(party_str) {
  if (!is.na(party_str)) {
    if (party_str == "CI_1") {
      party_str <- "INDEP"
    } else if (party_str == "CI") {
      party_str <- "INDEP"
    } else if (party_str == "CI_") {
      party_str <- "INDEP"
    } 
  }
  return(party_str)  # Ensure the function returns the modified value
}

db <- db%>%
  mutate(incumbent_party = sapply(incumbent_party , replace2),
         runnerup_party = sapply(runnerup_party , replace2))

# Create the new column INDEP by summing all columns that start with "CI_"
db <- db %>%
  mutate(INDEP = rowSums(select(., starts_with("CI_")), na.rm = TRUE)) %>%
  select(-starts_with("CI_")) # Remove all columns that start with "CI_"

# #Showing there are no repeated independent candidacies
# # Filter for cases where incumbent_party is CI_1
# db_indep_incumbent <- db %>% 
#   filter(incumbent_party == "CI_1") %>% 
#     group_by(uniqueid) %>% 
#   summarise(count = n_distinct(year))
# 
#   # Filter for cases where incumbent_party is CI_1
#   db_indep_runnerup_ <- db %>% 
#     filter(runnerup_party == "CI_1") %>% 
#     group_by(uniqueid) %>% 
#     summarise(count = n_distinct(year))

  
# Directly assign INDEP to vote columns based on conditions
db <- db %>%
  mutate(
    incumbent_vote = if_else(incumbent_party == "INDEP", INDEP, incumbent_vote),
    runnerup_vote = if_else(runnerup_party == "INDEP", INDEP, runnerup_vote),
    state_incumbent_vote = if_else(state_incumbent_party == "INDEP", INDEP, state_incumbent_vote)
  )
  

