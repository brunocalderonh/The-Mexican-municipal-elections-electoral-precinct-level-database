rm(list=ls())

library(readr)
library(stringr)
library(dplyr)
library(writexl)
library(rstudioapi)
library(haven)
  
# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    
# Set the working directory to the root of the repository
#Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../../../"))

db <- read_csv("Processed Data/guerrero/guerrero_final.csv")


#### duplicate values #####
# Check for duplicates
duplicates <- db %>%
  group_by(year, section) %>%
  filter(n() > 1)

if(nrow(duplicates) > 0) {
  print("There are duplicate observations:")
  print(duplicates)
} else {
  print("No duplicates found.")
}


#### turnout values #####
# Check turnout for reasonable values
invalid_turnout <- db %>%
  mutate(turnout = total/listanominal) %>% 
  select(turnout, everything()) %>% 
  filter(turnout > 1)

if(nrow(invalid_turnout) > 0) {
  print("Turnout values above 1 (100%) detected:")
  print(invalid_turnout)
} else {
  print("All turnout values are within a reasonable range.")
}


#### NA values #####
#1.
# Columns to include after incumbent_vote
incumbent_related_columns <- c("party_component", "incumbent_party_magar", "incumbent_party_JL", "incumbent_party_Horacio", "incumbent_party_inafed")

# Filter rows with NA in incumbent_vote, excluding other _vote columns
na_incumbent_vote <- db %>%
  filter(is.na(incumbent_vote)) %>%
  select(incumbent_vote, all_of(incumbent_related_columns), -matches("_vote$"), everything())

print("Rows with NA in 'incumbent_vote':")
print(na_incumbent_vote)


#2. 
# Columns to include after state_incumbent_vote
state_incumbent_related_columns <- c("state_year", "state_incumbent_vote_party_component", "state_incumbent_party")

# Filter rows with NA in state_incumbent_vote, excluding other _vote columns
na_state_incumbent_vote <- db %>%
  filter(is.na(state_incumbent_vote)) %>%
  select(state_incumbent_vote, all_of(state_incumbent_related_columns), -matches("_vote$"), everything())

print("Rows with NA in 'state_incumbent_vote':")
print(na_state_incumbent_vote)


state_validation <- db %>% 
  select(uniqueid,year,section,state_year, state_incumbent_vote, state_incumbent_vote_party_component, state_incumbent_party,)


#3. 
# Column to include after PRI_vote
pri_related_column <- "PRI_vote_party_component"

# Filter rows with NA in PRI_vote, excluding other _vote columns
na_PRI_vote <- db %>%
  filter(is.na(PRI_vote)) %>%
  select(PRI_vote, pri_related_column, -matches("_vote$"), everything())

print("Rows with NA in 'PRI_vote':")
print(na_PRI_vote)


#4.
# Column to include after PRD_vote
prd_related_column <- "PRD_vote_party_component"

# Filter rows with NA in PRD_vote, excluding other _vote columns
na_PRD_vote <- db %>%
  filter(is.na(PRD_vote)) %>%
  select(PRD_vote, prd_related_column, -matches("_vote$"), everything())

print("Rows with NA in 'PRD_vote':")
print(na_PRD_vote)


#5.
# Column to include after PAN_vote
pan_related_column <- "PAN_vote_party_component"

# Filter rows with NA in PAN_vote, excluding other _vote columns
na_PAN_vote <- db %>%
  filter(is.na(PAN_vote)) %>%
  select(PAN_vote, pan_related_column, -matches("_vote$"), everything())

print("Rows with NA in 'PAN_vote':")
print(na_PAN_vote)


#6.
# Column to include after MORENA_vote
morena_related_column <- "MORENA_vote_party_component"

# Filter rows with NA in MORENA_vote, excluding other _vote columns
na_MORENA_vote <- db %>%
  filter(is.na(MORENA_vote)) %>%
  select(MORENA_vote, morena_related_column, -matches("_vote$"), everything())

print("Rows with NA in 'MORENA_vote':")
print(na_MORENA_vote)


#7.

# Columns to include after runnerup_vote
runnerup_related_columns <- c("runnerup_party_magar", "runnerup_party_component")

# Filter rows with NA in runnerup_vote, excluding other _vote columns
na_runnerup_vote <- db %>%
  filter(is.na(runnerup_vote)) %>%
  select(runnerup_vote, all_of(runnerup_related_columns), -matches("_vote$"), everything())

print("Rows with NA in 'runnerup_vote':")
print(na_runnerup_vote)
#### Coalitions ####
coalition_parties <- list(
  PRI_PT_PVEM = c("PRI", "PT", "PVEM"),
  PRI_PVEM = c("PRI", "PVEM"),
  PAN_PANAL = c("PAN", "PANAL"),
  PRI_PVEM_PANAL = c("PRI", "PVEM", "PANAL"),
  PRD_PC = c("PRD", "PC"),
  PAN_PRD = c("PAN", "PRD"),
  PRI_PT_PANAL = c("PRI", "PT", "PANAL")
)

#### Validations Coalitions ####
validate_coalitions <- function(data, coalition_name, individual_parties) {
  issue_rows <- data %>%
    group_by(year, section) %>%
    filter(!is.na(!!sym(coalition_name)) & !!sym(coalition_name) != 0) %>%
    filter(rowSums(across(all_of(individual_parties), ~ . != 0)) > 0) %>%
    ungroup()
  
  if (nrow(issue_rows) > 0) {
    cat(paste("Validation issue: Coalition", coalition_name, "has non-zero values, but individual party columns are not zero in the same year and section.\n"))
    print(issue_rows)
  } else {
    cat(paste("Validation passed for coalition:", coalition_name, "\n"))
  }
}
# Apply validation function for each coalition
for (coalition in names(coalition_parties)) {
  validate_coalitions(db, coalition, coalition_parties[[coalition]])
}

# 1. Check consistency in the number of unique precincts across years
precinct_counts <- db %>%
  group_by(year) %>%
  summarize(unique_sections = n_distinct(section)) %>%
  ungroup()

# Find years with inconsistent precinct counts
expected_precincts <- mode(precinct_counts$unique_sections) # Mode gives the most frequent count

inconsistent_precincts <- precinct_counts %>%
  filter(unique_sections != expected_precincts)

inconsistent_precincts


# 1. Check consistency in the number of unique municipalities across years
municipality_counts <- db %>%
  group_by(year) %>%
  summarize(unique_municipalities = n_distinct(uniqueid)) %>%
  ungroup()

municipality_counts




