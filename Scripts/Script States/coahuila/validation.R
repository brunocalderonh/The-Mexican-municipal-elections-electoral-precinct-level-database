rm(list=ls())

library(readr)
library(stringr)
library(tidyverse)
library(writexl)
library(rstudioapi)
library(haven)
  
# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    
# Set the working directory to the root of the repository
#Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../../../"))

db <- read_csv("Processed Data/coahuila/coahuila_final.csv")
coalitions <- read_csv("Processed Data/coalition_dic.csv")

#### duplicate values #####
# Check for duplicates
duplicates <- db %>%
  group_by(year, uniqueid, section) %>%
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
#1. incumebent & runnerup
# incumbent
incumbent_related_columns <- c("party_component", "incumbent_party_magar", "incumbent_party_JL", "incumbent_party_Horacio", "incumbent_party_inafed")

# Filter rows with NA in incumbent_vote, excluding other _vote columns, and get distinct values of incumbent_party_magar
na_incumbent_vote <- db %>%
  filter(is.na(incumbent_vote)) %>% 
  select(incumbent_vote, all_of(incumbent_related_columns), -matches("_vote$"), everything()) %>%
  group_by(uniqueid, year) %>%
  distinct(incumbent_party_magar, .keep_all = TRUE) %>%
  ungroup()

# Get distinct, non-NA values of incumbent_party_magar from na_incumbent_vote
distinct_parties <- na_incumbent_vote %>%
  filter(!is.na(incumbent_party_magar)) %>%
  distinct(incumbent_party_magar) %>%
  pull(incumbent_party_magar)

# Function to check if a coalition (string of parties) matches any column in db
check_match <- function(coalition_string, columns) {
  # Split the coalition string by "_" and sort the parties
  coalition_parts <- sort(strsplit(coalition_string, "_")[[1]])
  
  # Iterate over columns to find a match
  for (col in columns) {
    col_parts <- sort(strsplit(col, "_")[[1]])
    if (all(coalition_parts %in% col_parts) && all(col_parts %in% coalition_parts)) {
      return(TRUE)  # Match found
    }
  }
  return(FALSE)  # No match found
}

# Initialize a flag to track if all parties found a match
all_matched <- TRUE

# Check each distinct, non-NA incumbent_party_magar value against columns in db
for (party in distinct_parties) {
  if (check_match(party, colnames(db))) {
    message(paste("Match passed for:", party))
  } else {
    message(paste("No match found for:", party))
    all_matched <- FALSE  # Set flag to FALSE if a match was not found
  }
}

# Print final message based on the value of all_matched
if (all_matched) {
  message("All found a match")
} else {
  message("At least one did not find a match")
}

#runnerup

# Define columns to include after runnerup_vote
runnerup_related_columns <- c("runnerup_party_magar", "runnerup_party_component")

# Filter rows with NA in runnerup_vote, excluding other _vote columns
na_runnerup_vote <- db %>%
  filter(is.na(runnerup_vote)) %>%
  select(runnerup_vote, all_of(runnerup_related_columns), -matches("_vote$"), everything())

# Get distinct, non-NA values of runnerup_party_magar from na_runnerup_vote
distinct_runnerup_parties <- na_runnerup_vote %>%
  filter(!is.na(runnerup_party_magar)) %>%
  distinct(runnerup_party_magar) %>%
  pull(runnerup_party_magar)

# Initialize a flag to track if all parties found a match
all_matched <- TRUE

# Loop through each party in distinct_runnerup_parties
for (party in distinct_runnerup_parties) {
  if (check_match(party, colnames(db))) {
    message(paste("Match passed for:", party))
  } else {
    message(paste("No match found for:", party))
    all_matched <- FALSE  # Set flag to FALSE if a match was not found
  }
}

# Print final message based on the value of all_matched
if (all_matched) {
  message("All found a match")
} else {
  message("At least one did not find a match")
}

#2. 
# Columns to include after state_incumbent_vote
state_incumbent_related_columns <- c( "state_incumbent_vote_party_component", "state_incumbent_party")

# Filter rows with NA in state_incumbent_vote, excluding other _vote columns
na_state_incumbent_vote <- db %>%
  filter(is.na(state_incumbent_vote)) %>%
  select(state_incumbent_vote, all_of(state_incumbent_related_columns), -matches("_vote$"), everything())

print("Rows with NA in 'state_incumbent_vote':")
print(na_state_incumbent_vote)

state_validation <- db %>% 
  select(uniqueid,year,section, state_incumbent_vote, state_incumbent_vote_party_component, state_incumbent_party,)

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

#### Coalitions ####
# Transform coal data to create a list where each coalition name maps to its components
coalition_parties <- coalitions %>%
  rowwise() %>%
  mutate(components = list(na.omit(c(components_1, components_2, components_3, components_4, components_5, components_6, components_7)))) %>%
  select(coalitions, components) %>%
  deframe()

# Define the validation function
validate_coalitions <- function(data, coalition_name, individual_parties) {
  # Check if the coalition column exists in db; silently skip if not found
  if (!coalition_name %in% colnames(data)) {
    return(NULL)
  }
  
  # Filter out any individual parties that are not present in the data
  existing_parties <- individual_parties[individual_parties %in% colnames(data)]
  
  # Skip validation if none of the individual party columns exist in the data
  if (length(existing_parties) == 0) {
    return(NULL)
  }
  
  # Perform validation
  issue_rows <- data %>%
    filter(!is.na(!!sym(coalition_name)) & !!sym(coalition_name) != 0) %>%
    filter(rowSums(across(all_of(existing_parties), ~ . != 0)) > 0)
  
  if (nrow(issue_rows) > 0) {
    message(paste("Validation issue for coalition:", coalition_name))
    return(issue_rows %>% select(year, section, coalition_name, all_of(existing_parties)))
  } else {
    message(paste("Validation passed for coalition:", coalition_name))
    return(NULL)
  }
}

# Run the validation for each coalition and collect issues in a list
validation_issues <- list()
for (coalition in names(coalition_parties)) {
  test <- validate_coalitions(db, coalition, coalition_parties[[coalition]])
  if (!is.null(test)) {
    validation_issues[[coalition]] <- test
  }
}

# Check if there were any issues
if (length(validation_issues) > 0) {
  message("Validation completed with issues in some coalitions.")
  # You can inspect the validation_issues list for details
} else {
  message("All coalitions passed validation.")
}

validation_issues  <- as.data.frame(validation_issues )

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


db$PRD_UDC



