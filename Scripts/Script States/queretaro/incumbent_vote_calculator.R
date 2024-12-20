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

finaldb <- read_csv("Processed Data/queretaro/queretaro_incumbent_manipulator.csv")

finaldb <- finaldb %>%
  select(state,mun,section,uniqueid,year,incumbent_party_magar,incumbent_candidate_magar,incumbent_party_JL, runnerup_party_magar, runnerup_candidate_magar, margin,everything())
replace_parties <- function(party_str) {
  replacements <- c( "PNA" = "PANAL", 
                    "CONVE" = "PC",
                    "PD1" ="PD",
                    "INDEP" = "CI_1",
                    "PFCRN" = "PartCardenista",
                    "PASD" = "PAS")
  
  for (replacement in names(replacements)) {
    party_str <- str_replace_all(party_str, replacements[replacement], replacement)
  }
  
  return(party_str)
}

# Apply the replacement function to the incumbent_party_magar column
finaldb <- finaldb %>%
  mutate(incumbent_party_magar = sapply(incumbent_party_magar, replace_parties)) %>%
  mutate(runnerup_party_magar = sapply(runnerup_party_magar, replace_parties))
  
assign_incumbent_vote <- function(data) {
  
  # Initialize columns
  data <- data %>%
    mutate(incumbent_vote = NA,
           party_component = NA,
           final_incumbent = NA)  # Add final_incumbent column
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    incumbent_party <- data$incumbent_party_magar[I]
    final_incumbent_value <- NA  # Initialize tracker for final_incumbent
    
    # Handle cases where all incumbent_party_ variables are NA
    if (is.na(incumbent_party) || incumbent_party == "") {
      all_incumbent_parties <- data %>%
        select(starts_with("incumbent_party_")) %>%
        filter(row_number() == I) %>%
        unlist(use.names = FALSE) %>%
        na.omit() %>%
        unique()
      
      if (length(all_incumbent_parties) > 0) {
        incumbent_party <- all_incumbent_parties[1]
        final_incumbent_value <- all_incumbent_parties[1]
      } else {
        next  # Skip if no valid incumbent_party values are found
      }
    }
    
    # Check if there's a mix of coalitions and single parties
    all_parties <- data %>%
      select(starts_with("incumbent_party_")) %>%
      filter(row_number() == I) %>%
      unlist(use.names = FALSE) %>%
      na.omit() %>%
      unique()
    
    coalition_parties <- all_parties[str_detect(all_parties, "_")]
    single_parties <- all_parties[!str_detect(all_parties, "_")]
    
    if (length(coalition_parties) > 0 && length(single_parties) > 0) {
      for (coalition in coalition_parties) {
        coalition_components <- unlist(str_split(coalition, "_"))
        for (single_party in single_parties) {
          if (single_party %in% coalition_components) {
            incumbent_party <- single_party
            final_incumbent_value <- single_party
            break
          }
        }
        if (incumbent_party %in% single_parties) break
      }
    }
    
    # Handle coalition parties
    if (str_detect(incumbent_party, "_")) {
      parties <- unlist(str_split(incumbent_party, "_"))
      
      coalition_vars <- names(data)[sapply(names(data), function(x) {
        party_components <- unlist(str_split(x, "_"))
        all(parties %in% party_components)
      })]
      
      valid_found <- FALSE
      for (var in coalition_vars) {
        if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
          data$incumbent_vote[I] <- data[[var]][I]
          data$party_component[I] <- var
          final_incumbent_value <- incumbent_party
          valid_found <- TRUE
          break
        }
      }
      
      if (!valid_found) next
    } else {
      # Handle single parties
      party <- incumbent_party
      
      if (party == "PAN") {
        party_regex <- "(^PAN$|_PAN$|^PAN_)"
      } else if (party == "PANAL") {
        party_regex <- "(^PANAL$|_PANAL$|^PANAL_)"
      } else {
        party_regex <- paste0("(^|_)", party, "($|_)")
      }
      
      candidate_vars <- names(data)[grepl(party_regex, names(data))]
      
      valid_found <- FALSE
      for (var in candidate_vars) {
        if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
          data$incumbent_vote[I] <- data[[var]][I]
          data$party_component[I] <- var
          final_incumbent_value <- party
          valid_found <- TRUE
          break
        }
      }
      
      if (!valid_found) {
        broader_coalition_vars <- names(data)[sapply(names(data), function(x) {
          party_components <- unlist(str_split(x, "_"))
          party %in% party_components
        })]
        
        for (var in broader_coalition_vars) {
          if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
            data$incumbent_vote[I] <- data[[var]][I]
            data$party_component[I] <- var
            final_incumbent_value <- party
            break
          }
        }
      }
    }
    
    # Assign the final_incumbent value
    if (!is.na(data$incumbent_vote[I])) {
      data$final_incumbent[I] <- final_incumbent_value
    }
  }
  
  return(data)
}


finaldb <- assign_incumbent_vote(finaldb)

assign_runnerup_vote <- function(data) {
  
  # Initialize columns for storing results
  data <- data %>%
    mutate(runnerup_vote = NA,
           runnerup_party_component = NA)
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    runnerup_party <- data$runnerup_party_magar[I]
    
    # Skip processing for NA or empty runnerup_party
    if (is.na(runnerup_party) || runnerup_party == "") next
    
    # List to collect all relevant columns
    candidate_vars <- c()
    
    # Single party: Search directly and within coalitions
    party_regex <- paste0("(^|_)", runnerup_party, "($|_)") # Regex to capture exact party within coalitions
    candidate_vars <- names(data)[grepl(party_regex, names(data))]
    
    # Check each potential column for a valid vote
    valid_found <- FALSE
    for (var in candidate_vars) {
      if (!is.na(data[[var]][I]) && data[[var]][I] != 0 && data[[var]][I] != "") {
        data$runnerup_vote[I] <- data[[var]][I]
        data$runnerup_party_component[I] <- var
        valid_found <- TRUE
        break
      }
    }
    
    # If no valid entry is found in direct or coalition matches, check for broader coalitions
    if (!valid_found && !str_detect(runnerup_party, "_")) {
      broader_coalition_vars <- names(data)[grepl(runnerup_party, names(data)) & grepl("_", names(data))]
      for (var in broader_coalition_vars) {
        if (!is.na(data[[var]][I]) && data[[var]][I] != 0 && data[[var]][I] != "") {
          data$runnerup_vote[I] <- data[[var]][I]
          data$runnerup_party_component[I] <- var
          break
        }
      }
    }
  }
  
  return(data)
}
finaldb <- assign_runnerup_vote(finaldb)

check_mutual_exclusivity <- function(data) {
  
  # Initialize mutually_exclusive column
  data <- data %>%
    mutate(mutually_exclusive = NA)
  
  # Loop through each row of the data
  for (i in 1:nrow(data)) {
    incumbent_party <- data$incumbent_party_magar[i]
    
    # Skip if incumbent_party is NA or empty
    if (is.na(incumbent_party) || incumbent_party == "") {
      data$mutually_exclusive[i] <- TRUE
      next
    }
    
    # Initialize a flag for mutual exclusivity
    is_exclusive <- TRUE
    
    # Check if it is a coalition
    if (str_detect(incumbent_party, "_")) {
      parties <- unlist(str_split(incumbent_party, "_"))
      
      # Initialize counters for total non-NA and non-zero values
      individual_non_na <- 0
      coalition_non_na <- 0
      
      # Check each party in the coalition
      for (party in parties) {
        party_vars <- names(data)[str_detect(names(data), paste0("\\b", party, "\\b"))]
        party_values <- data[i, party_vars, drop = TRUE]
        
        individual_non_na <- individual_non_na + sum(!is.na(party_values) & party_values != 0)
      }
      
      # Check the coalition column itself
      coalition_var <- names(data)[str_detect(names(data), paste0("\\b", incumbent_party, "\\b"))]
      if (length(coalition_var) > 0) {
        coalition_value <- data[i, coalition_var, drop = TRUE]
        coalition_non_na <- sum(!is.na(coalition_value) & coalition_value != 0)
      }
      
      # Check if both individual and coalition columns have non-zero values
      if (individual_non_na > 0 && coalition_non_na > 0) {
        is_exclusive <- FALSE
      }
      
    } else {
      # Handle single parties
      party_vars <- names(data)[str_detect(names(data), paste0("\\b", incumbent_party, "\\b"))]
      party_values <- data[i, party_vars, drop = TRUE]
      
      # Check if there are more than one non-NA and non-zero values
      if (sum(!is.na(party_values) & party_values != 0) > 1) {
        is_exclusive <- FALSE
      }
    }
    
    data$mutually_exclusive[i] <- is_exclusive
  }
  
  return(data)
}
finaldb <- check_mutual_exclusivity(finaldb)

finaldb <- finaldb %>%
  select(
    state,
    mun,
    section,
    uniqueid, 
    year, 
    incumbent_party_magar,
    incumbent_candidate_magar,
    final_incumbent,
    incumbent_vote,
    party_component,
    mutually_exclusive,
    incumbent_party_JL, 
    incumbent_candidate_JL,
    runnerup_party_magar,
    runnerup_candidate_magar,
    runnerup_vote ,
    runnerup_party_component,
    margin,
    listanominal,
    valid,
    total,
    everything()
  )

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/queretaro")
output_path <- file.path(output_dir, "queretaro_vote_calculator.csv")

# Use write_csv to save the file
write_csv(finaldb, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)
