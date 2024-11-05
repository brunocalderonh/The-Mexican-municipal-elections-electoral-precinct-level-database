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

finaldb <- read_csv("Processed Data/queretaro/queretaro_incumbent_manipulator.csv")

finaldb <- finaldb %>%
  select(state,mun,section,uniqueid,year,incumbent_party_magar,incumbent_candidate_magar,incumbent_party_Horacio,incumbent_party_JL,incumbent_party_inafed, incumbent_candidate_inafed, runnerup_party_magar, runnerup_candidate_magar, margin,everything())
replace_parties <- function(party_str) {
  replacements <- c( "PNA" = "PANAL", 
                    "CONVE" = "PC",
                    "PD1" ="PD",
                    "INDEP" = "CI_1")
  
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
           party_component = NA)
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    incumbent_party <- data$incumbent_party_magar[I]
    
    # Skip if incumbent_party is NA or empty
    if (is.na(incumbent_party) || incumbent_party == "") next
    
    # Check if it is a coalition
    if (str_detect(incumbent_party, "_")) {
      parties <- unlist(str_split(incumbent_party, "_"))
      
      # Check if any individual party within the coalition is present in other columns
      individual_party_found <- FALSE
      for (party in parties) {
        if (party %in% data$incumbent_party_JL[I] || 
            party %in% data$incumbent_party_Horacio[I] || 
            party %in% data$incumbent_party_inafed[I]) {
          individual_party_found <- TRUE
          party_vars <- names(data)[str_detect(names(data), paste0("\\b", party, "\\b"))]
          
          for (party_var in party_vars) {
            if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
              data$incumbent_vote[I] <- data[[party_var]][I]
              data$party_component[I] <- party_var
              break
            }
          }
          if (!is.na(data$incumbent_vote[I])) break
        }
      }
      
      # Proceed with coalition logic if no individual party is found
      if (!individual_party_found) {
        coalition_vars <- names(data)[sapply(names(data), function(x) all(parties %in% str_split(x, "_")[[1]]))]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$incumbent_vote[I] <- data[[coalition_var]][I]
            data$party_component[I] <- coalition_var
            break
          }
        }
      }
    } else {
      # Handle single parties
      party_vars <- names(data)[str_detect(names(data), paste0("\\b", incumbent_party, "\\b"))]
      
      for (party_var in party_vars) {
        # Ensure PAN is not confused with PANAL by using word boundaries
        if (str_detect(party_var, "\\bPAN\\b")) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$incumbent_vote[I] <- data[[party_var]][I]
            data$party_component[I] <- party_var
            break
          }
        } else if (!str_detect(party_var, "\\bPANAL\\b") && !str_detect(party_var, "\\bPAN\\b")) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$incumbent_vote[I] <- data[[party_var]][I]
            data$party_component[I] <- party_var
            break
          }
        }
      }
      
      # If no single party found, check coalitions containing the single party
      if (is.na(data$incumbent_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) incumbent_party %in% str_split(x, "_")[[1]])]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$incumbent_vote[I] <- data[[coalition_var]][I]
            data$party_component[I] <- coalition_var
            break
          }
        }
      }
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
    incumbent_vote,
    party_component,
    mutually_exclusive,
    incumbent_party_JL, 
    incumbent_candidate_JL,
    incumbent_party_Horacio, 
    incumbent_party_inafed, 
    incumbent_candidate_inafed,
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
