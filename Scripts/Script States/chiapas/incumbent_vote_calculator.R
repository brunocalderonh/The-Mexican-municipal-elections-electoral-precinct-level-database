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

finaldb <- read_csv("Processed Data/chiapas/chiapas_incumbent_manipulator.csv")


finaldb <- finaldb %>%
  select(state,mun,section,uniqueid,year,incumbent_party_magar,incumbent_candidate_magar,incumbent_party_Horacio,incumbent_party_JL,incumbent_party_inafed, incumbent_candidate_inafed, runnerup_party_magar, runnerup_candidate_magar, margin,everything())
replace_parties <- function(party_str) {
  replacements <- c( 
    #PPS no tenemos
    #PARM no tenemos
    # FUECIUD 2010
    "PMCH" = "MVC",
    "PNA" = "PANAL", 
    "POC" = "POP", #"POC"  Partido Orgullo Chiapas 2015 
                     "PCHU" = "PCU",
                    "INDEP" = "CI_1",
                    "CONVE" = "PC",
                    "JUSTA" = "Alianza_Justa",
                    "PFCRN" = "PartCardenista",
                    "FUECIUD" = "FC",
    "FUECIU" = "FC")
  for (replacement in names(replacements)) {
    party_str <- str_replace_all(party_str, replacements[replacement], replacement)
  }
  
  return(party_str)
}

replace1 <- function(party_str) {
  # Check for NA and replace "PRF" or "PFC" with "PartCardenista"
  if (!is.na(party_str) && (party_str == "PRF" || party_str == "PFC")) {
    party_str <- "PartCardenista"
  }
  return(party_str)
}
# Apply the replacement function to the incumbent_party_magar column

finaldb <- finaldb %>%
  mutate(incumbent_party_magar = sapply(incumbent_party_magar, replace_parties)) %>%
  mutate(runnerup_party_magar = sapply(runnerup_party_magar, replace_parties)) %>%
  mutate(incumbent_party_magar = sapply(incumbent_party_magar, replace1)) %>%
  mutate(runnerup_party_magar = sapply(runnerup_party_magar, replace1))


assign_incumbent_vote <- function(data) {
  
  # Initialize columns
  data <- data %>%
    mutate(incumbent_vote = NA,
           party_component = NA)
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    incumbent_party <- data$incumbent_party_magar[I]
    
    # Handle cases where all incumbent_party_ variables are NA
    if (is.na(incumbent_party) || incumbent_party == "") {
      incumbent_party <- data %>%
        select(starts_with("incumbent_party_")) %>%
        filter(row_number() == I) %>%
        unlist(use.names = FALSE) %>%
        na.omit() %>%
        unique()
      
      if (length(incumbent_party) == 0) next # Skip if no valid incumbent_party values are found
    }
    
    # Check if it is a coalition
    if (str_detect(incumbent_party, "_")) {
      # Split the coalition into individual parties
      parties <- unlist(str_split(incumbent_party, "_"))
      
      # Find columns that match all parties in any order (exact match or broader coalition)
      coalition_vars <- names(data)[sapply(names(data), function(x) {
        party_components <- unlist(str_split(x, "_"))
        all(parties %in% party_components) # Check if all parties are in the column
      })]
      
      # Check for valid votes in coalition columns
      valid_found <- FALSE
      for (var in coalition_vars) {
        if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
          data$incumbent_vote[I] <- data[[var]][I]
          data$party_component[I] <- var
          valid_found <- TRUE
          break
        }
      }
      
      # If no valid value found, continue to next row
      if (valid_found) next
    } else {
      # Handle single parties
      party <- incumbent_party
      
      # Regex to match the exact party name (exclude PANAL for PAN)
      if (party == "PAN") {
        party_regex <- "(^PAN$|_PAN$|^PAN_)"
      } else if (party == "PANAL") {
        party_regex <- "(^PANAL$|_PANAL$|^PANAL_)"
      } else {
        party_regex <- paste0("(^|_)", party, "($|_)")
      }
      
      # Find columns matching the standalone party
      candidate_vars <- names(data)[grepl(party_regex, names(data))]
      
      # Check for valid votes in candidate columns
      valid_found <- FALSE
      for (var in candidate_vars) {
        if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
          data$incumbent_vote[I] <- data[[var]][I]
          data$party_component[I] <- var
          valid_found <- TRUE
          break
        }
      }
      
      # If no valid value found, check for broader coalitions containing the party
      if (!valid_found) {
        broader_coalition_vars <- names(data)[sapply(names(data), function(x) {
          party_components <- unlist(str_split(x, "_"))
          party %in% party_components # Check if the party is part of the coalition
        })]
        
        for (var in broader_coalition_vars) {
          if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
            data$incumbent_vote[I] <- data[[var]][I]
            data$party_component[I] <- var
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

check_majority_party <- function(data) {
  
  # List of columns to check for incumbent parties
  incumbent_cols <- c("incumbent_party_Horacio", "incumbent_party_inafed", "incumbent_party_JL", "incumbent_party_magar")
  
  # Loop through each row of the data with missing incumbent_vote
  for (I in which(is.na(data$incumbent_vote))) {
    
    party_count <- list()
    panal_found <- FALSE
    
    # Check the special case for PC
    if (data$incumbent_party_magar[I] == "PC" && data$incumbent_candidate_magar[I] == "ALFREDO CRUZ GUZMAN" && data$year[I] == 2010) {
      pc_vars <- names(data)[str_detect(names(data), "PC")]
      for (pc_var in pc_vars) {
        if (!is.na(data[[pc_var]][I]) && data[[pc_var]][I] != 0) {
          data$incumbent_vote[I] <- data[[pc_var]][I]
          data$party_component[I] <- pc_var
          next
        }
      }
    }
    
    # Count the occurrence of each party in the specified columns
    for (col in incumbent_cols) {
      if (!is.na(data[[col]][I]) && data[[col]][I] != "") {
        party <- data[[col]][I]
        if (party == "PANAL") {
          panal_found <- TRUE
        }
        if (party %in% names(party_count)) {
          party_count[[party]] <- party_count[[party]] + 1
        } else {
          party_count[[party]] <- 1
        }
      }
    }
    
    # If PANAL is found in any of the columns, look for PANAL in the data
    if (panal_found) {
      panal_vars <- names(data)[str_detect(names(data), "PANAL")]
      for (panal_var in panal_vars) {
        if (!is.na(data[[panal_var]][I]) && data[[panal_var]][I] != 0) {
          data$incumbent_vote[I] <- data[[panal_var]][I]
          data$party_component[I] <- panal_var
          next
        }
      }
    }
    
    # Find the majority party (if any)
    majority_party <- NA
    for (party in names(party_count)) {
      if (party_count[[party]] > 1) {
        majority_party <- party
        break
      }
    }
    
    # If a majority party is found
    if (!is.na(majority_party) && majority_party != "") {
      
      # Check for individual party or coalition containing the party
      party_vars <- names(data)[str_detect(names(data), majority_party)]
      
      # Initialize flag to check if a party is found
      party_found <- FALSE
      
      for (party_var in party_vars) {
        # Distinguish between PAN and PANAL
        if (str_detect(party_var, "^PAN$") || (!str_detect(party_var, "PANAL") && str_detect(party_var, "PAN"))) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$incumbent_vote[I] <- data[[party_var]][I]
            data$party_component[I] <- party_var
            party_found <- TRUE
            break
          }
        } else if (!str_detect(party_var, "PAN") && !str_detect(party_var, "PANAL")) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$incumbent_vote[I] <- data[[party_var]][I]
            data$party_component[I] <- party_var
            party_found <- TRUE
            break
          }
        }
      }
      
      # If no individual party found, check coalitions containing the party
      if (!party_found) {
        coalition_vars <- names(data)[sapply(names(data), function(x) majority_party %in% str_split(x, "_")[[1]])]
        
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

finaldb <- check_majority_party(finaldb)

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
output_dir <- file.path(getwd(), "Processed Data/chiapas")
output_path <- file.path(output_dir, "chiapas_vote_calculator.csv")

# Use write_csv to save the file
write_csv(finaldb, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)




