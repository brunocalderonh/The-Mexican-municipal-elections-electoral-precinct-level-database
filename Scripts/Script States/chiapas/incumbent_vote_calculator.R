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
  select(state,mun,section,uniqueid,year,incumbent_party_magar,incumbent_candidate_magar,incumbent_party_JL, runnerup_party_magar, runnerup_candidate_magar, margin,everything())
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
  
  # Initialize columns for `assign_incumbent_vote`
  data <- data %>%
    mutate(incumbent_vote = NA,
           party_component = NA,
           final_incumbent = NA)  # Track the exact column used
  
  # assign_incumbent_vote logic
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
      
      if (length(incumbent_party) == 0) next  # Skip if no valid incumbent_party values are found
      if (length(incumbent_party) > 1) {
        incumbent_party <- incumbent_party[1]  # Use the first party if multiple are found
      }
    }
    
    # Check if it is a coalition
    if (str_detect(incumbent_party, "_")) {
      parties <- unlist(str_split(incumbent_party, "_"))
      
      # Find columns matching all parties (coalitions)
      coalition_vars <- names(data)[sapply(names(data), function(x) {
        party_components <- unlist(str_split(x, "_"))
        all(parties %in% party_components)  # Match coalition
      })]
      
      # Check for valid votes in coalition columns
      for (var in coalition_vars) {
        if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
          data$incumbent_vote[I] <- data[[var]][I]
          data$party_component[I] <- var
          data$final_incumbent[I] <- incumbent_party  # Assign the coalition as final_incumbent
          break
        }
      }
    } else {
      # Handle single parties
      party <- incumbent_party
      party_regex <- paste0("(^|_)", party, "($|_)")
      
      # Find columns matching the single party
      candidate_vars <- names(data)[grepl(party_regex, names(data))]
      for (var in candidate_vars) {
        if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
          data$incumbent_vote[I] <- data[[var]][I]
          data$party_component[I] <- var
          data$final_incumbent[I] <- incumbent_party  # Assign the single party as final_incumbent
          break
        }
      }
    }
  }
  
  # check_majority_party logic
  incumbent_cols <- c("incumbent_party_Horacio", "incumbent_party_inafed", "incumbent_party_JL", "incumbent_party_magar")
  
  for (I in which(is.na(data$incumbent_vote))) {
    party_count <- list()
    panal_found <- FALSE
    
    # Count the occurrence of each party in the specified columns
    for (col in incumbent_cols) {
      if (col %in% names(data)) {
        value <- data[[col]][I]
        if (!is.na(value) && value != "") {
          party <- value
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
    }
    
    # Check for majority party only if party_count is not empty
    if (length(party_count) > 0) {
      majority_party <- names(which.max(unlist(party_count)))
    } else {
      majority_party <- NA  # Set to NA if no parties are found
    }
    
    # Proceed only if majority_party is valid
    if (!is.na(majority_party) && majority_party != "") {
      party_vars <- names(data)[str_detect(names(data), majority_party)]
      
      for (party_var in party_vars) {
        if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
          data$incumbent_vote[I] <- data[[party_var]][I]
          data$party_component[I] <- party_var
          data$final_incumbent[I] <- majority_party
          break
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
output_dir <- file.path(getwd(), "Processed Data/chiapas")
output_path <- file.path(output_dir, "chiapas_vote_calculator.csv")

# Use write_csv to save the file
write_csv(finaldb, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)




