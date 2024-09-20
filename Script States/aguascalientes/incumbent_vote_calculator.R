rm(list=ls())

library(stringr)
library(dplyr)
library(writexl)
library(haven)

# Load the data
finaldb <- read_dta( "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/aguascalientes/aguascalientes_merged_IncumbentVote.dta")

finaldb <- finaldb %>%
  select(-winner) %>%
  select(state,mun,section,uniqueid,year,incumbent_party_magar,incumbent_candidate_magar,incumbent_party_Horacio,incumbent_party_JL,incumbent_party_inafed, incumbent_candidate_inafed, runnerup_party_magar,runnerup_candidate_magar, margin,everything())

replace_parties <- function(party_str) {
  replacements <- c("PNA" = "PANAL", 
                    "INDEP" = "CI_1",
                    "INDEP2" = "CI_2",
                    "INDEP3" = "CI_3",
                    "INDEP4" = "CI_4",
                    "CONVE" = "PC")
  
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
  for (i in 1:nrow(data)) {
    incumbent_party <- data$incumbent_party_magar[i]
    
    # Skip if incumbent_party is NA or empty
    if (is.na(incumbent_party) || incumbent_party == "") next
    
    # Check if it is a coalition
    if (str_detect(incumbent_party, "_")) {
      parties <- unlist(str_split(incumbent_party, "_"))
      
      # Find all variables that contain all elements of the coalition
      coalition_vars <- names(data)[sapply(names(data), function(x) all(parties %in% str_split(x, "_")[[1]]))]
      
      for (coalition_var in coalition_vars) {
        if (!is.na(data[[coalition_var]][i]) && data[[coalition_var]][i] != 0) {
          data$incumbent_vote[i] <- data[[coalition_var]][i]
          data$party_component[i] <- coalition_var
          break
        }
      }
    } else {
      # Handle single parties
      party_vars <- names(data)[str_detect(names(data), incumbent_party)]
      
      for (party_var in party_vars) {
        if (!is.na(data[[party_var]][i]) && data[[party_var]][i] != 0) {
          data$incumbent_vote[i] <- data[[party_var]][i]
          data$party_component[i] <- party_var
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
  filter(
    !(is.na(incumbent_party_JL) | incumbent_party_JL == "") | 
      !(is.na(incumbent_party_Horacio) | incumbent_party_Horacio == "") | 
      !(is.na(incumbent_party_inafed) | incumbent_party_inafed == "") | 
      !(is.na(incumbent_party_magar) | incumbent_party_magar == "")
  )
# Assuming your data frame is named 'df'
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

finaldb <- finaldb %>%
  filter(year != 2004)


write.csv(finaldb, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/aguascalientes/aguascalientes_FINAL_draft.csv")


#CLEAN DB
  # Select only the desired columns
  aguascalientes_finaldb <- finaldb %>% 
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
  ) %>%
    mutate(incumbent_vote = as.numeric(incumbent_vote))

  

  write.csv(aguascalientes_finaldb, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/aguascalientes/aguascalientes_FINAL.csv")
  