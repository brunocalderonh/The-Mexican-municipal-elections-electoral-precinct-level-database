# Clear the environment
rm(list = ls())

# Load necessary libraries
library(readxl)
library(dplyr)

# Set working directory
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/")

# Read the Excel files
db <- read_excel("quintanaroo/quintanaroo_collapsed_edited.xlsx")
og <- read.csv("quintanaroo/quintanaroo_FINAL_draft.csv")

# Select the relevant columns from the collapsed database
db_subset <- db %>%
  select(uniqueid, year, state_incumbent_party, state_incumbent_candidate, state_year, researched_incumbent, source_researched_incumbent)

db_subset <- db_subset[!duplicated(db_subset), ]

# Merge the datasets based on uniqueid and year
merged_data <- og %>%
  left_join(db_subset, by = c("uniqueid", "year"))


assign_votes <- function(data) {
  
  # Initialize columns for each party
  data <- data %>%
    mutate(
      PRI_vote = NA,
      PRI_vote_party_component = NA,
      PAN_vote = NA,
      PAN_vote_party_component = NA,
      MORENA_vote = NA,
      MORENA_vote_party_component = NA,
      PRD_vote = NA,
      PRD_vote_party_component = NA
    )
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    # Fill PRI_vote and PRI_vote_party_component
    pri_cols <- names(data)[str_detect(names(data), "PRI")]
    
    for (pri_col in pri_cols) {
      if (!is.na(data[[pri_col]][I]) && data[[pri_col]][I] != 0 && data[[pri_col]][I] != "") {
        data$PRI_vote[I] <- data[[pri_col]][I]
        data$PRI_vote_party_component[I] <- pri_col
        break
      }
    }
    
    # Repeat the same logic for PAN_vote and PAN_vote_party_component
    pan_cols <- names(data)[str_detect(names(data), "PAN")]
    
    for (pan_col in pan_cols) {
      if (!is.na(data[[pan_col]][I]) && data[[pan_col]][I] != 0 && data[[pan_col]][I] != "") {
        data$PAN_vote[I] <- data[[pan_col]][I]
        data$PAN_vote_party_component[I] <- pan_col
        break
      }
    }
    
    # Repeat the same logic for MORENA_vote and MORENA_vote_party_component
    morena_cols <- names(data)[str_detect(names(data), "MORENA")]
    
    for (morena_col in morena_cols) {
      if (!is.na(data[[morena_col]][I]) && data[[morena_col]][I] != 0 && data[[morena_col]][I] != "") {
        data$MORENA_vote[I] <- data[[morena_col]][I]
        data$MORENA_vote_party_component[I] <- morena_col
        break
      }
    }
    
    # Repeat the same logic for PRD_vote and PRD_vote_party_component
    prd_cols <- names(data)[str_detect(names(data), "PRD")]
    
    for (prd_col in prd_cols) {
      if (!is.na(data[[prd_col]][I]) && data[[prd_col]][I] != 0 && data[[prd_col]][I] != "") {
        data$PRD_vote[I] <- data[[prd_col]][I]
        data$PRD_vote_party_component[I] <- prd_col
        break
      }
    }
    
    # Process rows with a string value in researched_incumbent
    if (!is.na(data$researched_incumbent[I]) && data$researched_incumbent[I] != "") {
      incumbent_party <- data$researched_incumbent[I]
      
      # Reset incumbent_vote and party_component for these rows
      data$incumbent_vote[I] <- NA
      data$party_component[I] <- NA
      
      # Check if it is a coalition
      if (str_detect(incumbent_party, "_")) {
        parties <- unlist(str_split(incumbent_party, "_"))
        
        # Check if any individual party within the coalition is present in other columns
        individual_party_found <- FALSE
        for (party in parties) {
          party_vars <- names(data)[str_detect(names(data), party)]
          
          for (party_var in party_vars) {
            if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
              data$incumbent_vote[I] <- data[[party_var]][I]
              data$party_component[I] <- party_var
              individual_party_found <- TRUE
              break
            }
          }
          if (individual_party_found) break
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
        party_vars <- names(data)[str_detect(names(data), incumbent_party)]
        
        for (party_var in party_vars) {
          # Ensure PAN is not confused with PANAL
          if (str_detect(party_var, "^PAN$") || (!str_detect(party_var, "PANAL") && str_detect(party_var, "PAN"))) {
            if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
              data$incumbent_vote[I] <- data[[party_var]][I]
              data$party_component[I] <- party_var
              break
            }
          } else if (!str_detect(party_var, "PAN") && !str_detect(party_var, "PANAL")) {
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
  }
  
  return(data)
}


merged_data <- assign_votes(merged_data)



assign_state_incumbent_vote <- function(data) {
  
  # Initialize columns
  data <- data %>%
    mutate(state_incumbent_vote = NA,
           state_incumbent_vote_party_component = NA)
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    state_incumbent_party <- data$state_incumbent_party[I]
    
    # Skip if state_incumbent_party is NA or empty
    if (is.na(state_incumbent_party) || state_incumbent_party == "") next
    
    # Check if it is a coalition
    if (str_detect(state_incumbent_party, "_")) {
      parties <- unlist(str_split(state_incumbent_party, "_"))
      
      # Check if any individual party within the coalition is present in other columns
      individual_party_found <- FALSE
      for (party in parties) {
        party_vars <- names(data)[str_detect(names(data), paste0("^", party, "$"))]
        
        for (party_var in party_vars) {
          # Ensure PAN is not confused with PANAL
          if (party == "PAN" && !str_detect(party_var, "PANAL")) {
            if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
              data$state_incumbent_vote[I] <- data[[party_var]][I]
              data$state_incumbent_vote_party_component[I] <- party_var
              individual_party_found <- TRUE
              break
            }
          } else if (party != "PAN" || (!str_detect(party_var, "PANAL") && str_detect(party_var, party))) {
            if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
              data$state_incumbent_vote[I] <- data[[party_var]][I]
              data$state_incumbent_vote_party_component[I] <- party_var
              individual_party_found <- TRUE
              break
            }
          }
        }
        if (individual_party_found) break
      }
      
      # If no single party found, check coalitions containing the single party
      if (is.na(data$state_incumbent_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) all(parties %in% str_split(x, "_")[[1]]))]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$state_incumbent_vote[I] <- data[[coalition_var]][I]
            data$state_incumbent_vote_party_component[I] <- coalition_var
            break
          }
        }
      }
      
    } else {
      # Handle single parties
      party_vars <- names(data)[str_detect(names(data), paste0("^", state_incumbent_party, "$"))]
      
      for (party_var in party_vars) {
        # Ensure PAN is not confused with PANAL
        if (state_incumbent_party == "PAN" && !str_detect(party_var, "PANAL")) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$state_incumbent_vote[I] <- data[[party_var]][I]
            data$state_incumbent_vote_party_component[I] <- party_var
            break
          }
        } else if (state_incumbent_party != "PAN" || (!str_detect(party_var, "PANAL") && str_detect(party_var, state_incumbent_party))) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$state_incumbent_vote[I] <- data[[party_var]][I]
            data$state_incumbent_vote_party_component[I] <- party_var
            break
          }
        }
      }
      
      # If no single party found, check coalitions containing the single party
      if (is.na(data$state_incumbent_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) state_incumbent_party %in% str_split(x, "_")[[1]])]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$state_incumbent_vote[I] <- data[[coalition_var]][I]
            data$state_incumbent_vote_party_component[I] <- coalition_var
            break
          }
        }
      }
    }
  }
  
  return(data)
}

merged_data <- assign_state_incumbent_vote(merged_data)


merged_data <- merged_data %>%
  select(uniqueid,
         year,
         state, 
         mun, 
         section,
         incumbent_party_magar,
         incumbent_candidate_magar,
         incumbent_vote,
         party_component,
         mutually_exclusive,
         researched_incumbent,
         source_researched_incumbent,
         incumbent_party_JL,
         incumbent_candidate_JL,
         incumbent_party_Horacio,
         incumbent_party_inafed,
         incumbent_candidate_inafed,
         state_year,
         state_incumbent_party,
         state_incumbent_candidate,
         state_incumbent_vote,
         state_incumbent_vote_party_component,
         PRI_vote,
         PRI_vote_party_component,
         PRD_vote,
         PRD_vote_party_component,
         PAN_vote,
         PAN_vote_party_component,
         MORENA_vote,
         MORENA_vote_party_component,
         runnerup_party_magar,
         runnerup_candidate_magar,
         runnerup_vote ,
         runnerup_party_component,
         margin,
         listanominal,
         valid,
         total,
         everything())

write.csv(merged_data, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/quintanaroo/quintanaroo_FINALV.csv")



