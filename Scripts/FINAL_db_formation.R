# Clear the environment
rm(list = ls())
library(readxl)
library(tidyverse)
library(dplyr)
library(xtable)
library(rstudioapi)

###### APPEND 32 Individual finals state files to create a single file
# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../"))

# Define the base path relative to the current working directory
base_path <- file.path(getwd(), "Processed Data")

# Define the state names
states <- c("aguascalientes", "baja", "bajasur", "campeche", "chiapas", "chihuahua", 
            "coahuila", "colima", "durango", "guanajuato", "guerrero", "hidalgo", 
            "jalisco", "mexico", "michoacan", "morelos", "nayarit", 
            "nuevoleon", "oaxaca", "puebla", "queretaro", "quintanaroo", 
            "sanluispotosi", "sinaloa", "sonora", "tabasco", "tamaulipas", 
            "tlaxcala","veracruz", "yucatan", "zacatecas")

# Initialize an empty list to store the data frames
df_list <- list()

# Loop through each state, read the data, and append the relevant variables
for (state in states) {
  # Construct the file path with the correct naming convention
  file_path <- paste0(base_path, "/", state, "/", state, "_final.csv")
  
  # Check if the file exists before attempting to read it
  if (file.exists(file_path)) {
    # Read the CSV file
    data <- read.csv(file_path)
    
    # Select the relevant columns
    data_selected <- data %>%
      select(
        uniqueid,
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
        total)
    
    # Add the data to the list
    df_list[[state]] <- data_selected
  } else {
    warning(paste("File does not exist:", file_path))
  }
}

# Combine all data frames into one
final_df <- bind_rows(df_list)

####### Manipulate the previously created database in order to obtain a cleaned final database
final_df <- final_df %>% 
  select(-X)

# Define the function to determine final incumbent, excluding certain parties
determine_final_incumbent <- function(researched_incumbent, incumbent_party_magar, incumbent_party_JL,
                                      incumbent_party_Horacio, incumbent_party_inafed) {
  
  # Full list of forbidden parties (including newly added PMC)
  excluded_parties <- c("AXB", "AXA", "AXC", "AXCH", "AXS", "CONVER", "CXBDT", "AXU", "CXBNCH", "CXBT",
                        "CXBNDT", "ALIANZA_", "ALIANZA", "CXBDCH", "COALICIoN", "COAL", "CC", "CCC", 
                        "UXCH", "GNU", "MXH", "DPDG", "CD", "AXJ", "CPJ", "APT_", "APM", "CUPM", 
                        "CONVER.", "COAL.", "C.C.", "CM", "APN2005", "JXBDT", "NNU", "CAC", "AXM", 
                        "JXNL", "CPNL", "APTS", "CDPPN", "CXBP", "CUPG", "APA", "CPP", "PSI", 
                        "CCPQ", "CQRA", "CPTGM", "NPP", "CPSL", "APAG", "AP", "CPUGHYE", "TSTAMPS", 
                        "ATT", "PJS", "ASXXI", "APPT", "UPT", "CAFV", "CUV", "ZANUNE", 
                        "PRIZAC", "CUXT", "CSA", "CCS", "TRANSIN", "XGR", "QROOUNE", "APPJ", 
                        "APSP", "JPC", "UYC", "SNI", "XENQTQ", "CPM", "MNU", "ECV", "CPEM", "UPH", 
                        "CPG", "PPG", "CACPC", "NCCH", "CCCH", "UXBCS", "SUDTP", "COALICIÓN", "XGR",
                        "SQROO", "UPI", "PMC")  # Newly added PMC
  
  # Step 1: Check if researched_incumbent has a value
  if (!is.na(researched_incumbent) && researched_incumbent != "") {
    return(researched_incumbent)
  }
  
  # Step 2: Create a list of incumbent parties
  incumbents <- c(incumbent_party_magar, incumbent_party_JL, incumbent_party_Horacio, incumbent_party_inafed)
  
  # Step 3: Exclude values in the list of excluded parties
  valid_incumbents <- incumbents[!incumbents %in% excluded_parties & incumbents != "" & !is.na(incumbents)]
  
  # Step 4: Count the occurrences of each unique individual party (not coalitions)
  individual_parties <- valid_incumbents[!grepl("_", valid_incumbents)]
  
  if (length(individual_parties) > 0) {
    party_counts <- table(individual_parties)
    most_common_party <- names(party_counts)[which.max(party_counts)]
    
    # Return the most common individual party if one exists
    if (!is.null(most_common_party)) {
      return(most_common_party)
    }
  }
  
  # Step 5: Handle coalitions (values with underscores)
  coalitions <- valid_incumbents[grepl("_", valid_incumbents)]
  
  # Step 6: Check if individual parties from coalitions match any other party
  if (length(coalitions) > 0) {
    for (coalition in coalitions) {
      # Split the coalition into individual parties
      coalition_parties <- unlist(strsplit(coalition, "_"))
      
      # Filter incumbent variables to only consider parties from the coalition
      matching_parties <- coalition_parties[coalition_parties %in% valid_incumbents]
      
      if (length(matching_parties) > 0) {
        return(matching_parties[1])  # Return the first matched party
      }
    }
    # If no individual match found, return the coalition itself
    return(coalitions[1])
  }
  
  # If no valid values are found, return NA
  return(NA)
}




# Apply the function to your dataset
final_df <- final_df %>%
  rowwise() %>%
  mutate(incumbent_party = determine_final_incumbent(researched_incumbent,
                                                     incumbent_party_magar,
                                                     incumbent_party_JL,
                                                     incumbent_party_Horacio,
                                                     incumbent_party_inafed)) %>%
  ungroup()

final_df <- final_df %>%
  mutate(runnerup_party = runnerup_party_magar) %>% 
  mutate(runnerup_candidate = runnerup_party_magar)

final_df <- final_df %>%
  mutate(incumbent_party = case_when(
    incumbent_party %in% c("PCARDE", "PartCardenista", "PFCRN") ~ "PartCarden",
    incumbent_party %in% c("PI", "CI_1", "CI", "C.I.", "Independent") ~ "Independent",
    TRUE ~ incumbent_party
  )) %>% 
  mutate( incumbent_candidate = incumbent_candidate_magar)

final_df <- final_df %>% 
  mutate(turnout = total/listanominal)
final_df <- final_df %>% 
  mutate(valid_voteShare_incumbent_vote = ifelse(valid > 0, incumbent_vote / valid, NA),
         valid_voteShare_state_incumbent_vote = ifelse(valid > 0, state_incumbent_vote / valid, NA),
         valid_voteShare_PRI_vote = ifelse(valid > 0, PRI_vote / valid, NA),
         valid_voteShare_PRD_vote = ifelse(valid > 0, PRD_vote / valid, NA),
         valid_voteShare_PAN_vote = ifelse(valid > 0, PAN_vote / valid, NA),
         valid_voteShare_MORENA_vote = ifelse(valid > 0, MORENA_vote / valid, NA),
         valid_voteShare_runnerup_vote = ifelse(valid > 0, runnerup_vote / valid, NA),
         listanominal_voteShare_incumbent_vote = ifelse(listanominal > 0, incumbent_vote / listanominal, NA),
         listanominal_voteShare_state_incumbent_vote = ifelse(listanominal > 0, state_incumbent_vote / listanominal, NA),
         listanominal_voteShare_PRI_vote = ifelse(listanominal > 0, PRI_vote / listanominal, NA),
         listanominal_voteShare_PRD_vote = ifelse(listanominal > 0, PRD_vote / listanominal, NA),
         listanominal_voteShare_PAN_vote = ifelse(listanominal > 0, PAN_vote / listanominal, NA),
         listanominal_voteShare_MORENA_vote = ifelse(listanominal > 0, MORENA_vote / listanominal, NA),
         listanominal_voteShare_runnerup_vote = ifelse(listanominal > 0, runnerup_vote / listanominal, NA)) %>% 
  select(uniqueid,
         year,
         state, 
         mun, 
         section,
         incumbent_candidate,
         incumbent_party,
         incumbent_vote,
         valid_voteShare_incumbent_vote,
         listanominal_voteShare_incumbent_vote,
         state_incumbent_party,
         state_incumbent_vote,
         valid_voteShare_state_incumbent_vote,
         listanominal_voteShare_state_incumbent_vote,
         PRI_vote,
         valid_voteShare_PRI_vote,
         listanominal_voteShare_PRI_vote,
         PRD_vote,
         valid_voteShare_PRD_vote,
         listanominal_voteShare_PRD_vote,
         PAN_vote,
         valid_voteShare_PAN_vote,
         listanominal_voteShare_PAN_vote,
         MORENA_vote,
         valid_voteShare_MORENA_vote,
         listanominal_voteShare_MORENA_vote,
         runnerup_candidate,
         runnerup_party,
         runnerup_vote,
         valid_voteShare_runnerup_vote,
         listanominal_voteShare_runnerup_vote,
         margin,
         listanominal,
         valid,
         total) 

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Final Data")
output_path <- file.path(output_dir, "all_states_final.csv")

# Use write_csv to save the file
write_csv(final_df, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)
