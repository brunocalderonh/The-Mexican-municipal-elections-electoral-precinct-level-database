# Clear the environment
rm(list = ls())
library(readxl)
library(tidyverse)
library(dplyr)
library(xtable)
library(rstudioapi)
library(readr)

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


# Initialize a list to store missing columns for each state
missing_columns_log <- list()

# Define the required columns
required_columns <- c(
  "uniqueid", "year", "state", "mun", "section",
  "incumbent_party_magar", "incumbent_candidate_magar",
  "incumbent_vote", "party_component", "mutually_exclusive",
  "researched_incumbent", "source_researched_incumbent",
  "incumbent_party_JL", "incumbent_candidate_JL", "state_incumbent_party",
  "state_incumbent_vote", "state_incumbent_vote_party_component",
  "PRI_vote", "PRI_vote_party_component", "PRD_vote",
  "PRD_vote_party_component", "PAN_vote", "PAN_vote_party_component",
  "MORENA_vote", "MORENA_vote_party_component", "runnerup_party_magar",
  "runnerup_candidate_magar", "runnerup_vote", "runnerup_party_component",
  "margin", "listanominal", "valid", "total", "turnout","final_incumbent"
)

# Loop through each state file to check for missing columns
for (state in states) {
  # Construct the file path
  file_path <- paste0(base_path, "/", state, "/", state, "_final.csv")
  
  # Check if the file exists
  if (file.exists(file_path)) {
    # Read the CSV file
    data <- read.csv(file_path)
    
    # Find missing columns by comparing required columns with actual columns
    missing_columns <- setdiff(required_columns, names(data))
    
    # Log missing columns if there are any
    if (length(missing_columns) > 0) {
      missing_columns_log[[state]] <- missing_columns
    }
  } else {
    warning(paste("File does not exist:", file_path))
  }
}

# Display missing columns for each file
if (length(missing_columns_log) > 0) {
  for (state in names(missing_columns_log)) {
    cat("The following columns are missing in", state, "file:", missing_columns_log[[state]], "\n")
  }
} else {
  cat("All files contain the required columns.\n")
}


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
        final_incumbent,
        incumbent_vote,
        party_component,
        mutually_exclusive,
        incumbent_party_JL,
        incumbent_candidate_JL,
        state_incumbent_party,
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
        researched_incumbent,
        source_researched_incumbent,
        margin,
        listanominal,
        valid,
        total,
        turnout,
        starts_with("CI_"),
        everything()# Select all columns that start with 'CI_'
      )
    
    # Add the data to the list
    df_list[[state]] <- data_selected
  } else {
    warning(paste("File does not exist:", file_path))
  }
}

# Combine all data frames into one
final_df <- bind_rows(df_list)

####### Manipulate the previously created database in order to obtain a cleaned final database
# Load the duplicate cases

duplicate_cases <- read_csv("Processed Data/duplicate_cases.csv") %>%
  rename("uniqueid" = "mun_code",
         "section" = "precinct") %>%
  select(uniqueid, year, section) %>%
  mutate(id_duplicate = 1)  # Assign 1 for duplicates

# Perform a left join and assign 0 for non-matching rows
final_df <- final_df %>%
  left_join(duplicate_cases, by = c("uniqueid", "year", "section")) %>%
  mutate(id_duplicate = replace_na(id_duplicate, 0))  # Replace NA with 0

assign_incumbent_vote_duplicates <- function(data) {
  
  # Check required columns
  required_cols <- c("incumbent_vote", "party_component", "final_incumbent", "incumbent_party_magar")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain 'incumbent_vote', 'party_component', 'final_incumbent', and 'incumbent_party_magar' columns before calling this function.")
  }
  
  # Optional columns we will set to NA for all duplicates
  optional_cols <- c("researched_incumbent", "source_researched_incumbent")
  for (col in optional_cols) {
    if (!col %in% names(data)) {
      # If these columns don't exist, create them to avoid errors
      data[[col]] <- NA
    }
  }
  
  # Process only the rows where id_duplicate == 1
  duplicate_rows <- which(data$id_duplicate == 1)
  
  for (I in duplicate_rows) {
    # Erase researched_incumbent and source_researched_incumbent for all duplicates
    data$researched_incumbent[I] <- NA
    data$source_researched_incumbent[I] <- NA
    
    # Start from incumbent_party_magar
    final_incumbent_value <- data$incumbent_party_magar[I]
    
    # If incumbent_party_magar is NA or empty, skip the logic for votes
    if (is.na(final_incumbent_value) || final_incumbent_value == "") {
      # If empty, also clear final_incumbent, incumbent_vote, and party_component
      data$final_incumbent[I] <- NA
      data$incumbent_vote[I] <- NA
      data$party_component[I] <- NA
      next
    }
    
    # Overwrite final_incumbent with the value from incumbent_party_magar
    data$final_incumbent[I] <- final_incumbent_value
    
    valid_found <- FALSE
    
    # Coalition logic if underscore in final_incumbent_value
    if (str_detect(final_incumbent_value, "_")) {
      parties <- unlist(str_split(final_incumbent_value, "_"))
      
      # Identify coalition columns
      coalition_vars <- names(data)[sapply(names(data), function(x) {
        party_components <- unlist(str_split(x, "_"))
        all(parties %in% party_components) && 
          !x %in% c("final_incumbent", "incumbent_vote", "party_component", "incumbent_party_magar",
                    "researched_incumbent", "source_researched_incumbent")
      })]
      
      # Exclude any variable with "_vote" in it
      coalition_vars <- coalition_vars[!str_detect(coalition_vars, "_vote")]
      
      # Try to find a coalition column with a non-zero vote
      for (var in coalition_vars) {
        if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
          data$incumbent_vote[I] <- data[[var]][I]
          data$party_component[I] <- var
          valid_found <- TRUE
          break
        }
      }
      
    } else {
      # Single party logic
      party <- final_incumbent_value
      
      # Construct a regex to match columns for a single party
      if (party == "PAN") {
        party_regex <- "(^PAN$|_PAN$|^PAN_)"
      } else if (party == "PANAL") {
        party_regex <- "(^PANAL$|_PANAL$|^PANAL_)"
      } else {
        party_regex <- paste0("(^|_)", party, "($|_)")
      }
      
      # Find columns matching this single party pattern
      candidate_vars <- names(data)[grepl(party_regex, names(data))]
      
      # Exclude any variable with "_vote" in it
      candidate_vars <- candidate_vars[!str_detect(candidate_vars, "_vote")]
      
      # Try exact party columns first
      for (var in candidate_vars) {
        if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
          data$incumbent_vote[I] <- data[[var]][I]
          data$party_component[I] <- var
          valid_found <- TRUE
          break
        }
      }
      
      # If no direct match found, try coalitions containing the party
      if (!valid_found) {
        broader_coalition_vars <- names(data)[sapply(names(data), function(x) {
          party_components <- unlist(str_split(x, "_"))
          party %in% party_components && 
            !x %in% c("final_incumbent", "incumbent_vote", "party_component", "incumbent_party_magar",
                      "researched_incumbent", "source_researched_incumbent")
        })]
        
        # Exclude any variable with "_vote" in it
        broader_coalition_vars <- broader_coalition_vars[!str_detect(broader_coalition_vars, "_vote")]
        
        for (var in broader_coalition_vars) {
          if (!is.na(data[[var]][I]) && data[[var]][I] != 0) {
            data$incumbent_vote[I] <- data[[var]][I]
            data$party_component[I] <- var
            valid_found <- TRUE
            break
          }
        }
      }
    }
    
    # If not found any valid vote column, set these columns to NA
    if (!valid_found) {
      data$incumbent_vote[I] <- NA
      data$party_component[I] <- NA
      data$final_incumbent[I] <- NA
    }
  }
  
  return(data)
}
final_df <- assign_incumbent_vote_duplicates(final_df)

# Apply the function to your dataset
final_df <- final_df %>%
  mutate(incumbent_party = final_incumbent) 

final_df <- final_df %>%
  select(-incumbent_party_magar,-final_incumbent,-incumbent_party_JL)

final_df <- final_df %>%
  mutate(runnerup_party = runnerup_party_magar) %>% 
  mutate(runnerup_candidate = runnerup_party_magar) %>% 
  mutate(incumbent_candidate = incumbent_candidate_magar)

replace1 <- function(party_str) {
  if (!is.na(party_str)) {
    if (party_str == "INDEPENDIENTE") {
      party_str <- "INDEP"
    } 
  }
  return(party_str)  # Ensure the function returns the modified value
}

final_df <- final_df%>%
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

final_df <- final_df %>%
  mutate(incumbent_party = sapply(incumbent_party , replace2),
         runnerup_party = sapply(runnerup_party , replace2))

# Create the new column INDEP by summing all columns that start with "CI_"
final_df <- final_df %>%
  mutate(INDEP = rowSums(select(., starts_with("CI_")), na.rm = TRUE)) %>%
  select(-starts_with("CI_")) # Remove all columns that start with "CI_"

final_df <- final_df %>%
  mutate(
    incumbent_vote = if_else(incumbent_party == "INDEP", INDEP, incumbent_vote),
    runnerup_vote = if_else(runnerup_party == "INDEP", INDEP, runnerup_vote),
    state_incumbent_vote = if_else(state_incumbent_party == "INDEP", INDEP, state_incumbent_vote)
  )

final_df <- final_df %>%
  mutate(state_code = case_when(
    nchar(uniqueid) == 4 ~ substr(uniqueid, 1, 1),  # If `uniqueid` is 4 digits, take the first digit
    nchar(uniqueid) == 5 ~ substr(uniqueid, 1, 2)))   # If `uniqueid` is 5 digits, take the first 2 digits


final_df <- final_df %>% 
  mutate(share_incumbent_valid_vote = ifelse(valid > 0, incumbent_vote / valid, NA),
         share_state_incumbent_valid_vote = ifelse(valid > 0, state_incumbent_vote / valid, NA),
         share_PRI_valid_vote = ifelse(valid > 0, PRI_vote / valid, NA),
         share_PRD_valid_vote = ifelse(valid > 0, PRD_vote / valid, NA),
         share_PAN_valid_vote = ifelse(valid > 0, PAN_vote / valid, NA),
         share_MORENA_valid_vote = ifelse(valid > 0, MORENA_vote / valid, NA),
         share_runnerup_valid_vote= ifelse(valid > 0, runnerup_vote / valid, NA),
         share_incumbent_registered_voters = ifelse(listanominal > 0, incumbent_vote / listanominal, NA),
         share_state_incumbent_registered_voters = ifelse(listanominal > 0, state_incumbent_vote / listanominal, NA),
         share_PRI_registered_voters = ifelse(listanominal > 0, PRI_vote / listanominal, NA),
         share_PRD_registered_voters = ifelse(listanominal > 0, PRD_vote / listanominal, NA),
         share_PAN_registered_voters = ifelse(listanominal > 0, PAN_vote / listanominal, NA),
         share_MORENA_registered_voters = ifelse(listanominal > 0, MORENA_vote / listanominal, NA),
         share_runnerup_registered_voters = ifelse(listanominal > 0, runnerup_vote / listanominal, NA)) %>% 
  select(
    state_code,
    uniqueid,
    year,
    state, 
    mun, 
    section,
    incumbent_candidate,
    incumbent_party,
    incumbent_vote,
    party_component,
    share_incumbent_valid_vote,
    share_incumbent_registered_voters,
    state_incumbent_party,
    state_incumbent_vote,
    share_state_incumbent_valid_vote,
    share_state_incumbent_registered_voters,
    PRI_vote,
    share_PRI_valid_vote,
    share_PRI_registered_voters,
    PRD_vote,
    share_PRD_valid_vote,
    share_PRD_registered_voters,
    PAN_vote,
    share_PAN_valid_vote,
    share_PAN_registered_voters,
    MORENA_vote,
    share_MORENA_valid_vote,
    share_MORENA_registered_voters,
    runnerup_candidate,
    runnerup_party,
    runnerup_vote,
    share_runnerup_valid_vote,
    share_runnerup_registered_voters,
    researched_incumbent,
    source_researched_incumbent,
    margin,
    listanominal,
    valid,
    total,
    turnout
  )

final_df <- final_df %>%
  rename( "mun_code" = "uniqueid",
          "precinct" = "section",
          "incumbent_party_component"= "party_component",
          "registered_voters" = "listanominal",
          "mun_winning_margin" = "margin")


#### cretion of municipal level incumbents and 

db70 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit1970s.xlsx")
db80 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit1980s.xlsx")
db90 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit1990s.xlsx")
db00 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit2000s.xlsx")
db10 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit2010s.xlsx")
db20 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit2020s.xlsx")

magar_mun_db <- rbind(db70,db80)
magar_mun_db <- rbind(magar_mun_db, db90)
magar_mun_db <- rbind(magar_mun_db,db00)
magar_mun_db <- rbind(magar_mun_db,db10)
magar_mun_db <- rbind(magar_mun_db,db20)

magar_mun_db <- magar_mun_db %>% 
  select(-emm,-edon,-ife,-date,-ncand,-ncoal)

# Define identifier variables
id_vars <- c("yr", "mun", "inegi")

# Process each pair manually
pair_01 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l01", "v01"))) %>%
  filter(!is.na(l01)) %>%
  pivot_wider(
    names_from = l01,
    values_from = v01,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_02 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l02", "v02"))) %>%
  filter(!is.na(l02)) %>%
  pivot_wider(
    names_from = l02,
    values_from = v02,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_03 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l03", "v03"))) %>%
  filter(!is.na(l03)) %>%
  pivot_wider(
    names_from = l03,
    values_from = v03,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_04 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l04", "v04"))) %>%
  filter(!is.na(l04)) %>%
  pivot_wider(
    names_from = l04,
    values_from = v04,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_05 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l05", "v05"))) %>%
  filter(!is.na(l05)) %>%
  pivot_wider(
    names_from = l05,
    values_from = v05,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_06 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l06", "v06"))) %>%
  filter(!is.na(l06)) %>%
  pivot_wider(
    names_from = l06,
    values_from = v06,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_07 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l07", "v07"))) %>%
  filter(!is.na(l07)) %>%
  pivot_wider(
    names_from = l07,
    values_from = v07,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_08 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l08", "v08"))) %>%
  filter(!is.na(l08)) %>%
  pivot_wider(
    names_from = l08,
    values_from = v08,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_09 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l09", "v09"))) %>%
  filter(!is.na(l09)) %>%
  pivot_wider(
    names_from = l09,
    values_from = v09,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_10 <- magar_mun_db %>%
  select(all_of(c(id_vars, "l10", "v10"))) %>%
  filter(!is.na(l10)) %>%
  pivot_wider(
    names_from = l10,
    values_from = v10,
    values_fn = ~ paste(.x, collapse = ", ")
  )

# Continue similarly for all pairs (l11/v11 through l23/v23)...

# Step 2: Merge pairs one by one
final_magar_mun_db <- full_join(pair_01, pair_02, by = id_vars)
final_magar_mun_db <- full_join(final_magar_mun_db, pair_03, by = id_vars)
final_magar_mun_db <- full_join(final_magar_mun_db, pair_04, by = id_vars)
final_magar_mun_db <- full_join(final_magar_mun_db, pair_05, by = id_vars)
final_magar_mun_db <- full_join(final_magar_mun_db, pair_06, by = id_vars)
final_magar_mun_db <- full_join(final_magar_mun_db, pair_07, by = id_vars)
final_magar_mun_db <- full_join(final_magar_mun_db, pair_08, by = id_vars)
final_magar_mun_db <- full_join(final_magar_mun_db, pair_09, by = id_vars)
final_magar_mun_db <- full_join(final_magar_mun_db, pair_10, by = id_vars)
# Continue merging for remaining pairs...

# Step 3: Combine `.x` and `.y` columns dynamically
for (col in names(final_magar_mun_db)) {
  if (grepl("\\.x$", col)) {
    base_col <- sub("\\.x$", "", col)  # Get the base column name (e.g., 'pri')
    final_magar_mun_db[[base_col]] <- coalesce(final_magar_mun_db[[col]], final_magar_mun_db[[paste0(base_col, ".y")]])
  }
}

# Step 4: Remove `.x` and `.y` columns
final_magar_mun_db <- final_magar_mun_db %>%
  select(-ends_with(".x"), -ends_with(".y")) %>% 
  rename(
    "year" = "yr",
    "mun_code" = "inegi",
  ) %>% 
  mutate(mun_code = as.numeric(mun_code))%>%
  rename_with(
    ~ ifelse(. %in% c("year", "mun_code", "mun"), ., 
             str_replace_all(toupper(.), "-", "_"))
  )


final_magar_mun_db <- final_magar_mun_db %>%
  select(-mun)

##### merge with the final db incumbent and runnerup parties
#municipal level identifying variables
mun_db <- final_df %>% 
  select(
    mun_code,                              
    year,                                   
    incumbent_party,
    runnerup_party) # Use read_csv for CSV files

mun_db <- mun_db %>%
  group_by(mun_code, year) %>%
  summarise(
    incumbent_party = first(incumbent_party), # Assuming incumbent_party doesn't vary within mun_code
    runnerup_party = first(runnerup_party),   # Assuming runnerup_party doesn't vary within mun_code
    .groups = "drop" # Ensures the result is not grouped
  )



mun_db <- mun_db %>% 
  left_join(final_magar_mun_db, by = c("mun_code", "year"))


####
# Define replacements
replacements <- c("PNA" = "PANAL", 
                  "FUERCIUD" = "PFC", 
                  "CONVE" = "PC", 
                  "PJ1" = "PJ"
)

# Replace occurrences in column names
colnames(mun_db) <- colnames(mun_db) %>%
  str_replace_all(replacements)

indep_columns <- c(
  "INDEP_IVONNE", "INDEP_ROSA_M", "JOSE_RODRIGO_ROBINSON_BOURS_CASTELO", 
  "JOSE_RAMON_GUTIERREZ_MORALES", "JOSE_CORTES_PEREZ", "ARMANDO_FLORES_LOPEZ", 
  "INDEP_TAVO", "LUIS_FERNANDO_SERRANO_GARCIA", "AGUSTIN_TOLEDANO_AMARO", 
  "PEDRO_ANTONIO_MONTENEGRO_MORGADO", "CI_LUIS", "JOSE_RODRIGO_ROBINSON_BOURS",
  "INDEP_ELISA_PATRICIA_QUINTANILLA", "INDEP_HIRAM_PEÑA_GOMEZ", 
  "INDEP_CARLOS_LARA_MACIAS", "OMAR_GARCIA_ARAMBULA", "INDEP_DE_LA_TORRE", 
  "INDEP_CARLOS_TENA_NEVAREZ", "INDEP_LUIS_E_TERRAZAS_SEYFFERT", 
  "INDEP_ROBERTO_A_GONZALEZ_GARCIA", "INDEP_DIAZ", "INDEP_LOPEZ", 
  "INDEP_ORTIZ", "INDEP_AVILA", "INDEP_GABRIEL_DEL_MONTER", 
  "RODOLFO_VIDALES", "INDEP_JOSE_ROMERO_MERCADO", "INDEP_JOSE_ALFREDO_CASTRO", 
  "INDEP_JOSE_ZEPEDA_CONTRERAS", "INDEP_CBS", "INDEP_JLUS", "INDEP_DRH", 
  "INDEP_JOEL_A", "INDEP_BARUCH", "INDEP_RAYMUNDO", "INDEP_JOSE_D", 
  "INDEP_HUGO_AMADO_MUNOZ_FLORES", "INDEP_HIPOLITO_RIGOBERTO_PEREZ_MONTES", 
  "JOSE_FELIX_LOPEZ_MENDOZA", "RAYMUNDO_ARIAS_GALINDO", 
  "MIGUEL_OVED_ROBINSON_BOURS_DEL_CASTILLO", "DAVID_SANCHEZ_RINCON", 
  "INDEP_CSR", "INDEP_SEBASTIAN", "INDEP_JOSE_LUIS", "FELIPE_SALAZAR_CORREA", 
  "INDEP_FLORENTINO", "VICTOR_HUGO_RIVERA_MUÑOZ", "REYNALDO_LUJAN_ALVAREZ", 
  "RAFAEL_ANGEL_GARCIA_CANO", "CI_ESAU", "CI_AMELIO", "CI_IVAN", 
  "CI_JOSE_M", "INDEP_MIGUEL_RODRIGUEZ_SALAZAR", "YAIREMMANUELHERRERAFLORES", 
  "ALFREDO_MORENO_CARREÑO", "JUAN_CARLOS_MOLINA_TORRES", 
  "KEVIN_FERNANDO_PERAZA_ESTRADA", "FERNANDO_HOYOS_AGUILAR", 
  "INDEP_OSAIAS_OVILLA", "INDEP_GONZALEZ", "INDEP_RIVERA", "INDEP_REYES", 
  "INDEP_GUSTAVO_GARCIA_ARIAS", "HECTOR_FABELA", "INDEP_RAUL_JESUS_ROBLES_MEDINA", 
  "INDEP_VICTOR_MANUEL_PEREZ", "INDEP_MARIA_GUADALUPE_BECERRA", 
  "INDEP_JOSE_LUIS_GONZALEZ_GONZALEZ", "INDEP_MA_DE_LA_LUZ_RUIZ_RUBIO", 
  "INDEP_JOSE_FRANCISCO_SANCHEZ_PEREZ", "EDGAR_RENE_RUELAS_GIL", 
  "INDEP_MARIA_DEL_CARMEN_GALLEGOS", "INDEP_ALBERTO_ALFARO_GARCIA", 
  "INDEP_OMAR_CEBALLOS_MORENO", "INDEP_AFT", "INDEP_PAMM", "INDEP_RAE", 
  "INDEP_CABO", "INDEP_EIQB", "INDEP_PEDRO", "INDEP_ARTEMIO", 
  "INDEP_JUAN_DE_DIOS", "INDEP_IVAN", "INDEP_CARLOS_R", 
  "INDEP_FEDERICOMONTEROCASTILLO", "INDEP_CMR", "INDEP_JMS", 
  "INDEP_IZL", "ERNESTO_URIBE_CORONA", "GERMAN_AGUAYO_VALENZUELA", 
  "MARCO_ANTONIO_LUNA_ESPINDOLA", "RAMON_HUITRON_RAMIREZ", 
  "GUSTAVO_JIMENEZ_ROMERO", "ANGEL_COCOLETZI_COCOLETZI", "INDEP_MUCHARRAZ", 
  "INDEP_CACHIS", "INDEP_JUST", "LUIS_ENRIQUE", "DAVID_HDZ", 
  "ARTURO_BLANCO", "INDEP_LUPITA", "AURELIO_RUIZ", "INDEP_ALEXIS", 
  "INDEP_GUSTIN", "RAUL_ULLOA_GUZMAN", "MARIO_ADRIAN_REYES_SANTANA", 
  "SERAFIN_BERMUDEZ_VIRAMONTES", "J_JESUS_MAQUIR_ENRIQUEZ_RODRIGUEZ", 
  "WALTER_VALDES_GAMON", "GERARDO_GLEZ", "VICTOR_GUERRERO", 
  "ANA_EMILIA_PESCI_MARTINEZ", "CELSO_ARTURO_FIGUEROA_MENDEL", 
  "CI_JOEL", "CI_GEONATAN", "CI_LAURO", "PAS_MORENA", "CLEMENTE_NEYOY_YOCUPICIO", 
  "INDEP_ARNOLDO_JAVIER_RODRIGUEZ", "INDEP_PATRICIO_GARZA_TAPIA", 
  "JUAN_GABRIEL_ROMO_MURILLO", "GUSTAVO_FLORES_BETANZOS", 
  "GASTON_LUKEN_GARZA", "INDEP_RUBIO", "JESUS_MARISCAL_VARGAS", 
  "MANUEL_CANO_VILLALOBOS", "INDEP_MEDINA", "JOSE_GALLARDO_MOLINA", 
  "INDEP_ALEJANDRO_VINAY_MELGAR", "INDEP_MANUEL_HERNANDEZ_GONZALEZ_DRFOX", 
  "OSCAR_ANTONIO_VALDES_JIMENEZ", "INDEP_ARNE_AUSDEN_RUTHEN_HAAG", 
  "JESUS_BALDERAS", "JOSE_MENDOZA", "INDEP_JUAN_DIEGO_CASTRO_MORALES", 
  "INDEP_IGNACIO_TELLEZ_GONZALEZ", "MARIA_DEL_SOCORRO_VALDEZ", 
  "INDEP_CARLOS_ALBERTO_BLANCO", "INDEP_JUAN_CARLOS_BUSTAMANTE", 
  "INDEP_JESUS_OSWALDO_SILVA_MAGANA", "INDEP_ANTONIO_HORACIO_CRUZ", 
  "INDEP_JVJR", "INDEP_LAFG", "INDEP_MES", "INDEP_JABE", "INDEP_RAUL", 
  "INDEP_VICTOR", "INDEP_ISRAEL_GUERRERO_BOCANEGRA", "INDEP_CIZM", 
  "RUBEN_ARTURO_CHAVEZ_GARCIA", "NORBERTO_BARRAZA_ALMAZAN", 
  "GUSTAVO_COYOTZI_RODRIGUEZ", "ADAN_LIMA_GONZALEZ", "JOSE_ARMENTA_RAMOS", 
  "HECTOR_ZEPEDA", "INDEP_JONATHAN", "INDEP_CHRISTOPHER", "INDEP_EVM", 
  "INDEP_EMILIO", "INDEP_ALMANZA", "INDEP_COSSY", "JOSE_EZEQUIEL", 
  "INDEP_CID", "EVERARDO_CABAÑAS_SALCEDO", "HONESTTRABTRANSPAR", 
  "HILDE_SOSA", "RAUL_AVILA_GUILLEN", "MARCO_ANTONIO_VIZCARRA_CALDERON", 
  "CONCEPCION_TREJO_CARDOZA", "INDEP_HAAC", "CI_VICTOR", "CI_MANUEL", 
  "HECTOR_JUAN_SALCIDO_ALVAREZ", "CARLOS_ALBERTO_QUIROZ_ROMO", 
  "INDEP_CARLOS_ALBERTO_GUERRERO", "INDEP_MARGGID_ANTONIO_RODRIGUEZ"
)

# Make column names temporarily unique
names(mun_db) <- make.unique(names(mun_db))

mun_db <- mun_db %>%
  mutate(
    PFC = coalesce(PFC, `PFC.1`), # Combine the "PFC" columns
    PJ = coalesce(PJ, `PJ.1`)     # Combine the "PJ" columns
  ) %>%
  select(-`PFC.1`, -`PJ.1`) # Remove the duplicate columns

any(duplicated(names(mun_db))) # Should return FALSE

mun_db <- mun_db %>%
  mutate(
    INDEP = coalesce(!!!syms(indep_columns))
  ) %>%
  select(-all_of(indep_columns))



assign_incumbent_vote <- function(data) {
  
  # Initialize columns
  data <- data %>%
    mutate(mun_incumbent_vote = NA,
           mun_party_component = NA)
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    incumbent_party <- data$incumbent_party[I]
    
    # Skip if incumbent_party is NA or empty
    if (is.na(incumbent_party) || incumbent_party == "") next
    
    # Check if it is a coalition
    if (str_detect(incumbent_party, "_")) {
      parties <- unlist(str_split(incumbent_party, "_"))
      
      # Check if any individual party within the coalition is present in other columns
      for (party in parties) {
        # Differentiating between PAN and PANAL using word boundaries
        if (party == "PAN") {
          party_vars <- names(data)[str_detect(names(data), "\\bPAN\\b") & !str_detect(names(data), "PANAL")]
        } else if (party == "PANAL") {
          party_vars <- names(data)[str_detect(names(data), "\\bPANAL\\b")]
        } else {
          party_vars <- names(data)[str_detect(names(data), paste0("\\b", party, "\\b"))]
        }
        
        for (party_var in party_vars) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$mun_incumbent_vote[I] <- data[[party_var]][I]
            data$mun_party_component[I] <- party_var
            break
          }
        }
        if (!is.na(data$mun_incumbent_vote[I])) break
      }
      
      # If no individual party is found, proceed with coalition logic
      if (is.na(data$mun_incumbent_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) all(parties %in% str_split(x, "_")[[1]]))]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$mun_incumbent_vote[I] <- data[[coalition_var]][I]
            data$mun_party_component[I] <- coalition_var
            break
          }
        }
      }
    } else {
      # Handle single parties
      if (incumbent_party == "PAN") {
        party_vars <- names(data)[str_detect(names(data), "\\bPAN\\b") & !str_detect(names(data), "PANAL")]
      } else if (incumbent_party == "PANAL") {
        party_vars <- names(data)[str_detect(names(data), "\\bPANAL\\b")]
      } else {
        party_vars <- names(data)[str_detect(names(data), paste0("\\b", incumbent_party, "\\b"))]
      }
      
      for (party_var in party_vars) {
        if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
          data$mun_incumbent_vote[I] <- data[[party_var]][I]
          data$mun_party_component[I] <- party_var
          break
        }
      }
      
      # If no single party found, check coalitions containing the single party
      if (is.na(data$mun_incumbent_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) incumbent_party %in% str_split(x, "_")[[1]])]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$mun_incumbent_vote[I] <- data[[coalition_var]][I]
            data$mun_party_component[I] <- coalition_var
            break
          }
        }
      }
    }
  }
  
  return(data)
}

correct_runnerup_vote <- function(data) {
  
  # Initialize columns for storing results
  data <- data %>%
    mutate(mun_runnerup_vote = NA,
           mun_runnerup_party_component = NA)
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    runnerup_party <- data$runnerup_party[I]
    
    # Skip processing for NA or empty runnerup_party
    if (is.na(runnerup_party) || runnerup_party == "") next
    
    # Check if it is a coalition
    if (str_detect(runnerup_party, "_")) {
      parties <- unlist(str_split(runnerup_party, "_"))
      
      # Check if any individual party within the coalition is present in other columns
      for (party in parties) {
        # Differentiating between PAN and PANAL using word boundaries
        if (party == "PAN") {
          party_vars <- names(data)[str_detect(names(data), "\\bPAN\\b") & !str_detect(names(data), "PANAL")]
        } else if (party == "PANAL") {
          party_vars <- names(data)[str_detect(names(data), "\\bPANAL\\b")]
        } else {
          party_vars <- names(data)[str_detect(names(data), paste0("\\b", party, "\\b"))]
        }
        
        for (party_var in party_vars) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$mun_runnerup_vote[I] <- data[[party_var]][I]
            data$mun_runnerup_party_component[I] <- party_var
            break
          }
        }
        if (!is.na(data$mun_runnerup_vote[I])) break
      }
      
      # If no individual party is found, proceed with coalition logic
      if (is.na(data$mun_runnerup_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) all(parties %in% str_split(x, "_")[[1]]))]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$mun_runnerup_vote[I] <- data[[coalition_var]][I]
            data$mun_runnerup_party_component[I] <- coalition_var
            break
          }
        }
      }
    } else {
      # Handle single parties
      if (runnerup_party == "PAN") {
        party_vars <- names(data)[str_detect(names(data), "\\bPAN\\b") & !str_detect(names(data), "PANAL")]
      } else if (runnerup_party == "PANAL") {
        party_vars <- names(data)[str_detect(names(data), "\\bPANAL\\b")]
      } else {
        party_vars <- names(data)[str_detect(names(data), paste0("\\b", runnerup_party, "\\b"))]
      }
      
      for (party_var in party_vars) {
        if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
          data$mun_runnerup_vote[I] <- data[[party_var]][I]
          data$mun_runnerup_party_component[I] <- party_var
          break
        }
      }
      
      # If no single party found, check coalitions containing the single party
      if (is.na(data$mun_runnerup_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) runnerup_party %in% str_split(x, "_")[[1]])]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$mun_runnerup_vote[I] <- data[[coalition_var]][I]
            data$mun_runnerup_party_component[I] <- coalition_var
            break
          }
        }
      }
    }
  }
  
  return(data)
}

mun_db <- assign_incumbent_vote(mun_db)
mun_db <- correct_runnerup_vote(mun_db)
mun_db <- mun_db %>% 
  select(                             
          mun_code,                              
          year,
          mun_incumbent_vote,
          mun_party_component,
          mun_runnerup_vote,
          mun_runnerup_party_component
  ) %>% 
  mutate(
    mun_incumbent_vote = as.numeric(mun_incumbent_vote),
    mun_runnerup_vote = as.numeric(mun_incumbent_vote)
    )

final_data <- final_df %>% 
  left_join(mun_db, by = c("mun_code", "year")) 

final_data <- final_data %>% 
  rename(
    incumbent_party_candidate = incumbent_candidate,
    incumbent_party_vote = incumbent_vote,
    share_incumbent_party_valid_vote = share_incumbent_valid_vote,
    share_incumbent_party_registered_voters = share_incumbent_registered_voters,
    state_incumbent_party_vote = state_incumbent_vote,
    share_state_incumbent_party_valid_vote = share_state_incumbent_valid_vote,
    share_state_incumbent_party_registered_voters = share_state_incumbent_registered_voters,
    runnerup_party_candidate = runnerup_candidate,
    runnerup_party_vote = runnerup_vote,
    share_runnerup_party_valid_vote = share_runnerup_valid_vote,
    share_runnerup_party_registered_voters = share_runnerup_registered_voters,
    mun_incumbent_party_vote = mun_incumbent_vote,
    mun_incumbent_party_component = mun_party_component,
    mun_runnerup_party_vote = mun_runnerup_vote,
    researched_incumbent_party = researched_incumbent,
    source_researched_incumbent_party = source_researched_incumbent,
  )

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Final Data")
output_path <- file.path(output_dir, "all_states_final.csv")

# Use write_csv to save the file
write_csv(final_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path, "\n")

# Switch to the directory where the CSV is saved
old_dir <- getwd()
setwd(output_dir)

# Compress only the CSV file into a zip without any subfolder structure
zip::zipr("all_states_final.zip", files = "all_states_final.csv")

# Switch back to the original working directory
setwd(old_dir)

# Confirm compression
cat("File compressed at:", file.path(output_dir, "all_states_final.zip"), "\n")

# Delete the original CSV file
file.remove(output_path)

# Confirm deletion
cat("Original CSV file deleted:", output_path, "\n")

