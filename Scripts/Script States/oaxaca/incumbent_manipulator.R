rm(list = ls())
library(tidyr)
library(dplyr)
library(haven)
library(openxlsx)
library(purrr)
library(readxl)

#### Set Working Directory ####
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/Incumbents/")

####MAGAR's incumbents ####
mag_db <- read.csv("incumbents magar/aymu.incumbents-mex-pue.csv")

mag_db <- mag_db %>%
  filter(edon == 20)

# Get unique municipality names from mag_db
unique_municipios_mag <- unique(mag_db$mun)

# Construct initial mun_mapping data frame
mun_mapping <- data.frame(
  Municipio = unique_municipios_mag
)

# Create a unique mapping equivalence between Mun and inegi
uniquecodetemp <- mag_db %>%
  select(Municipio = mun, uniqueid = inegi) %>%
  distinct()

# Replace '-' with '_' for incumbent and runnerup
#rename variables
mag_db <- mag_db %>%
  mutate(across(c(part, part2nd), ~ gsub("-", "_", .))) %>%
  rename( incumbent_party_magar = part) %>%
  rename( incumbent_candidate_magar = incumbent) %>%
  rename( runnerup_party_magar = part2nd) %>%
  rename( runnerup_candidate_magar = runnerup) %>%
  rename( margin = mg) %>% 
  rename( year = yr) %>%
  rename(uniqueid = inegi)  %>%
  select(uniqueid, year, incumbent_party_magar, incumbent_candidate_magar, runnerup_party_magar, runnerup_candidate_magar, margin) %>%
  mutate(incumbent_party_magar = toupper(incumbent_party_magar)) %>% 
  mutate(runnerup_party_magar = toupper(runnerup_party_magar))

write_dta(mag_db, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/oaxaca/Incumbents/incumbent_magar.dta")


#### INAFED incumbent ####
# Read the Excel file
inafed_db <- read_excel("incumbent INAFED/incumbents_oaxaca_inafed.xlsx")

# Step 5: Convert the Municipio column to uppercase
inafed_db <- inafed_db %>%
  mutate(Municipio = toupper(Municipio))

# Get unique municipality names from inafed_db
unique_municipios_inafed <- unique(inafed_db$Municipio)

## Determine the maximum length of the unique municipalities
max_length <- max(length(unique_municipios_mag), length(unique_municipios_inafed))

# Pad the shorter vector with NAs
unique_municipios_mag <- c(unique_municipios_mag, rep(NA, max_length - length(unique_municipios_mag)))
unique_municipios_inafed <- c(unique_municipios_inafed, rep(NA, max_length - length(unique_municipios_inafed)))

# Construct the mun_mapping data frame
mun_mapping <- data.frame(
  Municipio = unique_municipios_mag,
  New_Municipio = unique_municipios_inafed
)

# Join with the mapping to get the new Municipio names
uniquecodetemp <- uniquecodetemp %>%
  left_join(mun_mapping, by = "Municipio") %>%
  mutate(Municipio = New_Municipio) %>%
  select(-New_Municipio)

# Step 6: Merge the inafed_db dataframe with the uniquecodetemp dataframe
inafed_db <- inafed_db %>%
  left_join(uniquecodetemp, by = "Municipio")

# Function to clean the Periodo column
clean_periodo <- function(periodo) {
  # Regex patterns
  pattern_range <- "\\d{4}"
  pattern_single <- "'\\d{2}|\\d{4}"
  
  # If it's a range
  if (grepl("al", periodo) | grepl("a", periodo)) {
    years <- unlist(regmatches(periodo, gregexpr(pattern_range, periodo)))
    if (length(years) == 2) {
      return(paste(years, collapse = "_"))
    }
  }
  
  # If it's a single year
  years <- unlist(regmatches(periodo, gregexpr(pattern_single, periodo)))
  if (length(years) == 1) {
    year <- years[1]
    return(ifelse(nchar(year) == 4, year, paste0("19", substr(year, 2, 3))))
  }
  
  return(periodo)
}

# Apply function to clean the Periodo column and rename it to gov_period
inafed_db$gov_period <- sapply(inafed_db$Periodo, clean_periodo)
inafed_db$Periodo <- NULL

# Create a function to extract the first numeric value before "_"
extract_election_year <- function(gov_period) {
  # Extract the first numeric part of the string before "_"
  str_extract(gov_period, "^\\d+")
}

# Apply the function to the 'gov_period' column and create a new column 'election_year'
inafed_db <- inafed_db %>%
  mutate(election_year = extract_election_year(gov_period))

# Filter out unwanted periods

inafed_db <- inafed_db %>%
  #filter(gov_period %in% valid_periods) %>%
  rename(incumbent_party_inafed = Partido) %>%
  rename(incumbent_candidate_inafed = `Presidente Municipal`) %>%
  rename(year = election_year) %>%
  select(uniqueid, year, incumbent_party_inafed,incumbent_candidate_inafed) %>%
  mutate(year = as.numeric(as.character(year)))

inafed_db <- inafed_db %>%
  mutate(year = case_when(
    year == 1997 ~ 1998,
    year == 2002 ~ 2001,
    year == 2000 ~ 2001,
    year == 2005 ~ 2004,
    year == 2008 ~ 2007,
    year == 2010 ~ 2011,
    year == 2015 ~ 2016,
    TRUE ~ year
  ))

inafed_db <- inafed_db %>%
  group_by(uniqueid, year) %>%
  summarize(across(everything(), ~first(.), .names = "{.col}")) %>%
  ungroup()

write_dta(inafed_db, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/oaxaca/Incumbents/incumbent_inafed.dta")


####JL incumbent####
# Read the CSV file
jl_db <- read.csv("incumbent JL/incumbent_JL.csv")

jl_db <- jl_db %>%
  filter(CVE_ENTIDAD == 20) %>%
  mutate(
    uniqueid = CVE_ENTIDAD * 1000 + CVE_MUNICIPIO,
    incumbent_candidate_JL = PRESIDENTE_MUNICIPAL,
    year = case_when(
      LAST_YEAR_PERIOD - FIRST_YEAR_PERIOD == 3 ~ LAST_YEAR_PERIOD - 3,
      LAST_YEAR_PERIOD - FIRST_YEAR_PERIOD == 2 ~ LAST_YEAR_PERIOD - 3,
      LAST_YEAR_PERIOD - FIRST_YEAR_PERIOD == 1 ~ LAST_YEAR_PERIOD - 3,
      TRUE ~ NA_real_  # Handle other cases if necessary
    )
  )  %>%
  select(uniqueid, year, PARTIDO, incumbent_candidate_JL, -PRESIDENTE_MUNICIPAL) %>%
  rename(incumbent_party_JL = PARTIDO)

jl_db<- jl_db %>%
  group_by(uniqueid, year) %>%
  summarise(incumbent_party_JL = first(incumbent_party_JL),
            incumbent_candidate_JL = first(incumbent_candidate_JL))

write_dta(jl_db, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/oaxaca/Incumbents/incumbent_JL.dta")


####Horacio incumbent####
horacio_db <- read_dta("incumbent Horacio/incumbent_Horacio.dta")
horacio_db <- horacio_db  %>%
  filter(state == 20) 
# Collapse the data by uniqueid and year, keeping the first occurrence of each combination
horacio_db<- horacio_db %>%
  group_by(section, uniqueid, year) %>%
  rename(incumbent_party_Horacio = incumbent) %>%
  select(uniqueid, year, incumbent_party_Horacio)

#shift part values one period backward
horacio_db <- horacio_db %>%
  group_by(section, uniqueid) %>%
  arrange(year) %>%
  mutate(incumbent_party_Horacio = lead(incumbent_party_Horacio, 1)) %>%
  ungroup()

horacio_db <- horacio_db %>%
  group_by(uniqueid, year) %>%
  summarize(incumbent_party_Horacio = first(incumbent_party_Horacio)) %>% 
  as.data.frame()

write_dta(horacio_db, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/oaxaca/Incumbents/incumbent_horacio.dta")



#### MERGE INCUMBENT DATA ####

mag_db <- mag_db %>%
  mutate(across(c(uniqueid, year), as.numeric))

inafed_db <- inafed_db %>%
  mutate(across(c(uniqueid, year), as.numeric))

merged_incumbent_data <- mag_db %>%
 left_join(horacio_db, by = c("uniqueid", "year"), suffix = c("_magar","_Horacio" )) %>%
  left_join(jl_db, by = c("uniqueid", "year"), suffix = c("", "_JL")) %>%
  left_join(inafed_db, by = c("uniqueid", "year"), suffix = c("", "_inafed"))


write_dta(merged_incumbent_data, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/oaxaca/Incumbents/incumbent_data_merged.dta")

#### MERGE INTO FINAL DB - INCUMBENT + VOTE ####
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/oaxaca/")

vote_db <- read_dta("oaxaca_vote.dta")

final_merged_data <- vote_db  %>%
  left_join(merged_incumbent_data, by = c("uniqueid", "year"))

#shift part values one period  foward
final_merged_data <- final_merged_data %>%
  group_by(section, uniqueid) %>%
  arrange(year) %>%
  mutate(incumbent_party_Horacio = lag(incumbent_party_Horacio, 1)) %>%
  mutate(incumbent_party_JL = lag(incumbent_party_JL, 1)) %>%
  mutate(incumbent_party_magar = lag(incumbent_party_magar, 1)) %>%
  mutate(runnerup_party_magar = lag(runnerup_party_magar, 1)) %>%
  mutate(incumbent_party_inafed = lag(incumbent_party_inafed, 1)) %>%
  mutate(incumbent_candidate_JL = lag(incumbent_candidate_JL, 1)) %>%
  mutate(incumbent_candidate_magar = lag(incumbent_candidate_magar, 1)) %>%
  mutate(runnerup_candidate_magar = lag(runnerup_candidate_magar, 1)) %>%
  mutate(incumbent_candidate_inafed = lag(incumbent_candidate_inafed, 1)) %>%
  mutate(margin = lag(margin, 1)) %>%
  ungroup()

write_dta(final_merged_data, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/oaxaca/oaxaca_merged_IncumbentVote.dta")


