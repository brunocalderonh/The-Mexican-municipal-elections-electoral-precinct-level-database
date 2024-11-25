rm(list = ls())
library(tidyr)
library(dplyr)
library(haven)
library(openxlsx)
library(purrr)
library(readxl)
library(rstudioapi)
library(readr)

# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../../../"))

####MAGAR's incumbents ####
mag_db <- read.csv("Data/incumbent data/incumbent magar/aymu.incumbents-que-zac.csv")


mag_db <- mag_db %>%
  filter(edon == 27)

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
  mutate(uniqueid = as.numeric(uniqueid)) %>%
  select(uniqueid, year, incumbent_party_magar, incumbent_candidate_magar, runnerup_party_magar, runnerup_candidate_magar, margin) %>%
  mutate(incumbent_party_magar = toupper(incumbent_party_magar)) %>% 
  mutate(runnerup_party_magar = toupper(runnerup_party_magar))


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/tabasco/Incumbents")
output_path <- file.path(output_dir, "incumbent_magar.csv")

# Use write_csv to save the file
write_csv(mag_db, output_path)


#### INAFED incumbent ####
# Read the Excel file
inafed_db <- read_excel("Data/incumbent data/incumbent INAFED/incumbents_tabasco_inafed.xlsx")


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
  group_by(uniqueid, year) %>%
  summarize(across(everything(), ~first(.), .names = "{.col}")) %>%
  mutate(uniqueid = as.numeric(uniqueid)) %>% 
  mutate(year = as.numeric(year)) %>% 
  ungroup()


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/tabasco/Incumbents")
output_path <- file.path(output_dir, "incumbent_inafed.csv")

# Use write_csv to save the file
write_csv(inafed_db, output_path)



####JL incumbent####
# Read the CSV file
jl_db <- read.csv("Data/incumbent data/incumbent JL/incumbent_JL.csv")


jl_db <- jl_db %>%
  filter(CVE_ENTIDAD == 27) %>%
  mutate(
    uniqueid = CVE_ENTIDAD * 1000 + CVE_MUNICIPIO,
    incumbent_candidate_JL = PRESIDENTE_MUNICIPAL,
    year = YEAR,
  )  %>%
  select(uniqueid, year, PARTIDO, incumbent_candidate_JL, -PRESIDENTE_MUNICIPAL) %>%
  rename(incumbent_party_JL = PARTIDO)

jl_db<- jl_db %>%
  group_by(uniqueid, year) %>%
  summarise(incumbent_party_JL = first(incumbent_party_JL),
            incumbent_candidate_JL = first(incumbent_candidate_JL))


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/tabasco/Incumbents")
output_path <- file.path(output_dir, "incumbent_JL.csv")

# Use write_csv to save the file
write_csv(jl_db, output_path)



####Horacio incumbent####
# Path to the .zip file
zip_file <- "Data/incumbent data/incumbent Horacio/incumbent_Horacio.dta.zip"

# Unzip the file to a temporary directory
temp_dir <- tempdir()  # Create a temporary directory
unzip(zip_file, exdir = temp_dir)  # Extract the contents to the temp directory

# Find the .dta file within the temp directory
unzipped_file <- file.path(temp_dir, "incumbent_Horacio.dta")

# Now read the unzipped .dta file from the temporary directory
horacio_db <- read_dta(unzipped_file)
horacio_db <- horacio_db  %>%
  filter(state == 27) 
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

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/tabasco/Incumbents")
output_path <- file.path(output_dir, "incumbent_horacio.csv")

# Use write_csv to save the file
write_csv(horacio_db, output_path)


#### MERGE INTO FINAL DB - INCUMBENT + VOTE ####
mag_db <- mag_db %>%
  group_by(uniqueid) %>%
  arrange(year) %>%
  mutate(incumbent_party_magar = lag(incumbent_party_magar, 1)) %>%
  mutate(runnerup_party_magar = lag(runnerup_party_magar, 1)) %>%
  mutate(incumbent_candidate_magar = lag(incumbent_candidate_magar, 1)) %>%
  mutate(runnerup_candidate_magar = lag(runnerup_candidate_magar, 1)) %>%
  mutate(margin = lag(margin, 1)) %>%
  rename("manipulated_year" = "year") %>% 
  ungroup()

horacio_db <- horacio_db %>%
  group_by(uniqueid) %>%
  arrange(year) %>%
  mutate(incumbent_party_Horacio = lag(incumbent_party_Horacio, 1))  %>%
  rename("manipulated_year" = "year") %>% 
  ungroup()

inafed_db <- inafed_db %>%
  group_by(uniqueid) %>%
  arrange(year) %>%
  mutate(incumbent_party_inafed = lag(incumbent_party_inafed , 1)) %>%
  mutate(incumbent_candidate_inafed  = lag(incumbent_candidate_inafed , 1))  %>%
  rename("manipulated_year" = "year") %>% 
  ungroup()

jl_db <- jl_db %>%
  rename("manipulated_year" = "year")



vote_db <- read_csv("Processed Data/tabasco/tabasco_vote_manipulation.csv")


# Add `normal_year` for observations with "EXTRAORDINARIO" in `mun`
vote_db <- vote_db %>%
  mutate(
    manipulated_year = ifelse(extra == 1, year - 1, year)
  )

final_merged_data <- vote_db  %>%
  left_join(mag_db, by = c("uniqueid","manipulated_year"))
final_merged_data <- final_merged_data %>% 
  left_join(jl_db, by = c("uniqueid","manipulated_year")) 

final_merged_data <- final_merged_data %>% 
  left_join(inafed_db, by = c("uniqueid","manipulated_year")) 
final_merged_data <- final_merged_data %>% 
  left_join(horacio_db, by = c("uniqueid","manipulated_year")) 

final_merged_data <- final_merged_data %>% 
  select(state,mun,uniqueid,section,year,-manipulated_year,-extra,incumbent_party_magar,incumbent_candidate_magar,incumbent_party_JL,incumbent_candidate_JL,incumbent_party_inafed,incumbent_candidate_inafed,incumbent_party_Horacio,runnerup_party_magar,runnerup_candidate_magar,margin,everything())


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/tabasco")
output_path <- file.path(output_dir, "tabasco_incumbent_manipulator.csv")

# Use write_csv to save the file
write_csv(final_merged_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)

