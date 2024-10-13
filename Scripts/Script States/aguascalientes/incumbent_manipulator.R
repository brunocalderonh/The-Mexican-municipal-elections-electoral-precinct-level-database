rm(list = ls())

library(tidyr)
library(dplyr)
library(haven)
library(openxlsx)
library(purrr)
library(rstudioapi)
library(readxl)

# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../../../"))

####MAGAR's incumbents ####
mag_db <- read.csv("Data/incumbent data/incumbent magar/aymu.incumbents-ags-jal.csv")

mag_db <- mag_db %>%
  filter(edon == 1)

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
output_dir <- file.path(getwd(), "Processed Data/aguascalientes/Incumbents")
output_path <- file.path(output_dir, "incumbent_magar.csv")

# Use write_csv to save the file
write_csv(mag_db, output_path)

#### INAFED incumbent ####
# Read the Excel file
inafed_db <- read_excel("Data/incumbent data/incumbent INAFED/incumbents_aguascalientes_inafed.xlsx")

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

# Define the mapping for gov_period to election_year
mapping <- c(
  "1987_1989" = 1986, "1990_1991" = 1989, "1993_1995" = 1992, "1996_1998" = 1995, 
  "1999_2001" = 1998, "2002_2004" = 2001, "2005_2007" = 2004, "2008_2010" = 2007, 
  "2011_2013" = 2010, "2014_2016" = 2013, "2017_2019" = 2016, "2019_2021" = 2019, 
  "2021_2024" = 2021
)

# Filter out unwanted periods
valid_periods <- names(mapping)
inafed_db <- inafed_db %>%
  filter(gov_period %in% valid_periods) %>%
  mutate(election_year = mapping[gov_period]) %>%
  rename(incumbent_party_inafed = Partido) %>%
  rename(incumbent_candidate_inafed = `Presidente Municipal`) %>%
  rename(year = election_year) %>%
  select(uniqueid, year, incumbent_party_inafed, incumbent_candidate_inafed)

# Assuming jl_db is your data frame
inafed_db <- inafed_db[-18, ]

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/aguascalientes/Incumbents")
output_path <- file.path(output_dir, "incumbent_inafed.csv")

# Use write_csv to save the file
write_csv(inafed_db, output_path)

####JL incumbent####
# Read the CSV file
jl_db <- read.csv("Data/incumbent data/incumbent JL/incumbent_JL.csv")

# List of valid election years
valid_election_years <- c(1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2021, 2024)

# Filter rows where CVE_ENTIDAD == 1, mutate inegi and election_year, and rename PATIDO
jl_db <- jl_db %>%
  filter(CVE_ENTIDAD == 1) %>%
  mutate(
    uniqueid = CVE_ENTIDAD * 1000 + CVE_MUNICIPIO,
    year = YEAR,
    incumbent_candidate_JL = PRESIDENTE_MUNICIPAL,
  ) %>%
  select(uniqueid, year, PARTIDO, incumbent_candidate_JL, -PRESIDENTE_MUNICIPAL) %>%
  rename(incumbent_party_JL = PARTIDO)
# Change the values of year 2020 to 2021 and 2018 to 2019
jl_db <- jl_db %>%
  mutate(year = case_when(
    year == 2020 ~ 2021,
    year == 2018 ~ 2019,
    TRUE ~ year
  ))

jl_db <- jl_db %>%
  group_by(uniqueid, year) %>%
  summarise(
    incumbent_party_JL = first(incumbent_party_JL),
    incumbent_candidate_JL = first(incumbent_candidate_JL)
  )

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/aguascalientes/Incumbents")
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

#horacio_db <- read_dta("Data/incumbent data/incumbent Horacio/incumbent_Horacio.dta")
horacio_db <- horacio_db  %>%
  filter(state == 1) 
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

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/aguascalientes/Incumbents")
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
  ungroup() 

horacio_db <- horacio_db %>%
  group_by(uniqueid) %>%
  arrange(year) %>%
  mutate(incumbent_party_Horacio = lag(incumbent_party_Horacio, 1)) %>%
  ungroup() 

inafed_db <- inafed_db %>%
  group_by(uniqueid) %>%
  arrange(year) %>%
  mutate(incumbent_party_inafed = lag(incumbent_party_inafed , 1)) %>%
  mutate(incumbent_candidate_inafed  = lag(incumbent_candidate_inafed , 1)) %>%
  ungroup() 

vote_db <- read_csv("Processed Data/aguascalientes/aguascalientes_vote.csv")

final_merged_data <- vote_db  %>%
  left_join(mag_db, by = c("uniqueid","year"))
final_merged_data <- final_merged_data %>% 
  left_join(jl_db, by = c("uniqueid","year")) 

final_merged_data <- final_merged_data %>% 
  left_join(inafed_db, by = c("uniqueid","year")) 
final_merged_data <- final_merged_data %>% 
  left_join(horacio_db, by = c("uniqueid","year","section")) 
  
final_merged_data <- final_merged_data %>% 
  select(state,mun,uniqueid,section,year,incumbent_party_magar,incumbent_candidate_magar,incumbent_party_JL,incumbent_candidate_JL,incumbent_party_inafed,incumbent_candidate_inafed,incumbent_party_Horacio,runnerup_party_magar,runnerup_candidate_magar,margin,everything())

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/aguascalientes")
output_path <- file.path(output_dir, "aguascalientes_merged_IncumbentVote.csv")

# Use write_csv to save the file
write_csv(final_merged_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)
