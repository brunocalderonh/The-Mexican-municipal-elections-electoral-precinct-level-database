rm(list = ls())
library(tidyr)
library(dplyr)
library(haven)
library(openxlsx)
library(purrr)
library(readxl)
library(readr)
library(rstudioapi)

# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../../../"))

####MAGAR's incumbents ####
mag_db <- read.csv("Data/incumbent data/incumbent magar/aymu.incumbents-ags-jal.csv")

mag_db <- mag_db %>%
  filter(edon == 3)

#Mapping for INAFED
# Mapping for Municipio to the desired values
mun_mapping <- data.frame(
  Municipio = c("COMONDU", "MULEGE", "PAZ--LA", "CABOS--LOS", "LORETO"),
  New_Municipio = c("COMONDÚ", "MULEGÉ", "LA PAZ", "LOS CABOS", "LORETO")
)

# Create a unique mapping equivalence between Mun and inegi
uniquecodetemp <- mag_db %>%
  select(Municipio = mun, uniqueid = inegi) %>%
  distinct()

# Join with the mapping to get the new Municipio names
uniquecodetemp <- uniquecodetemp %>%
  left_join(mun_mapping, by = "Municipio") %>%
  mutate(Municipio = New_Municipio) %>%
  select(-New_Municipio)

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
output_dir <- file.path(getwd(), "Processed Data/bajasur/Incumbents")
output_path <- file.path(output_dir, "incumbent_magar.csv")

# Use write_csv to save the file
write_csv(mag_db, output_path)


####JL incumbent####
# Read the CSV file
jl_db <- read.csv("Data/incumbent data/incumbent JL/incumbent_JL.csv")

jl_db <- jl_db %>%
  filter(CVE_ENTIDAD == 3) %>%
  mutate(
    uniqueid = CVE_ENTIDAD * 1000 + CVE_MUNICIPIO,
    year = YEAR ,
    incumbent_candidate_JL = PRESIDENTE_MUNICIPAL,
  ) %>%
  select(uniqueid, year, PARTIDO, incumbent_candidate_JL, -PRESIDENTE_MUNICIPAL) %>%
  rename(incumbent_party_JL = PARTIDO)

jl_db<- jl_db %>%
  group_by(uniqueid, year) %>%
  summarise(
    incumbent_party_JL = first(incumbent_party_JL),
    incumbent_candidate_JL = first(incumbent_candidate_JL)
    )

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/bajasur/Incumbents")
output_path <- file.path(output_dir, "incumbent_JL.csv")

# Use write_csv to save the file
write_csv(jl_db, output_path)




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

vote_db <- read_csv("Processed Data/bajasur/bajasur_vote_manipulation.csv")

final_merged_data <- vote_db  %>%
  left_join(mag_db, by = c("uniqueid","year"))
final_merged_data <- final_merged_data %>% 
  left_join(jl_db, by = c("uniqueid","year")) 

final_merged_data <- final_merged_data %>% 
  select(state,mun,uniqueid,section,year,incumbent_party_magar,incumbent_candidate_magar,incumbent_party_JL,incumbent_candidate_JL,runnerup_party_magar,runnerup_candidate_magar,margin,everything())


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/bajasur")
output_path <- file.path(output_dir, "bajasur_incumbent_manipulator.csv")

# Use write_csv to save the file
write_csv(final_merged_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)


