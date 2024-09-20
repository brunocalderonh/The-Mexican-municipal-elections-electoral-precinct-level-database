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
mag_db <- read.csv("incumbents magar/aymu.incumbents-ags-jal.csv")

mag_db <- mag_db %>%
  filter(edon == 7)

#Mapping for INAFED
# Mapping for Municipio to the desired values
mun_mapping <- data.frame(
  Municipio = c("ACACOYAGUA", "ACALA", "ACAPETAHUA", "ALTAMIRANO", "AMATAN", 
                "AMATENANGO DE LA FRONTERA", "AMATENANGO DEL VALLE", "ANGEL ALBINO CORZO", 
                "ARRIAGA", "BEJUCAL DE OCAMPO", "BELLA VISTA", "BERRIOZABAL", "BOCHIL", 
                "BOSQUE--EL", "CACAHOATAN", "CATAZAJA", "CINTALAPA", "COAPILLA", 
                "COMITAN DE DOMINGUEZ", "CONCORDIA--LA", "COPAINALA", "CHALCHIHUITAN", 
                "CHAMULA", "CHANAL", "CHAPULTENANGO", "CHENALHO", "CHIAPA DE CORZO", 
                "CHIAPILLA", "CHICOASEN", "CHICOMUSELO", "CHILON", "ESCUINTLA", 
                "FRANCISCO LEON", "FRONTERA COMALAPA", "FRONTERA HIDALGO", "GRANDEZA--LA", 
                "HUEHUETAN", "HUIXTAN", "HUITIUPAN", "HUIXTLA", "INDEPENDENCIA--LA", 
                "IXHUATAN", "IXTACOMITAN", "IXTAPA", "IXTAPANGAJOYA", "JIQUIPILAS", 
                "JITOTOL", "JUAREZ", "LARRAINZAR", "LIBERTAD--LA", "MAPASTEPEC", 
                "MARGARITAS--LAS", "MAZAPA DE MADERO", "MAZATAN", "METAPA", "MITONTIC", 
                "MOTOZINTLA", "NICOLAS RUIZ", "OCOSINGO", "OCOTEPEC", "OCOZOCOAUTLA DE ESPINOSA", 
                "OSTUACAN", "OSUMACINTA", "OXCHUC", "PALENQUE", "PANTELHO", "PANTEPEC", 
                "PICHUCALCO", "PIJIJIAPAN", "PORVENIR--EL", "VILLA COMALTITLAN", 
                "PUEBLO NUEVO SOLISTAHUACAN", "RAYON", "REFORMA", "ROSAS--LAS", 
                "SABANILLA", "SALTO DE AGUA", "SAN CRISTOBAL DE LAS CASAS", "SAN FERNANDO", 
                "SILTEPEC", "SIMOJOVEL", "SITALA", "SOCOLTENANGO", "SOLOSUCHIAPA", 
                "SOYALO", "SUCHIAPA", "SUCHIATE", "SUNUAPA", "TAPACHULA", "TAPALAPA", 
                "TAPILULA", "TECPATAN", "TENEJAPA", "TEOPISCA", "TILA", "TONALA", 
                "TOTOLAPA", "TRINITARIA--LA", "TUMBALA", "TUXTLA GUTIERREZ", 
                "TUXTLA CHICO", "TUZANTAN", "TZIMOL", "UNION JUAREZ", "VENUSTIANO CARRANZA", 
                "VILLA CORZO", "VILLAFLORES", "YAJALON", "SAN LUCAS", "ZINACANTAN", 
                "SAN JUAN CANCUC", "ALDAMA", "BENEMERITO DE LAS AMERICAS", "MARAVILLA TENEJAPA", 
                "MARQUES DE COMILLAS", "MONTECRISTO DE GUERRERO", "SAN ANDRES DURAZNAL", 
                "SANTIAGO EL PINAR", "CAPITAN LUIS ANGEL VIDAL", "RINCON CHAMULA", 
                "PARRAL--EL", "EMILIANO ZAPATA", "MEZCALAPA", "HONDURAS DE LA SIERRA", 
                "BELISARIO DOMINGUEZ"),
  New_Municipio = c("ACACOYAGUA", "ACALA", "ACAPETAHUA", "ALTAMIRANO", "AMATÁN", 
                    "AMATENANGO DE LA FRONTERA", "AMATENANGO DEL VALLE", "ÁNGEL ALBINO CORZO", 
                    "ARRIAGA", "BEJUCAL DE OCAMPO", "BELLA VISTA", "BERRIOZÁBAL", "BOCHIL", 
                    "EL BOSQUE", "CACAHOATÁN", "CATAZAJÁ", "CINTALAPA", "COAPILLA", 
                    "COMITÁN DE DOMÍNGUEZ", "LA CONCORDIA", "COPAINALÁ", "CHALCHIHUITÁN", 
                    "CHAMULA", "CHANAL", "CHAPULTENANGO", "CHENALHÓ", "CHIAPA DE CORZO", 
                    "CHIAPILLA", "CHICOASÉN", "CHICOMUSELO", "CHILÓN", "ESCUINTLA", 
                    "FRANCISCO LEÓN", "FRONTERA COMALAPA", "FRONTERA HIDALGO", "LA GRANDEZA", 
                    "HUEHUETÁN", "HUIXTÁN", "HUITIUPÁN", "HUIXTLA", "LA INDEPENDENCIA", 
                    "IXHUATÁN", "IXTACOMITÁN", "IXTAPA", "IXTAPANGAJOYA", "JIQUIPILAS", 
                    "JITOTOL", "JUÁREZ", "LARRÁINZAR", "LA LIBERTAD", "MAPASTEPEC", 
                    "LAS MARGARITAS", "MAZAPA DE MADERO", "MAZATÁN", "METAPA", "MITONTIC", 
                    "MOTOZINTLA", "NICOLÁS RUÍZ", "OCOSINGO", "OCOTEPEC", "OCOZOCOAUTLA DE ESPINOSA", 
                    "OSTUACÁN", "OSUMACINTA", "OXCHUC", "PALENQUE", "PANTELHÓ", "PANTEPEC", 
                    "PICHUCALCO", "PIJIJIAPAN", "EL PORVENIR", "VILLA COMALTITLÁN", 
                    "PUEBLO NUEVO SOLISTAHUACÁN", "RAYÓN", "REFORMA", "LAS ROSAS", 
                    "SABANILLA", "SALTO DE AGUA", "SAN CRISTÓBAL DE LAS CASAS", "SAN FERNANDO", 
                    "SILTEPEC", "SIMOJOVEL", "SITALÁ", "SOCOLTENANGO", "SOLOSUCHIAPA", 
                    "SOYALÓ", "SUCHIAPA", "SUCHIATE", "SUNUAPA", "TAPACHULA", "TAPALAPA", 
                    "TAPILULA", "TECPATÁN", "TENEJAPA", "TEOPISCA", "TILA", "TONALÁ", 
                    "TOTOLAPA", "LA TRINITARIA", "TUMBALÁ", "TUXTLA GUTIÉRREZ", 
                    "TUXTLA CHICO", "TUZANTÁN", "TZIMOL", "UNIÓN JUÁREZ", "VENUSTIANO CARRANZA", 
                    "VILLA CORZO", "VILLAFLORES", "YAJALÓN", "SAN LUCAS", "ZINACANTÁN", 
                    "SAN JUAN CANCUC", "ALDAMA", "BENEMÉRITO DE LAS AMÉRICAS", "MARAVILLA TENEJAPA", 
                    "MARQUÉS DE COMILLAS", "MONTECRISTO DE GUERRERO", "SAN ANDRÉS DURAZNAL", 
                    "SANTIAGO EL PINAR", "CAPITÁN LUIS ÁNGEL VIDAL", "RINCÓN CHAMULA SAN PEDRO", 
                    "EL PARRAL", "EMILIANO ZAPATA", "MEZCALAPA", "HONDURAS DE LA SIERRA", 
                    NA)
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
  select(uniqueid, year, incumbent_party_magar, incumbent_candidate_magar, runnerup_party_magar, runnerup_candidate_magar, margin) %>%
  mutate(incumbent_party_magar = toupper(incumbent_party_magar)) %>% 
  mutate(runnerup_party_magar = toupper(runnerup_party_magar))

write_dta(mag_db, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/chiapas/Incumbents/incumbent_magar.dta")


#### INAFED incumbent ####
# Read the Excel file
inafed_db <- read_excel("incumbent INAFED/incumbents_chiapas_inafed.xlsx")

# Step 5: Convert the Municipio column to uppercase
inafed_db <- inafed_db %>%
  mutate(Municipio = toupper(Municipio))

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
  select(uniqueid, year, incumbent_party_inafed, incumbent_candidate_inafed)

# Ensure the year column is numeric
inafed_db <- inafed_db %>%
  mutate(year = as.numeric(year))

# Update the year values according to the specified mappings
inafed_db <- inafed_db %>%
  mutate(year = case_when(
    year == 1996 ~ 1995,
    year == 1999 ~ 1998,
    year == 2002 ~ 2001,
    year == 2005 ~ 2004,
    year == 2008 ~ 2007,
    year == 2011 ~ 2012,
    year == 1993 ~ 1992,
    year == 2019 ~ 2018,
    year == 1994 ~ 1995,
    year == 1997 ~ 1998,
    year == 2000 ~ 2001,
    year == 2006 ~ 2007,
    TRUE ~ year
  ))

inafed_db <- inafed_db %>%
  group_by(uniqueid, year) %>%
  summarize(across(everything(), ~first(.), .names = "{.col}")) %>%
  ungroup()

write_dta(inafed_db, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/chiapas/Incumbents/incumbent_inafed.dta")

####JL incumbent####
# Read the CSV file
jl_db <- read.csv("incumbent JL/incumbent_JL.csv")

# Filter rows where CVE_ENTIDAD == 1, mutate inegi and election_year, and rename PATIDO
jl_db <- jl_db %>%
  filter(CVE_ENTIDAD == 7) %>%
  mutate(
    uniqueid = CVE_ENTIDAD * 1000 + CVE_MUNICIPIO,
    year = FIRST_YEAR_PERIOD - 1 ,
    incumbent_candidate_JL = PRESIDENTE_MUNICIPAL
  ) %>%
  select(uniqueid, year, PARTIDO, incumbent_candidate_JL, -PRESIDENTE_MUNICIPAL) %>%
  rename(incumbent_party_JL = PARTIDO)

jl_db<- jl_db %>%
  group_by(uniqueid, year) %>%
  summarise(incumbent_party_JL = first(incumbent_party_JL),
            incumbent_candidate_JL = first(incumbent_candidate_JL))

jl_db <- jl_db %>%
  mutate(year = case_when(
    year == 2011 ~ 2012,
    year == 2014 ~ 2015,
    year == 2017 ~ 2018,
    year == 1993 ~ 1992,
    year == 1994 ~ 1995,
    year == 2005 ~ 2004,
    TRUE ~ year
  ))


# jl_db <- jl_db %>%
#   filter(FIRST_YEAR_PERIOD != 1991, election_year %in% valid_election_years) %>%
#   rename(incumbent_party_JL = PARTIDO) 


write_dta(jl_db, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/chiapas/Incumbents/incumbent_JL.dta")


####Horacio incumbent####
horacio_db <- read_dta("incumbent Horacio/incumbent_Horacio.dta")
horacio_db <- horacio_db  %>%
  filter(state == 7) 
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


write_dta(horacio_db, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/chiapas/Incumbents/incumbent_horacio.dta")


#### MERGE INCUMBENT DATA ####

mag_db <- mag_db %>%
  mutate(across(c(uniqueid, year), as.numeric))

inafed_db <- inafed_db %>%
  mutate(across(c(uniqueid, year), as.numeric))

merged_incumbent_data <- mag_db %>%
  left_join(horacio_db, by = c("uniqueid", "year"), suffix = c("_magar","_Horacio" )) 


repeated_observations1 <- merged_incumbent_data %>%
  group_by(uniqueid, year) %>%
  summarize(count = n()) %>%
  filter(count > 1)

merged_incumbent_data <- merged_incumbent_data %>%
  left_join(jl_db, by = c("uniqueid", "year"), suffix = c("", "_JL")) 


repeated_observations2 <- merged_incumbent_data %>%
  group_by(uniqueid, year) %>%
  summarize(count = n()) %>%
  filter(count > 1)

merged_incumbent_data <- merged_incumbent_data %>%
  left_join(inafed_db, by = c("uniqueid", "year"), suffix = c("", "_inafed"))

repeated_observations3 <- merged_incumbent_data %>%
  group_by(uniqueid, year) %>%
  summarize(count = n()) %>%
  filter(count > 1)


# Remove all three specific duplicates
merged_incumbent_data <- merged_incumbent_data %>%
  filter(
    !(uniqueid == 7009 & year == 2018 & incumbent_candidate_JL == "JOSE ALFREDO TOLEDO BLAS (SUPLENTE)") &
      !(uniqueid == 7011 & year == 2004 & incumbent_candidate_JL == "RABINDRANATH SALAZAR SOLORIO") &
      !(uniqueid == 7023 & year == 2015 & incumbent_candidate_JL == "MATEO GOMEZ GOMEZ")
  )

# Check if all duplicates have been removed
repeated_observations_after_removal <- merged_incumbent_data %>%
  group_by(uniqueid, year) %>%
  summarize(count = n()) %>%
  filter(count > 1)

write_dta(merged_incumbent_data, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/chiapas/Incumbents/incumbent_data_merged.dta")

#### MERGE INTO FINAL DB - INCUMBENT + VOTE ####
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/chiapas/")

vote_db <- read_dta("chiapas_vote.dta")

final_merged_data <- vote_db  %>%
  left_join(merged_incumbent_data, by = c("uniqueid", "year"))

#shift part values one period foward
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


write_dta(final_merged_data, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/States/chiapas/chiapas_merged_IncumbentVote.dta")


