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
mag_db <- read.csv("Data/incumbent data/incumbent magar/aymu.incumbents-ags-jal.csv")

mag_db <- mag_db %>%
  filter(edon == 7 & yr < 2020)

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
  mutate(uniqueid = as.numeric(uniqueid)) %>% 
  select(uniqueid, year, incumbent_party_magar, incumbent_candidate_magar, runnerup_party_magar, runnerup_candidate_magar, margin) %>%
  mutate(incumbent_party_magar = toupper(incumbent_party_magar)) %>% 
  mutate(runnerup_party_magar = toupper(runnerup_party_magar))

### New Magar incumbents
newmag <- read.csv("Data/incumbent data/new incumbent magar/aymu1989-on.incumbents.csv") %>% 
  filter(edon == 7 & yr >= 2020) %>% 
  mutate(across(c(part, part2nd), ~ gsub("-", "_", .))) %>%
  rename( incumbent_party_magar = part) %>%
  rename( incumbent_candidate_magar = incumbent) %>%
  rename( runnerup_party_magar = part2nd) %>%
  rename( runnerup_candidate_magar = runnerup) %>%
  rename( margin = mg) %>% 
  rename( year = yr) %>%
  rename(uniqueid = inegi)  %>%
  mutate(uniqueid = as.numeric(uniqueid),
         margin = as.numeric(margin)) %>% 
  select(uniqueid, year, incumbent_party_magar, incumbent_candidate_magar, runnerup_party_magar, runnerup_candidate_magar, margin) %>%
  mutate(incumbent_party_magar = toupper(incumbent_party_magar)) %>% 
  mutate(runnerup_party_magar = toupper(runnerup_party_magar)) 

# Magar complete
mag_db <- bind_rows(mag_db, newmag)

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/chiapas/Incumbents")
output_path <- file.path(output_dir, "incumbent_magar.csv")

# Use write_csv to save the file
write_csv(mag_db, output_path)



####JL incumbent####
# Read the CSV file
jl_db <- read.csv("Data/incumbent data/incumbent JL/incumbent_JL.csv")
# Filter rows where CVE_ENTIDAD == 1, mutate inegi and election_year, and rename PATIDO
jl_db <- jl_db %>%
  filter(CVE_ENTIDAD == 7) %>%
  mutate(
    uniqueid = CVE_ENTIDAD * 1000 + CVE_MUNICIPIO,
    year = YEAR ,
    incumbent_candidate_JL = PRESIDENTE_MUNICIPAL
  ) %>%
  select(uniqueid, year, PARTIDO, incumbent_candidate_JL, -PRESIDENTE_MUNICIPAL) %>%
  rename(incumbent_party_JL = PARTIDO)

jl_db<- jl_db %>%
  group_by(uniqueid, year) %>%
  summarise(incumbent_party_JL = first(incumbent_party_JL),
            incumbent_candidate_JL = first(incumbent_candidate_JL))

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/chiapas/Incumbents")
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


vote_db <- read_csv("Processed Data/chiapas/chiapas_vote_manipulation.csv")

final_merged_data <- vote_db  %>%
  left_join(mag_db, by = c("uniqueid","year"))
final_merged_data <- final_merged_data %>% 
  left_join(jl_db, by = c("uniqueid","year")) 

final_merged_data <- final_merged_data %>% 
  select(state,mun,uniqueid,section,year,incumbent_party_magar,incumbent_candidate_magar,incumbent_party_JL,incumbent_candidate_JL,runnerup_party_magar,runnerup_candidate_magar,margin,everything())

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/chiapas")
output_path <- file.path(output_dir, "chiapas_incumbent_manipulator.csv")

# Use write_csv to save the file
write_csv(final_merged_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)
