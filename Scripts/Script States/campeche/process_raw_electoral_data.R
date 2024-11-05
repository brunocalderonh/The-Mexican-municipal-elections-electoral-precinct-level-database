# Basic setup
rm(list = ls())          # clear all objects in memory
#dev.off()                  # reload graphic device
cat("\014")                # clear console
options(max.print = 5000)  # expand display
options(scipen=10)
# Load packages
if (!require("pacman")) install.packages("pacman")  # load packages

pacman::p_load (dplyr
                , haven
                , readstata13
                , readxl
                , tidyverse
                , tidyr
                , openxlsx
                , data.table)

# Set working directory
# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, ""))

##############################################
### PROCESSING DATA FOR 1997
##############################################

# Load the 1997 data
data_1997 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997_No_LN.xlsx")
names(data_1997)
# Rename columns for consistency
data_1997 <- data_1997 %>%
  dplyr::rename(
    municipality = MUNICIPIO,
    section = SECCION)

# Create 'total' column summing party votes
data_1997 <- data_1997 %>%
  dplyr::mutate(total = rowSums(across(c(PAN, PRI, PRD, PC, PT, PVEM, PPS, PDM)), na.rm = TRUE))

# Filter out rows where 'total' is missing or zero
data_1997 <- data_1997 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_1997 <- data_1997 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN, PRI, PRD, PC, PT, PVEM, PPS, PDM, total), sum, na.rm = TRUE))

# Rename columns to match expected structure
collapsed_1997 <- collapsed_1997 %>%
  dplyr::rename(
    PartCardenista = PC,
    PD = PDM)

# Generate 'uniqueid' based on municipality names
collapsed_1997 <- collapsed_1997 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "Calkiní" ~ 4001,
      municipality == "Campeche" ~ 4002,
      municipality == "Carmen" ~ 4003,
      municipality == "Champotón" ~ 4004,
      municipality == "Hecelchakán" ~ 4005,
      municipality == "Hopelchén" ~ 4006,
      municipality == "Palizada" ~ 4007,
      municipality == "Tenabo" ~ 4008,
      municipality == "Escárcega" ~ 4009,
      municipality == "Calakmul" ~ 4010
    )
  )

# Generate 'valid' votes by summing all relevant party votes
collapsed_1997 <- collapsed_1997 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD, PartCardenista, PT, PVEM, PPS, PD)), na.rm = TRUE))

# Load and merge Lista Nominal data
ln_all_months_years <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

ln_all_months_years <- ln_all_months_years %>%
  dplyr::filter(month == "July" & year == 1997 & state == "CAMPECHE") %>%
  dplyr::select(section, lista)

collapsed_1997 <- collapsed_1997 %>%
  dplyr::left_join(ln_all_months_years, by = c("section")) %>% 
  dplyr::rename(listanominal=lista)

# Calculate 'turnout'
collapsed_1997 <- collapsed_1997 %>%
  dplyr::mutate(turnout = total / listanominal,
                month = "July", 
                year = 1997)

rm(data_1997)
rm(ln_all_months_years)
##############################################
### PROCESSING DATA FOR 2000
##############################################

# Load the 2000 data
data_2000 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2000_No_LN.xlsx")
names(data_2000)
# Rename columns for consistency
data_2000 <- data_2000 %>%
  dplyr::rename(
    municipality = MUNICIPIO,
    section = CASILLA
  )

# Create 'total' column summing party votes
data_2000 <- data_2000 %>%
  dplyr::mutate(total = rowSums(across(c(PAN, PRI, PRD, `PT-Convergencia-PAS-PSN`, 
                                  PVEM, PDS, PCD, PARM, DSPPN)), na.rm = TRUE))

# Filter out rows where 'total' is missing or zero
data_2000 <- data_2000 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2000 <- data_2000 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN, PRI, PRD, `PT-Convergencia-PAS-PSN`, 
                            PVEM, PDS, PCD, PARM, DSPPN, total), sum, na.rm = TRUE))

# Rename columns for easier interpretation
collapsed_2000 <- collapsed_2000 %>%
  dplyr::rename(
    PT_PC_PAS_PSN = `PT-Convergencia-PAS-PSN`)

# Generate 'uniqueid' based on municipality names
collapsed_2000 <- collapsed_2000 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "Calkiní" ~ 4001,
      municipality == "Campeche" ~ 4002,
      municipality == "Carmen" ~ 4003,
      municipality == "Champotón" ~ 4004,
      municipality == "Hecelchakán" ~ 4005,
      municipality == "Hopelchén" ~ 4006,
      municipality == "Palizada" ~ 4007,
      municipality == "Tenabo" ~ 4008,
      municipality == "Escárcega" ~ 4009,
      municipality == "Calakmul" ~ 4010,
      municipality == "Candelaria" ~ 4011))

# Calculate 'valid' votes by summing all relevant party votes
collapsed_2000 <- collapsed_2000 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT_PC_PAS_PSN, 
                                         PVEM, PDS, PCD, PARM, DSPPN)), na.rm = TRUE))
names(collapsed_2000)

# Load and merge Lista Nominal data
ln_all_months_years <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

ln_all_months_years <- ln_all_months_years %>%
  dplyr::filter(month == "July" & year == 2000 & state == "CAMPECHE") %>%
  dplyr::select(section, lista)

collapsed_2000 <- collapsed_2000 %>%
  dplyr::left_join(ln_all_months_years, by = c("section"))

collapsed_2000 <- collapsed_2000 %>%
  dplyr::rename(listanominal=lista)

# Calculate 'turnout'
collapsed_2000 <- collapsed_2000 %>%
  dplyr::mutate(turnout = total / listanominal,
                month = "July", 
                year = 2000)

rm(data_2000)
rm(ln_all_months_years)
##############################################
### PROCESSING DATA FOR 2003
##############################################

# Load the 2003 data
data_2003 <- read_csv("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003.csv")
names(data_2003)
# Rename columns for consistency
data_2003 <- data_2003 %>%
  dplyr::rename(
    municipality = Municipio,
    section = seccion,
    total = Total)

# Filter out rows where 'total' is missing or zero
data_2003 <- data_2003 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2003 <- data_2003 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(listanominal, PAN, PRI, PRD, PT, PVEM, PC, PSN, PAS,
                            FuerzaCiudadana,MexicoPosible, PLM, Nulos, total), sum, na.rm = TRUE))

# Rename columns for easier interpretation
collapsed_2003 <- collapsed_2003 %>%
  dplyr::rename(
    FUERZACIUDADANA = FuerzaCiudadana,
    MEXICOPOSIBLE = MexicoPosible,
    nulos = Nulos
  )

# Generate 'uniqueid' based on municipality names
collapsed_2003 <- collapsed_2003 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "CALKINI" ~ 4001,
      municipality == "CAMPECHE" ~ 4002,
      municipality == "CARMEN" ~ 4003,
      municipality == "CHAMPOTON" ~ 4004,
      municipality == "HECELCHAKAN" ~ 4005,
      municipality == "HOPELCHEN" ~ 4006,
      municipality == "PALIZADA" ~ 4007,
      municipality == "TENABO" ~ 4008,
      municipality == "ESCARCEGA" ~ 4009,
      municipality == "CALAKMUL" ~ 4010,
      municipality == "CANDELARIA" ~ 4011
    )
  )

# Calculate 'valid' votes by summing all relevant party votes
collapsed_2003 <- collapsed_2003 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PC, PSN, PAS,
                                  FUERZACIUDADANA,MEXICOPOSIBLE,MEXICOPOSIBLE)), na.rm = TRUE))

# Calculate 'turnout'
collapsed_2003 <- collapsed_2003 %>%
  dplyr::mutate(turnout = ifelse(listanominal != 0, (total/listanominal),NA),
                year = 2003,
                month = "July")

rm(data_2003)

##############################################
### PROCESSING DATA FOR 2006
##############################################

# Load the 2006 data
data_2006 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2006.xlsx")
names(data_2006)
# Rename columns for consistency
data_2006 <- data_2006 %>%
  dplyr::rename(
    municipality = MUNICIPIO,
    section = SECCION,
    total = Total,
    nulos = Nulos,
    listanominal = "LISTA NOMINAL"
  )

# Filter out rows where 'total' is missing or zero
data_2006 <- data_2006 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2006 <- data_2006 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(listanominal, PAN, PRI, "PRD-PT-PC", 
                            PVEM, PANAL, PAS, total, nulos), sum, na.rm = TRUE))

# Rename columns for easier interpretation
collapsed_2006 <- collapsed_2006 %>%
  dplyr::rename(
    PRD_PT_PC = "PRD-PT-PC"
  )

# Generate 'uniqueid' based on municipality names
collapsed_2006 <- collapsed_2006 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "CALKINÍ" ~ 4001,
      municipality == "CAMPECHE" ~ 4002,
      municipality == "CARMEN" ~ 4003,
      municipality == "CHAMPOTÓN" ~ 4004,
      municipality == "HECELCHAKÁN" ~ 4005,
      municipality == "HOPELCHÉN" ~ 4006,
      municipality == "PALIZADA" ~ 4007,
      municipality == "TENABO" ~ 4008,
      municipality == "ESCÁRCEGA" ~ 4009,
      municipality == "CALAKMUL" ~ 4010,
      municipality == "CANDELARIA" ~ 4011
    )
  )

# Calculate 'valid' votes by summing all relevant party votes
collapsed_2006 <- collapsed_2006 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD_PT_PC, PVEM, PANAL, PAS)), na.rm = TRUE))

# Calculate 'turnout'
collapsed_2006 <- collapsed_2006 %>%
  dplyr::mutate(turnout = total / listanominal,
                year = 2006,
                month = "July")

# Remove the original data from memory
rm(data_2006)

##############################################
### PROCESSING DATA FOR 2009
##############################################

# Load the 2009 data
data_2009 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2009.xlsx")
names(data_2009)
# Rename columns for consistency
data_2009 <- data_2009 %>%
  dplyr::rename(
    municipality = MUNICIPIO,
    section = SECCION,
    listanominal = LISTANOMINAL,
    total = TOTAL,
    nulos = NULOS)

# Filter out rows where 'total' is missing or zero
data_2009 <- data_2009 %>%
  dplyr::filter(!is.na(total) & total != 0)
names(data_2009)
# Rename columns for easier interpretation
data_2009 <- data_2009 %>%
  dplyr::rename(
    PRI_PANAL = "PRI-PANAL")

# Collapse by municipality and section
collapsed_2009 <- data_2009 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(listanominal, PAN, PRI_PANAL, PRD, PT, PVEM, PC, PSD, total,nulos), 
                   sum, na.rm = TRUE))

# Generate 'uniqueid' based on municipality names
collapsed_2009 <- collapsed_2009 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "CALKINÍ" ~ 4001,
      municipality == "CAMPECHE" ~ 4002,
      municipality == "CARMEN" ~ 4003,
      municipality == "CHAMPOTÓN" ~ 4004,
      municipality == "HECELCHAKÁN" ~ 4005,
      municipality == "HOPELCHÉN" ~ 4006,
      municipality == "PALIZADA" ~ 4007,
      municipality == "TENABO" ~ 4008,
      municipality == "ESCÁRCEGA" ~ 4009,
      municipality == "CALAKMUL" ~ 4010,
      municipality == "CANDELARIA" ~ 4011
    )
  )

# Calculate 'valid' votes by summing all relevant party votes
collapsed_2009 <- collapsed_2009 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI_PANAL, PRD, PT, PVEM, PC, PSD)), na.rm = TRUE))

# Calculate 'turnout'
collapsed_2009 <- collapsed_2009 %>%
  dplyr::mutate(turnout = total / listanominal,
                year = 2009,
                month = "July")

# Remove the original data from memory
rm(data_2009)

##############################################
### PROCESSING DATA FOR 2012
##############################################

# Load the 2012 data
data_2012 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2012.xlsx")
names(data_2012)
# Rename columns for consistency
data_2012 <- data_2012 %>%
  dplyr::rename(
    municipality = Municipio,
    section = Seccion,
    listanominal = ListaNominal,
    total = TOTAL,
    nulos = NULOS)

# Filter out rows where 'total' is missing or zero
data_2012 <- data_2012 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2012 <- data_2012 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(listanominal, PAN, PRI_PVEM, PRD, PT_PC, PANAL, total, nulos), sum, na.rm = TRUE))

# Generate 'uniqueid' based on municipality names
collapsed_2012 <- collapsed_2012 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "CALKINÍ" ~ 4001,
      municipality == "CAMPECHE" ~ 4002,
      municipality == "CARMEN" ~ 4003,
      municipality == "CHAMPOTÓN" ~ 4004,
      municipality == "HECELCHAKÁN" ~ 4005,
      municipality == "HOPELCHÉN" ~ 4006,
      municipality == "PALIZADA" ~ 4007,
      municipality == "TENABO" ~ 4008,
      municipality == "ESCÁRCEGA" ~ 4009,
      municipality == "CALAKMUL" ~ 4010,
      municipality == "CANDELARIA" ~ 4011
    )
  )

# Calculate 'valid' votes by summing all relevant party votes
collapsed_2012 <- collapsed_2012 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI_PVEM, PRD, PT_PC, PANAL)), na.rm = TRUE))

# Calculate 'turnout'
collapsed_2012 <- collapsed_2012 %>%
  dplyr::mutate(turnout = total / listanominal,
                year = 2012,
                month = "July")

# Remove the original data from memory
rm(data_2012)

##############################################
### PROCESSING DATA FOR 2015
##############################################

# Function to process data
process_data <- function(sheet_name, unique_id, output_file, file_path) {
  data <- read_excel(file_path, sheet = sheet_name) %>%
    filter(SECCION != "") %>%
    select(MUNICIPIO, SECCION, PAN, PRD, PT, MC, PANAL, Morena, PH, PES, PRIPVEM, 
           "VOTOS VÁLIDOS", "VOTOS NULOS", "VOTACIÓN TOTAL EMITIDA", "LISTA NOMINAL") %>%
    mutate(uniqueid = unique_id)
  
  # Save the processed data as CSV
  write.csv(data, paste0(output_file, "_15.csv"), row.names = FALSE)
}

# Processing all municipalities for 2015
file_path_2015 <- "../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx"
process_data("CALKINI", "4001", "CALKINI", file_path_2015)
process_data("CAMPECHE", "4002", "CAMPECHE", file_path_2015)
process_data("CARMEN", "4003", "CARMEN", file_path_2015)
process_data("CHAMPOTON", "4004", "CHAMPOTON", file_path_2015)
process_data("HECELCHAKAN", "4005", "HECELCHAKAN", file_path_2015)
process_data("HOPELCHEN", "4006", "HOPELCHEN", file_path_2015)
process_data("PALIZADA", "4007", "PALIZADA", file_path_2015)
process_data("TENABO", "4008", "TENABO", file_path_2015)
process_data("ESCARCEGA", "4009", "ESCARCEGA", file_path_2015)
process_data("CALAKMUL", "4010", "CALAKMUL", file_path_2015)
process_data("CANDELARIA", "4011", "CANDELARIA", file_path_2015)

# Combine all processed data for 2015
files_2015 <- list.files(pattern = "*_15.csv")
data_2015 <- do.call(rbind, lapply(files_2015, read.csv))

# Clean up intermediate files
file.remove(files_2015)
names(data_2015)
# Renaming columns, destring, and collapsing in 2015 data
collapsed_2015 <- data_2015 %>%
  dplyr::rename(nulos = "VOTOS.NULOS",
         total = "VOTACIÓN.TOTAL.EMITIDA",
         listanominal = "LISTA.NOMINAL",
         valid = "VOTOS.VÁLIDOS",
         municipality = MUNICIPIO,
         section = SECCION,
         MORENA = Morena,
         PRI_PVEM = PRIPVEM) %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN, PRD, PT, MC, PANAL, MORENA, PH, PES, PRI_PVEM, valid, total, listanominal,nulos), sum, na.rm = TRUE))

# Add year and month for 2015 data
collapsed_2015 <- collapsed_2015 %>%
  dplyr::mutate(year = 2015,
         month = "June",
         turnout = total / listanominal)
rm(data_2015)

##############################################
### PROCESSING DATA FOR 2018
##############################################

# Load and process data for Carmen (2018)
data_carmen_18 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2018.xlsx", sheet = "CARMEN")

# Process for other municipalities
data_hoja1 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2018.xlsx", sheet = "Hoja1") %>%
  dplyr::filter(Municipio != "Carmen")

# Combine Carmen with other municipalities for 2018
data_2018 <- bind_rows(data_hoja1, data_carmen_18) %>%
  dplyr::mutate(municipality = gsub("á", "a", Municipio),
         municipality = gsub("é", "e", municipality),
         municipality = gsub("í", "i", municipality),
         municipality = gsub("ó", "o", municipality),
         municipality = toupper(municipality)) %>%
  dplyr::mutate(uniqueid = case_when(
    municipality == "CALKINI" ~ "4001",
    municipality == "CAMPECHE" ~ "4002",
    municipality == "CARMEN" ~ "4003",
    municipality == "CHAMPOTON" ~ "4004",
    municipality == "HECELCHAKAN" ~ "4005",
    municipality == "HOPELCHEN" ~ "4006",
    municipality == "PALIZADA" ~ "4007",
    municipality == "TENABO" ~ "4008",
    municipality == "ESCARCEGA" ~ "4009",
    municipality == "CALAKMUL" ~ "4010",
    municipality == "CANDELARIA" ~ "4011"
  ),
  uniqueid = as.numeric(uniqueid)) %>%
  dplyr::rename(
         section = Seccion,
         total = TotalCalculado,
         listanominal = ListaNominal,
         PRI_PVEM_PANAL = "PRI-PVEM-PANAL",
         PRI_PVEM = "PRI-PVEM",
         PRI_PANAL = "PRI-PANAL",
         PVEM_PANAL = "PVEM-PANAL",
         PAN_MC = "PAN-MC",
         CI_1 = "CI-1",
         CI_2 = "CI-2",
         nulos = VotosNulos) %>%
  dplyr::mutate(PRI_PVEM_PANAL = rowSums(across(c(PRI_PVEM_PANAL,PRI, PVEM, 
                                                  PANAL,PRI_PVEM,PRI_PANAL,
                                                  PVEM_PANAL)), na.rm = TRUE),
         PAN_MC = rowSums(across(c(PAN, MC,PAN_MC)), na.rm = TRUE)) %>%
  dplyr::select(-PRI, -PVEM, -PANAL, -PRI_PVEM, -PRI_PANAL, -PVEM_PANAL,
                -PANAL, -PAN, -MC)

# Calculate valid votes and municipal summaries
collapsed_2018 <- data_2018 %>%
  dplyr::group_by(uniqueid, municipality, section) %>%
  dplyr::summarise(across(c(PAN_MC, PRI_PVEM_PANAL, PRD, PT, MORENA, PES, PLC, CI_1, CI_2, total, listanominal,nulos), sum, na.rm = TRUE)) %>%
  dplyr::mutate(valid = rowSums(across(c(PAN_MC, PRI_PVEM_PANAL, PRD, PT, MORENA, PES, PLC, CI_1, CI_2)), na.rm = TRUE))

# Add year and month for 2018 data
collapsed_2018 <- collapsed_2018 %>%
  dplyr::mutate(year = 2018,
         month = "July",
         turnout = total / listanominal)

rm(data_hoja1)
rm(data_carmen_18)
rm(data_2018)


# Combine the dataframes, handling different columns by filling with NA
campeche_all <- bind_rows(collapsed_1997,
                          collapsed_2000,
                          collapsed_2003,
                          collapsed_2006,
                          collapsed_2009,
                          collapsed_2012,
                          collapsed_2015,
                          collapsed_2018)

data.table::fwrite(campeche_all,"../../../Processed Data/campeche/campeche_process_raw_data.csv")

