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
data_1997 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/1997/Ayu_Seccion_1997_No_LN.xlsx")
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
data_2000 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2000/Ayu_Seccion_2000_No_LN.xlsx")
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
data_2003 <- read_csv("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2003/Ayu_Seccion_2003.csv")
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
data_2006 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2006/Ayu_Seccion_2006.xlsx")
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
data_2009 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2009/Ayu_Seccion_2009.xlsx")
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
data_2012 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2012/Ayu_Seccion_2012.xlsx")
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
file_path_2015 <- "../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2015/RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx"
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
data_carmen_18 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2018/Ayuntamientos_2018.xlsx", sheet = "CARMEN")

# Process for other municipalities
data_hoja1 <- read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2018/Ayuntamientos_2018.xlsx", sheet = "Hoja1") %>%
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


##############################################
### PROCESSING DATA FOR 2021 -----------------
##############################################

# Load 2021 data except Hecelchakan
data_2021_rest <- readxl::read_excel("../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2021/ayuntamientos_2021.xlsx") %>%
  select(!contains("%"))

names(data_2021_rest)

# Load 2021 data from Hecelchakan
data_2021_hec <- read_excel(
  "../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2021/AYUNTAMIENTO HECELCHAKÁN-Casilla.xlsx",
  skip = 7,
  col_names = c(
    "MUNICIPIO", "CASILLA", 
    "PAN", "PAN_%", "PRI", "PRI_%", "PRD", "PRD_%", 
    "PT", "PT_%", "PVEM", "PVEM_%", "MC", "MC_%", 
    "MORENA", "MORENA_%", "PES", "PES_%", "RSP", "RSP_%", 
    "FXM", "FXM_%", "PAN_PRI_PRD", "PAN_PRI_PRD_%", 
    "PAN_PRI", "PAN_PRI_%", "PAN_PRD", "PAN_PRD_%", 
    "PRI_PRD", "PRI_PRD_%", "CI_1", "CI_1_%", 
    "CANDIDATOS/AS NO REGISTRADOS/AS", "CANDIDATOS/AS NO REGISTRADOS/AS_%", 
    "VOTOS VÁLIDOS", "VOTOS VÁLIDOS_%", 
    "VOTOS NULOS", "VOTOS NULOS_%", 
    "TOTAL", "TOTAL_%", 
    "LISTA NOMINAL", "PARTICIPACIÓN CIUDADANA"
  )
) %>% 
  select(!contains("%"))

names(data_2021_hec)

# Combine 2021 data
data_2021 <- bind_rows(data_2021_rest, data_2021_hec) %>%
  dplyr::filter(CASILLA != "" & CASILLA != "TOTAL") %>%
  separate(CASILLA, into = c("section"), sep = " ", remove = FALSE) %>% 
  dplyr::rename(
    municipality = MUNICIPIO,
    total = TOTAL,
    listanominal = `LISTA NOMINAL`,
    valid = `VOTOS VÁLIDOS`, 
    nulos = `VOTOS NULOS`) %>% 
  dplyr::mutate(municipality = gsub("Á", "A", municipality),
                municipality = gsub("É", "E", municipality),
                municipality = gsub("Í", "I", municipality),
                municipality = gsub("Ó", "O", municipality))  %>%
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
    municipality == "CANDELARIA" ~ "4011",
    municipality == "SEYBAPLAYA" ~ "4012",
    municipality == "DZITBALCHE" ~ "4013"
  ),
  uniqueid = as.numeric(uniqueid)) %>% 
  dplyr::mutate(PAN_PRI_PRD = rowSums(across(c(PAN, PRI, PRD, PAN_PRI_PRD, PAN_PRI, PAN_PRD, PRI_PRD)), na.rm = TRUE)) %>%
  dplyr::select(municipality, section, PT, PVEM, MC, MORENA, PES, RSP, FXM, PAN_PRI_PRD, valid,
                nulos, total, listanominal, CI_1, uniqueid) %>% 
  dplyr::mutate(section = as.numeric(section)) %>% 
  filter(total > 1)

# Calculate valid votes and municipal summaries
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(uniqueid, municipality, section) %>%
  dplyr::summarise(across(c(PT, PVEM, MC, MORENA, PES, RSP, FXM, PAN_PRI_PRD, CI_1, valid, total, listanominal,nulos), sum, na.rm = TRUE))

# Add year and month for 2021 data
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(year = 2021,
                month = "June",
                turnout = total / listanominal)
rm(data_2021)
rm(data_2021_hec)
rm(data_2021_rest)

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 4) %>% 
  select(yr, inegi, coal1, coal2, coal3, coal4) %>% 
  rename(
    year = yr,
    uniqueid = inegi) %>% 
  mutate(
    across(
      coal1:coal4,
      ~ str_replace_all(., "-", "_") |> 
        str_replace_all(regex("PNA", ignore_case = TRUE), "PANAL") |> 
        str_to_upper()
    )
  )

process_coalitions <- function(electoral_data, magar_data) {
  
  # Store grouping and ungroup
  original_groups <- dplyr::groups(electoral_data)
  merged <- electoral_data %>%
    ungroup() %>%
    left_join(magar_data, by = c("uniqueid", "year")) %>%
    as.data.frame()
  
  # Get party columns (exclude metadata)
  metadata_cols <- c("uniqueid", "section", "year", "month", "no_reg", "nulos", 
                     "total", "CI_2", "CI_1", "listanominal", "valid", "turnout",
                     "coal1", "coal2", "coal3", "coal4")
  party_cols <- setdiff(names(merged), metadata_cols)
  party_cols <- party_cols[sapply(merged[party_cols], is.numeric)]
  
  # Get unique coalitions
  all_coalitions <- unique(c(merged$coal1, merged$coal2, merged$coal3, merged$coal4))
  all_coalitions <- all_coalitions[all_coalitions != "NONE" & !is.na(all_coalitions)]
  
  # Helper: find columns belonging to a coalition
  get_coalition_cols <- function(coal_name) {
    parties <- strsplit(coal_name, "_")[[1]]
    party_cols[sapply(party_cols, function(col) {
      all(strsplit(col, "_")[[1]] %in% parties)
    })]
  }
  
  # Calculate coalition votes (with temp names to avoid conflicts)
  for (coal in all_coalitions) {
    merged[[paste0("NEW_", coal)]] <- sapply(1:nrow(merged), function(i) {
      active <- c(merged$coal1[i], merged$coal2[i], merged$coal3[i], merged$coal4[i])
      if (coal %in% active) {
        sum(unlist(merged[i, get_coalition_cols(coal)]), na.rm = TRUE)
      } else {
        0
      }
    })
  }
  
  # Zero out constituent columns
  for (i in 1:nrow(merged)) {
    active <- c(merged$coal1[i], merged$coal2[i], merged$coal3[i], merged$coal4[i])
    active <- active[active != "NONE" & !is.na(active)]
    for (coal in active) {
      merged[i, get_coalition_cols(coal)] <- 0
    }
  }
  
  # Rename temp columns to final names
  for (coal in all_coalitions) {
    merged[[coal]] <- merged[[paste0("NEW_", coal)]]
    merged[[paste0("NEW_", coal)]] <- NULL
  }
  
  # Convert to tibble and restore grouping
  result <- as_tibble(merged)
  if (length(original_groups) > 0) {
    result <- result %>% group_by(!!!original_groups)
  }
  
  return(result)
}

# Apply
collapsed_2021 <- process_coalitions(collapsed_2021, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)


##############################################
### PROCESSING DATA FOR 2024 -----------------
##############################################

# Define the folder path
folder_path <- "../../../Data/Raw Electoral Data/Campeche - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2024/"

names_standard <- c(
  "municipality", "casilla","PAN", "PAN_pct", "PRI", "PRI_pct", "PRD", "PRD_pct",
  "PT", "PT_pct", "PVEM", "PVEM_pct", "MC", "MC_pct", "MORENA", "MORENA_pct",
  "PES", "PES_pct", "CL", "CL_pct", "EDC", "EDC_pct", "MLC", "MLC_pct",
  "PRI_PRD", "PRI_PRD_pct", "PT_PVEM_MORENA", "PT_PVEM_MORENA_pct",
  "PT_PVEM", "PT_PVEM_pct", "PT_MORENA", "PT_MORENA_pct",
  "PVEM_MORENA", "PVEM_MORENA_pct", "valid", "validos_pct",
  "no_reg", "no_reg_pct", "nulos", "nulos_pct", "total", "total_pct",
  "listanominal", "participacion"
)

names_7 <- c(
  "municipality", "casilla","PAN", "PAN_pct", "PRI", "PRI_pct", "PRD", "PRD_pct",
  "PT", "PT_pct", "PVEM", "PVEM_pct", "MC", "MC_pct", "MORENA", "MORENA_pct",
  "PES", "PES_pct", "CL", "CL_pct", "EDC", "EDC_pct", "MLC", "MLC_pct",
  "PRI_PRD", "PRI_PRD_pct", "valid", "validos_pct",
  "no_reg", "no_reg_pct", "nulos", "nulos_pct", "total", "total_pct",
  "listanominal", "participacion"
)

names_9 <- c(
  "municipality", "casilla","PAN", "PAN_pct", "PRI", "PRI_pct", "PRD", "PRD_pct",
  "PT", "PT_pct", "PVEM", "PVEM_pct", "MC", "MC_pct", "MORENA", "MORENA_pct",
  "PES", "PES_pct", "CL", "CL_pct", "EDC", "EDC_pct", "MLC", "MLC_pct",
  "CI_1", "CI_1_pct",
  "PRI_PRD", "PRI_PRD_pct", "PT_PVEM_MORENA", "PT_PVEM_MORENA_pct",
  "PT_PVEM", "PT_PVEM_pct", "PT_MORENA", "PT_MORENA_pct",
  "PVEM_MORENA", "PVEM_MORENA_pct", "valid", "validos_pct",
  "no_reg", "no_reg_pct", "nulos", "nulos_pct", "total", "total_pct",
  "listanominal", "participacion"
)

names_12 <- c(
  "municipality", "casilla","PAN", "PAN_pct", "PRI", "PRI_pct", "PRD", "PRD_pct",
  "PT", "PT_pct", "PVEM", "PVEM_pct", "MC", "MC_pct", "MORENA", "MORENA_pct",
  "PES", "PES_pct", "CL", "CL_pct", "EDC", "EDC_pct", "MLC", "MLC_pct",
  "CI_2", "CI_2_pct",
  "PRI_PRD", "PRI_PRD_pct", "PT_PVEM_MORENA", "PT_PVEM_MORENA_pct",
  "PT_PVEM", "PT_PVEM_pct", "PT_MORENA", "PT_MORENA_pct",
  "PVEM_MORENA", "PVEM_MORENA_pct", "valid", "validos_pct",
  "no_reg", "no_reg_pct", "nulos", "nulos_pct", "total", "total_pct",
  "listanominal", "participacion"
)

# Process all Excel files from 1 to 11
for (x in 1:13) {
  
  # Choose the appropriate column names
  if (x == 7) {
    manual_names <- names_7
  } else if (x == 9) {
    manual_names <- names_9
  } else if (x == 12) {
    manual_names <- names_12
  } else {
    manual_names <- names_standard
  }
  
  # Load Excel, skip header, and coerce to character
  file_path <- paste0(folder_path, "AYU_", x, ".xlsx")
  data <- read_excel(file_path, skip = 6, col_names = manual_names) %>%
    mutate(across(everything(), as.character))
  
  # Save cleaned file
  cleaned_file_path <- paste0(folder_path, "AYU_", x, "_clean.xlsx")
  write.xlsx(data, cleaned_file_path)
  
  # Reload cleaned file and add uniqueid
  data <- read_excel(cleaned_file_path) %>%
    mutate(uniqueid = ifelse(x > 9, paste0("40", x), paste0("400", x)))
  
  # Save as RDS
  saveRDS(data, file = paste0("dataset_", x, ".rds"))
  
  # Remove temp cleaned file
  file.remove(cleaned_file_path)
}

# Append all datasets
combined_2024 <- NULL
for (x in 1:13) {
  temp_data <- readRDS(paste0("dataset_", x, ".rds"))
  combined_2024 <- bind_rows(combined_2024, temp_data)
  file.remove(paste0("dataset_", x, ".rds"))  # Clean up
}

# Clean up casillas and other variables
combined_2024 <- combined_2024 %>%
  select(!contains("pct")) %>% 
  dplyr::mutate(across(
    .cols = -c(municipality, casilla),
    .fns = ~ as.numeric(.)
  )) %>% 
  dplyr::filter(casilla != "" & casilla != "TOTAL") %>%
  separate(casilla, into = c("section"), sep = " ", remove = FALSE) %>% 
  dplyr::mutate(municipality = gsub("Á", "A", municipality),
                municipality = gsub("É", "E", municipality),
                municipality = gsub("Í", "I", municipality),
                municipality = gsub("Ó", "O", municipality),
                PRI_PRD = rowSums(across(c(PRI, PRD, PRI_PRD)), na.rm = TRUE),
                PT_PVEM_MORENA = rowSums(across(c(PT, PVEM, MORENA, PT_PVEM,
                                                  PT_MORENA, PVEM_MORENA, PT_PVEM,
                                                  PT_PVEM,MORENA)), na.rm = TRUE)) %>% 
  dplyr::select(municipality, section, PAN, MC, PES, CL, EDC, MLC, PRI_PRD, PT_PVEM_MORENA, valid, nulos, total, listanominal, CI_1, CI_2, uniqueid) %>% 
  dplyr::mutate(section = as.numeric(section)) %>% 
  filter(total > 1)

# Calculate valid votes and municipal summaries
collapsed_2024 <- combined_2024 %>%
  dplyr::group_by(uniqueid, municipality, section) %>%
  dplyr::summarise(across(c(PAN, MC, PES, CL, EDC, MLC, PRI_PRD, PT_PVEM_MORENA, valid, nulos, total, listanominal, CI_1, CI_2), sum, na.rm = TRUE))

# Add year and month for 2024 data
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(year = 2024,
                month = "June",
                turnout = total / listanominal)
rm(data)
rm(temp_data)
rm(combined_2024)

process_coalitions <- function(electoral_data, magar_data) {
  
  # Store grouping and ungroup
  original_groups <- dplyr::groups(electoral_data)
  merged <- electoral_data %>%
    ungroup() %>%
    left_join(magar_data, by = c("uniqueid", "year")) %>%
    as.data.frame()
  
  # Get party columns (exclude metadata)
  metadata_cols <- c("uniqueid", "section", "year", "month", "no_reg", "nulos", 
                     "total", "CI_2", "CI_1", "listanominal", "valid", "turnout",
                     "coal1", "coal2", "coal3", "coal4")
  party_cols <- setdiff(names(merged), metadata_cols)
  party_cols <- party_cols[sapply(merged[party_cols], is.numeric)]
  
  # Get unique coalitions
  all_coalitions <- unique(c(merged$coal1, merged$coal2, merged$coal3, merged$coal4))
  all_coalitions <- all_coalitions[all_coalitions != "NONE" & !is.na(all_coalitions)]
  
  # Helper: find columns belonging to a coalition
  get_coalition_cols <- function(coal_name) {
    parties <- strsplit(coal_name, "_")[[1]]
    party_cols[sapply(party_cols, function(col) {
      all(strsplit(col, "_")[[1]] %in% parties)
    })]
  }
  
  # Calculate coalition votes (with temp names to avoid conflicts)
  for (coal in all_coalitions) {
    merged[[paste0("NEW_", coal)]] <- sapply(1:nrow(merged), function(i) {
      active <- c(merged$coal1[i], merged$coal2[i], merged$coal3[i], merged$coal4[i])
      if (coal %in% active) {
        sum(unlist(merged[i, get_coalition_cols(coal)]), na.rm = TRUE)
      } else {
        0
      }
    })
  }
  
  # Zero out constituent columns
  for (i in 1:nrow(merged)) {
    active <- c(merged$coal1[i], merged$coal2[i], merged$coal3[i], merged$coal4[i])
    active <- active[active != "NONE" & !is.na(active)]
    for (coal in active) {
      merged[i, get_coalition_cols(coal)] <- 0
    }
  }
  
  # Rename temp columns to final names
  for (coal in all_coalitions) {
    merged[[coal]] <- merged[[paste0("NEW_", coal)]]
    merged[[paste0("NEW_", coal)]] <- NULL
  }
  
  # Convert to tibble and restore grouping
  result <- as_tibble(merged)
  if (length(original_groups) > 0) {
    result <- result %>% group_by(!!!original_groups)
  }
  
  return(result)
}

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

# Combine the dataframes, handling different columns by filling with NA
campeche_all <- bind_rows(collapsed_1997,
                          collapsed_2000,
                          collapsed_2003,
                          collapsed_2006,
                          collapsed_2009,
                          collapsed_2012,
                          collapsed_2015,
                          collapsed_2018,
                          collapsed_2021,
                          collapsed_2024)

data.table::fwrite(campeche_all,"../../../Processed Data/campeche/campeche_process_raw_data.csv")
