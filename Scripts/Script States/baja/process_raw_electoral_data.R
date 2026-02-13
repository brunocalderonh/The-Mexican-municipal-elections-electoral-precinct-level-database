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
                , data.table
                , readr)


# Set working directory
# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, ""))

#####################################
### PROCESSING DATA FOR 1995
#####################################

# Load the 1995 data
data_1995 <- read_csv("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/1995/Ayu_Seccion_1995_No_LN.csv")

# Rename columns for easier manipulation
data_1995 <- data_1995 %>%
  dplyr::rename(
    municipality = "Municpio",  # Ensure exact case match
    section = "Seccion"         # Case-sensitive renaming
  )

# Compute the total column by summing party columns
data_1995 <- data_1995 %>%
  dplyr::mutate(total = rowSums(across(c(PAN, PRI, PRD, PFCRN, PT, PVEM, PRS, PPBC, Otros, Nulos)), na.rm = TRUE))

# Filter out rows where 'total' is missing or zero
data_1995 <- data_1995 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Drop the 'otros' column after summing
data_1995 <- data_1995 %>%
  dplyr::select(-Otros)

# Collapse by municipality and section
collapsed_1995 <- data_1995 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN, PRI, PRD, PFCRN, PT, PVEM, PRS, PPBC, total, Nulos), sum, na.rm = TRUE))

# Merge with the dataset "ln_all_months_years.dta" using seccion (section) and ed
data_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

data_all <- data_all %>% 
  dplyr::filter(state == "BAJA CALIFORNIA" & month == "June" & year == 1998) # Keep only records for June 2013

# Merge the datasets
collapsed_1995 <- collapsed_1995 %>%
  dplyr::left_join(data_all %>% dplyr::select(section,lista), by = c("section")) %>% 
  dplyr::rename("listanominal"="lista")

# Rename columns for easier interpretation
collapsed_1995 <- collapsed_1995 %>%
  dplyr::rename(
    PAN = "PAN",
    PRI = "PRI",
    PRD = "PRD",
    Partido_Cardenista = "PFCRN",
    PT = "PT",
    PVEM = "PVEM",
    PRS = "PRS",
    PPBC = "PPBC",
    nulos = "Nulos"
  )

# Generate uniqueid based on municipality names
collapsed_1995 <- collapsed_1995 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ENSENADA" ~ 2001,
      municipality == "MEXICALI" ~ 2002,
      municipality == "TECATE" ~ 2003,
      municipality == "TIJUANA" ~ 2004,
      TRUE ~ NA_real_
    )
  )

# Generate 'valid' column summing relevant votes
collapsed_1995 <- collapsed_1995 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD, Partido_Cardenista, PT, PVEM, PRS, PPBC)), na.rm = TRUE))

# Add year and month columns
collapsed_1995 <- collapsed_1995 %>%
  dplyr::mutate(
    year = 1995,
    month = "August",
    turnout = total / listanominal
  )
rm(data_1995)

#####################################
### PROCESSING DATA FOR 1998
#####################################

# Load the 1998 data
data_1998 <- data.table::fread("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/1998/Ayu_Seccion_1998_No_LN.csv")
names(data_1998)
# Rename columns
data_1998 <- data_1998 %>%
  dplyr::rename(
    municipality = "Municipio",
    section = "Secci\xf3n"
  )

# Compute the total column by summing party columns
data_1998 <- data_1998 %>%
  dplyr::mutate(total = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PEBC, PRS, PPBC, Nulos)), na.rm = TRUE))

# Filter out rows where 'total' is missing or zero
data_1998 <- data_1998 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_1998 <- data_1998 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN, PRI, PRD, PT, PVEM, PEBC, PRS, PPBC, total, Nulos), sum, na.rm = TRUE)) %>% 
  dplyr::rename(nulos=Nulos)

# Merge with the dataset "ln_all_months_years.dta" using seccion (section) and ed
data_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

data_all <- data_all %>% 
  dplyr::filter(state == "BAJA CALIFORNIA" & month == "June" & year == 1998) # Keep only records for June 2013

# Merge the datasets
collapsed_1998 <- collapsed_1998 %>%
  dplyr::left_join(data_all %>% dplyr::select(section,lista), by = c("section")) %>% 
  dplyr::rename("listanominal"="lista")

# Generate uniqueid
collapsed_1998 <- collapsed_1998 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "Ensenada" ~ 2001,
      municipality == "Mexicali" ~ 2002,
      municipality == "Tecate" ~ 2003,
      municipality == "Tijuana" ~ 2004,
      municipality == "Playas de Rosarito" ~ 2005,
      TRUE ~ NA_real_
    )
  )

# Generate 'valid' column summing relevant votes
collapsed_1998 <- collapsed_1998 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PRS, PPBC, PEBC)), na.rm = TRUE))

# Add year and month columns
collapsed_1998 <- collapsed_1998 %>%
  dplyr::mutate(
    year = 1998,
    month = "June",
    turnout = total / listanominal
  )

rm(data_1998)

#####################################
### PROCESSING DATA FOR 2001
#####################################

data_2001 <- read_csv("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/2001/Ayu_Seccion_2001_No_LN.csv")
names(data_2001)
# Rename columns
data_2001 <- data_2001 %>%
  dplyr::rename(
    municipality = "Municipio",
    section = "Casilla"
  )

# Compute total votes
data_2001 <- data_2001 %>%
  dplyr::mutate(total = rowSums(across(c(`PAN-PVEM`, PRI, PRD, PT, PEBC, PSN, `PC- PAS`, Nulos)), na.rm = TRUE))

# Filter out invalid rows
data_2001 <- data_2001 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2001 <- data_2001 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(`PAN-PVEM`, PRI, PRD, PT, PEBC, PSN, `PC- PAS`, total, Nulos), sum, na.rm = TRUE))%>% 
  dplyr::rename(nulos=Nulos)

# Merge with the dataset "ln_all_months_years.dta" using seccion (section) and ed
data_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

data_all <- data_all %>% 
  dplyr::filter(state == "BAJA CALIFORNIA" & month == "September" & year == 2001) # Keep only records for June 2013

# Merge the datasets
collapsed_2001 <- collapsed_2001 %>%
  dplyr::left_join(data_all %>% dplyr::select(section,lista), by = c("section")) %>% 
  dplyr::rename("listanominal"="lista")

# Rename parties
collapsed_2001 <- collapsed_2001 %>%
  dplyr::rename(
    PAN_PVEM = `PAN-PVEM`,
    PC_PAS = `PC- PAS`
  )

# Assign unique municipality IDs
collapsed_2001 <- collapsed_2001 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "Ensenada" ~ 2001,
      municipality == "Mexicali" ~ 2002,
      municipality == "Tecate" ~ 2003,
      municipality == "Tijuana" ~ 2004,
      municipality == "Playas de Rosarito" ~ 2005,
      TRUE ~ NA_real_
    )
  )

# Generate valid votes and apply ranking logic
collapsed_2001 <- collapsed_2001 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN_PVEM, PRI, PRD, PT, PEBC, PSN, PC_PAS)), na.rm = TRUE))

collapsed_2001 <- collapsed_2001 %>%
  dplyr::mutate(
    year = 2001,
    month = "August",
    turnout = total / listanominal
  )

rm(data_2001)

#####################################
### PROCESSING DATA FOR 2004
#####################################

data_2004 <- read_csv("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/2004/Ayu_Seccion_2004.csv")
names(data_2004)

# Rename columns
data_2004 <- data_2004 %>%
  dplyr::rename(
    municipality = "Municipio",
    section = "Seccion"
  )

# Compute total votes
data_2004 <- data_2004 %>%
  dplyr::mutate(total = rowSums(across(c(PAN, `PRI-PT-PVEM-PEBC`, PRD, PC, Nulos)), na.rm = TRUE))

# Filter out invalid rows
data_2004 <- data_2004 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2004 <- data_2004 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN, `PRI-PT-PVEM-PEBC`, PRD, PC, total,Nulos, `Lista Nominal`), sum, na.rm = TRUE))%>% 
  dplyr::rename(nulos=Nulos,
                listanominal = "Lista Nominal")

# Rename parties
collapsed_2004 <- collapsed_2004 %>%
  dplyr::rename(
    PRI_PT_PVEM_PEBC = `PRI-PT-PVEM-PEBC`,
  )

# Assign unique municipality IDs
collapsed_2004 <- collapsed_2004 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ENSENADA" ~ 2001,
      municipality == "MEXICALI" ~ 2002,
      municipality == "TECATE" ~ 2003,
      municipality == "TIJUANA" ~ 2004,
      municipality == "ROSARITO" ~ 2005,
      TRUE ~ NA_real_
    )
  )

# Generate valid votes and apply ranking logic
collapsed_2004 <- collapsed_2004 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI_PT_PVEM_PEBC, PRD, PC)), na.rm = TRUE))

collapsed_2004 <- collapsed_2004 %>%
  dplyr::mutate(
    year = 2004,
    month = "August",
    turnout = total / listanominal
  )

rm(data_2004)

#####################################
### PROCESSING DATA FOR 2007
#####################################

data_2007 <- data.table::fread("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/2007/Ayu_Seccion_2007.csv")
names(data_2007)
# Rename columns
data_2007 <- data_2007 %>%
  dplyr::rename(
    municipality = "municipio",
    section = "Secci\xf3n"
  )

# Compute total votes
data_2007 <- data_2007 %>%
  dplyr::mutate(total = rowSums(across(c(`PAN-PANAL-PES`, `PRI-PVEM-PEBC`, PRD, `PC-PT`, PAS, anulados)), na.rm = TRUE))

# Filter out invalid rows
data_2007 <- data_2007 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2007 <- data_2007 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(`PAN-PANAL-PES`, `PRI-PVEM-PEBC`, PRD, `PC-PT`, PAS, total, anulados, listanominal), sum, na.rm = TRUE))%>% 
  dplyr::rename(nulos=anulados)

# Rename parties
collapsed_2007 <- collapsed_2007 %>%
  dplyr::rename(
    PAN_PANAL_PES = `PAN-PANAL-PES`,
    PRI_PVEM_PEBC = `PRI-PVEM-PEBC`,
    PT_PC = `PC-PT`,
  )

# Assign unique municipality IDs
collapsed_2007 <- collapsed_2007 %>%
  mutate(
    uniqueid = case_when(
      municipality == "ENSENADA" ~ 2001,
      municipality == "MEXICALI" ~ 2002,
      municipality == "TECATE" ~ 2003,
      municipality == "TIJUANA" ~ 2004,
      municipality == "ROSARITO" ~ 2005,
      TRUE ~ NA_real_
    )
  )

# Generate valid votes and apply ranking logic
collapsed_2007 <- collapsed_2007 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN_PANAL_PES, PRI_PVEM_PEBC, PRD, PT_PC, PAS)), na.rm = TRUE))

# Add year and month
collapsed_2007 <- collapsed_2007 %>%
  dplyr::mutate(
    year = 2007,
    month = "August",
    turnout = total / listanominal
  )

rm(data_2007)

#####################################
### PROCESSING DATA FOR 2010
#####################################

data_2010 <- read_csv("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/2010/Ayu_Seccion_2010.csv")
names(data_2010)
# Rename columns
data_2010 <- data_2010 %>%
  dplyr::rename(
    municipality = "nombre_municipio",
    section = "seccion"
  )
# Compute total votes
data_2010 <- data_2010 %>%
  dplyr::mutate(total = rowSums(across(c(`PAN-PANAL-PES`, `PRI-PVEM`, PRD, `PC-PT`, PEBC, anulados)), na.rm = TRUE))

# Filter out invalid rows
data_2010 <- data_2010 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2010 <- data_2010 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(`PAN-PANAL-PES`, `PRI-PVEM`, PRD, `PC-PT`, PEBC, total, anulados, listanominal), sum, na.rm = TRUE))%>% 
  dplyr::rename(nulos=anulados)

# Rename parties
collapsed_2010 <- collapsed_2010 %>%
  dplyr::rename(
    PAN_PANAL_PES = `PAN-PANAL-PES`,
    PRI_PVEM = `PRI-PVEM`,
    PT_PC = `PC-PT`,
  )

# Assign unique municipality IDs
collapsed_2010 <- collapsed_2010 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ENSENADA" ~ 2001,
      municipality == "MEXICALI" ~ 2002,
      municipality == "TECATE" ~ 2003,
      municipality == "TIJUANA" ~ 2004,
      municipality == "ROSARITO" ~ 2005,
      TRUE ~ NA_real_
    )
  )
# Generate valid votes and apply ranking logic
collapsed_2010 <- collapsed_2010 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN_PANAL_PES, PRI_PVEM, PRD, PT_PC, PEBC)), na.rm = TRUE))

# Add year and month
collapsed_2010 <- collapsed_2010 %>%
  dplyr::mutate(
    year = 2010,
    month = "August",
    turnout = total / listanominal
  )

rm(data_2010)

#####################################
### PROCESSING DATA FOR 2013
#####################################

data_2013 <- read_csv("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/2013/Ayu_Seccion_2013.csv")
names(data_2013)
# Rename columns
data_2013 <- data_2013 %>%
  dplyr::rename(
    municipality = "MUNICIPIO",
    section = "CASILLA",
    listanominal = `LISTA NOMINAL`
  )


# Compute total votes
data_2013 <- data_2013 %>%
  dplyr::mutate(total = rowSums(across(c(PAN_PRD_PANAL_PEBC, PRI_PVEM_PES_PT, MC, NULO)), na.rm = TRUE))


# Filter out invalid rows
data_2013 <- data_2013 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2013 <- data_2013 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN_PRD_PANAL_PEBC, PRI_PVEM_PES_PT, MC, total,NULO,listanominal), sum, na.rm = TRUE))%>% 
  dplyr::rename(nulos=NULO)

# Assign unique municipality IDs
collapsed_2013 <- collapsed_2013 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ENSENADA" ~ 2001,
      municipality == "MEXICALI" ~ 2002,
      municipality == "TECATE" ~ 2003,
      municipality == "TIJUANA" ~ 2004,
      municipality == "P DE ROSARITO" ~ 2005,
      TRUE ~ NA_real_
    )
  )

# Generate valid votes and apply ranking logic
collapsed_2013 <- collapsed_2013 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN_PRD_PANAL_PEBC, PRI_PVEM_PES_PT, MC)), na.rm = TRUE))

# Add year and month
collapsed_2013 <- collapsed_2013 %>%
  dplyr::mutate(
    year = 2013,
    month = "July",
    turnout = total / listanominal
  )

rm(data_2013)

#####################################
### PROCESSING DATA FOR 2016
#####################################

# Load 2016 data from Excel
data_2016 <- readxl::read_excel("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/2016/Baja California_2016.xlsx", sheet = 1)
names(data_2016)
# Rename columns for consistency
data_2016 <- data_2016 %>%
  dplyr::mutate(
    PRI_PVEM_PT_PANAL = PRI + PVEM + PT + `NA` + C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11,
    MC = `MOV. CIUD.`,
    listanominal = lista_nominal
  ) %>%
  dplyr::select(-c(PRI, PVEM, PT, `NA`, starts_with("C"),`MOV. CIUD.`,lista_nominal))

# Compute total votes
data_2016 <- data_2016 %>%
  dplyr::mutate(total = rowSums(across(c(PAN,PRD,PBC,PES,MORENA,PPC,MUNICIPALISTA,
                                         HUMANISTA,PRI_PVEM_PT_PANAL,MC,starts_with("ind"), nulo)), na.rm = TRUE))

# Filter out invalid rows
data_2016 <- data_2016 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2016 <- data_2016 %>%
  dplyr::group_by(uniqueid, section) %>%
  dplyr::summarise(across(c(PAN,PRD,PBC,PES,MORENA,PPC,MUNICIPALISTA,
                            HUMANISTA,PRI_PVEM_PT_PANAL,MC,starts_with("ind"), total,nulo,listanominal), sum, na.rm = TRUE))%>% 
  dplyr::rename(nulos=nulo)
# Generate valid votes and apply ranking logic
collapsed_2016 <- collapsed_2016 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN,PRD,PBC,PES,MORENA,PPC,MUNICIPALISTA,
                                         HUMANISTA,PRI_PVEM_PT_PANAL,MC,starts_with("ind"))),
                                na.rm = TRUE))


# Generate year and month columns
collapsed_2016 <- collapsed_2016 %>%
  dplyr::mutate(
    year = 2016,
    month = "June",
    turnout = total / listanominal)

rm(data_2016)

#####################################
### PROCESSING DATA FOR 2019
#####################################

# Load 2019 data
data_2019 <- readxl::read_excel("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/2019/ComputoPorCasilla_Mun.xlsx", sheet = "Todos_Ayuntamientos", range = "A6:AF4811")
names(data_2019)
# Rename and clean columns
data_2019 <- data_2019 %>%
  dplyr::mutate(
    section = SECCION,
    municipality = MUNICIPIO,
    total = `TOTAL VOTOS`,
    listanominal = `LISTA NOMINAL`,
    PVEM_PT_TRA_MORENA = PVEM + PT + TRANS + MORENA + C1 + C2 + 
      C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11,
    ind1 = `Alfredo Moreno Carreño`,
    ind2 = `Kevin Fernando Peraza Estrada`,
    ind3 = `Gustavo Flores Betanzos`,
    ind4 = `Rogelio Castro Segovia`,
    nulos = `VOTO NULO`) %>%
  select(-c(PVEM, PT, TRANS, 
            MORENA, SECCION, MUNICIPIO,`TOTAL VOTOS`,`LISTA NOMINAL`,
            starts_with("C"), `Rogelio Castro Segovia`, `Gustavo Flores Betanzos`,
            `Kevin Fernando Peraza Estrada`,`Alfredo Moreno Carreño`,`VOTO NULO`))

names(data_2019)

# Assign uniqueid based on municipality

data_2019 <- data_2019 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ENSENADA" ~ 2001,
      municipality == "MEXICALI" ~ 2002,
      municipality == "TECATE" ~ 2003,
      municipality == "TIJUANA" ~ 2004,
      municipality == "ROSARITO" ~ 2005,
      TRUE ~ NA_real_
    )
  )
names(data_2019)
# Collapse by municipality and section
collapsed_2019 <- data_2019 %>%
  dplyr::group_by(uniqueid, section) %>%
  dplyr::summarise(across(c(PAN,PRI,PRD,PBC,MC,PVEM_PT_TRA_MORENA,total,listanominal,nulos,
                            ind1,ind2,ind3,ind4), sum, na.rm = TRUE))

# Calculate valid votes
collapsed_2019 <- collapsed_2019 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(valid = sum(c_across(c(PAN, PRI, PRD, PBC, MC, PVEM_PT_TRA_MORENA, starts_with("ind"))), na.rm = TRUE))

# Calculate turnout
collapsed_2019 <- collapsed_2019 %>%
  mutate(
    turnout = total / listanominal,
    year = 2019,
    month ="June"
  )

rm(data_2019)

#####################################
### PROCESSING DATA FOR 2021  ----
#####################################

# Load 2021 data
data_2021 <- readxl::read_excel("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/2021/ComputoPorCasilla_Mun_Ajustado_Tribunal_SG.xls", range = "A6:AF4973")
names(data_2021)

# Rename and clean columns
data_2021 <- data_2021 %>%
  dplyr::rename(
    section = SECCION,
    municipality = MUNICIPIO,
    total = `TOTAL VOTOS`,
    listanominal = `LISTA NOMINAL`,
    PAN_PRI_PRD = `PAN+\r\nPRI+\r\nPRD`,
    PAN_PRD = `PAN + PRD`,
    PAN_PRI = `PAN + PRI`,
    PRI_PRD = `PRI+PRD`,
    PVEM_PT_MORENA = `PT+PVEM+MORENA`,
    PVEM_MORENA = `PVEM+\r\nMORENA`,
    PT_MORENA = `PT+\r\nMORENA`,
    PT_PVEM = `PT+PVEM`,
    ind1 = `Rogelio Castro Segovia`,
    ind2 = `Marco Antonio Vizcarra Calderón`,
    ind3 = `Cesar Iván Sanchez Alvarez`,
    ind4 = `Celso Arturo Figueroa Medel`,
    ind5 = `Luis Fernando Serrano García`,
    nulos = `VOTO NULO`) 

names(data_2021)

# Assign uniqueid based on municipality

data_2021 <- data_2021 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ENSENADA" ~ 2001,
      municipality == "MEXICALI" ~ 2002,
      municipality == "TECATE" ~ 2003,
      municipality == "TIJUANA" ~ 2004,
      municipality == "ROSARITO" ~ 2005,
      TRUE ~ NA_real_
    )
  )
names(data_2021)
# Collapse by municipality and section
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(uniqueid, section) %>%
  dplyr::summarise(across(c(PAN:PVEM_MORENA, total, listanominal), \(x) sum(x, na.rm = TRUE)))

# Calculate valid votes
collapsed_2021 <- collapsed_2021 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(valid = sum(c_across(PAN:PVEM_MORENA), na.rm = TRUE))

# Calculate turnout
collapsed_2021 <- collapsed_2021 %>%
  mutate(
    turnout = total / listanominal,
    year = 2021,
    month ="June"
  )

rm(data_2021)

#####################################
### PROCESSING DATA FOR 2024  ----
#####################################

# Load 2024 data
data_2024 <- readxl::read_excel("../../../Data/Raw Electoral Data/Baja California - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019,2021,2024/2024/ComputoPorCasilla_Mun Encabezados.xls", range = "A6:V5396")
names(data_2024)

# Rename and clean columns
data_2024 <- data_2024 %>%
  dplyr::rename(
    section = SECCION,
    municipality = MUNICIPIO,
    total = `TOTAL VOTOS`,
    listanominal = `LISTA NOMINAL`,
    PVEM_MORENA_FXM = `PVEM +\r\nMORENA +\r\nFPM`,
    PVEM_MORENA = `PVEM + MORENA`,
    MORENA_FXM = `MORENA + FPM`,
    PVEM_FXM = `PVEM + FPM`,
    ind1 = `Alfredo Aviña Galvan`,
    nulos = `VOTO NULO`) 

names(data_2024)

# Assign uniqueid based on municipality

data_2024 <- data_2024 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ENSENADA" ~ 2001,
      municipality == "MEXICALI" ~ 2002,
      municipality == "TECATE" ~ 2003,
      municipality == "TIJUANA" ~ 2004,
      municipality == "ROSARITO" ~ 2005,
      municipality == "SAN QUINTIN" ~ 2006,
      municipality == "SAN FELIPE" ~ 2007,
      TRUE ~ NA_real_
    )
  )


# Collapse by municipality and section
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(uniqueid, section) %>%
  dplyr::summarise(across(c(PAN:ind1, total, listanominal), \(x) sum(x, na.rm = TRUE)))

# Calculate valid votes
collapsed_2024 <- collapsed_2024 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(valid = sum(c_across(PAN:ind1), na.rm = TRUE))

# Calculate turnout
collapsed_2024 <- collapsed_2024 %>%
  mutate(
    turnout = total / listanominal,
    year = 2024,
    month ="June"
  )

rm(data_2024)



#####################################
### Merging Data  ----
#####################################

# Load all collapsed data and combine into one dataset
final_data <- bind_rows(
  collapsed_1995,
  collapsed_1998,
  collapsed_2001,
  collapsed_2004,
  collapsed_2007,
  collapsed_2010,
  collapsed_2013,
  collapsed_2016,
  collapsed_2019,
  collapsed_2021,
  collapsed_2024
)

data.table::fwrite(final_data,"../../../Processed Data/baja/baja_process_raw_data.csv")
