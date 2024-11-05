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

#####################################
### PROCESSING DATA FOR 1999
#####################################

# Load 1999 data
data_1999 <- read_csv("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018/Ayu_Seccion_1999.csv")

# Rename columns for consistency
data_1999 <- data_1999 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCIN,
                listanominal = `LISTA NOMINAL`)

# Filter out missing municipality or section
data_1999 <- data_1999 %>%
  dplyr::filter(municipality != "" & section != ".")

# Collapse by municipality and section
collapsed_1999 <- data_1999 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN:PPS, NULO, TOTAL, listanominal), sum, na.rm = TRUE))

# Replace total values with NA if total is 0
collapsed_1999 <- collapsed_1999 %>%
  dplyr::rename(total = TOTAL) %>% 
  dplyr::mutate(total = ifelse(total == 0, NA, total))

# Rename columns
collapsed_1999 <- collapsed_1999 %>%
  dplyr::rename(nulos = NULO,
                PRD_PT = `PRD-PT`)

# Generate turnout and uniqueid
collapsed_1999 <- collapsed_1999 %>%
  dplyr::mutate(turnout = total / listanominal,
                uniqueid = case_when(
                  municipality == "COMONDU" ~ 3001,
                  municipality == "MULEGE" ~ 3002,
                  municipality == "LA PAZ" ~ 3003,
                  municipality == "LOS CABOS" ~ 3008,
                  municipality == "LORETO" ~ 3009,
                  TRUE ~ NA_real_
                ))

# Calculate valid votes
collapsed_1999 <- collapsed_1999 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD_PT, PVEM, MRPS, PPS)), na.rm = TRUE),
                turnout = total / listanominal)

# Add year and month columns
collapsed_1999 <- collapsed_1999 %>%
  mutate(year = 1999,
         month = "February")

# Save the dataset
rm(data_1999)

#####################################
### PROCESSING DATA FOR 2002
#####################################

# Load 2002 data
data_2002 <- read_csv("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018/Ayu_Seccion_2002.csv")

# Rename columns for consistency
data_2002 <- data_2002 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = `LISTA NOMINAL`)

# Filter out missing municipality or section, and non-positive total
data_2002 <- data_2002 %>%
  dplyr::filter(municipality != "" & section != "." & TOTAL > 0)

# Convert to numeric
data_2002 <- data_2002 %>%
  dplyr::mutate(across(c(PAN:PRS, TOTAL, listanominal), as.numeric))

# Collapse by municipality and section
collapsed_2002 <- data_2002 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN:PRS, TOTAL, listanominal, NULOS), sum, na.rm = TRUE))

# Rename columns
collapsed_2002 <- collapsed_2002 %>%
  dplyr::rename(PRD_PT = `PRD-PT`,
                PC = CPDPPN,
                total = TOTAL,
                nulos = NULOS)

# Generate turnout and uniqueid
collapsed_2002 <- collapsed_2002 %>%
  dplyr::mutate(turnout = total / listanominal,
                uniqueid = case_when(
                  municipality == "COMONDU" ~ 3001,
                  municipality == "MULEGE" ~ 3002,
                  municipality == "LA PAZ" ~ 3003,
                  municipality == "LOS CABOS" ~ 3008,
                  municipality == "LORETO" ~ 3009,
                  TRUE ~ NA_real_
                ))

# Calculate valid votes
collapsed_2002 <- collapsed_2002 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD_PT, PVEM, PSN, PC, PAS, PRS)), na.rm = TRUE))

# Add year and month columns
collapsed_2002 <- collapsed_2002 %>%
  dplyr::mutate(year = 2002,
                month = "February")

rm(data_2002)

#####################################
### PROCESSING DATA FOR 2005
#####################################

# Load 2005 data
data_2005 <- read_csv("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018/Ayu_Seccion_2005.csv")

# Rename columns for consistency
data_2005 <- data_2005 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTANOMINAL)

# Filter out missing municipality or section, and non-positive total
data_2005 <- data_2005 %>%
  dplyr::filter(municipality != "" & section != "." & TOTAL > 0)

# Convert to numeric
data_2005 <- data_2005 %>%
  dplyr::mutate(across(c(PAN:PT, TOTAL, listanominal, NULOS), as.numeric))

# Collapse by municipality and section
collapsed_2005 <- data_2005 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN:PT, TOTAL, listanominal,NULOS), sum, na.rm = TRUE))

# Rename columns
collapsed_2005 <- collapsed_2005 %>%
  dplyr::rename(PRI_PVEM = `PRI-PVEM`,
                PRD_PC = `PRD-PC`,
                total = TOTAL,
                nulos = NULOS)

# Generate turnout and uniqueid
collapsed_2005 <- collapsed_2005 %>%
  dplyr::mutate(turnout = total / listanominal,
                uniqueid = case_when(
                  municipality == "COMONDU" ~ 3001,
                  municipality == "MULEGE" ~ 3002,
                  municipality == "LA PAZ" ~ 3003,
                  municipality == "LOS CABOS" ~ 3008,
                  municipality == "LORETO" ~ 3009,
                  TRUE ~ NA_real_
                ))

# Calculate valid votes
collapsed_2005 <- collapsed_2005 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI_PVEM, PRD_PC, PT)), na.rm = TRUE))

# Add year and month columns
collapsed_2005 <- collapsed_2005 %>%
  dplyr::mutate(year = 2005,
                month = "February")

rm(data_2005)

#####################################
### PROCESSING DATA FOR 2008
#####################################

# Load 2008 data
data_2008 <- read_csv("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018/Ayu_Seccion_2008.csv")

# Rename columns for consistency
data_2008 <- data_2008 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTANOMINAL,
                nulos = NULOS,
                total = TOTAL)

# Filter out missing municipality or section, and non-positive total
data_2008 <- data_2008 %>%
  dplyr::filter(municipality != "" & section != "." & total > 0)

# Convert to numeric
data_2008 <- data_2008 %>%
  dplyr::mutate(across(c(PAN:PANAL, total, listanominal, nulos), as.numeric))

# Collapse by municipality and section
collapsed_2008 <- data_2008 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN:PANAL, total, listanominal, nulos), sum, na.rm = TRUE))

# Rename columns
collapsed_2008 <- collapsed_2008 %>%
  dplyr::rename(PAN_PVEM = "PAN-PVEM",
                PRD_PT_PC = "PRD-PT-PC",
                PRI_PMRPS = "PRI-PMRPS",
                PRI_PVEM = "PRI-PVEM")

# Generate turnout and uniqueid
collapsed_2008 <- collapsed_2008 %>%
  dplyr::mutate(turnout = total / listanominal,
                uniqueid = case_when(
                  municipality == "COMONDU" ~ 3001,
                  municipality == "MULEGE" ~ 3002,
                  municipality == "LA PAZ" ~ 3003,
                  municipality == "LOS CABOS" ~ 3008,
                  municipality == "LORETO" ~ 3009,
                  TRUE ~ NA_real_
                ))

# Calculate valid votes
collapsed_2008 <- collapsed_2008 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PAN_PVEM, PRI, PRI_PVEM, PRI_PMRPS, PRD_PT_PC, PANAL)), na.rm = TRUE))

# Add year and month columns
collapsed_2008 <- collapsed_2008 %>%
  dplyr::mutate(year = 2008,
                month = "February")

rm(data_2008)

#####################################
### PROCESSING DATA FOR 2011
#####################################

# Load 2011 data
data_2011 <- data.table::fread("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018/Ayu_Seccion_2011.csv")
names(data_2011)
# Rename columns for consistency
data_2011 <- data_2011 %>%
  dplyr::rename(municipality = Municipio,
                section = "Secci\xf3n",
                listanominal = ListaNominal,
                nulos = NULOS)

# Create 'total' column summing party votes
data_2011 <- data_2011 %>%
  dplyr::mutate(
    total = rowSums(across(c("PAN-PRS":nulos)), 
                    na.rm = TRUE))

# Filter out missing municipality or section, and non-positive total
data_2011 <- data_2011 %>%
  dplyr::filter(municipality != "" & section != "." & total > 0)

# Collapse by municipality and section
collapsed_2011 <- data_2011 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c("PAN-PRS":PANAL, total, listanominal, nulos), sum, na.rm = TRUE))

# Rename columns
collapsed_2011 <- collapsed_2011 %>%
  dplyr::rename(PAN_PRS = "PAN-PRS",
                PRD_PT = "PRD-PT",
                PRI_PVEM = "PRI-PVEM")

# Generate turnout and uniqueid
collapsed_2011 <- collapsed_2011 %>%
  dplyr::mutate(turnout = total / listanominal,
                uniqueid = case_when(
                  municipality == "Comondu" ~ 3001,
                  municipality == "Mulege" ~ 3002,
                  municipality == "La Paz" ~ 3003,
                  municipality == "Los Cabos" ~ 3008,
                  municipality == "Loreto" ~ 3009,
                  TRUE ~ NA_real_
                ))

# Calculate valid votes
collapsed_2011 <- collapsed_2011 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN_PRS, PRI_PVEM, PRD_PT, PC, PANAL)), na.rm = TRUE))

# Add year and month columns
collapsed_2011 <- collapsed_2011 %>%
  dplyr::mutate(year = 2011,
                month = "February")

rm(data_2011)

#####################################
### PROCESSING DATA FOR 2015
#####################################

# Load 2015 data
data_2015 <- read_excel("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018/BCS_2015.xlsx")
names(data_2015)

# Load and merge Lista Nominal data
ln_data_2015 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2015/LN2015.dta")

ln_data_2015 <- ln_data_2015 %>%
  dplyr::filter(entidad == 3 & month == 2) %>%
  dplyr::select(seccion, lista) %>% 
  dplyr::rename(section = seccion)

data_2015 <- data_2015 %>%
  dplyr::rename(total = TOTAL,
                PANAL = PNA,
                CI_1 = ind1)

# Collapse by municipality and section
collapsed_2015 <- data_2015 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN:CI_1, total, nulos), sum, na.rm = TRUE))

collapsed_2015 <- data_2015 %>%
  dplyr::left_join(ln_data_2015, by = c("section")) %>% 
  dplyr::rename(listanominal=lista)

# Generate uniqueid and valid votes
collapsed_2015 <- collapsed_2015 %>%
  dplyr::mutate(turnout = total / listanominal,
                uniqueid = case_when(
                  municipality == "COMONDU" ~ 3001,
                  municipality == "MULEGE" ~ 3002,
                  municipality == "LA PAZ" ~ 3003,
                  municipality == "LOS CABOS" ~ 3008,
                  municipality == "LORETO" ~ 3009,
                  TRUE ~ NA_real_
                ),
                valid = rowSums(across(c(PAN, PRI_PVEM, PRD_PT_MC, PANAL, MORENA, PH, PES, CI_1)), 
                                na.rm = TRUE))

# Add year and month columns
collapsed_2015 <- collapsed_2015 %>%
  dplyr::mutate(year = 2015,
                month = "June")

rm(data_2015)
rm(ln_data_2015)

#####################################
### PROCESSING DATA FOR 2018
#####################################

# Load 2018 data
data_2018 <- read_excel("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018/BCS_2018.xlsx")
names(data_2018)

# Rename columns for consistency
data_2018 <- data_2018 %>%
  dplyr::rename(nulos = nulo,
                total = TOTAL,
                CI_1 = ind1)

# Collapse by municipality and section
collapsed_2018 <- data_2018 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN_PRD_PH_PRS:CI_1, total, nulos), sum, na.rm = TRUE))

# Load and merge Lista Nominal data
ln_data_2018 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2018/LN2018.dta")

ln_data_2018 <- ln_data_2018 %>%
  dplyr::filter(entidad == 3 & month == 2) %>%
  dplyr::select(seccion, lista) %>% 
  dplyr::rename(section = seccion)

collapsed_2018 <- collapsed_2018 %>%
  dplyr::left_join(ln_data_2018, by = c("section")) %>% 
  dplyr::rename(listanominal=lista,
                PANAL=PNA)

# Generate uniqueid, turnout, and valid votes
collapsed_2018 <- collapsed_2018 %>%
  dplyr::mutate(turnout = total / listanominal,
                uniqueid = case_when(
                  municipality == "COMONDU" ~ 3001,
                  municipality == "MULEGE" ~ 3002,
                  municipality == "LA PAZ" ~ 3003,
                  municipality == "LOS CABOS" ~ 3008,
                  municipality == "LORETO" ~ 3009,
                  TRUE ~ NA_real_
                ),
                valid = rowSums(across(c(PAN_PRD_PH_PRS, PRI, PT, PVEM, MC, PANAL, MORENA, PES, BCSC, MORENA_PES, CI_1)), na.rm = TRUE))

# Add year and month columns
collapsed_2018 <- collapsed_2018 %>%
  dplyr::mutate(year = 2018,
                month = "July")

rm(data_2018)
rm(ln_data_2018)
summary(collapsed_2018)

# Combine all processed years into one dataset
Baja_California_Sur_ALL <- bind_rows(collapsed_1999, 
                                     collapsed_2002, 
                                     collapsed_2005, 
                                     collapsed_2008, 
                                     collapsed_2011, 
                                     collapsed_2015, 
                                     collapsed_2018)

summary(Baja_California_Sur_ALL)

data.table::fwrite(bajasur_all,"../../../Processed Data/bajasur/bajasur_process_raw_data.csv")

