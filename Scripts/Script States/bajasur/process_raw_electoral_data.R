# Basic setup
rm(list = ls())        # clear all objects in memory
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
data_1999 <- read_csv("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018,2021,2024/1999/Ayu_Seccion_1999.csv")

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
data_2002 <- read_csv("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018,2021,2024/2002/Ayu_Seccion_2002.csv")

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
data_2005 <- read_csv("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018,2021,2024/2005/Ayu_Seccion_2005.csv")

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
data_2008 <- read_csv("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018,2021,2024/2008/Ayu_Seccion_2008.csv")

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
data_2011 <- data.table::fread("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018,2021,2024/2011/Ayu_Seccion_2011.csv")
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
data_2015 <- read_excel("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018,2021,2024/2015/BCS_2015.xlsx")
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

collapsed_2015 <- collapsed_2015 %>%
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
data_2018 <- read_excel("../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018,2021,2024/2018/BCS_2018.xlsx")
names(data_2018)

# Rename columns for consistency
data_2018 <- data_2018 %>%
  dplyr::rename(nulos = nulo,
                total = TOTAL,
                CI_1 = ind1)

# Add a new column `MORENA_PES` by summing `MORENA`, `PES`, and `MORENA_PES` where municipality is not "LORETO"
data_2018 <- data_2018 %>%
  dplyr::mutate(MORENA_PES = ifelse(municipality != "LORETO", MORENA + PES + MORENA_PES, MORENA_PES))

# Set `MORENA` to NA where municipality is not "LORETO"
data_2018 <- data_2018 %>%
  dplyr::mutate(MORENA = ifelse(municipality != "LORETO", NA, MORENA))

# Set `PES` to NA where municipality is not "LORETO"
data_2018 <- data_2018 %>%
  dplyr::mutate(PES = ifelse(municipality != "LORETO", NA, PES))

# Set `MORENA_PES` to NA where municipality is "LORETO"
data_2018 <- data_2018 %>%
  dplyr::mutate(MORENA_PES = ifelse(municipality == "LORETO", NA, MORENA_PES))

# Rename columns: `ind1` to `CI_1`, `PNA` to `PANAL`, and `TOTAL` to `total`
data_2018 <- data_2018 %>%
  dplyr::rename(PANAL = PNA)

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
  dplyr::rename(listanominal=lista)

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

#####################################
### PROCESSING DATA FOR 2021 ----
#####################################

# Define the folder path
folder_path <- "../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018,2021,2024/2021/"

standard_names <- c("municipality", "section", "casilla", "PAN_PRI_PRD_PH_PRS", "PT", "PVEM", "MC", "MORENA", "BCSC", "PANAL", "PES", "RSP", "FXM", "MORENA_PT", "no_reg", "nulo", "total")

file2_names <- c("municipality", "section", "casilla", "PAN_PRI_PRD_PH_PRS", "PT", "MORENA", "BCSC", "PANAL", "PES", "RSP", "MORENA_PT", "no_reg", "nulo", "total")

file9_names <- c("municipality", "section", "casilla", "PAN_PRI_PRD_PH_PRS", "PT", "PVEM", "MC", "MORENA", "PES", "RSP", "FXM", "MORENA_PT", "no_reg", "nulo", "total")

# Process all Excel files from 1 to 11
for (x in c(1,2,3,8,9)) {
  
  # Choose the appropriate column names
  if (x == 2) {
    manual_names <- file2_names
  } else if (x == 9) {
    manual_names <- file9_names
  } else {
    manual_names <- standard_names
  }
  
  # Load the original Excel file, skip first two rows, and convert columns to character for cleaning
  file_path <- paste0(folder_path, "IEEBCS_PLE_2020-2021_AYUNTAMIENTOS_", x, ".xlsx")
  data <- read_excel(file_path, skip = 4, col_names = manual_names) %>%
    mutate(across(everything(), as.character))
  
  # Save the cleaned data as a new Excel file
  cleaned_file_path <- paste0(folder_path, "IEEBCS_PLE_2020-2021_AYUNTAMIENTOS_", x, "_clean.xlsx")
  write.xlsx(data, cleaned_file_path)
  
  # Reload the cleaned data and add uniqueid column
  data <- read_excel(cleaned_file_path)
  data <- data %>%
    mutate(uniqueid = paste0("300", x))
  
  # Change structure of file 2 and 9 to match the rest
  if (x == 2) {
    data <- data %>%
      mutate(
        PVEM = NA_character_,
        MC = NA_character_,
        FXM = NA_character_
      ) %>%
      # Reorder to match standard structure
      select(municipality, section, casilla, PAN_PRI_PRD_PH_PRS, PT, PVEM, MC, MORENA, 
             BCSC, PANAL, PES, RSP, FXM, MORENA_PT, no_reg, nulo, total, uniqueid)
  } else if (x == 9) {
    data <- data %>%
      mutate(
        BCSC = NA_character_,  # File 9 is missing BCSC
        PANAL = NA_character_, # File 9 is missing PANAL  
      ) %>%
      # Reorder to match standard structure
      select(municipality, section, casilla, PAN_PRI_PRD_PH_PRS, PT, PVEM, MC, MORENA, 
             BCSC, PANAL, PES, RSP, FXM, MORENA_PT, no_reg, nulo, total, uniqueid)
  }
  
  # Save the cleaned dataset as RDS
  saveRDS(data, file = paste0("dataset_", x, ".rds"))
  
  # Remove the temporary cleaned file
  file.remove(cleaned_file_path)
}

# Append all the datasets into one
combined_data <- NULL

for (x in c(1,2,3,8,9)) {
  temp_data <- readRDS(paste0("dataset_", x, ".rds"))
  combined_data <- bind_rows(combined_data, temp_data)
  file.remove(paste0("dataset_", x, ".rds"))  # Erase the individual dataset file after appending
}

names(combined_data)
# Collapse the data by municipality, uniqueid, and section, summing columns
collapsed_data <- combined_data %>%
  dplyr::mutate(across(PAN_PRI_PRD_PH_PRS:total, as.numeric)) %>% 
  dplyr::group_by(uniqueid,section, municipality) %>%
  dplyr::summarise(across(c(PAN_PRI_PRD_PH_PRS:MORENA_PT, total), \(x) sum(x, na.rm = TRUE)))

# Load the Lista Nominal 2021 data and filter by criteria
ln_2021 <- read_excel("../../../Data/Raw Electoral Data/Listas Nominales/listanom_pef21.xlsx", skip = 3, 
                      col_names = c("state_code", "district_code", "mun_code", 
                                    "section", "col_e", "col_f", "col_g", "col_h", 
                                    "col_i", "col_j", "col_k", "col_l",
                                    "listanominal", "col_n", "col_o", "col_p")) %>%
  dplyr::select(state_code, mun_code, section, listanominal) %>% 
  dplyr::filter(state_code == 3) %>%
  dplyr::select(section,listanominal)

collapsed_data$section <- as.numeric(collapsed_data$section)

# Merge Lista Nominal data with the collapsed data
collapsed_2021 <- collapsed_data %>%
  left_join(ln_2021, by = "section")

# Calculate the valid votes
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(valid = rowSums(across(PAN_PRI_PRD_PH_PRS:MORENA_PT), na.rm = TRUE),
                turnout = total / listanominal,  # Case-sensitive column names
                year = 2021,
                month = "June",
                uniqueid = as.numeric(uniqueid)
  )

rm(ln_2021)
rm(collapsed_data)
rm(combined_data)
rm(temp_data)
rm(data)

#####################################
### PROCESSING DATA FOR 2024 ----
#####################################

# Define the folder path
folder_path <- "../../../Data/Raw Electoral Data/Baja California Sur - 1999, 2002, 2005, 2008, 2011,2015,2018,2021,2024/2024/"

standard_names <- c("municipality", "section", "casilla", "PAN_PRI_PRD_PH_PRS", "MORENA_PT_PVEM_PANAL", "MC", "FXM", "ML", "no_reg", "nulo", "total")

other_names <- c("municipality", "section", "casilla", "PAN_PRI_PRD_PH_PRS", "MORENA_PT_PVEM_PANAL", "MC", "FXM", "no_reg", "nulo", "total")

# Process all Excel files from 1 to 11
for (x in c(1,2,3,8,9)) {
  
  # Choose the appropriate column names
  if (x == 2 | x == 9) {
    manual_names <- other_names
  } else {
    manual_names <- standard_names
  }
  
  # Load the original Excel file, skip first two rows, and convert columns to character for cleaning
  file_path <- paste0(folder_path, "IEEBCS_PLE_2023-2024_AYUNTAMIENTOS_", x, ".xlsx")
  data <- read_excel(file_path, skip = 4, col_names = manual_names) %>%
    mutate(across(everything(), as.character))
  
  # Save the cleaned data as a new Excel file
  cleaned_file_path <- paste0(folder_path, "IEEBCS_PLE_2023-2024_AYUNTAMIENTOS_", x, "_clean.xlsx")
  write.xlsx(data, cleaned_file_path)
  
  # Reload the cleaned data and add uniqueid column
  data <- read_excel(cleaned_file_path)
  data <- data %>%
    mutate(uniqueid = paste0("300", x))
  
  # Change structure of file 2 and 9 to match the rest
  if (x == 2 | x == 9) {
    data <- data %>%
      mutate(
        ML = NA_character_
      ) %>%
      # Reorder to match standard structure
      select(municipality, section, casilla, PAN_PRI_PRD_PH_PRS,
             MORENA_PT_PVEM_PANAL, MC, FXM, ML, no_reg, nulo, total, uniqueid)
  }
  
  # Save the cleaned dataset as RDS
  saveRDS(data, file = paste0("dataset_", x, ".rds"))
  
  # Remove the temporary cleaned file
  file.remove(cleaned_file_path)
}

# Append all the datasets into one
combined_data <- NULL

for (x in c(1,2,3,8,9)) {
  temp_data <- readRDS(paste0("dataset_", x, ".rds"))
  combined_data <- bind_rows(combined_data, temp_data)
  file.remove(paste0("dataset_", x, ".rds"))  # Erase the individual dataset file after appending
}

names(combined_data)
# Collapse the data by municipality, uniqueid, and section, summing columns
collapsed_data <- combined_data %>%
  dplyr::mutate(across(PAN_PRI_PRD_PH_PRS:total, as.numeric)) %>% 
  dplyr::group_by(uniqueid,section, municipality) %>%
  dplyr::summarise(across(c(PAN_PRI_PRD_PH_PRS:ML, total), \(x) sum(x, na.rm = TRUE)))

# Load the Lista Nominal 2024 data and filter by criteria
ln_2024 <- read_excel("../../../Data/Raw Electoral Data/Listas Nominales/listanom_pef24.xlsx", skip = 2, 
                      col_names = c("state_code", "district_code", "mun_code", 
                                    "section", "col_e", "col_f", "col_g", "col_h", 
                                    "col_i", "col_j", "col_k", "listanominal")) %>%
  dplyr::select(state_code, mun_code, section, listanominal) %>% 
  dplyr::filter(state_code == 3) %>%
  dplyr::select(section,listanominal)

collapsed_data$section <- as.numeric(collapsed_data$section)

# Merge Lista Nominal data with the collapsed data
collapsed_2024 <- collapsed_data %>%
  left_join(ln_2024, by = "section")

# Calculate the valid votes
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(valid = rowSums(across(PAN_PRI_PRD_PH_PRS:ML), na.rm = TRUE),
                turnout = total / listanominal,  # Case-sensitive column names
                year = 2024,
                month = "June",
                uniqueid = as.numeric(uniqueid)
  )

rm(ln_2024)
rm(collapsed_data)
rm(combined_data)
rm(temp_data)
rm(data)

# Combine all processed years into one dataset
bajasur_all <- bind_rows(collapsed_1999, 
                         collapsed_2002, 
                         collapsed_2005, 
                         collapsed_2008, 
                         collapsed_2011, 
                         collapsed_2015, 
                         collapsed_2018,
                         collapsed_2021,
                         collapsed_2024
)

validation <- bajasur_all %>% 
  dplyr::group_by(year,uniqueid,section) %>% 
  dplyr::summarise(Count = n())

data.table::fwrite(bajasur_all,"../../../Processed Data/bajasur/bajasur_process_raw_data.csv")

