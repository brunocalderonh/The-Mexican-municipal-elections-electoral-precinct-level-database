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
### PROCESSING DATA FOR 2004
#####################################

# Load the 2004 data
data_2004 <- read_csv("../../../Data/Raw Electoral Data/Aguascalientes - 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_2004.csv")
# Check column names for case sensitivity
names(data_2004)

# Rename columns for easier manipulation
data_2004 <- data_2004 %>%
  dplyr::rename(
    "municipality" = "Municipio",  # Ensure exact case match
    "section" = "Secc",            # Case-sensitive renaming
    "listanominal" = "List_Nomi"   # Case-sensitive renaming
  )

# Filter out rows where 'Total' is missing or zero
data_2004 <- data_2004 %>%
  dplyr::rename(total = Total) %>% 
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section (case-sensitive column names)
collapsed_2004 <- data_2004 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(Pan, Alianza, Coalicion, total, listanominal, Nulo), sum, na.rm = TRUE))

# Rename columns to match the structure
collapsed_2004 <- collapsed_2004 %>%
  dplyr::rename(
    "PAN" = "Pan",                # Column names are case-sensitive
    "PRI_PT_PVEM" = "Alianza",    # Ensure exact case match
    "PRD_PC" = "Coalicion",       # Ensure exact case match
      "nulos"     = "Nulo"
  )

# Generate 'turnout' and 'uniqueid' columns
collapsed_2004 <- collapsed_2004 %>%
  dplyr::mutate(
    turnout = total / listanominal,  # Case-sensitive column names
    uniqueid = case_when(
      municipality == "AGUASCALIENTES" ~ 1001,
      municipality == "ASIENTOS" ~ 1002,
      municipality == "CALVILLO" ~ 1003,
      municipality == "COSIO" ~ 1004,
      municipality == "EL LLANO" ~ 1010,
      municipality == "JESUS MARIA" ~ 1005,
      municipality == "PABELLON DE ARTEAGA" ~ 1006,
      municipality == "RINCON DE ROMOS" ~ 1007,
      municipality == "SAN FCO DE LOS ROMO" ~ 1011,
      municipality == "SAN JOSE DE GRACIA" ~ 1008,
      municipality == "TEPEZALA" ~ 1009,
      TRUE ~ NA_real_  # Ensuring all cases are handled
    )
  )

# Generate 'valid' column and calculate municipal-level aggregates
collapsed_2004 <- collapsed_2004 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(valid = sum(c_across(PAN:PRD_PC), na.rm = TRUE))  # Sum of key party votes

# Add year and month
collapsed_2004 <- collapsed_2004 %>%
  dplyr::mutate(
    year = 2004,     # Adding year as 2004
    month = "August" # Adding month as August
  )

rm(data_2004)
summary(collapsed_2004)
#####################################
### PROCESSING DATA FOR 2007
#####################################

# Load the 2007 data
data_2007 <- read_csv("../../../Data/Raw Electoral Data/Aguascalientes - 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_2007.csv")

# Check column names for case sensitivity
names(data_2007)

# Rename columns (ensure case-sensitive matching)
data_2007 <- data_2007 %>%
  dplyr::rename(
    "municipality" = "nombre_municipio",  # Correct case-sensitive match
    "section" = "seccion",                # Correct case-sensitive match
    "listanominal" = "lista_nominal"      # Correct case-sensitive match
  )

# Filter out rows where 'Total' is missing or zero
data_2007 <- data_2007 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2007 <- data_2007 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c("pan-panal", pri, prd, pt, pvem, pc, pa, total, listanominal, anulados), sum, na.rm = TRUE))

# Rename columns for case-sensitive matching
collapsed_2007 <- collapsed_2007 %>%
  dplyr::rename(
    "PAN_PANAL" = "pan-panal",  # Ensure exact case matching
    "PRI" = "pri",             # Ensure exact case matching
    "PRD" = "prd",             # Ensure exact case matching
    "PT" = "pt",               # Ensure exact case matching
    "PVEM" = "pvem",           # Ensure exact case matching
    "PC" = "pc",               # Ensure exact case matching
    "PAS" = "pa",               # Ensure exact case matching
    "nulos" = "anulados"
  )

# Generate 'turnout' and 'uniqueid' columns
collapsed_2007 <- collapsed_2007 %>%
  dplyr::mutate(
    turnout = total / listanominal,  # Case-sensitive column names
    uniqueid = case_when(
      municipality == "AGUASCALIENTES" ~ 1001,
      municipality == "ASIENTOS" ~ 1002,
      municipality == "CALVILLO" ~ 1003,
      municipality == "COSIO" ~ 1004,
      municipality == "JESUS MARIA" ~ 1005,
      municipality == "PABELLON DE ARTEAGA" ~ 1006,
      municipality == "RINCON DE ROMOS" ~ 1007,
      municipality == "SAN JOSE DE GRACIA" ~ 1008,
      municipality == "TEPEZALA" ~ 1009,
      municipality == "EL LLANO" ~ 1010,
      municipality == "SAN FRANCISCO DE LOS ROMO" ~ 1011,
      TRUE ~ NA_real_
    )
  )

# Generate 'valid' column summing votes and calculate municipal aggregates
collapsed_2007 <- collapsed_2007 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(valid = sum(c_across(PAN_PANAL:PAS), na.rm = TRUE))

# Add year and month
collapsed_2007 <- collapsed_2007 %>%
  dplyr::mutate(
    year = 2007,
    month = "August"
  )

rm(data_2007)
summary(collapsed_2007)
#####################################
### PROCESSING DATA FOR 2010
#####################################

# Load the 2010 data
data_2010 <- read_csv("../../../Data/Raw Electoral Data/Aguascalientes - 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_2010.csv")

# Check column names for case sensitivity
names(data_2010)

# Rename columns
data_2010 <- data_2010 %>%
  dplyr::rename(
    "municipality" = "nombre_municipio",
    "section" = "seccion",
    "listanominal" = "listanominal"
  )

# Drop rows where 'Total' is missing or zero
data_2010 <- data_2010 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse by municipality and section
collapsed_2010 <- data_2010 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN, PRD, PT, CONVERG, PRI, PVEM, PANAL, "PRI-PVEM", "PRI-PANAL","PVEM-PANAL", "PRI-PVEM-PANAL", total, listanominal,anulados), sum, na.rm = TRUE))

# Combine columns related to PRI, PVEM, PANAL
collapsed_2010 <- collapsed_2010 %>%
  dplyr::mutate(pripvempanal = PRI + PVEM + PANAL + `PRI-PVEM` + `PRI-PANAL` + `PVEM-PANAL` + `PRI-PVEM-PANAL`) %>%
  dplyr::select(-c(PRI, PVEM, PANAL, `PRI-PVEM`, `PRI-PANAL`, `PVEM-PANAL`,`PRI-PVEM-PANAL`)) %>%
  dplyr::rename(
    "PAN" = "PAN",
    "PRI_PVEM_PANAL" = "pripvempanal",
    "PRD" = "PRD",
    "PT" = "PT",
    "PC" = "CONVERG",
    "nulos" = "anulados"
  )
names(collapsed_2010)
# Generate 'turnout' and 'uniqueid' columns
collapsed_2010 <- collapsed_2010 %>%
  dplyr::mutate(
    turnout = total / listanominal,
    uniqueid = case_when(
      municipality == "AGUASCALIENTES" ~ 1001,
      municipality == "ASIENTOS" ~ 1002,
      municipality == "CALVILLO" ~ 1003,
      municipality == "COSIO" ~ 1004,
      municipality == "JESUS MARIA" ~ 1005,
      municipality == "PABELLON DE ARTEAGA" ~ 1006,
      municipality == "RINCON DE ROMOS" ~ 1007,
      municipality == "SAN JOSE DE GRACIA" ~ 1008,
      municipality == "TEPEZALA" ~ 1009,
      municipality == "EL LLANO" ~ 1010,
      municipality == "SAN FRANCISCO DE LOS ROMO" ~ 1011,
      TRUE ~ NA_real_
    )
  )

# Generate 'valid' column and calculate municipal aggregates
collapsed_2010 <- collapsed_2010 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(valid = sum(c_across(PAN:PC), na.rm = TRUE))

# Add year and month
collapsed_2010 <- collapsed_2010 %>%
  dplyr::mutate(
    year = 2010,
    month = "July"
  )


rm(data_2010)

#####################################
### PROCESSING DATA FOR 2013
#####################################

# Load the 2013 data from Excel
data_2013 <- read_excel("../../../Data/Raw Electoral Data/Aguascalientes - 2004, 2007, 2010, 2013,2016,2019/CASILLAS_AYUNTAMIENTOS_2013.xls", sheet = "Ayuntamientos")

# Check the column names for case sensitivity
names(data_2013)

# Rename columns
data_2013 <- data_2013 %>%
  dplyr::rename(
    "municipality" = "MUNICIPIO",
    "section" = "SECCIÓN"
  )

# Create 'total' column summing party votes
data_2013 <- data_2013 %>%
  dplyr::mutate(
    total = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, MC, PNA, `PAN-PRD`, `PRI-PVEM`, CNR, VN)), 
                    na.rm = TRUE))

# Filter out rows where 'total' is missing or zero
data_2013 <- data_2013 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Assign unique municipality IDs
data_2013 <- data_2013 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "AGUASCALIENTES" ~ 1001,
      municipality == "ASIENTOS" ~ 1002,
      municipality == "CALVILLO" ~ 1003,
      municipality == "COSÍO" ~ 1004,
      municipality == "JESÚS MARÍA" ~ 1005,
      municipality == "PABELLÓN DE ARTEAGA" ~ 1006,
      municipality == "RINCON DE ROMOS" | municipality == "RINCÓN DE ROMOS" ~ 1007,
      municipality == "SAN JOSÉ DE GRACIA" ~ 1008,
      municipality == "TEPEZALÁ" | municipality == "TEPEZALA" ~ 1009,
      municipality == "EL LLANO" ~ 1010,
      municipality == "SAN FRANCISCO DE LOS ROMO" ~ 1011,
      TRUE ~ NA_real_
    )
  )

# Collapse data by uniqueid and section, summing relevant columns
collapsed_2013 <- data_2013 %>%
  dplyr::group_by(uniqueid, municipality, section) %>%
  dplyr::summarise(across(c(PAN, PRI, PRD, PT, PVEM, MC, PNA, `PAN-PRD`, `PRI-PVEM`, total,VN), sum, na.rm = TRUE))

# Merge with the dataset "ln_all_months_years.dta" using seccion (section) and ed
data_all <- read_dta("../../../Data/Raw Electoral Data/Aguascalientes - 2004, 2007, 2010, 2013,2016,2019/Other/ln_all_months_years.dta")

data_all <- data_all %>% 
  dplyr::filter(state == "AGUASCALIENTES" & month == "June" & year == 2013)  # Keep only records for June 2013

# Merge the datasets
collapsed_2013 <- collapsed_2013 %>%
  dplyr::left_join(data_all %>% dplyr::select(section,lista), by = c("section")) %>% 
  dplyr::rename("listanominal"="lista")

# Generate new columns
collapsed_2013 <- collapsed_2013 %>%
  dplyr::mutate(
    PAN_PRD = PAN + PRD + `PAN-PRD`,     # Sum of PAN, PRD, and PANPRD
    PRI_PVEM = PRI + PVEM + `PRI-PVEM`,  # Sum of PRI, PVEM, and PRIPVEM
    turnout = total / listanominal    # Calculate turnout
  ) %>%
  dplyr::select(-c(PAN, PRD, `PAN-PRD`, PRI, PVEM, `PRI-PVEM`)) %>%
  dplyr::rename(PANAL = PNA,
                nulos = VN)          # Rename PNA to PANAL

# Generate 'valid' column summing relevant party columns
collapsed_2013 <- collapsed_2013 %>%
  dplyr::mutate(valid = rowSums(across(c(PT, MC, PANAL, PAN_PRD, PRI_PVEM)), na.rm = TRUE))

# Add year and month columns
collapsed_2013 <- collapsed_2013 %>%
  dplyr::mutate(
    year = 2013,
    month = "July"
  )

rm(data_2013)
rm(data_all)
names(collapsed_2013)

####################################
### PROCESSING DATA FOR 2016
####################################

# Load the 2016 data
data_2016 <- read_excel("../../../Data/Raw Electoral Data/Aguascalientes - 2004, 2007, 2010, 2013,2016,2019/Municipales2016.xlsx")

# Rename columns
data_2016 <- data_2016 %>%
  dplyr::rename(municipality = municipio,
         section = seccion,
         uniqueid = cve_inegi)

# Filter out rows where 'total' is missing or zero
data_2016 <- data_2016 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Combine parties into one column
data_2016 <- data_2016 %>%
  dplyr::mutate(PRI_PT_PANAL = `PRI-PT-PNA` + `PRI-PT` + `PRI-PNA` + `PT-PNA` + PRI + PT + PNA) %>%
  dplyr::select(-c(`PRI-PT-PNA`, `PRI-PT`, `PRI-PNA`, `PT-PNA`, PRI, PT, PNA))

# Rename independent candidates
data_2016 <- data_2016 %>%
  dplyr::rename(CI_1 = independiente1,
                CI_2 = independiente2)

# Convert section to numeric
data_2016 <- data_2016 %>%
  dplyr::mutate(section = as.numeric(section))

# Collapse by municipality, uniqueid, and section
collapsed_2016 <- data_2016 %>%
  dplyr::group_by(municipality, uniqueid, section) %>%
  dplyr::summarise(across(c(PAN:PRI_PT_PANAL), sum, na.rm = TRUE))

# Add year, month, and state columns
collapsed_2016 <- collapsed_2016 %>%
  dplyr::mutate(year = 2016,
         month = "June")

# Calculate valid votes
collapsed_2016 <- collapsed_2016 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRD, PVEM, MC, MORENA, PES, PRI_PT_PANAL, CI_1, CI_2)), na.rm = TRUE))

# Load and merge Lista Nominal data
ln_data_2016 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2016/LN2016.dta")

ln_data_2016 <- ln_data_2016 %>%
  dplyr::filter(entidad == 1 & month == 5) %>%
  dplyr::mutate(uniqueid = (entidad * 1000) + municipio) %>%
  dplyr::select(seccion, lista) %>% 
  dplyr::rename(section = seccion)

collapsed_2016 <- collapsed_2016 %>%
  dplyr::left_join(ln_data_2016, by = c("section")) %>%
  dplyr::rename(listanominal = lista)

# Calculate turnout
collapsed_2016 <- collapsed_2016 %>%
  dplyr::mutate(turnout = total / listanominal)

names(collapsed_2016)
rm(data_2016)
rm(ln_data_2016)
####################################
### PROCESSING DATA FOR 2019
####################################

# Define the folder path
folder_path <- "../../../Data/Raw Electoral Data/Aguascalientes - 2004, 2007, 2010, 2013,2016,2019/"

# Process all Excel files from 1 to 11
for (x in 1:11) {
  
  # Load the original Excel file, skip first two rows, and convert columns to character for cleaning
  file_path <- paste0(folder_path, "COMPUTO_MUNICIPIO_", x, ".xlsx")
  data <- read_excel(file_path, skip = 2) %>%
    mutate(across(everything(), as.character))
  
  # Drop the first two rows (already skipped in the previous step)
  data <- data %>% filter(row_number() > 2)
  
  # Save the cleaned data as a new Excel file
  cleaned_file_path <- paste0(folder_path, "COMPUTO_MUNICIPIO_", x, "_clean.xlsx")
  write.xlsx(data, cleaned_file_path)
  
  # Reload the cleaned data and add uniqueid column
  data <- read_excel(cleaned_file_path)
  data <- data %>%
    mutate(uniqueid = ifelse(x > 9, paste0("10", x), paste0("100", x)))
  
  # Save the cleaned dataset as RDS
  saveRDS(data, file = paste0("dataset_", x, ".rds"))
  
  # Remove the temporary cleaned file
  file.remove(cleaned_file_path)
}

# Append all the datasets into one
combined_data <- NULL

for (x in 1:11) {
  temp_data <- readRDS(paste0("dataset_", x, ".rds"))
  combined_data <- bind_rows(combined_data, temp_data)
  file.remove(paste0("dataset_", x, ".rds"))  # Erase the individual dataset file after appending
}

# Clean up Casillas and other variables
combined_data <- combined_data %>%
  dplyr::filter(Casillas != "" & Casillas != "TOTAL") %>%
  separate(Casillas, into = c("section"), sep = " ", remove = FALSE) %>%
  dplyr::mutate(across(everything(), as.numeric))

names(combined_data)

# Collapse the data by municipality, uniqueid, and section, summing columns
collapsed_data <- combined_data %>%
  dplyr::group_by(uniqueid,section) %>%
  dplyr::summarise(across(c(PAN:TOTAL,
                            CAND_IND1, CAND_IND2, 
                            CAND_IND3, CAND_IND4), sum, na.rm = TRUE))

collapsed_data <- collapsed_data %>%
  dplyr::rename(
         PVEM = PEVM, 
         MORENA = Morena, 
         nulos = NUM_VOTOS_NULOS,
         PANAL = NAA, 
         CI_1 = CAND_IND2, 
         CI_2 = CAND_IND1, 
         CI_3 = CAND_IND3, 
         CI_4 = CAND_IND4,
         total = TOTAL)

# Load the Lista Nominal 2019 data and filter by criteria
ln_2019 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2019/LN2019.dta") %>%
  dplyr::filter(entidad == 1 & month == 5) %>%
  dplyr::rename(section = seccion, listanominal = lista) %>% 
  dplyr::select(section,listanominal)

collapsed_data$section <- as.numeric(collapsed_data$section)

# Merge Lista Nominal data with the collapsed data
collapsed_2019 <- collapsed_data %>%
  left_join(ln_2019, by = "section")

# Calculate the valid votes
collapsed_2019 <- collapsed_2019 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, MC, MORENA, UPM, PLA, PANAL, CI_1, CI_2, CI_3, CI_4)), na.rm = TRUE),
                turnout = total / listanominal,  # Case-sensitive column names
                year = 2019,
                month = "June",
                uniqueid = as.numeric(uniqueid)
  )

# Combine the dataframes, handling different columns by filling with NA
Aguascalientes_all <- bind_rows(collapsed_2004,
                                collapsed_2007,
                                collapsed_2010,
                                collapsed_2013,
                                collapsed_2016,
                                collapsed_2019)

data.table::fwrite(Aguascalientes_all,"../../../Processed Data/aguascalientes/aguascalientes_process_raw_data.csv")


