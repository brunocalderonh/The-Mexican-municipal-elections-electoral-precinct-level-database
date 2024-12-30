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

# Load CSV file
ayu_seccion_1997 <- fread("../../../Data/Raw Electoral Data/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997.csv",
                          encoding = "Latin-1")
colnames(ayu_seccion_1997) <- tolower(colnames(ayu_seccion_1997))
# Remove "-" and spaces
names(ayu_seccion_1997) <- gsub("[- ]", "", names(ayu_seccion_1997))
# Rename columns
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  rename(
    municipality = municipio,
    section = sección
  )

# Drop rows with missing `municipality`, `section`, or invalid `total`
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  filter(municipality != "" & !is.na(section)) %>%
  filter(!is.na(total) & total != 0)

# Convert relevant columns from string to numeric (equivalent to `destring`)
numeric_vars <- c("listanominal", "pan", "pri", "prd", "pt", "pvem", "total")
ayu_seccion_1997[numeric_vars] <- lapply(ayu_seccion_1997[numeric_vars], as.numeric)

# Collapse (sum) relevant variables by `municipality` and `section`
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:pvem, sum, na.rm = TRUE))

# Rename political party columns to upper case
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem
  )

# Calculate voter turnout
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  mutate(turnout = total / listanominal)

# View the resulting dataset
print(ayu_seccion_1997)

# Create the `uniqueid` column and set its default value to 0
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  mutate(uniqueid = 0)

# Replace `uniqueid` based on municipality values
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  mutate(uniqueid = case_when(
    municipality == "AMEALCO DE BONFIL" ~ 22001,
    municipality == "ARROYO SECO" ~ 22003,
    municipality == "CADEREYTA DE MONTES" ~ 22004,
    municipality == "COLxD3N" ~ 22005,
    municipality == "CORREGIDORA" ~ 22006,
    municipality == "EL MARQUxC9S" ~ 22011,
    municipality == "EZEQUIEL MONTES" ~ 22007,
    municipality == "HUIMILPAN" ~ 22008,
    municipality == "JALPAN DE SERRA" ~ 22009,
    municipality == "LANDA DE MATAMOROS" ~ 22010,
    municipality == "PEDRO ESCOBEDO" ~ 22012,
    municipality == "PExD1AMILLER" ~ 22013,
    municipality == "PINAL DE AMOLES" ~ 22002,
    municipality == "QUERxC9TARO" ~ 22014,
    municipality == "SAN JOAQUxCDN" ~ 22015,
    municipality == "SAN JUAN DEL RxCDO" ~ 22016,
    municipality == "TEQUISQUIAPAN" ~ 22017,
    municipality == "TOLIMxC1N" ~ 22018,
    TRUE ~ uniqueid  # If the municipality doesn't match, keep the original uniqueid
  ))


# Calculate `valid` as the row total for PAN, PRI, PRD, PT, PVEM
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  mutate(valid = PAN + PRI + PRD + PT + PVEM)

# Add the `year` and `month` columns
data_1997 <- ayu_seccion_1997 %>%
  mutate(year = 1997, month = "July")


# Read the CSV file
ayu_seccion_2000 <- fread("../../../Data/Raw Electoral Data/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2000.csv",
                          encoding = "Latin-1")
colnames(ayu_seccion_2000) <- tolower(colnames(ayu_seccion_2000))
# Remove "-" and spaces
names(ayu_seccion_2000) <- gsub("[- ]", "", names(ayu_seccion_2000))
# Rename variables
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  rename(municipality = municipio,
         section = sección)

# Drop rows where `municipality` is empty and `section` is missing
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  filter(municipality != "" & !is.na(section))

# Drop rows where `total` is missing or equal to zero
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  filter(!is.na(total) & total != 0)

# Convert the specified columns to numeric
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  mutate(across(listanominal:psn, as.numeric))

# Collapse (sum) the relevant columns by `municipality` and `section`
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:psn, sum, na.rm = TRUE), .groups = "drop")

# Rename the columns for political parties
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PC = pc,
         PSN = psn)

# Generate the `turnout` variable as the ratio of `total` to `listanominal`
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  mutate(turnout = total / listanominal)

# Replace `uniqueid` based on municipality values
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  mutate(uniqueid = case_when(
    municipality == "AMEALCO DE BONFIL" ~ 22001,
    municipality == "ARROYO SECO" ~ 22003,
    municipality == "CADEREYTA DE MONTES" ~ 22004,
    municipality == "COLxD3N" ~ 22005,
    municipality == "CORREGIDORA" ~ 22006,
    municipality == "EL MARQUxC9S" ~ 22011,
    municipality == "EZEQUIEL MONTES" ~ 22007,
    municipality == "HUIMILPAN" ~ 22008,
    municipality == "JALPAN DE SERRA" ~ 22009,
    municipality == "LANDA DE MATAMOROS" ~ 22010,
    municipality == "PEDRO ESCOBEDO" ~ 22012,
    municipality == "PExD1AMILLER" ~ 22013,
    municipality == "PINAL DE AMOLES" ~ 22002,
    municipality == "QUERxC9TARO" ~ 22014,
    municipality == "SAN JOAQUxCDN" ~ 22015,
    municipality == "SAN JUAN DEL RxCDO" ~ 22016,
    municipality == "TEQUISQUIAPAN" ~ 22017,
    municipality == "TOLIMxC1N" ~ 22018,
    TRUE ~ uniqueid  # If the municipality doesn't match, keep the original uniqueid
  ))

# Rename variables
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  rename(municipality = municipio,
         section = seccin)

# Drop rows where `municipality` is empty and `section` is missing
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  filter(municipality != "" & !is.na(section))

# Drop rows where `total` is missing or equal to zero
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  filter(!is.na(total) & total != 0)

# Convert the specified columns to numeric
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  mutate(across(listanominal:psn, as.numeric))

# Collapse (sum) the relevant columns by `municipality` and `section`
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:psn, sum, na.rm = TRUE), .groups = "drop")

# Rename the columns for political parties
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PC = pc,
         PSN = psn)

# Generate the `turnout` variable as the ratio of `total` to `listanominal`
data_2000 <- ayu_seccion_2000 %>%
  mutate(turnout = total / listanominal)

# Read the CSV file (replace the file path with your actual path)
queretaro_2003 <- fread("../../../Data/Raw Electoral Data/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003.csv", stringsAsFactors = FALSE,
                           encoding = "Latin-1")
colnames(queretaro_2003) <- tolower(colnames(queretaro_2003))
# Remove "-" and spaces
names(queretaro_2003) <- gsub("[- ]", "", names(queretaro_2003))
# Rename columns
queretaro_2003 <- queretaro_2003 %>%
  rename(
    municipality = municipio,
    section = sección
  )

# Drop rows with missing municipality and section or invalid total values
queretaro_2003 <- queretaro_2003 %>%
  filter(municipality != "" & !is.na(section) & total != 0 & !is.na(total))

# Convert listanominal to psn to numeric (as a batch operation)
queretaro_2003 <- queretaro_2003 %>%
  mutate(across(listanominal:psn, as.numeric))

# Collapse (aggregate) the sum of listanominal to psn by municipality and section
queretaro_2003 <- queretaro_2003 %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:psn, sum, na.rm = TRUE)) %>%
  ungroup()

# Rename variables to match the party names
queretaro_2003 <- queretaro_2003 %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem,
    PC = pc,
    PSN = psn
  )

# Handle the special case where PRI equals PVEM (creating PRI_PVEM)
queretaro_2003 <- queretaro_2003 %>%
  mutate(
    PRI_PVEM = if_else(PRI == PVEM, PRI, NA_real_),  # Create PRI_PVEM where PRI == PVEM
    PRI = if_else(!is.na(PRI_PVEM), NA_real_, PRI),  # Set PRI to NA if PRI_PVEM is non-missing
    PVEM = if_else(!is.na(PRI_PVEM), NA_real_, PVEM) # Set PVEM to NA if PRI_PVEM is non-missing
  )

# Calculate turnout as total votes / listanominal
queretaro_2003 <- queretaro_2003 %>%
  mutate(turnout = total / listanominal)

# Replace `uniqueid` based on municipality values
queretaro_2003 <- queretaro_2003 %>%
  mutate(uniqueid = case_when(
    municipality == "AMEALCO DE BONFIL" ~ 22001,
    municipality == "ARROYO SECO" ~ 22003,
    municipality == "CADEREYTA DE MONTES" ~ 22004,
    municipality == "COLxD3N" ~ 22005,
    municipality == "CORREGIDORA" ~ 22006,
    municipality == "EL MARQUxC9S" ~ 22011,
    municipality == "EZEQUIEL MONTES" ~ 22007,
    municipality == "HUIMILPAN" ~ 22008,
    municipality == "JALPAN DE SERRA" ~ 22009,
    municipality == "LANDA DE MATAMOROS" ~ 22010,
    municipality == "PEDRO ESCOBEDO" ~ 22012,
    municipality == "PExD1AMILLER" ~ 22013,
    municipality == "PINAL DE AMOLES" ~ 22002,
    municipality == "QUERxC9TARO" ~ 22014,
    municipality == "SAN JOAQUxCDN" ~ 22015,
    municipality == "SAN JUAN DEL RxCDO" ~ 22016,
    municipality == "TEQUISQUIAPAN" ~ 22017,
    municipality == "TOLIMxC1N" ~ 22018,
    TRUE ~ NA  # If the municipality doesn't match, keep the original uniqueid
  ))

# Create 'valid' as the row sum of specified party columns
queretaro_2003 <- queretaro_2003 %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, PSN), na.rm = TRUE))

# Add the year and month columns
data_2003 <- queretaro_2003 %>%
  mutate(
    year = 2003,
    month = "July"
  )

# Load the CSV file into R
ayuntamiento_2006 <- fread("../../../Data/Raw Electoral Data/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2006.csv",
                           encoding = "Latin-1")
colnames(ayuntamiento_2006) <- tolower(colnames(ayuntamiento_2006))
# Remove "-" and spaces
names(ayuntamiento_2006) <- gsub("[- ]", "", names(ayuntamiento_2006))

# Clean and rename the columns
ayuntamiento_2006 <- ayuntamiento_2006 %>%
  rename(
    municipality = municipio,
    section = seccion
  )

# Drop rows where municipality or section is missing and filter out invalid 'total' values
ayuntamiento_2006 <- ayuntamiento_2006 %>%
  filter(!(is.na(municipality) | is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

# Convert relevant columns to numeric
ayuntamiento_2006 <- ayuntamiento_2006 %>%
  mutate(across(c(PAN:listanominal), as.numeric))

# Collapse (sum) by municipality and section, summing votes for each party and total votes/listanominal
ayuntamiento_2006_summary <- ayuntamiento_2006 %>%
  group_by(municipality, section) %>%
  summarise(
    PAN = sum(PAN, na.rm = TRUE),
    PRI = sum(PRI, na.rm = TRUE),
    PRD = sum(PRD, na.rm = TRUE),
    PT = sum(PT, na.rm = TRUE),
    PVEM = sum(PVEM, na.rm = TRUE),
    PC = sum(convergencia, na.rm = TRUE),
    PRI_PVEM = sum(pripvem, na.rm = TRUE),
    PANAL = sum(panal, na.rm = TRUE),
    total = sum(total, na.rm = TRUE),
    listanominal = sum(listanominal, na.rm = TRUE)
  )

# Generate turnout as the ratio of total votes to the list of nominal voters
ayuntamiento_2006_summary <- ayuntamiento_2006_summary %>%
  mutate(turnout = total / listanominal)

# Create and assign `uniqueid` based on municipality
queretaro_2006 <- queretaro_2006 %>%
  mutate(uniqueid = case_when(
    municipality == "Amealco de Bonfil" ~ 22001,
    municipality == "Arroyo Seco" ~ 22003,
    municipality == "Cadereyta de Montes" ~ 22004,
    municipality == "Colón" ~ 22005,
    municipality == "Corregidora" ~ 22006,
    municipality == "El Marqués" ~ 22011,
    municipality == "Ezequiel Montes" ~ 22007,
    municipality == "Huimilpan" ~ 22008,
    municipality == "Jalpan de Serra" ~ 22009,
    municipality == "Landa de Matamoros" ~ 22010,
    municipality == "Pedro Escobedo" ~ 22012,
    municipality == "Peñamiller" ~ 22013,
    municipality == "Pinal de Amoles" ~ 22002,
    municipality == "Querétaro" ~ 22014,
    municipality == "San Joaquín" ~ 22015,
    municipality == "San Juan del Río" ~ 22016,
    municipality == "Tequisquiapan" ~ 22017,
    municipality == "Tolimán" ~ 22018,
    TRUE ~ NA
  ))

# Calculate `valid` as the row total of relevant vote columns
queretaro_2006 <- queretaro_2006 %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PVEM, PC, PT, PANAL, PRI_PVEM), na.rm = TRUE))

# Add year and month columns
data_2006 <- data_2006 %>%
  mutate(year = 2006, month = "July")

# Sort by section
data_2006 <- data_2006 %>%
  arrange(section)

# Load the CSV data
ayuntamiento_2009 <- fread("../../../Data/Raw Electoral Data/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2009.csv",
                           encoding = "Latin-1")
colnames(ayuntamiento_2009) <- tolower(colnames(ayuntamiento_2009))
# Remove "-" and spaces
names(ayuntamiento_2009) <- gsub("[- ]", "", names(ayuntamiento_2009))

# Rename columns
ayuntamiento_2009 <- ayuntamiento_2009 %>%
  rename(
    municipality = nombre,
    section = seccion
  )

# Drop rows where municipality or section are missing, or total is missing/zero
ayuntamiento_2009 <- ayuntamiento_2009 %>%
  filter(municipality != "", section != "", !is.na(total), total != 0)

# Convert relevant columns to numeric
ayuntamiento_2009 <- ayuntamiento_2009 %>%
  mutate(across(listanominal:pvem, as.numeric))

# Collapse (sum) the data by municipality and section
ayuntamiento_2009_collapsed <- ayuntamiento_2009 %>%
  group_by(municipality, section) %>%
  summarize(across(listanominal:pvem, sum, na.rm = TRUE),
            total = sum(total, na.rm = TRUE))

# Rename the relevant columns
ayuntamiento_2009_collapsed <- ayuntamiento_2009_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem,
    PC = convergencia,
    PSD = psd,
    PANAL = "v9"
  )

# Generate turnout column
ayuntamiento_2009_collapsed <- ayuntamiento_2009_collapsed %>%
  mutate(turnout = total / listanominal)

# You can also write it to a CSV if needed:
# write_csv(ayuntamiento_2009_collapsed, "Queretaro_Section_2009.csv")

# Replace `uniqueid` based on municipality values
ayuntamiento_2009_collapsed <- ayuntamiento_2009_collapsed %>%
  mutate(uniqueid = case_when(
    municipality == "AMEALCO DE BONFIL" ~ 22001,
    municipality == "ARROYO SECO" ~ 22003,
    municipality == "CADEREYTA DE MONTES" ~ 22004,
    municipality == "COLxD3N" ~ 22005,
    municipality == "CORREGIDORA" ~ 22006,
    municipality == "EL MARQUxC9S" ~ 22011,
    municipality == "EZEQUIEL MONTES" ~ 22007,
    municipality == "HUIMILPAN" ~ 22008,
    municipality == "JALPAN DE SERRA" ~ 22009,
    municipality == "LANDA DE MATAMOROS" ~ 22010,
    municipality == "PEDRO ESCOBEDO" ~ 22012,
    municipality == "PExD1AMILLER" ~ 22013,
    municipality == "PINAL DE AMOLES" ~ 22002,
    municipality == "QUERxC9TARO" ~ 22014,
    municipality == "SAN JOAQUxCDN" ~ 22015,
    municipality == "SAN JUAN DEL RxCDO" ~ 22016,
    municipality == "TEQUISQUIAPAN" ~ 22017,
    municipality == "TOLIMxC1N" ~ 22018,
    TRUE ~ uniqueid  # If the municipality doesn't match, keep the original uniqueid
  ))

# Assume data is already loaded into a dataframe called `data`
# You should load the data using read_csv or readRDS if needed:
# data <- read_csv("Queretaro_Section_2009.csv")

# Calculate 'valid' (sum across PAN, PRI, PRD, PC, PANAL, PSD, PT, PVEM)
data <- ayuntamiento_2009_collapsed %>%
  mutate(valid = PAN + PRI + PRD + PC + PANAL + PSD + PT + PVEM)

# Additional variables
data_2009 <- data %>%
  mutate(
    year = 2009,
    month = "July"
  )

# Sort by section
data_2009 <- data %>%
  arrange(section)

# Load the dataset (assuming the file is in CSV format)
data <- read_xlsx("../../../Data/Raw Electoral Data/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2012.xlsx")
colnames(data) <- tolower(colnames(data))
# Remove "-" and spaces
names(data) <- gsub("[- ]", "", names(data))

# Drop rows where municipality or section is empty or total is missing/zero
data <- data %>%
  filter(municipality != "" & section != "" & !is.na(total) & total != 0)

# Collapse data by municipality and section
data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(starts_with("pan"):total, sum, na.rm = TRUE))

# Rename columns
data <- data %>%
  rename(PAN = pan, 
         PRI = pri, 
         PRD = prd, 
         PT = pt, 
         PVEM = pvem, 
         PC = mc, 
         PANAL = "na", 
         PRI_PVEM_PANAL = pripvemna, 
         PRI_PVEM = pripvem, 
         PRI_PANAL = prina)

# Create 'uniqueid' based on municipality names
data <- data %>%
  mutate(uniqueid = case_when(
    municipality == "AMEALCO DE BONFIL" ~ 22001,
    municipality == "ARROYO SECO" ~ 22003,
    municipality == "CADEREYTA DE MONTES" ~ 22004,
    municipality == "COLÓN" ~ 22005,
    municipality == "CORREGIDORA" ~ 22006,
    municipality == "EL MARQUÉS" ~ 22011,
    municipality == "EZEQUIEL MONTES" ~ 22007,
    municipality == "HUIMILPAN" ~ 22008,
    municipality == "JALPAN DE SERRA" ~ 22009,
    municipality == "LANDA DE MATAMOROS" ~ 22010,
    municipality == "PEDRO ESCOBEDO" ~ 22012,
    municipality == "PEÑAMILLER" ~ 22013,
    municipality == "PINAL DE AMOLES" ~ 22002,
    municipality == "QUERÉTARO" ~ 22014,
    municipality == "SAN JOAQUÍN" ~ 22015,
    municipality == "SAN JUAN DEL RÍO" ~ 22016,
    municipality == "TEQUISQUIAPAN" ~ 22017,
    municipality == "TOLIMÁN" ~ 22018,
    TRUE ~ 0  # Handle unmatched cases
  ))

# Create 'valid' as the row total across relevant columns
data <- data %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PC, PANAL, PVEM, PT, PRI_PVEM_PANAL, PRI_PVEM, PRI_PANAL)), na.rm = TRUE))

# 11) Lista Nominal

all_months <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") 

all_months <- all_months %>%
  filter(state == "QUERETARO" &
           month== "July" & 
           year==2012 ) %>% 
  select(section,lista)

data <- data %>%
  left_join(all_months, by=c("section"))

data <- data %>% 
  rename(listanominal = lista)

# Adjust columns after merge
data_2012 <- data %>%
  mutate(turnout = total / listanominal)

# Append all data sets together
all_data <- bind_rows(data1997, data2000, data2003, data2006, data2009, data2012)

# Load Excel file to get sheet names
file_path <- "../../../Data/Raw Electoral Data/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Municipios_2015.xlsx"
sheets <- excel_sheets(file_path)

# Loop through all sheets, load the data, and clean it
municipality_data_list <- list() # List to store data for each sheet
for (sheetname in sheets) {
  
  # Read the sheet
  data <- read_excel(file_path, sheet = sheetname, col_types = "text") 
  
  # Add municipality column
  data <- data %>%
    mutate(municipality = sheetname)
  
  # Replace empty values with "0" across all columns
  data[data == ""] <- "0"
  
  # Store cleaned data in the list
  municipality_data_list[[sheetname]] <- data
}

# Combine all sheets into one dataframe
all_data <- bind_rows(municipality_data_list)

# Clean up SECCIÓN column by dropping rows where it's empty
all_data <- all_data %>%
  filter(SECCIÓN != "")

# Create a new `uniqueid` column and populate it based on municipality values
data <- all_data %>%
  mutate(uniqueid = case_when(
    municipality == "AMEALCO DE BONFIL" ~ 22001,
    municipality == "ARROYO SECO" ~ 22003,
    municipality == "CADEREYTA DE MONTES" ~ 22004,
    municipality == "COLÓN" ~ 22005,
    municipality == "CORREGIDORA" ~ 22006,
    municipality == "EL MARQUÉS" ~ 22011,
    municipality == "EZEQUIEL MONTES" ~ 22007,
    municipality == "HUIMILPAN" ~ 22008,
    municipality == "JALPAN DE SERRA" ~ 22009,
    municipality == "LANDA DE MATAMOROS" ~ 22010,
    municipality == "PEDRO ESCOBEDO" ~ 22012,
    municipality == "PEÑAMILLER" ~ 22013,
    municipality == "PINAL DE AMOLES" ~ 22002,
    municipality == "QUERÉTARO" ~ 22014,
    municipality == "SAN JOAQUÍN" ~ 22015,
    municipality == "SAN JUAN DEL RÍO" ~ 22016,
    municipality == "TEQUISQUIAPAN" ~ 22017,
    municipality == "TOLIMÁN" ~ 22018,
    TRUE ~ NA_real_  # If municipality doesn't match, set to NA
  ))

# Rename `SECCIÓN` to `section`
data <- data %>%
  rename(section = SECCIÓN)

# Rename 'PANAL_PT' to 'PT_PANAL'
data <- data %>%
  rename(PT_PANAL = PANAL_PT)

## 3) Convert columns from character to numeric if possible ("destring")
data <- data %>%
  mutate(across(-municipality, ~ {
    if (is.character(.)) suppressWarnings(as.numeric(.)) else .
  }))

# Replace PRI_PVEM_PANAL based on the sum of relevant columns
data <- data %>%
  mutate(
    PRI_PVEM_PANAL = ifelse(!is.na(PRI_PVEM_PANAL), PRI_PVEM_PANAL + PRI + PANAL + PVEM + PVEM_PANAL + PRI_PANAL + PRI_PVEM, PRI_PVEM_PANAL),
    PRI = ifelse(!is.na(PRI_PVEM_PANAL), NA, PRI),
    PANAL = ifelse(!is.na(PRI_PVEM_PANAL), NA, PANAL),
    PVEM = ifelse(!is.na(PRI_PVEM_PANAL), NA, PVEM),
    PVEM_PANAL = ifelse(!is.na(PRI_PVEM_PANAL), NA, PVEM_PANAL),
    PRI_PANAL = ifelse(!is.na(PRI_PVEM_PANAL), NA, PRI_PANAL),
    PRI_PVEM = ifelse(!is.na(PRI_PVEM_PANAL), NA, PRI_PVEM)
  )

# Replace PRI_PT_PANAL based on the sum of relevant columns
data <- data %>%
  mutate(
    PRI_PT_PANAL = ifelse(!is.na(PRI_PT_PANAL), PRI_PT_PANAL + PRI + PANAL + PT + PT_PANAL + PRI_PANAL + PRI_PT, PRI_PT_PANAL),
    PRI = ifelse(!is.na(PRI_PT_PANAL), NA, PRI),
    PANAL = ifelse(!is.na(PRI_PT_PANAL), NA, PANAL),
    PT = ifelse(!is.na(PRI_PT_PANAL), NA, PT),
    PRI_PANAL = ifelse(!is.na(PRI_PT_PANAL), NA, PRI_PANAL),
    PT_PANAL = ifelse(!is.na(PRI_PT_PANAL), NA, PT_PANAL),
    PRI_PT = ifelse(!is.na(PRI_PT_PANAL), NA, PRI_PT)
  )

# Replace PVEM_PANAL if PRI_PVEM_PANAL is NA
data <- data %>%
  mutate(
    PVEM_PANAL = ifelse(is.na(PRI_PVEM_PANAL) & !is.na(PVEM_PANAL), PANAL + PVEM + PVEM_PANAL, PVEM_PANAL),
    PANAL = ifelse(is.na(PRI_PVEM_PANAL) & !is.na(PVEM_PANAL), NA, PANAL),
    PVEM = ifelse(is.na(PRI_PVEM_PANAL) & !is.na(PVEM_PANAL), NA, PVEM)
  )

# Replace PAN_PRD based on the sum of relevant columns
data <- data %>%
  mutate(
    PAN_PRD = ifelse(!is.na(PAN_PRD), PRD + PAN + PAN_PRD, PAN_PRD),
    PRD = ifelse(!is.na(PAN_PRD), NA, PRD),
    PAN = ifelse(!is.na(PAN_PRD), NA, PAN)
  )

# Replace PRI_PT if PRI_PT_PANAL is NA
data <- data %>%
  mutate(
    PRI_PT = ifelse(is.na(PRI_PT_PANAL) & !is.na(PRI_PT), PT + PRI + PRI_PT, PRI_PT),
    PT = ifelse(is.na(PRI_PT_PANAL) & !is.na(PRI_PT), NA, PT),
    PRI = ifelse(is.na(PRI_PT_PANAL) & !is.na(PRI_PT), NA, PRI)
  )

# Replace PRI_PVEM if PRI_PVEM_PANAL is NA
data <- data %>%
  mutate(
    PRI_PVEM = ifelse(is.na(PRI_PVEM_PANAL) & !is.na(PRI_PVEM), PVEM + PRI + PRI_PVEM, PRI_PVEM),
    PVEM = ifelse(is.na(PRI_PVEM_PANAL) & !is.na(PRI_PVEM), NA, PVEM),
    PRI = ifelse(is.na(PRI_PVEM_PANAL) & !is.na(PRI_PVEM), NA, PRI)
  )

# Drop the columns PRI_PANAL and PT_PANAL
data <- data %>%
  select(-PRI_PANAL, -PT_PANAL)

# Create a new 'total' column as the row sum of columns PAN to Nulos
data <- data %>%
  rowwise() %>%
  mutate(total = sum(c_across(PAN:Nulos), na.rm = TRUE))

# Collapse (sum) PAN to PRI_PVEM_PANAL_PT columns, grouping by municipality, section, and uniqueid
data <- data %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(PAN:PRI_PVEM_PANAL_PT, sum, na.rm = TRUE), total = sum(total, na.rm = TRUE))

# Generate a new 'valid' column as the row total for specific columns
data <- data %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRI_PVEM_PANAL_PT), na.rm = TRUE))

# Add year, month, and STATE columns
data_2015 <- data %>%
  mutate(
    year = 2015,
    month = "June",
    STATE = "QUERETARO"
  )

# Filter out rows where uniqueid equals 22008 (Huimilpan annulled election)
data_2015 <- data_2015 %>%
  filter(uniqueid != 22008)

# Load the Lista Nominal data for 2015
ln2015 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
  # Keep only rows where entidad == 22 (Queretaro) and month == 6 (June)
  filter(entidad == 22, month == 6, seccion != 0) %>%
  # Create the uniqueid and keep the necessary columns
  mutate(uniqueid = (entidad * 1000) + municipio) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section = seccion)


# Merge the election data with the lista nominal data by `section`
data_2015 <- data_2015 %>%
  left_join(ln2015, by = "section")

# Remove rows where the merge failed
data_2015 <- data_2015 %>%
  filter(!is.na(lista))

# Rename 'lista' column to 'listanominal'
data_2015 <- data_2015 %>%
  rename(listanominal = lista)

# Calculate municipal turnout and total turnout
data_2015 <- data_2015 %>%
  mutate(turnout = total / listanominal)

# Load the CSV file for Huimilpan extraordinary election
data <- read_csv("../../../Data/Raw Electoral Data/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/huimilpan.csv")
colnames(data) <- tolower(colnames(data))
# Remove "-" and spaces
names(data) <- gsub("[- ]", "", names(data))
# Rename 'seccion' to 'section'
data <- data %>%
  rename(section = seccion)

# Drop rows where 'pri' is missing
data <- data %>%
  filter(!is.na(pri))

# Convert all columns to numeric
data <- data %>%
  mutate(across(everything(), as.numeric))
# Create PRI_PVEM_PANAL and PAN_PRD coalitions
data <- data %>%
  mutate(
    PRI_PVEM_PANAL = pri + panal + pvem + pripanalpvem + pripanal + pripvem + panalpvem,
    PAN_PRD = pan + prd + panprd
  )

# Drop columns that are no longer needed
data <- data %>%
  select(-c(pri, panal, pvem, pripanalpvem, pripanal, pripvem, panalpvem, pan, prd, panprd))

# Rename columns
data <- data %>%
  rename(PES = pes, MORENA = morena, PT = pt)

# Add the unique ID and municipality name
data <- data %>%
  mutate(
    uniqueid = 22008,
    municipality = "HUIMILPAN EXTRAORDINARIO"
  )

# Create the 'total' column (sum of the vote columns)
data <- data %>%
  rowwise() %>%
  mutate(total = sum(c_across(PES:PRI_PVEM_PANAL), nr, nulos, na.rm = TRUE)) %>%
  ungroup()

# Collapse (aggregate) data by municipality, section, and uniqueid
data <- data %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(c(PES:total), sum, na.rm = TRUE))

# Create the 'valid' column as the sum of PES, MORENA, PT, PRI_PVEM_PANAL, and PAN_PRD
data <- data %>%
  mutate(valid = rowSums(across(c(PES, MORENA, PT, PRI_PVEM_PANAL, PAN_PRD)), na.rm = TRUE))

# Load the lista nominal data for November 2015 (Querétaro, Huimilpan only)
ln2015 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
  filter(entidad == 22, month == 11, year == 2015) %>%
  rename(section = seccion, listanominal = lista) %>%
  select(section, listanominal)

# Merge lista nominal data with the election data
data <- data %>%
  left_join(ln2015, by = "section")

# Drop rows where the merge failed
data <- data %>%
  filter(!is.na(listanominal))

# Calculate total turnout and municipal turnout
data <- data %>%
  mutate(
    turnout = total / listanominal)

# Add year, month, and state information
data_2015_extra <- data %>%
  mutate(
    year = 2015,
    month = "December",
    STATE = "QUERETARO"
  )

# Step 1: Load and Clean the Election Data
data <- read_excel("../../../Data/Raw Electoral Data/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Municipios_2018.xlsx", sheet = "Municipios")
# Remove "-" and spaces
names(data) <- gsub("-", "", names(data))
# Combine coalitions for MORENA, PT, PES
data <- data %>%
  mutate(
    MORENA_PT_PES = ifelse(!is.na(MORENA_PT_PES), MORENA_PT_PES + MORENA_PT + MORENA_PES + PT_PES + MORENA + PT + PES, MORENA_PT_PES),
    PT = ifelse(!is.na(MORENA_PT_PES), NA, PT),
    PES = ifelse(!is.na(MORENA_PT_PES), NA, PES),
    MORENA = ifelse(!is.na(MORENA_PT_PES), NA, MORENA)
  ) %>%
  select(-MORENA_PT, -MORENA_PES, -PT_PES)

# Combine coalitions for PAN, PRD, MC
data <- data %>%
  mutate(
    PAN_PRD_MC = ifelse(!is.na(PAN_PRD_MC), PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC, PAN_PRD_MC),
    PAN = ifelse(!is.na(PAN_PRD_MC), NA, PAN),
    PRD = ifelse(!is.na(PAN_PRD_MC), NA, PRD),
    MC = ifelse(!is.na(PAN_PRD_MC), NA, MC)
  ) 

# Handle PAN_PRD and PAN_MC
data <- data %>%
  mutate(
    PAN_PRD = ifelse(!is.na(CC2_PANPRD), CC2_PANPRD + PAN + PRD, PAN_PRD),
    PAN = ifelse(!is.na(CC2_PANPRD), NA, PAN),
    PRD = ifelse(!is.na(CC2_PANPRD), NA, PRD)
  ) %>%
  select(-CC2_PANPRD)

data <- data %>%
  mutate(
    PAN_MC = ifelse(!is.na(CC3_PANMC), CC3_PANMC + PAN + MC, PAN_MC),
    PAN = ifelse(!is.na(CC3_PANMC), NA, PAN),
    MC = ifelse(!is.na(CC3_PANMC), NA, MC)
  ) %>%
  select(-CC3_PANMC)

# Combine coalitions for PRI and PVEM
data <- data %>%
  mutate(
    PRI_PVEM = ifelse(!is.na(PRI_PVEM), PRI_PVEM + PRI + PVEM, PRI_PVEM),
    PRI = ifelse(!is.na(PRI_PVEM), NA, PRI),
    PVEM = ifelse(!is.na(PRI_PVEM), NA, PVEM)
  )

data <- data %>%
  mutate(
    PRI_PVEM = ifelse(!is.na(CC4_PRIPVEM), CC4_PRIPVEM + PRI + PVEM, PRI_PVEM),
    PRI = ifelse(!is.na(CC4_PRIPVEM), NA, PRI),
    PVEM = ifelse(!is.na(CC4_PRIPVEM), NA, PVEM)
  ) %>%
  select(-CC4_PRIPVEM)

# Step 2: Generate Independent Candidate Data
data <- data %>%
  rowwise() %>%
  mutate(
    CI_1 = sum(c_across(JAGRV:EFM), na.rm = TRUE),
    CI_2 = JNL + JANH + PRCL + ACM + AMG,
    CI_3 = JMMM + JLMS + AGAB,
    CI_4 = EFM
  ) %>%
  mutate(CI_1 = CI_1 - CI_2 - CI_3 - CI_4) %>%
  ungroup()

# Drop columns used for independent candidate calculation
data <- data %>%
  select(-JAGRV, -LGOD, -LBH, -OELO, -MGAG, -RMH, -EMB, -JANH, -JNL, -JMMM, -JAML, -PVM, -ACM, -DJD, -JBLL, -HMV, -RMS, -JLMS, -PRCL, -AMG, -RRT, -AGAB, -EFM)

# Step 3: Add uniqueid and municipality details
data <- data %>%
  mutate(uniqueid = 0) %>%
  mutate(
    uniqueid = case_when(
      municipality == "AMEALCO DE BONFIL" ~ 22001,
      municipality == "ARROYO SECO" ~ 22003,
      municipality == "CADEREYTA DE MONTES" ~ 22004,
      municipality == "COLÓN" ~ 22005,
      municipality == "CORREGIDORA" ~ 22006,
      municipality == "EL MARQUÉS" ~ 22011,
      municipality == "EZEQUIEL MONTES" ~ 22007,
      municipality == "HUIMILPAN" ~ 22008,
      municipality == "JALPAN DE SERRA" ~ 22009,
      municipality == "LANDA DE MATAMOROS" ~ 22010,
      municipality == "PEDRO ESCOBEDO" ~ 22012,
      municipality == "PEÑAMILLER" ~ 22013,
      municipality == "PINAL DE AMOLES" ~ 22002,
      municipality == "QUERÉTARO" ~ 22014,
      municipality == "SAN JOAQUÍN" ~ 22015,
      municipality == "SAN JUAN DEL RÍO" ~ 22016,
      municipality == "TEQUISQUIAPAN" ~ 22017,
      municipality == "TOLIMÁN" ~ 22018
    )
  )

# Step 4: Collapse data by municipality, section, and uniqueid
data <- data %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(c(PAN:CI_4, listanominal, total), sum, na.rm = TRUE)) %>%
  ungroup()

# Step 5: Calculate valid votes and municipality level sums
data <- data %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:CI_4), na.rm = TRUE)) %>%
  ungroup()

# Step 6: Calculate turnout
data <- data %>%
  mutate(turnout = total / listanominal)

# Step: Add additional columns for the year, month, and state
data_2018 <- data %>%
  mutate(year = 2018, month = "July", STATE = "QUERETARO") %>% 
  select(-c(NUM_BOLETAS_SOBRANTES,NUM_ESCRITOS,BOLETAS_OTRA_ELECCION,STATE))
#######################################################################

# Combine the datasets
combined_data <- bind_rows(data_2015, data_2015_extra, data_2018)


#######################################################################
# Step: Append 

# Append the newly combined data
data_master <- bind_rows(data_master, combined_data)

# Replace the municipality names to uppercase
data_master <- data_master %>%
  mutate(municipality = toupper(municipality))

data.table::fwrite(data_master,"../../../Processed Data/queretaro/queretaro_process_raw_data.csv")


