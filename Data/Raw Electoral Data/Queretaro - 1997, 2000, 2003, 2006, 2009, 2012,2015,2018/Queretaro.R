# Load necessary libraries
library(dplyr)
library(readr)

# Set working directory (Try the first, if it fails, try the second)
setwd("C:/Users/jmarshall/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012") || 
  setwd("C:/Users/Horacio/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012")

# Load CSV file
ayu_seccion_1997 <- read_csv("Ayu_Seccion_1997.csv")

# Rename columns
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  rename(
    municipality = municipio,
    section = seccin
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

# Print the result to check
print(ayu_seccion_1997)

# Calculate `valid` as the row total for PAN, PRI, PRD, PT, PVEM
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  mutate(valid = PAN + PRI + PRD + PT + PVEM)

# Sum for each variable by `uniqueid`
for (var in c("PAN", "PRI", "PRD", "PT", "PVEM", "total", "listanominal", "valid")) {
  ayu_seccion_1997 <- ayu_seccion_1997 %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    ungroup()
}

# Calculate inverse values for each `mun_` variable
for (var in c("PAN", "PRI", "PRD", "PT", "PVEM", "total", "listanominal", "valid")) {
  ayu_seccion_1997 <- ayu_seccion_1997 %>%
    mutate(!!paste0("inv_mun_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Calculate `mun_turnout` as the ratio of `mun_total` to `mun_listanominal`
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse `mun_` variables
rank_vars <- c("inv_mun_PAN", "inv_mun_PRI", "inv_mun_PRD", "inv_mun_PT", "inv_mun_PVEM")
rank_cols <- c("PAN_r", "PRI_r", "PRD_r", "PT_r", "PVEM_r")

for (i in seq_along(rank_vars)) {
  ayu_seccion_1997 <- ayu_seccion_1997 %>%
    mutate(!!rank_cols[i] := rank(!!sym(rank_vars[i]), ties.method = "first"))
}

# Drop the `inv_mun_` variables
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  select(-starts_with("inv_mun_"))

# Assign the `winner` based on ranks
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    TRUE ~ winner
  ))

# Drop the ranking variables
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  select(-c(PAN_r, PRI_r, PRD_r, PT_r, PVEM_r))

# Add the `year` and `month` columns
ayu_seccion_1997 <- ayu_seccion_1997 %>%
  mutate(year = 1997, month = "July")

# Save the file as a .RDS (R format equivalent to Stata's .dta format)
saveRDS(ayu_seccion_1997, file = "Queretaro_Section_1997.rds")

# Read the CSV file
ayu_seccion_2000 <- read_csv("Ayu_Seccion_2000.csv")

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
ayu_seccion_2000 <- ayu_seccion_2000 %>%
  mutate(turnout = total / listanominal)

# Optional: Save the data as an .RDS file (R equivalent to Stata's .dta)
saveRDS(ayu_seccion_2000, file = "Queretaro_Section_2000.rds")

# Read the CSV file (replace the file path with your actual path)
queretaro_2003 <- read.csv("Ayu_Seccion_2003.csv", stringsAsFactors = FALSE)

# Rename columns
queretaro_2003 <- queretaro_2003 %>%
  rename(
    municipality = municipio,
    section = seccin
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

# Save the processed data (replace with the desired output format)
saveRDS(queretaro_2003, "Queretaro_Section_2003.rds")

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
    TRUE ~ uniqueid  # If the municipality doesn't match, keep the original uniqueid
  ))

# Create 'valid' as the row sum of specified party columns
queretaro_2003 <- queretaro_2003 %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, PSN), na.rm = TRUE))

# Summarize the data at the municipal level by calculating the sum of each variable by uniqueid
queretaro_2003 <- queretaro_2003 %>%
  group_by(uniqueid) %>%
  summarise(
    across(c(PAN, PRI, PRD, PT, PVEM, PC, PSN, total, listanominal, valid), sum, na.rm = TRUE)
  ) %>%
  ungroup()

# Create inverse for each aggregated variable
queretaro_2003 <- queretaro_2003 %>%
  mutate(
    inv_mun_PAN = 1 / PAN,
    inv_mun_PRI = 1 / PRI,
    inv_mun_PRD = 1 / PRD,
    inv_mun_PT = 1 / PT,
    inv_mun_PVEM = 1 / PVEM,
    inv_mun_PC = 1 / PC,
    inv_mun_PSN = 1 / PSN
  )

# Calculate municipal turnout
queretaro_2003 <- queretaro_2003 %>%
  mutate(mun_turnout = total / listanominal)

# Rank the inverse votes for each party
queretaro_2003 <- queretaro_2003 %>%
  mutate(
    PAN_r = rank(inv_mun_PAN, ties.method = "min"),
    PRI_r = rank(inv_mun_PRI, ties.method = "min"),
    PRD_r = rank(inv_mun_PRD, ties.method = "min"),
    PT_r = rank(inv_mun_PT, ties.method = "min"),
    PVEM_r = rank(inv_mun_PVEM, ties.method = "min"),
    PC_r = rank(inv_mun_PC, ties.method = "min"),
    PSN_r = rank(inv_mun_PSN, ties.method = "min")
  )

# Determine the winner by checking the ranks
queretaro_2003 <- queretaro_2003 %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_r == 1 ~ "PRI",
      PRD_r == 1 ~ "PRD",
      PT_r == 1 ~ "PT",
      PVEM_r == 1 ~ "PVEM",
      PC_r == 1 ~ "PC",
      PSN_r == 1 ~ "PSN",
      TRUE ~ NA_character_
    )
  )

# Drop rank variables
queretaro_2003 <- queretaro_2003 %>%
  select(-contains("_r"))

# Add the year and month columns
queretaro_2003 <- queretaro_2003 %>%
  mutate(
    year = 2003,
    month = "July"
  )

# Save the processed data
saveRDS(queretaro_2003, "Queretaro_Section_2003.rds")

# Load the CSV file into R
ayuntamiento_2006 <- read_csv("Ayu_Seccion_2006.csv")

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

# Convert relevant columns to numeric (equivalent to Stata's 'destring')
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

# Save the processed data
saveRDS(ayuntamiento_2006_summary, "Queretaro_Section_2006.rds")

# Load necessary libraries
library(dplyr)
library(readr)

# Load the data (assuming you already loaded the collapsed data from previous steps)
queretaro_2006 <- readRDS("Queretaro_Section_2006.rds")

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
    TRUE ~ 0
  ))

# Calculate `valid` as the row total of relevant vote columns
queretaro_2006 <- queretaro_2006 %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PVEM, PC, PT, PANAL, PRI_PVEM), na.rm = TRUE))

# Collapse by `uniqueid`, calculate the sum of votes and generate inverses for each party
queretaro_2006 <- queretaro_2006 %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN, PRI, PRD, PVEM, PC, PT, PANAL, PRI_PVEM, total, listanominal, valid), sum, .names = "mun_{col}")) %>%
  mutate(across(starts_with("mun_"), ~ 1 / ., .names = "inv_{col}"))

# Calculate municipal-level turnout
queretaro_2006 <- queretaro_2006 %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Generate ranks based on the inverse vote totals
queretaro_2006 <- queretaro_2006 %>%
  mutate(across(starts_with("inv_mun_"), ~ rank(-.), .names = "{sub('inv_', '', col)}_r"))

# Determine the winner based on the ranks
queretaro_2006 <- queretaro_2006 %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PANAL_r == 1 ~ "PANAL",
    PRI_PVEM_r == 1 ~ "PRI_PVEM",
    TRUE ~ NA_character_
  ))

# Clean up by dropping the rank columns
queretaro_2006 <- queretaro_2006 %>%
  select(-ends_with("_r"))

# Add year and month columns
queretaro_2006 <- queretaro_2006 %>%
  mutate(year = 2006, month = "July")

# Sort by section
queretaro_2006 <- queretaro_2006 %>%
  arrange(section)

# Save the data to a file
saveRDS(queretaro_2006, "Queretaro_Section_2006.rds")

# Load the CSV data
ayuntamiento_2009 <- read_csv("Ayu_Seccion_2009.csv")

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
    PANAL = na
  )

# Generate turnout column
ayuntamiento_2009_collapsed <- ayuntamiento_2009_collapsed %>%
  mutate(turnout = total / listanominal)

# Save the collapsed data to an RDS file (or any desired format)
saveRDS(ayuntamiento_2009_collapsed, "Queretaro_Section_2009.rds")

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

# Calculate municipal sums for all variables
data <- data %>%
  group_by(uniqueid) %>%
  mutate(
    mun_PAN = sum(PAN, na.rm = TRUE),
    mun_PRI = sum(PRI, na.rm = TRUE),
    mun_PRD = sum(PRD, na.rm = TRUE),
    mun_PC = sum(PC, na.rm = TRUE),
    mun_PANAL = sum(PANAL, na.rm = TRUE),
    mun_PSD = sum(PSD, na.rm = TRUE),
    mun_PT = sum(PT, na.rm = TRUE),
    mun_PVEM = sum(PVEM, na.rm = TRUE),
    mun_total = sum(total, na.rm = TRUE),
    mun_listanominal = sum(listanominal, na.rm = TRUE),
    mun_valid = sum(valid, na.rm = TRUE)
  ) %>%
  ungroup()

# Inverse of the municipal sums
data <- data %>%
  mutate(
    inv_mun_PAN = 1 / mun_PAN,
    inv_mun_PRI = 1 / mun_PRI,
    inv_mun_PRD = 1 / mun_PRD,
    inv_mun_PC = 1 / mun_PC,
    inv_mun_PANAL = 1 / mun_PANAL,
    inv_mun_PSD = 1 / mun_PSD,
    inv_mun_PT = 1 / mun_PT,
    inv_mun_PVEM = 1 / mun_PVEM
  )

# Municipal turnout
data <- data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Row ranks for each party (1 means the highest value in the row)
# Use matrixStats::rowRanks to calculate the rank for each variable
party_columns <- c("inv_mun_PAN", "inv_mun_PRI", "inv_mun_PRD", "inv_mun_PC", "inv_mun_PANAL", "inv_mun_PSD", "inv_mun_PT", "inv_mun_PVEM")
rank_columns <- c("PAN_r", "PRI_r", "PRD_r", "PC_r", "PANAL_r", "PSD_r", "PT_r", "PVEM_r")

data[rank_columns] <- as.data.frame(t(rowRanks(as.matrix(data[party_columns]), ties.method = "min")))

# Determine the winner based on ranks (rank 1)
data <- data %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_r == 1 ~ "PRI",
      PRD_r == 1 ~ "PRD",
      PT_r == 1 ~ "PT",
      PVEM_r == 1 ~ "PVEM",
      PC_r == 1 ~ "PC",
      PANAL_r == 1 ~ "PANAL",
      PSD_r == 1 ~ "PSD",
      TRUE ~ NA_character_
    )
  )

# Additional variables
data <- data %>%
  mutate(
    year = 2009,
    month = "July"
  )

# Sort by section
data <- data %>%
  arrange(section)

# Save the dataset as .rds or .csv as needed
saveRDS(data, "Queretaro_Section_2009.rds")
# Alternatively, write as CSV
# write_csv(data, "Queretaro_Section_2009.csv")

# Load the dataset (assuming the file is in CSV format)
data <- read_excel("Ayu_Seccion_2012.xlsx")

# Drop rows where municipality or section is empty or total is missing/zero
data <- data %>%
  filter(municipality != "" & section != "" & !is.na(total) & total != 0)

# Collapse data by municipality and section
data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(starts_with("pan"):total, sum, na.rm = TRUE))

# Rename columns
data <- data %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = mc, PANAL = na, 
         PRI_PVEM_PANAL = pripvemna, PRI_PVEM = pripvem, PRI_PANAL = prina)

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

# Municipal sums (grouped by uniqueid) and inverses
data <- data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN, PRI, PRD, PC, PANAL, PVEM, PT, PRI_PVEM_PANAL, PRI_PVEM, PRI_PANAL, total, listanominal, valid), sum, .names = "mun_{col}")) %>%
  mutate(across(starts_with("mun_"), ~1/.x, .names = "inv_{col}"))

# Turnout and municipal turnout
data <- data %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / mun_listanominal)

# Ranking each party by their inverse municipal totals
data[grep("^inv_mun_", names(data))] <- lapply(data[grep("^inv_mun_", names(data))], rank, ties.method = "min")

# Assign the ranks to each party
data <- data %>%
  rename_with(~ gsub("inv_mun_", "", .), starts_with("inv_mun_")) %>%
  rename(PAN_r = PAN, PRI_r = PRI, PRD_r = PRD, PC_r = PC, PANAL_r = PANAL, PVEM_r = PVEM, PT_r = PT, 
         PRI_PVEM_PANAL_r = PRI_PVEM_PANAL, PRI_PVEM_r = PRI_PVEM, PRI_PANAL_r = PRI_PANAL)

# Determine the winner by assigning the party with rank 1
data <- data %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PANAL_r == 1 ~ "PANAL",
    PRI_PVEM_PANAL_r == 1 ~ "PRI_PVEM_PANAL",
    PRI_PVEM_r == 1 ~ "PRI_PVEM",
    PRI_PANAL_r == 1 ~ "PRI_PANAL",
    TRUE ~ NA_character_
  ))

# Merge external data (if available)
merge_data <- read_excel("all_months_years.dta")  # Replace with appropriate function for the file
data <- merge(data, merge_data %>% filter(month == 7, year == 2012, day == 1), by.x = c("ed", "seccion"), by.y = c("ed", "section"))

# Adjust columns after merge
data <- data %>%
  rename(listanominal = lista) %>%
  mutate(turnout = total / listanominal) %>%
  group_by(uniqueid) %>%
  mutate(mun_listanominal = sum(listanominal),
         mun_turnout = mun_total / mun_listanominal)

# Finalize with year, month, and save the output
data <- data %>%
  mutate(year = 2012, month = "July") %>%
  arrange(section)

# Save the final output
saveRDS(data, "Queretaro_Section_2012.rds")
# Optionally export to CSV
# write.csv(data, "Queretaro_Section_2012.csv")

# Load the data files
data1997 <- readRDS("Queretaro_Section_1997.rds")
data2000 <- readRDS("Queretaro_Section_2000.rds")
data2003 <- readRDS("Queretaro_Section_2003.rds")
data2006 <- readRDS("Queretaro_Section_2006.rds")
data2009 <- readRDS("Queretaro_Section_2009.rds")
data2012 <- readRDS("Queretaro_Section_2012.rds")

# Append all data sets together
all_data <- bind_rows(data1997, data2000, data2003, data2006, data2009, data2012)

# Remove the individual datasets after appending (if needed)
rm(data1997, data2000, data2003, data2006, data2009, data2012)

# Check the winner column
table(all_data$winner, useNA = "ifany")

# Check the years and municipalities where the winner is missing
all_data %>%
  filter(is.na(winner)) %>%
  count(year)

all_data %>%
  filter(is.na(winner)) %>%
  count(municipality)

# Generate a PAN_winner variable
all_data <- all_data %>%
  mutate(PAN_winner = ifelse(grepl("PAN", winner) & !grepl("PANAL", winner), 1, 0))

# Initialize winner_counter with PAN_winner
all_data <- all_data %>%
  mutate(winner_counter = PAN_winner)

# List of other party names to generate respective winner columns
party_list <- c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "PD", "PSD", "PAS", 
                "PSN", "PartidoMexicoPosible", "CDPPN", "PUP", "PEC")

# Loop through the parties, create their winner variables, and increment winner_counter
for (party in party_list) {
  winner_var <- paste0(party, "_winner")
  
  all_data <- all_data %>%
    mutate(!!winner_var := ifelse(grepl(party, winner), 1, 0),
           winner_counter = winner_counter + !!sym(winner_var))
}

# Check the number of rows with winner_counter equal to zero
zero_winners <- nrow(filter(all_data, winner_counter == 0))
print(paste("Number of rows with no winner:", zero_winners))

write.csv(all_data, "Queretaro_ALL.csv")

# Load Excel file to get sheet names
file_path <- "Municipios_2015.xlsx"
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

# Convert columns to numeric where appropriate
all_data <- all_data %>%
  mutate(across(everything(), ~as.numeric(.), .names = "num_{col}"))

# Save the final dataset
write.csv(all_data, "Querétaro_2015.csv", row.names = FALSE)

# Optional: save in RDS format
saveRDS(all_data, "Querétaro_2015.rds")

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

# Reorder columns so that 'PVEM_PANAL' to 'PRI_PVEM_PANAL_PT' come before 'PRI_PT'
# Adjust the column names as needed
# Assuming 'a(PRI_PT)' means you want 'PRI_PT' as the first column, reorder as follows:

data <- data %>%
  select(PVEM_PANAL:PRI_PVEM_PANAL_PT, PRI_PT, everything())  # Adjust as necessary

# Rename 'PANAL_PT' to 'PT_PANAL'
data <- data %>%
  rename(PT_PANAL = PANAL_PT)

# View the modified data frame (optional)
head(data)

# Save the cleaned data (optional)
write.csv(data, "cleaned_queretaro_data.csv", row.names = FALSE)

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

# View the updated data (optional)
head(data)

# Optionally save the updated dataset
write.csv(data, "updated_queretaro_data.csv", row.names = FALSE)

# Assuming your data is stored in a data frame called `data`

# Collapse (sum) PAN to PRI_PVEM_PANAL_PT columns, grouping by municipality, section, and uniqueid
data <- data %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(PAN:PRI_PVEM_PANAL_PT, sum, na.rm = TRUE), total = sum(total, na.rm = TRUE))

# Generate a new 'valid' column as the row total for specific columns
data <- data %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRI_PVEM_PANAL_PT), na.rm = TRUE))

# For each relevant variable, calculate 'mun_var' and 'i_var'
for (var in c("PAN", "PRI", "PRD", "MC", "PANAL", "PVEM", "PES", "MORENA", "PT", "PRI_PT", "PVEM_PANAL", "PH", 
              "CI_1", "PRI_PVEM_PANAL", "PRI_PVEM", "PRI_PT_PANAL", "CI_2", "PAN_PRD", "PRI_PVEM_PANAL_PT", "total", "valid")) {
  
  # Calculate 'mun_var' by summing within the uniqueid group
  data <- data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(get(var), na.rm = TRUE))
  
  # Calculate 'i_var' as 1 / 'mun_var'
  data <- data %>%
    mutate(!!paste0("i_", var) := 1 / get(paste0("mun_", var)))
}

# Rank the 'i_var' columns for each section and generate the corresponding rank columns
rank_vars <- c("PAN", "PRI", "PRD", "MC", "PANAL", "PVEM", "PES", "MORENA", "PT", "PRI_PT", "PVEM_PANAL", 
               "PH", "CI_1", "PRI_PVEM_PANAL", "PRI_PVEM", "PRI_PT_PANAL", "CI_2", "PAN_PRD", "PRI_PVEM_PANAL_PT")

for (var in rank_vars) {
  data <- data %>%
    mutate(!!paste0(var, "_r") := rank(get(paste0("i_", var)), ties.method = "first", na.last = "keep"))
}

# Drop all 'i_*' columns
data <- data %>%
  select(-starts_with("i_"))

# Initialize winner, second, and third columns
data <- data %>%
  mutate(winner = "", second = "", third = "")

# Loop through variables and update 'winner', 'second', and 'third'
for (var in rank_vars) {
  data <- data %>%
    mutate(
      winner = ifelse(get(paste0(var, "_r")) == 1, var, winner),
      second = ifelse(get(paste0(var, "_r")) == 2, var, second),
      third = ifelse(get(paste0(var, "_r")) == 3, var, third)
    )
}

# Replace CI_1 and CI_2 with 'Independent' for winner, second, and third
data <- data %>%
  mutate(
    winner = ifelse(winner %in% c("CI_1", "CI_2"), "Independent", winner),
    second = ifelse(second %in% c("CI_1", "CI_2"), "Independent", second),
    third = ifelse(third %in% c("CI_1", "CI_2"), "Independent", third)
  )

# Add year, month, and STATE columns
data <- data %>%
  mutate(
    year = 2015,
    month = "June",
    STATE = "QUERETARO"
  )

# View the updated data (optional)
head(data)

# Save the updated dataset
write.csv(data, "Queretaro_Section_2015.csv", row.names = FALSE)

# Load the election data for 2015
data <- read_dta("Queretaro_Section_2015.dta")

# Filter out rows where uniqueid equals 22008 (Huimilpan annulled election)
data <- data %>%
  filter(uniqueid != 22008)

# Load the Lista Nominal data for 2015
ln2015 <- read_dta("../Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
  # Keep only rows where entidad == 22 (Queretaro) and month == 6 (June)
  filter(entidad == 22, month == 6, seccion != 0) %>%
  # Create the uniqueid and keep the necessary columns
  mutate(uniqueid = (entidad * 1000) + municipio) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section = seccion)

# Save processed Lista Nominal data
write_dta(ln2015, "LN15_QUER.dta")

# Merge the election data with the lista nominal data by `section`
data <- data %>%
  left_join(ln2015, by = "section")

# Remove rows where the merge failed
data <- data %>%
  filter(!is.na(lista))

# Rename 'lista' column to 'listanominal'
data <- data %>%
  rename(listanominal = lista)

# Calculate municipal-level lista nominal (sum of listanominal by uniqueid)
data <- data %>%
  group_by(uniqueid) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE))

# Calculate municipal turnout and total turnout
data <- data %>%
  mutate(mun_turnout = mun_total / mun_listanominal,
         turnout = total / listanominal)

# Save the processed election data
write_dta(data, "Queretaro_Section_2015.dta")

# Collapse the data to get the first occurrence of the 'winner' by municipality and uniqueid
incumbents <- data %>%
  group_by(municipality, uniqueid) %>%
  summarise(winner = first(winner))

# Save the incumbents dataset
write_dta(incumbents, "incumbents2018.dta")

# Collapse data at the municipality level to get sum of various columns and first occurrences
municipal_data <- data %>%
  group_by(municipality, uniqueid) %>%
  summarise(across(c(PAN:total, listanominal), sum, na.rm = TRUE),
            turnout = first(mun_turnout),
            winner = first(winner),
            second = first(second),
            third = first(third),
            STATE = first(STATE),
            year = first(year),
            month = first(month))

# Save the municipal-level data
write_dta(municipal_data, "../../Update Municipal/Queretaro_2015.dta")
# Remove the intermediate files used in the process
file.remove("AMEALCO DE BONFIL.dta", "ARROYO SECO.dta", "CADEREYTA DE MONTES.dta",
            "COLÓN.dta", "CORREGIDORA.dta", "EL MARQUÉS.dta", "EZEQUIEL MONTES.dta",
            "HUIMILPAN.dta", "JALPAN DE SERRA.dta", "LANDA DE MATAMOROS.dta",
            "PEDRO ESCOBEDO.dta", "PEÑAMILLER.dta", "PINAL DE AMOLES.dta",
            "QUERÉTARO.dta", "SAN JOAQUÍN.dta", "SAN JUAN DEL RÍO.dta",
            "TEQUISQUIAPAN.dta", "TOLIMÁN.dta")

# Load the CSV file for Huimilpan extraordinary election
data <- read_csv("D:/Dropbox/Salvador Ascencio/Update Precincts/Queretaro/huimilpan.csv")

# Rename 'seccion' to 'section'
data <- data %>%
  rename(section = seccion)

# Drop rows where 'pri' is missing
data <- data %>%
  filter(!is.na(pri))

# Convert all columns to numeric (equivalent to destring in Stata)
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
ln2015 <- read_dta("../Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
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
    turnout = total / listanominal,
    mun_listanominal = sum(listanominal, na.rm = TRUE),
    mun_total = sum(total, na.rm = TRUE),
    mun_turnout = mun_total / mun_listanominal
  )

# Rank the parties and calculate the winner, second, and third
data <- data %>%
  rowwise() %>%
  mutate(
    PES_r = rank(-PES, ties.method = "min"),
    MORENA_r = rank(-MORENA, ties.method = "min"),
    PT_r = rank(-PT, ties.method = "min"),
    PRI_PVEM_PANAL_r = rank(-PRI_PVEM_PANAL, ties.method = "min"),
    PAN_PRD_r = rank(-PAN_PRD, ties.method = "min")
  ) %>%
  mutate(
    winner = case_when(
      PES_r == 1 ~ "PES",
      MORENA_r == 1 ~ "MORENA",
      PT_r == 1 ~ "PT",
      PRI_PVEM_PANAL_r == 1 ~ "PRI_PVEM_PANAL",
      PAN_PRD_r == 1 ~ "PAN_PRD"
    ),
    second = case_when(
      PES_r == 2 ~ "PES",
      MORENA_r == 2 ~ "MORENA",
      PT_r == 2 ~ "PT",
      PRI_PVEM_PANAL_r == 2 ~ "PRI_PVEM_PANAL",
      PAN_PRD_r == 2 ~ "PAN_PRD"
    ),
    third = case_when(
      PES_r == 3 ~ "PES",
      MORENA_r == 3 ~ "MORENA",
      PT_r == 3 ~ "PT",
      PRI_PVEM_PANAL_r == 3 ~ "PRI_PVEM_PANAL",
      PAN_PRD_r == 3 ~ "PAN_PRD"
    )
  ) %>%
  ungroup()

# Add year, month, and state information
data <- data %>%
  mutate(
    year = 2015,
    month = "December",
    STATE = "QUERETARO"
  )

# Save the final dataset as a .dta file
write_dta(data, "Queretaro_Section_2015_EXTRAORDINARIO.dta")

# Step 1: Load and Clean the Election Data
data <- read_excel("Municipios_2018.xlsx", sheet = "Municipios")

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
  ) %>%
  select(-PRD_MC, -PAN_PRD, -PAN_MC)

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

# Municipality level sums
data <- data %>%
  group_by(uniqueid) %>%
  mutate(
    mun_total = sum(total, na.rm = TRUE),
    mun_listanominal = sum(listanominal, na.rm = TRUE),
    mun_turnout = mun_total / mun_listanominal
  ) %>%
  ungroup()

# Step 7: Rank parties and determine winners
data <- data %>%
  rowwise() %>%
  mutate(
    PAN_r = rank(-PAN, ties.method = "min"),
    PRI_r = rank(-PRI, ties.method = "min"),
    PRD_r = rank(-PRD, ties.method = "min"),
    PVEM_r = rank(-PVEM, ties.method = "min"),
    PT_r = rank(-PT, ties.method = "min"),
    MC_r = rank(-MC, ties.method = "min"),
    PANAL_r = rank(-PANAL, ties.method = "min"),
    MORENA_r = rank(-MORENA, ties.method = "min"),
    PES_r = rank(-PES, ties.method = "min"),
    CQ_r = rank(-CQ, ties.method = "min"),
    QI_r = rank(-QI, ties.method = "min"),
    MORENA_PT_PES_r = rank(-MORENA_PT_PES, ties.method = "min"),
    PRI_PVEM_r = rank(-PRI_PVEM, ties.method = "min"),
    PAN_PRD_MC_r = rank(-PAN_PRD_MC, ties.method = "min"),
    PAN_PRD_r = rank(-PAN_PRD, ties.method = "min"),
    PAN_MC_r = rank(-PAN_MC, ties.method = "min"),
    CI_1_r = rank(-CI_1, ties.method = "min"),
    CI_2_r = rank(-CI_2, ties.method = "min"),
    CI_3_r = rank(-CI_3, ties.method = "min"),
    CI_4_r = rank(-CI_4, ties.method = "min")
  ) %>%
  ungroup()

# Step 8: Identify the winner, second, and third place
data <- data %>%
  rowwise() %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_r == 1 ~ "PRI",
      PRD_r == 1 ~ "PRD",
      PVEM_r == 1 ~ "PVEM",
      PT_r == 1 ~ "PT",
      MC_r == 1 ~ "MC",
      PANAL_r == 1 ~ "PANAL",
      MORENA_r == 1 ~ "MORENA",
      PES_r == 1 ~ "PES",
      CQ_r == 1 ~ "CQ",
      QI_r == 1 ~ "QI",
      MORENA_PT_PES_r == 1 ~ "MORENA_PT_PES",
      PRI_PVEM_r == 1 ~ "PRI_PVEM",
      PAN_PRD_MC_r == 1 ~ "PAN_PRD_MC",
      PAN_PRD_r == 1 ~ "PAN_PRD",
      PAN_MC_r == 1 ~ "PAN_MC",
      CI_1_r == 1 ~ "Independent",
      CI_2_r == 1 ~ "Independent",
      CI_3_r == 1 ~ "Independent",
      CI_4_r == 1 ~ "Independent"
    ),
    second = case_when(
      PAN_r == 2 ~ "PAN",
      PRI_r == 2 ~ "PRI",
      PRD_r == 2 ~ "PRD",
      PVEM_r == 2 ~ "PVEM",
      PT_r == 2 ~ "PT",
      MC_r == 2 ~ "MC",
      PANAL_r == 2 ~ "PANAL",
      MORENA_r == 2 ~ "MORENA",
      PES_r == 2 ~ "PES",
      CQ_r == 2 ~ "CQ",
      QI_r == 2 ~ "QI",
      MORENA_PT_PES_r == 2 ~ "MORENA_PT_PES",
      PRI_PVEM_r == 2 ~ "PRI_PVEM",
      PAN_PRD_MC_r == 2 ~ "PAN_PRD_MC",
      PAN_PRD_r == 2 ~ "PAN_PRD",
      PAN_MC_r == 2 ~ "PAN_MC",
      CI_1_r == 2 ~ "Independent",
      CI_2_r == 2 ~ "Independent",
      CI_3_r == 2 ~ "Independent",
      CI_4_r == 2 ~ "Independent"
    ),
    third = case_when(
      PAN_r == 3 ~ "PAN",
      PRI_r == 3 ~ "PRI",
      PRD_r == 3 ~ "PRD",
      PVEM_r == 3 ~ "PVEM",
      PT_r == 3 ~ "PT",
      MC_r == 3 ~ "MC",
      PANAL_r == 3 ~ "PANAL",
      MORENA_r == 3 ~ "MORENA",
      PES_r == 3 ~ "PES",
      CQ_r == 3 ~ "CQ",
      QI_r == 3 ~ "QI",
      MORENA_PT_PES_r == 3 ~ "MORENA_PT_PES",
      PRI_PVEM_r == 3 ~ "PRI_PVEM",
      PAN_PRD_MC_r == 3 ~ "PAN_PRD_MC",
      PAN_PRD_r == 3 ~ "PAN_PRD",
      PAN_MC_r == 3 ~ "PAN_MC",
      CI_1_r == 3 ~ "Independent",
      CI_2_r == 3 ~ "Independent",
      CI_3_r == 3 ~ "Independent",
      CI_4_r == 3 ~ "Independent"
    )
  ) %>%
  ungroup()

# Step 9: Add additional columns for the year, month, and state
data <- data %>%
  mutate(year = 2018, month = "July", STATE = "QUERETARO")

# Step 10: Merge with incumbents data
incumbents <- read_dta("incumbents2018.dta")
data <- data %>%
  left_join(incumbents, by = "uniqueid")

# Drop the incumbents file
unlink("incumbents2018.dta")

# Replace Huimilpan's incumbent information
data <- data %>%
  mutate(incumbent = ifelse(municipality == "HUIMILPAN", "PRI_PVEM_PANAL", incumbent))

# Step 11: Calculate incumbent vote shares
data <- data %>%
  rowwise() %>%
  mutate(
    incumbent_vote = case_when(
      incumbent == "PAN" ~ max(c_across(c(PAN_PRD_MC, PAN_PRD, PAN_MC, PAN)), na.rm = TRUE),
      incumbent == "PANAL" ~ PANAL,
      grepl("PRI", incumbent) ~ max(c_across(c(PRI, PRI_PVEM)), na.rm = TRUE),
      uniqueid == 22017 ~ PVEM,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Save the final dataset as .dta
write_dta(data, "Queretaro_Section_2018.dta")

# Step 1: Collapse data for Queretaro 2018 election and save the summary data
data_2018 <- read_dta("Queretaro_Section_2018.dta")

# Collapse data: (sum) listanominal-valid-incumbent_vote, (first) STATE-year-month-winner-second-third-mun_turnout-incumbent
collapsed_2018 <- data_2018 %>%
  group_by(municipality, uniqueid) %>%
  summarise(
    listanominal = sum(listanominal, na.rm = TRUE),
    valid = sum(valid, na.rm = TRUE),
    incumbent_vote = sum(incumbent_vote, na.rm = TRUE),
    STATE = first(STATE),
    year = first(year),
    month = first(month),
    winner = first(winner),
    second = first(second),
    third = first(third),
    mun_turnout = first(mun_turnout),
    incumbent = first(incumbent)
  )

# Rename the `mun_turnout` column to `turnout`
collapsed_2018 <- collapsed_2018 %>%
  rename(turnout = mun_turnout)

# Sort by uniqueid and re-order columns
collapsed_2018 <- collapsed_2018 %>%
  arrange(uniqueid) %>%
  select(STATE, municipality, uniqueid, everything())

# Save the data
write_dta(collapsed_2018, "../../Update Municipal/Queretaro_2018.dta")

#######################################################################
# Step 2: Append the 2015, 2015 Extraordinary, and 2018 datasets

# Load and append datasets
data_2015 <- read_dta("Queretaro_Section_2015.dta")
data_2015_extra <- read_dta("Queretaro_Section_2015_EXTRAORDINARIO.dta")
data_2018 <- read_dta("Queretaro_Section_2018.dta")

# Combine the datasets
combined_data <- bind_rows(data_2015, data_2015_extra, data_2018)

# Erase intermediate files
file.remove("Queretaro_Section_2015.dta")
file.remove("Queretaro_Section_2015_EXTRAORDINARIO.dta")
file.remove("Queretaro_Section_2018.dta")

#######################################################################
# Step 3: Generate Winner Counters

# Create a PAN winner flag
combined_data <- combined_data %>%
  mutate(
    PAN_winner = ifelse(grepl("PAN", winner) & !grepl("PANAL", winner), 1, 0),
    winner_counter = PAN_winner
  )

# Iterate through other parties to update winner_counter
parties <- c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "MORENA", "MC")
for (party in parties) {
  combined_data <- combined_data %>%
    mutate(!!paste0(party, "_winner") := ifelse(grepl(party, winner), 1, 0),
           winner_counter = winner_counter + get(paste0(party, "_winner")))
}

# View the winner_counter tabulation
table(combined_data$winner_counter)

# Count rows with no winner assigned
no_winner_count <- combined_data %>%
  filter(winner_counter == 0) %>%
  nrow()

# Print the count of rows with no winner assigned
print(no_winner_count)

# Save the combined dataset
write_dta(combined_data, "Queretaro_Section_15_18.dta")

#######################################################################
# Step 4: Append to "Queretaro_ALL.dta"

# Load the master dataset
data_master <- read_dta("../../Precinct/Queretaro_ALL.dta")

# Append the newly combined data
data_master <- bind_rows(data_master, combined_data)

# Remove the intermediate file
file.remove("Queretaro_Section_15_18.dta")

# Replace the municipality names to uppercase
data_master <- data_master %>%
  mutate(municipality = toupper(municipality))

# Save the updated master dataset
write_dta(data_master, "Queretaro_ALL_SALVADOR.dta")

