# Load necessary packages
library(dplyr)
library(readr)

# Read CSV file
df <- read_csv("Ayu_Seccion_1996.csv")

# Rename columns
df <- df %>%
  rename(
    municipality = nombre_municipio,
    section = seccion,
    listanominal = lista_nominal
  )

# Drop missing municipality and section rows
df <- df %>%
  filter(!(is.na(municipality) & is.na(section)))

# Convert columns to numeric (destring equivalent)
df <- df %>%
  mutate(across(listanominal:total, as.numeric))

# Drop rows where 'total' is missing or zero
df <- df %>%
  filter(!is.na(total) & total != 0)

# Collapse (sum) data by municipality and section
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan:total), sum, na.rm = TRUE))

# Rename columns
df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PartCardenista = pc,
    PT = pt,
    PVEM = pvem
  )

# Generate 'turnout' column
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# Drop 'nulos' column (if it exists)
df_collapsed <- df_collapsed %>% select(-nulos)

# Add uniqueid based on municipality
uniqueid_map <- c(
  "ACATLAN" = 13001, "ACAXOCHITLAN" = 13002, "ACTOPAN" = 13003, 
  "AGUA BLANCA DE ITURBIDE" = 13004, "AJACUBA" = 13005, "ALFAJAYUCAN" = 13006, 
  "ALMOLOYA" = 13007, "APAN" = 13008, "ATITALAQUIA" = 13010,
  # Add all mappings here, e.g. "MUNICIPALITY_NAME" = UNIQUE_ID
  # For brevity, only a few examples shown here
)

df_collapsed <- df_collapsed %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Generate 'valid' column (sum of votes across parties)
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PVEM), na.rm = TRUE))

# Summing at municipality level
df_collapsed <- df_collapsed %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN:valid, listanominal), sum, .names = "mun_{.col}"))

# Inverse of municipality totals
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("mun_"), ~1/.x, .names = "inv_{.col}"))

# Generate 'mun_turnout' column
df_collapsed <- df_collapsed %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse municipality totals and generate rank columns
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{gsub('inv_', '', .col)}_r"))

# Drop inverse columns
df_collapsed <- df_collapsed %>%
  select(-starts_with("inv_mun_"))

# Determine the top three ranks (winner, second, third)
df_collapsed <- df_collapsed %>%
  mutate(winner = case_when(PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PartCardenista_r == 1 ~ "PartCardenista", PRD_r == 1 ~ "PRD", PT_r == 1 ~ "PT", PVEM_r == 1 ~ "PVEM"),
         second = case_when(PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", PartCardenista_r == 2 ~ "PartCardenista", PRD_r == 2 ~ "PRD", PT_r == 2 ~ "PT", PVEM_r == 2 ~ "PVEM"),
         third = case_when(PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", PartCardenista_r == 3 ~ "PartCardenista", PRD_r == 3 ~ "PRD", PT_r == 3 ~ "PT", PVEM_r == 3 ~ "PVEM"))

# Drop rank columns
df_collapsed <- df_collapsed %>%
  select(-ends_with("_r"))

# Add year and month columns
df_collapsed <- df_collapsed %>%
  mutate(year = 1996, month = "November")

# Save the resulting data
write.csv(df_collapsed, "Hidalgo_Section_1996.csv", row.names = FALSE)

# Read the CSV file
df <- read_csv("Ayu_Seccion_1999.csv")

# Rename columns
df <- df %>%
  rename(
    municipality = nombre_municipio,
    section = seccion,
    listanominal = lista_nominal
  )

# Drop missing municipality and section rows
df <- df %>%
  filter(!(is.na(municipality) & is.na(section)))

# Convert specified columns to numeric
df <- df %>%
  mutate(across(listanominal:total, as.numeric))

# Drop rows where 'total' is missing or zero
df <- df %>%
  filter(!is.na(total) & total != 0)

# Collapse (sum) data by municipality and section
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan:total), sum, na.rm = TRUE))

# Rename columns
df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem
  )

# Generate 'turnout' column
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# Drop 'nulos' column if it exists (you may adapt this depending on the dataset)
df_collapsed <- df_collapsed %>% select(-nulos, everything())

# Add uniqueid based on municipality
uniqueid_map <- c(
  "ACATLAN" = 13001, "ACAXOCHITLAN" = 13002, "ACTOPAN" = 13003, 
  "AGUA BLANCA DE ITURBIDE" = 13004, "AJACUBA" = 13005, "ALFAJAYUCAN" = 13006, 
  "ALMOLOYA" = 13007, "APAN" = 13008, "ATITALAQUIA" = 13010,
  # Add all mappings here, continue for other municipalities...
)

df_collapsed <- df_collapsed %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Generate 'valid' column (sum of votes across parties)
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PVEM), na.rm = TRUE))

# Summing at municipality level
df_collapsed <- df_collapsed %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN:valid, listanominal), sum, .names = "mun_{.col}"))

# Inverse of municipality totals
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("mun_"), ~1/.x, .names = "inv_{.col}"))

# Generate 'mun_turnout' column
df_collapsed <- df_collapsed %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse municipality totals and generate rank columns
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{gsub('inv_', '', .col)}_r"))

# Drop inverse columns
df_collapsed <- df_collapsed %>%
  select(-starts_with("inv_mun_"))

# Determine the top three ranks (winner, second, third)
df_collapsed <- df_collapsed %>%
  mutate(winner = case_when(PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", PT_r == 1 ~ "PT", PVEM_r == 1 ~ "PVEM"),
         second = case_when(PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", PRD_r == 2 ~ "PRD", PT_r == 2 ~ "PT", PVEM_r == 2 ~ "PVEM"),
         third = case_when(PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", PRD_r == 3 ~ "PRD", PT_r == 3 ~ "PT", PVEM_r == 3 ~ "PVEM"))

# Drop rank columns
df_collapsed <- df_collapsed %>%
  select(-ends_with("_r"))

# Add year and month columns
df_collapsed <- df_collapsed %>%
  mutate(year = 1999, month = "November")

# Save the resulting data
write.csv(df_collapsed, "Hidalgo_Section_1999.csv", row.names = FALSE)

# Step 1: Read the CSV file for the 2002 data
df <- read_csv("Ayu_Seccion_2002.csv")

# Step 2: Rename columns to match desired format
df <- df %>%
  rename(
    municipality = nombre_municipio,  # Rename 'nombre_municipio' to 'municipality'
    section = seccion,                # Rename 'seccion' to 'section'
    listanominal = lista_nominal      # Rename 'lista_nominal' to 'listanominal'
  )

# Step 3: Drop rows where both municipality and section are missing
df <- df %>%
  filter(!(is.na(municipality) & is.na(section)))

# Step 4: Drop rows where 'total' is missing or zero
df <- df %>%
  filter(!is.na(total) & total != 0)

# Step 5: Convert specified columns to numeric (like 'destring' in Stata)
df <- df %>%
  mutate(across(listanominal:pripvem, as.numeric))

# Step 6: Collapse (sum) data by municipality and section
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan:pripvem, total), sum, na.rm = TRUE))

# Step 7: Rename party columns to uppercase to match the convention
df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem,
    PSN = psn,
    PC = pc,
    PAS = pas,
    PRI_PVEM = pripvem
  )

# Step 8: Calculate 'turnout' as the total votes divided by 'listanominal'
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# Step 9: Add uniqueid based on municipality (mapping from Stata code)
uniqueid_map <- c(
  "ACATLAN" = 13001, "ACAXOCHITLAN" = 13002, "ACTOPAN" = 13003, 
  "AGUA BLANCA DE ITURBIDE" = 13004, "AJACUBA" = 13005, "ALFAJAYUCAN" = 13006, 
  "ALMOLOYA" = 13007, "APAN" = 13008, "ATITALAQUIA" = 13010,
  # Add all mappings here as per the Stata code
)

df_collapsed <- df_collapsed %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Step 10: Generate 'valid' column (sum of votes across parties)
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRI_PVEM), na.rm = TRUE))

# Step 11: Summing at the municipality level
df_collapsed <- df_collapsed %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN:valid, listanominal), sum, .names = "mun_{.col}"))

# Step 12: Calculate the inverse of municipality-level totals
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("mun_"), ~1/.x, .names = "inv_{.col}"))

# Step 13: Generate 'mun_turnout' as the ratio of total votes to listanominal at the municipality level
df_collapsed <- df_collapsed %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 14: Rank the inverse municipality totals and create rank columns
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{gsub('inv_', '', .col)}_r"))

# Step 15: Drop inverse municipality columns
df_collapsed <- df_collapsed %>%
  select(-starts_with("inv_mun_"))

# Step 16: Determine the top three ranks (winner, second, third) based on the rank columns
df_collapsed <- df_collapsed %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", PT_r == 1 ~ "PT", PVEM_r == 1 ~ "PVEM", 
    PSN_r == 1 ~ "PSN", PC_r == 1 ~ "PC", PAS_r == 1 ~ "PAS", PRI_PVEM_r == 1 ~ "PRI_PVEM"),
    second = case_when(
      PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", PRD_r == 2 ~ "PRD", PT_r == 2 ~ "PT", PVEM_r == 2 ~ "PVEM", 
      PSN_r == 2 ~ "PSN", PC_r == 2 ~ "PC", PAS_r == 2 ~ "PAS", PRI_PVEM_r == 2 ~ "PRI_PVEM"),
    third = case_when(
      PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", PRD_r == 3 ~ "PRD", PT_r == 3 ~ "PT", PVEM_r == 3 ~ "PVEM", 
      PSN_r == 3 ~ "PSN", PC_r == 3 ~ "PC", PAS_r == 3 ~ "PAS", PRI_PVEM_r == 3 ~ "PRI_PVEM"))

# Step 17: Drop the ranking columns (those ending with '_r')
df_collapsed <- df_collapsed %>%
  select(-ends_with("_r"))

# Step 18: Add year and month columns
df_collapsed <- df_collapsed %>%
  mutate(year = 2002, month = "November")

# Step 19: Sort the data by 'section'
df_collapsed <- df_collapsed %>%
  arrange(section)

# Step 20: Save the resulting data to a CSV file
write.csv(df_collapsed, "Hidalgo_Section_2002.csv", row.names = FALSE)

# Load necessary libraries
library(dplyr)
library(readr)

# Step 1: Read the CSV file for the 2005 data
df <- read_csv("Ayu_Seccion_2005.csv")

# Step 2: Rename columns to match the desired format
df <- df %>%
  rename(
    municipality = nombre,    # Rename 'nombre' to 'municipality'
    section = casilla,        # Rename 'casilla' to 'section'
    listanominal = listadonominal,  # Rename 'listadonominal' to 'listanominal'
    total = totales           # Rename 'totales' to 'total'
  )

# Step 3: Drop rows where both municipality and section are missing
df <- df %>%
  filter(!(is.na(municipality) & is.na(section)))

# Step 4: Drop rows where 'total' is missing or zero
df <- df %>%
  filter(!is.na(total) & total != 0)

# Step 5: Convert specified columns to numeric (like 'destring' in Stata)
df <- df %>%
  mutate(across(listanominal:total, as.numeric))

# Step 6: Collapse (sum) data by municipality and section
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan:alternativa, total), sum, na.rm = TRUE))

# Step 7: Rename party columns to uppercase to match the convention
df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem,
    PC = pc,
    PAS = alternativa  # Rename 'alternativa' to 'PAS'
  )

# Step 8: Calculate 'turnout' as the total votes divided by 'listanominal'
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# Step 9: Add uniqueid based on municipality (mapping from Stata code)
uniqueid_map <- c(
  "ACATLÁN" = 13001, "ACAXOCHITLÁN" = 13002, "ACTOPAN" = 13003, 
  "AGUA BLANCA DE ITURBIDE" = 13004, "AJACUBA" = 13005, "ALFAJAYUCAN" = 13006, 
  "ALMOLOYA" = 13007, "APAN" = 13008, "ATITALAQUIA" = 13010,
  # Add all mappings here as per the Stata code
)

df_collapsed <- df_collapsed %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Step 10: Generate 'valid' column (sum of votes across parties)
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PAS), na.rm = TRUE))

# Step 11: Summing at the municipality level
df_collapsed <- df_collapsed %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN:valid, listanominal), sum, .names = "mun_{.col}"))

# Step 12: Calculate the inverse of municipality-level totals
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("mun_"), ~1/.x, .names = "inv_{.col}"))

# Step 13: Generate 'mun_turnout' as the ratio of total votes to listanominal at the municipality level
df_collapsed <- df_collapsed %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 14: Rank the inverse municipality totals and create rank columns
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{gsub('inv_', '', .col)}_r"))

# Step 15: Drop inverse municipality columns
df_collapsed <- df_collapsed %>%
  select(-starts_with("inv_mun_"))

# Step 16: Determine the top three ranks (winner, second, third) based on the rank columns
df_collapsed <- df_collapsed %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", PT_r == 1 ~ "PT", PVEM_r == 1 ~ "PVEM", 
    PC_r == 1 ~ "PC", PAS_r == 1 ~ "PAS"),
    second = case_when(
      PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", PRD_r == 2 ~ "PRD", PT_r == 2 ~ "PT", PVEM_r == 2 ~ "PVEM", 
      PC_r == 2 ~ "PC", PAS_r == 2 ~ "PAS"),
    third = case_when(
      PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", PRD_r == 3 ~ "PRD", PT_r == 3 ~ "PT", PVEM_r == 3 ~ "PVEM", 
      PC_r == 3 ~ "PC", PAS_r == 3 ~ "PAS"))

# Step 17: Drop the ranking columns (those ending with '_r')
df_collapsed <- df_collapsed %>%
  select(-ends_with("_r"))

# Step 18: Add year and month columns
df_collapsed <- df_collapsed %>%
  mutate(year = 2005, month = "November")

# Step 19: Sort the data by 'section'
df_collapsed <- df_collapsed %>%
  arrange(section)

# Step 20: Save the resulting data to a CSV file
write.csv(df_collapsed, "Hidalgo_Section_2005.csv", row.names = FALSE)

# Load the CSV file into a dataframe
df <- read_csv("Ayu_Seccion_2008.csv")

# Rename variables
df <- df %>%
  rename(
    municipality = nombre,
    section = seccion,
    listanominal = listadonominal
  )

# Drop rows with empty municipality and missing section
df <- df %>% filter(!(municipality == "" & is.na(section)))

# Drop rows where 'total' is missing or 0
df <- df %>% filter(!is.na(total) & total != 0)

# Convert the selected columns to numeric (destring equivalent)
df <- df %>%
  mutate(across(c(listanominal, pan:total), as.numeric, .names = "replaced"))

# Sum the selected columns by municipality and section
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan:prdpt, total), sum, .names = "summed"))

# Rename the political parties' variables to uppercase
df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan, PRI = pri, PRD = prd, PT = pt,
    PVEM = pv, PC = pc, PSD = psd, PANAL = alianza,
    PRI_PANAL = prialianza, PRD_PT = prdpt
  )

# Drop rows for specific municipalities
df_filtered <- df_collapsed %>%
  filter(!(municipality %in% c("HUAZALINGO", "ZIMAPAN", "EMILIANO ZAPATA")))

# Generate 'turnout' variable
df_filtered <- df_filtered %>%
  mutate(turnout = total / listanominal)

# Assign unique IDs to municipalities
municipality_ids <- c(
  "ACATLAN" = 13001, "ACAXOCHITLAN" = 13002, "ACTOPAN" = 13003,
  "AGUA BLANCA DE ITURBIDE" = 13004, "AJACUBA" = 13005, "ALFAJAYUCAN" = 13006,
  # Add all other municipalities here...
  "ZIMAPAN" = 13084
)

df_filtered <- df_filtered %>%
  mutate(uniqueid = municipality_ids[municipality])

# Sum across selected columns to generate 'valid'
df_filtered <- df_filtered %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRI_PANAL), na.rm = TRUE))

# Summarise data at municipality level
df_mun_summary <- df_filtered %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN:PRI_PANAL, total, listanominal, valid), sum, .names = "mun_{.col}"))

# Generate inverse of municipality-level variables
df_mun_summary <- df_mun_summary %>%
  mutate(across(starts_with("mun_"), ~ 1 / ., .names = "inv_{.col}"))

# Rank the inverses of the variables
df_mun_summary <- df_mun_summary %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r"))

# Identify the winner, second, and third place
df_mun_summary <- df_mun_summary %>%
  mutate(
    winner = case_when(inv_mun_PAN_r == 1 ~ "PAN", inv_mun_PRI_r == 1 ~ "PRI", TRUE ~ ""),
    second = case_when(inv_mun_PAN_r == 2 ~ "PAN", inv_mun_PRI_r == 2 ~ "PRI", TRUE ~ ""),
    third = case_when(inv_mun_PAN_r == 3 ~ "PAN", inv_mun_PRI_r == 3 ~ "PRI", TRUE ~ "")
  )

# Add year and month
df_mun_summary <- df_mun_summary %>%
  mutate(year = 2008, month = "November")

# Sort by section
df_mun_summary <- df_mun_summary %>%
  arrange(section)

# Save the processed data to a CSV file
write_csv(df_mun_summary, "Hidalgo_Section_2008.csv")

# Import the 2011 Excel data
df_2011 <- read_excel("Ayu_Seccion_2011.xlsx", sheet = "Ayu_Seccion_2011", col_names = TRUE)

# Rename variables to match the 2008 format
df_2011 <- df_2011 %>%
  rename(
    municipality = NOMBRE,
    section = CASILLAS,
    listanominal = LISTANOMINAL,
    total = TOTAL
  )

# Remove invalid rows
df_2011 <- df_2011 %>%
  filter(!CASILLAS %in% c("CASILLAS", "NOMINAL")) %>%
  filter(IdMun != "")

# Remove "B" from CASILLAS and clean other characters
df_2011 <- df_2011 %>%
  mutate(CASILLAS = str_replace_all(CASILLAS, "B", "")) %>%
  mutate(CASILLAS = str_replace_all(CASILLAS, paste0("C", 1:10), "")) %>%
  mutate(CASILLAS = str_replace_all(CASILLAS, paste0("X", 1:10), ""))

# Convert CASILLAS to numeric
df_2011 <- df_2011 %>%
  mutate(CASILLAS = as.numeric(CASILLAS))

# Replace "N/R" with empty string and convert to numeric
df_2011 <- df_2011 %>%
  mutate(across(c(PAN, PRI, PRD, PT, PVEM, PC, PANAL, PAN_PRD, PT_PC, PRI_PVEM), ~str_replace(., "N/R", ""))) %>%
  mutate(across(c(PAN, PRI, PRD, PT, PVEM, PC, PANAL, PAN_PRD, PT_PC, PRI_PVEM), as.numeric))

# Drop rows where specific text patterns occur in vote columns
df_2011 <- df_2011 %>%
  filter(!str_detect(PC, "CASILLA ANULADA")) %>%
  filter(!str_detect(PVEM, "CASILLA ANULADA"))

# Drop rows where both municipality and section are invalid
df_2011 <- df_2011 %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

# Collapse the data by municipality and section, summing the vote columns
df_2011_collapsed <- df_2011 %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal:PRI_PVEM, total), sum, .names = "summed"))

# Drop specific municipalities
df_2011_filtered <- df_2011_collapsed %>%
  filter(!(municipality %in% c("SANTIAGO TULANTEPEC", "XOCHICOATLAN")))

# Generate 'turnout' variable
df_2011_filtered <- df_2011_filtered %>%
  mutate(turnout = total / listanominal)

# Reuse or create a new list of municipality IDs for 2011
municipality_ids_2011 <- c(
  "ACATLAN" = 13001, "ACAXOCHITLAN" = 13002, "ACTOPAN" = 13003,
  # Add the remaining municipalities...
  "ZIMAPAN" = 13084
)

# Assign unique IDs to municipalities
df_2011_filtered <- df_2011_filtered %>%
  mutate(uniqueid = municipality_ids_2011[municipality])

# Calculate 'valid' as row totals of the vote columns
df_2011_filtered <- df_2011_filtered %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRI_PVEM), na.rm = TRUE))

# Summarise data at municipality level
df_mun_summary_2011 <- df_2011_filtered %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN:PRI_PVEM, total, listanominal, valid), sum, .names = "mun_{.col}"))

# Generate inverse of municipality-level variables
df_mun_summary_2011 <- df_mun_summary_2011 %>%
  mutate(across(starts_with("mun_"), ~ 1 / ., .names = "inv_{.col}"))

# Rank the inverse variables for each municipality
df_mun_summary_2011 <- df_mun_summary_2011 %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r"))

# Add year and month
df_mun_summary_2011 <- df_mun_summary_2011 %>%
  mutate(year = 2011, month = "July")

# Sort by section
df_mun_summary_2011 <- df_mun_summary_2011 %>%
  arrange(section)

# Save the processed data to a CSV file
write_csv(df_mun_summary_2011, "Hidalgo_Section_2011.csv")

# Get all sheet names
sheets <- excel_sheets("Ayuntamientos_Hgo_2016.xlsx")

# Loop through each sheet and read the data
for (sheet_name in sheets) {
  # Read the sheet
  df <- read_excel("Ayuntamientos_Hgo_2016.xlsx", sheet = sheet_name, col_types = "text")
  
  # Add a 'municipio' column with the sheet name
  df <- df %>%
    mutate(municipio = sheet_name)
  
  # Replace empty strings with "0"
  df <- df %>%
    mutate(across(everything(), ~ replace(., . == "", "0")))
  
  # Save each sheet as an RDS file
  saveRDS(df, paste0(sheet_name, ".rds"))
}

# List of municipality files
files <- c("Acatlán_A.rds", "Acaxochitlán_A.rds", "Actopan_A.rds", 
           "Agua Blanca de Iturbide_A.rds", "Ajacuba_A.rds", 
           # Add other municipality files here...
           "Zimapán_A.rds")

# Read and combine all the files
combined_df <- bind_rows(lapply(files, readRDS))

# List of municipality files
files <- c("Acatlán_A.rds", "Acaxochitlán_A.rds", "Actopan_A.rds", 
           "Agua Blanca de Iturbide_A.rds", "Ajacuba_A.rds", 
           # Add other municipality files here...
           "Zimapán_A.rds")

# Read and combine all the files
combined_df <- bind_rows(lapply(files, readRDS))

# Clean and fix specific municipality names
combined_df <- combined_df %>%
  mutate(Municipio = case_when(
    Municipio %in% c("Zapotlán De Juárez1619", "Zapotlán De Juárez1620") ~ "Zapotlán De Juárez",
    str_detect(Municipio, "Tula De Allende") ~ "Tula De Allende",
    TRUE ~ Municipio
  ))

# Convert municipality names to uppercase
combined_df <- combined_df %>%
  mutate(municipality = str_to_upper(Municipio))

# Generate unique IDs based on municipality
municipality_ids <- c(
  "ACATLÁN" = 13001, "ACAXOCHITLÁN" = 13002, "ACTOPAN" = 13003,
  # Add all the municipality name to ID mappings here...
  "ZIMAPÁN" = 13084
)

combined_df <- combined_df %>%
  mutate(uniqueid = municipality_ids[municipality])

# Drop rows where uniqueid is missing
combined_df <- combined_df %>%
  filter(!is.na(uniqueid))

# Drop unnecessary columns
combined_df <- combined_df %>%
  select(-c(Q:AX))  # Replace Q:AX with actual column names as necessary

# Handle missing values by replacing them with 0
combined_df <- combined_df %>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))

# Calculate total values for several columns
combined_df <- combined_df %>%
  rowwise() %>%
  mutate(CI_2 = sum(c(JGUADALUPESANTIAGOAGUILAR, ANDRÉSGILDARDOOCADIZIBARRA, JOSÉLUISARROYAVEHERNÁNDEZ, LUCIACARIÑOVITEHERNANDEZ, JUANLUISHERNÁNDEZCÁZARES, MARIOJARILLOHERNÁNDEZ, JAVIERGÓMEZPICHARDO), na.rm = TRUE)) %>%
  ungroup()

# Drop unused columns
combined_df <- combined_df %>%
  select(-c(JGUADALUPESANTIAGOAGUILAR: JAVIERGÓMEZPICHARDO))

# Group by municipality, section, and unique ID, and sum the relevant columns
summary_df <- combined_df %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(c(PAN:PRI_PVEM_PANAL, CI_3:total), sum, .names = "mun_{.col}")) %>%
  ungroup()

# Rank the columns based on their inverse values
summary_df <- summary_df %>%
  mutate(across(starts_with("mun_"), ~ rank(-., ties.method = "first"), .names = "{.col}_r"))

# Determine winners based on ranks
summary_df <- summary_df %>%
  mutate(
    winner = case_when(PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", TRUE ~ ""),
    second = case_when(PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", TRUE ~ ""),
    third = case_when(PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", TRUE ~ "")
  )

# Save the final combined dataset
write.csv(summary_df, "Hidalgo_Section_2016.csv", row.names = FALSE)













