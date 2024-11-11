# Load required libraries
library(dplyr)
library(tidyr)
library(readr)

# Set the working directory (adjust the path as needed)

#############################################
### Function to Process Individual Year Data
#############################################
process_year <- function(file, year, has_nominal = FALSE, nominal_file = NULL) {
  data <- read_csv(file)
  
  # Rename common columns
  data <- data %>%
    rename(municipality = municipio, section = seccion) %>%
    filter(!is.na(municipality) & !is.na(section)) %>%
    mutate_at(vars(pan:total), as.numeric) %>%
    filter(!is.na(total) & total > 0)
  
  # Custom renaming for each year
  if (year == 1995) {
    data <- data %>%
      rename(PAN = pan, PRI = pri, PRD = prd, PartCardenista = pfcrn, PT = pt, PVEM = pvem)
  } else if (year == 1998) {
    data <- data %>%
      rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem)
  } else if (year == 2001) {
    data <- data %>%
      rename(PAN = pan, PRI = pri, PRD = prd, PRD_PT = prdpt, PT = pt, PVEM = pvem, PC = pc, PSN = psn, PAS = pas, PD = pd)
  } else if (year == 2004) {
    data <- data %>%
      rename(PAN = pan, PRI = pri, PRD_PT = prdpt, PVEM = pvem, PD = pd)
  } else if (year == 2007) {
    data <- data %>%
      rename(PAN = pan, PRI_PANAL = pripanal, PRI_PANAL_PD = pripanalpd, PRD = prd, PT_PC = ptpc, PVEM = pvem, PAS = pas, PD = pd)
  } else if (year == 2010) {
    data <- data %>%
      rename(PAN_PRD_PC = panprdpc, PRI_PVEM_PANAL_PD = pripvempanalpd, PT = pt)
  } else if (year == 2013) {
    data <- data %>%
      rename(PAN = pan, PRI_PVEM_PANAL_PD = pri_pvem_panal_pd, PRD = prd, PT = pt, PC = mc)
  }
  
  # Process nominal data for turnout if available
  if (has_nominal) {
    nominal_data <- read_csv(nominal_file) %>%
      rename(section = SEC, listanominal = LISTA) %>%
      group_by(section) %>%
      summarise(listanominal = sum(listanominal, na.rm = TRUE))
    data <- left_join(data, nominal_data, by = "section")
  }
  
  # Add unique IDs for municipalities
  data <- data %>%
    mutate(uniqueid = case_when(
      municipality == "CANATLAN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO DE COMONFORT" ~ 10003,
      municipality == "CUENCAME" ~ 10004,
      municipality == "DURANGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      municipality == "SIMON BOLIVAR" ~ 10006,
      municipality == "GOMEZ PALACIO" ~ 10007,
      municipality == "GUADALUPE VICTORIA" ~ 10008,
      municipality == "GUANACEVI" ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "INDE" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIMI" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTAEZ" ~ 10019,
      municipality == "PANUCO DE CORONADO" ~ 10020,
      municipality == "PENON BLANCO" ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
      municipality == "SAN JUAN DEL RIO" ~ 10028,
      municipality == "SAN LUIS DEL CORDERO" ~ 10029,
      municipality == "SAN PEDRO DEL GALLO" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
      municipality == "SUCHIL" ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRERO" ~ 10038
    )) %>%
    group_by(municipality, section, uniqueid) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    ungroup()
  
  # Add turnout
  if (has_nominal) {
    data <- data %>%
      mutate(turnout = total / listanominal)
  }
  
  # Add year and month
  data <- data %>%
    mutate(year = year, month = "July")
  
  return(data)
}

#############################################
### Step 1: Process Each Year's Data
#############################################
data_1995 <- process_year("Ayu_Seccion_1995_No_LN.csv", 1995)
data_1998 <- process_year("Ayu_Seccion_1998_No_LN.csv", 1998)
data_2001 <- process_year("Ayu_Seccion_2001_No_LN.csv", 2001)
data_2004 <- process_year("Ayu_Seccion_2004_No_LN.csv", 2004, TRUE, "ListaNominal2004.csv")
data_2007 <- process_year("Ayu_Seccion_2007.dta", 2007, TRUE, "ListaNominal2007.csv")
data_2010 <- process_year("Ayu_Seccion_2010.csv", 2010)
data_2013 <- process_year("Ayu_Seccion_2013.csv", 2013)

#############################################
### Step 2: Combine All Data and Save
#############################################
combined_data <- bind_rows(data_1995, data_1998, data_2001, data_2004, data_2007, data_2010, data_2013)

# Save the combined dataset
saveRDS(combined_data, "Durango_1995_2013.rds")

#############################################
### Step 3: Determine Winners and Save Final Dataset
#############################################
# Calculate valid votes and rank candidates
combined_data <- combined_data %>%
  mutate(valid = rowSums(select(., starts_with("PAN"):starts_with("PVEM"), na.rm = TRUE))) %>%
  mutate(across(starts_with("mun_"), ~ rank(desc(.)), .names = "{.col}_r")) %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    TRUE ~ "Other"
  ))

# Save the final dataset with winners
saveRDS(combined_data, "Durango_final_1995_2013.rds")

#######################################
### Step 1: Process 2016 Data (Excel)
#######################################

# Import Excel sheet names
file_path_2016 <- "Ayuntamientos Municipio_2016.xlsx"
sheets_2016 <- excel_sheets(file_path_2016)

# Loop over sheets, process each municipality, and save as separate RDS files
for (sheetname in sheets_2016) {
  data <- read_excel(file_path_2016, sheet = sheetname, col_types = "text")
  data <- data %>%
    mutate(municipio = sheetname)  # Add municipality name as a column
  
  # Save each sheet as RDS file
  saveRDS(data, paste0(sheetname, ".rds"))
}

# Combine all the sheets into one dataset for 2016
combined_2016 <- do.call(bind_rows, lapply(sheets_2016, function(sheetname) {
  readRDS(paste0(sheetname, ".rds"))
}))

# Delete intermediate RDS files
file.remove(paste0(sheets_2016, ".rds"))

# Data cleaning and transformations for 2016
combined_2016 <- combined_2016 %>%
  mutate(munno = substr(municipio, 1, 2)) %>%
  mutate(munno = ifelse(municipio == "07. GRAL. SIMÓN BOLÍVAR", "06", munno),
         munno = ifelse(municipio == "06. GÓMEZ PALACIO", "07", munno)) %>%
  mutate(munno = as.numeric(munno),
         munno = ifelse(munno >= 18, munno - 1, munno),
         munno = ifelse(municipio == "17. NUEVO IDEAL", 39, munno),
         uniqueid = munno + 10000,
         municipality = substr(municipio, 4, nchar(municipio))) %>%
  select(-municipio)

# Rename columns for 2016
combined_2016 <- combined_2016 %>%
  rename(section = SECCIÓN,
         PAN_PRD = PANPRD,
         PRI_PVEM_PD_PANAL = PRIPVEMPDPNA,
         total = TOTAL,
         PANAL = PNA,
         listanominal = LNOMINAL)

# Data aggregation for 2016
combined_2016 <- combined_2016 %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(PAN_PRD:listanominal, sum, na.rm = TRUE)) %>%
  ungroup()

# Save the cleaned and processed 2016 dataset
saveRDS(combined_2016, "Durango_Section_2016.rds")

#######################################
### Step 2: Process 2019 Data (CSV)
#######################################

# Import CSV data for 2019
file_path_2019 <- "2019_SEE_AYUN _DGO_CAS.csv"
data_2019 <- read.csv(file_path_2019)

# Data cleaning and transformations for 2019
data_2019 <- data_2019 %>%
  rename(section = seccion,
         municipality = municipio_local,
         listanominal = lista_nominal,
         total = total_votos) %>%
  mutate(PAN_PRD = ifelse(!is.na(coal_pan_prd), pan + prd + coal_pan_prd, NA),
         pan = ifelse(!is.na(coal_pan_prd), NA, pan),
         prd = ifelse(!is.na(coal_pan_prd), NA, prd)) %>%
  select(-coal_pan_prd)

# Municipal unique IDs for 2019
data_2019 <- data_2019 %>%
  mutate(uniqueid = case_when(
    municipality == "CANATLAN" ~ 10001,
    municipality == "CANELAS" ~ 10002,
    municipality == "CONETO DE COMONFORT" ~ 10003,
    municipality == "CUENCAME" ~ 10004,
    municipality == "DURANGO" ~ 10005,
    municipality == "EL ORO" ~ 10018,
    municipality == "SIMON BOLIVAR" ~ 10006,
    municipality == "GOMEZ PALACIO" ~ 10007,
    municipality == "GUADALUPE VICTORIA" ~ 10008,
    municipality == "GUANACEVI" ~ 10009,
    municipality == "HIDALGO" ~ 10010,
    municipality == "INDE" ~ 10011,
    municipality == "LERDO" ~ 10012,
    municipality == "MAPIMI" ~ 10013,
    municipality == "MEZQUITAL" ~ 10014,
    municipality == "NAZAS" ~ 10015,
    municipality == "NOMBRE DE DIOS" ~ 10016,
    municipality == "NUEVO IDEAL" ~ 10039,
    municipality == "OCAMPO" ~ 10017,
    municipality == "OTAEZ" ~ 10019,
    municipality == "PANUCO DE CORONADO" ~ 10020,
    municipality == "PEÃON BLANCO" | municipality == "PENON BLANCO" ~ 10021,
    municipality == "POANAS" ~ 10022,
    municipality == "PUEBLO NUEVO" ~ 10023,
    municipality == "RODEO" ~ 10024,
    municipality == "SAN BERNARDO" ~ 10025,
    municipality == "SAN DIMAS" ~ 10026,
    municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
    municipality == "SAN JUAN DEL RIO" ~ 10028,
    municipality == "SAN LUIS DEL CORDERO" ~ 10029,
    municipality == "SAN PEDRO DEL GALLO" ~ 10030,
    municipality == "SANTA CLARA" ~ 10031,
    municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
    municipality == "SUCHIL" ~ 10033,
    municipality == "TAMAZULA" ~ 10034,
    municipality == "TEPEHUANES" ~ 10035,
    municipality == "TLAHUALILO" ~ 10036,
    municipality == "TOPIA" ~ 10037,
    municipality == "VICENTE GUERRERO" ~ 10038))

# Data aggregation for 2019
data_2019 <- data_2019 %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(PAN:listanominal, sum, na.rm = TRUE)) %>%
  ungroup()

# Save the cleaned and processed 2019 dataset
saveRDS(data_2019, "Durango_Section_2019.rds")

#######################################
### Step 3: Combining 2016 and 2019 Data
#######################################

# Load the saved datasets for 2016 and 2019
data_2016 <- readRDS("Durango_Section_2016.rds")
data_2019 <- readRDS("Durango_Section_2019.rds")

# Combine both years into one dataset
combined_all <- bind_rows(data_2016, data_2019)

# Save the final combined dataset
saveRDS(combined_all, "Durango_all_SALVADOR.rds")

#######################################
### Final Steps: Winners and Municipal DB Generation
#######################################

# Calculate valid votes and turnout for each section
combined_all <- combined_all %>%
  mutate(valid = rowSums(select(., PAN_PRD:CI_2), na.rm = TRUE),
         turnout = total / listanominal,
         mun_turnout = ave(total, uniqueid, FUN = sum) / ave(listanominal, uniqueid, FUN = sum))

# Ranking and determining winners
combined_all <- combined_all %>%
  mutate(across(starts_with("mun_"), ~ rank(desc(.)), .names = "{.col}_r")) %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    MORENA_r == 1 ~ "MORENA",
    # Add additional conditions for other parties
    TRUE ~ "Independent"
  ))

# Save the updated final dataset with winners
saveRDS(combined_all, "Durango_all_with_winner_SALVADOR.rds")