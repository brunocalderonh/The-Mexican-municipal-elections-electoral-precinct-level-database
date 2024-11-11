# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)

############################################
## Processing Data for All Years (1994 - 2012)
############################################

process_year <- function(file_path, year, municipality_mapping, merge_listanominal_path = NULL) {
  
  # Load CSV data
  data <- read.csv(file_path)
  
  # Common renaming and filtering across years
  data <- data %>%
    rename(municipality = municipio, section = seccion) %>%
    filter(!is.na(municipality) & !is.na(section), total > 0)
  
  # Destring all necessary columns (converting them to numeric)
  data <- data %>%
    mutate(across(where(is.character), as.numeric, .names = "numeric_{.col}"))
  
  # Collapse by municipality and section (sum)
  collapsed_data <- data %>%
    group_by(municipality, section) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    ungroup()
  
  # Add uniqueid using mapping
  collapsed_data <- collapsed_data %>%
    mutate(uniqueid = case_when(
      municipality %in% names(municipality_mapping) ~ municipality_mapping[municipality],
      TRUE ~ 0
    ))
  
  # Calculate valid votes and municipal sums
  collapsed_data <- collapsed_data %>%
    rowwise() %>%
    mutate(valid = sum(c_across(PAN:total), na.rm = TRUE)) %>%
    ungroup()
  
  # Calculate turnout and municipal turnout
  if (!is.null(merge_listanominal_path)) {
    listanominal_data <- readRDS(merge_listanominal_path)
    collapsed_data <- collapsed_data %>%
      left_join(listanominal_data, by = "section") %>%
      mutate(turnout = total / listanominal)
  }
  
  # Add year and month
  collapsed_data <- collapsed_data %>%
    mutate(year = year, month = ifelse(year == 1994, "August", "July"))
  
  return(collapsed_data)
}

# Define the mapping for unique municipality IDs
municipality_mapping <- c(
  "ARMERIA" = 6001,
  "COLIMA" = 6002,
  "COMALA" = 6003,
  "COQUIMATLAN" = 6004,
  "CUAUHTEMOC" = 6005,
  "IXTLAHUACAN" = 6006,
  "MANZANILLO" = 6007,
  "MINATITLAN" = 6008,
  "TECOMAN" = 6009,
  "VILLA DE ALVAREZ" = 6010
)

# Process data for each year
data_1994 <- process_year("Ayu_Seccion_1994_No_LN.csv", 1994, municipality_mapping)
data_1997 <- process_year("Ayu_Seccion_1997.csv", 1997, municipality_mapping)
data_2000 <- process_year("Ayu_Seccion_2000_No_LN.csv", 2000, municipality_mapping)
data_2003 <- process_year("Ayu_Seccion_2003.csv", 2003, municipality_mapping)
data_2006 <- process_year("Ayu_Seccion_2006.csv", 2006, municipality_mapping)
data_2009 <- process_year("Ayu_Seccion_2009.csv", 2009, municipality_mapping)
data_2012 <- process_year("CASILLAS_AYUNTAMIENTOS_2012/Ayu_Seccion_2012.dta", 2012, municipality_mapping)

############################################
## Merging Data for All Years
############################################

# Combine all years into a single dataset
all_data <- bind_rows(data_1994, data_1997, data_2000, data_2003, data_2006, data_2009, data_2012)

# Save the combined data for all years
saveRDS(all_data, "Colima_ALL.rds")

############################################
## Election Result Processing (Winner Calculation)
############################################

# Rank each municipality and determine the winner for each section
rank_and_winner <- function(data) {
  
  # Rank the results within each section
  ranked_data <- data %>%
    mutate(across(starts_with("mun_"), ~ rank(., ties.method = "min"), .names = "{.col}_r")) %>%
    mutate(winner = case_when(
      mun_PAN_r == 1 ~ "PAN",
      mun_PRI_r == 1 ~ "PRI",
      mun_PRD_r == 1 ~ "PRD",
      mun_PT_r == 1 ~ "PT",
      mun_PVEM_r == 1 ~ "PVEM",
      mun_PPS_r == 1 ~ "PPS",
      mun_PD_r == 1 ~ "PD"
    ))
  
  return(ranked_data)
}

# Apply ranking and winner calculation
all_data_ranked <- rank_and_winner(all_data)

# Save the final combined data with winners
saveRDS(all_data_ranked, "Colima_ALL_FINAL.rds")

############################################
## Final Steps: Tabulate Winners and Save
############################################

# Tabulate the winners
all_data_ranked %>%
  group_by(winner) %>%
  summarise(count = n()) %>%
  print()

# Calculate PAN winners and others
all_data_ranked <- all_data_ranked %>%
  mutate(PAN_winner = ifelse(str_detect(winner, "PAN") & !str_detect(winner, "PANAL"), 1, 0),
         winner_counter = PAN_winner)

# Add winner counters for each party
for (party in c("PRI", "PRD", "PT", "PVEM", "PANAL", "ADC")) {
  all_data_ranked <- all_data_ranked %>%
    mutate(!!paste0(party, "_winner") := ifelse(str_detect(winner, party), 1, 0),
           winner_counter = winner_counter + !!sym(paste0(party, "_winner")))
}

# Save the final combined data
saveRDS(all_data_ranked, "Colima_ALL_FINAL_with_winner.rds")


###########################################
### Function to Process Individual Sheets
###########################################
process_excel_sheet <- function(sheetname, uniqueid) {
  data <- read_excel("RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet = sheetname, col_types = "text") %>%
    mutate(municipality = sheetname, uniqueid = uniqueid) %>%
    filter(!is.na(TIPOCASILLA)) %>%
    fill(SECCION, .direction = "down") %>%
    select(SECCION, LISTANOMINAL, TOTAL, CANDIDATOSNOREGISTRADOS, VOTOSNULOS, Morena) %>%
    mutate_all(as.numeric) %>%
    replace(is.na(.), 0) %>%
    group_by(municipality, uniqueid, SECCION) %>%
    summarise(across(LISTANOMINAL:TOTAL, sum, na.rm = TRUE)) %>%
    rename(section = SECCION, listanominal = LISTANOMINAL, total = TOTAL, no_reg = CANDIDATOSNOREGISTRADOS, nulo = VOTOSNULOS, MORENA = Morena)
  
  return(data)
}

###########################################
### Step 1: Process 2015 Data
###########################################

# Process each municipality's sheet
armenia <- process_excel_sheet("ARMERIA", "6001")
colima <- process_excel_sheet("COLIMA", "6002")
comala <- process_excel_sheet("COMALA", "6003")
coquimatlan <- process_excel_sheet("COQUIMATLAN", "6004")
cuauhtemoc <- process_excel_sheet("CUAUHTEMOC", "6005")
ixtlahuacan <- process_excel_sheet("IXTLAHUACAN", "6006")
manzanillo <- process_excel_sheet("MANZANILLO", "6007")
minatitlan <- process_excel_sheet("MINATITLAN", "6008")
tecoman <- process_excel_sheet("TECOMAN", "6009")
villa_de_alvarez <- process_excel_sheet("VILLA DE ALVAREZ", "6010")

# Combine all sheets
combined_2015 <- bind_rows(armenia, colima, comala, coquimatlan, cuauhtemoc, ixtlahuacan, manzanillo, minatitlan, tecoman, villa_de_alvarez)

# Save each municipality dataset
saveRDS(armenia, "ARMERIA_PS.rds")
saveRDS(colima, "COLIMA_PS.rds")
saveRDS(comala, "COMALA_PS.rds")
saveRDS(coquimatlan, "COQUIMATLAN_PS.rds")
saveRDS(cuauhtemoc, "CUAUHTEMOC_PS.rds")
saveRDS(ixtlahuacan, "IXTLAHUACAN_PS.rds")
saveRDS(manzanillo, "MANZANILLO_PS.rds")
saveRDS(minatitlan, "MINATITLAN_PS.rds")
saveRDS(tecoman, "TECOMAN_PS.rds")
saveRDS(villa_de_alvarez, "VILLADEALVAREZ_PS.rds")

# Combine all data and save the full 2015 dataset
saveRDS(combined_2015, "COLIMA_2015.rds")

###########################################
### Step 2: Process 2018 Data
###########################################

# Load the 2018 data
data_2018 <- read_excel("1 COL RES AYUN.xlsx", sheet = "Reporte_Casillas_Ayuntamientos", col_types = "text") %>%
  mutate_all(as.numeric) %>%
  mutate(PAN_PRD = PAN + PRD + PANPRD,
         PRI_PVEM_PANAL = PRI + PVEM + PRIPVEM,
         MORENA_PT_PES = PTMORENAPES + PTMORENA + MORENAPES + PTPES + PT + MORENA + PES) %>%
  select(-PAN, -PRD, -PANPRD, -PRI, -PVEM, -PRIPVEM, -PTMORENAPES, -PTMORENA, -MORENAPES, -PTPES, -PT, -MORENA, -PES) %>%
  rename(section = Sección, municipality = Municipio, listanominal = ListaNominal, total = TOTALDEVOTOS, no_reg = NOREG, nulo = NULOS) %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:total, sum, na.rm = TRUE))

# Save the 2018 data
saveRDS(data_2018, "COLIMA_2018.rds")

###########################################
### Step 3: Combine 2015 and 2018 Data
###########################################

# Load the 2015 and 2018 datasets
data_2015 <- readRDS("COLIMA_2015.rds")
data_2018 <- readRDS("COLIMA_2018.rds")

# Combine both years
combined_data <- bind_rows(data_2015, data_2018)

# Save the combined data
saveRDS(combined_data, "COLIMA_2015_2018.rds")

###########################################
### Step 4: Calculate Turnout, Winners, and Save Final Dataset
###########################################

# Calculate turnout, winner ranks, and identify winners
combined_data <- combined_data %>%
  mutate(turnout = total / listanominal) %>%
  group_by(municipality, uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the parties
combined_data <- combined_data %>%
  rowwise() %>%
  mutate(winner = case_when(
    PAN_PRD == max(c(PAN_PRD, PRI_PVEM_PANAL, MORENA_PT_PES, MC, PT, PH, PES, PANAL)) ~ "PAN_PRD",
    PRI_PVEM_PANAL == max(c(PAN_PRD, PRI_PVEM_PANAL, MORENA_PT_PES, MC, PT, PH, PES, PANAL)) ~ "PRI_PVEM_PANAL",
    MORENA_PT_PES == max(c(PAN_PRD, PRI_PVEM_PANAL, MORENA_PT_PES, MC, PT, PH, PES, PANAL)) ~ "MORENA_PT_PES",
    TRUE ~ "Others"
  ))

# Save the final dataset with winners
saveRDS(combined_data, "COLIMA_Final_2015_2018.rds")
