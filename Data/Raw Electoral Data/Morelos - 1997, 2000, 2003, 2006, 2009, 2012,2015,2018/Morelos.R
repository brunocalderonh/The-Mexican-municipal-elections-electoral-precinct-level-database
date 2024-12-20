# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)

# Set working directory (with error handling, similar to Stata's 'capture cd')
setwd("D:/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Morelos - 1997, 2000, 2003, 2006, 2009, 2012")
if (!dir.exists(getwd())) {
  setwd("C:/Users/Horacio/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Morelos - 1997, 2000, 2003, 2006, 2009, 2012")
}

# Load the CSV file (equivalent to Stata's 'insheet' command)
data <- read_csv("Ayu_Seccion_1997_No_LN.csv")

# Rename the columns
data <- data %>%
  rename(
    municipality = municipio,
    section = seccion
  )

# Drop rows where both 'municipality' and 'section' are missing or empty
data <- data %>%
  filter(!(is.na(municipality) & is.na(section)))

# Create the 'total' variable as the row sum of the specified columns
data <- data %>%
  mutate(total = rowSums(select(., pan:pps, pdm, noregistrados, nulos), na.rm = TRUE))

# Drop rows where 'total' is missing or zero
data <- data %>%
  filter(!is.na(total) & total != 0)

# Convert columns from 'pan' to 'nulos' to numeric (equivalent to 'destring' in Stata)
data <- data %>%
  mutate(across(pan:nulos, as.numeric))

# Collapse the data by summing the variables by 'municipality' and 'section'
data_collapsed <- data %>%
  group_by(municipality, section) %>%
  summarise(across(pan:nulos, sum, na.rm = TRUE),
            total = sum(total, na.rm = TRUE)) %>%
  ungroup()

# Rename columns to match Stata code
data_collapsed <- data_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PartCardenista = pc,
    PT = pt,
    PVEM = pvem,
    PCM = pcm,
    PPS = pps,
    PDM = pdm
  )

# Drop 'noregistrados' and 'nulos' columns
data_collapsed <- data_collapsed %>%
  select(-noregistrados, -nulos)

# Create the 'uniqueid' variable based on 'municipality'
data_collapsed <- data_collapsed %>%
  mutate(uniqueid = case_when(
    municipality == "AMACUZAC" ~ 17001,
    municipality == "ATLATLAHUCAN" ~ 17002,
    municipality == "Axochiapan" ~ 17003,
    municipality == "AYALA" ~ 17004,
    municipality == "COATLAN DEL RIO" ~ 17005,
    municipality == "CUAUTLA" ~ 17006,
    municipality == "CUERNAVACA" ~ 17007,
    municipality == "EMILIANO ZAPATA" ~ 17008,
    municipality == "HUITZILAC" ~ 17009,
    municipality == "Jantetelco" ~ 17010,
    municipality == "JIUTEPEC" ~ 17011,
    municipality == "JOJUTLA" ~ 17012,
    municipality == "JONACATEPEC" ~ 17013,
    municipality == "MAZATEPEC" ~ 17014,
    municipality == "MIACATLAN" ~ 17015,
    municipality == "OCUITUCO" ~ 17016,
    municipality == "PUENTE DE IXTLA" ~ 17017,
    municipality == "TEMIXCO" ~ 17018,
    municipality == "TEMOAC" ~ 17033,
    municipality == "TEPALCINGO" ~ 17019,
    municipality == "TEPOZTLAN" ~ 17020,
    municipality == "TETECALA" ~ 17021,
    municipality == "TETELA DEL VOLCAN" ~ 17022,
    municipality == "TLALNEPANTLA" ~ 17023,
    municipality == "TLALTIZAPAN" ~ 17024,
    municipality == "TLAQUILTENANGO" ~ 17025,
    municipality == "TLAYACAPAN" ~ 17026,
    municipality == "TOTOLAPAN" ~ 17027,
    municipality == "XOCHITEPEC" ~ 17028,
    municipality == "YAUTEPEC" ~ 17029,
    municipality == "YECAPIXTLA" ~ 17030,
    municipality == "ZACATEPEC" ~ 17031,
    municipality == "ZACUALPAN DE AMILPAS" ~ 17032,
    TRUE ~ 0 # Default value
  ))

# Create 'valid' variable as row total of all the party columns
data_collapsed <- data_collapsed %>%
  mutate(valid = rowSums(select(., PAN:PartCardenista, PT, PVEM, PCM, PPS, PDM), na.rm = TRUE))

# Calculate municipality-level aggregates for each variable (equivalent to 'egen' with sum)
vars <- c("PAN", "PRI", "PRD", "PartCardenista", "PT", "PVEM", "PCM", "PPS", "PDM", "total", "valid")

for (var in vars) {
  data_collapsed <- data_collapsed %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / (!!sym(paste0("mun_", var))))
}

# Generate the 'ed' and 'seccion' variables
data_collapsed <- data_collapsed %>%
  mutate(ed = 17,
         seccion = section)

# Load and merge with external dataset (assuming .dta file for the merge)
all_months_years <- read_dta("../../all_months_years.dta")

# Merge datasets, keep only necessary columns
data_merged <- data_collapsed %>%
  left_join(all_months_years, by = c("ed", "seccion")) %>%
  filter(month == 1 & year == 1997) %>%
  select(-_merge, -ed, -seccion, -year, -month) %>%
  rename(listanominal = lista)

# Generate 'turnout' variable
data_merged <- data_merged %>%
  mutate(turnout = total / listanominal)

# Calculate municipality-level aggregates for 'listanominal' and its inverse
data_merged <- data_merged %>%
  group_by(uniqueid) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE),
         inv_mun_listanominal = 1 / mun_listanominal)

# Generate 'mun_turnout' variable
data_merged <- data_merged %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank parties at the municipality level (equivalent to 'rowranks' in Stata)
data_merged <- data_merged %>%
  mutate(
    PAN_r = dense_rank(desc(inv_mun_PAN)),
    PRI_r = dense_rank(desc(inv_mun_PRI)),
    PRD_r = dense_rank(desc(inv_mun_PRD)),
    PartCardenista_r = dense_rank(desc(inv_mun_PartCardenista)),
    PT_r = dense_rank(desc(inv_mun_PT)),
    PVEM_r = dense_rank(desc(inv_mun_PVEM)),
    PCM_r = dense_rank(desc(inv_mun_PCM)),
    PPS_r = dense_rank(desc(inv_mun_PPS)),
    PDM_r = dense_rank(desc(inv_mun_PDM))
  )

# Determine the winner
data_merged <- data_merged %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_r == 1 ~ "PRI",
      PRD_r == 1 ~ "PRD",
      PartCardenista_r == 1 ~ "PartCardenista",
      PT_r == 1 ~ "PT",
      PVEM_r == 1 ~ "PVEM",
      PCM_r == 1 ~ "PCM",
      PPS_r == 1 ~ "PPS",
      PDM_r == 1 ~ "PDM",
      TRUE ~ NA_character_
    )
  )

# Drop ranking variables
data_merged <- data_merged %>%
  select(-ends_with("_r"))

# Add 'year' and 'month' variables
data_merged <- data_merged %>%
  mutate(year = 1997,
         month = "March")

# Save the final dataset (equivalent to Stata's 'save')
write_dta(data_merged, "Morelos_Section_1997.dta")

# Read the CSV file
data_2000 <- read_csv("Ayu_Seccion_2000.csv")

# Rename columns
data_2000 <- data_2000 %>%
  rename(municipality = municipio, 
         section = seccion)

# Drop rows with missing municipality and section values
data_2000 <- data_2000 %>%
  filter(!(municipality == "" & is.na(section)))

# Calculate the total row sum
data_2000 <- data_2000 %>%
  mutate(total = rowSums(select(., pan:pas, noreg, nulos), na.rm = TRUE))

# Drop rows with missing or zero totals
data_2000 <- data_2000 %>%
  filter(!is.na(total) & total != 0)

# Convert selected columns to numeric
data_2000 <- data_2000 %>%
  mutate_at(vars(listanominal, pan:nulos, total), as.numeric)

# Create 'missing' column to identify rows where 'listanominal' is missing
data_2000 <- data_2000 %>%
  mutate(missing = ifelse(is.na(listanominal), 1, 0))

# Collapse (summarize) the dataset by municipality and section
data_2000 <- data_2000 %>%
  group_by(municipality, section) %>%
  summarize_at(vars(missing, listanominal, pan:nulos, total), sum, na.rm = TRUE)

# Create 'ed' and 'seccion' variables
data_2000 <- data_2000 %>%
  mutate(ed = 17,
         seccion = section)

# Load the external dataset and merge on 'ed' and 'seccion'
all_months_years <- read_dta("../../all_months_years.dta")

# Merge datasets
data_merged <- data_2000 %>%
  left_join(all_months_years, by = c("ed", "seccion")) %>%
  filter(month == 7 & year == 1997) %>%
  select(-ed, -seccion, -year, -month, -_merge)

# Replace 'listanominal' where 'missing' is greater than or equal to 1
data_merged <- data_merged %>%
  mutate(listanominal = ifelse(missing >= 1, lista, listanominal)) %>%
  select(-missing, -lista)

# Rename columns
data_merged <- data_merged %>%
  rename(PAN = pan,
         PRI = pri,
         PRD_PC_PCD_PSN = apm,
         PT = pt,
         PVEM = pvem,
         PCM = pcm,
         PARM = parm,
         PDS = pds,
         PAS = pas)

# Calculate 'turnout'
data_merged <- data_merged %>%
  mutate(turnout = total / listanominal)

# Drop unused columns
data_merged <- data_merged %>%
  select(-noreg, -nulos)

# Generate 'uniqueid' based on municipality names
data_merged <- data_merged %>%
  mutate(uniqueid = case_when(
    municipality == "AMACUZAC" ~ 17001,
    municipality == "ATLATLAHUCAN" ~ 17002,
    municipality == "AXOCHIAPAN" ~ 17003,
    municipality == "AYALA" ~ 17004,
    municipality == "COATLAN DEL RIO" ~ 17005,
    municipality == "CUAUTLA" ~ 17006,
    municipality == "CUERNAVACA" ~ 17007,
    municipality == "EMILIANO ZAPATA" ~ 17008,
    municipality == "HUITZILAC" ~ 17009,
    municipality == "JANTETELCO" ~ 17010,
    municipality == "JIUTEPEC" ~ 17011,
    municipality == "JOJUTLA" ~ 17012,
    municipality == "JONACATEPEC" ~ 17013,
    municipality == "MAZATEPEC" ~ 17014,
    municipality == "MIACATLAN" ~ 17015,
    municipality == "OCUITUCO" ~ 17016,
    municipality == "PUENTE DE IXTLA" ~ 17017,
    municipality == "TEMIXCO" ~ 17018,
    municipality == "TEMOAC" ~ 17033,
    municipality == "TEPALCINGO" ~ 17019,
    municipality == "TEPOZTLAN" ~ 17020,
    municipality == "TETECALA" ~ 17021,
    municipality == "TETELA DEL VOLCAN" ~ 17022,
    municipality == "TLALNEPANTLA" ~ 17023,
    municipality == "TLALTIZAPAN" ~ 17024,
    municipality == "TLAQUILTENANGO" ~ 17025,
    municipality == "TLAYACAPAN" ~ 17026,
    municipality == "TOTOLAPAN" ~ 17027,
    municipality == "XOCHITEPEC" ~ 17028,
    municipality == "YAUTEPEC" ~ 17029,
    municipality == "YECAPIXTLA" ~ 17030,
    municipality == "ZACATEPEC" ~ 17031,
    municipality == "ZACUALPAN" ~ 17032,
    TRUE ~ 0
  ))

# Calculate 'valid' votes (row total)
data_merged <- data_merged %>%
  mutate(valid = rowSums(select(., PAN: PAS), na.rm = TRUE))

# Aggregate at the municipality level and calculate inverse values
vars <- c("PAN", "PRI", "PRD_PC_PCD_PSN", "PT", "PVEM", "PCM", "PARM", "PDS", "PAS", "listanominal", "total", "valid")

for (var in vars) {
  data_merged <- data_merged %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / (!!sym(paste0("mun_", var))))
}

# Calculate 'mun_turnout'
data_merged <- data_merged %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank parties within the municipality (similar to rowranks in Stata)
data_merged <- data_merged %>%
  mutate(
    PAN_r = dense_rank(desc(inv_mun_PAN)),
    PRI_r = dense_rank(desc(inv_mun_PRI)),
    PRD_PC_PCD_PSN_r = dense_rank(desc(inv_mun_PRD_PC_PCD_PSN)),
    PT_r = dense_rank(desc(inv_mun_PT)),
    PVEM_r = dense_rank(desc(inv_mun_PVEM)),
    PCM_r = dense_rank(desc(inv_mun_PCM)),
    PARM_r = dense_rank(desc(inv_mun_PARM)),
    PDS_r = dense_rank(desc(inv_mun_PDS)),
    PAS_r = dense_rank(desc(inv_mun_PAS))
  )

# Determine winner
data_merged <- data_merged %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_PC_PCD_PSN_r == 1 ~ "PRD_PC_PCD_PSN",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PCM_r == 1 ~ "PCM",
    PARM_r == 1 ~ "PARM",
    PDS_r == 1 ~ "PDS",
    PAS_r == 1 ~ "PAS",
    TRUE ~ NA_character_
  ))

# Drop ranking variables
data_merged <- data_merged %>%
  select(-ends_with("_r"))

# Add 'year' and 'month' variables
data_merged <- data_merged %>%
  mutate(year = 2000,
         month = "July")

# Save the final dataset
write_dta(data_merged, "Morelos_Section_2000.dta")

# Load necessary libraries
library(dplyr)
library(readxl)
library(haven)

# Import Excel file and read the first sheet
data_2001 <- read_excel("Extraordinario 2001.xlsx", sheet = "Sheet1")

# Rename columns
data_2001 <- data_2001 %>%
  rename(section = SECCIÓN,
         total = SUMA,
         listanominal = LISTA)

# Collapse (sum) at the municipality and section level
data_2001 <- data_2001 %>%
  group_by(municipality, section) %>%
  summarize(across(c(listanominal, PAN:PVEM, total), sum, na.rm = TRUE))

# Create the uniqueid variable
data_2001 <- data_2001 %>%
  mutate(uniqueid = 17016)

# Calculate turnout
data_2001 <- data_2001 %>%
  mutate(turnout = total / listanominal)

# Calculate the valid votes (sum of selected columns)
data_2001 <- data_2001 %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, UDO, PVEM), na.rm = TRUE))

# Perform aggregation at the uniqueid level and calculate inverse
vars <- c("PAN", "PRI", "PRD", "UDO", "PVEM", "listanominal", "total", "valid")

for (var in vars) {
  data_2001 <- data_2001 %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / (!!sym(paste0("mun_", var))))
}

# Calculate municipality-level turnout
data_2001 <- data_2001 %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank parties within the municipality (similar to rowranks in Stata)
data_2001 <- data_2001 %>%
  mutate(
    PAN_r = dense_rank(desc(inv_mun_PAN)),
    PRI_r = dense_rank(desc(inv_mun_PRI)),
    PRD_r = dense_rank(desc(inv_mun_PRD)),
    UDO_r = dense_rank(desc(inv_mun_UDO)),
    PVEM_r = dense_rank(desc(inv_mun_PVEM))
  )

# Determine winner
data_2001 <- data_2001 %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    UDO_r == 1 ~ "UDO",
    PVEM_r == 1 ~ "PVEM",
    TRUE ~ NA_character_
  ))

# Drop ranking variables
data_2001 <- data_2001 %>%
  select(-ends_with("_r"))

# Add 'year' and 'month' variables
data_2001 <- data_2001 %>%
  mutate(year = 2001,
         month = "January")

# Save the final dataset as a .dta file
write_dta(data_2001, "Morelos_Section_2001.dta")

# Read the CSV file
data_2003 <- read.csv("Ayu_Seccion_2003_No_LN.csv")

# Rename columns
data_2003 <- data_2003 %>%
  rename(municipality = municipio,
         section = seccin)

# Remove rows with missing municipality or section
data_2003 <- data_2003 %>%
  filter(!(is.na(municipality) | is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

# Convert columns from string to numeric (equivalent to destring in Stata)
data_2003 <- data_2003 %>%
  mutate(across(pan:total, as.numeric))

# Collapse (sum) the variables at the municipality and section level
data_2003 <- data_2003 %>%
  group_by(municipality, section) %>%
  summarize(across(pan:fc, sum, na.rm = TRUE),
            total = sum(total, na.rm = TRUE))

# Rename columns
data_2003 <- data_2003 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         UDM = udemor,
         PVEM = pvem,
         PC = c,
         PSN = psn,
         PAS = pas,
         PMP = mp,
         PLM = plm,
         PFC = fc)

# Create uniqueid based on municipality
data_2003 <- data_2003 %>%
  mutate(uniqueid = case_when(
    municipality == "AMACUZAC" ~ 17001,
    municipality == "ATLATLAHUCAN" ~ 17002,
    municipality == "AXOCHIAPAN" ~ 17003,
    municipality == "AYALA" ~ 17004,
    municipality == "COATLAN DEL RIO" ~ 17005,
    municipality == "CUAUTLA" ~ 17006,
    municipality == "CUERNAVACA" ~ 17007,
    municipality == "EMILIANO ZAPATA" ~ 17008,
    municipality == "HUITZILAC" ~ 17009,
    municipality == "JANTETELCO" ~ 17010,
    municipality == "JIUTEPEC" ~ 17011,
    municipality == "JOJUTLA" ~ 17012,
    municipality == "JONACATEPEC" ~ 17013,
    municipality == "MAZATEPEC" ~ 17014,
    municipality == "MIACATLAN" ~ 17015,
    municipality == "OCUITUCO" ~ 17016,
    municipality == "PUENTE DE IXTLA" ~ 17017,
    municipality == "TEMIXCO" ~ 17018,
    municipality == "TEMOAC" ~ 17033,
    municipality == "TEPALCINGO" ~ 17019,
    municipality == "TEPOZTLAN" ~ 17020,
    municipality == "TETECALA" ~ 17021,
    municipality == "TETELA DEL VOLCAN" ~ 17022,
    municipality == "TLALNEPANTLA" ~ 17023,
    municipality == "TLALTIZAPAN" ~ 17024,
    municipality == "TLAQUILTENANGO" ~ 17025,
    municipality == "TLAYACAPAN" ~ 17026,
    municipality == "TOTOLAPAN" ~ 17027,
    municipality == "XOCHITEPEC" ~ 17028,
    municipality == "YAUTEPEC" ~ 17029,
    municipality == "YECAPIXTLA" ~ 17030,
    municipality == "ZACATEPEC" ~ 17031,
    municipality == "ZACUALPAN DE AMILPAS" ~ 17032,
    TRUE ~ 0
  ))

# Calculate the valid vote totals
data_2003 <- data_2003 %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, UDM, PVEM, PC, PSN, PAS, PMP, PLM, PFC), na.rm = TRUE))

# Perform aggregation at the uniqueid level and calculate the inverse for each variable
vars <- c("PAN", "PRI", "PRD", "UDM", "PVEM", "PC", "PSN", "PAS", "PMP", "PLM", "PFC", "total", "valid")

for (var in vars) {
  data_2003 <- data_2003 %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / (!!sym(paste0("mun_", var))))
}

# Generate turnout variable
data_2003 <- data_2003 %>%
  mutate(turnout = total / listanominal)

# Calculate municipal turnout
data_2003 <- data_2003 %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse variables for each party
data_2003 <- data_2003 %>%
  mutate(
    PAN_r = dense_rank(desc(inv_mun_PAN)),
    PRI_r = dense_rank(desc(inv_mun_PRI)),
    PRD_r = dense_rank(desc(inv_mun_PRD)),
    UDM_r = dense_rank(desc(inv_mun_UDM)),
    PVEM_r = dense_rank(desc(inv_mun_PVEM)),
    PC_r = dense_rank(desc(inv_mun_PC)),
    PSN_r = dense_rank(desc(inv_mun_PSN)),
    PAS_r = dense_rank(desc(inv_mun_PAS)),
    PMP_r = dense_rank(desc(inv_mun_PMP)),
    PLM_r = dense_rank(desc(inv_mun_PLM)),
    PFC_r = dense_rank(desc(inv_mun_PFC))
  )

# Determine the winner based on the rankings
data_2003 <- data_2003 %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    UDM_r == 1 ~ "UDM",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PSN_r == 1 ~ "PSN",
    PAS_r == 1 ~ "PAS",
    PMP_r == 1 ~ "PMP",
    PLM_r == 1 ~ "PLM",
    PFC_r == 1 ~ "PFC",
    TRUE ~ NA_character_
  ))

# Remove ranking variables
data_2003 <- data_2003 %>%
  select(-ends_with("_r"))

# Add the year and month variables
data_2003 <- data_2003 %>%
  mutate(year = 2003,
         month = "July")

# Sort by section
data_2003 <- data_2003 %>%
  arrange(section)

# Save the dataset as a .dta file
write_dta(data_2003, "Morelos_Section_2003.dta")

# Load CSV file
data_2006 <- read_csv("Ayu_Seccion_2006.csv")

# Rename columns
data_2006 <- data_2006 %>%
  rename(municipality = municipio,
         section = seccion)

# Drop rows where municipality or section is missing
data_2006 <- data_2006 %>%
  filter(!(is.na(municipality) | municipality == "") &
           !(is.na(section) | section == ""))

# Generate 'total' column by summing across selected columns (similar to rowtotal in Stata)
data_2006 <- data_2006 %>%
  mutate(total = rowSums(select(., pan, pri, prdptpc, pvem, pna, alternativa, cc, noregistrados, nulos), na.rm = TRUE))

# Drop rows where total is missing or zero
data_2006 <- data_2006 %>%
  filter(!(is.na(total) | total == 0))

# Convert relevant columns from character to numeric
data_2006 <- data_2006 %>%
  mutate(across(listanominal:total, as.numeric))

# Operations for 'CUAUTLA'
data_2006 <- data_2006 %>%
  mutate(pvem_pna = ifelse(municipality == "CUAUTLA", cc + pvem + pna, 0),
         pvem = ifelse(municipality == "CUAUTLA", 0, pvem),
         pna = ifelse(municipality == "CUAUTLA", 0, pna),
         cc = ifelse(municipality == "CUAUTLA", 0, cc))

# Operations for 'JIUTEPEC', 'PUENTE DE IXTLA', 'TOTOLAPAN', and 'ZACUALPAN'
data_2006 <- data_2006 %>%
  mutate(pri_pvem = ifelse(municipality %in% c("JIUTEPEC", "PUENTE DE IXTLA", "TOTOLAPAN", "ZACUALPAN"),
                           pri + pvem + cc, 0),
         pri = ifelse(municipality %in% c("JIUTEPEC", "PUENTE DE IXTLA", "TOTOLAPAN", "ZACUALPAN"), 0, pri),
         pvem = ifelse(municipality %in% c("JIUTEPEC", "PUENTE DE IXTLA", "TOTOLAPAN", "ZACUALPAN"), 0, pvem),
         cc = ifelse(municipality %in% c("JIUTEPEC", "PUENTE DE IXTLA", "TOTOLAPAN", "ZACUALPAN"), 0, cc))

# Drop 'cc' column
data_2006 <- data_2006 %>%
  select(-cc)

# Rename columns
data_2006 <- data_2006 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD_PT_PC = prdptpc,
         PVEM = pvem,
         PANAL = pna,
         PAS = alternativa,
         PVEM_PANAL = pvem_pna,
         PRI_PVEM = pri_pvem)

# Create 'turnout' variable
data_2006 <- data_2006 %>%
  mutate(turnout = total / listanominal)

# Generate 'uniqueid' based on municipality
data_2006 <- data_2006 %>%
  mutate(uniqueid = case_when(
    municipality == "AMACUZAC" ~ 17001,
    municipality == "ATLATLAHUCAN" ~ 17002,
    municipality == "AXOCHIAPAN" ~ 17003,
    municipality == "AYALA" ~ 17004,
    municipality == "COATLAN DEL RIO" ~ 17005,
    municipality == "CUAUTLA" ~ 17006,
    municipality == "CUERNAVACA" ~ 17007,
    municipality == "EMILIANO ZAPATA" ~ 17008,
    municipality == "HUITZILAC" ~ 17009,
    municipality == "JANTETELCO" ~ 17010,
    municipality == "JIUTEPEC" ~ 17011,
    municipality == "JOJUTLA" ~ 17012,
    municipality == "JONACATEPEC" ~ 17013,
    municipality == "MAZATEPEC" ~ 17014,
    municipality == "MIACATLAN" ~ 17015,
    municipality == "OCUITUCO" ~ 17016,
    municipality == "PUENTE DE IXTLA" ~ 17017,
    municipality == "TEMIXCO" ~ 17018,
    municipality == "TEMOAC" ~ 17033,
    municipality == "TEPALCINGO" ~ 17019,
    municipality == "TEPOZTLAN" ~ 17020,
    municipality == "TETECALA" ~ 17021,
    municipality == "TETELA DEL VOLCAN" ~ 17022,
    municipality == "TLALNEPANTLA" ~ 17023,
    municipality == "TLALTIZAPAN" ~ 17024,
    municipality == "TLAQUILTENANGO" ~ 17025,
    municipality == "TLAYACAPAN" ~ 17026,
    municipality == "TOTOLAPAN" ~ 17027,
    municipality == "XOCHITEPEC" ~ 17028,
    municipality == "YAUTEPEC" ~ 17029,
    municipality == "YECAPIXTLA" ~ 17030,
    municipality == "ZACATEPEC" ~ 17031,
    municipality == "ZACUALPAN" ~ 17032))

# Generate 'valid' votes by summing across columns
data_2006 <- data_2006 %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD_PT_PC, PVEM, PANAL, PAS, PVEM_PANAL, PRI_PVEM), na.rm = TRUE))

# Collapse and calculate municipal totals, inverse, and other steps
variables <- c("PAN", "PRI", "PRD_PT_PC", "PVEM", "PANAL", "PAS", "PVEM_PANAL", "PRI_PVEM", "total", "listanominal", "valid")

data_2006 <- data_2006 %>%
  group_by(uniqueid) %>%
  summarize(across(all_of(variables), sum, na.rm = TRUE)) %>%
  mutate(across(all_of(variables), ~ 1 / .x, .names = "inv_mun_{col}"))

# Generate municipal turnout
data_2006 <- data_2006 %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Ranking and assigning winner
rank_vars <- c("inv_mun_PAN", "inv_mun_PRI", "inv_mun_PRD_PT_PC", "inv_mun_PVEM", "inv_mun_PANAL", "inv_mun_PAS", "inv_mun_PVEM_PANAL", "inv_mun_PRI_PVEM")

data_2006 <- data_2006 %>%
  mutate(across(all_of(rank_vars), rank, .names = "{col}_r"))

# Assigning winner
data_2006 <- data_2006 %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_PT_PC_r == 1 ~ "PRD_PT_PC",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PANAL_r == 1 ~ "PANAL",
    inv_mun_PAS_r == 1 ~ "PAS",
    inv_mun_PVEM_PANAL_r == 1 ~ "PVEM_PANAL",
    inv_mun_PRI_PVEM_r == 1 ~ "PRI_PVEM"))

# Final variables
data_2006 <- data_2006 %>%
  mutate(year = 2006,
         month = "July")

# Sort by section and save
data_2006 <- data_2006 %>%
  arrange(section)

write_dta(data_2006, "Morelos_Section_2006.dta")

# Load CSV file
data_2009 <- read_csv("Ayu_Seccion_2009.csv")

# Rename columns
data_2009 <- data_2009 %>%
  rename(municipality = nombre_municipio,
         section = seccion)

# Drop rows where municipality or section is missing
data_2009 <- data_2009 %>%
  filter(!(is.na(municipality) | municipality == "") &
           !(is.na(section) | section == ""))

# Drop rows where 'total' is missing or zero
data_2009 <- data_2009 %>%
  filter(!(is.na(total) | total == 0))

# Convert relevant columns from character to numeric
data_2009 <- data_2009 %>%
  mutate(across(listanominal:nulos, as.numeric),
         total = as.numeric(total))

# Collapse (aggregate) the data by municipality and section, summing over listanominal, pan, and total
data_2009 <- data_2009 %>%
  group_by(municipality, section) %>%
  summarize(across(c(listanominal, pan:total), sum, na.rm = TRUE))

# View the resulting data
print(data_2009)

# Create dummy variables based on municipality conditions
data_2009 <- data_2009 %>%
  mutate(dummy_pri_pc = ifelse(municipality == "OCUITUCO", 1, 0),
         dummy_pan_psd = ifelse(municipality == "TETELA DEL VOLCAN", 1, 0),
         dummy_prd_pt = ifelse(municipality %in% c("CUERNAVACA", "XOCHITEPEC"), 1, 0),
         dummy_prd_pc = ifelse(municipality %in% c("AXOCHIAPAN", "JANTETELCO", "JONACATEPEC", "TEPALCINGO", "TEPOZTLAN"), 1, 0),
         dummy_prd_pt_pc = ifelse(municipality == "JIUTEPEC", 1, 0))

# Replace and drop variables according to conditions
data_2009 <- data_2009 %>%
  mutate(panpsd = ifelse(dummy_pan_psd == 1, panpsd + pan + psd, panpsd),
         pan = ifelse(dummy_pan_psd == 1, 0, pan),
         psd = ifelse(dummy_pan_psd == 1, 0, psd)) %>%
  select(-dummy_pan_psd) %>%
  mutate(pripc = ifelse(dummy_pri_pc == 1, pripc + pri + pc, pripc),
         pri = ifelse(dummy_pri_pc == 1, 0, pri),
         pc = ifelse(dummy_pri_pc == 1, 0, pc)) %>%
  select(-dummy_pri_pc) %>%
  mutate(prdpt = ifelse(dummy_prd_pt == 1, prdpt + prd + pt, prdpt),
         prd = ifelse(dummy_prd_pt == 1, 0, prd),
         pt = ifelse(dummy_prd_pt == 1, 0, pt)) %>%
  select(-dummy_prd_pt) %>%
  mutate(prdpc = ifelse(dummy_prd_pc == 1, prdpc + prd + pc, prdpc),
         prd = ifelse(dummy_prd_pc == 1, 0, prd),
         pc = ifelse(dummy_prd_pc == 1, 0, pc)) %>%
  select(-dummy_prd_pc) %>%
  mutate(prdptpc = ifelse(dummy_prd_pt_pc == 1, prdptpc + prd + pt + pc, prdptpc),
         prd = ifelse(dummy_prd_pt_pc == 1, 0, prd),
         pt = ifelse(dummy_prd_pt_pc == 1, 0, pt),
         pc = ifelse(dummy_prd_pt_pc == 1, 0, pc)) %>%
  select(-dummy_prd_pt_pc)

# Rename variables
data_2009 <- data_2009 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc, PANAL = panal, PSD = psd,
         PAN_PSD = panpsd, PRD_PC = prdpc, PRD_PT = prdpt, PRD_PT_PC = prdptpc, PRI_PC = pripc)

# Generate turnout
data_2009 <- data_2009 %>%
  mutate(turnout = total / listanominal)

# Generate uniqueid based on municipality
uniqueid_map <- c("AMACUZAC" = 17001, "ATLATLAHUCAN" = 17002, "AXOCHIAPAN" = 17003, "AYALA" = 17004, 
                  "COATLAN DEL RIO" = 17005, "CUAUTLA" = 17006, "CUERNAVACA" = 17007, "EMILIANO ZAPATA" = 17008, 
                  "HUITZILAC" = 17009, "JANTETELCO" = 17010, "JIUTEPEC" = 17011, "JOJUTLA" = 17012, 
                  "JONACATEPEC" = 17013, "MAZATEPEC" = 17014, "MIACATLAN" = 17015, "OCUITUCO" = 17016, 
                  "PUENTE DE IXTLA" = 17017, "TEMIXCO" = 17018, "TEMOAC" = 17033, "TEPALCINGO" = 17019, 
                  "TEPOZTLAN" = 17020, "TETECALA" = 17021, "TETELA DEL VOLCAN" = 17022, "TLALNEPANTLA" = 17023, 
                  "TLALTIZAPAN" = 17024, "TLAQUILTENANGO" = 17025, "TLAYACAPAN" = 17026, "TOTOLAPAN" = 17027, 
                  "XOCHITEPEC" = 17028, "YAUTEPEC" = 17029, "YECAPIXTLA" = 17030, "ZACATEPEC" = 17031, 
                  "ZACUALPAN" = 17032)

data_2009 <- data_2009 %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Generate `valid` vote total and compute mun_* and inv_mun_* variables
data_2009 <- data_2009 %>%
  rowwise() %>%
  mutate(valid = sum(PAN, PRI, PRD, PT, PVEM, PC, PANAL, PSD, PAN_PSD, PRI_PC, PRD_PT, PRD_PC, PRD_PT_PC, na.rm = TRUE))

# Summing and inverting for each variable
for (var in c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PANAL", "PSD", "PAN_PSD", "PRI_PC", "PRD_PT", "PRD_PC", "PRD_PT_PC", "total", "listanominal", "valid")) {
  data_2009 <- data_2009 %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE),
           !!paste0("inv_mun_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Compute municipal turnout
data_2009 <- data_2009 %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Compute row ranks
data_2009 <- data_2009 %>%
  mutate(across(starts_with("inv_mun_"), ~rank(-.), .names = "{col}_r"))

# Assign winners based on rankings
data_2009 <- data_2009 %>%
  mutate(winner = case_when(PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", PT_r == 1 ~ "PT",
                            PVEM_r == 1 ~ "PVEM", PC_r == 1 ~ "PC", PANAL_r == 1 ~ "PANAL", PSD_r == 1 ~ "PSD",
                            PAN_PSD_r == 1 ~ "PAN_PSD", PRI_PC_r == 1 ~ "PRI_PC", PRD_PT_r == 1 ~ "PRD_PT", 
                            PRD_PC_r == 1 ~ "PRD_PC", PRD_PT_PC_r == 1 ~ "PRD_PT_PC"))

# Generate year and month columns
data_2009 <- data_2009 %>%
  mutate(year = 2009, month = "July")

# Sort by section
data_2009 <- data_2009 %>%
  arrange(section)

# Save the dataset
write_csv(data_2009, "Morelos_Section_2009.csv")

# Load the dataset for 2012
data_2012 <- read.csv("Ayu_Seccion_2012.csv")

# Clean municipality names and drop empty values
data_2012 <- data_2012 %>%
  mutate(municipality = gsub("\\*", "", municipality)) %>%
  filter(municipality != "")

# Collapse the data by summing relevant columns
data_2012 <- data_2012 %>%
  group_by(municipality, section) %>%
  summarise(across(pan:nulos, sum, .names = "total_{col}"))

# Drop specific columns and rename others
data_2012 <- data_2012 %>%
  select(-c(pri_panal, prd_pt_pc, prd_pc, pvem_panal, pri_pvem)) %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PC = pc, PVEM = pvem, PANAL = panal, PSD = psd)

# Replace missing coalition columns with other data, then rename them
data_2012 <- data_2012 %>%
  mutate(c_pri_panal = ifelse(c_pri_panal == 0 & cc_pri_panal != 0, cc_pri_panal, c_pri_panal)) %>%
  select(-cc_pri_panal) %>%
  rename(PRI_PANAL = c_pri_panal, PRD_PT_PC = c_prd_pt_pc, PRD_PC = c_prd_pc, PVEM_PANAL = c_pvem_panal, PRI_PVEM = c_pri_pvem)

# Compute the total number of votes and drop invalid rows
data_2012 <- data_2012 %>%
  rowwise() %>%
  mutate(total = sum(c_across(PAN:PRI_PVEM), na.rm = TRUE)) %>%
  select(-nulos)

# Generate unique IDs based on the municipality names
uniqueid_map <- c("AMACUZAC" = 17001, "ATLATLAHUCAN" = 17002, "AXOCHIAPAN" = 17003, "AYALA" = 17004, 
                  "COATLAN DEL RIO" = 17005, "CUAUTLA" = 17006, "CUERNAVACA" = 17007, "EMILIANO ZAPATA" = 17008, 
                  "HUITZILAC" = 17009, "JANTETELCO" = 17010, "JIUTEPEC" = 17011, "JOJUTLA" = 17012, 
                  "JONACATEPEC" = 17013, "MAZATEPEC" = 17014, "MIACATLAN" = 17015, "OCUITUCO" = 17016, 
                  "PUENTE DE IXTLA" = 17017, "TEMIXCO" = 17018, "TEMOAC" = 17033, "TEPALCINGO" = 17019, 
                  "TEPOZTLAN" = 17020, "TETECALA" = 17021, "TETELA DEL VOLCAN" = 17022, "TLALNEPANTLA" = 17023, 
                  "TLALTIZAPAN" = 17024, "TLAQUILTENANGO" = 17025, "TLAYACAPAN" = 17026, "TOTOLAPAN" = 17027, 
                  "XOCHITEPEC" = 17028, "YAUTEPEC" = 17029, "YECAPIXTLA" = 17030, "ZACATEPEC" = 17031, 
                  "ZACUALPAN" = 17032)

data_2012 <- data_2012 %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Compute valid votes by summing relevant columns
data_2012 <- data_2012 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRI_PVEM), na.rm = TRUE))

# Calculate the total at the municipal level and inverse values for each column
for (var in c("PAN", "PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "PSD", "PRI_PANAL", "PRD_PT_PC", "PRD_PC", "PVEM_PANAL", "PRI_PVEM", "total", "valid")) {
  data_2012 <- data_2012 %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE),
           !!paste0("inv_mun_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Merge with listanominal data from an external file
listanominal_data <- read.csv("..\\..\\all_months_years.csv")
data_2012 <- merge(data_2012, listanominal_data, by.x = c("ed", "seccion"), by.y = c("ed", "seccion"))

# Keep only the relevant records for July 2012
data_2012 <- data_2012 %>%
  filter(month == 7 & year == 2012 & day == 1) %>%
  select(-c(ed, year, month, day, _merge))

# Calculate turnout
data_2012 <- data_2012 %>%
  mutate(turnout = total / listanominal)

# Perform replacements for coalition adjustments
data_2012 <- data_2012 %>%
  mutate(PRI = ifelse(mun_PRI_PANAL > 0, NA, PRI),
         PANAL = ifelse(mun_PRI_PANAL > 0, NA, PANAL),
         PRD = ifelse(mun_PRD_PT_PC > 0, NA, PRD),
         PT = ifelse(mun_PRD_PT_PC > 0, NA, PT),
         PC = ifelse(mun_PRD_PT_PC > 0, NA, PC),
         PRD = ifelse(mun_PRD_PC > 0, NA, PRD),
         PC = ifelse(mun_PRD_PC > 0, NA, PC),
         PVEM = ifelse(mun_PVEM_PANAL > 0, NA, PVEM),
         PANAL = ifelse(mun_PVEM_PANAL > 0, NA, PANAL),
         PRI = ifelse(mun_PRI_PVEM > 0, NA, PRI),
         PVEM = ifelse(mun_PRI_PVEM > 0, NA, PVEM))

# Replace inversed values accordingly
data_2012 <- data_2012 %>%
  mutate(inv_mun_PRI = ifelse(mun_PRI_PANAL > 0, NA, inv_mun_PRI),
         inv_mun_PANAL = ifelse(mun_PRI_PANAL > 0, NA, inv_mun_PANAL),
         inv_mun_PRD = ifelse(mun_PRD_PT_PC > 0, NA, inv_mun_PRD),
         inv_mun_PT = ifelse(mun_PRD_PT_PC > 0, NA, inv_mun_PT),
         inv_mun_PC = ifelse(mun_PRD_PT_PC > 0, NA, inv_mun_PC),
         inv_mun_PRD = ifelse(mun_PRD_PC > 0, NA, inv_mun_PRD),
         inv_mun_PC = ifelse(mun_PRD_PC > 0, NA, inv_mun_PC),
         inv_mun_PVEM = ifelse(mun_PVEM_PANAL > 0, NA, inv_mun_PVEM),
         inv_mun_PANAL = ifelse(mun_PVEM_PANAL > 0, NA, inv_mun_PANAL),
         inv_mun_PRI = ifelse(mun_PRI_PVEM > 0, NA, inv_mun_PRI),
         inv_mun_PVEM = ifelse(mun_PRI_PVEM > 0, NA, inv_mun_PVEM))

# Compute row ranks based on inverse municipal vote shares
data_2012 <- data_2012 %>%
  mutate(across(starts_with("inv_mun_"), ~rank(-.), .names = "{col}_r"))

# Assign winners based on rankings
data_2012 <- data_2012 %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", PT_r == 1 ~ "PT", PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC", PANAL_r == 1 ~ "PANAL", PSD_r == 1 ~ "PSD", PRI_PANAL_r == 1 ~ "PRI_PANAL", 
    PRD_PT_PC_r == 1 ~ "PRD_PT_PC", PRD_PC_r == 1 ~ "PRD_PC", PVEM_PANAL_r == 1 ~ "PVEM_PANAL", 
    PRI_PVEM_r == 1 ~ "PRI_PVEM"))

# Add year and month
data_2012 <- data_2012 %>%
  mutate(year = 2012, month = "July")

# Save final 2012 data
write.csv(data_2012, "Morelos_Section_2012.csv", row.names = FALSE)

# Append all years data
data_all <- bind_rows(
  read.csv("Morelos_Section_1997.csv"),
  read.csv("Morelos_Section_2000.csv"),
  read.csv("Morelos_Section_2001.csv"),
  read.csv("Morelos_Section_2003.csv"),
  read.csv("Morelos_Section_2006.csv"),
  read.csv("Morelos_Section_2009.csv"),
  data_2012
)

# Save combined data
write.csv(data_all, "Morelos_ALL.csv", row.names = FALSE)

# Clean up individual files
file.remove("Morelos_Section_1997.csv", "Morelos_Section_2000.csv", "Morelos_Section_2001.csv", 
            "Morelos_Section_2003.csv", "Morelos_Section_2006.csv", "Morelos_Section_2009.csv", "Morelos_Section_2012.csv")

# Analyze winners
data_all <- data_all %>%
  mutate(PAN_winner = ifelse(grepl("PAN", winner) & !grepl("PANAL", winner), 1, 0),
         winner_counter = PAN_winner)

# Iterate through each party to count their winners
for (var in c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "PSD")) {
  data_all <- data_all %>%
    mutate(!!paste0(var, "_winner") := ifelse(grepl(var, winner), 1, 0)) %>%
    mutate(winner_counter = winner_counter + !!sym(paste0(var, "_winner")))
}

# Count how many entries have no winner
no_winner <- nrow(filter(data_all, winner_counter == 0))

# Save final result
write.csv(data_all, "Morelos_ALL.csv", row.names = FALSE)

# Load the Excel file
data_2015 <- read_excel("Ayuntamientos_2015.xlsx")

# Replace PRI, PVEM, PANAL with NA where C_PRI_PVEM_PANAL is not NA
data_2015 <- data_2015 %>%
  mutate(PRI = ifelse(!is.na(C_PRI_PVEM_PANAL), NA, PRI),
         PVEM = ifelse(!is.na(C_PRI_PVEM_PANAL), NA, PVEM),
         PANAL = ifelse(!is.na(C_PRI_PVEM_PANAL), NA, PANAL),
         PRI_PVEM_PANAL = C_PRI_PVEM_PANAL) %>%
  select(-C_PRI_PVEM_PANAL)

# Replace PRD, PT with NA where C_PRD_PT is not NA and rename the column
data_2015 <- data_2015 %>%
  mutate(PRD = ifelse(!is.na(C_PRD_PT), NA, PRD),
         PT = ifelse(!is.na(C_PRD_PT), NA, PT),
         PRD_PT = C_PRD_PT) %>%
  select(-C_PRD_PT)

# Collapse data (sum) by municipality, section, uniqueid
data_2015 <- data_2015 %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(PAN:CI_1, sum, na.rm = TRUE),
            total = sum(total, na.rm = TRUE),
            listanominal = sum(listanominal, na.rm = TRUE)) %>%
  ungroup()

# Generate valid votes by summing relevant columns
data_2015 <- data_2015 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:CI_1), na.rm = TRUE)) %>%
  ungroup()

# Generate municipal total and inverse values for each variable
for (var in c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "PSD", "MORENA", "PES", "PH", "PRD_PT", "PRI_PVEM_PANAL", "CI_1", "total", "valid", "listanominal")) {
  data_2015 <- data_2015 %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE),
           !!paste0("i_", var) := 1 / !!sym(paste0("mun_", var))) %>%
    ungroup()
}

# Calculate turnout and municipal turnout
data_2015 <- data_2015 %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / mun_listanominal)

# Row ranks
data_2015 <- data_2015 %>%
  mutate(PAN_r = rank(-i_PAN), PRI_r = rank(-i_PRI), PRD_r = rank(-i_PRD), PT_r = rank(-i_PT), PVEM_r = rank(-i_PVEM),
         MC_r = rank(-i_MC), PANAL_r = rank(-i_PANAL), PSD_r = rank(-i_PSD), MORENA_r = rank(-i_MORENA),
         PES_r = rank(-i_PES), PH_r = rank(-i_PH), PRD_PT_r = rank(-i_PRD_PT), PRI_PVEM_PANAL_r = rank(-i_PRI_PVEM_PANAL), CI_1_r = rank(-i_CI_1)) %>%
  select(-starts_with("i_"))

# Generate winner, second, and third based on ranks
data_2015 <- data_2015 %>%
  rowwise() %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", PT_r == 1 ~ "PT", PVEM_r == 1 ~ "PVEM", MC_r == 1 ~ "MC",
    PANAL_r == 1 ~ "PANAL", PSD_r == 1 ~ "PSD", MORENA_r == 1 ~ "MORENA", PES_r == 1 ~ "PES", PH_r == 1 ~ "PH",
    PRD_PT_r == 1 ~ "PRD_PT", PRI_PVEM_PANAL_r == 1 ~ "PRI_PVEM_PANAL", CI_1_r == 1 ~ "Independent"),
    second = case_when(
      PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", PRD_r == 2 ~ "PRD", PT_r == 2 ~ "PT", PVEM_r == 2 ~ "PVEM", MC_r == 2 ~ "MC",
      PANAL_r == 2 ~ "PANAL", PSD_r == 2 ~ "PSD", MORENA_r == 2 ~ "MORENA", PES_r == 2 ~ "PES", PH_r == 2 ~ "PH",
      PRD_PT_r == 2 ~ "PRD_PT", PRI_PVEM_PANAL_r == 2 ~ "PRI_PVEM_PANAL", CI_1_r == 2 ~ "Independent"),
    third = case_when(
      PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", PRD_r == 3 ~ "PRD", PT_r == 3 ~ "PT", PVEM_r == 3 ~ "PVEM", MC_r == 3 ~ "MC",
      PANAL_r == 3 ~ "PANAL", PSD_r == 3 ~ "PSD", MORENA_r == 3 ~ "MORENA", PES_r == 3 ~ "PES", PH_r == 3 ~ "PH",
      PRD_PT_r == 3 ~ "PRD_PT", PRI_PVEM_PANAL_r == 3 ~ "PRI_PVEM_PANAL", CI_1_r == 3 ~ "Independent")) %>%
  ungroup()

# Assign fixed values for year, month, and state
data_2015 <- data_2015 %>%
  mutate(year = 2015, month = "June", STATE = "MORELOS")

# Save section-level dataset
write.csv(data_2015, "Morelos_Section_2015.csv", row.names = FALSE)

# Collapse by municipality and uniqueid to generate incumbents dataset
incumbents_2018 <- data_2015 %>%
  group_by(municipality, uniqueid) %>%
  summarise(incumbent = first(winner))

# Save incumbents dataset for 2018
write.csv(incumbents_2018, "incumbents2018.csv", row.names = FALSE)

# Step 1: Load the data from Morelos_Section_2015.dta
data <- read_dta("Morelos_Section_2015.dta")

# Step 2: Collapse (sum) PAN-valid, and keep the first of some variables (STATE, year, month, winner, second, third, mun_turnout)
collapsed_data <- data %>%
  group_by(municipality, uniqueid) %>%
  summarise(
    across(c(PAN:valid), sum, na.rm = TRUE),
    STATE = first(STATE),
    year = first(year),
    month = first(month),
    winner = first(winner),
    second = first(second),
    third = first(third),
    mun_turnout = first(mun_turnout)
  )

# Step 3: Rename mun_turnout to turnout
collapsed_data <- collapsed_data %>%
  rename(turnout = mun_turnout)

# Step 4: Sort by uniqueid
collapsed_data <- collapsed_data %>%
  arrange(uniqueid)

# Step 5: Reorder columns (place STATE, municipality, and uniqueid first)
collapsed_data <- collapsed_data %>%
  select(STATE, municipality, uniqueid, everything())

# Step 6: Save the collapsed data to the specified location
write_dta(collapsed_data, "../../Update Municipal/Morelos_2015.dta")

# List of sheet names to iterate through
sheet_names <- excel_sheets("Ayuntamientos_2018.xlsx")

# Step 1: Import and process each sheet
data_list <- list()
for (sheetname in sheet_names) {
  # Read each sheet and handle missing values
  data <- read_excel("Ayuntamientos_2018.xlsx", sheet = sheetname) %>%
    mutate(across(everything(), ~replace_na(.x, "0"))) %>%
    filter(!is.na(uniqueid))  # Drop rows where uniqueid is empty
  data_list[[sheetname]] <- data
}

# Combine all sheets into one dataset
combined_data <- bind_rows(data_list)

# Rename columns and drop unnecessary ones
combined_data <- combined_data %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PVEM = pvem, PT = pt, MC = mc, 
         PANAL = panal, PSD = psd, MORENA = morena, PES = pes, PH = humanista,
         no_reg = no_registrados, nulo = num_votos_nulos, 
         total = TotaldeVotos, listanominal = ListaNominal) %>%
  select(-Z, -AA, -AB, -AC, -AF, -Y)

# Set the path to your Excel file
file_path <- "Ayuntamientos_2018.xlsx"

# Step 1: Get the sheet names from the Excel file
sheets <- excel_sheets(file_path)

# Step 2: Loop over each sheet, import, clean, and save as .dta file
for (sheet_name in sheets) {
  # Import the current sheet, treating all columns as character type (similar to "allstring" in Stata)
  data <- read_excel(file_path, sheet = sheet_name, col_types = "text")
  
  # Drop rows where 'uniqueid' is missing or blank
  data <- data %>%
    filter(!is.na(uniqueid) & uniqueid != "")
  
  # Replace all empty values with "0"
  data <- data %>%
    mutate(across(everything(), ~ replace(., . == "", "0")))
  
  # Save the data frame as a .dta file
  write_dta(data, paste0("reporte_", sheet_name, ".dta"))
}

# Step 3: Append all saved .dta files
# List all the saved .dta files (reporte_*.dta)
dta_files <- list.files(pattern = "reporte_.*\\.dta$")

# Initialize an empty dataframe to store the combined data
combined_data <- data.frame()

# Loop through each .dta file and append it to the combined dataframe
for (file in dta_files) {
  temp_data <- read_dta(file)
  combined_data <- bind_rows(combined_data, temp_data)
}

# Step 4: Save the final appended data into a .dta file
write_dta(combined_data, "combined_report_2018.dta")

# Step 1: Clean and Process Data
data <- read_dta("combined_report_2018.dta")

# Destring all columns (convert from character to numeric if possible)
data <- data %>% mutate(across(everything(), as.numeric))

# Rename columns
colnames(data) <- gsub("^votos", "", colnames(data))
data <- data %>% rename(
  PAN = pan,
  PRI = pri,
  PRD = prd,
  PVEM = pvem,
  PT = pt,
  MC = mc,
  PANAL = na,
  PSD = psd,
  MORENA = morena,
  PES = es,
  PH = humanista,
  no_reg = no_registrados,
  nulo = num_votos_nulos,
  total = TotaldeVotos,
  listanominal = ListaNominal
)

# Replacing coalition votes and adjusting
data <- data %>%
  mutate(
    PRD_PVEM_PSD = ifelse(cc_prd_pvem_psd != 0, cc_prd_pvem_psd + PRD + PSD + PVEM + config_cc_prd_pvem + config_cc_prd_psd + config_cc_pvem_psd, NA),
    PT_MORENA_PES = ifelse(cc_pt_morena_pes != 0, cc_pt_morena_pes + config_cc_pt_morena + config_cc_pt_pes + config_cc_morena_pes + MORENA + PT + PES, NA),
    PAN_MC = ifelse(cc_pan_mc != 0, cc_pan_mc + PAN + MC, NA),
    PRD_PSD = ifelse(coal_prd_psd != 0, coal_prd_psd + PRD + PSD, NA)
  ) %>%
  mutate_at(vars(PRD, PSD, PVEM, MORENA, PT, PES, PAN, MC), ~ifelse(. %in% PRD_PVEM_PSD, NA, .)) %>%
  mutate_at(vars(cc_pan_mc, coal_prd_psd, cc_pt_morena_pes, cc_prd_pvem_psd), ~NA) %>%
  drop_na(cc_pan_mc, coal_prd_psd)

# Collapse data
collapsed_data <- data %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(starts_with("PAN"):listanominal, sum))

# Add turnout and other variables
collapsed_data <- collapsed_data %>%
  mutate(
    valid = rowSums(across(PAN:CI_3)),
    turnout = total / listanominal
  )

# Calculate ranks
collapsed_data <- collapsed_data %>%
  rowwise() %>%
  mutate(across(starts_with("i_"), ~1 / mun_))

# Generate winner, second, and third place
collapsed_data <- collapsed_data %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_r == 1 ~ "PRI",
      PRD_r == 1 ~ "PRD",
      TRUE ~ winner
    ),
    second = case_when(
      PAN_r == 2 ~ "PAN",
      PRI_r == 2 ~ "PRI",
      PRD_r == 2 ~ "PRD",
      TRUE ~ second
    ),
    third = case_when(
      PAN_r == 3 ~ "PAN",
      PRI_r == 3 ~ "PRI",
      PRD_r == 3 ~ "PRD",
      TRUE ~ third
    )
  )

# Handle Independent Candidates
collapsed_data <- collapsed_data %>%
  mutate(
    winner = ifelse(winner %in% c("CI_1", "CI_2", "CI_3"), "Independent", winner),
    second = ifelse(second %in% c("CI_1", "CI_2", "CI_3"), "Independent", second),
    third = ifelse(third %in% c("CI_1", "CI_2", "CI_3"), "Independent", third)
  )

# Save the data
write_dta(collapsed_data, "Morelos_Section_2018.dta")

# Step 2: Merge with incumbents data
incumbents <- read_dta("incumbents2018.dta")
final_data <- merge(collapsed_data, incumbents, by = "uniqueid", all.x = TRUE)

# Handle `incumbent_vote`
final_data <- final_data %>%
  mutate(
    incumbent_vote = case_when(
      incumbent == "MC" & coalpanmc == 0 ~ "MC",
      incumbent == "PAN" & coalpanmc == 0 ~ "PAN",
      TRUE ~ incumbent_vote
    )
  )

# Save the final data
write_dta(final_data, "Morelos_Section_2018.dta")

# Step 3: Append 2018 and 2015 datasets
data_2015 <- read_dta("Morelos_Section_2015.dta")
final_combined <- bind_rows(final_data, data_2015)

# Save the combined data
write_dta(final_combined, "Morelos_Section_15_18.dta")

# Step 4: Append the combined data to another existing dataset
precinct_data <- read_dta("Morelos_ALL.dta")
final_precinct_data <- bind_rows(precinct_data, final_combined)

# Save the updated precinct data
write_dta(final_precinct_data, "Morelos_ALL_SALVADOR.dta")

# Clean up temporary files
file.remove(list.files(pattern = "reporte \\(.*\\.dta$"))

# Erase unnecessary data
file.remove("Morelos_Section_15_18.dta")

