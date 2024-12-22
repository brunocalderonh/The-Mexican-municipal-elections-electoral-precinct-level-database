# Set working directory (handles both paths)
dir_1 <- ("~/Downloads/Horacio-BRUNO/States/Precinct/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018")

if (dir.exists(dir_1)) {
  setwd(dir_1)
} else if (dir.exists(dir_2)) {
  setwd(dir_2)
}

# Load necessary libraries
library(dplyr)    # For data manipulation
library(readr)    # For reading CSVs
library(stringr)  # For string operations
library(haven)

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 1995
# -------------------------------------- #
data <- data.table::fread("Ayu_Seccion_1995_No_LN.csv")
names(data)
# Rename variables (equivalent to Stata's `rename`)
data <- data %>%
  dplyr::rename(
    municipality = Municipio,
    section = "Secci\xf3n")

# Drop rows where `municipality` is empty and `section` is NA (equivalent to Stata's `drop`)
data <- data %>%
  dplyr::filter(!(municipality == "" & is.na(section)))

# Convert columns `pan` to `nulos` to numeric (equivalent to Stata's `destring`)
data <- data %>%
  dplyr::mutate(across(PAN:Nulos, as.numeric))

# Collapse (sum) values by `municipality` and `section` (equivalent to Stata's `collapse`)
collapsed_data <- data %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(PAN:Nulos, sum, na.rm = TRUE))

all_months_years <- read_dta("../all_months_years.dta") # Adjust as needed
names(all_months_years)

collapsed_data <- collapsed_data %>%
  left_join(all_months_years %>%
              filter(state == "MICHOACAN" & 
                       month == "March" &
                       year == 1998) %>%
              select(section, lista),
            by = "section") %>% 
  dplyr::rename(listanominal = lista)

# Generate a new variable `total` (equivalent to Stata's `gen`)
collapsed_data <- collapsed_data %>%
  dplyr::mutate(total = PAN + PRI + PRD + PFCRN + PT + NoReg + Nulos) %>% 
  dplyr::filter(!is.na(municipality), section != 0, total > 0)

# Rename variables (equivalent to Stata's `rename`)
collapsed_data <- collapsed_data %>%
  dplyr::rename(
    PartCardenista = PFCRN)

# Generate turnout rate (equivalent to `gen turnout = total/listanominal`)
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal)

# Drop `noreg`
collapsed_data <- collapsed_data %>%
  dplyr::select(-NoReg)

# Generate uniqueid and assign values based on the municipality name (equivalent to `gen uniqueid=0`)
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = case_when(
    municipality == "ACUITZIO" ~ 16001,
    municipality == "AGUILILLA" ~ 16002,
    municipality == "A. OBREGON." ~ 16003,
    municipality == "ANGAMACUTIRO" ~ 16004,
    municipality == "ANGANGUEO" ~ 16005,
    municipality == "APATZINGAN" ~ 16006,
    municipality == "APORO" ~ 16007,
    municipality == "AQUILA" ~ 16008,
    municipality == "ARIO DE R." ~ 16009,
    municipality == "ARTEAGA" ~ 16010,
    municipality == "BRISEAS" | str_detect(municipality, "BRISE") ~ 16011,
    municipality == "BUENAVISTA" ~ 16012,
    municipality == "CARACUARO" ~ 16013,
    municipality == "CHARAPAN" ~ 16021,
    municipality == "CHARO" ~ 16022,
    municipality == "CHAVINDA" ~ 16023,
    municipality == "CHERAN" ~ 16024,
    municipality == "CHILCHOTA" ~ 16025,
    municipality == "CHINICUILA" ~ 16026,
    municipality == "CHUCANDIRO" ~ 16027,
    municipality == "CHURINTZIO" ~ 16028,
    municipality == "CHURUMUCO" ~ 16029,
    municipality == "COAHUAYANA" ~ 16014,
    municipality == "COALCOMAN" ~ 16015,
    municipality == "COENEO" ~ 16016,
    municipality == "REGULES" ~ 16074,
    municipality == "CONTEPEC" ~ 16017,
    municipality == "COPANDARO" ~ 16018,
    municipality == "COTIJA" ~ 16019,
    municipality == "CUITZEO" ~ 16020,
    municipality == "ECUANDUREO" ~ 16030,
    municipality == "E. HUERTA" ~ 16031,
    municipality == "ERONGARI." ~ 16032,
    municipality == "GABRIEL ZAMORA" ~ 16033,
    municipality == "HIDALGO" ~ 16034,
    municipality == "HUANDACAREO" ~ 16036,
    municipality == "HUANIQUEO" ~ 16037,
    municipality == "HUETAMO" ~ 16038,
    municipality == "HUIRAMBA" ~ 16039,
    municipality == "INDAPARAPEO" ~ 16040,
    municipality == "IRIMBO" ~ 16041,
    municipality == "IXTLAN" ~ 16042,
    municipality == "JACONA" ~ 16043,
    municipality == "JIMENEZ" ~ 16044,
    municipality == "JIQUILPAN" ~ 16045,
    municipality == "JUAREZ" ~ 16046,
    municipality == "JUNGAPEO" ~ 16047,
    municipality == "LA HUACANA" ~ 16035,
    municipality == "LA PIEDAD" ~ 16069,
    municipality == "LAGUNILLAS" ~ 16048,
    municipality == "L. CARDENAS" ~ 16052,
    municipality == "LOS REYES" ~ 16075,
    municipality == "MADERO" ~ 16049,
    municipality == "MARAVATIO" ~ 16050,
    municipality == "M.CASTELLANOS" ~ 16051,
    municipality == "MORELIA" ~ 16053,
    municipality == "MORELOS" ~ 16054,
    municipality == "MUGICA" ~ 16055,
    municipality == "NAHUATZEN" ~ 16056,
    municipality == "NOCUPETARO" ~ 16057,
    municipality == "NUEVO PARANGARI" ~ 16058,
    municipality == "NUEVO URECHO" ~ 16059,
    municipality == "NUMARAN" ~ 16060,
    municipality == "OCAMPO" ~ 16061,
    municipality == "PAJACUARAN" ~ 16062,
    municipality == "PANINDICUARO" ~ 16063,
    municipality == "PARACHO" ~ 16065,
    municipality == "PARACUARO" ~ 16064,
    municipality == "PATZCUARO" ~ 16066,
    municipality == "PENJAMILLO" ~ 16067,
    municipality == "PERIBAN" ~ 16068,
    municipality == "PUREPERO" ~ 16070,
    municipality == "PURUANDIRO" ~ 16071,
    municipality == "QUERENDARO" ~ 16072,
    municipality == "QUIROGA" ~ 16073,
    municipality == "SAHUAYO" ~ 16076,
    municipality == "SALV. ESC." ~ 16079,
    municipality == "SAN LUCAS" ~ 16077,
    municipality == "STA. ANA M." ~ 16078,
    municipality == "SENGUIO" ~ 16080,
    municipality == "SUSUPUATO" ~ 16081,
    municipality == "TACAMBARO" ~ 16082,
    municipality == "TANCITARO" ~ 16083,
    municipality == "TANGAMANDAP." ~ 16084,
    municipality == "TANGANCI" ~ 16085,
    municipality == "TANHUATO" ~ 16086,
    municipality == "TARETAN" ~ 16087,
    municipality == "TARIMBARO" ~ 16088,
    municipality == "TEPALCATEP." ~ 16089,
    municipality == "TINGAMBATO" ~ 16090,
    municipality == "TINGUINDIN" ~ 16091,
    municipality == "TIQUICHEO" ~ 16092,
    municipality == "TLALPUJAHUA" ~ 16093,
    municipality == "TLAZAZALCA" ~ 16094,
    municipality == "TOCUMBO" ~ 16095,
    municipality == "TUMBISCATIO" ~ 16096,
    municipality == "TURICATO" ~ 16097,
    municipality == "TUXPAN" ~ 16098,
    municipality == "TUZANTLA" ~ 16099,
    municipality == "TZINTZUNTZAN" ~ 16100,
    municipality == "TZITZIO" ~ 16101,
    municipality == "URUAPAN" ~ 16102,
    municipality == "V. CARRANZA" ~ 16103,
    municipality == "VILLAMAR" ~ 16104,
    municipality == "VISTA HERMOSA" ~ 16105,
    municipality == "YURECUARO" ~ 16106,
    municipality == "ZACAPU" ~ 16107,
    municipality == "ZAMORA" ~ 16108,
    municipality == "ZINAPARO" ~ 16109,
    municipality == "ZINAPECUARO" ~ 16110,
    municipality == "ZIRACUARE" ~ 16111,
    municipality == "ZITACUARO" ~ 16112,
    municipality == "JOSE SIXTO VERDUZCO" ~ 16113,
    TRUE ~ NA  # Keep original uniqueid for others
  ))

# Generate valid (equivalent to `egen valid = rowtotal(PAN PRI PRD PartCardenista PT)`)
collapsed_data <- collapsed_data %>%
  mutate(valid = PAN + PRI + PRD + PartCardenista + PT)

# Loop-like aggregation for municipality totals (equivalent to `bys uniqueid: egen mun_`var' = sum(`var')`)
mun_totals <- collapsed_data %>%
  dplyr::group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI, PRD, PartCardenista, PT, total, valid), sum, .names = "mun_{.col}"))

# Generate inverse of municipality totals (equivalent to `gen inv_mun_`var' = 1/mun_`var'`)
mun_totals <- mun_totals %>%
  mutate(across(starts_with("mun_"), ~ 1 / ., .names = "inv_{.col}"))

# Merge the `mun_totals` back into the original dataset
collapsed_data <- collapsed_data %>%
  left_join(mun_totals, by = "uniqueid")

# Rank variables (equivalent to `rowranks`)
collapsed_data <- collapsed_data %>%
  mutate(across(starts_with("inv_mun_"), ~ rank(., ties.method = "first"), 
                .names = "{.col}_r"))

# Drop `inv_mun_*` variables
collapsed_data <- collapsed_data %>%
  select(-starts_with("inv_mun_"))

# Generate winner (equivalent to `gen winner = "PAN" if PAN_r == 1`)
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PartCardenista_r == 1 ~ "PartCardenista"
  ))

# Generate year and month (equivalent to `gen year = 1995` and `gen month = "November"`)
collapsed_data <- collapsed_data %>%
  mutate(year = 1995, month = "November")

# Save the dataset to a file (equivalent to `save`)
write_csv(collapsed_data, "Michoacan_Section_1995.csv")

# Select only municipality, section, and uniqueid for merging (equivalent to `keep`)
collapsed_data_to_merge <- collapsed_data %>%
  select(municipality, section, uniqueid)

# Save the subset for merging
write_csv(collapsed_data_to_merge, "Michoacan_Section_1995_to_Merge_with_1998.csv")

# Re-load the 1995 dataset and drop rows where winner is empty (equivalent to `drop if winner == ""`)
collapsed_data_1995 <- read_csv("Michoacan_Section_1995.csv") %>%
  filter(winner != "")

# Save the cleaned dataset
write_csv(collapsed_data_1995, "Michoacan_Section_1995.csv")

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 1998
# -------------------------------------- #
data_1998 <- read_csv("Ayu_Seccion_1998_Half_Missing.csv")

# Rename variables
data_1998 <- data_1998 %>%
  rename(municipality = MUNICIPIO,
         listanominal = "LISTA NOM.",
         section = SECCION,
         total = TOTAL,
         nulos = NULOS)
names(data_1998)
# Drop rows where both municipality and section are missing
data_1998 <- data_1998 %>%
  filter(!(is.na(municipality) & is.na(section)))

# Drop rows where total is missing or zero
data_1998 <- data_1998 %>%
  filter(!(is.na(total) | total == 0))

# Convert variables pan to total to numeric
data_1998 <- data_1998 %>%
  mutate(across(PAN:total, as.numeric))

# Generate the missing indicator
data_1998 <- data_1998 %>%
  mutate(missing = ifelse(listanominal == 0, 1, 0))

# Collapse the data by summing up across specified variables, grouping by municipality and section
collapsed_1998 <- data_1998 %>%
  group_by(municipality, section) %>%
  summarise(across(c(missing, PAN:total, listanominal), sum, na.rm = TRUE))


# Generate new variables
collapsed_1998 <- collapsed_1998 %>%
  mutate(ed = 16,
         seccion = section)

# Continue with merging steps (assuming `all_months_years.dta` is available)
# Assuming `all_months_years` data is pre-loaded or available
all_months_years <- read_dta("../all_months_years.dta") # Adjust as needed

merged_data <- collapsed_1998 %>%
  left_join(all_months_years %>%
              filter(state == "Michoacan") %>%
              select(section, month, year, lista),
            by = "seccion")

# Filter for September 1998
filtered_data <- merged_data %>%
  filter(month == 9 & year == 1998)

# Drop unnecessary columns
final_1998 <- filtered_data %>%
  select(-ed, -seccion, -year, -month)

# Replace listanominal with lista where missing >= 1
final_1998 <- final_1998 %>%
  mutate(listanominal = ifelse(missing >= 1, lista, listanominal)) %>%
  select(-missing, -lista)

# Generate turnout
final_1998 <- final_1998 %>%
  mutate(turnout = total / listanominal)

# Drop noregistrados and nulos (not specified, but assuming equivalent variables are dropped)
final_1998 <- final_1998 %>%
  select(-noregistrados, -nulos)

# Generate valid variable (sum of votes)
final_1998 <- final_1998 %>%
  mutate(valid = PAN + PRI + PRD + PT + PVEM)

# Set year and month
final_1998 <- final_1998 %>%
  mutate(year = 1998, month = "November")

# Drop municipality for merging
final_1998 <- final_1998 %>%
  select(-municipality)

# Sort by section and merge with 1995 data
michoacan_1995 <- read_csv("Michoacan_Section_1995_to_Merge_with_1998.csv")
merged_final <- final_1998 %>%
  left_join(michoacan_1995, by = "section") %>%
  filter(!is.na(uniqueid))

# Final adjustments (replacing uniqueid and municipality)
merged_final <- merged_final %>%
  mutate(uniqueid = ifelse(section == 935 & is.na(uniqueid), 16051, uniqueid),
         municipality = ifelse(section == 935 & uniqueid == 16051, "M.CASTELLANOS", municipality),
         uniqueid = ifelse(section == 2675 & is.na(uniqueid), 16053, uniqueid),
         municipality = ifelse(section == 2675 & uniqueid == 16053, "MORELIA", municipality))

# Save final data for 1998
write_csv(merged_final, "Michoacan_Section_1998.csv")

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 2001
# -------------------------------------- #
data_2001 <- read_csv("Ayu_Seccion_2001.csv")

# Rename variables
data_2001 <- data_2001 %>%
  rename(municipality = nombre_municipio,
         section = seccion)

# Drop rows where both municipality is empty and section is missing
data_2001 <- data_2001 %>%
  filter(!(municipality == "" & is.na(section)))

# Drop rows where total is missing or zero
data_2001 <- data_2001 %>%
  filter(!(is.na(total) | total == 0))

# Convert string variables to numeric
data_2001 <- data_2001 %>%
  mutate(across(listanominal:total, as.numeric))

# Collapse the data by summing up across specified variables, grouping by municipality and section
collapsed_2001 <- data_2001 %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan:total), sum, na.rm = TRUE))

# Rename variables
collapsed_2001 <- collapsed_2001 %>%
  rename(PAN = pan, PRI = pri, PRD_PT_PC_PVEM_PAS_PSN = prdptpvempaspsn)

# Generate turnout
collapsed_2001 <- collapsed_2001 %>%
  mutate(turnout = total / listanominal)

# Drop noreg and nulos
collapsed_2001 <- collapsed_2001 %>%
  select(-noreg, -nulos)

# Assign unique IDs based on municipality
collapsed_2001 <- collapsed_2001 %>%
  mutate(uniqueid = case_when(
    municipality == "Acuitzio" ~ 16001,
    municipality == "Aguililla" ~ 16002,
    municipality == "Alvaro Obregon" ~ 16003,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "Angangueo" ~ 16005,
    municipality == "Apatzingan" ~ 16006,
    municipality == "Aporo" ~ 16007,
    municipality == "Aquila" ~ 16008,
    municipality == "Ario" ~ 16009,
    municipality == "Arteaga" ~ 16010,
    municipality == "Briseñas" | str_detect(municipality, "Brise") ~ 16011,
    municipality == "Buenavista" ~ 16012,
    municipality == "Caracuaro" ~ 16013,
    municipality == "Charapan" ~ 16021,
    municipality == "Charo" ~ 16022,
    municipality == "Chavinda" ~ 16023,
    municipality == "Cheran" ~ 16024,
    municipality == "Chilchota" ~ 16025,
    municipality == "Chinicuila" ~ 16026,
    municipality == "Chucandiro" ~ 16027,
    municipality == "Churintzio" ~ 16028,
    municipality == "Churumuco" ~ 16029,
    municipality == "Coahuayana" ~ 16014,
    municipality == "Coalcoman" ~ 16015,
    municipality == "Coeneo" ~ 16016,
    municipality == "Regules" ~ 16074,
    municipality == "Contepec" ~ 16017,
    municipality == "Copandaro" ~ 16018,
    municipality == "Cotija" ~ 16019,
    municipality == "Cuitzeo" ~ 16020,
    municipality == "Ecuandureo" ~ 16030,
    municipality == "Epitacio Huerta" ~ 16031,
    municipality == "Erongaricuaro" ~ 16032,
    municipality == "Gabriel Zamora" ~ 16033,
    municipality == "Hidalgo" ~ 16034,
    municipality == "Huandacareo" ~ 16036,
    municipality == "Huaniqueo" ~ 16037,
    municipality == "Huetamo" ~ 16038,
    municipality == "Huiramba" ~ 16039,
    municipality == "Indaparapeo" ~ 16040,
    municipality == "Irimbo" ~ 16041,
    municipality == "Ixtlan" ~ 16042,
    municipality == "Jacona" ~ 16043,
    municipality == "Jimenez" ~ 16044,
    municipality == "Jiquilpan" ~ 16045,
    municipality == "Jose Sixto Verduzco" ~ 16113,
    municipality == "Juarez" ~ 16046,
    municipality == "Jungapeo" ~ 16047,
    municipality == "La Huacana" ~ 16035,
    municipality == "La Piedad" ~ 16069,
    municipality == "Lagunillas" ~ 16048,
    municipality == "Lazaro Cardenas" ~ 16052,
    municipality == "Los Reyes" ~ 16075,
    municipality == "Madero" ~ 16049,
    municipality == "Maravatio" ~ 16050,
    municipality == "Marcos Castellanos" ~ 16051,
    municipality == "Morelia" ~ 16053,
    municipality == "Morelos" ~ 16054,
    municipality == "Mugica" ~ 16055,
    municipality == "Nahuatzen" ~ 16056,
    municipality == "Nocupetaro" ~ 16057,
    municipality == "Nuevo Parangaricutiro" ~ 16058,
    municipality == "Nuevo Urecho" ~ 16059,
    municipality == "Numaran" ~ 16060,
    municipality == "Ocampo" ~ 16061,
    municipality == "Pajacuaran" ~ 16062,
    municipality == "Panindicuaro" ~ 16063,
    municipality == "Paracho" ~ 16065,
    municipality == "Paracuaro" ~ 16064,
    municipality == "Patzcuaro" ~ 16066,
    municipality == "Penjamillo" ~ 16067,
    municipality == "Periban" ~ 16068,
    municipality == "Purepero" ~ 16070,
    municipality == "Puruandiro" ~ 16071,
    municipality == "Querendaro" ~ 16072,
    municipality == "Quiroga" ~ 16073,
    municipality == "Sahuayo" ~ 16076,
    municipality == "Salvador Escalante" ~ 16079,
    municipality == "San Lucas" ~ 16077,
    municipality == "Santa Ana Maya" ~ 16078,
    municipality == "Senguio" ~ 16080,
    municipality == "Susupuato" ~ 16081,
    municipality == "Tacambaro" ~ 16082,
    municipality == "Tancitaro" ~ 16083,
    municipality == "Tangamandapio" ~ 16084,
    municipality == "Tangancicuaro" ~ 16085,
    municipality == "Tanhuato" ~ 16086,
    municipality == "Taretan" ~ 16087,
    municipality == "Tarimbaro" ~ 16088,
    municipality == "Tepalcatepec" ~ 16089,
    municipality == "Tingambato" ~ 16090,
    municipality == "Tinguindin" ~ 16091,
    municipality == "Tiquicheo" ~ 16092,
    municipality == "Tlalpujahua" ~ 16093,
    municipality == "Tlazazalca" ~ 16094,
    municipality == "Tocumbo" ~ 16095,
    municipality == "Tumbiscatio" ~ 16096,
    municipality == "Turicato" ~ 16097,
    municipality == "Tuxpan" ~ 16098,
    municipality == "Tuzantla" ~ 16099,
    municipality == "Tzintzuntzan" ~ 16100,
    municipality == "Tzitzio" ~ 16101,
    municipality == "Uruapan" ~ 16102,
    municipality == "Venustiano Carranza" ~ 16103,
    municipality == "Villamar" ~ 16104,
    municipality == "Vista Hermosa" ~ 16105,
    municipality == "Yurecuaro" ~ 16106,
    municipality == "Zacapu" ~ 16107,
    municipality == "Zamora" ~ 16108,
    municipality == "Zinaparo" ~ 16109,
    municipality == "Zinapecuaro" ~ 16110,
    municipality == "Ziracuaretiro" ~ 16111,
    municipality == "Zitacuaro" ~ 16112,
    TRUE ~ NA_real_
  ))

# Generate valid row total for votes
collapsed_2001 <- collapsed_2001 %>%
  mutate(valid = PAN + PRI + PRD_PT_PC_PVEM_PAS_PSN)

# Summarize data by unique ID and create inverse of municipality totals
mun_totals <- collapsed_2001 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI, PRD_PT_PC_PVEM_PAS_PSN, total, listanominal, valid), sum)) %>%
  mutate(across(c(PAN, PRI, PRD_PT_PC_PVEM_PAS_PSN, total, listanominal, valid), ~1/.))

# Rank the variables
collapsed_2001 <- collapsed_2001 %>%
  mutate(PAN_r = rank(-PAN), PRI_r = rank(-PRI), PRD_PT_PC_PVEM_PAS_PSN_r = rank(-PRD_PT_PC_PVEM_PAS_PSN))

# Determine the winner
collapsed_2001 <- collapsed_2001 %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_PT_PC_PVEM_PAS_PSN_r == 1 ~ "PRD_PT_PC_PVEM_PAS_PSN",
    TRUE ~ winner
  ))

# Assign year and month
collapsed_2001 <- collapsed_2001 %>%
  mutate(year = 2001, month = "November")

# Save the final file
write_csv(collapsed_2001, "Michoacan_Section_2001.csv")

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 2004
# -------------------------------------- #
data_2004 <- read_csv("Ayu_Seccion_2004.csv")

# Rename variables
data_2004 <- data_2004 %>%
  rename(municipality = nombre_municipio,
         section = seccion,
         listanominal = lista_nominal,
         total = Total,
         nulos = Nulos)

# Drop specific rows (matching Stata's `drop if` condition)
data_2004 <- data_2004 %>%
  filter(!(municipality == "Ario" & section == 170)) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

# Convert string variables to numeric (equivalent to `destring`)
data_2004 <- data_2004 %>%
  mutate(across(listanominal:total, as.numeric))

# Collapse data (equivalent to `collapse (sum)`)
collapsed_2004 <- data_2004 %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal:total), sum, na.rm = TRUE))

# Rename variables
collapsed_2004 <- collapsed_2004 %>%
  rename(
         PRI_PVEM = "PRI-PVEM")

# Generate turnout
collapsed_2004 <- collapsed_2004 %>%
  mutate(turnout = total / listanominal)

# Generate uniqueid (same logic for 2004)
collapsed_2004 <- collapsed_2004 %>%
  mutate(uniqueid = case_when(
    municipality == "Acuitzio" ~ 16001,
    municipality == "Aguililla" ~ 16002,
    municipality == "Alvaro Obregon" ~ 16003,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "Angangueo" ~ 16005,
    municipality == "Apatzingan" ~ 16006,
    municipality == "Aporo" ~ 16007,
    municipality == "Aquila" ~ 16008,
    municipality == "Ario" ~ 16009,
    municipality == "Arteaga" ~ 16010,
    municipality == "Briseñas" | str_detect(municipality, "Brise") ~ 16011,
    municipality == "Buenavista" ~ 16012,
    municipality == "Caracuaro" ~ 16013,
    municipality == "Acuitzio" ~ 16001,
    municipality == "Aguililla" ~ 16002,
    municipality == "Alvaro Obregon" ~ 16003,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "Angangueo" ~ 16005,
    municipality == "Apatzingan" ~ 16006,
    municipality == "Aporo" ~ 16007,
    municipality == "Aquila" ~ 16008,
    municipality == "Ario" ~ 16009,
    municipality == "Arteaga" ~ 16010,
    municipality == "Briseñas" | str_detect(municipality, "Brise") ~ 16011,
    municipality == "Buenavista" ~ 16012,
    municipality == "Caracuaro" ~ 16013,
    municipality == "Charapan" ~ 16021,
    municipality == "Charo" ~ 16022,
    municipality == "Chavinda" ~ 16023,
    municipality == "Cheran" ~ 16024,
    municipality == "Chilchota" ~ 16025,
    municipality == "Chinicuila" ~ 16026,
    municipality == "Chucandiro" ~ 16027,
    municipality == "Churintzio" ~ 16028,
    municipality == "Churumuco" ~ 16029,
    municipality == "Coahuayana" ~ 16014,
    municipality == "Coalcoman" ~ 16015,
    municipality == "Coeneo" ~ 16016,
    municipality == "Regules" ~ 16074,
    municipality == "Contepec" ~ 16017,
    municipality == "Copandaro" ~ 16018,
    municipality == "Cotija" ~ 16019,
    municipality == "Cuitzeo" ~ 16020,
    municipality == "Ecuandureo" ~ 16030,
    municipality == "Epitacio Huerta" ~ 16031,
    municipality == "Erongaricuaro" ~ 16032,
    municipality == "Gabriel Zamora" ~ 16033,
    municipality == "Hidalgo" ~ 16034,
    municipality == "Huandacareo" ~ 16036,
    municipality == "Huaniqueo" ~ 16037,
    municipality == "Huetamo" ~ 16038,
    municipality == "Huiramba" ~ 16039,
    municipality == "Indaparapeo" ~ 16040,
    municipality == "Irimbo" ~ 16041,
    municipality == "Ixtlan" ~ 16042,
    municipality == "Jacona" ~ 16043,
    municipality == "Jimenez" ~ 16044,
    municipality == "Jiquilpan" ~ 16045,
    municipality == "Jose Sixto Verduzco" ~ 16113,
    municipality == "Juarez" ~ 16046,
    municipality == "Jungapeo" ~ 16047,
    municipality == "La Huacana" ~ 16035,
    municipality == "La Piedad" ~ 16069,
    municipality == "Lagunillas" ~ 16048,
    municipality == "Lazaro Cardenas" ~ 16052,
    municipality == "Los Reyes" ~ 16075,
    municipality == "Madero" ~ 16049,
    municipality == "Maravatio" ~ 16050,
    municipality == "Marcos Castellanos" ~ 16051,
    municipality == "Morelia" ~ 16053,
    municipality == "Morelos" ~ 16054,
    municipality == "Mugica" ~ 16055,
    municipality == "Nahuatzen" ~ 16056,
    municipality == "Nocupetaro" ~ 16057,
    municipality == "Nuevo Parangaricutiro" ~ 16058,
    municipality == "Nuevo Urecho" ~ 16059,
    municipality == "Numaran" ~ 16060,
    municipality == "Ocampo" ~ 16061,
    municipality == "Pajacuaran" ~ 16062,
    municipality == "Panindicuaro" ~ 16063,
    municipality == "Paracho" ~ 16065,
    municipality == "Paracuaro" ~ 16064,
    municipality == "Patzcuaro" ~ 16066,
    municipality == "Penjamillo" ~ 16067,
    municipality == "Periban" ~ 16068,
    municipality == "Purepero" ~ 16070,
    municipality == "Puruandiro" ~ 16071,
    municipality == "Querendaro" ~ 16072,
    municipality == "Quiroga" ~ 16073,
    municipality == "Sahuayo" ~ 16076,
    municipality == "Salvador Escalante" ~ 16079,
    municipality == "San Lucas" ~ 16077,
    municipality == "Santa Ana Maya" ~ 16078,
    municipality == "Senguio" ~ 16080,
    municipality == "Susupuato" ~ 16081,
    municipality == "Tacambaro" ~ 16082,
    municipality == "Tancitaro" ~ 16083,
    municipality == "Tangamandapio" ~ 16084,
    municipality == "Tangancicuaro" ~ 16085,
    municipality == "Tanhuato" ~ 16086,
    municipality == "Taretan" ~ 16087,
    municipality == "Tarimbaro" ~ 16088,
    municipality == "Tepalcatepec" ~ 16089,
    municipality == "Tingambato" ~ 16090,
    municipality == "Tinguindin" ~ 16091,
    municipality == "Tiquicheo" ~ 16092,
    municipality == "Tlalpujahua" ~ 16093,
    municipality == "Tlazazalca" ~ 16094,
    municipality == "Tocumbo" ~ 16095,
    municipality == "Tumbiscatio" ~ 16096,
    municipality == "Turicato" ~ 16097,
    municipality == "Tuxpan" ~ 16098,
    municipality == "Tuzantla" ~ 16099,
    municipality == "Tzintzuntzan" ~ 16100,
    municipality == "Tzitzio" ~ 16101,
    municipality == "Uruapan" ~ 16102,
    municipality == "Venustiano Carranza" ~ 16103,
    municipality == "Villamar" ~ 16104,
    municipality == "Vista Hermosa" ~ 16105,
    municipality == "Yurecuaro" ~ 16106,
    municipality == "Zacapu" ~ 16107,
    municipality == "Zamora" ~ 16108,
    municipality == "Zinaparo" ~ 16109,
    municipality == "Zinapecuaro" ~ 16110,
    municipality == "Ziracuaretiro" ~ 16111,
    municipality == "Zitacuaro" ~ 16112,
    TRUE ~ NA_real_
  ))

# Generate `valid` (sum of votes for valid parties)
collapsed_2004 <- collapsed_2004 %>%
  mutate(valid = PAN + PRI_PVEM + PRD + PT + PC)

# Aggregate data by uniqueid (equivalent to `bys uniqueid: egen`)
mun_totals_2004 <- collapsed_2004 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI_PVEM, PRD, PT, PC, total, listanominal, valid), sum, na.rm = TRUE))

# Calculate inverse totals (equivalent to `gen inv_mun_`var'`)
mun_totals_2004 <- mun_totals_2004 %>%
  mutate(across(starts_with("mun_"), ~ 1 / ., .names = "inv_{.col}"))

# Rank the inverse values (equivalent to `rowranks`)
collapsed_2004 <- collapsed_2004 %>%
  mutate(across(starts_with("inv_mun_"), rank, ties.method = "first", .names = "{.col}_r"))

# Determine winner (equivalent to Stata's `gen winner`)
collapsed_2004 <- collapsed_2004 %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_PVEM_r == 1 ~ "PRI_PVEM",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PC_r == 1 ~ "PC",
    TRUE ~ NA_character_
  ))

# Set year and month
collapsed_2004 <- collapsed_2004 %>%
  mutate(year = 2004, month = "November")

# Save the final 2004 data
write_csv(collapsed_2004, "Michoacan_Section_2004.csv")

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 2005
# -------------------------------------- #
data_2005 <- read_excel("resultados_electorales_2005.xls", sheet = "Ayuntamiento, por Casilla, 2005", range = "A7:K36")

# Keep only rows that contain section data
data_2005 <- data_2005 %>%
  filter(str_detect(B, "ecci"))

# Extract section from string (equivalent to `substr`)
data_2005 <- data_2005 %>%
  mutate(section = str_sub(B, -4))

# Drop unnecessary columns
data_2005 <- data_2005 %>%
  select(-A, -B)

# Rename columns to match the structure
data_2005 <- data_2005 %>%
  rename(listanominal = ListaNominal,
         total = VotosTotales)

# Assign municipality and uniqueid
data_2005 <- data_2005 %>%
  mutate(municipality = "TUMBISCATIO EXTRAORDINARIO",
         uniqueid = 16096)

# Filter out rows with missing or zero `total`
data_2005 <- data_2005 %>%
  filter(!(is.na(total) | total == 0))

# Convert all columns to numeric
data_2005 <- data_2005 %>%
  mutate(across(everything(), as.numeric))

# Rename party columns
data_2005 <- data_2005 %>%
  rename(PRI = PartidoRevolucionarioInstituci,
         PRD = PartidodelaRevoluciónDemocrá,
         PT = PartidodelTrabajo,
         PC = Convergencia)

# Collapse data by municipality, section, and uniqueid
collapsed_2005 <- data_2005 %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(c(listanominal, PRI:PC, total), sum, na.rm = TRUE))

# Calculate turnout
collapsed_2005 <- collapsed_2005 %>%
  mutate(turnout = total / listanominal)

# Generate valid row total (sum of votes)
collapsed_2005 <- collapsed_2005 %>%
  mutate(valid = PRI + PRD + PT + PC)

# Aggregate by uniqueid
mun_totals_2005 <- collapsed_2005 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PRI, PRD, PT, PC, total, listanominal, valid), sum, na.rm = TRUE))

# Calculate inverse of municipality totals
mun_totals_2005 <- mun_totals_2005 %>%
  mutate(across(starts_with("mun_"), ~ 1 / ., .names = "inv_{.col}"))

# Rank the inverse values
collapsed_2005 <- collapsed_2005 %>%
  mutate(across(starts_with("inv_mun_"), rank, ties.method = "first", .names = "{.col}_r"))

# Determine winner based on ranks
collapsed_2005 <- collapsed_2005 %>%
  mutate(winner = case_when(
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PC_r == 1 ~ "PC",
    TRUE ~ NA_character_
  ))

# Set year and month
collapsed_2005 <- collapsed_2005 %>%
  mutate(year = 2005, month = "June")

# Save the final 2005 data
write_csv(collapsed_2005, "Michoacan_Section_2005.csv")

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 2007
# -------------------------------------- #
data_2007 <- read_excel("Ayu_Seccion_2007.xlsx")

# Rename variables
data_2007 <- data_2007 %>%
  rename(municipality = municipio,
         section = seccion)

# Drop rows with missing municipality and section
data_2007 <- data_2007 %>%
  filter(!(municipality == "" & is.na(section)))

# Generate `total` votes by summing the parties and coalitions
data_2007 <- data_2007 %>%
  mutate(total = rowSums(select(., pan:cc_pri_pvem_pna), na.rm = TRUE))

# Drop rows where total is missing or zero
data_2007 <- data_2007 %>%
  filter(!(is.na(total) | total == 0))

# Convert columns to numeric
data_2007 <- data_2007 %>%
  mutate(across(listanominal:total, as.numeric))

# Collapse data (sum by municipality and section)
collapsed_2007 <- data_2007 %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:total, sum, na.rm = TRUE))

# Coalition Handling: Replace coalitions with combined values
collapsed_2007 <- collapsed_2007 %>%
  mutate(prd_pt_pc = prd + pt + pc + coal_prd_pt_pc,
         pan_pvem = pan + pvem + cc_pan_pvem,
         # More replacements...
         pan_pri_pvem_pna = pan + pri + pvem + pna + cc_pan_pri_pvem_pna)

# Rename columns for final coalition names
collapsed_2007 <- collapsed_2007 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PC = pc,
         PANAL = pna)

# Calculate turnout
collapsed_2007 <- collapsed_2007 %>%
  mutate(turnout = total / listanominal)

# Assign `uniqueid` values
collapsed_2007 <- collapsed_2007 %>%
  mutate(uniqueid = case_when(
    municipality == "Acuitzio" ~ 16001,
    municipality == "Aguililla" ~ 16002,
    municipality == "Alvaro Obregón" ~ 16003,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "Angangueo" ~ 16005,
    municipality == "Apatzingán" ~ 16006,
    municipality == "Aporo" ~ 16007,
    municipality == "Aquila" ~ 16008,
    municipality == "Ario" ~ 16009,
    municipality == "Arteaga" ~ 16010,
    municipality == "Briseñas" | str_detect(municipality, "Brise") ~ 16011,
    municipality == "Buenavista" ~ 16012,
    municipality == "Caracuaro" ~ 16013,
    municipality == "Acuitzio" ~ 16001,
    municipality == "Aguililla" ~ 16002,
    municipality == "Alvaro Obregon" ~ 16003,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "Angangueo" ~ 16005,
    municipality == "Apatzingan" ~ 16006,
    municipality == "Aporo" ~ 16007,
    municipality == "Aquila" ~ 16008,
    municipality == "Ario" ~ 16009,
    municipality == "Arteaga" ~ 16010,
    municipality == "Briseñas" | str_detect(municipality, "Brise") ~ 16011,
    municipality == "Buenavista" ~ 16012,
    municipality == "Caracuaro" ~ 16013,
    municipality == "Charapan" ~ 16021,
    municipality == "Charo" ~ 16022,
    municipality == "Chavinda" ~ 16023,
    municipality == "Cheran" ~ 16024,
    municipality == "Chilchota" ~ 16025,
    municipality == "Chinicuila" ~ 16026,
    municipality == "Chucandiro" ~ 16027,
    municipality == "Churintzio" ~ 16028,
    municipality == "Churumuco" ~ 16029,
    municipality == "Coahuayana" ~ 16014,
    municipality == "Coalcoman" ~ 16015,
    municipality == "Coeneo" ~ 16016,
    municipality == "Regules" ~ 16074,
    municipality == "Contepec" ~ 16017,
    municipality == "Copandaro" ~ 16018,
    municipality == "Cotija" ~ 16019,
    municipality == "Cuitzeo" ~ 16020,
    municipality == "Ecuandureo" ~ 16030,
    municipality == "Epitacio Huerta" ~ 16031,
    municipality == "Erongaricuaro" ~ 16032,
    municipality == "Gabriel Zamora" ~ 16033,
    municipality == "Hidalgo" ~ 16034,
    municipality == "Huandacareo" ~ 16036,
    municipality == "Huaniqueo" ~ 16037,
    municipality == "Huetamo" ~ 16038,
    municipality == "Huiramba" ~ 16039,
    municipality == "Indaparapeo" ~ 16040,
    municipality == "Irimbo" ~ 16041,
    municipality == "Ixtlan" ~ 16042,
    municipality == "Jacona" ~ 16043,
    municipality == "Jimenez" ~ 16044,
    municipality == "Jiquilpan" ~ 16045,
    municipality == "Jose Sixto Verduzco" ~ 16113,
    municipality == "Juarez" ~ 16046,
    municipality == "Jungapeo" ~ 16047,
    municipality == "La Huacana" ~ 16035,
    municipality == "La Piedad" ~ 16069,
    municipality == "Lagunillas" ~ 16048,
    municipality == "Lazaro Cardenas" ~ 16052,
    municipality == "Los Reyes" ~ 16075,
    municipality == "Madero" ~ 16049,
    municipality == "Maravatio" ~ 16050,
    municipality == "Marcos Castellanos" ~ 16051,
    municipality == "Morelia" ~ 16053,
    municipality == "Morelos" ~ 16054,
    municipality == "Mugica" ~ 16055,
    municipality == "Nahuatzen" ~ 16056,
    municipality == "Nocupetaro" ~ 16057,
    municipality == "Nuevo Parangaricutiro" ~ 16058,
    municipality == "Nuevo Urecho" ~ 16059,
    municipality == "Numaran" ~ 16060,
    municipality == "Ocampo" ~ 16061,
    municipality == "Pajacuaran" ~ 16062,
    municipality == "Panindicuaro" ~ 16063,
    municipality == "Paracho" ~ 16065,
    municipality == "Paracuaro" ~ 16064,
    municipality == "Patzcuaro" ~ 16066,
    municipality == "Penjamillo" ~ 16067,
    municipality == "Periban" ~ 16068,
    municipality == "Purepero" ~ 16070,
    municipality == "Puruandiro" ~ 16071,
    municipality == "Querendaro" ~ 16072,
    municipality == "Quiroga" ~ 16073,
    municipality == "Sahuayo" ~ 16076,
    municipality == "Salvador Escalante" ~ 16079,
    municipality == "San Lucas" ~ 16077,
    municipality == "Santa Ana Maya" ~ 16078,
    municipality == "Senguio" ~ 16080,
    municipality == "Susupuato" ~ 16081,
    municipality == "Tacambaro" ~ 16082,
    municipality == "Tancitaro" ~ 16083,
    municipality == "Tangamandapio" ~ 16084,
    municipality == "Tangancicuaro" ~ 16085,
    municipality == "Tanhuato" ~ 16086,
    municipality == "Taretan" ~ 16087,
    municipality == "Tarimbaro" ~ 16088,
    municipality == "Tepalcatepec" ~ 16089,
    municipality == "Tingambato" ~ 16090,
    municipality == "Tinguindin" ~ 16091,
    municipality == "Tiquicheo" ~ 16092,
    municipality == "Tlalpujahua" ~ 16093,
    municipality == "Tlazazalca" ~ 16094,
    municipality == "Tocumbo" ~ 16095,
    municipality == "Tumbiscatio" ~ 16096,
    municipality == "Turicato" ~ 16097,
    municipality == "Tuxpan" ~ 16098,
    municipality == "Tuzantla" ~ 16099,
    municipality == "Tzintzuntzan" ~ 16100,
    municipality == "Tzitzio" ~ 16101,
    municipality == "Uruapan" ~ 16102,
    municipality == "Venustiano Carranza" ~ 16103,
    municipality == "Villamar" ~ 16104,
    municipality == "Vista Hermosa" ~ 16105,
    municipality == "Yurecuaro" ~ 16106,
    municipality == "Zacapu" ~ 16107,
    municipality == "Zamora" ~ 16108,
    municipality == "Zinaparo" ~ 16109,
    municipality == "Zinapecuaro" ~ 16110,
    municipality == "Ziracuaretiro" ~ 16111,
    municipality == "Zitacuaro" ~ 16112,
    TRUE ~ NA_real_
  ))

# Generate valid row total (sum of votes for all parties)
collapsed_2007 <- collapsed_2007 %>%
  mutate(valid = PAN + PRI + PRD + PT + PVEM + PC + PANAL)

# Aggregate data by `uniqueid`
mun_totals_2007 <- collapsed_2007 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI, PRD, PT, PVEM, PC, PANAL, total, valid), sum, na.rm = TRUE))

# Rank by inverse totals
collapsed_2007 <- collapsed_2007 %>%
  mutate(across(starts_with("inv_mun_"), rank, ties.method = "first", .names = "{.col}_r"))

# Determine winner
collapsed_2007 <- collapsed_2007 %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PC_r == 1 ~ "PC",
    inv_mun_PANAL_r == 1 ~ "PANAL",
    TRUE ~ NA_character_
  ))

# Set year and month
collapsed_2007 <- collapsed_2007 %>%
  mutate(year = 2007, month = "November")

# Save the final 2007 data
write_csv(collapsed_2007, "Michoacan_Section_2007.csv")

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 2008
# -------------------------------------- #
data_2008 <- read_excel("resultados_2008.xls", sheet = "Ayuntamiento Yurécuaro", range = "A8:L46")

# Step 2: Rename columns to match the Stata code
data_2008 <- data_2008 %>%
  rename(section = Sección, total = K)

# Step 3: Add municipality and uniqueid
data_2008 <- data_2008 %>%
  mutate(municipality = "Yurécuaro EXTRAORDINARIO", uniqueid = 16106)

# Step 4: Drop rows where total is missing or zero
data_2008 <- data_2008 %>%
  filter(!is.na(total) & total != 0)

# Step 5: Collapse (sum) by uniqueid, municipality, and section
collapsed_2008 <- data_2008 %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(PAN:PT, sum, na.rm = TRUE), total = sum(total, na.rm = TRUE))

### Handle `all_months_years.dta` data for merging:

# Load the "all_months_years.dta" dataset (assuming it’s in CSV or .dta format)
# Using read_dta for .dta format, replace with read_csv if it's a CSV file.
library(haven)  # For reading .dta files
all_months_years <- read_dta("all_months_years.dta")

# Filter data for ed == 16, month == 4, and year == 2008
filtered_data <- all_months_years %>%
  filter(ed == 16, month == 4, year == 2008) %>%
  select(section, listanominal)

# Step 6: Merge with the collapsed 2008 data by `section`
merged_data_2008 <- collapsed_2008 %>%
  left_join(filtered_data, by = "section")

# Step 7: Drop rows where merge failed (`drop if _merge == 2` equivalent)
# In R, this happens automatically during the merge if no match is found.

# Step 8: Calculate turnout (equivalent to `gen turnout = total / listanominal`)
merged_data_2008 <- merged_data_2008 %>%
  mutate(turnout = total / listanominal)

# Step 9: Generate the `valid` column (equivalent to `egen valid = rowtotal(PAN PRI PRD PT)`)
merged_data_2008 <- merged_data_2008 %>%
  mutate(valid = PAN + PRI + PRD + PT)

### Aggregate Data by `uniqueid`

# Step 10: Aggregate by `uniqueid` (equivalent to `bys uniqueid: egen mun_`var'= sum(`var')`)
mun_totals_2008 <- merged_data_2008 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI, PRD, PT, total, listanominal, valid), sum, na.rm = TRUE))

# Step 11: Create inverse of municipality totals (equivalent to `gen inv_mun_`var'= 1/mun_`var'`)
mun_totals_2008 <- mun_totals_2008 %>%
  mutate(across(c(PAN, PRI, PRD, PT), ~ 1 / ., .names = "inv_mun_{.col}"))

### Ranking the Inverse Totals and Determining the Winner

# Step 12: Rank the inverse municipality totals (equivalent to `rowranks`)
merged_data_2008 <- merged_data_2008 %>%
  mutate(PAN_r = rank(-inv_mun_PAN, ties.method = "first"),
         PRI_r = rank(-inv_mun_PRI, ties.method = "first"),
         PRD_r = rank(-inv_mun_PRD, ties.method = "first"),
         PT_r  = rank(-inv_mun_PT, ties.method = "first"))

# Step 13: Determine the winner based on ranks (equivalent to `gen winner`)
merged_data_2008 <- merged_data_2008 %>%
  mutate(winner = case_when(
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1  ~ "PT",
    PAN_r == 1 ~ "PAN",
    TRUE ~ NA_character_
  ))

### Cleaning and Final Steps

# Step 14: Drop rank columns (equivalent to `drop *_r`)
merged_data_2008 <- merged_data_2008 %>%
  select(-PAN_r, -PRI_r, -PRD_r, -PT_r)

# Step 15: Add year and month
merged_data_2008 <- merged_data_2008 %>%
  mutate(year = 2008, month = "May")

# Step 16: Save the final dataset (equivalent to `save Michoacan_Section_2008.dta, replace`)
write_dta(merged_data_2008, "Michoacan_Section_2008.dta")

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 2011
# -------------------------------------- #

# Step 1: Load the 2011 dataset
data_2011 <- read_dta("Ayu_Seccion_2011.dta")

# Step 2: Capitalize the first letter of the municipality (equivalent to `proper()`)
data_2011 <- data_2011 %>%
  mutate(municipality = stringr::str_to_title(municipality))

# Step 3: Generate the turnout variable (equivalent to `gen turnout = total/listanominal`)
data_2011 <- data_2011 %>%
  mutate(turnout = total / listanominal)

# Step 4: Drop unnecessary variables (equivalent to `drop noreg nulos`)
data_2011 <- data_2011 %>%
  select(-noreg, -nulos)

# Step 5: Initialize `uniqueid` with 0 and replace based on municipality names
data_2011 <- data_2011 %>%
  mutate(uniqueid = case_when(
    municipality == "Acuitzio" ~ 16001,
    municipality == "Aguililla" ~ 16002,
    municipality == "Alvaro Obregon" ~ 16003,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "Angangueo" ~ 16005,
    municipality == "Apatzingan" ~ 16006,
    municipality == "Aporo" ~ 16007,
    municipality == "Aquila" ~ 16008,
    municipality == "Ario" ~ 16009,
    municipality == "Arteaga" ~ 16010,
    municipality == "Brisenas" ~ 16011,
    municipality == "Buenavista" ~ 16012,
    municipality == "Caracuaro" ~ 16013,
    municipality == "Charapan" ~ 16021,
    municipality == "Charo" ~ 16022,
    municipality == "Chavinda" ~ 16023,
    municipality == "Cheran" ~ 16024,
    municipality == "Chilchota" ~ 16025,
    municipality == "Chinicuila" ~ 16026,
    municipality == "Chucandiro" ~ 16027,
    municipality == "Churintzio" ~ 16028,
    municipality == "Churumuco" ~ 16029,
    municipality == "Coahuayana" ~ 16014,
    municipality == "Coalcoman" ~ 16015,
    municipality == "Coeneo" ~ 16016,
    municipality == "Regules" ~ 16074,
    municipality == "Contepec" ~ 16017,
    municipality == "Copandaro" ~ 16018,
    municipality == "Cotija" ~ 16019,
    municipality == "Cuitzeo" ~ 16020,
    municipality == "Ecuandureo" ~ 16030,
    municipality == "Epitacio Huerta" ~ 16031,
    municipality == "Erongaricuaro" ~ 16032,
    municipality == "Gabriel Zamora" ~ 16033,
    municipality == "Hidalgo" ~ 16034,
    municipality == "Huandacareo" ~ 16036,
    municipality == "Huaniqueo" ~ 16037,
    municipality == "Huetamo" ~ 16038,
    municipality == "Huiramba" ~ 16039,
    municipality == "Indaparapeo" ~ 16040,
    municipality == "Irimbo" ~ 16041,
    municipality == "Ixtlan" ~ 16042,
    municipality == "Jacona" ~ 16043,
    municipality == "Jimenez" ~ 16044,
    municipality == "Jiquilpan" ~ 16045,
    municipality == "Jose Sixto Verduzco" ~ 16113,
    municipality == "Juarez" ~ 16046,
    municipality == "Jungapeo" ~ 16047,
    municipality == "La Huacana" ~ 16035,
    municipality == "La Piedad" ~ 16069,
    municipality == "Lagunillas" ~ 16048,
    municipality == "Lazaro Cardenas" ~ 16052,
    municipality == "Los Reyes" ~ 16075,
    municipality == "Madero" ~ 16049,
    municipality == "Maravatio" ~ 16050,
    municipality == "Marcos Castellanos" ~ 16051,
    municipality == "Morelia" ~ 16053,
    municipality == "Morelos" ~ 16054,
    municipality == "Mugica" ~ 16055,
    municipality == "Nahuatzen" ~ 16056,
    municipality == "Nocupetaro" ~ 16057,
    municipality == "Nuevo Parangaricutiro" ~ 16058,
    municipality == "Nuevo Urecho" ~ 16059,
    municipality == "Numaran" ~ 16060,
    municipality == "Ocampo" ~ 16061,
    municipality == "Pajacuaran" ~ 16062,
    municipality == "Panindicuaro" ~ 16063,
    municipality == "Paracho" ~ 16065,
    municipality == "Paracuaro" ~ 16064,
    municipality == "Patzcuaro" ~ 16066,
    municipality == "Penjamillo" ~ 16067,
    municipality == "Periban" ~ 16068,
    municipality == "Purepero" ~ 16070,
    municipality == "Puruandiro" ~ 16071,
    municipality == "Querendaro" ~ 16072,
    municipality == "Quiroga" ~ 16073,
    municipality == "Sahuayo" ~ 16076,
    municipality == "Salvador Escalante" ~ 16079,
    municipality == "San Lucas" ~ 16077,
    municipality == "Santa Ana Maya" ~ 16078,
    municipality == "Senguio" ~ 16080,
    municipality == "Susupuato" ~ 16081,
    municipality == "Tacambaro" ~ 16082,
    municipality == "Tancitaro" ~ 16083,
    municipality == "Tangamandapio" ~ 16084,
    municipality == "Tangancicuaro" ~ 16085,
    municipality == "Tanhuato" ~ 16086,
    municipality == "Taretan" ~ 16087,
    municipality == "Tarimbaro" ~ 16088,
    municipality == "Tepalcatepec" ~ 16089,
    municipality == "Tingambato" ~ 16090,
    municipality == "Tinguindin" ~ 16091,
    municipality == "Tiquicheo" ~ 16092,
    municipality == "Tlalpujahua" ~ 16093,
    municipality == "Tlazazalca" ~ 16094,
    municipality == "Tocumbo" ~ 16095,
    municipality == "Tumbiscatio" ~ 16096,
    municipality == "Turicato" ~ 16097,
    municipality == "Tuxpan" ~ 16098,
    municipality == "Tuzantla" ~ 16099,
    municipality == "Tzintzuntzan" ~ 16100,
    municipality == "Tzitzio" ~ 16101,
    municipality == "Uruapan" ~ 16102,
    municipality == "Venustiano Carranza" ~ 16103,
    municipality == "Villamar" ~ 16104,
    municipality == "Vista Hermosa" ~ 16105,
    municipality == "Yurecuaro" ~ 16106,
    municipality == "Zacapu" ~ 16107,
    municipality == "Zamora" ~ 16108,
    municipality == "Zinaparo" ~ 16109,
    municipality == "Zinapecuaro" ~ 16110,
    municipality == "Ziracuaretiro" ~ 16111,
    municipality == "Zitacuaro" ~ 16112,
    TRUE ~ NA  # Default value if municipality doesn't match
  ))

# Step 6: Handle coalitions (PRD_PT example given)
# If there's a condition where C_PRD_PT > 0 and not missing, adjust PRD_PT accordingly
data_2011 <- data_2011 %>%
  mutate(PRD_PT = ifelse(!is.na(C_PRD_PT) & C_PRD_PT > 0, C_PRD_PT, PRD_PT)) %>%
  select(-C_PRD_PT)  # Drop C_PRD_PT

### Coalition Adjustments
# Step 7: Adjust the coalition variables using case_when() for each coalition condition
data_2011 <- data_2011 %>%
  mutate(
    PAN_PRI_PVEM = ifelse(c_PAN_PRI_PVEM == 1, PAN + PRI + PVEM, PAN_PRI_PVEM),
    PRI = ifelse(c_PAN_PRI_PVEM == 1, 0, PRI),
    PAN = ifelse(c_PAN_PRI_PVEM == 1, 0, PAN),
    PVEM = ifelse(c_PAN_PRI_PVEM == 1, 0, PVEM),
    PAN_PRI_PVEM_PANAL = ifelse(c_PAN_PRI_PVEM_PANAL == 1, PAN + PRI + PVEM + PANAL, PAN_PRI_PVEM_PANAL),
    PRI = ifelse(c_PAN_PRI_PVEM_PANAL == 1, 0, PRI),
    PAN = ifelse(c_PAN_PRI_PVEM_PANAL == 1, 0, PAN),
    PVEM = ifelse(c_PAN_PRI_PVEM_PANAL == 1, 0, PVEM),
    PANAL = ifelse(c_PAN_PRI_PVEM_PANAL == 1, 0, PANAL),
    # Add remaining coalition adjustments for PAN_PRI_PANAL, PAN_PANAL, etc.
    PRI_PVEM = ifelse(c_PRI_PVEM == 1, PRI + PVEM, PRI_PVEM),
    PRI = ifelse(c_PRI_PVEM == 1, 0, PRI),
    PVEM = ifelse(c_PRI_PVEM == 1, 0, PVEM)
  ) %>%
  select(-starts_with("c_"))  # Drop all c_* variables

### Row totals and aggregations

# Step 8: Create the `valid` row total
data_2011 <- data_2011 %>%
  mutate(valid = PAN + PRI + PRD + PT + PVEM + PC + PANAL + PAN_PRI_PVEM + PAN_PRI_PVEM_PANAL + PRI_PVEM + PRD_PT + PRD_PT_PC + PRD_PC + PT_PC)

# Step 9: Aggregate by `uniqueid` and sum variables (equivalent to `bys uniqueid`)
mun_totals_2011 <- data_2011 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI, PRD, PT, PVEM, PC, PANAL, total, listanominal, valid), sum, na.rm = TRUE))

# Step 10: Calculate inverse totals (equivalent to `gen inv_mun_var`)
mun_totals_2011 <- mun_totals_2011 %>%
  mutate(across(starts_with("mun_"), ~ 1 / ., .names = "inv_{.col}"))

### Ranking and determining the winner

# Step 11: Rank the inverse municipality totals
mun_totals_2011 <- mun_totals_2011 %>%
  mutate(
    PAN_r = rank(-inv_mun_PAN, ties.method = "first"),
    PRI_r = rank(-inv_mun_PRI, ties.method = "first"),
    PRD_r = rank(-inv_mun_PRD, ties.method = "first"),
    PT_r  = rank(-inv_mun_PT, ties.method = "first"),
    PVEM_r = rank(-inv_mun_PVEM, ties.method = "first"),
    PC_r = rank(-inv_mun_PC, ties.method = "first"),
    PANAL_r = rank(-inv_mun_PANAL, ties.method = "first")
  )

# Step 12: Determine the winner based on ranks
mun_totals_2011 <- mun_totals_2011 %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1  ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PANAL_r == 1 ~ "PANAL",
    TRUE ~ NA_character_
  ))

# Step 13: Drop rank columns and finalize
mun_totals_2011 <- mun_totals_2011 %>%
  select(-PAN_r, -PRI_r, -PRD_r, -PT_r, -PVEM_r, -PC_r, -PANAL_r)

# Step 14: Add year and month
mun_totals_2011 <- mun_totals_2011 %>%
  mutate(year = 2011, month = "November")

# Step 15: Save the final dataset (equivalent to `save Michoacan_Section_2011.dta`)
write_dta(mun_totals_2011, "Michoacan_Section_2011.dta")


### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 2012
# -------------------------------------- #
data_2012 <- read_excel("Resultados_electorales_extraordinaria_2012.xlsx", sheet = "morelia2012_bd", range = "A8:S904")

# Rename columns
data_2012 <- data_2012 %>%
  rename(section = SECCIÓN,
         total = TOTALVOTOS)

# Assign municipality and uniqueid
data_2012 <- data_2012 %>%
  mutate(municipality = "MORELIA EXTRAORDINARIO",
         uniqueid = 16053)

# Drop rows where total is missing or zero
data_2012 <- data_2012 %>%
  filter(!(is.na(total) | total == 0))

# Handle coalitions
data_2012 <- data_2012 %>%
  mutate(PAN_PANAL = PAN + PNA + CCPANPNA,
         PRD_PT_PMC = PRD + PT + PMC + CCPRDPTPMC,
         PRI_PVEM = CCM)

# Collapse data by section and uniqueid
collapsed_2012 <- data_2012 %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(c(PAN_PANAL, PRD_PT_PMC, PRI_PVEM, total), sum, na.rm = TRUE))

# Merge with `all_months_years` data to get `listanominal`
all_months_years <- read_csv("all_months_years.csv")
merged_2012 <- collapsed_2012 %>%
  left_join(all_months_years %>% 
              filter(fecha == "01/07/2012", year == 2012) %>%
              select(section, listanominal), by = "section")

# Calculate turnout
merged_2012 <- merged_2012 %>%
  mutate(turnout = total / listanominal)

# Generate valid row total (sum of votes for all parties)
merged_2012 <- merged_2012 %>%
  mutate(valid = PAN_PANAL + PRD_PT_PMC + PRI_PVEM)

# Aggregate data by uniqueid
mun_totals_2012 <- merged_2012 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN_PANAL, PRD_PT_PMC, PRI_PVEM, total, listanominal, valid), sum, na.rm = TRUE))

# Rank by inverse totals
merged_2012 <- merged_2012 %>%
  mutate(across(starts_with("inv_mun_"), rank, ties.method = "first", .names = "{.col}_r"))

# Determine winner based on ranks
merged_2012 <- merged_2012 %>%
  mutate(winner = case_when(
    inv_mun_PRI_PVEM_r == 1 ~ "PRI_PVEM",
    inv_mun_PRD_PT_PMC_r == 1 ~ "PRD_PT_PMC",
    inv_mun_PAN_PANAL_r == 1 ~ "PAN_PANAL",
    TRUE ~ NA_character_
  ))

# Set year and month
merged_2012 <- merged_2012 %>%
  mutate(year = 2012, month = "July")

# Save final 2012 data
write_csv(merged_2012, "Michoacan_Section_2012.csv")

michoacan_all <- bind_rows(mich_1995, 
                           mich_1998, 
                           mich_2001, 
                           mich_2004, 
                           mich_2005, 
                           mich_2007, 
                           mich_2008, 
                           mich_2011, 
                           mich_2012)

# Step 3: Delete the individual datasets from memory (equivalent to `erase`)
# In R, you can remove objects from memory using `rm()`:
rm(mich_1995, mich_1998, mich_2001, mich_2004, mich_2005, mich_2007, mich_2008, mich_2011, mich_2012)

# Step 4: Generate `PAN_winner` and initialize to 0
# Then update it based on the condition from the `winner` column (using `str_detect` for string search)
michoacan_all <- michoacan_all %>%
  mutate(PAN_winner = ifelse(str_detect(winner, "PAN") & !str_detect(winner, "PANAL"), 1, 0))

# Step 5: Initialize `winner_counter` to `PAN_winner`
michoacan_all <- michoacan_all %>%
  mutate(winner_counter = PAN_winner)

# Step 6: Loop through other parties and create winner flags (equivalent to Stata's `foreach`)
parties <- c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "PD", "PSD", "PAS", "PSN")

for (party in parties) {
  michoacan_all <- michoacan_all %>%
    mutate(!!paste0(party, "_winner") := ifelse(str_detect(winner, party), 1, 0),
           winner_counter = winner_counter + !!sym(paste0(party, "_winner")))
}

# Step 7: Tabulate winner_counter (equivalent to `tab winner_counter`)
table(michoacan_all$winner_counter)

# Step 8: Count rows where winner_counter == 0 (equivalent to `count if winner_counter == 0`)
count_no_winner <- sum(michoacan_all$winner_counter == 0)

# Print out the count of rows with no winner
cat("Number of rows with no winners: ", count_no_winner, "\n")

# Step 9: Save the final dataset (equivalent to `saveold Michoacan_ALL.dta`)
write_dta(michoacan_all, "Michoacan_ALL.dta")

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 2015 and 2018
# -------------------------------------- #


### First identify coalitions

# Import Excel file with specified range and first row as headers
coalitions <- read_excel("computos_municipales_2015.xlsx", sheet = "Hoja1", range = "A3:BY115")

# Get the number of columns in the dataframe
num_cols <- ncol(coalitions)

# Function to generate column names: A, B, C, ..., Z, AA, AB, AC, ..., AZ, BA, BB, ..., ZZ, AAA, ...
generate_column_names <- function(n) {
  # Create a sequence of names similar to Excel's column naming convention
  col_names <- character(n)
  for (i in 1:n) {
    name <- ""
    num <- i - 1
    while (num >= 0) {
      name <- paste0(LETTERS[(num %% 26) + 1], name)
      num <- (num %/% 26) - 1
    }
    col_names[i] <- name
  }
  return(col_names)
}

# Generate the required column names based on the number of columns
col_names <- generate_column_names(num_cols)

# Assign these column names to the dataframe
colnames(coalitions) <- col_names

# Keep specific columns (MUNICIPIO and AW:BT)
coalitions <- coalitions %>%
  dplyr::select(MUNICIPIO = D, AW:BT)

# Replace "-" with empty strings and convert to numeric
coalitions <- coalitions %>%
  mutate(across(AW:BT, ~ na_if(., "-"))) %>%
  mutate(across(AW:BT, ~ as.numeric(!is.na(.))))

# Rename columns
colnames(coalitions)[2:30] <- c("PAN_PRI_PRD_PANAL_PH_PES", "PAN_PRD_PT_PANAL_PH", "PAN_PRD_PT_PANAL_PES", 
                                "PRD_PT_PANAL_PH", "PRD_PT_PANAL_PES", "PAN_PRI_PVEM", "PAN_PRD_PT", "PRD_PT_PANAL", 
                                "PRD_PT_PH", "PRD_PT_PES", "PRD_PANAL_PES", "PT_PANAL_PH", "PT_PES_PH", "PAN_PRD", 
                                "PAN_PT", "PAN_MC", "PAN_PH", "PRI_PVEM", "PRD_PT", "PRD_PANAL", "PRD_PES", "PT_MC", 
                                "PT_PH", "PT_PES")

# Prefix all columns except MUNICIPIO with "coal_"
coalitions <- coalitions %>%
  rename_with(~ paste0("coal_", .), -MUNICIPIO)

# Standardize MUNICIPIO names
coalitions <- coalitions %>%
  mutate(MUNICIPIO = str_replace_all(MUNICIPIO, c("Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U")),
         MUNICIPIO = case_when(
           MUNICIPIO == "COALCOMAN" ~ "COALCOMAN DE VAZQUEZ PALLARES",
           MUNICIPIO == "TIQUICHEO" ~ "TIQUICHEO DE NICOLAS ROMERO",
           MUNICIPIO == "REGULES" ~ "COJUMATLAN DE REGULES",
           TRUE ~ MUNICIPIO
         ))

# Save the cleaned data as a .dta file
write_dta(coalitions, "coalitions.dta")

### Preprocessed data, now process auxiliary sheets from Excel

# Load sheet names from the Excel file
sheet_names <- excel_sheets("Ayuntamientos_Mich_2015.xlsx")

# Process each sheet and save as .dta
for (sheetname in sheet_names) {
  data <- read_excel("Ayuntamientos_Mich_2015.xlsx", sheet = sheetname) %>%
    filter(!(CVO %in% c("TOTAL", ""))) %>%
    mutate(across(everything(), ~ replace_na(., "0")))
  
  # Save each sheet as a .dta file
  write_dta(data, paste0(sheetname, ".dta"))
}

### Append all processed sheets

# List of all files to append
files_to_append <- c("LAGUNILLAS.dta", "ARTEAGA.dta", "ZIRACUARETIRO.dta", "SUSUPUATO.dta", "TZINTZUNTZAN.dta",
                     "ERONGARICUARO.dta", "TINGAMBATO.dta", "TEPALCATEPEC.dta", "JUAREZ.dta", "Regules.dta", 
                     "TUZANTLA.dta", "CHURINTZIO.dta", "CHURUMUCO.dta", "Periban.dta", "CHINICUILA.dta", 
                     "JUNGAPEO.dta", "Tanhuato.dta", "CHUCANDIRO.dta", "Yurecuaro.dta", "Marcos Castellanos.dta", 
                     "Tangamandapio.dta", "TLAZAZALCA.dta", "INDAPARAPEO.dta", "CHARO.dta", "ANGAMACUTIRO.dta", 
                     "APORO.dta", "Ixtlan.dta", "QUERENDARO.dta", "SAN LUCAS.dta", "IRIMBO.dta", "Copandaro.dta", 
                     "La Huacana.dta", "Huandacareo.dta", "La Piedad.dta", "LOS REYES.dta", "Epitacio huerta.dta", 
                     "TZITZIO.dta", "BRISENAS.dta", "Senguio.dta", "HUIRAMBA.dta", "Jiquilpan.dta", "MORELIA.dta", 
                     "Sahuayo.dta", "Venustiano Carranza.dta", "Vista Hermosa.dta", "Tlalpujahua.dta", 
                     "Pajacuaran.dta", "Chavinda.dta", "Jacona.dta", "TANGANCICUARO.dta", "COENEO.dta", "Villamar.dta", 
                     "BUENAVISTA.dta", "Zamora.dta", "HUANIQUEO.dta", "Jimenez.dta", "APATZINGAN.dta", "PUREPERO.dta", 
                     "Zacapu.dta", "OBREGON.dta", "CHARAPAN.dta", "CUITZEO.dta", "TARIMBARO.dta", "ZINAPECUARO.dta", 
                     "COTIJA.dta", "TANCITARO.dta", "TOCUMBO.dta", "NUEVO URECHO.dta", "ANGANGUEO.dta", 
                     "OCAMPO.dta", "NOCUPETARO.dta", "PATZCUARO.dta", "URUAPAN.dta", "MUGICA.dta", 
                     "NVO PARANGARICUTIRO.dta", "AGUILILLA.dta", "CARACUARO.dta", "SALVADOR ESCALANTE.dta", 
                     "ROMERO.dta", "QUIROGA.dta", "Morelos.dta", "HIDALGO.dta", "Ario.dta", "Aquila.dta", 
                     "Chilchota.dta", "ACUITZIO.dta", "ZITACUARO.dta", "COAHUAYANA.dta", "MADERO.dta", "PARACHO.dta", 
                     "TUXPAN.dta", "TACAMBARO.dta", "COALCOMAN.dta", "Cardenas.dta", "HUETAMO.dta", 
                     "ECUANDUREO.dta", "Nahuatzen.dta", "Jose Sixto Verduzco.dta", "Puruandiro.dta", 
                     "MARAVATIO.dta", "Paracuaro.dta", "Tumbiscatio.dta", "Numaran.dta", "TARETAN.dta", 
                     "TINGUINDIN.dta", "SANTA ANA MAYA.dta", "Zinaparo.dta", "PANINDICUARO.dta", "CONTEPEC.dta", 
                     "GABRIEL ZAMORA.dta", "TURICATO.dta")

# Load the dataset
data <- read_dta("coalitions.dta")

# Drop specific columns
data <- data %>% select(-W, -R, -V, -T, -U, -Q)

# Convert all character columns to numeric where possible (similar to Stata's destring)
data <- data %>%
  mutate(across(everything(), ~ as.numeric(as.character(.)), .names = "destring_{col}"))

# Drop rows where MUNICIPIO is missing or empty
data <- data %>% filter(MUNICIPIO != "")

# Merge with coalitions.dta on MUNICIPIO (m:1 equivalent to a left join)
coalitions <- read_dta("coalitions.dta")
data <- data %>% left_join(coalitions, by = "MUNICIPIO")

# Remove rows where the merge failed (_merge == 2 in Stata means no match in using dataset)
data <- data %>% filter(!is.na(MUNICIPIO))

# Remove the merged indicator
data <- data %>% select(-_merge)

# Remove the coalitions.dta file after merging
file.remove("coalitions.dta")

# Rename columns
data <- data %>%
  rename(municipality = MUNICIPIO, section = SECCIÓN)

# Drop rows where PAN is "N/I"
data <- data %>% filter(PAN != "N/I")

# Convert PAN to numeric (if needed)
data <- data %>%
  mutate(PAN = as.numeric(PAN))

# Drop unnecessary columns
data <- data %>%
  select(-SUMADEVOTOSVALIDOS, -BOLETASENCASILLA, -VOTACIONEMITIDA, -VOTACIONTOTAL)

# Replace party votes with NA based on coalition conditions
data <- data %>%
  mutate(PAN = ifelse(coal_PAN_PRI_PRD_PANAL_PH_PES == 1, NA, PAN),
         PRI = ifelse(coal_PAN_PRI_PRD_PANAL_PH_PES == 1, NA, PRI),
         PRD = ifelse(coal_PAN_PRI_PRD_PANAL_PH_PES == 1, NA, PRD),
         PANAL = ifelse(coal_PAN_PRI_PRD_PANAL_PH_PES == 1, NA, PANAL),
         PH = ifelse(coal_PAN_PRI_PRD_PANAL_PH_PES == 1, NA, PH),
         PES = ifelse(coal_PAN_PRI_PRD_PANAL_PH_PES == 1, NA, PES)) %>%
  select(-coal_PAN_PRI_PRD_PANAL_PH_PES)

# Apply similar conditional replacements for other coalitions
data <- data %>%
  mutate(PAN = ifelse(coal_PAN_PRD_PT_PANAL_PH == 1, NA, PAN),
         PRD = ifelse(coal_PAN_PRD_PT_PANAL_PH == 1, NA, PRD),
         PT = ifelse(coal_PAN_PRD_PT_PANAL_PH == 1, NA, PT),
         PES = ifelse(coal_PAN_PRD_PT_PANAL_PH == 1, NA, PES),
         PANAL = ifelse(coal_PAN_PRD_PT_PANAL_PH == 1, NA, PANAL)) %>%
  select(-coal_PAN_PRD_PT_PANAL_PH)

# Repeat similar replacements for remaining coalitions
data <- data %>%
  mutate(PAN = ifelse(coal_PAN_PRD_PT_PANAL_PES == 1, NA, PAN),
         PRD = ifelse(coal_PAN_PRD_PT_PANAL_PES == 1, NA, PRD),
         PT = ifelse(coal_PAN_PRD_PT_PANAL_PES == 1, NA, PT),
         PES = ifelse(coal_PAN_PRD_PT_PANAL_PES == 1, NA, PES),
         PANAL = ifelse(coal_PAN_PRD_PT_PANAL_PES == 1, NA, PANAL)) %>%
  select(-coal_PAN_PRD_PT_PANAL_PES)

# Continue for other coalitions
data <- data %>%
  mutate(PRD = ifelse(coal_PRD_PT_PANAL_PH == 1, NA, PRD),
         PT = ifelse(coal_PRD_PT_PANAL_PH == 1, NA, PT),
         PH = ifelse(coal_PRD_PT_PANAL_PH == 1, NA, PH),
         PANAL = ifelse(coal_PRD_PT_PANAL_PH == 1, NA, PANAL)) %>%
  select(-coal_PRD_PT_PANAL_PH)

# More coalition conditions
data <- data %>%
  mutate(PRD = ifelse(coal_PRD_PT_PANAL_PES == 1, NA, PRD),
         PANAL = ifelse(coal_PRD_PT_PANAL_PES == 1, NA, PANAL),
         PES = ifelse(coal_PRD_PT_PANAL_PES == 1, NA, PES),
         PT = ifelse(coal_PRD_PT_PANAL_PES == 1, NA, PT)) %>%
  select(-coal_PRD_PT_PANAL_PES)

# Drop other coalition flags after replacements
data <- data %>%
  mutate(PAN = ifelse(coal_PAN_PRI_PVEM == 1, NA, PAN),
         PRI = ifelse(coal_PAN_PRI_PVEM == 1, NA, PRI),
         PVEM = ifelse(coal_PAN_PRI_PVEM == 1, NA, PVEM)) %>%
  select(-coal_PAN_PRI_PVEM)

# Repeat for remaining coalitions
data <- data %>%
  mutate(PAN = ifelse(coal_PAN_PRD_PT == 1, NA, PAN),
         PRD = ifelse(coal_PAN_PRD_PT == 1, NA, PRD),
         PT = ifelse(coal_PAN_PRD_PT == 1, NA, PT)) %>%
  select(-coal_PAN_PRD_PT)

data <- data %>%
  mutate(PRD = ifelse(coal_PRD_PT_PANAL == 1, NA, PRD),
         PT = ifelse(coal_PRD_PT_PANAL == 1, NA, PT),
         PANAL = ifelse(coal_PRD_PT_PANAL == 1, NA, PANAL)) %>%
  select(-coal_PRD_PT_PANAL)

data <- data %>%
  mutate(PRD = ifelse(coal_PRD_PT_PH == 1, NA, PRD),
         PT = ifelse(coal_PRD_PT_PH == 1, NA, PT),
         PH = ifelse(coal_PRD_PT_PH == 1, NA, PH)) %>%
  select(-coal_PRD_PT_PH)

data <- data %>%
  mutate(PRD = ifelse(coal_PRD_PT_PES == 1, NA, PRD),
         PT = ifelse(coal_PRD_PT_PES == 1, NA, PT),
         PES = ifelse(coal_PRD_PT_PES == 1, NA, PES)) %>%
  select(-coal_PRD_PT_PES)

data <- data %>%
  mutate(PRD = ifelse(coal_PRD_PANAL_PES == 1, NA, PRD),
         PANAL = ifelse(coal_PRD_PANAL_PES == 1, NA, PANAL),
         PES = ifelse(coal_PRD_PANAL_PES == 1, NA, PES)) %>%
  select(-coal_PRD_PANAL_PES)

data <- data %>%
  mutate(PT = ifelse(coal_PT_PANAL_PH == 1, NA, PT),
         PANAL = ifelse(coal_PT_PANAL_PH == 1, NA, PANAL),
         PH = ifelse(coal_PT_PANAL_PH == 1, NA, PH)) %>%
  select(-coal_PT_PANAL_PH)

data <- data %>%
  mutate(PT = ifelse(coal_PT_PES_PH == 1, NA, PT),
         PES = ifelse(coal_PT_PES_PH == 1, NA, PES),
         PH = ifelse(coal_PT_PES_PH == 1, NA, PH)) %>%
  select(-coal_PT_PES_PH)

data <- data %>%
  mutate(PAN = ifelse(coal_PAN_PRD == 1, NA, PAN),
         PRD = ifelse(coal_PAN_PRD == 1, NA, PRD)) %>%
  select(-coal_PAN_PRD)

data <- data %>%
  mutate(PAN = ifelse(coal_PAN_PT == 1, NA, PAN),
         PT = ifelse(coal_PAN_PT == 1, NA, PT)) %>%
  select(-coal_PAN_PT)

data <- data %>%
  mutate(PAN = ifelse(coal_PAN_MC == 1, NA, PAN),
         MC = ifelse(coal_PAN_MC == 1, NA, MC)) %>%
  select(-coal_PAN_MC)

data <- data %>%
  mutate(PAN = ifelse(coal_PAN_PH == 1, NA, PAN),
         PH = ifelse(coal_PAN_PH == 1, NA, PH)) %>%
  select(-coal_PAN_PH)

data <- data %>%
  mutate(PRI = ifelse(coal_PRI_PVEM == 1, NA, PRI),
         PVEM = ifelse(coal_PRI_PVEM == 1, NA, PVEM)) %>%
  select(-coal_PRI_PVEM)

data <- data %>%
  mutate(PRD = ifelse(coal_PRD_PT == 1, NA, PRD),
         PT = ifelse(coal_PRD_PT == 1, NA, PT)) %>%
  select(-coal_PRD_PT)

data <- data %>%
  mutate(PRD = ifelse(coal_PRD_PANAL == 1, NA, PRD),
         PANAL = ifelse(coal_PRD_PANAL == 1, NA, PANAL)) %>%
  select(-coal_PRD_PANAL)

data <- data %>%
  mutate(PRD = ifelse(coal_PRD_PES == 1, NA, PRD),
         PES = ifelse(coal_PRD_PES == 1, NA, PES)) %>%
  select(-coal_PRD_PES)

data <- data %>%
  mutate(PT = ifelse(coal_PT_MC == 1, NA, PT),
         MC = ifelse(coal_PT_MC == 1, NA, MC)) %>%
  select(-coal_PT_MC)

data <- data %>%
  mutate(PT = ifelse(coal_PT_PH == 1, NA, PT),
         PH = ifelse(coal_PT_PH == 1, NA, PH)) %>%
  select(-coal_PT_PH)

data <- data %>%
  mutate(PT = ifelse(coal_PT_PES == 1, NA, PT),
         PES = ifelse(coal_PT_PES == 1, NA, PES)) %>%
  select(-coal_PT_PES)

# Convert municipality names to proper case
data <- data %>%
  mutate(municipality = str_to_title(municipality))

# Generate a uniqueid column and replace values based on conditions
data <- data %>%
  mutate(uniqueid = case_when(
    municipality == "Acuitzio" ~ 16001,
    municipality == "Aguililla" ~ 16002,
    municipality == "Alvaro Obregon" ~ 16003,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "Angangueo" ~ 16005,
    municipality == "Apatzingan" ~ 16006,
    municipality == "Aporo" ~ 16007,
    municipality == "Aquila" ~ 16008,
    municipality == "Ario" ~ 16009,
    municipality == "Arteaga" ~ 16010,
    municipality == "Brisenas" | municipality == "BriseÑAs" ~ 16011,
    municipality == "Buenavista" ~ 16012,
    municipality == "Caracuaro" ~ 16013,
    municipality == "Charapan" ~ 16021,
    municipality == "Charo" ~ 16022,
    municipality == "Chavinda" ~ 16023,
    municipality == "Cheran" ~ 16024,
    municipality == "Chilchota" ~ 16025,
    municipality == "Chinicuila" ~ 16026,
    municipality == "Chucandiro" ~ 16027,
    municipality == "Churintzio" ~ 16028,
    municipality == "Churumuco" ~ 16029,
    municipality == "Coahuayana" ~ 16014,
    municipality == "Coalcoman" | municipality == "Coalcoman De Vazquez Pallares" ~ 16015,
    municipality == "Coeneo" ~ 16016,
    municipality == "Regules" | municipality == "Cojumatlan De Regules" ~ 16074,
    municipality == "Contepec" ~ 16017,
    municipality == "Copandaro" ~ 16018,
    municipality == "Cotija" ~ 16019,
    municipality == "Cuitzeo" ~ 16020,
    municipality == "Ecuandureo" ~ 16030,
    municipality == "Epitacio Huerta" ~ 16031,
    municipality == "Erongaricuaro" ~ 16032,
    municipality == "Gabriel Zamora" ~ 16033,
    municipality == "Hidalgo" ~ 16034,
    municipality == "Huandacareo" ~ 16036,
    municipality == "Huaniqueo" ~ 16037,
    municipality == "Huetamo" ~ 16038,
    municipality == "Huiramba" ~ 16039,
    municipality == "Indaparapeo" ~ 16040,
    municipality == "Irimbo" ~ 16041,
    municipality == "Ixtlan" ~ 16042,
    municipality == "Jacona" ~ 16043,
    municipality == "Jimenez" ~ 16044,
    municipality == "Jiquilpan" ~ 16045,
    municipality == "Jose Sixto Verduzco" ~ 16113,
    municipality == "Juarez" ~ 16046,
    municipality == "Jungapeo" ~ 16047,
    municipality == "La Huacana" ~ 16035,
    municipality == "La Piedad" ~ 16069,
    municipality == "Lagunillas" ~ 16048,
    municipality == "Lazaro Cardenas" ~ 16052,
    municipality == "Los Reyes" ~ 16075,
    municipality == "Madero" ~ 16049,
    municipality == "Maravatio" ~ 16050,
    municipality == "Marcos Castellanos" ~ 16051,
    municipality == "Morelia" ~ 16053,
    municipality == "Morelos" ~ 16054,
    municipality == "Mugica" ~ 16055,
    municipality == "Nahuatzen" ~ 16056,
    municipality == "Nocupetaro" ~ 16057,
    municipality == "Nuevo Parangaricutiro" ~ 16058,
    municipality == "Nuevo Urecho" ~ 16059,
    municipality == "Numaran" ~ 16060,
    municipality == "Ocampo" ~ 16061,
    municipality == "Pajacuaran" ~ 16062,
    municipality == "Panindicuaro" ~ 16063,
    municipality == "Paracho" ~ 16065,
    municipality == "Paracuaro" ~ 16064,
    municipality == "Patzcuaro" ~ 16066,
    municipality == "Penjamillo" ~ 16067,
    municipality == "Periban" ~ 16068,
    municipality == "Purepero" ~ 16070,
    municipality == "Puruandiro" ~ 16071,
    municipality == "Querendaro" ~ 16072,
    municipality == "Quiroga" ~ 16073,
    municipality == "Sahuayo" ~ 16076,
    municipality == "Salvador Escalante" ~ 16079,
    municipality == "San Lucas" ~ 16077,
    municipality == "Santa Ana Maya" ~ 16078,
    municipality == "Senguio" ~ 16080,
    municipality == "Susupuato" ~ 16081,
    municipality == "Tacambaro" ~ 16082,
    municipality == "Tancitaro" ~ 16083,
    municipality == "Tangamandapio" ~ 16084,
    municipality == "Tangancicuaro" ~ 16085,
    municipality == "Tanhuato" ~ 16086,
    municipality == "Taretan" ~ 16087,
    municipality == "Tarimbaro" ~ 16088,
    municipality == "Tepalcatepec" ~ 16089,
    municipality == "Tingambato" ~ 16090,
    municipality == "Tinguindin" | municipality == "TingÜIndin" ~ 16091,
    municipality == "Tiquicheo" | municipality == "Tiquicheo De Nicolas Romero" ~ 16092,
    municipality == "Tlalpujahua" ~ 16093,
    municipality == "Tlazazalca" ~ 16094,
    municipality == "Tocumbo" ~ 16095,
    municipality == "Tumbiscatio" ~ 16096,
    municipality == "Turicato" ~ 16097,
    municipality == "Tuxpan" ~ 16098,
    municipality == "Tuzantla" ~ 16099,
    municipality == "Tzintzuntzan" ~ 16100,
    municipality == "Tzitzio" ~ 16101,
    municipality == "Uruapan" ~ 16102,
    municipality == "Venustiano Carranza" ~ 16103,
    municipality == "Villamar" ~ 16104,
    municipality == "Vista Hermosa" ~ 16105,
    municipality == "Yurecuaro" ~ 16106,
    municipality == "Zacapu" ~ 16107,
    municipality == "Zamora" ~ 16108,
    municipality == "Zinaparo" ~ 16109,
    municipality == "Zinapecuaro" ~ 16110,
    municipality == "Ziracuaretiro" ~ 16111,
    municipality == "Zitacuaro" ~ 16112,
    TRUE ~ uniqueid # Keep existing uniqueid where no condition matches
  ))

# Convert the municipality names to upper case
data <- data %>%
  mutate(municipality = str_to_upper(municipality))

# Drop the columns SUMATOTALDEVOTOS and VOTACIÓNEMITIDA
data <- data %>% select(-SUMATOTALDEVOTOS, -VOTACIÓNEMITIDA)

# Collapse (sum) operation for variables PAN to PAN_PRD_PT_PANAL_PH, grouped by municipality, uniqueid, and section
collapsed_data <- data %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(starts_with("PAN"), sum, na.rm = TRUE))

# Replace CI with the sum of CI and CI_1, then drop CI_1
collapsed_data <- collapsed_data %>%
  mutate(CI = CI + CI_1) %>%
  select(-CI_1)

# Rename CI to CI_1
collapsed_data <- collapsed_data %>%
  rename(CI_1 = CI)

# Load necessary libraries
library(dplyr)
library(haven)   # For reading/writing Stata .dta files
library(stringr) # For string manipulation

# Calculate the sum of votes across parties (similar to Stata's rowtotal)
data <- data %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, MC, PANAL, MORENA, PH, PES, PRI_PVEM, 
                                PRD_PT_PANAL, PRD_PT_PANAL_PES, PRD_PANAL, PRD_PANAL_PES, PT_PANAL_PH, 
                                PAN_PH, PAN_MC, PAN_PRD_PT, PRD_PES, PRD_PT_PES, PT_PES_PH, CI_1, 
                                PT_PES, PRD_PT_PANAL_PH, PRD_PT_PH, PT_MC, PT_PH, PRD_PT, PAN_PT, 
                                PAN_PRI_PRD_PANAL_PH_PES, PAN_PRD, PAN_PRD_PT_PANAL_PES, PAN_PRI_PVEM, 
                                PAN_PRD_PT_PANAL_PH), na.rm = TRUE))

# Generate the total votes (valid + VOTOSNULOS + NOREGISTRADOS)
data <- data %>%
  mutate(total = valid + VOTOSNULOS + NOREGISTRADOS)

# Drop the columns NOREGISTRADOS and VOTOSNULOS
data <- data %>%
  select(-NOREGISTRADOS, -VOTOSNULOS)

# Calculate municipal-level sums for each party variable, create inverse, and rank
party_vars <- c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "MORENA", "PH", "PES", "PRI_PVEM",
                "PRD_PT_PANAL", "PRD_PT_PANAL_PES", "PRD_PANAL", "PRD_PANAL_PES", "PT_PANAL_PH", 
                "PAN_PH", "PAN_MC", "PAN_PRD_PT", "PRD_PES", "PRD_PT_PES", "PT_PES_PH", "CI_1", 
                "PT_PES", "PRD_PT_PANAL_PH", "PRD_PT_PH", "PT_MC", "PT_PH", "PRD_PT", "PAN_PT", 
                "PAN_PRI_PRD_PANAL_PH_PES", "PAN_PRD", "PAN_PRD_PT_PANAL_PES", "PAN_PRI_PVEM", 
                "PAN_PRD_PT_PANAL_PH", "total", "valid")

# For each variable, create municipal sums and inverse values
for (var in party_vars) {
  data <- data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    mutate(!!paste0("i_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Calculate the ranks
rank_vars <- paste0("i_", party_vars)
ranked_data <- data %>%
  rowwise() %>%
  mutate(across(all_of(rank_vars), rank, .names = "{col}_r"))

# Drop the intermediate inverse variables
data <- data %>%
  select(-starts_with("i_"))

# Initialize empty winner, second, third columns
data <- data %>%
  mutate(winner = "", second = "", third = "")

# Assign winner, second, and third based on the ranks
for (var in party_vars) {
  data <- data %>%
    mutate(winner = ifelse(get(paste0(var, "_r")) == 1, var, winner),
           second = ifelse(get(paste0(var, "_r")) == 2, var, second),
           third = ifelse(get(paste0(var, "_r")) == 3, var, third))
}

# Drop the rank variables
data <- data %>%
  select(-ends_with("_r"))

# Replace winners with "Independent" where appropriate
data <- data %>%
  mutate(winner = ifelse(winner == "CI_1", "Independent", winner),
         second = ifelse(second == "CI_1", "Independent", second),
         third = ifelse(third == "CI_1", "Independent", third))

# Add year, month, and STATE columns
data <- data %>%
  mutate(year = 2015, month = "June", STATE = "MICHOACAN")

# Load "LN2015.dta" to get the list of nominal voters (election roll)
LN2015 <- read_dta("..\Listas Nominales\LN 2012-2019\2015\LN2015.dta") %>%
  filter(entidad == 16 & month == 6) %>%
  mutate(uniqueid = (entidad * 1000) + municipio) %>%
  filter(seccion != 0) %>%
  select(uniqueid, section = seccion, lista)

# Save the processed list nominal data
write_dta(LN2015, "LN15_MICH.dta")

# Merge the electoral data with the list nominal data on section
data <- data %>%
  left_join(LN2015, by = "section") %>%
  filter(!is.na(lista))

# Drop the "_merge" column and remove the LN15_MICH.dta file
file.remove("LN15_MICH.dta")

# Rename 'lista' to 'listanominal'
data <- data %>%
  rename(listanominal = lista)

# Calculate the municipal-level sum of 'listanominal'
data <- data %>%
  group_by(uniqueid) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE))

# Calculate the municipal and section-level turnout
data <- data %>%
  mutate(mun_turnout = mun_total / mun_listanominal,
         turnout = total / listanominal)

# Save the final dataset
write_dta(data, "Michoacan_Section_2015.dta")

# Collapse to get the first winner for each municipality and uniqueid
collapsed_data <- data %>%
  group_by(municipality, uniqueid) %>%
  summarise(winner = first(winner))

# Special case for uniqueid 16076
collapsed_data <- collapsed_data %>%
  mutate(winner = ifelse(uniqueid == 16076, "PAN_PRD_PANAL", winner))

# Rename 'winner' to 'incumbent'
collapsed_data <- collapsed_data %>%
  rename(incumbent = winner)

# Save the incumbents data for 2018
write_dta(collapsed_data, "incumbents2018.dta")

# Load the data from "Michoacan_Section_2015.dta"
data <- read_dta("Michoacan_Section_2015.dta")

# Rearrange columns with STATE, municipality, and uniqueid at the front
data <- data %>%
  select(STATE, municipality, uniqueid, everything())

# Collapse (sum) the specified columns by municipality and uniqueid
collapsed_data <- data %>%
  group_by(municipality, uniqueid) %>%
  summarise(across(c(PAN:total, listanominal), sum, na.rm = TRUE),       # Sum variables PAN to total and listanominal
            STATE = first(STATE),                                       # Take the first occurrence of STATE
            year = first(year),                                         # Take the first occurrence of year
            month = first(month),                                       # Take the first occurrence of month
            winner = first(winner),                                     # Take the first occurrence of winner
            second = first(second),                                     # Take the first occurrence of second
            third = first(third),                                       # Take the first occurrence of third
            mun_turnout = first(mun_turnout))                           # Take the first occurrence of mun_turnout

# Rename 'mun_turnout' to 'turnout'
collapsed_data <- collapsed_data %>%
  rename(turnout = mun_turnout)

# Sort the data by uniqueid
collapsed_data <- collapsed_data %>%
  arrange(uniqueid)

# Save the collapsed data to a new file "Michoacan_2015.dta"
write_dta(collapsed_data, "..\\..\\Update Municipal\\Michoacan_2015.dta")

# List of files to delete
files_to_delete <- c("LAGUNILLAS.dta", "ARTEAGA.dta", "ZIRACUARETIRO.dta", "SUSUPUATO.dta", "TZINTZUNTZAN.dta", 
                     "ERONGARICUARO.dta", "TINGAMBATO.dta", "TEPALCATEPEC.dta", "JUAREZ.dta", "Regules .dta", 
                     "TUZANTLA.dta", "CHURINTZIO.dta", "CHURUMUCO.dta", "Periban.dta", "CHINICUILA.dta", 
                     "JUNGAPEO.dta", "Tanhuato.dta", "CHUCANDIRO.dta", "Yurecuaro.dta", "Marcos Castellanos.dta", 
                     "Tangamandapio.dta", "TLAZAZALCA.dta", "INDAPARAPEO.dta", "CHARO.dta", "ANGAMACUTIRO.dta", 
                     "APORO.dta", "Ixtlan.dta", "QUERENDARO.dta", "SAN LUCAS.dta", "IRIMBO.dta", "Copandaro.dta", 
                     "La Huacana.dta", "Huandacareo.dta", "La Piedad.dta", "LOS REYES.dta", "Epitacio huerta.dta", 
                     "TZITZIO.dta", "BRISEÑAS.dta", "Senguio.dta", "HUIRAMBA.dta", "Jiquilpan.dta", "MORELIA.dta", 
                     "Sahuayo.dta", "Venustiano Carranza.dta", "Vista Hermosa.dta", "Tlalpujahua.dta", "Pajacuaran.dta", 
                     "Chavinda.dta", "Jacona.dta", "TANGANCICUARO.dta", "COENEO.dta", "Villamar.dta", "BUENAVISTA.dta", 
                     "Zamora.dta", "HUANIQUEO.dta", "Jimenez.dta", "APATZINGAN.dta", "PUREPERO.dta", "Zacapu.dta", 
                     "OBREGON.dta", "CHARAPAN.dta", "CUITZEO.dta", "TARIMBARO.dta", "ZINAPECUARO.dta", "COTIJA.dta", 
                     "TANCITARO.dta", "TOCUMBO.dta", "NUEVO URECHO.dta", "ANGANGUEO.dta", "OCAMPO.dta", 
                     "NOCUPETARO.dta", "PATZCUARO.dta", "URUAPAN.dta", "MUGICA.dta", "NVO PARANGARICUTIRO.dta", 
                     "AGUILILLA.dta", "CARACUARO.dta", "SALVADOR ESCALANTE.dta", "ROMERO.dta", "QUIROGA.dta", 
                     "Morelos.dta", "HIDALGO.dta", "Ario.dta", "Aquila.dta", "Chilchota.dta", "ACUITZIO.dta", 
                     "ZITACUARO.dta", "COAHUAYANA.dta", "MADERO.dta", "PARACHO.dta", "TUXPAN.dta", "TACAMBARO.dta", 
                     "COALCOMAN.dta", "Cardenas.dta", "HUETAMO.dta", "ECUANDUREO.dta", "Nahuatzen.dta", 
                     "Jose Sixto Verduzco.dta", "Puruandiro.dta", "MARAVATIO.dta", "Paracuaro.dta", 
                     "Tumbiscatio.dta", "Numaran.dta", "TARETAN.dta", "TINGÜINDIN.dta", "SANTA ANA MAYA.dta", 
                     "Zinaparo.dta", "PANINDICUARO.dta", "CONTEPEC.dta", "GABRIEL ZAMORA.dta", "TURICATO.dta")

# Remove files
file.remove(files_to_delete)

# Read the Excel sheet "COMPUTO FINAL" and the specified cell range (A9:AE101)
data <- read_excel("SAHUAYO ELECCION EXTRAORDINARIA.xlsx", sheet = "COMPUTO FINAL", range = "A9:AE101")

# Rename columns
data <- data %>%
  rename(section = E,         # Rename column E to section
         listanominal = G,    # Rename column G to listanominal
         total = AC)          # Rename column AC to total

# Add constant columns for municipality and uniqueid
data <- data %>%
  mutate(municipality = "SAHUAYO EXTRAORDINARIO", uniqueid = 16076)

# Drop rows where 'total' is NA or 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Rename additional columns
data <- data %>%
  rename(PAN_PRD_PANAL = U, PRI_PT_PVEM = Z, MC = M, MORENA = O, PES = P)

# Collapse data by summing across columns by uniqueid, municipality, and section
collapsed_data <- data %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(c(listanominal, PAN_PRD_PANAL, PRI_PT_PVEM, MC, MORENA, PES, total), sum, na.rm = TRUE))

# Generate turnout as total / listanominal
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal)

# Calculate the valid votes as row totals
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(select(., PAN_PRD_PANAL, PRI_PT_PVEM, MC, MORENA, PES), na.rm = TRUE))

# Calculate municipal-level sums and inverses
vars_to_process <- c("PAN_PRD_PANAL", "PRI_PT_PVEM", "MC", "MORENA", "PES", "total", "listanominal", "valid")

for (var in vars_to_process) {
  collapsed_data <- collapsed_data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Calculate the municipal-level turnout
collapsed_data <- collapsed_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse values
collapsed_data <- collapsed_data %>%
  mutate(PAN_PRD_PANAL_r = rank(-inv_mun_PAN_PRD_PANAL),
         PRI_PT_PVEM_r = rank(-inv_mun_PRI_PT_PVEM),
         MC_r = rank(-inv_mun_MC),
         MORENA_r = rank(-inv_mun_MORENA),
         PES_r = rank(-inv_mun_PES))

# Drop the inverse variables
collapsed_data <- collapsed_data %>%
  select(-starts_with("inv_mun_"))

# Generate the 'winner' column based on the ranks
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    PAN_PRD_PANAL_r == 1 ~ "PAN_PRD_PANAL",
    PRI_PT_PVEM_r == 1 ~ "PRI_PT_PVEM",
    MC_r == 1 ~ "MC",
    MORENA_r == 1 ~ "MORENA",
    PES_r == 1 ~ "PES",
    TRUE ~ winner
  ))

# Drop the ranking columns
collapsed_data <- collapsed_data %>%
  select(-ends_with("_r"))

# Add year and month columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2015, month = "December")

# Save the final dataset as a .dta file
write_dta(collapsed_data, "Michoacan_Section_2015_EXTRAORDINARIO.dta")

# Read the description of the Excel file (this gives an overview of the sheets in the workbook)
excel_sheets("Ayuntamientos_Mich_2018.xlsx")

# Loop through all sheets
sheet_list <- excel_sheets("Ayuntamientos_Mich_2018.xlsx")
data_combined <- data.frame()  # Initialize an empty dataframe

for (sheet in sheet_list) {
  # Read each sheet into a dataframe
  data <- read_excel("Ayuntamientos_Mich_2018.xlsx", sheet = sheet, col_names = TRUE)
  
  # Drop rows where ID_MUNICIPIO is empty
  data <- data %>%
    filter(!is.na(ID_MUNICIPIO) & ID_MUNICIPIO != "")
  
  # Replace empty cells with "0"
  data[data == ""] <- "0"
  
  # Append the processed data from each sheet into a combined dataframe
  data_combined <- bind_rows(data_combined, data)
  
  # Optionally save each sheet as a .dta file (optional step, based on Stata code)
  write_dta(data, paste0(sheet, ".dta"))
}

# Clear data (not applicable in R, as we work in environments and do not need to explicitly clear memory)
rm(data)

# Now the combined data will be processed for assigning municipality names and unique IDs
data_combined <- data_combined %>%
  mutate(municipality = tools::toTitleCase(MUNICIPIO))  # Convert municipality names to proper case

# Generate unique ID based on municipality names
data_combined <- data_combined %>%
  mutate(uniqueid = case_when(
    municipality == "Acuitzio" ~ 16001,
    municipality == "Aguililla" ~ 16002,
    municipality == "Álvaro Obregón" ~ 16003,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "Angangueo" ~ 16005,
    municipality == "Apatzingán" ~ 16006,
    municipality == "Áporo" ~ 16007,
    municipality == "Aquila" ~ 16008,
    municipality == "Ario" ~ 16009,
    municipality == "Arteaga" ~ 16010,
    municipality == "Brisenas" | municipality == "BriseÑAs" ~ 16011,
    municipality == "Buenavista" ~ 16012,
    municipality == "Carácuaro" ~ 16013,
    municipality == "Charapan" ~ 16021,
    municipality == "Charo" ~ 16022,
    municipality == "Chavinda" ~ 16023,
    municipality == "Cheran" ~ 16024,
    municipality == "Chilchota" ~ 16025,
    municipality == "Chinicuila" ~ 16026,
    municipality == "Chucándiro" ~ 16027,
    municipality == "Churintzio" ~ 16028,
    municipality == "Churumuco" ~ 16029,
    municipality == "Coahuayana" ~ 16014,
    municipality == "Coalcomán De Vazquez Pallares" | municipality == "Coalcomán" ~ 16015,
    municipality == "Coeneo" ~ 16016,
    municipality == "Cojumatlán De Régules" | municipality == "Regules" ~ 16074,
    municipality == "Contepec" ~ 16017,
    municipality == "Copándaro" ~ 16018,
    municipality == "Cotija" ~ 16019,
    municipality == "Cuitzeo" ~ 16020,
    municipality == "Ecuandureo" ~ 16030,
    municipality == "Epitacio Huerta" ~ 16031,
    municipality == "Erongarícuaro" ~ 16032,
    municipality == "Gabriel Zamora" ~ 16033,
    municipality == "Hidalgo" ~ 16034,
    municipality == "Huandacareo" ~ 16036,
    municipality == "Huaníqueo" ~ 16037,
    municipality == "Huetamo" ~ 16038,
    municipality == "Huiramba" ~ 16039,
    municipality == "Indaparapeo" ~ 16040,
    municipality == "Irimbo" ~ 16041,
    municipality == "Ixtlán" ~ 16042,
    municipality == "Jacona" ~ 16043,
    municipality == "Jiménez" ~ 16044,
    municipality == "Jiquilpan" ~ 16045,
    municipality == "José Sixto Verduzco" ~ 16113,
    municipality == "Juárez" ~ 16046,
    municipality == "Jungapeo" ~ 16047,
    municipality == "La Huacana" ~ 16035,
    municipality == "La Piedad" ~ 16069,
    municipality == "Lagunillas" ~ 16048,
    municipality == "Lázaro Cárdenas" ~ 16052,
    municipality == "Los Reyes" ~ 16075,
    municipality == "Madero" ~ 16049,
    municipality == "Maravatío" ~ 16050,
    municipality == "Marcos Castellanos" ~ 16051,
    municipality == "Morelia" ~ 16053,
    municipality == "Morelos" ~ 16054,
    municipality == "Múgica" ~ 16055,
    municipality == "Nahuatzen" ~ 16056,
    municipality == "Nocupétaro" ~ 16057,
    municipality == "Nuevo Parangaricutiro" ~ 16058,
    municipality == "Nuevo Urecho" ~ 16059,
    municipality == "Numarán" ~ 16060,
    municipality == "Ocampo" ~ 16061,
    municipality == "Pajacuarán" ~ 16062,
    municipality == "Panindícuaro" ~ 16063,
    municipality == "Paracho" ~ 16065,
    municipality == "Parácuaro" ~ 16064,
    municipality == "Pátzcuaro" ~ 16066,
    municipality == "Penjamillo" ~ 16067,
    municipality == "Peribán" ~ 16068,
    municipality == "Purépero" ~ 16070,
    municipality == "Puruándiro" ~ 16071,
    municipality == "Queréndaro" ~ 16072,
    municipality == "Quiroga" ~ 16073,
    municipality == "Sahuayo" ~ 16076,
    municipality == "Salvador Escalante" ~ 16079,
    municipality == "San Lucas" ~ 16077,
    municipality == "Santa Ana Maya" ~ 16078,
    municipality == "Senguio" ~ 16080,
    municipality == "Susupuato" ~ 16081,
    municipality == "Tacámbaro" ~ 16082,
    municipality == "Tancítaro" ~ 16083,
    municipality == "Tangamandapio" ~ 16084,
    municipality == "Tangancícuaro" ~ 16085,
    municipality == "Tanhuato" ~ 16086,
    municipality == "Taretan" ~ 16087,
    municipality == "Tarímbaro" ~ 16088,
    municipality == "Tepalcatepec" ~ 16089,
    municipality == "Tingambato" ~ 16090,
    municipality == "Tinguindín" | municipality == "Tingüindín" ~ 16091,
    municipality == "Tiquicheo" | municipality == "Tiquicheo De Nicolás Romero" ~ 16092,
    municipality == "Tlalpujahua" ~ 16093,
    municipality == "Tlazazalca" ~ 16094,
    municipality == "Tocumbo" ~ 16095,
    municipality == "Tumbiscatío" ~ 16096,
    municipality == "Turicato" ~ 16097,
    municipality == "Tuxpan" ~ 16098,
    municipality == "Tuzantla" ~ 16099,
    municipality == "Tzintzuntzan" ~ 16100,
    municipality == "Tzitzio" ~ 16101,
    municipality == "Uruapan" ~ 16102,
    municipality == "Venustiano Carranza" ~ 16103,
    municipality == "Villamar" ~ 16104,
    municipality == "Vista Hermosa" ~ 16105,
    municipality == "Yurécuaro" ~ 16106,
    municipality == "Zacapu" ~ 16107,
    municipality == "Zamora" ~ 16108,
    municipality == "Zináparo" ~ 16109,
    municipality == "Zinapécuaro" ~ 16110,
    municipality == "Ziracuaretiro" ~ 16111,
    municipality == "Zitácuaro" ~ 16112))

# Save the final data as a .dta file
write_dta(data_combined, "Ayuntamientos_Mich_2018_processed.dta")

# Create new variables and perform calculations
data <- data %>%
  mutate(
    PAN_PRD_MC = ifelse(!is.na(CO_PAN_PRD_PMC), CO_PAN_PRD_PMC, 0) + 
      ifelse(!is.na(CO_PAN_PRD), CO_PAN_PRD, 0) + 
      ifelse(!is.na(CO_PAN_PMC), CO_PAN_PMC, 0) + 
      ifelse(!is.na(CO_PRD_PMC), CO_PRD_PMC, 0) + 
      PAN + MC + PRD,
    PAN = ifelse(!is.na(CO_PAN_PRD_PMC), NA, PAN),
    MC = ifelse(!is.na(CO_PAN_PRD_PMC), NA, MC),
    PRD = ifelse(!is.na(CO_PAN_PRD_PMC), NA, PRD),
    coalfrente = ifelse(!is.na(CO_PAN_PRD_PMC), 1, 0)
  ) %>%
  select(-c(CO_PAN_PRD_PMC, CO_PAN_PRD, CO_PAN_PMC, CO_PRD_PMC))

# Continue with similar transformations for other coalitions
data <- data %>%
  mutate(
    PAN_PMC = ifelse(!is.na(CC_PAN_PMC), CC_PAN_PMC, 0) + PAN + MC,
    PAN = ifelse(!is.na(CC_PAN_PMC), 0, PAN),
    MC = ifelse(!is.na(CC_PAN_PMC), 0, MC),
    coalpanmc = ifelse(!is.na(CC_PAN_PMC), 1, 0)
  ) %>%
  select(-CC_PAN_PMC) %>%
  
  mutate(
    PAN_PRD = ifelse(!is.na(CC_PAN_PRD), CC_PAN_PRD, 0) + PAN + PRD,
    PAN = ifelse(!is.na(CC_PAN_PRD), 0, PAN),
    PRD = ifelse(!is.na(CC_PAN_PRD), 0, PRD),
    coalpanprd = ifelse(!is.na(CC_PAN_PRD), 1, 0)
  ) %>%
  select(-CC_PAN_PRD) %>%
  
  mutate(
    PRD_PMC = ifelse(!is.na(CC_PRD_PMC), CC_PRD_PMC, 0) + PRD + MC,
    PRD = ifelse(!is.na(CC_PRD_PMC), 0, PRD),
    MC = ifelse(!is.na(CC_PRD_PMC), 0, MC),
    coalprdmc = ifelse(!is.na(CC_PRD_PMC), 1, 0)
  ) %>%
  select(-CC_PRD_PMC) %>%
  
  mutate(
    PRD_PVEM = ifelse(!is.na(CC_PRD_PVEM), CC_PRD_PVEM, 0) + PRD + PVEM,
    PRD = ifelse(!is.na(CC_PRD_PVEM), 0, PRD),
    PVEM = ifelse(!is.na(CC_PRD_PVEM), 0, PVEM),
    coalprdpvem = ifelse(!is.na(CC_PRD_PVEM), 1, 0)
  ) %>%
  select(-CC_PRD_PVEM) %>%
  
  mutate(
    PT_MORENA = ifelse(!is.na(CO_PT_MORENA), CO_PT_MORENA, 0) + PT + MORENA,
    PT = ifelse(!is.na(CO_PT_MORENA), 0, PT),
    MORENA = ifelse(!is.na(CO_PT_MORENA), 0, MORENA),
    coalptmorena = ifelse(!is.na(CO_PT_MORENA), 1, 0)
  ) %>%
  select(-CO_PT_MORENA)

# Renaming variables
data <- data %>%
  rename(
    section = SECCION,
    PANAL = PNA
  )

# Rearranging columns (R doesn't need explicit column ordering for most cases)
# Perform the collapse operations
# Summing columns by municipality, section, and uniqueid
collapsed_data <- data %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(c(PAN:CI_2), sum, na.rm = TRUE),
            across(starts_with("coal"), first, na.rm = TRUE))

# Calculate the valid row total
collapsed_data <- collapsed_data %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:CI_2), na.rm = TRUE),
         total = valid + NULO + NOREG) %>%
  ungroup() %>%
  select(-NULO, -NOREG)

# Calculate municipal sums
foreach_vars <- c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "MORENA", "PES", "PAN_PRD_MC", 
                  "PT_MORENA", "PAN_MC", "PRD_PVEM", "PAN_PRD", "PRD_MC", "CI_1", "CI_2", "total", "valid")

for (var in foreach_vars) {
  collapsed_data <- collapsed_data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE),
           !!paste0("i_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Ranking the candidates
collapsed_data <- collapsed_data %>%
  mutate_at(vars(starts_with("i_")), funs(row_number())) %>%
  rename_with(~paste0(., "_r"), starts_with("i_")) %>%
  ungroup() %>%
  select(-starts_with("i_"))

# Determine winner, second, and third place
collapsed_data <- collapsed_data %>%
  mutate(winner = "", second = "", third = "")

for (var in foreach_vars[-c(length(foreach_vars)-1, length(foreach_vars))]) {
  collapsed_data <- collapsed_data %>%
    mutate(winner = ifelse(get(paste0(var, "_r")) == 1, var, winner),
           second = ifelse(get(paste0(var, "_r")) == 2, var, second),
           third = ifelse(get(paste0(var, "_r")) == 3, var, third))
}

# Replacing independent candidate results
collapsed_data <- collapsed_data %>%
  mutate(
    winner = ifelse(winner == "CI_1", "Independent", winner),
    second = ifelse(second == "CI_1", "Independent", second),
    third = ifelse(third == "CI_1", "Independent", third),
    year = 2018,
    month = "July",
    STATE = "MICHOACAN"
  )

# Merging with incumbents data
incumbents <- read_dta("incumbents2018.dta")
merged_data <- merge(collapsed_data, incumbents, by = "uniqueid", all.x = TRUE)

# Post-merge adjustments
merged_data <- merged_data %>%
  mutate(incumbent = ifelse(municipality == "Sahuayo", "PAN_PRD_PANAL", incumbent),
         incumbent_vote = NA_real_)

# Saving the results
write_dta(merged_data, "Michoacan_Section_2018.dta")

# Replace incumbent votes based on specific conditions
data <- data %>%
  mutate(
    incumbent_vote = case_when(
      uniqueid == 16082 ~ "PRI",  # Megacoalition PAN_PRI_PRD_PANAL_PH_PES in Tacambaro
      uniqueid == 16056 ~ "PRD_PVEM",  # Megacoalition PAN_PRD_PT_PANAL_PES in Nahuatzen
      incumbent == "Independent" ~ "CI_1",  # Incumbent independent mayor in Morelia
      (incumbent == "PAN" | grepl("PAN_", incumbent) | incumbent == "PRD" | incumbent == "MC") & coalfrente == 1 ~ "PAN_PRD_MC",
      incumbent == "PAN" & coalfrente == 0 & coalpanmc == 1 ~ "PAN_MC",
      incumbent == "PAN" & coalfrente == 0 & coalpanprd == 1 ~ "PAN_PRD",
      incumbent == "PAN" & coalfrente == 0 ~ "PAN",
      incumbent == "PRD" & coalfrente == 0 & coalprdpvem == 1 ~ "PRD_PVEM",
      incumbent == "PRD" & coalfrente == 0 & coalprdmc == 1 ~ "PRD_MC",
      incumbent == "PRD" & coalfrente == 0 & coalpanprd == 1 ~ "PAN_PRD",
      incumbent == "PRD" & coalfrente == 0 & coalprdpvem == 0 & coalprdmc == 0 ~ "PRD",
      incumbent == "MC" & coalfrente == 0 & coalprdmc == 1 & coalpanmc == 1 ~ "MC",
      incumbent == "MC" & coalpanmc == 1 ~ "PAN_MC",
      incumbent == "MC" & coalprdmc == 1 ~ "PRD_MC",
      incumbent == "PRI" ~ "PRI",
      incumbent == "PVEM" & coalprdpvem == 0 ~ "PVEM",
      incumbent == "PVEM" & coalprdpvem == 1 ~ "PRD_PVEM",
      incumbent == "PT" ~ "PT",
      coalptmorena == 1 & (incumbent == "MORENA" | incumbent == "PT") ~ "PT_MORENA",
      incumbent == "PANAL" ~ "PANAL",
      TRUE ~ incumbent_vote  # Keep existing incumbent_vote if no condition matches
    )
  )

# Replace with the max value for coalitions using row-wise maximum
data <- data %>%
  rowwise() %>%
  mutate(
    maxpanprdpanal = max(c_across(c(PAN, PAN_PRD_MC, PAN_MC, PAN_PRD, PRD_MC, PRD_PVEM, PRD, PANAL)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PAN_PRD_PANAL", maxpanprdpanal, incumbent_vote),
    
    maxpanprdptpanalph = max(c_across(c(PAN, PAN_PRD_MC, PAN_MC, PAN_PRD, PRD_MC, PRD_PVEM, PRD, PANAL, PT_MORENA, PT)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PAN_PRD_PT_PANAL_PH", maxpanprdptpanalph, incumbent_vote),
    
    maxprdpanal = max(c_across(c(PRD, PAN_PRD_MC, PAN_PRD, PRD_MC, PRD_PVEM, PANAL)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PRD_PANAL", maxprdpanal, incumbent_vote),
    
    maxprdpes = max(c_across(c(PRD, PAN_PRD_MC, PAN_PRD, PRD_MC, PRD_PVEM, PES)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PRD_PES", maxprdpes, incumbent_vote),
    
    maxprdpanalpes = max(c_across(c(PRD, PAN_PRD_MC, PAN_PRD, PRD_MC, PRD_PVEM, PANAL, PES)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PRD_PANAL_PES", maxprdpanalpes, incumbent_vote),
    
    maxprdpt = max(c_across(c(PRD, PAN_PRD_MC, PAN_PRD, PRD_MC, PRD_PVEM, PT)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PRD_PT", maxprdpt, incumbent_vote),
    
    maxprdptpanal = max(c_across(c(PRD, PAN_PRD_MC, PAN_PRD, PRD_MC, PRD_PVEM, PT, PANAL)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PRD_PT_PANAL", maxprdptpanal, incumbent_vote),
    
    maxprdptpes = max(c_across(c(PRD, PAN_PRD_MC, PAN_PRD, PRD_MC, PRD_PVEM, PT, PES)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PRD_PT_PES", maxprdptpes, incumbent_vote),
    
    maxprdptph = max(c_across(c(PRD, PAN_PRD_MC, PAN_PRD, PRD_MC, PRD_PVEM, PT)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PRD_PT_PH", maxprdptph, incumbent_vote),
    
    maxpripvem = max(c_across(c(PRI, PVEM)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PRI_PVEM", maxpripvem, incumbent_vote),
    
    maxptpes = max(c_across(c(PT, PT_MORENA, PES)), na.rm = TRUE),
    incumbent_vote = ifelse(incumbent == "PT_PES", maxptpes, incumbent_vote)
  ) %>%
  ungroup() %>%
  select(-starts_with("max"))

# Drop coalition variables
data <- data %>%
  select(-starts_with("coal"))

# Loading and merging with nominal data
ln_data <- read_dta("../Listas Nominales/ListadoNominalPREP2018.dta") %>%
  filter(STATE == "MICHOACAN")

# Save the Listado Nominal
write_dta(ln_data, "MICH_LN18.dta")

# Merge and post-processing
data <- data %>%
  left_join(ln_data, by = c("STATE", "section")) %>%
  mutate(
    turnout = total / listanominal,
    mun_listanominal = ave(listanominal, uniqueid, FUN = sum),
    mun_turnout = mun_total / mun_listanominal
  )

# Save the final data
write_dta(data, "Michoacan_Section_2018.dta")

# Load necessary libraries
library(dplyr)
library(haven)
library(stringr)

# Load the Michoacan 2018 data
data_2018 <- read_dta("Michoacan_Section_2018.dta")

# Collapse the data by municipality and uniqueid, summing up the numeric variables
data_2018_collapsed <- data_2018 %>%
  group_by(municipality, uniqueid) %>%
  summarise(across(PAN:total, sum, na.rm = TRUE),
            STATE = first(STATE),
            year = first(year),
            month = first(month),
            winner = first(winner),
            second = first(second),
            third = first(third),
            mun_turnout = first(mun_turnout),
            incumbent = first(incumbent))

# Rename the collapsed turnout variable
data_2018_collapsed <- data_2018_collapsed %>%
  rename(turnout = mun_turnout) %>%
  arrange(uniqueid)

# Save the collapsed dataset
write_dta(data_2018_collapsed, "../../Update Municipal/Michoacan_2018.dta")

# Clear environment
rm(data_2018_collapsed)

# Append data from different years and additional sources
data_2018 <- read_dta("Michoacan_Section_2018.dta")
data_2015 <- read_dta("Michoacan_Section_2015.dta")
data_extraordinary <- read_dta("Michoacan_Section_2015_EXTRAORDINARIO.dta")

# Append all the datasets
data_all <- bind_rows(data_2018, data_2015, data_extraordinary)

# Save the appended data
write_dta(data_all, "Michoacan_Section_15_18.dta")

# Erase old datasets
file.remove("Michoacan_Section_2018.dta")
file.remove("Michoacan_Section_2015.dta")
file.remove("Michoacan_Section_2015_EXTRAORDINARIO.dta")

# Add new variable PAN_winner based on the 'winner' column
data_all <- data_all %>%
  mutate(
    PAN_winner = ifelse(str_detect(winner, "PAN") & !str_detect(winner, "PANAL"), 1, 0),
    winner_counter = PAN_winner
  )

# Iterate over the other party variables and create the corresponding winner indicators
for (party in c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "MORENA", "MC")) {
  data_all <- data_all %>%
    mutate(!!paste0(party, "_winner") := ifelse(str_detect(winner, party), 1, 0)) %>%
    mutate(winner_counter = winner_counter + !!sym(paste0(party, "_winner")))
}

# Display the frequency of winner_counter and count where no winner is found
table(data_all$winner_counter)
nrow(data_all %>% filter(winner_counter == 0))

# Save the combined dataset
write_dta(data_all, "Michoacan_Section_15_18.dta")

# Load precinct-level data
data_precinct <- read_dta("../../Precinct/Michoacan_ALL.dta")

# Append the newly created dataset to the precinct-level data
data_precinct <- bind_rows(data_precinct, data_all)

# Save the updated precinct data
write_dta(data_precinct, "Michoacan_ALL_SALVADOR.dta")

# Erase the intermediate file
file.remove("Michoacan_Section_15_18.dta")

# Erase all auxiliary datasets created during the process
for (i in 2:113) {
  file.remove(paste0("Hoja1 (", i, ").dta"))
}

