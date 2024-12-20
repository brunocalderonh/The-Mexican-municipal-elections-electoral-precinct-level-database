# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)

# Set working directories (change as needed)
setwd("D:/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012")
setwd("C:/Users/Horacio/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012")

# Load the 1996 dataset
data_1996 <- read_csv("Ayu_Seccion_1996_No_LN.csv")

# Rename variables
data_1996 <- data_1996 %>%
  rename(municipality = municipio, section = seccion)

# Drop rows where municipality is empty and section is NA
data_1996 <- data_1996 %>%
  filter(!(municipality == "" & is.na(section)))

# Convert character columns to numeric where needed
data_1996 <- data_1996 %>%
  mutate(across(pan:nulos, ~ as.numeric(.), .names = "destring_{col}"))

# Generate 'total' variable as the row sum of the specified columns
data_1996 <- data_1996 %>%
  rowwise() %>%
  mutate(total = sum(c_across(c(pan, pri, pps, prd, pc, prt, pvem, pt, cc1, validos, nulos)), na.rm = TRUE))

# Drop rows where total is missing or zero
data_1996 <- data_1996 %>%
  filter(!(is.na(total) | total == 0))

# Collapse (aggregate) data by municipality, section, and coalition
collapsed_1996 <- data_1996 %>%
  group_by(municipality, section, coalition) %>%
  summarise(across(pan:total, sum, na.rm = TRUE))

#************************************************************************************************
# Generate 'pps_pc' and adjust for coalition values
collapsed_1996 <- collapsed_1996 %>%
  mutate(pps_pc = ifelse(coalition == "PPS,PC", pps + pc + cc1, 0),
         pps = ifelse(coalition == "PPS,PC", 0, pps),
         pc = ifelse(coalition == "PPS,PC", 0, pc),
         cc1 = ifelse(coalition == "PPS,PC", 0, cc1))

collapsed_1996 <- collapsed_1996 %>%
  mutate(pps_prd_pc = ifelse(coalition == "PPS,PRD,PC", pps + prd + pc + cc1, 0),
         pps = ifelse(coalition == "PPS,PRD,PC", 0, pps),
         prd = ifelse(coalition == "PPS,PRD,PC", 0, prd),
         pc = ifelse(coalition == "PPS,PRD,PC", 0, pc),
         cc1 = ifelse(coalition == "PPS,PRD,PC", 0, cc1))

collapsed_1996 <- collapsed_1996 %>%
  mutate(prd_pc = ifelse(coalition == "PRD,PC", prd + pc + cc1, 0),
         prd = ifelse(coalition == "PRD,PC", 0, prd),
         pc = ifelse(coalition == "PRD,PC", 0, pc),
         cc1 = ifelse(coalition == "PRD,PC", 0, cc1))

collapsed_1996 <- collapsed_1996 %>%
  mutate(prd_pc_prt = ifelse(coalition == "PRD,PC,PRT", prd + pc + prt + cc1, 0),
         prd = ifelse(coalition == "PRD,PC,PRT", 0, prd),
         pc = ifelse(coalition == "PRD,PC,PRT", 0, pc),
         prt = ifelse(coalition == "PRD,PC,PRT", 0, prt),
         cc1 = ifelse(coalition == "PRD,PC,PRT", 0, cc1))

collapsed_1996 <- collapsed_1996 %>%
  mutate(prd_pc_pvem_pt = ifelse(coalition == "PRD,PC,PVEM,PT", prd + pc + pvem + pt + cc1, 0),
         prd = ifelse(coalition == "PRD,PC,PVEM,PT", 0, prd),
         pc = ifelse(coalition == "PRD,PC,PVEM,PT", 0, pc),
         pvem = ifelse(coalition == "PRD,PC,PVEM,PT", 0, pvem),
         pt = ifelse(coalition == "PRD,PC,PVEM,PT", 0, pt),
         cc1 = ifelse(coalition == "PRD,PC,PVEM,PT", 0, cc1))

# Drop 'cc1' column
collapsed_1996 <- collapsed_1996 %>% select(-cc1)

#************************************************************************************************
# Rename the variables to uppercase format
collapsed_1996 <- collapsed_1996 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PartCardenista = pc, PT = pt, PVEM = pvem, PPS = pps, PRT = prt,
         PartCardenista_PPS = pps_pc, PRD_PartCardenista_PPS = pps_prd_pc, PRD_PartCardenista = prd_pc,
         PRD_PartCardenista_PRT = prd_pc_prt, PRD_PartCardenista_PVEM_PT = prd_pc_pvem_pt)

# Drop 'validos' and 'nulos' columns
collapsed_1996 <- collapsed_1996 %>%
  select(-validos, -nulos)

#************************************************************************************************
# Generate 'uniqueid' and assign values based on municipality
collapsed_1996 <- collapsed_1996 %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPULCO DE JUAREZ" ~ 12001,
    municipality == "ACATEPEC" ~ 12076,
    municipality == "AHUACUOTZINGO" ~ 12002,
    municipality == "AJUCHITLAN DEL PROGRESO" ~ 12003,
    municipality == "ALCOZAUCA DE GUERRERO" ~ 12004,
    municipality == "ALPOYECA" ~ 12005,
    municipality == "APAXTLA" ~ 12006,
    municipality == "ARCELIA" ~ 12007,
    municipality == "ATENANGO DEL RIO" ~ 12008,
    municipality == "ATLAMAJALCINGO DEL MONTE" ~ 12009,
    municipality == "ATLIXTAC" ~ 12010,
    municipality == "ATOYAC DE ALVAREZ" ~ 12011,
    municipality == "AYUTLA DE LOS LIBRES" ~ 12012,
    municipality == "AZOYU" ~ 12013,
    municipality == "BENITO JUAREZ" ~ 12014,
    municipality == "BUENAVISTA DE CUELLAR" ~ 12015,
    municipality == "CHILAPA DE ALVAREZ" ~ 12028,
    municipality == "CHILPANCINGO DE LOS BRAVO" ~ 12029,
    municipality == "COAHUAYUTLA DE JOSE MA IZAZAGA" ~ 12016,
    municipality == "COCULA" ~ 12017,
    municipality == "COPALA" ~ 12018,
    municipality == "COPALILLO" ~ 12019,
    municipality == "COPANATOYAC" ~ 12020,
    municipality == "COYUCA DE BENITEZ" ~ 12021,
    municipality == "COYUCA DE CATALAN" ~ 12022,
    municipality == "CUAJINICUILAPA" ~ 12023,
    municipality == "CUALAC" ~ 12024,
    municipality == "CUAUTEPEC" ~ 12025,
    municipality == "CUETZALA DEL PROGRESO" ~ 12026,
    municipality == "CUTZAMALA DE PINZON" ~ 12027,
    municipality == "EDUARDO NERI" ~ 12075,
    municipality == "FLORENCIO VILLARREAL" ~ 12030,
    municipality == "GENERAL CANUTO A. NERI" ~ 12031,
    municipality == "GENERAL HELIODORO CASTILLO" ~ 12032,
    municipality == "HUAMUXTITLAN" ~ 12033,
    municipality == "HUITZUCO DE LOS FIGUEROA" ~ 12034,
    municipality == "IGUALA DE LA INDEPENDENCIA" ~ 12035,
    municipality == "IGUALAPA" ~ 12036,
    municipality == "IXCATEOPAN DE CUAUHTEMOC" ~ 12037,
    municipality == "JUAN R. ESCUDERO" ~ 12039,
    municipality == "UNION DE ISIDORO MONTES DE OCA, LA" ~ 12068,
    municipality == "LEONARDO BRAVO" ~ 12040,
    municipality == "MALINALTEPEC" ~ 12041,
    municipality == "MARTIR DE CUILAPA" ~ 12042,
    municipality == "METLATONOC" ~ 12043,
    municipality == "MOCHITLAN" ~ 12044,
    municipality == "OLINALA" ~ 12045,
    municipality == "OMETEPEC" ~ 12046,
    municipality == "PEDRO ASCENCIO ALQUISIRAS" ~ 12047,
    municipality == "PETATLAN" ~ 12048,
    municipality == "PILCAYA" ~ 12049,
    municipality == "PUNGARABATO" ~ 12050,
    municipality == "QUECHULTENANGO" ~ 12051,
    municipality == "SAN LUIS ACATLAN" ~ 12052,
    municipality == "SAN MARCOS" ~ 12053,
    municipality == "SAN MIGUEL TOTOLAPAN" ~ 12054,
    municipality == "TAXCO DE ALARCON" ~ 12055,
    municipality == "TECOANAPA" ~ 12056,
    municipality == "TECPAN DE GALEANA" ~ 12057,
    municipality == "TELOLOAPAN" ~ 12058,
    municipality == "TEPECOACUILCO DE TRUJANO" ~ 12059,
    municipality == "TETIPAC" ~ 12060,
    municipality == "TIXTLA DE GUERRERO" ~ 12061,
    municipality == "TLACOACHISTLAHUACA" ~ 12062,
    municipality == "TLACOAPA" ~ 12063,
    municipality == "TLALCHAPA" ~ 12064,
    municipality == "TLALIXTAQUILLA DE MALDONADO" ~ 12065,
    municipality == "TLAPA DE COMONFORT" ~ 12066,
    municipality == "TLAPEHUALA" ~ 12067,
    municipality == "XALPATLAHUAC" ~ 12069,
    municipality == "XOCHIHUEHUETLAN" ~ 12070,
    municipality == "XOCHISTLAHUACA" ~ 12071,
    municipality == "ZAPOTITLAN TABLAS" ~ 12072,
    municipality == "JOSE AZUETA" ~ 12038,
    municipality == "ZIRANDARO" ~ 12073,
    municipality == "ZITLALA" ~ 12074,
    TRUE ~ 0))

# Generate the 'valid' variable as the row total of specific columns
collapsed_1996 <- collapsed_1996 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PPS, PRD, PartCardenista, PRT, PVEM, PT, PartCardenista_PPS,
                                PRD_PartCardenista_PPS, PRD_PartCardenista, PRD_PartCardenista_PRT,
                                PRD_PartCardenista_PVEM_PT)), na.rm = TRUE))


# Step 1: Read the 1999 CSV data
data_1999 <- read_csv("Ayu_Seccion_1999_No_LN.csv")

# Step 2: Rename columns for municipality and section
data_1999 <- data_1999 %>%
  rename(municipality = municipio, section = seccion)

# Step 3: Drop rows where municipality is empty and section is NA
data_1999 <- data_1999 %>%
  filter(!(municipality == "" & is.na(section)))

# Step 4: Convert the range of columns (pan to total) to numeric
data_1999 <- data_1999 %>%
  mutate(across(pan:total, as.numeric))

# Step 5: Drop rows where 'total' is NA or zero
data_1999 <- data_1999 %>%
  filter(!is.na(total) & total != 0)

# Step 6: Collapse data by municipality and section (sum by group)
data_1999_collapsed <- data_1999 %>%
  group_by(municipality, section) %>%
  summarise(across(pan:total, sum, na.rm = TRUE))

# Step 7: Rename columns to match the structure
data_1999_collapsed <- data_1999_collapsed %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PRS = prs)

# Step 8: Drop 'nulos' column (if present)
data_1999_collapsed <- data_1999_collapsed %>%
  select(-nulos)

# Step 9: Create 'uniqueid' and replace values based on municipality
data_1999_collapsed <- data_1999_collapsed %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPULCO DE JUAREZ" ~ 12001,
    municipality == "ACATEPEC" ~ 12076,
    municipality == "AHUACUOTZINGO" ~ 12002,
    municipality == "AJUCHITLAN DEL PROGRESO" ~ 12003,
    municipality == "ALCOZAUCA DE GUERRERO" ~ 12004,
    municipality == "ALPOYECA" ~ 12005,
    municipality == "APAXTLA" ~ 12006,
    municipality == "ARCELIA" ~ 12007,
    municipality == "ATENANGO DEL RIO" ~ 12008,
    municipality == "ATLAMAJALCINGO DEL MONTE" ~ 12009,
    municipality == "ATLIXTAC" ~ 12010,
    municipality == "ATOYAC DE ALVAREZ" ~ 12011,
    municipality == "AYUTLA DE LOS LIBRES" ~ 12012,
    municipality == "AZOYU" ~ 12013,
    municipality == "BENITO JUAREZ" ~ 12014,
    municipality == "BUENAVISTA DE CUELLAR" ~ 12015,
    municipality == "CHILAPA DE ALVAREZ" ~ 12028,
    municipality == "CHILPANCINGO DE LOS BRAVO" ~ 12029,
    municipality == "COAHUAYUTLA" ~ 12016,
    municipality == "COCULA" ~ 12017,
    municipality == "COPALA" ~ 12018,
    municipality == "COPALILLO" ~ 12019,
    municipality == "COPANATOYAC" ~ 12020,
    municipality == "COYUCA DE BENITEZ" ~ 12021,
    municipality == "COYUCA DE CATALAN" ~ 12022,
    municipality == "CUAJINICUILAPA" ~ 12023,
    municipality == "CUALAC" ~ 12024,
    municipality == "CUAUTEPEC" ~ 12025,
    municipality == "CUETZALA DEL PROGRESO" ~ 12026,
    municipality == "CUTZAMALA DE PINZON" ~ 12027,
    municipality == "EDUARDO NERI" ~ 12075,
    municipality == "FLORENCIO VILLARREAL" ~ 12030,
    municipality == "GENERAL CANUTO A. NERI" ~ 12031,
    municipality == "GRAL. HELIODORO CASTILLO" ~ 12032,
    municipality == "HUAMUXTITLAN" ~ 12033,
    municipality == "HUITZUCO DE LOS FIGUEROA" ~ 12034,
    municipality == "IGUALA DE LA INDEPENDENCIA" ~ 12035,
    municipality == "IGUALAPA" ~ 12036,
    municipality == "IXCATEOPAN DE CUAUHTEMOC" ~ 12037,
    municipality == "JUAN R. ESCUDERO" ~ 12039,
    municipality == "LA UNION" ~ 12068,
    municipality == "LEONARDO BRAVO" ~ 12040,
    municipality == "MALINALTEPEC" ~ 12041,
    municipality == "MARTIR DE CUILAPAN" ~ 12042,
    municipality == "METLATONOC" ~ 12043,
    municipality == "MOCHITLAN" ~ 12044,
    municipality == "OLINALA" ~ 12045,
    municipality == "OMETEPEC" ~ 12046,
    municipality == "PEDRO ASCENCIO ALQUISIRAS" ~ 12047,
    municipality == "PETATLAN" ~ 12048,
    municipality == "PILCAYA" ~ 12049,
    municipality == "PUNGARABATO" ~ 12050,
    municipality == "QUECHULTENANGO" ~ 12051,
    municipality == "SAN LUIS ACATLAN" ~ 12052,
    municipality == "SAN MARCOS" ~ 12053,
    municipality == "SAN MIGUEL TOTOLAPAN" ~ 12054,
    municipality == "TAXCO DE ALARCON" ~ 12055,
    municipality == "TECOANAPA" ~ 12056,
    municipality == "TECPAN DE GALEANA" ~ 12057,
    municipality == "TELOLOAPAN" ~ 12058,
    municipality == "TEPECOACUILCO DE TRUJANO" ~ 12059,
    municipality == "TETIPAC" ~ 12060,
    municipality == "TIXTLA DE GUERRERO" ~ 12061,
    municipality == "TLACOACHISTLAHUACA" ~ 12062,
    municipality == "TLACOAPA" ~ 12063,
    municipality == "TLALCHAPA" ~ 12064,
    municipality == "TLALIXTAQUILLA DE MALDONADO" ~ 12065,
    municipality == "TLAPA DE COMONFORT" ~ 12066,
    municipality == "TLAPEHUALA" ~ 12067,
    municipality == "XALPATLAHUAC" ~ 12069,
    municipality == "XOCHIHUEHUETLAN" ~ 12070,
    municipality == "XOCHISTLAHUACA" ~ 12071,
    municipality == "ZAPOTITLAN TABLAS" ~ 12072,
    municipality == "JOSE AZUETA" ~ 12038,
    municipality == "ZIRANDARO DE LOS CHAVEZ" ~ 12073,
    municipality == "ZITLALA" ~ 12074,
    TRUE ~ 0
  ))

# Step 10: Calculate 'valid' votes as the sum of the selected columns
data_1999_collapsed <- data_1999_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PRD, PT, PVEM, PRS)), na.rm = TRUE))

# Step 11: Collapse data by uniqueid (municipality level)
data_1999_collapsed <- data_1999_collapsed %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI, PRD, PT, PVEM, PRS, total, valid), sum, na.rm = TRUE)) %>%
  mutate(across(c(PAN, PRI, PRD, PT, PVEM, PRS, total, valid), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 12: Handle duplicate sections (remove duplicates)
data_1999_collapsed <- data_1999_collapsed %>%
  janitor::get_dupes(section) %>%
  filter(dupe_count == 1) %>%
  select(-dupe_count)

# Step 13: Load additional dataset (all_months_years.dta equivalent in R, possibly CSV or RDS format)
# Assuming the file is in CSV format for R usage
all_months_years <- read_csv("all_months_years.csv")

# Merge based on 'section'
data_1999_merged <- data_1999_collapsed %>%
  left_join(all_months_years, by = c("section" = "seccion")) %>%
  filter(month == 9 & year == 1999)

# Step 14: Calculate turnout
data_1999_merged <- data_1999_merged %>%
  mutate(turnout = total / lista)

# Step 15: Calculate municipality-level 'turnout'
data_1999_merged <- data_1999_merged %>%
  mutate(mun_turnout = mun_total / mun_lista)

# Step 16: Rank the parties based on their votes
data_1999_merged <- data_1999_merged %>%
  mutate(across(starts_with("inv_mun"), rank, .names = "{col}_r"))

# Step 17: Determine the winner based on the ranking
data_1999_merged <- data_1999_merged %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PRS_r == 1 ~ "PRS",
    TRUE ~ NA_character_
  ))

# Step 18: Add year and month information
data_1999_merged <- data_1999_merged %>%
  mutate(year = 1999, month = "October")

# Step 19: Sort by section
data_1999_merged <- data_1999_merged %>%
  arrange(section)

# Step 20: Save the final dataset to a CSV file
write.csv(data_1999_merged, "Guerrero_Section_1999.csv", row.names = FALSE)


pdln12_data <- read_excel("INE-CI141-2014 Horacio Larreguy Arbesu/pdln12_edms_PEL_2002_2005.xls", sheet = "pdln12_edms")

# Keep rows where 'FECHA' equals "20021006"
pdln12_data <- pdln12_data %>%
  filter(FECHA == "20021006")

# Rename columns
pdln12_data <- pdln12_data %>%
  rename(section = SEC, listanominal = LISTA)

# Collapse (sum) listanominal by section
pdln12_collapsed <- pdln12_data %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE))

# Save to a temporary CSV file (in R, we avoid direct saving to `.dta`)
write.csv(pdln12_collapsed, "Listanominal2002.csv", row.names = FALSE)

# Step 2: Process the 2002 CSV data (Ayu_Seccion_2002_No_LN.csv)
data_2002 <- read_csv("Ayu_Seccion_2002_No_LN.csv")

# Rename columns
data_2002 <- data_2002 %>%
  rename(municipality = nombre_municipio, section = seccion)

# Drop rows where municipality is empty or section is NA
data_2002 <- data_2002 %>%
  filter(!(municipality == "" & is.na(section)))

# Drop rows where 'total' is missing or zero
data_2002 <- data_2002 %>%
  filter(!is.na(total) & total != 0)

# Convert 'pan' to 'total' columns to numeric
data_2002 <- data_2002 %>%
  mutate(across(pan:total, as.numeric))

# Collapse (sum) data by municipality and section
data_2002_collapsed <- data_2002 %>%
  group_by(municipality, section) %>%
  summarise(across(pan:prdpt, sum, na.rm = TRUE))

# Rename columns
data_2002_collapsed <- data_2002_collapsed %>%
  rename(PAN = pan, PRI_PVEM = pripvem, PRD = prd, PT = pt, PRS = prs, PC = pc, PSN = psn, PAS = pas, PSM = psm, PRD_PT = prdpt)

# Drop a specific row based on condition
data_2002_collapsed <- data_2002_collapsed %>%
  filter(!(municipality == "ACATEPEC" & section == 2527))

# Merge with the listanominal data from Excel
listanominal_data <- read_csv("Listanominal2002.csv")

# Merge the two datasets by section
data_2002_merged <- data_2002_collapsed %>%
  left_join(listanominal_data, by = "section")

# Drop rows where the merge failed
data_2002_merged <- data_2002_merged %>%
  filter(!is.na(listanominal))

# Calculate turnout as total divided by listanominal
data_2002_merged <- data_2002_merged %>%
  mutate(turnout = total / listanominal)

# Create uniqueid and assign values based on municipality
data_2002_merged <- data_2002_merged %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPULCO DE JUAREZ" ~ 12001,
    municipality == "ACATEPEC" ~ 12076,
    municipality == "AHUACUOTZINGO" ~ 12002,
    municipality == "AJUCHITLAN DEL PROGRESO" ~ 12003,
    municipality == "ALCOZAUCA DE GUERRERO" ~ 12004,
    municipality == "ALPOYECA" ~ 12005,
    municipality == "APAXTLA DE CASTREJON" ~ 12006,
    municipality == "ARCELIA" ~ 12007,
    municipality == "ATENANGO DEL RIO" ~ 12008,
    municipality == "ATLAMAJALCINGO DEL MONTE" ~ 12009,
    municipality == "ATLIXTAC" ~ 12010,
    municipality == "ATOYAC DE ALVAREZ" ~ 12011,
    municipality == "AYUTLA DE LOS LIBRES" ~ 12012,
    municipality == "AZOYU" ~ 12013,
    municipality == "BENITO JUAREZ" ~ 12014,
    municipality == "BUENAVISTA DE CUELLAR" ~ 12015,
    municipality == "CHILAPA DE ALVAREZ" ~ 12028,
    municipality == "CHILPANCINGO DE LOS BRAVO" ~ 12029,
    municipality == "COAHUAYUTLA" ~ 12016,
    municipality == "COCULA" ~ 12017,
    municipality == "COPALA" ~ 12018,
    municipality == "COPALILLO" ~ 12019,
    municipality == "COPANATOYAC" ~ 12020,
    municipality == "COYUCA DE BENITEZ" ~ 12021,
    municipality == "COYUCA DE CATALAN" ~ 12022,
    municipality == "CUAJINICUILAPA" ~ 12023,
    municipality == "CUALAC" ~ 12024,
    municipality == "CUAUTEPEC" ~ 12025,
    municipality == "CUETZALA DEL PROGRESO" ~ 12026,
    municipality == "CUTZAMALA DE PINZON" ~ 12027,
    municipality == "EDUARDO NERI" ~ 12075,
    municipality == "FLORENCIO VILLARREAL" ~ 12030,
    municipality == "GENERAL CANUTO A. NERI" ~ 12031,
    municipality == "GRAL. HELIODORO CASTILLO" ~ 12032,
    municipality == "HUAMUXTITLAN" ~ 12033,
    municipality == "HUITZUCO DE LOS FIGUEROA" ~ 12034,
    municipality == "IGUALA DE LA INDEPENDENCIA" ~ 12035,
    municipality == "IGUALAPA" ~ 12036,
    municipality == "IXCATEOPAN DE CUAUHTEMOC" ~ 12037,
    municipality == "JUAN R. ESCUDERO" ~ 12039,
    municipality == "LA UNION" ~ 12068,
    municipality == "LEONARDO BRAVO" ~ 12040,
    municipality == "MALINALTEPEC" ~ 12041,
    municipality == "MARTIR DE CUILAPAN" ~ 12042,
    municipality == "METLATONOC" ~ 12043,
    municipality == "MOCHITLAN" ~ 12044,
    municipality == "OLINALA" ~ 12045,
    municipality == "OMETEPEC" ~ 12046,
    municipality == "PEDRO ASCENCIO ALQUISIRAS" ~ 12047,
    municipality == "PETATLAN" ~ 12048,
    municipality == "PILCAYA" ~ 12049,
    municipality == "PUNGARABATO" ~ 12050,
    municipality == "QUECHULTENANGO" ~ 12051,
    municipality == "SAN LUIS ACATLAN" ~ 12052,
    municipality == "SAN MARCOS" ~ 12053,
    municipality == "SAN MIGUEL TOTOLAPAN" ~ 12054,
    municipality == "TAXCO DE ALARCON" ~ 12055,
    municipality == "TECOANAPA" ~ 12056,
    municipality == "TECPAN DE GALEANA" ~ 12057,
    municipality == "TELOLOAPAN" ~ 12058,
    municipality == "TEPECOACUILCO DE TRUJANO" ~ 12059,
    municipality == "TETIPAC" ~ 12060,
    municipality == "TIXTLA DE GUERRERO" ~ 12061,
    municipality == "TLACOACHISTLAHUACA" ~ 12062,
    municipality == "TLACOAPA" ~ 12063,
    municipality == "TLALCHAPA" ~ 12064,
    municipality == "TLALIXTAQUILLA DE MALDONADO" ~ 12065,
    municipality == "TLAPA DE COMONFORT" ~ 12066,
    municipality == "TLAPEHUALA" ~ 12067,
    municipality == "XALPATLAHUAC" ~ 12069,
    municipality == "XOCHIHUEHUETLAN" ~ 12070,
    municipality == "XOCHISTLAHUACA" ~ 12071,
    municipality == "ZAPOTITLAN TABLAS" ~ 12072,
    municipality == "JOSE AZUETA" ~ 12038,
    municipality == "ZIRANDARO DE LOS CHAVEZ" ~ 12073,
    municipality == "ZITLALA" ~ 12074,
    TRUE ~ 0
  ))

# Calculate the 'valid' column as the sum of specified columns
data_2002_merged <- data_2002_merged %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI_PVEM, PRD, PT, PRS, PC, PSN, PAS, PSM, PRD_PT)), na.rm = TRUE))

# Calculate mun_x variables (group by uniqueid)
data_2002_merged <- data_2002_merged %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI_PVEM, PRD, PT, PRS, PC, PSN, PAS, PSM, PRD_PT, total, listanominal, valid), sum, na.rm = TRUE)) %>%
  mutate(across(c(PAN, PRI_PVEM, PRD, PT, PRS, PC, PSN, PAS, PSM, PRD_PT, total, valid), ~ 1 / ., .names = "inv_mun_{col}"))

# Calculate municipality-level turnout
data_2002_merged <- data_2002_merged %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the parties
data_2002_merged <- data_2002_merged %>%
  mutate(across(starts_with("inv_mun"), rank, .names = "{col}_r"))

# Determine the winner based on the ranking
data_2002_merged <- data_2002_merged %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_PVEM_r == 1 ~ "PRI_PVEM",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PRS_r == 1 ~ "PRS",
    inv_mun_PC_r == 1 ~ "PC",
    inv_mun_PSN_r == 1 ~ "PSN",
    inv_mun_PAS_r == 1 ~ "PAS",
    inv_mun_PSM_r == 1 ~ "PSM",
    inv_mun_PRD_PT_r == 1 ~ "PRD_PT",
    TRUE ~ NA_character_
  ))

# Add year and month information
data_2002_merged <- data_2002_merged %>%
  mutate(year = 2002, month = "October")

# Sort by section
data_2002_merged <- data_2002_merged %>%
  arrange(section)

# Save the result to a CSV file (use a CSV since R avoids direct `.dta` saves)
write.csv(data_2002_merged, "Guerrero_Section_2002.csv", row.names = FALSE)

# Read Excel file for 2005
listanominal_2005 <- read_excel("INE-CI141-2014 Horacio Larreguy Arbesu/pdln12_edms_PEL_2002_2005.xls", 
                                sheet = "pdln12_edms")

# Filter data to keep only the relevant date
listanominal_2005 <- listanominal_2005 %>%
  filter(FECHA == "20051002") %>%
  rename(section = SEC, listanominal = LISTA) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE))

# Save the listanominal data
write_csv(listanominal_2005, "Listanominal2005.csv")

# Read the 2005 CSV data
data_2005 <- read_csv("Ayu_Seccion_2005_No_LN.csv") %>%
  rename(municipality = nombre_municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  mutate(across(pan:prdprs, as.numeric)) %>%
  filter(!is.na(total) & total != 0)

# Collapse the data by municipality and section
data_2005_collapsed <- data_2005 %>%
  group_by(municipality, section) %>%
  summarise(across(pan:total, sum, na.rm = TRUE)) %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PRS = prs, PC = pc, PRD_PRS = prdprs)

# Merge with the listanominal data
data_2005_merged <- data_2005_collapsed %>%
  left_join(listanominal_2005, by = "section") %>%
  mutate(turnout = total / listanominal) %>%
  filter(!is.na(turnout))

# Create uniqueid based on municipality
data_2005_merged <- data_2005_merged %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPULCO DE JUAREZ" ~ 12001,
    municipality == "ACATEPEC" ~ 12076,
    municipality == "AHUACUOTZINGO" ~ 12002,
    # Add all other cases following the pattern
    TRUE ~ 0
  ))

# Calculate valid votes
data_2005_merged <- data_2005_merged %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRD_PRS), na.rm = TRUE))

# Collapse data by uniqueid (municipality level)
data_2005_merged <- data_2005_merged %>%
  group_by(uniqueid) %>%
  summarise(across(PAN:valid, sum, na.rm = TRUE)) %>%
  mutate(across(PAN:valid, ~ 1 / ., .names = "inv_mun_{col}"))

# Rank the parties and determine the winner
data_2005_merged <- data_2005_merged %>%
  mutate(across(starts_with("inv_mun"), rank, .names = "{col}_r")) %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PRS_r == 1 ~ "PRS",
    inv_mun_PC_r == 1 ~ "PC",
    inv_mun_PRD_PRS_r == 1 ~ "PRD_PRS",
    TRUE ~ NA_character_
  ))

# Add year and month info
data_2005_merged <- data_2005_merged %>%
  mutate(year = 2005, month = "October")

# Save the final dataset
write_csv(data_2005_merged, "Guerrero_Section_2005.csv")

# Read the 2008 CSV data
data_2008 <- read_csv("Ayu_Seccion_2008.csv") %>%
  rename(municipality = nombre_municipio, section = seccion, listanominal = lista_nominal) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  mutate(across(listanominal:total, as.numeric)) %>%
  filter(!is.na(total) & total != 0)

# Collapse the data by municipality and section
data_2008_collapsed <- data_2008 %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:total, sum, na.rm = TRUE)) %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc, PANAL = panal, PAS = pas, PAG = alianzaguerrero, 
         PRI_PVEM = pripvem, PT_PC = ptpc)

# Calculate turnout
data_2008_collapsed <- data_2008_collapsed %>%
  mutate(turnout = total / listanominal)

# Assign uniqueid
data_2008_collapsed <- data_2008_collapsed %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPULCO DE JUAREZ" ~ 12001,
    municipality == "ACATEPEC" ~ 12076,
    # Continue for other municipalities
    TRUE ~ 0
  ))

# Calculate valid votes
data_2008_collapsed <- data_2008_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PT_PC), na.rm = TRUE))

# Rank the parties
data_2008_collapsed <- data_2008_collapsed %>%
  mutate(across(starts_with("inv_mun"), rank, .names = "{col}_r")) %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PC_r == 1 ~ "PC",
    inv_mun_PANAL_r == 1 ~ "PANAL",
    inv_mun_PAS_r == 1 ~ "PAS",
    inv_mun_PAG_r == 1 ~ "PAG",
    inv_mun_PRI_PVEM_r == 1 ~ "PRI_PVEM",
    inv_mun_PT_PC_r == 1 ~ "PT_PC",
    TRUE ~ NA_character_
  ))

# Add year and month info
data_2008_collapsed <- data_2008_collapsed %>%
  mutate(year = 2008, month = "October")

# Save the final dataset
write_csv(data_2008_collapsed, "Guerrero_Section_2008.csv")

# Read Excel data for 2009 special elections
data_2009 <- read_excel("Resultados_eleccion_extraordinaria_2009.xls", sheet = "Extra Malinaltepec", range = "A8:J40") %>%
  rename(section = B, PAN = D, PRI = E, PRD = F, PT = G, total = TOTAL) %>%
  filter(!is.na(total) & total != 0) %>%
  mutate(across(PAN:total, as.numeric), municipality = "MALINALTEPEC EXTRAORDINARIO")

# Merge with all_months_years data
all_months_years <- read_csv("all_months_years.csv")
merge_data <- all_months_years %>%
  filter(month == 3 & year == 2009 & ed == 12) %>%
  rename(section = seccion, listanominal = lista)

data_2009_merged <- data_2009 %>%
  left_join(merge_data, by = "section") %>%
  mutate(turnout = total / listanominal)

# Add uniqueid
data_2009_merged <- data_2009_merged %>%
  mutate(uniqueid = 12041)

# Calculate valid votes
data_2009_merged <- data_2009_merged %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PT), na.rm = TRUE))

# Rank the parties
data_2009_merged <- data_2009_merged %>%
  mutate(across(starts_with("inv_mun"), rank, .names = "{col}_r")) %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    TRUE ~ NA_character_
  ))

# Add year and month info
data_2009_merged <- data_2009_merged %>%
  mutate(year = 2009, month = "April")

# Save the final dataset
write_csv(data_2009_merged, "Guerrero_Section_2009.csv")

# Read the 2012 Excel data
data_2012 <- read_excel("Ayu_Seccion_2012.xlsx", sheet = "Sheet1") %>%
  rename(municipality = MUNICIPIO, section = SECCION, total = TOTALES) %>%
  filter(!(municipality == "" | section == "" | is.na(total) | total == 0))

# Assign uniqueid
data_2012 <- data_2012 %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPULCO DE JUAREZ" ~ 12001,
    municipality == "ACATEPEC" ~ 12076,
    # Continue with other municipalities
    TRUE ~ 0
  ))

# Merge with the all_months_years data
data_2012_merged <- data_2012 %>%
  left_join(all_months_years, by = c("section" = "seccion")) %>%
  filter(month == 7 & year == 2012 & day == 1) %>%
  rename(listanominal = lista)

# Calculate turnout and valid votes
data_2012_merged <- data_2012_merged %>%
  mutate(turnout = total / listanominal) %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRD_PT_PC), na.rm = TRUE))

# Rank the parties
data_2012_merged <- data_2012_merged %>%
  mutate(across(starts_with("inv_mun"), rank, .names = "{col}_r")) %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PC_r == 1 ~ "PC",
    inv_mun_PANAL_r == 1 ~ "PANAL",
    inv_mun_PRI_PVEM_r == 1 ~ "PRI_PVEM",
    inv_mun_PRD_PT_PC_r == 1 ~ "PRD_PT_PC",
    TRUE ~ NA_character_
  ))

# Add year and month info
data_2012_merged <- data_2012_merged %>%
  mutate(year = 2012, month = "July")

# Save the final dataset
write_csv(data_2012_merged, "Guerrero_Section_2012.csv")

# Load all the datasets for different years
data_1999 <- read_csv("Guerrero_Section_1999.csv")
data_2005 <- read_csv("Guerrero_Section_2005.csv")
data_2008 <- read_csv("Guerrero_Section_2008.csv")
data_2009 <- read_csv("Guerrero_Section_2009.csv")
data_2012 <- read_csv("Guerrero_Section_2012.csv")

# Combine them into a single dataset
guerrero_all <- bind_rows(data_1999, data_2005, data_2008, data_2009, data_2012)

# Handle duplicate sections across years and remove duplicates
guerrero_all <- guerrero_all %>%
  distinct(section, year, .keep_all = TRUE)

# Save the combined dataset
write_csv(guerrero_all, "Guerrero_ALL.csv")

# Set working directory
setwd("D:/Dropbox/Salvador Ascencio/Update Precincts/Guerrero")

# Read Excel file for 2015
data_2015 <- read_excel("Ayuntamientos_Gro_2015.xlsx", sheet = "Ayuntamientos") %>%
  mutate(across(everything(), as.numeric))

# Handle coalitions
data_2015 <- data_2015 %>%
  mutate(PRI_PVEM2 = ifelse(coalpripvem == 1, PRI + PVEM + PRI_PVEM, NA),
         PRI = ifelse(coalpripvem == 1, NA, PRI),
         PVEM = ifelse(coalpripvem == 1, NA, PVEM),
         PRD_PT2 = ifelse(coalprdpt == 1, PRD + PT + PRD_PT, NA),
         PRD = ifelse(coalprdpt == 1, NA, PRD),
         PT = ifelse(coalprdpt == 1, NA, PT),
         PRI_PVEM_PANAL2 = ifelse(coalpripvempanal == 1, PRI + PVEM + PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL, NA),
         PRI = ifelse(coalpripvempanal == 1, NA, PRI),
         PVEM = ifelse(coalpripvempanal == 1, NA, PVEM),
         PANAL = ifelse(coalpripvempanal == 1, NA, PANAL),
         PRD_PT_PAN2 = ifelse(coalprdptpan == 1, PRD + PT + PAN + PRD_PT + PRD_PAN + PT_PAN, NA),
         PRD = ifelse(coalprdptpan == 1, NA, PRD),
         PT = ifelse(coalprdptpan == 1, NA, PT),
         PAN = ifelse(coalprdptpan == 1, NA, PAN))

# Rename and drop columns after coalition calculation
data_2015 <- data_2015 %>%
  select(-c(PRI_PVEM_PANAL, PRI_PVEM, PRI_PANAL, PVEM_PANAL, PRD_PT, PRD_PT_PAN, PRD_PAN, PT_PAN)) %>%
  rename(PRI_PVEM_PANAL = PRI_PVEM_PANAL2, PRI_PVEM = PRI_PVEM2, PRD_PT = PRD_PT2, PRD_PT_PAN = PRD_PT_PAN2)

# Assign uniqueid based on municipality
data_2015 <- data_2015 %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPULCO DE JUAREZ" ~ 12001,
    municipality == "ACATEPEC" ~ 12076,
    municipality == "AHUACUOTZINGO" ~ 12002,
    # Add all other cases
    TRUE ~ 0
  )) %>%
  filter(uniqueid != 0)

# Collapse data by section, municipality, and uniqueid
data_2015_collapsed <- data_2015 %>%
  group_by(section, municipality, uniqueid) %>%
  summarise(across(PAN:CI_1, sum, na.rm = TRUE)) %>%
  summarise(total = sum(total), listanominal = sum(listanominal))

# Calculate valid votes
data_2015_collapsed <- data_2015_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRI_PVEM_PANAL), na.rm = TRUE))

# Generate ranks and determine the winner
data_2015_collapsed <- data_2015_collapsed %>%
  group_by(uniqueid) %>%
  mutate(across(starts_with("mun_"), rank, .names = "{col}_r"))

# Determine winner, second, third
data_2015_collapsed <- data_2015_collapsed %>%
  mutate(winner = case_when(PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", TRUE ~ "Other"),
         second = case_when(PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", PRD_r == 2 ~ "PRD", TRUE ~ "Other"),
         third = case_when(PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", PRD_r == 3 ~ "PRD", TRUE ~ "Other"))

# Add election year and other metadata
data_2015_collapsed <- data_2015_collapsed %>%
  mutate(year = 2015, month = ifelse(municipality == "TIXTLA DE GUERRERO", "November", "June"))

# Save the final dataset for 2015
write_csv(data_2015_collapsed, "Guerrero_Section_2015.csv")

# Read communal candidates file for 2018
communal_2018 <- read_excel("communal candidates 2018.xlsx", sheet = "MUNCAND") %>%
  mutate(across(PAN_PRD_MC:MORENA_ES, ~ifelse(is.na(.), 0, 1))) %>%
  rename_with(~paste0("coal_", .), everything()) %>%
  rename(municipality = coal_MUNICIPIO)

# Save communal indicators
write_csv(communal_2018, "coalindicators.csv")

# Read 2018 election results
data_2018 <- read_csv("2018_SEE_GRO_CAS_AYUN.csv") %>%
  rename(municipality = MUNICIPIO, section = SECCION, total = TOTAL_VOTOS, listanominal = LISTA_NOMINAL) %>%
  filter(!is.na(total) & total != 0) %>%
  mutate(across(CAND_IND1:CAND_IND9, ~ifelse(is.na(.), 0, .))) %>%
  rename_with(~paste0("CI_", gsub("CAND_", "", .)), starts_with("CAND_"))

# Handle coalitions for 2018
data_2018 <- data_2018 %>%
  mutate(PAN_PRD_MC2 = ifelse(coal_PAN_PRD_MC == 1, PAN + PRD + MC + PAN_PRD_MC + PRD_MC, NA),
         PRI_PVEM = ifelse(coal_PRI_PVEM == 1, PRI + PVEM + PRI_PVEM, NA),
         MORENA_PES = ifelse(coal_MORENA_ES == 1, MORENA + PES + MORENA_ES, NA))

# Rename and drop columns after coalition handling
data_2018 <- data_2018 %>%
  select(-c(PAN_PRD_MC, PRI, PRD, PVEM, MC, MORENA, PES)) %>%
  rename(PAN_PRD_MC = PAN_PRD_MC2)

# Merge with coalindicators
data_2018 <- data_2018 %>%
  left_join(communal_2018, by = "municipality") %>%
  filter(!is.na(listanominal))

# Assign uniqueid based on municipality
data_2018 <- data_2018 %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPULCO DE JUAREZ" ~ 12001,
    municipality == "ACATEPEC" ~ 12076,
    # Continue for other municipalities
    TRUE ~ 0
  ))

# Collapse data by municipality, section, and uniqueid
data_2018_collapsed <- data_2018 %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(PAN:MORENA_PES, sum, na.rm = TRUE), total = sum(total), listanominal = sum(listanominal))

# Handle coalitions again
data_2018_collapsed <- data_2018_collapsed %>%
  mutate(PRI_PVEM = PRI_PVEM + PRI + PVEM, PRI = ifelse(coal_PRI_PVEM == 1, NA, PRI))

# Generate ranks
data_2018_collapsed <- data_2018_collapsed %>%
  group_by(uniqueid) %>%
  mutate(across(starts_with("mun_"), rank, .names = "{col}_r"))

# Determine winners
data_2018_collapsed <- data_2018_collapsed %>%
  mutate(winner = case_when(PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", TRUE ~ "Other"))

# Save the final dataset for 2018
write_csv(data_2018_collapsed, "Guerrero_Section_2018.csv")

# Read both 2015 and 2018 data
data_2015 <- read_csv("Guerrero_Section_2015.csv")
data_2018 <- read_csv("Guerrero_Section_2018.csv")

# Combine 2015 and 2018 data
data_15_18 <- bind_rows(data_2015, data_2018)

# Save the combined dataset
write_csv(data_15_18, "Guerrero_Section_15_18.csv")

# Read previous datasets (Guerrero_ALL)
guerrero_all <- read_csv("Guerrero_ALL.csv")

# Append the 2015-2018 data to the previous dataset
guerrero_all_updated <- bind_rows(guerrero_all, data_15_18)

# Save the final dataset
write_csv(guerrero_all_updated, "Guerrero_ALL_SALVADOR.csv")

