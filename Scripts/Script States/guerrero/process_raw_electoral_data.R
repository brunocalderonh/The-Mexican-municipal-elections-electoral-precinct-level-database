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

###########################################
### Step 1: Process 1996 Data
###########################################

# Load the 1996 dataset
data_1996 <- read_csv("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/1996/Ayu_Seccion_1996_No_LN.csv")

# Convert column names to lowercase
data_1996 <- data_1996 %>% rename_with(tolower)

# Rename variables
data_1996 <- data_1996 %>%
  rename(municipality = municipio, 
         section = seccion)

# Drop rows where municipality is empty and section is NA
data_1996 <- data_1996 %>%
  filter(!(municipality == "" & is.na(section)))

# Convert character columns to numeric where needed
data_1996 <- data_1996 %>%
  mutate(across(pan:nulos, ~ as.numeric(.)))

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


collapsed_1996 <- collapsed_1996 %>% select(-cc1)

# Rename the variables to uppercase format
collapsed_1996 <- collapsed_1996 %>%
  rename(PAN = pan, 
         PRI = pri, 
         PRD = prd, 
         PartCardenista = pc, 
         PT = pt, 
         PVEM = pvem, 
         PPS = pps, 
         PRT = prt,
         PartCardenista_PPS = pps_pc, 
         PRD_PartCardenista_PPS = pps_prd_pc, 
         PRD_PartCardenista = prd_pc,
         PRD_PartCardenista_PRT = prd_pc_prt, 
         PRD_PartCardenista_PVEM_PT = prd_pc_pvem_pt)

# Drop 'validos' and 'nulos' columns
collapsed_1996 <- collapsed_1996 %>%
  select(-validos, -nulos)
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
    municipality == "CUETZALA DEL PROGESO" ~ 12026,
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
    municipality == "PEDRO ASCENCIO A." ~ 12047,
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
    municipality == "TEPECOACUILCO" ~ 12059,
    municipality == "TETIPAC" ~ 12060,
    municipality == "TIXTLA DE GUERRERO" ~ 12061,
    municipality == "TLACOACHISTLAHUACA" ~ 12062,
    municipality == "TLACOAPA" ~ 12063,
    municipality == "TLALCHAPA" ~ 12064,
    municipality == "TLALIXTAQUILLA" ~ 12065,
    municipality == "TLAPA DE COMONFORT" ~ 12066,
    municipality == "TLAPEHUALA" ~ 12067,
    municipality == "XALPATLAHUAC" ~ 12069,
    municipality == "XOCHIHUEHUETLAN" ~ 12070,
    municipality == "XOCHISTLAHUACA" ~ 12071,
    municipality == "ZAPOTITLAN TABLAS" ~ 12072,
    municipality == "JOSE AZUETA" ~ 12038,
    municipality == "ZIRANDARO" ~ 12073,
    municipality == "ZITLALA" ~ 12074,
    municipality == "MARQUELIA" ~ 12077,
    TRUE ~ NA))

# Generate the 'valid' variable as the row total of specific columns
collapsed_1996 <- collapsed_1996 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PPS, PRD, PartCardenista, PRT, PVEM, PT, PartCardenista_PPS,
                                PRD_PartCardenista_PPS, PRD_PartCardenista, PRD_PartCardenista_PRT,
                                PRD_PartCardenista_PVEM_PT)), na.rm = TRUE))

ln_all_months_years <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta")

ln_all_months_years <- ln_all_months_years %>% 
  dplyr::filter(state == "GUERRERO" & month == "March" & year == 1999)

# Merge the datasets

collapsed_1996 <- collapsed_1996 %>%
  dplyr::left_join(ln_all_months_years %>% dplyr::select(section,lista), by = c("section")) %>% 
  dplyr::mutate(listanominal = lista) %>% 
  dplyr::select(-lista)

# Calculate turnout as total divided by listanominal
collapsed_1996 <- collapsed_1996 %>%
  mutate(turnout = total / listanominal)
  
rm(data_1996)
rm(lista_nominal)
summary(collapsed_1996)

###########################################
### Step 1: Process 1999 Data
###########################################

# Step 1: Read the 1999 CSV data
data_1999 <- read_csv("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/1999/Ayu_Seccion_1999_No_LN.csv")

# Convert column names to lowercase
data_1999 <- data_1999 %>% rename_with(tolower)

# Step 2: Rename columns for municipality and section
data_1999 <- data_1999 %>%
  rename(municipality = "municipio:", 
         section = seccion)

# Step 3: Drop rows where municipality is empty and section is NA
data_1999 <- data_1999 %>%
  filter(!(municipality == "" & 
             is.na(section)))

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
  rename(PAN = pan, 
         PRI = pri, 
         PRD = prd, 
         PT = pt, 
         PVEM = pvem, 
         PRS = prs)

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
    municipality == "MARQUELIA" ~ 12077,
    TRUE ~ NA
  ))

# Step 10: Calculate 'valid' votes as the sum of the selected columns
data_1999_collapsed <- data_1999_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PRD, PT, PVEM, PRS)), na.rm = TRUE))

#

lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "GUERRERO") %>% 
  filter(year == 1999 & month == "September")

# Merge based on 'section'
data_1999_collapsed <- data_1999_collapsed %>%
  left_join(lista_nominal %>% select(section,lista), by = c("section" = "section")) %>% 
  rename(listanominal=lista)

# Step 14: Calculate turnout
data_1999_collapsed <- data_1999_collapsed %>%
  mutate(turnout = total / listanominal)

# Step 15: Add year and month information
data_1999_collapsed <- data_1999_collapsed %>%
  mutate(year = 1999, month = "October")

# Step 16: Sort by section
data_1999_collapsed <- data_1999_collapsed %>%
  arrange(section)

###########################################
### Step 1: Process 2002 Data
###########################################

# Step 1: Read the 2002 CSV data

pdln12_data <- read_excel("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/Other/INE-CI141-2014 Horacio Larreguy Arbesu/pdln12_edms_PEL_2002_2005.xls", sheet = "pdln12_edms")

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

# Step 2: Process the 2002 CSV data (Ayu_Seccion_2002_No_LN.csv)
data_2002 <- read_csv("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2002/Ayu_Seccion_2002_No_LN.csv")

# Rename columns
data_2002 <- data_2002 %>%
  rename(municipality = nombre_municipio, 
         section = seccion,
         total = Total)

# Drop rows where municipality is empty or section is NA
data_2002 <- data_2002 %>%
  filter(!(municipality == "" & is.na(section)))

# Drop rows where 'total' is missing or zero
data_2002 <- data_2002 %>%
  filter(!is.na(total) & total != 0)

# Convert 'pan' to 'total' columns to numeric
data_2002 <- data_2002 %>%
  mutate(across(PAN:total, as.numeric))

# Collapse (sum) data by municipality and section
data_2002_collapsed <- data_2002 %>%
  group_by(municipality, section) %>%
  summarise(across(PAN:total, sum, na.rm = TRUE))

# Rename columns
data_2002_collapsed <- data_2002_collapsed %>%
  rename(PRI_PVEM = "PRI-PVEM", 
         PRD_PT = "PRD-PT")

# Drop a specific row based on condition
data_2002_collapsed <- data_2002_collapsed %>%
  filter(!(municipality == "ACATEPEC" & section == 2527))

# Merge the two datasets by section
data_2002_collapsed <- data_2002_collapsed %>%
  left_join(pdln12_data %>% 
              select(section,listanominal), 
            by = "section")

# Drop rows where the merge failed
data_2002_collapsed <- data_2002_collapsed %>%
  filter(!is.na(listanominal))

# Calculate turnout as total divided by listanominal
data_2002_collapsed <- data_2002_collapsed %>%
  mutate(turnout = total / listanominal)

# Create uniqueid and assign values based on municipality
data_2002_collapsed <- data_2002_collapsed %>%
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
    municipality == "GRAL. CANUTO A. NERI" ~ 12031,
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
    municipality == "TLALIXTAQUILLA DE M." ~ 12065,
    municipality == "TLAPA DE COMONFORT" ~ 12066,
    municipality == "TLAPEHUALA" ~ 12067,
    municipality == "XALPATLAHUAC" ~ 12069,
    municipality == "XOCHIHUEHUETLAN" ~ 12070,
    municipality == "XOCHISTLAHUACA" ~ 12071,
    municipality == "ZAPOTITLAN TABLAS" ~ 12072,
    municipality == "JOSE AZUETA" ~ 12038,
    municipality == "ZIRANDARO DE LOS CHAVEZ" ~ 12073,
    municipality == "ZITLALA" ~ 12074,
    municipality == "MARQUELIA" ~ 12077,
    TRUE ~ NA
  ))

# Calculate the 'valid' column as the sum of specified columns
data_2002_collapsed <- data_2002_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI_PVEM, PRD, PT, PRS, PC, PSN, PAS, PSM, PRD_PT)), na.rm = TRUE))

# Add year and month information
data_2002_collapsed <- data_2002_collapsed %>%
  mutate(year = 2002, month = "October")

# Sort by section
data_2002_collapsed <- data_2002_collapsed %>%
  arrange(section)

###########################################
### Step 1: Process 2005 Data
###########################################

# Read Excel file for 2005
listanominal_2005 <- read_excel("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/Other/INE-CI141-2014 Horacio Larreguy Arbesu/pdln12_edms_PEL_2002_2005.xls", 
                                sheet = "pdln12_edms")

# Filter data to keep only the relevant date
listanominal_2005 <- listanominal_2005 %>%
  filter(FECHA == "20051002") %>%
  rename(section = SEC, 
         listanominal = LISTA) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE))

# Read the 2005 CSV data
data_2005 <- read_csv("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2005/Ayu_Seccion_2005_No_LN.csv") 

# Convert column names to lowercase
data_2005 <- data_2005 %>% rename_with(tolower)
names(data_2005)

data_2005 <- data_2005 %>%
  rename(municipality = nombre_municipio, 
         section = seccion) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  mutate(across(pan:total, as.numeric)) %>%
  filter(!is.na(total) & total != 0)

# Collapse the data by municipality and section
data_2005_collapsed <- data_2005 %>%
  group_by(municipality, section) %>%
  summarise(across(pan:total, sum, na.rm = TRUE)) %>%
  rename(PAN = pan, 
         PRI = pri, 
         PRD = prd, 
         PT = pt, 
         PVEM = pvem, 
         PRS = prs, 
         PC = pc, 
         PRD_PRS = "prd-prs")

# Merge with the listanominal data
data_2005_collapsed <- data_2005_collapsed %>%
  left_join(listanominal_2005, by = "section") %>%
  mutate(turnout = total / listanominal) %>%
  filter(!is.na(turnout))

# Create uniqueid based on municipality
data_2005_collapsed <- data_2005_collapsed %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPULCO DE JUAREZ" ~ 12001,
    municipality == "ACATEPEC" ~ 12076,
    municipality == "AHUACUOTZINGO" ~ 12002,
      municipality == "AJUCHITLAN DEL PROGRESO" ~ 12003,
      municipality == "ALCOZAUCA" ~ 12004,
      municipality == "ALPOYECA" ~ 12005,
      municipality == "APAXTLA DE CASTREJON" ~ 12006,
      municipality == "ARCELIA" ~ 12007,
      municipality == "ATENANGO DEL RIO" ~ 12008,
      municipality == "ATLAMAJALCINTO DEL MONTE" ~ 12009,
      municipality == "ATLIXTAC" ~ 12010,
      municipality == "ATOYAC DE ALVAREZ" ~ 12011,
      municipality == "AYUTLA DE LOS LIBRES" ~ 12012,
      municipality == "AZOYU" ~ 12013,
      municipality == "BENITO JUAREZ" ~ 12014,
      municipality == "BUENAVISTA DE CUELLAR" ~ 12015,
      municipality == "CHILAPA DE ALVAREZ" ~ 12028,
      municipality == "CHILPANCINGO DE LOS BRAVO" ~ 12029,
      municipality == "COAHUAYUTLA  DE J. M. I." ~ 12016,
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
      municipality == "GRAL. CANUTO A. NERI" ~ 12031,
      municipality == "GRAL. HELIODORO CASTILLO" ~ 12032,
      municipality == "HUAMUXTITLAN" ~ 12033,
      municipality == "HUITZUCO DE LOS FIGUEROA" ~ 12034,
      municipality == "IGUALA DE LA INDEPENDENCIA" ~ 12035,
      municipality == "IGUALAPA" ~ 12036,
      municipality == "IXCATEOPAN DE C." ~ 12037,
      municipality == "JUAN R. ESCUDERO" ~ 12039,
      municipality == "LA UNION DE ISIDORO M. O." ~ 12068,
      municipality == "LEONARDO BRAVO" ~ 12040,
      municipality == "MALINALTEPEC" ~ 12041,
      municipality == "MARTIR DE CUILAPAN" ~ 12042,
      municipality == "METLATONOC" ~ 12043,
      municipality == "MOCHITLAN" ~ 12044,
      municipality == "OLINALA" ~ 12045,
      municipality == "OMETEPEC" ~ 12046,
      municipality == "PEDRO ASCENCIO A." ~ 12047,
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
      municipality == "TEPECOACUILCO" ~ 12059,
      municipality == "TETIPAC" ~ 12060,
      municipality == "TIXTLA DE GUERRERO" ~ 12061,
      municipality == "TLACOACHISTLAHUACA" ~ 12062,
      municipality == "TLACOAPA" ~ 12063,
      municipality == "TLALCHAPA" ~ 12064,
      municipality == "TLALIXTAQUILLA" ~ 12065,
      municipality == "TLAPA DE COMONFORT" ~ 12066,
      municipality == "TLAPEHUALA" ~ 12067,
      municipality == "XALPATLAHUAC" ~ 12069,
      municipality == "XOCHIHUEHUETLAN" ~ 12070,
      municipality == "XOCHISTLAHUACA" ~ 12071,
      municipality == "ZAPOTITLAN TABLAS" ~ 12072,
      municipality == "JOSE AZUETA" ~ 12038,
      municipality == "ZIRANDARO DE LOS CHAVEZ" ~ 12073,
      municipality == "ZITLALA" ~ 12074,
      municipality == "MARQUELIA" ~ 12077,
      TRUE ~ NA
    ))

# Calculate valid votes
data_2005_collapsed <- data_2005_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRD_PRS), na.rm = TRUE))

# Add year and month info
data_2005_collapsed <- data_2005_collapsed %>%
  mutate(year = 2005, month = "October")

###########################################
### Step 1: Process 2008 Data
###########################################

# Read the 2008 CSV data
data_2008 <- read_csv("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2008/Ayu_Seccion_2008.csv") 
# Convert column names to lowercase
data_2008 <- data_2008 %>% rename_with(tolower)

data_2008 <- data_2008 %>%
  rename(municipality = nombre_municipio, 
         section = seccion) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  mutate(listanominal = lista_nominal) %>% 
  mutate(across(listanominal:total, as.numeric)) %>%
  filter(!is.na(total) & total != 0) %>% 
  select(-lista_nominal)

# Collapse the data by municipality and section
data_2008_collapsed <- data_2008 %>%
  group_by(municipality, section) %>%
  summarise(across(pan:listanominal, sum, na.rm = TRUE)) %>%
  rename(PAN = pan, 
         PRI = pri, 
         PRD = prd, 
         PT = pt, 
         PVEM = pvem, 
         PC = pc, 
         PANAL = panal, 
         PAS = pas, 
         PAG = "alianza guerrero", 
         PRI_PVEM = "pri-pvem", 
         PT_PC = "pt-pc")

# Calculate turnout
data_2008_collapsed <- data_2008_collapsed %>%
  mutate(turnout = total / listanominal)

# Assign uniqueid
data_2008_collapsed <- data_2008_collapsed %>%
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
    municipality == "COAHUAYUTLA DE JOSE MARIA IZAZAGA" ~ 12016,
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
    municipality == "LA UNION DE ISIDORO MONTES DE OCA" ~ 12068,
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
    municipality == "ZIRANDARO" ~ 12073,
    municipality == "ZITLALA" ~ 12074,
    municipality == "MARQUELIA" ~ 12077,
    municipality == "COCHOAPA EL GRANDE" ~ 12078,
    municipality == "ILIATENCO" ~ 12081,
    municipality == "JOSE JOAQUIN DE HERRERA" ~ 12079,
    municipality == "JUCHITAN" ~ 12080,
    municipality == "ZIHUATANEJO DE AZUETA" ~ 12038,
    TRUE ~ NA
  ))

# Calculate valid votes
data_2008_collapsed <- data_2008_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PT_PC), na.rm = TRUE))

# Add year and month info
data_2008_collapsed <- data_2008_collapsed %>%
  mutate(year = 2008, month = "October")

###########################################
### Step 1: Process 2009 Data
###########################################

# Read Excel data for 2009 special elections
data_2009 <- read_excel("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2008/Resultados_eleccion_extraordinaria_2009.xls", sheet = "Extra Malinaltepec", range = "A8:J40") 
names(data_2009)

data_2009 <- data_2009 %>%
  rename(section = "...2", 
         PAN = "...4", 
         PRI = "...5", 
         PRD = "...6", 
         PT = "...7", 
         total = TOTAL) %>%
  filter(!is.na(total) & total != 0) %>%
  mutate(across(PAN:total, as.numeric), 
         municipality = "MALINALTEPEC EXTRAORDINARIO")

# Merge with all_months_years data
lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "GUERRERO") 

lista_nominal <- lista_nominal %>%
  filter(month == "March" & year == 2009) %>%
  rename(listanominal = lista)

data_2009_collapsed <- data_2009 %>%
  left_join(lista_nominal, by = "section") %>%
  mutate(turnout = total / listanominal)

# Add uniqueid
data_2009_collapsed <- data_2009_collapsed %>%
  mutate(uniqueid = 12041)

# Calculate valid votes
data_2009_collapsed <- data_2009_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PT), na.rm = TRUE))

# Add year and month info
data_2009_collapsed <- data_2009_collapsed %>%
  mutate(year = 2009, month = "April")

###########################################
### Step 1: Process 2012 Data
###########################################

# Read the 2012 Excel data
data_2012 <- read_excel("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2012/Ayu_Seccion_2012.xlsx", sheet = "Sheet1") %>%
  rename(municipality = MUNICIPIO, section = SECCION, total = TOTALES) %>%
  filter(!(municipality == "" | section == "" | is.na(total) | total == 0))

# Assign uniqueid
data_2012 <- data_2012 %>%
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
    municipality == "LA UNION DE ISIDORO MONTES DE OCA" ~ 12068,
    municipality == "LEONARDO BRAVO" ~ 12040,
    municipality == "MALINALTEPEC" ~ 12041,
    municipality == "MARTIR DE CUILAPAN" ~ 12042,
    municipality == "METLATONOC" ~ 12043,
    municipality == "MOCHITLAN" ~ 12044,
    municipality == "OLINALA" ~ 12045,
    municipality == "OMETEPEC" ~ 12046,
    municipality == "PEDRO ASCENCIO ALQUISIRAS" ~ 12047,
    municipality == "PETATLAN*" ~ 12048,
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
    municipality == "MARQUELIA" ~ 12077,
    municipality == "COCHOAPA EL GRANDE" ~ 12078,
    municipality == "ILIATENCO" ~ 12081,
    municipality == "JOSE JOAQUIN DE HERRERA" ~ 12079,
    municipality == "JUCHITAN" ~ 12080,
    municipality == "ZIHUATANEJO DE AZUETA" ~ 12038,
    TRUE ~ NA
  ))

# Collapse the data by municipality and section
data_2012_collapsed <- data_2012 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(PAN:total, sum, na.rm = TRUE))

# Merge with all_months_years data
lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "GUERRERO") 

lista_nominal <- lista_nominal %>%
  filter(month == "July" & year == 2012) %>%
  rename(listanominal = lista)

# Merge with the all_months_years data
data_2012_collapsed <- data_2012_collapsed %>%
  left_join(lista_nominal, by = c("section" = "section"))

# Calculate turnout and valid votes
data_2012_collapsed <- data_2012_collapsed %>%
  mutate(turnout = total / listanominal) %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:PRD_PT_PC), na.rm = TRUE))

# Add year and month info
data_2012_collapsed <- data_2012_collapsed %>%
  mutate(year = 2012, month = "July")
summary(data_2012_collapsed)

###########################################
### Step 1: Process 2015 Data
###########################################
data_2015 <- read_excel("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2015/Ayuntamientos_Gro_2015.xlsx", 
                        sheet = "Ayuntamientos") %>%
  mutate(across(uniqueid:"coal prd pt pan", as.numeric))

# Handle coalitions
data_2015 <- data_2015 %>%
  mutate(PRI_PVEM2 = ifelse(`coal pri pvem` == 1, PRI + PVEM + PRI_PVEM, NA),
         PRI = ifelse(`coal pri pvem` == 1, NA, PRI),
         PVEM = ifelse(`coal pri pvem` == 1, NA, PVEM),
         PRD_PT2 = ifelse(`coal prd pt` == 1, PRD + PT + PRD_PT, NA),
         PRD = ifelse(`coal prd pt` == 1, NA, PRD),
         PT = ifelse(`coal prd pt` == 1, NA, PT),
         PRI_PVEM_PANAL2 = ifelse(`coal pri pvem panal` == 1, PRI + PVEM + PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL, NA),
         PRI = ifelse(`coal pri pvem panal` == 1, NA, PRI),
         PVEM = ifelse(`coal pri pvem panal` == 1, NA, PVEM),
         PANAL = ifelse(`coal pri pvem panal` == 1, NA, PANAL),
         PRD_PT_PAN2 = ifelse(`coal prd pt pan` == 1, PRD + PT + PAN + PRD_PT + PRD_PAN + PT_PAN, NA),
         PRD = ifelse(`coal prd pt pan` == 1, NA, PRD),
         PT = ifelse(`coal prd pt pan` == 1, NA, PT),
         PAN = ifelse(`coal prd pt pan` == 1, NA, PAN))

# Rename and drop columns after coalition calculation
data_2015 <- data_2015 %>%
  select(-c(PRI_PVEM_PANAL, PRI_PVEM, PRI_PANAL, PVEM_PANAL, PRD_PT, PRD_PT_PAN, PRD_PAN, PT_PAN)) %>%
  rename(PRI_PVEM_PANAL = PRI_PVEM_PANAL2, 
         PRI_PVEM = PRI_PVEM2, 
         PRD_PT = PRD_PT2, 
         PRD_PT_PAN = PRD_PT_PAN2)

# Collapse data by section, municipality, and uniqueid
data_2015_collapsed <- data_2015 %>%
  select(-c(turnout:"coal prd pt pan")) %>% 
  group_by(section, municipality, uniqueid) %>%
  summarise(across(PAN:PRD_PT_PAN, sum, na.rm = TRUE)) %>% 
  select(-c(no_reg,nulo))

# Calculate valid votes
data_2015_collapsed <- data_2015_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(PAN,PRI,PRD,
                     PT,PVEM,MC,
                     PANAL,MORENA,
                     PH,PES,PPG,
                     CI_1,PRI_PVEM,
                     PRD_PT,
                     PRD_PT_PAN,
                     PRI_PVEM_PANAL, na.rm = TRUE)) %>% 
  mutate(turnout = ifelse(listanominal == 0, NA, total/listanominal))

# Add election year and other metadata
data_2015_collapsed <- data_2015_collapsed %>%
  mutate(year = 2015, 
         month = ifelse(municipality == "TIXTLA DE GUERRERO", "November", "June"))

#------------------------------------------------------------
# Step 1: Import communal candidates 2018.xlsx, sheet MUNCAND

mun_data <- fread("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2018/communal_candidates_2018.csv")

# Step 3: For each var in PAN_PRD_MC - MORENA_ES,
# replace var=0 if var==. and var=1 if var>0
# NA in R means missing. Replace NA with 0, and any value >0 with 1.
mun_data <- mun_data %>%
  select(-c(NUM_VOTOS_VALIDOS:LISTA_NOMINAL)) %>% 
  mutate(across(PAN_PRD_MC:PAN_PRD, ~ if_else(is.na(.), 0, if_else(. > 0, 1, 0))))

# Step 4: rename * coal_* and rename coal_MUNICIPIO municipality
# This means prepend 'coal_' to all columns except MUNICIPIO?
# The code: rename * coal_* renames all variables starting from scratch with coal_
# We'll do:
mun_data <- mun_data %>%
  rename_with(~paste0("coal_", .), PAN_PRD_MC:PAN_PRD)

# Now rename coal_MUNICIPIO to municipality
mun_data <- mun_data %>%
  rename(municipality = MUNICIPIO) %>% 
  select(municipality,c(coal_PAN_PRD_MC:coal_PAN_PRD))

#------------------------------------------------------------
# Step 6: import delimited "2018_SEE_GRO_CAS_AYUN.csv"
see_data <- fread("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2018/2018_SEE_GRO_CAS_AYUN.csv")

names(see_data)
# replace PAN_PRD = v39 if PAN_PRD==.
# replace PRD_MC = v40 if PRD_MC==.
see_data <- see_data %>% select(-c(39, 40))

# rename (CAND_IND1 CAND_IND2 CAND_IND4 CAND_IND5 CAND_IND7 CAND_IND8 CAND_IND9) (CI_1 CI_2 CI_4 CI_5 CI_7 CI_8 CI_9)
see_data <- see_data %>%
  rename(CI_1 = CAND_IND1,
         CI_2 = CAND_IND2,
         CI_4 = CAND_IND4,
         CI_5 = CAND_IND5,
         CI_7 = CAND_IND7,
         CI_8 = CAND_IND8,
         CI_9 = CAND_IND9)
names(see_data)
# Drop no packet delivered cases and AYUTLA DE LOS LIBRES municipality
# drop if TOTAL_VOTOS==0 | TOTAL_VOTOS==.
see_data <- see_data %>%
  filter(!is.na(TOTAL_VOTOS) & TOTAL_VOTOS != 0)

# rename MUNICIPIO municipality, SECCION section
see_data <- see_data %>%
  rename(municipality = MUNICIPIO,
         section = SECCION)
names(see_data)
# rename NA PANAL, IH PIH, CG PCG, ES PES
see_data <- see_data %>%
  rename(PANAL = V18,
         PIH = IH,
         PCG = CG,
         PES = ES)

# rename TOTAL_VOTOS total, LISTA_NOMINAL listanominal
see_data <- see_data %>%
  rename(total = TOTAL_VOTOS,
         listanominal = LISTA_NOMINAL)

# gen uniqueid=0 and replace based on municipality name
# Create a named vector for uniqueid mapping:
uniqueid_map <- c("ACAPULCO DE JUAREZ"=12001, "ACATEPEC"=12076, "AHUACUOTZINGO"=12002,
                  "AJUCHITLAN DEL PROGRESO"=12003, "ALCOZAUCA DE GUERRERO"=12004,
                  "ALPOYECA"=12005, "APAXTLA DE CASTREJON"=12006, "APAXTLA"=12006,
                  "ARCELIA"=12007, "ATENANGO DEL RIO"=12008,
                  "ATLAMAJALCINGO DEL MONTE"=12009, "ATLIXTAC"=12010, "ATOYAC DE ALVAREZ"=12011,
                  "AYUTLA DE LOS LIBRES"=12012, "AZOYU"=12013, "BENITO JUAREZ"=12014,
                  "BUENAVISTA DE CUELLAR"=12015, "CHILAPA DE ALVAREZ"=12028,
                  "CHILPANCINGO DE LOS BRAVO"=12029,
                  "COAHUAYUTLA DE JOSE MARIA IZAZAGA"=12016, "COAHUAYUTLA DE JOSE MA IZAZAGA"=12016,
                  "COCHOAPA EL GRANDE"=12078, "COCULA"=12017, "COPALA"=12018, "COPALILLO"=12019,
                  "COPANATOYAC"=12020, "COYUCA DE BENITEZ"=12021, "COYUCA DE CATALAN"=12022,
                  "CUAJINICUILAPA"=12023, "CUALAC"=12024, "CUAUTEPEC"=12025,
                  "CUETZALA DEL PROGRESO"=12026, "CUTZAMALA DE PINZON"=12027,
                  "EDUARDO NERI"=12075, "FLORENCIO VILLARREAL"=12030,
                  "GENERAL CANUTO A. NERI"=12031, "GENERAL HELIODORO CASTILLO"=12032,
                  "HUAMUXTITLAN"=12033, "HUITZUCO DE LOS FIGUEROA"=12034,
                  "IGUALA DE LA INDEPENDENCIA"=12035, "IGUALAPA"=12036, "ILIATENCO"=12081,
                  "IXCATEOPAN DE CUAUHTEMOC"=12037, "JOSE JOAQUIN DE HERRERA"=12079,
                  "JUAN R. ESCUDERO"=12039, "JUCHITAN"=12080,
                  "LA UNION DE ISIDORO MONTES DE OCA"=12068, "LEONARDO BRAVO"=12040,
                  "MALINALTEPEC"=12041, "MARQUELIA"=12077, "MARTIR DE CUILAPAN"=12042,
                  "METLATONOC"=12043, "MOCHITLAN"=12044, "OLINALA"=12045, "OMETEPEC"=12046,
                  "PEDRO ASCENCIO ALQUISIRAS"=12047, "PETATLAN"=12048, "PETATLAN*"=12048,
                  "PILCAYA"=12049, "PUNGARABATO"=12050, "QUECHULTENANGO"=12051,
                  "SAN LUIS ACATLAN"=12052, "SAN MARCOS"=12053, "SAN MIGUEL TOTOLAPAN"=12054,
                  "TAXCO DE ALARCON"=12055, "TECOANAPA"=12056, "TECPAN DE GALEANA"=12057,
                  "TELOLOAPAN"=12058, "TEPECOACUILCO DE TRUJANO"=12059,
                  "TETIPAC"=12060, "TIXTLA DE GUERRERO"=12061, "TLACOACHISTLAHUACA"=12062,
                  "TLACOAPA"=12063, "TLALCHAPA"=12064, "TLALIXTAQUILLA DE MALDONADO"=12065,
                  "TLAPA DE COMONFORT"=12066, "TLAPEHUALA"=12067,
                  "XALPATLAHUAC"=12069, "XOCHIHUEHUETLAN"=12070, "XOCHISTLAHUACA"=12071,
                  "ZAPOTITLAN TABLAS"=12072, "ZIHUATANEJO DE AZUETA"=12038,
                  "ZIRANDARO"=12073, "ZITLALA"=12074)

see_data <- see_data %>%
  mutate(uniqueid = uniqueid_map[municipality])

vars_to_collapse2 <- c("PAN","PRI","PRD","PVEM","PT","MC","PANAL","MORENA","PES","PPG","PIH","PCG","PSM","PSG",
                       "PAN_PRD","PRD_MC","MORENA_ES","PRI_PVEM","PAN_PRD_MC",
                       "CI_1","CI_2","CI_4","CI_5","CI_7","CI_8","CI_9","total","listanominal")


see_data_sum <- see_data %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(all_of(vars_to_collapse2), sum, na.rm = TRUE), .groups = "drop")

see_data_sum <- see_data_sum %>%
  left_join(mun_data, by = "municipality")

# Coalition logic:
# replace PRI_PVEM = PRI_PVEM + PRI + PVEM if coal_PRI_PVEM==1
see_data_sum <- see_data_sum %>%
  mutate(
    PRI_PVEM = ifelse(coal_PRI_PVEM == 1, PRI_PVEM + PRI + PVEM, PRI_PVEM),
    PRI = ifelse(coal_PRI_PVEM == 1, NA, PRI),
    PVEM = ifelse(coal_PRI_PVEM == 1, NA, PVEM)
  )

# rename MORENA_ES to MORENA_PES
see_data_sum <- see_data_sum %>%
  rename(MORENA_PES = MORENA_ES)

# replace MORENA_PES= MORENA_PES + MORENA + PES if coal_MORENA_ES==1
see_data_sum <- see_data_sum %>%
  mutate(
    MORENA_PES = ifelse(coal_MORENA_ES == 1, MORENA_PES + MORENA + PES, MORENA_PES),
    MORENA = ifelse(coal_MORENA_ES == 1, NA, MORENA),
    PES = ifelse(coal_MORENA_ES == 1, NA, PES)
  )

# PAN_PRD_MC2 = PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC if coal_PAN_PRD_MC==1
# Wait, PAN_MC not previously defined? It's mentioned in code. We must assume it exists.
see_data_sum <- see_data_sum %>%
  mutate(
    PAN_PRD_MC2 = ifelse(coal_PAN_PRD_MC == 1, PAN_PRD_MC + PAN_PRD  + PRD_MC + PAN + PRD + MC, NA),
    PAN = ifelse(coal_PAN_PRD_MC == 1, NA, PAN),
    PRD = ifelse(coal_PAN_PRD_MC == 1, NA, PRD),
    MC = ifelse(coal_PAN_PRD_MC == 1, NA, MC)
  )


# PAN_PRD2 = PAN_PRD + PAN + PRD if coal_PAN_PRD==1
see_data_sum <- see_data_sum %>%
  mutate(
    PAN_PRD2 = ifelse(coal_PAN_PRD == 1, PAN_PRD + PAN + PRD, NA),
    PAN = ifelse(coal_PAN_PRD == 1, NA, PAN),
    PRD = ifelse(coal_PAN_PRD == 1, NA, PRD)
  )


# PRD_MC2 = PRD_MC + MC + PRD if coal_PRD_MC==1
see_data_sum <- see_data_sum %>%
  mutate(
    PRD_MC2 = ifelse(coal_PRD_MC == 1, PRD_MC + MC + PRD, NA),
    MC = ifelse(coal_PRD_MC == 1, NA, MC),
    PRD = ifelse(coal_PRD_MC == 1, NA, PRD)
  )

# drop PAN_PRD_MC PAN_PRD PAN_MC PRD_MC
see_data_sum <- see_data_sum %>% select(-PAN_PRD_MC, -PAN_PRD, -PRD_MC)

# rename PAN_PRD_MC2 PAN_PRD_MC, PAN_PRD2 PAN_PRD, PRD_MC2 PRD_MC
see_data_sum <- see_data_sum %>%
  rename(PAN_PRD_MC = PAN_PRD_MC2,
         PAN_PRD = PAN_PRD2,
         PRD_MC = PRD_MC2)

# order PAN_PRD PRD_MC MORENA_PES PRI_PVEM PAN_PRD_MC a(PSG)
# Reordering columns in R:
col_order <- c("PSG", "PAN_PRD", "PRD_MC", "MORENA_PES", "PRI_PVEM", "PAN_PRD_MC")
existing_cols <- names(see_data_sum)
# We want PSG at the start, then the rest of the columns:
col_order_final <- c("PSG", setdiff(existing_cols, "PSG"))
see_data_sum <- see_data_sum %>% select(all_of(col_order_final))

# egen valid= rowtotal (PAN PRI PRD PVEM PT MC PANAL MORENA PES PPG PIH PCG PSM PSG PAN_PRD PRD_MC MORENA_PES PRI_PVEM PAN_PRD_MC CI_1 CI_2 CI_4 CI_5 CI_7 CI_8 CI_9)
valid_vars <- c("PAN","PRI","PRD","PVEM","PT","MC","PANAL","MORENA","PES","PPG","PIH","PCG","PSM","PSG",
                "PAN_PRD","PRD_MC","MORENA_PES","PRI_PVEM","PAN_PRD_MC",
                "CI_1","CI_2","CI_4","CI_5","CI_7","CI_8","CI_9")
see_data_sum <- see_data_sum %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm = TRUE),
         turnout = total/listanominal)

see_data_sum <- see_data_sum %>%
  select(-starts_with("coal_"))

# g year=2018, month="July", STATE="GUERRERO"
see_data_sum <- see_data_sum %>%
  mutate(year=2018,
         month="July",
         STATE="GUERRERO")

summary(see_data_sum)

#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

# Load the 2021 dataset from the excel
data_2021 <- read_excel("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2021/2021_SEE_AYUN_GRO_SECCI‡N.xlsx", skip = 6) %>% 
  filter(!MUNICIPIO == "ILIATENCO")

data_ext <- read_excel("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2021/2021_SEE_AYUN_GRO_CAS_ILIATENCO.xlsx", skip = 6)

data_2021 <- bind_rows(data_2021,
                       data_ext)

names(data_2021)

# Rename columns
data_2021 <- data_2021 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL_VOTOS,
                no_reg = NUM_VOTOS_CAN_NREG,
                nulos = NUM_VOTOS_NULOS,
                valid = NUM_VOTOS_VALIDOS,
                FXM = FXP) %>%
  rename_with(~ gsub("CAND_IND", "CI_", .x), starts_with("cand_ind")) %>% 
  dplyr::mutate(
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(section)
  )

# Assign uniqueids
data_2021 <- data_2021 %>% 
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
    municipality == "COAHUAYUTLA DE JOSE MARIA IZAZAGA" ~ 12016,
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
    municipality == "LA UNION DE ISIDORO MONTES DE OCA" ~ 12068,
    municipality == "LEONARDO BRAVO" ~ 12040,
    municipality == "MALINALTEPEC" ~ 12041,
    municipality == "MARTIR DE CUILAPAN" ~ 12042,
    municipality == "METLATONOC" ~ 12043,
    municipality == "MOCHITLAN" ~ 12044,
    municipality == "OLINALA" ~ 12045,
    municipality == "OMETEPEC" ~ 12046,
    municipality == "PEDRO ASCENCIO ALQUISIRAS" ~ 12047,
    municipality == "PETATLAN*" ~ 12048,
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
    municipality == "MARQUELIA" ~ 12077,
    municipality == "COCHOAPA EL GRANDE" ~ 12078,
    municipality == "ILIATENCO" ~ 12081,
    municipality == "JOSE JOAQUIN DE HERRERA" ~ 12079,
    municipality == "JUCHITAN" ~ 12080,
    municipality == "ZIHUATANEJO DE AZUETA" ~ 12038,
    TRUE ~ NA
  ))

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:listanominal, PVEM_MORENA), 
                          sum, na.rm = TRUE))

# Calculate valid votes and final details
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    year = 2021,
    month = case_when(
      municipality == "ILIATENCO" ~ "November",
      TRUE ~ "June"
    )
  )

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 12) %>% 
  select(yr, inegi, coal1, coal2, coal3, coal4) %>% 
  rename(
    year = yr,
    uniqueid = inegi) %>% 
  mutate(
    across(
      coal1:coal4,
      ~ str_replace_all(., "-", "_") |> 
        str_replace_all(regex("PNA", ignore_case = TRUE), "PANAL") |> 
        str_to_upper()
    )
  )

process_coalitions <- function(electoral_data, magar_data) {
  
  # Store grouping and ungroup
  original_groups <- dplyr::groups(electoral_data)
  merged <- electoral_data %>%
    ungroup() %>%
    left_join(magar_data, by = c("uniqueid", "year")) %>%
    as.data.frame()
  
  # Get party columns (exclude metadata)
  metadata_cols <- c("uniqueid", "section", "municipality", "year", "month", "no_reg", "nulos", 
                     "total", "CI_2", "CI_1", "listanominal", "valid", "turnout",
                     "coal1", "coal2", "coal3", "coal4")
  party_cols <- setdiff(names(merged), metadata_cols)
  party_cols <- party_cols[sapply(merged[party_cols], is.numeric)]
  
  # Get unique coalitions
  all_coalitions <- unique(c(merged$coal1, merged$coal2, merged$coal3, merged$coal4))
  all_coalitions <- all_coalitions[all_coalitions != "NONE" & !is.na(all_coalitions)]
  
  # Helper: find columns belonging to a coalition
  get_coalition_cols <- function(coal_name) {
    parties <- strsplit(coal_name, "_")[[1]]
    party_cols[sapply(party_cols, function(col) {
      all(strsplit(col, "_")[[1]] %in% parties)
    })]
  }
  
  # Calculate coalition votes (with temp names to avoid conflicts)
  for (coal in all_coalitions) {
    merged[[paste0("NEW_", coal)]] <- sapply(1:nrow(merged), function(i) {
      active <- c(merged$coal1[i], merged$coal2[i], merged$coal3[i], merged$coal4[i])
      if (coal %in% active) {
        sum(unlist(merged[i, get_coalition_cols(coal)]), na.rm = TRUE)
      } else {
        0
      }
    })
  }
  
  # Zero out constituent columns
  for (i in 1:nrow(merged)) {
    active <- c(merged$coal1[i], merged$coal2[i], merged$coal3[i], merged$coal4[i])
    active <- active[active != "NONE" & !is.na(active)]
    for (coal in active) {
      merged[i, get_coalition_cols(coal)] <- 0
    }
  }
  
  # Rename temp columns to final names
  for (coal in all_coalitions) {
    merged[[coal]] <- merged[[paste0("NEW_", coal)]]
    merged[[paste0("NEW_", coal)]] <- NULL
  }
  
  # Convert to tibble and restore grouping
  result <- as_tibble(merged)
  if (length(original_groups) > 0) {
    result <- result %>% group_by(!!!original_groups)
  }
  
  return(result)
}

# Apply coalition processing function
collapsed_2021 <- process_coalitions(collapsed_2021, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

#####################################
### PROCESSING DATA FOR 2024 -------
#####################################

# Load the 2024 dataset from the CSV
data_2024 <- read_csv("../../../Data/Raw Electoral Data/Guerrero - 1996, 1999, 2002, 2005, 2008, 2012,2015,2018,2021,2024/2024/2024_SEE_AYUN_GRO_CAS.csv")

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL_VOTOS,
                no_reg = NUM_VOTOS_CAN_NREG,
                nulos = NUM_VOTOS_NULOS,
                valid = NUM_VOTOS_VALIDOS,
                FXM = FXMG,
                MAGRO = MA,
                PRG = REGENERACION
  ) %>%
  dplyr::mutate(
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(section)
  ) %>% 
  dplyr::filter(section > 0)

# Assign uniqueids
data_2024 <- data_2024 %>% 
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
    municipality == "COAHUAYUTLA DE JOSE MARIA IZAZAGA" ~ 12016,
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
    municipality == "LA UNION DE ISIDORO MONTES DE OCA" ~ 12068,
    municipality == "LEONARDO BRAVO" ~ 12040,
    municipality == "MALINALTEPEC" ~ 12041,
    municipality == "MARTIR DE CUILAPAN" ~ 12042,
    municipality == "METLATONOC" ~ 12043,
    municipality == "MOCHITLAN" ~ 12044,
    municipality == "OLINALA" ~ 12045,
    municipality == "OMETEPEC" ~ 12046,
    municipality == "PEDRO ASCENCIO ALQUISIRAS" ~ 12047,
    municipality == "PETATLAN*" ~ 12048,
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
    municipality == "MARQUELIA" ~ 12077,
    municipality == "COCHOAPA EL GRANDE" ~ 12078,
    municipality == "ILIATENCO" ~ 12081,
    municipality == "JOSE JOAQUIN DE HERRERA" ~ 12079,
    municipality == "JUCHITAN" ~ 12080,
    municipality == "ZIHUATANEJO DE AZUETA" ~ 12038,
    municipality == "LAS VIGAS" ~ 12082,
    municipality == "SAN NICOLAS" ~ 12085,
    municipality == "SANTA CRUZ DEL RINCON" ~ 12084,
    TRUE ~ NA
  ))

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:listanominal), 
                          sum, na.rm = TRUE))

# Calculate valid votes and final details
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    year = 2024,
    month = "June"
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

guerrero_all <- bind_rows(data_1999_collapsed,
                          data_2002_collapsed,
                          data_2005_collapsed,
                          data_2008_collapsed,
                          data_2009_collapsed,
                          data_2012_collapsed,
                          data_2015_collapsed,
                          see_data_sum,
                          collapsed_2021,
                          collapsed_2024) 


data.table::fwrite(guerrero_all,"../../../Processed Data/guerrero/guerrero_process_raw_data.csv")

