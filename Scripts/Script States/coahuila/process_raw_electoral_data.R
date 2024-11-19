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
##########################
# Year 1996
##########################
# Read CSV file for 1996
data_1996 <- read_csv("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayu_Seccion_1996_No_LN.csv")
names(data_1996)
# Rename columns as needed
colnames(data_1996) <- c("municipality", "section", "pan", "pri", "prd", "pc", "pt", "pvem", "prt", "pps", "pdm", "nulos", "total")

# Drop rows where 'total' is missing or zero
data_1996 <- data_1996 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse the data by municipality and section, summing party columns
data_1996 <- data_1996 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(pan, pri, prd, pc, pt, pvem, prt, pps, pdm,nulos, total), sum, na.rm = TRUE))

# Rename columns for clarity
data_1996 <- data_1996 %>%
  dplyr::rename(PAN = pan, 
                PRI = pri, 
                PRD = prd, 
                PCC = pc, 
                PT = pt, 
                PVEM = pvem, 
                PRT = prt, 
                PPS = pps, 
                PDM = pdm)

# Assign unique municipality IDs
data_1996 <- data_1996 %>%
  dplyr::mutate(uniqueid = case_when(
    municipality == "Abasolo" ~ 5001,
    str_detect(municipality, "Acu") ~ 5002,
    municipality == "Allende" ~ 5003,
    municipality == "Arteaga" ~ 5004,
    municipality == "Candela" ~ 5005,
    str_detect(municipality, "Casta") ~ 5006,
    str_detect(municipality, "Cuatroci") ~ 5007,
    municipality == "Escobedo" ~ 5008,
    municipality == "Francisco I. Madero" ~ 5009,
    municipality == "Frontera" ~ 5010,
    municipality == "General Cepeda" ~ 5011,
    municipality == "Guerrero" ~ 5012,
    municipality == "Hidalgo" ~ 5013,
    str_detect(municipality, "Jim") ~ 5014,
    str_detect(municipality, "rez") ~ 5015,
    municipality == "Lamadrid" ~ 5016,
    municipality == "Matamoros" ~ 5017,
    municipality == "Monclova" ~ 5018,
    municipality == "Morelos" ~ 5019,
    str_detect(municipality, "zquiz") ~ 5020,
    municipality == "Nadadores" ~ 5021,
    municipality == "Nava" ~ 5022,
    municipality == "Ocampo" ~ 5023,
    municipality == "Parras" ~ 5024,
    municipality == "Piedras Negras" ~ 5025,
    municipality == "Progreso" ~ 5026,
    municipality == "Ramos Arizpe" ~ 5027,
    municipality == "Sabinas" ~ 5028,
    municipality == "Sacramento" ~ 5029,
    municipality == "Saltillo" ~ 5030,
    municipality == "San Buenaventura" ~ 5031,
    municipality == "San Juan de Sabinas" ~ 5032,
    municipality == "San Pedro" ~ 5033,
    municipality == "Sierra Mojada" ~ 5034,
    str_detect(municipality, "Torre") ~ 5035,
    municipality == "Viesca" ~ 5036,
    str_detect(municipality, "Villa Uni") ~ 5037,
    municipality == "Zaragoza" ~ 5038,
    TRUE ~ NA
  ))

# Calculate 'valid' votes (total votes for valid parties)
data_1996 <- data_1996 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD, PCC, PT, PVEM, PRT, PPS, PDM)), na.rm = TRUE))

# Add year and month columns
data_1996 <- data_1996 %>%
  dplyr::mutate(year = 1996, month = "November")

ln_all_months_years <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>%
  dplyr::filter(state == "COAHUILA" & month == "March" & year == 1999) %>%
  dplyr::select(section,lista)

# Merge Lista Nominal data with the collapsed data
data_1996 <- data_1996 %>%
  left_join(ln_all_months_years, by = "section")

names(data_1996)
# Calculate the valid votes
data_1996 <- data_1996 %>%
  dplyr::rename(listanominal=lista) %>% 
  dplyr::mutate(turnout = total / listanominal)

summary(data_1996)

##########################
# Year 1999
##########################
# Read Excel file for 1999
data_1999 <- read_excel("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayu_Seccion_1999_No_LN.xlsx", sheet = "Ayu_Seccion_1999_No_LN")

# Rename columns
data_1999 <- data_1999 %>%
  dplyr::rename(municipality = MUNICIPIO, 
                section = SECCION,
                nulos = NULOS, 
                total = TOTAL)
# Drop rows where 'total' is missing or zero
data_1999 <- data_1999 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Collapse the data by municipality and section
data_1999 <- data_1999 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN:total), sum, na.rm = TRUE))

# Assign unique municipality IDs (repeating the same logic)
data_1999 <- data_1999 %>%
  dplyr::mutate(uniqueid = case_when(
    municipality == "Abasolo" ~ 5001,
    str_detect(municipality, "Acu") ~ 5002,
    municipality == "Allende" ~ 5003,
    municipality == "Arteaga" ~ 5004,
    municipality == "Candela" ~ 5005,
    str_detect(municipality, "Casta") ~ 5006,
    str_detect(municipality, "Cuatroci") ~ 5007,
    municipality == "Escobedo" ~ 5008,
    municipality == "Francisco I. Madero" | municipality == "Fco. I. Madero" ~ 5009,
    municipality == "Frontera" ~ 5010,
    municipality == "General Cepeda" ~ 5011,
    municipality == "Guerrero" ~ 5012,
    municipality == "Hidalgo" ~ 5013,
    str_detect(municipality, "Jim") ~ 5014,
    str_detect(municipality, "rez") ~ 5015,
    municipality == "Lamadrid" ~ 5016,
    municipality == "Matamoros" ~ 5017,
    municipality == "Monclova" ~ 5018,
    municipality == "Morelos" ~ 5019,
    str_detect(municipality, "zquiz") ~ 5020,
    municipality == "Nadadores" ~ 5021,
    municipality == "Nava" ~ 5022,
    municipality == "Ocampo" ~ 5023,
    municipality == "Parras" ~ 5024,
    municipality == "Piedras Negras" ~ 5025,
    municipality == "Progreso" ~ 5026,
    municipality == "Ramos Arizpe" ~ 5027,
    municipality == "Sabinas" ~ 5028,
    municipality == "Sacramento" ~ 5029,
    municipality == "Saltillo" ~ 5030,
    municipality == "San Buenaventura" ~ 5031,
    municipality == "San Juan de Sabinas" ~ 5032,
    municipality == "San Pedro" ~ 5033,
    municipality == "Sierra Mojada" ~ 5034,
    str_detect(municipality, "Torre") ~ 5035,
    municipality == "Viesca" ~ 5036,
    str_detect(municipality, "Villa Uni") ~ 5037,
    municipality == "Zaragoza" ~ 5038,
    TRUE ~ NA
  ))

# Calculate 'valid' votes (total votes for valid parties)
data_1999 <- data_1999 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN:COALICION)), na.rm = TRUE))

# Add year and month columns
data_1999 <- data_1999 %>%
  dplyr::mutate(year = 1999, month = "September")

# Load the Lista Nominal 2019 data and filter by criteria
ln_all_months_years <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>%
  dplyr::filter(state == "COAHUILA" & month == "September" & year == 1999) %>%
  dplyr::select(section,lista)

# Merge Lista Nominal data with the collapsed data
data_1999 <- data_1999 %>%
  left_join(ln_all_months_years, by = "section")

names(data_1999)
# Calculate the valid votes
data_1999 <- data_1999 %>%
  dplyr::rename(listanominal=lista,
                PAN_PRD_PT_PVEM = COALICION) %>% 
  dplyr::mutate(turnout = total / listanominal)

summary(data_1999)

##########################
# Year 2002
##########################
# Read CSV file for 2002

# Step 1: Load the CSV file
data_2002 <- read_csv("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayu_Seccion_2002.csv")
names(data_2002)
# Step 2: Rename columns
data_2002 <- data_2002 %>%
  dplyr::rename(
    municipality = MUNICIPIO,
    section = SECCION,
    listanominal = LNOM,
    nulos = NULOS,
    total = TOTAL,
  )

# Step 3: Drop rows where 'total' is missing or zero
data_2002 <- data_2002 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Step 4: Convert relevant columns from string to numeric
data_2002 <- data_2002 %>%
  dplyr::mutate(across(listanominal:total, as.numeric))

# Step 5: Collapse (sum) data by municipality and section for the listed variables
data_2002 <- data_2002 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(listanominal:total, sum, na.rm = TRUE))


# Step 7: Merge with coalition data
coalitions_data <- read_csv("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayu_Seccion_2002_Colaitions_by_Municipality.csv") %>% 
  dplyr::rename("municipality"="municipio")

data_2002 <- left_join(data_2002, coalitions_data, 
                   by = c("municipality"))

# Step 8: Drop the _merge variable (not applicable in R)
names(data_2002)
# Step 9: Handle PRD-PT coalition cases
data_2002 <- data_2002 %>%
  dplyr::mutate(
    prd_pt = ifelse(CCP1 == "PRD,PT ", rowSums(PRD,PT,CCM1,na.rm = TRUE), 0),
    PRD = ifelse(CCP1 == "PRD,PT ", 0, PRD),
    PT = ifelse(CCP1 == "PRD,PT ", 0, PT),
    CCM1 = ifelse(CCP1 == "PRD,PT ", 0, CCM1))

# Step 10: Handle PRD-PT-PCD coalition
data_2002 <- data_2002 %>%
  dplyr::mutate(
    prd_pt_pcd = ifelse(CCP1 == "PRD,PT,PCD ", rowSums(PRD,PT,PCD,CCM1,na.rm = TRUE), 0),
    PRD = ifelse(CCP1 == "PRD,PT,PCD ", 0, PRD),
    PT = ifelse(CCP1 == "PRD,PT,PCD ", 0, PT),
    PCD = ifelse(CCP1 == "PRD,PT,PCD ", 0, PCD),
    CCM1 = ifelse(CCP1 == "PRD,PT,PCD ", 0, CCM1))

# Step 11: Handle PRD-PT-UDC coalition
data_2002 <- data_2002 %>%
  dplyr::mutate(
    prd_pt_pudc = ifelse(CCP1 == "PRD,PT,UDC ", rowSums(PRD,PT,PUDC,CCM1,na.rm = TRUE), 0),
    PRD = ifelse(CCP1 == "PRD,PT,UDC ", 0, PRD),
    PT = ifelse(CCP1 == "PRD,PT,UDC ", 0, PT),
    PUDC = ifelse(CCP1 == "PRD,PT,UDC ", 0, PUDC),
    CCM1 = ifelse(CCP1 == "PRD,PT,UDC ", 0, CCM1))

# Step 12: Handle PRD-PT-UDC-PCC coalition
data_2002 <- data_2002 %>%
  dplyr::mutate(
    prd_pt_pudc_pcc = ifelse(CCP1 == "PRD,PT,UDC,PCC ", rowSums(PRD,PT,PUDC,PCC,CCM1,na.rm = TRUE), 0),
    PRD = ifelse(CCP1 == "PRD,PT,UDC,PCC ", 0, PRD),
    PT = ifelse(CCP1 == "PRD,PT,UDC,PCC ", 0, PT),
    PUDC = ifelse(CCP1 == "PRD,PT,UDC,PCC ", 0, PUDC),
    PCC = ifelse(CCP1 == "PRD,PT,UDC,PCC ", 0, PCC),
    CCM1 = ifelse(CCP1 == "PRD,PT,UDC,PCC ", 0, CCM1))

# Step 13: Handle PRD-UDC coalition
data_2002 <- data_2002 %>%
  dplyr::mutate(
    prd_pudc = ifelse(CCP1 == "PRD,UDC ", rowSums(PRD,PUDC,CCM1,na.rm = TRUE), 0),
    PRD = ifelse(CCP1 == "PRD,UDC ", 0, PRD),
    PUDC = ifelse(CCP1 == "PRD,UDC ", 0, PUDC),
    CCM1 = ifelse(CCP1 == "PRD,UDC ", 0, CCM1))

data_2002 <- data_2002 %>%
  dplyr::mutate(
    prd_pudc_pcc_pl = ifelse(CCP1 == "PRD,UDC,PCC,PL", (PRD + PUDC + PCC + PL + CCM1), 0),
    PRD = ifelse(CCP1 == "PRD,UDC,PCC,PL", 0, PRD),
    PUDC = ifelse(CCP1 == "PRD,UDC,PCC,PL", 0, PUDC),
    PCC = ifelse(CCP1 == "PRD,UDC,PCC,PL", 0, PCC),
    PL = ifelse(CCP1 == "PRD,UDC,PCC,PL", 0, PL),
    CCM1 = ifelse(CCP1 == "PRD,UDC,PCC,PL", 0, CCM1))

# Step 15: Handle PT-PCC coalition
data_2002 <- data_2002 %>%
  dplyr::mutate(
    pt_pcc = ifelse(CCP1 == "PT,PCC ", rowSums(PT,PCC,CCM1,na.rm = TRUE), 0),
    PT = ifelse(CCP1 == "PT,PCC ", 0, PT),
    PCC = ifelse(CCP1 == "PT,PCC ", 0, PCC),
    CCM1 = ifelse(CCP1 == "PT,PCC ", 0, CCM1))

# Step 16: Handle UDC-PCC-PAS coalition
data_2002 <- data_2002 %>%
  dplyr::mutate(
    pudc_pcc_pas = ifelse(CCP2 == "UDC,PCC,PAS ", rowSums(PUDC,PCC,PAS,CCM2,na.rm = TRUE), 0),
    PUDC = ifelse(CCP2 == "UDC,PCC,PAS ", 0, PUDC),
    PCC = ifelse(CCP2 == "UDC,PCC,PAS ", 0, PCC),
    PAS = ifelse(CCP2 == "UDC,PCC,PAS ", 0, PAS),
    CCM2 = ifelse(CCP2 == "UDC,PCC,PAS ", 0, CCM2))

# Step 17: Drop coalition and ccm variables
data_2002 <- data_2002 %>%
  dplyr::select(-c(CCM1, CCM2, CCP1, CCP2))

# Step 18: Rename columns for parties
data_2002 <- data_2002 %>%
  dplyr::rename(
    PRD_PT = prd_pt,
    PRD_PT_PCD = prd_pt_pcd,
    PRD_PT_PUDC = prd_pt_pudc,
    PRD_PT_PUDC_PCC = prd_pt_pudc_pcc,
    PRD_PUDC = prd_pudc,
    PRD_PUDC_PCC_PL = prd_pudc_pcc_pl,
    PT_PCC = pt_pcc,
    PUDC_PCC_PAS = pudc_pcc_pas)

# Step 19: Calculate turnout
data_2002 <- data_2002 %>%
  dplyr::mutate(turnout = total / listanominal)

# Step 20: Generate unique IDs by municipality
data_2002 <- data_2002 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "Abasolo" ~ 5001,
      str_detect(municipality, "Acu") ~ 5002,
      municipality == "Allende" ~ 5003,
      municipality == "Arteaga" ~ 5004,
      municipality == "Candela" ~ 5005,
      str_detect(municipality, "Casta") ~ 5006,
      str_detect(municipality, "Cuatroci") ~ 5007,
      municipality == "Escobedo" ~ 5008,
      municipality == "Francisco I. Madero" ~ 5009,
      municipality == "Frontera" ~ 5010,
      municipality == "General Cepeda" ~ 5011,
      municipality == "Guerrero" ~ 5012,
      municipality == "Hidalgo" ~ 5013,
      str_detect(municipality, "Jim") ~ 5014,
      str_detect(municipality, "rez") ~ 5015,
      municipality == "Lamadrid" ~ 5016,
      municipality == "Matamoros" ~ 5017,
      municipality == "Monclova" ~ 5018,
      municipality == "Morelos" ~ 5019,
      str_detect(municipality, "zquiz") ~ 5020,
      municipality == "Nadadores" ~ 5021,
      municipality == "Nava" ~ 5022,
      municipality == "Ocampo" ~ 5023,
      municipality == "Parras" ~ 5024,
      municipality == "Piedras Negras" ~ 5025,
      municipality == "Progreso" ~ 5026,
      municipality == "Ramos Arizpe" ~ 5027,
      municipality == "Sabinas" ~ 5028,
      municipality == "Sacramento" ~ 5029,
      municipality == "Saltillo" ~ 5030,
      municipality == "San Buenaventura" ~ 5031,
      municipality == "San Juan de Sabinas" ~ 5032,
      municipality == "San Pedro" ~ 5033,
      municipality == "Sierra Mojada" ~ 5034,
      str_detect(municipality, "Torre") ~ 5035,
      municipality == "Viesca" ~ 5036,
      str_detect(municipality, "Villa Uni") ~ 5037,
      municipality == "Zaragoza" ~ 5038
    )
  )

# Step 21: Calculate valid votes
data_2002 <- data_2002 %>%
  dplyr::mutate(valid = sum(across(c(PAN:PSN,PRD_PT,PRD_PT_PCD,
                                     PRD_PT_PUDC_PCC,PRD_PUDC,
                                     PRD_PUDC_PCC_PL,PT_PCC,
                                     PUDC_PCC_PAS)), na.rm = TRUE))

# Step 25: Add year and month
data_2002 <- data_2002 %>%
  dplyr::mutate(
    year = 2002,
    month = "September"
  )

##########################
# Year 2005
##########################

# Step 1: Import 'LISTA NOMINAL COAHUILA 2005' Excel file
lista_nominal_2005 <- read_excel("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/LISTA NOMINAL COAHUILA 2005.xls", sheet = "estad_lp_05_01")

# Step 2: Rename 'SECCION' to 'section'
lista_nominal_2005 <- lista_nominal_2005 %>%
  dplyr::rename(section = SECCION)

# Step 3: Drop rows where 'section' is missing
lista_nominal_2005 <- lista_nominal_2005 %>%
  dplyr::filter(!is.na(section))

# Step 4: Rename 'LN' to 'listanominal'
lista_nominal_2005 <- lista_nominal_2005 %>%
  dplyr::rename(listanominal = "L.N.")

# Step 5: Collapse data by 'section' and sum 'listanominal'
lista_nominal_2005 <- lista_nominal_2005 %>%
  dplyr::group_by(section) %>%
  dplyr::summarise(listanominal = sum(listanominal))

# Step 7: Import 'Ayu_Seccion_2005_No_LN.csv' file
seccion_2005 <- read.csv("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayu_Seccion_2005_No_LN.csv")
names(seccion_2005)
# Step 8: Rename columns
seccion_2005 <- seccion_2005 %>%
  dplyr::rename(municipality = MUNICIPIO, 
                section = SECCION,
                total = TOTAL,
                nulos = NULOS)

# Step 9: Drop rows where 'total' is missing or zero
seccion_2005 <- seccion_2005 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Step 10: Convert relevant columns to numeric
seccion_2005 <- seccion_2005 %>%
  dplyr::mutate(across(PAN:total, as.numeric))

# Step 11: Collapse (sum) data by 'municipality' and 'section' for all columns
data_2005 <- seccion_2005 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(PAN:total, sum, na.rm = TRUE))

# *****************************
# ****** Coalition Adjustments *
# *****************************

# Step 12: Create 'pan_pudc' for selected municipalities
data_2005 <- data_2005 %>%
  dplyr::mutate(
    pan_pudc = ifelse(municipality %in% c("Acuña", "Castaños", "Cuatrociénegas", "Escobedo", "Frontera", 
                                          "General Cepeda", "Jiménez", "Morelos", "Múzquiz", "Nadadores", 
                                          "Parras", "Piedras Negras", "Progreso", "Ramos Arizpe", "Sabinas", 
                                          "Sacramento", "Saltillo", "San Juan de Sabinas", "Sierra Mojada", 
                                          "Viesca", "Villa Unión", "Zaragoza"), PAN + PUDC + CCM1, 0),
    CCM1 = ifelse(municipality %in% c("Acuña", "Castaños", "Cuatrociénegas", "Escobedo", "Frontera", 
                                      "General Cepeda", "Jiménez", "Morelos", "Múzquiz", "Nadadores", 
                                      "Parras", "Piedras Negras", "Progreso", "Ramos Arizpe", "Sabinas", 
                                      "Sacramento", "Saltillo", "San Juan de Sabinas", "Sierra Mojada", 
                                      "Viesca", "Villa Unión", "Zaragoza"), 0, CCM1),
    PAN = ifelse(municipality %in% c("Acuña", "Castaños", "Cuatrociénegas", "Escobedo", "Frontera", 
                                     "General Cepeda", "Jiménez", "Morelos", "Múzquiz", "Nadadores", 
                                     "Parras", "Piedras Negras", "Progreso", "Ramos Arizpe", "Sabinas", 
                                     "Sacramento", "Saltillo", "San Juan de Sabinas", "Sierra Mojada", 
                                     "Viesca", "Villa Unión", "Zaragoza"), 0, PAN),
    PUDC = ifelse(municipality %in% c("Acuña", "Castaños", "Cuatrociénegas", "Escobedo", "Frontera", 
                                      "General Cepeda", "Jiménez", "Morelos", "Múzquiz", "Nadadores", 
                                      "Parras", "Piedras Negras", "Progreso", "Ramos Arizpe", "Sabinas", 
                                      "Sacramento", "Saltillo", "San Juan de Sabinas", "Sierra Mojada", 
                                      "Viesca", "Villa Unión", "Zaragoza"), 0, PUDC))

# Step 13: Create 'prd_pt' for selected municipalities
data_2005 <- data_2005 %>%
  dplyr::mutate(
    prd_pt = case_when(
      municipality == "Matamoros" ~ PRD + PT + CCM1,
      municipality %in% c("Jiménez", "Múzquiz") ~ PRD + PT + CCM2,
      TRUE ~ 0),
    PRD = ifelse(municipality %in% c("Jiménez", "Matamoros", "Múzquiz"), 0, PRD),
    PT = ifelse(municipality %in% c("Jiménez", "Matamoros", "Múzquiz"), 0, PT),
    CCM1 = ifelse(municipality == "Matamoros", 0, CCM1),
    CCM2 = ifelse(municipality %in% c("Jiménez", "Múzquiz"), 0, CCM2))

# Step 14: Drop columns 'ccm1' and 'ccm2'
data_2005 <- data_2005 %>%
  dplyr::select(-CCM1, -CCM2)

# Step 15: Rename columns
data_2005 <- data_2005 %>%
  dplyr::rename(
    PAN_PUDC = pan_pudc,
    PRD_PT = prd_pt
  )

# *****************************
# ****** Merge with ListaNominal2005 *********
# *****************************

# Step 16: Load and merge 'ListaNominal2005.rds' by 'section'
lista_nominal_2005 <- read.dta13("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/ListaNominal2005.dta")
data_2005 <- left_join(data_2005, lista_nominal_2005, 
                          by = "section")

# Step 17: Drop rows where '_merge' is 2 (those without matches in the merge)
data_2005 <- data_2005 %>%
  dplyr::filter(!is.na(listanominal))

# Step 18: Calculate turnout
data_2005 <- data_2005 %>%
  dplyr::mutate(turnout = total / listanominal)

# *****************************
# ****** Generate Unique IDs *********
# *****************************

# Step 19: Assign unique IDs based on municipalities
data_2005 <- data_2005 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "Abasolo" ~ 5001,
      str_detect(municipality, "Acu") ~ 5002,
      municipality == "Allende" ~ 5003,
      municipality == "Arteaga" ~ 5004,
      municipality == "Candela" ~ 5005,
      str_detect(municipality, "Casta") ~ 5006,
      str_detect(municipality, "Cuatroci") ~ 5007,
      municipality == "Escobedo" ~ 5008,
      municipality == "Francisco I. Madero" ~ 5009,
      municipality == "Frontera" ~ 5010,
      municipality == "General Cepeda" ~ 5011,
      municipality == "Guerrero" ~ 5012,
      municipality == "Hidalgo" ~ 5013,
      str_detect(municipality, "Jim") ~ 5014,
      str_detect(municipality, "rez") ~ 5015,
      municipality == "Lamadrid" ~ 5016,
      municipality == "Matamoros" ~ 5017,
      municipality == "Monclova" ~ 5018,
      municipality == "Morelos" ~ 5019,
      str_detect(municipality, "zquiz") ~ 5020,
      municipality == "Nadadores" ~ 5021,
      municipality == "Nava" ~ 5022,
      municipality == "Ocampo" ~ 5023,
      municipality == "Parras" ~ 5024,
      municipality == "Piedras Negras" ~ 5025,
      municipality == "Progreso" ~ 5026,
      municipality == "Ramos Arizpe" ~ 5027,
      municipality == "Sabinas" ~ 5028,
      municipality == "Sacramento" ~ 5029,
      municipality == "Saltillo" ~ 5030,
      municipality == "San Buenaventura" ~ 5031,
      municipality == "San Juan de Sabinas" ~ 5032,
      municipality == "San Pedro" ~ 5033,
      municipality == "Sierra Mojada" ~ 5034,
      str_detect(municipality, "Torre") ~ 5035,
      municipality == "Viesca" ~ 5036,
      str_detect(municipality, "Villa Uni") ~ 5037,
      municipality == "Zaragoza" ~ 5038
    )
  )

# Step 20: Calculate valid votes
data_2005 <- data_2005 %>%
  dplyr::mutate(valid = sum(c_across(PAN:PRD_PT), na.rm = TRUE))



# Step 25: Add year and month
data_2005 <- data_2005 %>%
  dplyr::mutate(
    year = 2005,
    month = "September"
  )
summary(data_2005)
# *****************************
# ****** PART 2: 2006 *********
# *****************************

# Step 28: Load "all_months_years.dta" file
# Step 16: Load and merge 'ListaNominal2005.rds' by 'section'
lista <- read.dta13("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/ListaNominal2005.dta")

# Step 32: Load 'Abasolo extraord 2006' Excel file
abasolo_2006 <- read_excel("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Abasolo extraord 2006.xlsx", sheet = "Hoja1", range = "A7:K10")

# Step 33: Rename columns and drop missing 'section' rows
abasolo_2006 <- abasolo_2006 %>%
  dplyr::rename(section = SECC) %>%
  dplyr::filter(section != "")

# Step 34: Convert all columns to numeric where necessary
abasolo_2006 <- abasolo_2006 %>%
  dplyr::mutate(across(everything(), as.numeric))


abasolo_2006 <- left_join(abasolo_2006, lista, 
                          by = "section")


# Step 35: Add municipality and calculate 'PRI_PANAL'
abasolo_2006 <- abasolo_2006 %>%
  dplyr::mutate(
    municipality = "ABASOLO EXTRAORDINARIO",
    PRI_PANAL = PRI + PNA + `CC PRI-PNA`) %>%
  dplyr::select(-PRI, -PNA, -`CC PRI-PNA`)

# Step 36: Collapse by 'municipality' and 'section'
abasolo_2006 <- abasolo_2006 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN:listanominal,PRI_PANAL), sum))

# Step 37: Assign 'uniqueid' and merge with 'lista_2006'
abasolo_2006 <- abasolo_2006 %>%
  dplyr::mutate(uniqueid = 5001)

# Step 38: Drop rows without a match from the merge
abasolo_2006 <- abasolo_2006 %>%
  dplyr::filter(!is.na(listanominal))
names(abasolo_2006)
# Step 39: Calculate turnout
abasolo_2006 <- abasolo_2006 %>%
  dplyr::rename(nulos=NULOS,
                total=TOTAL) %>% 
  dplyr::mutate(turnout = total / listanominal)

# Step 40: Calculate valid votes
abasolo_2006 <- abasolo_2006 %>%
  dplyr::mutate(valid = sum(PAN, PRD, PASDC, PRI_PANAL, na.rm = TRUE)) %>% 
  dplyr::select(-VALIDOS)

# Step 44: Add year and month information
abasolo_2006 <- abasolo_2006 %>%
  dplyr::mutate(
    year = 2006,
    month = "September")

data_2005 <- rbind(data_2005,
                   abasolo_2006)


# *****************************
# ****** data 2009
# *****************************


# Step 1: Import 'Ayu_Seccion_2009.csv'
seccion_2009 <- read_delim("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayu_Seccion_2009.csv", delim = ",")
names(seccion_2009)
# Step 2: Rename columns
seccion_2009 <- seccion_2009 %>%
  dplyr::rename(municipality = MUNICIPIO, 
                section = SECC,
                listanominal = LISTANOMINAL,
                nulos = NULOS,
                total = TOTAL,
                valid = VALIDOS)

# Step 3: Drop rows where 'total' is missing or zero
seccion_2009 <- seccion_2009 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Step 4: Convert relevant columns to numeric
seccion_2009 <- seccion_2009 %>%
  dplyr::mutate(across(listanominal:total, as.numeric))

# Step 5: Collapse (sum) data by 'municipality' and 'section'
data_2009 <- seccion_2009 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(listanominal:total, sum, na.rm = TRUE))

# ***************************************************************************************
# ****** Coalition Adjustments **********************************************************
# ***************************************************************************************

# Step 6: Create 'pri_pvem' for selected municipalities
data_2009 <- data_2009 %>%
  dplyr::mutate(
    pri_pvem = case_when(
      municipality %in% c("Arteaga", "Juárez", "Matamoros", "San Pedro") ~ PRI + PVEM + CC1,
      municipality %in% c("La Madrid", "Ramos Arizpe") ~ PRI + PVEM + CC2,
      TRUE ~ 0),
    PRI = ifelse(municipality %in% c("Arteaga", "Juárez", "Matamoros", "La Madrid", "San Pedro", "Ramos Arizpe"), 0, PRI),
    PVEM = ifelse(municipality %in% c("Arteaga", "Juárez", "Matamoros", "La Madrid", "San Pedro", "Ramos Arizpe"), 0, PVEM),
    CC1 = ifelse(municipality %in% c("Arteaga", "Juárez", "Matamoros", "San Pedro"), 0, CC1),
    CC2 = ifelse(municipality %in% c("La Madrid", "Ramos Arizpe"), 0, CC2))

# Step 7: Create 'pri_pvem_pna' for selected municipalities
data_2009 <- data_2009 %>%
  dplyr::mutate(
    pri_pvem_pna = ifelse(municipality == "Abasolo", PRI + PVEM + PNA + CC1, 0),
    PRI = ifelse(municipality == "Abasolo", 0, PRI),
    PVEM = ifelse(municipality == "Abasolo", 0, PVEM),
    PNA = ifelse(municipality == "Abasolo", 0, PNA),
    CC1 = ifelse(municipality == "Abasolo", 0, CC1))

# Step 8: Create 'pri_pvem_psd' for selected municipalities
data_2009 <- data_2009 %>%
  dplyr::mutate(
    pri_pvem_psd = case_when(
      municipality %in% c("Nadadores", "Ocampo", "San Buenaventura") ~ PRI + PVEM + PSD + CC1,
      municipality %in% c("Sacramento", "Viesca") ~ PRI + PVEM + PSD + CC2,
      TRUE ~ 0),
    PRI = ifelse(municipality %in% c("Nadadores", "Ocampo", "San Buenaventura", "Sacramento", "Viesca"), 0, PRI),
    PVEM = ifelse(municipality %in% c("Nadadores", "Ocampo", "San Buenaventura", "Sacramento", "Viesca"), 0, PVEM),
    PSD = ifelse(municipality %in% c("Nadadores", "Ocampo", "San Buenaventura", "Sacramento", "Viesca"), 0, PSD),
    CC1 = ifelse(municipality %in% c("Nadadores", "Ocampo", "San Buenaventura"), 0, CC1),
    CC2 = ifelse(municipality %in% c("Sacramento", "Viesca"), 0, CC2))

# Step 9: Create 'pri_pvem_pna_psd' for selected municipalities
data_2009 <- data_2009 %>%
  dplyr::mutate(
    pri_pvem_pna_psd = case_when(
      municipality %in% c("Allende", "Candela", "Cuatrociénegas", "Escobedo", "Guerrero", "Hidalgo", 
                          "Morelos", "Múzquiz", "Sierra Mojada", "Villa Unión", "Zaragoza") ~ PRI + PVEM + PNA + PSD + CC1,
      municipality %in% c("Castaños", "Sabinas", "Progreso") ~ PRI + PVEM + PNA + PSD + CC2,TRUE ~ 0),
    PRI = ifelse(municipality %in% c("Allende", "Candela", "Castaños", "Cuatrociénegas", "Escobedo", "Guerrero", 
                                     "Hidalgo", "Morelos", "Múzquiz", "Sierra Mojada", "Villa Unión", "Zaragoza", "Sabinas", "Progreso"), 0, PRI),
    PVEM = ifelse(municipality %in% c("Allende", "Candela", "Castaños", "Cuatrociénegas", "Escobedo", "Guerrero", 
                                      "Hidalgo", "Morelos", "Múzquiz", "Sierra Mojada", "Villa Unión", "Zaragoza", "Sabinas", "Progreso"), 0, PVEM),
    PNA = ifelse(municipality %in% c("Allende", "Candela", "Castaños", "Cuatrociénegas", "Escobedo", "Guerrero", 
                                     "Hidalgo", "Morelos", "Múzquiz", "Sierra Mojada", "Villa Unión", "Zaragoza", "Sabinas", "Progreso"), 0, PNA),
    PSD = ifelse(municipality %in% c("Allende", "Candela", "Castaños", "Cuatrociénegas", "Escobedo", "Guerrero", 
                                     "Hidalgo", "Morelos", "Múzquiz", "Sierra Mojada", "Villa Unión", "Zaragoza", "Sabinas", "Progreso"), 0, PSD),
    CC1 = ifelse(municipality %in% c("Allende", "Candela", "Cuatrociénegas", "Escobedo", "Guerrero", "Hidalgo", 
                                     "Morelos", "Múzquiz", "Sierra Mojada", "Villa Unión", "Zaragoza"), 0, CC1),
    CC2 = ifelse(municipality %in% c("Castaños", "Sabinas", "Progreso"), 0, CC2))

# Repeat similar steps for other coalition combinations...
names(data_2009)
# Drop any unused columns such as 'cc1', 'cc2', 'cc3', 'cc4'
data_2009 <- data_2009 %>%
  select(-CC1, -CC2, -CC3, -CC4)

# Step 10: Calculate 'turnout' as total votes divided by 'listanominal'
data_2009 <- data_2009 %>%
  dplyr::mutate(turnout = total / listanominal)

# ***************************************************************************************
# ****** Generate Unique IDs ************************************************************
# ***************************************************************************************

# Step 11: Assign unique IDs based on municipalities
data_2009 <- data_2009 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "Abasolo" ~ 5001,
      str_detect(municipality, "Acu") ~ 5002,
      municipality == "Allende" ~ 5003,
      municipality == "Arteaga" ~ 5004,
      municipality == "Candela" ~ 5005,
      str_detect(municipality, "Casta") ~ 5006,
      str_detect(municipality, "Cuatroci") ~ 5007,
      municipality == "Escobedo" ~ 5008,
      municipality == "Francisco I. Madero" ~ 5009,
      municipality == "Frontera" ~ 5010,
      municipality == "General Cepeda" ~ 5011,
      municipality == "Guerrero" ~ 5012,
      municipality == "Hidalgo" ~ 5013,
      str_detect(municipality, "Jim") ~ 5014,
      str_detect(municipality, "rez") ~ 5015,
      municipality == "La Madrid" ~ 5016,
      municipality == "Matamoros" ~ 5017,
      municipality == "Monclova" ~ 5018,
      municipality == "Morelos" ~ 5019,
      str_detect(municipality, "zquiz") ~ 5020,
      municipality == "Nadadores" ~ 5021,
      municipality == "Nava" ~ 5022,
      municipality == "Ocampo" ~ 5023,
      municipality == "Parras" ~ 5024,
      municipality == "Piedras Negras" ~ 5025,
      municipality == "Progreso" ~ 5026,
      municipality == "Ramos Arizpe" ~ 5027,
      municipality == "Sabinas" ~ 5028,
      municipality == "Sacramento" ~ 5029,
      municipality == "Saltillo" ~ 5030,
      municipality == "San Buenaventura" ~ 5031,
      municipality == "San Juan de Sabinas" ~ 5032,
      municipality == "San Pedro" ~ 5033,
      municipality == "Sierra Mojada" ~ 5034,
      str_detect(municipality, "Torre") ~ 5035,
      municipality == "Viesca" ~ 5036,
      str_detect(municipality, "Villa Uni") ~ 5037,
      municipality == "Zaragoza" ~ 5038
    )
  )

# Step 17: Add year and month
data_2009 <- data_2009 %>%
  dplyr::mutate(
    year = 2009,
    month = "October")

summary(data_2009)

# Read the specific range and sheet from the Excel file
data_2010 <- read_excel("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayuntamientos Juarez-Lamadrid 2010.xls", 
                   sheet = "Hoja1", range = "A7:N22")

# Step 2: Drop rows with index 6 to 12
data_2010 <- data_2010 %>% slice(-6:-12)

# Step 3: Rename columns
data_2010 <- data_2010 %>%
              dplyr::rename("municipality" = "MUNICIPIO",
                            "total" = "TOTAL",
                            "listanominal" = "L.N.")

# Step 4: Create a new column "section" from "CASILLA"
data_2010 <- data_2010 %>%
  dplyr::mutate(section = substr(CASILLA, 2, 4))


# Step 5: Convert all columns to numeric where possible
data_2010 <- data_2010 %>%
  dplyr::mutate(across(c(listanominal:section), as.numeric))

# Step 6: Drop rows where `total` is NA or 0
data_2010 <- data_2010 %>% 
  dplyr::filter(!(is.na(total) | total == 0))

# Step 7: Create `PRI_PVEM`
data_2010 <- data_2010 %>%
  dplyr::mutate(PRI_PVEM = ifelse(municipality == "JUÁREZ EXTRAORDINARIO", PRI + PVEM + CCM1, NA),
                PRI_PVEM = ifelse(municipality == "LAMADRID EXTRAORDINARIO", PRI + PVEM + CCM2, PRI_PVEM),
                CCM2 = ifelse(municipality=="JUÁREZ EXTRAORDINARIO", NA, CCM2))

# Step 8: Drop columns `PRI` and `PVEM`
data_2010 <- data_2010 %>% 
  dplyr::select(-PRI, -PVEM)

# Step 9: Create `PAN_PUDC` and drop `PAN` and `PUDC`
data_2010 <- data_2010 %>%
  dplyr::mutate(PAN_PUDC = ifelse(municipality == "LAMADRID EXTRAORDINARIO", PAN + PUDC + CCM1, NA)) %>%
  dplyr::select(-PAN, -PUDC, -CCM1, -CCM2)

# Step 10: Reorder columns to place `PRI_PVEM` and `PAN_PUDC` before `PRD`
data_2010 <- data_2010 %>%
  relocate(PRI_PVEM, PAN_PUDC, .before = PRD)

# Step 11: Collapse data by `municipality` and `section`, summing numeric variables
data_2010 <- data_2010 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarize(across(listanominal:total, sum, na.rm = TRUE))

# Step 12: Generate `turnout`
data_2010 <- data_2010 %>%
  dplyr::mutate(turnout = total / listanominal)

# Step 13: Create `uniqueid`
data_2010 <- data_2010 %>%
  dplyr::mutate(uniqueid = case_when(
    municipality == "JUÁREZ EXTRAORDINARIO" ~ 5015,
    municipality == "LAMADRID EXTRAORDINARIO" ~ 5016,
    TRUE ~ 0
  ))

# Step 14: Calculate `valid` as the row total of specific columns
data_2010 <- data_2010 %>%
  dplyr::mutate(valid = rowSums(across(c(PRI_PVEM, PAN_PUDC, PRD)), na.rm = TRUE))

# Step 18: Add year and month
data_2010 <- data_2010 %>%
  dplyr::mutate(year = 2010, month = "July")

# ***************************************************************************************
# ****** Data 2013
# ***************************************************************************************
# Import the Excel data

# Step 1: Import the Excel file
coalitions <- read_excel("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Coalitions_2013.xlsx", sheet = "Sheet1", col_names = TRUE)

# Step 1: Initialize empty coalition columns
coalitions <- coalitions %>%
  mutate(coal_CC1 = "",
         coal_CC2 = "",
         coal_CC3 = "")

# Step 2: Populate `coal_CC1` while handling `NA` values
coalitions <- coalitions %>%
  mutate(
    coal_CC1 = ifelse(!is.na(cc_PAN) & cc_PAN == 1, str_c(coal_CC1, "-PAN"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PRI) & cc_PRI == 1, str_c(coal_CC1, "-PRI"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PRD) & cc_PRD == 1, str_c(coal_CC1, "-PRD"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PT) & cc_PT == 1, str_c(coal_CC1, "-PT"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PVEM) & cc_PVEM == 1, str_c(coal_CC1, "-PVEM"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_UDC) & cc_UDC == 1, str_c(coal_CC1, "-UDC"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PMC) & cc_PMC == 1, str_c(coal_CC1, "-PMC"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PNA) & cc_PNA == 1, str_c(coal_CC1, "-PNA"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PSDC) & cc_PSDC == 1, str_c(coal_CC1, "-PSDC"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PPC) & cc_PPC == 1, str_c(coal_CC1, "-PPC"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PJ) & cc_PJ == 1, str_c(coal_CC1, "-PJ"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PRC) & cc_PRC == 1, str_c(coal_CC1, "-PRC"), coal_CC1),
    coal_CC1 = ifelse(!is.na(cc_PPRO) & cc_PPRO == 1, str_c(coal_CC1, "-PPRO"), coal_CC1)
  )

# Remove leading dash from `coal_CC1`
coalitions <- coalitions %>%
  mutate(coal_CC1 = str_sub(coal_CC1, 2))

# Step 3: Populate `coal_CC2` while handling `NA` values
coalitions <- coalitions %>%
  mutate(
    coal_CC2 = ifelse(!is.na(cc_PAN) & cc_PAN == 2, str_c(coal_CC2, "-PAN"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PRI) & cc_PRI == 2, str_c(coal_CC2, "-PRI"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PRD) & cc_PRD == 2, str_c(coal_CC2, "-PRD"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PT) & cc_PT == 2, str_c(coal_CC2, "-PT"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PVEM) & cc_PVEM == 2, str_c(coal_CC2, "-PVEM"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_UDC) & cc_UDC == 2, str_c(coal_CC2, "-UDC"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PMC) & cc_PMC == 2, str_c(coal_CC2, "-PMC"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PNA) & cc_PNA == 2, str_c(coal_CC2, "-PNA"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PSDC) & cc_PSDC == 2, str_c(coal_CC2, "-PSDC"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PPC) & cc_PPC == 2, str_c(coal_CC2, "-PPC"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PJ) & cc_PJ == 2, str_c(coal_CC2, "-PJ"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PRC) & cc_PRC == 2, str_c(coal_CC2, "-PRC"), coal_CC2),
    coal_CC2 = ifelse(!is.na(cc_PPRO) & cc_PPRO == 2, str_c(coal_CC2, "-PPRO"), coal_CC2)
  )

# Remove leading dash from `coal_CC2`
coalitions <- coalitions %>%
  mutate(coal_CC2 = str_sub(coal_CC2, 2))

# Step 3: Populate `coal_CC3` explicitly for each party, handling NA values
coalitions <- coalitions %>%
  mutate(
    coal_CC3 = ifelse(!is.na(cc_PAN) & cc_PAN == 3, str_c(coal_CC3, "-PAN"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PRI) & cc_PRI == 3, str_c(coal_CC3, "-PRI"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PRD) & cc_PRD == 3, str_c(coal_CC3, "-PRD"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PT) & cc_PT == 3, str_c(coal_CC3, "-PT"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PVEM) & cc_PVEM == 3, str_c(coal_CC3, "-PVEM"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_UDC) & cc_UDC == 3, str_c(coal_CC3, "-UDC"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PMC) & cc_PMC == 3, str_c(coal_CC3, "-PMC"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PNA) & cc_PNA == 3, str_c(coal_CC3, "-PNA"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PSDC) & cc_PSDC == 3, str_c(coal_CC3, "-PSDC"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PPC) & cc_PPC == 3, str_c(coal_CC3, "-PPC"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PJ) & cc_PJ == 3, str_c(coal_CC3, "-PJ"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PRC) & cc_PRC == 3, str_c(coal_CC3, "-PRC"), coal_CC3),
    coal_CC3 = ifelse(!is.na(cc_PPRO) & cc_PPRO == 3, str_c(coal_CC3, "-PPRO"), coal_CC3)
  )

# Step 3: Remove leading dash from `coal_CC3`
coalitions <- coalitions %>%
  mutate(coal_CC3 = str_sub(coal_CC3, 2))

# Step 3: Drop unnecessary columns
coalitions <- coalitions %>%
  select(-cc_CC1, -cc_CC2, -cc_CC3, -starts_with("cc_"))

# Step 1: Import the Excel file
ayuntamientos <- read_excel("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayuntamientos2013 x SECCION.xlsx", 
                            sheet = "Ayuntamientos2013 x seccion", 
                            col_names = TRUE)

# Step 2: Drop rows where TOTAL is NA or 0
ayuntamientos <- ayuntamientos %>%
  dplyr::filter(!(is.na(TOTAL) | TOTAL == 0))

# Step 3: Collapse data by MUNICIPIO and SECC (sum values from PAN to LN)
data_2013 <- ayuntamientos %>%
  group_by(MUNICIPIO, SECC) %>%
  summarize(across(PAN:"L.N.", sum, na.rm = TRUE), .groups = "drop")

data_2013 <- data_2013 %>%
  left_join(coalitions, by = c("MUNICIPIO" = "MUNICIPIO"))

# Step 6: Rename columns
data_2013 <- data_2013 %>%
  rename(
    municipality = MUNICIPIO,
    section = SECC,
    valid = VALIDOS,
    total = TOTAL,
    listanominal = "L.N."
  )

# Step 7: Generate `total_CC1` and adjust parties based on `coal_CC1`
data_2013 <- data_2013 %>%
  mutate(total_CC1 = CC1)

# Manually adjust `total_CC1` and set party values to 0 if in `coal_CC1`
parties <- c("PAN", "PRI", "PRD", "PT", "PVEM", "UDC", "PMC", "PNA", "PSDC", "PPC", "PJ", "PRC", "PPRO")

for (party in parties) {
  data_2013 <- data_2013 %>%
    mutate(
      total_CC1 = ifelse(str_detect(coal_CC1, party), total_CC1 + get(party), total_CC1),
      !!party := ifelse(str_detect(coal_CC1, party), 0, get(party))
    )
}

# Step 8: Generate `total_CC2` and adjust parties based on `coal_CC2`
data_2013 <- data_2013 %>%
  mutate(total_CC2 = CC2)

for (party in parties) {
  data_2013 <- data_2013 %>%
    mutate(
      total_CC2 = ifelse(str_detect(coal_CC2, party), total_CC2 + get(party), total_CC2),
      !!party := ifelse(str_detect(coal_CC2, party), 0, get(party))
    )
}

# Step 9: Generate `total_CC3` and adjust parties based on `coal_CC3`
data_2013 <- data_2013 %>%
  mutate(total_CC3 = CC3)

for (party in parties) {
  data_2013 <- data_2013 %>%
    mutate(
      total_CC3 = ifelse(str_detect(coal_CC3, party), total_CC3 + get(party), total_CC3),
      !!party := ifelse(str_detect(coal_CC3, party), 0, get(party))
    )
}

# Step 10: Drop `CC1`, `CC2`, `CC3` columns
data_2013 <- data_2013 %>%
  select(-CC1, -CC2, -CC3)

# Step 11: Replace dashes with underscores in `coal_CC1`, `coal_CC2`, `coal_CC3`
data_2013 <- data_2013 %>%
  mutate(
    coal_CC1 = str_replace_all(coal_CC1, "-", "_"),
    coal_CC2 = str_replace_all(coal_CC2, "-", "_"),
    coal_CC3 = str_replace_all(coal_CC3, "-", "_")
  )

# Step 12: Generate coalition-specific columns
coalitions_list <- c(
  "PAN_PPRO", "PAN_PRD_PT_UDC_PPRO", "PAN_PRD_UDC", "PAN_PRD_UDC_PPRO",
  "PAN_PT", "PAN_PT_UDC", "PAN_PT_UDC_PPRO", "PAN_UDC", "PAN_UDC_PPRO",
  "PRI_PNA_PSDC_PJ_PRC", "PRI_PNA", "PRI_PNA_PRC", "PRI_PNA_PSDC_PJ",
  "PRI_PNA_PSDC_PRC", "PRI_PVEM_PNA", "PRI_PVEM_PNA_PJ", "PRI_PVEM_PNA_PJ_PRC",
  "PRI_PVEM_PNA_PRC", "PRI_PVEM_PNA_PSDC", "PRI_PVEM_PNA_PSDC_PJ",
  "PRI_PVEM_PNA_PSDC_PJ_PRC", "PT_PVEM", "PMC_PSDC", "PPC_PJ",
  "PRD_PPC_PJ_PPRO", "PRD_PSDC_PPC", "PRD_PT_PSDC_PPC", "PSDC_PPC_PJ"
)

for (coalition in coalitions_list) {
  data_2013 <- data_2013 %>%
    mutate(!!coalition := ifelse(coal_CC1 == coalition, total_CC1,
                                 ifelse(coal_CC2 == coalition, total_CC2,
                                        ifelse(coal_CC3 == coalition, total_CC3, 0))))
}

# Step 13: Drop temporary columns
data_2013 <- data_2013 %>%
  select(-total_CC1, -total_CC2, -total_CC3, -coal_CC1, -coal_CC2, -coal_CC3)

# Step 1: Calculate turnout
data_2013 <- data_2013 %>%
  mutate(turnout = total / listanominal)

# Step 2: Drop the "NULOS" column (if it exists)
data_2013 <- data_2013 %>%
  select(-NULOS)

# Step 3: Capitalize municipality names properly
data_2013 <- data_2013 %>%
  mutate(municipality = str_to_title(municipality))  # Ensures proper capitalization

# Step 4: Generate `uniqueid` and assign values based on `municipality`
data_2013 <- data_2013 %>%
  mutate(uniqueid = NA,  # Initialize `uniqueid` column
         uniqueid = ifelse(municipality == "Abasolo", 5001, uniqueid),
         uniqueid = ifelse(str_detect(municipality, "Acu"), 5002, uniqueid),
         uniqueid = ifelse(municipality == "Allende", 5003, uniqueid),
         uniqueid = ifelse(municipality == "Arteaga", 5004, uniqueid),
         uniqueid = ifelse(municipality == "Candela", 5005, uniqueid),
         uniqueid = ifelse(str_detect(municipality, "Casta"), 5006, uniqueid),
         uniqueid = ifelse(str_detect(municipality, "Cuatroci"), 5007, uniqueid),
         uniqueid = ifelse(municipality == "Escobedo", 5008, uniqueid),
         uniqueid = ifelse(municipality == "Francisco I.madero", 5009, uniqueid),
         uniqueid = ifelse(municipality == "Frontera", 5010, uniqueid),
         uniqueid = ifelse(municipality == "Gral. Cepeda", 5011, uniqueid),
         uniqueid = ifelse(municipality == "Guerrero", 5012, uniqueid),
         uniqueid = ifelse(municipality == "Hidalgo", 5013, uniqueid),
         uniqueid = ifelse(str_detect(municipality, "Jim"), 5014, uniqueid),
         uniqueid = ifelse(str_detect(municipality, "rez"), 5015, uniqueid),
         uniqueid = ifelse(municipality == "Lamadrid", 5016, uniqueid),
         uniqueid = ifelse(municipality == "Matamoros", 5017, uniqueid),
         uniqueid = ifelse(municipality == "Monclova", 5018, uniqueid),
         uniqueid = ifelse(municipality == "Morelos", 5019, uniqueid),
         uniqueid = ifelse(str_detect(municipality, "zquiz"), 5020, uniqueid),
         uniqueid = ifelse(municipality == "Nadadores", 5021, uniqueid),
         uniqueid = ifelse(municipality == "Nava", 5022, uniqueid),
         uniqueid = ifelse(municipality == "Ocampo", 5023, uniqueid),
         uniqueid = ifelse(municipality == "Parras", 5024, uniqueid),
         uniqueid = ifelse(municipality == "Piedras Negras", 5025, uniqueid),
         uniqueid = ifelse(municipality == "Progreso", 5026, uniqueid),
         uniqueid = ifelse(municipality == "Ramos Arizpe", 5027, uniqueid),
         uniqueid = ifelse(municipality == "Sabinas", 5028, uniqueid),
         uniqueid = ifelse(municipality == "Sacramento", 5029, uniqueid),
         uniqueid = ifelse(municipality == "Saltillo", 5030, uniqueid),
         uniqueid = ifelse(municipality == "San Buenaventura", 5031, uniqueid),
         uniqueid = ifelse(municipality == "San Juan De Sabinas", 5032, uniqueid),
         uniqueid = ifelse(municipality == "San Pedro", 5033, uniqueid),
         uniqueid = ifelse(municipality == "Sierra Mojada", 5034, uniqueid),
         uniqueid = ifelse(str_detect(municipality, "Torre"), 5035, uniqueid),
         uniqueid = ifelse(municipality == "Viesca", 5036, uniqueid),
         uniqueid = ifelse(str_detect(municipality, "Villa Uni"), 5037, uniqueid),
         uniqueid = ifelse(municipality == "Zaragoza", 5038, uniqueid))
# Rename columns to replace `PNA` with `PANAL`

data_2013 <- data_2013 %>%
  rename(
    PANAL = PNA,
    PRI_PANAL_PSDC_PJ_PRC = PRI_PNA_PSDC_PJ_PRC,
    PRI_PANAL = PRI_PNA,
    PRI_PANAL_PRC = PRI_PNA_PRC,
    PRI_PANAL_PSDC_PJ = PRI_PNA_PSDC_PJ,
    PRI_PANAL_PSDC_PRC = PRI_PNA_PSDC_PRC,
    PRI_PVEM_PANAL = PRI_PVEM_PNA,
    PRI_PVEM_PANAL_PJ = PRI_PVEM_PNA_PJ,
    PRI_PVEM_PANAL_PJ_PRC = PRI_PVEM_PNA_PJ_PRC,
    PRI_PVEM_PANAL_PRC = PRI_PVEM_PNA_PRC,
    PRI_PVEM_PANAL_PSDC = PRI_PVEM_PNA_PSDC,
    PRI_PVEM_PANAL_PSDC_PJ = PRI_PVEM_PNA_PSDC_PJ,
    PRI_PVEM_PANAL_PSDC_PJ_PRC = PRI_PVEM_PNA_PSDC_PJ_PRC
  )

# Step 1: Drop all columns ending with `_r`
data_2013 <- data_2013 %>%
  select(-ends_with("_r"))

# Step 2: Add `year` and `month` columns
data_2013 <- data_2013 %>%
  mutate(
    year = 2013,
    month = "July"
  )
############################################
## 2017 Data Processing
############################################

# Step 1: Import the first Excel file
ayuntamientos_2017 <- read_excel("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayuntamientos_2017.xlsx")

# Keep specific columns and remove duplicates
merge_data <- ayuntamientos_2017 %>%
  select(municipio, municipality, uniqueid) %>%
  distinct()

# Save the intermediate data (to mimic saving and erasing "merge.dta" in Stata)
write.csv(merge_data, "merge.csv", row.names = FALSE)

# Step 2: Import the second Excel file
ayuntamiento_xcasilla <- read_excel("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/Ayuntamiento_Xcasilla.xlsx", sheet = "Ayuntamiento_xcasilla")

# Merge the datasets on "municipio"
merged_data <- ayuntamiento_xcasilla %>%
  left_join(merge_data, by = "municipio")

# Drop the intermediate file
file.remove("merge.csv")

colnames(merged_data) <- gsub("-", "", colnames(merged_data))

# Step 3: Create `PAN_UDC_PPC_PES` and drop unnecessary columns
merged_data <- merged_data %>%
  mutate(PAN_UDC_PPC_PES = pan + udc + ppc + es + PANUDCPPCES + PANUDCPPC + PANUDCES + PANPPCES + UDCPPCES +
           PANUDC + PANPPC + PANES + UDCPPC + UDCES + PPCES) %>%
  select(-c(pan, udc, ppc, es, PANUDCPPCES, PANUDCPPC, PANUDCES, PANPPCES, UDCPPCES, PANUDC, PANPPC, PANES, UDCPPC, UDCES, PPCES))

# Step 4: Create `pri_coalition` and compute `PRI_PVEM_PANAL_SI_PJ_PRC_PCP`
merged_data <- merged_data %>%
  mutate(
    pri_coalition = !(municipality %in% c(
      "ALLENDE", "FRANCISCO I. MADERO", "FRONTERA", "GENERAL CEPEDA", "JIMÉNEZ",
      "MONCLOVA", "NADADORES", "PARRAS", "SAN BUENAVENTURA", "VIESCA", "ZARAGOZA"
    ))
  )

# Step 1: Compute `summer` as the row total of specified columns
# List of column names included in the `rowtotal`
summer_cols <- c(
  "PRIPVEMPNASIPJPRCPCP", "PRIPVEMPNASIPJPRC", "PRIPVEMPNASIPJPCP", "PRIPVEMPNASIPRCPCP",
  "PRIPVEMPNAPJPRCPCP", "PRIPVEMSIPJPRCPCP", "PRIPNASIPJPRCPCP", "PVEMPNASIPJPRCPCP",
  "PRIPVEMPNASIPJ", "PRIPVEMPNASIPRC", "PRIPVEMPNASIPCP", "PRIPVEMPNAPJPRC",
  "PRIPVEMPNAPJPCP", "PRIPVEMPNAPRCPCP", "PRIPVEMSIPJPRC", "PRIPVEMSIPJPCP",
  "PRIPVEMSIPRCPCP", "PRIPVEMPJPRCPCP", "PRIPNASIPJPRC", "PRIPNASIPJPCP", 
  "PRIPNASIPRCPCP", "PRIPNAPJPRCPCP", "PRISIPJPRCPCP", "PVEMPNASIPJPRC",
  "PVEMPNASIPJPCP", "PVEMPNASIPRCPCP", "PVEMPNAPJPRCPCP", "PVEMSIPJPRCPCP",
  "PNASIPJPRCPCP", "PRIPVEMPNASI", "PRIPVEMPNAPJ", "PRIPVEMPNAPRC", 
  "PRIPVEMPNAPCP", "PRIPVEMSIPJ", "PRIPVEMSIPRC", "PRIPVEMSIPCP", "PRIPVEMPJPRC", 
  "PRIPVEMPJPCP", "PRIPVEMPRCPCP", "PRIPNASIPJ", "PRIPNASIPRC", "PRIPNASIPCP", 
  "PRIPNAPJPRC", "PRIPNAPJPCP", "PRIPNAPRCPCP", "PRISIPJPRC", "PRISIPJPCP",
  "PRISIPRCPCP", "PRIPJPRCPCP", "PVEMPNASIPJ", "PVEMPNASIPRC", "PVEMPNASIPCP",
  "PVEMPNAPJPRC", "PVEMPNAPJPCP", "PVEMPNAPRCPCP", "PVEMSIPJPRC", "PVEMSIPJPCP",
  "PVEMSIPRCPCP", "PVEMPJPRCPCP", "PNASIPJPRC", "PNASIPJPCP", "PNASIPRCPCP",
  "PNAPJPRCPCP", "SIPJPRCPCP", "PRIPVEMPNA", "PRIPVEMSI", "PRIPVEMPJ", 
  "PRIPVEMPRC", "PRIPVEMPCP", "PRIPNASI", "PRIPNAPJ", "PRIPNAPRC", 
  "PRIPNAPCP", "PRISIPJ", "PRISIPRC", "PRISIPCP", "PRIPJPRC", "PRIPJPCP", 
  "PRIPRCPCP", "PVEMPNASI", "PVEMPNAPJ", "PVEMPNAPRC", "PVEMPNAPCP", 
  "PVEMSIPJ", "PVEMSIPRC", "PVEMSIPCP", "PVEMPJPRC", "PVEMPJPCP", "PVEMPRCPCP", 
  "PNASIPJ", "PNASIPRC", "PNASIPCP", "PNAPJPRC", "PNAPJPCP", "PNAPRCPCP", 
  "SIPJPRC", "SIPJPCP", "SIPRCPCP", "PJPRCPCP", "PRIPVEM", "PRIPNA", "PRISI", 
  "PRIPJ", "PRIPRC", "PRIPCP", "PVEMPNA", "PVEMSI", "PVEMPJ", "PVEMPRC", 
  "PVEMPCP", "PNASI", "PNAPJ", "PNAPRC", "PNAPCP", "SIPJ", "SIPRC", "SIPCP", 
  "PJPRC", "PJPCP", "PRCPCP"
)

# Compute the row total
merged_data <- merged_data %>%
  rowwise() %>%
  mutate(summer = sum(c_across(all_of(summer_cols)), na.rm = TRUE)) %>%
  ungroup()

# Step 2: Compute `PRI_PVEM_PANAL_SI_PJ_PRC_PCP` conditionally
merged_data <- merged_data %>%
  mutate(PRI_PVEM_PANAL_SI_PJ_PRC_PCP = ifelse(pri_coalition == 1, 
                                               pri + pvem + pna + si + pj + prc + pcp + summer, NA),
         pri = ifelse(pri_coalition == 1, NA, pri),
         pvem = ifelse(pri_coalition == 1, NA, pvem),
         pna = ifelse(pri_coalition == 1, NA, pna),
         si = ifelse(pri_coalition == 1, NA, si),
         pj = ifelse(pri_coalition == 1, NA, pj),
         prc = ifelse(pri_coalition == 1, NA, prc),
         pcp = ifelse(pri_coalition == 1, NA, pcp))

columns_to_drop <- c(
  "summer", "pri_coalition", "PRIPVEMPNASIPJPRCPCP", "PRIPVEMPNASIPJPRC", "PRIPVEMPNASIPJPCP",
  "PRIPVEMPNASIPRCPCP", "PRIPVEMPNAPJPRCPCP", "PRIPVEMSIPJPRCPCP", "PRIPNASIPJPRCPCP", "PVEMPNASIPJPRCPCP",
  "PRIPVEMPNASIPJ", "PRIPVEMPNASIPRC", "PRIPVEMPNASIPCP", "PRIPVEMPNAPJPRC", "PRIPVEMPNAPJPCP", 
  "PRIPVEMPNAPRCPCP", "PRIPVEMSIPJPRC", "PRIPVEMSIPJPCP", "PRIPVEMSIPRCPCP", "PRIPVEMPJPRCPCP", 
  "PRIPNASIPJPRC", "PRIPNASIPJPCP", "PRIPNASIPRCPCP", "PRIPNAPJPRCPCP", "PRISIPJPRCPCP", "PVEMPNASIPJPRC",
  "PVEMPNASIPJPCP", "PVEMPNASIPRCPCP", "PVEMPNAPJPRCPCP", "PVEMSIPJPRCPCP", "PNASIPJPRCPCP", "PRIPVEMPNASI",
  "PRIPVEMPNAPJ", "PRIPVEMPNAPRC", "PRIPVEMPNAPCP", "PRIPVEMSIPJ", "PRIPVEMSIPRC", "PRIPVEMSIPCP", 
  "PRIPVEMPJPRC", "PRIPVEMPJPCP", "PRIPVEMPRCPCP", "PRIPNASIPJ", "PRIPNASIPRC", "PRIPNASIPCP", "PRIPNAPJPRC",
  "PRIPNAPJPCP", "PRIPNAPRCPCP", "PRISIPJPRC", "PRISIPJPCP", "PRISIPRCPCP", "PRIPJPRCPCP", "PVEMPNASIPJ", 
  "PVEMPNASIPRC", "PVEMPNASIPCP", "PVEMPNAPJPRC", "PVEMPNAPJPCP", "PVEMPNAPRCPCP", "PVEMSIPJPRC", 
  "PVEMSIPJPCP", "PVEMSIPRCPCP", "PVEMPJPRCPCP", "PNASIPJPRC", "PNASIPJPCP", "PNASIPRCPCP", "PNAPJPRCPCP", 
  "SIPJPRCPCP", "PRIPVEMPNA", "PRIPVEMSI", "PRIPVEMPJ", "PRIPVEMPRC", "PRIPVEMPCP", "PRIPNASI", "PRIPNAPJ",
  "PRIPNAPRC", "PRIPNAPCP", "PRISIPJ", "PRISIPRC", "PRISIPCP", "PRIPJPRC", "PRIPJPCP", "PRIPRCPCP", 
  "PVEMPNASI", "PVEMPNAPJ", "PVEMPNAPRC", "PVEMPNAPCP", "PVEMSIPJ", "PVEMSIPRC", "PVEMSIPCP", "PVEMPJPRC", 
  "PVEMPJPCP", "PVEMPRCPCP", "PNASIPJ", "PNASIPRC", "PNASIPCP", "PNAPJPRC", "PNAPJPCP", "PNAPRCPCP", 
  "SIPJPRC", "SIPJPCP", "SIPRCPCP", "PJPRCPCP", "PRIPVEM", "PRIPNA", "PRISI", "PRIPJ", "PRIPRC", "PRIPCP", 
  "PVEMPNA", "PVEMSI", "PVEMPJ", "PVEMPRC", "PVEMPCP", "PNASI", "PNAPJ", "PNAPRC", "PNAPCP", "SIPJ", "SIPRC", 
  "SIPCP", "PJPRC", "PJPCP", "PRCPCP"
)

merged_data <- merged_data %>%
  select(-all_of(columns_to_drop))

# Step 5: Reorder columns and rename variables
merged_data <- merged_data %>%
  relocate(PAN_UDC_PPC_PES, PRI_PVEM_PANAL_SI_PJ_PRC_PCP, .before = morena) %>%
  rename(
    PRI = pri, PRD = prd, PT = pt, PVEM = pvem, MC = pmc, PANAL = pna, SI = si, PJ = pj,
    PRC = prc, PCP = pcp, MORENA = morena, CI_1 = cand_ind1, CI_2 = cand_ind2, CI_3 = cand_ind3
  )

# Rename additional columns
merged_data <- merged_data %>%
  rename(
    section = seccion,
    total = TOTAL
  )

# Step 6: Collapse data (aggregate by sum) and add new variables
data_2017 <- merged_data %>%
  group_by(municipality, uniqueid, section) %>%
  summarize(across(PRI:CI_3, sum, na.rm = TRUE), total = sum(total, na.rm = TRUE), .groups = "drop")

data_2017 <- data_2017 %>%
  mutate(
    year = 2017,
    month = "June"
  )

# Step 7: Compute `valid` as the row sum of specific columns
valid_cols <- c(
  "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "SI", "PJ", "PRC", "PCP", "MORENA",
  "PAN_UDC_PPC_PES", "PRI_PVEM_PANAL_SI_PJ_PRC_PCP", "CI_1", "CI_2", "CI_3"
)

data_2017 <- data_2017 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(all_of(valid_cols)), na.rm = TRUE)) %>%
  ungroup()

# Step 1: Add the `STATE` column
data_2017 <- data_2017 %>%
  mutate(STATE = "COAHUILA")

# Step 2: Load the LN2017 dataset
ln2017 <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2017/LN2017.dta") %>%
  filter(entidad == 5, month == 5) %>%  # Filter for "entidad == 5" and "month == 5"
  mutate(
    uniqueid = (entidad * 1000) + municipio  # Generate `uniqueid`
  ) %>%
  filter(seccion != 0) %>%  # Exclude rows where `seccion == 0`
  arrange(uniqueid, seccion) %>%  # Sort by `uniqueid` and `seccion`
  select(uniqueid, seccion, lista) %>%  # Keep necessary columns
  rename(section = seccion)  # Rename `seccion` to `section`

# Step 3: Merge `collapsed_data` with `LN17_COAH`
data_2017 <- data_2017 %>%
  left_join(ln2017, by = c("section","uniqueid"))  # Perform a 1:1 merge on `section`

# Drop rows where `_merge == 2` (equivalent to unmatched rows in LN17)
data_2017 <- data_2017 %>%
  filter(!is.na(lista))  # Keep rows where `lista` is not NA

# Step 4: Rename `lista` to `listanominal`
data_2017 <- data_2017 %>%
  rename(listanominal = lista)

# Step 6: Compute `mun_turnout` and `turnout`
data_2017 <- data_2017 %>%
  mutate(
    turnout = total / listanominal  # Section-level turnout
  )
summary(data_2017)
############################################
## 2018 Data Processing
############################################

# Step 1: Import computos2018_XCasilla_.xlsx
computos_2018 <- read_excel("../../../Data/Raw Electoral Data/Coahuila - 1996, 1999, 2002, 2005, 2009, 2013,2017,2018/computos2018_XCasilla_.xlsx", sheet = "computos2018-0507_1030")

# Step 2: Process and manipulate columns similar to 2017
computos_2018 <- computos_2018 %>%
  dplyr::rename(section = SECC, 
                municipality = MUNICIPIO, 
                PANAL = NVA_ALIANZA) %>%
  dplyr::mutate(PRI_PVEM_PANAL = PRI + PVEM + PANAL + PRI_PVEM_NVA_ALIANZA + PRI_PVEM + PRI_NVA_ALIANZA + PVEM_NVA_ALIANZA) %>%
  dplyr::select(-PRI, -PVEM, -PANAL, -PRI_PVEM_NVA_ALIANZA, -PRI_PVEM, -PRI_NVA_ALIANZA, -PVEM_NVA_ALIANZA)

# Replace PAN_UDC_MC with the sum of several variables and drop others
computos_2018 <- computos_2018 %>%
  dplyr::mutate(PAN_UDC_MC = PAN + UDC + MC + PAN_UDC_MC + PAN_UDC + PAN_MC + UDC_MC) %>%
  dplyr::select(-PAN, -UDC, -MC, -PAN_UDC, -PAN_MC, -UDC_MC)

# Rename variable ES to PES
computos_2018 <- computos_2018 %>% 
  dplyr::rename(PES = ES)

# Create PT_MORENA_PES for "VILLA UNION" and adjust variables accordingly
computos_2018 <- computos_2018 %>%
  dplyr::mutate(
      PT_MORENA_PES = ifelse(
      municipality == "VILLA UNION",
      PT + MORENA + PES + PT_MORENA_ES + PT_MORENA + PT_ES + MORENA_ES,
      NA
    ),
    PT = ifelse(municipality == "VILLA UNION", NA, PT),
    MORENA = ifelse(municipality == "VILLA UNION", NA, MORENA),
    PES = ifelse(municipality == "VILLA UNION", NA, PES)
  ) %>%
  select(-PT_MORENA, -PT_ES, -MORENA_ES, -PT_MORENA_ES)

# Rename candidate indicator variables
computos_2018 <- computos_2018 %>%
  dplyr::rename(
    CI_1 = cand_ind1,
    CI_2 = cand_ind2,
    CI_3 = cand_ind3,
    CI_4 = cand_ind4,
    CI_5 = cand_ind5,
    CI_6 = cand_ind6,
    CI_7 = cand_ind7,
    CI_8 = cand_ind8,
    CI_9 = cand_ind9,
    CI_10 = cand_ind10
  )

# Preserve the dataset and prepare a subset for merging
subset_data <- ayuntamientos_2017 %>% 
  dplyr::select(municipality, uniqueid) %>%
  distinct() # Remove duplicates

# Merge with the saved subset
computos_2018 <- computos_2018 %>%
  left_join(subset_data, by = "municipality")

# Replace unique IDs for specific municipalities
computos_2018 <- computos_2018 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "CUATROCIENEGAS" ~ 5007,
      municipality == "FCO. I MADERO" ~ 5009,
      municipality == "JIMENEZ" ~ 5014,
      municipality == "JUAREZ" ~ 5015,
      municipality == "MUZQUIZ" ~ 5020,
      municipality == "TORREON" ~ 5035,
      municipality == "VILLA UNION" ~ 5037,
      TRUE ~ uniqueid
    )
  )

# Rename `total_votos` to `total`
computos_2018 <- computos_2018 %>% 
  dplyr::rename(total = total_votos)

# Collapse data: sum PRD-CI_10 and total by municipality, uniqueid, and section
computos_2018 <- computos_2018 %>%
  dplyr::group_by(municipality, uniqueid, section) %>%
  dplyr::summarise(across(c(PRD:CI_10, total,PRI_PVEM_PANAL,PT_MORENA_PES), 
                          sum, na.rm = TRUE), .groups = "drop")

# Create a valid vote total
computos_2018 <- computos_2018 %>%
  dplyr::mutate(valid = rowSums(across(c(PRD, PT, MORENA, PES, PRI_PVEM_PANAL, PT_MORENA_PES,
                                  PAN_UDC_MC, CI_1:CI_10)), na.rm = TRUE))
# Add year, month, and state information
computos_2018 <- computos_2018 %>%
  dplyr::mutate(
    year = 2018,
    month = "July",
    STATE = "COAHUILA"
  )

# Process Listado Nominal data
listado_nominal <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ListadoNominalPREP2018.dta") %>%
  dplyr::filter(STATE == "COAHUILA") %>%
  dplyr::select(STATE, section, ListadoNominalINE) %>%
  dplyr::rename(listanominal = ListadoNominalINE)

# Merge with the collapsed dataset
computos_2018 <- computos_2018 %>%
  left_join(listado_nominal, by = c("STATE", "section"))

# Calculate turnout rates
data_2018 <- computos_2018 %>%
  dplyr::mutate(
    turnout = total / listanominal
  )

# Combine the dataframes, handling different columns by filling with NA
coahuila_all <- bind_rows(data_1996,
                          data_1999,
                          data_2002,
                          data_2005,
                          data_2009,
                          data_2010,
                          data_2013,
                          data_2017,
                          data_2018 )

summary(coahuila_all)

data.table::fwrite(coahuila_all,"../../../Processed Data/coahuila/coahuila_process_raw_data.csv")

