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

#####################################
### PROCESSING DATA FOR 1995
#####################################

# Load the 1995 data
data_1995 <- read_csv("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_1995.csv")
names(data_1995)
# Rename columns
data_1995 <- data_1995 %>%
  dplyr::rename(
    municipality = Mpio,
    section = Secc,
    listanominal = "Lista Nominal",
    nulos = "Nulos",
    total = TOTAL)

# Remove rows with missing or zero totals
data_1995 <- data_1995 %>%
  dplyr::filter(!is.na(total), total != 0)

# Collapse the data by municipality and section, summing the variables
collapsed_1995 <- data_1995 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(listanominal, PAN:nulos,total), sum))

# Rename parties
collapsed_1995 <- collapsed_1995 %>%
  dplyr::rename(
    PartCardenista = PFCRN)

# Calculate turnout
collapsed_1995 <- collapsed_1995 %>%
  dplyr::mutate(turnout = total / listanominal)

# Generate a unique ID based on the municipality
# Create unique IDs based on municipality
collapsed_1995 <- collapsed_1995 %>%
  dplyr::mutate(uniqueid = case_when(
    municipality == 1 ~ 7001,
    municipality == 2 ~ 7002,
    municipality == 3 ~ 7003,
    municipality == 4 ~ 7004,
    municipality == 5 ~ 7005,
    municipality == 6 ~ 7006,
    municipality == 7 ~ 7007,
    municipality == 8 ~ 7008,
    municipality == 9 ~ 7009,
    municipality == 10 ~ 7010,
    municipality == 11 ~ 7011,
    municipality == 12 ~ 7012,
    municipality == 13 ~ 7013,
    municipality == 14 ~ 7014,
    municipality == 15 ~ 7015,
    municipality == 16 ~ 7016,
    municipality == 17 ~ 7017,
    municipality == 18 ~ 7018,
    municipality == 19 ~ 7019,
    municipality == 20 ~ 7020,
    municipality == 21 ~ 7021,
    municipality == 22 ~ 7022,
    municipality == 23 ~ 7023,
    municipality == 24 ~ 7024,
    municipality == 25 ~ 7025,
    municipality == 26 ~ 7026,
    municipality == 27 ~ 7027,
    municipality == 28 ~ 7028,
    municipality == 29 ~ 7029,
    municipality == 30 ~ 7030,
    municipality == 31 ~ 7031,
    municipality == 32 ~ 7032,
    municipality == 33 ~ 7033,
    municipality == 34 ~ 7034,
    municipality == 35 ~ 7035,
    municipality == 36 ~ 7036,
    municipality == 37 ~ 7037,
    municipality == 38 ~ 7039,
    municipality == 39 ~ 7038,
    municipality == 40 ~ 7040,
    municipality == 41 ~ 7041,
    municipality == 42 ~ 7042,
    municipality == 43 ~ 7043,
    municipality == 44 ~ 7044,
    municipality == 45 ~ 7045,
    municipality == 46 ~ 7046,
    municipality == 47 ~ 7047,
    municipality == 48 ~ 7048,
    municipality == 49 ~ 7049,
    municipality == 50 ~ 7050,
    municipality == 51 ~ 7051,
    municipality == 52 ~ 7052,
    municipality == 53 ~ 7053,
    municipality == 54 ~ 7054,
    municipality == 55 ~ 7055,
    municipality == 56 ~ 7056,
    municipality == 57 ~ 7057,
    municipality == 58 ~ 7058,
    municipality == 59 ~ 7059,
    municipality == 60 ~ 7060,
    municipality == 61 ~ 7061,
    municipality == 62 ~ 7062,
    municipality == 63 ~ 7063,
    municipality == 64 ~ 7064,
    municipality == 65 ~ 7065,
    municipality == 66 ~ 7066,
    municipality == 67 ~ 7067,
    municipality == 68 ~ 7068,
    municipality == 69 ~ 7069,
    municipality == 70 ~ 7070,
    municipality == 71 ~ 7072,
    municipality == 72 ~ 7073,
    municipality == 73 ~ 7074,
    municipality == 74 ~ 7075,
    municipality == 75 ~ 7076,
    municipality == 76 ~ 7077,
    municipality == 77 ~ 7078,
    municipality == 78 ~ 7079,
    municipality == 79 ~ 7112,
    municipality == 80 ~ 7110,
    municipality == 81 ~ 7080,
    municipality == 82 ~ 7081,
    municipality == 83 ~ 7082,
    municipality == 84 ~ 7083,
    municipality == 85 ~ 7084,
    municipality == 86 ~ 7085,
    municipality == 87 ~ 7086,
    municipality == 88 ~ 7087,
    municipality == 89 ~ 7088,
    municipality == 90 ~ 7089,
    municipality == 91 ~ 7090,
    municipality == 92 ~ 7091,
    municipality == 93 ~ 7092,
    municipality == 94 ~ 7093,
    municipality == 95 ~ 7094,
    municipality == 96 ~ 7096,
    municipality == 97 ~ 7097,
    municipality == 98 ~ 7098,
    municipality == 99 ~ 7099,
    municipality == 100 ~ 7100,
    municipality == 101 ~ 7102,
    municipality == 102 ~ 7101,
    municipality == 103 ~ 7103,
    municipality == 104 ~ 7104,
    municipality == 105 ~ 7105,
    municipality == 106 ~ 7106,
    municipality == 107 ~ 7107,
    municipality == 108 ~ 7108,
    municipality == 109 ~ 7109,
    municipality == 110 ~ 7110,
    municipality == 111 ~ 7111
  ))

# Calculate the total valid votes
collapsed_1995 <- collapsed_1995 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(valid = sum(c_across(PAN:PFCPCH)))

# Add year and month variables
collapsed_1995 <- collapsed_1995 %>%
  as.data.frame() %>% 
  dplyr::mutate(year = 1995, month = "October") %>% 
  dplyr::select(-municipality)

summary(collapsed_1995)

#####################################
### PROCESSING DATA FOR 1998
#####################################

# Load the 1998 data
data_1998 <- read_csv("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_1998_No_LN.csv")
names(data_1998)
# Rename columns
data_1998 <- data_1998 %>%
  dplyr::rename(
    municipality = MUNICIPIO,
    section = SECCION,
    nulos = Nulos)

# Remove rows with missing or zero totals
data_1998 <- data_1998 %>%
  dplyr::mutate(total = rowSums(across(c(PAN:nulos)), na.rm = TRUE)) %>%
  dplyr::filter(!is.na(total), total != 0) %>%
  filter(!is.na(municipality), !is.na(section)) 

# Collapse the data by municipality and section, summing the variables
collapsed_1998 <- data_1998 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN:JUSTA, nulos, total), sum, na.rm = TRUE))

collapsed_1998 <- collapsed_1998 %>%
  dplyr::rename(
         PartCardenista = PFC, 
         PT_PFCPCH = "ALIANZA FRAYL.",
         Alianza_Justa = JUSTA) %>%
  dplyr::mutate(uniqueid = case_when(
    municipality == "ACALA" ~ 7002, municipality == "ACACOYAGUA" ~ 7001, municipality == "ACAPETAHUA" ~ 7003,
    municipality == "ALTAMIRANO" ~ 7004, municipality == "AMATAN" ~ 7005, municipality == "AMATENANGO DE LA FRA." ~ 7006,
    municipality == "AMATENANGO DEL VALLE" ~ 7007, municipality == "ANGEL ALBINO CORZO" ~ 7008, municipality == "ARRIAGA" ~ 7009,
    municipality == "BEJUCAL DE OCAMPO" ~ 7010, municipality == "BELLAVISTA" ~ 7011, municipality == "BERRIOZABAL" ~ 7012,
    municipality == "BOCHIL" ~ 7013, municipality == "BOSQUE, EL" ~ 7014, municipality == "CACAHOATAN" ~ 7015,
    municipality == "CATAZAJA" ~ 7016, municipality == "CHALCHIHUITAN" ~ 7022, municipality == "CHANAL" ~ 7024,
    municipality == "CHAPULTENANGO" ~ 7025, municipality == "CHENALHO" ~ 7026, municipality == "CHIAPA DE CORZO" ~ 7027,
    municipality == "CHIAPILLA" ~ 7028, municipality == "CHICOASEN" ~ 7029, municipality == "CHICOMUSELO" ~ 7030,
    municipality == "CHILON" ~ 7031, municipality == "CINTALAPA" ~ 7017, municipality == "COAPILLA" ~ 7018,
    municipality == "COMITAN" ~ 7019, municipality == "CONCORDIA, LA" ~ 7020, municipality == "COPAINALA" ~ 7021,
    municipality == "FRANCISCO LEON" ~ 7033, municipality == "FRONTERA COMALAPA" ~ 7034, municipality == "FRONTERA HIDALGO" ~ 7035,
    municipality == "GRANDEZA, LA" ~ 7036, municipality == "HUEHUETAN" ~ 7037, municipality == "HUITIUPAN" ~ 7039,
    municipality == "HUIXTAN" ~ 7038, municipality == "INDEPENDENCIA, LA" ~ 7041, municipality == "IXHUATAN" ~ 7042,
    municipality == "IXTACOMITAN" ~ 7043, municipality == "IXTAPA" ~ 7044, municipality == "IXTAPANGAJOYA" ~ 7045,
    municipality == "JIQUIPILAS" ~ 7046, municipality == "JITOTOL" ~ 7047, municipality == "JUAREZ" ~ 7048,
    municipality == "LARRAINZAR" ~ 7049, municipality == "LIBERTAD, LA" ~ 7050, municipality == "MARGARITAS, LAS" ~ 7052,
    municipality == "MAZAPA DE MADERO" ~ 7053, municipality == "MAZATAN" ~ 7054, municipality == "METAPA DE DOMINGUEZ" ~ 7055,
    municipality == "MITONTIC" ~ 7056, municipality == "N CRISTOBAL DE LAS CAS" ~ 7078, municipality == "NICOLAS RUIZ" ~ 7058,
    municipality == "OCOSINGO" ~ 7059, municipality == "OCOTEPEC" ~ 7060, municipality == "OCOZOCOAUTLA" ~ 7061,
    municipality == "OSTUACAN" ~ 7062, municipality == "OSUMACINTA" ~ 7063, municipality == "OXCHUC" ~ 7064,
    municipality == "PALENQUE" ~ 7065, municipality == "PANTELHO" ~ 7066, municipality == "PANTEPEC" ~ 7067,
    municipality == "PICHUCALCO" ~ 7068, municipality == "PORVENIR, EL" ~ 7070, municipality == "RAYON" ~ 7073,
    municipality == "REFORMA" ~ 7074, municipality == "ROSAS, LAS" ~ 7075, municipality == "SABANILLA" ~ 7076,
    municipality == "Salto de Agua" ~ 7077, municipality == "SAN FERNANDO" ~ 7079, municipality == "SAN JUAN CANCUC" ~ 7112,
    municipality == "SAN LUCAS" ~ 7110, municipality == "SIMOJOVEL" ~ 7081, municipality == "SITALA" ~ 7082,
    municipality == "SOCOLTENANGO" ~ 7083, municipality == "SOLOSUCHIAPA" ~ 7084, municipality == "SOYALO" ~ 7085,
    municipality == "SUCHIAPA" ~ 7086, municipality == "SUCHIATE" ~ 7087, municipality == "SUNUAPA" ~ 7088,
    municipality == "TAPACHULA" ~ 7089, municipality == "TAPALAPA" ~ 7090, municipality == "TAPILULA" ~ 7091,
    municipality == "TECPATAN" ~ 7092, municipality == "TENEJAPA" ~ 7093, municipality == "TEOPISCA" ~ 7094,
    municipality == "TILA" ~ 7096, municipality == "TONALA" ~ 7097, municipality == "TOTOLAPA" ~ 7098,
    municipality == "TRINITARIA, LA" ~ 7099, municipality == "TUMBALA" ~ 7100, municipality == "TUXTLA CHICO" ~ 7102,
    municipality == "TUXTLA GUTIERREZ" ~ 7101, municipality == "TUZANTAN" ~ 7103, municipality == "TZIMOL" ~ 7104,
    municipality == "UNION JUAREZ" ~ 7105, municipality == "VENUSTIANO CARRANZA" ~ 7106, municipality == "VILLACORZO" ~ 7107,
    municipality == "VILLAFLORES" ~ 7108, municipality == "YAJALON" ~ 7109, municipality == "ZINACANTAN" ~ 7111,
    municipality == "CHAMULA" ~ 7023,
    municipality == "EBLO NVO. SOLISTAHUAC" ~ 7072,
    municipality == "ESCUINTLA" ~ 7032,
    municipality == "HUIXTLA" ~ 7040,
    municipality == "MAPASTEPEC" ~ 7051,
    municipality == "MOTOZINTLA" ~ 7057,
    municipality == "PIJIJIAPAN" ~ 7069,
    municipality == "SILTEPEC" ~ 7080,
    municipality == "VILLA COMALTITLAN" ~ 7071)) %>%
  dplyr::mutate(valid = sum(c_across(PAN:Alianza_Justa), na.rm = TRUE))

# Load and merge Lista Nominal data
ln_all_months_years <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

ln_all_months_years <- ln_all_months_years %>%
  dplyr::filter(month == "June" & year == 1998 & state == "CHIAPAS") %>%
  dplyr::select(section, lista)

collapsed_1998 <- collapsed_1998 %>%
  dplyr::left_join(ln_all_months_years, by = c("section"))

collapsed_1998 <- collapsed_1998 %>%
  dplyr::rename(listanominal=lista)

# Calculate the total valid votes
collapsed_1998 <- collapsed_1998 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(valid = sum(c_across(PAN:Alianza_Justa), na.rm = TRUE))

# Calculate 'turnout'
collapsed_1998 <- collapsed_1998 %>%
  dplyr::mutate(turnout = total / listanominal,
                month = "June", 
                year = 1998)

rm(data_1998)
rm(ln_all_months_years)

#####################################
### PROCESSING DATA FOR 2001
#####################################

data_2001 <- data.table::fread("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_2001_No_LN.csv") %>%
  dplyr::rename(municipality = MUNICIPIO, 
                section = "Secci\xf3n",
                total = TOTAL,
                nulos = Nulos) %>%
  dplyr::filter(municipality != "" & !is.na(section))%>%
  dplyr::filter(!is.na(total), total != 0)

names(data_2001)
data_2001 <- data_2001 %>%
  dplyr::group_by(CVE, municipality, section) %>%
  dplyr::summarise(across(PAN:total, sum, na.rm = TRUE)) %>%
  dplyr::mutate(prdptpvempsn = ifelse(municipality == "Tuxtla Gutierrez", 
                                      rowSums(across(c(PRD , PT , PVEM , PSN , COALICION)), na.rm = TRUE), 0),
         PRD = ifelse(municipality == "Tuxtla Gutierrez", 0, PRD),
         PT = ifelse(municipality == "Tuxtla Gutierrez", 0, PT),
         PVEM = ifelse(municipality == "Tuxtla Gutierrez", 0, PVEM),
         PSN = ifelse(municipality == "Tuxtla Gutierrez", 0, PSN),
         COALICION = ifelse(municipality == "Tuxtla Gutierrez", 0, COALICION),
         prdpt = ifelse(municipality %in% c("Acala", 
                                            "Chiapilla", 
                                            "Comitan de Dominguez", 
                                            "Socoltenango", 
                                            "Suchiapa", 
                                            "Villa Corzo"), 
                        rowSums(across(c(PRD , PT , COALICION)), na.rm = TRUE), 0),
         PRD = ifelse(municipality %in% c("Acala", 
                                          "Chiapilla", 
                                          "Comitan de Dominguez", 
                                          "Socoltenango", 
                                          "Suchiapa", 
                                          "Villa Corzo"),  0, PRD),
         PT = ifelse(municipality %in% c("Acala", 
                                         "Chiapilla", 
                                         "Comitan de Dominguez", 
                                         "Socoltenango", 
                                         "Suchiapa", 
                                         "Villa Corzo"), 0, PT),
         COALICION = ifelse(municipality %in% c("Acala", 
                                                "Chiapilla", 
                                                "Comitan de Dominguez", 
                                                "Socoltenango", 
                                                "Suchiapa", 
                                                "Villa Corzo"),  0, COALICION),
         prdpvem = ifelse(municipality == "Jiquipilas", 
                          rowSums(across(c(PRD , PVEM , COALICION)), na.rm = TRUE), 0)) %>% 
  dplyr::ungroup()

table(data_2001$CVE)


collapsed_2001 <- data_2001 %>%
  dplyr::rename(UNKNOWN = COALICION, 
         PRD_PT_PVEM_PSN = prdptpvempsn, 
         PRD_PT = prdpt, 
         PRD_PVEM = prdpvem) %>%
  dplyr::select(-c("No Registrados")) %>%
  dplyr::mutate(uniqueid = case_when(
    CVE == 1 ~ 7001,
    CVE == 2 ~ 7002,
    CVE == 3 ~ 7003,
    CVE == 4 ~ 7004,
    CVE == 5 ~ 7005,
    CVE == 6 ~ 7006,
    CVE == 7 ~ 7007,
    CVE == 8 ~ 7008,
    CVE == 9 ~ 7009,
    CVE == 10 ~ 7010,
    CVE == 11 ~ 7011,
    CVE == 12 ~ 7012,
    CVE == 13 ~ 7013,
    CVE == 14 ~ 7014,
    CVE == 15 ~ 7015,
    CVE == 16 ~ 7016,
    CVE == 17 ~ 7017,
    CVE == 18 ~ 7018,
    CVE == 19 ~ 7019,
    CVE == 20 ~ 7020,
    CVE == 21 ~ 7021,
    CVE == 22 ~ 7022,
    CVE == 23 ~ 7023,
    CVE == 24 ~ 7024,
    CVE == 25 ~ 7025,
    CVE == 26 ~ 7026,
    CVE == 27 ~ 7027,
    CVE == 28 ~ 7028,
    CVE == 29 ~ 7029,
    CVE == 30 ~ 7030,
    CVE == 31 ~ 7031,
    CVE == 32 ~ 7032,
    CVE == 33 ~ 7033,
    CVE == 34 ~ 7034,
    CVE == 35 ~ 7035,
    CVE == 36 ~ 7036,
    CVE == 37 ~ 7037,
    CVE == 38 ~ 7039,
    CVE == 39 ~ 7038,
    CVE == 40 ~ 7040,
    CVE == 41 ~ 7041,
    CVE == 42 ~ 7042,
    CVE == 43 ~ 7043,
    CVE == 44 ~ 7044,
    CVE == 45 ~ 7045,
    CVE == 46 ~ 7046,
    CVE == 47 ~ 7047,
    CVE == 48 ~ 7048,
    CVE == 49 ~ 7049,
    CVE == 50 ~ 7050,
    CVE == 51 ~ 7051,
    CVE == 52 ~ 7052,
    CVE == 53 ~ 7053,
    CVE == 54 ~ 7054,
    CVE == 55 ~ 7055,
    CVE == 56 ~ 7056,
    CVE == 57 ~ 7057,
    CVE == 58 ~ 7058,
    CVE == 59 ~ 7059,
    CVE == 60 ~ 7060,
    CVE == 61 ~ 7061,
    CVE == 62 ~ 7062,
    CVE == 63 ~ 7063,
    CVE == 64 ~ 7064,
    CVE == 65 ~ 7065,
    CVE == 66 ~ 7066,
    CVE == 67 ~ 7067,
    CVE == 68 ~ 7068,
    CVE == 69 ~ 7069,
    CVE == 70 ~ 7070,
    CVE == 71 ~ 7072,
    CVE == 72 ~ 7073,
    CVE == 73 ~ 7074,
    CVE == 74 ~ 7075,
    CVE == 75 ~ 7076,
    CVE == 76 ~ 7077,
    CVE == 77 ~ 7078,
    CVE == 78 ~ 7079,
    CVE == 79 ~ 7112,
    CVE == 80 ~ 7110,
    CVE == 81 ~ 7080,
    CVE == 82 ~ 7081,
    CVE == 83 ~ 7082,
    CVE == 84 ~ 7083,
    CVE == 85 ~ 7084,
    CVE == 86 ~ 7085,
    CVE == 87 ~ 7086,
    CVE == 88 ~ 7087,
    CVE == 89 ~ 7088,
    CVE == 90 ~ 7089,
    CVE == 91 ~ 7090,
    CVE == 92 ~ 7091,
    CVE == 93 ~ 7092,
    CVE == 94 ~ 7093,
    CVE == 95 ~ 7094,
    CVE == 96 ~ 7096,
    CVE == 97 ~ 7097,
    CVE == 98 ~ 7098,
    CVE == 99 ~ 7099,
    CVE == 100 ~ 7100,
    CVE == 101 ~ 7102,
    CVE == 102 ~ 7101,
    CVE == 103 ~ 7103,
    CVE == 104 ~ 7104,
    CVE == 105 ~ 7105,
    CVE == 106 ~ 7106,
    CVE == 107 ~ 7107,
    CVE == 108 ~ 7108,
    CVE == 109 ~ 7109,
    CVE == 110 ~ 7110,
    CVE == 111 ~ 7111,
    CVE == 112 ~ 7112,
    CVE == 113 ~ 7113,
    CVE == 114 ~ 7114,
    CVE == 115 ~ 7115,
    CVE == 116 ~ 7116,
    CVE == 117 ~ 7117,
    CVE == 118 ~ 7118)) %>% 
  dplyr::select(-CVE)

collapsed_2001 <- collapsed_2001 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN:UNKNOWN,PRD_PT_PVEM_PSN,PRD_PT,PRD_PVEM)), na.rm = TRUE))

# Load and merge Lista Nominal data
ln_all_months_years <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

ln_all_months_years <- ln_all_months_years %>%
  dplyr::filter(month == "June" & year == 2001 & state == "CHIAPAS") %>%
  dplyr::select(section, lista)

collapsed_2001 <- collapsed_2001 %>%
  dplyr::left_join(ln_all_months_years, by = c("section"))

collapsed_2001 <- collapsed_2001 %>%
  dplyr::rename(listanominal=lista)

# Calculate turnout
collapsed_2001 <- collapsed_2001 %>%
  dplyr::mutate(turnout = total / listanominal)

# Add year and month variables
collapsed_2001 <- collapsed_2001 %>%
  dplyr::mutate(year = 2001, month = "June")
#####################################
### PROCESSING DATA FOR 2004
#####################################

data_2004 <- read.csv("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_2004.csv") %>%
  dplyr::rename(municipality = MUNICIPIO, 
         section = SECCION,
         listanominal = "LISTA.NOMINAL",
         nulos = NULOS) %>%
  dplyr::filter(municipality != "" & !is.na(section)) %>%
  dplyr::mutate(total = rowSums(across(c(PAN:Convergencia,NOREGISTRADOS)), na.rm = TRUE)) %>%
  dplyr::filter(!is.na(total), total != 0)

collapsed_2004 <- data_2004 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(listanominal:total, sum, na.rm = TRUE)) %>%
  dplyr::rename(
         PC = Convergencia, 
         PAN_PRD = "PAN.PRD", 
         PAN_PRD_PT = "PAN.PRD.PT", 
         PAN_PT = "PAN.PT", 
         PRI_PVEM = "PRI.PVEM", 
         PRD_PT = "PRD.PT") %>%
  dplyr::mutate(turnout = total / listanominal) %>%
  dplyr::select(-NOREGISTRADOS)

collapsed_2004 <- collapsed_2004 %>%
  dplyr::mutate(valid = rowSums(across(PAN:PC), na.rm = TRUE)) %>%
  dplyr::mutate(year = 2004, month = "October") %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ACALA" ~ 7002,
      municipality == "ALDAMA" ~ 7113,
      municipality == "AMATENANGO DE LA FRONTERA" ~ 7006,
      municipality == "AMAT\xc1N" ~ 7005,
      municipality == "ACACOYAGUA" ~ 7001,
      municipality == "ACAPETAHUA" ~ 7003,
      municipality == "ALTAMIRANO" ~ 7004,
      municipality == "AMATAN" ~ 7005,
      municipality == "AMATENANGO DE LA FRA." ~ 7006,
      municipality == "AMATENANGO DEL VALLE" ~ 7007,
      municipality == "\xc1NGEL ALBINO CORZO" ~ 7008,
      municipality == "ARRIAGA" ~ 7009,
      municipality == "BEJUCAL DE OCAMPO" ~ 7010,
      municipality == "BELLAVISTA" ~ 7011,
      municipality == "BERRIOZ\xc1BAL" ~ 7012,
      municipality == "BOCHIL" ~ 7013,
      municipality == "BOSQUE, EL" ~ 7014,
      municipality == "CACAHOAT\xc1N" ~ 7015,
      municipality == "CATAZAJ\xc1" ~ 7016,
      municipality == "CHALCHIHUIT\xc1N" ~ 7022,
      municipality == "CHANAL" ~ 7024,
      municipality == "CHAPULTENANGO" ~ 7025,
      municipality == "CHENALH\xd3" ~ 7026,
      municipality == "CHIAPA DE CORZO" ~ 7027,
      municipality == "CHIAPILLA" ~ 7028,
      municipality == "CHICOAS\xc9N" ~ 7029,
      municipality == "CHICOMUSELO" ~ 7030,
      municipality == "CHIL\xd3N" ~ 7031,
      municipality == "CINTALAPA" ~ 7017,
      municipality == "COAPILLA" ~ 7018,
      municipality == "COMIT\xc1N" ~ 7019,
      municipality == "CONCORDIA, LA" ~ 7020,
      municipality == "COPAINAL\xc1" ~ 7021,
      municipality == "FRANCISCO LE\xd3N" ~ 7033,
      municipality == "FRONTERA COMALAPA" ~ 7034,
      municipality == "FRONTERA HIDALGO" ~ 7035,
      municipality == "GRANDEZA, LA" ~ 7036,
      municipality == "HUEHUET\xc1N" ~ 7037,
      municipality == "HUITIUP\xc1N" ~ 7039,
      municipality == "HUIXTLA" ~ 7040,
      municipality == "HUIXT\xc1N" ~ 7038,
      municipality == "INDEPENDENCIA, LA" ~ 7041,
      municipality == "IXHUAT\xc1N" ~ 7042,
      municipality == "IXTACOMIT\xc1N" ~ 7043,
      municipality == "IXTAPA" ~ 7044,
      municipality == "IXTAPANGAJOYA" ~ 7045,
      municipality == "JIQUIPILAS" ~ 7046,
      municipality == "JITOTOL" ~ 7047,
      municipality == "JU\xc1REZ" ~ 7048,
      municipality == "LARRA\xcdNZAR" ~ 7049,
      municipality == "LIBERTAD, LA" ~ 7050,
      municipality == "MARGARITAS, LAS" ~ 7052,
      municipality == "MAZAPA DE MADERO" ~ 7053,
      municipality == "MAZAT\xc1N" ~ 7054,
      municipality == "METAPA DE DOM\xcdNGUEZ" ~ 7055,
      municipality == "MITONTIC" ~ 7056,
      municipality == "N CRISTOBAL DE LAS CAS" ~ 7078,
      municipality == "NICOL\xc1S RUIZ" ~ 7058,
      municipality == "OCOSINGO" ~ 7059,
      municipality == "OCOTEPEC" ~ 7060,
      municipality == "OCOZOCOAUTLA" ~ 7061,
      municipality == "OSTUAC\xc1N" ~ 7062,
      municipality == "OSUMACINTA" ~ 7063,
      municipality == "OXCHUC" ~ 7064,
      municipality == "PALENQUE" ~ 7065,
      municipality == "PANTELH\xd3" ~ 7066,
      municipality == "PANTEPEC" ~ 7067,
      municipality == "PICHUCALCO" ~ 7068,
      municipality == "PORVENIR, EL" ~ 7070,
      municipality == "RAY\xd3N" ~ 7073,
      municipality == "REFORMA" ~ 7074,
      municipality == "ROSAS, LAS" ~ 7075,
      municipality == "SABANILLA" ~ 7076,
      municipality == "Salto de Agua" ~ 7077,
      municipality == "SAN FERNANDO" ~ 7079,
      municipality == "SAN JUAN CANCUC" ~ 7112,
      municipality == "SAN LUCAS" ~ 7110,
      municipality == "SIMOJOVEL" ~ 7081,
      municipality == "SITAL\xc1" ~ 7082,
      municipality == "SOCOLTENANGO" ~ 7083,
      municipality == "SOLOSUCHIAPA" ~ 7084,
      municipality == "SOYAL\xd3" ~ 7085,
      municipality == "SUCHIAPA" ~ 7086,
      municipality == "SUCHIATE" ~ 7087,
      municipality == "SUNUAPA" ~ 7088,
      municipality == "TAPACHULA" ~ 7089,
      municipality == "TAPALAPA" ~ 7090,
      municipality == "TAPILULA" ~ 7091,
      municipality == "TECPAT\xc1N" ~ 7092,
      municipality == "TENEJAPA" ~ 7093,
      municipality == "TEOPISCA" ~ 7094,
      municipality == "TILA" ~ 7096,
      municipality == "TONALA" ~ 7097,
      municipality == "TOTOLAPA" ~ 7098,
      municipality == "TRINITARIA, LA" ~ 7099,
      municipality == "TUMBALA" ~ 7100,
      municipality == "TUXTLA CHICO" ~ 7102,
      municipality == "TUXTLA GUTI\xc9RREZ" ~ 7101,
      municipality == "TUZANT\xc1N" ~ 7103,
      municipality == "TZIMOL" ~ 7104,
      municipality == "UNI\xd3N JU\xc1REZ" ~ 7105,
      municipality == "VILLA CORZO" ~ 7107,
      municipality == "VILLAFLORES" ~ 7108,
      municipality == "YAJAL\xd3N" ~ 7109,
      municipality == "ZINACANT\xc1N" ~ 7111,
      municipality == "BENEM\xc9RITO DE LAS AM\xc9RICAS" ~ 7114, 
      municipality == "CHAMULA" ~ 7023,
      municipality == "EL BOSQUE" ~ 7014,
      municipality == "EL PORVENIR" ~ 7070,
      municipality == "ESCUINTLA" ~ 7032,
      municipality == "LA CONCORDIA" ~ 7020,
      municipality == "LA GRANDEZA" ~ 7036,
      municipality == "LA INDEPENDENCIA" ~ 7041,
      municipality == "LA LIBERTAD" ~ 7050,
      municipality == "LA TRINITARIA" ~ 7099,
      municipality == "LAS MARGARITAS" ~ 7052,
      municipality == "LAS ROSAS" ~ 7075,
      municipality == "MAPASTEPEC" ~ 7051,
      municipality == "MARAVILLA TENEJAPA" ~ 7115,
      municipality == "MARQU\xc9S DE COMILLAS" ~ 7116,
      municipality == "MONTECRISTO DE GUERRERO" ~ 7117,
      municipality == "MOTOZINTLA" ~ 7057,
      municipality == "PIJIJIAPAN" ~ 7069,
      municipality == "PUEBLO NVO. SOLISTAHUAC\xc1N" ~ 7072,
      municipality == "SALTO DE AGUA" ~ 7077,
      municipality == "SAN ANDR\xc9S DURAZNAL" ~ 7118,
      municipality == "SAN CRIST\xd3BAL DE LAS CASAS" ~ 7078,
      municipality == "SANTIAGO EL PINAR" ~ 7119,
      municipality == "SILTEPEC" ~ 7080,
      municipality == "TONAL\xc1" ~ 7097,
      municipality == "TUMBAL\xc1" ~ 7100,
      municipality == "VENUSTIANO CARRANZA" ~ 7106,
      municipality == "VILLA COMALTITL\xc1N" ~ 7071,
      TRUE ~ NA_real_ )) # Default case for municipalities that are not in the list

rm(data_2004)

#####################################
### PROCESSING DATA FOR 2007
#####################################

# Processing Year 2007
data_2007 <- read.csv("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_2007.csv") %>%
  dplyr::rename(municipality = municipio, 
                section = seccion,
                listanominal = "lista.nominal",
                PC = "Convergencia") %>%
  dplyr::filter(municipality != "" & !is.na(section)) %>%
  dplyr::filter(!is.na(total), total != 0)

# Update PAN_PVEM to include the sum of PAN and PVEM where PAN_PVEM != 0
data_2007 <- data_2007 %>%
  mutate(PAN_PVEM = ifelse(PAN_PVEM != 0, PAN_PVEM + PAN + PVEM, PAN_PVEM))

# Set PAN to 0 where PAN_PVEM != 0
data_2007 <- data_2007 %>%
  mutate(PAN = ifelse(PAN_PVEM != 0, 0, PAN))

# Set PVEM to 0 where PAN_PVEM != 0
data_2007 <- data_2007 %>%
  mutate(PVEM = ifelse(PAN_PVEM != 0, 0, PVEM))


collapsed_2007 <-data_2007 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(nulos:total,listanominal), sum, na.rm = TRUE)) %>%
  dplyr::mutate(turnout = total / listanominal)

collapsed_2007 <- collapsed_2007 %>%
  dplyr::mutate(valid = rowSums(across(PAN:PT_PC), na.rm = TRUE)) %>%
  mutate(year = 2007, month = "October")%>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ACALA" ~ 7002,
      municipality == "ALDAMA" ~ 7113,
      municipality == "AMATENANGO DE LA FRONTERA" ~ 7006,
      municipality == "AMAT\xc1N" ~ 7005,
      municipality == "ACACOYAGUA" ~ 7001,
      municipality == "ACAPETAHUA" ~ 7003,
      municipality == "ALTAMIRANO" ~ 7004,
      municipality == "AMATAN" ~ 7005,
      municipality == "AMATENANGO DE LA FRA." ~ 7006,
      municipality == "AMATENANGO DEL VALLE" ~ 7007,
      municipality == "\xc1NGEL ALBINO CORZO" ~ 7008,
      municipality == "ARRIAGA" ~ 7009,
      municipality == "BEJUCAL DE OCAMPO" ~ 7010,
      municipality == "BELLAVISTA" ~ 7011,
      municipality == "BERRIOZ\xc1BAL" ~ 7012,
      municipality == "BOCHIL" ~ 7013,
      municipality == "BOSQUE, EL" ~ 7014,
      municipality == "CACAHOAT\xc1N" ~ 7015,
      municipality == "CATAZAJ\xc1" ~ 7016,
      municipality == "CHALCHIHUIT\xc1N" ~ 7022,
      municipality == "CHANAL" ~ 7024,
      municipality == "CHAPULTENANGO" ~ 7025,
      municipality == "CHENALH\xd3" ~ 7026,
      municipality == "CHIAPA DE CORZO" ~ 7027,
      municipality == "CHIAPILLA" ~ 7028,
      municipality == "CHICOAS\xc9N" ~ 7029,
      municipality == "CHICOMUSELO" ~ 7030,
      municipality == "CHIL\xd3N" ~ 7031,
      municipality == "CINTALAPA" ~ 7017,
      municipality == "COAPILLA" ~ 7018,
      municipality == "COMIT\xc1N" ~ 7019,
      municipality == "CONCORDIA, LA" ~ 7020,
      municipality == "COPAINAL\xc1" ~ 7021,
      municipality == "FRANCISCO LE\xd3N" ~ 7033,
      municipality == "FRONTERA COMALAPA" ~ 7034,
      municipality == "FRONTERA HIDALGO" ~ 7035,
      municipality == "GRANDEZA, LA" ~ 7036,
      municipality == "HUEHUET\xc1N" ~ 7037,
      municipality == "HUITIUP\xc1N" ~ 7039,
      municipality == "HUIXTLA" ~ 7040,
      municipality == "HUIXT\xc1N" ~ 7038,
      municipality == "INDEPENDENCIA, LA" ~ 7041,
      municipality == "IXHUAT\xc1N" ~ 7042,
      municipality == "IXTACOMIT\xc1N" ~ 7043,
      municipality == "IXTAPA" ~ 7044,
      municipality == "IXTAPANGAJOYA" ~ 7045,
      municipality == "JIQUIPILAS" ~ 7046,
      municipality == "JITOTOL" ~ 7047,
      municipality == "JU\xc1REZ" ~ 7048,
      municipality == "LARRA\xcdNZAR" ~ 7049,
      municipality == "LIBERTAD, LA" ~ 7050,
      municipality == "MARGARITAS, LAS" ~ 7052,
      municipality == "MAZAPA DE MADERO" ~ 7053,
      municipality == "MAZAT\xc1N" ~ 7054,
      municipality == "METAPA DE DOM\xcdNGUEZ" ~ 7055,
      municipality == "MITONTIC" ~ 7056,
      municipality == "N CRISTOBAL DE LAS CAS" ~ 7078,
      municipality == "NICOL\xc1S RUIZ" ~ 7058,
      municipality == "OCOSINGO" ~ 7059,
      municipality == "OCOTEPEC" ~ 7060,
      municipality == "OCOZOCOAUTLA" ~ 7061,
      municipality == "OSTUAC\xc1N" ~ 7062,
      municipality == "OSUMACINTA" ~ 7063,
      municipality == "OXCHUC" ~ 7064,
      municipality == "PALENQUE" ~ 7065,
      municipality == "PANTELH\xd3" ~ 7066,
      municipality == "PANTEPEC" ~ 7067,
      municipality == "PICHUCALCO" ~ 7068,
      municipality == "PORVENIR, EL" ~ 7070,
      municipality == "RAY\xd3N" ~ 7073,
      municipality == "REFORMA" ~ 7074,
      municipality == "ROSAS, LAS" ~ 7075,
      municipality == "SABANILLA" ~ 7076,
      municipality == "Salto de Agua" ~ 7077,
      municipality == "SAN FERNANDO" ~ 7079,
      municipality == "SAN JUAN CANCUC" ~ 7112,
      municipality == "SAN LUCAS" ~ 7110,
      municipality == "SIMOJOVEL" ~ 7081,
      municipality == "SITAL\xc1" ~ 7082,
      municipality == "SOCOLTENANGO" ~ 7083,
      municipality == "SOLOSUCHIAPA" ~ 7084,
      municipality == "SOYAL\xd3" ~ 7085,
      municipality == "SUCHIAPA" ~ 7086,
      municipality == "SUCHIATE" ~ 7087,
      municipality == "SUNUAPA" ~ 7088,
      municipality == "TAPACHULA" ~ 7089,
      municipality == "TAPALAPA" ~ 7090,
      municipality == "TAPILULA" ~ 7091,
      municipality == "TECPAT\xc1N" ~ 7092,
      municipality == "TENEJAPA" ~ 7093,
      municipality == "TEOPISCA" ~ 7094,
      municipality == "TILA" ~ 7096,
      municipality == "TONALA" ~ 7097,
      municipality == "TOTOLAPA" ~ 7098,
      municipality == "TRINITARIA, LA" ~ 7099,
      municipality == "TUMBALA" ~ 7100,
      municipality == "TUXTLA CHICO" ~ 7102,
      municipality == "TUXTLA GUTI\xc9RREZ" ~ 7101,
      municipality == "TUZANT\xc1N" ~ 7103,
      municipality == "TZIMOL" ~ 7104,
      municipality == "UNI\xd3N JU\xc1REZ" ~ 7105,
      municipality == "VILLA CORZO" ~ 7107,
      municipality == "VILLAFLORES" ~ 7108,
      municipality == "YAJAL\xd3N" ~ 7109,
      municipality == "ZINACANT\xc1N" ~ 7111,
      municipality == "BENEM\xc9RITO DE LAS AM\xc9RICAS" ~ 7114, 
      municipality == "CHAMULA" ~ 7023,
      municipality == "EL BOSQUE" ~ 7014,
      municipality == "EL PORVENIR" ~ 7070,
      municipality == "ESCUINTLA" ~ 7032,
      municipality == "LA CONCORDIA" ~ 7020,
      municipality == "LA GRANDEZA" ~ 7036,
      municipality == "LA INDEPENDENCIA" ~ 7041,
      municipality == "LA LIBERTAD" ~ 7050,
      municipality == "LA TRINITARIA" ~ 7099,
      municipality == "LAS MARGARITAS" ~ 7052,
      municipality == "LAS ROSAS" ~ 7075,
      municipality == "MAPASTEPEC" ~ 7051,
      municipality == "MARAVILLA TENEJAPA" ~ 7115,
      municipality == "MARQU\xc9S DE COMILLAS" ~ 7116,
      municipality == "MONTECRISTO DE GUERRERO" ~ 7117,
      municipality == "MOTOZINTLA" ~ 7057,
      municipality == "PIJIJIAPAN" ~ 7069,
      municipality == "PUEBLO NVO. SOLISTAHUAC\xc1N" ~ 7072,
      municipality == "SALTO DE AGUA" ~ 7077,
      municipality == "SAN ANDR\xc9S DURAZNAL" ~ 7118,
      municipality == "SAN CRIST\xd3BAL DE LAS CASAS" ~ 7078,
      municipality == "SANTIAGO EL PINAR" ~ 7119,
      municipality == "SILTEPEC" ~ 7080,
      municipality == "TONAL\xc1" ~ 7097,
      municipality == "TUMBAL\xc1" ~ 7100,
      municipality == "VENUSTIANO CARRANZA" ~ 7106,
      municipality == "VILLA COMALTITL\xc1N" ~ 7071,
      municipality == "ANGEL ALBINO CORZO" ~ 7008,
      municipality == "BELLA VISTA" ~ 7011,
      municipality == "BENEMERITO DE LAS AMERICAS" ~ 7114,
      municipality == "BERRIOZABAL" ~ 7012,
      municipality == "CACAHOATAN" ~ 7015,
      municipality == "CATAZAJA" ~ 7016,
      municipality == "CHALCHIHUITAN" ~ 7022,
      municipality == "CHENALHO" ~ 7026,
      municipality == "CHICOASEN" ~ 7029,
      municipality == "CHILON" ~ 7031,
      municipality == "COMITAN DE DOMINGUEZ" ~ 7019,
      municipality == "COPAINALA" ~ 7021,
      municipality == "FRANCISCO LEON" ~ 7033,
      municipality == "HUEHUETAN" ~ 7037,
      municipality == "HUITIUPAN" ~ 7039,
      municipality == "HUIXTAN" ~ 7038,
      municipality == "IXHUATAN" ~ 7042,
      municipality == "IXTACOMITAN" ~ 7043,
      municipality == "JUAREZ" ~ 7048,
      municipality == "LARRAINZAR" ~ 7049,
      municipality == "MARQUES DE COMILLAS" ~ 7116,
      municipality == "MAZATAN" ~ 7054,
      municipality == "METAPA DE DOMINGUEZ" ~ 7055,
      municipality == "NICOLAS RUIZ" ~ 7058,
      municipality == "OCOZOCOAUTLA DE ESPINOSA" ~ 7061,
      municipality == "OSTUACAN" ~ 7062,
      municipality == "PANTELHO" ~ 7066,
      municipality == "PUEBLO NVO SOLISTAHUACAN" ~ 7072,
      municipality == "RAYON" ~ 7073,
      municipality == "SAN ANDRES DURAZNAL" ~ 7118,
      municipality == "SAN CRISTOBAL DE LAS CASAS" ~ 7078,
      municipality == "SITALA" ~ 7082,
      municipality == "SOYALO" ~ 7085,
      municipality == "TECPATAN" ~ 7092,
      municipality == "TUXTLA GUTIERREZ" ~ 7101,
      municipality == "TUZANTAN" ~ 7103,
      municipality == "UNION JUAREZ" ~ 7105,
      municipality == "VILLACOMALTITLAN" ~ 7071,
      municipality == "YAJALON" ~ 7109,
      municipality == "ZINACANTAN" ~ 7111,
      TRUE ~ NA_real_  # Default case for municipalities that are not in the list
    ))

#####################################
### PROCESSING DATA FOR 2010
#####################################

data_2010 <- data.table::fread("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_2010_No_LN.csv") %>%
  dplyr::rename(municipality = MUNICIPIO, 
         section = "SECCI\xd3N") %>%
  dplyr::filter(municipality != "" & !is.na(section)) %>%
  dplyr::mutate(total = rowSums(across(PRI:"NO REGISTRADOS"), na.rm = TRUE)) %>%
  dplyr::filter(!is.na(total), total != 0)

collapsed_2010 <- data_2010 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(PRI:total, sum, na.rm = TRUE)) %>%
  dplyr::rename(PAN_PRD_PC_PANAL = "PAN-PRD-PC-PANAL", 
         PRI_PVEM = "PRI-PVEM" , 
         PAN_PRI_PRD_PT_PVEM_PC_PANAL_PSD = "PAN-PRI-PRD-PT-PVEM-PC-PANAL-PSD", 
         nulos = NULOS) 

collapsed_2010 <- collapsed_2010 %>%
  mutate(uniqueid = case_when(
    municipality == "Acacoyagua" ~ 7001,
    municipality == "Acala" ~ 7002,
    municipality == "Acapetahua" ~ 7003,
    municipality == "Aldama" ~ 7113,
    municipality == "Altamirano" ~ 7004,
    municipality == "Amatan" ~ 7005,
    municipality == "Amatenango De La Frontera" | municipality == "Amatenango de la Frontera" ~ 7006,
    municipality == "Amatenango del Valle" ~ 7007,
    municipality == "Angel Albino Corzo" ~ 7008,
    municipality == "Arriaga" ~ 7009,
    municipality == "Bella Vista" ~ 7011,
    municipality == "Bejucal De Ocampo"| municipality == "Bejucal de Ocampo" ~ 7010,
    municipality == "Benemerito De Las Americas" | municipality == "Benemerito de las Am\xe9ricas" ~ 7114,
    municipality == "Berriozabal" ~ 7012,
    municipality == "Bochil" ~ 7013,
    municipality == "Cacahoatan" | municipality == "Cacahoat\xe1n" ~ 7015,
    municipality == "Catazaja" ~ 7016,
    municipality == "Chalchihuitan" | municipality == "Chalchihuit\xe1n" ~ 7022,
    municipality == "Chamula" | municipality == "Chamula EXTRAORDINARIO" ~ 7023,
    municipality == "Chanal" ~ 7024,
    municipality == "Chapultenango" ~ 7025,
    municipality == "Chenalho" ~ 7026,
    municipality == "Chiapa De Corzo" | municipality == "Chiapa de Corzo" ~ 7027,
    municipality == "Chiapilla" ~ 7028,
    municipality == "Chicoasen" ~ 7029,
    municipality == "Chicomuselo" ~ 7030,
    municipality == "Chilon" | municipality == "Chil\xf3n" ~ 7031,
    municipality == "Cintalapa" ~ 7017,
    municipality == "Coapilla" ~ 7018,
    municipality == "Comitan De Dominguez" | municipality == "Comit\xe1n de Dom\xednguez" ~ 7019,
    municipality == "Copainala" | municipality == "Copainal\xe1" ~ 7021,
    municipality == "El Bosque" ~ 7014,
    municipality == "El Parral" ~ 7122,
    municipality == "El Porvenir" ~ 7070,
    municipality == "Emiliano Zapata" ~ 7121,
    municipality == "Escuintla" ~ 7032,
    municipality == "Francisco Leon" | municipality == "Francisco Le\xf3n" ~ 7033,
    municipality == "Frontera Comalapa" ~ 7034,
    municipality == "Frontera Hidalgo" ~ 7035,
    municipality == "Huehuetan" ~ 7037,
    municipality == "Huitiupan" ~ 7039,
    municipality == "Huixtan" ~ 7038,
    municipality == "Huixtla" ~ 7040,
    municipality == "Ixhuatan" | municipality == "Ixhuat\xe1n" ~ 7042,
    municipality == "Ixtacomitan" | municipality == "Ixtacomit\xe1n" ~ 7043,
    municipality == "Ixtapa" ~ 7044,
    municipality == "Ixtapangajoya" ~ 7045,
    municipality == "Jiquipilas" ~ 7046,
    municipality == "Jitotol" ~ 7047,
    municipality == "Juarez" | municipality == "Ju\xe1rez" ~ 7048,
    municipality == "La Concordia" ~ 7020,
    municipality == "La Grandeza" ~ 7036,
    municipality == "La Independencia" ~ 7041,
    municipality == "La Libertad" ~ 7050,
    municipality == "La Trinitaria" ~ 7099,
    municipality == "Larrainzar" ~ 7049,
    municipality == "Las Margaritas" ~ 7052,
    municipality == "Las Rosas" ~ 7075,
    municipality == "Mapastepec" ~ 7051,
    municipality == "Maravilla Tenejapa" ~ 7115,
    municipality == "Marques De Comillas" | municipality == "Marqu\xe9s de Comillas" ~ 7116,
    municipality == "Mazapa De Madero" | municipality == "Mazapa de Madero" ~ 7053,
    municipality == "Mazatan" | municipality == "Mazat\xe1n" ~ 7054,
    municipality == "Metapa De Dominguez" | municipality == "Metapa de Dom\xednguez" ~ 7055,
    municipality == "Mezcalapa" ~ 7123,
    municipality == "Mitontic" ~ 7056,
    municipality == "Montecristo De Guerrero" | municipality == "Montecristo de Guerrero" ~ 7117,
    municipality == "Motozintla" ~ 7057,
    municipality == "Nicolas Ruiz" | municipality == "Nicolas Ru\xedz" ~ 7058,
    municipality == "Ocosingo" ~ 7059,
    municipality == "Ocotepec" ~ 7060,
    municipality == "Ocozocoautla De Espinosa" | municipality == "Ocozocuautla de Espinoza" | municipality == "Ocozocuautla De Espinoza" ~ 7061,
    municipality == "Ostuacan" | municipality == "Ostuac\xe1n" ~ 7062,
    municipality == "Osumacinta" ~ 7063,
    municipality == "Oxchuc" ~ 7064,
    municipality == "Palenque" ~ 7065,
    municipality == "Pantelho" ~ 7066,
    municipality == "Pantepec" ~ 7067,
    municipality == "Pichucalco" ~ 7068,
    municipality == "Pijijiapan" ~ 7069,
    municipality == "Pueblo Nuevo Solistahuacan" | municipality == "Pueblo Nuevo Solistauacan" | municipality == "Pueblo Nuevo Solistauac\xe1n" ~ 7072,
    municipality == "Rayon" | municipality == "Ray\xf3n" ~ 7073,
    municipality == "Reforma" ~ 7074,
    municipality == "Sabanilla" ~ 7076,
    municipality == "Salto De Agua" | municipality == "Salto de Agua" ~ 7077,
    municipality == "San Andres Duraznal" | municipality == "San Andr\xe9s Duraznal" ~ 7118,
    municipality == "San Cristobal De Las Casas" | municipality == "San Cristobal de las casas" ~ 7078,
    municipality == "San Fernando" ~ 7079,
    municipality == "San Juan Cancuc" ~ 7112,
    municipality == "San Lucas" ~ 7110,
    municipality == "Santiago El Pinar" | municipality == "Santiago el Pinar" ~ 7119,
    municipality == "Siltepec" ~ 7080,
    municipality == "Simojovel" ~ 7081,
    municipality == "Sitala" | municipality == "Sital\xe1" ~ 7082,
    municipality == "Socoltenango" ~ 7083,
    municipality == "Solosuchiapa" ~ 7084,
    municipality == "Soyalo" | municipality == "Soyal\xf3" ~ 7085,
    municipality == "Suchiapa" ~ 7086,
    municipality == "Suchiate" ~ 7087,
    municipality == "Sunuapa" ~ 7088,
    municipality == "Tapachula" ~ 7089,
    municipality == "Tapalapa" ~ 7090,
    municipality == "Tapilula" ~ 7091,
    municipality == "Tecpatan" ~ 7092,
    municipality == "Tenejapa" ~ 7093,
    municipality == "Teopisca" ~ 7094,
    municipality == "Tila" ~ 7096,
    municipality == "Tonala" | municipality == "Tonala" ~ 7097,
    municipality == "Totolapa" ~ 7098,
    municipality == "Tumbala" | municipality == "Tumbal\xe1" ~ 7100,
    municipality == "Tuxtla Chico" ~ 7102,
    municipality == "Tuxtla Gutierrez" | municipality == "Tuxtla Guti\xe9rrez" ~ 7101,
    municipality == "Tuzantan" ~ 7103,
    municipality == "Tzimol" ~ 7104,
    municipality == "Union Juarez" | municipality == "Uni\xf3n Ju\xe1rez" ~ 7105,
    municipality == "Venustiano Carranza" ~ 7106,
    municipality == "Villa Comaltitlan" | municipality == "Villacomaltitlan" ~ 7071,
    municipality == "Villa Corzo" ~ 7107,
    municipality == "Villaflores" ~ 7108,
    municipality == "Yajalon" | municipality == "Yajal\xf3n" ~ 7109,
    municipality == "Zinacantan" | municipality == "Zinacant\xe1n" ~ 7111,
    TRUE ~ NA_real_  # Default case for unmatched municipalities
  ))

# Load and merge Lista Nominal data
ln_all_months_years <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

ln_all_months_years <- ln_all_months_years %>%
  dplyr::filter(month == "June" & year == 2010 & state == "CHIAPAS") %>%
  dplyr::select(section, lista)

collapsed_2010 <- collapsed_2010 %>%
  dplyr::left_join(ln_all_months_years, by = c("section"))

collapsed_2010 <- collapsed_2010 %>%
  dplyr::rename(listanominal=lista)

collapsed_2010 <- collapsed_2010 %>%
  dplyr::mutate(turnout = total / listanominal) %>%
  dplyr::mutate(valid = rowSums(across(c(PRI:PAN_PRI_PRD_PT_PVEM_PC_PANAL_PSD,"NO REGISTRADOS")), na.rm = TRUE)) %>% 
  dplyr::select(-c("NO REGISTRADOS"))

collapsed_2010 <- collapsed_2010 %>%
  dplyr::mutate(month = "June",
                year = 2010)

rm(data_2010)
summary(collapsed_2010)
#####################################
### PROCESSING DATA FOR 2012
#####################################

data_2012 <- read_excel("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/2012/Ayu_Seccion_No_LN_2012.xlsx") %>%
  dplyr::rename(municipality = Municipio, 
                section = Seccion) %>%
  dplyr::mutate(section = as.numeric(section),
                PAN = as.numeric(PAN),
                PRI = as.numeric(PRI),
                PRD_PT_PC = as.numeric(PRD_PT_PC),
                PVEM = as.numeric(PVEM),
                PANAL = as.numeric(PANAL),
                POP = as.numeric(POP),
                total = rowSums(across(PAN:Nulos), na.rm = TRUE)) %>%
  dplyr::filter(!is.na(total), total != 0) %>%
  dplyr::filter(municipality != "" & !is.na(section))

# Replace special characters in the municipality column
data_2012 <- data_2012 %>%
  dplyr::mutate(municipality = gsub("Á", "A", municipality),
         municipality = gsub("É", "E", municipality),
         municipality = gsub("Í", "I", municipality),
         municipality = gsub("Ó", "O", municipality),
         municipality = gsub("Ú", "U", municipality),
         municipality = gsub("Ü", "U", municipality),
         municipality = gsub("Ñ", "N", municipality),
         municipality = tools::toTitleCase(municipality))

collapsed_2012 <- data_2012 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(PAN:total, sum, na.rm = TRUE)) %>%
  dplyr::rename(nulos = Nulos) 

collapsed_2012 <- collapsed_2012 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN:"No Registrados")), na.rm = TRUE)) %>%
  dplyr::mutate(year = 2012, month = "October")%>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ACALA" ~ 7002,
      municipality == "ALDAMA" ~ 7113,
      municipality == "AMATENANGO DE LA FRONTERA" ~ 7006,
      municipality == "AMAT\xc1N" ~ 7005,
      municipality == "ACACOYAGUA" ~ 7001,
      municipality == "ACAPETAHUA" ~ 7003,
      municipality == "ALTAMIRANO" ~ 7004,
      municipality == "AMATAN" ~ 7005,
      municipality == "AMATENANGO DE LA FRA." ~ 7006,
      municipality == "AMATENANGO DEL VALLE" ~ 7007,
      municipality == "\xc1NGEL ALBINO CORZO" ~ 7008,
      municipality == "ARRIAGA" ~ 7009,
      municipality == "BEJUCAL DE OCAMPO" ~ 7010,
      municipality == "BELLAVISTA" ~ 7011,
      municipality == "BERRIOZ\xc1BAL" ~ 7012,
      municipality == "BOCHIL" ~ 7013,
      municipality == "BOSQUE, EL" ~ 7014,
      municipality == "CACAHOAT\xc1N" ~ 7015,
      municipality == "CATAZAJ\xc1" ~ 7016,
      municipality == "CHALCHIHUIT\xc1N" ~ 7022,
      municipality == "CHANAL" ~ 7024,
      municipality == "CHAPULTENANGO" ~ 7025,
      municipality == "CHENALH\xd3" ~ 7026,
      municipality == "CHIAPA DE CORZO" ~ 7027,
      municipality == "CHIAPILLA" ~ 7028,
      municipality == "CHICOAS\xc9N" ~ 7029,
      municipality == "CHICOMUSELO" ~ 7030,
      municipality == "CHIL\xd3N" ~ 7031,
      municipality == "CINTALAPA" ~ 7017,
      municipality == "COAPILLA" ~ 7018,
      municipality == "COMIT\xc1N" ~ 7019,
      municipality == "CONCORDIA, LA" ~ 7020,
      municipality == "COPAINAL\xc1" ~ 7021,
      municipality == "FRANCISCO LE\xd3N" ~ 7033,
      municipality == "FRONTERA COMALAPA" ~ 7034,
      municipality == "FRONTERA HIDALGO" ~ 7035,
      municipality == "GRANDEZA, LA" ~ 7036,
      municipality == "HUEHUET\xc1N" ~ 7037,
      municipality == "HUITIUP\xc1N" ~ 7039,
      municipality == "HUIXTLA" ~ 7040,
      municipality == "HUIXT\xc1N" ~ 7038,
      municipality == "INDEPENDENCIA, LA" ~ 7041,
      municipality == "IXHUAT\xc1N" ~ 7042,
      municipality == "IXTACOMIT\xc1N" ~ 7043,
      municipality == "IXTAPA" ~ 7044,
      municipality == "IXTAPANGAJOYA" ~ 7045,
      municipality == "JIQUIPILAS" ~ 7046,
      municipality == "JITOTOL" ~ 7047,
      municipality == "JU\xc1REZ" ~ 7048,
      municipality == "LARRA\xcdNZAR" ~ 7049,
      municipality == "LIBERTAD, LA" ~ 7050,
      municipality == "MARGARITAS, LAS" ~ 7052,
      municipality == "MAZAPA DE MADERO" ~ 7053,
      municipality == "MAZAT\xc1N" ~ 7054,
      municipality == "METAPA DE DOM\xcdNGUEZ" ~ 7055,
      municipality == "MITONTIC" ~ 7056,
      municipality == "N CRISTOBAL DE LAS CAS" ~ 7078,
      municipality == "NICOL\xc1S RUIZ" ~ 7058,
      municipality == "OCOSINGO" ~ 7059,
      municipality == "OCOTEPEC" ~ 7060,
      municipality == "OCOZOCOAUTLA" ~ 7061,
      municipality == "OSTUAC\xc1N" ~ 7062,
      municipality == "OSUMACINTA" ~ 7063,
      municipality == "OXCHUC" ~ 7064,
      municipality == "PALENQUE" ~ 7065,
      municipality == "PANTELH\xd3" ~ 7066,
      municipality == "PANTEPEC" ~ 7067,
      municipality == "PICHUCALCO" ~ 7068,
      municipality == "PORVENIR, EL" ~ 7070,
      municipality == "RAY\xd3N" ~ 7073,
      municipality == "REFORMA" ~ 7074,
      municipality == "ROSAS, LAS" ~ 7075,
      municipality == "SABANILLA" ~ 7076,
      municipality == "Salto de Agua" ~ 7077,
      municipality == "SAN FERNANDO" ~ 7079,
      municipality == "SAN JUAN CANCUC" ~ 7112,
      municipality == "SAN LUCAS" ~ 7110,
      municipality == "SIMOJOVEL" ~ 7081,
      municipality == "SITAL\xc1" ~ 7082,
      municipality == "SOCOLTENANGO" ~ 7083,
      municipality == "SOLOSUCHIAPA" ~ 7084,
      municipality == "SOYAL\xd3" ~ 7085,
      municipality == "SUCHIAPA" ~ 7086,
      municipality == "SUCHIATE" ~ 7087,
      municipality == "SUNUAPA" ~ 7088,
      municipality == "TAPACHULA" ~ 7089,
      municipality == "TAPALAPA" ~ 7090,
      municipality == "TAPILULA" ~ 7091,
      municipality == "TECPAT\xc1N" ~ 7092,
      municipality == "TENEJAPA" ~ 7093,
      municipality == "TEOPISCA" ~ 7094,
      municipality == "TILA" ~ 7096,
      municipality == "TONALA" ~ 7097,
      municipality == "TOTOLAPA" ~ 7098,
      municipality == "TRINITARIA, LA" ~ 7099,
      municipality == "TUMBALA" ~ 7100,
      municipality == "TUXTLA CHICO" ~ 7102,
      municipality == "TUXTLA GUTI\xc9RREZ" ~ 7101,
      municipality == "TUZANT\xc1N" ~ 7103,
      municipality == "TZIMOL" ~ 7104,
      municipality == "UNI\xd3N JU\xc1REZ" ~ 7105,
      municipality == "VILLA CORZO" ~ 7107,
      municipality == "VILLAFLORES" ~ 7108,
      municipality == "YAJAL\xd3N" ~ 7109,
      municipality == "ZINACANT\xc1N" ~ 7111,
      municipality == "BENEM\xc9RITO DE LAS AM\xc9RICAS" ~ 7114, 
      municipality == "CHAMULA" ~ 7023,
      municipality == "EL BOSQUE" ~ 7014,
      municipality == "EL PORVENIR" ~ 7070,
      municipality == "ESCUINTLA" ~ 7032,
      municipality == "LA CONCORDIA" ~ 7020,
      municipality == "LA GRANDEZA" ~ 7036,
      municipality == "LA INDEPENDENCIA" ~ 7041,
      municipality == "LA LIBERTAD" ~ 7050,
      municipality == "LA TRINITARIA" ~ 7099,
      municipality == "LAS MARGARITAS" ~ 7052,
      municipality == "LAS ROSAS" ~ 7075,
      municipality == "MAPASTEPEC" ~ 7051,
      municipality == "MARAVILLA TENEJAPA" ~ 7115,
      municipality == "MARQU\xc9S DE COMILLAS" ~ 7116,
      municipality == "MONTECRISTO DE GUERRERO" ~ 7117,
      municipality == "MOTOZINTLA" ~ 7057,
      municipality == "PIJIJIAPAN" ~ 7069,
      municipality == "PUEBLO NVO. SOLISTAHUAC\xc1N" ~ 7072,
      municipality == "SALTO DE AGUA" ~ 7077,
      municipality == "SAN ANDR\xc9S DURAZNAL" ~ 7118,
      municipality == "SAN CRIST\xd3BAL DE LAS CASAS" ~ 7078,
      municipality == "SANTIAGO EL PINAR" ~ 7119,
      municipality == "SILTEPEC" ~ 7080,
      municipality == "TONAL\xc1" ~ 7097,
      municipality == "TUMBAL\xc1" ~ 7100,
      municipality == "VENUSTIANO CARRANZA" ~ 7106,
      municipality == "VILLA COMALTITL\xc1N" ~ 7071,
      municipality == "ANGEL ALBINO CORZO" ~ 7008,
      municipality == "BELLA VISTA" ~ 7011,
      municipality == "BENEMERITO DE LAS AMERICAS" ~ 7114,
      municipality == "BERRIOZABAL" ~ 7012,
      municipality == "CACAHOATAN" ~ 7015,
      municipality == "CATAZAJA" ~ 7016,
      municipality == "CHALCHIHUITAN" ~ 7022,
      municipality == "CHENALHO" ~ 7026,
      municipality == "CHICOASEN" ~ 7029,
      municipality == "CHILON" ~ 7031,
      municipality == "COMITAN DE DOMINGUEZ" ~ 7019,
      municipality == "COPAINALA" ~ 7021,
      municipality == "FRANCISCO LEON" ~ 7033,
      municipality == "HUEHUETAN" ~ 7037,
      municipality == "HUITIUPAN" ~ 7039,
      municipality == "HUIXTAN" ~ 7038,
      municipality == "IXHUATAN" ~ 7042,
      municipality == "IXTACOMITAN" ~ 7043,
      municipality == "JUAREZ" ~ 7048,
      municipality == "LARRAINZAR" ~ 7049,
      municipality == "MARQUES DE COMILLAS" ~ 7116,
      municipality == "MAZATAN" ~ 7054,
      municipality == "METAPA DE DOMINGUEZ" ~ 7055,
      municipality == "NICOLAS RUIZ" ~ 7058,
      municipality == "OCOZOCOAUTLA DE ESPINOSA" ~ 7061,
      municipality == "OSTUACAN" ~ 7062,
      municipality == "PANTELHO" ~ 7066,
      municipality == "PUEBLO NVO SOLISTAHUACAN" ~ 7072,
      municipality == "RAYON" ~ 7073,
      municipality == "SAN ANDRES DURAZNAL" ~ 7118,
      municipality == "SAN CRISTOBAL DE LAS CASAS" ~ 7078,
      municipality == "SITALA" ~ 7082,
      municipality == "SOYALO" ~ 7085,
      municipality == "TECPATAN" ~ 7092,
      municipality == "TUXTLA GUTIERREZ" ~ 7101,
      municipality == "TUZANTAN" ~ 7103,
      municipality == "UNION JUAREZ" ~ 7105,
      municipality == "VILLACOMALTITLAN" ~ 7071,
      municipality == "YAJALON" ~ 7109,
      municipality == "ZINACANTAN" ~ 7111,
      municipality == "AMILIANO ZAPATA" ~ 7121,
      municipality == "MEZCALAPA" ~ 7123,
      municipality == "PUEBLO NUEVO SOLISTAHUACAN" ~ 7072,
      municipality == "VILLA COMALTITLAN" ~ 7071,
      municipality == "EL PARRAL" ~ 7122,
      TRUE ~ NA_real_  # Default case for municipalities that are not in the list
    ))

# Load and merge Lista Nominal data
ln_2012 <- read_excel("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/2012/LN_2012.xlsx")

ln_2012 <- ln_2012 %>%
  dplyr::mutate(section = as.numeric(Secc),
                Total_LN = as.numeric(Total_LN)) %>% 
  dplyr::group_by(section) %>% 
  dplyr::summarise(listanominal = sum(Total_LN, na.rm = TRUE))

collapsed_2012 <- collapsed_2012 %>%
  dplyr::left_join(ln_2012, by = c("section"))

# Adjust PRI, PVEM, and POP for specific municipalities
collapsed_2012 <- collapsed_2012 %>%
  dplyr::mutate(
    PRI_PVEM_POP = ifelse(municipality == "TUXTLA GUTIERREZ", rowSums(across(c(PRI , PVEM , POP)), na.rm = TRUE),NA),
    PRI = ifelse(municipality == "TUXTLA GUTIERREZ", 0, PRI),
    PVEM = ifelse(municipality == "TUXTLA GUTIERREZ", 0, PVEM),
    POP = ifelse(municipality == "TUXTLA GUTIERREZ", 0, POP))

# Combine PRI and POP for specified municipalities and set to zero in the original columns
pri_pop_munis <- c("ARRIAGA", "BELLA VISTA", "CATAZAJA", "COMITAN DE DOMINGUEZ", "CHICOMUSELO", 
                   "FRONTERA COMALAPA", "LA INDEPENDENCIA", "LA TRINITARIA", "LAS MARGARITAS", 
                   "MOTOZINTLA", "OCOZOCOAUTLA DE ESPINOSA", "PALENQUE", "PIJIJIAPAN", 
                   "SOCOLTENANGO", "TZIMOL", "VENUSTIANO CARRANZA", "VILLAFLORES")

collapsed_2012 <- collapsed_2012 %>%
  dplyr::mutate(
    PRI_POP = ifelse(municipality %in% pri_pop_munis, rowSums(across(c(PRI,POP)), na.rm = TRUE),NA),
    PRI = ifelse(municipality %in% pri_pop_munis, 0, PRI),
    POP = ifelse(municipality %in% pri_pop_munis, 0, POP))

# PVEM_POP municipalities
pvem_pop_munis <- c("TEOPISCA", "PUEBLO NUEVO SOLISTAHUACAN", "SALTO DE AGUA")

collapsed_2012 <- collapsed_2012 %>%
  dplyr::mutate(
    PVEM_POP = ifelse(municipality %in% pvem_pop_munis, rowSums(across(c(PVEM , POP)), na.rm = TRUE),NA),
    PVEM = ifelse(municipality %in% pvem_pop_munis, 0, PVEM),
    POP = ifelse(municipality %in% pvem_pop_munis, 0, POP))

# Adjust PAN_POP for specific municipalities
pan_pop_munis <- c("ACACOYAGUA", "ACALA", "ACAPETAHUA", "ALDAMA", "AMATAN", "ANGEL ALBINO CORZO", 
                   "BEJUCAL DE OCAMPO", "BELISARIO DOMINGUEZ", "BOCHIL", "LA CONCORDIA", 
                   "CHALCHIHUITAN", "CHAMULA", "CHANAL", "CHAPULTENANGO", "CHENALHO", 
                   "CHIAPA DE CORZO", "CHIAPILLA", "FRONTERA HIDALGO", "HUEHUETAN", 
                   "HUIXTAN", "HUIXTLA", "IXHUATAN", "IXTACOMITAN", "IXTAPA", "IXTAPANGOJOYA", 
                   "JITOTOL", "LARRAINZAR", "MAZATAN", "MONTECRISTO DE GUERRERO", "MITONTIC", 
                   "OCOTEPEC", "OSTUACAN", "OXCHUC", "PANTELHO", "EL PARRAL", "RAYON", "REFORMA", 
                   "SAN ANDRES DURAZNAL", "SAN CRISTOBAL DE LAS CASAS", "SAN JUAN CANCUC", 
                   "SIMOJOVEL", "SOLOSUCHIAPA", "SOYALO", "SUCHIATE", "SUNUAPA", "TAPACHULA", 
                   "TAPALAPA", "TAPILULA", "TECPTAN", "TENEJAPA", "TUZANTAN", "VILLA COMALTITLAN", 
                   "VILLA CORZO", "ZINACANTAN")

collapsed_2012 <- collapsed_2012 %>%
  dplyr::mutate(
    PAN_POP = ifelse(municipality %in% pan_pop_munis, rowSums(across(c(PAN , POP)), na.rm = TRUE),NA),
    PAN = ifelse(municipality %in% pan_pop_munis, 0, PAN),
    POP = ifelse(municipality %in% pan_pop_munis, 0, POP))

# Calculate turnout and valid votes
collapsed_2012 <- collapsed_2012 %>%
  dplyr::mutate(turnout = total / listanominal,
                valid = rowSums(across(c(PAN:POP,PRI_PVEM_POP,
                                         PRI_POP,PAN_POP,
                                         PVEM_POP)), na.rm = TRUE))
rm(ln_2012)
rm(data_2012)
View(collapsed_2012 %>% dplyr::filter(is.na(uniqueid)))
#####################################
### PROCESSING DATA FOR 2015
#####################################

# Load the dataset from the specific Excel file
data_2015 <- read_excel("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/01_RESULTADOS_ELECTORALES_CASILLA_2015_AYUNTAMIENTOS_MODXTEECHYTEPJF.xlsx", sheet = "Table 1")

# Drop rows for the dissolved government
data_2015 <- data_2015 %>%
  dplyr::filter(MUNICIPIO != "BELISARIO DOMINGUEZ" & MUNICIPIO != "BELISARIO DOMÍNGUEZ")

data_2015 <- data_2015%>%
  dplyr::rename(municipality = MUNICIPIO, 
                section = "SECCIÓN") %>%
  dplyr::mutate(total = rowSums(across(c(PAN:"C I","NO REG","VOTOS NULOS")), na.rm = TRUE)) %>%
  dplyr::filter(!is.na(total), total != 0) %>%
  dplyr::filter(municipality != "" & !is.na(section))

# Create municipality column in proper case
data_2015 <- data_2015 %>%
  mutate(municipality = tools::toTitleCase(municipality))

# Drop and generate uniqueid column
data_2015$uniqueid <- NA

# Replace special characters in the municipality column
data_2015 <- data_2015 %>%
  dplyr::mutate(municipality = gsub("Á", "A", municipality),
                municipality = gsub("É", "E", municipality),
                municipality = gsub("Í", "I", municipality),
                municipality = gsub("Ó", "O", municipality),
                municipality = gsub("Ú", "U", municipality),
                municipality = gsub("Ü", "U", municipality),
                municipality = gsub("Ñ", "N", municipality),
                municipality = tools::toTitleCase(municipality))

summary(data_2015)

data_2015 <- data_2015 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN:"C I","NO REG")), na.rm = TRUE))

# Manually assign unique IDs based on municipality names
data_2015 <- data_2015 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ACALA" ~ 7002,
      municipality == "ALDAMA" ~ 7113,
      municipality == "AMATENANGO DE LA FRONTERA" ~ 7006,
      municipality == "AMAT\xc1N" ~ 7005,
      municipality == "ACACOYAGUA" ~ 7001,
      municipality == "ACAPETAHUA" ~ 7003,
      municipality == "ALTAMIRANO" ~ 7004,
      municipality == "AMATAN" ~ 7005,
      municipality == "AMATENANGO DE LA FRA." ~ 7006,
      municipality == "AMATENANGO DEL VALLE" ~ 7007,
      municipality == "\xc1NGEL ALBINO CORZO" ~ 7008,
      municipality == "ARRIAGA" ~ 7009,
      municipality == "BEJUCAL DE OCAMPO" ~ 7010,
      municipality == "BELLAVISTA" ~ 7011,
      municipality == "BERRIOZ\xc1BAL" ~ 7012,
      municipality == "BOCHIL" ~ 7013,
      municipality == "BOSQUE, EL" ~ 7014,
      municipality == "CACAHOAT\xc1N" ~ 7015,
      municipality == "CATAZAJ\xc1" ~ 7016,
      municipality == "CHALCHIHUIT\xc1N" ~ 7022,
      municipality == "CHANAL" ~ 7024,
      municipality == "CHAPULTENANGO" ~ 7025,
      municipality == "CHENALH\xd3" ~ 7026,
      municipality == "CHIAPA DE CORZO" ~ 7027,
      municipality == "CHIAPILLA" ~ 7028,
      municipality == "CHICOAS\xc9N" ~ 7029,
      municipality == "CHICOMUSELO" ~ 7030,
      municipality == "CHIL\xd3N" ~ 7031,
      municipality == "CINTALAPA" ~ 7017,
      municipality == "COAPILLA" ~ 7018,
      municipality == "COMIT\xc1N" ~ 7019,
      municipality == "CONCORDIA, LA" ~ 7020,
      municipality == "COPAINAL\xc1" ~ 7021,
      municipality == "FRANCISCO LE\xd3N" ~ 7033,
      municipality == "FRONTERA COMALAPA" ~ 7034,
      municipality == "FRONTERA HIDALGO" ~ 7035,
      municipality == "GRANDEZA, LA" ~ 7036,
      municipality == "HUEHUET\xc1N" ~ 7037,
      municipality == "HUITIUP\xc1N" ~ 7039,
      municipality == "HUIXTLA" ~ 7040,
      municipality == "HUIXT\xc1N" ~ 7038,
      municipality == "INDEPENDENCIA, LA" ~ 7041,
      municipality == "IXHUAT\xc1N" ~ 7042,
      municipality == "IXTACOMIT\xc1N" ~ 7043,
      municipality == "IXTAPA" ~ 7044,
      municipality == "IXTAPANGAJOYA" ~ 7045,
      municipality == "JIQUIPILAS" ~ 7046,
      municipality == "JITOTOL" ~ 7047,
      municipality == "JU\xc1REZ" ~ 7048,
      municipality == "LARRA\xcdNZAR" ~ 7049,
      municipality == "LIBERTAD, LA" ~ 7050,
      municipality == "MARGARITAS, LAS" ~ 7052,
      municipality == "MAZAPA DE MADERO" ~ 7053,
      municipality == "MAZAT\xc1N" ~ 7054,
      municipality == "METAPA DE DOM\xcdNGUEZ" ~ 7055,
      municipality == "MITONTIC" ~ 7056,
      municipality == "N CRISTOBAL DE LAS CAS" ~ 7078,
      municipality == "NICOL\xc1S RUIZ" ~ 7058,
      municipality == "OCOSINGO" ~ 7059,
      municipality == "OCOTEPEC" ~ 7060,
      municipality == "OCOZOCOAUTLA" ~ 7061,
      municipality == "OSTUAC\xc1N" ~ 7062,
      municipality == "OSUMACINTA" ~ 7063,
      municipality == "OXCHUC" ~ 7064,
      municipality == "PALENQUE" ~ 7065,
      municipality == "PANTELH\xd3" ~ 7066,
      municipality == "PANTEPEC" ~ 7067,
      municipality == "PICHUCALCO" ~ 7068,
      municipality == "PORVENIR, EL" ~ 7070,
      municipality == "RAY\xd3N" ~ 7073,
      municipality == "REFORMA" ~ 7074,
      municipality == "ROSAS, LAS" ~ 7075,
      municipality == "SABANILLA" ~ 7076,
      municipality == "Salto de Agua" ~ 7077,
      municipality == "SAN FERNANDO" ~ 7079,
      municipality == "SAN JUAN CANCUC" ~ 7112,
      municipality == "SAN LUCAS" ~ 7110,
      municipality == "SIMOJOVEL" ~ 7081,
      municipality == "SITAL\xc1" ~ 7082,
      municipality == "SOCOLTENANGO" ~ 7083,
      municipality == "SOLOSUCHIAPA" ~ 7084,
      municipality == "SOYAL\xd3" ~ 7085,
      municipality == "SUCHIAPA" ~ 7086,
      municipality == "SUCHIATE" ~ 7087,
      municipality == "SUNUAPA" ~ 7088,
      municipality == "TAPACHULA" ~ 7089,
      municipality == "TAPALAPA" ~ 7090,
      municipality == "TAPILULA" ~ 7091,
      municipality == "TECPAT\xc1N" ~ 7092,
      municipality == "TENEJAPA" ~ 7093,
      municipality == "TEOPISCA" ~ 7094,
      municipality == "TILA" ~ 7096,
      municipality == "TONALA" ~ 7097,
      municipality == "TOTOLAPA" ~ 7098,
      municipality == "TRINITARIA, LA" ~ 7099,
      municipality == "TUMBALA" ~ 7100,
      municipality == "TUXTLA CHICO" ~ 7102,
      municipality == "TUXTLA GUTI\xc9RREZ" ~ 7101,
      municipality == "TUZANT\xc1N" ~ 7103,
      municipality == "TZIMOL" ~ 7104,
      municipality == "UNI\xd3N JU\xc1REZ" ~ 7105,
      municipality == "VILLA CORZO" ~ 7107,
      municipality == "VILLAFLORES" ~ 7108,
      municipality == "YAJAL\xd3N" ~ 7109,
      municipality == "ZINACANT\xc1N" ~ 7111,
      municipality == "BENEM\xc9RITO DE LAS AM\xc9RICAS" ~ 7114, 
      municipality == "CHAMULA" ~ 7023,
      municipality == "EL BOSQUE" ~ 7014,
      municipality == "EL PORVENIR" ~ 7070,
      municipality == "ESCUINTLA" ~ 7032,
      municipality == "LA CONCORDIA" ~ 7020,
      municipality == "LA GRANDEZA" ~ 7036,
      municipality == "LA INDEPENDENCIA" ~ 7041,
      municipality == "LA LIBERTAD" ~ 7050,
      municipality == "LA TRINITARIA" ~ 7099,
      municipality == "LAS MARGARITAS" ~ 7052,
      municipality == "LAS ROSAS" ~ 7075,
      municipality == "MAPASTEPEC" ~ 7051,
      municipality == "MARAVILLA TENEJAPA" ~ 7115,
      municipality == "MARQU\xc9S DE COMILLAS" ~ 7116,
      municipality == "MONTECRISTO DE GUERRERO" ~ 7117,
      municipality == "MOTOZINTLA" ~ 7057,
      municipality == "PIJIJIAPAN" ~ 7069,
      municipality == "PUEBLO NVO. SOLISTAHUAC\xc1N" ~ 7072,
      municipality == "SALTO DE AGUA" ~ 7077,
      municipality == "SAN ANDR\xc9S DURAZNAL" ~ 7118,
      municipality == "SAN CRIST\xd3BAL DE LAS CASAS" ~ 7078,
      municipality == "SANTIAGO EL PINAR" ~ 7119,
      municipality == "SILTEPEC" ~ 7080,
      municipality == "TONAL\xc1" ~ 7097,
      municipality == "TUMBAL\xc1" ~ 7100,
      municipality == "VENUSTIANO CARRANZA" ~ 7106,
      municipality == "VILLA COMALTITL\xc1N" ~ 7071,
      municipality == "ANGEL ALBINO CORZO" ~ 7008,
      municipality == "BELLA VISTA" ~ 7011,
      municipality == "BENEMERITO DE LAS AMERICAS" ~ 7114,
      municipality == "BERRIOZABAL" ~ 7012,
      municipality == "CACAHOATAN" ~ 7015,
      municipality == "CATAZAJA" ~ 7016,
      municipality == "CHALCHIHUITAN" ~ 7022,
      municipality == "CHENALHO" ~ 7026,
      municipality == "CHICOASEN" ~ 7029,
      municipality == "CHILON" ~ 7031,
      municipality == "COMITAN DE DOMINGUEZ" ~ 7019,
      municipality == "COPAINALA" ~ 7021,
      municipality == "FRANCISCO LEON" ~ 7033,
      municipality == "HUEHUETAN" ~ 7037,
      municipality == "HUITIUPAN" ~ 7039,
      municipality == "HUIXTAN" ~ 7038,
      municipality == "IXHUATAN" ~ 7042,
      municipality == "IXTACOMITAN" ~ 7043,
      municipality == "JUAREZ" ~ 7048,
      municipality == "LARRAINZAR" ~ 7049,
      municipality == "MARQUES DE COMILLAS" ~ 7116,
      municipality == "MAZATAN" ~ 7054,
      municipality == "METAPA DE DOMINGUEZ" ~ 7055,
      municipality == "NICOLAS RUIZ" ~ 7058,
      municipality == "OCOZOCOAUTLA DE ESPINOSA" ~ 7061,
      municipality == "OSTUACAN" ~ 7062,
      municipality == "PANTELHO" ~ 7066,
      municipality == "PUEBLO NVO SOLISTAHUACAN" ~ 7072,
      municipality == "RAYON" ~ 7073,
      municipality == "SAN ANDRES DURAZNAL" ~ 7118,
      municipality == "SAN CRISTOBAL DE LAS CASAS" ~ 7078,
      municipality == "SITALA" ~ 7082,
      municipality == "SOYALO" ~ 7085,
      municipality == "TAPILULA" | municipality == "TAPILULA EXTRAORDINARIO"~ 7091,
      municipality == "TECPATAN" ~ 7092,
      municipality == "TUXTLA GUTIERREZ" ~ 7101,
      municipality == "TUZANTAN" ~ 7103,
      municipality == "UNION JUAREZ" ~ 7105,
      municipality == "VILLACOMALTITLAN" ~ 7071,
      municipality == "YAJALON" ~ 7109,
      municipality == "ZINACANTAN" ~ 7111,
      municipality == "AMILIANO ZAPATA" | municipality == "EMILIANO ZAPATA"~ 7121,
      municipality == "MEZCALAPA" ~ 7123,
      municipality == "PUEBLO NUEVO SOLISTAHUACAN" ~ 7072,
      municipality == "VILLA COMALTITLAN" ~ 7071,
      municipality == "EL PARRAL" ~ 7122,
      TRUE ~ NA_real_  # Default case for municipalities that are not in the list
    ))

# Generate the coalition variable for TUXTLA GUTIÉRREZ

data_2015 <- data_2015 %>%
  dplyr::mutate(PRI_PVEM_PANAL_PCU = ifelse(municipality == "TUXTLA GUTIERREZ",
                                            rowSums(across(c(PRI , PVEM , PCU , `NA`)), na.rm = TRUE),NA),
                PCU = ifelse(municipality == "TUXTLA GUTIERREZ", 0, PCU),
                PRI = ifelse(municipality == "TUXTLA GUTIERREZ", 0, PRI),
                PVEM = ifelse(municipality == "TUXTLA GUTIERREZ", 0, PVEM),
                `NA` = ifelse(municipality == "TUXTLA GUTIERREZ", 0, `NA`))

# Generate PRI_PVEM_PANAL for TAPACHULA
data_2015 <- data_2015 %>%
  dplyr::mutate(PRI_PVEM_PANAL = ifelse(municipality == "TAPACHULA", 
                                        rowSums(across(c(PRI , PVEM , `NA`)), na.rm = TRUE),NA),
         PRI = if_else(municipality == "TAPACHULA", 0, PRI),
         PVEM = if_else(municipality == "TAPACHULA", 0, PVEM),
         `NA` = if_else(municipality == "TAPACHULA", 0, `NA`))

# Generate PVEM_PANAL for municipalities that are not TAPACHULA or TUXTLA GUTIÉRREZ
data_2015 <- data_2015 %>%
  dplyr::mutate(PVEM_PANAL = ifelse(municipality != "TAPACHULA" & municipality != "TUXTLA GUTIÉRREZ", 
                                    rowSums(across(c(PVEM , `NA`)), na.rm = TRUE),NA),
         PVEM = ifelse(municipality != "TAPACHULA" & municipality != "TUXTLA GUTIÉRREZ" , 0, PVEM),
         `NA` = ifelse(municipality != "TAPACHULA" & municipality != "TUXTLA GUTIÉRREZ", 0, `NA`))
names(data_2015)
# Generate PT_MC for municipalities
table(data_2015$`PT MC`)

data_2015 <- data_2015 %>%
  dplyr::mutate(PT_MC = ifelse(`PT MC` == TRUE, rowSums(across(c(PT , MC)), na.rm = TRUE),NA))

# Set PT and MC to missing if PTMC is not missing
data_2015 <- data_2015 %>%
  dplyr::mutate(PT = ifelse(!is.na(PT_MC), NA, PT),
                MC = ifelse(!is.na(PT_MC), NA, MC))

# Rename NA and MOR to PANAL and MORENA

data_2015 <- data_2015 %>%
  dplyr::rename(PANAL = `NA`, 
                MORENA = MOR)
names(data_2015)

# Collapse the data by municipality and section, summing the variables
collapsed_2015 <- data_2015 %>%
  dplyr::rename(nulos = "VOTOS NULOS") %>% 
  dplyr::group_by(uniqueid, section) %>%
  dplyr::summarise(across(c(PAN:"C I",nulos,total,PRI_PVEM_PANAL_PCU,PRI_PVEM_PANAL,PVEM_PANAL,PT_MC,valid), sum)) %>%
  dplyr::mutate(year = 2015, month = "October") %>% 
  dplyr::rename(CI_1 = "C I")

# Load and merge Lista Nominal data
ln_data_2015 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2015/LN2015.dta")

ln_data_2015 <- ln_data_2015 %>%
  dplyr::filter(entidad == 7 & month == 6) %>%
  dplyr::mutate(uniqueid = (entidad * 1000) , municipio) %>%
  dplyr::select(seccion, lista) %>% 
  dplyr::rename(section = seccion)

collapsed_2015 <- collapsed_2015 %>%
  dplyr::left_join(ln_data_2015, by = c("section")) %>%
  dplyr::rename(listanominal = lista)

# Calculate turnout
collapsed_2015 <- collapsed_2015 %>%
  dplyr::mutate(turnout = total / listanominal)

summary(collapsed_2015)
rm(data_2015)
rm(ln_data_2015)

# ---------------------- 2018 Data Processing ---------------------- #

# Import the 2018 election data
data_2018 <- read_excel("../../../Data/Raw Electoral Data/Chiapas - 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Resultados de Computos Municipales por Casilla de la Eleccion de Ayuntamientos - Chiapas 2018.xlsx")
names(data_2018)
# Rename municipality column
data_2018 <- data_2018 %>%
  dplyr::rename(municipality = MUNICIPIO_LOCAL,
         section = "SECCION")

# Convert municipality names to proper case
data_2018 <- data_2018 %>%
  mutate(municipality = tools::toTitleCase(municipality))

# Drop elections for OXCHUC (special case)
data_2018 <- data_2018 %>%
  dplyr::filter(municipality != "OXCHUC") %>%
  dplyr::mutate(across(c(PAN:"TOTAL VOTOS"), as.numeric))

# Drop and generate uniqueid column
data_2018$uniqueid <- NA

# Replace special characters in the municipality column
data_2018 <- data_2018 %>%
  dplyr::mutate(municipality = gsub("Á", "A", municipality),
                municipality = gsub("É", "E", municipality),
                municipality = gsub("Í", "I", municipality),
                municipality = gsub("Ó", "O", municipality),
                municipality = gsub("Ú", "U", municipality),
                municipality = gsub("Ü", "U", municipality),
                municipality = gsub("Ñ", "N", municipality),
                municipality = tools::toTitleCase(municipality))

summary(data_2018)

# Manually assign unique IDs based on municipality names
data_2018 <- data_2018 %>%
  dplyr::mutate(
    uniqueid = case_when(
      municipality == "ACALA" ~ 7002,
      municipality == "ALDAMA" ~ 7113,
      municipality == "AMATENANGO DE LA FRONTERA" ~ 7006,
      municipality == "AMAT\xc1N" ~ 7005,
      municipality == "ACACOYAGUA" ~ 7001,
      municipality == "ACAPETAHUA" ~ 7003,
      municipality == "ALTAMIRANO" ~ 7004,
      municipality == "AMATAN" ~ 7005,
      municipality == "AMATENANGO DE LA FRA." ~ 7006,
      municipality == "AMATENANGO DEL VALLE" ~ 7007,
      municipality == "\xc1NGEL ALBINO CORZO" ~ 7008,
      municipality == "ARRIAGA" ~ 7009,
      municipality == "BEJUCAL DE OCAMPO" ~ 7010,
      municipality == "BELLAVISTA" ~ 7011,
      municipality == "BERRIOZ\xc1BAL" ~ 7012,
      municipality == "BOCHIL" ~ 7013,
      municipality == "BOSQUE, EL" ~ 7014,
      municipality == "CACAHOAT\xc1N" ~ 7015,
      municipality == "CATAZAJ\xc1" ~ 7016,
      municipality == "CHALCHIHUIT\xc1N" ~ 7022,
      municipality == "CHANAL" ~ 7024,
      municipality == "CHAPULTENANGO" ~ 7025,
      municipality == "CHENALH\xd3" ~ 7026,
      municipality == "CHIAPA DE CORZO" ~ 7027,
      municipality == "CHIAPILLA" ~ 7028,
      municipality == "CHICOAS\xc9N" ~ 7029,
      municipality == "CHICOMUSELO" ~ 7030,
      municipality == "CHIL\xd3N" ~ 7031,
      municipality == "CINTALAPA" ~ 7017,
      municipality == "COAPILLA" ~ 7018,
      municipality == "COMIT\xc1N" ~ 7019,
      municipality == "CONCORDIA, LA" ~ 7020,
      municipality == "COPAINAL\xc1" ~ 7021,
      municipality == "FRANCISCO LE\xd3N" ~ 7033,
      municipality == "FRONTERA COMALAPA" ~ 7034,
      municipality == "FRONTERA HIDALGO" ~ 7035,
      municipality == "GRANDEZA, LA" ~ 7036,
      municipality == "HUEHUET\xc1N" ~ 7037,
      municipality == "HUITIUP\xc1N" ~ 7039,
      municipality == "HUIXTLA" ~ 7040,
      municipality == "HUIXT\xc1N" ~ 7038,
      municipality == "INDEPENDENCIA, LA" ~ 7041,
      municipality == "IXHUAT\xc1N" ~ 7042,
      municipality == "IXTACOMIT\xc1N" ~ 7043,
      municipality == "IXTAPA" ~ 7044,
      municipality == "IXTAPANGAJOYA" ~ 7045,
      municipality == "JIQUIPILAS" ~ 7046,
      municipality == "JITOTOL" ~ 7047,
      municipality == "JU\xc1REZ" ~ 7048,
      municipality == "LARRA\xcdNZAR" ~ 7049,
      municipality == "LIBERTAD, LA" ~ 7050,
      municipality == "MARGARITAS, LAS" ~ 7052,
      municipality == "MAZAPA DE MADERO" ~ 7053,
      municipality == "MAZAT\xc1N" ~ 7054,
      municipality == "METAPA DE DOM\xcdNGUEZ" ~ 7055,
      municipality == "MITONTIC" ~ 7056,
      municipality == "N CRISTOBAL DE LAS CAS" ~ 7078,
      municipality == "NICOL\xc1S RUIZ" ~ 7058,
      municipality == "OCOSINGO" ~ 7059,
      municipality == "OCOTEPEC" ~ 7060,
      municipality == "OCOZOCOAUTLA" ~ 7061,
      municipality == "OSTUAC\xc1N" ~ 7062,
      municipality == "OSUMACINTA" ~ 7063,
      municipality == "OXCHUC" ~ 7064,
      municipality == "PALENQUE" ~ 7065,
      municipality == "PANTELH\xd3" ~ 7066,
      municipality == "PANTEPEC" ~ 7067,
      municipality == "PICHUCALCO" ~ 7068,
      municipality == "PORVENIR, EL" ~ 7070,
      municipality == "RAY\xd3N" ~ 7073,
      municipality == "REFORMA" ~ 7074,
      municipality == "ROSAS, LAS" ~ 7075,
      municipality == "SABANILLA" ~ 7076,
      municipality == "Salto de Agua" ~ 7077,
      municipality == "SAN FERNANDO" ~ 7079,
      municipality == "SAN JUAN CANCUC" ~ 7112,
      municipality == "SAN LUCAS" ~ 7110,
      municipality == "SIMOJOVEL" ~ 7081,
      municipality == "SITAL\xc1" ~ 7082,
      municipality == "SOCOLTENANGO" ~ 7083,
      municipality == "SOLOSUCHIAPA" ~ 7084,
      municipality == "SOYAL\xd3" ~ 7085,
      municipality == "SUCHIAPA" ~ 7086,
      municipality == "SUCHIATE" ~ 7087,
      municipality == "SUNUAPA" ~ 7088,
      municipality == "TAPACHULA" ~ 7089,
      municipality == "TAPALAPA" ~ 7090,
      municipality == "TAPILULA" ~ 7091,
      municipality == "TECPAT\xc1N" ~ 7092,
      municipality == "TENEJAPA" ~ 7093,
      municipality == "TEOPISCA" ~ 7094,
      municipality == "TILA" ~ 7096,
      municipality == "TONALA" ~ 7097,
      municipality == "TOTOLAPA" ~ 7098,
      municipality == "TRINITARIA, LA" ~ 7099,
      municipality == "TUMBALA" ~ 7100,
      municipality == "TUXTLA CHICO" ~ 7102,
      municipality == "TUXTLA GUTI\xc9RREZ" ~ 7101,
      municipality == "TUZANT\xc1N" ~ 7103,
      municipality == "TZIMOL" ~ 7104,
      municipality == "UNI\xd3N JU\xc1REZ" ~ 7105,
      municipality == "VILLA CORZO" ~ 7107,
      municipality == "VILLAFLORES" ~ 7108,
      municipality == "YAJAL\xd3N" ~ 7109,
      municipality == "ZINACANT\xc1N" ~ 7111,
      municipality == "BENEM\xc9RITO DE LAS AM\xc9RICAS" ~ 7114, 
      municipality == "CHAMULA" ~ 7023,
      municipality == "EL BOSQUE" ~ 7014,
      municipality == "EL PORVENIR" ~ 7070,
      municipality == "ESCUINTLA" ~ 7032,
      municipality == "LA CONCORDIA" ~ 7020,
      municipality == "LA GRANDEZA" ~ 7036,
      municipality == "LA INDEPENDENCIA" ~ 7041,
      municipality == "LA LIBERTAD" ~ 7050,
      municipality == "LA TRINITARIA" ~ 7099,
      municipality == "LAS MARGARITAS" ~ 7052,
      municipality == "LAS ROSAS" ~ 7075,
      municipality == "MAPASTEPEC" ~ 7051,
      municipality == "MARAVILLA TENEJAPA" ~ 7115,
      municipality == "MARQU\xc9S DE COMILLAS" ~ 7116,
      municipality == "MONTECRISTO DE GUERRERO" ~ 7117,
      municipality == "MOTOZINTLA" ~ 7057,
      municipality == "PIJIJIAPAN" ~ 7069,
      municipality == "PUEBLO NVO. SOLISTAHUAC\xc1N" ~ 7072,
      municipality == "SALTO DE AGUA" ~ 7077,
      municipality == "SAN ANDR\xc9S DURAZNAL" ~ 7118,
      municipality == "SAN CRIST\xd3BAL DE LAS CASAS" ~ 7078,
      municipality == "SANTIAGO EL PINAR" ~ 7119,
      municipality == "SILTEPEC" ~ 7080,
      municipality == "TONAL\xc1" ~ 7097,
      municipality == "TUMBAL\xc1" ~ 7100,
      municipality == "VENUSTIANO CARRANZA" ~ 7106,
      municipality == "VILLA COMALTITL\xc1N" ~ 7071,
      municipality == "ANGEL ALBINO CORZO" ~ 7008,
      municipality == "BELLA VISTA" ~ 7011,
      municipality == "BENEMERITO DE LAS AMERICAS" ~ 7114,
      municipality == "BERRIOZABAL" ~ 7012,
      municipality == "CACAHOATAN" ~ 7015,
      municipality == "CATAZAJA" ~ 7016,
      municipality == "CHALCHIHUITAN" ~ 7022,
      municipality == "CHENALHO" ~ 7026,
      municipality == "CHICOASEN" ~ 7029,
      municipality == "CHILON" ~ 7031,
      municipality == "COMITAN DE DOMINGUEZ" ~ 7019,
      municipality == "COPAINALA" ~ 7021,
      municipality == "FRANCISCO LEON" ~ 7033,
      municipality == "HUEHUETAN" ~ 7037,
      municipality == "HUITIUPAN" ~ 7039,
      municipality == "HUIXTAN" ~ 7038,
      municipality == "IXHUATAN" ~ 7042,
      municipality == "IXTACOMITAN" ~ 7043,
      municipality == "JUAREZ" ~ 7048,
      municipality == "LARRAINZAR" ~ 7049,
      municipality == "MARQUES DE COMILLAS" ~ 7116,
      municipality == "MAZATAN" ~ 7054,
      municipality == "METAPA DE DOMINGUEZ" ~ 7055,
      municipality == "NICOLAS RUIZ" ~ 7058,
      municipality == "OCOZOCOAUTLA DE ESPINOSA" ~ 7061,
      municipality == "OSTUACAN" ~ 7062,
      municipality == "PANTELHO" ~ 7066,
      municipality == "PUEBLO NVO SOLISTAHUACAN" ~ 7072,
      municipality == "RAYON" ~ 7073,
      municipality == "SAN ANDRES DURAZNAL" ~ 7118,
      municipality == "SAN CRISTOBAL DE LAS CASAS" ~ 7078,
      municipality == "SITALA" ~ 7082,
      municipality == "SOYALO" ~ 7085,
      municipality == "TAPILULA" | municipality == "TAPILULA EXTRAORDINARIO"~ 7091,
      municipality == "TECPATAN" ~ 7092,
      municipality == "TUXTLA GUTIERREZ" ~ 7101,
      municipality == "TUZANTAN" ~ 7103,
      municipality == "UNION JUAREZ" ~ 7105,
      municipality == "VILLACOMALTITLAN" ~ 7071,
      municipality == "YAJALON" ~ 7109,
      municipality == "ZINACANTAN" ~ 7111,
      municipality == "AMILIANO ZAPATA" | municipality == "EMILIANO ZAPATA"~ 7121,
      municipality == "MEZCALAPA" ~ 7123,
      municipality == "PUEBLO NUEVO SOLISTAHUACAN" ~ 7072,
      municipality == "VILLA COMALTITLAN" ~ 7071,
      municipality == "EL PARRAL" ~ 7122,
      municipality == "METAPA" ~ 7055,
      municipality == "RINCON CHAMULA SAN PEDRO" ~ 7121,
      municipality == "CAPITAN LUIS ANGEL VIDAL" ~ 7120,
      TRUE ~ NA_real_  # Default case for municipalities that are not in the list
    ))

names(data_2018)

# Collapse the data by municipality and section, summing the variables
collapsed_2018 <- data_2018 %>%
  dplyr::rename(nulos = "VOTOS NULOS",
                total = "TOTAL VOTOS")

collapsed_2018 <- collapsed_2018 %>%
  dplyr::rename(
      CI_19 = "CAND_IND_19", CI_6 = "CAND_IND_6", CI_5 = "CAND_IND_5",
      CI_4 = "CAND_IND_4", CI_3 = "CAND_IND_3", CI_2 = "CAND_IND_2",
      CI_7 = "CAND_IND_7", CI_9 = "CAND_IND_9", CI_10 = "CAND_IND_10",
      CI_11 = "CAND_IND_11", CI_12 = "CAND_IND_12", CI_1 = "CAND_IND_1",
      CI_13 = "CAND_IND_13", CI_14 = "CAND_IND_14", CI_15 = "CAND_IND_15",
      CI_16 = "CAND_IND_16", CI_17 = "CAND_IND_17", CI_18 = "CAND_IND_18",
      CI_8 = "CAND_IND_8", CI_20 = "CAND_IND_20", CI_21 = "CAND_IND_21",
      CI_22 = "CAND_IND_22", CI_23 = "CAND_IND_23", CI_24 = "CAND_IND_24",
      CI_25 = "CAND_IND_25", CI_26 = "CAND_IND_26", CI_27 = "CAND_IND_27",
      CI_28 = "CAND_IND_28", CI_29 = "CAND_IND_29", CI_30 = "CAND_IND_30",
      CI_31 = "CAND_IND_31", CI_32 = "CAND_IND_32", CI_33 = "CAND_IND_33",
      CI_34 = "CAND_IND_34", CI_35 = "CAND_IND_35", CI_36 = "CAND_IND_36",
      CI_37 = "CAND_IND_37", CI_38 = "CAND_IND_38", CI_39 = "CAND_IND_39",
      CI_40 = "CAND_IND_40", CI_41 = "CAND_IND_41")

names(collapsed_2018)
# Create the PAN_PRD_MC variable
collapsed_2018 <- collapsed_2018 %>%
  dplyr::mutate(PAN_PRD_MC = rowSums(across(c(PAN,PRD,PMC,"PAN PRD","PAN PMC","PRD PMC",
                                                                     "PAN PRD PMC")), na.rm = TRUE),
         PAN_PRD_MC = ifelse(!is.na("PAN PRD PMC"), PAN_PRD_MC, NA),
         PAN = ifelse(!is.na("PAN PRD PMC"), NA, PAN),
         PRD = ifelse(!is.na("PAN PRD PMC"), NA, PRD),
         PMC = ifelse(!is.na("PAN PRD PMC"), NA, PMC))

# Drop unnecessary columns
collapsed_2018 <- collapsed_2018 %>%
  dplyr::select(-c("PAN PRD", "PAN PMC", "PRD PMC", "PAN PRD PMC")) %>%
  dplyr::rename(MC = PMC)

# Create the data_2018 variable
collapsed_2018 <- collapsed_2018 %>%
  dplyr::mutate(PRI_PVEM_PANAL_PCU = rowSums(across(c(PRI , PVEM , PNA , PCHU , "PRI PVEM PNA PCHU" , "PRI PVEM PNA" , 
                                       "PRI PVEM PCHU" , "PRI PNA PCHU" , "PVEM PNA PCHU" , "PRI PVEM" , "PRI PNA" , 
                                       "PRI PCHU" , "PVEM PNA" , "PVEM PCHU" , "PNA PCHU")), na.rm = TRUE),
                PRI_PVEM_PANAL_PCU = ifelse(!is.na("PRI PVEM PNA PCHU"), PRI_PVEM_PANAL_PCU, NA),
         PRI = ifelse(!is.na("PRI PVEM PNA PCHU"), NA, PRI),
         PVEM = ifelse(!is.na("PRI PVEM PNA PCHU"), NA, PVEM),
         PNA = ifelse(!is.na("PRI PVEM PNA PCHU"), NA, PNA),
         PCHU = ifelse(!is.na("PRI PVEM PNA PCHU"), NA, PCHU))

# Drop unnecessary columns and rename variables
collapsed_2018 <- collapsed_2018 %>%
  dplyr::select(-c("PRI PVEM PNA PCHU" , "PRI PVEM PNA" , 
            "PRI PVEM PCHU" , "PRI PNA PCHU" , "PVEM PNA PCHU" , "PRI PVEM" , "PRI PNA" , 
            "PRI PCHU" , "PVEM PNA" , "PVEM PCHU" , "PNA PCHU")) %>%
  dplyr::rename(PANAL = PNA, PCU = PCHU)

# Create PT_MORENA_PES variable
collapsed_2018 <- collapsed_2018 %>%
  dplyr::mutate(PT_MORENA_PES = ifelse(uniqueid != 7030 & uniqueid != 7083, 
                                       rowSums(across(c(PT , MORENA , PES , "PT MORENA" , "PT PES" , 
                                                        "MORENA PES" , "PT MORENA PES")), na.rm = TRUE),NA))

# Set PT, MORENA, PES to NA where appropriate
collapsed_2018 <- collapsed_2018 %>%
  dplyr::mutate(across(c(PT, MORENA, PES), ~ ifelse(uniqueid != 7030 & uniqueid != 7083, NA, .)))

# Drop unnecessary columns related to PT_MORENA_PES coalition
collapsed_2018 <- collapsed_2018 %>%
  dplyr::select(-c("PT MORENA PES", "PT MORENA", "PT PES", "MORENA PES"))

# Rename PMCH to MVC
collapsed_2018 <- collapsed_2018 %>%
  dplyr::rename(MVC = PMCH)
names(collapsed_2018)
# Collapse (group by) municipality, section, and uniqueid, summing across relevant columns
collapsed_2018 <- collapsed_2018 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:PT_MORENA_PES), sum, na.rm = TRUE))

# Create valid votes by summing specific columns
collapsed_2018 <- collapsed_2018 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, MC, PANAL, PCU, MORENA, PES, MVC, 
                                  PRI_PVEM_PANAL_PCU, PAN_PRD_MC, PT_MORENA_PES, 
                                  CI_19, CI_6, CI_5, CI_4, CI_3, CI_2, CI_7, CI_9, CI_10, CI_11, CI_12, CI_1, 
                                  CI_13, CI_14, CI_15, CI_16, CI_17, CI_18, CI_8, CI_20, CI_21, CI_22, CI_23, 
                                  CI_24, CI_25, CI_26, CI_27, CI_28, CI_29, CI_30, CI_31, CI_32, CI_33, 
                                  CI_34, CI_35, CI_36, CI_37, CI_38, CI_39, CI_40, CI_41)), na.rm = TRUE))

# Load and merge Lista Nominal data
ln_data_2018 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2018/LN2018.dta")

ln_data_2018 <- ln_data_2018 %>%
  dplyr::filter(entidad == 7 & month == 8) %>%
  dplyr::mutate(uniqueid = (entidad * 1000) , municipio) %>%
  dplyr::select(seccion, lista) %>% 
  dplyr::rename(section = seccion)

collapsed_2018 <- collapsed_2018 %>%
  dplyr::left_join(ln_data_2018, by = c("section")) %>%
  dplyr::rename(listanominal = lista)

# Calculate turnout and municipal turnout
collapsed_2018 <- collapsed_2018 %>%
  dplyr::mutate(turnout = total / listanominal,
                year = 2018, 
                month = "August")

# Combine the dataframes, handling different columns by filling with NA
chiapas_all <- bind_rows(collapsed_1995,
                         collapsed_1998,
                         collapsed_2001,
                         collapsed_2004,
                         collapsed_2007,
                         collapsed_2010,
                         collapsed_2012,
                         collapsed_2015,
                         collapsed_2018)

data.table::fwrite(chiapas_all,"../../../Processed Data/chiapas/chiapas_process_raw_data.csv")



