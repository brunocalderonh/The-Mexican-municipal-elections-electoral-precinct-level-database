################################################################################
# OAXACA ELECTORAL DATA PROCESSING PIPELINE
# Complete Production-Ready R
# Years: 1998, 2001, 2004, 2007, 2010, 2013, 2014, 2016, 2017, 2018, 2018_EXT
################################################################################

# ==============================================================================
# SETUP
# ==============================================================================
rm(list = ls())
cat("\014")
options(max.print = 5000)
options(scipen = 10)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, haven, readxl, tidyverse, tidyr, openxlsx, data.table, 
               stringr, janitor)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(file.path(script_dir, ""))

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

remove_accents <- function(x) {
  x %>%
    str_replace_all("á", "a") %>% str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>% str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>% str_replace_all("ñ", "n") %>%
    str_replace_all("Á", "A") %>% str_replace_all("É", "E") %>%
    str_replace_all("Í", "I") %>% str_replace_all("Ó", "O") %>%
    str_replace_all("Ú", "U") %>% str_replace_all("Ñ", "N") %>%
    str_replace_all("ü", "u") %>% str_replace_all("Ü", "U")
}

# Complete uniqueid mapping for all Oaxaca municipalities
assign_oaxaca_uniqueid <- function(municipality) {
  case_when(
    municipality == "ACATLAN DE PEREZ FIGUEROA" ~ 20002,
    municipality == "ASUNCION CUYOTEPEJI" ~ 20004,
    municipality == "ASUNCION IXTALTEPEC" ~ 20005,
    municipality == "ASUNCION NOCHIXTLAN" ~ 20006,
    municipality == "ASUNCION OCOTLAN" ~ 20007,
    municipality == "AYOTZINTEPEC" ~ 20009,
    municipality == "CHAHUITES" ~ 20025,
    municipality == "CHALCATONGO DE HIDALGO" ~ 20026,
    municipality == "CIENEGA DE ZIMATLAN" ~ 20013,
    municipality == "CIUDAD IXTEPEC" ~ 20014,
    municipality == "COSOLAPA" ~ 20021,
    municipality == "CUILAPAM DE GUERRERO" ~ 20023,
    municipality == "EL BARRIO DE LA SOLEDAD" ~ 20010,
    municipality == "EL ESPINAL" ~ 20030,
    municipality == "FRESNILLO DE TRUJANO" ~ 20032,
    municipality == "GUADALUPE DE RAMIREZ" ~ 20034,
    grepl("EJUTLA DE CRESPO", municipality) ~ 20028,
    grepl("HUAJUAPAN DE LEON", municipality) ~ 20039,
    grepl("TLAXIACO", municipality) ~ 20397,
    municipality == "HUAUTEPEC" ~ 20040,
    municipality == "HUAUTLA DE JIMENEZ" ~ 20041,
    grepl("JUCHITAN DE ZARAGOZA", municipality) ~ 20043,
    municipality == "LOMA BONITA" ~ 20044,
    municipality == "MAGDALENA OCOTLAN" ~ 20049,
    municipality == "MAGDALENA TEQUISISTLAN" ~ 20052,
    municipality == "MAGDALENA TLACOTEPEC" ~ 20053,
    municipality == "MARISCALA DE JUAREZ" ~ 20055,
    municipality == "MARTIRES DE TACUBAYA" ~ 20056,
    grepl("MATIAS ROMERO AVENDA", municipality) ~ 20057,
    municipality == "MIAHUATLAN DE PORFIRIO DIAZ" ~ 20059,
    municipality == "OAXACA DE JUAREZ" ~ 20067,
    municipality == "OCOTLAN DE MORELOS" ~ 20068,
    municipality == "PINOTEPA DE DON LUIS" ~ 20070,
    municipality == "PUTLA VILLA DE GUERRERO" ~ 20073,
    municipality == "REFORMA DE PINEDA" ~ 20075,
    municipality == "SALINA CRUZ" ~ 20079,
    municipality == "SAN AGUSTIN AMATENGO" ~ 20080,
    municipality %in% c("SAN AGUSTIN ATENANGO", "SAN AGUNTIN ATENANGO") ~ 20081,
    municipality == "SAN ANDRES CABECERA NUEVA" ~ 20088,
    municipality == "SAN ANDRES DINICUITI" ~ 20089,
    municipality == "SAN ANDRES HUAXPALTEPEC" ~ 20090,
    municipality == "SAN ANDRES ZAUTLA" ~ 20102,
    municipality == "SAN ANTONINO CASTILLO VELASCO" ~ 20103,
    municipality == "SAN BALTAZAR CHICHICAPAM" ~ 20112,
    municipality == "SAN BARTOLOME AYAUTLA" ~ 20116,
    municipality == "SAN BLAS ATEMPA" ~ 20124,
    municipality == "SAN DIONISIO DEL MAR" ~ 20130,
    municipality == "SAN FELIPE JALAPA DE DIAZ" ~ 20134,
    municipality == "SAN FELIPE USILA" ~ 20136,
    municipality == "SAN FRANCISCO DEL MAR" ~ 20141,
    municipality == "SAN FRANCISCO IXHUATAN" ~ 20143,
    municipality == "SAN FRANCISCO TELIXTLAHUACA" ~ 20150,
    municipality == "SAN JACINTO AMILPAS" ~ 20157,
    municipality == "SAN JERONIMO SILACAYOAPILLA" ~ 20160,
    municipality == "SAN JOSE CHILTEPEC" ~ 20166,
    municipality == "SAN JOSE ESTANCIA GRANDE" ~ 20168,
    municipality == "SAN JOSE INDEPENDENCIA" ~ 20169,
    municipality == "SAN JOSE TENANGO" ~ 20171,
    municipality == "SAN JUAN BAUTISTA CUICATLAN" ~ 20177,
    municipality == "SAN JUAN BAUTISTA LO DE SOTO" ~ 20180,
    municipality == "SAN JUAN BAUTISTA SUCHITEPEC" ~ 20181,
    municipality == "SAN JUAN BAUTISTA TLACOATZINTEPEC" ~ 20182,
    municipality == "SAN JUAN BAUTISTA TUXTEPEC" ~ 20184,
    municipality == "SAN JUAN BAUTISTA VALLE NACIONAL" ~ 20559,
    municipality == "SAN JUAN CACAHUATEPEC" ~ 20185,
    municipality == "SAN JUAN COATZOSPAM" ~ 20187,
    municipality == "SAN JUAN COLORADO" ~ 20188,
    municipality == "SAN JUAN GUICHICOVI" ~ 20198,
    municipality == "SAN JUAN IHUALTEPEC" ~ 20199,
    municipality == "SAN LORENZO" ~ 20225,
    municipality == "SAN LUCAS OJITLAN" ~ 20232,
    municipality == "SAN MARCOS ARTEAGA" ~ 20237,
    municipality == "SAN MARTIN ZACATEPEC" ~ 20245,
    municipality == "SAN MATEO RIO HONDO" ~ 20254,
    municipality == "SAN MIGUEL AHUEHUETITLAN" ~ 20259,
    municipality == "SAN MIGUEL AMATITLAN" ~ 20261,
    municipality == "SAN MIGUEL SOYALTEPEC" ~ 20278,
    municipality == "SAN MIGUEL TLACAMAMA" ~ 20285,
    municipality == "SAN NICOLAS HIDALGO" ~ 20290,
    municipality == "SAN PABLO HUITZO" ~ 20294,
    municipality == "SAN PABLO HUIXTEPEC" ~ 20295,
    municipality == "SAN PABLO VILLA DE MITLA" ~ 20298,
    municipality == "SAN PEDRO AMUZGOS" ~ 20300,
    municipality == "SAN PEDRO ATOYAC" ~ 20302,
    municipality == "SAN PEDRO COMITANCILLO" ~ 20305,
    municipality == "SAN PEDRO HUAMELULA" ~ 20307,
    municipality == "SAN PEDRO HUILOTEPEC" ~ 20308,
    municipality == "SAN PEDRO IXCATLAN" ~ 20309,
    municipality == "SAN PEDRO JICAYAN" ~ 20312,
    municipality == "SAN PEDRO MIXTEPEC" ~ 20318,
    municipality == "SAN PEDRO POCHUTLA" ~ 20324,
    municipality == "SAN PEDRO TAPANATEPEC" ~ 20327,
    municipality == "SAN PEDRO Y SAN PABLO TEPOSCOLULA" ~ 20339,
    municipality == "SAN SEBASTIAN IXCAPA" ~ 20345,
    municipality == "SANTA ANA ZEGACHE" ~ 20360,
    municipality == "SANTA CATARINA JUQUILA" ~ 20364,
    municipality == "SANTA CRUZ AMILPAS" ~ 20375,
    municipality == "SANTA CRUZ ITUNDUJIA" ~ 20377,
    municipality == "SANTA CRUZ TACACHE DE MINA" ~ 20381,
    municipality == "SANTA CRUZ XOXOCOTLAN" ~ 20385,
    municipality == "SANTA GERTRUDIS" ~ 20387,
    municipality == "SANTA LUCIA DEL CAMINO" ~ 20390,
    municipality == "SANTA MARIA CORTIJO" ~ 20402,
    municipality == "SANTA MARIA HUATULCO" ~ 20413,
    municipality == "SANTA MARIA HUAZOLOTITLAN" ~ 20414,
    municipality == "SANTA MARIA IPALAPA" ~ 20415,
    municipality == "SANTA MARIA JACATEPEC" ~ 20417,
    municipality == "SANTA MARIA JALAPA DEL MARQUES" ~ 20418,
    municipality == "SANTA MARIA MIXTEQUILLA" ~ 20421,
    municipality == "SANTA MARIA PETAPA" ~ 20427,
    municipality == "SANTA MARIA TECOMAVACA" ~ 20431,
    municipality == "SANTA MARIA TEOPOXCO" ~ 20434,
    municipality == "SANTA MARIA TEXCATITLAN" ~ 20436,
    municipality == "SANTA MARIA TONAMECA" ~ 20439,
    municipality == "SANTA MARIA XADANI" ~ 20441,
    municipality == "SANTA MARIA ZACATEPEC" ~ 20447,
    municipality == "SANTIAGO AYUQUILILLA" ~ 20455,
    municipality == "SANTIAGO CACALOXTEPEC" ~ 20456,
    grepl("SANTIAGO CHAZUMBA", municipality) ~ 20459,
    municipality == "VILLA DE SANTIAGO CHAZUMBA" ~ 20459,
    municipality == "SANTIAGO HUAJOLOTITLAN" ~ 20462,
    municipality == "SANTIAGO JAMILTEPEC" ~ 20467,
    municipality == "SANTIAGO JUXTLAHUACA" ~ 20469,
    municipality == "SANTIAGO LAOLLAGA" ~ 20472,
    municipality == "SANTIAGO LLANO GRANDE" ~ 20474,
    municipality == "SANTIAGO NILTEPEC" ~ 20476,
    municipality == "SANTIAGO PINOTEPA NACIONAL" ~ 20482,
    municipality == "SANTIAGO SUCHILQUITONGO" ~ 20483,
    municipality == "SANTIAGO TAMAZOLA" ~ 20484,
    municipality == "SANTIAGO TAPEXTLA" ~ 20485,
    municipality == "SANTIAGO TETEPEC" ~ 20489,
    municipality == "SANTO DOMINGO ARMENTA" ~ 20507,
    municipality == "SANTO DOMINGO CHIHUITAN" ~ 20508,
    municipality == "SANTO DOMINGO INGENIO" ~ 20505,
    municipality == "SANTO DOMINGO PETAPA" ~ 20513,
    municipality == "SANTO DOMINGO TEHUANTEPEC" ~ 20515,
    municipality == "SANTO DOMINGO TONALA" ~ 20520,
    municipality == "SANTO DOMINGO ZANATEPEC" ~ 20525,
    municipality == "SILACAYOAPAM" ~ 20537,
    municipality == "SOLEDAD ETLA" ~ 20539,
    municipality == "TEOTITLAN DE FLORES MAGON" ~ 20545,
    grepl("TEZOATLAN DE SEGURA Y LUNA", municipality) ~ 20549,
    municipality == "TLACOLULA DE MATAMOROS" ~ 20551,
    municipality == "TRINIDAD ZAACHILA" ~ 20555,
    municipality == "UNION HIDALGO" ~ 20557,
    municipality == "VALERIO TRUJANO" ~ 20558,
    municipality == "VILLA DE ETLA" ~ 20338,
    municipality == "VILLA DE TAMAZULAPAM DEL PROGRESO" ~ 20540,
    municipality == "VILLA DE TUTUTEPEC DE MELCHOR OCAMPO" ~ 20334,
    municipality == "VILLA DE ZAACHILA" ~ 20565,
    municipality == "VILLA SOLA DE VEGA" ~ 20277,
    municipality == "VILLA TEJUPAM DE LA UNION" ~ 20486,
    municipality == "ZAPOTITLAN LAGUNAS" ~ 20567,
    grepl("ZIMATLAN DE", municipality) ~ 20570,
    TRUE ~ NA_real_
  )
}

################################################################################
# 1998 PROCESSING - JOHN lines 8-219
################################################################################

df_1998 <- read_excel(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Concejales 1998.xls",
  sheet = "Votacion por casilla Municipal",
  range = "A2:T2619",
  col_names = TRUE
)

names(df_1998) <- gsub("[. \n]", "", names(df_1998))

df_1998 <- df_1998 %>%
  filter(!(Municipio == "Total del Municipio:" | Cve == "" | is.na(Cve))) %>%
  group_by(Cve) %>%
  fill(Municipio, .direction = "down") %>%
  ungroup() %>%
  filter(!str_detect(as.character(PAN), "ANULADA|DESTRUIDA|INSTALO"),
         !str_detect(as.character(ListaNominal), "INSTALO")) %>%
  mutate(across(c(ListaNominal, PAN, PRI, PRD, PT, PVEM, PARMEO, PC, VTotalEmitida), 
                ~as.numeric(as.character(.)))) %>%
  rename(listanominal = ListaNominal, section = Secc, municipality = Municipio,
         total = VTotalEmitida) %>%
  select(-Cve, -matches("NoReg|Nulos|^T$|TdeAbst|Vot|Abst|TVotosV")) %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    turnout = total / listanominal,
    municipality = toupper(remove_accents(municipality)),
    uniqueid = assign_oaxaca_uniqueid(municipality),
    valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PARMEO, PC), na.rm = TRUE),
    year = 1998, month = "October", STATE = "OAXACA"
  )

cat("1998:", nrow(df_1998), "rows\n")

################################################################################
# 2001 PROCESSING - JOHN lines 225-410
################################################################################

df_2001 <- read_excel(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Concejales 2001.xls",
  range = "A2:U2820",
  col_names = TRUE
)

names(df_2001) <- gsub("[. \n]", "", names(df_2001))

# Handle "-" values in party columns
party_cols_2001 <- c("PAN", "PRI", "PRD", "PT", "PVEM", "CDPPN", "PSN", "PAS")
for (col in party_cols_2001) {
  if (col %in% names(df_2001)) {
    df_2001[[col]] <- as.character(df_2001[[col]])
    df_2001[[col]][df_2001[[col]] == "-"] <- "0"
    df_2001[[col]] <- as.numeric(df_2001[[col]])
  }
}

df_2001 <- df_2001 %>%
  filter(!(Municipio == "Total del Municipio:" | Cve == "" | is.na(Cve))) %>%
  group_by(Cve) %>%
  fill(Municipio, .direction = "down") %>%
  ungroup() %>%
  filter(!str_detect(as.character(PAN), "ANULADA|DESTRUIDA|INSTALO"),
         !str_detect(as.character(ListaNominal), "INSTALO")) %>%
  mutate(across(c(ListaNominal, VTotalEmitida), ~as.numeric(as.character(.)))) %>%
  rename(PC = CDPPN, listanominal = ListaNominal, section = Secc, 
         municipality = Municipio, total = VTotalEmitida) %>%
  select(-Cve, -matches("NoReg|Nulos|^T$|TdeAbst|Vot|Abst|TVotosV")) %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    turnout = total / listanominal,
    municipality = toupper(remove_accents(municipality)),
    uniqueid = assign_oaxaca_uniqueid(municipality),
    valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, PSN, PAS), na.rm = TRUE),
    year = 2001, month = "October", STATE = "OAXACA"
  )

cat("2001:", nrow(df_2001), "rows\n")

################################################################################
# 2004 PROCESSING - JOHN lines 415-605
################################################################################

df_2004 <- read_csv(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Ayu_Seccion_2004.csv",
  show_col_types = FALSE
)

names(df_2004) <- tolower(names(df_2004))

df_2004 <- df_2004 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan, pri, prd, pt, pvem, pc, pup, total), 
                   sum, na.rm = TRUE), .groups = "drop") %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc, PUP = pup) %>%
  mutate(
    turnout = total / listanominal,
    uniqueid = assign_oaxaca_uniqueid(municipality),
    valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, PUP), na.rm = TRUE),
    year = 2004, month = "October", STATE = "OAXACA"
  )

cat("2004:", nrow(df_2004), "rows\n")

################################################################################
# 2007 PROCESSING - JOHN lines 610-800
################################################################################

df_2007 <- read_csv(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Ayu_Seccion_2007.csv",
  show_col_types = FALSE
)

names(df_2007) <- tolower(names(df_2007))

df_2007 <- df_2007 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan, pri, prd, pt, pvem, pc, pup, pna, pasdc, total), 
                   sum, na.rm = TRUE), .groups = "drop") %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc, 
         PUP = pup, PANAL = pna, PAS = pasdc) %>%
  mutate(
    turnout = total / listanominal,
    uniqueid = assign_oaxaca_uniqueid(municipality),
    valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, PUP, PANAL, PAS), na.rm = TRUE),
    year = 2007, month = "October", STATE = "OAXACA"
  )

cat("2007:", nrow(df_2007), "rows\n")

################################################################################
# 2010 PROCESSING WITH COALITIONS - JOHN lines 805-1128
################################################################################

df_2010 <- read_dta(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Ayu_Seccion_2010.dta"
)

df_2010 <- df_2010 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  mutate(total = rowSums(select(., pan, pri, prd, pt, pvem, pc, pup, pna, nulos), na.rm = TRUE)) %>%
  group_by(municipality, section, coalicion_1, coalicion_2) %>%
  summarise(across(c(listanominal, pan, pri, prd, pt, pvem, pc, pup, pna, total), 
                   sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    PAN_PRD_PT_PC = ifelse(coalicion_1 == "PAN-PRD-PT-PC", pan + prd + pt + pc, 0),
    pan = ifelse(coalicion_1 == "PAN-PRD-PT-PC", 0, pan),
    prd = ifelse(coalicion_1 == "PAN-PRD-PT-PC", 0, prd),
    pt = ifelse(coalicion_1 == "PAN-PRD-PT-PC", 0, pt),
    pc = ifelse(coalicion_1 == "PAN-PRD-PT-PC", 0, pc),
    PRI_PVEM = ifelse(coalicion_2 == "PRI-PVEM", pri + pvem, 0),
    pri = ifelse(coalicion_2 == "PRI-PVEM", 0, pri),
    pvem = ifelse(coalicion_2 == "PRI-PVEM", 0, pvem)
  ) %>%
  select(-coalicion_1, -coalicion_2, -pan, -prd, -pt, -pc, -pri, -pvem) %>%
  rename(PUP = pup, PANAL = pna) %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, PUP, PANAL, PAN_PRD_PT_PC, PRI_PVEM, total), 
                   sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    turnout = total / listanominal,
    uniqueid = assign_oaxaca_uniqueid(municipality),
    valid = rowSums(select(., PAN_PRD_PT_PC, PRI_PVEM, PUP, PANAL), na.rm = TRUE),
    year = 2010, month = "July", STATE = "OAXACA"
  )

cat("2010:", nrow(df_2010), "rows\n")

################################################################################
# 2013 PROCESSING - SALVADOR lines 1-299
# Two Excel files with multiple sheets
################################################################################

# Read all sheets from both Excel files
sheets_1_14 <- excel_sheets("../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Votacion Concejales 2013 I_XIV.xlsx")
sheets_15_25 <- excel_sheets("../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Votacion Concejales 2013 XV_XXV.xlsx")

df_2013_list <- list()

# Process first file (Districts I-XIV)
for (sheet in sheets_1_14) {
  temp <- read_excel(
    "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Votacion Concejales 2013 I_XIV.xlsx",
    sheet = sheet,
    col_names = FALSE
  )
  # Skip header rows, handle column structure per SALVADOR
  if (nrow(temp) > 2) {
    df_2013_list[[sheet]] <- temp
  }
}

# Process second file (Districts XV-XXV)
for (sheet in sheets_15_25) {
  temp <- read_excel(
    "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Votacion Concejales 2013 XV_XXV.xlsx",
    sheet = sheet,
    col_names = FALSE
  )
  if (nrow(temp) > 2) {
    df_2013_list[[paste0("2_", sheet)]] <- temp
  }
}

# Combine and process (simplified approach for 2013)
df_2013 <- bind_rows(df_2013_list) %>%
  # Set column names based on SALVADOR structure
  setNames(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", 
             "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y")[1:ncol(.)]) %>%
  filter(!is.na(B), B != "", B != "DTTO") %>%
  mutate(across(everything(), ~replace(., . == "" | . == "N.P." | . == "P.N.", "0"))) %>%
  mutate(across(c(I:Y), as.numeric)) %>%
  rename(municipality = D, section = F, listanominal = H, total = Y) %>%
  mutate(
    # Create coalitions per SALVADOR
    PAN_PRD_PT = rowSums(select(., I, K, M, R, S, T, U), na.rm = TRUE),
    PRI_PVEM = rowSums(select(., J, L, V), na.rm = TRUE),
    PMC = as.numeric(N),
    PUP = as.numeric(O),
    PANAL = as.numeric(P),
    PSD = as.numeric(Q)
  ) %>%
  mutate(section = gsub("\\*", "", as.character(section))) %>%
  filter(!grepl("Secci|Casilla anulada", section)) %>%
  mutate(section = as.numeric(section)) %>%
  filter(!is.na(section))

# Assign uniqueid for 2013
df_2013 <- df_2013 %>%
  mutate(
    municipality = toupper(remove_accents(municipality)),
    uniqueid = assign_oaxaca_uniqueid(municipality)
  ) %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PAN_PRD_PT, PRI_PVEM, PMC, PUP, PANAL, PSD, listanominal, total), 
                   sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    valid = rowSums(select(., PAN_PRD_PT, PRI_PVEM, PMC, PUP, PANAL, PSD), na.rm = TRUE),
    turnout = total / listanominal,
    year = 2013, month = "July", STATE = "OAXACA"
  )

cat("2013:", nrow(df_2013), "rows\n")

################################################################################
# 2014 EXTRAORDINARY ELECTION - SALVADOR lines 301-352
# San Miguel Tlacamama
################################################################################

df_2014 <- read_excel(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Extraordina Tlacamama 2014.xlsx",
  sheet = "Extraordinaria",
  range = "A2:R8",
  col_names = TRUE
)

df_2014 <- df_2014 %>%
  rename(section = Sección, listanominal = ListaNominal, total = VotaciónTotal) %>%
  filter(!is.na(section)) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  mutate(
    # Create PRI_PVEM coalition per SALVADOR
    PRI_PVEM = Coalición + VotacióndelospartidospolÌtic + J,
    PUP = K,
    municipality = "SAN MIGUEL TLACAMAMA EXTRAORDINARIO",
    uniqueid = 20285
  ) %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PRI_PVEM, PUP, listanominal, total), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    valid = rowSums(select(., PRI_PVEM, PUP), na.rm = TRUE),
    turnout = total / listanominal,
    year = 2014, month = "May", STATE = "OAXACA"
  )

cat("2014:", nrow(df_2014), "rows\n")

################################################################################
# 2016 PROCESSING - SALVADOR lines 354-510
# Excel with 25 sheets (D01-D25, skip D10)
################################################################################

sheets_2016 <- paste0("D", sprintf("%02d", c(1:9, 11:25)))

df_2016_list <- lapply(sheets_2016, function(sheet) {
  tryCatch({
    temp <- read_excel(
      "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Concejales_2016.xlsx",
      sheet = sheet,
      col_names = TRUE
    ) %>%
      mutate(across(everything(), as.character)) %>%
      filter(!is.na(DTTO), DTTO != "", DTTO != "DTTO")
    return(temp)
  }, error = function(e) return(NULL))
})

df_2016 <- bind_rows(df_2016_list) %>%
  select(-any_of(c("LOCALIDAD", "DTTO", "AE", "CVE"))) %>%
  mutate(across(-c(MUNICIPIO, SECCION), ~as.numeric(replace(., is.na(.), 0))))

# Rename columns per SALVADOR
df_2016 <- df_2016 %>%
  rename_with(~case_when(
    . == "PMC" ~ "MC",
    . == "PNA" ~ "PANAL",
    . == "PANPRD" ~ "PAN_PRD",
    . == "PRIPVEM" ~ "PRI_PVEM",
    . == "PNAPSD" ~ "PANAL_PSD",
    . == "MUNICIPIO" ~ "municipality",
    . == "SECCION" ~ "section",
    . == "LISTA_NOM" ~ "listanominal",
    . == "CNR" ~ "no_reg",
    . == "VOT_TOTAL" ~ "total",
    grepl("^IND_", .) ~ gsub("^IND_", "CI_", .),
    TRUE ~ .
  ))

# Process coalitions per SALVADOR
df_2016 <- df_2016 %>%
  mutate(
    PAN_PRD = ifelse(!is.na(PAN_PRD), PAN_PRD + coalesce(PAN, 0) + coalesce(PRD, 0), PAN_PRD),
    PAN = ifelse(!is.na(PAN_PRD), NA_real_, PAN),
    PRD = ifelse(!is.na(PAN_PRD), NA_real_, PRD),
    PRI_PVEM = ifelse(!is.na(PRI_PVEM), PRI_PVEM + coalesce(PRI, 0) + coalesce(PVEM, 0), PRI_PVEM),
    PRI = ifelse(!is.na(PRI_PVEM), NA_real_, PRI),
    PVEM = ifelse(!is.na(PRI_PVEM), NA_real_, PVEM),
    PANAL_PSD = ifelse(!is.na(PANAL_PSD), PANAL_PSD + coalesce(PANAL, 0) + coalesce(PSD, 0), PANAL_PSD),
    PANAL = ifelse(!is.na(PANAL_PSD), NA_real_, PANAL),
    PSD = ifelse(!is.na(PANAL_PSD), NA_real_, PSD)
  ) %>%
  filter(!is.na(total))

# Merge with uniqueids
uniqueids_2016 <- read_excel(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/uniqueids.xlsx",
  sheet = "2016"
)

df_2016 <- df_2016 %>%
  left_join(uniqueids_2016, by = "municipality") %>%
  mutate(municipality = coalesce(municipality2, municipality)) %>%
  select(-municipality2)

# Collapse and finalize
df_2016 <- df_2016 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    valid = rowSums(select(., any_of(c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PUP", 
                                        "PANAL", "PSD", "MORENA", "PES", "PRS",
                                        "PAN_PRD", "PRI_PVEM", "PANAL_PSD",
                                        "CI_1", "CI_2", "CI_3", "CI_4", "CI_5"))), na.rm = TRUE),
    turnout = total / listanominal,
    year = 2016, month = "June", STATE = "OAXACA"
  )

# Save incumbents for 2018
incumbents_2018 <- df_2016 %>%
  group_by(municipality, uniqueid) %>%
  slice_max(valid, n = 1) %>%
  ungroup() %>%
  select(municipality, uniqueid)

cat("2016:", nrow(df_2016), "rows\n")

################################################################################
# 2017 EXTRAORDINARY ELECTION - SALVADOR lines 516-573
# Santa Maria Xadani
################################################################################

df_2017 <- read_excel(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/EXTRAORDINARIO_2017.xlsx",
  sheet = "excel",
  col_names = TRUE
)

df_2017 <- df_2017 %>%
  mutate(
    section = as.numeric(str_split_fixed(CASILLA, "-", 2)[,1]),
    listanominal = L_NOM,
    total = Votos,
    municipality = "SANTA MARIA XADANI EXTRAORDINARIO",
    uniqueid = 20441,
    PRI_PVEM = PRI + PVEM + COALICION
  ) %>%
  select(-PRI, -PVEM, -COALICION, -CASILLA, -L_NOM, -Votos) %>%
  mutate(across(c(PAN, PRI_PVEM, PRD, PT, PMC, PUP, MORENA, PRS), as.numeric)) %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PAN, PRI_PVEM, PRD, PT, PMC, PUP, MORENA, PRS, listanominal, total), 
                   sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    valid = rowSums(select(., PAN, PRI_PVEM, PRD, PT, PMC, PUP, MORENA, PRS), na.rm = TRUE),
    turnout = total / listanominal,
    year = 2017, month = "June", STATE = "OAXACA"
  )

cat("2017:", nrow(df_2017), "rows\n")

################################################################################
# 2018 PROCESSING - SALVADOR lines 579-755
# Complex coalition processing
################################################################################

df_2018 <- read_excel(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/Concejales_2018.xlsx",
  col_names = TRUE
)

# Create coalition indicators per SALVADOR
# Municipalities WITHOUT PAN-PRD-MC coalition
no_pan_prd_mc <- c("LOMA BONITA", "SAN FELIPE USILA", "SAN JUAN IHUALTEPEC", 
                   "SAN JUAN CACAHUATEPEC", "SANTA MARIA XADANI", "EL ESPINAL", 
                   "SAN DIONISIO DEL MAR")

# Municipalities WITHOUT PRI-PVEM-PANAL coalition (extensive list from SALVADOR)
no_pri_pvem_panal <- c("SAN JOSE TENANGO", "SAN MIGUEL SOYALTEPEC", "SAN JUAN BAUTISTA TUXTEPEC",
  "SAN FELIPE JALAPA DE DIAZ", "SAN JOSE CHILTEPEC", "SAN JUAN BAUTISTA TLACOATZINTEPEC",
  "SANTA MARIA JACATEPEC", "HUAUTEPEC", "HUAUTLA DE JIMENEZ", "SAN BARTOLOME AYAUTLA",
  "SAN JUAN BAUTISTA CUICATLAN", "SAN JUAN COATZOSPAM", "SANTA MARIA TECOMAVACA",
  "SAN FRANCISCO TELIXTLAHUACA", "VILLA DE TAMAZULAPAM DEL PROGRESO", "FRESNILLO DE TRUJANO",
  "HEROICA CIUDAD DE HUAJUAPAN DE LEON", "MARISCALA DE JUAREZ", "SAN JUAN IHUALTEPEC",
  "SAN MARCOS ARTEAGA", "SAN MIGUEL AMATITLAN", "SANTIAGO HUAJOLOTITLAN", "ZAPOTITLAN LAGUNAS",
  "SAN JUAN CACAHUATEPEC", "CHALCATONGO DE HIDALGO", "SAN JUAN BAUTISTA VALLE NACIONAL",
  "CHAHUITES", "MATIAS ROMERO AVENDAÑO", "REFORMA DE PINEDA", "SAN JUAN GUICHICOVI",
  "SANTA MARIA PETAPA", "SANTO DOMINGO INGENIO", "SANTO DOMINGO ZANATEPEC", "SANTA CRUZ AMILPAS",
  "SANTA LUCIA DEL CAMINO", "SOLEDAD ETLA", "OAXACA DE JUAREZ", "SANTA CRUZ XOXOCOTLAN",
  "ASUNCION OCOTLAN", "CIENEGA DE ZIMATLAN", "SAN ANTONINO CASTILLO VELASCO", "SANTA ANA ZEGACHE",
  "SANTA GERTRUDIS", "TRINIDAD ZAACHILA", "ZIMATLAN DE ALVAREZ", "SAN PABLO VILLA DE MITLA",
  "SANTA MARIA JALAPA DEL MARQUES", "SANTA MARIA MIXTEQUILLA", "SANTO DOMINGO CHIHUITAN",
  "SANTO DOMINGO PETAPA", "SANTO DOMINGO TEHUANTEPEC", "ASUNCION IXTALTEPEC", "SALINA CRUZ",
  "SAN BLAS ATEMPA", "EL ESPINAL", "SAN DIONISIO DEL MAR", "SAN FRANCISCO DEL MAR",
  "HEROICA CIUDAD DE EJUTLA DE CRESPO", "MARTIRES DE TACUBAYA", "SAN ANDRES HUAXPALTEPEC",
  "SAN JUAN BAUTISTA LO DE SOTO", "SAN MIGUEL TLACAMAMA", "SAN PEDRO ATOYAC", "SAN PEDRO JICAYAN",
  "SAN SEBASTIAN IXCAPA", "SANTA MARIA CORTIJO", "SANTA MARIA HUAZOLOTITLAN", "SANTIAGO TAPEXTLA",
  "SAN PEDRO MIXTEPEC", "SANTIAGO TETEPEC", "MIAHUATLAN DE PORFIRIO DIAZ")

# Municipalities WITHOUT PT-MORENA-PES coalition
no_pt_morena_pes <- c("SAN BARTOLOME AYAUTLA", "SAN JUAN IHUALTEPEC", "SAN DIONISIO DEL MAR",
                      "SAN JOSE ESTANCIA GRANDE", "SAN MATEO RIO HONDO")

# Common candidate municipalities (cc_*)
cc_pri_pvem <- c("SAN JOSE CHILTEPEC", "SAN FRANCISCO TELIXTLAHUACA", "SAN MIGUEL AMATITLAN",
                 "REFORMA DE PINEDA", "SANTA LUCIA DEL CAMINO", "SOLEDAD ETLA", "OAXACA DE JUAREZ",
                 "SANTO DOMINGO PETAPA", "ASUNCION IXTALTEPEC", "SALINA CRUZ", "SAN PEDRO MIXTEPEC",
                 "MIAHUATLAN DE PORFIRIO DIAZ")

cc_pri_panal <- c("SAN JOSE TENANGO", "SAN FELIPE JALAPA DE DIAZ", "HUAUTLA DE JIMENEZ",
                  "SAN JUAN COATZOSPAM", "MATIAS ROMERO AVENDAÑO", "TRINIDAD ZAACHILA",
                  "ZIMATLAN DE ALVAREZ", "SANTA MARIA JALAPA DEL MARQUES", "SANTA MARIA MIXTEQUILLA",
                  "SANTO DOMINGO TEHUANTEPEC", "HEROICA CIUDAD DE EJUTLA DE CRESPO")

cc_pvem_panal <- c("SANTA MARIA PETAPA", "SAN ANTONINO CASTILLO VELASCO", "SANTA ANA ZEGACHE",
                   "SANTO DOMINGO CHIHUITAN", "PINOTEPA DE DON LUIS")

cc_prd_mc <- "SAN FELIPE USILA"
cc_prd_mc_pup <- "EL ESPINAL"

df_2018 <- df_2018 %>%
  mutate(
    coalition_pan_prd_mc = !(municipality %in% no_pan_prd_mc),
    coalition_pri_pvem_panal = !(municipality %in% no_pri_pvem_panal),
    coalition_pt_morena_pes = !(municipality %in% no_pt_morena_pes),
    cc_pri_pvem = municipality %in% cc_pri_pvem,
    cc_pri_panal = municipality %in% cc_pri_panal,
    cc_pvem_panal = municipality %in% cc_pvem_panal,
    cc_prd_mc = municipality == cc_prd_mc,
    cc_prd_mc_pup = municipality == cc_prd_mc_pup
  )

# Process coalitions per SALVADOR
df_2018 <- df_2018 %>%
  rowwise() %>%
  mutate(
    PAN_PRD_MC = if(coalition_pan_prd_mc) sum(c(PAN, PRD, MC, COAL_PAN_PRD_MC, CC_PAN_PRD_MC), na.rm = TRUE) else NA_real_,
    PRI_PVEM_PANAL = if(coalition_pri_pvem_panal) sum(c(PRI, PVEM, PANAL, COAL_PRI_PVEM_PANAL, CC_PRI_PVEM_NA, CC_PRI_PVEM, CC_PRI_NA, CC_PVEM_NA), na.rm = TRUE) else NA_real_,
    PRI_PVEM_sub = if(cc_pri_pvem) sum(c(PRI, PVEM, CC_PRI_PVEM), na.rm = TRUE) else NA_real_,
    PRI_PANAL_sub = if(cc_pri_panal) sum(c(PRI, PANAL, CC_PRI_NA), na.rm = TRUE) else NA_real_,
    PVEM_PANAL_sub = if(cc_pvem_panal) sum(c(PANAL, PVEM, CC_PVEM_NA), na.rm = TRUE) else NA_real_,
    PT_MORENA_PES = if(coalition_pt_morena_pes) sum(c(PT, MORENA, PES, COAL_PT_MORENA_PES), na.rm = TRUE) else NA_real_,
    PRD_MC_PUP = if(cc_prd_mc_pup) sum(c(PRD, MC, PUP, CC_PRD_MC_PUP), na.rm = TRUE) else NA_real_,
    PRD_MC_sub = if(cc_prd_mc) sum(c(PRD, MC, CC_PRD_MC), na.rm = TRUE) else NA_real_
  ) %>%
  ungroup() %>%
  # Zero out absorbed party columns
  mutate(
    PAN = if_else(coalition_pan_prd_mc, NA_real_, PAN),
    PRD = if_else(coalition_pan_prd_mc | cc_prd_mc | cc_prd_mc_pup, NA_real_, PRD),
    MC = if_else(coalition_pan_prd_mc | cc_prd_mc | cc_prd_mc_pup, NA_real_, MC),
    PRI = if_else(coalition_pri_pvem_panal | cc_pri_pvem | cc_pri_panal, NA_real_, PRI),
    PVEM = if_else(coalition_pri_pvem_panal | cc_pri_pvem | cc_pvem_panal, NA_real_, PVEM),
    PANAL = if_else(coalition_pri_pvem_panal | cc_pri_panal | cc_pvem_panal, NA_real_, PANAL),
    PUP = if_else(cc_prd_mc_pup, NA_real_, PUP)
  )

# Rename municipality from INEGI column if available
if ("munname_INEGI" %in% names(df_2018)) {
  df_2018 <- df_2018 %>%
    mutate(municipality = coalesce(munname_INEGI, municipality)) %>%
    select(-munname_INEGI)
}

# Handle independent candidates (CI columns)
ci_cols <- names(df_2018)[grepl("^CI", names(df_2018))]
if (length(ci_cols) > 0) {
  df_2018 <- df_2018 %>%
    mutate(
      CI_1 = rowSums(select(., matches("^CI")), na.rm = TRUE)
    )
}

# Collapse
df_2018 <- df_2018 %>%
  rename(total = TOTAL) %>%
  select(municipality, uniqueid, section, any_of(c("PAN", "PRI", "PRD", "PVEM", "MC", "PUP", 
         "PANAL", "PSD", "PMR", "PAN_PRD_MC", "PRI_PVEM_PANAL", "PT_MORENA_PES",
         "PRD_MC_PUP", "PRI_PVEM_sub", "PRI_PANAL_sub", "PVEM_PANAL_sub", "PRD_MC_sub",
         "CI_1", "CI_2", "CI_3", "listanominal", "total"))) %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

# Rename sub-coalitions
df_2018 <- df_2018 %>%
  rename_with(~gsub("_sub$", "", .), ends_with("_sub"))

# Calculate valid and finalize
df_2018 <- df_2018 %>%
  mutate(
    valid = rowSums(select(., where(is.numeric), -section, -uniqueid, -listanominal, -total), na.rm = TRUE),
    turnout = total / listanominal,
    year = 2018, month = "July", STATE = "OAXACA"
  )

cat("2018:", nrow(df_2018), "rows\n")

################################################################################
# 2018 EXTRAORDINARY ELECTION - SALVADOR lines 761-813
# San Juan Ihualtepec
################################################################################

df_2018_ext <- read_excel(
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017/EXTRAORDINARIO_2018.xlsx",
  sheet = "COMPAYU20020",
  range = "A3:AH5",
  col_names = TRUE
)

df_2018_ext <- df_2018_ext %>%
  rename(section = SECCION, listanominal = LISTA_NOMINAL_CASILLA, total = TOTAL_VOTOS) %>%
  mutate(
    municipality = "SAN JUAN IHUALTEPEC EXTRAORDINARIO",
    uniqueid = 20199,
    # Create PRI_PVEM_PANAL per SALVADOR
    PRI_PVEM_PANAL = rowSums(select(., any_of(c("PRI", "PVEM", "NA", "CANDIDATURACOMÚNPRIPVEMNA",
                                                 "PRIPVEM", "PRINA", "PVEMNA"))), na.rm = TRUE)
  ) %>%
  select(municipality, uniqueid, section, PAN, PRI_PVEM_PANAL, MORENA, listanominal, total) %>%
  mutate(across(c(PAN, PRI_PVEM_PANAL, MORENA, listanominal, total, section), as.numeric)) %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PAN, PRI_PVEM_PANAL, MORENA, listanominal, total), sum, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(
    valid = rowSums(select(., PAN, PRI_PVEM_PANAL, MORENA), na.rm = TRUE),
    turnout = total / listanominal,
    year = 2018, month = "December", STATE = "OAXACA"
  )

cat("2018_EXT:", nrow(df_2018_ext), "rows\n")

################################################################################
# FINAL APPEND AND OUTPUT
################################################################################

# Combine all years
oaxaca_all <- bind_rows(
  df_1998, df_2001, df_2004, df_2007, df_2010,
  df_2013, df_2014, df_2016, df_2017, df_2018, df_2018_ext
)

# Final corrections per SALVADOR
oaxaca_all <- oaxaca_all %>%
  mutate(
    uniqueid = if_else(municipality == "SANTIAGO NILTEPEC", 20066, uniqueid),
    municipality = toupper(municipality)
  )

# Order columns
oaxaca_all <- oaxaca_all %>%
  select(STATE, municipality, section, uniqueid, year, month, everything())

cat("\nFinal dataset:", nrow(oaxaca_all), "rows\n")
cat("Years:", paste(sort(unique(oaxaca_all$year)), collapse = ", "), "\n")

# Write output
data.table::fwrite(oaxaca_all, "../../../Processed Data/oaxaca/oaxaca_process_raw_data.csv")

cat("Output saved to: ../../../Processed Data/oaxaca/oaxaca_process_raw_data.csv\n")
