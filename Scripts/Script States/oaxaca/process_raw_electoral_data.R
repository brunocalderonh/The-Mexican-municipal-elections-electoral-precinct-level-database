################################################################################
# OAXACA ELECTORAL DATA PROCESSING PIPELINE
# Complete Production-Ready R
# Years: 1998, 2001, 2004, 2007, 2010, 2013, 2014, 2016, 2017, 2018, 2018_EXT, 2021, 2022_EXT, 2024
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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Concejales 1998.xls",
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
    section = as.numeric(section),
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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Concejales 2001.xls",
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
    section = as.numeric(section),
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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Ayu_Seccion_2004.csv",
  show_col_types = FALSE
) %>% 
  rename(listanominal = "LISTA NOMINAL")

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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Ayu_Seccion_2007.csv",
  show_col_types = FALSE
)  %>% 
  rename(listanominal = "LISTA NOMINAL")

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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Other/Ayu_Seccion_2010.dta"
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
sheets_1_14 <- excel_sheets("../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Votacion Concejales 2013 I_XIV.xlsx")
sheets_15_25 <- excel_sheets("../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Votacion Concejales 2013 XV_XXV.xlsx")

df_2013_list <- list()

# Process first file (Districts I-XIV)
for (sheet in sheets_1_14) {
  temp <- read_excel(
    "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Votacion Concejales 2013 I_XIV.xlsx",
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
    "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Votacion Concejales 2013 XV_XXV.xlsx",
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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Extraordina Tlacamama 2014.xlsx",
  sheet = "Extraordinaria",
  range = "A2:R8",
  col_names = TRUE
)

# Robust column handling: use position-based references for unreliable names
# Columns: A=Municipio, B=Sección, ..., H=ListaNominal, I=Coalición, 
# J=Votación_partidos, K(=...10)=unnamed, L(=...11)=PUP, ..., R=VotaciónTotal
col_names_2014 <- names(df_2014)

# Find section column (B) - matches "Secci" regardless of encoding
section_col <- grep("ecci", col_names_2014, value = TRUE)[1]
# Find listanominal (H) - matches "Lista" or "Nominal"
ln_col <- grep("ista.*ominal|ominal", col_names_2014, value = TRUE)[1]
# Find total (R, last column) - matches "Total"
total_col <- grep("otal$", col_names_2014, value = TRUE)
total_col <- total_col[length(total_col)]  # last match
# Coalición (I) - matches "oalici"
coal_col <- grep("oalici", col_names_2014, value = TRUE)[1]
# Votación partidos (J) - matches "artidos" or "pol"
vot_col <- grep("artidos|pol", col_names_2014, value = TRUE)[1]
# Unnamed columns (K = ...10, L = ...11)
col_10 <- grep("^\\.{3}10$", col_names_2014, value = TRUE)
if (length(col_10) == 0) col_10 <- col_names_2014[10]  # fallback to position
else col_10 <- col_10[1]
col_11 <- grep("^\\.{3}11$", col_names_2014, value = TRUE)
if (length(col_11) == 0) col_11 <- col_names_2014[11]
else col_11 <- col_11[1]

df_2014 <- df_2014 %>%
  rename(section = !!section_col, listanominal = !!ln_col, total = !!total_col) %>%
  filter(!is.na(section)) %>%
  mutate(across(everything(), ~suppressWarnings(as.numeric(as.character(.))))) %>%
  mutate(
    # Create PRI_PVEM coalition per SALVADOR: Coalición + Votación_partidos + col_10
    PRI_PVEM = .data[[coal_col]] + .data[[vot_col]] + .data[[col_10]],
    PUP = .data[[col_11]],
    municipality = "SAN MIGUEL TLACAMAMA EXTRAORDINARIO",
    uniqueid = 20285
  ) %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PRI_PVEM, PUP, listanominal, total), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    valid = PRI_PVEM + PUP,
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
      "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Concejales_2016.xlsx",
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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/uniqueids.xlsx",
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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/EXTRAORDINARIO_2017.xlsx",
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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/Concejales_2018.xlsx",
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
  # Zero out absorbed party columns per SALVADOR
  mutate(
    PAN = if_else(coalition_pan_prd_mc, NA_real_, PAN),
    PRD = if_else(coalition_pan_prd_mc | cc_prd_mc | cc_prd_mc_pup, NA_real_, PRD),
    MC = if_else(coalition_pan_prd_mc | cc_prd_mc | cc_prd_mc_pup, NA_real_, MC),
    PRI = if_else(coalition_pri_pvem_panal | cc_pri_pvem | cc_pri_panal, NA_real_, PRI),
    PVEM = if_else(coalition_pri_pvem_panal | cc_pri_pvem | cc_pvem_panal, NA_real_, PVEM),
    PANAL = if_else(coalition_pri_pvem_panal | cc_pri_panal | cc_pvem_panal, NA_real_, PANAL),
    PUP = if_else(cc_prd_mc_pup, NA_real_, PUP),
    # SALVADOR: drop PT MORENA PES (after creating PT_MORENA_PES coalition)
    PT = NA_real_,
    MORENA = NA_real_,
    PES = NA_real_
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
  "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/EXTRAORDINARIO_2018.xlsx",
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


### PROCESSING DATA FOR 2021 -------
#####################################

# Process and consolidate each separate file:
library(readxl)
library(dplyr)
library(purrr)

# Define the base path
base_path <- "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/21"

# Function to read a single district file
read_district <- function(district_num, base_path) {
  
  # Construct the file path
  file_path <- file.path(base_path, paste0("Dtto ", district_num, ".xlsx"))
  
  # Check if file exists
  if (!file.exists(file_path)) {
    message(paste("File not found:", file_path))
    return(NULL)
  }
  
  # Get all sheet names (municipalities)
  sheet_names <- excel_sheets(file_path)
  
  # Read each sheet and combine
  district_data <- map_dfr(sheet_names, function(sheet) {
    tryCatch({
      # Read from row 3 onwards
      data <- read_excel(file_path, sheet = sheet, skip = 2)
      
      # Add district and municipality identifiers
      data <- data %>%
        mutate(
          District = district_num,
          Municipality = sheet,
          .before = 1
        )
      
      return(data)
    }, error = function(e) {
      message(paste("Error reading sheet", sheet, "from district", district_num, ":", e$message))
      return(NULL)
    })
  })
  
  return(district_data)
}

# Process all 25 districts
districts_to_process <- setdiff(1:25, 10)
all_districts <- map_dfr(districts_to_process, ~read_district(.x, base_path))

# View the combined data
print(paste("Total rows:", nrow(all_districts)))
print(paste("Districts processed:", length(unique(all_districts$District))))

# Save the combined dataset
write.csv(all_districts, "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/oaxaca_combined_21.csv", row.names = FALSE)

# Load the 2021 data
data_2021 <- read_csv("../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/oaxaca_combined_21.csv")

# Define base path
base_path <- "../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/22 ext"

# Read each file
ext_01 <- read_excel(file.path(base_path, "COMPAYU01520.xlsx"), skip = 2)
ext_02 <- read_excel(file.path(base_path, "COMPAYU07220.xlsx"), skip = 2)
ext_03 <- read_excel(file.path(base_path, "COMPAYU29620.xlsx"), skip = 2)
ext_04 <- read_excel(file.path(base_path, "COMPAYU38620.xlsx"), skip = 2)
ext_05 <- read_excel(file.path(base_path, "COMPAYU42220.xlsx"), skip = 2)
ext_06 <- read_excel(file.path(base_path, "COMPAYU44220.xlsx"), skip = 2)
ext_07 <- read_excel(file.path(base_path, "COMPAYU47320.xlsx"), skip = 2)

data_2021 <- bind_rows(data_2021, ext_01, ext_02, ext_03, ext_04, ext_05, ext_06, ext_07)

# Rename columns
library(stringr)

data_2021 <- data_2021 %>%
  select(-"COALICION PAN_PRI_PRD") %>% 
  dplyr::rename(
    municipality = MUNICIPIO_LOCAL,
    section = SECCION,
    listanominal = LISTA_NOMINAL_CASILLA,
    total = TOTAL_VOTOS,
    no_reg = NO_REGISTRADOS,
    nulos = NUM_VOTOS_NULOS,
    valid = NUMERO_VOTOS_VALIDOS,
    PANAL = NAO,
    CC_PT_PVEM_PUP_MORENA = `CANDIDATURA COMÚN PT-PVEM-PUP-MORENA`
  ) %>%
  dplyr::mutate(
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(section)
  ) %>% 
  dplyr::filter(section > 0) %>%
  # Fix column names
  rename_with(~ gsub("NAO", "PANAL", .x)) %>%
  rename_with(~ gsub("COMÃ\u009aN", "COMUN", .x)) %>% 
  rename_with(~ gsub("COMÃ\\u009aN", "COMUN", .x)) %>%
  rename_with(~ gsub("^CANDIDATURA COMUN ", "CC_", .x)) %>% 
  rename_with(~ gsub("CI_", "CI ", .x)) %>% 
  rename_with(~ gsub("-", "_", .x))

# Rename CI columns to CI_1, CI_2, etc.
ci_cols <- grep("^CI ", names(data_2021), value = TRUE)

# Correct way: new names = old names
ci_rename <- setNames(ci_cols, paste0("CI_", seq_along(ci_cols)))

data_2021 <- data_2021 %>%
  rename(!!!ci_rename)

# Define specific order for party columns
individual_parties <- c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "MORENA", "PANAL", "PES", "FXM", "PUP", "RSP")

# Get coalition columns
coalition_cols <- grep("^(PAN|PRI|PRD|PT|PVEM|PUP)_", names(data_2021), value = TRUE)
cc_cols <- grep("^CC_", names(data_2021), value = TRUE)
ci_cols <- grep("^CI_\\d+$", names(data_2021), value = TRUE)

# Reorder with specific ordering
data_2021 <- data_2021 %>%
  select(
    municipality,
    section,
    # Parties first (in order, only those that exist)
    any_of(individual_parties),
    # Then coalitions without CC
    all_of(coalition_cols),
    # Then CC coalitions
    all_of(cc_cols),
    # Then independent candidates
    all_of(ci_cols),
    # Then vote totals
    nulos,
    no_reg,
    total,
    valid,
    listanominal,
    # Everything else
    everything()
  )

# Deal with candidatura comun
data_2021 <- data_2021 %>%
  mutate(
    CC_PAN_PRI_PRD = coalesce(CC_PAN_PRI_PRD, `CANDIDATURA COMÚN PAN_PRI_PRD`),
    CC_PAN_PANAL = coalesce(CC_PAN_PANAL, `CANDIDATURA COMÚN PAN_PANAL`),
    CC_PRI_PRD = coalesce(CC_PRI_PRD, `CANDIDATURA COMÚN PRI_PRD`),
    CC_PAN_PRI_PRD_PANAL = coalesce(CC_PAN_PRI_PRD_PANAL, `CANDIDATURA COMÚN PAN_PRI_PRD_PANAL`),
    CC_PAN_PRI = coalesce(CC_PAN_PRI, `CANDIDATURA COMÚN PAN_PRI`)
  ) %>%
  # Remove all "CANDIDATURA COMÚN" columns
  select(-starts_with("CANDIDATURA COMÚN"))

# Assign uniqueids
uniqueid_2024 <- read_excel("../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/uniqueids_2024.xlsx", skip = 3) %>% 
  select(CVEGEO, NOM_MUN)

uniqueid_2024 <- uniqueid_2024 %>% 
  dplyr::rename(
    municipality = NOM_MUN,
    uniqueid = CVEGEO) %>% 
  dplyr::mutate(
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    uniqueid = as.numeric(uniqueid)
  )

data_2021 <- left_join(data_2021, uniqueid_2024, by = "municipality") %>% 
  mutate(uniqueid = case_when(
    municipality == "VILLA DE TUTUTEPEC DE MELCHOR OCAMPO" ~ 20334,
    str_detect(municipality, "MATIAS ROMERO") ~ 20057,
    municipality == "SAN BLAS ATEMPA" ~ 20124,
    TRUE ~ uniqueid
  ))

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:listanominal), 
                          sum, na.rm = TRUE))

# Add relevant data
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    year = case_when(
      municipality %in% c("SANTIAGO LAOLLAGA", "CHAHUITES", "REFORMA DE PINEDA", 
                          "SANTA MARIA XADANI", "SANTA MARIA MIXTEQUILLA",
                          "SAN PABLO VILLA DE MITLA", "SANTA CRUZ XOXOCOTLAN") ~ 2022,
      TRUE ~ 2021
    ),
    month = case_when(
      municipality %in% c("SANTIAGO LAOLLAGA", "CHAHUITES", "REFORMA DE PINEDA", 
                          "SANTA MARIA XADANI", "SANTA MARIA MIXTEQUILLA",
                          "SAN PABLO VILLA DE MITLA", "SANTA CRUZ XOXOCOTLAN") ~ "March",
      TRUE ~ "June"
    ),
    STATE = "OAXACA"
  )

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 20) %>% 
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
  
  # Helper: extract party names from a column name (handles all CC/CO variations)
  extract_parties <- function(col_name) {
    # Remove CC_ or CO_ prefix if present
    col_name <- sub("^(CC|CO)_", "", col_name)
    # Remove _CO or _CC suffix if present
    col_name <- sub("_(CO|CC)$", "", col_name)
    # Split by underscore
    strsplit(col_name, "_")[[1]]
  }
  
  # Helper: find columns belonging to a coalition
  get_coalition_cols <- function(coal_name) {
    coal_parties <- strsplit(coal_name, "_")[[1]]
    party_cols[sapply(party_cols, function(col) {
      col_parties <- extract_parties(col)
      # Check if all parties in the column match the coalition parties
      setequal(col_parties, coal_parties)
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

# Load 2024 data
data_2024 <- read_excel("../../../Data/Raw Electoral Data/Oaxaca - 1998, 2001, 2004, 2007, 2010,2013,2017,2021,2024/24/ayuntamientos2024.xlsx", skip =1)

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(
    municipality = Municipio,
    listanominal = Lista_nom,
    total = Total,
    no_reg = NO_REG,
    nulos = NULOS,
    PANAL = NAO,
    FXM = FXMO,
    CI7 = CI_INDIG_1
  ) %>%
  dplyr::mutate(
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(gsub("^(\\d+).*", "\\1", Casilla))
  ) %>% 
  rename_with(~ gsub("-", "_", .x)) %>%
  rename_with(~ gsub("NAO", "PANAL", .x)) %>%
  rename_with(~ gsub("FXMO", "FXM", .x)) %>%
  rename_with(~ gsub("CI", "CI_", .x)) %>%
  rename_with(~ gsub("CANDIDATURA COMÚN ", "CC_", .x)) %>%
  filter(section > 0) %>% 
  # Handle the ...number pattern and duplicates together
  {
    # First, create clean names by removing ...number
    clean_names <- gsub("\\.{3}\\d+$", "", names(.))
    
    # Find which names will be duplicated
    unique_names <- unique(clean_names)
    
    # For each unique name, sum all columns that will have that name
    map_dfc(unique_names, function(col_name) {
      # Find positions of columns that match this clean name
      col_positions <- which(clean_names == col_name)
      
      if(length(col_positions) == 1) {
        # Single column, just select it
        select(., all_of(col_positions))
      } else {
        # Multiple columns will have same name after cleaning, sum them
        tibble(!!col_name := rowSums(select(., all_of(col_positions)), na.rm = TRUE))
      }
    })
  }

# Assign uniqueids
data_2024 <- left_join(data_2024, uniqueid_2024, by = "municipality") %>% 
  mutate(uniqueid = case_when(
    municipality == "VILLA DE TUTUTEPEC DE MELCHOR OCAMPO" ~ 20334,
    str_detect(municipality, "MATIAS ROMERO") ~ 20057,
    municipality == "SAN BLAS ATEMPA" ~ 20124,
    TRUE ~ uniqueid)
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(listanominal,PAN:total), 
                          sum, na.rm = TRUE))

# Add relevant data
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    valid = rowSums(across(PAN:CC_PANAL_FXM), na.rm = TRUE),
    turnout = total/listanominal,
    year = 2024,
    month = "June",
    STATE = "OAXACA"
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

################################################################################
# FINAL APPEND AND OUTPUT
################################################################################

# Combine all years
oaxaca_all <- bind_rows(
  df_1998, df_2001, df_2004, df_2007, df_2010,
  df_2013, df_2014, df_2016, df_2017, df_2018, df_2018_ext,
  collapsed_2021, collapsed_2024
)

# Final corrections per SALVADOR and JOHN
oaxaca_all <- oaxaca_all %>%
  filter(section != 416) %>%  # JOHN: drop duplicated section 416
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