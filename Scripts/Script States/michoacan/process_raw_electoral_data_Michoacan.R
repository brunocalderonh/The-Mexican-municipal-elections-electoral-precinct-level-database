###############################################################################
### process_raw_electoral_data_Michoacan.R
### Full pipeline: MICHOACAN municipal election data processing
### Election years: 1995, 1998, 2001, 2004, 2005, 2007, 2008, 2011, 2012, 2015, 2018
### Data path: state_folder/YYYY/filename (year-based subfolder structure)
###############################################################################

rm(list = ls())
cat("\014")
options(max.print = 5000, scipen = 10)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, haven, readstata13, readxl, tidyverse, tidyr,
               openxlsx, data.table)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(file.path(script_dir, ""))

# --- PATH CONFIGURATION ---
raw_data_path <- "../../../Data/Raw Electoral Data/Michoacan"
ln_path       <- "../../../Data/Raw Electoral Data/Listas Nominales"
processed_path <- "../../../Processed Data/michoacan"

STATE_NAME <- "MICHOACAN"
state_ed   <- 16  # INEGI state code

# Helper function: remove accents
remove_accents <- function(x) {
  x <- gsub("\u00c1|\u00e1", "A", x)
  x <- gsub("\u00c9|\u00e9", "E", x)
  x <- gsub("\u00cd|\u00ed", "I", x)
  x <- gsub("\u00d3|\u00f3", "O", x)
  x <- gsub("\u00da|\u00fa", "U", x)
  x <- gsub("\u00dc|\u00fc", "U", x)
  x <- gsub("\u00d1|\u00f1", "N", x)
  return(x)
}

cat(paste0("=========================================\n"))
cat(paste0("Processing ", STATE_NAME, " electoral data\n"))
cat(paste0("Election years: 1995, 1998, 2001, 2004, 2005, 2007, 2008, 2011, 2012, 2015, 2018\n"))
cat(paste0("=========================================\n"))

# Load all_months_years LN data (used by multiple years)
ln_data <- tryCatch(
  read_dta(file.path(ln_path, "all_months_years.dta")),
  error = function(e) {
    cat("WARNING: Could not load all_months_years.dta\n")
    data.frame()
  }
)

# =============================================================================
# MUNICIPALITY -> UNIQUEID MAPPING
# All name variants across all election years
# =============================================================================
assign_uniqueid <- function(municipality) {
  dplyr::case_when(
    municipality == "ACUITZIO" ~ 16001,
    municipality == "Acuitzio" ~ 16001,
    municipality == "AGUILILLA" ~ 16002,
    municipality == "Aguililla" ~ 16002,
    municipality == "A. OBREGON." ~ 16003,
    municipality == "Alvaro Obregon" ~ 16003,
    municipality == "Alvaro ObregÃ³n" ~ 16003,
    municipality == "ÃLvaro ObregÃ“N" ~ 16003,
    municipality == "ANGAMACUTIRO" ~ 16004,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "ANGANGUEO" ~ 16005,
    municipality == "Angangueo" ~ 16005,
    municipality == "APATZINGAN" ~ 16006,
    municipality == "Apatzingan" ~ 16006,
    municipality == "ApatzingÃN" ~ 16006,
    municipality == "ApatzingÃ¡n" ~ 16006,
    municipality == "APORO" ~ 16007,
    municipality == "Aporo" ~ 16007,
    municipality == "ÃPoro" ~ 16007,
    municipality == "AQUILA" ~ 16008,
    municipality == "Aquila" ~ 16008,
    municipality == "ARIO DE R." ~ 16009,
    municipality == "Ario" ~ 16009,
    municipality == "ARTEAGA" ~ 16010,
    municipality == "Arteaga" ~ 16010,
    municipality == "Brisenas" ~ 16011,
    municipality == "BriseÃ±as" ~ 16011,
    municipality == "BriseÃ‘As" ~ 16011,
    municipality == "BUENAVISTA" ~ 16012,
    municipality == "Buenavista" ~ 16012,
    municipality == "CARACUARO" ~ 16013,
    municipality == "Caracuaro" ~ 16013,
    municipality == "CarÃCuaro" ~ 16013,
    municipality == "CarÃ¡cuaro" ~ 16013,
    municipality == "COAHUAYANA" ~ 16014,
    municipality == "Coahuayana" ~ 16014,
    municipality == "COALCOMAN" ~ 16015,
    municipality == "Coalcoman" ~ 16015,
    municipality == "Coalcoman De Vazquez Pallares" ~ 16015,
    municipality == "CoalcomÃN De Vazquez Pallares" ~ 16015,
    municipality == "CoalcomÃ¡n" ~ 16015,
    municipality == "COENEO" ~ 16016,
    municipality == "Coeneo" ~ 16016,
    municipality == "CONTEPEC" ~ 16017,
    municipality == "Contepec" ~ 16017,
    municipality == "COPANDARO" ~ 16018,
    municipality == "Copandaro" ~ 16018,
    municipality == "CopÃNdaro" ~ 16018,
    municipality == "CopÃ¡ndaro" ~ 16018,
    municipality == "COTIJA" ~ 16019,
    municipality == "Cotija" ~ 16019,
    municipality == "CUITZEO" ~ 16020,
    municipality == "Cuitzeo" ~ 16020,
    municipality == "CHARAPAN" ~ 16021,
    municipality == "Charapan" ~ 16021,
    municipality == "CHARO" ~ 16022,
    municipality == "Charo" ~ 16022,
    municipality == "CHAVINDA" ~ 16023,
    municipality == "Chavinda" ~ 16023,
    municipality == "CHERAN" ~ 16024,
    municipality == "Cheran" ~ 16024,
    municipality == "CherÃ¡n" ~ 16024,
    municipality == "CHILCHOTA" ~ 16025,
    municipality == "Chilchota" ~ 16025,
    municipality == "CHINICUILA" ~ 16026,
    municipality == "Chinicuila" ~ 16026,
    municipality == "CHUCANDIRO" ~ 16027,
    municipality == "Chucandiro" ~ 16027,
    municipality == "ChucÃNdiro" ~ 16027,
    municipality == "ChucÃ¡ndiro" ~ 16027,
    municipality == "CHURINTZIO" ~ 16028,
    municipality == "Churintzio" ~ 16028,
    municipality == "CHURUMUCO" ~ 16029,
    municipality == "Churumuco" ~ 16029,
    municipality == "ECUANDUREO" ~ 16030,
    municipality == "Ecuandureo" ~ 16030,
    municipality == "E. HUERTA" ~ 16031,
    municipality == "Epitacio Huerta" ~ 16031,
    municipality == "ERONGARI." ~ 16032,
    municipality == "Erongaricuaro" ~ 16032,
    municipality == "ErongarÃCuaro" ~ 16032,
    municipality == "ErongarÃ­cuaro" ~ 16032,
    municipality == "GABRIEL ZAMORA" ~ 16033,
    municipality == "Gabriel Zamora" ~ 16033,
    municipality == "HIDALGO" ~ 16034,
    municipality == "Hidalgo" ~ 16034,
    municipality == "LA HUACANA" ~ 16035,
    municipality == "La Huacana" ~ 16035,
    municipality == "HUANDACAREO" ~ 16036,
    municipality == "Huandacareo" ~ 16036,
    municipality == "HUANIQUEO" ~ 16037,
    municipality == "Huaniqueo" ~ 16037,
    municipality == "HuanÃQueo" ~ 16037,
    municipality == "HuanÃ­queo" ~ 16037,
    municipality == "HUETAMO" ~ 16038,
    municipality == "Huatamo" ~ 16038,
    municipality == "Huetamo" ~ 16038,
    municipality == "HUIRAMBA" ~ 16039,
    municipality == "Huiramba" ~ 16039,
    municipality == "INDAPARAPEO" ~ 16040,
    municipality == "Indaparapeo" ~ 16040,
    municipality == "IRIMBO" ~ 16041,
    municipality == "Irimbo" ~ 16041,
    municipality == "IXTLAN" ~ 16042,
    municipality == "Ixtlan" ~ 16042,
    municipality == "IxtlÃN" ~ 16042,
    municipality == "IxtlÃ¡n" ~ 16042,
    municipality == "JACONA" ~ 16043,
    municipality == "Jacona" ~ 16043,
    municipality == "JIMENEZ" ~ 16044,
    municipality == "Jimenez" ~ 16044,
    municipality == "JimÃ©nez" ~ 16044,
    municipality == "JimÃ‰Nez" ~ 16044,
    municipality == "JIQUILPAN" ~ 16045,
    municipality == "Jiquilpan" ~ 16045,
    municipality == "JUAREZ" ~ 16046,
    municipality == "Juarez" ~ 16046,
    municipality == "JuÃRez" ~ 16046,
    municipality == "JuÃ¡rez" ~ 16046,
    municipality == "JUNGAPEO" ~ 16047,
    municipality == "Juagapeo" ~ 16047,
    municipality == "Jungapeo" ~ 16047,
    municipality == "LAGUNILLAS" ~ 16048,
    municipality == "Lagunillas" ~ 16048,
    municipality == "MADERO" ~ 16049,
    municipality == "Madero" ~ 16049,
    municipality == "MARAVATIO" ~ 16050,
    municipality == "Maravatio" ~ 16050,
    municipality == "MaravatÃO" ~ 16050,
    municipality == "M.CASTELLANOS" ~ 16051,
    municipality == "Marcos Castellanos" ~ 16051,
    municipality == "L. CARDENAS" ~ 16052,
    municipality == "Lazaro Cardenas" ~ 16052,
    municipality == "LÃZaro CÃRdenas" ~ 16052,
    municipality == "LÃ¡zaro CÃ¡rdenas" ~ 16052,
    municipality == "MORELIA" ~ 16053,
    municipality == "Morelia" ~ 16053,
    municipality == "MORELOS" ~ 16054,
    municipality == "Morelos" ~ 16054,
    municipality == "MUGICA" ~ 16055,
    municipality == "Mugica" ~ 16055,
    municipality == "MÃºgica" ~ 16055,
    municipality == "MÃšGica" ~ 16055,
    municipality == "NAHUATZEN" ~ 16056,
    municipality == "Nahuatzen" ~ 16056,
    municipality == "NahuÃTzen" ~ 16056,
    municipality == "NahuÃ¡tzen" ~ 16056,
    municipality == "NOCUPETARO" ~ 16057,
    municipality == "Nocupetaro" ~ 16057,
    municipality == "NocupÃ©taro" ~ 16057,
    municipality == "NocupÃ‰Taro" ~ 16057,
    municipality == "NUEVO PARANGARI" ~ 16058,
    municipality == "Nuevo Parangaricutiro" ~ 16058,
    municipality == "NUEVO URECHO" ~ 16059,
    municipality == "Nuevo Urecho" ~ 16059,
    municipality == "NUMARAN" ~ 16060,
    municipality == "Numaran" ~ 16060,
    municipality == "NumarÃN" ~ 16060,
    municipality == "NumarÃ¡n" ~ 16060,
    municipality == "OCAMPO" ~ 16061,
    municipality == "Ocampo" ~ 16061,
    municipality == "PAJACUARAN" ~ 16062,
    municipality == "Pajacuaran" ~ 16062,
    municipality == "PajacuarÃN" ~ 16062,
    municipality == "PajacuarÃ¡n" ~ 16062,
    municipality == "PANINDICUARO" ~ 16063,
    municipality == "Panindicuaro" ~ 16063,
    municipality == "PanindÃCuaro" ~ 16063,
    municipality == "PanindÃ­cuaro" ~ 16063,
    municipality == "PARACUARO" ~ 16064,
    municipality == "Paracuaro" ~ 16064,
    municipality == "ParÃCuaro" ~ 16064,
    municipality == "ParÃ¡cuaro" ~ 16064,
    municipality == "PARACHO" ~ 16065,
    municipality == "Paracho" ~ 16065,
    municipality == "PATZCUARO" ~ 16066,
    municipality == "Patzcuaro" ~ 16066,
    municipality == "PÃTzcuaro" ~ 16066,
    municipality == "PÃ¡tzcuaro" ~ 16066,
    municipality == "PENJAMILLO" ~ 16067,
    municipality == "Penjamillo" ~ 16067,
    municipality == "PERIBAN" ~ 16068,
    municipality == "Periban" ~ 16068,
    municipality == "PeribÃN" ~ 16068,
    municipality == "PeribÃ¡n" ~ 16068,
    municipality == "LA PIEDAD" ~ 16069,
    municipality == "La Piedad" ~ 16069,
    municipality == "PUREPERO" ~ 16070,
    municipality == "Purepero" ~ 16070,
    municipality == "PurÃ©pero" ~ 16070,
    municipality == "PurÃ‰Pero" ~ 16070,
    municipality == "PURUANDIRO" ~ 16071,
    municipality == "Puruandiro" ~ 16071,
    municipality == "PuruÃNdiro" ~ 16071,
    municipality == "PuruÃ¡ndiro" ~ 16071,
    municipality == "QUERENDARO" ~ 16072,
    municipality == "Querendaro" ~ 16072,
    municipality == "QuerÃ©ndaro" ~ 16072,
    municipality == "QuerÃ‰Ndaro" ~ 16072,
    municipality == "QUIROGA" ~ 16073,
    municipality == "Quiroga" ~ 16073,
    municipality == "Cojumatlan De Regules" ~ 16074,
    municipality == "CojumatlÃN De RÃ‰Gules" ~ 16074,
    municipality == "REGULES" ~ 16074,
    municipality == "Regules" ~ 16074,
    municipality == "RÃ©gules" ~ 16074,
    municipality == "LOS REYES" ~ 16075,
    municipality == "Los Reyes" ~ 16075,
    municipality == "SAHUAYO" ~ 16076,
    municipality == "Sahuayo" ~ 16076,
    municipality == "SAN LUCAS" ~ 16077,
    municipality == "San Lucas" ~ 16077,
    municipality == "STA. ANA M." ~ 16078,
    municipality == "Santa Ana Maya" ~ 16078,
    municipality == "SALV. ESC." ~ 16079,
    municipality == "Salvador Escalante" ~ 16079,
    municipality == "SENGUIO" ~ 16080,
    municipality == "Senguio" ~ 16080,
    municipality == "SUSUPUATO" ~ 16081,
    municipality == "Susupuato" ~ 16081,
    municipality == "TACAMBARO" ~ 16082,
    municipality == "Tacambaro" ~ 16082,
    municipality == "TacÃMbaro" ~ 16082,
    municipality == "TacÃ¡mbaro" ~ 16082,
    municipality == "TANCITARO" ~ 16083,
    municipality == "Tancitaro" ~ 16083,
    municipality == "TancÃTaro" ~ 16083,
    municipality == "TancÃ­taro" ~ 16083,
    municipality == "TANGAMANDAP." ~ 16084,
    municipality == "Tangamandapio" ~ 16084,
    municipality == "TANGANCI" ~ 16085,
    municipality == "Tangancicuaro" ~ 16085,
    municipality == "TangancÃCuaro" ~ 16085,
    municipality == "TangancÃ­cuaro" ~ 16085,
    municipality == "TANHUATO" ~ 16086,
    municipality == "Tanhuato" ~ 16086,
    municipality == "TARETAN" ~ 16087,
    municipality == "Taretan" ~ 16087,
    municipality == "TARIMBARO" ~ 16088,
    municipality == "Tarimbaro" ~ 16088,
    municipality == "TarÃMbaro" ~ 16088,
    municipality == "TarÃ­mbaro" ~ 16088,
    municipality == "TEPALCATEP." ~ 16089,
    municipality == "Tepalcatepec" ~ 16089,
    municipality == "TINGAMBATO" ~ 16090,
    municipality == "Tingambato" ~ 16090,
    municipality == "TINGUINDIN" ~ 16091,
    municipality == "Tinguindin" ~ 16091,
    municipality == "TingÃ¼indÃ­n" ~ 16091,
    municipality == "TingÃœIndin" ~ 16091,
    municipality == "TingÃœIndÃN" ~ 16091,
    municipality == "TIQUICHEO" ~ 16092,
    municipality == "Tiquicheo" ~ 16092,
    municipality == "Tiquicheo De Nicolas Romero" ~ 16092,
    municipality == "TLALPUJAHUA" ~ 16093,
    municipality == "Tlalpujahua" ~ 16093,
    municipality == "TLAZAZALCA" ~ 16094,
    municipality == "Tlazazalca" ~ 16094,
    municipality == "TOCUMBO" ~ 16095,
    municipality == "Tocumbo" ~ 16095,
    municipality == "TUMBISCATIO" ~ 16096,
    municipality == "Tumbiscatio" ~ 16096,
    municipality == "TURICATO" ~ 16097,
    municipality == "Turicato" ~ 16097,
    municipality == "TUXPAN" ~ 16098,
    municipality == "Tuxpan" ~ 16098,
    municipality == "TUZANTLA" ~ 16099,
    municipality == "Tuzantla" ~ 16099,
    municipality == "TZINTZUNTZAN" ~ 16100,
    municipality == "Tzintzuntzan" ~ 16100,
    municipality == "TZITZIO" ~ 16101,
    municipality == "Tzitzio" ~ 16101,
    municipality == "URUAPAN" ~ 16102,
    municipality == "Uruapan" ~ 16102,
    municipality == "V. CARRANZA" ~ 16103,
    municipality == "Venustiano Carranza" ~ 16103,
    municipality == "VILLAMAR" ~ 16104,
    municipality == "Villamar" ~ 16104,
    municipality == "VISTA HERMOSA" ~ 16105,
    municipality == "Vista Hermosa" ~ 16105,
    municipality == "YURECUARO" ~ 16106,
    municipality == "Yurecuaro" ~ 16106,
    municipality == "YurÃ©cuaro" ~ 16106,
    municipality == "YurÃ‰Cuaro" ~ 16106,
    municipality == "ZACAPU" ~ 16107,
    municipality == "Zacapu" ~ 16107,
    municipality == "ZAMORA" ~ 16108,
    municipality == "Zamora" ~ 16108,
    municipality == "ZINAPARO" ~ 16109,
    municipality == "Zinaparo" ~ 16109,
    municipality == "ZinÃParo" ~ 16109,
    municipality == "ZinÃ¡paro" ~ 16109,
    municipality == "ZINAPECUARO" ~ 16110,
    municipality == "Zinapecuaro" ~ 16110,
    municipality == "ZinapÃ©cuaro" ~ 16110,
    municipality == "ZinapÃ‰Cuaro" ~ 16110,
    municipality == "ZIRACUARE" ~ 16111,
    municipality == "Ziracuaretiro" ~ 16111,
    municipality == "ZITACUARO" ~ 16112,
    municipality == "Zitacuaro" ~ 16112,
    municipality == "ZitÃCuaro" ~ 16112,
    municipality == "ZitÃ¡cuaro" ~ 16112,
    municipality == "JOSE SIXTO VERDUZCO" ~ 16113,
    municipality == "Jose Sixto Verduzco" ~ 16113,
    municipality == "JosÃ© Sixto Verduzco" ~ 16113,
    municipality == "JosÃ‰ Sixto Verduzco" ~ 16113,
    TRUE ~ NA_real_
  )
}

###############################################################################
### PROCESSING 1995 ELECTION
###############################################################################
cat("Processing 1995...\n")

data_1995 <- read.csv(file.path(raw_data_path, "1995", "Ayu_Seccion_1995_No_LN.csv"),
                       stringsAsFactors = FALSE)

data_1995 <- data_1995 %>%
  rename(municipality = municipio, section = seccinnom)

data_1995 <- data_1995 %>%
  filter(!(municipality == "" & is.na(section)))
data_1995 <- data_1995 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_1995 <- setdiff(names(data_1995), c("municipality", "section"))
data_1995 <- data_1995 %>%
  mutate(across(all_of(party_cols_1995), ~suppressWarnings(as.numeric(as.character(.)))))

data_1995$municipality <- toupper(trimws(data_1995$municipality))
data_1995$municipality <- remove_accents(data_1995$municipality)

# --- Collapse by municipality section ---
data_1995 <- data_1995 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Rename to standard party names ---
# Raw data has: pan, pri, prd, pfcrn, pt (plus noreg, nulos)
data_1995 <- data_1995 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PartCardenista = pfcrn, PT = pt)

# --- Lista nominal from all_months_years ---
ln_1995 <- ln_data %>%
  filter(ed == state_ed, month == 9, year == 1998) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

# Handle missing LN - use all_months if file LN is missing
data_1995 <- data_1995 %>%
  left_join(ln_1995 %>% rename(lista_all = listanominal), by = "section") %>%
  mutate(listanominal = ifelse(is.na(listanominal) | listanominal == 0,
                                lista_all, listanominal)) %>%
  select(-lista_all)

data_1995 <- data_1995 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_1995 <- data_1995 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_1995 <- data_1995 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_1995 <- data_1995 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PartCardenista, PT)), na.rm = TRUE))

data_1995 <- data_1995 %>%
  mutate(year = 1995, month = "November")

collapsed_1995 <- data_1995
cat(paste0("  1995: ", nrow(collapsed_1995), " obs\n"))

###############################################################################
### PROCESSING 1998 ELECTION
###############################################################################
cat("Processing 1998...
")

# 1998 data: CSV with half-missing sections (LN supplemented from all_months_years)
data_1998 <- read.csv(file.path(raw_data_path, "1998", "Ayu_Seccion_1998_Half_Missing.csv"),
                       stringsAsFactors = FALSE)

data_1998 <- data_1998 %>%
  rename(municipality = municipionom) %>%
  filter(!(is.na(municipality) & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

party_cols_1998 <- setdiff(names(data_1998), c("municipality", "section"))
data_1998 <- data_1998 %>%
  mutate(across(all_of(party_cols_1998), ~suppressWarnings(as.numeric(as.character(.)))))

data_1998$municipality <- toupper(trimws(data_1998$municipality))
data_1998$municipality <- remove_accents(data_1998$municipality)

# Flag missing LN
data_1998 <- data_1998 %>%
  mutate(missing = as.numeric(listanominal == 0 | is.na(listanominal)))

# --- Collapse ---
data_1998 <- data_1998 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Rename ---
data_1998 <- data_1998 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem)

# --- LN from all_months_years (fill missing) ---
ln_1998 <- ln_data %>%
  filter(ed == state_ed, month == 9, year == 1998) %>%
  select(seccion, lista) %>%
  rename( lista_all = lista) %>%
  group_by(section) %>%
  summarise(lista_all = sum(lista_all, na.rm = TRUE), .groups = "drop")

data_1998 <- data_1998 %>%
  left_join(ln_1998, by = "section") %>%
  mutate(listanominal = ifelse(missing >= 1 & !is.na(lista_all), lista_all, listanominal)) %>%
  select(-missing, -lista_all)

data_1998 <- data_1998 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_1998 <- data_1998 %>%
  select(-any_of(c("noregistrados", "nulos")))

# --- Uniqueid: merge from 1995 section-uniqueid mapping ---
# 1998 data may lack municipality names for some sections
# Use the section->uniqueid mapping from 1995 to fill gaps
data_1998 <- data_1998 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# Manual fixes for sections that don't match standard names
data_1998 <- data_1998 %>%
  mutate(
    uniqueid = case_when(
      section == 935 & is.na(uniqueid) ~ 16051,  # M.CASTELLANOS
      section == 2675 & is.na(uniqueid) ~ 16053,  # MORELIA
      TRUE ~ uniqueid
    ),
    municipality = case_when(
      section == 935 & municipality == "" ~ "M.CASTELLANOS",
      section == 2675 & municipality == "" ~ "MORELIA",
      TRUE ~ municipality
    )
  )

# --- Valid votes ---
data_1998 <- data_1998 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM)), na.rm = TRUE))

data_1998 <- data_1998 %>%
  mutate(year = 1998, month = "November")

collapsed_1998 <- data_1998
cat(paste0("  1998: ", nrow(collapsed_1998), " obs
"))

###############################################################################
### PROCESSING 2001 ELECTION
###############################################################################
cat("Processing 2001...\n")

data_2001 <- read.csv(file.path(raw_data_path, "2001", "Ayu_Seccion_2001.csv"),
                       stringsAsFactors = FALSE)

data_2001 <- data_2001 %>%
  rename(municipality = nombre_municipio)

data_2001 <- data_2001 %>%
  filter(!(municipality == "" & is.na(section)))
data_2001 <- data_2001 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2001 <- setdiff(names(data_2001), c("municipality", "section"))
data_2001 <- data_2001 %>%
  mutate(across(all_of(party_cols_2001), ~suppressWarnings(as.numeric(as.character(.)))))

data_2001$municipality <- toupper(trimws(data_2001$municipality))
data_2001$municipality <- remove_accents(data_2001$municipality)

# --- Collapse by municipality section ---
data_2001 <- data_2001 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Rename to standard party names ---
data_2001 <- data_2001 %>%
  rename(PAN = pan, PRI = pri, PRD_PT_PC_PVEM_PAS_PSN = prdptpvempaspsnconvergencia)

data_2001 <- data_2001 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2001 <- data_2001 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2001 <- data_2001 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2001 <- data_2001 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD_PT_PC_PVEM_PAS_PSN)), na.rm = TRUE))

data_2001 <- data_2001 %>%
  mutate(year = 2001, month = "November")

collapsed_2001 <- data_2001
cat(paste0("  2001: ", nrow(collapsed_2001), " obs\n"))

###############################################################################
### PROCESSING 2004 ELECTION
###############################################################################
cat("Processing 2004...\n")

data_2004 <- read.csv(file.path(raw_data_path, "2004", "Ayu_Seccion_2004.csv"),
                       stringsAsFactors = FALSE)

data_2004 <- data_2004 %>%
  rename(municipality = nombre_municipio_nominal)

data_2004 <- data_2004 %>%
  filter(!(municipality == "" & is.na(section)))
data_2004 <- data_2004 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2004 <- setdiff(names(data_2004), c("municipality", "section"))
data_2004 <- data_2004 %>%
  mutate(across(all_of(party_cols_2004), ~suppressWarnings(as.numeric(as.character(.)))))

data_2004$municipality <- toupper(trimws(data_2004$municipality))
data_2004$municipality <- remove_accents(data_2004$municipality)

# --- Collapse by municipality section ---
data_2004 <- data_2004 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Rename to standard party names ---
data_2004 <- data_2004 %>%
  rename(PAN = pan, PRI_PVEM = pripvem, PRD = prd, PT = pt, PC = pc)

data_2004 <- data_2004 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2004 <- data_2004 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2004 <- data_2004 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2004 <- data_2004 %>%
  mutate(valid = rowSums(across(c(PAN, PRI_PVEM, PRD, PT, PC)), na.rm = TRUE))

data_2004 <- data_2004 %>%
  mutate(year = 2004, month = "November")

collapsed_2004 <- data_2004
cat(paste0("  2004: ", nrow(collapsed_2004), " obs\n"))

###############################################################################
### PROCESSING 2005 ELECTION
###############################################################################
cat("Processing 2005...\n")

data_2005 <- read_excel(file.path(raw_data_path, "2005", "resultados_electorales_2005.xls"),
                         sheet = "Ayuntamiento, por Casilla, 2005")

data_2005 <- data_2005 %>%
  rename(listanominal = ListaNominal)

data_2005 <- data_2005 %>%
  filter(!(municipality == "" & is.na(section)))
data_2005 <- data_2005 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2005 <- setdiff(names(data_2005), c("municipality", "section"))
data_2005 <- data_2005 %>%
  mutate(across(all_of(party_cols_2005), ~suppressWarnings(as.numeric(as.character(.)))))

data_2005$municipality <- toupper(trimws(data_2005$municipality))
data_2005$municipality <- remove_accents(data_2005$municipality)

# --- Collapse by uniqueid municipality section ---
data_2005 <- data_2005 %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

data_2005 <- data_2005 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2005 <- data_2005 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2005 <- data_2005 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2005 <- data_2005 %>%
  mutate(valid = rowSums(across(c(PRI, PRD, PT, PC)), na.rm = TRUE))

data_2005 <- data_2005 %>%
  mutate(year = 2005, month = "June")

collapsed_2005 <- data_2005
cat(paste0("  2005: ", nrow(collapsed_2005), " obs\n"))

###############################################################################
### PROCESSING 2007 ELECTION
###############################################################################
cat("Processing 2007...\n")

data_2007 <- read_excel(file.path(raw_data_path, "2007", "Ayu_Seccion_2007.xlsx"))


data_2007 <- data_2007 %>%
  rename(municipality = municipio)

data_2007 <- data_2007 %>%
  filter(!(municipality == "" & is.na(section)))
data_2007 <- data_2007 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2007 <- setdiff(names(data_2007), c("municipality", "section"))
data_2007 <- data_2007 %>%
  mutate(across(all_of(party_cols_2007), ~suppressWarnings(as.numeric(as.character(.)))))

data_2007$municipality <- toupper(trimws(data_2007$municipality))
data_2007$municipality <- remove_accents(data_2007$municipality)

# --- Collapse by municipality section ---
data_2007 <- data_2007 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Compute total ---
data_2007 <- data_2007 %>%
  mutate(total = rowSums(across(c(pan, pri, prd, pt, pvem, pc, pna, pas, coal_prd_pt, coal_prd_pt_pc, coal_prd_pc, cc_pan_pri, cc_pan_pri_pvem, cc_pan_pvem, cc_pan_pvem_pna, cc_pan_pna, cc_pri_pvem, cc_pri_pvem_pna, noreg, nulos)), na.rm = TRUE))
data_2007 <- data_2007 %>% filter(total > 0)

# --- Coalition aggregation + zeroing (replicate Stata logic) ---
# In Stata, where a coalition exists (>0), component votes are ADDED to coalition
# total, then components are zeroed to prevent double-counting.
# Order matters: processed sequentially as in Stata.
data_2007 <- data_2007 %>%
  # coal_prd_pt: just rename (no aggregation in Stata)
  # coal_prd_pt_pc: add prd+pt+pc, then zero them
  mutate(
    coal_prd_pt_pc = ifelse(coal_prd_pt_pc > 0, prd + pt + pc + coal_prd_pt_pc, coal_prd_pt_pc),
    prd = ifelse(coal_prd_pt_pc > 0, 0, prd),
    pt  = ifelse(coal_prd_pt_pc > 0, 0, pt),
    pc  = ifelse(coal_prd_pt_pc > 0, 0, pc)
  ) %>%
  # coal_prd_pc: just rename (no aggregation in Stata)
  # cc_pan_pvem: add pan+pvem
  mutate(
    cc_pan_pvem = ifelse(cc_pan_pvem > 0, cc_pan_pvem + pan + pvem, cc_pan_pvem),
    pan  = ifelse(cc_pan_pvem > 0, 0, pan),
    pvem = ifelse(cc_pan_pvem > 0, 0, pvem)
  ) %>%
  # cc_pan_pvem_pna: add pan+pvem+pna
  mutate(
    cc_pan_pvem_pna = ifelse(cc_pan_pvem_pna > 0, cc_pan_pvem_pna + pan + pvem + pna, cc_pan_pvem_pna),
    pan  = ifelse(cc_pan_pvem_pna > 0, 0, pan),
    pvem = ifelse(cc_pan_pvem_pna > 0, 0, pvem),
    pna  = ifelse(cc_pan_pvem_pna > 0, 0, pna)
  ) %>%
  # cc_pan_pna: add pan+pna
  mutate(
    cc_pan_pna = ifelse(cc_pan_pna > 0, cc_pan_pna + pan + pna, cc_pan_pna),
    pan = ifelse(cc_pan_pna > 0, 0, pan),
    pna = ifelse(cc_pan_pna > 0, 0, pna)
  ) %>%
  # cc_pan_pri: add pan+pri
  mutate(
    cc_pan_pri = ifelse(cc_pan_pri > 0, pan + pri + cc_pan_pri, cc_pan_pri),
    pan = ifelse(cc_pan_pri > 0, 0, pan),
    pri = ifelse(cc_pan_pri > 0, 0, pri)
  ) %>%
  # cc_pan_pri_pvem: add pan+pri+pvem
  mutate(
    cc_pan_pri_pvem = ifelse(cc_pan_pri_pvem > 0, pan + pri + pvem + cc_pan_pri_pvem, cc_pan_pri_pvem),
    pan  = ifelse(cc_pan_pri_pvem > 0, 0, pan),
    pri  = ifelse(cc_pan_pri_pvem > 0, 0, pri),
    pvem = ifelse(cc_pan_pri_pvem > 0, 0, pvem)
  ) %>%
  # cc_pri_pvem: add pri+pvem
  mutate(
    cc_pri_pvem = ifelse(cc_pri_pvem > 0, pri + pvem + cc_pri_pvem, cc_pri_pvem),
    pri  = ifelse(cc_pri_pvem > 0, 0, pri),
    pvem = ifelse(cc_pri_pvem > 0, 0, pvem)
  ) %>%
  # cc_pri_pvem_pna: add pri+pvem+pna
  mutate(
    cc_pri_pvem_pna = ifelse(cc_pri_pvem_pna > 0, pri + pvem + pna + cc_pri_pvem_pna, cc_pri_pvem_pna),
    pri  = ifelse(cc_pri_pvem_pna > 0, 0, pri),
    pvem = ifelse(cc_pri_pvem_pna > 0, 0, pvem),
    pna  = ifelse(cc_pri_pvem_pna > 0, 0, pna)
  )

# --- Rename to standard party names ---
data_2007 <- data_2007 %>%
  rename(prd_pt = coal_prd_pt, prd_pt_pc = coal_prd_pt_pc, prd_pc = coal_prd_pc, pan_pvem = cc_pan_pvem, pan_pvem_pna = cc_pan_pvem_pna, pan_pna = cc_pan_pna, pan_pri = cc_pan_pri, pan_pri_pvem = cc_pan_pri_pvem, pri_pvem = cc_pri_pvem, pri_pvem_pna = cc_pri_pvem_pna, PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc, PANAL = pna, PAS = pas, PRD_PT = prd_pt, PRD_PT_PC = prd_pt_pc, PRD_PC = prd_pc, PAN_PVEM = pan_pvem, PAN_PVEM_PANAL = pan_pvem_pna, PAN_PANAL = pan_pna, PAN_PRI = pan_pri, PAN_PRI_PVEM = pan_pri_pvem, PRI_PVEM = pri_pvem, PRI_PVEM_PANAL = pri_pvem_pna)

data_2007 <- data_2007 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2007 <- data_2007 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2007 <- data_2007 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2007 <- data_2007 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PC, PANAL, PAS, PRD_PT, PRD_PT_PC, PRD_PC, PAN_PRI, PAN_PRI_PVEM, PAN_PVEM, PAN_PVEM_PANAL, PAN_PANAL, PRI_PVEM, PRI_PVEM_PANAL)), na.rm = TRUE))

data_2007 <- data_2007 %>%
  mutate(year = 2007, month = "November")

collapsed_2007 <- data_2007
cat(paste0("  2007: ", nrow(collapsed_2007), " obs\n"))

###############################################################################
### PROCESSING 2008 ELECTION
###############################################################################
cat("Processing 2008...\n")

data_2008 <- read_excel(file.path(raw_data_path, "2008", "resultados_2008.xls"),
                         sheet = "Ayuntamiento YurÃ©cuaro")

data_2008 <- data_2008 %>%
  rename(section = SecciÃ³n)

data_2008 <- data_2008 %>%
  filter(!(municipality == "" & is.na(section)))
data_2008 <- data_2008 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2008 <- setdiff(names(data_2008), c("municipality", "section"))
data_2008 <- data_2008 %>%
  mutate(across(all_of(party_cols_2008), ~suppressWarnings(as.numeric(as.character(.)))))

data_2008$municipality <- toupper(trimws(data_2008$municipality))
data_2008$municipality <- remove_accents(data_2008$municipality)

# --- Collapse by uniqueid municipality section ---
data_2008 <- data_2008 %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Lista nominal from file ---
# LN file: merge.dta
# NOTE: Update path as needed based on your file structure
ln_2008_raw <- read_dta(file.path(raw_data_path, "2008", "merge.dta"))
ln_2008 <- ln_2008_raw %>%
  select(section, listanominal) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")
data_2008 <- data_2008 %>%
  left_join(ln_2008, by = "section")

data_2008 <- data_2008 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2008 <- data_2008 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2008 <- data_2008 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2008 <- data_2008 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT)), na.rm = TRUE))

data_2008 <- data_2008 %>%
  mutate(year = 2008, month = "May")

collapsed_2008 <- data_2008
cat(paste0("  2008: ", nrow(collapsed_2008), " obs\n"))

###############################################################################
### PROCESSING 2011 ELECTION
###############################################################################
cat("Processing 2011...\n")

data_2011 <- read_dta(file.path(raw_data_path, "2011", "Ayu_Seccion_2011.dta"))

data_2011 <- data_2011 %>%
  filter(!(municipality == "" & is.na(section)))

# Convert party columns to numeric
party_cols_2011 <- setdiff(names(data_2011), c("municipality", "section"))
data_2011 <- data_2011 %>%
  mutate(across(all_of(party_cols_2011), ~suppressWarnings(as.numeric(as.character(.)))))

data_2011$municipality <- toupper(trimws(data_2011$municipality))
data_2011$municipality <- remove_accents(data_2011$municipality)

data_2011 <- data_2011 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2011 <- data_2011 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2011 <- data_2011 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Coalition aggregation + zeroing (replicate Stata logic) ---
# Handle C_PRD_PT correction
if ("C_PRD_PT" %in% names(data_2011)) {
  data_2011 <- data_2011 %>%
    mutate(PRD_PT = ifelse(!is.na(C_PRD_PT) & C_PRD_PT != 0 & (is.na(PRD_PT) | PRD_PT == 0),
                           C_PRD_PT, PRD_PT)) %>%
    select(-C_PRD_PT)
}

# Create municipality-level coalition indicators
# (bys uniqueid: egen c_VAR = sum(VAR) → c_VAR = (c_VAR > 0))
coal_cols_2011 <- c("PAN_PRI_PVEM", "PAN_PRI_PVEM_PANAL", "PAN_PRI_PANAL",
                     "PAN_PANAL", "PRI_PRD_PVEM_PC_PANAL", "PRI_PVEM",
                     "PRD_PT", "PRD_PT_PC", "PRD_PC", "PT_PC")
coal_indicators <- data_2011 %>%
  group_by(uniqueid) %>%
  summarise(across(all_of(coal_cols_2011), ~as.numeric(sum(., na.rm = TRUE) > 0),
                   .names = "c_{.col}"), .groups = "drop")
data_2011 <- data_2011 %>% left_join(coal_indicators, by = "uniqueid")

# Sequential aggregation + zeroing (order matches Stata)
data_2011 <- data_2011 %>%
  mutate(
    PAN_PRI_PVEM = ifelse(c_PAN_PRI_PVEM == 1, PAN_PRI_PVEM + PAN + PRI + PVEM, PAN_PRI_PVEM),
    PAN = ifelse(c_PAN_PRI_PVEM == 1, 0, PAN),
    PRI = ifelse(c_PAN_PRI_PVEM == 1, 0, PRI),
    PVEM = ifelse(c_PAN_PRI_PVEM == 1, 0, PVEM)
  ) %>% mutate(
    PAN_PRI_PVEM_PANAL = ifelse(c_PAN_PRI_PVEM_PANAL == 1, PAN_PRI_PVEM_PANAL + PAN + PRI + PVEM + PANAL, PAN_PRI_PVEM_PANAL),
    PAN = ifelse(c_PAN_PRI_PVEM_PANAL == 1, 0, PAN),
    PRI = ifelse(c_PAN_PRI_PVEM_PANAL == 1, 0, PRI),
    PVEM = ifelse(c_PAN_PRI_PVEM_PANAL == 1, 0, PVEM),
    PANAL = ifelse(c_PAN_PRI_PVEM_PANAL == 1, 0, PANAL)
  ) %>% mutate(
    PAN_PRI_PANAL = ifelse(c_PAN_PRI_PANAL == 1, PAN_PRI_PANAL + PAN + PRI + PANAL, PAN_PRI_PANAL),
    PAN = ifelse(c_PAN_PRI_PANAL == 1, 0, PAN),
    PRI = ifelse(c_PAN_PRI_PANAL == 1, 0, PRI),
    PANAL = ifelse(c_PAN_PRI_PANAL == 1, 0, PANAL)
  ) %>% mutate(
    PAN_PANAL = ifelse(c_PAN_PANAL == 1, PAN_PANAL + PAN + PANAL, PAN_PANAL),
    PAN = ifelse(c_PAN_PANAL == 1, 0, PAN),
    PANAL = ifelse(c_PAN_PANAL == 1, 0, PANAL)
  ) %>% mutate(
    PRI_PRD_PVEM_PC_PANAL = ifelse(c_PRI_PRD_PVEM_PC_PANAL == 1, PRI_PRD_PVEM_PC_PANAL + PRI + PRD + PVEM + PC + PANAL, PRI_PRD_PVEM_PC_PANAL),
    PRI = ifelse(c_PRI_PRD_PVEM_PC_PANAL == 1, 0, PRI),
    PRD = ifelse(c_PRI_PRD_PVEM_PC_PANAL == 1, 0, PRD),
    PVEM = ifelse(c_PRI_PRD_PVEM_PC_PANAL == 1, 0, PVEM),
    PC = ifelse(c_PRI_PRD_PVEM_PC_PANAL == 1, 0, PC),
    PANAL = ifelse(c_PRI_PRD_PVEM_PC_PANAL == 1, 0, PANAL)
  ) %>% mutate(
    PRI_PVEM = ifelse(c_PRI_PVEM == 1, PRI_PVEM + PRI + PVEM, PRI_PVEM),
    PRI = ifelse(c_PRI_PVEM == 1, 0, PRI),
    PVEM = ifelse(c_PRI_PVEM == 1, 0, PVEM)
  ) %>% mutate(
    PRD_PT = ifelse(c_PRD_PT == 1, PRD_PT + PRD + PT, PRD_PT),
    PRD = ifelse(c_PRD_PT == 1, 0, PRD),
    PT = ifelse(c_PRD_PT == 1, 0, PT)
  ) %>% mutate(
    PRD_PT_PC = ifelse(c_PRD_PT_PC == 1, PRD_PT_PC + PRD + PT + PC, PRD_PT_PC),
    PRD = ifelse(c_PRD_PT_PC == 1, 0, PRD),
    PT = ifelse(c_PRD_PT_PC == 1, 0, PT),
    PC = ifelse(c_PRD_PT_PC == 1, 0, PC)
  ) %>% mutate(
    PRD_PC = ifelse(c_PRD_PC == 1, PRD_PC + PRD + PC, PRD_PC),
    PRD = ifelse(c_PRD_PC == 1, 0, PRD),
    PC = ifelse(c_PRD_PC == 1, 0, PC)
  ) %>% mutate(
    PT_PC = ifelse(c_PT_PC == 1, PT_PC + PT + PC, PT_PC),
    PT = ifelse(c_PT_PC == 1, 0, PT),
    PC = ifelse(c_PT_PC == 1, 0, PC)
  ) %>%
  select(-starts_with("c_"))

# --- Valid votes ---
data_2011 <- data_2011 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PC, PANAL, PAN_PRI_PVEM, PAN_PRI_PVEM_PANAL, PAN_PRI_PANAL, PAN_PANAL, PRI_PRD_PVEM_PC_PANAL, PRI_PVEM, PRD_PT, PRD_PT_PC, PRD_PC, PT_PC)), na.rm = TRUE))

data_2011 <- data_2011 %>%
  mutate(year = 2011, month = "November")

collapsed_2011 <- data_2011
cat(paste0("  2011: ", nrow(collapsed_2011), " obs\n"))

###############################################################################
### PROCESSING 2012 ELECTION
###############################################################################
cat("Processing 2012...\n")

data_2012 <- read_excel(file.path(raw_data_path, "2012", "Resultados_electorales_extraordinaria_2012.xlsx"),
                         sheet = "morelia2012_bd")


data_2012 <- data_2012 %>%
  filter(!(municipality == "" & is.na(section)))
data_2012 <- data_2012 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2012 <- setdiff(names(data_2012), c("municipality", "section"))
data_2012 <- data_2012 %>%
  mutate(across(all_of(party_cols_2012), ~suppressWarnings(as.numeric(as.character(.)))))

data_2012$municipality <- toupper(trimws(data_2012$municipality))
data_2012$municipality <- remove_accents(data_2012$municipality)

# --- Collapse by uniqueid municipality section ---
data_2012 <- data_2012 %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Lista nominal from file ---
# LN file: merge.dta
# NOTE: Update path as needed based on your file structure
ln_2012_raw <- read_dta(file.path(raw_data_path, "2012", "merge.dta"))
ln_2012 <- ln_2012_raw %>%
  select(section, listanominal) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")
data_2012 <- data_2012 %>%
  left_join(ln_2012, by = "section")

data_2012 <- data_2012 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2012 <- data_2012 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2012 <- data_2012 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2012 <- data_2012 %>%
  mutate(valid = rowSums(across(c(PAN_PANAL, PRD_PT_PMC, PRI_PVEM)), na.rm = TRUE))

data_2012 <- data_2012 %>%
  mutate(year = 2012, month = "July")

collapsed_2012 <- data_2012
cat(paste0("  2012: ", nrow(collapsed_2012), " obs\n"))

###############################################################################
### PROCESSING 2015 REGULAR ELECTION (June - all municipalities)
###############################################################################
cat("Processing 2015 regular...\n")

# --- Load coalition mapping from computos_municipales_2015.xlsx ---
coal_2015 <- read_excel(file.path(raw_data_path, "2015", "computos_municipales_2015.xlsx"),
                         sheet = "Hoja1", range = "A3:BY115")
# Rename coalition columns AW-BT to named coalitions
coal_rename_map <- c(
  "PAN_PRI_PRD_PANAL_PH_PES", "PAN_PRD_PT_PANAL_PH", "PAN_PRD_PT_PANAL_PES",
  "PRD_PT_PANAL_PH", "PRD_PT_PANAL_PES", "PAN_PRI_PVEM", "PAN_PRD_PT",
  "PRD_PT_PANAL", "PRD_PT_PH", "PRD_PT_PES", "PRD_PANAL_PES", "PT_PANAL_PH",
  "PT_PES_PH", "PAN_PRD", "PAN_PT", "PAN_MC", "PAN_PH", "PRI_PVEM",
  "PRD_PT", "PRD_PANAL", "PRD_PES", "PT_MC", "PT_PH", "PT_PES"
)
# Extract just municipality + coalition flags
coal_2015 <- coal_2015 %>%
  select(D, any_of(names(coal_2015)[which(names(coal_2015) >= "AW")])) %>%
  rename(municipality = D)
# Apply coalition names to the renamed range
if (ncol(coal_2015) > 1) {
  coal_col_indices <- 2:min(ncol(coal_2015), length(coal_rename_map) + 1)
  names(coal_2015)[coal_col_indices] <- paste0("coal_", coal_rename_map[1:length(coal_col_indices)])
}
coal_2015$municipality <- toupper(trimws(coal_2015$municipality))

# --- Load section-level data from multi-sheet Ayuntamientos_Mich_2015.xlsx ---
sheets_2015 <- excel_sheets(file.path(raw_data_path, "2015", "Ayuntamientos_Mich_2015.xlsx"))
data_2015_list <- list()
for (sh in sheets_2015) {
  d <- tryCatch({
    read_excel(file.path(raw_data_path, "2015", "Ayuntamientos_Mich_2015.xlsx"),
               sheet = sh, col_types = "text")
  }, error = function(e) NULL)
  if (!is.null(d) && nrow(d) > 0) {
    d <- d %>% filter(!is.na(CVO) & CVO != "TOTAL" & CVO != "")
    d <- d %>% mutate(across(everything(), ~ifelse(is.na(.) | . == "", "0", .)))
    data_2015_list[[sh]] <- d
  }
}
data_2015_reg <- bind_rows(data_2015_list)

# Rename key columns
if ("MUNICIPIO" %in% names(data_2015_reg)) {
  data_2015_reg <- data_2015_reg %>% rename(municipality = MUNICIPIO)
}
data_2015_reg <- data_2015_reg %>%
  rename_with(~gsub("SECCI\u00d3N|SECCION", "section", .), .cols = everything())

# Destring numeric columns
for (col in setdiff(names(data_2015_reg), c("municipality", "section", "CVO"))) {
  data_2015_reg[[col]] <- suppressWarnings(as.numeric(data_2015_reg[[col]]))
}
data_2015_reg$section <- suppressWarnings(as.numeric(data_2015_reg$section))

data_2015_reg$municipality <- toupper(trimws(data_2015_reg$municipality))
data_2015_reg$municipality <- remove_accents(data_2015_reg$municipality)

# Assign uniqueid
data_2015_reg <- data_2015_reg %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# Handle CI columns: combine CI and CI_1 into CI_1
if ("CI" %in% names(data_2015_reg) && "CI_1" %in% names(data_2015_reg)) {
  data_2015_reg <- data_2015_reg %>%
    mutate(CI_1 = coalesce(CI, 0) + coalesce(CI_1, 0)) %>%
    select(-CI)
} else if ("CI" %in% names(data_2015_reg)) {
  data_2015_reg <- data_2015_reg %>% rename(CI_1 = CI)
}

# Collapse by municipality uniqueid section
data_2015_reg <- data_2015_reg %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Valid votes (35 columns from Stata) ---
valid_cols_2015 <- c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "MORENA", "PH", "PES",
  "PRI_PVEM", "PRD_PT_PANAL", "PRD_PT_PANAL_PES", "PRD_PANAL", "PRD_PANAL_PES",
  "PT_PANAL_PH", "PAN_PH", "PAN_MC", "PAN_PRD_PT", "PRD_PES", "PRD_PT_PES",
  "PT_PES_PH", "CI_1", "PT_PES", "PRD_PT_PANAL_PH", "PRD_PT_PH", "PT_MC",
  "PT_PH", "PRD_PT", "PAN_PT", "PAN_PRI_PRD_PANAL_PH_PES", "PAN_PRD",
  "PAN_PRD_PT_PANAL_PES", "PAN_PRI_PVEM", "PAN_PRD_PT_PANAL_PH")

data_2015_reg <- data_2015_reg %>%
  mutate(valid = rowSums(across(any_of(valid_cols_2015), ~replace(., is.na(.), 0))))

# Compute total = valid + VOTOSNULOS + NOREGISTRADOS
data_2015_reg <- data_2015_reg %>%
  mutate(total = valid + coalesce(VOTOSNULOS, 0) + coalesce(NOREGISTRADOS, 0)) %>%
  select(-any_of(c("NOREGISTRADOS", "VOTOSNULOS", "CVO")))

# --- Lista nominal from LN2015.dta ---
ln_2015 <- read_dta(file.path(ln_path, "LN 2012-2019", "2015", "LN2015.dta"))
ln_2015 <- ln_2015 %>%
  filter(entidad == state_ed, month == 6) %>%
  mutate(uniqueid = (entidad * 1000) + municipio) %>%
  filter(seccion != 0) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

data_2015_reg <- data_2015_reg %>%
  left_join(ln_2015, by = "section")

data_2015_reg <- data_2015_reg %>%
  mutate(turnout = total / listanominal)

data_2015_reg <- data_2015_reg %>%
  mutate(year = 2015, month = "June", STATE = STATE_NAME)

collapsed_2015_reg <- data_2015_reg
cat(paste0("  2015 regular: ", nrow(collapsed_2015_reg), " obs\n"))

###############################################################################
### PROCESSING 2015 EXTRAORDINARY ELECTION (December - Sahuayo only)
###############################################################################
cat("Processing 2015 extraordinary...\n")

data_2015_ext <- read_excel(file.path(raw_data_path, "2015", "SAHUAYO ELECCION EXTRAORDINARIA.xlsx"),
                         sheet = "COMPUTO FINAL")

data_2015_ext <- data_2015_ext %>%
  rename(section = E, listanominal = G)

data_2015_ext <- data_2015_ext %>%
  filter(!(municipality == "" & is.na(section)))
data_2015_ext <- data_2015_ext %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2015e <- setdiff(names(data_2015_ext), c("municipality", "section"))
data_2015_ext <- data_2015_ext %>%
  mutate(across(all_of(party_cols_2015e), ~suppressWarnings(as.numeric(as.character(.)))))

data_2015_ext$municipality <- toupper(trimws(data_2015_ext$municipality))
data_2015_ext$municipality <- remove_accents(data_2015_ext$municipality)

# --- Collapse ---
data_2015_ext <- data_2015_ext %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

data_2015_ext <- data_2015_ext %>%
  mutate(turnout = total / listanominal)

data_2015_ext <- data_2015_ext %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

data_2015_ext <- data_2015_ext %>%
  mutate(uniqueid = case_when(
    municipality == "SAHUAYO" ~ 16076,
    TRUE ~ assign_uniqueid(municipality)
  ))

data_2015_ext <- data_2015_ext %>%
  mutate(valid = rowSums(across(c(PAN_PRD_PANAL, PRI_PT_PVEM, MC, MORENA, PES)), na.rm = TRUE))

data_2015_ext <- data_2015_ext %>%
  mutate(year = 2015, month = "December", STATE = STATE_NAME)

collapsed_2015_ext <- data_2015_ext
cat(paste0("  2015 extraordinary: ", nrow(collapsed_2015_ext), " obs\n"))

# Combine both 2015 elections
collapsed_2015 <- bind_rows(collapsed_2015_reg, collapsed_2015_ext)
cat(paste0("  2015 combined: ", nrow(collapsed_2015), " obs\n"))

###############################################################################
### PROCESSING 2018 ELECTION
###############################################################################
cat("Processing 2018...\n")

# --- Multi-sheet import (SALVADOR imports 113 sheets and appends) ---
excel_path_2018 <- file.path(raw_data_path, "2018", "Ayuntamientos_Mich_2018.xlsx")
sheet_names <- excel_sheets(excel_path_2018)
data_2018 <- bind_rows(lapply(sheet_names, function(s) {
  df <- tryCatch(read_excel(excel_path_2018, sheet = s, col_types = "text"),
                  error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- df %>% filter(!is.na(ID_MUNICIPIO) & ID_MUNICIPIO != "")
  df
}))

# Convert all columns to appropriate types
data_2018 <- data_2018 %>%
  mutate(across(-any_of(c("MUNICIPIO", "CASILLA", "ID_MUNICIPIO")),
                ~suppressWarnings(as.numeric(as.character(.)))))

data_2018 <- data_2018 %>%
  mutate(municipality = toupper(trimws(MUNICIPIO)),
         section = suppressWarnings(as.numeric(substr(CASILLA, 1, 4)))) %>%
  select(-any_of(c("MUNICIPIO", "CASILLA", "ID_MUNICIPIO")))

data_2018$municipality <- remove_accents(data_2018$municipality)

# --- Assign uniqueid ---
data_2018 <- data_2018 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Coalition creation + zeroing (replicate SALVADOR Stata logic) ---
# NOTE: PMC = MC (Movimiento Ciudadano). Rename after coalition creation.

# 1. PAN_PRD_PMC: full 3-way coalition (coalfrente)
data_2018 <- data_2018 %>%
  mutate(
    PAN_PRD_MC = ifelse(!is.na(CO_PAN_PRD_PMC),
      coalesce(CO_PAN_PRD_PMC, 0) + coalesce(CO_PAN_PRD, 0) + coalesce(CO_PAN_PMC, 0) +
      coalesce(CO_PRD_PMC, 0) + coalesce(PAN, 0) + coalesce(PMC, 0) + coalesce(PRD, 0),
      NA_real_),
    PAN = ifelse(!is.na(CO_PAN_PRD_PMC), NA_real_, PAN),
    PMC = ifelse(!is.na(CO_PAN_PRD_PMC), NA_real_, PMC),
    PRD = ifelse(!is.na(CO_PAN_PRD_PMC), NA_real_, PRD)
  ) %>%
  select(-any_of(c("CO_PAN_PRD_PMC", "CO_PAN_PRD", "CO_PAN_PMC", "CO_PRD_PMC")))

# 2. PAN_MC (candidatura comun)
data_2018 <- data_2018 %>%
  mutate(
    PAN_MC = ifelse(!is.na(CC_PAN_PMC),
      coalesce(CC_PAN_PMC, 0) + coalesce(PAN, 0) + coalesce(PMC, 0), NA_real_),
    PAN = ifelse(!is.na(CC_PAN_PMC), 0, PAN),
    PMC = ifelse(!is.na(CC_PAN_PMC), 0, PMC)
  ) %>%
  select(-any_of("CC_PAN_PMC"))

# 3. PAN_PRD (candidatura comun)
data_2018 <- data_2018 %>%
  mutate(
    PAN_PRD = ifelse(!is.na(CC_PAN_PRD),
      coalesce(CC_PAN_PRD, 0) + coalesce(PAN, 0) + coalesce(PRD, 0), NA_real_),
    PAN = ifelse(!is.na(CC_PAN_PRD), 0, PAN),
    PRD = ifelse(!is.na(CC_PAN_PRD), 0, PRD)
  ) %>%
  select(-any_of("CC_PAN_PRD"))

# 4. PRD_MC (candidatura comun)
data_2018 <- data_2018 %>%
  mutate(
    PRD_MC = ifelse(!is.na(CC_PRD_PMC),
      coalesce(CC_PRD_PMC, 0) + coalesce(PRD, 0) + coalesce(PMC, 0), NA_real_),
    PRD = ifelse(!is.na(CC_PRD_PMC), 0, PRD),
    PMC = ifelse(!is.na(CC_PRD_PMC), 0, PMC)
  ) %>%
  select(-any_of("CC_PRD_PMC"))

# 5. PRD_PVEM (candidatura comun)
data_2018 <- data_2018 %>%
  mutate(
    PRD_PVEM = ifelse(!is.na(CC_PRD_PVEM),
      coalesce(CC_PRD_PVEM, 0) + coalesce(PRD, 0) + coalesce(PVEM, 0), NA_real_),
    PRD = ifelse(!is.na(CC_PRD_PVEM), 0, PRD),
    PVEM = ifelse(!is.na(CC_PRD_PVEM), 0, PVEM)
  ) %>%
  select(-any_of("CC_PRD_PVEM"))

# 6. PT_MORENA (coalicion)
data_2018 <- data_2018 %>%
  mutate(
    PT_MORENA = ifelse(!is.na(CO_PT_MORENA),
      coalesce(CO_PT_MORENA, 0) + coalesce(PT, 0) + coalesce(MORENA, 0), NA_real_),
    PT = ifelse(!is.na(CO_PT_MORENA), 0, PT),
    MORENA = ifelse(!is.na(CO_PT_MORENA), 0, MORENA)
  ) %>%
  select(-any_of("CO_PT_MORENA"))

# Rename PMC -> MC
data_2018 <- data_2018 %>%
  rename_with(~gsub("PMC", "MC", .), matches("^PMC$"))

# --- Collapse by municipality section ---
data_2018 <- data_2018 %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Drop noregistrados, nulos ---
data_2018 <- data_2018 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                    "NOREGISTRADOS", "NULOS", "no_reg", "nulo", "NOREG", "NULO")))

# --- Compute total and valid ---
data_2018 <- data_2018 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, MC, PANAL, MORENA, PES,
                                    PAN_PRD_MC, PT_MORENA, PAN_MC, PRD_PVEM,
                                    PAN_PRD, PRD_MC, CI_1, CI_2)), na.rm = TRUE))

# --- Lista nominal from file ---
ln_2018_raw <- read_dta(file.path(raw_data_path, "2018", "MICH_LN18.dta"))
ln_2018 <- ln_2018_raw %>%
  select(section, listanominal) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")
data_2018 <- data_2018 %>%
  left_join(ln_2018, by = "section")

data_2018 <- data_2018 %>%
  mutate(total = valid + rowSums(across(any_of(c("nulo", "no_reg", "NULO", "NOREG"))), na.rm = TRUE),
         turnout = total / listanominal)

data_2018 <- data_2018 %>%
  mutate(year = 2018, month = "July")

collapsed_2018 <- data_2018
cat(paste0("  2018: ", nrow(collapsed_2018), " obs\n"))


###############################################################################
### COMBINE ALL YEARS
###############################################################################
cat("Combining all years...\n")

michoacan_all <- bind_rows(
  collapsed_1995,
  collapsed_1998,
  collapsed_2001,
  collapsed_2004,
  collapsed_2005,
  collapsed_2007,
  collapsed_2008,
  collapsed_2011,
  collapsed_2012,
  collapsed_2015,
  collapsed_2018
)

cat(paste0("Total observations: ", nrow(michoacan_all), "\n"))

# Save output
dir.create(processed_path, recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(michoacan_all, file.path(processed_path, "michoacan_process_raw_data.csv"))
cat(paste0("MICHOACAN processing complete!\n"))
