################################################################################
## Process Raw Electoral Data - Tlaxcala
## Years: 2001, 2004, 2007, 2010, 2013, 2013_EXT, 2014_EXT, 2016
################################################################################

rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, haven, readxl, tidyverse, data.table, stringr)

# Set working directory
setwd("D:/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Tlaxcala 2001, 2004, 2007, 2010, 2013")

################################################################################
## Helper Functions
################################################################################

remove_accents <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("Á", "A", x)
  x <- gsub("É", "E", x)
  x <- gsub("Í", "I", x)
  x <- gsub("Ó", "O", x)
  x <- gsub("Ú", "U", x)
  x <- gsub("Ñ", "N", x)
  x <- gsub("á", "A", x)
  x <- gsub("é", "E", x)
  x <- gsub("í", "I", x)
  x <- gsub("ó", "O", x)
  x <- gsub("ú", "U", x)
  x <- gsub("ñ", "N", x)
  x <- gsub("ü", "U", x)
  x <- gsub("Ü", "U", x)
  return(x)
<<<<<<< HEAD
}

assign_tlaxcala_uniqueid <- function(municipality) {
  case_when(
    grepl("ACUAMANALA", municipality) ~ 29022,
    grepl("AMAXAC", municipality) ~ 29001,
    grepl("APETATITLAN", municipality) ~ 29002,
    grepl("APIZACO", municipality) ~ 29005,
    grepl("ATLANGATEPEC", municipality) ~ 29003,
    grepl("ATLTZAYANCA|ALTZAYANCA", municipality) ~ 29004,
    grepl("BENITO JUAREZ|BENITO JU", municipality) ~ 29045,
    grepl("CALPULALPAN", municipality) ~ 29006,
    grepl("CHIAUTEMPAN", municipality) ~ 29010,
    grepl("CONTLA", municipality) ~ 29018,
    grepl("CUAPIAXTLA", municipality) ~ 29008,
    grepl("CUAXOMULCO", municipality) ~ 29009,
    grepl("CARMEN TEQUEXQUITLA|EL CARMEN", municipality) ~ 29007,
    grepl("EMILIANO ZAPATA", municipality) ~ 29046,
    grepl("ESPANITA|ESPAÑITA", municipality) ~ 29012,
    grepl("HUAMANTLA", municipality) ~ 29013,
    grepl("HUEYOTLIPAN", municipality) ~ 29014,
    grepl("IXTACUIXTLA", municipality) ~ 29015,
    grepl("IXTENCO", municipality) ~ 29016,
    grepl("MAGDALENA TLALTELULCO", municipality) ~ 29048,
    grepl("LAZARO CARDENAS|LÁZARO CÁRDENAS", municipality) & !grepl("SANCTORUM", municipality) ~ 29047,
    grepl("MAZATECOCHCO", municipality) ~ 29017,
    grepl("MUNOZ DE DOMINGO|MUÑOZ DE DOMINGO", municipality) ~ 29011,
    grepl("NANACAMILPA", municipality) ~ 29021,
    grepl("NATIVITAS", municipality) ~ 29023,
    grepl("PANOTLA", municipality) ~ 29024,
    grepl("PAPALOTLA", municipality) ~ 29041,
    grepl("SAN DAMIAN TEXOLOC|SAN DAMIÁN TEXOLOC", municipality) ~ 29049,
    grepl("SAN FRANCISCO TETLANOHCAN", municipality) ~ 29050,
    grepl("SAN JERONIMO ZACUALPAN|SAN JERÓNIMO ZACUALPAN", municipality) ~ 29051,
    grepl("SAN JOSE TEACALCO|SAN JOSÉ TEACALCO", municipality) ~ 29052,
    grepl("SAN JUAN HUACTZINCO", municipality) ~ 29053,
    grepl("SAN LORENZO AXOCOMANITLA", municipality) ~ 29054,
    grepl("SAN LUCAS TECOPILCO", municipality) ~ 29055,
    grepl("SAN PABLO DEL MONTE", municipality) ~ 29025,
    grepl("SANCTORUM", municipality) ~ 29020,
    grepl("SANTA ANA NOPALUCAN", municipality) ~ 29056,
    grepl("SANTA APOLONIA TEACALCO", municipality) ~ 29057,
    grepl("SANTA CATARINA AYOMETLA", municipality) ~ 29058,
    grepl("SANTA CRUZ QUILEHTLA", municipality) ~ 29059,
    grepl("SANTA CRUZ TLAXCALA", municipality) ~ 29026,
    grepl("SANTA ISABEL XILOXOXTLA", municipality) ~ 29060,
    grepl("TENANCINGO", municipality) ~ 29027,
    grepl("TEOLOCHOLCO|SAN LUIS TEOLOCHOLCO", municipality) ~ 29028,
    grepl("TEPETITLA", municipality) ~ 29019,
    grepl("TEPEYANCO", municipality) ~ 29029,
    grepl("TERRENATE", municipality) ~ 29030,
    grepl("TETLA DE LA SOLIDARIDAD|TETLA", municipality) & !grepl("TETLATLAHUCA", municipality) ~ 29031,
    grepl("TETLATLAHUCA", municipality) ~ 29032,
    grepl("^TLAXCALA$", municipality) ~ 29033,
    grepl("TLAXCO", municipality) ~ 29034,
    grepl("TOCATLAN|TOCATLÁN", municipality) ~ 29035,
    grepl("TOTOLAC", municipality) ~ 29036,
    grepl("TZOMPANTEPEC", municipality) ~ 29038,
    grepl("XALOZTOC", municipality) ~ 29039,
    grepl("XALTOCAN", municipality) ~ 29040,
    grepl("XICOHTZINCO", municipality) ~ 29042,
    grepl("YAUHQUEMEHCAN", municipality) ~ 29043,
    grepl("ZACATELCO", municipality) ~ 29044,
    grepl("ZITLALTEPEC", municipality) ~ 29037,
    TRUE ~ NA_real_
  )
}

################################################################################
## 2001
################################################################################

df_2001 <- read_excel("Ayu_Seccion_2001_No_LN.xlsx") %>%
  rename_all(tolower)

df_2001 <- df_2001 %>%
  rename(municipality = municipio,
         section = seccion) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  mutate(across(c(pri, pan, prd, pt, pvem, partidodemocrata, psn, pc, pas, pcdt, pjs, nulos), 
                ~as.numeric(as.character(.))))

# Calculate total and filter
df_2001 <- df_2001 %>%
  mutate(total = rowSums(select(., pan, pri, prd, pt, pvem, partidodemocrata, psn, pc, pas, pcdt, pjs, nulos), na.rm = TRUE)) %>%
  filter(!is.na(total) & total > 0)

# Collapse by municipality and section
df_2001 <- df_2001 %>%
  group_by(municipality, section) %>%
  summarise(across(c(pri, pan, prd, pt, pvem, partidodemocrata, psn, pc, pas, pcdt, pjs, total), ~sum(., na.rm = TRUE)),
            .groups = "drop")

# Rename parties
df_2001 <- df_2001 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem,
         PC = pc, Partido_Democrata = partidodemocrata, PCDT = pcdt,
         PSN = psn, PAS = pas, PJS = pjs)

# Assign uniqueid
df_2001 <- df_2001 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_tlaxcala_uniqueid(municipality))

# Calculate valid votes
df_2001 <- df_2001 %>%
  mutate(valid = rowSums(select(., PRI, PAN, PRD, PT, PVEM, Partido_Democrata, PSN, PC, PAS, PCDT, PJS), na.rm = TRUE))

# Drop duplicated section 150
df_2001 <- df_2001 %>%
  filter(section != 150 | is.na(section))

# Merge lista nominal from all_months_years.dta
all_months <- read_dta("../../all_months_years.dta") %>%
  filter(ed == 29, month == 9, year == 2001) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2001 <- df_2001 %>%
  left_join(all_months, by = "section")

# Calculate municipal aggregates
df_2001 <- df_2001 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2001 <- df_2001 %>%
  mutate(year = 2001,
         month = "November",
         turnout = total / listanominal)

cat("2001 processed:", nrow(df_2001), "rows\n")

################################################################################
## 2004
################################################################################

# Read lista nominal
ln_2004 <- read.csv("Ayu_Seccion_2004_LN.csv", stringsAsFactors = FALSE) %>%
  rename(section = seccion) %>%
  select(section, listanominal)

# Read main data
df_2004 <- read.csv("Ayu_Seccion_2004_No_LN.csv", stringsAsFactors = FALSE)
names(df_2004) <- tolower(names(df_2004))

df_2004 <- df_2004 %>%
  rename(municipality = municipio,
         section = seccion,
         total = emitidos) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  filter(!is.na(total) & total > 0)

# Collapse
df_2004 <- df_2004 %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.)))) %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge lista nominal
df_2004 <- df_2004 %>%
  left_join(ln_2004, by = "section")

# Also try merge from all_months_years
all_months_2004 <- read_dta("../../all_months_years.dta") %>%
  filter(ed == 29, month == 10, year == 2004) %>%
  select(seccion, lista) %>%
  rename(section = seccion, lista = lista)

df_2004 <- df_2004 %>%
  left_join(all_months_2004, by = "section") %>%
  mutate(listanominal = ifelse(is.na(listanominal), lista, listanominal)) %>%
  select(-lista)

# Rename parties
df_2004 <- df_2004 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc,
         PCDT = pcdt, PJS = pjs,
         PRI_PVEM = pripvem,
         PT_PCDT_PJS = ptpcdtpjs,
         PT_PCDT = ptpcdt,
         PT_PJS = ptpjs,
         PCDT_PJS = pcdtpjs)

df_2004 <- df_2004 %>%
  mutate(turnout = total / listanominal)

# Assign uniqueid
df_2004 <- df_2004 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_tlaxcala_uniqueid(municipality))

# Calculate valid votes
party_cols_2004 <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PCDT", "PJS",
                     "PRI_PVEM", "PT_PCDT_PJS", "PCDT_PJS", "PT_PCDT", "PT_PJS")
party_cols_2004 <- party_cols_2004[party_cols_2004 %in% names(df_2004)]

df_2004 <- df_2004 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2004)), na.rm = TRUE))

# Calculate municipal aggregates
df_2004 <- df_2004 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2004 <- df_2004 %>%
  mutate(year = 2004,
         month = "November")

cat("2004 processed:", nrow(df_2004), "rows\n")

################################################################################
## 2007
################################################################################

# Read lista nominal 2007
ln_2007 <- read_excel("Listanominal2007.xlsx", sheet = "datos") %>%
  rename(section = section) %>%
  select(section, listanominal)

# Read main data
df_2007 <- read_excel("Ayu_Seccion_2007.xlsx", sheet = "Sheet1")
names(df_2007) <- tolower(names(df_2007))

df_2007 <- df_2007 %>%
  filter(!is.na(municipality) & municipality != "")

# Handle CC1_PRI_PVEM and CC2_PRI_PVEM columns if they exist
if ("cc1_pri_pvem" %in% names(df_2007)) {
  df_2007 <- df_2007 %>%
    mutate(pri_pvem = ifelse(is.na(pri_pvem) & !is.na(cc1_pri_pvem), cc1_pri_pvem, pri_pvem))
}
if ("cc2_pri_pvem" %in% names(df_2007)) {
  df_2007 <- df_2007 %>%
    mutate(pri_pvem = ifelse(is.na(pri_pvem) & !is.na(cc2_pri_pvem), cc2_pri_pvem, pri_pvem))
}

# Calculate total and filter
df_2007 <- df_2007 %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.)))) %>%
  mutate(total = rowSums(select(., matches("pri|pan|prd|pt|pvem|pc|pcdt|panal|pas|ps|noregistrados|emitidos|validos")), na.rm = TRUE)) %>%
  filter(total > 0)

# Collapse
df_2007 <- df_2007 %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge lista nominal
df_2007 <- df_2007 %>%
  left_join(ln_2007, by = "section")

# Rename columns
df_2007 <- df_2007 %>%
  rename_with(~toupper(.), -c(municipality, section, listanominal, total)) %>%
  rename(PAN_PAC = PAN_PAC, PRI_PVEM = PRI_PVEM, PRI_PVEM_PS = PRI_PVEM_PS)

# Assign uniqueid
df_2007 <- df_2007 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_tlaxcala_uniqueid(municipality))

# Rename VALIDOS to valid
if ("VALIDOS" %in% names(df_2007)) {
  df_2007 <- df_2007 %>% rename(valid = VALIDOS)
} else {
  # Calculate valid
  party_cols_2007 <- c("PRI", "PAN_PAC", "PRI_PVEM", "PRD", "PT", "PVEM", "PC", "PCDT", "PANAL", "PAS", "PS", "PRI_PVEM_PS")
  party_cols_2007 <- party_cols_2007[party_cols_2007 %in% names(df_2007)]
  df_2007 <- df_2007 %>%
    mutate(valid = rowSums(select(., all_of(party_cols_2007)), na.rm = TRUE))
}

# Calculate municipal aggregates and turnout
df_2007 <- df_2007 %>%
=======
}

assign_tlaxcala_uniqueid <- function(municipality) {
  case_when(
    grepl("ACUAMANALA", municipality) ~ 29022,
    grepl("AMAXAC", municipality) ~ 29001,
    grepl("APETATITLAN", municipality) ~ 29002,
    grepl("APIZACO", municipality) ~ 29005,
    grepl("ATLANGATEPEC", municipality) ~ 29003,
    grepl("ATLTZAYANCA|ALTZAYANCA", municipality) ~ 29004,
    grepl("BENITO JUAREZ|BENITO JU", municipality) ~ 29045,
    grepl("CALPULALPAN", municipality) ~ 29006,
    grepl("CHIAUTEMPAN", municipality) ~ 29010,
    grepl("CONTLA", municipality) ~ 29018,
    grepl("CUAPIAXTLA", municipality) ~ 29008,
    grepl("CUAXOMULCO", municipality) ~ 29009,
    grepl("CARMEN TEQUEXQUITLA|EL CARMEN", municipality) ~ 29007,
    grepl("EMILIANO ZAPATA", municipality) ~ 29046,
    grepl("ESPANITA|ESPAÑITA", municipality) ~ 29012,
    grepl("HUAMANTLA", municipality) ~ 29013,
    grepl("HUEYOTLIPAN", municipality) ~ 29014,
    grepl("IXTACUIXTLA", municipality) ~ 29015,
    grepl("IXTENCO", municipality) ~ 29016,
    grepl("MAGDALENA TLALTELULCO", municipality) ~ 29048,
    grepl("LAZARO CARDENAS|LÁZARO CÁRDENAS", municipality) & !grepl("SANCTORUM", municipality) ~ 29047,
    grepl("MAZATECOCHCO", municipality) ~ 29017,
    grepl("MUNOZ DE DOMINGO|MUÑOZ DE DOMINGO", municipality) ~ 29011,
    grepl("NANACAMILPA", municipality) ~ 29021,
    grepl("NATIVITAS", municipality) ~ 29023,
    grepl("PANOTLA", municipality) ~ 29024,
    grepl("PAPALOTLA", municipality) ~ 29041,
    grepl("SAN DAMIAN TEXOLOC|SAN DAMIÁN TEXOLOC", municipality) ~ 29049,
    grepl("SAN FRANCISCO TETLANOHCAN", municipality) ~ 29050,
    grepl("SAN JERONIMO ZACUALPAN|SAN JERÓNIMO ZACUALPAN", municipality) ~ 29051,
    grepl("SAN JOSE TEACALCO|SAN JOSÉ TEACALCO", municipality) ~ 29052,
    grepl("SAN JUAN HUACTZINCO", municipality) ~ 29053,
    grepl("SAN LORENZO AXOCOMANITLA", municipality) ~ 29054,
    grepl("SAN LUCAS TECOPILCO", municipality) ~ 29055,
    grepl("SAN PABLO DEL MONTE", municipality) ~ 29025,
    grepl("SANCTORUM", municipality) ~ 29020,
    grepl("SANTA ANA NOPALUCAN", municipality) ~ 29056,
    grepl("SANTA APOLONIA TEACALCO", municipality) ~ 29057,
    grepl("SANTA CATARINA AYOMETLA", municipality) ~ 29058,
    grepl("SANTA CRUZ QUILEHTLA", municipality) ~ 29059,
    grepl("SANTA CRUZ TLAXCALA", municipality) ~ 29026,
    grepl("SANTA ISABEL XILOXOXTLA", municipality) ~ 29060,
    grepl("TENANCINGO", municipality) ~ 29027,
    grepl("TEOLOCHOLCO|SAN LUIS TEOLOCHOLCO", municipality) ~ 29028,
    grepl("TEPETITLA", municipality) ~ 29019,
    grepl("TEPEYANCO", municipality) ~ 29029,
    grepl("TERRENATE", municipality) ~ 29030,
    grepl("TETLA DE LA SOLIDARIDAD|TETLA", municipality) & !grepl("TETLATLAHUCA", municipality) ~ 29031,
    grepl("TETLATLAHUCA", municipality) ~ 29032,
    grepl("^TLAXCALA$", municipality) ~ 29033,
    grepl("TLAXCO", municipality) ~ 29034,
    grepl("TOCATLAN|TOCATLÁN", municipality) ~ 29035,
    grepl("TOTOLAC", municipality) ~ 29036,
    grepl("TZOMPANTEPEC", municipality) ~ 29038,
    grepl("XALOZTOC", municipality) ~ 29039,
    grepl("XALTOCAN", municipality) ~ 29040,
    grepl("XICOHTZINCO", municipality) ~ 29042,
    grepl("YAUHQUEMEHCAN", municipality) ~ 29043,
    grepl("ZACATELCO", municipality) ~ 29044,
    grepl("ZITLALTEPEC", municipality) ~ 29037,
    TRUE ~ NA_real_
  )
}

################################################################################
## 2001
################################################################################

df_2001 <- read_excel("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Ayu_Seccion_2001_No_LN.xlsx") %>%
  rename_all(tolower)

names(df_2001) <- gsub(" ", "", names(df_2001))

df_2001 <- df_2001 %>%
  rename(municipality = municipio,
         section = seccion) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  mutate(across(c(pri, pan, prd, pt, pvem, partidodemocrata, psn, pc, pas, pcdt, pjs, nulos), 
                ~as.numeric(as.character(.))))

# Calculate total and filter
df_2001 <- df_2001 %>%
  mutate(total = rowSums(select(., pan, pri, prd, pt, pvem, partidodemocrata, psn, pc, pas, pcdt, pjs, nulos), na.rm = TRUE)) %>%
  filter(!is.na(total) & total > 0)

# Collapse by municipality and section
df_2001 <- df_2001 %>%
  group_by(municipality, section) %>%
  summarise(across(c(pri, pan, prd, pt, pvem, partidodemocrata, psn, pc, pas, pcdt, pjs, total), ~sum(., na.rm = TRUE)),
            .groups = "drop")

# Rename parties
df_2001 <- df_2001 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem,
         PC = pc, Partido_Democrata = partidodemocrata, PCDT = pcdt,
         PSN = psn, PAS = pas, PJS = pjs)

# Assign uniqueid
df_2001 <- df_2001 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_tlaxcala_uniqueid(municipality))

# Calculate valid votes
df_2001 <- df_2001 %>%
  mutate(valid = rowSums(select(., PRI, PAN, PRD, PT, PVEM, Partido_Democrata, PSN, PC, PAS, PCDT, PJS), na.rm = TRUE))

# Drop duplicated section 150
df_2001 <- df_2001 %>%
  filter(section != 150 | is.na(section))

# Merge lista nominal from all_months_years.dta
all_months <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>%
  filter(state == "TLAXCALA", month == "September", year == 2001) %>%
  select(section, lista) %>%
  rename(listanominal = lista)

df_2001 <- df_2001 %>%
  left_join(all_months, by = "section")

# Calculate municipal aggregates
df_2001 <- df_2001 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2001 <- df_2001 %>%
  mutate(year = 2001,
         month = "November",
         turnout = total / listanominal)

cat("2001 processed:", nrow(df_2001), "rows\n")


################################################################################
## 2004
################################################################################

# Read lista nominal
ln_2004 <- read.csv("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Ayu_Seccion_2004_LN.csv", stringsAsFactors = FALSE) %>%
  rename(section = Seccion, listanominal = "Lista.Nominal") %>%
  select(section, listanominal)

# Read main data
df_2004 <- read.csv("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Ayu_Seccion_2004_No_LN.csv", stringsAsFactors = FALSE)
names(df_2004) <- tolower(names(df_2004))

df_2004 <- df_2004 %>%
  rename(municipality = municipio,
         section = seccion,
         total = emitidos) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  filter(!is.na(total) & total > 0)

# Collapse
df_2004 <- df_2004 %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.)))) %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge lista nominal
df_2004 <- df_2004 %>%
  left_join(ln_2004, by = "section")

# Also try merge from all_months_years
all_months_2004 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>%
  filter(state == "TLAXCALA", month == "October", year == 2004) %>%
  select(section, lista) %>%
  rename(lista = lista)

df_2004 <- df_2004 %>%
  left_join(all_months_2004, by = "section") %>%
  rename_with(~ gsub("\\.", "", .)) %>% 
  mutate(listanominal = ifelse(is.na(listanominal), lista, listanominal)) %>%
  select(-lista)

# Rename parties
df_2004 <- df_2004 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc,
         PCDT = pcdt, PJS = pjs,
         PRI_PVEM = pripvem,
         PT_PCDT_PJS = ptpcdtpjs,
         PT_PCDT = ptpcdt,
         PT_PJS = ptpjs,
         PCDT_PJS = pcdtpjs)

df_2004 <- df_2004 %>%
  mutate(turnout = total / listanominal)

# Assign uniqueid
df_2004 <- df_2004 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_tlaxcala_uniqueid(municipality))

# Calculate valid votes
party_cols_2004 <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PCDT", "PJS",
                     "PRI_PVEM", "PT_PCDT_PJS", "PCDT_PJS", "PT_PCDT", "PT_PJS")
party_cols_2004 <- party_cols_2004[party_cols_2004 %in% names(df_2004)]

df_2004 <- df_2004 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2004)), na.rm = TRUE))

# Calculate municipal aggregates
df_2004 <- df_2004 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2004 <- df_2004 %>%
  mutate(year = 2004,
         month = "November")

cat("2004 processed:", nrow(df_2004), "rows\n")


################################################################################
# 1) Read "Ayu_Seccion_2007.xlsx" (sheet "Sheet1"), drop rows with blank municipality
################################################################################
df_tlax <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Ayu_Seccion_2007.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()
names(df_tlax) <- gsub("[- ]", "", names(df_tlax))
# drop if municipality==""
df_tlax <- df_tlax %>%
  filter(municipality != "")

################################################################################
# 2) Move coalition columns CC1_PRI_PVEM, CC2_PRI_PVEM into PRI_PVEM if missing
################################################################################

# count if CC1_PRI_PVEM!=. => in R, let's see how many are non-NA
# replace PRI_PVEM = CC1_PRI_PVEM if PRI_PVEM==. & CC1_PRI_PVEM!=.
if ("CC1_PRI_PVEM" %in% names(df_tlax)) {
  # number of non-NA
  cat("Non-NA in CC1_PRI_PVEM:", sum(!is.na(df_tlax$CC1_PRI_PVEM)), "\n")
  
  df_tlax <- df_tlax %>%
    mutate(
      PRI_PVEM = if_else(is.na(PRI_PVEM) & !is.na(CC1_PRI_PVEM), CC1_PRI_PVEM, PRI_PVEM)
    ) %>%
    select(-CC1_PRI_PVEM)
}

# similarly for CC2_PRI_PVEM
if ("CC2_PRI_PVEM" %in% names(df_tlax)) {
  cat("Non-NA in CC2_PRI_PVEM:", sum(!is.na(df_tlax$CC2_PRI_PVEM)), "\n")
  
  df_tlax <- df_tlax %>%
    mutate(
      PRI_PVEM = if_else(is.na(PRI_PVEM) & !is.na(CC2_PRI_PVEM), CC2_PRI_PVEM, PRI_PVEM)
    ) %>%
    select(-CC2_PRI_PVEM)
}

################################################################################
# 3) Create total = rowtotal(...) for columns in row
################################################################################

vote_cols <- c("PRI","PAN_PAC","PRI_PVEM","PRD","PT","PVEM","PC","PCDT","PANAL","PAS","PS","PRI_PVEM_PS","NOREGISTRADOS","EMITIDOS","VALIDOS")
df_tlax <- df_tlax %>%
  rowwise() %>%
  mutate(
    total = sum(c_across(any_of(vote_cols)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  filter(total != 0)

################################################################################
# 4) Collapse (sum) columns, by (municipality, section)
################################################################################
#   collapse (sum) PRI - total, by(municipality section)
# meaning sum all columns from PRI to total. We'll do a partial approach.

# We'll identify columns from PRI to total
# Or we can just specify an explicit list: c("PRI","PAN_PAC","PRI_PVEM",..., "total")
collapse_cols <- c("PRI","PAN_PAC","PRI_PVEM","PRD","PT","PVEM","PC","PCDT","PANAL","PAS","PS","PRI_PVEM_PS",
                   "NOREGISTRADOS","EMITIDOS","VALIDOS","total")

df_collapse <- df_tlax %>%
  group_by(municipality, section) %>%
  summarise(
    across(any_of(collapse_cols), sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 5) Sort by section, merge with "Listanominal2007.dta" on 'section', drop unmatched,
#    erase "Listanominal2007.dta"
################################################################################

df_collapse <- df_collapse %>%
  arrange(section)

df_ln <- read_excel("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Listanominal2007.xlsx")

df_merge1 <- df_collapse %>%
  left_join(df_ln, by=c("section", "municipality")) %>%
  filter(!is.na(listanominal))  # drop if no match

################################################################################
# 6) gen turnout = total/listanominal, drop NOREGISTRADOS, EMITIDOS
################################################################################
df_merge1 <- df_merge1 %>%
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  mutate(turnout = total / listanominal) %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2007 <- df_2007 %>%
  mutate(year = 2007,
         month = "November")

cat("2007 processed:", nrow(df_2007), "rows\n")

################################################################################
## 2010
################################################################################

df_2010 <- read.csv("Ayu_Seccion_2010.csv", stringsAsFactors = FALSE)
names(df_2010) <- tolower(names(df_2010))

df_2010 <- df_2010 %>%
  rename(municipality = nombre_municipio,
         section = seccion,
         listanominal = lista_nominal) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section))

# Convert to numeric
df_2010 <- df_2010 %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.)))) %>%
  filter(!is.na(total) & total > 0)

<<<<<<< HEAD
# Collapse
df_2010 <- df_2010 %>%
=======
df_tlax10 <- read_csv("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Ayu_Seccion_2010.csv", show_col_types = FALSE) %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality == "" & is.na(section)))  # drop if municipality=="" & section==.
colnames(df_tlax10) <- tolower(colnames(df_tlax10))
names(df_tlax10) <- gsub("[- ]", "", names(df_tlax10))
# parse columns from panpna to total as numeric
# We'll identify them explicitly or do a partial approach:
vote_cols <- c("panpna","pripvem","pri","prips","prd", "prdpc","prdpcpt","prdpcpt2",
               "prdpt","prdpt2","ptpc","pt","convergencia","pvem","pac","pp","plt","ppt","ps","nulos")

df_tlax10 <- df_tlax10 %>%
  mutate(across(all_of(intersect(vote_cols, names(.))), as.numeric))

# drop if total ==. or total==0
df_tlax10 <- df_tlax10 %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 2) collapse (sum) listanominal panpna - total, by(municipality section)
################################################################################

# We'll find the columns from panpna to total plus listanominal:
collapse_cols <- c("listanominal","panpna","pripvem","pri","prips", "prd", "prdpc","prdpcpt","prdpcpt2","prdpt","prdpt2","ptpc","pt",
                   "convergencia","pvem","pac","pp","plt","ppt","ps","nulos","total")

df_collapsed <- df_tlax10 %>%
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Coalition corrections per do-file:
# NATIVITAS: pvem = 0 when pripvem != 0
# TERRENATE: pri = 0 when pripvem != 0
df_2010 <- df_2010 %>%
  mutate(pvem = ifelse(municipality == "NATIVITAS" & pripvem != 0, 0, pvem),
         pri = ifelse(municipality == "TERRENATE" & pripvem != 0, 0, pri))

# Handle prdpcpt coalitions
df_2010 <- df_2010 %>%
  mutate(prdpcpt = ifelse(prdpcpt != 0, prdpcpt + pt, prdpcpt),
         pt = ifelse(prdpcpt != 0, 0, pt))

# prdpcpt2: prd = 0 when prdpcpt2 != 0
df_2010 <- df_2010 %>%
  mutate(prd = ifelse(!is.na(prdpcpt2) & prdpcpt2 != 0, 0, prd))

# ptpc: pt = 0 when ptpc != 0
df_2010 <- df_2010 %>%
  mutate(pt = ifelse(!is.na(ptpc) & ptpc != 0, 0, pt))

# Merge prdpcpt2 into prdpcpt
df_2010 <- df_2010 %>%
  mutate(prdpcpt = ifelse(prdpcpt == 0 & !is.na(prdpcpt2) & prdpcpt2 != 0, prdpcpt2, prdpcpt))

# Merge prdpt2 into prdpt
df_2010 <- df_2010 %>%
  mutate(prdpt = ifelse(!is.na(prdpt) & prdpt == 0 & !is.na(prdpt2) & prdpt2 != 0, prdpt2, prdpt))

# Rename parties
df_2010 <- df_2010 %>%
  rename(PAN_PANAL = panpna,
         PRI = pri,
         PRI_PVEM = pripvem,
         PRI_PST = prips,
         PRD = prd,
         PRD_PT_PC = prdpcpt,
         PRD_PC = prdpc,
         PRD_PT = prdpt,
         PT_PC = ptpc,
         PT = pt,
         PC = convergencia,
         PVEM = pvem,
         PAC = pac,
         PP = pp,
         PLT = plt,
         PPT = ppt,
         PST = ps)

df_2010 <- df_2010 %>%
  mutate(turnout = total / listanominal)

# Assign uniqueid
df_2010 <- df_2010 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_tlaxcala_uniqueid(municipality))

# Calculate valid votes
party_cols_2010 <- c("PAN_PANAL", "PRI", "PRI_PVEM", "PRI_PST", "PRD", "PRD_PT_PC", 
                     "PRD_PC", "PRD_PT", "PT", "PT_PC", "PVEM", "PC", "PST", "PAC", "PP", "PLT", "PPT")
party_cols_2010 <- party_cols_2010[party_cols_2010 %in% names(df_2010)]

df_2010 <- df_2010 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2010)), na.rm = TRUE))

# Calculate municipal aggregates
df_2010 <- df_2010 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2010 <- df_2010 %>%
  mutate(year = 2010,
         month = "July")

cat("2010 processed:", nrow(df_2010), "rows\n")

################################################################################
## 2013
################################################################################

<<<<<<< HEAD
# Read lista nominal 2013
ln_2013 <- read_excel("Listanominal2013.xlsx", sheet = "datos") %>%
  rename(section = section) %>%
=======
# 1) Read "Ayu_Seccion_2013.xlsx" (sheet "Sheet1"), drop blank municipalities
df_july2013 <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Ayu_Seccion_2013.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()

names(df_july2013) <- gsub("[- ]", "", names(df_july2013))

df_july2013 <- df_july2013 %>%
  filter(municipality != "")

# 2) Rename columns: Emitidos->total, Validos->valid
df_july2013 <- df_july2013 %>%
  rename(
    total = Emitidos,
    valid = Validos
  )

# 3) Collapse (sum) from PAN through 'valid', by (municipality, section, Coalition)
vote_cols <- names(df_july2013)
# We'll identify the columns from "PAN" to "valid" if needed, or do an explicit list
# For clarity, let's do an explicit approach if known.
collapse_cols <- c("PAN","PAC","PS","PRI","PRD","PT","PVEM","PC","PCDT","PANAL","PAS","VALID",
                   "Coalition","NoRegistrados","Emitidos","total") # adapt if needed
# But your actual column set might differ; adjust as necessary.

df_collapsed <- df_july2013 %>%
  group_by(municipality, section, Coalition) %>%
  summarise(
    across(where(is.numeric), sum, na.rm=TRUE),
    .groups="drop"
  )
# 4) If Coalition=="PAC_PS", "PAN_PAC", "PRD_PT", "PRI_PVEM", "PT_PAC" => add sums, zero out old columns, then drop Coalition
df_collapsed <- df_collapsed %>%
  mutate(
    PAC_PS = if_else(Coalition=="PAC_PS", coalesce(PAC,0) + coalesce(PS,0), NA_real_),
    PAC = if_else(Coalition=="PAC_PS", 0, PAC),
    PS  = if_else(Coalition=="PAC_PS", 0, PS),
    
    PAN_PAC = if_else(Coalition=="PAN_PAC", coalesce(PAN,0)+coalesce(PAC,0), NA_real_),
    PAN = if_else(Coalition=="PAN_PAC", 0, PAN),
    PAC = if_else(Coalition=="PAN_PAC", 0, PAC),
    
    PRD_PT = if_else(Coalition=="PRD_PT", coalesce(PRD,0)+coalesce(PT,0), NA_real_),
    PRD = if_else(Coalition=="PRD_PT", 0, PRD),
    PT  = if_else(Coalition=="PRD_PT", 0, PT),
    
    PRI_PVEM = if_else(Coalition=="PRI_PVEM", coalesce(PRI,0)+coalesce(PVEM,0), NA_real_),
    PRI  = if_else(Coalition=="PRI_PVEM", 0, PRI),
    PVEM = if_else(Coalition=="PRI_PVEM", 0, PVEM),
    
    PT_PAC = if_else(Coalition=="PT_PAC", coalesce(PT,0)+coalesce(PAC,0), NA_real_),
    PT  = if_else(Coalition=="PT_PAC", 0, PT),
    PAC = if_else(Coalition=="PT_PAC", 0, PAC)
  ) %>%
  select(-Coalition)

# 5) Sort by section, merge with "Listanominal2013.dta" on `section`, drop unmatched
df_collapsed <- df_collapsed %>%
  arrange(section)

df_ln2013 <- read_excel("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Listanominal2013.xlsx")
df_merged <- df_collapsed %>%
  left_join(df_ln2013, by=c("section", "municipality")) %>%
  filter(!is.na(listanominal))

# 6) gen turnout= total/listanominal, drop NoRegistrados
df_merged <- df_merged %>%
  mutate(
    turnout = total / listanominal
  ) %>%
  select(-any_of("NoRegistrados"))

# 7) Convert municipality to uppercase, assign uniqueid
df_merged <- df_merged %>%
  mutate(
    municipality = toupper(municipality),
    uniqueid = 0
  )

df_2013 <- df_merged %>%
  mutate(
    uniqueid = case_when(
      municipality=="ACUAMANALA DE MIGUEL HIDALGO"        ~ 29022,
      municipality=="AMAXAC DE GUERRERO"                 ~ 29001,
      municipality=="APETATITLAN DE ANTONIO CARVAJAL"    ~ 29002,
      municipality=="APIZACO"                            ~ 29005,
      municipality=="ATLANGATEPEC"                       ~ 29003,
      municipality=="ATLTZAYANCA"                        ~ 29004,
      municipality=="BENITO JUAREZ"                      ~ 29045,
      municipality=="CALPULALPAN"                        ~ 29006,
      municipality=="CHIAUTEMPAN"                        ~ 29010,
      municipality=="CONTLA DE JUAN CUAMATZI"            ~ 29018,
      municipality=="CUAPIAXTLA"                         ~ 29008,
      municipality=="CUAXOMULCO"                         ~ 29009,
      municipality=="EL CARMEN TEQUEXQUITLA"             ~ 29007,
      municipality=="EMILIANO ZAPATA"                    ~ 29046,
      municipality=="ESPANITA"                           ~ 29012,
      municipality=="HUAMANTLA"                          ~ 29013,
      municipality=="HUEYOTLIPAN"                        ~ 29014,
      municipality=="IXTACUIXTLA DE MARIANO MATAMOROS"   ~ 29015,
      municipality=="IXTENCO"                            ~ 29016,
      municipality=="LA MAGDALENA TLALTELULCO"           ~ 29048,
      municipality=="LAZARO CARDENAS"                    ~ 29047,
      municipality=="MAZATECOCHCO DE JOSE MARIA MORELOS" ~ 29017,
      municipality=="MUNOZ DE DOMINGO ARENAS"            ~ 29011,
      municipality=="NANACAMILPA DE MARIANO ARISTA"      ~ 29021,
      municipality=="NATIVITAS"                          ~ 29023,
      municipality=="PANOTLA"                            ~ 29024,
      municipality=="PAPALOTLA DE XICOHTENCATL"          ~ 29041,
      municipality=="SAN DAMIAN TEXOLOC"                 ~ 29049,
      municipality=="SAN FRANCISCO TETLANOHCAN"          ~ 29050,
      municipality=="SAN JERONIMO ZACUALPAN"             ~ 29051,
      municipality=="SAN JOSE TEACALCO"                  ~ 29052,
      municipality=="SAN JUAN HUACTZINCO"                ~ 29053,
      municipality=="SAN LORENZO AXOCOMANITLA"           ~ 29054,
      municipality=="SAN LUCAS TECOPILCO"                ~ 29055,
      municipality=="SAN PABLO DEL MONTE"                ~ 29025,
      municipality=="SANCTORUM DE LAZARO CARDENAS"       ~ 29020,
      municipality=="SANTA ANA NOPALUCAN"                ~ 29056,
      municipality=="SANTA APOLONIA TEACALCO"            ~ 29057,
      municipality=="SANTA CATARINA AYOMETLA"            ~ 29058,
      municipality=="SANTA CRUZ QUILEHTLA"               ~ 29059,
      municipality=="SANTA CRUZ TLAXCALA"                ~ 29026,
      municipality=="SANTA ISABEL XILOXOXTLA"            ~ 29060,
      municipality=="TENANCINGO"                         ~ 29027,
      municipality=="TEOLOCHOLCO"                        ~ 29028,
      municipality=="TEPETITLA DE LARDIZABAL"            ~ 29019,
      municipality=="TEPEYANCO"                          ~ 29029,
      municipality=="TERRENATE"                          ~ 29030,
      municipality=="TETLA DE LA SOLIDARIDAD"            ~ 29031,
      municipality=="TETLATLAHUCA"                       ~ 29032,
      municipality=="TLAXCALA"                           ~ 29033,
      municipality=="TLAXCO"                             ~ 29034,
      municipality=="TOCATLAN"                           ~ 29035,
      municipality=="TOTOLAC"                            ~ 29036,
      municipality=="TZOMPANTEPEC"                       ~ 29038,
      municipality=="XALOZTOC"                           ~ 29039,
      municipality=="XALTOCAN"                           ~ 29040,
      municipality=="XICOHTZINCO"                        ~ 29042,
      municipality=="YAUHQUEMEHCAN"                      ~ 29043,
      municipality=="ZACATELCO"                          ~ 29044,
      municipality=="ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS" ~ 29037,
      TRUE ~ uniqueid
    )
  ) %>%
  mutate(
    year  = 2013,
    month = "July"
  ) %>%
  arrange(section)

################################################################################
# Part B: Tlaxcala 2013 Extraordinario (December)
################################################################################

# 1) Read "Resultados Extraordinaria 8 Diciembre 2013.xlsx" (sheet "Sheet1"), rename columns
df_extra <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Resultados Extraordinaria 8 Diciembre 2013.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()

df_extra <- df_extra %>%
  rename(
    section = Sección,
    total   = EMITIDOS,
    valid   = VALIDOS,
    PVEM    = VERDE
  ) %>%
  mutate(uniqueid = 29002)  # from the code: g uniqueid=29002

# 2) collapse (sum) columns from "PAN" to "PS" plus "total" and "valid", by(municipality uniqueid section)
vote_cols2 <- c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","PAC","PS","PC","PartidoX",   # adapt as needed
                "total","valid")

df_extra_collapse <- df_extra %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(any_of(vote_cols2), sum, na.rm=TRUE), .groups="drop")

# 3) Merge with "all_months_years.dta" for (ed=29, month=11, year=2013)? The code:
#    preserve
#    use "..\..\all_months_years.dta", clear
#    keep if ed==29 & month==11 & year==2013
#    rename seccion->section, rename lista->listanominal
#    save "merge.dta", restore
#    merge 1:1 section using "merge.dta", keepusing(listanominal)
#    drop if _merge==2
#    drop _merge
#    erase "merge.dta"

df_allm <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  filter(state=="TLAXCALA", month== "November", year==2013) %>%
  rename(listanominal=lista) %>%
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  select(section, listanominal)

# Read main data
df_2013 <- read_excel("Ayu_Seccion_2013.xlsx", sheet = "Sheet1")
names(df_2013) <- tolower(names(df_2013))

df_2013 <- df_2013 %>%
  filter(!is.na(municipality) & municipality != "") %>%
  rename(total = emitidos,
         valid = validos)

# Collapse
df_2013 <- df_2013 %>%
  mutate(across(-c(municipality, section, coalition), ~as.numeric(as.character(.)))) %>%
  group_by(municipality, section, coalition) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Create coalition columns based on Coalition variable
df_2013 <- df_2013 %>%
  mutate(PAC_PS = ifelse(coalition == "PAC_PS", pac + ps, NA),
         PAN_PAC = ifelse(coalition == "PAN_PAC", pan + pac, NA),
         PRD_PT = ifelse(coalition == "PRD_PT", prd + pt, NA),
         PRI_PVEM = ifelse(coalition == "PRI_PVEM", pri + pvem, NA),
         PT_PAC = ifelse(coalition == "PT_PAC", pt + pac, NA))

<<<<<<< HEAD
# Zero out absorbed parties
df_2013 <- df_2013 %>%
  mutate(pac = ifelse(coalition %in% c("PAC_PS", "PAN_PAC", "PT_PAC"), 0, pac),
         ps = ifelse(coalition == "PAC_PS", 0, ps),
         pan = ifelse(coalition == "PAN_PAC", 0, pan),
         prd = ifelse(coalition == "PRD_PT", 0, prd),
         pt = ifelse(coalition %in% c("PRD_PT", "PT_PAC"), 0, pt),
         pri = ifelse(coalition == "PRI_PVEM", 0, pri),
         pvem = ifelse(coalition == "PRI_PVEM", 0, pvem))

# Collapse again without coalition
df_2013 <- df_2013 %>%
  select(-coalition) %>%
=======
df_2014_extra <- read_excel(
  path     = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Resultados Extraordinaria 23 Febrero 2014.xlsx",
  sheet    = "Hoja1",
  range    = "A6:K13",
  col_names= TRUE
) %>%
  as.data.frame()
names(df_2014_extra)
# rename Sección->section, EMIT->total, VALID->valid
df_2014_extra <- df_2014_extra %>%
  rename(
    section = Sección,
    total   = EMITIDOS,
    valid   = VALIDOS
  ) %>%
  mutate(
    uniqueid    = 29022,
    municipality= "ACUAMANALA DE MIGUEL HIDALGO EXTRAORDINARIO"
  )

################################################################################
# 2) collapse (sum) from PAN to MC (plus total and valid) 
#    by (municipality, uniqueid, section)
################################################################################

# Identify columns from "PAN" to "MC" plus "total" and "valid".
# For example, if your columns are named exactly PAN, PRI, PRD, PT, ... , MC, total, valid.
# We'll specify them explicitly for clarity:
collapse_cols <- c("PAN","PRI","PRD","PT","PVEM","MC","total","valid")

df_collapsed <- df_2014_extra %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(all_of(intersect(collapse_cols, names(.))), sum, na.rm=TRUE),
            .groups="drop")

################################################################################
# 3) Merging with all_months_years.dta for (ed=29, month=1, year=2014)
################################################################################

df_allm <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  filter(state=="TLAXCALA", month=="January", year==2014) %>%
  rename(listanominal=lista) %>%
  select(section, listanominal)

df_merged <- df_collapsed %>%
  left_join(df_allm, by="section") %>%
  filter(!is.na(listanominal))  # dropping unmatched

################################################################################
# 4) Compute turnout= total/listanominal, set year=2014, month="February"
################################################################################

df_2014 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2014,
    month   = "February"
  ) %>%
  arrange(section)

################################################################################
# Part A: Reading each sheet from "Ayuntamientos_Tlaxcala_2016.xlsx" and
#         saving as .dta
################################################################################

all_sheets <- excel_sheets("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Ayuntamientos_Tlaxcala_2016.xlsx")

for (sheetname in all_sheets) {
  df_sheet <- read_excel(
    path = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/Ayuntamientos_Tlaxcala_2016.xlsx",
    sheet = sheetname,
    col_names = TRUE,
    col_types = "text"
  ) %>%
    as.data.frame()
  
  # drop if section==""
  if ("section" %in% names(df_sheet)) {
    df_sheet <- df_sheet %>%
      filter(section!="")
  }
  
  # for each column, replace "" with "0"
  df_sheet <- df_sheet %>%
    mutate(across(everything(), ~ if_else(. == "", "0", .)))
  
  # save as `sheetname`.dta
  write_dta(df_sheet, paste0(sheetname, ".dta"))
}

################################################################################
# Part B: Appending multiple .dta files (like "Table 1.dta", "Table 2.dta", etc.)
################################################################################

append_files <- c(
  "Table 1.dta", "Table 2.dta", "Table 3.dta", "Table 4.dta", "Table 5.dta",
  "Table 7.dta", "Table 8.dta", "Table 9.dta", "Table 11.dta", "Table 13.dta",
  "Table 15.dta", "Table 16.dta", "Table 17.dta", "Table 18.dta", "Table 19.dta",
  "Table 20.dta", "Table 22.dta", "Table 23.dta", "Table 25.dta", "Table 26.dta",
  "Table 27.dta", "Table 28.dta", "Table 29.dta", "Table 30.dta", "Table 31.dta",
  "Table 32.dta", "Table 33.dta", "Table 34.dta", "Table 35.dta", "Table 36.dta",
  "Table 37.dta", "Table 38.dta", "Table 39.dta", "Table 40.dta", "Table 41.dta",
  "Table 43.dta", "Table 44.dta", "Table 45.dta", "Table 46.dta", "Table 47.dta",
  "Table 48.dta", "Table 49.dta", "Table 50.dta", "Table 51.dta", "Table 52.dta",
  "Table 53.dta", "Table 54.dta", "Table 55.dta", "Table 56.dta", "Table 57.dta",
  "Table 59.dta", "Table 61.dta", "Table 62.dta", "Table 63.dta", "Table 64.dta",
  "Table 65.dta", "Table 66.dta", "Table 67.dta", "Table 68.dta", "Table 70.dta"
)

df_all <- data.frame()

for (f in append_files) {
  if (file.exists(f)) {
    df_new <- read_dta(f)
    df_all <- bind_rows(df_all, df_new)
  }
}

################################################################################
# Part C: Data cleaning after append
################################################################################

# drop P Q R S T if exist
df_all <- df_all %>%
  select(-any_of(c("P","Q","R","S","T")))

# replace municipality=upper(municipality)
if ("municipality" %in% names(df_all)) {
  df_all <- df_all %>%
    mutate(municipality = toupper(municipality))
}

# destring *, replace => in R, parse columns as numeric if they are characters:
df_all <- df_all %>%
  mutate(across(PAN:PVEM_PS, ~ suppressWarnings(as.numeric(.))))

# drop if strpos(municipality,"TOTAL")>0 or municipality==""
df_all <- df_all %>%
  filter(!str_detect(municipality, "TOTAL")) %>%
  filter(municipality != "")

# collapse (sum) by (municipality, section): columns from PAN to PVEM_PS
collapse_cols2 <- names(df_all)
# We'll guess you want from "PAN" through "PVEM_PS". Adapt as needed.

df_collapsed <- df_all %>%
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge lista nominal
df_2013 <- df_2013 %>%
  left_join(ln_2013, by = "section")

# Rename parties
df_2013 <- df_2013 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, 
         MC = mc, PANAL = panal, PAC = pac, PS = ps, PC = pc)

df_2013 <- df_2013 %>%
  mutate(turnout = total / listanominal)

# Assign uniqueid
df_2013 <- df_2013 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_tlaxcala_uniqueid(municipality))

# Recalculate valid if needed
party_cols_2013 <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PANAL", "PAC", "PS",
                     "PAC_PS", "PAN_PAC", "PRD_PT", "PRI_PVEM", "PT_PAC", "MC")
party_cols_2013 <- party_cols_2013[party_cols_2013 %in% names(df_2013)]

df_2013 <- df_2013 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2013)), na.rm = TRUE))

# Calculate municipal aggregates
df_2013 <- df_2013 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2013 <- df_2013 %>%
  mutate(year = 2013,
         month = "July")

cat("2013 processed:", nrow(df_2013), "rows\n")

################################################################################
## 2013 Extraordinary (December) - APETATITLAN DE ANTONIO CARVAJAL
################################################################################

<<<<<<< HEAD
df_2013_ext <- read_excel("Resultados Extraordinaria 8 Diciembre 2013.xlsx", sheet = "Sheet1")
=======
# preserve
df_uniqueids <- read_excel("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/uniqueids16.xlsx", col_names=TRUE) %>%
  as.data.frame()
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

df_2013_ext <- df_2013_ext %>%
  rename(section = `Sección`,
         total = EMIT,
         valid = VALID,
         PVEM = VERDE) %>%
  mutate(uniqueid = 29002,
         municipality = "APETATITLAN DE ANTONIO CARVAJAL EXTRAORDINARIO")

# Select relevant columns
party_cols_ext <- c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "PAC", "PS")
party_cols_ext <- party_cols_ext[party_cols_ext %in% names(df_2013_ext)]

df_2013_ext <- df_2013_ext %>%
  select(municipality, uniqueid, section, all_of(party_cols_ext), total, valid)

# Collapse
df_2013_ext <- df_2013_ext %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge lista nominal from all_months_years
all_months_2013_ext <- read_dta("../../all_months_years.dta") %>%
  filter(ed == 29, month == 11, year == 2013) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2013_ext <- df_2013_ext %>%
  left_join(all_months_2013_ext, by = "section")

df_2013_ext <- df_2013_ext %>%
  mutate(turnout = total / listanominal) %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2013_ext <- df_2013_ext %>%
  mutate(year = 2013,
         month = "December")

cat("2013 Extraordinary processed:", nrow(df_2013_ext), "rows\n")

################################################################################
## 2014 Extraordinary (February) - ACUAMANALA DE MIGUEL HIDALGO
################################################################################

df_2014_ext <- read_excel("Resultados Extraordinaria 23 Febrero 2014.xlsx", 
                          sheet = "Hoja1", range = "A6:K13")

<<<<<<< HEAD
df_2014_ext <- df_2014_ext %>%
  rename(section = `Sección`,
         total = EMIT,
         valid = VALID) %>%
  mutate(uniqueid = 29022,
         municipality = "ACUAMANALA DE MIGUEL HIDALGO EXTRAORDINARIO")

# Select relevant columns
party_cols_2014_ext <- c("PAN", "PRI", "PRD", "PT", "MC")
party_cols_2014_ext <- party_cols_2014_ext[party_cols_2014_ext %in% names(df_2014_ext)]

df_2014_ext <- df_2014_ext %>%
  select(municipality, uniqueid, section, all_of(party_cols_2014_ext), total, valid)

# Collapse
df_2014_ext <- df_2014_ext %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge lista nominal from all_months_years
all_months_2014_ext <- read_dta("../../all_months_years.dta") %>%
  filter(ed == 29, month == 1, year == 2014) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2014_ext <- df_2014_ext %>%
  left_join(all_months_2014_ext, by = "section")

df_2014_ext <- df_2014_ext %>%
  mutate(turnout = total / listanominal) %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2014_ext <- df_2014_ext %>%
  mutate(year = 2014,
         month = "February")

cat("2014 Extraordinary processed:", nrow(df_2014_ext), "rows\n")
=======
df_merge_ids <- df_merge_ids %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(any_of(valid_cols)), na.rm=TRUE),
    section = as.numeric(section)
  ) %>%
  ungroup() %>%
  mutate(
    total = valid + coalesce(no_reg,0) + coalesce(nulo,0)
  ) %>%
  select(-any_of(c("no_reg","nulo"))) %>%
  mutate(
    year  = 2016,
    month = "June",
    STATE = "TLAXCALA"
  )
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

################################################################################
## 2016 (SALVADOR)
################################################################################

<<<<<<< HEAD
# Read all sheets from Excel file
sheets_2016 <- excel_sheets("Ayuntamientos_Tlaxcala_2016.xlsx")
=======
# preserve => in R we won't do it the same, we'll just read LN2016:
df_ln16 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2016/LN2016.dta") %>%
  filter(entidad==29, month==5) %>%
  mutate(
    uniqueid = (entidad*1000) + municipio
  ) %>%
  filter(seccion!=0) %>%
  arrange(uniqueid,seccion) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section=seccion)
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# Table numbers to read (matching SALVADOR do-file)
table_nums <- c(1,2,3,4,5,7,8,9,11,13,15,16,17,18,19,20,22,23,25,26,27,28,29,30,
                31,32,33,34,35,36,37,38,39,40,41,43,44,45,46,47,48,49,50,51,52,
                53,54,55,56,57,59,61,62,63,64,65,66,67,68,70)

table_sheets <- paste0("Table ", table_nums)
table_sheets <- table_sheets[table_sheets %in% sheets_2016]

<<<<<<< HEAD
df_2016_list <- lapply(table_sheets, function(sheet) {
  tryCatch({
    df <- read_excel("Ayuntamientos_Tlaxcala_2016.xlsx", sheet = sheet, 
                     col_types = "text")
    names(df) <- tolower(names(df))
    df <- df %>% filter(!is.na(section) & section != "")
    # Replace empty with 0
    df <- df %>% mutate(across(everything(), ~ifelse(is.na(.) | . == "", "0", .)))
    df
  }, error = function(e) NULL)
})

df_2016 <- bind_rows(df_2016_list)

# Remove extra columns P Q R S T if they exist
df_2016 <- df_2016 %>%
  select(-matches("^p$|^q$|^r$|^s$|^t$", ignore.case = TRUE))
=======
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# Convert municipality to upper and filter
df_2016 <- df_2016 %>%
  mutate(municipality = toupper(municipality)) %>%
  filter(!grepl("TOTAL", municipality)) %>%
  filter(!is.na(municipality) & municipality != "")

# Convert numeric columns
df_2016 <- df_2016 %>%
  mutate(across(-c(municipality), ~as.numeric(as.character(.))))

# Rename section
df_2016 <- df_2016 %>%
  rename(section = section)

# Collapse by municipality and section
df_2016 <- df_2016 %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Read uniqueids mapping
uniqueids_2016 <- read_excel("uniqueids16.xlsx") %>%
  rename_all(tolower) %>%
  select(municipality, uniqueid, municipio) %>%
  distinct()

df_2016 <- df_2016 %>%
  left_join(uniqueids_2016, by = "municipality") %>%
  mutate(municipality = ifelse(!is.na(municipio), municipio, municipality)) %>%
  select(-municipio)

# Drop validos and total columns if they exist, recalculate
df_2016 <- df_2016 %>%
  select(-matches("^validos$|^total$", ignore.case = TRUE))

# Rename columns to uppercase
df_2016 <- df_2016 %>%
  rename_with(~toupper(.), -c(municipality, section, uniqueid))

# Calculate valid votes - parties per SALVADOR do-file
party_cols_2016 <- c("PAN", "PRI", "PRD_PT", "PVEM", "MC", "PANAL", "PAC", "PS", 
                     "MORENA", "PES", "PRD", "PT", "PRI_PANAL", "PRI_PVEM_PANAL_PS",
                     "PRI_PVEM", "PRI_PANAL_PS", "PRI_PS", "PRI_PT", "PRI_PVEM_PANAL",
                     "PVEM_PS", "CI_1", "CI_2", "CI_3", "CI_4")
party_cols_2016 <- party_cols_2016[party_cols_2016 %in% names(df_2016)]

# Handle missing columns
for (col in c("NO_REG", "NULO")) {
  if (!col %in% names(df_2016)) {
    df_2016[[col]] <- 0
  }
}

<<<<<<< HEAD
df_2016 <- df_2016 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2016)), na.rm = TRUE),
         total = valid + NO_REG + NULO) %>%
  select(-NO_REG, -NULO)

# Calculate municipal aggregates
df_2016 <- df_2016 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE)) %>%
  ungroup()
=======
#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

# Load the 2021 dataset from the excel
data_2021 <- read_excel("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/ayun_tlax_2021.xlsx") %>% 
  rename_with(~ gsub("CAND_IND", "CI_", .) %>% 
                gsub("CAND_IND0", "CI_", .)) %>% 
  rename(
    valid = NUM_VOTOS_VALIDOS,
    no_reg = NUM_VOTOS_CAN_NREG,
    nulos = NUM_VOTOS_NULOS,
    total = TOTAL_VOTOS,
    listanominal = LISTA_NOMINAL,
    section = SECCION,
    municipality = MUNICIPIO
  ) 

# Load the 
data_ext <- read_excel("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/ayun_tlax_2021_ext.xlsx") %>%
  rename_with(~ gsub("CAND_IND", "CI_", .)) %>% 
  rename(
    PANAL = PANALT,
    valid = NUMERO_VOTOS_VALIDOS,
    no_reg = NO_REGISTRADOS,
    nulos = NUM_VOTOS_NULOS,
    total = TOTAL_VOTOS,
    listanominal = LISTA_NOMINAL,
    section = SECCION,
    municipality = MUNICIPIO
  ) 

# Assign uniqueid
data_2021 <- bind_rows(data_ext, data_2021) %>%
  mutate(
    section = as.numeric(section),
    municipality = toupper(municipality),
    municipality = remove_accents(municipality),
    uniqueid = assign_tlaxcala_uniqueid(municipality)
    )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:listanominal), 
                          sum, na.rm = TRUE))

# Calculate valid votes and final details
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    year = 2021,
    month = case_when(
      municipality %in% c("ATLANGATEPEC", "CHIAUTEMPAN", "TEPETITLA DE LARDIZABAL", "NANACAMILPA DE MARIANO ARISTA", "TOTOLAC") ~ "November",
      TRUE ~ "June"
    )
  )


#####################################
### PROCESSING DATA FOR 2024 -------
#####################################

# Load the 2024 dataset from the excel
data_2024 <- read_excel("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016,2021,2024/2024_SEE_AYUN_TLAX_CAS.xlsx") %>% 
  rename(
    municipality = MUNICIPIO,
    section = SECCION,
    no_reg = NUM_VOTOS_CAN_NREG,
    valid = NUM_VOTOS_VALIDOS,
    nulos = NUM_VOTOS_NULOS,
    total = TOTAL_VOTOS,
    listanominal = LISTA_NOMINAL
  ) %>% 
  rename_with(~ gsub("PANALT", "PANAL", .) %>% 
                  gsub("CAND_IND0", "CI_", .))

# Assign uniqueid
data_2024 <- data_2024 %>%
  mutate(
    section = as.numeric(section),
    municipality = toupper(municipality),
    municipality = remove_accents(municipality),
    uniqueid = assign_tlaxcala_uniqueid(municipality)
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:total, listanominal), 
                          sum, na.rm = TRUE))

# Calculate valid votes and final details
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    year = 2024,
    month = "June"
    )

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 29) %>% 
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
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

# Combine the dataframes, handling different columns by filling with NA
tlaxcala_all <- bind_rows(df_2001,
                          df_2004,
                          df_2007,
                          df_2010,
                          df_2013,
                          df_extra_2013,
                          df_2014,
                          df_final,
                          collapsed_2021,
                          collapsed_2024
)
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# Merge lista nominal 2016
ln_2016 <- read_dta("../Listas Nominales/LN 2012-2019/2016/LN2016.dta") %>%
  filter(entidad == 29, month == 5) %>%
  mutate(uniqueid = entidad * 1000 + municipio) %>%
  filter(seccion != 0) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2016 <- df_2016 %>%
  left_join(ln_2016, by = c("uniqueid", "section"))

df_2016 <- df_2016 %>%
  group_by(uniqueid) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mun_turnout = mun_total / mun_listanominal,
         turnout = total / listanominal)

df_2016 <- df_2016 %>%
  mutate(year = 2016,
         month = "June",
         STATE = "TLAXCALA")

cat("2016 processed:", nrow(df_2016), "rows\n")

################################################################################
## Append All Years
################################################################################

# Standardize columns across all dataframes
all_years <- list(df_2001, df_2004, df_2007, df_2010, df_2013, df_2013_ext, df_2014_ext, df_2016)

# Get all unique column names
all_cols <- unique(unlist(lapply(all_years, names)))

# Ensure all dataframes have all columns
standardize_df <- function(df, all_cols) {
  for (col in all_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df
}

all_years <- lapply(all_years, standardize_df, all_cols)

# Combine all years
tlaxcala_all <- bind_rows(all_years)

################################################################################
## Final Corrections (from SALVADOR)
################################################################################

tlaxcala_all <- tlaxcala_all %>%
  mutate(municipality = case_when(
    municipality == "ALTZAYANCA" ~ "ATLTZAYANCA",
    municipality == "TEOLOCHOLCO" ~ "SAN LUIS TEOLOCHOLCO",
    TRUE ~ municipality
  ))

# Ensure uppercase
tlaxcala_all <- tlaxcala_all %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality))

# Final uniqueid assignment for any missing
tlaxcala_all <- tlaxcala_all %>%
  mutate(uniqueid = ifelse(is.na(uniqueid), assign_tlaxcala_uniqueid(municipality), uniqueid))

################################################################################
## Save Output
################################################################################

# Reorder columns
key_cols <- c("municipality", "uniqueid", "section", "year", "month")
other_cols <- setdiff(names(tlaxcala_all), key_cols)
tlaxcala_all <- tlaxcala_all %>%
  select(all_of(key_cols), all_of(other_cols))

# Save
data.table::fwrite(tlaxcala_all, "../../../Processed Data/tlaxcala/tlaxcala_process_raw_data.csv")

cat("\n=== TLAXCALA PROCESSING COMPLETE ===\n")
cat("Total observations:", nrow(tlaxcala_all), "\n")
cat("Years:", paste(sort(unique(tlaxcala_all$year)), collapse = ", "), "\n")
cat("Municipalities:", length(unique(tlaxcala_all$uniqueid)), "\n")
