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

# Collapse
df_2010 <- df_2010 %>%
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

# Read lista nominal 2013
ln_2013 <- read_excel("Listanominal2013.xlsx", sheet = "datos") %>%
  rename(section = section) %>%
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

df_2013_ext <- read_excel("Resultados Extraordinaria 8 Diciembre 2013.xlsx", sheet = "Sheet1")

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

################################################################################
## 2016 (SALVADOR)
################################################################################

# Read all sheets from Excel file
sheets_2016 <- excel_sheets("Ayuntamientos_Tlaxcala_2016.xlsx")

# Table numbers to read (matching SALVADOR do-file)
table_nums <- c(1,2,3,4,5,7,8,9,11,13,15,16,17,18,19,20,22,23,25,26,27,28,29,30,
                31,32,33,34,35,36,37,38,39,40,41,43,44,45,46,47,48,49,50,51,52,
                53,54,55,56,57,59,61,62,63,64,65,66,67,68,70)

table_sheets <- paste0("Table ", table_nums)
table_sheets <- table_sheets[table_sheets %in% sheets_2016]

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
