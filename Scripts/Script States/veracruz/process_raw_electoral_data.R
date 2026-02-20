################################################################################
## Process Raw Electoral Data - Veracruz
## Years: 2000, 2004, 2005_EXT, 2007, 2010, 2013, 2017, 2018_EXT
################################################################################

rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, haven, readxl, tidyverse, data.table, stringr)

# Set working directory
setwd("D:/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Veracruz 2000, 2004, 2007, 2010, 2013")

################################################################################
## Helper Functions
################################################################################

remove_accents <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("Á|á", "A", x)
  x <- gsub("É|é", "E", x)
  x <- gsub("Í|í", "I", x)
  x <- gsub("Ó|ó", "O", x)
  x <- gsub("Ú|ú", "U", x)
  x <- gsub("Ñ|ñ", "N", x)
  x <- gsub("ü|Ü", "U", x)
  return(x)
}

# Comprehensive uniqueid assignment for Veracruz (212 municipalities)
assign_veracruz_uniqueid <- function(municipality) {
  municipality <- toupper(remove_accents(municipality))
  case_when(
    grepl("^ACAJETE$", municipality) ~ 30001,
    grepl("^ACATLAN$", municipality) ~ 30002,
    grepl("^ACAYUCAN$", municipality) ~ 30003,
    grepl("^ACTOPAN$", municipality) ~ 30004,
    grepl("^ACULA$", municipality) ~ 30005,
    grepl("^ACULTZINGO$", municipality) ~ 30006,
    grepl("CAMARON DE TEJEDA", municipality) ~ 30007,
    grepl("ALPATLAHUAC", municipality) ~ 30008,
    grepl("ALTO LUCERO", municipality) ~ 30009,
    grepl("^ALTOTONGA$", municipality) ~ 30010,
    grepl("^ALVARADO$", municipality) ~ 30011,
    grepl("^AMATITLAN$", municipality) ~ 30012,
    grepl("NARANJOS AMATLAN", municipality) ~ 30013,
    grepl("AMATLAN DE LOS REYES", municipality) ~ 30014,
    grepl("ANGEL R", municipality) ~ 30015,
    grepl("^LA ANTIGUA$", municipality) ~ 30016,
    grepl("^APAZAPAN$", municipality) ~ 30017,
    grepl("^AQUILA$", municipality) ~ 30018,
    grepl("^ASTACINGA$", municipality) ~ 30019,
    grepl("ATLAHUICO|ATLAHUILCO", municipality) ~ 30020,
    grepl("^ATOYAC$", municipality) ~ 30021,
    grepl("^ATZACAN$", municipality) ~ 30022,
    grepl("^ATZALAN$", municipality) ~ 30023,
    grepl("TLALTETELA", municipality) ~ 30024,
    grepl("AYAHUALULCO", municipality) ~ 30025,
    grepl("^BANDERILLA$", municipality) ~ 30026,
    grepl("^BENITO JUAREZ$", municipality) ~ 30027,
    grepl("BOCA DEL RIO", municipality) ~ 30028,
    grepl("CALCAHUALCO", municipality) ~ 30029,
    grepl("CAMERINO Z", municipality) ~ 30030,
    grepl("CARRILLO PUERTO", municipality) ~ 30031,
    grepl("^CATEMACO$", municipality) ~ 30032,
    grepl("CAZONES", municipality) ~ 30033,
    grepl("CERRO AZUL", municipality) ~ 30034,
    grepl("CITLALT", municipality) ~ 30035,
    grepl("COACOATZINTLA", municipality) ~ 30036,
    grepl("COAHUITLAN", municipality) ~ 30037,
    grepl("^COATEPEC$", municipality) ~ 30038,
    grepl("COATZACOALCOS", municipality) ~ 30039,
    grepl("COATZINTLA", municipality) ~ 30040,
    grepl("COETZALA", municipality) ~ 30041,
    grepl("^COLIPA$", municipality) ~ 30042,
    grepl("^COMAPA$", municipality) ~ 30043,
    grepl("CORDOBA", municipality) ~ 30044,
    grepl("COSAMALOAPAN", municipality) ~ 30045,
    grepl("COSAUTLAN", municipality) ~ 30046,
    grepl("COSCOMATEPEC", municipality) ~ 30047,
    grepl("COSOLEACAQUE", municipality) ~ 30048,
    grepl("COTAXTLA", municipality) ~ 30049,
    grepl("COXQUIHUI", municipality) ~ 30050,
    grepl("COYUTLA", municipality) ~ 30051,
    grepl("^CUICHAPA$", municipality) ~ 30052,
    grepl("CUITLAHUAC", municipality) ~ 30053,
    grepl("CHACALTIANGUIS", municipality) ~ 30054,
    grepl("^CHALMA$", municipality) ~ 30055,
    grepl("CHICONAMEL", municipality) ~ 30056,
    grepl("CHICONQUIACO", municipality) ~ 30057,
    grepl("CHICONTEPEC", municipality) ~ 30058,
    grepl("^CHINAMECA$", municipality) ~ 30059,
    grepl("CHINAMPA", municipality) ~ 30060,
    grepl("LAS CHOAPAS", municipality) ~ 30061,
    grepl("CHOCAMAN", municipality) ~ 30062,
    grepl("^CHONTLA$", municipality) ~ 30063,
    grepl("CHUMATLAN", municipality) ~ 30064,
    grepl("^EMILIANO ZAPATA", municipality) ~ 30065,
    grepl("^ESPINAL$", municipality) ~ 30066,
    grepl("FILOMENO MATA", municipality) ~ 30067,
    grepl("^FORTIN$", municipality) ~ 30068,
    grepl("GUTIERREZ ZAMORA", municipality) ~ 30069,
    grepl("HIDALGOTITLAN", municipality) ~ 30070,
    grepl("^HUATUSCO$", municipality) ~ 30071,
    grepl("HUAYACOCOTLA", municipality) ~ 30072,
    grepl("HUEYAPAN", municipality) ~ 30073,
    grepl("HUILOAPAN", municipality) ~ 30074,
    grepl("IGNACIO DE LA LLAVE", municipality) ~ 30075,
    grepl("ILAMATLAN", municipality) ~ 30076,
    grepl("^ISLA$", municipality) ~ 30077,
    grepl("IXCATEPEC", municipality) ~ 30078,
    grepl("IXHUACAN DE LOS REYES|DE LOS REYES", municipality) & grepl("IXHUA", municipality) ~ 30079,
    grepl("IXHUATLAN DEL CAF", municipality) ~ 30080,
    grepl("IXHUATLANCILLO", municipality) ~ 30081,
    grepl("IXHUATLAN DEL SURESTE", municipality) ~ 30082,
    grepl("IXHUATLAN DE MADERO", municipality) ~ 30083,
    grepl("IXMATLAHUACAN", municipality) ~ 30084,
    grepl("IXTACZOQUITLAN", municipality) ~ 30085,
    grepl("^JALACINGO$", municipality) ~ 30086,
    grepl("^XALAPA$", municipality) ~ 30087,
    grepl("JALCOMULCO", municipality) ~ 30088,
    grepl("^JALTIPAN$", municipality) ~ 30089,
    grepl("^JAMAPA$", municipality) ~ 30090,
    grepl("JESUS CARRANZA", municipality) ~ 30091,
    grepl("^XICO$", municipality) ~ 30092,
    grepl("^JILOTEPEC$", municipality) ~ 30093,
    grepl("JUAN RODRIGUEZ CLARA", municipality) ~ 30094,
    grepl("JUCHIQUE DE FERRER", municipality) ~ 30095,
    grepl("LANDERO Y COSS", municipality) ~ 30096,
    grepl("LERDO DE TEJADA", municipality) ~ 30097,
    grepl("^MAGDALENA$", municipality) ~ 30098,
    grepl("^MALTRATA$", municipality) ~ 30099,
    grepl("MANLIO FABIO", municipality) ~ 30100,
    grepl("MARIANO ESCOBEDO", municipality) ~ 30101,
    grepl("MARTINEZ DE LA TORRE", municipality) ~ 30102,
    grepl("MECATLAN", municipality) ~ 30103,
    grepl("MECAYAPAN", municipality) ~ 30104,
    grepl("^MEDELLIN$", municipality) ~ 30105,
    grepl("MIAHUATLAN", municipality) ~ 30106,
    grepl("LAS MINAS", municipality) ~ 30107,
    grepl("MINATITLAN", municipality) ~ 30108,
    grepl("^MISANTLA$", municipality) ~ 30109,
    grepl("MIXTLA DE ALTAMIRANO", municipality) ~ 30110,
    grepl("MOLOACAN", municipality) ~ 30111,
    grepl("^NAOLINCO$", municipality) ~ 30112,
    grepl("^NARANJAL$", municipality) ~ 30113,
    grepl("^NAUTLA$", municipality) ~ 30114,
    grepl("^NOGALES$", municipality) ~ 30115,
    grepl("^OLUTA$", municipality) ~ 30116,
    grepl("^OMEALCA$", municipality) ~ 30117,
    grepl("^ORIZABA$", municipality) ~ 30118,
    grepl("OTATITLAN", municipality) ~ 30119,
    grepl("^OTEAPAN$", municipality) ~ 30120,
    grepl("OZULUAMA", municipality) ~ 30121,
    grepl("^PAJAPAN$", municipality) ~ 30122,
    grepl("^PANUCO$", municipality) ~ 30123,
    grepl("^PAPANTLA$", municipality) ~ 30124,
    grepl("PASO DEL MACHO", municipality) ~ 30125,
    grepl("PASO DE OVEJAS", municipality) ~ 30126,
    grepl("LA PERLA", municipality) ~ 30127,
    grepl("^PEROTE$", municipality) ~ 30128,
    grepl("PLATON SANCHEZ|PLAT.N S.NCHEZ", municipality) ~ 30129,
    grepl("PLAYA VICENTE", municipality) ~ 30130,
    grepl("POZA RICA|POZARICA", municipality) ~ 30131,
    grepl("LAS VIGAS", municipality) ~ 30132,
    grepl("PUEBLO VIEJO", municipality) ~ 30133,
    grepl("PUENTE NACIONAL", municipality) ~ 30134,
    grepl("RAFAEL DELGADO", municipality) ~ 30135,
    grepl("RAFAEL LUCIO", municipality) ~ 30136,
    grepl("LOS REYES", municipality) & !grepl("IXHUA", municipality) ~ 30137,
    grepl("RIO BLANCO", municipality) ~ 30138,
    grepl("SALTABARRANCA", municipality) ~ 30139,
    grepl("SAN ANDRES TENEJAPAN", municipality) ~ 30140,
    grepl("SAN ANDRES TUXTLA", municipality) ~ 30141,
    grepl("SAN JUAN EVANGELISTA", municipality) ~ 30142,
    grepl("SANTIAGO TUXTLA", municipality) ~ 30143,
    grepl("SAYULA DE ALEMAN", municipality) ~ 30144,
    grepl("^SOCONUSCO$", municipality) ~ 30145,
    grepl("^SOCHIAPA$", municipality) ~ 30146,
    grepl("SOLEDAD ATZOMPA", municipality) ~ 30147,
    grepl("SOLEDAD DE DOBLADO", municipality) ~ 30148,
    grepl("SOTEAPAN", municipality) ~ 30149,
    grepl("^TAMALIN$", municipality) ~ 30150,
    grepl("^TAMIAHUA$", municipality) ~ 30151,
    grepl("TAMPICO ALTO", municipality) ~ 30152,
    grepl("^TANCOCO$", municipality) ~ 30153,
    grepl("^TANTIMA$", municipality) ~ 30154,
    grepl("^TANTOYUCA$", municipality) ~ 30155,
    grepl("^TATATILA$", municipality) ~ 30156,
    grepl("CASTILLO DE TEAYO", municipality) ~ 30157,
    grepl("^TECOLUTLA$", municipality) ~ 30158,
    grepl("TEHUIPANGO", municipality) ~ 30159,
    grepl("TEMAPACHE|^ALAMO$", municipality) ~ 30160,
    grepl("^TEMPOAL$", municipality) ~ 30161,
    grepl("^TENAMPA$", municipality) ~ 30162,
    grepl("TENOCHTITLAN", municipality) ~ 30163,
    grepl("^TEOCELO$", municipality) ~ 30164,
    grepl("TEPATLAXCO", municipality) ~ 30165,
    grepl("^TEPETLAN$", municipality) ~ 30166,
    grepl("TEPETZINTLA", municipality) ~ 30167,
    grepl("^TEQUILA$", municipality) ~ 30168,
    grepl("JOSE AZUETA", municipality) ~ 30169,
    grepl("TEXCATEPEC", municipality) ~ 30170,
    grepl("TEXHUACAN", municipality) ~ 30171,
    grepl("TEXISTEPEC", municipality) ~ 30172,
    grepl("TEZONAPA", municipality) ~ 30173,
    grepl("TIERRA BLANCA", municipality) ~ 30174,
    grepl("TIHUATLAN", municipality) ~ 30175,
    grepl("TLACOJALPAN", municipality) ~ 30176,
    grepl("TLACOLULAN", municipality) ~ 30177,
    grepl("TLACOTALPAN", municipality) ~ 30178,
    grepl("TLACOTEPEC DE MEJIA", municipality) ~ 30179,
    grepl("TLACHICHILCO", municipality) ~ 30180,
    grepl("TLALIXCOYAN", municipality) ~ 30181,
    grepl("TLALNELHUAYOCAN", municipality) ~ 30182,
    grepl("TLAPACOYAN", municipality) ~ 30183,
    grepl("TLAQUILPA", municipality) ~ 30184,
    grepl("TLILAPAN", municipality) ~ 30185,
    grepl("^TOMATLAN$", municipality) ~ 30186,
    grepl("^TONAYAN$", municipality) ~ 30187,
    grepl("^TOTUTLA$", municipality) ~ 30188,
    grepl("^TUXPAN$", municipality) ~ 30189,
    grepl("^TUXTILLA$", municipality) ~ 30190,
    grepl("URSULO GALVAN", municipality) ~ 30191,
    grepl("VEGA DE ALATORRE", municipality) ~ 30192,
    grepl("^VERACRUZ$", municipality) ~ 30193,
    grepl("VILLA ALDAMA", municipality) ~ 30194,
    grepl("XOXOCOTLA", municipality) ~ 30195,
    grepl("^YANGA$", municipality) ~ 30196,
    grepl("YECUATLA", municipality) ~ 30197,
    grepl("ZACUALPAN", municipality) ~ 30198,
    grepl("^ZARAGOZA$", municipality) ~ 30199,
    grepl("^ZENTLA$", municipality) ~ 30200,
    grepl("^ZONGOLICA$", municipality) ~ 30201,
    grepl("ZONTECOMATLAN", municipality) ~ 30202,
    grepl("ZOZOCOLCO", municipality) ~ 30203,
    grepl("AGUA DULCE", municipality) ~ 30204,
    grepl("EL HIGO", municipality) ~ 30205,
    grepl("NANCHITAL", municipality) ~ 30206,
    grepl("TRES VALLES", municipality) ~ 30207,
    grepl("CARLOS A. CARRILLO", municipality) ~ 30208,
    grepl("TATAHUICAPAN", municipality) ~ 30209,
    grepl("UXPANAPA", municipality) ~ 30210,
    grepl("SAN RAFAEL", municipality) ~ 30211,
    grepl("SANTIAGO SOCHIAPAN", municipality) ~ 30212,
    TRUE ~ NA_real_
  )
}

################################################################################
## 2000 - Loaded from external script (DoFile_Section_Veracruz2000.do)
## If this file exists, it would be processed separately
################################################################################

# Placeholder for 2000 - typically would source external do-file
# df_2000 <- ... (process from DoFile_Section_Veracruz2000.do)
cat("Note: 2000 data should be processed from DoFile_Section_Veracruz2000.do\n")

################################################################################
## 2004
################################################################################

# Read lista nominal
ln_2004 <- read_excel("Lisado Nominal 2004.xlsx", sheet = "Sheet1") %>%
  select(section, listanominal) %>%
  filter(!is.na(section) & !is.na(listanominal))

# Read main data
df_2004 <- read.csv("Ayu_Seccion_2004_No_LN.csv", stringsAsFactors = FALSE)
names(df_2004) <- tolower(names(df_2004))

df_2004 <- df_2004 %>%
  rename(municipality = municipio,
         section = secc) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.))))

# Collapse
df_2004 <- df_2004 %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Calculate total
df_2004 <- df_2004 %>%
  mutate(total = rowSums(select(., pan, fide, prd, prv, no_reg, nulos), na.rm = TRUE)) %>%
  filter(!is.na(total) & total > 0)

# Rename parties
df_2004 <- df_2004 %>%
  rename(PAN = pan,
         PRI_PVEM = fide,
         PRD_PT_PC = prd,
         PRV = prv) %>%
  select(-nulos, -no_reg)

# Merge lista nominal
df_2004 <- df_2004 %>%
  left_join(ln_2004, by = "section")

df_2004 <- df_2004 %>%
  mutate(turnout = total / listanominal)

# Assign uniqueid
df_2004 <- df_2004 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_veracruz_uniqueid(municipality))

# Calculate valid
df_2004 <- df_2004 %>%
  mutate(valid = rowSums(select(., PAN, PRI_PVEM, PRD_PT_PC, PRV), na.rm = TRUE))

# Municipal aggregates
df_2004 <- df_2004 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2004 <- df_2004 %>%
  mutate(year = 2004,
         month = "September")

cat("2004 processed:", nrow(df_2004), "rows\n")

################################################################################
## 2005 Extraordinary - LANDERO Y COSS (uniqueid 30096)
################################################################################

df_2005 <- read_excel("Extraordinario 2005.xlsx", sheet = "Sheet1")

df_2005 <- df_2005 %>%
  mutate(section = as.numeric(CASILLA),
         section = ifelse(row_number() >= 2, 2220, section)) %>%
  mutate(total = rowSums(select(., PAN, PRI, PT, NR, NULO), na.rm = TRUE))

df_2005 <- df_2005 %>%
  group_by(municipality, section) %>%
  summarise(PAN = sum(PAN, na.rm = TRUE),
            PRI = sum(PRI, na.rm = TRUE),
            PT = sum(PT, na.rm = TRUE),
            total = sum(total, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(uniqueid = 30096)

# Merge lista nominal
all_months_2005 <- read_dta("../../all_months_years.dta") %>%
  filter(ed == 30, month == 6, year == 2005) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2005 <- df_2005 %>%
  left_join(all_months_2005, by = "section")

df_2005 <- df_2005 %>%
  mutate(turnout = total / listanominal,
         valid = rowSums(select(., PAN, PRI, PT), na.rm = TRUE))

df_2005 <- df_2005 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2005 <- df_2005 %>%
  mutate(year = 2005,
         month = "June")

cat("2005 Extraordinary processed:", nrow(df_2005), "rows\n")

################################################################################
## 2007
################################################################################

# Read lista nominal
ln_2007 <- read_excel("Lisado Nominal 2007.xlsx", sheet = "Sheet1") %>%
  rename_with(~"section", matches("SECC|SECCIÓN")) %>%
  rename(listanominal = LISTA) %>%
  filter(!is.na(section) & section != "total Distrital" & !is.na(listanominal)) %>%
  mutate(section = as.numeric(section))

# Read main data
df_2007 <- read.csv("Ayu_Seccion_2007_No_LN.csv", stringsAsFactors = FALSE)
names(df_2007) <- tolower(names(df_2007))

df_2007 <- df_2007 %>%
  rename(municipality = municipio,
         section = secc) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.))))

# Collapse
df_2007 <- df_2007 %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
  filter(total > 0)

# Rename parties
df_2007 <- df_2007 %>%
  rename(PAN = pan,
         PRI_PVEM = pripvem,
         PRI_PVEM_PANAL = pripvempanal,
         PRD = prd,
         PRD_PT_PC = prdptpc,
         PT_PC = ptpc,
         PT = pt,
         PC = pc,
         PRV = prv,
         PAS = pas,
         PANAL = panal,
         MC = mc)

# Special correction for HUEYAPAN DE OCAMPO
df_2007 <- df_2007 %>%
  mutate(PRI_PVEM_PANAL = ifelse(municipality == "HUEYAPAN DE OCAMPO", 
                                  PRI_PVEM_PANAL + PANAL, PRI_PVEM_PANAL),
         PANAL = ifelse(municipality == "HUEYAPAN DE OCAMPO", 0, PANAL))

# Merge lista nominal
df_2007 <- df_2007 %>%
  left_join(ln_2007, by = "section")

df_2007 <- df_2007 %>%
  mutate(turnout = total / listanominal)

# Drop nulos and noregistrados
df_2007 <- df_2007 %>%
  select(-matches("^nulos$|^noregistrados$", ignore.case = TRUE))

# Assign uniqueid
df_2007 <- df_2007 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_veracruz_uniqueid(municipality))

# Calculate valid
party_cols_2007 <- c("PAN", "PRI_PVEM_PANAL", "PRI_PVEM", "PRD", "PRD_PT_PC", 
                     "PT_PC", "PT", "PC", "PRV", "PAS", "PANAL", "MC")
party_cols_2007 <- party_cols_2007[party_cols_2007 %in% names(df_2007)]

df_2007 <- df_2007 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2007)), na.rm = TRUE))

# Municipal aggregates
df_2007 <- df_2007 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2007 <- df_2007 %>%
  mutate(year = 2007,
         month = "September")

cat("2007 processed:", nrow(df_2007), "rows\n")

################################################################################
## 2010
################################################################################

# Read lista nominal
ln_2010 <- read_excel("Lisado Nominal 2010.xlsx", sheet = "Sheet1") %>%
  rename(section = SECCION,
         listanominal = LISTA) %>%
  filter(!is.na(section) & !is.na(listanominal)) %>%
  select(section, listanominal)

# Read main data
df_2010 <- read.csv("Ayu_Seccion_2010_No_LN.csv", stringsAsFactors = FALSE)
names(df_2010) <- tolower(names(df_2010))

df_2010 <- df_2010 %>%
  rename(municipality = municipio,
         section = secc) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.)))) %>%
  filter(!is.na(total) & total > 0)

# Collapse
df_2010 <- df_2010 %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Create section-level dummies for coalition presence
df_2010 <- df_2010 %>%
  mutate(sec_dummy_pan_panal = ifelse(!is.na(panpanal) & panpanal > 0, 1, 0),
         sec_dummy_pri_pvem_prv = ifelse(!is.na(pripvemprv) & pripvemprv > 0, 1, 0),
         sec_dummy_prd_pt_pc = ifelse(!is.na(prdptpc) & prdptpc > 0, 1, 0))

# Calculate municipality-level dummies
df_2010 <- df_2010 %>%
  group_by(municipality) %>%
  mutate(dummy_pan_panal = max(sec_dummy_pan_panal, na.rm = TRUE),
         dummy_pri_pvem_prv = max(sec_dummy_pri_pvem_prv, na.rm = TRUE),
         dummy_prd_pt_pc = max(sec_dummy_prd_pt_pc, na.rm = TRUE)) %>%
  ungroup()

# Special correction for JALCOMULCO
df_2010 <- df_2010 %>%
  mutate(dummy_pri_pvem_prv = ifelse(municipality == "JALCOMULCO", 1, dummy_pri_pvem_prv))

# Create coalition columns
df_2010 <- df_2010 %>%
  mutate(
    PAN_PANAL = ifelse(dummy_pan_panal == 1, pan + panal + panpanal, NA),
    pan = ifelse(dummy_pan_panal == 1, NA, pan),
    panal = ifelse(dummy_pan_panal == 1, NA, panal),
    
    PRI_PVEM_PRV = ifelse(dummy_pri_pvem_prv == 1, pripvemprv + pri + pvem + prv, NA),
    pri = ifelse(dummy_pri_pvem_prv == 1, NA, pri),
    pvem = ifelse(dummy_pri_pvem_prv == 1, NA, pvem),
    prv = ifelse(dummy_pri_pvem_prv == 1, NA, prv),
    
    PRD_PT_PC = ifelse(dummy_prd_pt_pc == 1, prdptpc + prd + pt + pc, NA),
    prd = ifelse(dummy_prd_pt_pc == 1, NA, prd),
    pt = ifelse(dummy_prd_pt_pc == 1, NA, pt),
    pc = ifelse(dummy_prd_pt_pc == 1, NA, pc)
  )

# Rename remaining columns
df_2010 <- df_2010 %>%
  rename(PAN = pan,
         PRD = prd,
         PT = pt,
         PC = pc,
         PANAL = panal)

# Merge lista nominal
df_2010 <- df_2010 %>%
  left_join(ln_2010, by = "section")

df_2010 <- df_2010 %>%
  mutate(turnout = total / listanominal)

# Drop unnecessary columns
df_2010 <- df_2010 %>%
  select(-matches("^nulos$|^noregistrados$|^dummy_|^sec_dummy_|^panpanal$|^pripvemprv$|^prdptpc$|^pri$|^pvem$|^prv$", 
                  ignore.case = TRUE))

# Assign uniqueid
df_2010 <- df_2010 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_veracruz_uniqueid(municipality))

# Calculate valid
party_cols_2010 <- c("PAN", "PANAL", "PRD", "PT", "PC", "PAN_PANAL", "PRI_PVEM_PRV", "PRD_PT_PC")
party_cols_2010 <- party_cols_2010[party_cols_2010 %in% names(df_2010)]

df_2010 <- df_2010 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2010)), na.rm = TRUE))

# Municipal aggregates
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

df_2013 <- read_dta("Ayu_Seccion_2013_No_LN.dta")

# Rename columns
df_2013 <- df_2013 %>%
  rename(PC = MC,
         PartCardenista = PFCRN)

# Drop nulos and noregistrados
df_2013 <- df_2013 %>%
  select(-matches("NoRegistrados|Nulos"))

# Assign uniqueid
df_2013 <- df_2013 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_veracruz_uniqueid(municipality))

# Calculate valid
party_cols_2013 <- c("PAN", "PRI_PVEM_PANAL", "PRD", "PT", "PC", "AVE", "PartCardenista")
party_cols_2013 <- party_cols_2013[party_cols_2013 %in% names(df_2013)]

df_2013 <- df_2013 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2013)), na.rm = TRUE))

# Merge lista nominal from all_months_years
all_months_2013 <- read_dta("../../all_months_years.dta") %>%
  filter(ed == 30, month == 6, year == 2013) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2013 <- df_2013 %>%
  left_join(all_months_2013, by = "section")

df_2013 <- df_2013 %>%
  mutate(turnout = total / listanominal)

# Municipal aggregates
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
## 2017 (SALVADOR) - Elections held 2016-2017
################################################################################

# Read all sheets from Ayuntamientos_2016.xlsx and Ayuntamientos_2016b.xlsx
# First file - main results
sheets_2016 <- excel_sheets("Ayuntamientos_2016.xlsx")

# List of sheets to skip (numbered gaps in SALVADOR file)
sheet_nums_main <- c(1:8, 10:162, 164:169, 171:178)
sheet_names_main <- paste0("Resultados", c("", paste0(" (", 2:178, ")")))
sheet_names_main <- sheet_names_main[sheet_nums_main]
sheet_names_main <- sheet_names_main[sheet_names_main %in% sheets_2016]

df_2017_list_main <- lapply(sheets_2016, function(sheet) {
  tryCatch({
    df <- read_excel("Ayuntamientos_2016.xlsx", sheet = sheet, col_types = "text")
    names(df) <- tolower(names(df))
    # Fill municipality from first row
    if ("municipality" %in% names(df)) {
      df$municipality <- df$municipality[1]
      df <- df %>% filter(!is.na(seccion) & seccion != "" & seccion != "TOTAL")
    }
    # Replace empty with 0
    df <- df %>% mutate(across(everything(), ~ifelse(is.na(.) | . == "", "0", .)))
    df
  }, error = function(e) NULL)
})

df_2017_main <- bind_rows(df_2017_list_main)

# Second file - additional municipalities (Ayuntamientos_2016b.xlsx)
if (file.exists("AyuntamientosB/Ayuntamientos_2016b.xlsx")) {
  sheets_2016b <- excel_sheets("AyuntamientosB/Ayuntamientos_2016b.xlsx")
  
  df_2017_list_b <- lapply(sheets_2016b, function(sheet) {
    tryCatch({
      df <- read_excel("AyuntamientosB/Ayuntamientos_2016b.xlsx", sheet = sheet, col_types = "text")
      names(df) <- tolower(names(df))
      if ("municipality" %in% names(df)) {
        df$municipality <- df$municipality[1]
        df <- df %>% filter(!is.na(seccion) & seccion != "" & seccion != "TOTAL")
      }
      df <- df %>% mutate(across(everything(), ~ifelse(is.na(.) | . == "", "0", .)))
      df
    }, error = function(e) NULL)
  })
  
  df_2017_b <- bind_rows(df_2017_list_b) %>%
    filter(!municipality %in% c("Jaltipan", "Zontecomatlán"))
  
  df_2017 <- bind_rows(df_2017_main, df_2017_b)
} else {
  df_2017 <- df_2017_main
}

# Clean municipality name
df_2017 <- df_2017 %>%
  mutate(municipality = gsub("Reporte de Casillas del municipio de ", "", municipality))

# Drop columns O, P if they exist
df_2017 <- df_2017 %>%
  select(-matches("^o$|^p$", ignore.case = TRUE))

# Rename section
df_2017 <- df_2017 %>%
  rename(section = seccion)

# Convert to numeric
df_2017 <- df_2017 %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.))))

# Handle coalition variants with different spellings/typos
# PAN_PRD coalition
df_2017 <- df_2017 %>%
  mutate(across(matches("CoalicionPANPRD|CoaliciónPANPRD", ignore.case = TRUE),
                ~. + ifelse(!is.na(PAN), PAN, 0) + ifelse(!is.na(PRD), PRD, 0), .names = "temp_panprd"))

if ("temp_panprd" %in% names(df_2017)) {
  df_2017 <- df_2017 %>%
    mutate(PAN_PRD = temp_panprd) %>%
    select(-temp_panprd)
}

# PRI_PVEM coalition
df_2017 <- df_2017 %>%
  mutate(across(matches("CoalicionPRIVERDE|CoalicionPriverde", ignore.case = TRUE),
                ~. + ifelse(!is.na(PRI), PRI, 0) + ifelse(!is.na(VERDE), VERDE, 0) + 
                  ifelse(!is.na(PVEM), PVEM, 0), .names = "temp_priverde"))

if ("temp_priverde" %in% names(df_2017)) {
  df_2017 <- df_2017 %>%
    mutate(PRI_PVEM = temp_priverde) %>%
    select(-temp_priverde, -matches("CoalicionPRIVERDE|CoalicionPriverde", ignore.case = TRUE))
}

# Standardize party name variants
# PANAL (multiple spellings)
df_2017 <- df_2017 %>%
  mutate(PANAL = rowSums(select(., matches("NuevaAlianza|NuevaALianza|Nuevaalianza", ignore.case = TRUE)), na.rm = TRUE))

# MC (typo variants)
df_2017 <- df_2017 %>%
  mutate(MC = rowSums(select(., matches("MovimientoCiudadano|MovimientoCuidadano", ignore.case = TRUE)), na.rm = TRUE))

# PT (case variants)
df_2017 <- df_2017 %>%
  mutate(PT = rowSums(select(., matches("^PT$|^Pt$|^pt$")), na.rm = TRUE))

# PES
df_2017 <- df_2017 %>%
  rename_with(~"PES", matches("EncuentroSocial"))

# MORENA
df_2017 <- df_2017 %>%
  mutate(MORENA = rowSums(select(., matches("^MORENA$|^Morena$", ignore.case = TRUE)), na.rm = TRUE))

# Independent candidates (CI_1, CI_2, CI_3)
ci_cols <- names(df_2017)[grepl("Perez|Juarez|Marin|Martinez|Dionisio|Becerra|Pimentel|Andrade|Hernandez|Penaloza|Chazaro|Archer", 
                                 names(df_2017), ignore.case = TRUE)]
if (length(ci_cols) > 0) {
  df_2017 <- df_2017 %>%
    mutate(CI_1 = rowSums(select(., all_of(ci_cols)), na.rm = TRUE))
}

# Collapse by municipality and section
df_2017 <- df_2017 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Handle nulos columns
nulo_cols <- names(df_2017)[grepl("Q|VotosNulos", names(df_2017), ignore.case = TRUE)]
df_2017 <- df_2017 %>%
  mutate(nulo = rowSums(select(., any_of(nulo_cols)), na.rm = TRUE))

# Calculate valid and total
party_cols_2017 <- c("PAN", "PRI", "PRD", "PVEM", "PT", "PANAL", "MC", "MORENA", "PES", 
                     "PRI_PVEM", "PAN_PRD", "CI_1", "CI_2", "CI_3")
party_cols_2017 <- party_cols_2017[party_cols_2017 %in% names(df_2017)]

df_2017 <- df_2017 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2017)), na.rm = TRUE),
         total = valid + nulo)

# Read uniqueids mapping
uniqueids_2017 <- read_excel("uniqueids.xlsx") %>%
  rename_all(tolower) %>%
  select(municipality, uniqueid, MUN) %>%
  distinct()

df_2017 <- df_2017 %>%
  left_join(uniqueids_2017, by = "municipality") %>%
  mutate(municipality = ifelse(!is.na(MUN), MUN, municipality)) %>%
  select(-MUN)

# Assign uniqueid for any missing
df_2017 <- df_2017 %>%
  mutate(municipality = toupper(remove_accents(municipality)),
         uniqueid = ifelse(is.na(uniqueid), assign_veracruz_uniqueid(municipality), uniqueid))

# Filter out missing sections
df_2017 <- df_2017 %>%
  filter(!is.na(section))

# Merge lista nominal 2017
ln_2017 <- read_dta("../Listas Nominales/LN 2012-2019/2017/LN2017.dta") %>%
  filter(entidad == 30, month == 5) %>%
  mutate(uniqueid = entidad * 1000 + municipio) %>%
  filter(seccion != 0) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2017 <- df_2017 %>%
  left_join(ln_2017, by = c("uniqueid", "section"))

# Municipal aggregates
df_2017 <- df_2017 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup() %>%
  mutate(turnout = total / listanominal)

df_2017 <- df_2017 %>%
  mutate(year = 2017,
         month = "June",
         STATE = "VERACRUZ")

cat("2017 processed:", nrow(df_2017), "rows\n")

################################################################################
## 2018 Extraordinary (SALVADOR)
## Municipalities: CAMARON DE TEJEDA, EMILIANO ZAPATA, SAYULA DE ALEMAN
################################################################################

df_2018 <- read_excel("VERACRUZ_EXTRAORDINARIAMPAL_2018.xlsx", 
                       sheet = "VERACRUZ_EXTRAORDINARIAMPAL_201",
                       range = "A7:AM138")

df_2018 <- df_2018 %>%
  rename(section = SECCION,
         municipality = MUNICIPIO,
         listanominal = LISTA_NOMINAL,
         total = TOTAL)

# Create coalitions
df_2018 <- df_2018 %>%
  mutate(PAN_PRD = PAN + PRD + PANPRD,
         PRI_PVEM = PRI + PVEM + PRIPVEM)

# Remove original party columns absorbed into coalitions
df_2018 <- df_2018 %>%
  select(-PAN, -PRD, -PANPRD, -PRI, -PVEM, -PRIPVEM)

# MORENA_PT_PES coalition for EMILIANO ZAPATA
df_2018 <- df_2018 %>%
  mutate(MORENA_PT_PES = ifelse(municipality == "EMILIANO ZAPATA",
                                MORENA + PES + PT + PTMORENAPES + PTPES + PTMORENA + MORENAPES, NA))

# Remove absorbed columns
df_2018 <- df_2018 %>%
  select(-PT, -MORENA, -PTMORENAPES, -PTPES, -PTMORENA, -MORENAPES)

# Set PES to NA for EMILIANO ZAPATA
df_2018 <- df_2018 %>%
  mutate(PES = ifelse(municipality == "EMILIANO ZAPATA", NA, PES))

# Collapse
df_2018 <- df_2018 %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN_PRD, PRI_PVEM, MORENA_PT_PES, PANAL, PES, listanominal, total), 
                   ~sum(., na.rm = TRUE)), .groups = "drop")

# Assign uniqueid
df_2018 <- df_2018 %>%
  mutate(uniqueid = case_when(
    municipality == "CAMARON DE TEJEDA" ~ 30007,
    municipality == "EMILIANO ZAPATA" ~ 30065,
    municipality == "SAYULA DE ALEMAN" ~ 30144,
    TRUE ~ NA_real_
  ))

# Mark as extraordinary
df_2018 <- df_2018 %>%
  mutate(municipality = paste0(municipality, " EXTRAORDINARIO"))

df_2018 <- df_2018 %>%
  mutate(turnout = total / listanominal)

# Calculate valid
party_cols_2018 <- c("PAN_PRD", "PRI_PVEM", "MORENA_PT_PES", "PANAL", "PES")
df_2018 <- df_2018 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2018)), na.rm = TRUE))

# Municipal aggregates
df_2018 <- df_2018 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2018 <- df_2018 %>%
  mutate(year = 2018,
         month = "March")

cat("2018 Extraordinary processed:", nrow(df_2018), "rows\n")

################################################################################
## Append All Years
################################################################################

all_years <- list(df_2004, df_2005, df_2007, df_2010, df_2013, df_2017, df_2018)

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
veracruz_all <- bind_rows(all_years)

################################################################################
## Final Corrections
################################################################################

veracruz_all <- veracruz_all %>%
  mutate(municipality = toupper(remove_accents(municipality)))

# Final uniqueid assignment for any missing
veracruz_all <- veracruz_all %>%
  mutate(uniqueid = ifelse(is.na(uniqueid), assign_veracruz_uniqueid(municipality), uniqueid))

################################################################################
## Save Output
################################################################################

# Reorder columns
key_cols <- c("municipality", "uniqueid", "section", "year", "month")
other_cols <- setdiff(names(veracruz_all), key_cols)
veracruz_all <- veracruz_all %>%
  select(all_of(key_cols), all_of(other_cols))

# Save
data.table::fwrite(veracruz_all, "../../../Processed Data/veracruz/veracruz_process_raw_data.csv")

cat("\n=== VERACRUZ PROCESSING COMPLETE ===\n")
cat("Total observations:", nrow(veracruz_all), "\n")
cat("Years:", paste(sort(unique(veracruz_all$year)), collapse = ", "), "\n")
cat("Municipalities:", length(unique(veracruz_all$uniqueid)), "\n")
