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

# Read the CSV file
data <- fread("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/Ayu_Seccion_1998_No_LN.csv",
              encoding = "Latin-1")
colnames(data) <- tolower(colnames(data))
names(data) <- gsub("[- ]", "", names(data))
# Rename columns 
data <- data %>%
  rename(
    municipality = municipio,
    section = "sección"
  )

# Drop rows where both `municipality` is empty and `section` is NA
data <- data %>%
  filter(!(municipality == "" & is.na(section)))

# Drop rows where `total` is NA or 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Convert specific columns from character to numeric
data <- data %>%
  mutate(across(pan:total, as.numeric))

# Collapse (sum) data by `municipality` and `section`
data <- data %>%
  group_by(municipality, section) %>%
  summarize(across(pan:total, sum, na.rm = TRUE))

# Rename variables to uppercase party names
data <- data %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem,
    PCP = pcp)

# Drop columns `noreg` and `nulos` if they exist
data <- data %>%
  select(-noregistrados, -nulos)

# Initialize 'uniqueid' variable with 0
data <- data %>%
  mutate(uniqueid = 0)

# Use a sequence of conditional replacements to assign unique IDs based on 'municipality'
data <- data %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACAJETE" ~ 21001,
      municipality == "ACATENO" ~ 21002,
      municipality == "ACATLAN" ~ 21003,
      municipality == "ACATZINGO" ~ 21004,
      municipality == "ACTEOPAN" ~ 21005,
      municipality == "AHUACATLAN" ~ 21006,
      municipality == "AHUATLAN" ~ 21007,
      municipality == "AHUAZOTEPEC" ~ 21008,
      municipality == "AHUEHUETITLA" ~ 21009,
      municipality == "AJALPAN" ~ 21010,
      municipality == "ALBINO ZERTUCHE" ~ 21011,
      municipality == "ALJOJUCA" ~ 21012,
      municipality == "ALTEPEXI" ~ 21013,
      municipality == "AMIXTLAN" ~ 21014,
      municipality == "AMOZOC" ~ 21015,
      municipality == "AQUIXTLA" ~ 21016,
      municipality == "ATEMPAN" ~ 21017,
      municipality == "ATEXCAL" ~ 21018,
      municipality == "ATLEQUIZAYAN" ~ 21080,
      municipality == "ATLIXCO" ~ 21019,
      municipality == "ATOYATEMPAN" ~ 21020,
      municipality == "ATZALA" ~ 21021,
      municipality == "ATZITZIHUACAN" ~ 21022,
      municipality == "ATZITZINTLA" ~ 21023,
      municipality == "AXUTLA" ~ 21024,
      municipality == "AYOTOXCO DE GUERRERO" ~ 21025,
      municipality == "CALPAN" ~ 21026,
      municipality == "CALTEPEC" ~ 21027,
      municipality == "CAMOCUAUTLA" ~ 21028,
      municipality == "CAXHUACAN" ~ 21029,
      municipality == "CHALCHICOMULA DE SESMA" ~ 21045,
      municipality == "CHAPULCO" ~ 21046,
      municipality == "CHIAUTLA" ~ 21047,
      municipality == "CHIAUTZINGO" ~ 21048,
      municipality == "CHICHIQUILA" ~ 21050,
      municipality == "CHICONCUAUTLA" ~ 21049,
      municipality == "CHIETLA" ~ 21051,
      municipality == "CHIGMECATITLAN" ~ 21052,
      municipality == "CHIGNAHUAPAN" ~ 21053,
      municipality == "CHIGNAUTLA" ~ 21054,
      municipality == "CHILA" ~ 21055,
      municipality == "CHILA DE LA SAL" ~ 21056,
      municipality == "CHILCHOTLA" ~ 21058,
      municipality == "CHINANTLA" ~ 21059,
      municipality == "COATEPEC" ~ 21030,
      municipality == "COATZINGO" ~ 21031,
      municipality == "COHETZALA" ~ 21032,
      municipality == "COHUECAN" ~ 21033,
      municipality == "CORONANGO" ~ 21034,
      municipality == "COXCATLAN" ~ 21035,
      municipality == "COYOMEAPAN" ~ 21036,
      municipality == "COYOTEPEC" ~ 21037,
      municipality == "CUAPIAXTLA DE MADERO" ~ 21038,
      municipality == "CUAUTEMPAN" ~ 21039,
      municipality == "CUAUTINCHAN" ~ 21040,
      municipality == "CUAUTLANCINGO" ~ 21041,
      municipality == "CUAYUCA DE ANDRADE" ~ 21042,
      municipality == "CUETZALAN DEL PROGRESO" ~ 21043,
      municipality == "CUYOACO" ~ 21044,
      municipality == "DOMINGO ARENAS" ~ 21060,
      municipality == "ELOXOCHITLAN" ~ 21061,
      municipality == "EPATLAN" ~ 21062,
      municipality == "ESPERANZA" ~ 21063,
      municipality == "FRANCISCO Z. MENA" ~ 21064,
      municipality == "GENERAL FELIPE ANGELES" ~ 21065,
      municipality == "GUADALUPE" ~ 21066,
      municipality == "GUADALUPE VICTORIA" ~ 21067,
      municipality == "HERMENEGILDO GALEANA" ~ 21068,
      municipality == "HONEY" ~ 21057,
      municipality == "HUAQUECHULA" ~ 21069,
      municipality == "HUATLATLAUCA" ~ 21070,
      municipality == "HUAUCHINANGO" ~ 21071,
      municipality == "HUEHUETLA" ~ 21072,
      municipality == "HUEHUETLAN EL CHICO" ~ 21073,
      municipality == "HUEHUETLAN EL GRANDE" ~ 21150,
      municipality == "HUEJOTZINGO" ~ 21074,
      municipality == "HUEYAPAN" ~ 21075,
      municipality == "HUEYTAMALCO" ~ 21076,
      municipality == "HUEYTLALPAN" ~ 21077,
      municipality == "HUITZILAN DE SERDAN" ~ 21078,
      municipality == "HUITZILTEPEC" ~ 21079,
      municipality == "IXCAMILPA DE GUERRERO" ~ 21081,
      municipality == "IXCAQUIXTLA" ~ 21082,
      municipality == "IXTACAMAXTITLAN" ~ 21083,
      municipality == "IXTEPEC" ~ 21084,
      municipality == "IZUCAR DE MATAMOROS" ~ 21085,
      municipality == "JALPAN" ~ 21086,
      municipality == "JOLALPAN" ~ 21087,
      municipality == "JONOTLA" ~ 21088,
      municipality == "JOPALA" ~ 21089,
      municipality == "JUAN C. BONILLA" ~ 21090,
      municipality == "JUAN GALINDO" ~ 21091,
      municipality == "JUAN N. MENDEZ" ~ 21092,
      municipality == "MAGDALENA TLATLAUQUITEPEC" ~ 21095,
      municipality == "LAFRAGUA" ~ 21093,
      municipality == "LIBRES" ~ 21094,
      municipality == "LOS REYES DE JUAREZ" ~ 21118,
      municipality == "MAZAPILTEPEC DE JUAREZ" ~ 21096,
      municipality == "MIXTLA" ~ 21097,
      municipality == "MOLCAXAC" ~ 21098,
      municipality == "NAUPAN" ~ 21100,
      municipality == "NAUZONTLA" ~ 21101,
      municipality == "NEALTICAN" ~ 21102,
      municipality == "NICOLAS BRAVO" ~ 21103,
      municipality == "NOPALUCAN" ~ 21104,
      municipality == "OCOTEPEC" ~ 21105,
      municipality == "OCOYUCAN" ~ 21106,
      municipality == "OLINTLA" ~ 21107,
      municipality == "ORIENTAL" ~ 21108,
      municipality == "PAHUATLAN" ~ 21109,
      municipality == "PALMAR DE BRAVO" ~ 21110,
      municipality == "PANTEPEC" ~ 21111,
      municipality == "PETLALCINGO" ~ 21112,
      municipality == "PIAXTLA" ~ 21113,
      municipality == "PUEBLA" ~ 21114,
      municipality == "QUECHOLAC" ~ 21115,
      municipality == "QUIMIXTLAN" ~ 21116,
      municipality == "RAFAEL LARA GRAJALES" ~ 21117,
      municipality == "SAN ANDRES CHOLULA" ~ 21119,
      municipality == "SAN ANTONIO CAxD1ADA" ~ 21120 | str_detect(municipality, "SAN ANTONIO CA"),
      municipality == "SAN DIEGO LA MESA TOCHIMILTZINGO" ~ 21121,
      municipality == "SAN FELIPE TEOTLALCINGO" ~ 21122,
      municipality == "SAN FELIPE TEPATLAN" ~ 21123,
      municipality == "SAN GABRIEL CHILAC" ~ 21124,
      municipality == "SAN GREGORIO ATZOMPA" ~ 21125,
      municipality == "SAN JERONIMO TECUANIPAN" ~ 21126,
      municipality == "SAN JERONIMO XAYACATLAN" ~ 21127,
      municipality == "SAN JOSE CHIAPA" ~ 21128,
      municipality == "SAN JOSE MIAHUATLAN" ~ 21129,
      municipality == "SAN JUAN ATENCO" ~ 21130,
      municipality == "SAN JUAN ATZOMPA" ~ 21131,
      municipality == "SAN MARTIN TEXMELUCAN" ~ 21132,
      municipality == "SAN MARTIN TOTOLTEPEC" ~ 21133,
      municipality == "SAN MATIAS TLALANCALECA" ~ 21134,
      municipality == "SAN MIGUEL IXITLAN" ~ 21135,
      municipality == "SAN MIGUEL XOXTLA" ~ 21136,
      municipality == "SAN NICOLAS BUENOS AIRES" ~ 21137,
      municipality == "SAN NICOLAS DE LOS RANCHOS" ~ 21138,
      municipality == "SAN PABLO ANICANO" ~ 21139,
      municipality == "SAN PEDRO CHOLULA" ~ 21140,
      municipality == "SAN PEDRO YELOIXTLAHUACA" ~ 21141,
      municipality == "SAN SALVADOR EL SECO" ~ 21142,
      municipality == "SAN SALVADOR EL VERDE" ~ 21143,
      municipality == "SAN SALVADOR HUIXCOLOTLA" ~ 21144,
      municipality == "SAN SEBASTIAN TLACOTEPEC" ~ 21145,
      municipality == "SANTA CATARINA TLALTEMPAN" ~ 21146,
      municipality == "SANTA INES AHUATEMPAN" ~ 21147,
      municipality == "SANTA ISABEL CHOLULA" ~ 21148,
      municipality == "SANTIAGO MIAHUATLAN" ~ 21149,
      municipality == "SANTO TOMAS HUEYOTLIPAN" ~ 21151,
      municipality == "SOLTEPEC" ~ 21152,
      municipality == "TECALI DE HERRERA" ~ 21153,
      municipality == "TECAMACHALCO" ~ 21154,
      municipality == "TECOMATLAN" ~ 21155,
      municipality == "TEHUACAN" ~ 21156,
      municipality == "TEHUITZINGO" ~ 21157,
      municipality == "TENAMPULCO" ~ 21158,
      municipality == "TEOPANTLAN" ~ 21159,
      municipality == "TEOTLALCO" ~ 21160,
      municipality == "TEPANCO DE LOPEZ" ~ 21161,
      municipality == "TEPANGO DE RODRIGUEZ" ~ 21162,
      municipality == "TEPATLAXCO DE HIDALGO" ~ 21163,
      municipality == "TEPEACA" ~ 21164,
      municipality == "TEPEMAXALCO" ~ 21165,
      municipality == "TEPEOJUMA" ~ 21166,
      municipality == "TEPETZINTLA" ~ 21167,
      municipality == "TEPEXCO" ~ 21168,
      municipality == "TEPEXI DE RODRIGUEZ" ~ 21169,
      municipality == "TEPEYAHUALCO" ~ 21170,
      municipality == "TEPEYAHUALCO DE CUAUHTEMOC" ~ 21171,
      municipality == "TETELA DE OCAMPO" ~ 21172,
      municipality == "TETELES DE AVILA CASTILLO" ~ 21173,
      municipality == "TEZIUTLAN" ~ 21174,
      municipality == "TIANGUISMANALCO" ~ 21175,
      municipality == "TILAPA" ~ 21176,
      municipality == "TLACHICHUCA" ~ 21179,
      municipality == "TLACOTEPEC DE BENITO JUAREZ" ~ 21177,
      municipality == "TLACUILOTEPEC" ~ 21178,
      municipality == "TLAHUAPAN" ~ 21180,
      municipality == "TLALTENANGO" ~ 21181,
      municipality == "TLANEPANTLA" ~ 21182,
      municipality == "TLAOLA" ~ 21183,
      municipality == "TLAPACOYA" ~ 21184,
      municipality == "TLAPANALA" ~ 21185,
      municipality == "TLATLAUQUITEPEC" ~ 21186,
      municipality == "TLAXCO" ~ 21187,
      municipality == "TOCHIMILCO" ~ 21188,
      municipality == "TOCHTEPEC" ~ 21189,
      municipality == "TOTOLTEPEC DE GUERRERO" ~ 21190,
      municipality == "TULCINGO DE VALLE" ~ 21191,
      municipality == "TUZAMAPAN DE GALEANA" ~ 21192,
      municipality == "TZICATLACOYAN" ~ 21193,
      municipality == "VENUSTIANO CARRANZA" ~ 21194,
      municipality == "VICENTE GUERRERO" ~ 21195,
      municipality == "XAYACATLAN DE BRAVO" ~ 21196,
      municipality == "XICOTEPEC" ~ 21197,
      municipality == "XICOTLAN" ~ 21198,
      municipality == "XIUTETELCO" ~ 21199,
      municipality == "XOCHIAPULCO" ~ 21200,
      municipality == "XOCHILTEPEC" ~ 21201,
      municipality == "XOCHITLAN DE VICENTE SUAREZ" ~ 21202,
      municipality == "XOCHITLAN TODOS SANTOS" ~ 21203,
      municipality == "YAONAHUAC" ~ 21204,
      municipality == "YEHUALTEPEC" ~ 21205,
      municipality == "ZACAPALA" ~ 21206,
      municipality == "ZACAPOAXTLA" ~ 21207,
      municipality == "ZACATLAN" ~ 21208,
      municipality == "ZAPOTITLAN SALINAS" ~ 21209,
      municipality == "ZAPOTITLAN DE MENDEZ" ~ 21210,
      municipality == "ZARAGOZA" ~ 21211,
      municipality == "ZAUTLA" ~ 21212,
      municipality == "ZIHUATEUTLA" ~ 21213,
      municipality == "ZINACATEPEC" ~ 21214,
      municipality == "ZONGOZOTLA" ~ 21215,
      municipality == "ZOQUIAPAN" ~ 21216,
      municipality == "ZOQUITLAN" ~ 21217,
      TRUE ~ uniqueid # Retain existing uniqueid if no match
    )
  )

# Create 'valid' as the row total of specified columns (sum of PAN, PRI, PRD, PT, PVEM, PCP)
data <- data %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PCP)), na.rm = TRUE))

# Generate municipal-level sums and their inverses
for (var in c("PAN", "PRI", "PRD", "PT", "PVEM", "PCP", "total", "valid")) {
  data <- data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(get(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / get(paste0("mun_", var)))
}

# Add new columns for `ed` and `seccion`
data <- data %>%
  mutate(ed = 21, seccion = section)

# Merge the data with an external dataset (all_months_years.dta) by 'ed' and 'seccion'
external_data <- read_dta("../../all_months_years.dta") %>%
  filter(month == 9 & year == 1998)

# Merge by 'ed' and 'seccion', keeping 'month', 'year', and 'lista' from the external data
merged_data <- data %>%
  left_join(external_data %>% select(ed, seccion, month, year, lista), by = c("ed", "seccion"))

# Drop unnecessary columns after merging
filtered_data <- filtered_data %>%
  select(-ed, -seccion, -year, -month)

# Rename 'lista' to 'listanominal'
filtered_data <- filtered_data %>%
  rename(listanominal = lista)

# Calculate turnout
filtered_data <- filtered_data %>%
  mutate(turnout = total / listanominal)

# Add year and month columns
df_1998 <- filtered_data %>%
  mutate(year = 1998, month = "November")
rm(filtered_data)

# Read the CSV file
data <- read_csv("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/Ayu_Seccion_2001.csv")
colnames(data) <- tolower(colnames(data))
# Rename columns
data <- data %>%
  rename(
    municipality = municipio,
    section = seccion,
    total = votaciontotal
  )

# Drop rows where both 'municipality' is empty and 'section' is missing (NA)
data <- data %>%
  filter(!(municipality == "" & is.na(section)))

# Drop rows where 'total' is NA or equal to 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Convert columns from character to numeric 
data <- data %>%
  mutate(across(pan:listanominal, as.numeric))

# Create a 'missing' flag for rows where 'listanominal' is missing
data <- data %>%
  mutate(missing = ifelse(is.na(listanominal), 1, 0))

# Collapse the data by municipality and section, summing all numeric columns including 'missing'
data <- data %>%
  group_by(municipality, section) %>%
  summarize(across(c(pan:listanominal, missing), sum, na.rm = TRUE))

# Add new columns for 'ed' and 'seccion'
data <- data %>%
  mutate(ed = 21, seccion = section)

# Merge the dataset with external data (all_months_years.dta) by 'ed' and 'seccion'
external_data <- read_dta("../../all_months_years.dta")%>%
  filter(month == 9 & year == 2001)

# Merge, keeping 'month', 'year', and 'lista' from the external data
data <- data %>%
  left_join(external_data %>% select(ed, seccion, month, year, lista), by = c("ed", "seccion"))

# Drop unwanted columns after the merge
data <- data %>%
  select(-_merge, -ed, -seccion, -year, -month)

# Replace 'listanominal' with 'lista' if 'missing' flag is 1 or greater
data <- data %>%
  mutate(listanominal = ifelse(missing >= 1, lista, listanominal))

# Drop 'lista' and 'missing' columns
data <- data %>%
  select(-lista, -missing)

# Rename columns for political parties
data <- data %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem,
    PC = cdppn,
    PSN = psn,
    PAS = pas
  )

# Calculate 'turnout' as the ratio of 'total' to 'listanominal'
data <- data %>%
  mutate(turnout = total / listanominal)

# Drop unwanted columns 'noreg' and 'nulos' if they exist
data <- data %>%
  select(-noreg, -nulos, everything())

# Use a sequence of conditional replacements to assign unique IDs based on 'municipality'
data <- data %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACAJETE" ~ 21001,
      municipality == "ACATENO" ~ 21002,
      municipality == "ACATLAN" ~ 21003,
      municipality == "ACATZINGO" ~ 21004,
      municipality == "ACTEOPAN" ~ 21005,
      municipality == "AHUACATLAN" ~ 21006,
      municipality == "AHUATLAN" ~ 21007,
      municipality == "AHUAZOTEPEC" ~ 21008,
      municipality == "AHUEHUETITLA" ~ 21009,
      municipality == "AJALPAN" ~ 21010,
      municipality == "ALBINO ZERTUCHE" ~ 21011,
      municipality == "ALJOJUCA" ~ 21012,
      municipality == "ALTEPEXI" ~ 21013,
      municipality == "AMIXTLAN" ~ 21014,
      municipality == "AMOZOC" ~ 21015,
      municipality == "AQUIXTLA" ~ 21016,
      municipality == "ATEMPAN" ~ 21017,
      municipality == "ATEXCAL" ~ 21018,
      municipality == "ATLEQUIZAYAN" ~ 21080,
      municipality == "ATLIXCO" ~ 21019,
      municipality == "ATOYATEMPAN" ~ 21020,
      municipality == "ATZALA" ~ 21021,
      municipality == "ATZITZIHUACAN" ~ 21022,
      municipality == "ATZITZINTLA" ~ 21023,
      municipality == "AXUTLA" ~ 21024,
      municipality == "AYOTOXCO DE GUERRERO" ~ 21025,
      municipality == "CALPAN" ~ 21026,
      municipality == "CALTEPEC" ~ 21027,
      municipality == "CAMOCUAUTLA" ~ 21028,
      municipality == "CAXHUACAN" ~ 21029,
      municipality == "CHALCHICOMULA DE SESMA" ~ 21045,
      municipality == "CHAPULCO" ~ 21046,
      municipality == "CHIAUTLA" ~ 21047,
      municipality == "CHIAUTZINGO" ~ 21048,
      municipality == "CHICHIQUILA" ~ 21050,
      municipality == "CHICONCUAUTLA" ~ 21049,
      municipality == "CHIETLA" ~ 21051,
      municipality == "CHIGMECATITLAN" ~ 21052,
      municipality == "CHIGNAHUAPAN" ~ 21053,
      municipality == "CHIGNAUTLA" ~ 21054,
      municipality == "CHILA" ~ 21055,
      municipality == "CHILA DE LA SAL" ~ 21056,
      municipality == "CHILCHOTLA" ~ 21058,
      municipality == "CHINANTLA" ~ 21059,
      municipality == "COATEPEC" ~ 21030,
      municipality == "COATZINGO" ~ 21031,
      municipality == "COHETZALA" ~ 21032,
      municipality == "COHUECAN" ~ 21033,
      municipality == "CORONANGO" ~ 21034,
      municipality == "COXCATLAN" ~ 21035,
      municipality == "COYOMEAPAN" ~ 21036,
      municipality == "COYOTEPEC" ~ 21037,
      municipality == "CUAPIAXTLA DE MADERO" ~ 21038,
      municipality == "CUAUTEMPAN" ~ 21039,
      municipality == "CUAUTINCHAN" ~ 21040,
      municipality == "CUAUTLANCINGO" ~ 21041,
      municipality == "CUAYUCA DE ANDRADE" ~ 21042,
      municipality == "CUETZALAN DEL PROGRESO" ~ 21043,
      municipality == "CUYOACO" ~ 21044,
      municipality == "DOMINGO ARENAS" ~ 21060,
      municipality == "ELOXOCHITLAN" ~ 21061,
      municipality == "EPATLAN" ~ 21062,
      municipality == "ESPERANZA" ~ 21063,
      municipality == "FRANCISCO Z. MENA" ~ 21064,
      municipality == "GENERAL FELIPE ANGELES" ~ 21065,
      municipality == "GUADALUPE" ~ 21066,
      municipality == "GUADALUPE VICTORIA" ~ 21067,
      municipality == "HERMENEGILDO GALEANA" ~ 21068,
      municipality == "HONEY" ~ 21057,
      municipality == "HUAQUECHULA" ~ 21069,
      municipality == "HUATLATLAUCA" ~ 21070,
      municipality == "HUAUCHINANGO" ~ 21071,
      municipality == "HUEHUETLA" ~ 21072,
      municipality == "HUEHUETLAN EL CHICO" ~ 21073,
      municipality == "HUEHUETLAN EL GRANDE" ~ 21150,
      municipality == "HUEJOTZINGO" ~ 21074,
      municipality == "HUEYAPAN" ~ 21075,
      municipality == "HUEYTAMALCO" ~ 21076,
      municipality == "HUEYTLALPAN" ~ 21077,
      municipality == "HUITZILAN DE SERDAN" ~ 21078,
      municipality == "HUITZILTEPEC" ~ 21079,
      municipality == "IXCAMILPA DE GUERRERO" ~ 21081,
      municipality == "IXCAQUIXTLA" ~ 21082,
      municipality == "IXTACAMAXTITLAN" ~ 21083,
      municipality == "IXTEPEC" ~ 21084,
      municipality == "IZUCAR DE MATAMOROS" ~ 21085,
      municipality == "JALPAN" ~ 21086,
      municipality == "JOLALPAN" ~ 21087,
      municipality == "JONOTLA" ~ 21088,
      municipality == "JOPALA" ~ 21089,
      municipality == "JUAN C. BONILLA" ~ 21090,
      municipality == "JUAN GALINDO" ~ 21091,
      municipality == "JUAN N. MENDEZ" ~ 21092,
      municipality == "MAGDALENA TLATLAUQUITEPEC" ~ 21095,
      municipality == "LAFRAGUA" ~ 21093,
      municipality == "LIBRES" ~ 21094,
      municipality == "LOS REYES DE JUAREZ" ~ 21118,
      municipality == "MAZAPILTEPEC DE JUAREZ" ~ 21096,
      municipality == "MIXTLA" ~ 21097,
      municipality == "MOLCAXAC" ~ 21098,
      municipality == "NAUPAN" ~ 21100,
      municipality == "NAUZONTLA" ~ 21101,
      municipality == "NEALTICAN" ~ 21102,
      municipality == "NICOLAS BRAVO" ~ 21103,
      municipality == "NOPALUCAN" ~ 21104,
      municipality == "OCOTEPEC" ~ 21105,
      municipality == "OCOYUCAN" ~ 21106,
      municipality == "OLINTLA" ~ 21107,
      municipality == "ORIENTAL" ~ 21108,
      municipality == "PAHUATLAN" ~ 21109,
      municipality == "PALMAR DE BRAVO" ~ 21110,
      municipality == "PANTEPEC" ~ 21111,
      municipality == "PETLALCINGO" ~ 21112,
      municipality == "PIAXTLA" ~ 21113,
      municipality == "PUEBLA" ~ 21114,
      municipality == "QUECHOLAC" ~ 21115,
      municipality == "QUIMIXTLAN" ~ 21116,
      municipality == "RAFAEL LARA GRAJALES" ~ 21117,
      municipality == "SAN ANDRES CHOLULA" ~ 21119,
      municipality == "SAN ANTONIO CAxD1ADA" ~ 21120 | str_detect(municipality, "SAN ANTONIO CA"),
      municipality == "SAN DIEGO LA MESA TOCHIMILTZINGO" ~ 21121,
      municipality == "SAN FELIPE TEOTLALCINGO" ~ 21122,
      municipality == "SAN FELIPE TEPATLAN" ~ 21123,
      municipality == "SAN GABRIEL CHILAC" ~ 21124,
      municipality == "SAN GREGORIO ATZOMPA" ~ 21125,
      municipality == "SAN JERONIMO TECUANIPAN" ~ 21126,
      municipality == "SAN JERONIMO XAYACATLAN" ~ 21127,
      municipality == "SAN JOSE CHIAPA" ~ 21128,
      municipality == "SAN JOSE MIAHUATLAN" ~ 21129,
      municipality == "SAN JUAN ATENCO" ~ 21130,
      municipality == "SAN JUAN ATZOMPA" ~ 21131,
      municipality == "SAN MARTIN TEXMELUCAN" ~ 21132,
      municipality == "SAN MARTIN TOTOLTEPEC" ~ 21133,
      municipality == "SAN MATIAS TLALANCALECA" ~ 21134,
      municipality == "SAN MIGUEL IXITLAN" ~ 21135,
      municipality == "SAN MIGUEL XOXTLA" ~ 21136,
      municipality == "SAN NICOLAS BUENOS AIRES" ~ 21137,
      municipality == "SAN NICOLAS DE LOS RANCHOS" ~ 21138,
      municipality == "SAN PABLO ANICANO" ~ 21139,
      municipality == "SAN PEDRO CHOLULA" ~ 21140,
      municipality == "SAN PEDRO YELOIXTLAHUACA" ~ 21141,
      municipality == "SAN SALVADOR EL SECO" ~ 21142,
      municipality == "SAN SALVADOR EL VERDE" ~ 21143,
      municipality == "SAN SALVADOR HUIXCOLOTLA" ~ 21144,
      municipality == "SAN SEBASTIAN TLACOTEPEC" ~ 21145,
      municipality == "SANTA CATARINA TLALTEMPAN" ~ 21146,
      municipality == "SANTA INES AHUATEMPAN" ~ 21147,
      municipality == "SANTA ISABEL CHOLULA" ~ 21148,
      municipality == "SANTIAGO MIAHUATLAN" ~ 21149,
      municipality == "SANTO TOMAS HUEYOTLIPAN" ~ 21151,
      municipality == "SOLTEPEC" ~ 21152,
      municipality == "TECALI DE HERRERA" ~ 21153,
      municipality == "TECAMACHALCO" ~ 21154,
      municipality == "TECOMATLAN" ~ 21155,
      municipality == "TEHUACAN" ~ 21156,
      municipality == "TEHUITZINGO" ~ 21157,
      municipality == "TENAMPULCO" ~ 21158,
      municipality == "TEOPANTLAN" ~ 21159,
      municipality == "TEOTLALCO" ~ 21160,
      municipality == "TEPANCO DE LOPEZ" ~ 21161,
      municipality == "TEPANGO DE RODRIGUEZ" ~ 21162,
      municipality == "TEPATLAXCO DE HIDALGO" ~ 21163,
      municipality == "TEPEACA" ~ 21164,
      municipality == "TEPEMAXALCO" ~ 21165,
      municipality == "TEPEOJUMA" ~ 21166,
      municipality == "TEPETZINTLA" ~ 21167,
      municipality == "TEPEXCO" ~ 21168,
      municipality == "TEPEXI DE RODRIGUEZ" ~ 21169,
      municipality == "TEPEYAHUALCO" ~ 21170,
      municipality == "TEPEYAHUALCO DE CUAUHTEMOC" ~ 21171,
      municipality == "TETELA DE OCAMPO" ~ 21172,
      municipality == "TETELES DE AVILA CASTILLO" ~ 21173,
      municipality == "TEZIUTLAN" ~ 21174,
      municipality == "TIANGUISMANALCO" ~ 21175,
      municipality == "TILAPA" ~ 21176,
      municipality == "TLACHICHUCA" ~ 21179,
      municipality == "TLACOTEPEC DE BENITO JUAREZ" ~ 21177,
      municipality == "TLACUILOTEPEC" ~ 21178,
      municipality == "TLAHUAPAN" ~ 21180,
      municipality == "TLALTENANGO" ~ 21181,
      municipality == "TLANEPANTLA" ~ 21182,
      municipality == "TLAOLA" ~ 21183,
      municipality == "TLAPACOYA" ~ 21184,
      municipality == "TLAPANALA" ~ 21185,
      municipality == "TLATLAUQUITEPEC" ~ 21186,
      municipality == "TLAXCO" ~ 21187,
      municipality == "TOCHIMILCO" ~ 21188,
      municipality == "TOCHTEPEC" ~ 21189,
      municipality == "TOTOLTEPEC DE GUERRERO" ~ 21190,
      municipality == "TULCINGO DE VALLE" ~ 21191,
      municipality == "TUZAMAPAN DE GALEANA" ~ 21192,
      municipality == "TZICATLACOYAN" ~ 21193,
      municipality == "VENUSTIANO CARRANZA" ~ 21194,
      municipality == "VICENTE GUERRERO" ~ 21195,
      municipality == "XAYACATLAN DE BRAVO" ~ 21196,
      municipality == "XICOTEPEC" ~ 21197,
      municipality == "XICOTLAN" ~ 21198,
      municipality == "XIUTETELCO" ~ 21199,
      municipality == "XOCHIAPULCO" ~ 21200,
      municipality == "XOCHILTEPEC" ~ 21201,
      municipality == "XOCHITLAN DE VICENTE SUAREZ" ~ 21202,
      municipality == "XOCHITLAN TODOS SANTOS" ~ 21203,
      municipality == "YAONAHUAC" ~ 21204,
      municipality == "YEHUALTEPEC" ~ 21205,
      municipality == "ZACAPALA" ~ 21206,
      municipality == "ZACAPOAXTLA" ~ 21207,
      municipality == "ZACATLAN" ~ 21208,
      municipality == "ZAPOTITLAN SALINAS" ~ 21209,
      municipality == "ZAPOTITLAN DE MENDEZ" ~ 21210,
      municipality == "ZARAGOZA" ~ 21211,
      municipality == "ZAUTLA" ~ 21212,
      municipality == "ZIHUATEUTLA" ~ 21213,
      municipality == "ZINACATEPEC" ~ 21214,
      municipality == "ZONGOZOTLA" ~ 21215,
      municipality == "ZOQUIAPAN" ~ 21216,
      municipality == "ZOQUITLAN" ~ 21217,
      TRUE ~ uniqueid # Retain existing uniqueid if no match
    )
  )

# Create 'valid' as the row total of specified columns (sum of PAN, PRI, PRD, PT, PVEM, PC, PSN, PAS)
data <- data %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PC, PSN, PAS)), na.rm = TRUE))

# Add year and month columns
data_2001 <- data %>%
  mutate(year = 2001, month = "November")

# Import the Excel sheet and specify the range of cells to read
data <- read_excel("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/Resultados por casilla elección 2002 Molcaxac.xlsx", sheet = "aytos", range = "A8:V16", col_names = TRUE)

colnames(data) <- tolower(colnames(data))

gen_excel_col_names <- function(n) {
  # Generates Excel-like column names from A..Z, AA..ZZ, etc.
  # for n columns.
  out <- character(n)
  for (i in seq_len(n)) {
    num  <- i
    name <- ""
    while (num > 0) {
      r     <- (num - 1) %% 26
      name  <- paste0(LETTERS[r + 1], name)
      num   <- (num - r - 1) %/% 26
    }
    out[i] <- name
  }
  out
}

# Example:
n_cols  <- ncol(data)        # how many columns do you have?
new_names <- gen_excel_col_names(n_cols)
names(data) <- new_names

# Rename columns
data <- data %>%
  rename(
    "Dtto." = `A`,
    "municipality" = `B`,
    section = `C`,
    casilla = `D`,
    PAN = `F`,
    PRI = `G`,
    PRD = `H`,
    PC = `K`,
    PAS = `M`,
    listanominal = `O`,
    total = `S`
  )

# Drop rows where `section` is missing (NA)
data <- data %>%
  filter(!is.na(section))

# Generate new variables for municipality and uniqueid
data <- data %>%
  mutate(
    municipality = "MOLCAXAZ EXTRAORDINARIO",
    uniqueid = 21098
  )

# Collapse data by municipality, uniqueid, and section
collapsed_data <- data %>%
  group_by(municipality, uniqueid, section) %>%
  summarize(across(c(PAN, PRI, PRD, PC, PAS, total, listanominal), sum, na.rm = TRUE))

# Calculate turnout
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal)

# Create 'valid' as the row total of specified columns (sum of PAN, PRI, PRD, PC, PAS)
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PC, PAS)), na.rm = TRUE))

# Add year and month columns
data_2002 <- collapsed_data %>%
  mutate(year = 2002, month = "June")

# Read the CSV file
data <- read_csv("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/Ayu_Seccion_2004.csv")
colnames(data) <- tolower(colnames(data))
# Rename columns
data <- data %>%
  rename(
    municipality = municipio,
    section = seccion,
    total = votaciontotal
  )

# Drop rows where both 'municipality' is empty and 'section' is missing (NA)
data <- data %>%
  filter(!(municipality == "" & is.na(section)))

# Drop rows where 'total' is missing or 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Convert specified columns from character to numeric
data <- data %>%
  mutate(across(pan:conv, as.numeric)) %>%
  mutate(across(c(total, listanominal), as.numeric))

# Collapse (sum) data by 'municipality' and 'section'
data <- data %>%
  group_by(municipality, section) %>%
  summarize(across(c(pan:conv, total, listanominal), sum, na.rm = TRUE))

# Rename variables for political parties
data <- data %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem,
    PC = conv  # 'conv' renamed to 'PC'
  )

# Calculate turnout as the ratio of 'total' to 'listanominal'
data <- data %>%
  mutate(turnout = total / listanominal)

# Use a sequence of conditional replacements to assign unique IDs based on 'municipality'
data <- data %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACAJETE" ~ 21001,
      municipality == "ACATENO" ~ 21002,
      municipality == "ACATLAN" ~ 21003,
      municipality == "ACATZINGO" ~ 21004,
      municipality == "ACTEOPAN" ~ 21005,
      municipality == "AHUACATLAN" ~ 21006,
      municipality == "AHUATLAN" ~ 21007,
      municipality == "AHUAZOTEPEC" ~ 21008,
      municipality == "AHUEHUETITLA" ~ 21009,
      municipality == "AJALPAN" ~ 21010,
      municipality == "ALBINO ZERTUCHE" ~ 21011,
      municipality == "ALJOJUCA" ~ 21012,
      municipality == "ALTEPEXI" ~ 21013,
      municipality == "AMIXTLAN" ~ 21014,
      municipality == "AMOZOC" ~ 21015,
      municipality == "AQUIXTLA" ~ 21016,
      municipality == "ATEMPAN" ~ 21017,
      municipality == "ATEXCAL" ~ 21018,
      municipality == "ATLEQUIZAYAN" ~ 21080,
      municipality == "ATLIXCO" ~ 21019,
      municipality == "ATOYATEMPAN" ~ 21020,
      municipality == "ATZALA" ~ 21021,
      municipality == "ATZITZIHUACAN" ~ 21022,
      municipality == "ATZITZINTLA" ~ 21023,
      municipality == "AXUTLA" ~ 21024,
      municipality == "AYOTOXCO DE GUERRERO" ~ 21025,
      municipality == "CALPAN" ~ 21026,
      municipality == "CALTEPEC" ~ 21027,
      municipality == "CAMOCUAUTLA" ~ 21028,
      municipality == "CAXHUACAN" ~ 21029,
      municipality == "CHALCHICOMULA DE SESMA" ~ 21045,
      municipality == "CHAPULCO" ~ 21046,
      municipality == "CHIAUTLA" ~ 21047,
      municipality == "CHIAUTZINGO" ~ 21048,
      municipality == "CHICHIQUILA" ~ 21050,
      municipality == "CHICONCUAUTLA" ~ 21049,
      municipality == "CHIETLA" ~ 21051,
      municipality == "CHIGMECATITLAN" ~ 21052,
      municipality == "CHIGNAHUAPAN" ~ 21053,
      municipality == "CHIGNAUTLA" ~ 21054,
      municipality == "CHILA" ~ 21055,
      municipality == "CHILA DE LA SAL" ~ 21056,
      municipality == "CHILCHOTLA" ~ 21058,
      municipality == "CHINANTLA" ~ 21059,
      municipality == "COATEPEC" ~ 21030,
      municipality == "COATZINGO" ~ 21031,
      municipality == "COHETZALA" ~ 21032,
      municipality == "COHUECAN" ~ 21033,
      municipality == "CORONANGO" ~ 21034,
      municipality == "COXCATLAN" ~ 21035,
      municipality == "COYOMEAPAN" ~ 21036,
      municipality == "COYOTEPEC" ~ 21037,
      municipality == "CUAPIAXTLA DE MADERO" ~ 21038,
      municipality == "CUAUTEMPAN" ~ 21039,
      municipality == "CUAUTINCHAN" ~ 21040,
      municipality == "CUAUTLANCINGO" ~ 21041,
      municipality == "CUAYUCA DE ANDRADE" ~ 21042,
      municipality == "CUETZALAN DEL PROGRESO" ~ 21043,
      municipality == "CUYOACO" ~ 21044,
      municipality == "DOMINGO ARENAS" ~ 21060,
      municipality == "ELOXOCHITLAN" ~ 21061,
      municipality == "EPATLAN" ~ 21062,
      municipality == "ESPERANZA" ~ 21063,
      municipality == "FRANCISCO Z. MENA" ~ 21064,
      municipality == "GENERAL FELIPE ANGELES" ~ 21065,
      municipality == "GUADALUPE" ~ 21066,
      municipality == "GUADALUPE VICTORIA" ~ 21067,
      municipality == "HERMENEGILDO GALEANA" ~ 21068,
      municipality == "HONEY" ~ 21057,
      municipality == "HUAQUECHULA" ~ 21069,
      municipality == "HUATLATLAUCA" ~ 21070,
      municipality == "HUAUCHINANGO" ~ 21071,
      municipality == "HUEHUETLA" ~ 21072,
      municipality == "HUEHUETLAN EL CHICO" ~ 21073,
      municipality == "HUEHUETLAN EL GRANDE" ~ 21150,
      municipality == "HUEJOTZINGO" ~ 21074,
      municipality == "HUEYAPAN" ~ 21075,
      municipality == "HUEYTAMALCO" ~ 21076,
      municipality == "HUEYTLALPAN" ~ 21077,
      municipality == "HUITZILAN DE SERDAN" ~ 21078,
      municipality == "HUITZILTEPEC" ~ 21079,
      municipality == "IXCAMILPA DE GUERRERO" ~ 21081,
      municipality == "IXCAQUIXTLA" ~ 21082,
      municipality == "IXTACAMAXTITLAN" ~ 21083,
      municipality == "IXTEPEC" ~ 21084,
      municipality == "IZUCAR DE MATAMOROS" ~ 21085,
      municipality == "JALPAN" ~ 21086,
      municipality == "JOLALPAN" ~ 21087,
      municipality == "JONOTLA" ~ 21088,
      municipality == "JOPALA" ~ 21089,
      municipality == "JUAN C. BONILLA" ~ 21090,
      municipality == "JUAN GALINDO" ~ 21091,
      municipality == "JUAN N. MENDEZ" ~ 21092,
      municipality == "MAGDALENA TLATLAUQUITEPEC" ~ 21095,
      municipality == "LAFRAGUA" ~ 21093,
      municipality == "LIBRES" ~ 21094,
      municipality == "LOS REYES DE JUAREZ" ~ 21118,
      municipality == "MAZAPILTEPEC DE JUAREZ" ~ 21096,
      municipality == "MIXTLA" ~ 21097,
      municipality == "MOLCAXAC" ~ 21098,
      municipality == "NAUPAN" ~ 21100,
      municipality == "NAUZONTLA" ~ 21101,
      municipality == "NEALTICAN" ~ 21102,
      municipality == "NICOLAS BRAVO" ~ 21103,
      municipality == "NOPALUCAN" ~ 21104,
      municipality == "OCOTEPEC" ~ 21105,
      municipality == "OCOYUCAN" ~ 21106,
      municipality == "OLINTLA" ~ 21107,
      municipality == "ORIENTAL" ~ 21108,
      municipality == "PAHUATLAN" ~ 21109,
      municipality == "PALMAR DE BRAVO" ~ 21110,
      municipality == "PANTEPEC" ~ 21111,
      municipality == "PETLALCINGO" ~ 21112,
      municipality == "PIAXTLA" ~ 21113,
      municipality == "PUEBLA" ~ 21114,
      municipality == "QUECHOLAC" ~ 21115,
      municipality == "QUIMIXTLAN" ~ 21116,
      municipality == "RAFAEL LARA GRAJALES" ~ 21117,
      municipality == "SAN ANDRES CHOLULA" ~ 21119,
      municipality == "SAN ANTONIO CAxD1ADA" ~ 21120 | str_detect(municipality, "SAN ANTONIO CA"),
      municipality == "SAN DIEGO LA MESA TOCHIMILTZINGO" ~ 21121,
      municipality == "SAN FELIPE TEOTLALCINGO" ~ 21122,
      municipality == "SAN FELIPE TEPATLAN" ~ 21123,
      municipality == "SAN GABRIEL CHILAC" ~ 21124,
      municipality == "SAN GREGORIO ATZOMPA" ~ 21125,
      municipality == "SAN JERONIMO TECUANIPAN" ~ 21126,
      municipality == "SAN JERONIMO XAYACATLAN" ~ 21127,
      municipality == "SAN JOSE CHIAPA" ~ 21128,
      municipality == "SAN JOSE MIAHUATLAN" ~ 21129,
      municipality == "SAN JUAN ATENCO" ~ 21130,
      municipality == "SAN JUAN ATZOMPA" ~ 21131,
      municipality == "SAN MARTIN TEXMELUCAN" ~ 21132,
      municipality == "SAN MARTIN TOTOLTEPEC" ~ 21133,
      municipality == "SAN MATIAS TLALANCALECA" ~ 21134,
      municipality == "SAN MIGUEL IXITLAN" ~ 21135,
      municipality == "SAN MIGUEL XOXTLA" ~ 21136,
      municipality == "SAN NICOLAS BUENOS AIRES" ~ 21137,
      municipality == "SAN NICOLAS DE LOS RANCHOS" ~ 21138,
      municipality == "SAN PABLO ANICANO" ~ 21139,
      municipality == "SAN PEDRO CHOLULA" ~ 21140,
      municipality == "SAN PEDRO YELOIXTLAHUACA" ~ 21141,
      municipality == "SAN SALVADOR EL SECO" ~ 21142,
      municipality == "SAN SALVADOR EL VERDE" ~ 21143,
      municipality == "SAN SALVADOR HUIXCOLOTLA" ~ 21144,
      municipality == "SAN SEBASTIAN TLACOTEPEC" ~ 21145,
      municipality == "SANTA CATARINA TLALTEMPAN" ~ 21146,
      municipality == "SANTA INES AHUATEMPAN" ~ 21147,
      municipality == "SANTA ISABEL CHOLULA" ~ 21148,
      municipality == "SANTIAGO MIAHUATLAN" ~ 21149,
      municipality == "SANTO TOMAS HUEYOTLIPAN" ~ 21151,
      municipality == "SOLTEPEC" ~ 21152,
      municipality == "TECALI DE HERRERA" ~ 21153,
      municipality == "TECAMACHALCO" ~ 21154,
      municipality == "TECOMATLAN" ~ 21155,
      municipality == "TEHUACAN" ~ 21156,
      municipality == "TEHUITZINGO" ~ 21157,
      municipality == "TENAMPULCO" ~ 21158,
      municipality == "TEOPANTLAN" ~ 21159,
      municipality == "TEOTLALCO" ~ 21160,
      municipality == "TEPANCO DE LOPEZ" ~ 21161,
      municipality == "TEPANGO DE RODRIGUEZ" ~ 21162,
      municipality == "TEPATLAXCO DE HIDALGO" ~ 21163,
      municipality == "TEPEACA" ~ 21164,
      municipality == "TEPEMAXALCO" ~ 21165,
      municipality == "TEPEOJUMA" ~ 21166,
      municipality == "TEPETZINTLA" ~ 21167,
      municipality == "TEPEXCO" ~ 21168,
      municipality == "TEPEXI DE RODRIGUEZ" ~ 21169,
      municipality == "TEPEYAHUALCO" ~ 21170,
      municipality == "TEPEYAHUALCO DE CUAUHTEMOC" ~ 21171,
      municipality == "TETELA DE OCAMPO" ~ 21172,
      municipality == "TETELES DE AVILA CASTILLO" ~ 21173,
      municipality == "TEZIUTLAN" ~ 21174,
      municipality == "TIANGUISMANALCO" ~ 21175,
      municipality == "TILAPA" ~ 21176,
      municipality == "TLACHICHUCA" ~ 21179,
      municipality == "TLACOTEPEC DE BENITO JUAREZ" ~ 21177,
      municipality == "TLACUILOTEPEC" ~ 21178,
      municipality == "TLAHUAPAN" ~ 21180,
      municipality == "TLALTENANGO" ~ 21181,
      municipality == "TLANEPANTLA" ~ 21182,
      municipality == "TLAOLA" ~ 21183,
      municipality == "TLAPACOYA" ~ 21184,
      municipality == "TLAPANALA" ~ 21185,
      municipality == "TLATLAUQUITEPEC" ~ 21186,
      municipality == "TLAXCO" ~ 21187,
      municipality == "TOCHIMILCO" ~ 21188,
      municipality == "TOCHTEPEC" ~ 21189,
      municipality == "TOTOLTEPEC DE GUERRERO" ~ 21190,
      municipality == "TULCINGO DE VALLE" ~ 21191,
      municipality == "TUZAMAPAN DE GALEANA" ~ 21192,
      municipality == "TZICATLACOYAN" ~ 21193,
      municipality == "VENUSTIANO CARRANZA" ~ 21194,
      municipality == "VICENTE GUERRERO" ~ 21195,
      municipality == "XAYACATLAN DE BRAVO" ~ 21196,
      municipality == "XICOTEPEC" ~ 21197,
      municipality == "XICOTLAN" ~ 21198,
      municipality == "XIUTETELCO" ~ 21199,
      municipality == "XOCHIAPULCO" ~ 21200,
      municipality == "XOCHILTEPEC" ~ 21201,
      municipality == "XOCHITLAN DE VICENTE SUAREZ" ~ 21202,
      municipality == "XOCHITLAN TODOS SANTOS" ~ 21203,
      municipality == "YAONAHUAC" ~ 21204,
      municipality == "YEHUALTEPEC" ~ 21205,
      municipality == "ZACAPALA" ~ 21206,
      municipality == "ZACAPOAXTLA" ~ 21207,
      municipality == "ZACATLAN" ~ 21208,
      municipality == "ZAPOTITLAN SALINAS" ~ 21209,
      municipality == "ZAPOTITLAN DE MENDEZ" ~ 21210,
      municipality == "ZARAGOZA" ~ 21211,
      municipality == "ZAUTLA" ~ 21212,
      municipality == "ZIHUATEUTLA" ~ 21213,
      municipality == "ZINACATEPEC" ~ 21214,
      municipality == "ZONGOZOTLA" ~ 21215,
      municipality == "ZOQUIAPAN" ~ 21216,
      municipality == "ZOQUITLAN" ~ 21217,
      TRUE ~ uniqueid # Retain existing uniqueid if no match
    )
  )

# Create 'valid' as the row total of specified columns (sum of PAN, PRI, PRD, PT, PVEM, PC)
data <- data %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PC)), na.rm = TRUE))

# Add year and month columns
data_2004 <- data %>%
  mutate(year = 2004, month = "November")

# Sort the data by 'section'
data <- data %>%
  arrange(section)

# Import the Excel file with case normalization (convert to lowercase) and keep first row as headers
data <- read_excel("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/Ayu_Seccion_2007.xlsx", .name_repair = "minimal")
colnames(data) <- tolower(colnames(data))
# Rename columns
data <- data %>%
  rename(
    municipality = municipio,
    section = seccion  # Renaming 'sección' to 'seccin' and then 'seccin' to 'section'
  )

# Drop the 'total' column if it exists
data <- data %>%
  select(-total, everything())

# Drop rows where both 'municipality' is empty and 'section' is missing (NA)
data <- data %>%
  filter(!(municipality == "" & is.na(section)))

# Generate 'total' as the row total of specified columns (PAN, CUPS, CPBP, etc.)
data <- data %>%
  mutate(total = rowSums(across(c(pan, cupg, cpbp, pt, pna, asppn, pec, noregistrados, nulos)), na.rm = TRUE))

# Drop rows where 'total' is missing or 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Convert specified columns from character to numeric 
data <- data %>%
  mutate(across(pan:listanominal, as.numeric))

# Collapse (sum) the data by 'municipality' and 'section'
data <- data %>%
  group_by(municipality, section) %>%
  summarize(across(c(pan:pec, listanominal, total), sum, na.rm = TRUE))

# Rename variables for political parties
data <- data %>%
  rename(
    PAN = pan,
    PRI_PVEM = cupg,    # Rename 'cupg' to 'PRI_PVEM'
    PRD_PC = cpbp,      # Rename 'cpbp' to 'PRD_PC'
    PT = pt,
    PANAL = pna,        # Rename 'pna' to 'PANAL'
    PAS = asppn,        # Rename 'asppn' to 'PAS'
    PEC = pec
  )

# Calculate turnout as the ratio of 'total' to 'listanominal'
data <- data %>%
  mutate(turnout = total / listanominal)

# Replace accented characters in 'municipality'
data <- data %>%
  mutate(
    municipality = str_replace_all(municipality, c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ñ" = "n", 
                                                   "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U"))
  )

# Initialize 'uniqueid' with 0
collapsed_data <- data %>%
  mutate(uniqueid = 0)

# Use a sequence of conditional replacements to assign unique IDs based on 'municipality'
collapsed_data <- collapsed_data %>%
  mutate(
    uniqueid = case_when(
      municipality == "Acajete" ~ 21001,
      municipality == "Acateno" ~ 21002,
      municipality == "Acatlan" ~ 21003,
      municipality == "Acatzingo" ~ 21004,
      municipality == "Acteopan" ~ 21005,
      municipality == "Ahuacatlan" ~ 21006,
      municipality == "Ahuatlan" ~ 21007,
      municipality == "Ahuazotepec" ~ 21008,
      municipality == "Ahuehuetitla" ~ 21009,
      municipality == "Ajalpan" ~ 21010,
      municipality == "Albino Zertuche" ~ 21011,
      municipality == "Aljojuca" ~ 21012,
      municipality == "Altepexi" ~ 21013,
      municipality == "Amixtlan" ~ 21014,
      municipality == "Amozoc" ~ 21015,
      municipality == "Aquixtla" ~ 21016,
      municipality == "Atempan" ~ 21017,
      municipality == "Atexcal" ~ 21018,
      municipality == "Atlequizayan" ~ 21080,
      municipality == "Atlixco" ~ 21019,
      municipality == "Atoyatempan" ~ 21020,
      municipality == "Atzala" ~ 21021,
      municipality == "Atzitzihuacan" ~ 21022,
      municipality == "Atzitzintla" ~ 21023,
      municipality == "Axutla" ~ 21024,
      municipality == "Ayotoxco de Guerrero" ~ 21025,
      municipality == "Calpan" ~ 21026,
      municipality == "Caltepec" ~ 21027,
      municipality == "Camocuautla" ~ 21028,
      municipality == "Canada Morelos" ~ 21099,
      municipality == "Caxhuacan" ~ 21029,
      municipality == "Chalchicomula de Sesma" ~ 21045,
      municipality == "Chapulco" ~ 21046,
      municipality == "Chiautla" ~ 21047,
      municipality == "Chiautzingo" ~ 21048,
      municipality == "Chichiquila" ~ 21050,
      municipality == "Chiconcuautla" ~ 21049,
      municipality == "Chietla" ~ 21051,
      municipality == "Chigmecatitlan" ~ 21052,
      municipality == "Chignahuapan" ~ 21053,
      municipality == "Chignautla" ~ 21054,
      municipality == "Chila" ~ 21055,
      municipality == "Chila de la Sal" ~ 21056,
      municipality == "Chilchotla" ~ 21058,
      municipality == "Chinantla" ~ 21059,
      municipality == "Coatepec" ~ 21030,
      municipality == "Coatzingo" ~ 21031,
      municipality == "Cohetzala" ~ 21032,
      municipality == "Cohuecan" ~ 21033,
      municipality == "Coronango" ~ 21034,
      municipality == "Coxcatlan" ~ 21035,
      municipality == "Coyomeapan" ~ 21036,
      municipality == "Coyotepec" ~ 21037,
      municipality == "Cuapiaxtla de Madero" ~ 21038,
      municipality == "Cuautempan" ~ 21039,
      municipality == "Cuautinchan" ~ 21040,
      municipality == "Cuautlancingo" ~ 21041,
      municipality == "Cuayuca de Andrade" ~ 21042,
      municipality == "Cuetzalan del Progreso" ~ 21043,
      municipality == "Cuyoaco" ~ 21044,
      municipality == "Domingo Arenas" ~ 21060,
      municipality == "Eloxochitlan" ~ 21061,
      municipality == "Epatlan" ~ 21062,
      municipality == "Esperanza" ~ 21063,
      municipality == "Francisco Z. Mena" ~ 21064,
      municipality == "General Felipe Angeles" ~ 21065,
      municipality == "Guadalupe" ~ 21066,
      municipality == "Guadalupe Victoria" ~ 21067,
      municipality == "Hermenegildo Galeana" ~ 21068,
      municipality == "Honey" ~ 21057,
      municipality == "Huaquechula" ~ 21069,
      municipality == "Huatlatlauca" ~ 21070,
      municipality == "Huauchinango" ~ 21071,
      municipality == "Huehuetla" ~ 21072,
      municipality == "Huehuetlan el Chico" ~ 21073,
      municipality == "Huehuetlan El Grande" ~ 21150,
      municipality == "Huejotzingo" ~ 21074,
      municipality == "Hueyapan" ~ 21075,
      municipality == "Hueytamalco" ~ 21076,
      municipality == "Hueytlalpan" ~ 21077,
      municipality == "Huitzilan de Serdan" ~ 21078,
      municipality == "Huitziltepec" ~ 21079,
      municipality == "Ixcamilpa de Guerrero" ~ 21081,
      municipality == "Ixcaquixtla" ~ 21082,
      municipality == "Ixtacamaxtitlan" ~ 21083,
      municipality == "Ixtepec" ~ 21084,
      municipality == "Izucar de Matamoros" ~ 21085,
      municipality == "Jalpan" ~ 21086,
      municipality == "Jolalpan" ~ 21087,
      municipality == "Jonotla" ~ 21088,
      municipality == "Jopala" ~ 21089,
      municipality == "Juan C. Bonilla" ~ 21090,
      municipality == "Juan Galindo" ~ 21091,
      municipality == "Juan N. Mendez" ~ 21092,
      municipality == "La Magdalena Tlatlauquitepec" ~ 21095,
      municipality == "Lafragua" ~ 21093,
      municipality == "Libres" ~ 21094,
      municipality == "Los Reyes de Juarez" ~ 21118,
      municipality == "Mazapiltepec de Juarez" ~ 21096,
      municipality == "Mixtla" ~ 21097,
      municipality == "Molcaxac" ~ 21098,
      municipality == "Naupan" ~ 21100,
      municipality == "Nauzontla" ~ 21101,
      municipality == "Nealtican" ~ 21102,
      municipality == "Nicolas Bravo" ~ 21103,
      municipality == "Nopalucan" ~ 21104,
      municipality == "Ocotepec" ~ 21105,
      municipality == "Ocoyucan" ~ 21106,
      municipality == "Olintla" ~ 21107,
      municipality == "Oriental" ~ 21108,
      municipality == "Pahuatlan" ~ 21109,
      municipality == "Palmar de Bravo" ~ 21110,
      municipality == "Pantepec" ~ 21111,
      municipality == "Petlalcingo" ~ 21112,
      municipality == "Piaxtla" ~ 21113,
      municipality == "Puebla" | municipality == "Heroica Ciudad de Puebla de Zaragoza" ~ 21114,
      municipality == "Quecholac" ~ 21115,
      municipality == "Quimixtlan" ~ 21116,
      municipality == "Rafael Lara Grajales" ~ 21117,
      municipality == "San Andres Cholula" ~ 21119,
      municipality == "San Antonio Canada" ~ 21120,
      municipality == "San Diego La Mesa Tochimiltzingo" ~ 21121,
      municipality == "San Felipe Teotlalcingo" ~ 21122,
      municipality == "San Felipe Tepatlan" ~ 21123,
      municipality == "San Gabriel Chilac" ~ 21124,
      municipality == "San Gregorio Atzompa" ~ 21125,
      municipality == "San Jeronimo Tecuanipan" ~ 21126,
      municipality == "San Jeronimo Xayacatlan" ~ 21127,
      municipality == "San Jose Chiapa" ~ 21128,
      municipality == "San Jose Miahuatlan" ~ 21129,
      municipality == "San Juan Atenco" ~ 21130,
      municipality == "San Juan Atzompa" ~ 21131,
      municipality == "San Martin Texmelucan" ~ 21132,
      municipality == "San Martin Totoltepec" ~ 21133,
      municipality == "San Matias Tlalancaleca" ~ 21134,
      municipality == "San Miguel Ixitlan" ~ 21135,
      municipality == "San Miguel Xoxtla" ~ 21136,
      municipality == "San Nicolas Buenos Aires" ~ 21137,
      municipality == "San Nicolas de los Ranchos" ~ 21138,
      municipality == "San Pablo Anicano" ~ 21139,
      municipality == "San Pedro Cholula" ~ 21140,
      municipality == "San Pedro Yeloixtlahuaca" ~ 21141,
      municipality == "San Salvador El Seco" ~ 21142,
      municipality == "San Salvador El Verde" ~ 21143,
      municipality == "San Salvador Huixcolotla" ~ 21144,
      municipality == "San Sebastian Tlacotepec" ~ 21145,
      municipality == "Santa Catarina Tlaltempan" ~ 21146,
      municipality == "Santa Ines Ahuatempan" ~ 21147,
      municipality == "Santa Isabel Cholula" ~ 21148,
      municipality == "Santiago Miahuatlan" ~ 21149,
      municipality == "Santo Tomas Hueyotlipan" ~ 21151,
      municipality == "Soltepec" ~ 21152,
      municipality == "Tecali de Herrera" ~ 21153,
      municipality == "Tecamachalco" ~ 21154,
      municipality == "Tecomatlan" ~ 21155,
      municipality == "Tehuacan" ~ 21156,
      municipality == "Tehuitzingo" ~ 21157,
      municipality == "Tenampulco" ~ 21158,
      municipality == "Teopantlan" ~ 21159,
      municipality == "Teotlalco" ~ 21160,
      municipality == "Tepanco de Lopez" ~ 21161,
      municipality == "Tepango de Rodriguez" ~ 21162,
      municipality == "Tepatlaxco de Hidalgo" ~ 21163,
      municipality == "Tepeaca" ~ 21164,
      municipality == "Tepemaxalco" ~ 21165,
      municipality == "Tepeojuma" ~ 21166,
      municipality == "Tepetzintla" ~ 21167,
      municipality == "Tepexco" ~ 21168,
      municipality == "Tepexi de Rodriguez" ~ 21169,
      municipality == "Tepeyahualco" ~ 21170,
      municipality == "Tepeyahualco de Cuauhtemoc" ~ 21171,
      municipality == "Tetela de Ocampo" ~ 21172,
      municipality == "Teteles de Avila Castillo" ~ 21173,
      municipality == "Teziutlan" ~ 21174,
      municipality == "Tianguismanalco" ~ 21175,
      municipality == "Tilapa" ~ 21176,
      municipality == "Tlachichuca" ~ 21179,
      municipality == "Tlacotepec de Benito Juarez" ~ 21177,
      municipality == "Tlacuilotepec" ~ 21178,
      municipality == "Tlahuapan" ~ 21180,
      municipality == "Tlaltenango" ~ 21181,
      municipality == "Tlanepantla" ~ 21182,
      municipality == "Tlaola" ~ 21183,
      municipality == "Tlapacoya" ~ 21184,
      municipality == "Tlapanala" ~ 21185,
      municipality == "Tlatlauquitepec" ~ 21186,
      municipality == "Tlaxco" ~ 21187,
      municipality == "Tochimilco" ~ 21188,
      municipality == "Tochtepec" ~ 21189,
      municipality == "Totoltepec de Guerrero" ~ 21190,
      municipality == "Tulcingo" ~ 21191,
      municipality == "Tuzamapan de Galeana" ~ 21192,
      municipality == "Tzicatlacoyan" ~ 21193,
      municipality == "Venustiano Carranza" ~ 21194,
      municipality == "Vicente Guerrero" ~ 21195,
      municipality == "Xayacatlan de Bravo" ~ 21196,
      municipality == "Xicotepec" ~ 21197,
      municipality == "Xicotlan" ~ 21198,
      municipality == "Xiutetelco" ~ 21199,
      municipality == "Xochiapulco" ~ 21200,
      municipality == "Xochiltepec" ~ 21201,
      municipality == "Xochitlan de Vicente Suarez" ~ 21202,
      municipality == "Xochitlan Todos Santos" ~ 21203,
      municipality == "Yaonahuac" ~ 21204,
      municipality == "Yehualtepec" ~ 21205,
      municipality == "Zacapala" ~ 21206,
      municipality == "Zacapoaxtla" ~ 21207,
      municipality == "Zacatlan" ~ 21208,
      municipality == "Zapotitlan" ~ 21209,
      municipality == "Zapotitlan de Mendez" ~ 21210,
      municipality == "Zaragoza" ~ 21211,
      municipality == "Zautla" ~ 21212,
      municipality == "Zihuateutla" ~ 21213,
      municipality == "Zinacatepec" ~ 21214,
      municipality == "Zongozotla" ~ 21215,
      municipality == "Zoquiapan" ~ 21216,
      municipality == "Zoquitlan" ~ 21217,
      TRUE ~ uniqueid  # Retain existing value if no match is found
    )
  )

# Create 'valid' as the row total of specified columns (sum of PAN, PRI_PVEM, PRD_PC, PT, PANAL, PAS, PEC)
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN, PRI_PVEM, PRD_PC, PT, PANAL, PAS, PEC)), na.rm = TRUE))

# Add year and month columns
data_2007 <- collapsed_data %>%
  mutate(year = 2007, month = "November")

# Import the Excel sheet with specified cell range
data <- read_excel("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/ResultadosDeLaElecciónVotaciónXCasilla Aytos extra. 2008.xlsx", sheet = "ayto", range = "A7:R25", col_names = TRUE)
# Remove "-" and spaces
names(data) <- gsub("[. ]", "", names(data))
# Convert all columns to numeric
data <- data %>%
  mutate(across(everything(), as.numeric))

# Rename columns
data <- data %>%
  rename(
    section = Sección,
    PAN = PARTIDOACCIÓNNACIONAL,
    PT = PARTIDODELTRABAJO,
    PC = CONVERGENCIA,
    PANAL = PARTIDONUEVAALIANZA,
    PAS = "ALTERNATIVASOCIALDEMÓCRATA,PARTIDOPOLÍTICONACIONAL",
    total = VotaciónTotal,
    listanominal = ListaNominal
  )

# Drop rows where 'section' is missing
data <- data %>%
  filter(!is.na(section))

# Generate new columns for municipality and uniqueid
data <- data %>%
  mutate(
    municipality = "GENERAL FELIPE ANGELES EXTRAORDINARIO",
    uniqueid = 21065
  )

# Collapse the data by municipality, uniqueid, and section
collapsed_data <- data %>%
  group_by(municipality, uniqueid, section) %>%
  summarize(across(c(PAN:PAS, total, listanominal), sum, na.rm = TRUE))

# Calculate turnout
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal)

# Create 'valid' as the row total of specified columns (sum of PAN, PRI, PRD, PT, PVEM, PC, PANAL, PAS)
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PC, PANAL, PAS)), na.rm = TRUE))

# Add year and month columns
data_2008 <- collapsed_data %>%
  mutate(year = 2008, month = "June")

# Import the Excel file and convert column names to lowercase
data <- read_excel("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/Ayu_Seccion_2010.xlsx", .name_repair = "minimal")
colnames(data) <- tolower(colnames(data))
# Rename columns
data <- data %>%
  rename(
    municipality = municipio,
    section = seccion
  )

# Drop rows where both 'municipality' is empty and 'section' is missing (NA)
data <- data %>%
  filter(!(municipality == "" & is.na(section)))

# Drop rows where 'total' is missing or equal to 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Convert specified columns from character to numeric
data <- data %>%
  mutate(across(c(pan_prd_pc_panal:pt, total, listanominal), as.numeric))

# Collapse (sum) the data by 'municipality' and 'section'
collapsed_data <- data %>%
  group_by(municipality, section) %>%
  summarize(across(c(pan_prd_pc_panal:pt, total, listanominal), sum, na.rm = TRUE))

# Rename variables for political coalitions
collapsed_data <- collapsed_data %>%
  rename(
    PAN_PRD_PC_PANAL = pan_prd_pc_panal,
    PRI_PVEM = pri_pvem,
    PT = pt
  )

# Calculate turnout as the ratio of 'total' to 'listanominal'
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal)

# Replace accented characters and standardize municipality names
collapsed_data <- collapsed_data %>%
  mutate(
    municipality = str_replace_all(municipality, c("Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U", "Ñ" = "N")),
    municipality = str_to_title(municipality),
    municipality = str_trim(municipality)       # Remove leading and trailing spaces
  )

# The resulting 'collapsed_data'

# Initialize 'uniqueid' with 0
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = 0)

# Use case_when to assign uniqueid based on 'municipality'
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = case_when(
    municipality == "Acajete" ~ 21001,
    municipality == "Acateno" ~ 21002,
    municipality == "Acatlan" ~ 21003,
    municipality == "Acatzingo" ~ 21004,
    municipality == "Acteopan" ~ 21005,
    municipality == "Ahuacatlan" ~ 21006,
    municipality == "Ahuatlan" ~ 21007,
    municipality == "Ahuazotepec" ~ 21008,
    municipality == "Ahuehuetitla" ~ 21009,
    municipality == "Ajalpan" ~ 21010,
    municipality == "Albino Zertuche" ~ 21011,
    municipality == "Aljojuca" ~ 21012,
    municipality == "Altepexi" ~ 21013,
    municipality == "Amixtlan" ~ 21014,
    municipality == "Amozoc" ~ 21015,
    municipality == "Aquixtla" ~ 21016,
    municipality == "Atempan" ~ 21017,
    municipality == "Atexcal" ~ 21018,
    municipality == "Atlequizayan" ~ 21080,
    municipality == "Atlixco" ~ 21019,
    municipality == "Atoyatempan" ~ 21020,
    municipality == "Atzala" ~ 21021,
    municipality == "Atzitzihuacan" ~ 21022,
    municipality == "Atzitzintla" ~ 21023,
    municipality == "Axutla" ~ 21024,
    municipality == "Ayotoxco De Guerrero" ~ 21025,
    municipality == "Calpan" ~ 21026,
    municipality == "Caltepec" ~ 21027,
    municipality == "Camocuautla" ~ 21028,
    municipality == "Canada Morelos" ~ 21099,
    municipality == "Caxhuacan" ~ 21029,
    municipality == "Chalchicomula De Sesma" ~ 21045,
    municipality == "Chapulco" ~ 21046,
    municipality == "Chiautla" ~ 21047,
    municipality == "Chiautzingo" ~ 21048,
    municipality == "Chichiquila" ~ 21050,
    municipality == "Chiconcuautla" ~ 21049,
    municipality == "Chietla" ~ 21051,
    municipality == "Chigmecatitlan" ~ 21052,
    municipality == "Chignahuapan" ~ 21053,
    municipality == "Chignautla" ~ 21054,
    municipality == "Chila" ~ 21055,
    municipality == "Chila De La Sal" ~ 21056,
    municipality == "Chilchotla" ~ 21058,
    municipality == "Chinantla" ~ 21059,
    municipality == "Coatepec" ~ 21030,
    municipality == "Coatzingo" ~ 21031,
    municipality == "Cohetzala" ~ 21032,
    municipality == "Cohuecan" ~ 21033,
    municipality == "Coronango" ~ 21034,
    municipality == "Coxcatlan" ~ 21035,
    municipality == "Coyomeapan" ~ 21036,
    municipality == "Coyotepec" ~ 21037,
    municipality == "Cuapiaxtla De Madero" ~ 21038,
    municipality == "Cuautempan" ~ 21039,
    municipality == "Cuautinchan" ~ 21040,
    municipality == "Cuautlancingo" ~ 21041,
    municipality == "Cuayuca De Andrade" ~ 21042,
    municipality == "Cuetzalan Del Progreso" ~ 21043,
    municipality == "Cuyoaco" ~ 21044,
    municipality == "Domingo Arenas" ~ 21060,
    municipality == "Eloxochitlan" ~ 21061,
    municipality == "Epatlan" ~ 21062,
    municipality == "Esperanza" ~ 21063,
    municipality == "Francisco Z. Mena" ~ 21064,
    municipality == "General Felipe Angeles" ~ 21065,
    municipality == "Guadalupe" ~ 21066,
    municipality == "Guadalupe Victoria" ~ 21067,
    municipality == "Hermenegildo Galeana" ~ 21068,
    municipality == "Honey" ~ 21057,
    municipality == "Huaquechula" ~ 21069,
    municipality == "Huatlatlauca" ~ 21070,
    municipality == "Huauchinango" ~ 21071,
    municipality == "Huehuetla" ~ 21072,
    municipality == "Huehuetlan El Chico" ~ 21073,
    municipality == "Huehuetlan El Grande" ~ 21150,
    municipality == "Huejotzingo" ~ 21074,
    municipality == "Hueyapan" ~ 21075,
    municipality == "Hueytamalco" ~ 21076,
    municipality == "Hueytlalpan" ~ 21077,
    municipality == "Huitzilan De Serdan" ~ 21078,
    municipality == "Huitziltepec" ~ 21079,
    municipality == "Ixcamilpa De Guerrero" ~ 21081,
    municipality == "Ixcaquixtla" ~ 21082,
    municipality == "Ixtacamaxtitlan" ~ 21083,
    municipality == "Ixtepec" ~ 21084,
    municipality == "Izucar De Matamoros" ~ 21085,
    municipality == "Jalpan" ~ 21086,
    municipality == "Jolalpan" ~ 21087,
    municipality == "Jonotla" ~ 21088,
    municipality == "Jopala" ~ 21089,
    municipality == "Juan C. Bonilla" ~ 21090,
    municipality == "Juan Galindo" ~ 21091,
    municipality == "Juan N. Mendez" ~ 21092,
    municipality == "La Magdalena Tlatlauquitepec" ~ 21095,
    municipality == "Lafragua" ~ 21093,
    municipality == "Libres" ~ 21094,
    municipality == "Los Reyes De Juarez" ~ 21118,
    municipality == "Mazapiltepec De Juarez" ~ 21096,
    municipality == "Mixtla" ~ 21097,
    municipality == "Molcaxac" ~ 21098,
    municipality == "Naupan" ~ 21100,
    municipality == "Nauzontla" ~ 21101,
    municipality == "Nealtican" ~ 21102,
    municipality == "Nicolas Bravo" ~ 21103,
    municipality == "Nopalucan" ~ 21104,
    municipality == "Ocotepec" ~ 21105,
    municipality == "Ocoyucan" ~ 21106,
    municipality == "Olintla" ~ 21107,
    municipality == "Oriental" ~ 21108,
    municipality == "Pahuatlan" ~ 21109,
    municipality == "Palmar De Bravo" ~ 21110,
    municipality == "Pantepec" ~ 21111,
    municipality == "Petlalcingo" ~ 21112,
    municipality == "Piaxtla" ~ 21113,
    municipality == "Puebla" ~ 21114,
    municipality == "Quecholac" ~ 21115,
    municipality == "Quimixtlan" ~ 21116,
    municipality == "Rafael Lara Grajales" ~ 21117,
    municipality == "San Andres Cholula" ~ 21119,
    municipality == "San Antonio Canada" ~ 21120,
    municipality == "San Diego La Mesa Tochimiltzingo" ~ 21121,
    municipality == "San Felipe Teotlalcingo" ~ 21122,
    municipality == "San Felipe Tepatlan" ~ 21123,
    municipality == "San Gabriel Chilac" ~ 21124,
    municipality == "San Gregorio Atzompa" ~ 21125,
    municipality == "San Jeronimo Tecuanipan" ~ 21126,
    municipality == "San Jeronimo Xayacatlan" ~ 21127,
    municipality == "San Jose Chiapa" ~ 21128,
    municipality == "San Jose Miahuatlan" ~ 21129,
    municipality == "San Juan Atenco" ~ 21130,
    municipality == "San Juan Atzompa" ~ 21131,
    municipality == "San Martin Texmelucan" ~ 21132,
    municipality == "San Martin Totoltepec" ~ 21133,
    municipality == "San Matias Tlalancaleca" ~ 21134,
    municipality == "San Miguel Ixitlan" ~ 21135,
    municipality == "San Miguel Xoxtla" ~ 21136,
    municipality == "San Nicolas Buenos Aires" ~ 21137,
    municipality == "San Nicolas De Los Ranchos" ~ 21138,
    municipality == "San Pablo Anicano" ~ 21139,
    municipality == "San Pedro Cholula" ~ 21140,
    municipality == "San Pedro Yeloixtlahuaca" ~ 21141,
    municipality == "San Salvador El Seco" ~ 21142,
    municipality == "San Salvador El Verde" ~ 21143,
    municipality == "San Salvador Huixcolotla" ~ 21144,
    municipality == "San Sebastian Tlacotepec" ~ 21145,
    municipality == "Santa Catarina Tlaltempan" ~ 21146,
    municipality == "Santa Ines Ahuatempan" ~ 21147,
    municipality == "Santa Isabel Cholula" ~ 21148,
    municipality == "Santiago Miahuatlan" ~ 21149,
    municipality == "Santo Tomas Hueyotlipan" ~ 21151,
    municipality == "Soltepec" ~ 21152,
    municipality == "Tecali De Herrera" ~ 21153,
    municipality == "Tecamachalco" ~ 21154,
    municipality == "Tecomatlan" ~ 21155,
    municipality == "Tehuacan" ~ 21156,
    municipality == "Tehuitzingo" ~ 21157,
    municipality == "Tenampulco" ~ 21158,
    municipality == "Teopantlan" ~ 21159,
    municipality == "Teotlalco" ~ 21160,
    municipality == "Tepanco De Lopez" ~ 21161,
    municipality == "Tepango De Rodriguez" ~ 21162,
    municipality == "Tepatlaxco De Hidalgo" ~ 21163,
    municipality == "Tepeaca" ~ 21164,
    municipality == "Tepemaxalco" ~ 21165,
    municipality == "Tepeojuma" ~ 21166,
    municipality == "Tepetzintla" ~ 21167,
    municipality == "Tepexco" ~ 21168,
    municipality == "Tepexi De Rodriguez" ~ 21169,
    municipality == "Tepeyahualco" ~ 21170,
    municipality == "Tepeyahualco De Cuauhtemoc" ~ 21171,
    municipality == "Tetela De Ocampo" ~ 21172,
    municipality == "Teteles De Avila Castillo" ~ 21173,
    municipality == "Teziutlan" ~ 21174,
    municipality == "Tianguismanalco" ~ 21175,
    municipality == "Tilapa" ~ 21176,
    municipality == "Tlachichuca" ~ 21179,
    municipality == "Tlacotepec De Benito Juarez" ~ 21177,
    municipality == "Tlacuilotepec" ~ 21178,
    municipality == "Tlahuapan" ~ 21180,
    municipality == "Tlaltenango" ~ 21181,
    municipality == "Tlanepantla" ~ 21182,
    municipality == "Tlaola" ~ 21183,
    municipality == "Tlapacoya" ~ 21184,
    municipality == "Tlapanala" ~ 21185,
    municipality == "Tlatlauquitepec" ~ 21186,
    municipality == "Tlaxco" ~ 21187,
    municipality == "Tochimilco" ~ 21188,
    municipality == "Tochtepec" ~ 21189,
    municipality == "Totoltepec De Guerrero" ~ 21190,
    municipality == "Tulcingo" ~ 21191,
    municipality == "Tuzamapan De Galeana" ~ 21192,
    municipality == "Tzicatlacoyan" ~ 21193,
    municipality == "Venustiano Carranza" ~ 21194,
    municipality == "Vicente Guerrero" ~ 21195,
    municipality == "Xayacatlan De Bravo" ~ 21196,
    municipality == "Xicotepec" ~ 21197,
    municipality == "Xicotlan" ~ 21198,
    municipality == "Xiutetelco" ~ 21199,
    municipality == "Xochiapulco" ~ 21200,
    municipality == "Xochiltepec" ~ 21201,
    municipality == "Xochitlan De Vicente Suarez" ~ 21202,
    municipality == "Xochitlan Todos Santos" ~ 21203,
    municipality == "Yaonahuac" ~ 21204,
    municipality == "Yehualtepec" ~ 21205,
    municipality == "Zacapala" ~ 21206,
    municipality == "Zacapoaxtla" ~ 21207,
    municipality == "Zacatlan" ~ 21208,
    municipality == "Zapotitlan" ~ 21209,
    municipality == "Zapotitlan De Mendez" ~ 21210,
    municipality == "Zaragoza" ~ 21211,
    municipality == "Zautla" ~ 21212,
    municipality == "Zihuateutla" ~ 21213,
    municipality == "Zinacatepec" ~ 21214,
    municipality == "Zongozotla" ~ 21215,
    municipality == "Zoquiapan" ~ 21216,
    municipality == "Zoquitlan" ~ 21217,
    TRUE ~ uniqueid  # Retain existing uniqueid value if no match is found
  ))

# Calculate 'valid' as the row total of the specified columns
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN_PRD_PC_PANAL, PRI_PVEM, PT)), na.rm = TRUE))

# Calculate 'valid' as the row total of PAN_PRD_PC_PANAL, PRI_PVEM, and PT
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN_PRD_PC_PANAL, PRI_PVEM, PT)), na.rm = TRUE))

# Add year and month columns
data_2010 <- collapsed_data %>%
  mutate(year = 2010, month = "July")

# Sort the data by 'section'
collapsed_data <- collapsed_data %>%
  arrange(section)

# Import the Excel file with the specified sheet and cell range
data <- read_excel("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/Resul_ por casillas e individuales elec extraordinaria 2011.xlsx", 
                   sheet = "2011", range = "A6:K75", col_names = TRUE)
names(data)
# Rename columns
data <- data %>%
  rename(
    section = SECCION,
    municipality = MUNICIPIO,
    PAN = "...5",
    PRI_PVEM = "...6",
    PC = "...7",
    PANAL = "...8",
    total = "VOTACION TOTAL"
  )

# Drop rows where 'section' is missing
data <- data %>%
  filter(!is.na(section))

# Append " EXTRAORDINARIO" to municipality names
data <- data %>%
  mutate(municipality = paste(municipality, "EXTRAORDINARIO"))

# Initialize 'uniqueid' and assign specific values based on municipality
data <- data %>%
  mutate(uniqueid = case_when(
    municipality == "IXCAMILPA DE GUERRERO EXTRAORDINARIO" ~ 21081,
    municipality == "SAN JERONIMO TECUANIPAN EXTRAORDINARIO" ~ 21126,
    municipality == "TLAOLA EXTRAORDINARIO" ~ 21183,
    TRUE ~ 0  # Default value for other municipalities
  ))

# Collapse the data by 'municipality', 'uniqueid', and 'section'
collapsed_data <- data %>%
  group_by(municipality, uniqueid, section) %>%
  summarize(across(c(PAN:PANAL, total), sum, na.rm = TRUE))

# Load the auxiliary dataset to get 'listanominal'
aux_data <- read_dta("../../all_months_years.dta") %>%
  filter(ed == 21, month == 6, year == 2011) %>%
  select(section, listanominal)

# Merge the auxiliary dataset to the collapsed data on 'section'
collapsed_data <- collapsed_data %>%
  left_join(aux_data, by = "section") %>%
  filter(!is.na(listanominal))

# Calculate turnout
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal)

# Calculate 'valid' as the row total of specified columns
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN, PRI_PVEM, PC, PANAL)), na.rm = TRUE))

# Add year and month columns
data_2011 <- collapsed_data %>%
  mutate(year = 2011, month = "July")

# Import the Excel sheet with specified cell range
data <- read_excel("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/RESULTADOS_POR_CASILLA_AYUNTAMIENTOS Y DIPUTADOS_2012_2013.xlsx", 
                   sheet = "AYUNTAMIENTOS FINAL", range = "A7:O6903", col_names = TRUE)
names(data)
# Rename columns
data <- data %>%
  rename(
    municipality = MUNICIPIO,
    section = SECCIÓN,
    listanominal = "No.  DE CIUDADANOS EN LISTA NOMINAL",
    total = "VOTACIÓN TOTAL"
  )

# Drop rows where both 'municipality' and 'section' are empty
data <- data %>%
  filter(!(is.na(municipality) & is.na(section)))

# Convert 'section' to numeric 
data <- data %>%
  mutate(section = as.numeric(section))

# Drop rows where 'total' is missing or equal to 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Drop columns 'N' and 'O' (assuming there are no corresponding column names in the provided code)
data <- data %>%
  select(-N, -O)

# Compress 'municipality' by removing extra spaces
data <- data %>%
  mutate(municipality = str_trim(municipality))

# Convert specified columns from character to numeric
data <- data %>%
  mutate(across(c(PAN_PRD_PANAL_CPP, listanominal), as.numeric))

# Collapse (sum) the data by 'municipality' and 'section'
collapsed_data <- data %>%
  group_by(municipality, section) %>%
  summarize(across(c(PAN_PRD_PANAL_CPP:listanominal), sum, na.rm = TRUE))

# Replace accented characters and standardize municipality names
collapsed_data <- collapsed_data %>%
  mutate(
    municipality = str_replace_all(municipality, c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ñ" = "n")),
    municipality = str_to_title(municipality)
  )

# Merge with 'Coaliciones_2013.dta' on 'municipality'
coalition_data <- read_dta("Coaliciones_2013.dta")

collapsed_data <- collapsed_data %>%
  left_join(coalition_data, by = "municipality") %>%
  filter(!is.na(coalition))  # Drop rows where there is no match

# Adjust coalitions as per the provided logic
collapsed_data <- collapsed_data %>%
  mutate(
    PAN_PRD_PANAL_CPP_PC = ifelse(coalition == "PAN_PRD_PANAL_CPP, PC", PAN_PRD_PANAL_CPP + PC, NA),
    PAN_PRD_PANAL_CPP_PC_PSI = ifelse(coalition == "PAN_PRD_PANAL_CPP, PC, PSI", PAN_PRD_PANAL_CPP + PC + PSI, NA),
    PAN_PRD_PANAL_CPP_PSI = ifelse(coalition == "PAN_PRD_PANAL_CPP, PSI", PAN_PRD_PANAL_CPP + PSI, NA),
    PAN_PRD_PANAL_CPP_PT = ifelse(coalition == "PAN_PRD_PANAL_CPP, PT", PAN_PRD_PANAL_CPP + PT, NA),
    PAN_PRD_PANAL_CPP_PT_PSI = ifelse(coalition == "PAN_PRD_PANAL_CPP, PT, PSI", PAN_PRD_PANAL_CPP + PT + PSI, NA)
  )

# Replace values to 0 based on coalition conditions
collapsed_data <- collapsed_data %>%
  mutate(
    PAN_PRD_PANAL_CPP = ifelse(coalition %in% c("PAN_PRD_PANAL_CPP, PC", "PAN_PRD_PANAL_CPP, PC, PSI", 
                                                "PAN_PRD_PANAL_CPP, PSI", "PAN_PRD_PANAL_CPP, PT", 
                                                "PAN_PRD_PANAL_CPP, PT, PSI"), 0, PAN_PRD_PANAL_CPP),
    PC = ifelse(coalition %in% c("PAN_PRD_PANAL_CPP, PC", "PAN_PRD_PANAL_CPP, PC, PSI"), 0, PC),
    PSI = ifelse(coalition %in% c("PAN_PRD_PANAL_CPP, PC, PSI", "PAN_PRD_PANAL_CPP, PT, PSI", 
                                  "PAN_PRD_PANAL_CPP, PSI"), 0, PSI),
    PT = ifelse(coalition %in% c("PAN_PRD_PANAL_CPP, PT", "PAN_PRD_PANAL_CPP, PT, PSI"), 0, PT)
  )

# Drop unnecessary columns
collapsed_data <- collapsed_data %>%
  select(-VOTOSNULOS, -CANDIDATOSNOREGISTRADOS)

# Calculate turnout
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal)

# The resulting 'collapsed_data' 

# Initialize 'uniqueid' column with 0
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = 0)

# Replace 'uniqueid' values based on 'municipality'
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = case_when(
    municipality == "Acajete" ~ 21001,
    municipality == "Acateno" ~ 21002,
    municipality == "Acatlan" ~ 21003,
    municipality == "Acatzingo" ~ 21004,
    municipality == "Acteopan" ~ 21005,
    municipality == "Ahuacatlan" ~ 21006,
    municipality == "Ahuatlan" ~ 21007,
    municipality == "Ahuazotepec" ~ 21008,
    municipality == "Ahuehuetitla" ~ 21009,
    municipality == "Ajalpan" ~ 21010,
    municipality == "Albino Zertuche" ~ 21011,
    municipality == "Aljojuca" ~ 21012,
    municipality == "Altepexi" ~ 21013,
    municipality == "Amixtlan" ~ 21014,
    municipality == "Amozoc" ~ 21015,
    municipality == "Aquixtla" ~ 21016,
    municipality == "Atempan" ~ 21017,
    municipality == "Atexcal" ~ 21018,
    municipality == "Atlequizayan" ~ 21080,
    municipality == "Atlixco" ~ 21019,
    municipality == "Atoyatempan" ~ 21020,
    municipality == "Atzala" ~ 21021,
    municipality == "Atzitzihuacan" ~ 21022,
    municipality == "Atzitzintla" ~ 21023,
    municipality == "Axutla" ~ 21024,
    municipality == "Ayotoxco De Guerrero" ~ 21025,
    municipality == "Calpan" ~ 21026,
    municipality == "Caltepec" ~ 21027,
    municipality == "Camocuautla" ~ 21028,
    municipality == "Canada Morelos" ~ 21099,
    municipality == "Caxhuacan" ~ 21029,
    municipality == "Chalchicomula De Sesma" ~ 21045,
    municipality == "Chapulco" ~ 21046,
    municipality == "Chiautla" ~ 21047,
    municipality == "Chiautzingo" ~ 21048,
    municipality == "Chichiquila" ~ 21050,
    municipality == "Chiconcuautla" ~ 21049,
    municipality == "Chietla" ~ 21051,
    municipality == "Chigmecatitlan" ~ 21052,
    municipality == "Chignahuapan" ~ 21053,
    municipality == "Chignautla" ~ 21054,
    municipality == "Chila" ~ 21055,
    municipality == "Chila De La Sal" ~ 21056,
    municipality == "Chilchotla" ~ 21058,
    municipality == "Chinantla" ~ 21059,
    municipality == "Coatepec" ~ 21030,
    municipality == "Coatzingo" ~ 21031,
    municipality == "Cohetzala" ~ 21032,
    municipality == "Cohuecan" ~ 21033,
    municipality == "Coronango" ~ 21034,
    municipality == "Coxcatlan" ~ 21035,
    municipality == "Coyomeapan" ~ 21036,
    municipality == "Coyotepec" ~ 21037,
    municipality == "Cuapiaxtla De Madero" ~ 21038,
    municipality == "Cuautempan" ~ 21039,
    municipality == "Cuautinchan" ~ 21040,
    municipality == "Cuautlancingo" ~ 21041,
    municipality == "Cuayuca De Andrade" ~ 21042,
    municipality == "Cuetzalan Del Progreso" ~ 21043,
    municipality == "Cuyoaco" ~ 21044,
    municipality == "Domingo Arenas" ~ 21060,
    municipality == "Eloxochitlan" ~ 21061,
    municipality == "Epatlan" ~ 21062,
    municipality == "Esperanza" ~ 21063,
    municipality == "Francisco Z. Mena" ~ 21064,
    municipality == "General Felipe Angeles" ~ 21065,
    municipality == "Guadalupe" ~ 21066,
    municipality == "Guadalupe Victoria" ~ 21067,
    municipality == "Hermenegildo Galeana" ~ 21068,
    municipality == "Honey" ~ 21057,
    municipality == "Huaquechula" ~ 21069,
    municipality == "Huatlatlauca" ~ 21070,
    municipality == "Huauchinango" ~ 21071,
    municipality == "Huehuetla" ~ 21072,
    municipality == "Huehuetlan El Chico" ~ 21073,
    municipality == "Huehuetlan El Grande" ~ 21150,
    municipality == "Huejotzingo" ~ 21074,
    municipality == "Hueyapan" ~ 21075,
    municipality == "Hueytamalco" ~ 21076,
    municipality == "Hueytlalpan" ~ 21077,
    municipality == "Huitzilan De Serdan" ~ 21078,
    municipality == "Huitziltepec" ~ 21079,
    municipality == "Ixcamilpa De Guerrero" ~ 21081,
    municipality == "Ixcaquixtla" ~ 21082,
    municipality == "Ixtacamaxtitlan" ~ 21083,
    municipality == "Ixtepec" ~ 21084,
    municipality == "Izucar De Matamoros" ~ 21085,
    municipality == "Jalpan" ~ 21086,
    municipality == "Jolalpan" ~ 21087,
    municipality == "Jonotla" ~ 21088,
    municipality == "Jopala" ~ 21089,
    municipality == "Juan C. Bonilla" ~ 21090,
    municipality == "Juan Galindo" ~ 21091,
    municipality == "Juan N. Mendez" ~ 21092,
    municipality == "La Magdalena Tlatlauquitepec" ~ 21095,
    municipality == "Lafragua" ~ 21093,
    municipality == "Libres" ~ 21094,
    municipality == "Los Reyes De Juarez" ~ 21118,
    municipality == "Mazapiltepec De Juarez" ~ 21096,
    municipality == "Mixtla" ~ 21097,
    municipality == "Molcaxac" ~ 21098,
    municipality == "Naupan" ~ 21100,
    municipality == "Nauzontla" ~ 21101,
    municipality == "Nealtican" ~ 21102,
    municipality == "Nicolas Bravo" ~ 21103,
    municipality == "Nopalucan" ~ 21104,
    municipality == "Ocotepec" ~ 21105,
    municipality == "Ocoyucan" ~ 21106,
    municipality == "Olintla" ~ 21107,
    municipality == "Oriental" ~ 21108,
    municipality == "Pahuatlan" ~ 21109,
    municipality == "Palmar De Bravo" ~ 21110,
    municipality == "Pantepec" ~ 21111,
    municipality == "Petlalcingo" ~ 21112,
    municipality == "Piaxtla" ~ 21113,
    municipality == "Puebla" ~ 21114,
    municipality == "Quecholac" ~ 21115,
    municipality == "Quimixtlan" ~ 21116,
    municipality == "Rafael Lara Grajales" ~ 21117,
    municipality == "San Andres Cholula" ~ 21119,
    municipality == "San Antonio Canada" ~ 21120,
    municipality == "San Diego La Mesa Tochimiltzingo" ~ 21121,
    municipality == "San Felipe Teotlalcingo" ~ 21122,
    municipality == "San Felipe Tepatlan" ~ 21123,
    municipality == "San Gabriel Chilac" ~ 21124,
    municipality == "San Gregorio Atzompa" ~ 21125,
    municipality == "San Jeronimo Tecuanipan" ~ 21126,
    municipality == "San Jeronimo Xayacatlan" ~ 21127,
    municipality == "San Jose Chiapa" ~ 21128,
    municipality == "San Jose Miahuatlan" ~ 21129,
    municipality == "San Juan Atenco" ~ 21130,
    municipality == "San Juan Atzompa" ~ 21131,
    municipality == "San Martin Texmelucan" ~ 21132,
    municipality == "San Martin Totoltepec" ~ 21133,
    municipality == "San Matias Tlalancaleca" ~ 21134,
    municipality == "San Miguel Ixitlan" ~ 21135,
    municipality == "San Miguel Xoxtla" ~ 21136,
    municipality == "San Nicolas Buenos Aires" ~ 21137,
    municipality == "San Nicolas De Los Ranchos" ~ 21138,
    municipality == "San Pablo Anicano" ~ 21139,
    municipality == "San Pedro Cholula" ~ 21140,
    municipality == "San Pedro Yeloixtlahuaca" ~ 21141,
    municipality == "San Salvador El Seco" ~ 21142,
    municipality == "San Salvador El Verde" ~ 21143,
    municipality == "San Salvador Huixcolotla" ~ 21144,
    municipality == "San Sebastian Tlacotepec" ~ 21145,
    municipality == "Santa Catarina Tlaltempan" ~ 21146,
    municipality == "Santa Ines Ahuatempan" ~ 21147,
    municipality == "Santa Isabel Cholula" ~ 21148,
    municipality == "Santiago Miahuatlan" ~ 21149,
    municipality == "Santo Tomas Hueyotlipan" ~ 21151,
    municipality == "Soltepec" ~ 21152,
    municipality == "Tecali De Herrera" ~ 21153,
    municipality == "Tecamachalco" ~ 21154,
    municipality == "Tecomatlan" ~ 21155,
    municipality == "Tehuacan" ~ 21156,
    municipality == "Tehuitzingo" ~ 21157,
    municipality == "Tenampulco" ~ 21158,
    municipality == "Teopantlan" ~ 21159,
    municipality == "Teotlalco" ~ 21160,
    municipality == "Tepanco De Lopez" ~ 21161,
    municipality == "Tepango De Rodriguez" ~ 21162,
    municipality == "Tepatlaxco De Hidalgo" ~ 21163,
    municipality == "Tepeaca" ~ 21164,
    municipality == "Tepemaxalco" ~ 21165,
    municipality == "Tepeojuma" ~ 21166,
    municipality == "Tepetzintla" ~ 21167,
    municipality == "Tepexco" ~ 21168,
    municipality == "Tepexi De Rodriguez" ~ 21169,
    municipality == "Tepeyahualco" ~ 21170,
    municipality == "Tepeyahualco De Cuauhtemoc" ~ 21171,
    municipality == "Tetela De Ocampo" ~ 21172,
    municipality == "Teteles De Avila Castillo" ~ 21173,
    municipality == "Teziutlan" ~ 21174,
    municipality == "Tianguismanalco" ~ 21175,
    municipality == "Tilapa" ~ 21176,
    municipality == "Tlachichuca" ~ 21179,
    municipality == "Tlacotepec De Benito Juarez" ~ 21177,
    municipality == "Tlacuilotepec" ~ 21178,
    municipality == "Tlahuapan" ~ 21180,
    municipality == "Tlaltenango" ~ 21181,
    municipality == "Tlanepantla" ~ 21182,
    municipality == "Tlaola" ~ 21183,
    municipality == "Tlapacoya" ~ 21184,
    municipality == "Tlapanala" ~ 21185,
    municipality == "Tlatlauquitepec" ~ 21186,
    municipality == "Tlaxco" ~ 21187,
    municipality == "Tochimilco" ~ 21188,
    municipality == "Tochtepec" ~ 21189,
    municipality == "Totoltepec De Guerrero" ~ 21190,
    municipality == "Tulcingo" ~ 21191,
    municipality == "Tuzamapan De Galeana" ~ 21192,
    municipality == "Tzicatlacoyan" ~ 21193,
    municipality == "Venustiano Carranza" ~ 21194,
    municipality == "Vicente Guerrero" ~ 21195,
    municipality == "Xayacatlan De Bravo" ~ 21196,
    municipality == "Xicotepec" ~ 21197,
    municipality == "Xicotlan" ~ 21198,
    municipality == "Xiutetelco" ~ 21199,
    municipality == "Xochiapulco" ~ 21200,
    municipality == "Xochiltepec" ~ 21201,
    municipality == "Xochitlan De Vicente Suarez" ~ 21202,
    municipality == "Xochitlan Todos Santos" ~ 21203,
    municipality == "Yaonahuac" ~ 21204,
    municipality == "Yehualtepec" ~ 21205,
    municipality == "Zacapala" ~ 21206,
    municipality == "Zacapoaxtla" ~ 21207,
    municipality == "Zacatlan" ~ 21208,
    municipality == "Zapotitlan" ~ 21209,
    municipality == "Zapotitlan De Mendez" ~ 21210,
    municipality == "Zaragoza" ~ 21211,
    municipality == "Zautla" ~ 21212,
    municipality == "Zihuateutla" ~ 21213,
    municipality == "Zinacatepec" ~ 21214,
    municipality == "Zongozotla" ~ 21215,
    municipality == "Zoquiapan" ~ 21216,
    municipality == "Zoquitlan" ~ 21217,
    TRUE ~ uniqueid  # Keep existing uniqueid where no match
  ))

# Calculate the 'valid' column by summing across the specified columns
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN_PRD_PANAL_CPP, PRI_PVEM, PT, PC, PSI, PAN_PRD_PANAL_CPP_PC,
                                  PAN_PRD_PANAL_CPP_PC_PSI, PAN_PRD_PANAL_CPP_PSI, PAN_PRD_PANAL_CPP_PT, 
                                  PAN_PRD_PANAL_CPP_PT_PSI)), na.rm = TRUE))

# Add 'year' and 'month' columns
data_2013 <- collapsed_data %>%
  mutate(year = 2013, month = "July")

# Sort by 'section' (optional in R, as this might not be needed before saving)
data_2013 <- data_2013 %>%
  arrange(section)

# Import Excel data
puebla_data <- read_excel("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/FINAL_Ayu_Ext_2014.xlsx", sheet = "2014", col_names = TRUE)

# Replace 'uniqueid' based on 'municipality' values
puebla_data <- puebla_data %>%
  mutate(
    uniqueid = case_when(
      municipality == "Acajete" ~ 21001,
      municipality == "Cuapiaxtla de Madero" ~ 21038,
      TRUE ~ uniqueid
    ),
    municipality = case_when(
      municipality == "Acajete" ~ "ACAJETE EXTRAORDINARIO",
      municipality == "Cuapiaxtla de Madero" ~ "CUAPIAXTLA DE MADERO EXTRAORDINARIO",
      TRUE ~ municipality
    )
  )

# Collapse (sum) over the specified columns by 'municipality', 'section', 'uniqueid'
collapsed_data <- puebla_data %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(c(listanominal, total, MC:PRI_PVEM), sum, na.rm = TRUE))

# Create 'valid' variable as the row total
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PRI_PVEM, PAN_PRD_PANAL_CP_PSI, MC)), na.rm = TRUE))

# Create turnout variables
collapsed_data <- collapsed_data %>%
  mutate(turnout = valid / listanominal)

# Add year, month, and state columns
data_2014 <- collapsed_data %>%
  mutate(year = 2014, month = "July", STATE = "PUEBLA")

# Sort by 'uniqueid'
data_2014 <- data_2014 %>%
  arrange(uniqueid)

# Import the Excel file
puebla_2018 <- read_excel("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/Ayuntamientos_2018.xlsx", sheet = "Ayuntamientos")
names(puebla_2018)
# Apply proper case to the municipality names
puebla_2018 <- puebla_2018 %>%
  mutate(municipality = str_to_title(municipality))  # equivalent of proper()

# Drop 'uniqueid' if it exists in the data
puebla_2018 <- puebla_2018 %>%
  select(-uniqueid)

# Initialize the 'uniqueid' column and assign values based on 'municipality'
puebla_2018 <- puebla_2018 %>%
  mutate(
    uniqueid = case_when(
      municipality == "Acajete" ~ 21001,
      municipality == "Acateno" ~ 21002,
      municipality == "Acatlan" ~ 21003,
      municipality == "Acatzingo" ~ 21004,
      municipality == "Acteopan" ~ 21005,
      municipality == "Ahuacatlan" ~ 21006,
      municipality == "Ahuatlan" ~ 21007,
      municipality == "Ahuazotepec" ~ 21008,
      municipality == "Ahuehuetitla" ~ 21009,
      municipality == "Ajalpan" ~ 21010,
      municipality == "Albino Zertuche" ~ 21011,
      municipality == "Aljojuca" ~ 21012,
      municipality == "Altepexi" ~ 21013,
      municipality == "Amixtlan" ~ 21014,
      municipality == "Amozoc" ~ 21015,
      municipality == "Aquixtla" ~ 21016,
      municipality == "Atempan" ~ 21017,
      municipality == "Atexcal" ~ 21018,
      municipality == "Atlequizayan" ~ 21080,
      municipality == "Atlixco" ~ 21019,
      municipality == "Atoyatempan" ~ 21020,
      municipality == "Atzala" ~ 21021,
      municipality == "Atzitzihuacan" ~ 21022,
      municipality == "Atzitzintla" ~ 21023,
      municipality == "Axutla" ~ 21024,
      municipality == "Ayotoxco De Guerrero" ~ 21025,
      municipality == "Calpan" ~ 21026,
      municipality == "Caltepec" ~ 21027,
      municipality == "Camocuautla" ~ 21028,
      municipality == "CaÑAda Morelos" ~ 21099,
      municipality == "Caxhuacan" ~ 21029,
      municipality == "Chalchicomula De Sesma" ~ 21045,
      municipality == "Chapulco" ~ 21046,
      municipality == "Chiautla" ~ 21047,
      municipality == "Chiautzingo" ~ 21048,
      municipality == "Chichiquila" ~ 21050,
      municipality == "Chiconcuautla" ~ 21049,
      municipality == "Chietla" ~ 21051,
      municipality == "Chigmecatitlan" ~ 21052,
      municipality == "Chignahuapan" ~ 21053,
      municipality == "Chignautla" ~ 21054,
      municipality == "Chila" ~ 21055,
      municipality == "Chila De La Sal" ~ 21056,
      municipality == "Chilchotla" ~ 21058,
      municipality == "Chinantla" ~ 21059,
      municipality == "Coatepec" ~ 21030,
      municipality == "Coatzingo" ~ 21031,
      municipality == "Cohetzala" ~ 21032,
      municipality == "Cohuecan" ~ 21033,
      municipality == "Coronango" ~ 21034,
      municipality == "Coxcatlan" ~ 21035,
      municipality == "Coyomeapan" ~ 21036,
      municipality == "Coyotepec" ~ 21037,
      municipality == "Cuapiaxtla De Madero" ~ 21038,
      municipality == "Cuautempan" ~ 21039,
      municipality == "Cuautinchan" ~ 21040,
      municipality == "Cuautlancingo" ~ 21041,
      municipality == "Cuayuca De Andrade" ~ 21042,
      municipality == "Cuetzalan Del Progreso" ~ 21043,
      municipality == "Cuyoaco" ~ 21044,
      municipality == "Domingo Arenas" ~ 21060,
      municipality == "Eloxochitlan" ~ 21061,
      municipality == "Epatlan" ~ 21062,
      municipality == "Esperanza" ~ 21063,
      municipality == "Francisco Z. Mena" ~ 21064,
      municipality == "General Felipe Angeles" ~ 21065,
      municipality == "Guadalupe" ~ 21066,
      municipality == "Guadalupe Victoria" ~ 21067,
      municipality == "Hermenegildo Galeana" ~ 21068,
      municipality == "Honey" ~ 21057,
      municipality == "Huaquechula" ~ 21069,
      municipality == "Huatlatlauca" ~ 21070,
      municipality == "Huauchinango" ~ 21071,
      municipality == "Huehuetla" ~ 21072,
      municipality == "Huehuetlan El Chico" ~ 21073,
      municipality == "Huehuetlan El Grande" ~ 21150,
      municipality == "Huejotzingo" ~ 21074,
      municipality == "Hueyapan" ~ 21075,
      municipality == "Hueytamalco" ~ 21076,
      municipality == "Hueytlalpan" ~ 21077,
      municipality == "Huitzilan De Serdan" ~ 21078,
      municipality == "Huitziltepec" ~ 21079,
      municipality == "Ixcamilpa De Guerrero" ~ 21081,
      municipality == "Ixcaquixtla" ~ 21082,
      municipality == "Ixtacamaxtitlan" ~ 21083,
      municipality == "Ixtepec" ~ 21084,
      municipality == "Izucar De Matamoros" ~ 21085,
      municipality == "Jalpan" ~ 21086,
      municipality == "Jolalpan" ~ 21087,
      municipality == "Jonotla" ~ 21088,
      municipality == "Jopala" ~ 21089,
      municipality == "Juan C. Bonilla" ~ 21090,
      municipality == "Juan Galindo" ~ 21091,
      municipality == "Juan N. Mendez" ~ 21092,
      municipality == "La Magdalena Tlatlauquitepec" ~ 21095,
      municipality == "Lafragua" ~ 21093,
      municipality == "Libres" ~ 21094,
      municipality == "Los Reyes De Juarez" ~ 21118,
      municipality == "Mazapiltepec De Juarez" ~ 21096,
      municipality == "Mixtla" ~ 21097,
      municipality == "Molcaxac" ~ 21098,
      municipality == "Naupan" ~ 21100,
      municipality == "Nauzontla" ~ 21101,
      municipality == "Nealtican" ~ 21102,
      municipality == "Nicolas Bravo" ~ 21103,
      municipality == "Nopalucan" ~ 21104,
      municipality == "Ocotepec" ~ 21105,
      municipality == "Ocoyucan" ~ 21106,
      municipality == "Olintla" ~ 21107,
      municipality == "Oriental" ~ 21108,
      municipality == "Pahuatlan" ~ 21109,
      municipality == "Palmar De Bravo" ~ 21110,
      municipality == "Pantepec" ~ 21111,
      municipality == "Petlalcingo" ~ 21112,
      municipality == "Piaxtla" ~ 21113,
      municipality == "Puebla" ~ 21114,
      municipality == "Quecholac" ~ 21115,
      municipality == "Quimixtlan" ~ 21116,
      municipality == "Rafael Lara Grajales" ~ 21117,
      municipality == "San Andres Cholula" ~ 21119,
      municipality == "San Antonio CaÑada" ~ 21120,
      municipality == "San Diego La Mesa Tochimiltzingo" ~ 21121,
      municipality == "San Felipe Teotlalcingo" ~ 21122,
      municipality == "San Felipe Tepatlan" ~ 21123,
      municipality == "San Gabriel Chilac" ~ 21124,
      municipality == "San Gregorio Atzompa" ~ 21125,
      municipality == "San Jeronimo Tecuanipan" ~ 21126,
      municipality == "San Jeronimo Xayacatlan" ~ 21127,
      municipality == "San Jose Chiapa" ~ 21128,
      municipality == "San Jose Miahuatlan" ~ 21129,
      municipality == "San Juan Atenco" ~ 21130,
      municipality == "San Juan Atzompa" ~ 21131,
      municipality == "San Martin Texmelucan" ~ 21132,
      municipality == "San Martin Totoltepec" ~ 21133,
      municipality == "San Matias Tlalancaleca" ~ 21134,
      municipality == "San Miguel Ixitlan" ~ 21135,
      municipality == "San Miguel Xoxtla" ~ 21136,
      municipality == "San Nicolas Buenos Aires" ~ 21137,
      municipality == "San Nicolas De Los Ranchos" ~ 21138,
      municipality == "San Pablo Anicano" ~ 21139,
      municipality == "San Pedro Cholula" ~ 21140,
      municipality == "San Pedro Yeloixtlahuaca" ~ 21141,
      municipality == "San Salvador El Seco" ~ 21142,
      municipality == "San Salvador El Verde" ~ 21143,
      municipality == "San Salvador Huixcolotla" ~ 21144,
      municipality == "San Sebastian Tlacotepec" ~ 21145,
      municipality == "Santa Catarina Tlaltempan" ~ 21146,
      municipality == "Santa Ines Ahuatempan" ~ 21147,
      municipality == "Santa Isabel Cholula" ~ 21148,
      municipality == "Santiago Miahuatlan" ~ 21149,
      municipality == "Santo Tomas Hueyotlipan" ~ 21151,
      municipality == "Soltepec" ~ 21152,
      municipality == "Tecali De Herrera" ~ 21153,
      municipality == "Tecamachalco" ~ 21154,
      municipality == "Tecomatlan" ~ 21155,
      municipality == "Tehuacan" ~ 21156,
      municipality == "Tehuitzingo" ~ 21157,
      municipality == "Tenampulco" ~ 21158,
      municipality == "Teopantlan" ~ 21159,
      municipality == "Teotlalco" ~ 21160,
      municipality == "Tepanco De Lopez" ~ 21161,
      municipality == "Tepango De Rodriguez" ~ 21162,
      municipality == "Tepatlaxco De Hidalgo" ~ 21163,
      municipality == "Tepeaca" ~ 21164,
      municipality == "Tepemaxalco" ~ 21165,
      municipality == "Tepeojuma" ~ 21166,
      municipality == "Tepetzintla" ~ 21167,
      municipality == "Tepexco" ~ 21168,
      municipality == "Tepexi De Rodriguez" ~ 21169,
      municipality == "Tepeyahualco" ~ 21170,
      municipality == "Tepeyahualco De Cuauhtemoc" ~ 21171,
      municipality == "Tetela De Ocampo" ~ 21172,
      municipality == "Teteles De Avila Castillo" ~ 21173,
      municipality == "Teziutlan" ~ 21174,
      municipality == "Tianguismanalco" ~ 21175,
      municipality == "Tilapa" ~ 21176,
      municipality == "Tlachichuca" ~ 21179,
      municipality == "Tlacotepec De Benito Juarez" ~ 21177,
      municipality == "Tlacuilotepec" ~ 21178,
      municipality == "Tlahuapan" ~ 21180,
      municipality == "Tlaltenango" ~ 21181,
      municipality == "Tlanepantla" ~ 21182,
      municipality == "Tlaola" ~ 21183,
      municipality == "Tlapacoya" ~ 21184,
      municipality == "Tlapanala" ~ 21185,
      municipality == "Tlatlauquitepec" ~ 21186,
      municipality == "Tlaxco" ~ 21187,
      municipality == "Tochimilco" ~ 21188,
      municipality == "Tochtepec" ~ 21189,
      municipality == "Totoltepec De Guerrero" ~ 21190,
      municipality == "Tulcingo" ~ 21191,
      municipality == "Tuzamapan De Galeana" ~ 21192,
      municipality == "Tzicatlacoyan" ~ 21193,
      municipality == "Venustiano Carranza" ~ 21194,
      municipality == "Vicente Guerrero" ~ 21195,
      municipality == "Xayacatlan De Bravo" ~ 21196,
      municipality == "Xicotepec" ~ 21197,
      municipality == "Xicotlan" ~ 21198,
      municipality == "Xiutetelco" ~ 21199,
      municipality == "Xochiapulco" ~ 21200,
      municipality == "Xochiltepec" ~ 21201,
      municipality == "Xochitlan De Vicente Suarez" ~ 21202,
      municipality == "Xochitlan Todos Santos" ~ 21203,
      municipality == "Yaonahuac" ~ 21204,
      municipality == "Yehualtepec" ~ 21205,
      municipality == "Zacapala" ~ 21206,
      municipality == "Zacapoaxtla" ~ 21207,
      municipality == "Zacatlan" ~ 21208,
      municipality == "Zapotitlan" ~ 21209,
      municipality == "Zapotitlan De Mendez" ~ 21210,
      municipality == "Zaragoza" ~ 21211,
      municipality == "Zautla" ~ 21212,
      municipality == "Zihuateutla" ~ 21213,
      municipality == "Zinacatepec" ~ 21214,
      municipality == "Zongozotla" ~ 21215,
      municipality == "Zoquiapan" ~ 21216,
      municipality == "Zoquitlan" ~ 21217,
      TRUE ~ 0  # Default value if no match is found
    )
  )

# Convert the municipality column to uppercase
puebla_2018 <- puebla_2018 %>%
  mutate(municipality = toupper(municipality))

# Preserve and import the Excel file with coalitions
# Save it as a temporary file to later merge
coalitions <- read_excel("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/puebla_coalitions_2018.xlsx", sheet = "Sheet1")

# Replace missing values (NA) in cc_* columns with 0
puebla_2018 <- puebla_2018 %>%
  mutate(across(starts_with("cc_"), ~ replace_na(., 0)))

# Rename columns
puebla_2018 <- puebla_2018 %>%
  rename(
    PANAL = NA,
    PRI_PANAL = PRI_NA,
    PANAL_PCPP = NA_PCPP,
    CI_1 = CANDIDATURAINDEPENDIENTE,
    CI_2 = CANDIDATURAINDEPENDIENTE2,
    total = TOTAL
  )

# Collapse (sum) by municipality, uniqueid, and section
collapsed_data <- puebla_2018 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PAN:CI_2, total), sum, na.rm = TRUE), .groups = "drop")

# Save the collapsed data
saveRDS(collapsed_data, file = "collapsed_puebla_2018.rds")

# Generating composite variables and handling conditions

collapsed_data <- collapsed_data %>%
  mutate(
    PAN_PRD_MC_PCPP_PSI2 = rowSums(select(., PAN, PRD, MC, PCPP, PSI, PAN_PRD_MC_PCPP_PSI, PAN_PRD_MC_PCPP, PAN_PRD_MC_PSI, PAN_PRD_PCPP_PSI,
                                          PAN_MC_PCPP_PSI, PRD_MC_PCPP_PSI, PAN_PRD_MC, PAN_PRD_PCPP, PAN_PRD_PSI, PAN_MC_PCPP, PAN_MC_PSI,
                                          PAN_PCPP_PSI, PRD_MC_PCPP, PRD_MC_PSI, PRD_PCPP_PSI, MC_PCPP_PSI, PAN_PRD, PAN_MC, PAN_PCPP,
                                          PAN_PSI, PRD_MC, PRD_PCPP, PRD_PSI, MC_PCPP, MC_PSI, PCPP_PSI), na.rm = TRUE),
    PAN_PRD_MC_PSI2 = rowSums(select(., PAN, PRD, MC, PSI, PAN_PRD_MC_PSI, PAN_PRD_MC, PAN_PRD_PSI, PAN_MC_PSI, PRD_MC_PSI, PAN_PRD, PAN_MC, PAN_PSI, PRD_MC, PRD_PSI, MC_PSI), na.rm = TRUE),
    PAN_PRD_PCPP_PSI2 = rowSums(select(., PAN, PRD, PCPP, PSI, PAN_PRD_PCPP_PSI, PAN_PRD_PCPP, PAN_PRD_PSI, PAN_PCPP_PSI, PRD_PCPP_PSI, PAN_PRD, PAN_PCPP, PAN_PSI, PRD_PCPP, PRD_PSI, PCPP_PSI), na.rm = TRUE),
    PAN_PRD_MC_PCPP2 = rowSums(select(., PAN, PRD, MC, PCPP, PAN_PRD_MC_PCPP, PAN_PRD_MC, PAN_PRD_PCPP, PAN_MC_PCPP, PRD_MC_PCPP, PAN_PRD, PAN_MC, PAN_PCPP, PRD_MC, PRD_PCPP, MC_PCPP), na.rm = TRUE),
    PAN_MC_PCPP_PSI2 = rowSums(select(., PAN, MC, PCPP, PSI, PAN_MC_PCPP_PSI, PAN_MC_PCPP, PAN_MC_PSI, PAN_PCPP_PSI, MC_PCPP_PSI, PAN_MC, PAN_PCPP, PAN_PSI, MC_PCPP, MC_PSI, PCPP_PSI), na.rm = TRUE),
    PAN_PRD_MC2 = rowSums(select(., PAN, PRD, MC, PAN_PRD_MC, PAN_PRD, PAN_MC, PRD_MC), na.rm = TRUE),
    PAN_PCPP_PSI2 = rowSums(select(., PAN, PCPP, PSI, PAN_PCPP_PSI, PAN_PCPP, PAN_PSI, PCPP_PSI), na.rm = TRUE),
    PAN_MC_PSI2 = rowSums(select(., PAN, MC, PSI, PAN_MC_PSI, PAN_MC, PAN_PSI, MC_PSI), na.rm = TRUE),
    PAN_MC_PCPP2 = rowSums(select(., PAN, MC, PCPP, PAN_MC_PCPP, PAN_MC, PAN_PCPP, MC_PCPP), na.rm = TRUE),
    PAN_PRD_PSI2 = rowSums(select(., PAN, PRD, PSI, PAN_PRD_PSI, PAN_PRD, PAN_PSI, PRD_PSI), na.rm = TRUE),
    PAN_PRD_PCPP2 = rowSums(select(., PAN, PRD, PCPP, PAN_PRD_PCPP, PAN_PRD, PAN_PCPP, PRD_PCPP), na.rm = TRUE),
    PAN_PRD2 = rowSums(select(., PAN, PRD, PAN_PRD), na.rm = TRUE),
    PAN_PSI2 = rowSums(select(., PAN, PSI, PAN_PSI), na.rm = TRUE),
    PAN_MC2 = rowSums(select(., PAN, MC, PAN_MC), na.rm = TRUE),
    PAN_PCPP2 = rowSums(select(., PAN, PCPP, PAN_PCPP), na.rm = TRUE),
    PRD_MC_PSI2 = rowSums(select(., PRD, MC, PSI, PRD_MC_PSI, PRD_MC, PRD_PSI, MC_PSI), na.rm = TRUE),
    PRD_MC_PCPP2 = rowSums(select(., PRD, MC, PCPP, PRD_MC_PCPP, PRD_MC, PRD_PCPP, MC_PCPP), na.rm = TRUE),
    PRD_PCPP_PSI2 = rowSums(select(., PRD, PCPP, PSI, PRD_PCPP_PSI, PRD_PCPP, PRD_PSI, PCPP_PSI), na.rm = TRUE),
    PRD_MC2 = rowSums(select(., PRD, MC, PRD_MC), na.rm = TRUE),
    PRD_PCPP2 = rowSums(select(., PRD, PCPP, PRD_PCPP), na.rm = TRUE),
    PRD_PSI2 = rowSums(select(., PRD, PSI, PRD_PSI), na.rm = TRUE),
    MC_PCPP_PSI2 = rowSums(select(., MC, PCPP, PSI, MC_PCPP_PSI, MC_PCPP, MC_PSI, PCPP_PSI), na.rm = TRUE),
    MC_PCPP2 = rowSums(select(., MC, PCPP, MC_PCPP), na.rm = TRUE),
    MC_PSI2 = rowSums(select(., MC, PSI, MC_PSI), na.rm = TRUE),
    PANAL_PCPP2 = rowSums(select(., PANAL, PCPP, PANAL_PCPP), na.rm = TRUE),
    PVEM_PCPP_PSI2 = rowSums(select(., PVEM, PCPP, PSI, PVEM_PCPP_PSI, PVEM_PCPP, PVEM_PSI, PCPP_PSI), na.rm = TRUE),
    PVEM_PSI2 = rowSums(select(., PVEM, PSI, PVEM_PSI), na.rm = TRUE),
    PVEM_PCPP2 = rowSums(select(., PVEM, PCPP, PVEM_PCPP), na.rm = TRUE),
    PCPP_PSI2 = rowSums(select(., PCPP, PSI, PCPP_PSI), na.rm = TRUE),
    PRI_PANAL2 = rowSums(select(., PRI, PANAL, PRI_PANAL), na.rm = TRUE)
  )

# Replace values with NA for corresponding conditions (if cc_* == 1)
# You can adjust this for every specific group of conditions like cc_PAN_PRD_MC_PCPP_PSI == 1 etc.
collapsed_data <- collapsed_data %>%
  mutate(across(c(PAN, PRD, MC, PCPP, PSI, PAN_PRD_MC_PCPP_PSI, PAN_PRD_MC_PCPP, PAN_PRD_MC_PSI, PAN_PRD_PCPP_PSI,
                  PAN_MC_PCPP_PSI, PRD_MC_PCPP_PSI, PAN_PRD_MC, PAN_PRD_PCPP, PAN_PRD_PSI, PAN_MC_PCPP, PAN_MC_PSI,
                  PAN_PCPP_PSI, PRD_MC_PCPP, PRD_MC_PSI, PRD_PCPP_PSI, MC_PCPP_PSI, PAN_PRD, PAN_MC, PAN_PCPP,
                  PAN_PSI, PRD_MC, PRD_PCPP, PRD_PSI, MC_PCPP, MC_PSI, PCPP_PSI), ~if_else(cc_PAN_PRD_MC_PCPP_PSI == 1, NA, .)))

# Drop unwanted variables and rename 
collapsed_data <- collapsed_data %>%
  select(-c(PAN_PRD_MC_PCPP_PSI, PAN_PRD_MC_PSI, PAN_PRD_PCPP_PSI, PAN_PRD_MC_PCPP, PAN_MC_PCPP_PSI, PRD_MC_PCPP_PSI, ...)) %>%
  rename_with(~str_replace(., "2", ""), contains("2"))

# Replace values for PT_MORENA_PES
collapsed_data <- collapsed_data %>%
  mutate(PT_MORENA_PES = PT_MORENA_PES + rowSums(select(., PT_MORENA, PT_PES, MORENA_PES, PT, MORENA, PES), na.rm = TRUE))

# Drop unwanted variables
collapsed_data <- collapsed_data %>%
  select(-c(PT, MORENA, PES, PT_MORENA, PT_PES, MORENA_PES, PRD_MC_PCPP_PSI))

# Generate new columns based on row sums
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PVEM, MC, PANAL, PCPP, PSI, PT_MORENA_PES, PAN_PRD_MC_PCPP_PSI,
                                PAN_PRD_MC_PSI, PAN_PRD_PCPP_PSI, PAN_PRD_MC_PCPP, PAN_MC_PCPP_PSI, PAN_PRD_MC,
                                PAN_PCPP_PSI, PAN_MC_PSI, PAN_MC_PCPP, PAN_PRD_PSI, PAN_PRD_PCPP, PAN_PRD, PAN_PSI,
                                PAN_MC, PAN_PCPP, PRD_MC_PSI, PRD_MC_PCPP, PRD_PCPP_PSI, PRD_MC, PRD_PCPP, PRD_PSI,
                                MC_PCPP_PSI, MC_PCPP, MC_PSI, PANAL_PCPP, PVEM_PCPP_PSI, PVEM_PSI, PVEM_PCPP, PCPP_PSI, PRI_PANAL, CI_1, CI_2), na.rm = TRUE))

# Add year, month, STATE, and incumbent_vote columns
puebla_data_2018 <- puebla_data %>%
  mutate(
    year = 2018,
    month = "July",
    STATE = "PUEBLA",
  )

# Drop all columns starting with 'cc'
puebla_data_2018 <- puebla_data_2018 %>% select(-starts_with("cc"))

# Load the Listado Nominal PREP 2018 data
prep_data <- readRDS("../Listas Nominales/ListadoNominalPREP2018.rds")  # Adjust to the correct file path

# Filter for PUEBLA state
prep_data <- prep_data %>% filter(STATE == "PUEBLA")


# Merge puebla_data with prep_data using "section" column
puebla_data_2018 <- puebla_data_2018 %>%
  left_join(prep_data, by = "section") %>%
  filter(!is.na(ListadoNominalINE))  # Drop rows where no match was found 

# Drop the temporary saved data
unlink("PUE_LN18.rds")

# Rename the column `ListadoNominalINE` to `listanominal`
puebla_data_2018 <- puebla_data_2018 %>%
  rename(listanominal = ListadoNominalINE)

# Calculate turnout at the precinct and municipal levels
puebla_data_2018 <- puebla_data_2018 %>%
  mutate(turnout = total / listanominal)

# Sort by uniqueid
data_2018 <- puebla_data_2018 %>%
  arrange(uniqueid)

# Import CSV files and save them as individual RDS files
municipalities <- c("AHUAZOTEPEC", "CAÑADA MORELOS", "MAZAPILTEPEC DE JUAREZ", "OCOYUCAN", "TEPEOJUMA")
for (x in municipalities) {
  file_path <- paste0("../../../Data/Raw Electoral Data/Puebla - 1998, 2001, 2004, 2007, 2010,  2013,2018/20190609_0800_CW_PRESIDENTE_MUNICIPAL/PRESIDENTE_MUNICIPAL_", x, "_2019.csv")
  data <- read_csv(file_path, col_names = TRUE)
  saveRDS(data, paste0(x, ".rds"))
}

# Append the individual RDS files except for TEPEOJUMA
append_data <- NULL
for (x in municipalities[-length(municipalities)]) {
  data <- readRDS(paste0(x, ".rds"))
  append_data <- bind_rows(append_data, data)
  file.remove(paste0(x, ".rds")) # Erase the file after appending
}


# Continue with the necessary manipulations
append_data <- append_data %>%
  rename(section = seccion) %>%
  filter(!is.na(section)) %>%
  rename(listanominal = lista_nominal_casilla,
         total = total_votos_calculados)

# Conditional recoding based on municipality
append_data <- append_data %>%
  mutate(
    PT_PVEM_MORENA_PES = if_else(municipio %in% c("AHUAZOTEPEC", "CAÑADA MORELOS", "OCOYUCAN"),
                                 pt_pvem_morena_pes + pt_pvem_morena + pt_pvem_pes + pt_morena_pes + 
                                   pvem_morena_pes + pt_pvem + pt_morena + pt_pes + pvem_morena + 
                                   pvem_pes + morena_pes + pvem + pt + morena, NA_real_),
    PAN_CPP = if_else(municipio == "AHUAZOTEPEC", pan + cpp + pan_cpp, NA_real_),
    PAN_PRD_MC_CPP = if_else(municipio == "CAÑADA MORELOS", pan_prd_mc_cpp + pan_prd_mc + pan_prd_cpp + 
                               pan_mc_cpp + prd_mc_cpp + pan_prd + pan_mc + pan_cpp + prd_mc + 
                               prd_cpp + mc_cpp + pan + prd + mc + cpp, NA_real_),
    PRI_CPP = if_else(municipio == "MAZAPILTEPEC DE JUAREZ", pri + cpp + pri_cpp, NA_real_),
    PRD_MC_CPP = if_else(municipio == "OCOYUCAN", prd_mc_cpp + prd_mc + prd_cpp + mc_cpp + 
                           prd + mc + cpp, NA_real_),
    PT_MORENA_PES = if_else(municipio == "TEPEOJUMA", pt_morena_pes + pt_morena + pt_pes + 
                              morena_pes + pt + morena + pes, NA_real_)
  )

# Replace specific values to NA
cols_to_na <- c("pt_pvem_morena_pes", "pt_pvem_morena", "pt_pvem_pes", "pt_morena_pes", 
                "pvem_morena_pes", "pt_pvem", "pt_morena", "pt_pes", "pvem_morena", 
                "pvem_pes", "morena_pes", "pes", "pvem", "pt", "morena", "pan", "cpp", 
                "pan_cpp")
append_data <- append_data %>%
  mutate(across(all_of(cols_to_na), ~ if_else(municipio == "AHUAZOTEPEC", NA_real_, .)))

cols_to_na_morelos <- c("pan_prd_mc_cpp", "pan_prd_mc", "pan_prd_cpp", "pan_mc_cpp", 
                        "prd_mc_cpp", "pan_prd", "pan_mc", "pan_cpp", "prd_mc", 
                        "prd_cpp", "mc_cpp", "pan", "prd", "mc", "cpp")
append_data <- append_data %>%
  mutate(across(all_of(cols_to_na_morelos), ~ if_else(municipio == "CAÑADA MORELOS", NA_real_, .)))

cols_to_na_mazapiltepec <- c("pri", "cpp", "pri_cpp")
append_data <- append_data %>%
  mutate(across(all_of(cols_to_na_mazapiltepec), ~ if_else(municipio == "MAZAPILTEPEC DE JUAREZ", NA_real_, .)))

cols_to_na_ocoyucan <- c("pt_pvem_morena_pes", "pt_pvem_morena", "pt_pvem_pes", "pt_morena_pes", 
                         "pvem_morena_pes", "pt_pvem", "pt_morena", "pt_pes", "pvem_morena", 
                         "pvem_pes", "morena_pes", "pes", "pvem", "pt", "morena", 
                         "prd_mc_cpp", "prd_mc", "prd_cpp", "mc_cpp", "prd", "mc", "cpp")
append_data <- append_data %>%
  mutate(across(all_of(cols_to_na_ocoyucan), ~ if_else(municipio == "OCOYUCAN", NA_real_, .)))

cols_to_na_tepeojuma <- c("pt_morena_pes", "pt_morena", "pt_pes", "morena_pes", "pt", "morena", "pes")
append_data <- append_data %>%
  mutate(across(all_of(cols_to_na_tepeojuma), ~ if_else(municipio == "TEPEOJUMA", NA_real_, .)))

# Clean up remaining columns
append_data <- append_data %>%
  select(-matches("^cc_"))

# Rename columns
append_data <- append_data %>%
  rename(PAN = pan, PRI = pri, PT = pt, PVEM = pvem, PSI = psi, NAP = nap, MC = mc, MORENA = morena, PES = pes)

# Add uniqueid based on municipality
append_data <- append_data %>%
  mutate(uniqueid = case_when(
    municipio == "AHUAZOTEPEC" ~ 21008,
    municipio == "CAÑADA MORELOS" ~ 21099,
    municipio == "MAZAPILTEPEC DE JUAREZ" ~ 21096,
    municipio == "OCOYUCAN" ~ 21106,
    municipio == "TEPEOJUMA" ~ 21166
  ),
  municipality = paste0(municipio, " EXTRAORDINARIO"))

# Collapse by municipality, uniqueid, and section
append_data_collapsed <- append_data %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PRI:PES, PAN:PT_MORENA_PES, total, listanominal), sum, na.rm = TRUE)) %>%
  ungroup()

# Calculate turnout
append_data_collapsed <- append_data_collapsed %>%
  mutate(turnout = total / listanominal)

# Generate the 'valid' variable (rowtotal equivalent)
append_data_collapsed <- append_data_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PRI:PT_MORENA_PES), na.rm = TRUE))

# Add year and month
data_2019 <- append_data_collapsed %>%
  mutate(year = 2019, month = "June")

# Append the data
puebla_all <- bind_rows(data_2014, data_2018, data_2019)

puebla_combined <- bind_rows(puebla_all_data, puebla_all)

# Replace `municipality` values with uppercase
puebla_combined <- puebla_combined %>%
  mutate(municipality = str_to_upper(municipality))

data.table::fwrite(puebla_combined,"../../../Processed Data/puebla/puebla_process_raw_data.csv")