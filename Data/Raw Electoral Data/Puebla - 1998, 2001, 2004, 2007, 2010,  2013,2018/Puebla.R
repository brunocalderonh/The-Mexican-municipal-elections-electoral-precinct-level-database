# Load necessary libraries
library(readr)   # For reading CSV files
library(dplyr)   # For data manipulation

# Change the working directory (in R, it's typically set once at the start)
# You can use `setwd()` to change the directory, but R doesn't support a "capture" like Stata. 
# If needed, check both directories manually using:
# setwd("D:/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Puebla - 1998, 2001, 2004, 2007, 2010, 2013")
# setwd("C:/Users/Horacio/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Puebla - 1998, 2001, 2004, 2007, 2010, 2013")

# Read the CSV file
data <- read_csv("Ayu_Seccion_1998_No_LN.csv")

# Rename columns to match Stata code
data <- data %>%
  rename(
    municipality = municipio,
    section = seccin
  )

# Drop rows where both `municipality` is empty and `section` is NA
data <- data %>%
  filter(!(municipality == "" & is.na(section)))

# Drop rows where `total` is NA or 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Convert specific columns from character to numeric (destring in Stata)
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
  select(-noreg, -nulos)

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
external_data <- read_dta("../../all_months_years.dta")

# Merge by 'ed' and 'seccion', keeping 'month', 'year', and 'lista' from the external data
merged_data <- data %>%
  left_join(external_data %>% select(ed, seccion, month, year, lista), by = c("ed", "seccion"))

# Filter rows where 'month' is 9 and 'year' is 1998
filtered_data <- merged_data %>%
  filter(month == 9 & year == 1998)

# Drop unnecessary columns after merging
filtered_data <- filtered_data %>%
  select(-_merge, -ed, -seccion, -year, -month)

# Rename 'lista' to 'listanominal'
filtered_data <- filtered_data %>%
  rename(listanominal = lista)

# Calculate turnout
filtered_data <- filtered_data %>%
  mutate(turnout = total / listanominal)

# Generate municipal-level sums and their inverses for listanominal
filtered_data <- filtered_data %>%
  group_by(uniqueid) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE)) %>%
  mutate(inv_mun_listanominal = 1 / mun_listanominal)

# Calculate municipal turnout
filtered_data <- filtered_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# The resulting `filtered_data` now matches the operations in your Stata code

# Rank the inverse municipal values using frankv() from the data.table package
filtered_data <- filtered_data %>%
  mutate(PAN_r = frankv(inv_mun_PAN, ties.method = "min", na.last = "keep"),
         PRI_r = frankv(inv_mun_PRI, ties.method = "min", na.last = "keep"),
         PRD_r = frankv(inv_mun_PRD, ties.method = "min", na.last = "keep"),
         PT_r = frankv(inv_mun_PT, ties.method = "min", na.last = "keep"),
         PVEM_r = frankv(inv_mun_PVEM, ties.method = "min", na.last = "keep"),
         PCP_r = frankv(inv_mun_PCP, ties.method = "min", na.last = "keep"))

# Drop the inverse municipal columns (inv_mun_*)
filtered_data <- filtered_data %>%
  select(-starts_with("inv_mun_"))

# Determine the winner based on rankings
filtered_data <- filtered_data %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PCP_r == 1 ~ "PCP",
    TRUE ~ winner  # Retain any existing values if no match is found
  ))

# Drop the ranking columns
filtered_data <- filtered_data %>%
  select(-ends_with("_r"))

# Add year and month columns
filtered_data <- filtered_data %>%
  mutate(year = 1998, month = "November")

# Save the dataset to a .dta file
write_dta(filtered_data, "Puebla_Section_1998.dta")

# Read the CSV file
data <- read_csv("Ayu_Seccion_2001.csv")

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

# Convert columns from character to numeric (similar to destring in Stata)
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
external_data <- read_dta("../../all_months_years.dta")

# Keep rows where 'month' is September (9) and 'year' is 2001
external_data <- external_data %>%
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

# The resulting 'filtered_data' is now ready and matches the operations in your Stata code

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

# Generate municipal-level sums and inverses
for (var in c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PSN", "PAS", "total", "listanominal", "valid")) {
  data <- data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(get(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / get(paste0("mun_", var)))
}

# Calculate municipal turnout
data <- data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse municipal values using frankv() from data.table
data <- data %>%
  mutate(PAN_r = frankv(inv_mun_PAN, ties.method = "min", na.last = "keep"),
         PRI_r = frankv(inv_mun_PRI, ties.method = "min", na.last = "keep"),
         PRD_r = frankv(inv_mun_PRD, ties.method = "min", na.last = "keep"),
         PT_r = frankv(inv_mun_PT, ties.method = "min", na.last = "keep"),
         PVEM_r = frankv(inv_mun_PVEM, ties.method = "min", na.last = "keep"),
         PC_r = frankv(inv_mun_PC, ties.method = "min", na.last = "keep"),
         PSN_r = frankv(inv_mun_PSN, ties.method = "min", na.last = "keep"),
         PAS_r = frankv(inv_mun_PAS, ties.method = "min", na.last = "keep"))

# Drop the inverse municipal columns (inv_mun_*)
data <- data %>%
  select(-starts_with("inv_mun_"))

# Determine the winner based on rankings
data <- data %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PSN_r == 1 ~ "PSN",
    PAS_r == 1 ~ "PAS",
    TRUE ~ winner  # Retain any existing values if no match is found
  ))

# Drop the ranking columns
data <- data %>%
  select(-ends_with("_r"))

# Add year and month columns
data <- data %>%
  mutate(year = 2001, month = "November")

# Save the dataset to a .dta file
write_dta(data, "Puebla_Section_2001.dta")

# Import the Excel sheet and specify the range of cells to read
data <- read_excel("Resultados por casilla elección 2002 Molcaxac.xlsx", sheet = "aytos", range = "A8:V16", col_names = TRUE)

# Rename columns
data <- data %>%
  rename(
    section = `sección`,
    PAN = F,
    PRI = G,
    PRD = H,
    PC = K,
    PAS = M,
    listanominal = CiudadanosenListaNominal,
    total = VotaciónTotal
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

# Generate municipal-level sums and their inverses
for (var in c("PAN", "PRI", "PRD", "PC", "PAS", "total", "listanominal", "valid")) {
  collapsed_data <- collapsed_data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(get(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / get(paste0("mun_", var)))
}

# Calculate municipal turnout
collapsed_data <- collapsed_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse municipal values using frankv() from data.table
collapsed_data <- collapsed_data %>%
  mutate(PAN_r = frankv(inv_mun_PAN, ties.method = "min", na.last = "keep"),
         PRI_r = frankv(inv_mun_PRI, ties.method = "min", na.last = "keep"),
         PRD_r = frankv(inv_mun_PRD, ties.method = "min", na.last = "keep"),
         PC_r = frankv(inv_mun_PC, ties.method = "min", na.last = "keep"),
         PAS_r = frankv(inv_mun_PAS, ties.method = "min", na.last = "keep"))

# Drop the inverse municipal columns (inv_mun_*)
collapsed_data <- collapsed_data %>%
  select(-starts_with("inv_mun_"))

# Determine the winner based on rankings
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PC_r == 1 ~ "PC",
    PAS_r == 1 ~ "PAS",
    TRUE ~ winner  # Retain any existing values if no match is found
  ))

# Drop the ranking columns
collapsed_data <- collapsed_data %>%
  select(-ends_with("_r"))

# Add year and month columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2002, month = "June")

# Save the dataset to a .dta file
write_dta(collapsed_data, "Puebla_Section_2002.dta")

# Read the CSV file
data <- read_csv("Ayu_Seccion_2004.csv")

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

# Convert specified columns from character to numeric (similar to destring in Stata)
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

# Generate municipal-level sums and inverses
for (var in c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "total", "listanominal", "valid")) {
  data <- data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(get(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / get(paste0("mun_", var)))
}

# Calculate municipal turnout
data <- data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse municipal values using frankv() from data.table
data <- data %>%
  mutate(PAN_r = frankv(inv_mun_PAN, ties.method = "min", na.last = "keep"),
         PRI_r = frankv(inv_mun_PRI, ties.method = "min", na.last = "keep"),
         PRD_r = frankv(inv_mun_PRD, ties.method = "min", na.last = "keep"),
         PT_r = frankv(inv_mun_PT, ties.method = "min", na.last = "keep"),
         PVEM_r = frankv(inv_mun_PVEM, ties.method = "min", na.last = "keep"),
         PC_r = frankv(inv_mun_PC, ties.method = "min", na.last = "keep"))

# Drop the inverse municipal columns (inv_mun_*)
data <- data %>%
  select(-starts_with("inv_mun_"))

# Determine the winner based on rankings
data <- data %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    TRUE ~ winner  # Retain any existing values if no match is found
  ))

# Drop the ranking columns
data <- data %>%
  select(-ends_with("_r"))

# Add year and month columns
data <- data %>%
  mutate(year = 2004, month = "November")

# Sort the data by 'section'
data <- data %>%
  arrange(section)

# Save the dataset to a .dta file
write_dta(data, "Puebla_Section_2004.dta")

# Import the Excel file with case normalization (convert to lowercase) and keep first row as headers
data <- read_excel("Ayu_Seccion_2007.xlsx", .name_repair = "minimal")

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

# Convert specified columns from character to numeric (similar to destring in Stata)
data <- data %>%
  mutate(across(pan:listanominal, as.numeric))

# Collapse (sum) the data by 'municipality' and 'section'
v <- data %>%
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

# Generate municipal-level sums and inverses
for (var in c("PAN", "PRI_PVEM", "PRD_PC", "PT", "PANAL", "PAS", "PEC", "total", "listanominal", "valid")) {
  collapsed_data <- collapsed_data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(get(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / get(paste0("mun_", var)))
}

# Calculate municipal turnout
collapsed_data <- collapsed_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse municipal values using frankv() from data.table
collapsed_data <- collapsed_data %>%
  mutate(PAN_r = frankv(inv_mun_PAN, ties.method = "min", na.last = "keep"),
         PRI_PVEM_r = frankv(inv_mun_PRI_PVEM, ties.method = "min", na.last = "keep"),
         PRD_PC_r = frankv(inv_mun_PRD_PC, ties.method = "min", na.last = "keep"),
         PT_r = frankv(inv_mun_PT, ties.method = "min", na.last = "keep"),
         PANAL_r = frankv(inv_mun_PANAL, ties.method = "min", na.last = "keep"),
         PAS_r = frankv(inv_mun_PAS, ties.method = "min", na.last = "keep"),
         PEC_r = frankv(inv_mun_PEC, ties.method = "min", na.last = "keep"))

# Drop the inverse municipal columns (inv_mun_*)
collapsed_data <- collapsed_data %>%
  select(-starts_with("inv_mun_"))

# Determine the winner based on rankings
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_PVEM_r == 1 ~ "PRI_PVEM",
    PRD_PC_r == 1 ~ "PRD_PC",
    PT_r == 1 ~ "PT",
    PANAL_r == 1 ~ "PANAL",
    PAS_r == 1 ~ "PAS",
    PEC_r == 1 ~ "PEC",
    TRUE ~ winner  # Retain existing values if no match is found
  ))

# Drop the ranking columns
collapsed_data <- collapsed_data %>%
  select(-ends_with("_r"))

# Add year and month columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2007, month = "November")

# Sort the data by 'section'
collapsed_data <- collapsed_data %>%
  arrange(section)

# Save the dataset to a .dta file
write_dta(collapsed_data, "Puebla_Section_2007.dta")

# Import the Excel sheet with specified cell range
data <- read_excel("ResultadosDeLaElecciónVotaciónXCasilla Aytos extra. 2008.xlsx", sheet = "ayto", range = "A7:R25", col_names = TRUE)

# Convert all columns to numeric (similar to destring in Stata)
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
    PAS = ALTERNATIVASOCIALDEMÓCRATAPAR,
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

# Generate municipal-level sums and inverses
for (var in c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PANAL", "PAS", "total", "listanominal", "valid")) {
  collapsed_data <- collapsed_data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(get(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / get(paste0("mun_", var)))
}

# Calculate municipal turnout
collapsed_data <- collapsed_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse municipal values using frankv() from data.table
collapsed_data <- collapsed_data %>%
  mutate(PAN_r = frankv(inv_mun_PAN, ties.method = "min", na.last = "keep"),
         PRI_r = frankv(inv_mun_PRI, ties.method = "min", na.last = "keep"),
         PRD_r = frankv(inv_mun_PRD, ties.method = "min", na.last = "keep"),
         PT_r = frankv(inv_mun_PT, ties.method = "min", na.last = "keep"),
         PVEM_r = frankv(inv_mun_PVEM, ties.method = "min", na.last = "keep"),
         PC_r = frankv(inv_mun_PC, ties.method = "min", na.last = "keep"),
         PANAL_r = frankv(inv_mun_PANAL, ties.method = "min", na.last = "keep"),
         PAS_r = frankv(inv_mun_PAS, ties.method = "min", na.last = "keep"))

# Drop the inverse municipal columns (inv_mun_*)
collapsed_data <- collapsed_data %>%
  select(-starts_with("inv_mun_"))

# Determine the winner based on rankings
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PANAL_r == 1 ~ "PANAL",
    PT_r == 1 ~ "PT",
    PAS_r == 1 ~ "PAS",
    TRUE ~ winner  # Retain any existing values if no match is found
  ))

# Drop the ranking columns
collapsed_data <- collapsed_data %>%
  select(-ends_with("_r"))

# Add year and month columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2008, month = "June")

# Save the dataset to a .dta file
write_dta(collapsed_data, "Puebla_Section_2008.dta")

# Import the Excel file and convert column names to lowercase
data <- read_excel("Ayu_Seccion_2010.xlsx", .name_repair = "minimal")

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

# Convert specified columns from character to numeric (similar to destring in Stata)
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
    municipality = str_to_title(municipality),  # Equivalent to 'proper' in Stata
    municipality = str_trim(municipality)       # Remove leading and trailing spaces
  )

# The resulting 'collapsed_data' matches the operations in your Stata code

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

# Generate municipal-level sums and inverses
for (var in c("PAN_PRD_PC_PANAL", "PRI_PVEM", "PT", "total", "listanominal", "valid")) {
  collapsed_data <- collapsed_data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(get(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / get(paste0("mun_", var)))
}

# Calculate municipal turnout
collapsed_data <- collapsed_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse municipal values using frankv() from data.table
collapsed_data <- collapsed_data %>%
  mutate(PAN_PRD_PC_PANAL_r = frankv(inv_mun_PAN_PRD_PC_PANAL, ties.method = "min", na.last = "keep"),
         PRI_PVEM_r = frankv(inv_mun_PRI_PVEM, ties.method = "min", na.last = "keep"),
         PT_r = frankv(inv_mun_PT, ties.method = "min", na.last = "keep"))

# Drop the inverse municipal columns (inv_mun_*)
collapsed_data <- collapsed_data %>%
  select(-starts_with("inv_mun_"))

# Determine the winner based on rankings
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    PAN_PRD_PC_PANAL_r == 1 ~ "PAN_PRD_PC_PANAL",
    PRI_PVEM_r == 1 ~ "PRI_PVEM",
    PT_r == 1 ~ "PT",
    TRUE ~ winner  # Retain any existing values if no match is found
  ))

# Drop the ranking columns
collapsed_data <- collapsed_data %>%
  select(-ends_with("_r"))

# Add year and month columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2010, month = "July")

# Sort the data by 'section'
collapsed_data <- collapsed_data %>%
  arrange(section)

# Save the dataset to a .dta file
write_dta(collapsed_data, "Puebla_Section_2010.dta")

# Import the Excel file with the specified sheet and cell range
data <- read_excel("Resul_ por casillas e individuales elec extraordinaria 2011.xlsx", 
                   sheet = "2011", range = "A6:K75", col_names = TRUE)

# Rename columns
data <- data %>%
  rename(
    section = SECCION,
    municipality = MUNICIPIO,
    PAN = E,
    PRI_PVEM = F,
    PC = G,
    PANAL = H,
    total = VOTACIONTOTAL
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

# Generate municipal-level sums and inverses
for (var in c("PAN", "PRI_PVEM", "PC", "PANAL", "total", "listanominal", "valid")) {
  collapsed_data <- collapsed_data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(get(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / get(paste0("mun_", var)))
}

# Calculate municipal turnout
collapsed_data <- collapsed_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse municipal values using frankv() from data.table
collapsed_data <- collapsed_data %>%
  mutate(PAN_r = frankv(inv_mun_PAN, ties.method = "min", na.last = "keep"),
         PRI_PVEM_r = frankv(inv_mun_PRI_PVEM, ties.method = "min", na.last = "keep"),
         PC_r = frankv(inv_mun_PC, ties.method = "min", na.last = "keep"),
         PANAL_r = frankv(inv_mun_PANAL, ties.method = "min", na.last = "keep"))

# Drop the inverse municipal columns (inv_mun_*)
collapsed_data <- collapsed_data %>%
  select(-starts_with("inv_mun_"))

# Determine the winner based on rankings
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_PVEM_r == 1 ~ "PRI_PVEM",
    PC_r == 1 ~ "PC",
    PANAL_r == 1 ~ "PANAL",
    TRUE ~ winner  # Retain any existing values if no match is found
  ))

# Drop the ranking columns
collapsed_data <- collapsed_data %>%
  select(-ends_with("_r"))

# Add year and month columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2011, month = "July")

# Save the dataset to a .dta file
write_dta(collapsed_data, "Puebla_Section_2011.dta")

# Import the Excel sheet with specified cell range
data <- read_excel("RESULTADOS_POR_CASILLA_AYUNTAMIENTOS Y DIPUTADOS_2012_2013.xlsx", 
                   sheet = "AYUNTAMIENTOS FINAL", range = "A7:O6903", col_names = TRUE)

# Rename columns
data <- data %>%
  rename(
    municipality = MUNICIPIO,
    section = SECCIÓN,
    listanominal = NoDECIUDADANOSENLISTANOMI,
    total = VOTACIÓNTOTAL
  )

# Drop rows where both 'municipality' and 'section' are empty
data <- data %>%
  filter(!(is.na(municipality) & is.na(section)))

# Convert 'section' to numeric (similar to destring in Stata)
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

# Convert specified columns from character to numeric (similar to destring in Stata)
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
    municipality = str_to_title(municipality) # Proper case equivalent in Stata
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

# The resulting 'collapsed_data' matches the operations in your Stata code

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

# Generate 'mun_' variables (summation by uniqueid)
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN_PRD_PANAL_CPP, PRI_PVEM, PT, PC, PSI, PAN_PRD_PANAL_CPP_PC, PAN_PRD_PANAL_CPP_PC_PSI, 
                  PAN_PRD_PANAL_CPP_PSI, PAN_PRD_PANAL_CPP_PT, PAN_PRD_PANAL_CPP_PT_PSI, total, listanominal, valid),
                ~ sum(.x, na.rm = TRUE), 
                .names = "mun_{col}"))

# Create inverse of 'mun_' variables (equivalent to 'inv_mun_`var' in Stata)
collapsed_data <- collapsed_data %>%
  mutate(across(starts_with("mun_"), ~ 1 / .x, .names = "inv_{col}"))

# Calculate 'mun_turnout' as the ratio of 'mun_total' to 'mun_listanominal'
collapsed_data <- collapsed_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse 'mun_' variables (equivalent to 'rowranks' in Stata)
collapsed_data <- collapsed_data %>%
  mutate(across(starts_with("inv_mun_"), 
                ~ rowRanks(as.matrix(.), ties.method = "average", decreasing = TRUE),
                .names = "{str_replace(.col, 'inv_mun_', '')}_r"))

# Drop 'inv_mun_*' columns
collapsed_data <- collapsed_data %>%
  select(-starts_with("inv_mun_"))

# Determine the 'winner' based on the ranking
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    PAN_PRD_PANAL_CPP_r == 1 ~ "PAN_PRD_PANAL_CPP",
    PAN_PRD_PANAL_CPP_PC_r == 1 ~ "PAN_PRD_PANAL_CPP_PC",
    PAN_PRD_PANAL_CPP_PC_PSI_r == 1 ~ "PAN_PRD_PANAL_CPP_PC_PSI",
    PAN_PRD_PANAL_CPP_PSI_r == 1 ~ "PAN_PRD_PANAL_CPP_PSI",
    PAN_PRD_PANAL_CPP_PT_r == 1 ~ "PAN_PRD_PANAL_CPP_PT",
    PAN_PRD_PANAL_CPP_PT_PSI_r == 1 ~ "PAN_PRD_PANAL_CPP_PT_PSI",
    PRI_PVEM_r == 1 ~ "PRI_PVEM",
    PT_r == 1 ~ "PT",
    PC_r == 1 ~ "PC",
    PSI_r == 1 ~ "PSI"
  ))

# Add 'year' and 'month' columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2013, month = "July")

# Sort by 'section' (optional in R, as this might not be needed before saving)
collapsed_data <- collapsed_data %>%
  arrange(section)

# Save the final dataset as a .dta file
write_dta(collapsed_data, "Puebla_Section_2013.dta")

# Load and append datasets
puebla_section <- read_dta("Puebla_Section_1998.dta") %>%
  bind_rows(
    read_dta("Puebla_Section_2001.dta"),
    read_dta("Puebla_Section_2002.dta"),
    read_dta("Puebla_Section_2004.dta"),
    read_dta("Puebla_Section_2005.dta"),
    read_dta("Puebla_Section_2007.dta"),
    read_dta("Puebla_Section_2008.dta"),
    read_dta("Puebla_Section_2010.dta"),
    read_dta("Puebla_Section_2011.dta"),
    read_dta("Puebla_Section_2013.dta")
  )

# Remove old files
file.remove("Puebla_Section_1998.dta", "Puebla_Section_2001.dta", "Puebla_Section_2002.dta",
            "Puebla_Section_2004.dta", "Puebla_Section_2005.dta", "Puebla_Section_2007.dta",
            "Puebla_Section_2008.dta", "Puebla_Section_2010.dta", "Puebla_Section_2011.dta",
            "Puebla_Section_2013.dta")

# View frequency table of 'winner'
table(puebla_section$winner)

# Browse for missing winners
puebla_section %>%
  filter(winner == "") %>%
  View()

# Browse for specific rows with 'municipality' == "Albino Zertuche" and 'year' == 2010
puebla_section %>%
  filter(municipality == "Albino Zertuche" & year == 2010) %>%
  View()

# Browse for 'San Jeronimo Tecuanipan' in 2010
puebla_section %>%
  filter(municipality == "San Jeronimo Tecuanipan" & year == 2010) %>%
  View()

# Frequency table including missing values
table(puebla_section$winner, useNA = "ifany")

# Create PAN_winner column and winner_counter
puebla_section <- puebla_section %>%
  mutate(
    PAN_winner = ifelse(grepl("PAN", winner) & !grepl("PANAL", winner), 1, 0),
    winner_counter = PAN_winner
  )

# Create other party winner columns and update winner_counter
parties <- c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "PD", "PSD", "PAS", 
             "PSN", "PartidoMexicoPosible", "CDPPN", "PUP", "PEC", "CPP", "PSI")

for (party in parties) {
  puebla_section <- puebla_section %>%
    mutate(
      !!paste0(party, "_winner") := ifelse(grepl(party, winner), 1, 0),
      winner_counter = winner_counter + !!sym(paste0(party, "_winner"))
    )
}

# Tabulate winner_counter
table(puebla_section$winner_counter)

# Count how many rows have zero winner_counter
sum(puebla_section$winner_counter == 0)

# Save the updated dataset in older .dta format (version 12)
write_dta(puebla_section, "../Puebla_ALL.dta", version = 12)

# Import Excel data
puebla_data <- read_excel("FINAL_Ayu_Ext_2014.xlsx", sheet = "2014", col_names = TRUE)

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

# Generate 'mun_' and 'inv_mun_' variables
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PRI_PVEM, PAN_PRD_PANAL_CP_PSI, MC, total, valid, listanominal), 
                ~ sum(.x, na.rm = TRUE), 
                .names = "mun_{col}")) %>%
  mutate(across(starts_with("mun_"), ~ 1 / .x, .names = "i_{col}"))

# Create turnout variables
collapsed_data <- collapsed_data %>%
  mutate(turnout = valid / listanominal,
         mun_turnout = mun_valid / mun_listanominal)

# Rank the inverse 'mun_' variables
collapsed_data <- collapsed_data %>%
  mutate(across(starts_with("i_"), 
                ~ rowRanks(as.matrix(.), ties.method = "average", decreasing = TRUE),
                .names = "{str_replace(.col, 'i_', '')}_r"))

# Drop the 'i_*' variables
collapsed_data <- collapsed_data %>%
  select(-starts_with("i_"))

# Generate empty 'winner', 'second', 'third' columns
collapsed_data <- collapsed_data %>%
  mutate(winner = "", second = "", third = "")

# Assign 'winner', 'second', 'third' based on ranks
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    PRI_PVEM_r == 1 ~ "PRI_PVEM",
    PAN_PRD_PANAL_CP_PSI_r == 1 ~ "PAN_PRD_PANAL_CP_PSI",
    MC_r == 1 ~ "MC",
    TRUE ~ winner
  ),
  second = case_when(
    PRI_PVEM_r == 2 ~ "PRI_PVEM",
    PAN_PRD_PANAL_CP_PSI_r == 2 ~ "PAN_PRD_PANAL_CP_PSI",
    MC_r == 2 ~ "MC",
    TRUE ~ second
  ),
  third = case_when(
    PRI_PVEM_r == 3 ~ "PRI_PVEM",
    PAN_PRD_PANAL_CP_PSI_r == 3 ~ "PAN_PRD_PANAL_CP_PSI",
    MC_r == 3 ~ "MC",
    TRUE ~ third
  ))

# Drop ranking variables
collapsed_data <- collapsed_data %>%
  select(-ends_with("_r"))

# Add year, month, and state columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2014, month = "July", STATE = "PUEBLA")

# Save the data to .dta format
write_dta(collapsed_data, "Puebla_Section_2014.dta")

# Collapse data by 'municipality' and 'uniqueid' to get incumbents
incumbents_data <- collapsed_data %>%
  group_by(municipality, uniqueid) %>%
  summarise(winner = first(winner)) %>%
  rename(incumbent = winner)

# Save incumbents data
write_dta(incumbents_data, "incumbents2018b.dta")

# Load the data
puebla_section_2014 <- read_dta("Puebla_Section_2014.dta")

# Collapse (sum) over 'listanominal' to 'valid' and (first) for STATE, year, month, winner, second, third, and mun_turnout
collapsed_data <- puebla_section_2014 %>%
  group_by(municipality, uniqueid) %>%
  summarise(across(c(listanominal:valid), sum, na.rm = TRUE),
            across(c(STATE, year, month, winner, second, third, mun_turnout), 
                   first, na.rm = TRUE))

# Rename 'mun_turnout' to 'turnout'
collapsed_data <- collapsed_data %>%
  rename(turnout = mun_turnout)

# Sort by 'uniqueid'
collapsed_data <- collapsed_data %>%
  arrange(uniqueid)

# Save the data to .dta format
write_dta(collapsed_data, "Puebla_2014.dta", version = 12)

# Import the Excel file
puebla_2018 <- read_excel("Ayuntamientos_2018.xlsx", sheet = "Ayuntamientos")

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
coalitions <- read_excel("puebla_coalitions_2018.xlsx", sheet = "Sheet1")
saveRDS(coalitions, file = "merge.rds")

# Perform the merge operation
puebla_2018 <- puebla_2018 %>%
  left_join(readRDS("merge.rds"), by = "municipality") %>%
  select(-"_merge")  # Drop any '_merge' column if it exists

# Remove the temporary file
file.remove("merge.rds")

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

# Generate 'mun_' prefixed columns by summing up variables grouped by 'uniqueid'
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI, PRD, PVEM, MC, PANAL, PCPP, PSI, PT_MORENA_PES, PAN_PRD_MC_PCPP_PSI, PAN_PRD_MC_PSI, PAN_PRD_PCPP_PSI,
                     PAN_PRD_MC_PCPP, PAN_MC_PCPP_PSI, PAN_PRD_MC, PAN_PCPP_PSI, PAN_MC_PSI, PAN_MC_PCPP, PAN_PRD_PSI, PAN_PRD_PCPP, PAN_PRD,
                     PAN_PSI, PAN_MC, PAN_PCPP, PRD_MC_PSI, PRD_MC_PCPP, PRD_PCPP_PSI, PRD_MC, PRD_PCPP, PRD_PSI, MC_PCPP_PSI, MC_PCPP, MC_PSI,
                     PANAL_PCPP, PVEM_PCPP_PSI, PVEM_PSI, PVEM_PCPP, PCPP_PSI, PRI_PANAL, CI_1, CI_2, total, valid), sum, na.rm = TRUE))

# Ranking and generating winner, second, and third place
collapsed_data <- collapsed_data %>%
  mutate(across(c(PAN_r, PRI_r, PRD_r, PVEM_r, MC_r, PANAL_r, PCPP_r, PSI_r, PT_MORENA_PES_r, PAN_PRD_MC_PCPP_PSI_r,
                  PAN_PRD_MC_PSI_r, PAN_PRD_PCPP_PSI_r, PAN_PRD_MC_PCPP_r, PAN_MC_PCPP_PSI_r, PAN_PRD_MC_r, PAN_PCPP_PSI_r,
                  PAN_MC_PSI_r, PAN_MC_PCPP_r, PAN_PRD_PSI_r, PAN_PRD_PCPP_r, PAN_PRD_r, PAN_PSI_r, PAN_MC_r, PAN_PCPP_r,
                  PRD_MC_PSI_r, PRD_MC_PCPP_r, PRD_PCPP_PSI_r, PRD_MC_r, PRD_PCPP_r, PRD_PSI_r, MC_PCPP_PSI_r, MC_PCPP_r,
                  MC_PSI_r, PANAL_PCPP_r, PVEM_PCPP_PSI_r, PVEM_PSI_r, PVEM_PCPP_r, PCPP_PSI_r, PRI_PANAL_r, CI_1_r, CI_2_r),
                rank))

# Generate winner, second, third
collapsed_data <- collapsed_data %>%
  mutate(
    winner = case_when(across(c(PAN_r, PRI_r, PRD_r, PVEM_r, MC_r, PANAL_r, PCPP_r, PSI_r, PT_MORENA_PES_r, PAN_PRD_MC_PCPP_PSI_r,
                                PAN_PRD_MC_PSI_r, PAN_PRD_PCPP_PSI_r, PAN_PRD_MC_PCPP_r, PAN_MC_PCPP_PSI_r, PAN_PRD_MC_r, PAN_PCPP_PSI_r,
                                PAN_MC_PSI_r, PAN_MC_PCPP_r, PAN_PRD_PSI_r, PAN_PRD_PCPP_r, PAN_PRD_r, PAN_PSI_r, PAN_MC_r, PAN_PCPP_r,
                                PRD_MC_PSI_r, PRD_MC_PCPP_r, PRD_PCPP_PSI_r, PRD_MC_r, PRD_PCPP_r, PRD_PSI_r, MC_PCPP_PSI_r, MC_PCPP_r,
                                MC_PSI_r, PANAL_PCPP_r, PVEM_PCPP_PSI_r, PVEM_PSI_r, PVEM_PCPP_r, PCPP_PSI_r, PRI_PANAL_r, CI_1_r, CI_2_r), min, na.rm = TRUE)),
    second = case_when(across(c(PAN_r, PRI_r, PRD_r, PVEM_r, MC_r, PANAL_r, PCPP_r, PSI_r, PT_MORENA_PES_r, PAN_PRD_MC_PCPP_PSI_r,
                                PAN_PRD_MC_PSI_r, PAN_PRD_PCPP_PSI_r, PAN_PRD_MC_PCPP_r, PAN_MC_PCPP_PSI_r, PAN_PRD_MC_r, PAN_PCPP_PSI_r,
                                PAN_MC_PSI_r, PAN_MC_PCPP_r, PAN_PRD_PSI_r, PAN_PRD_PCPP_r, PAN_PRD_r, PAN_PSI_r, PAN_MC_r, PAN_PCPP_r,
                                PRD_MC_PSI_r, PRD_MC_PCPP_r, PRD_PCPP_PSI_r, PRD_MC_r, PRD_PCPP_r, PRD_PSI_r, MC_PCPP_PSI_r, MC_PCPP_r,
                                MC_PSI_r, PANAL_PCPP_r, PVEM_PCPP_PSI_r, PVEM_PSI_r, PVEM_PCPP_r, PCPP_PSI_r, PRI_PANAL_r, CI_1_r, CI_2_r), second_min)),
    third = case_when(across(c(PAN_r, PRI_r, PRD_r, PVEM_r, MC_r, PANAL_r, PCPP_r, PSI_r, PT_MORENA_PES_r, PAN_PRD_MC_PCPP_PSI_r,
                               PAN_PRD_MC_PSI_r, PAN_PRD_PCPP_PSI_r, PAN_PRD_MC_PCPP_r, PAN_MC_PCPP_PSI_r, PAN_PRD_MC_r, PAN_PCPP_PSI_r,
                               PAN_MC_PSI_r, PAN_MC_PCPP_r, PAN_PRD_PSI_r, PAN_PRD_PCPP_r, PAN_PRD_r, PAN_PSI_r, PAN_MC_r, PAN_PCPP_r,
                               PRD_MC_PSI_r, PRD_MC_PCPP_r, PRD_PCPP_PSI_r, PRD_MC_r, PRD_PCPP_r, PRD_PSI_r, MC_PCPP_PSI_r, MC_PCPP_r,
                               MC_PSI_r, PANAL_PCPP_r, PVEM_PCPP_PSI_r, PVEM_PSI_r, PVEM_PCPP_r, PCPP_PSI_r, PRI_PANAL_r, CI_1_r, CI_2_r), third_min))
  )

# Drop all *_r columns
puebla_data <- collapsed_data %>% select(-contains("_r"))

# Replace winner, second, third with "Independent" if values are "CI_1" or "CI_2"
puebla_data <- puebla_data %>%
  mutate(
    winner = ifelse(winner %in% c("CI_1", "CI_2"), "Independent", winner),
    second = ifelse(second %in% c("CI_1", "CI_2"), "Independent", second),
    third = ifelse(third %in% c("CI_1", "CI_2"), "Independent", third)
  )

# Add year, month, STATE, and incumbent_vote columns
puebla_data <- puebla_data %>%
  mutate(
    year = 2018,
    month = "July",
    STATE = "PUEBLA",
    incumbent_vote = NA_real_
  )

# Drop all columns starting with 'cc'
puebla_data <- puebla_data %>% select(-starts_with("cc"))

# Preserve the current dataset (equivalent to Stata's `preserve`)
puebla_data_backup <- puebla_data

# Load the Listado Nominal PREP 2018 data
prep_data <- readRDS("../Listas Nominales/ListadoNominalPREP2018.rds")  # Adjust to the correct file path

# Filter for PUEBLA state
prep_data <- prep_data %>% filter(STATE == "PUEBLA")

# Save the filtered data (for temporary purposes, similar to the `save` in Stata)
saveRDS(prep_data, "PUE_LN18.rds")

# Restore the original dataset (equivalent to Stata's `restore`)
puebla_data <- puebla_data_backup

# Merge puebla_data with prep_data using "section" column
puebla_data <- puebla_data %>%
  left_join(prep_data, by = "section") %>%
  filter(!is.na(ListadoNominalINE))  # Drop rows where no match was found (equivalent to `_merge==2` in Stata)

# Drop the temporary saved data
unlink("PUE_LN18.rds")

# Rename the column `ListadoNominalINE` to `listanominal`
puebla_data <- puebla_data %>%
  rename(listanominal = ListadoNominalINE)

# Calculate turnout at the precinct and municipal levels
puebla_data <- puebla_data %>%
  mutate(turnout = total / listanominal) %>%
  group_by(uniqueid) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Save the final data
saveRDS(puebla_data, "Puebla_Section_2018.rds")  # Adjust the path and format to your requirements

# Collapse (sum) PAN-valid and listanominal, and take the first value for STATE, year, month, winner, second, third, mun_turnout, incumbent
puebla_data_collapsed <- puebla_data %>%
  group_by(municipality, uniqueid) %>%
  summarise(
    PAN = sum(PAN, na.rm = TRUE),
    valid = sum(valid, na.rm = TRUE),
    listanominal = sum(listanominal, na.rm = TRUE),
    STATE = first(STATE),
    year = first(year),
    month = first(month),
    winner = first(winner),
    second = first(second),
    third = first(third),
    turnout = first(mun_turnout),
    incumbent = first(incumbent)
  ) %>%
  ungroup()

# Sort by uniqueid
puebla_data_collapsed <- puebla_data_collapsed %>%
  arrange(uniqueid)

# Save the dataset
saveRDS(puebla_data_collapsed, "../../Update Municipal/Puebla_2018.rds")  # Adjust to your correct file path and format

# Import CSV files and save them as individual RDS files
municipalities <- c("AHUAZOTEPEC", "CAÑADA MORELOS", "MAZAPILTEPEC DE JUAREZ", "OCOYUCAN", "TEPEOJUMA")
for (x in municipalities) {
  file_path <- paste0("20190609_0800_CW_PRESIDENTE_MUNICIPAL/PRESIDENTE_MUNICIPAL_", x, "_2019.csv")
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

file.remove("TEPEOJUMA.rds") # Erase TEPEOJUMA file

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

# Generate mun-level sums and inverse variables
append_data_collapsed <- append_data_collapsed %>%
  group_by(uniqueid) %>%
  mutate(across(PRI:valid, list(mun_sum = sum, inv = ~1/sum(.)))) %>%
  ungroup()

# Calculate municipal-level turnout
append_data_collapsed <- append_data_collapsed %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Calculate rankings
append_data_collapsed <- append_data_collapsed %>%
  rowwise() %>%
  mutate(across(starts_with("inv_mun"), ~ rank(.), .names = "{col}_r"))

# Determine the winner
append_data_collapsed <- append_data_collapsed %>%
  mutate(winner = case_when(
    PRI_r == 1 ~ "PRI",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    MC_r == 1 ~ "MC",
    MORENA_r == 1 ~ "MORENA",
    PES_r == 1 ~ "PES",
    PAN_r == 1 ~ "PAN",
    PSI_r == 1 ~ "PSI",
    NAP_r == 1 ~ "NAP",
    PT_PVEM_MORENA_PES_r == 1 ~ "PT_PVEM_MORENA_PES",
    PAN_CPP_r == 1 ~ "PAN_CPP",
    PAN_PRD_MC_CPP_r == 1 ~ "PAN_PRD_MC_CPP",
    PRI_CPP_r == 1 ~ "PRI_CPP",
    PRD_MC_CPP_r == 1 ~ "PRD_MC_CPP",
    PT_MORENA_PES_r == 1 ~ "PT_MORENA_PES"
  ))

# Add year and month
append_data_collapsed <- append_data_collapsed %>%
  mutate(year = 2019, month = "June")

# Save final dataset
saveRDS(append_data_collapsed, "Puebla_Section_2019.rds")

# Load the RDS files
puebla_2014 <- readRDS("Puebla_Section_2014.rds")
puebla_2018 <- readRDS("Puebla_Section_2018.rds")
puebla_2019 <- readRDS("Puebla_Section_2019.rds")

# Append the data
puebla_all <- bind_rows(puebla_2014, puebla_2018, puebla_2019)

# Remove the individual datasets from the directory
file.remove("Puebla_Section_2014.rds")
file.remove("Puebla_Section_2018.rds")
file.remove("Puebla_Section_2019.rds")

# Generate `PAN_winner` based on the winner string
puebla_all <- puebla_all %>%
  mutate(
    PAN_winner = if_else(str_detect(winner, "PAN") & !str_detect(winner, "PANAL"), 1, 0),
    winner_counter = PAN_winner
  )

# Loop over other parties and generate the respective winner variables
parties <- c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "MORENA", "MC")
for (party in parties) {
  puebla_all <- puebla_all %>%
    mutate(!!paste0(party, "_winner") := if_else(str_detect(winner, party), 1, 0)) %>%
    mutate(winner_counter = winner_counter + !!sym(paste0(party, "_winner")))
}

# Check `winner_counter` and count zeros
table(puebla_all$winner_counter)
n_zero_winners <- sum(puebla_all$winner_counter == 0)

# Save the combined dataset
saveRDS(puebla_all, "Puebla_Section_14_18.rds")

# Clear current dataset and append to an existing one
puebla_all_data <- readRDS("../../Precinct/Puebla_ALL.rds")
puebla_combined <- bind_rows(puebla_all_data, puebla_all)

# Replace `municipality` values with uppercase
puebla_combined <- puebla_combined %>%
  mutate(municipality = str_to_upper(municipality))

# Save the final combined dataset
saveRDS(puebla_combined, "Puebla_ALL_SALVADOR.rds")

# Remove temporary dataset
file.remove("Puebla_Section_14_18.rds")
