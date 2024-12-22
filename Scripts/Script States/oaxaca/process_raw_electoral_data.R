# Load necessary libraries
library(dplyr)
library(readxl)
library(stringr)

# Step 1: Set working directory (similar to capture cd in Stata)
setwd("D:/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Oaxaca - 1998, 2001, 2004, 2007, 2010 - Missing 2013 (Requested)")  # or C:/Users/Horacio/...

# Step 2: Import the Excel sheet
oaxaca_data <- read_excel("Concejales 1998.xls", sheet = "Votacion por casilla Municipal", range = "A2:T2619")

# Step 3: Data cleaning - drop rows based on specific conditions
# Remove rows where Municipio is "Total del Municipio:" or where Cve is empty
oaxaca_data <- oaxaca_data %>%
  filter(Municipio != "Total del Municipio:", !is.na(Cve))

# Replace Municipio with the value from the previous row if Cve matches
oaxaca_data <- oaxaca_data %>%
  mutate(Municipio = if_else(Cve == lag(Cve), lag(Municipio), Municipio))

# Step 4: Further filtering based on PAN and ListaNominal columns
# Remove rows where PAN or ListaNominal contains specific text like "ANULADA", "DESTRUIDA", or "INSTALO"
oaxaca_data <- oaxaca_data %>%
  filter(!str_detect(PAN, "ANULADA|DESTRUIDA|INSTALO")) %>%
  filter(!str_detect(ListaNominal, "INSTALO"))

# Step 5: Convert columns to numeric
oaxaca_data <- oaxaca_data %>%
  mutate(across(c(ListaNominal, PAN), as.numeric))

# Step 6: Rename columns to match the Stata code
oaxaca_data <- oaxaca_data %>%
  rename(listanominal = ListaNominal,
         section = Secc,
         municipality = Municipio,
         total = VTotalEmitida,
         valid = TVotosVlidos)

# Step 7: Drop unwanted columns
oaxaca_data <- oaxaca_data %>%
  select(-Cve, -NoReg, -Nulos, -TdeAbst, -Vot, -Abst, -T)

# Step 8: Collapse (sum) the data by municipality and section
oaxaca_data <- oaxaca_data %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, PAN, valid, total), sum, na.rm = TRUE))

# Step 9: Calculate turnout
oaxaca_data <- oaxaca_data %>%
  mutate(turnout = total / listanominal)

# Step 10: Replace accented characters and make municipality names uppercase
oaxaca_data <- oaxaca_data %>%
  mutate(municipality = str_replace_all(municipality, c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ñ" = "n")),
         municipality = toupper(municipality))

# Step 11: Save the cleaned and processed dataset (if needed)
# write.csv(oaxaca_collapsed, "Oaxaca_1998_Cleaned.csv", row.names = FALSE) # Save as CSV if necessary

# Assuming `oaxaca_collapsed` is your dataframe or continue working with "oaxaca_data"
oaxaca_data <- oaxaca_data %>%
  mutate(uniqueid = 0,  # Initialize the uniqueid column
         
         # Replace uniqueid based on the municipality names
         uniqueid = if_else(municipality == "ACATLAN DE PEREZ FIGUEROA", 20002, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION CUYOTEPEJI", 20004, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION IXTALTEPEC", 20005, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION NOCHIXTLAN", 20006, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION OCOTLAN", 20007, uniqueid),
         uniqueid = if_else(municipality == "AYOTZINTEPEC", 20009, uniqueid),
         uniqueid = if_else(municipality == "CHAHUITES", 20025, uniqueid),
         uniqueid = if_else(municipality == "CHALCATONGO DE HIDALGO", 20026, uniqueid),
         uniqueid = if_else(municipality == "CIENEGA DE ZIMATLAN", 20013, uniqueid),
         uniqueid = if_else(municipality == "CIUDAD IXTEPEC", 20014, uniqueid),
         uniqueid = if_else(municipality == "COSOLAPA", 20021, uniqueid),
         uniqueid = if_else(municipality == "CUILAPAM DE GUERRERO", 20023, uniqueid),
         uniqueid = if_else(municipality == "EJUTLA DE CRESPO", 20028, uniqueid),
         uniqueid = if_else(municipality == "BARRIO DE LA SOLEDAD, EL", 20010, uniqueid),
         uniqueid = if_else(municipality == "EL ESPINAL", 20030, uniqueid),
         uniqueid = if_else(municipality == "FRESNILLO DE TRUJANO", 20032, uniqueid),
         uniqueid = if_else(municipality == "GUADALUPE DE RAMIREZ", 20034, uniqueid),
         uniqueid = if_else(municipality == "H. CIUDAD DE TLAXIACO", 20397, uniqueid),
         uniqueid = if_else(municipality == "HUAJUAPAN DE LEON", 20039, uniqueid),
         uniqueid = if_else(municipality == "HUAUTEPEC", 20040, uniqueid),
         uniqueid = if_else(municipality == "HUAUTLA DE JIMENEZ", 20041, uniqueid),
         uniqueid = if_else(municipality == "JUCHITAN DE ZARAGOZA", 20043, uniqueid),
         uniqueid = if_else(municipality == "LOMA BONITA", 20044, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA OCOTLAN", 20049, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TEQUISISTLAN", 20052, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TLACOTEPEC", 20053, uniqueid),
         uniqueid = if_else(municipality == "MARISCALA DE JUAREZ", 20055, uniqueid),
         uniqueid = if_else(municipality == "MARTIRES DE TACUBAYA", 20056, uniqueid),
         uniqueid = if_else(municipality == "MATIAS ROMERO", 20057, uniqueid),
         uniqueid = if_else(municipality == "MIAHUATLAN DE PORFIRIO DIAZ", 20059, uniqueid),
         uniqueid = if_else(municipality == "OAXACA DE JUAREZ", 20067, uniqueid),
         uniqueid = if_else(municipality == "OCOTLAN DE MORELOS", 20068, uniqueid),
         uniqueid = if_else(municipality == "PINOTEPA DE DON LUIS", 20070, uniqueid),
         uniqueid = if_else(municipality == "PUTLA VILLA DE GUERRERO", 20073, uniqueid),
         uniqueid = if_else(municipality == "REFORMA DE PINEDA", 20075, uniqueid),
         uniqueid = if_else(municipality == "SALINA CRUZ", 20079, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN AMATENGO", 20080, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN ATENANGO", 20081, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES DINICUITI", 20089, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES HUAXPALTEPEC", 20090, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES ZAUTLA", 20102, uniqueid),
         uniqueid = if_else(municipality == "SAN ANTONINO CASTILLO VELASCO", 20103, uniqueid),
         uniqueid = if_else(municipality == "SAN BALTAZAR CHICHICAPAM", 20112, uniqueid),
         uniqueid = if_else(municipality == "SAN BARTOLOME AYAUTLA", 20116, uniqueid),
         uniqueid = if_else(municipality == "SAN BLAS ATEMPA", 20124, uniqueid),
         uniqueid = if_else(municipality == "SAN DIONISIO DEL MAR", 20130, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE JALAPA DE DIAZ", 20134, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE USILA", 20136, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO DEL MAR", 20141, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO IXHUATAN", 20143, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO TELIXTLAHUACA", 20150, uniqueid),
         uniqueid = if_else(municipality == "SAN JACINTO AMILPAS", 20157, uniqueid),
         uniqueid = if_else(municipality == "SAN JERONIMO SILACAYOAPILLA", 20160, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE CHILTEPEC", 20166, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE ESTANCIA GRANDE", 20168, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE INDEPENDENCIA", 20169, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE TENANGO", 20171, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA CUICATLAN", 20177, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA LO DE SOTO", 20180, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA SUCHITEPEC", 20181, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BTTA TLACOATZINTEPEC", 20182, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA TUXTEPEC", 20184, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA VALLE NACIONAL", 20559, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN CACAHUATEPEC", 20185, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COATZOSPAM", 20187, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COLORADO", 20188, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN GUICHICOVI", 20198, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN IHUALTEPEC", 20199, uniqueid),
         uniqueid = if_else(municipality == "SAN LORENZO", 20225, uniqueid),
         uniqueid = if_else(municipality == "SAN LUCAS OJITLAN", 20232, uniqueid),
         uniqueid = if_else(municipality == "SAN MARCOS ARTEAGA", 20237, uniqueid),
         uniqueid = if_else(municipality == "SAN MARTIN ZACATEPEC", 20245, uniqueid),
         uniqueid = if_else(municipality == "SAN MATEO RIO HONDO", 20254, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AHUEHUETITLAN", 20259, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AMATITLAN", 20261, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL SOYALTEPEC", 20278, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL TLACAMAMA", 20285, uniqueid),
         uniqueid = if_else(municipality == "SAN NICOLAS HIDALGO", 20290, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUITZO", 20294, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUIXTEPEC", 20295, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO VILLA DE MITLA", 20298, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO AMUZGOS", 20300, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO ATOYAC", 20302, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO COMITANCILLO", 20305, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUAMELULA", 20307, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUILOTEPEC", 20308, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO IXCATLAN", 20309, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO JICAYAN", 20312, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO MIXTEPEC", 20318, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO POCHUTLA", 20324, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TAPANATEPEC", 20327, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TUTUTEPEC", 20334, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO Y SAN PABLO TEPOSCOLULA", 20339, uniqueid),
         uniqueid = if_else(municipality == "SAN SEBASTIAN IXCAPA", 20345, uniqueid),
         uniqueid = if_else(municipality == "SANTA ANA ZEGACHE", 20360, uniqueid),
         uniqueid = if_else(municipality == "SANTA CATARINA JUQUILA", 20364, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ AMILPAS", 20375, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ ITUNDUJIA", 20377, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ TACACHE DE MINA", 20381, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ XOXOCOTLAN", 20385, uniqueid),
         uniqueid = if_else(municipality == "SANTA DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(municipality == "SANTA GERTRUDIS", 20387, uniqueid),
         uniqueid = if_else(municipality == "SANTA LUCIA DEL CAMINO", 20390, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA CORTIJOS", 20402, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUATULCO", 20413, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUAZOLOTITLAN", 20414, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA IPALAPA", 20415, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JACATEPEC", 20417, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JALAPA DEL MARQUES", 20418, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA MIXTEQUILLA", 20421, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA PETAPA", 20427, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TECOMAVACA", 20431, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEOPOXCO", 20434, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEXCATITLAN", 20436, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TONAMECA", 20439, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA XADANI", 20441, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA ZACATEPEC", 20447, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO AYUQUILILLA", 20455, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CACALOXTEPEC", 20456, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CHAZUMBA", 20459, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO HUAJOLOTITLAN", 20462, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JAMILTEPEC", 20467, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JUXTLAHUACA", 20469, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LAOLLAGA", 20472, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LLANO GRANDE", 20474, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO NILTEPEC", 20476, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO PINOTEPA NACIONAL", 20482, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO SUCHILQUITONGO", 20483, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAMAZOLA", 20484, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAPEXTLA", 20485, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TETEPEC", 20489, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ARMENTA", 20507, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO CHIHUITAN", 20508, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO PETAPA", 20513, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TEHUANTEPEC", 20515, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TONALA", 20520, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ZANATEPEC", 20525, uniqueid),
         uniqueid = if_else(municipality == "SILACAYOAPAM", 20537, uniqueid),
         uniqueid = if_else(municipality == "SOLEDAD ETLA", 20539, uniqueid),
         uniqueid = if_else(municipality == "TEOTITLAN DE FLORES MAGON", 20545, uniqueid),
         uniqueid = if_else(municipality == "TEZOATLAN DE SEGURA Y LUNA", 20549, uniqueid),
         uniqueid = if_else(municipality == "TLACOLULA DE MATAMOROS", 20551, uniqueid),
         uniqueid = if_else(municipality == "TRINIDAD ZAACHILA", 20555, uniqueid),
         uniqueid = if_else(municipality == "UNION HIDALGO", 20557, uniqueid),
         uniqueid = if_else(municipality == "VALERIO TRUJANO", 20558, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ETLA", 20338, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(str_detect(municipality, "SANTO DOMINGO TONALA"), 20520, uniqueid),
         uniqueid = if_else(municipality == "TAMAZULAPAM DEL PROGRESO", 20540, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ZAACHILA", 20565, uniqueid),
         uniqueid = if_else(municipality == "VILLA SOLA DE VEGA", 20277, uniqueid),
         uniqueid = if_else(municipality == "TEJUPAM DE LA UNION", 20486, uniqueid),
         uniqueid = if_else(municipality == "ZAPOTITLAN LAGUNAS", 20567, uniqueid),
         uniqueid = if_else(municipality == "ZIMATLAN DE ALVAREZ", 20568, uniqueid))

# Assuming the data is already loaded into a dataframe called 'oaxaca_data'
# If needed, load the dataset (if in Stata format, use haven::read_dta)
# oaxaca_data <- haven::read_dta("Oaxaca_1998.dta")

# Create total and listanominal columns and ensure they are numeric
oaxaca_data <- oaxaca_data %>%
  mutate(across(c(PAN, PRI, PRD, PT, PVEM, PARMEO, PC, total, listanominal), as.numeric))  # Ensure numeric columns

# Aggregate (sum) by uniqueid
oaxaca_data <- oaxaca_data %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI, PRD, PT, PVEM, PARMEO, PC, total, listanominal, valid), sum, na.rm = TRUE)) %>%
  ungroup()

# Generate inv_mun_* columns (inverse of the municipality-level sums)
oaxaca_data <- oaxaca_data %>%
  mutate(inv_mun_PAN = 1 / PAN,
         inv_mun_PRI = 1 / PRI,
         inv_mun_PRD = 1 / PRD,
         inv_mun_PT = 1 / PT,
         inv_mun_PVEM = 1 / PVEM,
         inv_mun_PARMEO = 1 / PARMEO,
         inv_mun_PC = 1 / PC)

# Generate municipal turnout
oaxaca_data <- oaxaca_data %>%
  mutate(mun_turnout = total / listanominal)

# Rank by inverse values for each party
oaxaca_data <- oaxaca_data %>%
  mutate(PAN_r = rowRanks(as.matrix(inv_mun_PAN), ties.method = "first"),
         PRI_r = rowRanks(as.matrix(inv_mun_PRI), ties.method = "first"),
         PRD_r = rowRanks(as.matrix(inv_mun_PRD), ties.method = "first"),
         PT_r = rowRanks(as.matrix(inv_mun_PT), ties.method = "first"),
         PVEM_r = rowRanks(as.matrix(inv_mun_PVEM), ties.method = "first"),
         PARMEO_r = rowRanks(as.matrix(inv_mun_PARMEO), ties.method = "first"),
         PC_r = rowRanks(as.matrix(inv_mun_PC), ties.method = "first"))

# Generate winner column based on rank
oaxaca_data <- oaxaca_data %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PARMEO_r == 1 ~ "PARMEO",
    TRUE ~ winner))

# Drop unnecessary rank columns
oaxaca_data <- oaxaca_data %>%
  select(-PAN_r, -PRI_r, -PRD_r, -PT_r, -PVEM_r, -PARMEO_r, -PC_r)

# Add year and month columns
oaxaca_data <- oaxaca_data %>%
  mutate(year = 1998,
         month = "October")

# Save the resulting data as a Stata file
haven::write_dta(oaxaca_data, "Oaxaca_1998.dta")

# Load the Excel sheet, assuming it's in your working directory
# Adjust the file path if needed
oaxaca_2001 <- read_excel("Concejales 2001.xls", sheet = "Votacion por casilla Municipal", range = "A2:U2820")

# Drop rows where Municipio is "Total del Municipio:" or Cve is missing
oaxaca_2001 <- oaxaca_2001 %>%
  filter(Municipio != "Total del Municipio:", !is.na(Cve))

# Replace Municipio values where Cve matches the previous row
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(Municipio = ifelse(Cve == lag(Cve), lag(Municipio), Municipio))

# Drop rows containing specific strings in PAN, ListaNominal, etc.
oaxaca_2001 <- oaxaca_2001 %>%
  filter(!str_detect(PAN, "ANULADA|DESTRUIDA|INSTALO"),
         !str_detect(ListaNominal, "ANULADA|INSTALO"))

# Remove "-" from PAN, PRD, PT, PVEM, PSN, CDPPN, PAS columns
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(across(c(PAN, PRD, PT, PVEM, PSN, CDPPN, PAS), ~str_replace_all(., "-", "")))

# Convert relevant columns to numeric
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(across(c(ListaNominal, PAN, PRD, PT, PVEM, PSN, CDPPN, PAS), as.numeric))

# Rename columns to match the Stata code
oaxaca_2001 <- oaxaca_2001 %>%
  rename(listanominal = ListaNominal, PC = CDPPN, section = Secc, total = VTotalEmitida, valid = TVotosVlidos)

# Drop unnecessary columns
oaxaca_2001 <- oaxaca_2001 %>%
  select(-Cve, -NoReg, -Nulos, -TdeAbst, -Vot, -Abst, -U)

# Collapse (sum) by municipality and section
oaxaca_2001 <- oaxaca_2001 %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, PAN, PRD, PT, PVEM, PSN, PC, PAS, total, valid), sum, na.rm = TRUE)) %>%
  ungroup()

# Calculate turnout
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(turnout = total / listanominal)

# Replace accented characters and convert municipality names to uppercase
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(municipality = str_replace_all(municipality, c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ñ" = "n")),
         municipality = toupper(municipality))

# Assuming `oaxaca_collapsed` is your dataframe or continue working with "oaxaca_2001"
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(uniqueid = 0,  # Initialize the uniqueid column
         
         # Replace uniqueid based on the municipality names
         uniqueid = if_else(municipality == "ACATLAN DE PEREZ FIGUEROA", 20002, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION CUYOTEPEJI", 20004, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION IXTALTEPEC", 20005, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION NOCHIXTLAN", 20006, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION OCOTLAN", 20007, uniqueid),
         uniqueid = if_else(municipality == "AYOTZINTEPEC", 20009, uniqueid),
         uniqueid = if_else(municipality == "CHAHUITES", 20025, uniqueid),
         uniqueid = if_else(municipality == "CHALCATONGO DE HIDALGO", 20026, uniqueid),
         uniqueid = if_else(municipality == "CIENEGA DE ZIMATLAN", 20013, uniqueid),
         uniqueid = if_else(municipality == "CIUDAD IXTEPEC", 20014, uniqueid),
         uniqueid = if_else(municipality == "COSOLAPA", 20021, uniqueid),
         uniqueid = if_else(municipality == "CUILAPAM DE GUERRERO", 20023, uniqueid),
         uniqueid = if_else(municipality == "EJUTLA DE CRESPO", 20028, uniqueid),
         uniqueid = if_else(municipality == "BARRIO DE LA SOLEDAD, EL", 20010, uniqueid),
         uniqueid = if_else(municipality == "EL ESPINAL", 20030, uniqueid),
         uniqueid = if_else(municipality == "FRESNILLO DE TRUJANO", 20032, uniqueid),
         uniqueid = if_else(municipality == "GUADALUPE DE RAMIREZ", 20034, uniqueid),
         uniqueid = if_else(municipality == "H. CIUDAD DE TLAXIACO", 20397, uniqueid),
         uniqueid = if_else(municipality == "HUAJUAPAN DE LEON", 20039, uniqueid),
         uniqueid = if_else(municipality == "HUAUTEPEC", 20040, uniqueid),
         uniqueid = if_else(municipality == "HUAUTLA DE JIMENEZ", 20041, uniqueid),
         uniqueid = if_else(municipality == "JUCHITAN DE ZARAGOZA", 20043, uniqueid),
         uniqueid = if_else(municipality == "LOMA BONITA", 20044, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA OCOTLAN", 20049, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TEQUISISTLAN", 20052, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TLACOTEPEC", 20053, uniqueid),
         uniqueid = if_else(municipality == "MARISCALA DE JUAREZ", 20055, uniqueid),
         uniqueid = if_else(municipality == "MARTIRES DE TACUBAYA", 20056, uniqueid),
         uniqueid = if_else(municipality == "MATIAS ROMERO", 20057, uniqueid),
         uniqueid = if_else(municipality == "MIAHUATLAN DE PORFIRIO DIAZ", 20059, uniqueid),
         uniqueid = if_else(municipality == "OAXACA DE JUAREZ", 20067, uniqueid),
         uniqueid = if_else(municipality == "OCOTLAN DE MORELOS", 20068, uniqueid),
         uniqueid = if_else(municipality == "PINOTEPA DE DON LUIS", 20070, uniqueid),
         uniqueid = if_else(municipality == "PUTLA VILLA DE GUERRERO", 20073, uniqueid),
         uniqueid = if_else(municipality == "REFORMA DE PINEDA", 20075, uniqueid),
         uniqueid = if_else(municipality == "SALINA CRUZ", 20079, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN AMATENGO", 20080, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN ATENANGO", 20081, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES DINICUITI", 20089, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES HUAXPALTEPEC", 20090, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES ZAUTLA", 20102, uniqueid),
         uniqueid = if_else(municipality == "SAN ANTONINO CASTILLO VELASCO", 20103, uniqueid),
         uniqueid = if_else(municipality == "SAN BALTAZAR CHICHICAPAM", 20112, uniqueid),
         uniqueid = if_else(municipality == "SAN BARTOLOME AYAUTLA", 20116, uniqueid),
         uniqueid = if_else(municipality == "SAN BLAS ATEMPA", 20124, uniqueid),
         uniqueid = if_else(municipality == "SAN DIONISIO DEL MAR", 20130, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE JALAPA DE DIAZ", 20134, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE USILA", 20136, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO DEL MAR", 20141, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO IXHUATAN", 20143, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO TELIXTLAHUACA", 20150, uniqueid),
         uniqueid = if_else(municipality == "SAN JACINTO AMILPAS", 20157, uniqueid),
         uniqueid = if_else(municipality == "SAN JERONIMO SILACAYOAPILLA", 20160, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE CHILTEPEC", 20166, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE ESTANCIA GRANDE", 20168, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE INDEPENDENCIA", 20169, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE TENANGO", 20171, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA CUICATLAN", 20177, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA LO DE SOTO", 20180, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA SUCHITEPEC", 20181, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BTTA TLACOATZINTEPEC", 20182, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA TUXTEPEC", 20184, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA VALLE NACIONAL", 20559, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN CACAHUATEPEC", 20185, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COATZOSPAM", 20187, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COLORADO", 20188, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN GUICHICOVI", 20198, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN IHUALTEPEC", 20199, uniqueid),
         uniqueid = if_else(municipality == "SAN LORENZO", 20225, uniqueid),
         uniqueid = if_else(municipality == "SAN LUCAS OJITLAN", 20232, uniqueid),
         uniqueid = if_else(municipality == "SAN MARCOS ARTEAGA", 20237, uniqueid),
         uniqueid = if_else(municipality == "SAN MARTIN ZACATEPEC", 20245, uniqueid),
         uniqueid = if_else(municipality == "SAN MATEO RIO HONDO", 20254, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AHUEHUETITLAN", 20259, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AMATITLAN", 20261, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL SOYALTEPEC", 20278, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL TLACAMAMA", 20285, uniqueid),
         uniqueid = if_else(municipality == "SAN NICOLAS HIDALGO", 20290, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUITZO", 20294, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUIXTEPEC", 20295, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO VILLA DE MITLA", 20298, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO AMUZGOS", 20300, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO ATOYAC", 20302, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO COMITANCILLO", 20305, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUAMELULA", 20307, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUILOTEPEC", 20308, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO IXCATLAN", 20309, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO JICAYAN", 20312, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO MIXTEPEC", 20318, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO POCHUTLA", 20324, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TAPANATEPEC", 20327, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TUTUTEPEC", 20334, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO Y SAN PABLO TEPOSCOLULA", 20339, uniqueid),
         uniqueid = if_else(municipality == "SAN SEBASTIAN IXCAPA", 20345, uniqueid),
         uniqueid = if_else(municipality == "SANTA ANA ZEGACHE", 20360, uniqueid),
         uniqueid = if_else(municipality == "SANTA CATARINA JUQUILA", 20364, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ AMILPAS", 20375, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ ITUNDUJIA", 20377, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ TACACHE DE MINA", 20381, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ XOXOCOTLAN", 20385, uniqueid),
         uniqueid = if_else(municipality == "SANTA DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(municipality == "SANTA GERTRUDIS", 20387, uniqueid),
         uniqueid = if_else(municipality == "SANTA LUCIA DEL CAMINO", 20390, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA CORTIJOS", 20402, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUATULCO", 20413, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUAZOLOTITLAN", 20414, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA IPALAPA", 20415, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JACATEPEC", 20417, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JALAPA DEL MARQUES", 20418, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA MIXTEQUILLA", 20421, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA PETAPA", 20427, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TECOMAVACA", 20431, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEOPOXCO", 20434, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEXCATITLAN", 20436, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TONAMECA", 20439, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA XADANI", 20441, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA ZACATEPEC", 20447, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO AYUQUILILLA", 20455, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CACALOXTEPEC", 20456, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CHAZUMBA", 20459, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO HUAJOLOTITLAN", 20462, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JAMILTEPEC", 20467, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JUXTLAHUACA", 20469, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LAOLLAGA", 20472, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LLANO GRANDE", 20474, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO NILTEPEC", 20476, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO PINOTEPA NACIONAL", 20482, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO SUCHILQUITONGO", 20483, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAMAZOLA", 20484, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAPEXTLA", 20485, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TETEPEC", 20489, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ARMENTA", 20507, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO CHIHUITAN", 20508, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO PETAPA", 20513, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TEHUANTEPEC", 20515, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TONALA", 20520, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ZANATEPEC", 20525, uniqueid),
         uniqueid = if_else(municipality == "SILACAYOAPAM", 20537, uniqueid),
         uniqueid = if_else(municipality == "SOLEDAD ETLA", 20539, uniqueid),
         uniqueid = if_else(municipality == "TEOTITLAN DE FLORES MAGON", 20545, uniqueid),
         uniqueid = if_else(municipality == "TEZOATLAN DE SEGURA Y LUNA", 20549, uniqueid),
         uniqueid = if_else(municipality == "TLACOLULA DE MATAMOROS", 20551, uniqueid),
         uniqueid = if_else(municipality == "TRINIDAD ZAACHILA", 20555, uniqueid),
         uniqueid = if_else(municipality == "UNION HIDALGO", 20557, uniqueid),
         uniqueid = if_else(municipality == "VALERIO TRUJANO", 20558, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ETLA", 20338, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(str_detect(municipality, "SANTO DOMINGO TONALA"), 20520, uniqueid),
         uniqueid = if_else(municipality == "TAMAZULAPAM DEL PROGRESO", 20540, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ZAACHILA", 20565, uniqueid),
         uniqueid = if_else(municipality == "VILLA SOLA DE VEGA", 20277, uniqueid),
         uniqueid = if_else(municipality == "TEJUPAM DE LA UNION", 20486, uniqueid),
         uniqueid = if_else(municipality == "ZAPOTITLAN LAGUNAS", 20567, uniqueid),
         uniqueid = if_else(municipality == "ZIMATLAN DE ALVAREZ", 20568, uniqueid))

# Assuming you already have the data loaded as `oaxaca_2001` from the previous step

# Step 1: Summing columns by uniqueid
oaxaca_2001 <- oaxaca_2001 %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN, PRI, PRD, PT, PVEM, PSN, PC, PAS, total, listanominal, valid), sum, .names = "mun_{col}")) %>%
  ungroup()

# Step 2: Generating inverse of municipal totals
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(across(starts_with("mun_"), ~ 1 / ., .names = "inv_{col}"))

# Step 3: Generating municipal turnout
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 4: Ranking the inverse of municipal vote shares
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(
    PAN_r = rank(-inv_mun_PAN, ties.method = "min"),
    PRI_r = rank(-inv_mun_PRI, ties.method = "min"),
    PRD_r = rank(-inv_mun_PRD, ties.method = "min"),
    PT_r = rank(-inv_mun_PT, ties.method = "min"),
    PVEM_r = rank(-inv_mun_PVEM, ties.method = "min"),
    PSN_r = rank(-inv_mun_PSN, ties.method = "min"),
    PC_r = rank(-inv_mun_PC, ties.method = "min"),
    PAS_r = rank(-inv_mun_PAS, ties.method = "min")
  )

# Step 5: Determining the winner based on ranking
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_r == 1 ~ "PRI",
      PRD_r == 1 ~ "PRD",
      PT_r == 1 ~ "PT",
      PVEM_r == 1 ~ "PVEM",
      PC_r == 1 ~ "PC",
      PSN_r == 1 ~ "PSN",
      PAS_r == 1 ~ "PAS",
      TRUE ~ winner
    )
  )

# Step 6: Dropping the ranking variables
oaxaca_2001 <- oaxaca_2001 %>%
  select(-ends_with("_r"), -starts_with("inv_"))

# Step 7: Adding the year and month
oaxaca_2001 <- oaxaca_2001 %>%
  mutate(year = 2001, month = "Octubre")

# Optional: View the final dataset
print(oaxaca_2001)

# Step 8: Save the dataset (optional)
# If you want to save it as a .dta file (Stata format), use haven package
# haven::write_dta(oaxaca_2001, "Oaxaca_2001.dta")

# Step 1: Read the dataset (equivalent to 'insheet using')
data <- read_csv("Ayu_Seccion_2004.csv")

# Step 2: Rename the columns (equivalent to 'rename')
data <- data %>%
  rename(
    municipality = municipio,
    section = seccion
  )

# Step 3: Drop rows where 'municipality' or 'section' is missing, and drop rows where 'total' is missing or 0
data <- data %>%
  filter(municipality != "" & !is.na(municipality) & section != "" & !is.na(section)) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert columns 'listanominal' to 'total' from character to numeric (equivalent to 'destring')
data <- data %>%
  mutate(across(listanominal:total, as.numeric))

# Step 5: Collapse data by 'municipality' and 'section' (equivalent to 'collapse (sum)')
data_collapsed <- data %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:total, sum, na.rm = TRUE), .groups = 'drop')

# Step 6: Rename columns (equivalent to 'rename')
data_collapsed <- data_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem,
    PC = pc,
    PUP = pup
  )

# Step 7: Calculate turnout (equivalent to 'gen turnout')
data_collapsed <- data_collapsed %>%
  mutate(turnout = total / listanominal)

# Optional: View the collapsed data
print(data_collapsed)

# Optional: Save the cleaned and processed data to a CSV file or any other format
# write_csv(data_collapsed, "Oaxaca_Seccion_2004_Cleaned.csv")

# Assuming `data_collapsed` is your dataframe 
data_collapsed <- data_collapsed %>%
  mutate(uniqueid = 0,  # Initialize the uniqueid column
         
         # Replace uniqueid based on the municipality names
         uniqueid = if_else(municipality == "ACATLAN DE PEREZ FIGUEROA", 20002, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION CUYOTEPEJI", 20004, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION IXTALTEPEC", 20005, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION NOCHIXTLAN", 20006, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION OCOTLAN", 20007, uniqueid),
         uniqueid = if_else(municipality == "AYOTZINTEPEC", 20009, uniqueid),
         uniqueid = if_else(municipality == "CHAHUITES", 20025, uniqueid),
         uniqueid = if_else(municipality == "CHALCATONGO DE HIDALGO", 20026, uniqueid),
         uniqueid = if_else(municipality == "CIENEGA DE ZIMATLAN", 20013, uniqueid),
         uniqueid = if_else(municipality == "CIUDAD IXTEPEC", 20014, uniqueid),
         uniqueid = if_else(municipality == "COSOLAPA", 20021, uniqueid),
         uniqueid = if_else(municipality == "CUILAPAM DE GUERRERO", 20023, uniqueid),
         uniqueid = if_else(municipality == "EJUTLA DE CRESPO", 20028, uniqueid),
         uniqueid = if_else(municipality == "BARRIO DE LA SOLEDAD, EL", 20010, uniqueid),
         uniqueid = if_else(municipality == "EL ESPINAL", 20030, uniqueid),
         uniqueid = if_else(municipality == "FRESNILLO DE TRUJANO", 20032, uniqueid),
         uniqueid = if_else(municipality == "GUADALUPE DE RAMIREZ", 20034, uniqueid),
         uniqueid = if_else(municipality == "H. CIUDAD DE TLAXIACO", 20397, uniqueid),
         uniqueid = if_else(municipality == "HUAJUAPAN DE LEON", 20039, uniqueid),
         uniqueid = if_else(municipality == "HUAUTEPEC", 20040, uniqueid),
         uniqueid = if_else(municipality == "HUAUTLA DE JIMENEZ", 20041, uniqueid),
         uniqueid = if_else(municipality == "JUCHITAN DE ZARAGOZA", 20043, uniqueid),
         uniqueid = if_else(municipality == "LOMA BONITA", 20044, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA OCOTLAN", 20049, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TEQUISISTLAN", 20052, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TLACOTEPEC", 20053, uniqueid),
         uniqueid = if_else(municipality == "MARISCALA DE JUAREZ", 20055, uniqueid),
         uniqueid = if_else(municipality == "MARTIRES DE TACUBAYA", 20056, uniqueid),
         uniqueid = if_else(municipality == "MATIAS ROMERO", 20057, uniqueid),
         uniqueid = if_else(municipality == "MIAHUATLAN DE PORFIRIO DIAZ", 20059, uniqueid),
         uniqueid = if_else(municipality == "OAXACA DE JUAREZ", 20067, uniqueid),
         uniqueid = if_else(municipality == "OCOTLAN DE MORELOS", 20068, uniqueid),
         uniqueid = if_else(municipality == "PINOTEPA DE DON LUIS", 20070, uniqueid),
         uniqueid = if_else(municipality == "PUTLA VILLA DE GUERRERO", 20073, uniqueid),
         uniqueid = if_else(municipality == "REFORMA DE PINEDA", 20075, uniqueid),
         uniqueid = if_else(municipality == "SALINA CRUZ", 20079, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN AMATENGO", 20080, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN ATENANGO", 20081, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES DINICUITI", 20089, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES HUAXPALTEPEC", 20090, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES ZAUTLA", 20102, uniqueid),
         uniqueid = if_else(municipality == "SAN ANTONINO CASTILLO VELASCO", 20103, uniqueid),
         uniqueid = if_else(municipality == "SAN BALTAZAR CHICHICAPAM", 20112, uniqueid),
         uniqueid = if_else(municipality == "SAN BARTOLOME AYAUTLA", 20116, uniqueid),
         uniqueid = if_else(municipality == "SAN BLAS ATEMPA", 20124, uniqueid),
         uniqueid = if_else(municipality == "SAN DIONISIO DEL MAR", 20130, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE JALAPA DE DIAZ", 20134, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE USILA", 20136, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO DEL MAR", 20141, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO IXHUATAN", 20143, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO TELIXTLAHUACA", 20150, uniqueid),
         uniqueid = if_else(municipality == "SAN JACINTO AMILPAS", 20157, uniqueid),
         uniqueid = if_else(municipality == "SAN JERONIMO SILACAYOAPILLA", 20160, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE CHILTEPEC", 20166, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE ESTANCIA GRANDE", 20168, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE INDEPENDENCIA", 20169, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE TENANGO", 20171, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA CUICATLAN", 20177, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA LO DE SOTO", 20180, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA SUCHITEPEC", 20181, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BTTA TLACOATZINTEPEC", 20182, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA TUXTEPEC", 20184, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA VALLE NACIONAL", 20559, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN CACAHUATEPEC", 20185, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COATZOSPAM", 20187, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COLORADO", 20188, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN GUICHICOVI", 20198, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN IHUALTEPEC", 20199, uniqueid),
         uniqueid = if_else(municipality == "SAN LORENZO", 20225, uniqueid),
         uniqueid = if_else(municipality == "SAN LUCAS OJITLAN", 20232, uniqueid),
         uniqueid = if_else(municipality == "SAN MARCOS ARTEAGA", 20237, uniqueid),
         uniqueid = if_else(municipality == "SAN MARTIN ZACATEPEC", 20245, uniqueid),
         uniqueid = if_else(municipality == "SAN MATEO RIO HONDO", 20254, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AHUEHUETITLAN", 20259, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AMATITLAN", 20261, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL SOYALTEPEC", 20278, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL TLACAMAMA", 20285, uniqueid),
         uniqueid = if_else(municipality == "SAN NICOLAS HIDALGO", 20290, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUITZO", 20294, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUIXTEPEC", 20295, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO VILLA DE MITLA", 20298, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO AMUZGOS", 20300, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO ATOYAC", 20302, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO COMITANCILLO", 20305, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUAMELULA", 20307, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUILOTEPEC", 20308, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO IXCATLAN", 20309, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO JICAYAN", 20312, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO MIXTEPEC", 20318, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO POCHUTLA", 20324, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TAPANATEPEC", 20327, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TUTUTEPEC", 20334, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO Y SAN PABLO TEPOSCOLULA", 20339, uniqueid),
         uniqueid = if_else(municipality == "SAN SEBASTIAN IXCAPA", 20345, uniqueid),
         uniqueid = if_else(municipality == "SANTA ANA ZEGACHE", 20360, uniqueid),
         uniqueid = if_else(municipality == "SANTA CATARINA JUQUILA", 20364, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ AMILPAS", 20375, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ ITUNDUJIA", 20377, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ TACACHE DE MINA", 20381, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ XOXOCOTLAN", 20385, uniqueid),
         uniqueid = if_else(municipality == "SANTA DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(municipality == "SANTA GERTRUDIS", 20387, uniqueid),
         uniqueid = if_else(municipality == "SANTA LUCIA DEL CAMINO", 20390, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA CORTIJOS", 20402, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUATULCO", 20413, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUAZOLOTITLAN", 20414, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA IPALAPA", 20415, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JACATEPEC", 20417, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JALAPA DEL MARQUES", 20418, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA MIXTEQUILLA", 20421, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA PETAPA", 20427, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TECOMAVACA", 20431, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEOPOXCO", 20434, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEXCATITLAN", 20436, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TONAMECA", 20439, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA XADANI", 20441, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA ZACATEPEC", 20447, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO AYUQUILILLA", 20455, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CACALOXTEPEC", 20456, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CHAZUMBA", 20459, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO HUAJOLOTITLAN", 20462, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JAMILTEPEC", 20467, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JUXTLAHUACA", 20469, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LAOLLAGA", 20472, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LLANO GRANDE", 20474, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO NILTEPEC", 20476, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO PINOTEPA NACIONAL", 20482, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO SUCHILQUITONGO", 20483, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAMAZOLA", 20484, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAPEXTLA", 20485, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TETEPEC", 20489, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ARMENTA", 20507, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO CHIHUITAN", 20508, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO PETAPA", 20513, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TEHUANTEPEC", 20515, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TONALA", 20520, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ZANATEPEC", 20525, uniqueid),
         uniqueid = if_else(municipality == "SILACAYOAPAM", 20537, uniqueid),
         uniqueid = if_else(municipality == "SOLEDAD ETLA", 20539, uniqueid),
         uniqueid = if_else(municipality == "TEOTITLAN DE FLORES MAGON", 20545, uniqueid),
         uniqueid = if_else(municipality == "TEZOATLAN DE SEGURA Y LUNA", 20549, uniqueid),
         uniqueid = if_else(municipality == "TLACOLULA DE MATAMOROS", 20551, uniqueid),
         uniqueid = if_else(municipality == "TRINIDAD ZAACHILA", 20555, uniqueid),
         uniqueid = if_else(municipality == "UNION HIDALGO", 20557, uniqueid),
         uniqueid = if_else(municipality == "VALERIO TRUJANO", 20558, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ETLA", 20338, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(str_detect(municipality, "SANTO DOMINGO TONALA"), 20520, uniqueid),
         uniqueid = if_else(municipality == "TAMAZULAPAM DEL PROGRESO", 20540, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ZAACHILA", 20565, uniqueid),
         uniqueid = if_else(municipality == "VILLA SOLA DE VEGA", 20277, uniqueid),
         uniqueid = if_else(municipality == "TEJUPAM DE LA UNION", 20486, uniqueid),
         uniqueid = if_else(municipality == "ZAPOTITLAN LAGUNAS", 20567, uniqueid),
         uniqueid = if_else(municipality == "ZIMATLAN DE ALVAREZ", 20568, uniqueid))

# Step 1: Assuming data is already read into R (from previous steps)
# Ensure `data_collapsed` is your processed dataframe
# If starting fresh, you can load the cleaned data again
# data_collapsed <- read_csv("Oaxaca_Seccion_2004_Cleaned.csv")

# Step 2: Create the 'valid' column by summing across specified columns
data_collapsed <- data_collapsed %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, PUP), na.rm = TRUE))

# Step 3: Calculate 'mun_' columns as sums by 'uniqueid'
data_collapsed <- data_collapsed %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN, PRI, PRD, PT, PVEM, PC, PUP, total, listanominal, valid), sum, na.rm = TRUE), .groups = 'drop')

# Step 4: Generate the 'inv_mun_' columns as 1 divided by 'mun_' columns
data_collapsed <- data_collapsed %>%
  mutate(across(c(PAN, PRI, PRD, PT, PVEM, PC, PUP), 
                .fns = ~ 1 / ., 
                .names = "inv_mun_{col}"))

# Step 5: Calculate 'mun_turnout' as 'mun_total' divided by 'mun_listanominal'
data_collapsed <- data_collapsed %>%
  mutate(mun_turnout = total / listanominal)

# Step 6: Rank the 'inv_mun_' columns using row ranking
data_collapsed <- data_collapsed %>%
  mutate(
    PAN_r = rowRanks(as.matrix(select(., starts_with("inv_mun_PAN"))), ties.method = "min"),
    PRI_r = rowRanks(as.matrix(select(., starts_with("inv_mun_PRI"))), ties.method = "min"),
    PRD_r = rowRanks(as.matrix(select(., starts_with("inv_mun_PRD"))), ties.method = "min"),
    PT_r = rowRanks(as.matrix(select(., starts_with("inv_mun_PT"))), ties.method = "min"),
    PVEM_r = rowRanks(as.matrix(select(., starts_with("inv_mun_PVEM"))), ties.method = "min"),
    PC_r = rowRanks(as.matrix(select(., starts_with("inv_mun_PC"))), ties.method = "min"),
    PUP_r = rowRanks(as.matrix(select(., starts_with("inv_mun_PUP"))), ties.method = "min")
  )

# Step 7: Determine the winner based on ranking
data_collapsed <- data_collapsed %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_r == 1 ~ "PRI",
      PRD_r == 1 ~ "PRD",
      PT_r == 1 ~ "PT",
      PVEM_r == 1 ~ "PVEM",
      PC_r == 1 ~ "PC",
      PUP_r == 1 ~ "PUP",
      TRUE ~ NA_character_
    )
  )

# Step 8: Drop unnecessary ranking columns
data_collapsed <- data_collapsed %>%
  select(-ends_with("_r"))

# Step 9: Add year and month columns
data_collapsed <- data_collapsed %>%
  mutate(
    year = 2004,
    month = "October"
  )

# Step 10: Save the data to a file
# Write the processed data to a new CSV file (or another file format)
write_csv(data_collapsed, "Oaxaca_2004.csv")

# Step 1: Load the data
data <- read_csv("Ayu_Seccion_2007.csv")

# Step 2: Rename columns
data <- data %>%
  rename(
    municipality = municipio,
    section = seccion,
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
    PVEM = pvem,
    PC = pc,
    PUP = pup,
    PANAL = pna,
    PAS = pasdc
  )

# Step 3: Drop rows where `municipality` or `section` is missing, or if `total` is missing or zero
data <- data %>%
  filter(municipality != "" & section != "", !is.na(total) & total != 0)

# Step 4: Convert the necessary columns to numeric (destring operation)
data <- data %>%
  mutate(across(listanominal:total, as.numeric))

# Step 5: Collapse the data by `municipality` and `section`, summing relevant columns
data_collapsed <- data %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:total, sum, na.rm = TRUE), .groups = "drop")

# Step 6: Generate 'turnout' column (total votes / list of nominal voters)
data_collapsed <- data_collapsed %>%
  mutate(turnout = total / listanominal)

# Step 7: Drop the columns `noregistrados` and `nulos` if they exist
data_collapsed <- data_collapsed %>%
  select(-noregistrados, -nulos, everything())

# Assuming `data_collapsed` is your dataframe 
data_collapsed <- data_collapsed %>%
  mutate(uniqueid = 0,  # Initialize the uniqueid column
         
         # Replace uniqueid based on the municipality names
         uniqueid = if_else(municipality == "ACATLAN DE PEREZ FIGUEROA", 20002, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION CUYOTEPEJI", 20004, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION IXTALTEPEC", 20005, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION NOCHIXTLAN", 20006, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION OCOTLAN", 20007, uniqueid),
         uniqueid = if_else(municipality == "AYOTZINTEPEC", 20009, uniqueid),
         uniqueid = if_else(municipality == "CHAHUITES", 20025, uniqueid),
         uniqueid = if_else(municipality == "CHALCATONGO DE HIDALGO", 20026, uniqueid),
         uniqueid = if_else(municipality == "CIENEGA DE ZIMATLAN", 20013, uniqueid),
         uniqueid = if_else(municipality == "CIUDAD IXTEPEC", 20014, uniqueid),
         uniqueid = if_else(municipality == "COSOLAPA", 20021, uniqueid),
         uniqueid = if_else(municipality == "CUILAPAM DE GUERRERO", 20023, uniqueid),
         uniqueid = if_else(municipality == "EJUTLA DE CRESPO", 20028, uniqueid),
         uniqueid = if_else(municipality == "BARRIO DE LA SOLEDAD, EL", 20010, uniqueid),
         uniqueid = if_else(municipality == "EL ESPINAL", 20030, uniqueid),
         uniqueid = if_else(municipality == "FRESNILLO DE TRUJANO", 20032, uniqueid),
         uniqueid = if_else(municipality == "GUADALUPE DE RAMIREZ", 20034, uniqueid),
         uniqueid = if_else(municipality == "H. CIUDAD DE TLAXIACO", 20397, uniqueid),
         uniqueid = if_else(municipality == "HUAJUAPAN DE LEON", 20039, uniqueid),
         uniqueid = if_else(municipality == "HUAUTEPEC", 20040, uniqueid),
         uniqueid = if_else(municipality == "HUAUTLA DE JIMENEZ", 20041, uniqueid),
         uniqueid = if_else(municipality == "JUCHITAN DE ZARAGOZA", 20043, uniqueid),
         uniqueid = if_else(municipality == "LOMA BONITA", 20044, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA OCOTLAN", 20049, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TEQUISISTLAN", 20052, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TLACOTEPEC", 20053, uniqueid),
         uniqueid = if_else(municipality == "MARISCALA DE JUAREZ", 20055, uniqueid),
         uniqueid = if_else(municipality == "MARTIRES DE TACUBAYA", 20056, uniqueid),
         uniqueid = if_else(municipality == "MATIAS ROMERO", 20057, uniqueid),
         uniqueid = if_else(municipality == "MIAHUATLAN DE PORFIRIO DIAZ", 20059, uniqueid),
         uniqueid = if_else(municipality == "OAXACA DE JUAREZ", 20067, uniqueid),
         uniqueid = if_else(municipality == "OCOTLAN DE MORELOS", 20068, uniqueid),
         uniqueid = if_else(municipality == "PINOTEPA DE DON LUIS", 20070, uniqueid),
         uniqueid = if_else(municipality == "PUTLA VILLA DE GUERRERO", 20073, uniqueid),
         uniqueid = if_else(municipality == "REFORMA DE PINEDA", 20075, uniqueid),
         uniqueid = if_else(municipality == "SALINA CRUZ", 20079, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN AMATENGO", 20080, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN ATENANGO", 20081, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES DINICUITI", 20089, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES HUAXPALTEPEC", 20090, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES ZAUTLA", 20102, uniqueid),
         uniqueid = if_else(municipality == "SAN ANTONINO CASTILLO VELASCO", 20103, uniqueid),
         uniqueid = if_else(municipality == "SAN BALTAZAR CHICHICAPAM", 20112, uniqueid),
         uniqueid = if_else(municipality == "SAN BARTOLOME AYAUTLA", 20116, uniqueid),
         uniqueid = if_else(municipality == "SAN BLAS ATEMPA", 20124, uniqueid),
         uniqueid = if_else(municipality == "SAN DIONISIO DEL MAR", 20130, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE JALAPA DE DIAZ", 20134, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE USILA", 20136, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO DEL MAR", 20141, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO IXHUATAN", 20143, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO TELIXTLAHUACA", 20150, uniqueid),
         uniqueid = if_else(municipality == "SAN JACINTO AMILPAS", 20157, uniqueid),
         uniqueid = if_else(municipality == "SAN JERONIMO SILACAYOAPILLA", 20160, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE CHILTEPEC", 20166, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE ESTANCIA GRANDE", 20168, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE INDEPENDENCIA", 20169, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE TENANGO", 20171, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA CUICATLAN", 20177, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA LO DE SOTO", 20180, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA SUCHITEPEC", 20181, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BTTA TLACOATZINTEPEC", 20182, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA TUXTEPEC", 20184, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA VALLE NACIONAL", 20559, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN CACAHUATEPEC", 20185, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COATZOSPAM", 20187, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COLORADO", 20188, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN GUICHICOVI", 20198, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN IHUALTEPEC", 20199, uniqueid),
         uniqueid = if_else(municipality == "SAN LORENZO", 20225, uniqueid),
         uniqueid = if_else(municipality == "SAN LUCAS OJITLAN", 20232, uniqueid),
         uniqueid = if_else(municipality == "SAN MARCOS ARTEAGA", 20237, uniqueid),
         uniqueid = if_else(municipality == "SAN MARTIN ZACATEPEC", 20245, uniqueid),
         uniqueid = if_else(municipality == "SAN MATEO RIO HONDO", 20254, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AHUEHUETITLAN", 20259, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AMATITLAN", 20261, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL SOYALTEPEC", 20278, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL TLACAMAMA", 20285, uniqueid),
         uniqueid = if_else(municipality == "SAN NICOLAS HIDALGO", 20290, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUITZO", 20294, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUIXTEPEC", 20295, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO VILLA DE MITLA", 20298, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO AMUZGOS", 20300, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO ATOYAC", 20302, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO COMITANCILLO", 20305, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUAMELULA", 20307, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUILOTEPEC", 20308, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO IXCATLAN", 20309, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO JICAYAN", 20312, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO MIXTEPEC", 20318, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO POCHUTLA", 20324, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TAPANATEPEC", 20327, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TUTUTEPEC", 20334, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO Y SAN PABLO TEPOSCOLULA", 20339, uniqueid),
         uniqueid = if_else(municipality == "SAN SEBASTIAN IXCAPA", 20345, uniqueid),
         uniqueid = if_else(municipality == "SANTA ANA ZEGACHE", 20360, uniqueid),
         uniqueid = if_else(municipality == "SANTA CATARINA JUQUILA", 20364, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ AMILPAS", 20375, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ ITUNDUJIA", 20377, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ TACACHE DE MINA", 20381, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ XOXOCOTLAN", 20385, uniqueid),
         uniqueid = if_else(municipality == "SANTA DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(municipality == "SANTA GERTRUDIS", 20387, uniqueid),
         uniqueid = if_else(municipality == "SANTA LUCIA DEL CAMINO", 20390, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA CORTIJOS", 20402, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUATULCO", 20413, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUAZOLOTITLAN", 20414, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA IPALAPA", 20415, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JACATEPEC", 20417, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JALAPA DEL MARQUES", 20418, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA MIXTEQUILLA", 20421, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA PETAPA", 20427, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TECOMAVACA", 20431, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEOPOXCO", 20434, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEXCATITLAN", 20436, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TONAMECA", 20439, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA XADANI", 20441, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA ZACATEPEC", 20447, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO AYUQUILILLA", 20455, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CACALOXTEPEC", 20456, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CHAZUMBA", 20459, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO HUAJOLOTITLAN", 20462, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JAMILTEPEC", 20467, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JUXTLAHUACA", 20469, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LAOLLAGA", 20472, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LLANO GRANDE", 20474, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO NILTEPEC", 20476, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO PINOTEPA NACIONAL", 20482, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO SUCHILQUITONGO", 20483, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAMAZOLA", 20484, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAPEXTLA", 20485, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TETEPEC", 20489, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ARMENTA", 20507, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO CHIHUITAN", 20508, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO PETAPA", 20513, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TEHUANTEPEC", 20515, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TONALA", 20520, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ZANATEPEC", 20525, uniqueid),
         uniqueid = if_else(municipality == "SILACAYOAPAM", 20537, uniqueid),
         uniqueid = if_else(municipality == "SOLEDAD ETLA", 20539, uniqueid),
         uniqueid = if_else(municipality == "TEOTITLAN DE FLORES MAGON", 20545, uniqueid),
         uniqueid = if_else(municipality == "TEZOATLAN DE SEGURA Y LUNA", 20549, uniqueid),
         uniqueid = if_else(municipality == "TLACOLULA DE MATAMOROS", 20551, uniqueid),
         uniqueid = if_else(municipality == "TRINIDAD ZAACHILA", 20555, uniqueid),
         uniqueid = if_else(municipality == "UNION HIDALGO", 20557, uniqueid),
         uniqueid = if_else(municipality == "VALERIO TRUJANO", 20558, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ETLA", 20338, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(str_detect(municipality, "SANTO DOMINGO TONALA"), 20520, uniqueid),
         uniqueid = if_else(municipality == "TAMAZULAPAM DEL PROGRESO", 20540, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ZAACHILA", 20565, uniqueid),
         uniqueid = if_else(municipality == "VILLA SOLA DE VEGA", 20277, uniqueid),
         uniqueid = if_else(municipality == "TEJUPAM DE LA UNION", 20486, uniqueid),
         uniqueid = if_else(municipality == "ZAPOTITLAN LAGUNAS", 20567, uniqueid),
         uniqueid = if_else(municipality == "ZIMATLAN DE ALVAREZ", 20568, uniqueid))

# Assuming the dataset is already loaded in `data_collapsed` from previous steps

# Step 1: Generate 'valid' votes total
data_collapsed <- data_collapsed %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PC, PUP, PANAL, PAS)), na.rm = TRUE))

# Step 2: Calculate `mun_*` columns (sum of votes by unique id)
data_collapsed <- data_collapsed %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN, PRI, PRD, PT, PVEM, PC, PUP, PANAL, PAS, total, listanominal, valid), 
                list(mun = sum), .names = "mun_{col}"))

# Step 3: Generate inverse of `mun_*` columns
data_collapsed <- data_collapsed %>%
  mutate(across(starts_with("mun_"), ~ 1 / ., .names = "inv_{col}"))

# Step 4: Generate 'mun_turnout' (municipal turnout)
data_collapsed <- data_collapsed %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 5: Rank the inverse municipal votes and generate ranking columns
data_collapsed <- data_collapsed %>%
  mutate(across(starts_with("inv_mun_"), rank, ties.method = "first", .names = "{col}_r"))

# Step 6: Generate the 'winner' column based on rankings
data_collapsed <- data_collapsed %>%
  mutate(
    winner = case_when(
      inv_mun_PAN_r == 1 ~ "PAN",
      inv_mun_PRI_r == 1 ~ "PRI",
      inv_mun_PRD_r == 1 ~ "PRD",
      inv_mun_PT_r == 1 ~ "PT",
      inv_mun_PVEM_r == 1 ~ "PVEM",
      inv_mun_PC_r == 1 ~ "PC",
      inv_mun_PUP_r == 1 ~ "PUP",
      inv_mun_PANAL_r == 1 ~ "PANAL",
      inv_mun_PAS_r == 1 ~ "PAS"
    )
  )

# Step 7: Clean up ranking columns
data_collapsed <- data_collapsed %>%
  select(-starts_with("inv_mun_"), -starts_with("mun_"))

# Step 8: Add 'year' and 'month' columns
data_collapsed <- data_collapsed %>%
  mutate(year = 2007, month = "October")

# Step 9: Sort by section and save the dataset
data_collapsed <- data_collapsed %>%
  arrange(section)

# Save the dataset
write_dta(data_collapsed, "Oaxaca_2007.dta")

# Step 1: Load the dataset
data_2010 <- read_dta("Ayu_Seccion_2010.dta")

# Step 2: Rename columns
data_2010 <- data_2010 %>%
  rename(municipality = municipio, section = seccion)

# Step 3: Drop rows where municipality or section is missing
data_2010 <- data_2010 %>%
  filter(municipality != "" & !is.na(section)) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert relevant columns from string to numeric
data_2010 <- data_2010 %>%
  mutate(across(listanominal:total, as.numeric))

# Step 5: Drop incorrect total and calculate a new total
data_2010 <- data_2010 %>%
  select(-total) %>%  # Drop the existing 'total' column
  rowwise() %>% 
  mutate(total = sum(c_across(pan:nulos), na.rm = TRUE)) %>%
  ungroup()

# Step 6: Collapse the data by summing votes and grouping by municipality, section, coalicion_1, and coalicion_2
data_collapsed <- data_2010 %>%
  group_by(municipality, section, coalicion_1, coalicion_2) %>%
  summarise(across(listanominal:pna, sum, na.rm = TRUE),
            total = sum(total, na.rm = TRUE), .groups = "drop")

# Optional: Save the dataset
write_dta(data_collapsed, "Ayu_Seccion_2010_collapsed.dta")

# Load necessary libraries
library(dplyr)

# Assuming 'data_collapsed' is the dataset we are working with

# Step 1: Create 'pan_prd_pt_pc' column and update based on 'coalicion_1'
data_collapsed <- data_collapsed %>%
  mutate(pan_prd_pt_pc = ifelse(coalicion_1 == "PAN-PRD-PT-PC", pan + prd + pt + pc, 0)) %>%
  mutate(
    pan = ifelse(coalicion_1 == "PAN-PRD-PT-PC", 0, pan),
    prd = ifelse(coalicion_1 == "PAN-PRD-PT-PC", 0, prd),
    pt = ifelse(coalicion_1 == "PAN-PRD-PT-PC", 0, pt),
    pc = ifelse(coalicion_1 == "PAN-PRD-PT-PC", 0, pc)
  )

# Step 2: Drop 'pan', 'prd', 'pt', and 'pc' columns
data_collapsed <- data_collapsed %>%
  select(-pan, -prd, -pt, -pc)

# Step 3: Create 'pri_pvem' column and update based on 'coalicion_2'
data_collapsed <- data_collapsed %>%
  mutate(pri_pvem = ifelse(coalicion_2 == "PRI-PVEM", pri + pvem, 0)) %>%
  mutate(
    pri = ifelse(coalicion_2 == "PRI-PVEM", 0, pri),
    pvem = ifelse(coalicion_2 == "PRI-PVEM", 0, pvem)
  )

# Step 4: Drop 'pri' and 'pvem' columns
data_collapsed <- data_collapsed %>%
  select(-pri, -pvem)

# Step 5: Drop 'coalicion_1' and 'coalicion_2' columns
data_collapsed <- data_collapsed %>%
  select(-coalicion_1, -coalicion_2)

# Step 6: (Optional) Summarize to verify results if needed
summary(data_collapsed$pan_prd_pt_pc)
summary(data_collapsed$pri_pvem)

# Step 7: (Optional) Save the final dataset if required
write_dta(data_collapsed, "Ayu_Seccion_2010_modified.dta")


data_collapsed <- data_collapsed %>%
  rename(
    PAN_PRD_PT_PC = pan_prd_pt_pc,
    PRI_PVEM = pri_pvem,
    PUP = pup,
    PANAL = pna)

# Creating the new 'turnout' variable
data_collapsed <- data_collapsed %>%
  mutate(turnout = total / listanominal)

# Assuming `data_collapsed` is your dataframe 
data_collapsed <- data_collapsed %>%
  mutate(uniqueid = 0,  # Initialize the uniqueid column
         
         # Replace uniqueid based on the municipality names
         uniqueid = if_else(municipality == "ACATLAN DE PEREZ FIGUEROA", 20002, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION CUYOTEPEJI", 20004, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION IXTALTEPEC", 20005, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION NOCHIXTLAN", 20006, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION OCOTLAN", 20007, uniqueid),
         uniqueid = if_else(municipality == "AYOTZINTEPEC", 20009, uniqueid),
         uniqueid = if_else(municipality == "CHAHUITES", 20025, uniqueid),
         uniqueid = if_else(municipality == "CHALCATONGO DE HIDALGO", 20026, uniqueid),
         uniqueid = if_else(municipality == "CIENEGA DE ZIMATLAN", 20013, uniqueid),
         uniqueid = if_else(municipality == "CIUDAD IXTEPEC", 20014, uniqueid),
         uniqueid = if_else(municipality == "COSOLAPA", 20021, uniqueid),
         uniqueid = if_else(municipality == "CUILAPAM DE GUERRERO", 20023, uniqueid),
         uniqueid = if_else(municipality == "EJUTLA DE CRESPO", 20028, uniqueid),
         uniqueid = if_else(municipality == "BARRIO DE LA SOLEDAD, EL", 20010, uniqueid),
         uniqueid = if_else(municipality == "EL ESPINAL", 20030, uniqueid),
         uniqueid = if_else(municipality == "FRESNILLO DE TRUJANO", 20032, uniqueid),
         uniqueid = if_else(municipality == "GUADALUPE DE RAMIREZ", 20034, uniqueid),
         uniqueid = if_else(municipality == "H. CIUDAD DE TLAXIACO", 20397, uniqueid),
         uniqueid = if_else(municipality == "HUAJUAPAN DE LEON", 20039, uniqueid),
         uniqueid = if_else(municipality == "HUAUTEPEC", 20040, uniqueid),
         uniqueid = if_else(municipality == "HUAUTLA DE JIMENEZ", 20041, uniqueid),
         uniqueid = if_else(municipality == "JUCHITAN DE ZARAGOZA", 20043, uniqueid),
         uniqueid = if_else(municipality == "LOMA BONITA", 20044, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA OCOTLAN", 20049, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TEQUISISTLAN", 20052, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TLACOTEPEC", 20053, uniqueid),
         uniqueid = if_else(municipality == "MARISCALA DE JUAREZ", 20055, uniqueid),
         uniqueid = if_else(municipality == "MARTIRES DE TACUBAYA", 20056, uniqueid),
         uniqueid = if_else(municipality == "MATIAS ROMERO", 20057, uniqueid),
         uniqueid = if_else(municipality == "MIAHUATLAN DE PORFIRIO DIAZ", 20059, uniqueid),
         uniqueid = if_else(municipality == "OAXACA DE JUAREZ", 20067, uniqueid),
         uniqueid = if_else(municipality == "OCOTLAN DE MORELOS", 20068, uniqueid),
         uniqueid = if_else(municipality == "PINOTEPA DE DON LUIS", 20070, uniqueid),
         uniqueid = if_else(municipality == "PUTLA VILLA DE GUERRERO", 20073, uniqueid),
         uniqueid = if_else(municipality == "REFORMA DE PINEDA", 20075, uniqueid),
         uniqueid = if_else(municipality == "SALINA CRUZ", 20079, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN AMATENGO", 20080, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN ATENANGO", 20081, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES DINICUITI", 20089, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES HUAXPALTEPEC", 20090, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES ZAUTLA", 20102, uniqueid),
         uniqueid = if_else(municipality == "SAN ANTONINO CASTILLO VELASCO", 20103, uniqueid),
         uniqueid = if_else(municipality == "SAN BALTAZAR CHICHICAPAM", 20112, uniqueid),
         uniqueid = if_else(municipality == "SAN BARTOLOME AYAUTLA", 20116, uniqueid),
         uniqueid = if_else(municipality == "SAN BLAS ATEMPA", 20124, uniqueid),
         uniqueid = if_else(municipality == "SAN DIONISIO DEL MAR", 20130, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE JALAPA DE DIAZ", 20134, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE USILA", 20136, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO DEL MAR", 20141, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO IXHUATAN", 20143, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO TELIXTLAHUACA", 20150, uniqueid),
         uniqueid = if_else(municipality == "SAN JACINTO AMILPAS", 20157, uniqueid),
         uniqueid = if_else(municipality == "SAN JERONIMO SILACAYOAPILLA", 20160, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE CHILTEPEC", 20166, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE ESTANCIA GRANDE", 20168, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE INDEPENDENCIA", 20169, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE TENANGO", 20171, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA CUICATLAN", 20177, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA LO DE SOTO", 20180, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA SUCHITEPEC", 20181, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BTTA TLACOATZINTEPEC", 20182, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA TUXTEPEC", 20184, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA VALLE NACIONAL", 20559, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN CACAHUATEPEC", 20185, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COATZOSPAM", 20187, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COLORADO", 20188, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN GUICHICOVI", 20198, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN IHUALTEPEC", 20199, uniqueid),
         uniqueid = if_else(municipality == "SAN LORENZO", 20225, uniqueid),
         uniqueid = if_else(municipality == "SAN LUCAS OJITLAN", 20232, uniqueid),
         uniqueid = if_else(municipality == "SAN MARCOS ARTEAGA", 20237, uniqueid),
         uniqueid = if_else(municipality == "SAN MARTIN ZACATEPEC", 20245, uniqueid),
         uniqueid = if_else(municipality == "SAN MATEO RIO HONDO", 20254, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AHUEHUETITLAN", 20259, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AMATITLAN", 20261, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL SOYALTEPEC", 20278, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL TLACAMAMA", 20285, uniqueid),
         uniqueid = if_else(municipality == "SAN NICOLAS HIDALGO", 20290, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUITZO", 20294, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUIXTEPEC", 20295, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO VILLA DE MITLA", 20298, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO AMUZGOS", 20300, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO ATOYAC", 20302, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO COMITANCILLO", 20305, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUAMELULA", 20307, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUILOTEPEC", 20308, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO IXCATLAN", 20309, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO JICAYAN", 20312, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO MIXTEPEC", 20318, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO POCHUTLA", 20324, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TAPANATEPEC", 20327, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TUTUTEPEC", 20334, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO Y SAN PABLO TEPOSCOLULA", 20339, uniqueid),
         uniqueid = if_else(municipality == "SAN SEBASTIAN IXCAPA", 20345, uniqueid),
         uniqueid = if_else(municipality == "SANTA ANA ZEGACHE", 20360, uniqueid),
         uniqueid = if_else(municipality == "SANTA CATARINA JUQUILA", 20364, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ AMILPAS", 20375, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ ITUNDUJIA", 20377, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ TACACHE DE MINA", 20381, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ XOXOCOTLAN", 20385, uniqueid),
         uniqueid = if_else(municipality == "SANTA DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(municipality == "SANTA GERTRUDIS", 20387, uniqueid),
         uniqueid = if_else(municipality == "SANTA LUCIA DEL CAMINO", 20390, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA CORTIJOS", 20402, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUATULCO", 20413, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUAZOLOTITLAN", 20414, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA IPALAPA", 20415, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JACATEPEC", 20417, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JALAPA DEL MARQUES", 20418, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA MIXTEQUILLA", 20421, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA PETAPA", 20427, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TECOMAVACA", 20431, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEOPOXCO", 20434, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEXCATITLAN", 20436, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TONAMECA", 20439, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA XADANI", 20441, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA ZACATEPEC", 20447, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO AYUQUILILLA", 20455, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CACALOXTEPEC", 20456, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CHAZUMBA", 20459, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO HUAJOLOTITLAN", 20462, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JAMILTEPEC", 20467, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JUXTLAHUACA", 20469, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LAOLLAGA", 20472, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LLANO GRANDE", 20474, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO NILTEPEC", 20476, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO PINOTEPA NACIONAL", 20482, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO SUCHILQUITONGO", 20483, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAMAZOLA", 20484, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAPEXTLA", 20485, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TETEPEC", 20489, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ARMENTA", 20507, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO CHIHUITAN", 20508, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO PETAPA", 20513, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TEHUANTEPEC", 20515, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TONALA", 20520, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ZANATEPEC", 20525, uniqueid),
         uniqueid = if_else(municipality == "SILACAYOAPAM", 20537, uniqueid),
         uniqueid = if_else(municipality == "SOLEDAD ETLA", 20539, uniqueid),
         uniqueid = if_else(municipality == "TEOTITLAN DE FLORES MAGON", 20545, uniqueid),
         uniqueid = if_else(municipality == "TEZOATLAN DE SEGURA Y LUNA", 20549, uniqueid),
         uniqueid = if_else(municipality == "TLACOLULA DE MATAMOROS", 20551, uniqueid),
         uniqueid = if_else(municipality == "TRINIDAD ZAACHILA", 20555, uniqueid),
         uniqueid = if_else(municipality == "UNION HIDALGO", 20557, uniqueid),
         uniqueid = if_else(municipality == "VALERIO TRUJANO", 20558, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ETLA", 20338, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(str_detect(municipality, "SANTO DOMINGO TONALA"), 20520, uniqueid),
         uniqueid = if_else(municipality == "TAMAZULAPAM DEL PROGRESO", 20540, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ZAACHILA", 20565, uniqueid),
         uniqueid = if_else(municipality == "VILLA SOLA DE VEGA", 20277, uniqueid),
         uniqueid = if_else(municipality == "TEJUPAM DE LA UNION", 20486, uniqueid),
         uniqueid = if_else(municipality == "ZAPOTITLAN LAGUNAS", 20567, uniqueid),
         uniqueid = if_else(municipality == "ZIMATLAN DE ALVAREZ", 20568, uniqueid))

# Calculate row-wise totals across specific columns (equivalent to `egen rowtotal`)
data_collapsed <- data_collapsed %>%
  mutate(valid = rowSums(select(., PAN_PRD_PT_PC, PRI_PVEM, PUP, PANAL)))

# Loop over specified variables to create 'mun_' and 'inv_mun_' variables
variables <- c("PAN_PRD_PT_PC", "PRI_PVEM", "PUP", "PANAL", "total", "listanominal", "valid")
for (var in variables) {
  # Group by uniqueid and sum across each variable, then create inverse
  data_collapsed <- data_collapsed %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(!!paste0("inv_mun_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Generate mun_turnout (equivalent to `gen mun_turnout`)
data_collapsed <- data_collapsed %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Rank the inverse values (equivalent to `rowranks`)
data_collapsed <- data_collapsed %>%
  mutate(
    PAN_PRD_PT_PC_r = rank(-inv_mun_PAN_PRD_PT_PC, ties.method = "min"),
    PRI_PVEM_r = rank(-inv_mun_PRI_PVEM, ties.method = "min"),
    PUP_r = rank(-inv_mun_PUP, ties.method = "min"),
    PANAL_r = rank(-inv_mun_PANAL, ties.method = "min")
  )

# Determine the winner (equivalent to multiple `replace winner`)
data_collapsed <- data_collapsed %>%
  mutate(
    winner = case_when(
      PAN_PRD_PT_PC_r == 1 ~ "PAN_PRD_PT_PC",
      PRI_PVEM_r == 1 ~ "PRI_PVEM",
      PUP_r == 1 ~ "PUP",
      PANAL_r == 1 ~ "PANAL",
      TRUE ~ winner
    )
  )

# Drop the ranking columns (equivalent to `drop *_r`)
data_collapsed <- data_collapsed %>%
  select(-ends_with("_r"))

# Add year and month columns
data_collapsed <- data_collapsed %>%
  mutate(
    year = 2010,
    month = "July"
  )

# Sorting by 'section' (equivalent to `sort section`)
data_collapsed <- data_collapsed %>%
  arrange(section)

# Saving the data (equivalent to `save Oaxaca_2010.dta, replace`)
write.csv(data_collapsed, "Oaxaca_2010.csv", row.names = FALSE)  # Saving as CSV. Use a different function if you prefer another format.

# Load Oaxaca_1998 data
data <- read_dta("Oaxaca_1998.dta")

# Append other datasets
data_2001 <- read_dta("Oaxaca_2001.dta")
data_2004 <- read_dta("Oaxaca_2004.dta")
data_2007 <- read_dta("Oaxaca_2007.dta")
data_2010 <- read_dta("Oaxaca_2010.dta")

# Append all data
data <- bind_rows(data, data_2001, data_2004, data_2007, data_2010)

# Remove the original files (equivalent to erase)
file.remove("Oaxaca_1998.dta", "Oaxaca_2001.dta", "Oaxaca_2004.dta", "Oaxaca_2007.dta", "Oaxaca_2010.dta")

# Check for missing 'winner' values (equivalent to `tab winner, missing`)
table(data$winner, useNA = "ifany")

# Browse rows with missing 'winner' (equivalent to `br if winner == ""`)
missing_winner <- data %>% filter(winner == "")

# Browse specific cases (equivalent to `br if municipality == "SANTA CRUZ ITUNDUJIA" & year == 2007`)
santa_cruz_2007 <- data %>% filter(municipality == "SANTA CRUZ ITUNDUJIA", year == 2007)

# Browse another specific case (equivalent to `br if municipality == "SAN JOSE ESTANCIA GRANDE" & year == 2010`)
san_jose_2010 <- data %>% filter(municipality == "SAN JOSE ESTANCIA GRANDE", year == 2010)

# Create PAN_winner (equivalent to `gen PAN_winner = 0` and `replace PAN_winner = 1`)
data <- data %>%
  mutate(
    PAN_winner = ifelse(str_detect(winner, "PAN") & !str_detect(winner, "PANAL"), 1, 0),
    winner_counter = PAN_winner
  )

# Create winner variables for each party and update the counter
parties <- c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "PD", "PSD", "PAS", "PSN", "PartidoMexicoPosible", "CDPPN", "PUP")
for (party in parties) {
  data <- data %>%
    mutate(!!paste0(party, "_winner") := ifelse(str_detect(winner, party), 1, 0)) %>%
    mutate(winner_counter = winner_counter + !!sym(paste0(party, "_winner")))
}

# Tabulate winner_counter (equivalent to `tab winner_counter`)
table(data$winner_counter)

# Count observations with no winner (equivalent to `count if winner_counter == 0`)
count_no_winner <- data %>% filter(winner_counter == 0) %>% nrow()

# Drop duplicated section (equivalent to `drop if section == 416`)
data <- data %>% filter(section != 416)

# Modify the `municipality` column to be a character vector of length 244 (equivalent to `recast str244 municipality`)
data <- data %>%
  mutate(municipality = str_pad(as.character(municipality), width = 244, side = "right"))

# Save the dataset (equivalent to `saveold "..\Oaxaca_ALL.dta", replace version(12)`)
write_dta(data, "../Oaxaca_ALL.dta", version = 12)

# Function to process sheets
process_sheet <- function(sheetname, file) {
  data <- read_excel(file, sheet = sheetname, col_names = TRUE)
  
  # Drop rows where B is empty or "DTTO"
  data <- data %>%
    filter(!(B == "" | B == "DTTO"))
  
  # Replace values in variables I to Y
  data <- data %>%
    mutate(across(I:Y, ~ replace(., . == "", "0"))) %>%
    mutate(across(I:Y, ~ replace(., . %in% c("N.P.", "P.N", "N.P"), "")))
  
  # Save processed data (adjust path if necessary)
  write_dta(data, paste0(sheetname, ".dta"))
}

# Process all sheets in the first Excel file
file1 <- "Votacion Concejales 2013 I_XIV.xlsx"
sheets1 <- excel_sheets(file1)
lapply(sheets1, process_sheet, file = file1)

# Process all sheets in the second Excel file
file2 <- "Votacion Concejales 2013 XV_XXV.xlsx"
sheets2 <- excel_sheets(file2)
lapply(sheets2, process_sheet, file = file2)

# Load all datasets and append them
files_to_append <- c("Dtto I.dta", "Dtto II.dta", "Dtto IV.dta", "Dtto V.dta", "Dtto VI.dta", 
                     "Dtto VII.dta", "Dtto VIII.dta", "IX.dta", "Dtto X.dta", "Dtto XI.dta", 
                     "Dtto XII.dta", "Dtto XIII.dta", "Dtto XIV.dta", "Dtto XV.dta", 
                     "Dtto XVI.dta", "Dtto XVII.dta", "Dtto XVIII.dta", "Dtto XIX.dta", 
                     "Dtto XXI.dta", "Dtto XXII.dta", "Dtto XXIII.dta", "Dtto XXIV.dta", "Dtto XXV.dta")

data <- bind_rows(lapply(files_to_append, read_dta))

# Remove files after appending
file.remove(files_to_append)

# Rename columns
data <- data %>%
  rename(municipality = D, section = F)

# Clean section column
data <- data %>%
  mutate(section = str_replace_all(section, "\\*", "")) %>%
  filter(!(section == "Sección" | section == "" | H == "Casilla anulada *" | str_detect(I, "asilla")))

# Convert all columns to numeric (equivalent to `destring _all`)
data <- data %>%
  mutate(across(everything(), as.numeric, .names = "converted_{col}"))

# Rename specific columns
data <- data %>%
  rename(listanominal = H, PMC = N, PUP = O, PANAL = P, PSD = Q)

# Generate PAN_PRD_PT and PRI_PVEM variables
data <- data %>%
  mutate(PAN_PRD_PT = I + K + M + R + S + T + U,
         PRI_PVEM = J + L + V)

# Drop unwanted columns
data <- data %>%
  select(-I:-M, -R:-X)

# Rename total column
data <- data %>%
  rename(total = Y)

# Standardize municipality names (remove accents and replace ñ)
data <- data %>%
  mutate(municipality = str_to_upper(municipality)) %>%
  mutate(municipality = str_replace_all(municipality, c("á" = "A", "é" = "E", "í" = "I", "ó" = "O", "ú" = "U", "ñ" = "N")))

# Final data is now in 'data' DataFrame. You can now save it.
write_dta(data, "Final_Oaxaca_Data.dta")

# Assuming `data_collapsed` is your dataframe 
data <- data %>%
  mutate(uniqueid = 0,  # Initialize the uniqueid column
         
         # Replace uniqueid based on the municipality names
         uniqueid = if_else(municipality == "ACATLAN DE PEREZ FIGUEROA", 20002, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION CUYOTEPEJI", 20004, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION IXTALTEPEC", 20005, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION NOCHIXTLAN", 20006, uniqueid),
         uniqueid = if_else(municipality == "ASUNCION OCOTLAN", 20007, uniqueid),
         uniqueid = if_else(municipality == "AYOTZINTEPEC", 20009, uniqueid),
         uniqueid = if_else(municipality == "CHAHUITES", 20025, uniqueid),
         uniqueid = if_else(municipality == "CHALCATONGO DE HIDALGO", 20026, uniqueid),
         uniqueid = if_else(municipality == "CIENEGA DE ZIMATLAN", 20013, uniqueid),
         uniqueid = if_else(municipality == "CIUDAD IXTEPEC", 20014, uniqueid),
         uniqueid = if_else(municipality == "COSOLAPA", 20021, uniqueid),
         uniqueid = if_else(municipality == "CUILAPAM DE GUERRERO", 20023, uniqueid),
         uniqueid = if_else(municipality == "EJUTLA DE CRESPO", 20028, uniqueid),
         uniqueid = if_else(municipality == "BARRIO DE LA SOLEDAD, EL", 20010, uniqueid),
         uniqueid = if_else(municipality == "EL ESPINAL", 20030, uniqueid),
         uniqueid = if_else(municipality == "FRESNILLO DE TRUJANO", 20032, uniqueid),
         uniqueid = if_else(municipality == "GUADALUPE DE RAMIREZ", 20034, uniqueid),
         uniqueid = if_else(municipality == "H. CIUDAD DE TLAXIACO", 20397, uniqueid),
         uniqueid = if_else(municipality == "HUAJUAPAN DE LEON", 20039, uniqueid),
         uniqueid = if_else(municipality == "HUAUTEPEC", 20040, uniqueid),
         uniqueid = if_else(municipality == "HUAUTLA DE JIMENEZ", 20041, uniqueid),
         uniqueid = if_else(municipality == "JUCHITAN DE ZARAGOZA", 20043, uniqueid),
         uniqueid = if_else(municipality == "LOMA BONITA", 20044, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA OCOTLAN", 20049, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TEQUISISTLAN", 20052, uniqueid),
         uniqueid = if_else(municipality == "MAGDALENA TLACOTEPEC", 20053, uniqueid),
         uniqueid = if_else(municipality == "MARISCALA DE JUAREZ", 20055, uniqueid),
         uniqueid = if_else(municipality == "MARTIRES DE TACUBAYA", 20056, uniqueid),
         uniqueid = if_else(municipality == "MATIAS ROMERO", 20057, uniqueid),
         uniqueid = if_else(municipality == "MIAHUATLAN DE PORFIRIO DIAZ", 20059, uniqueid),
         uniqueid = if_else(municipality == "OAXACA DE JUAREZ", 20067, uniqueid),
         uniqueid = if_else(municipality == "OCOTLAN DE MORELOS", 20068, uniqueid),
         uniqueid = if_else(municipality == "PINOTEPA DE DON LUIS", 20070, uniqueid),
         uniqueid = if_else(municipality == "PUTLA VILLA DE GUERRERO", 20073, uniqueid),
         uniqueid = if_else(municipality == "REFORMA DE PINEDA", 20075, uniqueid),
         uniqueid = if_else(municipality == "SALINA CRUZ", 20079, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN AMATENGO", 20080, uniqueid),
         uniqueid = if_else(municipality == "SAN AGUSTIN ATENANGO", 20081, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES DINICUITI", 20089, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES HUAXPALTEPEC", 20090, uniqueid),
         uniqueid = if_else(municipality == "SAN ANDRES ZAUTLA", 20102, uniqueid),
         uniqueid = if_else(municipality == "SAN ANTONINO CASTILLO VELASCO", 20103, uniqueid),
         uniqueid = if_else(municipality == "SAN BALTAZAR CHICHICAPAM", 20112, uniqueid),
         uniqueid = if_else(municipality == "SAN BARTOLOME AYAUTLA", 20116, uniqueid),
         uniqueid = if_else(municipality == "SAN BLAS ATEMPA", 20124, uniqueid),
         uniqueid = if_else(municipality == "SAN DIONISIO DEL MAR", 20130, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE JALAPA DE DIAZ", 20134, uniqueid),
         uniqueid = if_else(municipality == "SAN FELIPE USILA", 20136, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO DEL MAR", 20141, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO IXHUATAN", 20143, uniqueid),
         uniqueid = if_else(municipality == "SAN FRANCISCO TELIXTLAHUACA", 20150, uniqueid),
         uniqueid = if_else(municipality == "SAN JACINTO AMILPAS", 20157, uniqueid),
         uniqueid = if_else(municipality == "SAN JERONIMO SILACAYOAPILLA", 20160, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE CHILTEPEC", 20166, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE ESTANCIA GRANDE", 20168, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE INDEPENDENCIA", 20169, uniqueid),
         uniqueid = if_else(municipality == "SAN JOSE TENANGO", 20171, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA CUICATLAN", 20177, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA LO DE SOTO", 20180, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA SUCHITEPEC", 20181, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BTTA TLACOATZINTEPEC", 20182, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA TUXTEPEC", 20184, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN BAUTISTA VALLE NACIONAL", 20559, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN CACAHUATEPEC", 20185, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COATZOSPAM", 20187, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN COLORADO", 20188, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN GUICHICOVI", 20198, uniqueid),
         uniqueid = if_else(municipality == "SAN JUAN IHUALTEPEC", 20199, uniqueid),
         uniqueid = if_else(municipality == "SAN LORENZO", 20225, uniqueid),
         uniqueid = if_else(municipality == "SAN LUCAS OJITLAN", 20232, uniqueid),
         uniqueid = if_else(municipality == "SAN MARCOS ARTEAGA", 20237, uniqueid),
         uniqueid = if_else(municipality == "SAN MARTIN ZACATEPEC", 20245, uniqueid),
         uniqueid = if_else(municipality == "SAN MATEO RIO HONDO", 20254, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AHUEHUETITLAN", 20259, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL AMATITLAN", 20261, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL SOYALTEPEC", 20278, uniqueid),
         uniqueid = if_else(municipality == "SAN MIGUEL TLACAMAMA", 20285, uniqueid),
         uniqueid = if_else(municipality == "SAN NICOLAS HIDALGO", 20290, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUITZO", 20294, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO HUIXTEPEC", 20295, uniqueid),
         uniqueid = if_else(municipality == "SAN PABLO VILLA DE MITLA", 20298, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO AMUZGOS", 20300, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO ATOYAC", 20302, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO COMITANCILLO", 20305, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUAMELULA", 20307, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO HUILOTEPEC", 20308, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO IXCATLAN", 20309, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO JICAYAN", 20312, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO MIXTEPEC", 20318, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO POCHUTLA", 20324, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TAPANATEPEC", 20327, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO TUTUTEPEC", 20334, uniqueid),
         uniqueid = if_else(municipality == "SAN PEDRO Y SAN PABLO TEPOSCOLULA", 20339, uniqueid),
         uniqueid = if_else(municipality == "SAN SEBASTIAN IXCAPA", 20345, uniqueid),
         uniqueid = if_else(municipality == "SANTA ANA ZEGACHE", 20360, uniqueid),
         uniqueid = if_else(municipality == "SANTA CATARINA JUQUILA", 20364, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ AMILPAS", 20375, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ ITUNDUJIA", 20377, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ TACACHE DE MINA", 20381, uniqueid),
         uniqueid = if_else(municipality == "SANTA CRUZ XOXOCOTLAN", 20385, uniqueid),
         uniqueid = if_else(municipality == "SANTA DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(municipality == "SANTA GERTRUDIS", 20387, uniqueid),
         uniqueid = if_else(municipality == "SANTA LUCIA DEL CAMINO", 20390, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA CORTIJOS", 20402, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUATULCO", 20413, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA HUAZOLOTITLAN", 20414, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA IPALAPA", 20415, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JACATEPEC", 20417, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA JALAPA DEL MARQUES", 20418, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA MIXTEQUILLA", 20421, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA PETAPA", 20427, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TECOMAVACA", 20431, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEOPOXCO", 20434, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TEXCATITLAN", 20436, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA TONAMECA", 20439, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA XADANI", 20441, uniqueid),
         uniqueid = if_else(municipality == "SANTA MARIA ZACATEPEC", 20447, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO AYUQUILILLA", 20455, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CACALOXTEPEC", 20456, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO CHAZUMBA", 20459, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO HUAJOLOTITLAN", 20462, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JAMILTEPEC", 20467, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO JUXTLAHUACA", 20469, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LAOLLAGA", 20472, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO LLANO GRANDE", 20474, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO NILTEPEC", 20476, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO PINOTEPA NACIONAL", 20482, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO SUCHILQUITONGO", 20483, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAMAZOLA", 20484, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TAPEXTLA", 20485, uniqueid),
         uniqueid = if_else(municipality == "SANTIAGO TETEPEC", 20489, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ARMENTA", 20507, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO CHIHUITAN", 20508, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO PETAPA", 20513, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TEHUANTEPEC", 20515, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO TONALA", 20520, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO ZANATEPEC", 20525, uniqueid),
         uniqueid = if_else(municipality == "SILACAYOAPAM", 20537, uniqueid),
         uniqueid = if_else(municipality == "SOLEDAD ETLA", 20539, uniqueid),
         uniqueid = if_else(municipality == "TEOTITLAN DE FLORES MAGON", 20545, uniqueid),
         uniqueid = if_else(municipality == "TEZOATLAN DE SEGURA Y LUNA", 20549, uniqueid),
         uniqueid = if_else(municipality == "TLACOLULA DE MATAMOROS", 20551, uniqueid),
         uniqueid = if_else(municipality == "TRINIDAD ZAACHILA", 20555, uniqueid),
         uniqueid = if_else(municipality == "UNION HIDALGO", 20557, uniqueid),
         uniqueid = if_else(municipality == "VALERIO TRUJANO", 20558, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ETLA", 20338, uniqueid),
         uniqueid = if_else(municipality == "SANTO DOMINGO INGENIO", 20505, uniqueid),
         uniqueid = if_else(str_detect(municipality, "SANTO DOMINGO TONALA"), 20520, uniqueid),
         uniqueid = if_else(municipality == "TAMAZULAPAM DEL PROGRESO", 20540, uniqueid),
         uniqueid = if_else(municipality == "VILLA DE ZAACHILA", 20565, uniqueid),
         uniqueid = if_else(municipality == "VILLA SOLA DE VEGA", 20277, uniqueid),
         uniqueid = if_else(municipality == "TEJUPAM DE LA UNION", 20486, uniqueid),
         uniqueid = if_else(municipality == "ZAPOTITLAN LAGUNAS", 20567, uniqueid),
         uniqueid = if_else(municipality == "ZIMATLAN DE ALVAREZ", 20568, uniqueid))

# Collapse data by summing up columns by groups (equivalent to `collapse` in Stata)
data_collapsed <- data %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PAN_PRD_PT, PRI_PVEM, PMC:PSD, listanominal, total), sum, na.rm = TRUE))

# Create 'valid' variable (equivalent to `egen rowtotal`)
data_collapsed <- data_collapsed %>%
  mutate(valid = rowSums(across(c(PAN_PRD_PT, PRI_PVEM, PMC, PUP, PANAL, PSD)), na.rm = TRUE))

# Generate 'mun_' and 'i_' variables for each party
variables <- c("PAN_PRD_PT", "PRI_PVEM", "PMC", "PUP", "PANAL", "PSD", "total", "valid", "listanominal")
for (var in variables) {
  # Group by uniqueid to sum the variables (equivalent to `bys uniqueid: egen`)
  data_collapsed <- data_collapsed %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(!!paste0("i_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Generate 'turnout' and 'mun_turnout' variables
data_collapsed <- data_collapsed %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / mun_listanominal)

# Rank the 'i_' variables (equivalent to `rowranks`)
data_collapsed <- data_collapsed %>%
  mutate(
    PAN_PRD_PT_r = rank(-i_PAN_PRD_PT, ties.method = "min"),
    PRI_PVEM_r = rank(-i_PRI_PVEM, ties.method = "min"),
    PMC_r = rank(-i_PMC, ties.method = "min"),
    PUP_r = rank(-i_PUP, ties.method = "min"),
    PANAL_r = rank(-i_PANAL, ties.method = "min"),
    PSD_r = rank(-i_PSD, ties.method = "min")
  )

# Drop 'i_' variables
data_collapsed <- data_collapsed %>%
  select(-starts_with("i_"))

# Create 'winner', 'second', and 'third' variables
data_collapsed <- data_collapsed %>%
  mutate(winner = "", second = "", third = "")

# Assign winner, second, and third places based on ranks
for (var in c("PAN_PRD_PT", "PRI_PVEM", "PMC", "PUP", "PANAL", "PSD")) {
  data_collapsed <- data_collapsed %>%
    mutate(
      winner = ifelse(!!sym(paste0(var, "_r")) == 1, var, winner),
      second = ifelse(!!sym(paste0(var, "_r")) == 2, var, second),
      third = ifelse(!!sym(paste0(var, "_r")) == 3, var, third)
    )
}

# Drop ranking variables (equivalent to `drop *_r`)
data_collapsed <- data_collapsed %>%
  select(-ends_with("_r"))

# Add year, month, and STATE variables
data_collapsed <- data_collapsed %>%
  mutate(year = 2013, month = "July", STATE = "OAXACA")

# Reorder columns (equivalent to `order STATE municipality section uniqueid *`)
data_collapsed <- data_collapsed %>%
  select(STATE, municipality, section, uniqueid, everything())

# Save the final data (equivalent to `save Oaxaca_Section_2013.dta, replace`)
write_dta(data_collapsed, "Oaxaca_Section_2013.dta")

# Load required packages
library(readxl)
library(dplyr)
library(stringr)
library(haven)

# Read and process Excel sheets
sheets <- excel_sheets("Concejales_2016.xlsx")

for (sheetname in sheets) {
  data <- read_excel("Concejales_2016.xlsx", sheet = sheetname, col_types = "text")
  # Drop rows where DTTO is empty or equal to "DTTO"
  data <- data %>% filter(DTTO != "" & DTTO != "DTTO")
  # Save each processed sheet to a separate file
  write_dta(data, paste0(sheetname, ".dta"))
}

# Append all D*.dta files
files <- c("D01.dta", "D02.dta", "D03.dta", "D04.dta", "D05.dta", "D06.dta", "D07.dta", 
           "D08.dta", "D09.dta", "D11.dta", "D12.dta", "D13.dta", "D14.dta", "D15.dta", 
           "D16.dta", "D17.dta", "D18.dta", "D19.dta", "D20.dta", "D21.dta", "D22.dta", 
           "D23.dta", "D24.dta", "D25.dta")

data <- bind_rows(lapply(files, read_dta))

# Drop unwanted columns
data <- data %>%
  select(-LOCALIDAD, -DTTO, -AE, -CVE)

# Convert all columns from text to appropriate types (equivalent to destring)
data <- data %>%
  mutate(across(everything(), as.numeric))

# Rename variables to match the Stata code
data <- data %>%
  rename(MC = PMC, PANAL = PNA, PAN_PRD = PANPRD, PRI_PVEM = PRIPVEM, PANAL_PSD = PNAPSD,
         municipality = MUNICIPIO, section = SECCION, listanominal = LISTA_NOM, no_reg = CNR) %>%
  rename_at(vars(starts_with("IND")), ~ str_replace(., "IND_", "CI_"))

# Consolidate PAN, PRD, PRI_PVEM, PANAL_PSD
data <- data %>%
  mutate(
    PAN_PRD = ifelse(!is.na(PAN_PRD), PAN_PRD + PAN + PRD, PAN_PRD),
    PAN = ifelse(!is.na(PAN_PRD), NA, PAN),
    PRD = ifelse(!is.na(PAN_PRD), NA, PRD),
    PRI_PVEM = ifelse(!is.na(PRI_PVEM), PRI_PVEM + PRI + PVEM, PRI_PVEM),
    PRI = ifelse(!is.na(PRI_PVEM), NA, PRI),
    PVEM = ifelse(!is.na(PRI_PVEM), NA, PVEM),
    PANAL_PSD = ifelse(!is.na(PANAL_PSD), PANAL_PSD + PANAL + PSD, PANAL_PSD),
    PANAL = ifelse(!is.na(PANAL_PSD), NA, PANAL),
    PSD = ifelse(!is.na(PANAL_PSD), NA, PSD)
  )

# Drop column V and reorder columns (equivalent to order)
data <- data %>%
  select(-V) %>%
  relocate(PANAL_PSD, .after = PRI_PVEM)

# Rename total column
data <- data %>%
  rename(total = VOT_TOTAL)

# Drop rows where total is missing
data <- data %>%
  filter(!is.na(total))

# Collapse the data (sum over grouped variables)
data_collapsed <- data %>%
  group_by(municipality, section) %>%
  summarise(across(c(total, listanominal, PAN:CI_5), sum, na.rm = TRUE))

# Save the collapsed data (equivalent to preserve/restore)
write_dta(data_collapsed, "collapsed_data.dta")

# Import unique IDs and merge with the main data
uniqueids <- read_excel("uniqueids.xlsx", sheet = "2016", col_types = "text")
data_collapsed <- left_join(data_collapsed, uniqueids, by = "municipality")

# Remove original municipality and rename the merged one
data_collapsed <- data_collapsed %>%
  rename(municipality2 = municipality) %>%
  select(municipality2, everything())

# Calculate row totals (equivalent to `egen rowtotal`)
data_collapsed <- data_collapsed %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PVEM, PT, MC, PUP, PANAL, PSD, MORENA, PES, PRS, PAN_PRD, PRI_PVEM, PANAL_PSD, CI_1:CI_5), na.rm = TRUE))

# Generate mun_ and i_ variables
variables <- c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PUP", "PANAL", "PSD", "MORENA", "PES", "PRS", "PAN_PRD", "PRI_PVEM", "PANAL_PSD", "CI_1", "CI_2", "CI_3", "CI_4", "CI_5", "total", "valid", "listanominal")

for (var in variables) {
  data_collapsed <- data_collapsed %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(!!paste0("i_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Create turnout and mun_turnout
data_collapsed <- data_collapsed %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / listanominal)

# Rank the i_ variables (equivalent to rowranks)
ranks <- c("PAN", "PRI", "PRD", "PVEM", "PT", "PUP", "PSD", "MORENA", "PES", "PRS", "MC", "PANAL", "PAN_PRD", "PRI_PVEM", "PANAL_PSD", "CI_1", "CI_2", "CI_3", "CI_4", "CI_5")

for (var in ranks) {
  data_collapsed <- data_collapsed %>%
    mutate(!!paste0(var, "_r") := rank(-!!sym(paste0("i_", var)), ties.method = "min"))
}

# Drop i_ variables
data_collapsed <- data_collapsed %>%
  select(-starts_with("i_"))

# Create winner, second, and third columns
data_collapsed <- data_collapsed %>%
  mutate(winner = "", second = "", third = "")

# Assign winner, second, third
for (var in ranks) {
  data_collapsed <- data_collapsed %>%
    mutate(winner = ifelse(!!sym(paste0(var, "_r")) == 1, var, winner),
           second = ifelse(!!sym(paste0(var, "_r")) == 2, var, second),
           third = ifelse(!!sym(paste0(var, "_r")) == 3, var, third))
}

# Replace "CI_" with "Independent"
data_collapsed <- data_collapsed %>%
  mutate(winner = ifelse(str_detect(winner, "CI_"), "Independent", winner),
         second = ifelse(str_detect(second, "CI_"), "Independent", second),
         third = ifelse(str_detect(third, "CI_"), "Independent", third))

# Add year, month, and state
data_collapsed <- data_collapsed %>%
  mutate(year = 2016, month = "June", STATE = "OAXACA")

# Save final data
write_dta(data_collapsed, "Oaxaca_Section_2016.dta")

# Collapse to find incumbents
incumbents <- data_collapsed %>%
  group_by(municipality, uniqueid) %>%
  summarise(incumbent = first(winner))

# Handle specific uniqueid
incumbents <- incumbents %>%
  mutate(incumbent = ifelse(uniqueid == 20441, "", incumbent))

# Save the incumbents file
write_dta(incumbents, "incumbents18.dta")

# Load the dataset
data <- read_dta("Oaxaca_Section_2016.dta")

# Collapse (sum and first)
collapsed_data <- data %>%
  group_by(municipality, uniqueid) %>%
  summarize(
    total_valid_sum = sum(total_valid, na.rm = TRUE),  # sum of 'total-valid'
    STATE_first = first(STATE),                       # first 'STATE'
    year_first = first(year),                         # first 'year'
    month_first = first(month),                       # first 'month'
    winner_first = first(winner),                     # first 'winner'
    second_first = first(second),                     # first 'second'
    third_first = first(third),                       # first 'third'
    turnout_first = first(mun_turnout)                # first 'mun_turnout'
  )

# Rename 'turnout_first' to 'turnout'
collapsed_data <- collapsed_data %>%
  rename(turnout = turnout_first)

# Sort by 'uniqueid'
collapsed_data <- collapsed_data %>%
  arrange(uniqueid)

# Reorder columns to have 'STATE', 'municipality', 'uniqueid' first, followed by all other variables
collapsed_data <- collapsed_data %>%
  select(STATE_first, municipality, uniqueid, everything())

# Save the resulting dataset
write_dta(collapsed_data, "../../Update Municipal/Oaxaca_2016.dta")

# Remove specific files (similar to the 'erase' commands in Stata)
file.remove("D01.dta", "D02.dta", "D03.dta", "D04.dta", "D05.dta", "D06.dta", 
            "D07.dta", "D08.dta", "D09.dta", "D11.dta", "D12.dta", "D13.dta", 
            "D14.dta", "D15.dta", "D16.dta", "D17.dta", "D18.dta", "D19.dta", 
            "D20.dta", "D21.dta", "D22.dta", "D23.dta", "D24.dta", "D25.dta")

# Import the Excel sheet
data <- read_excel("EXTRAORDINARIO_2017.xlsx", sheet = "excel", col_names = TRUE)

# Split the 'CASILLA' column using "-" as delimiter
data <- data %>%
  separate(CASILLA, into = c("section", "CASILLA_rest"), sep = "-")

# Rename columns
data <- data %>%
  rename(
    listanominal = L_NOM,  # Rename 'L_NOM' to 'listanominal'
    total = Votos          # Rename 'Votos' to 'total'
  )

# Generate new columns for municipality and uniqueid
data <- data %>%
  mutate(
    municipality = "SANTA MARIA XADANI EXTRAORDINARIO",
    uniqueid = 20441
  )

# Convert all columns to numeric (similar to 'destring' in Stata)
data <- data %>%
  mutate(across(everything(), as.numeric, .names = "destring_{col}"))

# Replace 'PRI' with sum of 'PRI', 'PVEM', and 'COALICION', drop 'PVEM' and 'COALICION'
data <- data %>%
  mutate(PRI_PVEM = PRI + PVEM + COALICION) %>%
  select(-PVEM, -COALICION)

# Collapse the data (sum by group)
collapsed_data <- data %>%
  group_by(municipality, uniqueid, section) %>%
  summarize(across(c(PAN:PRS, listanominal, total), sum, na.rm = TRUE))

# Create 'valid' as the sum of selected columns
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN, PRI_PVEM, PRD, PT, PMC, PUP, MORENA, PRS))))

# Loop to generate municipal-level sums and inverse variables
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN, PRI_PVEM, PRD, PT, PMC, PUP, MORENA, PRS, total, valid, listanominal),
                ~ sum(.x, na.rm = TRUE), .names = "mun_{col}"),
         across(c(PAN, PRI_PVEM, PRD, PT, PMC, PUP, MORENA, PRS),
                ~ 1 / get(paste0("mun_", cur_column())), .names = "i_{col}"))

# Calculate turnout and municipal-level turnout
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / mun_listanominal)

# Rank inverses and create ranking variables
rank_cols <- c("i_PAN", "i_PRI_PVEM", "i_PRD", "i_PT", "i_PMC", "i_PUP", "i_MORENA", "i_PRS")
collapsed_data <- collapsed_data %>%
  mutate(across(all_of(rank_cols), ~ frankv(.x, ties.method = "min", na.last = "keep"),
                .names = "{col}_r"))

# Drop the inverse variables
collapsed_data <- collapsed_data %>%
  select(-starts_with("i_"))

# Initialize empty columns for winner, second, and third
collapsed_data <- collapsed_data %>%
  mutate(winner = "", second = "", third = "")

# Loop to determine winner, second, and third based on rankings
for (var in c("PAN", "PRI_PVEM", "PRD", "PT", "PMC", "PUP", "MORENA", "PRS")) {
  collapsed_data <- collapsed_data %>%
    mutate(
      winner = ifelse(get(paste0(var, "_r")) == 1, var, winner),
      second = ifelse(get(paste0(var, "_r")) == 2, var, second),
      third = ifelse(get(paste0(var, "_r")) == 3, var, third)
    )
}

# Replace CI_1, CI_2, CI_3 with "Independent" in winner, second, and third
collapsed_data <- collapsed_data %>%
  mutate(
    winner = ifelse(winner %in% c("CI_1", "CI_2", "CI_3"), "Independent", winner),
    second = ifelse(second %in% c("CI_1", "CI_2", "CI_3"), "Independent", second),
    third = ifelse(third %in% c("CI_1", "CI_2", "CI_3"), "Independent", third)
  )

# Add year, month, and STATE columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2017, month = "June", STATE = "OAXACA")

# Reorder the columns
collapsed_data <- collapsed_data %>%
  select(STATE, municipality, section, uniqueid, everything())

# Save the data as .dta file
write_dta(collapsed_data, "Oaxaca_Section_2017.dta")

# Import the Excel sheet
data <- read_excel("Concejales_2018.xlsx")

# Create coalition and communal candidate variables based on municipality conditions
data <- data %>%
  mutate(
    coalition_pan_prd_mc = !(municipality %in% c("LOMA BONITA", "SAN FELIPE USILA", "SAN JUAN IHUALTEPEC", 
                                                 "SAN JUAN CACAHUATEPEC", "SANTA MARIA XADANI", "EL ESPINAL", 
                                                 "SAN DIONISIO DEL MAR")),
    
    coalition_pri_pvem_panal = !(municipality %in% c("SAN JOSE TENANGO", "SAN MIGUEL SOYALTEPEC", 
                                                     "SAN JUAN BAUTISTA TUXTEPEC", "SAN FELIPE JALAPA DE DIAZ", 
                                                     "SAN JOSE CHILTEPEC", "SAN JUAN BAUTISTA TLACOATZINTEPEC", 
                                                     "SANTA MARIA JACATEPEC", "HUAUTEPEC", "HUAUTLA DE JIMENEZ", 
                                                     "SAN BARTOLOME AYAUTLA", "SAN JUAN BAUTISTA CUICATLAN", 
                                                     "SAN JUAN COATZOSPAM", "SANTA MARIA TECOMAVACA", 
                                                     "SAN FRANCISCO TELIXTLAHUACA", "VILLA DE TAMAZULAPAM DEL PROGRESO", 
                                                     "FRESNILLO DE TRUJANO", "HEROICA CIUDAD DE HUAJUAPAN DE LEON", 
                                                     "MARISCALA DE JUAREZ", "SAN JUAN IHUALTEPEC", 
                                                     "SAN MARCOS ARTEAGA", "SAN MIGUEL AMATITLAN", 
                                                     "SANTIAGO HUAJOLOTITLAN", "ZAPOTITLAN LAGUNAS", 
                                                     "SAN JUAN CACAHUATEPEC", "CHALCATONGO DE HIDALGO", 
                                                     "SAN JUAN BAUTISTA VALLE NACIONAL", "CHAHUITES", 
                                                     "MATIAS ROMERO AVENDAÑO", "REFORMA DE PINEDA", 
                                                     "SAN JUAN GUICHICOVI", "SANTA MARIA PETAPA", 
                                                     "SANTO DOMINGO INGENIO", "SANTO DOMINGO ZANATEPEC", 
                                                     "SANTA CRUZ AMILPAS", "SANTA LUCIA DEL CAMINO", 
                                                     "SOLEDAD ETLA", "OAXACA DE JUAREZ", "SANTA CRUZ XOXOCOTLAN", 
                                                     "ASUNCION OCOTLAN", "CIENEGA DE ZIMATLAN", 
                                                     "SAN ANTONINO CASTILLO VELASCO", "SANTA ANA ZEGACHE", 
                                                     "SANTA GERTRUDIS", "TRINIDAD ZAACHILA", 
                                                     "ZIMATLAN DE ALVAREZ", "SAN PABLO VILLA DE MITLA", 
                                                     "SANTA MARIA JALAPA DEL MARQUES", "SANTA MARIA MIXTEQUILLA", 
                                                     "SANTO DOMINGO CHIHUITAN", "SANTO DOMINGO PETAPA", 
                                                     "SANTO DOMINGO TEHUANTEPEC", "ASUNCION IXTALTEPEC", 
                                                     "SALINA CRUZ", "SAN BLAS ATEMPA", "EL ESPINAL", 
                                                     "SAN DIONISIO DEL MAR", "SAN FRANCISCO DEL MAR", 
                                                     "HEROICA CIUDAD DE EJUTLA DE CRESPO", 
                                                     "MARTIRES DE TACUBAYA", "SAN ANDRES HUAXPALTEPEC", 
                                                     "SAN JUAN BAUTISTA LO DE SOTO", "SAN MIGUEL TLACAMAMA", 
                                                     "SAN PEDRO ATOYAC", "SAN PEDRO JICAYAN", 
                                                     "SAN SEBASTIAN IXCAPA", "SANTA MARIA CORTIJO", 
                                                     "SANTA MARIA HUAZOLOTITLAN", "SANTIAGO TAPEXTLA", 
                                                     "SAN PEDRO MIXTEPEC", "SANTIAGO TETEPEC", 
                                                     "MIAHUATLAN DE PORFIRIO DIAZ")),
    
    coalition_pt_morena_pes = !(municipality %in% c("SAN BARTOLOME AYAUTLA", "SAN JUAN IHUALTEPEC", 
                                                    "SAN DIONISIO DEL MAR", "SAN JOSE ESTANCIA GRANDE", 
                                                    "SAN MATEO RIO HONDO")),
    
    cc_pan_prd_mc = municipality == "LOMA BONITA",
    
    cc_pri_pvem_panal = municipality %in% c("SAN MIGUEL SOYALTEPEC", "SAN JUAN BAUTISTA CUICATLAN", 
                                            "SANTA MARIA TECOMAVACA", "FRESNILLO DE TRUJANO", 
                                            "SAN JUAN CACAHUATEPEC", "SAN JUAN GUICHICOVI", 
                                            "SANTO DOMINGO INGENIO", "SANTO DOMINGO ZANATEPEC", 
                                            "SANTA GERTRUDIS", "SAN BLAS ATEMPA", "EL ESPINAL", 
                                            "MARTIRES DE TACUBAYA", "SAN SEBASTIAN IXCAPA"),
    
    cc_pri_pvem = municipality %in% c("SAN JOSE CHILTEPEC", "SAN FRANCISCO TELIXTLAHUACA", 
                                      "SAN MIGUEL AMATITLAN", "REFORMA DE PINEDA", 
                                      "SANTA LUCIA DEL CAMINO", "SOLEDAD ETLA", 
                                      "OAXACA DE JUAREZ", "SANTO DOMINGO PETAPA", 
                                      "ASUNCION IXTALTEPEC", "SALINA CRUZ", 
                                      "SAN PEDRO MIXTEPEC", "MIAHUATLAN DE PORFIRIO DIAZ"),
    
    cc_pri_panal = municipality %in% c("SAN JOSE TENANGO", "SAN FELIPE JALAPA DE DIAZ", 
                                       "HUAUTLA DE JIMENEZ", "SAN JUAN COATZOSPAM", 
                                       "MATIAS ROMERO AVENDAÑO", "TRINIDAD ZAACHILA", 
                                       "ZIMATLAN DE ALVAREZ", "SANTA MARIA JALAPA DEL MARQUES", 
                                       "SANTA MARIA MIXTEQUILLA", "SANTO DOMINGO TEHUANTEPEC", 
                                       "HEROICA CIUDAD DE EJUTLA DE CRESPO"),
    
    cc_pvem_panal = municipality %in% c("SANTA MARIA PETAPA", "SAN ANTONINO CASTILLO VELASCO", 
                                        "SANTA ANA ZEGACHE", "SANTO DOMINGO CHIHUITAN", 
                                        "PINOTEPA DE DON LUIS"),
    
    cc_prd_mc = municipality == "SAN FELIPE USILA",
    
    cc_prd_mc_pup = municipality == "EL ESPINAL"
  )

# The resulting 'data' dataframe now includes the coalition and communal candidate variables.

# Replace coalition flags based on communal candidate flags
data <- data %>%
  mutate(
    coalition_pri_pvem_panal = ifelse(cc_pri_pvem_panal == 1, 1, coalition_pri_pvem_panal),
    coalition_pan_prd_mc = ifelse(cc_pan_prd_mc == 1, 1, coalition_pan_prd_mc)
  ) %>%
  select(-cc_pri_pvem_panal, -cc_pan_prd_mc)

# Aggregate PAN_PRD_MC if the coalition exists
data <- data %>%
  mutate(PAN_PRD_MC = ifelse(coalition_pan_prd_mc == 1, rowSums(select(., PAN, PRD, MC, COAL_PAN_PRD_MC, CC_PAN_PRD_MC), na.rm = TRUE), NA),
         PAN = ifelse(coalition_pan_prd_mc == 1, NA, PAN),
         PRD = ifelse(coalition_pan_prd_mc == 1, NA, PRD),
         MC = ifelse(coalition_pan_prd_mc == 1, NA, MC))

# Aggregate PRI_PVEM_PANAL if the coalition exists
data <- data %>%
  mutate(PRI_PVEM_PANAL = ifelse(coalition_pri_pvem_panal == 1, rowSums(select(., PRI, PVEM, PANAL, COAL_PRI_PVEM_PANAL, CC_PRI_PVEM_NA, CC_PRI_PVEM, CC_PRI_NA, CC_PVEM_NA), na.rm = TRUE), NA),
         PRI = ifelse(coalition_pri_pvem_panal == 1, NA, PRI),
         PVEM = ifelse(coalition_pri_pvem_panal == 1, NA, PVEM),
         PANAL = ifelse(coalition_pri_pvem_panal == 1, NA, PANAL))

# Aggregating for other coalitions and combinations
data <- data %>%
  mutate(PRI_PVEM = ifelse(cc_pri_pvem == 1, rowSums(select(., PRI, PVEM, CC_PRI_PVEM), na.rm = TRUE), NA),
         PRI = ifelse(cc_pri_pvem == 1, NA, PRI),
         PVEM = ifelse(cc_pri_pvem == 1, NA, PVEM)) %>%
  
  mutate(PRI_PANAL = ifelse(cc_pri_panal == 1, rowSums(select(., PRI, PANAL, CC_PRI_NA), na.rm = TRUE), NA),
         PRI = ifelse(cc_pri_panal == 1, NA, PRI),
         PANAL = ifelse(cc_pri_panal == 1, NA, PANAL)) %>%
  
  mutate(PVEM_PANAL = ifelse(cc_pvem_panal == 1, rowSums(select(., PVEM, PANAL, CC_PVEM_NA), na.rm = TRUE), NA),
         PVEM = ifelse(cc_pvem_panal == 1, NA, PVEM),
         PANAL = ifelse(cc_pvem_panal == 1, NA, PANAL)) %>%
  
  mutate(PT_MORENA_PES = ifelse(coalition_pt_morena_pes == 1, rowSums(select(., PT, MORENA, PES, COAL_PT_MORENA_PES), na.rm = TRUE), NA)) %>%
  select(-PT, -MORENA, -PES) %>%
  
  mutate(PRD_MC_PUP = ifelse(cc_prd_mc_pup == 1, rowSums(select(., PRD, MC, PUP, CC_PRD_MC_PUP), na.rm = TRUE), NA),
         PRD = ifelse(cc_prd_mc_pup == 1, NA, PRD),
         MC = ifelse(cc_prd_mc_pup == 1, NA, MC),
         PUP = ifelse(cc_prd_mc_pup == 1, NA, PUP)) %>%
  
  mutate(PRD_MC = ifelse(cc_prd_mc == 1, rowSums(select(., PRD, MC, CC_PRD_MC), na.rm = TRUE), NA),
         PRD = ifelse(cc_prd_mc == 1, NA, PRD),
         MC = ifelse(cc_prd_mc == 1, NA, MC))

# Dropping auxiliary variables
data <- data %>%
  select(-starts_with("CC"), -starts_with("COAL"))

# Reordering and renaming columns
data <- data %>%
  rename(municipality = munname_INEGI, total = TOTAL)

# Aggregating independent candidate votes
data <- data %>%
  mutate(CI_1 = rowSums(select(., starts_with("CIALBERTO"):CIJOELA), na.rm = TRUE),
         CI_2 = rowSums(select(., CIELIAZAR, CIIVAN, CIADOLFOJ), na.rm = TRUE),
         CI_3 = rowSums(select(., CIRAUL, CIGERMAN), na.rm = TRUE)) %>%
  mutate(CI_1 = CI_1 - CI_2 - CI_3) %>%
  select(-starts_with("CIALBERTO"))

# Summing variables using `collapse` operation
collapsed_data <- data %>%
  group_by(municipality, uniqueid, section) %>%
  summarize(across(c(PAN:CI_3, listanominal, total), sum, na.rm = TRUE),
            across(c(starts_with("cc"), starts_with("coalition")), first, na.rm = TRUE))

# Create 'valid' variable (sum of all vote columns)
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(select(., PAN:CI_3), na.rm = TRUE))

# Generate municipal sums and inverse variables
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN:CI_3, total, valid, listanominal), 
                ~ sum(.x, na.rm = TRUE), .names = "mun_{col}"),
         across(c(PAN:CI_3, total, valid, listanominal), 
                ~ 1 / get(paste0("mun_", cur_column())), .names = "i_{col}"))

# Calculate turnout and municipal turnout
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / mun_listanominal)

# Ranking
collapsed_data <- collapsed_data %>%
  mutate(across(starts_with("i_"), ~ frankv(.x, ties.method = "min", na.last = "keep"), 
                .names = "{col}_r"))

# Drop the inverse variables
collapsed_data <- collapsed_data %>%
  select(-starts_with("i_"))

# Initialize winner, second, and third columns
collapsed_data <- collapsed_data %>%
  mutate(winner = "", second = "", third = "")

# Assign winner, second, and third based on ranks
for (var in c("PAN", "PRI", "PRD", "PVEM", "MC", "PUP", "PANAL", "PSD", "PMR", "PAN_PRD_MC",
              "PRI_PVEM_PANAL", "PT_MORENA_PES", "PRD_MC_PUP", "PRI_PVEM", "PRI_PANAL", 
              "PVEM_PANAL", "PRD_MC", "CI_1", "CI_2", "CI_3")) {
  collapsed_data <- collapsed_data %>%
    mutate(
      winner = ifelse(get(paste0(var, "_r")) == 1, var, winner),
      second = ifelse(get(paste0(var, "_r")) == 2, var, second),
      third = ifelse(get(paste0(var, "_r")) == 3, var, third)
    )
}

# Replace Independent candidates
collapsed_data <- collapsed_data %>%
  mutate(winner = ifelse(winner %in% c("CI_1", "CI_2", "CI_3"), "Independent", winner),
         second = ifelse(second %in% c("CI_1", "CI_2", "CI_3"), "Independent", second),
         third = ifelse(third %in% c("CI_1", "CI_2", "CI_3"), "Independent", third))

# Add year, month, and state columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2018, month = "July", STATE = "OAXACA")

# Reordering columns
collapsed_data <- collapsed_data %>%
  select(STATE, municipality, section, uniqueid, everything())

# Assuming the incumbents18 data is also loaded
incumbents_data <- read_dta("incumbents18.dta")

# Merge with incumbents data
collapsed_data <- collapsed_data %>%
  left_join(incumbents_data, by = "uniqueid") %>%
  select(-c("_merge"))

# Assigning incumbents' votes
collapsed_data <- collapsed_data %>%
  mutate(incumbent_vote = NA)

for (var in c("PAN", "PRI", "PRD", "PANAL", "MC", "PUP", "PSD")) {
  collapsed_data <- collapsed_data %>%
    mutate(incumbent_vote = ifelse(incumbent == var, get(var), incumbent_vote))
}

collapsed_data <- collapsed_data %>%
  mutate(incumbent_vote = ifelse(incumbent %in% c("PAN", "PRD", "MC") & coalition_pan_prd_mc == 1, PAN_PRD_MC, incumbent_vote),
         incumbent_vote = ifelse(incumbent %in% c("PRI", "PVEM", "PANAL") & coalition_pri_pvem_panal == 1, PRI_PVEM_PANAL, incumbent_vote),
         incumbent_vote = ifelse(incumbent %in% c("PRI", "PVEM") & cc_pri_pvem == 1, PRI_PVEM, incumbent_vote),
         incumbent_vote = ifelse(incumbent %in% c("PRI", "PANAL") & cc_pri_panal == 1, PRI_PANAL, incumbent_vote),
         incumbent_vote = ifelse(incumbent %in% c("PVEM", "PANAL") & cc_pvem_panal == 1, PVEM_PANAL, incumbent_vote),
         incumbent_vote = ifelse(incumbent %in% c("PT", "MORENA", "PES") & coalition_pt_morena_pes == 1, PT_MORENA_PES, incumbent_vote),
         incumbent_vote = ifelse(incumbent %in% c("PRD", "MC", "PUP") & cc_prd_mc_pup == 1, PRD_MC_PUP, incumbent_vote),
         incumbent_vote = ifelse(incumbent %in% c("PRD", "MC") & cc_prd_mc == 1, PRD_MC, incumbent_vote),
         incumbent_vote = ifelse(incumbent == "Independent", CI_1, incumbent_vote))

# Save the final dataset
write_dta(collapsed_data, "Oaxaca_Section_2018.dta")

# Collapse data (sum and first) by municipality and uniqueid
collapsed_data <- collapsed_data %>%
  group_by(municipality, uniqueid) %>%
  summarize(across(c(PAN:total, incumbent_vote), sum, na.rm = TRUE),
            across(c(STATE, year, month, winner, second, third, incumbent), first, na.rm = TRUE),
            mun_turnout = first(mun_turnout, na.rm = TRUE))

# Rename 'mun_turnout' to 'turnout'
collapsed_data <- collapsed_data %>%
  rename(turnout = mun_turnout)

# Sort by 'uniqueid'
collapsed_data <- collapsed_data %>%
  arrange(uniqueid)

# Reorder columns with 'STATE', 'municipality', 'uniqueid' first
collapsed_data <- collapsed_data %>%
  select(STATE, municipality, uniqueid, everything())

# Save the data as .dta file
write_dta(collapsed_data, "../../Update Municipal/Oaxaca_2018.dta")

# Import the Excel sheet with specified cell range
data <- read_excel("EXTRAORDINARIO_2018.xlsx", sheet = "COMPAYU20020", range = "A3:AH5", col_names = TRUE)

# Rename columns and generate new variables
data <- data %>%
  rename(section = SECCION,
         listanominal = LISTA_NOMINAL_CASILLA,
         total = TOTAL_VOTOS) %>%
  mutate(
    municipality = "SAN JUAN IHUALTEPEC EXTRAORDINARIO",
    uniqueid = 20199
  )

# Replace PRI with the sum of multiple columns and drop unnecessary ones
data <- data %>%
  mutate(PRI_PVEM_PANAL = PRI + PVEM + NA + CANDIDATURACOMÚNPRIPVEMNA + PRIPVEM + PRINA + PVEMNA) %>%
  select(-PVEM, -NA, -CANDIDATURACOMÚNPRIPVEMNA, -PRIPVEM, -PRINA, -PVEMNA)

# Collapse data by grouping on municipality, uniqueid, and section
collapsed_data <- data %>%
  group_by(municipality, uniqueid, section) %>%
  summarize(across(c(PAN:MORENA, listanominal, total), sum, na.rm = TRUE))

# Generate 'valid' votes as the sum of PAN, PRI_PVEM_PANAL, and MORENA
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN, PRI_PVEM_PANAL, MORENA)), na.rm = TRUE))

# Calculate municipal-level sums and inverses
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN, PRI_PVEM_PANAL, MORENA, total, valid, listanominal),
                ~ sum(.x, na.rm = TRUE), .names = "mun_{col}"),
         across(c(PAN, PRI_PVEM_PANAL, MORENA),
                ~ 1 / get(paste0("mun_", cur_column())), .names = "i_{col}"))

# Calculate turnout and municipal turnout
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / mun_listanominal)

# Rank the inverses and create ranking variables
collapsed_data <- collapsed_data %>%
  mutate(across(c(i_PAN, i_PRI_PVEM_PANAL, i_MORENA), 
                ~ frankv(.x, ties.method = "min", na.last = "keep"), 
                .names = "{col}_r"))

# Drop inverse variables after ranking
collapsed_data <- collapsed_data %>%
  select(-starts_with("i_"))

# Initialize winner, second, and third columns
collapsed_data <- collapsed_data %>%
  mutate(winner = "", second = "", third = "")

# Assign winner, second, and third based on ranks
for (var in c("PAN", "PRI_PVEM_PANAL", "MORENA")) {
  collapsed_data <- collapsed_data %>%
    mutate(
      winner = ifelse(get(paste0(var, "_r")) == 1, var, winner),
      second = ifelse(get(paste0(var, "_r")) == 2, var, second),
      third = ifelse(get(paste0(var, "_r")) == 3, var, third)
    )
}

# Handle Independent candidates
collapsed_data <- collapsed_data %>%
  mutate(winner = ifelse(winner %in% c("CI_1", "CI_2", "CI_3"), "Independent", winner),
         second = ifelse(second %in% c("CI_1", "CI_2", "CI_3"), "Independent", second),
         third = ifelse(third %in% c("CI_1", "CI_2", "CI_3"), "Independent", third))

# Add year, month, and STATE columns
collapsed_data <- collapsed_data %>%
  mutate(year = 2018, month = "December", STATE = "OAXACA")

# Reorder columns
collapsed_data <- collapsed_data %>%
  select(STATE, municipality, section, uniqueid, everything())

# Save the data as .dta file
write_dta(collapsed_data, "Oaxaca_Section_2018_EXTRAORDINARIO.dta")

# Clear workspace (optional in R, typically we don't need to explicitly clear)
rm(list = ls())

# Load and append all datasets
data_2013 <- read_dta("Oaxaca_Section_2013.dta")
data_2014 <- read_dta("Oaxaca_Section_2014.dta")
data_2016 <- read_dta("Oaxaca_Section_2016.dta")
data_2017 <- read_dta("Oaxaca_Section_2017.dta")
data_2018 <- read_dta("Oaxaca_Section_2018.dta")
data_2018_extra <- read_dta("Oaxaca_Section_2018_EXTRAORDINARIO.dta")

# Append all datasets
all_data <- bind_rows(data_2013, data_2014, data_2016, data_2017, data_2018, data_2018_extra)

# Remove the individual files (equivalent to erase)
file.remove("Oaxaca_Section_2013.dta", "Oaxaca_Section_2014.dta", "Oaxaca_Section_2016.dta", 
            "Oaxaca_Section_2017.dta", "Oaxaca_Section_2018.dta", "Oaxaca_Section_2018_EXTRAORDINARIO.dta")

# Create `PAN_winner` variable
all_data <- all_data %>%
  mutate(PAN_winner = ifelse(str_detect(winner, "PAN") & !str_detect(winner, "PANAL"), 1, 0))

# Initialize winner_counter with PAN_winner
all_data <- all_data %>%
  mutate(winner_counter = PAN_winner)

# Loop to create winner variables for other parties and update winner_counter
for (var in c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "MORENA", "MC")) {
  winner_var <- paste0(var, "_winner")
  all_data <- all_data %>%
    mutate(!!winner_var := ifelse(str_detect(winner, var), 1, 0)) %>%
    mutate(winner_counter = winner_counter + get(winner_var))
}

# Display the tabulation of winner_counter
table(all_data$winner_counter)

# Count where winner_counter equals 0
sum(all_data$winner_counter == 0)

# Save the appended dataset
write_dta(all_data, "Oaxaca_Section_14_18.dta")

# Load the precinct dataset and append the new data
precinct_data <- read_dta("../../Precinct/Oaxaca_ALL.dta")
all_data_combined <- bind_rows(precinct_data, all_data)

# Remove the temporary dataset (equivalent to erase)
file.remove("Oaxaca_Section_14_18.dta")

# Replace uniqueid and capitalize municipality names
all_data_combined <- all_data_combined %>%
  mutate(uniqueid = ifelse(municipality == "SANTIAGO NILTEPEC", 20066, uniqueid),
         municipality = toupper(municipality))

# Save the final dataset
write_dta(all_data_combined, "Oaxaca_ALL.dta")

