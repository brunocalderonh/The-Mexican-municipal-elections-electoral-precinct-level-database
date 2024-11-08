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
### PROCESSING DATA FOR 1998
#####################################
data_1998 <- read_csv("../../../Data/Raw Electoral Data/Chihuahua - 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_1998.csv")
# Rename columns
data_1998 <- data_1998 %>%
  dplyr::rename(municipality = NOMBRE, 
                section = SECCION,
                total = TOTAL,
                listanominal = "LISTA NOMINAL")

# Convert relevant columns to numeric
data_1998 <- data_1998 %>%
  dplyr::mutate(across(c(PAN:listanominal), as.numeric))

# Drop rows where 'total' is missing or zero
data_1998 <- data_1998 %>% dplyr::filter(!is.na(total) & total != 0)

# Collapse (group and sum)
data_1998_collapsed <- data_1998 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(PAN:listanominal, sum, na.rm = TRUE))

# Rename columns after collapse
data_1998_collapsed <- data_1998_collapsed %>%
  dplyr::rename(nulos = "VOTOS NULOS", 
                PCDP = CDP)

# Generate turnout
data_1998_collapsed <- data_1998_collapsed %>%
  dplyr::mutate(turnout = total / listanominal)

# Drop unnecessary columns
data_1998_collapsed <- data_1998_collapsed %>%
  dplyr::select(-c("NO REGISTRADOS"))

# Define a named vector for municipality IDs
municipality_ids <- c(
  "AHUMADA" = 8001, "ALDAMA" = 8002, "ALLENDE" = 8003, 
  "AQUILES SERDÁN" = 8004, 
  "AQUILES SERDAN" = 8004, 
  "ASCENSIÓN" = 8005, 
  "ASCENSION" = 8005, 
  "BACHÍNIVA" = 8006, 
  "BACHINIVA" = 8006,
  "BALLEZA" = 8007, 
  "BATOPILAS" = 8008,
  "BATOPILAS DE MANUEL GOMEZ MORIN" = 8008,
  "BOCOYNA" = 8009, "BUENAVENTURA" = 8010, "CAMARGO" = 8011, "CARICHI" = 8012, 
  "CASAS GRANDES" = 8013, "CHIHUAHUA" = 8019, 
  "CHÍNIPAS" = 8020, 
  "CHINIPAS" = 8020,
  "CORONADO" = 8014, 
  "COYAME" = 8015, 
  "CUAUHTÉMOC" = 8017, 
  "CUAUHTEMOC" = 8017,
  "CUAUHTE?MOC" = 8017, 
  "CUSIHUIRIACHI" = 8018, "DELICIAS" = 8021, 
  "DR. BELISARIO DOMÍNGUEZ" = 8022,
  "DR. BELISARIO DOMINGUEZ"= 8022,
  "DOCTOR BELISARIO DOMINGUEZ" = 8022, 
  "EL TULE" = 8064, "GALEANA" = 8023, 
  "GÓMEZ FARÍAS" = 8025, 
  "GOMEZ FARIAS" = 8025,
  "GRAN MORELOS" = 8026, "GUACHOCHI" = 8027, 
  "GUADALUPE" = 8028, "GUADALUPE Y CALVO" = 8029, "GUAZAPARES" = 8030, 
  "GUERRERO" = 8031, "HIDALGO DEL PARRAL" = 8032, 
  "HUEJOTITÁN" = 8033, 
  "HUEJOTITAN" = 8033, 
  "IGNACIO ZARAGOZA" = 8034, "JANOS" = 8035, "JIMÉNEZ" = 8036, 
  "JIMENEZ" = 8036,
  "JUÁREZ" = 8037, 
  "JUAREZ" = 8037, 
  "JULIMES" = 8038, "LA CRUZ" = 8016, 
  "LÓPEZ" = 8039, 
  "LOPEZ" = 8039,
  "MADERA" = 8040, 
  "MAGUARICHI" = 8041, "MANUEL BENAVIDES" = 8042, 
  "MATACHÍ" = 8043, 
  "MATACHI" = 8043,
  "MATAMOROS" = 8044, 
  "MEOQUI" = 8045, "MORELOS" = 8046, "MORIS" = 8047, "NAMIQUIPA" = 8048, 
  "NONOAVA" = 8049, "NUEVO CASAS GRANDES" = 8050, "OCAMPO" = 8051, "OJINAGA" = 8052, 
  "PRAXEDIS G. GUERRERO" = 8053, 
  "PRAXEDIS G GUERRERO" = 8053,
  "RIVA PALACIO" = 8054, "ROSALES" = 8055, 
  "ROSARIO" = 8056, "SAN FRANCISCO DE BORJA" = 8057, "SAN FRANCISCO DE CONCHOS" = 8058, 
  "SAN FRANCISCO DEL ORO" = 8059, 
  "SANTA BÁRBARA" = 8060, 
  "SANTA BARBARA" = 8060, 
  "SANTA ISABEL" = 8024, 
  "SATEVÓ" = 8061, 
  "SATEVO" = 8061,
  "SAUCILLO" = 8062, "TEMÓSACHI" = 8063, "URIQUE" = 8065, 
  "URUACHI" = 8066, "VALLE DE ZARAGOZA" = 8067,
  "AHUMADA" = 8001, "ALDAMA" = 8002, "ALLENDE" = 8003, 
  "AQUILES SERD\xc1N" = 8004,                 # AQUILES SERD\xc1N
  "ASCENSI\xd3N" = 8005,                      # ASCENSI\xd3N
  "BACH\xcdNIVA" = 8006,                      # BACH\xcdNIVA
  "CH\xcdNIPAS" = 8020,                       # CH\xcdNIPAS
  "CUAUHT\xc9MOC" = 8017,                     # CUAUHT\xc9MOC
  "DR. BELISARIO DOM\xcdNGUEZ" = 8022,        # DR. BELISARIO DOM\xcdNGUEZ
  "G\xd3MEZ FAR\xcdAS" = 8025,                   # G\xd3MEZ FAR\xcdAS
  "HUEJOTIT\xc1N" = 8033,                     # HUEJOTIT\xc1N
  "JIM\xc9NEZ" = 8036,                        # JIM\xc9NEZ
  "JU\xc1REZ" = 8037,                         # JU\xc1REZ
  "L\xd3PEZ" = 8039,                          # L\xd3PEZ
  "MATACH\xcd" = 8043,                        # MATACH\xcd
  "NVO. CASAS GRANDES" = 8050,            # NVO. CASAS GRANDES
  "SAN FCO. DE BORJA" = 8057,         # SAN FCO. DE BORJA
  "SAN FCO. DE CONCHOS" = 8058,       # SAN FCO. DE CONCHOS
  "SAN FCO. DEL ORO" = 8059,          # SAN FCO. DEL ORO
  "SANTA B\xc1RBARA" = 8060,                  # SANTA B\xc1RBARA
  "SATEV\xd3" = 8061,                         # SATEV\xd3
  "COYAME DEL SOTOL" = 8015,
  "COYAME DEL SOTOL EXTRAORDINARIO" = 8015,
  "CARICH\xcd" = 8012, 
  "TEM\xd3SACHI" = 8063,
  "TEMOSACHI" = 8063, # TEM\xd3SACHI
  "TEMOSACHIC" = 8063
)

# Assign unique IDs based on municipality name
data_1998_collapsed <- data_1998_collapsed %>%
  dplyr::mutate(uniqueid = municipality_ids[municipality])


# Calculate the sum of valid votes across PAN, PRI, PRD, PT, PVEM, PCDP
data_1998_collapsed <- data_1998_collapsed %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PCDP)), na.rm = TRUE))

# Calculate turnout as total votes / listanominal
data_1998_collapsed <- data_1998_collapsed %>%
  dplyr::mutate(
                year = 1998, 
                month = "July")

#####################################
### PROCESSING DATA FOR 2001
#####################################

# Import 2001 CSV data
data_2001 <- data.table::fread("../../../Data/Raw Electoral Data/Chihuahua - 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2001.csv")
names(data_2001)
# Rename columns
data_2001 <- data_2001 %>%
  dplyr::rename(municipality = MUNICIPIO, 
                section = SECCION,
                nulos = "VOTOS NULOS",
                listanominal = "LISTA NOMINAL",
                total = TOTAL)

# Drop rows where total is missing or zero
data_2001 <- data_2001 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Group by municipality and section, summing relevant columns
data_2001_collapsed <- data_2001 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(PAN:listanominal, sum, na.rm = TRUE))

# Rename variables after collapse
data_2001_collapsed <- data_2001_collapsed %>%
  dplyr::rename( 
         PC = "CONV.", 
         PAN_PRD = "COAL. PAN - PRD", 
         PRD_PT = "COAL. PRD - PT" , 
         PT_PVEM = "ALIANZA POR JU\xc1REZ", 
         PC_PSN = "ALIANZA CIUDADANA")

# Calculate turnout as total votes / listanominal
data_2001_collapsed <- data_2001_collapsed %>%
  dplyr::mutate(turnout = total / listanominal)

# Drop unnecessary columns
data_2001_collapsed <- data_2001_collapsed %>%
  dplyr::select(-"CAND. NO REGIST.")

names(data_2001_collapsed)

# Assign unique IDs based on municipality name
data_2001_collapsed <- data_2001_collapsed %>%
  dplyr::mutate(uniqueid = municipality_ids[municipality])%>% 
  dplyr::mutate(year = 2001, month = "July")

summary(data_2001_collapsed)

#####################################
### PROCESSING DATA FOR 2004
#####################################


# Import 2004 CSV data
data_2004 <- read_csv("../../../Data/Raw Electoral Data/Chihuahua - 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2004.csv")
names(data_2004)
# Rename columns
data_2004 <- data_2004 %>%
  dplyr::rename(municipality = MUNICIPIO, 
                section = SECCION,
                total = TOTAL,
                PAN_PRD_PC = TS, 
                PRI_PT_PVEM = ACLG, 
                listanominal = "LISTA NOMINAL",
                nulos = "VOTOS NULOS", 
                PC = "CONV.")

# Drop rows where total is missing or zero
data_2004 <- data_2004 %>%
  filter(!is.na(total) & total != 0)

# Group by municipality and section, summing relevant columns
data_2004_collapsed <- data_2004 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(PAN:listanominal, sum, na.rm = TRUE))

# Calculate turnout as total votes / listanominal
data_2004_collapsed <- data_2004_collapsed %>%
  dplyr::mutate(turnout = total / listanominal)

# Assign unique IDs based on municipality name
data_2004_collapsed <- data_2004_collapsed %>%
  dplyr::mutate(uniqueid = municipality_ids[municipality])

names(data_2004_collapsed)

# Calculate valid votes as the row total of relevant columns
data_2004_collapsed <- data_2004_collapsed %>%
  dplyr::mutate(valid = rowSums(across(c(PAN:PC)), na.rm = TRUE))

# Calculate turnout as total votes / listanominal
data_2004_collapsed <- data_2004_collapsed %>%
  dplyr::select(-"CAND. NO REGIST.") %>% 
  dplyr::mutate(year = 2004, month = "July")
  
  #####################################
  ### PROCESSING DATA FOR 2007
  #####################################
  
  # Import 2007 CSV data
data_2007 <- read_csv("../../../Data/Raw Electoral Data/Chihuahua - 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2007.csv")

  
  # Rename columns
data_2007 <- data_2007 %>%
    dplyr::rename(municipality = nombre_municipio, 
                  section = seccion, 
                  listanominal = lista_nominal,
                  total = Total,
                  nulos = Nulos
                  )
  
  # Drop rows where total is missing or zero
data_2007 <- data_2007 %>%
    filter(!is.na(total) & total != 0)
  
  # Group by municipality and section, summing relevant columns
data_2007_collapsed <- data_2007 %>%
    dplyr::group_by(municipality, section) %>%
    dplyr::summarise(across(c(PAN:total,listanominal), sum, na.rm = TRUE))
  
  # Rename variables after collapse
data_2007_collapsed <- data_2007_collapsed %>%
    dplyr::rename(
                  PRI_PANAL = "PRI-PANAL", 
                  PRD_PC = "PRD-PC")
  
  # Calculate turnout as total votes / listanominal
data_2007_collapsed <- data_2007_collapsed %>%
    dplyr::mutate(turnout = total / listanominal)

  # Assign unique IDs based on municipality name
data_2007_collapsed <- data_2007_collapsed %>%
    dplyr::mutate(uniqueid = municipality_ids[municipality])

  # Calculate valid votes as the row total of relevant columns
data_2007_collapsed <- data_2007_collapsed %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI_PANAL, PRD_PC, PT, PVEM,PAS)), na.rm = TRUE))

# Add year and month variables
data_2007_collapsed <- data_2007_collapsed %>%
  dplyr::mutate(year = 2007, month = "July")

#####################################
### PROCESSING DATA FOR 2010
#####################################

data_2010 <- read_csv("../../../Data/Raw Electoral Data/Chihuahua - 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2010.csv")
names(data_2010)
# Rename columns
data_2010 <- data_2010 %>%
  dplyr::rename(municipality = nombre_municipio, 
                section = seccion,
                nulos = Anulados,
                listanominal = "lista nominal")

# Drop rows where total is missing or zero
data_2010 <- data_2010 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Handle pripvempanal missing values
data_2010 <- data_2010 %>%
  dplyr::rename(pripvempanal="PRI-PVEM-PANAL",
                pripanalpevem="PRI-PANAL-PVEM") %>% 
  dplyr::mutate(pripvempanal = ifelse(is.na(pripvempanal) & !is.na(pripanalpevem), 
                         pripanalpevem, pripvempanal))

# Drop the duplicate column
data_2010 <- data_2010 %>%
  dplyr::select(-pripanalpevem)
summary(data_2010)
# Group by municipality and section, summing relevant columns
data_2010_collapsed <- data_2010 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(c(PAN:total,listanominal), sum, na.rm = TRUE))

# Rename variables after collapse
data_2010_collapsed <- data_2010_collapsed %>%
 dplyr::rename(PAN_PRD = "PAN-PRD", 
         PRI_PVEM_PANAL = pripvempanal, 
         PRI_PANAL = "PRI-PANAL", 
         PT_PC = "PT-PC")

# Calculate turnout as total votes / listanominal
data_2010_collapsed <- data_2010_collapsed %>%
  dplyr::mutate(turnout = total / listanominal)  

# Assign unique IDs based on municipality name
data_2010_collapsed <- data_2010_collapsed %>%
  dplyr::mutate(uniqueid = municipality_ids[municipality])

# Calculate valid votes as the row total of relevant columns
data_2010_collapsed <- data_2010_collapsed %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PAN_PRD, PRI_PVEM_PANAL, 
                                       PRI_PANAL, PRD, PT, PT_PC, PVEM, PC)), na.rm = TRUE))%>% 
  dplyr::mutate(year = 2010, month = "July")

summary(data_2010_collapsed)

#####################################
### PROCESSING DATA FOR 2013
#####################################


# Import 2013 Excel data
data_2013 <- read_excel("../../../Data/Raw Electoral Data/Chihuahua - 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2013.xlsx", sheet = "Sheet1")

# Rename columns
data_2013 <- data_2013 %>%
  dplyr::rename(municipality = MUNICIPIO)

# Split and convert 'CASILLA' into section
data_2013 <- data_2013 %>%
  separate(CASILLA, into = c("section"), sep = " ") %>%
  dplyr::mutate(section = as.numeric(section),
                total = rowSums(across(c(PAN:NULOS)), na.rm = TRUE))

# Drop rows where total is missing or zero
data_2013 <- data_2013 %>%
  dplyr::filter(!is.na(total) & total != 0)

# Group by municipality and section, summing relevant columns
data_2013_collapsed <- data_2013 %>%
  dplyr::group_by(municipality, section) %>%
  dplyr::summarise(across(PAN:total, sum, na.rm = TRUE))

# Import Listanominal 2013 data
listanominal_2013 <- read_excel("../../../Data/Raw Electoral Data/Chihuahua - 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Listanominal2013.xlsx", sheet = "Hoja1")

# Rename columns
listanominal_2013 <- listanominal_2013 %>%
  dplyr::rename(section = SECC, listanominal = LN) %>%
  dplyr::filter(!is.na(section) & listanominal != "") %>%
  dplyr::filter(listanominal != "N.I.") %>%
  dplyr::mutate(listanominal = as.numeric(listanominal))

# Sort sections and merge with 2013 data
data_2013_collapsed <- data_2013_collapsed %>%
  arrange(section) %>%
  left_join(listanominal_2013, by = "section")

# Handle extraordinary cases (replace listanominal values)
data_2013_collapsed <- data_2013_collapsed %>%
  dplyr::mutate(listanominal = ifelse(municipality == "COYAME DEL SOTOL EXTRAORDINARIO" & section == 261, 967, listanominal),
         listanominal = ifelse(municipality == "COYAME DEL SOTOL EXTRAORDINARIO" & section == 263, 120, listanominal),
         listanominal = ifelse(municipality == "COYAME DEL SOTOL EXTRAORDINARIO" & section == 264, 234, listanominal),
         listanominal = ifelse(municipality == "COYAME DEL SOTOL EXTRAORDINARIO" & section == 265, 528, listanominal))

# Calculate turnout as total votes / listanominal
data_2013_collapsed <- data_2013_collapsed %>%
  dplyr::mutate(turnout = total / listanominal)

# Assign unique IDs based on municipality name
data_2013_collapsed <- data_2013_collapsed %>%
  dplyr::mutate(uniqueid = municipality_ids[municipality])

# Calculate valid votes as the row total of relevant columns
data_2013_collapsed <- data_2013_collapsed %>%
  dplyr::mutate(valid = rowSums(across(c(PAN:PC)), na.rm = TRUE))

# Add year and month variables
data_2013_collapsed <- data_2013_collapsed %>%
  dplyr::mutate(year = 2013, month = ifelse(municipality == "COYAME DEL SOTOL EXTRAORDINARIO", "November", "July"))


#####################################
### PROCESSING DATA FOR 2016
#####################################

data_2016 <- read_excel("../../../Data/Raw Electoral Data/Chihuahua - 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayuntamientos_2016.xlsx", sheet = "Ayuntamientos")

# Create 'valid' by summing across relevant columns
data_2016 <- data_2016 %>%
  dplyr::mutate(valid = rowSums(across(c(PAN, PRI_PVEM_PANAL, PRI_PANAL, PRI_PVEM_PANAL_PT, 
                                         PRD, PVEM, PT, MC, MORENA, PES, CI_1, CI_2, CI_3)), 
                                na.rm = TRUE))

# Generate 'total' as valid + no_reg + nulo
data_2016 <- data_2016 %>%
  dplyr::mutate(total = valid + no_reg + nulo)

# Assign unique IDs based on municipality name
data_2016 <- data_2016 %>%
  dplyr::mutate(uniqueid = municipality_ids[municipality]) %>% 
  dplyr::rename(nulos = nulo)

# Group by municipality, section, and uniqueid, and sum the relevant columns
data_2016_collapsed <- data_2016 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN, PRI_PVEM_PANAL, PRI_PANAL, PRI_PVEM_PANAL_PT, 
                     PRD, PVEM, PT, MC, MORENA, PES, CI_1, CI_2, CI_3, total, valid, nulos), 
                   sum, na.rm = TRUE))

# Add year, month, and state variables
data_2016_collapsed <- data_2016_collapsed %>%
  mutate(year = 2016, month = "June")

# Load the Listanominal dataset for 2016
ln_2016 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2016/LN2016.dta")

ln_2016 <- ln_2016 %>%
  dplyr::filter(entidad == 8 & month == 6) %>%
  dplyr::mutate(uniqueid = (entidad * 1000) + municipio) %>%
  dplyr::select(seccion, lista) %>% 
  dplyr::rename(section = seccion)

data_2016_collapsed <- data_2016_collapsed %>%
  dplyr::left_join(ln_2016, by = c("section")) %>%
  dplyr::rename(listanominal = lista)

# Calculate turnout
data_2016_collapsed <- data_2016_collapsed %>%
  dplyr::mutate(turnout = total / listanominal)

summary(data_2016_collapsed)
rm(data_2016)
rm(ln_2016)

#####################################
### PROCESSING DATA FOR 2018
#####################################

# Load the 2018 dataset from the Excel sheet
data_2018 <- read_excel("../../../Data/Raw Electoral Data/Chihuahua - 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayuntamientos_2018.xlsx", 
                        sheet = "Ayuntamiento")
names(data_2018)
# Update PT_MORENA_PES and drop relevant columns for specified municipalities

data_2018 <- data_2018 %>%
  dplyr::mutate(PT_MORENA_PES = ifelse(municipality != "BATOPILAS DE MANUEL GOMEZ MORIN" & 
                                  municipality != "CARICHI" &
                                  municipality != "LA CRUZ" & 
                                  municipality != "HUEJOTITAN" &
                                  municipality != "IGNACIO ZARAGOZA" & 
                                  municipality != "SANTA BARBARA",
                                rowSums(across(c(PT_MORENA_PES,PT_MORENA,PT_PES,
                                                   MORENA_PES,MORENA,PES,PT)),na.rm = TRUE), PT_MORENA_PES)) %>%
    dplyr::mutate(across(c(PT, MORENA, PES), ~ifelse(municipality != "BATOPILAS DE MANUEL GOMEZ MORIN" & municipality != "CARICHI" &
                                              municipality != "LA CRUZ" & municipality != "HUEJOTITAN" &
                                              municipality != "IGNACIO ZARAGOZA" & municipality != "SANTA BARBARA", NA, .)))

# Drop unnecessary columns
data_2018 <- data_2018 %>% 
  dplyr::select(-PT_MORENA, -PT_PES, -MORENA_PES)

# Combine PAN and MC for specified municipalities
data_2018 <- data_2018 %>%
  dplyr::mutate(PAN_MC = ifelse(municipality != "BACHINIVA" & municipality != "BOCOYNA" &
                           municipality != "CASAS GRANDES" & municipality != "HIDALGO DEL PARRAL" &
                           municipality != "IGNACIO ZARAGOZA" & municipality != "LOPEZ" &
                           municipality != "NONOAVA" & municipality != "ROSARIO",
                           rowSums(across(c(PAN,MC,PAN_MC)),na.rm = TRUE), PAN_MC)) %>%
  dplyr::mutate(across(c(PAN, MC), ~ifelse(municipality != "BACHINIVA" & municipality != "BOCOYNA" &
                                      municipality != "CASAS GRANDES" & municipality != "HIDALGO DEL PARRAL" &
                                      municipality != "IGNACIO ZARAGOZA" & municipality != "LOPEZ" &
                                      municipality != "NONOAVA" & municipality != "ROSARIO", NA, .)))

# Rename columns for consistency
data_2018 <- data_2018 %>%
  dplyr::rename(PANAL = PNA, 
                no_reg = "CAND. NO REG", 
                nulos = NULOS)

# Assign unique IDs based on municipality name
data_2018 <- data_2018 %>%
  dplyr::mutate(uniqueid = municipality_ids[municipality])
names(data_2018)
# Create 'total' by summing across relevant columns
data_2018 <- data_2018 %>%
  dplyr::mutate(total = rowSums(across(c(PAN:nulos)),na.rm = TRUE))
                         
# Group by municipality, section, and uniqueid, and sum the relevant columns
data_2018_collapsed <- data_2018 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(listanominal:total), 
                          sum, na.rm = TRUE))

# Calculate turnout

data_2018_collapsed <- data_2018_collapsed %>%
  dplyr::mutate(turnout = total / listanominal) %>% 
  dplyr::select(-no_reg)%>% 
  dplyr::mutate(year = 2018, month = "July")

summary(Chihuahua_all)

Chihuahua_all <- bind_rows(
                         data_1998_collapsed,
                         data_2001_collapsed,
                         data_2004_collapsed,
                         data_2007_collapsed,
                         data_2010_collapsed,
                         data_2013_collapsed,
                         data_2016_collapsed,
                         data_2018_collapsed)

data.table::fwrite(Chihuahua_all,"../../../Processed Data/chihuahua/chihuahua_process_raw_data.csv")
