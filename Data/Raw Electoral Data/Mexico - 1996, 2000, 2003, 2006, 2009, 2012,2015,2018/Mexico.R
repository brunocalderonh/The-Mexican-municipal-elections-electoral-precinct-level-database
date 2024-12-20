# Load required libraries
library(dplyr)
library(readr)

# Set working directory (adjust this based on your file location)
setwd("D:/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Mexico - 1996, 2000, 2003, 2006, 2009, 2012")

# Helper function to identify winners based on max votes
get_winner <- function(df, party_vars) {
  df <- df %>%
    rowwise() %>%
    mutate(winner = names(df)[which.max(c_across(all_of(party_vars)))]) %>%
    ungroup()
  return(df)
}

# Helper function to create uniqueid
create_uniqueid <- function(df, year, municipality_var) {
  df <- df %>%
    mutate(uniqueid = case_when(
      municipality == "ACAMBAY" ~ 15001,
      municipality == "ACOLMAN" ~ 15002,
      municipality == "ACULCO" ~ 15003,
      municipality == "ALMOLOYA DE ALQUISIRAS" ~ 15004,
      municipality == "ALMOLOYA DE JUAREZ" ~ 15005,
      municipality == "ALMOLOYA DEL RIO" ~ 15006,
      municipality == "AMANALCO" ~ 15007,
      municipality == "AMATEPEC" ~ 15008,
      municipality == "AMECAMECA" ~ 15009,
      municipality == "APAXCO" ~ 15010,
      municipality == "ATENCO" ~ 15011,
      municipality == "ATIZAPAN" ~ 15012,
      municipality == "ATIZAPAN DE ZARAGOZA" ~ 15013,
      municipality == "ATLACOMULCO" ~ 15014,
      municipality == "ATLAUTLA" ~ 15015,
      municipality == "AXAPUSCO" ~ 15016,
      municipality == "AYAPANGO" ~ 15017,
      municipality == "CALIMAYA" ~ 15018,
      municipality == "CAPULHUAC" ~ 15019,
      municipality == "CHALCO" ~ 15025,
      municipality == "CHAPA DE MOTA" ~ 15026,
      municipality == "CHAPULTEPEC" ~ 15027,
      municipality == "CHIAUTLA" ~ 15028,
      municipality == "CHICOLOAPAN" ~ 15029,
      municipality == "CHICONCUAC" ~ 15030,
      municipality == "CHIMALHUACAN" ~ 15031,
      municipality == "COACALCO" ~ 15020,
      municipality == "COATEPEC HARINAS" ~ 15021,
      municipality == "COCOTITLAN" ~ 15022,
      municipality == "COYOTEPEC" ~ 15023,
      municipality == "CUAUTITLAN" ~ 15024,
      municipality == "CUAUTITLAN IZCALLI" ~ 15121,
      TRUE ~ 0
    ))
  return(df)
}

### Process Data for Each Year ###

# -------------------------------------- #
# 1. Process Election Data for 1996
# -------------------------------------- #
process_1996 <- function() {
  # Read the CSV file for 1996
  df_1996 <- read_csv("Ayu_Seccion_1996_No_LN.csv")
  
  # Rename columns
  df_1996 <- df_1996 %>%
    rename(municipality = nombre,
           section = seccion,
           noregistrados = otros,
           PAN = pan,
           PRI = pri,
           PRD = prd,
           PC = pc,
           PT = pt,
           PVEM = pvem,
           PPS = pps,
           PDM = pdm,
           PPM = ppm,
           total = total)
  
  # Filter and clean data
  df_1996 <- df_1996 %>%
    filter(!is.na(municipality), section != 0, total > 0) %>%
    select(municipality, section, PAN, PRI, PRD, PC, PT, PVEM, PPS, PDM, PPM, total)
  
  # Collapse by municipality and section (sum votes)
  df_1996 <- df_1996 %>%
    group_by(municipality, section) %>%
    summarize(across(PAN:total, sum, na.rm = TRUE))
  
  # Generate valid votes and turnout
  df_1996 <- df_1996 %>%
    mutate(valid = rowSums(across(PAN:PPM), na.rm = TRUE))
  
  # Create uniqueid
  df_1996 <- create_uniqueid(df_1996, 1996, "municipality")
  
  # Identify the winner based on the highest votes
  df_1996 <- get_winner(df_1996, party_vars = c("PAN", "PRI", "PRD", "PC", "PT", "PVEM", "PPS", "PDM", "PPM"))
  
  # Assign year and month
  df_1996 <- df_1996 %>%
    mutate(year = 1996, month = "November")
  
  return(df_1996)
}

# -------------------------------------- #
# 2. Process Election Data for 2000
# -------------------------------------- #
process_2000 <- function() {
  # Read the CSV file for 2000
  df_2000 <- read_csv("Ayu_Seccion_2000_No_LN.csv")
  
  # Rename columns
  df_2000 <- df_2000 %>%
    rename(municipality = nombre,
           section = seccion,
           PAN = pan,
           PRI = pri,
           PRD = prd,
           PC = cd,  # Partido Cardenista (PC)
           PT = pt,
           PVEM = pvem,
           PAS = pas,
           PCD = pcd,  # Partido de Centro Democrático
           PSN = psn,
           PARM = parm,
           PDS = ds,  # Partido Democracia Social
           total = total)
  
  # Filter and clean data
  df_2000 <- df_2000 %>%
    filter(!is.na(municipality), section != 0, total > 0) %>%
    select(municipality, section, PAN, PRI, PRD, PC, PT, PVEM, PAS, PCD, PSN, PARM, PDS, total)
  
  # Collapse by municipality and section
  df_2000 <- df_2000 %>%
    group_by(municipality, section) %>%
    summarize(across(PAN:total, sum, na.rm = TRUE))
  
  # Generate valid votes and turnout
  df_2000 <- df_2000 %>%
    mutate(valid = rowSums(across(PAN:PDS), na.rm = TRUE))
  
  # Create uniqueid
  df_2000 <- create_uniqueid(df_2000, 2000, "municipality")
  
  # Identify the winner
  df_2000 <- get_winner(df_2000, party_vars = c("PAN", "PRI", "PRD", "PC", "PT", "PVEM", "PAS", "PCD", "PSN", "PARM", "PDS"))
  
  # Assign year and month
  df_2000 <- df_2000 %>%
    mutate(year = 2000, month = "July")
  
  return(df_2000)
}

# -------------------------------------- #
# 3. Process Election Data for 2003
# -------------------------------------- #
process_2003 <- function() {
  # Read the CSV file for 2003
  df_2003 <- read_csv("Ayu_Seccion_2003_No_LN.csv")
  
  # Rename columns
  df_2003 <- df_2003 %>%
    rename(municipality = nombre,
           section = seccion,
           PAN = pan,
           PRI_PVEM = apt,  # PRI and PVEM coalition
           PRD = prd,
           PC = conv,
           PT = pt,
           PSN = psn,
           PAS = pas,
           PCEM = pacem,  # Partido Campesino
           total = total)
  
  # Filter and clean data
  df_2003 <- df_2003 %>%
    filter(!is.na(municipality), section != 0, total > 0) %>%
    select(municipality, section, PAN, PRI_PVEM, PRD, PC, PT, PSN, PAS, PCEM, total)
  
  # Collapse by municipality and section
  df_2003 <- df_2003 %>%
    group_by(municipality, section) %>%
    summarize(across(PAN:total, sum, na.rm = TRUE))
  
  # Generate valid votes and turnout
  df_2003 <- df_2003 %>%
    mutate(valid = rowSums(across(PAN:PCEM), na.rm = TRUE))
  
  # Create uniqueid
  df_2003 <- create_uniqueid(df_2003, 2003, "municipality")
  
  # Identify the winner
  df_2003 <- get_winner(df_2003, party_vars = c("PAN", "PRI_PVEM", "PRD", "PC", "PT", "PSN", "PAS", "PCEM"))
  
  # Assign year and month
  df_2003 <- df_2003 %>%
    mutate(year = 2003, month = "March")
  
  return(df_2003)
}

# -------------------------------------- #
# 4. Process Election Data for 2006
# -------------------------------------- #
process_2006 <- function() {
  # Read the CSV file for 2006
  df_2006 <- read_csv("Ayu_Seccion_2006.csv")
  
  # Rename columns
  df_2006 <- df_2006 %>%
    rename(municipality = nombre_municipio,
           section = seccion,
           listanominal = lista_nominal,
           PAN = pan,
           PRI = pri,
           PRD = prd,
           PT = pt,
           PC = pc,
           PAN_PC = pan_pc,
           PAN_PRD = pan_prd,
           PAN_PRD_PT = pan_prd_pt,
           PAN_PRD_PT_PC = pan_prd_pt_pc,
           PRD_PC = prd_pc,
           PRD_PT = prd_pt,
           PRD_PT_PC = prd_pt_pc,
           PT_PC = pt_pc,
           total = total)
  
  # Filter and clean data
  df_2006 <- df_2006 %>%
    filter(!is.na(municipality), section != 0, total > 0) %>%
    select(municipality, section, PAN, PRI, PRD, PT, PC, PAN_PC, PAN_PRD, PAN_PRD_PT, PAN_PRD_PT_PC, PRD_PC, PRD_PT, PRD_PT_PC, PT_PC, total)
  
  # Collapse by municipality and section
  df_2006 <- df_2006 %>%
    group_by(municipality, section) %>%
    summarize(across(PAN:total, sum, na.rm = TRUE))
  
  # Generate valid votes and turnout
  df_2006 <- df_2006 %>%
    mutate(valid = rowSums(across(PAN:PT_PC), na.rm = TRUE))
  
  # Create uniqueid
  df_2006 <- create_uniqueid(df_2006, 2006, "municipality")
  
  # Identify the winner
  df_2006 <- get_winner(df_2006, party_vars = c("PAN", "PRI", "PRD", "PT", "PC", "PAN_PC", "PAN_PRD", "PAN_PRD_PT", "PAN_PRD_PT_PC", "PRD_PC", "PRD_PT", "PRD_PT_PC", "PT_PC"))
  
  # Assign year and month
  df_2006 <- df_2006 %>%
    mutate(year = 2006, month = "March")
  
  return(df_2006)
}

# -------------------------------------- #
# 5. Process Election Data for 2009
# -------------------------------------- #
process_2009 <- function() {
  # Read the CSV file for 2009
  df_2009 <- read_csv("Ayu_Seccion_2009.csv")
  
  # Rename columns
  df_2009 <- df_2009 %>%
    rename(municipality = nombre_municipio,
           section = seccion,
           listanominal = lista_nominal,
           PAN = pan,
           PRI = pri,
           PRD = prd,
           PT = pt,
           PC = pc,
           PAN_PC = panc,  # PAN-PC coalition
           PRI_PVEM_PANAL_PSD_PFD = pripvemnapsdpfd,  # PRI-PVEM-PANAL-PSD-PFD coalition
           PRD_PT = prdpt,
           PT_PC = ptc,
           total = total)
  
  # Filter and clean data
  df_2009 <- df_2009 %>%
    filter(!is.na(municipality), section != 0, total > 0) %>%
    select(municipality, section, PAN, PRI, PRD, PT, PC, PAN_PC, PRI_PVEM_PANAL_PSD_PFD, PRD_PT, PT_PC, total, listanominal)
  
  # Collapse by municipality and section
  df_2009 <- df_2009 %>%
    group_by(municipality, section) %>%
    summarize(across(PAN:total, sum, na.rm = TRUE))
  
  # Generate valid votes
  df_2009 <- df_2009 %>%
    mutate(valid = rowSums(across(PAN:PT_PC), na.rm = TRUE))
  
  # Create uniqueid
  df_2009 <- create_uniqueid(df_2009, 2009, "municipality")
  
  # Identify the winner
  df_2009 <- get_winner(df_2009, party_vars = c("PAN", "PRI", "PRD", "PT", "PC", "PAN_PC", "PRI_PVEM_PANAL_PSD_PFD", "PRD_PT", "PT_PC"))
  
  # Assign year and month
  df_2009 <- df_2009 %>%
    mutate(year = 2009, month = "July")
  
  return(df_2009)
}

# -------------------------------------- #
# 6. Process Election Data for 2012
# -------------------------------------- #
process_2012 <- function() {
  # Read the Excel file for 2012
  df_2012 <- read.csv("Ayu_Seccion_2012.csv")
  
  # Rename columns
  df_2012 <- df_2012 %>%
    rename(municipality = nombre_municipio,
           section = seccion,
           listanominal = lista_nominal,
           PAN = PAN,
           PRI_PVEM_PANAL = PRIPVEMPANAL,  # PRI, PVEM, PANAL coalition
           PRD_PT_PC = PRDPTMC,  # PRD, PT, PC coalition
           PRD_PT = PRDPT,
           PRD_PC = PRDMC,  # PRD, PC coalition
           PT_PC = PRMC,  # PT, PC coalition
           PC = MC,  # MC (Movimiento Ciudadano)
           total = total)
  
  # Filter and clean data
  df_2012 <- df_2012 %>%
    filter(!is.na(municipality), section != 0, total > 0) %>%
    select(municipality, section, PAN, PRI_PVEM_PANAL, PRD_PT_PC, PRD_PT, PRD_PC, PT_PC, PC, total, listanominal)
  
  # Collapse by municipality and section
  df_2012 <- df_2012 %>%
    group_by(municipality, section) %>%
    summarize(across(PAN:total, sum, na.rm = TRUE))
  
  # Generate valid votes
  df_2012 <- df_2012 %>%
    mutate(valid = rowSums(across(PAN:PT_PC), na.rm = TRUE))
  
  # Create uniqueid
  df_2012 <- create_uniqueid(df_2012, 2012, "municipality")
  
  # Identify the winner
  df_2012 <- get_winner(df_2012, party_vars = c("PAN", "PRI_PVEM_PANAL", "PRD_PT_PC", "PRD_PT", "PRD_PC", "PT_PC", "PC"))
  
  # Assign year and month
  df_2012 <- df_2012 %>%
    mutate(year = 2012, month = "July")
  
  return(df_2012)
}

# -------------------------------------- #
# Combine All Years Into One Dataset
# -------------------------------------- #
combine_all_years <- function() {
  # Process each year's data
  df_1996 <- process_1996()
  df_2000 <- process_2000()
  df_2003 <- process_2003()
  df_2006 <- process_2006()
  df_2009 <- process_2009()
  df_2012 <- process_2012()
  
  # Combine all years into one dataset
  df_all <- bind_rows(df_1996, df_2000, df_2003, df_2006, df_2009, df_2012)
  
  # Save the final dataset
  write.csv(df_all, "Mexico_ALL.csv", row.names = FALSE)
}

# Run the combine function to process all years and save the dataset
combine_all_years()

# Load necessary libraries
library(dplyr)
library(readxl)
library(Hmisc)

# -------------------------------------- #
# Helper Functions for Unique ID
# -------------------------------------- #
create_uniqueid <- function(df) {
  df <- df %>%
    mutate(uniqueid = case_when(
      municipality == "ACAMBAY" ~ 15001,
      municipality == "ACOLMAN" ~ 15002,
      municipality == "ACULCO" ~ 15003,
      municipality == "ALMOLOYA DE ALQUISIRAS" ~ 15004,
      municipality == "ALMOLOYA DE JUAREZ" ~ 15005,
      municipality == "ALMOLOYA DEL RIO" ~ 15006,
      municipality == "AMANALCO" ~ 15007,
      municipality == "AMATEPEC" ~ 15008,
      municipality == "AMECAMECA" ~ 15009,
      municipality == "APAXCO" ~ 15010,
      municipality == "ATENCO" ~ 15011,
      municipality == "ATIZAPAN" ~ 15012,
      municipality == "ATIZAPAN DE ZARAGOZA" ~ 15013,
      municipality == "ATLACOMULCO" ~ 15014,
      municipality == "ATLAUTLA" ~ 15015,
      municipality == "AXAPUSCO" ~ 15016,
      municipality == "AYAPANGO" ~ 15017,
      municipality == "CALIMAYA" ~ 15018,
      municipality == "CAPULHUAC" ~ 15019,
      municipality == "CHALCO" ~ 15025,
      municipality == "CHAPA DE MOTA" ~ 15026,
      municipality == "CHAPULTEPEC" ~ 15027,
      municipality == "CHIAUTLA" ~ 15028,
      municipality == "CHICOLOAPAN" ~ 15029,
      municipality == "CHICONCUAC" ~ 15030,
      municipality == "CHIMALHUACAN" ~ 15031,
      municipality == "COACALCO DE BERRIOZABAL" | municipality == "COACALCO" ~ 15020,
      municipality == "COATEPEC HARINAS" ~ 15021,
      municipality == "COCOTITLAN" ~ 15022,
      municipality == "COYOTEPEC" ~ 15023,
      municipality == "CUAUTITLAN" ~ 15024,
      municipality == "CUAUTITLAN IZCALLI" ~ 15121,
      municipality == "DONATO GUERRA" ~ 15032,
      municipality == "ECATEPEC DE MORELOS" | municipality == "ECATEPEC" ~ 15033,
      municipality == "ECATZINGO" ~ 15034,
      municipality == "EL ORO" ~ 15064,
      municipality == "HUEHUETOCA" ~ 15035,
      municipality == "HUEYPOXTLA" ~ 15036,
      municipality == "HUIXQUILUCAN" ~ 15037,
      municipality == "ISIDRO FABELA" ~ 15038,
      municipality == "IXTAPALUCA" ~ 15039,
      municipality == "IXTAPAN DE LA SAL" ~ 15040,
      municipality == "IXTAPAN DEL ORO" ~ 15041,
      municipality == "IXTLAHUACA" ~ 15042,
      municipality == "JALTENCO" ~ 15044,
      municipality == "JILOTEPEC" ~ 15045,
      municipality == "JILOTZINGO" ~ 15046,
      municipality == "JIQUIPILCO" ~ 15047,
      municipality == "JOCOTITLAN" ~ 15048,
      municipality == "JOQUICINGO" ~ 15049,
      municipality == "JUCHITEPEC" ~ 15050,
      municipality == "LA PAZ" ~ 15070,
      municipality == "LERMA" ~ 15051,
      municipality == "LUVIANOS" ~ 15123,
      municipality == "MALINALCO" ~ 15052,
      municipality == "MELCHOR OCAMPO" ~ 15053,
      municipality == "METEPEC" ~ 15054,
      municipality == "MEXICALTZINGO" ~ 15055,
      municipality == "MORELOS" ~ 15056,
      municipality == "NAUCALPAN DE JUAREZ" | municipality == "NAUCALPAN" ~ 15057,
      municipality == "NEXTLALPAN" ~ 15059,
      municipality == "NEZAHUALCOYOTL" ~ 15058,
      municipality == "NICOLAS ROMERO" ~ 15060,
      municipality == "NOPALTEPEC" ~ 15061,
      municipality == "OCOYOACAC" ~ 15062,
      municipality == "OCUILAN" ~ 15063,
      municipality == "OTUMBA" ~ 15065,
      municipality == "OTZOLOAPAN" ~ 15066,
      municipality == "OTZOLOTEPEC" ~ 15067,
      municipality == "OZUMBA" ~ 15068,
      municipality == "PAPALOTLA" ~ 15069,
      municipality == "POLOTITLAN" ~ 15071,
      municipality == "RAYON" ~ 15072,
      municipality == "SAN ANTONIO LA ISLA" ~ 15073,
      municipality == "SAN FELIPE DEL PROGRESO" ~ 15074,
      municipality == "SAN JOSE DEL RINCON" ~ 15124,
      municipality == "SAN MARTIN DE LAS PIRAMIDES" ~ 15075,
      municipality == "SAN MATEO ATENCO" ~ 15076,
      municipality == "SAN SIMON DE GUERRERO" ~ 15077,
      municipality == "SANTO TOMAS" ~ 15078,
      municipality == "SOYANIQUILPAN DE JUAREZ" ~ 15079,
      municipality == "SULTEPEC" ~ 15080,
      municipality == "TECAMAC" ~ 15081,
      municipality == "TEJUPILCO" ~ 15082,
      municipality == "TEMAMATLA" ~ 15083,
      municipality == "TEMASCALAPA" ~ 15084,
      municipality == "TEMASCALCINGO" ~ 15085,
      municipality == "TEMASCALTEPEC" ~ 15086,
      municipality == "TEMOAYA" ~ 15087,
      municipality == "TENANCINGO" ~ 15088,
      municipality == "TENANGO DEL AIRE" ~ 15089,
      municipality == "TENANGO DEL VALLE" ~ 15090,
      municipality == "TEOLOYUCAN" ~ 15091,
      municipality == "TEOTIHUACAN" ~ 15092,
      municipality == "TEPETLAOXTOC" ~ 15093,
      municipality == "TEPETLIXPA" ~ 15094,
      municipality == "TEPOTZOTLAN" ~ 15095,
      municipality == "TEQUIXQUIAC" ~ 15096,
      municipality == "TEXCALTITLAN" ~ 15097,
      municipality == "TEXCALYACAC" ~ 15098,
      municipality == "TEXCOCO" ~ 15099,
      municipality == "TEZOYUCA" ~ 15100,
      municipality == "TIANGUISTENCO" ~ 15101,
      municipality == "TIMILPAN" ~ 15102,
      municipality == "TLALMANALCO" ~ 15103,
      municipality == "TLALNEPANTLA DE BAZ" | municipality == "TLALNEPANTLA" ~ 15104,
      municipality == "TLATLAYA" ~ 15105,
      municipality == "TOLUCA" ~ 15106,
      municipality == "TONANITLA" ~ 15125,
      municipality == "TONATICO" ~ 15107,
      municipality == "TULTEPEC" ~ 15108,
      municipality == "TULTITLAN" ~ 15109,
      municipality == "VALLE DE BRAVO" ~ 15110,
      municipality == "VALLE DE CHALCO SOLIDARIDAD" | municipality == "V. DE CHALCO SOLIDARIDAD" ~ 15122,
      municipality == "VILLA DE ALLENDE" ~ 15111,
      municipality == "VILLA DEL CARBON" ~ 15112,
      municipality == "VILLA GUERRERO" ~ 15113,
      municipality == "VILLA VICTORIA" ~ 15114,
      municipality == "XALATLACO" ~ 15043,
      municipality == "XONACATLAN" ~ 15115,
      municipality == "ZACAZONAPAN" ~ 15116,
      municipality == "ZACUALPAN" ~ 15117,
      municipality == "ZINACANTEPEC" ~ 15118,
      municipality == "ZUMPAHUACAN" ~ 15119,
      municipality == "ZUMPANGO" ~ 15120,
      TRUE ~ 0  # For any unrecognized municipality, set uniqueid to 0
    ))
  return(df)
}

# -------------------------------------- #
# 1. Process Election Data for 2015
# -------------------------------------- #
process_2015 <- function() {
  # Read the Excel file for 2015
  df_2015 <- read_excel("Ayuntamientos_2015.xlsx", sheet = "Aytto x secccion", col_names = TRUE)
  
  # Drop irrelevant columns and rename variables
  df_2015 <- df_2015 %>%
    select(-Municipio) %>%
    rename(municipality = municipio, section = seccion)
  
  # Create uniqueid
  df_2015 <- create_uniqueid(df_2015)
  
  # Collapse by municipality and section
  df_2015 <- df_2015 %>%
    group_by(municipality, section, uniqueid) %>%
    summarize(across(PAN:total, sum, na.rm = TRUE))
  
  # Calculate coalitions and clean data
  df_2015 <- df_2015 %>%
    mutate(PRI_PVEM_PANAL = if_else(coalPRIPVEMNA == 1, PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL, PRI_PVEM_PANAL),
           PAN_PT = if_else(coalPANPT == 1, PAN_PT + PAN + PT, PAN_PT),
           PRI = if_else(coalPRIPVEMNA == 1, NA, PRI),
           PVEM = if_else(coalPRIPVEMNA == 1, NA, PVEM),
           PANAL = if_else(coalPRIPVEMNA == 1, NA, PANAL),
           PAN = if_else(coalPANPT == 1, NA, PAN),
           PT = if_else(coalPANPT == 1, NA, PT)) %>%
    select(-coalPRIPVEMNA, -coalPANPT)
  
  # Generate valid votes and municipal totals
  df_2015 <- df_2015 %>%
    mutate(valid = rowSums(select(., PAN:PRI_PVEM_PANAL), na.rm = TRUE))
  
  # Calculate turnout and ranks
  df_2015 <- df_2015 %>%
    group_by(uniqueid) %>%
    mutate(across(PAN:valid, sum, na.rm = TRUE, .names = "mun_{.col}"),
           inv_PAN = 1 / mun_PAN, inv_PRI = 1 / mun_PRI, inv_PRD = 1 / mun_PRD,
           inv_PT = 1 / mun_PT, inv_PVEM = 1 / mun_PVEM) %>%
    mutate(PAN_r = rank(inv_PAN), PRI_r = rank(inv_PRI), PRD_r = rank(inv_PRD),
           PT_r = rank(inv_PT), PVEM_r = rank(inv_PVEM))
  
  # Identify winners, second, and third places
  df_2015 <- df_2015 %>%
    mutate(winner = case_when(PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", PT_r == 1 ~ "PT", PVEM_r == 1 ~ "PVEM", TRUE ~ ""),
           second = case_when(PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", PRD_r == 2 ~ "PRD", PT_r == 2 ~ "PT", PVEM_r == 2 ~ "PVEM", TRUE ~ ""),
           third = case_when(PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", PRD_r == 3 ~ "PRD", PT_r == 3 ~ "PT", PVEM_r == 3 ~ "PVEM", TRUE ~ ""),
           winner = if_else(grepl("CI_", winner), "Independent", winner),
           second = if_else(grepl("CI_", second), "Independent", second),
           third = if_else(grepl("CI_", third), "Independent", third),
           year = 2015, month = "June", STATE = "ESTADO DE MEXICO")
  
  return(df_2015)
}

# -------------------------------------- #
# 2. Process Election Data for 2016
# -------------------------------------- #
process_2016 <- function() {
  # Similar steps for 2016 as done for 2015
  df_2016 <- read_excel("Extraordinario 2016.xlsx", sheet = "Sheet1", col_names = TRUE)
  df_2016 <- df_2016 %>%
    rename(section = SECCIÓN) %>%
    mutate(PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL,
           PRD_PT = PRD_PT + PRD + PT) %>%
    select(-PRI, -PVEM, -PANAL, -PRI_PVEM, -PRI_PANAL, -PVEM_PANAL, -PRD, -PT) %>%
    mutate(uniqueid = 15028) %>%
    group_by(municipality, section, uniqueid) %>%
    summarize(across(PAN:PRD_PT, sum, na.rm = TRUE))
  
  return(df_2016)
}

# -------------------------------------- #
# 3. Process Election Data for 2018
# -------------------------------------- #
process_2018 <- function() {
  # Read the Excel file for 2018
  df_2018 <- read_excel("Ayuntamientos_2018.xlsx", sheet = "Ayuntamientos", col_names = TRUE)
  
  # Handle Independent votes (CI_ variables) and rename columns
  df_2018 <- df_2018 %>%
    mutate(indep1 = rowSums(select(., starts_with("CI_")), na.rm = TRUE),
           indep2 = CI_16) %>%
    select(-starts_with("CI_")) %>%
    rename(CI_1 = indep1, CI_2 = indep2, municipality = municipio, section = id_seccion)
  
  # Create uniqueid
  df_2018 <- create_uniqueid(df_2018)
  
  # Collapse by municipality and section
  df_2018 <- df_2018 %>%
    group_by(municipality, section, uniqueid) %>%
    summarize(across(listanominal:CI_2, sum, na.rm = TRUE))
  
  # Adjust coalitions and calculate valid votes
  df_2018 <- df_2018 %>%
    mutate(PT_MORENA_PES = if_else(coalpbt == 1, PT_MORENA_PES + PT_MORENA + PT_PES + MORENA_PES + PT + MORENA + PES, PT_MORENA_PES),
           PAN_PRD_MC = if_else(coalfrente == 1, PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC, PAN_PRD_MC)) %>%
    select(-PT_MORENA, -PT_PES, -MORENA_PES, -PAN_PRD, -PAN_MC, -PRD_MC) %>%
    rename(PANAL = NA)
  
  # Calculate valid votes, turnout, and municipal totals
  df_2018 <- df_2018 %>%
    mutate(valid = rowSums(select(., PAN:CI_2), na.rm = TRUE))
  
  # Calculate rankings and determine winners
  df_2018 <- df_2018 %>%
    group_by(uniqueid) %>%
    mutate(across(PAN:valid, sum, na.rm = TRUE, .names = "mun_{.col}")) %>%
    rowranks(c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "MORENA", "PES", "VIA_RADICAL", "PAN_PRD_MC", "PT_MORENA_PES", "CI_1", "CI_2"),
             gen = c("PAN_r", "PRI_r", "PRD_r", "PT_r", "PVEM_r", "MC_r", "PANAL_r", "MORENA_r", "PES_r", "VIA_RADICAL_r", "PAN_PRD_MC_r", "PT_MORENA_PES_r", "CI_1_r", "CI_2_r")) %>%
    mutate(winner = case_when(PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", PT_r == 1 ~ "PT", PVEM_r == 1 ~ "PVEM", MC_r == 1 ~ "MC",
                              PANAL_r == 1 ~ "PANAL", MORENA_r == 1 ~ "MORENA", PES_r == 1 ~ "PES", VIA_RADICAL_r == 1 ~ "VIA_RADICAL",
                              PAN_PRD_MC_r == 1 ~ "PAN_PRD_MC", PT_MORENA_PES_r == 1 ~ "PT_MORENA_PES", CI_1_r == 1 ~ "Independent", TRUE ~ ""),
           second = case_when(PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", PRD_r == 2 ~ "PRD", PT_r == 2 ~ "PT", PVEM_r == 2 ~ "PVEM", MC_r == 2 ~ "MC",
                              PANAL_r == 2 ~ "PANAL", MORENA_r == 2 ~ "MORENA", PES_r == 2 ~ "PES", VIA_RADICAL_r == 2 ~ "VIA_RADICAL",
                              PAN_PRD_MC_r == 2 ~ "PAN_PRD_MC", PT_MORENA_PES_r == 2 ~ "PT_MORENA_PES", CI_1_r == 2 ~ "Independent", TRUE ~ ""),
           third = case_when(PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", PRD_r == 3 ~ "PRD", PT_r == 3 ~ "PT", PVEM_r == 3 ~ "PVEM", MC_r == 3 ~ "MC",
                             PANAL_r == 3 ~ "PANAL", MORENA_r == 3 ~ "MORENA", PES_r == 3 ~ "PES", VIA_RADICAL_r == 3 ~ "VIA_RADICAL",
                             PAN_PRD_MC_r == 3 ~ "PAN_PRD_MC", PT_MORENA_PES_r == 3 ~ "PT_MORENA_PES", CI_1_r == 3 ~ "Independent", TRUE ~ ""),
           year = 2018, month = "July", STATE = "ESTADO DE MEXICO")
  
  return(df_2018)
}

# -------------------------------------- #
# Append Data and Save
# -------------------------------------- #
# Process each year and append
df_2015 <- process_2015()
df_2016 <- process_2016()
df_2018 <- process_2018()

df_all <- bind_rows(df_2015, df_2016, df_2018)

# Save final combined dataset
write.csv(df_all, "Mexico_Section_15_18.csv", row.names = FALSE)