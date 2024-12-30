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

# --------------------------------------------------------------------------------
# 1. Read CSV Data
# --------------------------------------------------------------------------------
data_1996 <- read_csv("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1996_No_LN.csv", show_col_types = FALSE)

# --------------------------------------------------------------------------------
# 2. Rename variables
#     rename nombre municipality
#     rename seccion section
#     rename otros noregistrados
# --------------------------------------------------------------------------------
data_1996 <- data_1996 %>%
  rename(
    municipality   = nombre,
    section        = seccion,
    noregistrados  = otros
  )

# --------------------------------------------------------------------------------
# 3. Drop if municipality == "" & section == .
#    Drop if total == . | total == 0
# --------------------------------------------------------------------------------
# In R, is.na() is used for missing numeric data. 
# The condition `municipality == ""` checks for an empty string. 
# Also check if 'section' or 'total' might be numeric or character.

data_1996 <- data_1996 %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

# --------------------------------------------------------------------------------
# 4. destring pan - total, replace
#    Convert columns from `pan` to `total` to numeric if needed
# --------------------------------------------------------------------------------
# Identify columns from "pan" to "total"
all_cols <- names(data_1996)
start_var <- "pan"
end_var   <- "total"
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var, all_cols)
vars_to_num <- all_cols[start_pos:end_pos]

# Convert them to numeric
data_1996 <- data_1996 %>%
  mutate(across(all_of(vars_to_num), as.numeric))

# --------------------------------------------------------------------------------
# 5. collapse (sum) pan - total, by (municipality section)
#    Summation by group
# --------------------------------------------------------------------------------
data_1996 <- data_1996 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_to_num), sum, na.rm=TRUE), .groups="drop")

# --------------------------------------------------------------------------------
# 6. Rename party variables
#    rename  pan -> PAN
#            pri -> PRI
#            prd -> PRD
#            pc -> PartCardenista
#            pt -> PT
#            pvem -> PVEM
#            pps -> PPS
#            pdm -> PDM
#            ppm -> PPM
# --------------------------------------------------------------------------------
data_1996 <- data_1996 %>%
  rename(
    PAN             = pan,
    PRI             = pri,
    PRD             = prd,
    PartCardenista  = pc,
    PT              = pt,
    PVEM            = pvem,
    PPS             = pps,
    PDM             = pdm,
    PPM             = ppm
  )

# --------------------------------------------------------------------------------
# 8. drop noreg nulos
#    In your code, you wrote: drop noreg  nulos
#    We handle if they exist in the data
# --------------------------------------------------------------------------------
drop_vars <- c("noreg", "nulos")
drop_vars <- intersect(drop_vars, names(data_1996))
data_1996 <- data_1996 %>% select(-all_of(drop_vars))

# --------------------------------------------------------------------------------
# 9. Generate uniqueid = 0 and replace based on municipality
# --------------------------------------------------------------------------------
data_1996 <- data_1996 %>%
  mutate(uniqueid = 0) %>%
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
    municipality == "DONATO GUERRA" ~ 15032,
    municipality == "ECATEPEC" ~ 15033,
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
    municipality == "MALINALCO" ~ 15052,
    municipality == "MELCHOR OCAMPO" ~ 15053,
    municipality == "METEPEC" ~ 15054,
    municipality == "MEXICALTZINGO" ~ 15055,
    municipality == "MORELOS" ~ 15056,
    municipality == "NAUCALPAN" ~ 15057,
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
    municipality == "TLALNEPANTLA" ~ 15104,
    municipality == "TLATLAYA" ~ 15105,
    municipality == "TOLUCA" ~ 15106,
    municipality == "TONATICO" ~ 15107,
    municipality == "TULTEPEC" ~ 15108,
    municipality == "TULTITLAN" ~ 15109,
    municipality == "VALLE DE BRAVO" ~ 15110,
    municipality == "V. DE CHALCO SOLIDARIDAD" ~ 15122,
    municipality == "VILLA DE ALLENDE" ~ 15111,
    municipality == "VILLA DEL CARBON" ~ 15112,
    municipality == "VILLA GUERRERO" ~ 15113,
    municipality == "VILLA VICTORIA" ~ 15114,
    municipality == "JALATLACO" ~ 15043,
    municipality == "XONACATLAN" ~ 15115,
    municipality == "ZACAZONAPAN" ~ 15116,
    municipality == "ZACUALPAN" ~ 15117,
    municipality == "ZINACANTEPEC" ~ 15118,
    municipality == "ZUMPAHUACAN" ~ 15119,
    municipality == "ZUMPANGO" ~ 15120,
    TRUE ~ uniqueid
  ))

# --------------------------------------------------------------------------------
# 10. egen valid = rowtotal(...)
# --------------------------------------------------------------------------------
# In R, we'll do rowSums across relevant columns:
valid_vars <- c("PAN","PRI","PRD","PartCardenista","PVEM","PT","PPS","PDM","PPM")
valid_vars <- intersect(valid_vars, names(data_1996))

data_1996 <- data_1996 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# --------------------------------------------------------------------------------
# 11. gen year=1996, gen month="November"
# --------------------------------------------------------------------------------
data_1996 <- data_1996 %>%
  mutate(
    year  = 1996,
    month = "November")

# -------------------------------------------------------------------------
# 1. READ CSV 
# -------------------------------------------------------------------------
data_2000 <- read_csv("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2000_No_LN.csv", show_col_types = FALSE)
# Convert column names to lowercase
data_2000 <- data_2000 %>% rename_with(tolower)
# -------------------------------------------------------------------------
# 2. RENAME VARIABLES
#    rename nombre -> municipality
#    rename seccion -> section
# -------------------------------------------------------------------------
data_2000 <- data_2000 %>%
  rename(
    municipality = nombre,
    section      = seccion
  )

# -------------------------------------------------------------------------
# 3. DROP if municipality=="" & section==. 
#    DROP if total==. | total==0
# -------------------------------------------------------------------------
data_2000 <- data_2000 %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

# -------------------------------------------------------------------------
# 4. DESTRING pan - total
#    Convert columns from "pan" through "total" to numeric
# -------------------------------------------------------------------------
all_cols <- names(data_2000)
start_var <- "pan"
end_var   <- "total"
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var, all_cols)
vars_to_num <- all_cols[start_pos:end_pos]

data_2000 <- data_2000 %>%
  mutate(across(all_of(vars_to_num), as.numeric))

# -------------------------------------------------------------------------
# 5. COLLAPSE (sum) pan - total, by (municipality, section)
# -------------------------------------------------------------------------
data_2000 <- data_2000 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_to_num), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------------
# 6. RENAME party variables
#    pan -> PAN, pri -> PRI, prd -> PRD, cd -> PC, pt -> PT, pvem -> PVEM,
#    pas -> PAS, pcd -> PCD, psn -> PSN, parm -> PARM, ds -> PDS
# -------------------------------------------------------------------------
data_2000 <- data_2000 %>%
  rename(
    PAN  = pan,
    PRI  = pri,
    PRD  = prd,
    PC   = cd,
    PT   = pt,
    PVEM = pvem,
    PAS  = pas,
    PCD  = pcd,
    PSN  = psn,
    PARM = parm,
    PDS  = ds
  )

# -------------------------------------------------------------------------
# 7. DROP noreg nulos IF THEY EXIST
# -------------------------------------------------------------------------
drop_vars <- c("noreg", "nulos")
drop_vars <- intersect(drop_vars, names(data_2000))
data_2000 <- data_2000 %>% select(-all_of(drop_vars))

# -------------------------------------------------------------------------
# 8. GEN uniqueid=0, REPLACE uniqueid BY municipality
# -------------------------------------------------------------------------
data_2000 <- data_2000 %>%
  mutate(uniqueid = 0) %>%
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
    municipality == "DONATO GUERRA" ~ 15032,
    municipality == "ECATEPEC" ~ 15033,
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
    municipality == "MALINALCO" ~ 15052,
    municipality == "MELCHOR OCAMPO" ~ 15053,
    municipality == "METEPEC" ~ 15054,
    municipality == "MEXICALTZINGO" ~ 15055,
    municipality == "MORELOS" ~ 15056,
    municipality == "NAUCALPAN" ~ 15057,
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
    municipality == "TLALNEPANTLA" ~ 15104,
    municipality == "TLATLAYA" ~ 15105,
    municipality == "TOLUCA" ~ 15106,
    municipality == "TONATICO" ~ 15107,
    municipality == "TULTEPEC" ~ 15108,
    municipality == "TULTITLAN" ~ 15109,
    municipality == "VALLE DE BRAVO" ~ 15110,
    municipality == "V. DE CHALCO SOLIDARIDAD" ~ 15122,
    municipality == "VILLA DE ALLENDE" ~ 15111,
    municipality == "VILLA DEL CARBON" ~ 15112,
    municipality == "VILLA GUERRERO" ~ 15113,
    municipality == "VILLA VICTORIA" ~ 15114,
    municipality == "JALATLACO" ~ 15043,
    municipality == "XONACATLAN" ~ 15115,
    municipality == "ZACAZONAPAN" ~ 15116,
    municipality == "ZACUALPAN" ~ 15117,
    municipality == "ZINACANTEPEC" ~ 15118,
    municipality == "ZUMPAHUACAN" ~ 15119,
    municipality == "ZUMPANGO" ~ 15120,
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------------
# 9. egen valid = rowtotal(PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS)
# -------------------------------------------------------------------------
valid_vars <- c("PAN","PRI","PRD","PT","PVEM","PC","PCD","PSN","PARM","PAS","PDS")
valid_vars <- intersect(valid_vars, names(data_2000))

data_2000 <- data_2000 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# -------------------------------------------------------------------------
# 10. MERGE with all_months_years.dta (ed=15, seccion=section),
#     keep if month==7 & year==2000
# -------------------------------------------------------------------------
data_2000 <- data_2000 %>%
  mutate(ed=15, seccion=section)

# Merge with the dataset "ln_all_months_years.dta" using seccion (section) and ed
all_months <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

all_months <- all_months %>% 
  dplyr::filter(state == "MEXICO" & month == "March" & year == 2000)

# Merge the datasets
data_2000 <- data_2000 %>%
  dplyr::left_join(all_months %>% dplyr::select(section,lista), by = c("section")) %>% 
  dplyr::mutate(listanominal = ifelse(year == 2000,lista,listanominal)) %>% 
  dplyr::select(-lista)

# -------------------------------------------------------------------------
# 11. GEN turnout = total/listanominal
# -------------------------------------------------------------------------
data_2000 <- data_2000 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------------
# 12. GEN year=2000, month="July"
# -------------------------------------------------------------------------
data_2000 <- data_2000 %>%
  mutate(
    year=2000,
    month="July"
  )

# -------------------------------------------------------------------
# 1. Read CSV
# -------------------------------------------------------------------
data_2003 <- read_csv("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003_No_LN.csv", show_col_types = FALSE)

# -------------------------------------------------------------------
# 2. Rename: seccion -> section
# -------------------------------------------------------------------
data_2003 <- data_2003 %>%
  rename(section = seccion)

# -------------------------------------------------------------------
# 3. sort section
# -------------------------------------------------------------------
data_2003 <- data_2003 %>% arrange(section)

# -------------------------------------------------------------------
# 4. merge section using Mapping_Section_2003.dta
#    No merge key in R by default. We'll do a left_join on "section"
# -------------------------------------------------------------------
mapping_2003 <- read_dta("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Other/Mapping_Section_2003.dta")

data_2003 <- data_2003 %>%
  left_join(mapping_2003, by="section")

# -------------------------------------------------------------------
# 5. replace municipality if _merge==1 & municipality=="" & municipio==34 or 63
#    In R, left_join doesn't create _merge. We check if municipality=="" and municipio==34, etc.
# -------------------------------------------------------------------
data_2003 <- data_2003 %>%
  mutate(
    municipality = if_else(municipality=="" & municipio==34, "ECATEPEC", municipality),
    municipality = if_else(municipality=="" & municipio==63, "OCOYOACAC", municipality)
  )

# drop _merge municipio (in R, we have no _merge, so we drop municipio)
data_2003 <- data_2003 %>% select(-municipio)

# -------------------------------------------------------------------
# 6. drop if municipality=="" & section==.
#    drop if total==. | total==0
# -------------------------------------------------------------------
data_2003 <- data_2003 %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

# -------------------------------------------------------------------
# 7. destring pan - total
# -------------------------------------------------------------------
all_cols <- names(data_2003)
start_var <- "pan"
end_var   <- "total"
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var, all_cols)
vars_to_num <- all_cols[start_pos:end_pos]

data_2003 <- data_2003 %>%
  mutate(across(all_of(vars_to_num), as.numeric))

# -------------------------------------------------------------------
# 8. collapse (sum) pan - pacem total, by(municipality, section)
# -------------------------------------------------------------------
data_2003 <- data_2003 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_to_num), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 9. rename variables
#    pan->PAN, apt->PRI_PVEM, prd->PRD, conv->PC, pt->PT, psn->PSN,
#    pas->PAS, pacem->PCEM
# -------------------------------------------------------------------
data_2003 <- data_2003 %>%
  rename(
    PAN       = pan,
    PRI_PVEM  = apt,
    PRD       = prd,
    PC        = conv,
    PT        = pt,
    PSN       = psn,
    PAS       = pas,
    PCEM      = pacem
  )

# -------------------------------------------------------------------
# 10. Generate uniqueid=0, then replace by municipality name
# -------------------------------------------------------------------
data_2003 <- data_2003 %>%
  mutate(uniqueid=0) %>%
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
    municipality == "DONATO GUERRA" ~ 15032,
    municipality == "ECATEPEC" ~ 15033,
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
    municipality == "NAUCALPAN" ~ 15057,
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
    municipality == "SAN MARTIN DE LAS PIRAMIDE" ~ 15075, # note slight difference from original "LAS PIRAMIDES" ?
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
    municipality == "TLALNEPANTLA" ~ 15104,
    municipality == "TLATLAYA" ~ 15105,
    municipality == "TOLUCA" ~ 15106,
    municipality == "TONANITLA" ~ 15125,
    municipality == "TONATICO" ~ 15107,
    municipality == "TULTEPEC" ~ 15108,
    municipality == "TULTITLAN" ~ 15109,
    municipality == "VALLE DE BRAVO" ~ 15110,
    municipality == "VALLE DE CHALCO SOLIDARIDA" ~ 15122,
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
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 11. egen valid = rowtotal(PAN PRI_PVEM PRD PT PC PSN PAS PCEM)
# -------------------------------------------------------------------
valid_vars <- c("PAN","PRI_PVEM","PRD","PT","PC","PSN","PAS","PCEM")
valid_vars <- intersect(valid_vars, names(data_2003))
data_2003 <- data_2003 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# -------------------------------------------------------------------
# 12. Merge with all_months_years.dta (ed=15, seccion=section),
#     keep if month==7 & year==2000
# -------------------------------------------------------------------
data_2003 <- data_2003 %>%
  mutate(ed=15, seccion=section)

all_months <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  select(ed, seccion, month, year, lista)

data_2003 <- data_2003 %>%
  left_join(all_months, by=c("ed","seccion")) %>%
  filter(month==7 & year==2000)

# drop if _merge==2 => in R no _merge
# drop _merge, ed, seccion, year, month
drops <- c("ed","seccion","year","month")
data_2003 <- data_2003 %>% select(-all_of(drops))

# rename lista -> listanominal
data_2003 <- data_2003 %>% rename(listanominal=lista)

# -------------------------------------------------------------------
# 13. gen turnout= total/listanominal
# -------------------------------------------------------------------
data_2003 <- data_2003 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 14. gen year=2003, month="March"
# -------------------------------------------------------------------
data_2003 <- data_2003 %>%
  mutate(
    year=2003,
    month="March"
  )

# =============================================================================
# SPECIAL ELECTIONS (Atenco, Chalco, Tepotzotlan) - Only Atenco Available
# =============================================================================

extra_2003 <- read_excel("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Extraordinarios 2003.xlsx", sheet="Sheet1")

# rename Sección -> section
extra_2003 <- extra_2003 %>%
  rename(section = Sección)

# drop if section==. or total==. | total==0
extra_2003 <- extra_2003 %>%
  filter(!is.na(section)) %>%
  filter(!(is.na(total) | total==0))

# collapse (sum) listanominal PAN - PAS total, by(municipality, section)
# Identify columns from PAN to PAS + listanominal + total
all_cols_2 <- names(extra_2003)
start_var2 <- "PAN"
end_var2   <- "PAS"
pos_start2 <- match(start_var2, all_cols_2)
pos_end2   <- match(end_var2,   all_cols_2)
vars_range2 <- all_cols_2[pos_start2:pos_end2]
# Also keep listanominal, total
vars_collapse_2 <- unique(c("listanominal", vars_range2, "total"))

extra_2003 <- extra_2003 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse_2), sum, na.rm=TRUE), .groups="drop")

# gen uniqueid=15011 (Atenco)
extra_2003 <- extra_2003 %>%
  mutate(uniqueid=15011)

# egen valid = rowtotal(PAN PRI_PVEM PRD PT PAS)
valid_extra <- c("PAN","PRI_PVEM","PRD","PT","PAS")
valid_extra <- intersect(valid_extra, names(extra_2003))
extra_2003 <- extra_2003 %>%
  mutate(valid = rowSums(across(all_of(valid_extra)), na.rm=TRUE))

# gen turnout = total/listanominal
# gen mun_turnout = mun_total/mun_listanominal
extra_2003 <- extra_2003 %>%
  mutate(
    turnout  = total / listanominal)


# gen year=2003, month="October"
extra_2003 <- extra_2003 %>%
  mutate(
    year  = 2003,
    month = "October"
  )

# -------------------------------------------------------------------
# 1. READ CSV
# -------------------------------------------------------------------
data_2006 <- read_csv("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2006.csv", show_col_types = FALSE)

# -------------------------------------------------------------------
# 2. RENAME Variables
#    rename nombre_municipio -> municipality
#    rename seccion -> section
#    rename lista_nominal -> listanominal
# -------------------------------------------------------------------
data_2006 <- data_2006 %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  )

# -------------------------------------------------------------------
# 3. DROP if municipality=="" & section==.
#    DROP if total==. | total==0
# -------------------------------------------------------------------
data_2006 <- data_2006 %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

# -------------------------------------------------------------------
# 4. DESTRING pan - total
#    Convert columns from "pan" to "total" to numeric
# -------------------------------------------------------------------
all_cols <- names(data_2006)
start_var <- "pan"
end_var   <- "total"
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var, all_cols)
vars_to_num <- all_cols[start_pos:end_pos]

data_2006 <- data_2006 %>%
  mutate(across(all_of(vars_to_num), as.numeric))

# -------------------------------------------------------------------
# 5. COLLAPSE (sum) listanominal + pan - pc + total, by(municipality, section)
# -------------------------------------------------------------------
vars_collapse <- unique(c("listanominal", vars_to_num))
data_2006 <- data_2006 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 6. CREATE and USE DUMMY Variables for Coalition Adjustments
# -------------------------------------------------------------------
data_2006 <- data_2006 %>%
  mutate(
    dummy_pan_pc = 0,
    dummy_pan_prd = 0,
    dummy_pan_prd_pt = 0,
    dummy_pan_prd_pt_pc = 0,
    dummy_pan_pt = 0,
    dummy_prd_pc = 0,
    dummy_prd_pt = 0,
    dummy_prd_pt_pc = 0,
    dummy_pt_pc = 0
  )

data_2006 <- data_2006 %>%
  mutate(
    dummy_pan_pc = if_else(municipality=="XONACATLAN", 1, dummy_pan_pc),
    dummy_pan_prd = if_else(municipality=="TEMOAYA", 1, dummy_pan_prd),
    dummy_pan_prd_pt = if_else(municipality=="TEMASCALCINGO", 1, dummy_pan_prd_pt),
    dummy_pan_prd_pt_pc = if_else(municipality=="ACAMBAY", 1, dummy_pan_prd_pt_pc),
    dummy_pan_pt = if_else(municipality %in% c("VILLA DE ALLENDE","VALLE DE CHALCO SOLIDARIDA"), 1, dummy_pan_pt),
    dummy_prd_pc = if_else(municipality %in% c("CUAUTITLAN","JALTENCO","MORELOS","SAN FELIPE DEL PROGRESO"), 1, dummy_prd_pc),
    dummy_prd_pt = if_else(municipality %in% c("ALMOLOYA DE ALQUISIRAS","ALMOLOYA DE JUAREZ","ATIZAPAN",
                                               "COATEPEC HARINAS","COYOTEPEC","CHICOLOAPAN","CHIMALHUACAN",
                                               "ECATEPEC","HUIXQUILUCAN","IXTAPAN DE LA SAL","JIQUIPILCO",
                                               "METEPEC","MEXICALTZINGO","NAUCALPAN","NOPALTEPEC","OCUILAN",
                                               "OTZOLOAPAN","SAN SIMON DE GUERRERO","TEPETLAOXTOC","TEPOTZOTLAN",
                                               "TEZOYUCA","TLALNEPANTLA","TONATICO","TULTEPEC","TULTITLAN",
                                               "VILLA GUERRERO","ZACUALPAN"), 1, dummy_prd_pt),
    dummy_prd_pt_pc = if_else(municipality %in% c("AMANALCO","ATIZAPAN DE ZARAGOZA","IXTLAHUACA",
                                                  "JOQUICINGO","VILLA DEL CARBON"), 1, dummy_prd_pt_pc),
    dummy_pt_pc = if_else(municipality %in% c("ACULCO","APAXCO","ATLACOMULCO","CHIAUTLA","ECATZINGO","IXTAPALUCA",
                                              "XALATLACO","JILOTEPEC","LERMA","MALINALCO","OTUMBA","LA PAZ",
                                              "POLOTITLAN","SAN MARTIN DE LAS PIRAMIDE","SOYANIQUILPAN DE JUAREZ",
                                              "SULTEPEC","TEQUIXQUIAC","TEXCALYACAC","TIANGUISTENCO","TIMILPAN",
                                              "TOLUCA","ZUMPAHUACAN","TONANITLA"), 1, dummy_pt_pc)
  )

# -------------------------------------------------------------------
# 7. GEN NEW Columns for Each Coalition & Adjust Original
# -------------------------------------------------------------------
data_2006 <- data_2006 %>%
  mutate(
    pan_pc = 0,
    pan_prd = 0,
    pan_prd_pt = 0,
    pan_prd_pt_pc = 0,
    pan_pt = 0,
    prd_pc = 0,
    prd_pt = 0,
    prd_pt_pc = 0,
    pt_pc = 0
  )

# Coalition: pan_pc
data_2006 <- data_2006 %>%
  mutate(
    pan_pc = if_else(dummy_pan_pc==1, pan + pc, pan_pc),
    pan    = if_else(dummy_pan_pc==1, 0, pan),
    pc     = if_else(dummy_pan_pc==1, 0, pc)
  )

# Coalition: pan_prd
data_2006 <- data_2006 %>%
  mutate(
    pan_prd = if_else(dummy_pan_prd==1, pan + prd, pan_prd),
    pan     = if_else(dummy_pan_prd==1, 0, pan),
    prd     = if_else(dummy_pan_prd==1, 0, prd)
  )

# Coalition: pan_prd_pt
data_2006 <- data_2006 %>%
  mutate(
    pan_prd_pt = if_else(dummy_pan_prd_pt==1, pan + prd + pt, pan_prd_pt),
    pan        = if_else(dummy_pan_prd_pt==1, 0, pan),
    prd        = if_else(dummy_pan_prd_pt==1, 0, prd),
    pt         = if_else(dummy_pan_prd_pt==1, 0, pt)
  )

# Coalition: pan_prd_pt_pc
data_2006 <- data_2006 %>%
  mutate(
    pan_prd_pt_pc = if_else(dummy_pan_prd_pt_pc==1, pan + prd + pt + pc, pan_prd_pt_pc),
    pan           = if_else(dummy_pan_prd_pt_pc==1, 0, pan),
    prd           = if_else(dummy_pan_prd_pt_pc==1, 0, prd),
    pt            = if_else(dummy_pan_prd_pt_pc==1, 0, pt),
    pc            = if_else(dummy_pan_prd_pt_pc==1, 0, pc)
  )

# Coalition: pan_pt
data_2006 <- data_2006 %>%
  mutate(
    pan_pt = if_else(dummy_pan_pt==1, pan + pt, pan_pt),
    pan    = if_else(dummy_pan_pt==1, 0, pan),
    pt     = if_else(dummy_pan_pt==1, 0, pt)
  )

# Coalition: prd_pc
data_2006 <- data_2006 %>%
  mutate(
    prd_pc = if_else(dummy_prd_pc==1, prd + pc, prd_pc),
    prd    = if_else(dummy_prd_pc==1, 0, prd),
    pc     = if_else(dummy_prd_pc==1, 0, pc)
  )

# Coalition: prd_pt
data_2006 <- data_2006 %>%
  mutate(
    prd_pt = if_else(dummy_prd_pt==1, prd + pt, prd_pt),
    prd    = if_else(dummy_prd_pt==1, 0, prd),
    pt     = if_else(dummy_prd_pt==1, 0, pt)
  )

# Coalition: prd_pt_pc
data_2006 <- data_2006 %>%
  mutate(
    prd_pt_pc = if_else(dummy_prd_pt_pc==1, prd + pt + pc, prd_pt_pc),
    prd       = if_else(dummy_prd_pt_pc==1, 0, prd),
    pt        = if_else(dummy_prd_pt_pc==1, 0, pt),
    pc        = if_else(dummy_prd_pt_pc==1, 0, pc)
  )

# Coalition: pt_pc
data_2006 <- data_2006 %>%
  mutate(
    pt_pc = if_else(dummy_pt_pc==1, pt + pc, pt_pc),
    pt    = if_else(dummy_pt_pc==1, 0, pt),
    pc    = if_else(dummy_pt_pc==1, 0, pc)
  )

# drop dummy variables
data_2006 <- data_2006 %>%
  select(-dummy_pan_pc, -dummy_pan_prd, -dummy_pan_prd_pt, -dummy_pan_prd_pt_pc,
         -dummy_pan_pt, -dummy_prd_pc, -dummy_prd_pt, -dummy_prd_pt_pc, -dummy_pt_pc)

# -------------------------------------------------------------------
# 8. FINAL RENAME
# -------------------------------------------------------------------
data_2006 <- data_2006 %>%
  rename(
    PAN        = pan,
    PRI_PVEM   = alianza, 
    PRD        = prd,
    PC         = pc,
    PT         = pt,
    PAN_PC     = pan_pc,
    PAN_PRD    = pan_prd,
    PAN_PRD_PT = pan_prd_pt,
    PAN_PRD_PT_PC = pan_prd_pt_pc,
    PAN_PT     = pan_pt,
    PRD_PC     = prd_pc,
    PRD_PT     = prd_pt,
    PRD_PT_PC  = prd_pt_pc,
    PT_PC      = pt_pc
  )

# -------------------------------------------------------------------
# 9. GEN turnout = total/listanominal
# -------------------------------------------------------------------
data_2006 <- data_2006 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 10. UNIQUEID: assign codes
# -------------------------------------------------------------------
data_2006 <- data_2006 %>%
  mutate(uniqueid = 0) %>%
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
    municipality == "DONATO GUERRA" ~ 15032,
    municipality == "ECATEPEC" ~ 15033,
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
    municipality == "NAUCALPAN" ~ 15057,
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
    municipality == "SAN MARTIN DE LAS PIRAMIDE" ~ 15075,
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
    municipality == "TLALNEPANTLA" ~ 15104,
    municipality == "TLATLAYA" ~ 15105,
    municipality == "TOLUCA" ~ 15106,
    municipality == "TONANITLA" ~ 15125,
    municipality == "TONATICO" ~ 15107,
    municipality == "TULTEPEC" ~ 15108,
    municipality == "TULTITLAN" ~ 15109,
    municipality == "VALLE DE BRAVO" ~ 15110,
    municipality == "VALLE DE CHALCO SOLIDARIDA" ~ 15122,
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
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 11. EGEN valid = rowtotal(...) 
# -------------------------------------------------------------------
valid_vars <- c("PAN","PRI_PVEM","PRD","PT","PC","PAN_PC","PAN_PRD","PAN_PRD_PT",
                "PAN_PRD_PT_PC","PAN_PT","PRD_PC","PRD_PT","PRD_PT_PC","PT_PC")
valid_vars <- intersect(valid_vars, names(data_2006))
data_2006 <- data_2006 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# gen mun_turnout = mun_total/mun_listanominal

# -------------------------------------------------------------------
# 15. GEN year=2006, month="March"
# -------------------------------------------------------------------
data_2006 <- data_2006 %>%
  mutate(
    year=2006,
    month="March"
  ) %>%
  arrange(section)

# -------------------------------------------------------------------
# 1. READ CSV
# -------------------------------------------------------------------
data_2009 <- read_csv("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2009.csv", show_col_types = FALSE)

# -------------------------------------------------------------------
# 2. RENAME AND DROP ROWS
#   rename nombre_municipio -> municipality
#   rename seccion -> section
#   rename lista_nominal -> listanominal
# -------------------------------------------------------------------
data_2009 <- data_2009 %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality=="" & is.na(section))) %>%  # drop if municipality=="" & section==.
  filter(!(is.na(total) | total==0))               # drop if total==. | total==0

# -------------------------------------------------------------------
# 3. DESTRING pan - total
#   Convert columns from `pan` to `total` to numeric
# -------------------------------------------------------------------
all_cols <- names(data_2009)
start_var <- "pan"
end_var   <- "total"
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var, all_cols)
vars_to_num <- all_cols[start_pos:end_pos]

data_2009 <- data_2009 %>%
  mutate(across(all_of(vars_to_num), as.numeric))

# -------------------------------------------------------------------
# 4. collapse (sum) listanominal + pan - ptc + total, by(municipality, section)
# -------------------------------------------------------------------
# Identify columns from listanominal + pan - ptc + total
vars_collapse <- unique(c("listanominal", vars_to_num))
data_2009 <- data_2009 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 5. CREATE & ADJUST DUMMY VARIABLES for special sections
# -------------------------------------------------------------------
# The code indicates we first create four section-level dummies:
#   sec_dummy_pan_pc, sec_dummy_pri_pvem_na_psd_pfd, sec_dummy_prd_pt, sec_dummy_pt_pc
# Then we do a by municipality: egen dummy_... = max(sec_dummy_...)

data_2009 <- data_2009 %>%
  mutate(
    sec_dummy_pan_pc = 0,
    sec_dummy_pri_pvem_na_psd_pfd = 0,
    sec_dummy_prd_pt = 0,
    sec_dummy_pt_pc = 0
  )

data_2009 <- data_2009 %>%
  mutate(
    sec_dummy_pan_pc = if_else(!is.na(panc) & panc > 0, 1, sec_dummy_pan_pc),
    sec_dummy_pri_pvem_na_psd_pfd = if_else(!is.na(pripvemnapsdpfd) & pripvemnapsdpfd > 0, 1, sec_dummy_pri_pvem_na_psd_pfd),
    sec_dummy_prd_pt  = if_else(!is.na(prdpt) & prdpt > 0, 1, sec_dummy_prd_pt),
    sec_dummy_pt_pc   = if_else(!is.na(ptc)  & ptc > 0, 1, sec_dummy_pt_pc)
  )

# Next, by municipality: egen = max()
# In R: group_by(municipality) then summarise or mutate with max
data_2009 <- data_2009 %>%
  group_by(municipality) %>%
  mutate(
    dummy_pan_pc = max(sec_dummy_pan_pc, na.rm=TRUE),
    dummy_pri_pvem_na_psd_pfd = max(sec_dummy_pri_pvem_na_psd_pfd, na.rm=TRUE),
    dummy_prd_pt = max(sec_dummy_prd_pt, na.rm=TRUE),
    dummy_pt_pc = max(sec_dummy_pt_pc, na.rm=TRUE)
  ) %>%
  ungroup()

# drop the sec_* dummies
data_2009 <- data_2009 %>%
  select(-sec_dummy_pan_pc, -sec_dummy_pri_pvem_na_psd_pfd, -sec_dummy_prd_pt, -sec_dummy_pt_pc)

# -------------------------------------------------------------------
# 6. CREATE COALITIONS & ZERO OUT ORIGINALS
# -------------------------------------------------------------------
# gen pan_pc = panc + pan + pc if dummy_pan_pc==1
# replace pan=0 if dummy_pan_pc==1 etc.
# drop panc

data_2009 <- data_2009 %>%
  mutate(
    pan_pc = if_else(dummy_pan_pc==1, coalesce(panc,0) + coalesce(pan,0) + coalesce(pc,0), 0),
    pan    = if_else(dummy_pan_pc==1, 0, pan),
    pc     = if_else(dummy_pan_pc==1, 0, pc)
  ) %>%
  select(-panc)

# gen pri_pvem_na_psd_pfd = pripvemnapsdpfd + pri + pvem + panal + psd + pfd if dummy_pri_pvem_na_psd_pfd==1
# zero out original
data_2009 <- data_2009 %>%
  mutate(
    pri_pvem_na_psd_pfd = if_else(dummy_pri_pvem_na_psd_pfd==1,
                                  coalesce(pripvemnapsdpfd,0) + coalesce(pri,0) + coalesce(pvem,0) + 
                                    coalesce(panal,0) + coalesce(psd,0) + coalesce(pfd,0),
                                  0
    ),
    pri   = if_else(dummy_pri_pvem_na_psd_pfd==1, 0, pri),
    pvem  = if_else(dummy_pri_pvem_na_psd_pfd==1, 0, pvem),
    panal = if_else(dummy_pri_pvem_na_psd_pfd==1, 0, panal),
    psd   = if_else(dummy_pri_pvem_na_psd_pfd==1, 0, psd),
    pfd   = if_else(dummy_pri_pvem_na_psd_pfd==1, 0, pfd)
  ) %>%
  select(-pripvemnapsdpfd)

# gen prd_pt = prdpt + prd + pt if dummy_prd_pt==1
# zero out original
data_2009 <- data_2009 %>%
  mutate(
    prd_pt = if_else(dummy_prd_pt==1, coalesce(prdpt,0) + coalesce(prd,0) + coalesce(pt,0), 0),
    prd    = if_else(dummy_prd_pt==1, 0, prd),
    pt     = if_else(dummy_prd_pt==1, 0, pt)
  ) %>%
  select(-prdpt)

# gen pt_pc = ptc + pt + pc if dummy_pt_pc==1
# zero out original
data_2009 <- data_2009 %>%
  mutate(
    pt_pc = if_else(dummy_pt_pc==1, coalesce(ptc,0) + coalesce(pt,0) + coalesce(pc,0), 0),
    pt    = if_else(dummy_pt_pc==1, 0, pt),
    pc    = if_else(dummy_pt_pc==1, 0, pc)
  ) %>%
  select(-ptc)

# drop all the dummy_ flags
data_2009 <- data_2009 %>%
  select(-dummy_pan_pc, -dummy_pri_pvem_na_psd_pfd, -dummy_prd_pt, -dummy_pt_pc)

# drop pri pvem panal psd pfd
drop_vars <- c("pri","pvem","panal","psd","pfd")
drop_vars <- intersect(drop_vars, names(data_2009))
data_2009 <- data_2009 %>% select(-all_of(drop_vars))

# -------------------------------------------------------------------
# 7. RENAME
#   pan -> PAN, pri_pvem_na_psd_pfd -> PRI_PVEM_PANAL_PSD_PFD,
#   prd -> PRD, pc -> PC, pt-> PT, pan_pc->PAN_PC, prd_pt->PRD_PT, pt_pc->PT_PC
# -------------------------------------------------------------------
data_2009 <- data_2009 %>%
  rename(
    PAN      = pan,
    PRI_PVEM_PANAL_PSD_PFD = pri_pvem_na_psd_pfd,
    PRD      = prd,
    PC       = pc,
    PT       = pt,
    PAN_PC   = pan_pc,
    PRD_PT   = prd_pt,
    PT_PC    = pt_pc
  )

# -------------------------------------------------------------------
# 8. GEN turnout = total/listanominal
# -------------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(turnout = total/listanominal)

# -------------------------------------------------------------------
# 9. UNIQUEID by municipality
# -------------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(uniqueid = 0) %>%
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
    municipality == "DONATO GUERRA" ~ 15032,
    municipality == "ECATEPEC" ~ 15033,
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
    municipality == "NAUCALPAN" ~ 15057,
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
    municipality == "TLALNEPANTLA" ~ 15104,
    municipality == "TLATLAYA" ~ 15105,
    municipality == "TOLUCA" ~ 15106,
    municipality == "TONANITLA" ~ 15125,
    municipality == "TONATICO" ~ 15107,
    municipality == "TULTEPEC" ~ 15108,
    municipality == "TULTITLAN" ~ 15109,
    municipality == "VALLE DE BRAVO" ~ 15110,
    municipality == "VALLE DE CHALCO SOLIDARIDAD" ~ 15122,
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
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 10. egen valid = rowtotal(...)
# -------------------------------------------------------------------
valid_vars <- c("PAN","PRD","PT","PC","PAN_PC","PRI_PVEM_PANAL_PSD_PFD","PRD_PT","PT_PC")
valid_vars <- intersect(valid_vars, names(data_2009))

data_2009 <- data_2009 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))


# -------------------------------------------------------------------
# 11. GEN year=2009, month="July"
#     sort section
#     SAVE Mexico_Section_2009.dta
# -------------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(
    year=2009,
    month="July"
  ) %>%
  arrange(section)

# -------------------------------------------------------------------
# 1. READ EXCEL
# -------------------------------------------------------------------
data_2012 <- read_excel(
  "../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2012.xlsx", 
  sheet = "CASILLAS_AYUNTAMIENTOS_2012"
)

# -------------------------------------------------------------------
# 2. RENAME Variables
#   rename nombre_municipio -> municipality
#   rename seccion -> section
#   rename lista_nominal -> listanominal
# -------------------------------------------------------------------
data_2012 <- data_2012 %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  )

# -------------------------------------------------------------------
# 3. Additional Renaming
-----------------------------------------
data_2012 <- data_2012 %>%
  rename(
    PRI_PVEM_PANAL = PRIPVEMPANAL,
    PRD_PT_PC      = PRDPTMC,
    PRD_PT         = PRDPT,
    PRD_PC         = PRDMC,
    PT_PC          = PRMC,   # "PRMC" -> "PT_PC"
    PC             = MC
  )

# -------------------------------------------------------------------
# 4. CREATE total = rowtotal(PAN PRI_PVEM_PANAL PRD PT PC PRD_PT_PC PRD_PT PRD_PC PT_PC NoRegistrados Nulos)
# -------------------------------------------------------------------
# In R, we do rowSums. Double-check your columns:
row_vars <- c("PAN","PRI_PVEM_PANAL","PRD","PT","PC","PRD_PT_PC","PRD_PT","PRD_PC","PT_PC",
              "NoRegistrados","Nulos")
row_vars <- intersect(row_vars, names(data_2012))

data_2012 <- data_2012 %>%
  mutate(total = rowSums(across(all_of(row_vars)), na.rm=TRUE))

# -------------------------------------------------------------------
# 5. DROP if total==. | total==0
# -------------------------------------------------------------------
data_2012 <- data_2012 %>%
  filter(!(is.na(total) | total == 0))

# -------------------------------------------------------------------
# 6. COLLAPSE (sum) listanominal PAN - PT_PC total, by(municipality, section)
# -------------------------------------------------------------------
# Identify columns from listanominal + PAN through PT_PC + total
all_cols <- names(data_2012)
start_var <- "PAN"
end_var   <- "PT_PC"
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var,   all_cols)
vars_range <- all_cols[start_pos:end_pos]
vars_collapse <- unique(c("listanominal", vars_range, "total"))

data_2012 <- data_2012 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 7. gen turnout = total/listanominal
# -------------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 8. uniqueid=0, then replace ...
# -------------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(uniqueid=0) %>%
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
    municipality == "COACALCO DE BERRIOZABAL" ~ 15020,
    municipality == "COATEPEC HARINAS" ~ 15021,
    municipality == "COCOTITLAN" ~ 15022,
    municipality == "COYOTEPEC" ~ 15023,
    municipality == "CUAUTITLAN" ~ 15024,
    municipality == "CUAUTITLAN IZCALLI" ~ 15121,
    municipality == "DONATO GUERRA" ~ 15032,
    municipality == "ECATEPEC DE MORELOS" ~ 15033,
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
    municipality == "NAUCALPAN DE JUAREZ" ~ 15057,
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
    municipality == "TLALNEPANTLA DE BAZ" ~ 15104,
    municipality == "TLATLAYA" ~ 15105,
    municipality == "TOLUCA" ~ 15106,
    municipality == "TONANITLA" ~ 15125,
    municipality == "TONATICO" ~ 15107,
    municipality == "TULTEPEC" ~ 15108,
    municipality == "TULTITLAN" ~ 15109,
    municipality == "VALLE DE BRAVO" ~ 15110,
    municipality == "VALLE DE CHALCO SOLIDARIDAD" ~ 15122,
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
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 9. egen valid = rowtotal(PAN PRI_PVEM_PANAL PRD PT PC PRD_PT_PC PRD_PT PRD_PC PT_PC)
# -------------------------------------------------------------------
valid_vars <- c("PAN","PRI_PVEM_PANAL","PRD","PT","PC","PRD_PT_PC","PRD_PT","PRD_PC","PT_PC")
valid_vars <- intersect(valid_vars, names(data_2012))

data_2012 <- data_2012 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# -------------------------------------------------------------------
# 10. gen year=2012, month="July"
#     sort section
#     save Mexico_Section_2012.dta
# -------------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(
    year  = 2012,
    month = "July"
  ) %>%
  arrange(section)

# -------------------------------------------------------------------
# 1. Read Excel
# -------------------------------------------------------------------
data_2015 <- read_excel("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2015.xlsx", sheet="Aytto x secccion", guess_max = 100000)

# -------------------------------------------------------------------
# 2. Drop Municipio, uniqueid from the original dataset
# -------------------------------------------------------------------
drop_vars <- c("Municipio","uniqueid")
drop_vars <- intersect(drop_vars, names(data_2015))
data_2015 <- data_2015 %>% select(-all_of(drop_vars))

# -------------------------------------------------------------------
# 3. Create uniqueid=0, then replace by municipality name
# -------------------------------------------------------------------
data_2015 <- data_2015 %>%
  mutate(uniqueid = 0) %>%
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
    municipality == "NAUCALPAN DE JUAREZ" | municipality=="NAUCALPAN" ~ 15057,
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
    municipality == "TLALNEPANTLA DE BAZ" | municipality=="TLALNEPANTLA" ~ 15104,
    municipality == "TLATLAYA" ~ 15105,
    municipality == "TOLUCA" ~ 15106,
    municipality == "TONANITLA" ~ 15125,
    municipality == "TONATICO" ~ 15107,
    municipality == "TULTEPEC" ~ 15108,
    municipality == "TULTITLAN" ~ 15109,
    municipality == "VALLE DE BRAVO" ~ 15110,
    municipality == "VALLE DE CHALCO SOLIDARIDAD" | municipality=="V. DE CHALCO SOLIDARIDAD" ~ 15122,
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
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 4. Convert all string columns to numeric if possible
# -------------------------------------------------------------------
data_2015 <- data_2015 %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.)))))

# -------------------------------------------------------------------
# 5. collapse (sum) PAN-total (first) coal*, by(municipality section uniqueid)
# -------------------------------------------------------------------
# Identify numeric columns from PAN through total. 
# We also keep any columns that start with "coal".
all_cols_2 <- names(data_2015)
start_var2 <- "PAN"
end_var2   <- "total"

pos_start2 <- match(start_var2, all_cols_2)
pos_end2   <- match(end_var2, all_cols_2)
vars_range2 <- all_cols_2[pos_start2:pos_end2]

# We'll also keep columns that start with "coal"
coal_cols <- grep("^coal", names(data_2015), value=TRUE)

vars_collapse_2 <- unique(c(vars_range2, coal_cols))

group_vars <- c("municipality","section","uniqueid")

data_2015 <- data_2015 %>%
  group_by(across(all_of(group_vars))) %>%
  summarize(
    across(all_of(vars_collapse_2), sum, na.rm=TRUE),
    .groups="drop"
  )

# -------------------------------------------------------------------
# 6. Replace for coalPRIPVEMNA, coalPANPT, etc.
#    Merging columns for those coalitions
# -------------------------------------------------------------------
# if coalPRIPVEMNA==1 => PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRI_PVEM + ...
if("coalPRIPVEMNA" %in% names(data_2015)) {
  data_2015 <- data_2015 %>%
    mutate(
      PRI_PVEM_PANAL = if_else(coalPRIPVEMNA==1, coalesce(PRI_PVEM_PANAL,0) + coalesce(PRI_PVEM,0) + coalesce(PRI_PANAL,0)
                               + coalesce(PVEM_PANAL,0) + coalesce(PRI,0) + coalesce(PVEM,0) + coalesce(PANAL,0),
                               PRI_PVEM_PANAL),
      PRI = if_else(coalPRIPVEMNA==1, 0, PRI),
      PVEM= if_else(coalPRIPVEMNA==1, 0, PVEM),
      PANAL= if_else(coalPRIPVEMNA==1, 0, PANAL)
    )
  # drop any partial variables
  drop_cols1 <- c("PRI_PVEM","PRI_PANAL","PVEM_PANAL")
  drop_cols1 <- intersect(drop_cols1, names(data_2015))
  data_2015 <- data_2015 %>% select(-all_of(drop_cols1))
}

# if coalPANPT==1 => PAN_PT = PAN_PT + PAN + PT
if("coalPANPT" %in% names(data_2015)) {
  data_2015 <- data_2015 %>%
    mutate(
      PAN_PT = if_else(coalPANPT==1, coalesce(PAN_PT,0) + coalesce(PAN,0) + coalesce(PT,0), PAN_PT),
      PAN    = if_else(coalPANPT==1, 0, PAN),
      PT     = if_else(coalPANPT==1, 0, PT)
    )
}

# drop all coal* columns
coal_drop <- grep("^coal", names(data_2015), value=TRUE)
if(length(coal_drop)>0) {
  data_2015 <- data_2015 %>% select(-all_of(coal_drop))
}

# -------------------------------------------------------------------
# 7. egen valid = rowtotal(PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PFD PRI_PVEM_PANAL PAN_PT CI_1 CI_2)
# -------------------------------------------------------------------
valid_vars <- c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","MORENA","PH","PES","PFD",
                "PRI_PVEM_PANAL","PAN_PT","CI_1","CI_2")
valid_vars <- intersect(valid_vars, names(data_2015))

data_2015 <- data_2015 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# -------------------------------------------------------------------
# 8. Add year=2015, month="June", STATE="ESTADO DE MEXICO"
#     Save Mexico_Section_2015.dta
# -------------------------------------------------------------------
data_2015 <- data_2015 %>%
  mutate(
    year = 2015,
    month = "June",
    STATE = "ESTADO DE MEXICO"
  )

# -------------------------------------------------------------------
# 9. preserve / restore logic with LN2015 data
#     read LN2015.dta, filter, rename
# -------------------------------------------------------------------

ln2015 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
  filter(entidad==15 & month==6) %>%  # keep if entidad==15 & month==6
  mutate(uniqueid = (entidad*1000)+ municipio) %>%
  filter(seccion!=0) %>%
  arrange(uniqueid,seccion) %>%
  select(uniqueid, section=seccion, lista)

# -------------------------------------------------------------------
# 10. merge 1:1 section using LN15_MEX.dta
#     use Mexico_Section_2015.dta, then merge
# -------------------------------------------------------------------
data_2015 <- read_dta("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Mexico_Section_2015.dta")

data_2015 <- data_2015 %>%
  left_join(ln15_mex, by="section")  # left_join = merge 1:1 section

# rename lista -> listanominal
data_2015 <- data_2015 %>% rename(listanominal = lista)

# g turnout = total/listanominal
data_2015 <- data_2015 %>%
  mutate(
    turnout = total / listanominal)

# -------------------------------------------------------------------
# 1. IMPORT EXCEL
# -------------------------------------------------------------------
data_2016 <- read_excel("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Extraordinario 2016.xlsx", sheet="Sheet1", guess_max=100000)

# -------------------------------------------------------------------
# 2. RENAME SECCIÓN -> section
# -------------------------------------------------------------------
data_2016 <- data_2016 %>%
  rename(section = SECCIÓN)

# -------------------------------------------------------------------
# 3. Adjust coalition variables
#   replace PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + ...
#   then drop partial columns
# -------------------------------------------------------------------
# If these columns exist, combine them, zero out the partials
data_2016 <- data_2016 %>%
  mutate(
    PRI_PVEM_PANAL = PRI_PVEM_PANAL + coalesce(PRI_PVEM,0) + coalesce(PRI_PANAL,0) + coalesce(PVEM_PANAL,0) + coalesce(PRI,0) + coalesce(PVEM,0) + coalesce(PANAL,0)
  )

drop_coal_1 <- c("PRI","PVEM","PANAL","PRI_PVEM","PRI_PANAL","PVEM_PANAL")
drop_coal_1 <- intersect(drop_coal_1, names(data_2016))
data_2016 <- data_2016 %>% select(-all_of(drop_coal_1))

# replace PRD_PT = PRD_PT + PRD + PT
# rename ES PES
data_2016 <- data_2016 %>%
  mutate(
    PRD_PT = PRD_PT + coalesce(PRD,0) + coalesce(PT,0)
  ) %>%
  rename(PES = ES)

drop_coal_2 <- c("PRD","PT")
drop_coal_2 <- intersect(drop_coal_2, names(data_2016))
data_2016 <- data_2016 %>% select(-all_of(drop_coal_2))

# -------------------------------------------------------------------
# 4. GENERATE total = rowtotal(PAN-NULOS)
# -------------------------------------------------------------------
# Identify columns from PAN through NULOS
all_cols <- names(data_2016)
start_var <- "PAN"
end_var   <- "NULOS"
pos_start <- match(start_var, all_cols)
pos_end   <- match(end_var,   all_cols)
if(!is.na(pos_start) & !is.na(pos_end)) {
  vars_for_total <- all_cols[pos_start:pos_end]
} else {
  vars_for_total <- c("PAN","NULOS") # fallback if can't find them
}

data_2016 <- data_2016 %>%
  mutate(
    total = rowSums(across(all_of(vars_for_total)), na.rm=TRUE)
  )

# -------------------------------------------------------------------
# 5. gen uniqueid=15028 (Atenco?), then collapse
# -------------------------------------------------------------------
data_2016 <- data_2016 %>%
  mutate(uniqueid = 15028)

# collapse (sum) PAN-PRD_PT total, by(municipality section uniqueid)
# Identify from PAN to PRD_PT + total
start_var_2 <- "PAN"
end_var_2   <- "PRD_PT"
pos_start_2 <- match(start_var_2, names(data_2016))
pos_end_2   <- match(end_var_2,   names(data_2016))
vars_range2 <- all_cols[pos_start_2:pos_end_2]
vars_collapse <- unique(c(vars_range2, "total"))

group_vars <- c("municipality","section","uniqueid")

data_2016 <- data_2016 %>%
  group_by(across(all_of(group_vars))) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 6. egen valid = rowtotal(PAN MC MORENA PES PFD PRI_PVEM_PANAL PRD_PT)
# -------------------------------------------------------------------
valid_vars <- c("PAN","MC","MORENA","PES","PFD","PRI_PVEM_PANAL","PRD_PT")
valid_vars <- intersect(valid_vars, names(data_2016))

data_2016 <- data_2016 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# -------------------------------------------------------------------
# 7. MERGE LN2016 data
# -------------------------------------------------------------------
ln2016 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2016/LN2016.dta") %>%
  filter(entidad==15, month==2) %>% # keep if entidad==15 & month==2
  select(seccion, lista) %>%
  rename(section=seccion, listanominal=lista)

data_2016 <- data_2016 %>%
  left_join(ln2016, by="section")

# -------------------------------------------------------------------
# 8. gen turnout= total/listanominal
# -------------------------------------------------------------------
data_2016 <- data_2016 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 9. gen year=2016, month="March", STATE="ESTADO DE MEXICO"
#     sort uniqueid section
#     save Mexico_Section_2016.dta
# -------------------------------------------------------------------
data_2016 <- data_2016 %>%
  mutate(
    year=2016,
    month="March",
    STATE="ESTADO DE MEXICO"
  ) %>%
  arrange(uniqueid, section)

# -------------------------------------------------------------------
# 1. READ EXCEL
# -------------------------------------------------------------------
data_2018 <- read_excel("../../../Data/Raw Electoral Data/Mexico - 1996, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2018.xlsx", sheet="Ayuntamientos", guess_max=100000)

# -------------------------------------------------------------------
# 2. CONVERT allstring -> numeric
# -------------------------------------------------------------------
data_2018 <- data_2018 %>%
  mutate(across(id_seccion:total, ~ suppressWarnings(as.numeric(as.character(.)))))

# -------------------------------------------------------------------
# 3. MERGE INDEPENDENT columns
#    egen indep1 = rowtotal(CI_1-CI_15 CI_17 CI_18 CI_19)
#    gen indep2=CI_16
#    drop CI_*, rename indep1 -> CI_1, indep2 -> CI_2
# -------------------------------------------------------------------
# Identify columns from CI_1 to CI_15, plus CI_17, CI_18, CI_19
col_CI_1_15 <- paste0("CI_", 1:15)
extra_cols  <- c("CI_17","CI_18","CI_19")
all_CI_cols <- c(col_CI_1_15, extra_cols)
all_CI_cols <- intersect(all_CI_cols, names(data_2018))

# row total for indep1
data_2018 <- data_2018 %>%
  mutate(
    indep1 = rowSums(across(all_of(all_CI_cols)), na.rm=TRUE),
    indep2 = coalesce(CI_16, 0)  # if exists
  )

# drop all CI_* columns
drop_ci_cols <- grep("^CI_", names(data_2018), value=TRUE)
data_2018 <- data_2018 %>% select(-all_of(drop_ci_cols))

# rename
data_2018 <- data_2018 %>%
  rename(
    CI_1 = indep1,
    CI_2 = indep2
  )

# -------------------------------------------------------------------
# 4. RENAME municipio -> municipality, id_seccion -> section
# -------------------------------------------------------------------
data_2018 <- data_2018 %>%
  rename(
    municipality = municipio,
    section = id_seccion
  )

# -------------------------------------------------------------------
# 5. Generate uniqueid=0, then assign by municipality
# -------------------------------------------------------------------
data_2018 <- data_2018 %>%
  mutate(uniqueid = 0) %>%
  mutate(uniqueid = case_when(
    municipality ==  "ACAMBAY" ~ 15001,
    municipality ==  "ACOLMAN" ~ 15002,
    municipality ==  "ACULCO" ~ 15003,
    municipality ==  "ALMOLOYA DE ALQUISIRAS" ~ 15004,
    municipality ==  "ALMOLOYA DE JUAREZ" ~ 15005,
    municipality ==  "ALMOLOYA DEL RIO" ~ 15006,
    municipality ==  "AMANALCO" ~ 15007,
    municipality ==  "AMATEPEC" ~ 15008,
    municipality ==  "AMECAMECA" ~ 15009,
    municipality ==  "APAXCO" ~ 15010,
    municipality ==  "ATENCO" ~ 15011,
    municipality ==  "ATIZAPAN" ~ 15012,
    municipality ==  "ATIZAPAN DE ZARAGOZA" ~ 15013,
    municipality ==  "ATLACOMULCO" ~ 15014,
    municipality ==  "ATLAUTLA" ~ 15015,
    municipality ==  "AXAPUSCO" ~ 15016,
    municipality ==  "AYAPANGO" ~ 15017,
    municipality ==  "CALIMAYA" ~ 15018,
    municipality ==  "CAPULHUAC" ~ 15019,
    municipality ==  "CHALCO" ~ 15025,
    municipality ==  "CHAPA DE MOTA" ~ 15026,
    municipality ==  "CHAPULTEPEC" ~ 15027,
    municipality ==  "CHIAUTLA" ~ 15028,
    municipality ==  "CHICOLOAPAN" ~ 15029,
    municipality ==  "CHICONCUAC" ~ 15030,
    municipality ==  "CHIMALHUACAN" ~ 15031,
    municipality ==  "COACALCO DE BERRIOZABAL" | municipality == "COACALCO" ~ 15020,
    municipality ==  "COATEPEC HARINAS" ~ 15021,
    municipality ==  "COCOTITLAN" ~ 15022,
    municipality ==  "COYOTEPEC" ~ 15023,
    municipality ==  "CUAUTITLAN" ~ 15024,
    municipality ==  "CUAUTITLAN IZCALLI" ~ 15121,
    municipality ==  "DONATO GUERRA" ~ 15032,
    municipality ==  "ECATEPEC DE MORELOS" | municipality == "ECATEPEC" ~ 15033,
    municipality ==  "ECATZINGO" ~ 15034,
    municipality ==  "EL ORO" ~ 15064,
    municipality ==  "HUEHUETOCA" ~ 15035,
    municipality ==  "HUEYPOXTLA" ~ 15036,
    municipality ==  "HUIXQUILUCAN" ~ 15037,
    municipality ==  "ISIDRO FABELA" ~ 15038,
    municipality ==  "IXTAPALUCA" ~ 15039,
    municipality ==  "IXTAPAN DE LA SAL" ~ 15040,
    municipality ==  "IXTAPAN DEL ORO" ~ 15041,
    municipality ==  "IXTLAHUACA" ~ 15042,
    municipality ==  "JALTENCO" ~ 15044,
    municipality ==  "JILOTEPEC" ~ 15045,
    municipality ==  "JILOTZINGO" ~ 15046,
    municipality ==  "JIQUIPILCO" ~ 15047,
    municipality ==  "JOCOTITLAN" ~ 15048,
    municipality ==  "JOQUICINGO" ~ 15049,
    municipality ==  "JUCHITEPEC" ~ 15050,
    municipality ==  "LA PAZ" ~ 15070,
    municipality ==  "LERMA" ~ 15051,
    municipality ==  "LUVIANOS" ~ 15123,
    municipality ==  "MALINALCO" ~ 15052,
    municipality ==  "MELCHOR OCAMPO" ~ 15053,
    municipality ==  "METEPEC" ~ 15054,
    municipality ==  "MEXICALTZINGO" ~ 15055,
    municipality ==  "MORELOS" ~ 15056,
    municipality ==  "NAUCALPAN DE JUAREZ" | municipality=="NAUCALPAN" ~ 15057,
    municipality ==  "NEXTLALPAN" ~ 15059,
    municipality ==  "NEZAHUALCOYOTL" ~ 15058,
    municipality ==  "NICOLAS ROMERO" ~ 15060,
    municipality ==  "NOPALTEPEC" ~ 15061,
    municipality ==  "OCOYOACAC" ~ 15062,
    municipality ==  "OCUILAN" ~ 15063,
    municipality ==  "OTUMBA" ~ 15065,
    municipality ==  "OTZOLOAPAN" ~ 15066,
    municipality ==  "OTZOLOTEPEC" ~ 15067,
    municipality ==  "OZUMBA" ~ 15068,
    municipality ==  "PAPALOTLA" ~ 15069,
    municipality ==  "POLOTITLAN" ~ 15071,
    municipality ==  "RAYON" ~ 15072,
    municipality ==  "SAN ANTONIO LA ISLA" ~ 15073,
    municipality ==  "SAN FELIPE DEL PROGRESO" ~ 15074,
    municipality ==  "SAN JOSE DEL RINCON" ~ 15124,
    municipality ==  "SAN MARTIN DE LAS PIRAMIDES" ~ 15075,
    municipality ==  "SAN MATEO ATENCO" ~ 15076,
    municipality ==  "SAN SIMON DE GUERRERO" ~ 15077,
    municipality ==  "SANTO TOMAS" ~ 15078,
    municipality ==  "SOYANIQUILPAN DE JUAREZ" ~ 15079,
    municipality ==  "SULTEPEC" ~ 15080,
    municipality ==  "TECAMAC" ~ 15081,
    municipality ==  "TEJUPILCO" ~ 15082,
    municipality ==  "TEMAMATLA" ~ 15083,
    municipality ==  "TEMASCALAPA" ~ 15084,
    municipality ==  "TEMASCALCINGO" ~ 15085,
    municipality ==  "TEMASCALTEPEC" ~ 15086,
    municipality ==  "TEMOAYA" ~ 15087,
    municipality ==  "TENANCINGO" ~ 15088,
    municipality ==  "TENANGO DEL AIRE" ~ 15089,
    municipality ==  "TENANGO DEL VALLE" ~ 15090,
    municipality ==  "TEOLOYUCAN" ~ 15091,
    municipality ==  "TEOTIHUACAN" ~ 15092,
    municipality ==  "TEPETLAOXTOC" ~ 15093,
    municipality ==  "TEPETLIXPA" ~ 15094,
    municipality ==  "TEPOTZOTLAN" ~ 15095,
    municipality ==  "TEQUIXQUIAC" ~ 15096,
    municipality ==  "TEXCALTITLAN" ~ 15097,
    municipality ==  "TEXCALYACAC" ~ 15098,
    municipality ==  "TEXCOCO" ~ 15099,
    municipality ==  "TEZOYUCA" ~ 15100,
    municipality ==  "TIANGUISTENCO" ~ 15101,
    municipality ==  "TIMILPAN" ~ 15102,
    municipality ==  "TLALMANALCO" ~ 15103,
    municipality ==  "TLALNEPANTLA DE BAZ" | municipality=="TLALNEPANTLA" ~ 15104,
    municipality ==  "TLATLAYA" ~ 15105,
    municipality ==  "TOLUCA" ~ 15106,
    municipality ==  "TONANITLA" ~ 15125,
    municipality ==  "TONATICO" ~ 15107,
    municipality ==  "TULTEPEC" ~ 15108,
    municipality ==  "TULTITLAN" ~ 15109,
    municipality ==  "VALLE DE BRAVO" ~ 15110,
    municipality ==  "VALLE DE CHALCO SOLIDARIDAD" ~ 15122,
    municipality ==  "VILLA DE ALLENDE" ~ 15111,
    municipality ==  "VILLA DEL CARBON" ~ 15112,
    municipality ==  "VILLA GUERRERO" ~ 15113,
    municipality ==  "VILLA VICTORIA" ~ 15114,
    municipality ==  "XALATLACO" ~ 15043,
    municipality ==  "XONACATLAN" ~ 15115,
    municipality ==  "ZACAZONAPAN" ~ 15116,
    municipality ==  "ZACUALPAN" ~ 15117,
    municipality ==  "ZINACANTEPEC" ~ 15118,
    municipality ==  "ZUMPAHUACAN" ~ 15119,
    municipality ==  "ZUMPANGO" ~ 15120,
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 6. collapse (sum) listanominal-CI_2 (first) coalpbt coalfrente, by(municipality uniqueid section)
# -------------------------------------------------------------------
# Identify columns from listanominal to CI_2, plus "coalpbt" & "coalfrente"
all_cols_3 <- names(data_2018)
start_var_3 <- "listanominal"
end_var_3   <- "CI_2"
pos_start_3 <- match(start_var_3, all_cols_3)
pos_end_3   <- match(end_var_3,   all_cols_3)
vars_range3 <- all_cols_3[pos_start_3:pos_end_3]

extra_coal <- c("coalpbt","coalfrente") # also keep (first) logic
vars_collapse3 <- unique(c(vars_range3, extra_coal))

group_vars2 <- c("municipality","uniqueid","section")

data_2018 <- data_2018 %>%
  group_by(across(all_of(group_vars2))) %>%
  summarize(
    across(all_of(vars_collapse3), sum, na.rm=TRUE),
    .groups="drop"
  )

# -------------------------------------------------------------------
# 7. replace PT_MORENA_PES etc. if coalpbt==1
#    Then drop partial columns
# -------------------------------------------------------------------
if("coalpbt" %in% names(data_2018) && "PT_MORENA_PES" %in% names(data_2018)) {
  data_2018 <- data_2018 %>%
    mutate(
      PT_MORENA_PES = if_else(coalpbt==1,
                              coalesce(PT_MORENA_PES,0) + coalesce(PT_MORENA,0) + coalesce(PT_PES,0) +
                                coalesce(MORENA_PES,0) + coalesce(PT,0) + coalesce(MORENA,0) + coalesce(PES,0),
                              PT_MORENA_PES
      ),
      PT   = if_else(coalpbt==1, 0, PT),
      MORENA = if_else(coalpbt==1, 0, MORENA),
      PES  = if_else(coalpbt==1, 0, PES)
    )
  
  # drop PT_MORENA PT_PES MORENA_PES
  drop_coalpbt_cols <- c("PT_MORENA","PT_PES","MORENA_PES")
  drop_coalpbt_cols <- intersect(drop_coalpbt_cols, names(data_2018))
  data_2018 <- data_2018 %>% select(-all_of(drop_coalpbt_cols))
}

# replace PAN_PRD_MC = PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC if coalfrente==1
# then zero out
if("coalfrente" %in% names(data_2018) && "PAN_PRD_MC" %in% names(data_2018)) {
  data_2018 <- data_2018 %>%
    mutate(
      PAN_PRD_MC = if_else(coalfrente==1,
                           coalesce(PAN_PRD_MC,0) + coalesce(PAN_PRD,0) + coalesce(PAN_MC,0) + coalesce(PRD_MC,0)
                           + coalesce(PAN,0) + coalesce(PRD,0) + coalesce(MC,0),
                           PAN_PRD_MC
      ),
      PAN = if_else(coalfrente==1, 0, PAN),
      PRD = if_else(coalfrente==1, 0, PRD),
      MC  = if_else(coalfrente==1, 0, MC)
    )
  
  # drop partial
  drop_coalfrente_cols <- c("PAN_PRD","PAN_MC","PRD_MC")
  drop_coalfrente_cols <- intersect(drop_coalfrente_cols, names(data_2018))
  data_2018 <- data_2018 %>% select(-all_of(drop_coalfrente_cols))
}

# rename NA -> PANAL if needed
if("NA" %in% names(data_2018)) {
  data_2018 <- data_2018 %>% rename(PANAL = NA)
}

# -------------------------------------------------------------------
# 8. egen valid = rowtotal(...)
# -------------------------------------------------------------------
valid_vars <- c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","MORENA","PES","VIA_RADICAL",
                "PAN_PRD_MC","PT_MORENA_PES","CI_1","CI_2")
valid_vars <- intersect(valid_vars, names(data_2018))

data_2018 <- data_2018 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))


# -------------------------------------------------------------------
# 9. gen year=2018, month="July", STATE="ESTADO DE MEXICO"
#     merge incumbents2018.dta (m:1 uniqueid)
# -------------------------------------------------------------------
data_2018 <- data_2018 %>%
  mutate(
    year=2018,
    month="July",
    STATE="ESTADO DE MEXICO"
  )



# drop coal* no_reg nulo if they exist
drop_coal_etc <- c(grep("^coal", names(data_2018), value=TRUE), "no_reg","nulo")
drop_coal_etc <- intersect(drop_coal_etc, names(data_2018))
data_2018 <- data_2018 %>% select(-all_of(drop_coal_etc))

# Combine the dataframes, handling different columns by filling with NA
Edo_mexico_all <- bind_rows(data_1996,
                            data_2000,
                            data_2003,
                            extra_2003,#extra elections
                            data_2006,
                            data_2009,
                            data_2012,
                            data_2015,
                            data_2016)#extra elections

data.table::fwrite(Edo_mexico_all,"../../../Processed Data/mexico/mexico_process_raw_data.csv")

