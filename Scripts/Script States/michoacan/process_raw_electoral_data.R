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

# -------------------------------------- #
# 1. Process Election Data for 1995
# -------------------------------------- #
data_1995 <- data.table::fread("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Ayu_Seccion_1995_No_LN.csv")

# -------------------------------------------------------------------
# 2. RENAME municipio -> municipality, seccin -> section
# -------------------------------------------------------------------
data_1995 <- data_1995 %>%
  dplyr::rename(
    municipality = Municipio,
    section = "Secci\xf3n")

# Convert column names to lowercase
data_1995 <- data_1995 %>% rename_with(tolower)

# -------------------------------------------------------------------
# 3. DROP if municipality=="" & section==.
#    In R: filter out
# -------------------------------------------------------------------
data_1995 <- data_1995 %>%
  filter(!(municipality=="" & is.na(section)))

# -------------------------------------------------------------------
# 4. Convert columns from pan to nulos into numeric
#    Stata: destring pan - nulos
# -------------------------------------------------------------------
# Find columns from "pan" through "nulos"
all_cols   <- names(data_1995)
start_var  <- "pan"
end_var    <- "nulos"

start_pos  <- match(start_var, all_cols)
end_pos    <- match(end_var,   all_cols)

vars_to_num <- all_cols[start_pos:end_pos]

data_1995 <- data_1995 %>%
  mutate(across(all_of(vars_to_num), ~as.numeric(as.character(.))))

# -------------------------------------------------------------------
# 5. collapse (sum) pan-nulos, by(municipality, section)
# -------------------------------------------------------------------
data_1995 <- data_1995 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_to_num), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 6. gen total = pan + pri + prd + pfcrn + pt + noreg + nulos
# -------------------------------------------------------------------
# It's possible that some columns might not exist if your data lacks them.
# We'll do a rowSums if columns exist.
needed_for_total <- c("pan","pri","prd","pfcrn","pt","noreg","nulos")
needed_for_total <- intersect(needed_for_total, names(data_1995))

data_1995 <- data_1995 %>%
  mutate(total = rowSums(across(all_of(needed_for_total)), na.rm=TRUE))

# (Optional) drop if total==. | total==0 if you want to replicate Stata logic exactly.
# The code is commented in Stata, so we skip.

# -------------------------------------------------------------------
# 7. RENAME party variables
#    pan->PAN, pri->PRI, prd->PRD, pfcrn->PartCardenista, pt->PT
# -------------------------------------------------------------------
data_1995 <- data_1995 %>%
  rename(
    PAN             = pan,
    PRI             = pri,
    PRD             = prd,
    PartCardenista  = pfcrn,
    PT              = pt
  )

# -------------------------------------------------------------------
# 8. DROP noreg, nulos
# -------------------------------------------------------------------
vars_drop <- c("noreg","nulos")
vars_drop <- intersect(vars_drop, names(data_1995))
data_1995 <- data_1995 %>% select(-all_of(vars_drop))

# -------------------------------------------------------------------
# 9. gen uniqueid=0, then replace by municipality
# -------------------------------------------------------------------
data_1995 <- data_1995 %>%
  mutate(uniqueid=0) %>%
  mutate(uniqueid = case_when(
    municipality=="ACUITZIO" ~ 16001,
    municipality=="AGUILILLA" ~ 16002,
    municipality=="A. OBREGON." ~ 16003,
    municipality=="ANGAMACUTIRO" ~ 16004,
    municipality=="ANGANGUEO" ~ 16005,
    municipality=="APATZINGAN" ~ 16006,
    municipality=="APORO" ~ 16007,
    municipality=="AQUILA" ~ 16008,
    municipality=="ARIO DE R." ~ 16009,
    municipality=="ARTEAGA" ~ 16010,
    # e.g. "BRISEÑAS" | strpos(municipality, "BRISE")>0 in R => use str_detect
    municipality=="BRISEÑAS" | str_detect(municipality, "BRISE") ~ 16011,
    municipality=="BUENAVISTA" ~ 16012,
    municipality=="CARACUARO" ~ 16013,
    municipality=="CHARAPAN" ~ 16021,
    municipality=="CHARO" ~ 16022,
    municipality=="CHAVINDA" ~ 16023,
    municipality=="CHERAN" ~ 16024,
    municipality=="CHILCHOTA" ~ 16025,
    municipality=="CHINICUILA" ~ 16026,
    municipality=="CHUCANDIRO" ~ 16027,
    municipality=="CHURINTZIO" ~ 16028,
    municipality=="CHURUMUCO" ~ 16029,
    municipality=="COAHUAYANA" ~ 16014,
    municipality=="COALCOMAN" ~ 16015,
    municipality=="COENEO" ~ 16016,
    municipality=="REGULES" ~ 16074, 
    municipality=="CONTEPEC" ~ 16017,
    municipality=="COPANDARO" ~ 16018,
    municipality=="COTIJA" ~ 16019,
    municipality=="CUITZEO" ~ 16020,
    municipality=="ECUANDUREO" ~ 16030,
    municipality=="E. HUERTA" ~ 16031,
    municipality=="ERONGARI." ~ 16032, 
    municipality=="GABRIEL ZAMORA" ~ 16033,
    municipality=="HIDALGO" ~ 16034,
    municipality=="HUANDACAREO" ~ 16036,
    municipality=="HUANIQUEO" ~ 16037,
    municipality=="HUETAMO" ~ 16038,
    municipality=="HUIRAMBA" ~ 16039,
    municipality=="INDAPARAPEO" ~ 16040,
    municipality=="IRIMBO" ~ 16041,
    municipality=="IXTLAN" ~ 16042,
    municipality=="JACONA" ~ 16043,
    municipality=="JIMENEZ" ~ 16044,
    municipality=="JIQUILPAN" ~ 16045,
    municipality=="JUAREZ" ~ 16046,
    municipality=="JUNGAPEO" ~ 16047,
    municipality=="LA HUACANA" ~ 16035,
    municipality=="LA PIEDAD" ~ 16069,
    municipality=="LAGUNILLAS" ~ 16048,
    municipality=="L. CARDENAS" ~ 16052,
    municipality=="LOS REYES" ~ 16075,
    municipality=="MADERO" ~ 16049,
    municipality=="MARAVATIO" ~ 16050,
    municipality=="M.CASTELLANOS" ~ 16051,
    municipality=="MORELIA" ~ 16053,
    municipality=="MORELOS" ~ 16054,
    municipality=="MUGICA" ~ 16055,
    municipality=="NAHUATZEN" ~ 16056,
    municipality=="NOCUPETARO" ~ 16057,
    municipality=="NUEVO PARANGARI" ~ 16058,
    municipality=="NUEVO URECHO" ~ 16059,
    municipality=="NUMARAN" ~ 16060,
    municipality=="OCAMPO" ~ 16061,
    municipality=="PAJACUARAN" ~ 16062,
    municipality=="PANINDICUARO" ~ 16063,
    municipality=="PARACHO" ~ 16065,
    municipality=="PARACUARO" ~ 16064,
    municipality=="PATZCUARO" ~ 16066,
    municipality=="PENJAMILLO" ~ 16067,
    municipality=="PERIBAN" ~ 16068,
    municipality=="PUREPERO" ~ 16070,
    municipality=="PURUANDIRO" ~ 16071,
    municipality=="QUERENDARO" ~ 16072,
    municipality=="QUIROGA" ~ 16073,
    municipality=="SAHUAYO" ~ 16076,
    municipality=="SALV. ESC." ~ 16079,
    municipality=="SAN LUCAS" ~ 16077,
    municipality=="STA. ANA M." ~ 16078,
    municipality=="SENGUIO" ~ 16080,
    municipality=="SUSUPUATO" ~ 16081,
    municipality=="TACAMBARO" ~ 16082,
    municipality=="TANCITARO" ~ 16083,
    municipality=="TANGAMANDAP." ~ 16084,
    municipality=="TANGANCI" ~ 16085,
    municipality=="TANHUATO" ~ 16086,
    municipality=="TARETAN" ~ 16087,
    municipality=="TARIMBARO" ~ 16088,
    municipality=="TEPALCATEP." ~ 16089,
    municipality=="TINGAMBATO" ~ 16090,
    municipality=="TINGUINDIN" ~ 16091,
    municipality=="TIQUICHEO" ~ 16092,
    municipality=="TLALPUJAHUA" ~ 16093,
    municipality=="TLAZAZALCA" ~ 16094,
    municipality=="TOCUMBO" ~ 16095,
    municipality=="TUMBISCATIO" ~ 16096,
    municipality=="TURICATO" ~ 16097,
    municipality=="TUXPAN" ~ 16098,
    municipality=="TUZANTLA" ~ 16099,
    municipality=="TZINTZUNTZAN" ~ 16100,
    municipality=="TZITZIO" ~ 16101,
    municipality=="URUAPAN" ~ 16102,
    municipality=="V. CARRANZA" ~ 16103,
    municipality=="VILLAMAR" ~ 16104,
    municipality=="VISTA HERMOSA" ~ 16105,
    municipality=="YURECUARO" ~ 16106,
    municipality=="ZACAPU" ~ 16107,
    municipality=="ZAMORA" ~ 16108,
    municipality=="ZINAPARO" ~ 16109,
    municipality=="ZINAPECUARO" ~ 16110,
    municipality=="ZIRACUARE" ~ 16111,
    municipality=="ZITACUARO" ~ 16112,
    municipality=="JOSE SIXTO VERDUZCO" ~ 16113,
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 10. egen valid = rowtotal(PAN PRI PRD PartCardenista PT)
# -------------------------------------------------------------------
party_vars <- c("PAN","PRI","PRD","PartCardenista","PT")
party_vars <- intersect(party_vars, names(data_1995))

data_1995 <- data_1995 %>%
  mutate(valid = rowSums(across(all_of(party_vars)), na.rm=TRUE))

# -------------------------------------------------------------------
# 11. gen year=1995, month="November"
# -------------------------------------------------------------------
data_1995 <- data_1995 %>%
  mutate(
    year  = 1995,
    month = "November"
  )

# -------------------------------------------------------------------
# 1. READ CSV (Stata: insheet using Ayu_Seccion_1998_Half_Missing.csv)
# -------------------------------------------------------------------
data_1998 <- read_csv("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Ayu_Seccion_1998_Half_Missing.csv", show_col_types = FALSE)
# Convert column names to lowercase
data_1998 <- data_1998 %>% rename_with(tolower)
names(data_1998) <- gsub("[- ]", "", names(data_1998))
names(data_1998) <- gsub("[.]", "", names(data_1998))
# -------------------------------------------------------------------
# 2. RENAME municipio -> municipality, listanom -> listanominal, seccion -> section
# -------------------------------------------------------------------
data_1998 <- data_1998 %>%
  rename(
    municipality  = municipio,
    listanominal = listanom,
    section       = seccion
  )

# -------------------------------------------------------------------
# 3. DROP if municipality==. & section==. or total==. | total==0
#    In R, we check for NA, numeric
# -------------------------------------------------------------------
data_1998 <- data_1998 %>%
  filter(!(is.na(municipality) & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

# -------------------------------------------------------------------
# 4. Convert columns from pan through total to numeric
#    Stata: destring pan - total
# -------------------------------------------------------------------
all_cols <- names(data_1998)
start_var <- "pan"
end_var   <- "total"
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var,   all_cols)

vars_to_num <- all_cols[start_pos:end_pos]

data_1998 <- data_1998 %>%
  mutate(across(all_of(vars_to_num), ~as.numeric(as.character(.))))

# -------------------------------------------------------------------
# 5. gen missing = listanominal==0
# -------------------------------------------------------------------
data_1998 <- data_1998 %>%
  mutate(missing = if_else(listanominal==0, 1, 0))

# -------------------------------------------------------------------
# 6. collapse (sum) missing pan - total listanominal, by(municipality, section)
# -------------------------------------------------------------------
vars_collapse <- unique(c("missing", vars_to_num, "listanominal"))
data_1998 <- data_1998 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 7. rename pan->PAN, pri->PRI, prd->PRD, pt->PT, pvem->PVEM
# -------------------------------------------------------------------
data_1998 <- data_1998 %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT  = pt,
    PVEM= pvem
  )

# -------------------------------------------------------------------
# 8. MERGE with "..\..\all_months_years.dta" by ed=16, seccion=section
#    keep if month==9 & year==1998
# -------------------------------------------------------------------
data_1998 <- data_1998 %>%
  mutate(ed=16, seccion=section)

all_months <- read_dta("../../all_months_years.dta") %>%
  select(ed, seccion, month, year, lista)

data_1998 <- data_1998 %>%
  left_join(all_months, by=c("ed","seccion")) %>%
  filter(month==9 & year==1998)

# drop if _merge==2 => not used in left_join
# drop _merge, ed, seccion, year, month
drops <- c("ed","seccion","year","month")
data_1998 <- data_1998 %>%
  select(-all_of(drops))

# -------------------------------------------------------------------
# 9. replace listanominal = lista if missing>=1
# -------------------------------------------------------------------
data_1998 <- data_1998 %>%
  mutate(listanominal = if_else(missing >= 1, lista, listanominal))

# drop missing, lista
drop_vars <- c("missing","lista")
drop_vars <- intersect(drop_vars, names(data_1998))
data_1998 <- data_1998 %>% select(-all_of(drop_vars))

# -------------------------------------------------------------------
# 10. gen turnout = total/listanominal
# -------------------------------------------------------------------
data_1998 <- data_1998 %>%
  mutate(turnout = total / listanominal)

# drop noregistrados nulos => if they exist
extra_drop <- c("noregistrados","nulos")
extra_drop <- intersect(extra_drop, names(data_1998))
data_1998 <- data_1998 %>% select(-all_of(extra_drop))

# -------------------------------------------------------------------
# 11. egen valid = rowtotal(PAN PRI PRD PT PVEM)
# -------------------------------------------------------------------
valid_vars <- c("PAN","PRI","PRD","PT","PVEM")
valid_vars <- intersect(valid_vars, names(data_1998))

data_1998 <- data_1998 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# -------------------------------------------------------------------
# 12. gen year=1998, month="November"
# -------------------------------------------------------------------
data_1998 <- data_1998 %>%
  mutate(
    year=1998,
    month="November"
  )

# drop municipality per code
data_1998 <- data_1998 %>% select(-municipality)

# -------------------------------------------------------------------
# 1. READ CSV (Stata: insheet using "Ayu_Seccion_2001.csv")
# -------------------------------------------------------------------
data_2001 <- read_csv("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Ayu_Seccion_2001.csv", show_col_types = FALSE)
# Convert column names to lowercase
data_2001 <- data_2001 %>% rename_with(tolower)
names(data_2001) <- gsub("[- ]", "", names(data_2001))
names(data_2001) <- gsub("[.]", "", names(data_2001))
# -------------------------------------------------------------------
# 2. RENAME municipio -> municipality, seccion -> section
# -------------------------------------------------------------------
data_2001 <- data_2001 %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion7
  )

# -------------------------------------------------------------------
# 3. Drop if municipality=="" & section==. or total==. | total==0
# -------------------------------------------------------------------
data_2001 <- data_2001 %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

# -------------------------------------------------------------------
# 4. Destring listanominal pan - total
#    Convert columns from 'pan' through 'total' into numeric
# -------------------------------------------------------------------
all_cols  <- names(data_2001)
start_var <- "pan"
end_var   <- "total"

start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var,   all_cols)
vars_to_num <- c("listanominal", all_cols[start_pos:end_pos])

# Convert them to numeric
data_2001 <- data_2001 %>%
  mutate(across(all_of(vars_to_num), ~ as.numeric(as.character(.))))

# -------------------------------------------------------------------
# 5. collapse (sum) listanominal pan - total, by(municipality, section)
# -------------------------------------------------------------------
vars_collapse <- unique(vars_to_num)

data_2001 <- data_2001 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 6. RENAME columns
#    pan->PAN, pri->PRI, prdptpvempaspsnconvergencia->PRD_PT_PC_PVEM_PAS_PSN
# -------------------------------------------------------------------
data_2001 <- data_2001 %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD_PT_PC_PVEM_PAS_PSN = prdptpvempaspsnconvergencia
  )

# -------------------------------------------------------------------
# 7. gen turnout = total/listanominal
# -------------------------------------------------------------------
data_2001 <- data_2001 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 8. Drop noreg, nulos if they exist
# -------------------------------------------------------------------
drop_vars <- c("noreg","nulos")
drop_vars <- intersect(drop_vars, names(data_2001))
data_2001 <- data_2001 %>% select(-all_of(drop_vars))

# -------------------------------------------------------------------
# 9. uniqueid=0, then assign codes by municipality
# -------------------------------------------------------------------
data_2001 <- data_2001 %>%
  mutate(uniqueid = 0) %>%
  mutate(uniqueid = case_when(
    municipality == "Acuitzio" ~ 16001,
    municipality == "Aguililla" ~ 16002,
    municipality == "Alvaro Obregon" ~ 16003,
    municipality == "Angamacutiro" ~ 16004,
    municipality == "Angangueo" ~ 16005,
    municipality == "Apatzingan" ~ 16006,
    municipality == "Aporo" ~ 16007,
    municipality == "Aquila" ~ 16008,
    municipality == "Ario" ~ 16009,
    municipality == "Arteaga" ~ 16010,
    (municipality == "Brise?as" | str_detect(municipality, "Brise")) ~ 16011,
    municipality == "Buenavista" ~ 16012,
    municipality == "Caracuaro" ~ 16013,
    municipality == "Charapan" ~ 16021,
    municipality == "Charo" ~ 16022,
    municipality == "Chavinda" ~ 16023,
    municipality == "Cheran" ~ 16024,
    municipality == "Chilchota" ~ 16025,
    municipality == "Chinicuila" ~ 16026,
    municipality == "Chucandiro" ~ 16027,
    municipality == "Churintzio" ~ 16028,
    municipality == "Churumuco" ~ 16029,
    municipality == "Coahuayana" ~ 16014,
    municipality == "Coalcoman" ~ 16015,
    municipality == "Coeneo" ~ 16016,
    municipality == "Regules" ~ 16074,
    municipality == "Contepec" ~ 16017,
    municipality == "Copandaro" ~ 16018,
    municipality == "Cotija" ~ 16019,
    municipality == "Cuitzeo" ~ 16020,
    municipality == "Ecuandureo" ~ 16030,
    municipality == "Epitacio Huerta" ~ 16031,
    municipality == "Erongaricuaro" ~ 16032,
    municipality == "Gabriel Zamora" ~ 16033,
    municipality == "Hidalgo" ~ 16034,
    municipality == "Huandacareo" ~ 16036,
    municipality == "Huaniqueo" ~ 16037,
    municipality == "Huatamo" ~ 16038,
    municipality == "Huiramba" ~ 16039,
    municipality == "Indaparapeo" ~ 16040,
    municipality == "Irimbo" ~ 16041,
    municipality == "Ixtlan" ~ 16042,
    municipality == "Jacona" ~ 16043,
    municipality == "Jimenez" ~ 16044,
    municipality == "Jiquilpan" ~ 16045,
    municipality == "Jose Sixto Verduzco" ~ 16113,
    municipality == "Juarez" ~ 16046,
    municipality == "Juagapeo" ~ 16047,
    municipality == "La Huacana" ~ 16035,
    municipality == "La Piedad" ~ 16069,
    municipality == "Lagunillas" ~ 16048,
    municipality == "Lazaro Cardenas" ~ 16052,
    municipality == "Los Reyes" ~ 16075,
    municipality == "Madero" ~ 16049,
    municipality == "Maravatio" ~ 16050,
    municipality == "Marcos Castellanos" ~ 16051,
    municipality == "Morelia" ~ 16053,
    municipality == "Morelos" ~ 16054,
    municipality == "Mugica" ~ 16055,
    municipality == "Nahuatzen" ~ 16056,
    municipality == "Nocupetaro" ~ 16057,
    municipality == "Nuevo Parangaricutiro" ~ 16058,
    municipality == "Nuevo Urecho" ~ 16059,
    municipality == "Numaran" ~ 16060,
    municipality == "Ocampo" ~ 16061,
    municipality == "Pajacuaran" ~ 16062,
    municipality == "Panindicuaro" ~ 16063,
    municipality == "Paracho" ~ 16065,
    municipality == "Paracuaro" ~ 16064,
    municipality == "Patzcuaro" ~ 16066,
    municipality == "Penjamillo" ~ 16067,
    municipality == "Periban" ~ 16068,
    municipality == "Purepero" ~ 16070,
    municipality == "Puruandiro" ~ 16071,
    municipality == "Querendaro" ~ 16072,
    municipality == "Quiroga" ~ 16073,
    municipality == "Sahuayo" ~ 16076,
    municipality == "Salvador Escalante" ~ 16079,
    municipality == "San Lucas" ~ 16077,
    municipality == "Santa Ana Maya" ~ 16078,
    municipality == "Senguio" ~ 16080,
    municipality == "Susupuato" ~ 16081,
    municipality == "Tacambaro" ~ 16082,
    municipality == "Tancitaro" ~ 16083,
    municipality == "Tangamandapio" ~ 16084,
    municipality == "Tangancicuaro" ~ 16085,
    municipality == "Tanhuato" ~ 16086,
    municipality == "Taretan" ~ 16087,
    municipality == "Tarimbaro" ~ 16088,
    municipality == "Tepalcatepec" ~ 16089,
    municipality == "Tingambato" ~ 16090,
    municipality == "Tinguindin" ~ 16091,
    municipality == "Tiquicheo" ~ 16092,
    municipality == "Tlalpujahua" ~ 16093,
    municipality == "Tlazazalca" ~ 16094,
    municipality == "Tocumbo" ~ 16095,
    municipality == "Tumbiscatio" ~ 16096,
    municipality == "Turicato" ~ 16097,
    municipality == "Tuxpan" ~ 16098,
    municipality == "Tuzantla" ~ 16099,
    municipality == "Tzintzuntzan" ~ 16100,
    municipality == "Tzitzio" ~ 16101,
    municipality == "Uruapan" ~ 16102,
    municipality == "Venustiano Carranza" ~ 16103,
    municipality == "Villamar" ~ 16104,
    municipality == "Vista Hermosa" ~ 16105,
    municipality == "Yurecuaro" ~ 16106,
    municipality == "Zacapu" ~ 16107,
    municipality == "Zamora" ~ 16108,
    municipality == "Zinaparo" ~ 16109,
    municipality == "Zinapecuaro" ~ 16110,
    municipality == "Ziracuaretiro" ~ 16111,
    municipality == "Zitacuaro" ~ 16112,
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 10. egen valid = rowtotal(PAN PRI PRD_PT_PC_PVEM_PAS_PSN)
# -------------------------------------------------------------------
data_2001 <- data_2001 %>%
  mutate(valid = rowSums(across(c("PAN","PRI","PRD_PT_PC_PVEM_PAS_PSN")), na.rm=TRUE))

# -------------------------------------------------------------------
# 11. gen year=2001, month="November"
# -------------------------------------------------------------------
data_2001 <- data_2001 %>%
  mutate(
    year=2001,
    month="November"
  )

# -------------------------------------------------------------------
# 1. IMPORT EXCEL
#    Stata: import excel "resultados_electorales_2005.xls", sheet("Ayuntamiento, por Casilla, 2005") cellrange(A7:K36) firstrow clear
# -------------------------------------------------------------------
data_2005 <- read_excel(
  "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/resultados_electorales_2005.xls",
  sheet = "Ayuntamiento, por Casilla, 2005",
  range = "A7:K36"
)
# Convert column names to lowercase
data_2005 <- data_2005 %>% rename_with(tolower)
names(data_2005) <- gsub("[- ]", "", names(data_2005))

# -------------------------------------------------------------------
# 2. KEEP if strpos(B, "ecci")>0
#    In R: filter rows where column B has "ecci"
# -------------------------------------------------------------------
# Column B might be named something else from read_excel. Let's identify it.
# Suppose the second column is named something like "B". 
# We'll rename them or directly refer to them by col index if needed.
# Check names(data_2005) to see actual column names.
names(data_2005)

# For clarity, let's assume column B is literally named "B".
# Filter for rows where str_detect(B,"ecci")
data_2005 <- data_2005 %>%
  filter(str_detect(...2, "ecci"))

# -------------------------------------------------------------------
# 3. CREATE section = substr(B, -4, 4)
# -------------------------------------------------------------------
data_2005 <- data_2005 %>%
  mutate(section = str_sub(...2, start = -4, end = -1))

# -------------------------------------------------------------------
# 4. DROP columns A, B
# -------------------------------------------------------------------
data_2005 <- data_2005 %>%
  select(-...1, -...2)

# -------------------------------------------------------------------
# 5. RENAME columns to match Stata code
#    rename ListaNominal -> listanominal
#    rename VotosTotales -> total
# -------------------------------------------------------------------
# Adjust as needed if your actual column names differ:
data_2005 <- data_2005 %>%
  rename(
    listanominal = listanominal,
    total = votostotales
  )

# -------------------------------------------------------------------
# 6. GEN municipality="TUMBISCATIO EXTRAORDINARIO", uniqueid=16096
# -------------------------------------------------------------------
data_2005 <- data_2005 %>%
  mutate(
    municipality = "TUMBISCATIO EXTRAORDINARIO",
    uniqueid = 16096
  )

# -------------------------------------------------------------------
# 7. DROP if total==. | total==0
# -------------------------------------------------------------------
data_2005 <- data_2005 %>%
  filter(!(is.na(total) | total==0))

# -------------------------------------------------------------------
# 8. DESTRING _all, replace
#    In R: convert everything to numeric if possible
# -------------------------------------------------------------------
data_2005 <- data_2005 %>%
  mutate(across(listanominal:section, ~ suppressWarnings(as.numeric(as.character(.)))))

# -------------------------------------------------------------------
# 9. RENAME columns for parties
#    rename PartidoRevolucionarioInstituci -> PRI
#    rename PartidodelaRevoluciónDemocrá -> PRD
#    rename PartidodelTrabajo -> PT
#    rename Convergencia -> PC
# -------------------------------------------------------------------
# Adjust based on your actual column names from read_excel output:
data_2005 <- data_2005 %>%
  rename(
    PRI = partidorevolucionarioinstitucional,
    PRD = partidodelarevolucióndemocrática,
    PT  = partidodeltrabajo,
    PC  = convergencia
  )
# Possibly rename others as needed if you have them (like "PAN").

# -------------------------------------------------------------------
# 10. collapse (sum) listanominal-PC total, by (uniqueid municipality section)
# -------------------------------------------------------------------
# Identify columns from listanominal through PC plus total
all_cols <- names(data_2005)
start_var <- "listanominal"
end_var   <- "PC"
pos_start <- match(start_var, all_cols)
pos_end   <- match(end_var,   all_cols)

vars_to_sum <- all_cols[pos_start:pos_end]
vars_to_sum <- unique(c(vars_to_sum, "total"))

group_vars <- c("uniqueid","municipality","section")

data_2005 <- data_2005 %>%
  group_by(across(all_of(group_vars))) %>%
  summarize(across(all_of(vars_to_sum), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 11. gen turnout = total/listanominal
# -------------------------------------------------------------------
data_2005 <- data_2005 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 12. egen valid = rowtotal(PRI PRD PT PC)
# -------------------------------------------------------------------
party_vars <- c("PRI","PRD","PT","PC")
party_vars <- intersect(party_vars, names(data_2005))

data_2005 <- data_2005 %>%
  mutate(valid = rowSums(across(all_of(party_vars)), na.rm=TRUE))

# -------------------------------------------------------------------
# 13. gen year=2005, month="June"
# -------------------------------------------------------------------
data_2005 <- data_2005 %>%
  mutate(
    year = 2005,
    month= "June"
  )

# -------------------------------------------------------------------
# 1. IMPORT EXCEL
#    Stata: import excel Ayu_Seccion_2007.xlsx, clear firstrow case(lower)
# -------------------------------------------------------------------
data_2007 <- read_excel("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Ayu_Seccion_2007.xlsx", sheet = 1, # adjust if needed
                        col_names = TRUE) %>%
  # "case(lower)" might suggest we convert colnames to lowercase:
  rename_with(tolower)
names(data_2007)
# For clarity, check what columns you have. 
# We'll proceed assuming columns match the Stata references:
#   municipio -> municipality
#   seccion   -> section
#   listanom  -> listanominal
# etc.

# -------------------------------------------------------------------
# 2. RENAME municipio -> municipality, seccion -> section
# -------------------------------------------------------------------
# If the columns are already in lowercase from read_excel, 
# we can rename them if they aren't already.
# We'll do direct renaming to confirm:

data_2007 <- data_2007 %>%
  rename(
    municipality = municipio,
    section      = seccion,
    listanominal      = "lista nominal"
  )

# -------------------------------------------------------------------
# 3. DROP invalid rows
#    drop if municipality=="" & section==.
#    Then create `total` using rowtotal
# -------------------------------------------------------------------
data_2007 <- data_2007 %>%
  filter(!(municipality == "" & is.na(section)))

# egen total = rowtotal(pan pri prd pt pvem pc pna pas coal_prd_pt ...
# then drop if total==. | total==0
# We'll do something similar in R:

# Let's identify the columns for rowtotal
# The code references:
#   pan pri prd pt pvem pc pna pas 
#   coal_prd_pt coal_prd_pt_pc coal_prd_pc
#   cc_pan_pri cc_pan_pri_pvem cc_pan_pvem  cc_pan_pvem_pna cc_pan_pna cc_pri_pvem cc_pri_pvem_pna noreg nulos
cols_for_total <- c("pan","pri","prd","pt","pvem","pc","pna","pas",
                    "coal_prd_pt","coal_prd_pt_pc","coal_prd_pc",
                    "cc_pan_pri","cc_pan_pri_pvem","cc_pan_pvem","cc_pan_pvem_pna","cc_pan_pna",
                    "cc_pri_pvem","cc_pri_pvem_pna","noreg","nulos")

cols_for_total <- intersect(cols_for_total, names(data_2007))

data_2007 <- data_2007 %>%
  rowwise() %>%
  mutate(total = sum(c_across(all_of(cols_for_total)), na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!(is.na(total) | total==0))

# -------------------------------------------------------------------
# 4. DESTRING (Stata: destring listanominal - cc_pri_pvem_pna total)
#    In R: convert relevant columns to numeric if needed
# -------------------------------------------------------------------
# We'll guess listanominal might be "listanominal" or something similar
# If we want to convert everything from 'listanominal' to 'cc_pri_pvem_pna' plus 'total', 
# we identify those columns:

all_cols <- names(data_2007)
start_var <- "listanominal"
end_var   <- "cc_pri_pvem_pna"
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var,   all_cols)
vars_to_num <- all_cols[start_pos:end_pos]
vars_to_num <- c(vars_to_num, "total") # include total

# Convert them to numeric
data_2007 <- data_2007 %>%
  mutate(across(all_of(vars_to_num), ~ as.numeric(as.character(.))))

# -------------------------------------------------------------------
# 5. collapse (sum) from listanominal - cc_pri_pvem_pna plus total,
#    by(municipality, section)
# -------------------------------------------------------------------
vars_collapse <- unique(c(vars_to_num))
data_2007 <- data_2007 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 6. Process Coalitions: e.g. rename coal_prd_pt -> prd_pt,
#    merges with prd + pt + pc, etc.
# -------------------------------------------------------------------
# The code after "***********************************************************************************" merges 
# the coalition columns with the base party columns and zeroes them out if > 0.
# We'll replicate that logic carefully.

# rename coal_prd_pt -> prd_pt
if("coal_prd_pt" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    rename(prd_pt = coal_prd_pt)
}

# replace coal_prd_pt_pc = prd + pt + pc + coal_prd_pt_pc if coal_prd_pt_pc>0
# then zero out prd,pt,pc
if("coal_prd_pt_pc" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    mutate(
      coal_prd_pt_pc = if_else(coal_prd_pt_pc > 0, coalesce(prd,0) + coalesce(pt,0) + coalesce(pc,0) + coal_prd_pt_pc, coal_prd_pt_pc),
      prd = if_else(coal_prd_pt_pc > 0, 0, prd),
      pt  = if_else(coal_prd_pt_pc > 0, 0, pt),
      pc  = if_else(coal_prd_pt_pc > 0, 0, pc)
    ) %>%
    rename(prd_pt_pc = coal_prd_pt_pc)
}

# rename coal_prd_pc -> prd_pc
if("coal_prd_pc" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    rename(prd_pc = coal_prd_pc)
}

# *** Common candidates ***
# replace cc_pan_pvem = cc_pan_pvem + pan + pvem if cc_pan_pvem>0
# zero out pan, pvem => rename cc_pan_pvem -> pan_pvem
if("cc_pan_pvem" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    mutate(
      cc_pan_pvem = if_else(cc_pan_pvem>0, cc_pan_pvem + coalesce(pan,0) + coalesce(pvem,0), cc_pan_pvem),
      pan  = if_else(cc_pan_pvem>0, 0, pan),
      pvem = if_else(cc_pan_pvem>0, 0, pvem)
    ) %>%
    rename(pan_pvem = cc_pan_pvem)
}

# replace cc_pan_pvem_pna => pan_pvem_pna, zero out pan, pvem, pna
if("cc_pan_pvem_pna" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    mutate(
      cc_pan_pvem_pna = if_else(cc_pan_pvem_pna>0, cc_pan_pvem_pna + coalesce(pan,0) + coalesce(pvem,0) + coalesce(pna,0), cc_pan_pvem_pna),
      pan  = if_else(cc_pan_pvem_pna>0, 0, pan),
      pvem = if_else(cc_pan_pvem_pna>0, 0, pvem),
      pna  = if_else(cc_pan_pvem_pna>0, 0, pna)
    ) %>%
    rename(pan_pvem_pna = cc_pan_pvem_pna)
}

# replace cc_pan_pna => pan_pna, zero out pan, pna
if("cc_pan_pna" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    mutate(
      cc_pan_pna = if_else(cc_pan_pna>0, cc_pan_pna + coalesce(pan,0) + coalesce(pna,0), cc_pan_pna),
      pan = if_else(cc_pan_pna>0, 0, pan),
      pna = if_else(cc_pan_pna>0, 0, pna)
    ) %>%
    rename(pan_pna = cc_pan_pna)
}

# replace cc_pan_pri => pan_pri, zero out pan, pri
if("cc_pan_pri" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    mutate(
      # Stata code: replace cc_pan_pri = pan + pri + cc_pan_pri if cc_pan_pri>0
      cc_pan_pri = if_else(cc_pan_pri>0, coalesce(pan,0) + coalesce(pri,0) + cc_pan_pri, cc_pan_pri),
      pan = if_else(cc_pan_pri>0, 0, pan),
      pri = if_else(cc_pan_pri>0, 0, pri)
    ) %>%
    rename(pan_pri = cc_pan_pri)
}

# replace cc_pan_pri_pvem => pan_pri_pvem, zero out pan, pri, pvem
if("cc_pan_pri_pvem" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    mutate(
      cc_pan_pri_pvem = if_else(cc_pan_pri_pvem>0, coalesce(pan,0) + coalesce(pri,0) + coalesce(pvem,0) + cc_pan_pri_pvem, cc_pan_pri_pvem),
      pan = if_else(cc_pan_pri_pvem>0, 0, pan),
      pri = if_else(cc_pan_pri_pvem>0, 0, pri),
      pvem= if_else(cc_pan_pri_pvem>0, 0, pvem)
    ) %>%
    rename(pan_pri_pvem = cc_pan_pri_pvem)
}

# replace cc_pri_pvem => pri_pvem, zero out pri, pvem
if("cc_pri_pvem" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    mutate(
      cc_pri_pvem = if_else(cc_pri_pvem>0, coalesce(pri,0) + coalesce(pvem,0) + cc_pri_pvem, cc_pri_pvem),
      pri= if_else(cc_pri_pvem>0, 0, pri),
      pvem=if_else(cc_pri_pvem>0, 0, pvem)
    ) %>%
    rename(pri_pvem = cc_pri_pvem)
}

# replace cc_pri_pvem_pna => pri_pvem_pna, zero out pri, pvem, pna
if("cc_pri_pvem_pna" %in% names(data_2007)) {
  data_2007 <- data_2007 %>%
    mutate(
      cc_pri_pvem_pna = if_else(cc_pri_pvem_pna>0, coalesce(pri,0)+coalesce(pvem,0)+coalesce(pna,0)+ cc_pri_pvem_pna, cc_pri_pvem_pna),
      pri= if_else(cc_pri_pvem_pna>0, 0, pri),
      pvem=if_else(cc_pri_pvem_pna>0, 0, pvem),
      pna= if_else(cc_pri_pvem_pna>0, 0, pna)
    ) %>%
    rename(pri_pvem_pna = cc_pri_pvem_pna)
}

# -------------------------------------------------------------------
# 7. Final rename of columns
#    rename pan->PAN, pri->PRI, prd->PRD, ...
#    etc. per the code.
# -------------------------------------------------------------------
rename_list <- c(
  "pan"="PAN","pri"="PRI","prd"="PRD","pt"="PT","pvem"="PVEM","pc"="PC",
  "pna"="PANAL","pas"="PAS","prd_pt"="PRD_PT","prd_pt_pc"="PRD_PT_PC","prd_pc"="PRD_PC",
  "pan_pvem"="PAN_PVEM","pan_pvem_pna"="PAN_PVEM_PANAL","pan_pna"="PAN_PANAL",
  "pan_pri"="PAN_PRI","pan_pri_pvem"="PAN_PRI_PVEM","pri_pvem"="PRI_PVEM","pri_pvem_pna"="PRI_PVEM_PANAL"
)

# We'll rename columns if they exist:
for(old_name in names(rename_list)) {
  if(old_name %in% names(data_2007)) {
    new_name <- rename_list[[old_name]]
    data_2007 <- data_2007 %>% rename(!!new_name := !!sym(old_name))
  }
}

# -------------------------------------------------------------------
# 8. gen turnout = total/listanominal
# -------------------------------------------------------------------
data_2007 <- data_2007 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 9. uniqueid assignment
# -------------------------------------------------------------------
data_2007 <- data_2007 %>%
  mutate(uniqueid = 0) %>%
  mutate(uniqueid = case_when(
    municipality=="Acuitzio" ~ 16001,
    municipality=="Aguililla" ~ 16002,
    municipality=="Alvaro Obregón" ~ 16003,
    municipality=="Angamacutiro" ~ 16004,
    municipality=="Angangueo" ~ 16005,
    municipality=="Apatzingán" ~ 16006,
    municipality=="Aporo" ~ 16007,
    municipality=="Aquila" ~ 16008,
    municipality=="Ario" ~ 16009,
    municipality=="Arteaga" ~ 16010,
    municipality=="Briseñas" ~ 16011,
    municipality=="Buenavista" ~ 16012,
    municipality=="Carácuaro" ~ 16013,
    municipality=="Charapan" ~ 16021,
    municipality=="Charo" ~ 16022,
    municipality=="Chavinda" ~ 16023,
    municipality=="Cherán" ~ 16024,
    municipality=="Chilchota" ~ 16025,
    municipality=="Chinicuila" ~ 16026,
    municipality=="Chucándiro" ~ 16027,
    municipality=="Churintzio" ~ 16028,
    municipality=="Churumuco" ~ 16029,
    municipality=="Coahuayana" ~ 16014,
    municipality=="Coalcomán" ~ 16015,
    municipality=="Coeneo" ~ 16016,
    municipality=="Régules" ~ 16074,
    municipality=="Contepec" ~ 16017,
    municipality=="Copándaro" ~ 16018,
    municipality=="Cotija" ~ 16019,
    municipality=="Cuitzeo" ~ 16020,
    municipality=="Ecuandureo" ~ 16030,
    municipality=="Epitacio Huerta" ~ 16031,
    municipality=="Erongarícuaro" ~ 16032,
    municipality=="Gabriel Zamora" ~ 16033,
    municipality=="Hidalgo" ~ 16034,
    municipality=="Huandacareo" ~ 16036,
    municipality=="Huaníqueo" ~ 16037,
    municipality=="Huetamo" ~ 16038,
    municipality=="Huiramba" ~ 16039,
    municipality=="Indaparapeo" ~ 16040,
    municipality=="Irimbo" ~ 16041,
    municipality=="Ixtlán" ~ 16042,
    municipality=="Jacona" ~ 16043,
    municipality=="Jiménez" ~ 16044,
    municipality=="Jiquilpan" ~ 16045,
    municipality=="José Sixto Verduzco" ~ 16113,
    municipality=="Juárez" ~ 16046,
    municipality=="Jungapeo" ~ 16047,
    municipality=="La Huacana" ~ 16035,
    municipality=="La Piedad" ~ 16069,
    municipality=="Lagunillas" ~ 16048,
    municipality=="Lázaro Cárdenas" ~ 16052,
    municipality=="Los Reyes" ~ 16075,
    municipality=="Madero" ~ 16049,
    municipality=="Maravatio" ~ 16050,
    municipality=="Marcos Castellanos" ~ 16051,
    municipality=="Morelia" ~ 16053,
    municipality=="Morelos" ~ 16054,
    municipality=="Múgica" ~ 16055,
    municipality=="Nahuátzen" ~ 16056,
    municipality=="Nocupétaro" ~ 16057,
    municipality=="Nuevo Parangaricutiro" ~ 16058,
    municipality=="Nuevo Urecho" ~ 16059,
    municipality=="Numarán" ~ 16060,
    municipality=="Ocampo" ~ 16061,
    municipality=="Pajacuarán" ~ 16062,
    municipality=="Panindícuaro" ~ 16063,
    municipality=="Paracho" ~ 16065,
    municipality=="Parácuaro" ~ 16064,
    municipality=="Pátzcuaro" ~ 16066,
    municipality=="Penjamillo" ~ 16067,
    municipality=="Peribán" ~ 16068,
    municipality=="Purépero" ~ 16070,
    municipality=="Puruándiro" ~ 16071,
    municipality=="Queréndaro" ~ 16072,
    municipality=="Quiroga" ~ 16073,
    municipality=="Sahuayo" ~ 16076,
    municipality=="Salvador Escalante" ~ 16079,
    municipality=="San Lucas" ~ 16077,
    municipality=="Santa Ana Maya" ~ 16078,
    municipality=="Senguio" ~ 16080,
    municipality=="Susupuato" ~ 16081,
    municipality=="Tacámbaro" ~ 16082,
    municipality=="Tancítaro" ~ 16083,
    municipality=="Tangamandapio" ~ 16084,
    municipality=="Tangancícuaro" ~ 16085,
    municipality=="Tanhuato" ~ 16086,
    municipality=="Taretan" ~ 16087,
    municipality=="Tarímbaro" ~ 16088,
    municipality=="Tepalcatepec" ~ 16089,
    municipality=="Tingambato" ~ 16090,
    municipality=="Tingüindín" ~ 16091,
    municipality=="Tiquicheo" ~ 16092,
    municipality=="Tlalpujahua" ~ 16093,
    municipality=="Tlazazalca" ~ 16094,
    municipality=="Tocumbo" ~ 16095,
    municipality=="Tumbiscatio" ~ 16096,
    municipality=="Turicato" ~ 16097,
    municipality=="Tuxpan" ~ 16098,
    municipality=="Tuzantla" ~ 16099,
    municipality=="Tzintzuntzan" ~ 16100,
    municipality=="Tzitzio" ~ 16101,
    municipality=="Uruapan" ~ 16102,
    municipality=="Venustiano Carranza" ~ 16103,
    municipality=="Villamar" ~ 16104,
    municipality=="Vista Hermosa" ~ 16105,
    municipality=="Yurécuaro" ~ 16106,
    municipality=="Zacapu" ~ 16107,
    municipality=="Zamora" ~ 16108,
    municipality=="Zináparo" ~ 16109,
    municipality=="Zinapécuaro" ~ 16110,
    municipality=="Ziracuaretiro" ~ 16111,
    municipality=="Zitácuaro" ~ 16112,
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 10. Compute valid = rowtotal(...) for a bunch of combos
# -------------------------------------------------------------------
# The code:
#  egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAS PRD_PT ...
# We'll gather them up if they exist:
large_party_list <- c("PAN","PRI","PRD","PT","PVEM","PC",
                      "PANAL","PAS","PRD_PT","PRD_PT_PC","PRD_PC",
                      "PAN_PRI","PAN_PRI_PVEM","PAN_PVEM","PAN_PVEM_PANAL","PAN_PANAL",
                      "PRI_PVEM","PRI_PVEM_PANAL")
large_party_list <- intersect(large_party_list, names(data_2007))

data_2007 <- data_2007 %>%
  mutate(valid = rowSums(across(all_of(large_party_list)), na.rm=TRUE))

# -------------------------------------------------------------------
# 14. gen year=2007, month="November"
# -------------------------------------------------------------------
data_2007 <- data_2007 %>%
  mutate(
    year = 2007,
    month= "November"
  )

# -------------------------------------------------------------------
# 1. IMPORT EXCEL
#    Stata: import excel "resultados_2008.xls", sheet("Ayuntamiento Yurécuaro ") cellrange(A8:L46) firstrow clear
# -------------------------------------------------------------------
data_2008 <- read_excel(
  "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/resultados_2008.xls",
  sheet = "Ayuntamiento Yurécuaro ",    # adjust exact sheet name
  range = "A8:L46",                     # matches the Stata cellrange
  col_names = TRUE                      # firstrow is header
)

# -------------------------------------------------------------------
# 2. RENAME
#    Sección -> section
#    K -> total
# -------------------------------------------------------------------
# We'll assume columns match exactly: "Sección" is named "Sección" and "K" is the 11th column. 
# Check your actual data for these column names.
data_2008 <- data_2008 %>%
  rename(
    section = `Sección`,
    total   = `...11`
  )

# -------------------------------------------------------------------
# 3. CREATE municipality = "Yurécuaro EXTRAORDINARIO", uniqueid=16106
# -------------------------------------------------------------------
data_2008 <- data_2008 %>%
  mutate(
    municipality = "Yurécuaro EXTRAORDINARIO",
    uniqueid     = 16106
  )

# -------------------------------------------------------------------
# 4. DROP if total==. | total==0
# -------------------------------------------------------------------
data_2008 <- data_2008 %>%
  filter(!(is.na(total) | total==0))

# -------------------------------------------------------------------
# 5. collapse (sum) PAN-PT total, by (uniqueid municipality section)
#    Identify columns from PAN to PT plus total
# -------------------------------------------------------------------
# Suppose the columns for party votes are named:
#   "PAN", "PRI", "PRD", "PT", etc.
# If you have different names, adjust accordingly.
all_cols <- names(data_2008)
start_var <- "PAN"
end_var   <- "PT"
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var,   all_cols)

vars_for_collapse <- all_cols[start_pos:end_pos]
vars_for_collapse <- c(vars_for_collapse, "total")  # also include total

group_vars <- c("uniqueid","municipality","section")

data_2008 <- data_2008 %>%
  group_by(across(all_of(group_vars))) %>%
  summarize(across(all_of(vars_for_collapse), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 6. MERGE with "..\..\all_months_years.dta" => keep if ed==16 & month==4 & year==2008
#    rename lista -> listanominal, rename seccion->section
# -------------------------------------------------------------------
months_data <- read_dta("../../all_months_years.dta") %>%
  filter(ed==16, month==4, year==2008) %>%
  rename(listanominal=lista, section=seccion)

# Now restore data_2008 and merge 1:1 on section
# Instead of preserve/restore, we'll just do a left_join in R:
data_2008 <- data_2008 %>%
  left_join(months_data %>% select(section, listanominal), by="section")

# drop if _merge==2 => not needed with left_join
# but we can filter out unmatched rows if listanominal is NA:
data_2008 <- data_2008 %>% filter(!is.na(listanominal))

# -------------------------------------------------------------------
# 7. gen turnout = total/listanominal
# -------------------------------------------------------------------
data_2008 <- data_2008 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 8. egen valid = rowtotal(PAN PRI PRD PT)
# -------------------------------------------------------------------
party_vars <- c("PAN","PRI","PRD","PT")
party_vars <- intersect(party_vars, names(data_2008))

data_2008 <- data_2008 %>%
  mutate(valid = rowSums(across(all_of(party_vars)), na.rm=TRUE))

# -------------------------------------------------------------------
# 9. gen year=2008, month="May"
# -------------------------------------------------------------------
data_2008 <- data_2008 %>%
  mutate(
    year = 2008,
    month= "May"
  )

# -------------------------------------------------------------------
# 1. READ DTA (Stata: use Ayu_Seccion_2011.dta, clear)
# -------------------------------------------------------------------
# Adjust file path to your local environment:
data_2011 <- read_dta("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Other/Ayu_Seccion_2011.dta")
names(data_2011)
# -------------------------------------------------------------------
# 2. REPLACE municipality = proper(municipality)
#    R doesn't have a built-in 'proper()' for strings. We'll do:
#    str_to_title from stringr, or a custom approach.
# -------------------------------------------------------------------
data_2011 <- data_2011 %>%
  mutate(municipality = str_to_title(municipality))

# -------------------------------------------------------------------
# 3. gen turnout = total/listanominal
# -------------------------------------------------------------------
data_2011 <- data_2011 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 4. DROP noreg, nulos if they exist
# -------------------------------------------------------------------
drop_vars <- c("noreg","nulos")
drop_vars <- intersect(drop_vars, names(data_2011))
data_2011 <- data_2011 %>% select(-all_of(drop_vars))

# -------------------------------------------------------------------
# 5. Assign uniqueid by municipality
# -------------------------------------------------------------------
data_2011 <- data_2011 %>%
  mutate(uniqueid=0) %>%
  mutate(uniqueid = case_when(
    municipality=="Acuitzio" ~ 16001,
    municipality=="Aguililla" ~ 16002,
    municipality=="Alvaro Obregon" ~ 16003,
    municipality=="Angamacutiro" ~ 16004,
    municipality=="Angangueo" ~ 16005,
    municipality=="Apatzingan" ~ 16006,
    municipality=="Aporo" ~ 16007,
    municipality=="Aquila" ~ 16008,
    municipality=="Ario" ~ 16009,
    municipality=="Arteaga" ~ 16010,
    municipality=="Brisenas" ~ 16011,
    municipality=="Buenavista" ~ 16012,
    municipality=="Caracuaro" ~ 16013,
    municipality=="Charapan" ~ 16021,
    municipality=="Charo" ~ 16022,
    municipality=="Chavinda" ~ 16023,
    municipality=="Cheran" ~ 16024,
    municipality=="Chilchota" ~ 16025,
    municipality=="Chinicuila" ~ 16026,
    municipality=="Chucandiro" ~ 16027,
    municipality=="Churintzio" ~ 16028,
    municipality=="Churumuco" ~ 16029,
    municipality=="Coahuayana" ~ 16014,
    municipality=="Coalcoman" ~ 16015,
    municipality=="Coeneo" ~ 16016,
    municipality=="Regules" ~ 16074,
    municipality=="Contepec" ~ 16017,
    municipality=="Copandaro" ~ 16018,
    municipality=="Cotija" ~ 16019,
    municipality=="Cuitzeo" ~ 16020,
    municipality=="Ecuandureo" ~ 16030,
    municipality=="Epitacio Huerta" ~ 16031,
    municipality=="Erongaricuaro" ~ 16032,
    municipality=="Gabriel Zamora" ~ 16033,
    municipality=="Hidalgo" ~ 16034,
    municipality=="Huandacareo" ~ 16036,
    municipality=="Huaniqueo" ~ 16037,
    municipality=="Huetamo" ~ 16038,
    municipality=="Huiramba" ~ 16039,
    municipality=="Indaparapeo" ~ 16040,
    municipality=="Irimbo" ~ 16041,
    municipality=="Ixtlan" ~ 16042,
    municipality=="Jacona" ~ 16043,
    municipality=="Jimenez" ~ 16044,
    municipality=="Jiquilpan" ~ 16045,
    municipality=="Jose Sixto Verduzco" ~ 16113,
    municipality=="Juarez" ~ 16046,
    municipality=="Jungapeo" ~ 16047,
    municipality=="La Huacana" ~ 16035,
    municipality=="La Piedad" ~ 16069,
    municipality=="Lagunillas" ~ 16048,
    municipality=="Lazaro Cardenas" ~ 16052,
    municipality=="Los Reyes" ~ 16075,
    municipality=="Madero" ~ 16049,
    municipality=="Maravatio" ~ 16050,
    municipality=="Marcos Castellanos" ~ 16051,
    municipality=="Morelia" ~ 16053,
    municipality=="Morelos" ~ 16054,
    municipality=="Mugica" ~ 16055,
    municipality=="Nahuatzen" ~ 16056,
    municipality=="Nocupetaro" ~ 16057,
    municipality=="Nuevo Parangaricutiro" ~ 16058,
    municipality=="Nuevo Urecho" ~ 16059,
    municipality=="Numaran" ~ 16060,
    municipality=="Ocampo" ~ 16061,
    municipality=="Pajacuaran" ~ 16062,
    municipality=="Panindicuaro" ~ 16063,
    municipality=="Paracho" ~ 16065,
    municipality=="Paracuaro" ~ 16064,
    municipality=="Patzcuaro" ~ 16066,
    municipality=="Penjamillo" ~ 16067,
    municipality=="Periban" ~ 16068,
    municipality=="Purepero" ~ 16070,
    municipality=="Puruandiro" ~ 16071,
    municipality=="Querendaro" ~ 16072,
    municipality=="Quiroga" ~ 16073,
    municipality=="Sahuayo" ~ 16076,
    municipality=="Salvador Escalante" ~ 16079,
    municipality=="San Lucas" ~ 16077,
    municipality=="Santa Ana Maya" ~ 16078,
    municipality=="Senguio" ~ 16080,
    municipality=="Susupuato" ~ 16081,
    municipality=="Tacambaro" ~ 16082,
    municipality=="Tancitaro" ~ 16083,
    municipality=="Tangamandapio" ~ 16084,
    municipality=="Tangancicuaro" ~ 16085,
    municipality=="Tanhuato" ~ 16086,
    municipality=="Taretan" ~ 16087,
    municipality=="Tarimbaro" ~ 16088,
    municipality=="Tepalcatepec" ~ 16089,
    municipality=="Tingambato" ~ 16090,
    municipality=="Tinguindin" ~ 16091,
    municipality=="Tiquicheo" ~ 16092,
    municipality=="Tlalpujahua" ~ 16093,
    municipality=="Tlazazalca" ~ 16094,
    municipality=="Tocumbo" ~ 16095,
    municipality=="Tumbiscatio" ~ 16096,
    municipality=="Turicato" ~ 16097,
    municipality=="Tuxpan" ~ 16098,
    municipality=="Tuzantla" ~ 16099,
    municipality=="Tzintzuntzan" ~ 16100,
    municipality=="Tzitzio" ~ 16101,
    municipality=="Uruapan" ~ 16102,
    municipality=="Venustiano Carranza" ~ 16103,
    municipality=="Villamar" ~ 16104,
    municipality=="Vista Hermosa" ~ 16105,
    municipality=="Yurecuaro" ~ 16106,
    municipality=="Zacapu" ~ 16107,
    municipality=="Zamora" ~ 16108,
    municipality=="Zinaparo" ~ 16109,
    municipality=="Zinapecuaro" ~ 16110,
    municipality=="Ziracuaretiro" ~ 16111,
    municipality=="Zitacuaro" ~ 16112,
    TRUE ~ uniqueid
  ))

# -------------------------------------------------------------------
# 6. sum PRD_PT PRD PT if C_PRD_PT>0 & C_PRD_PT!=. 
#    Then replace PRD_PT = C_PRD_PT if conditions => drop C_PRD_PT
# -------------------------------------------------------------------
# We'll assume 'C_PRD_PT' might be a column. 
# Check if it exists:
if("C_PRD_PT" %in% names(data_2011)) {
  # sum command in Stata is just to inspect. 
  # We'll replicate logic: replace PRD_PT with C_PRD_PT if it's non-zero:
  data_2011 <- data_2011 %>%
    mutate(
      PRD_PT = if_else(!is.na(C_PRD_PT) & C_PRD_PT!=0 & PRD_PT==0, C_PRD_PT, PRD_PT)
    ) %>%
    select(-C_PRD_PT) # drop it
}

# -------------------------------------------------------------------
# 7. For each var in [PAN_PRI_PVEM, PAN_PRI_PVEM_PANAL, ... PT_PC], 
#    sum them by uniqueid => c_var => c_var=(c_var>0). 
#    Then if c_var==1 => coalition merges columns
# -------------------------------------------------------------------
coal_vars <- c("PAN_PRI_PVEM","PAN_PRI_PVEM_PANAL","PAN_PRI_PANAL","PAN_PANAL","PRI_PRD_PVEM_PC_PANAL","PRI_PVEM","PRD_PT","PRD_PT_PC","PRD_PC","PT_PC")

coal_vars <- intersect(coal_vars, names(data_2011))

#  (1) bys uniqueid: egen c_`var' = sum(`var')
#      replace c_`var'=(c_`var'>0)
for(cv in coal_vars) {
  new_name <- paste0("c_", cv)
  # sum by uniqueid
  data_2011 <- data_2011 %>%
    group_by(uniqueid) %>%
    mutate("{new_name}" := sum(.data[[cv]], na.rm=TRUE)) %>%
    ungroup() %>%
    mutate("{new_name}" := if_else(.data[[new_name]]>0, 1, 0))
}

# Now we do the merges, if c_var==1 => coalition merges with base columns, zeroing them out
# We'll define a helper function to add and zero-out certain parties:
merge_coal <- function(df, coalition_col, c_coal_col, base_parties) {
  if(all(c(coalition_col, c_coal_col, base_parties) %in% names(df))) {
    df <- df %>%
      mutate(
        !!sym(coalition_col) := if_else(.data[[c_coal_col]]==1, 
                                        rowSums(across(all_of(c(coalition_col, base_parties))), na.rm=TRUE),
                                        .data[[coalition_col]]
        )
      )
    # zero out the base parties
    for(bp in base_parties) {
      df <- df %>%
        mutate(
          !!sym(bp) := if_else(.data[[c_coal_col]]==1, 0, .data[[bp]])
        )
    }
  }
  df
}

# replicate the logic:
data_2011 <- merge_coal(data_2011, "PAN_PRI_PVEM",       "c_PAN_PRI_PVEM",       c("PAN","PRI","PVEM"))
data_2011 <- merge_coal(data_2011, "PAN_PRI_PVEM_PANAL", "c_PAN_PRI_PVEM_PANAL", c("PAN","PRI","PVEM","PANAL"))
data_2011 <- merge_coal(data_2011, "PAN_PRI_PANAL",      "c_PAN_PRI_PANAL",      c("PAN","PRI","PANAL"))
data_2011 <- merge_coal(data_2011, "PAN_PANAL",          "c_PAN_PANAL",          c("PAN","PANAL"))
data_2011 <- merge_coal(data_2011, "PRI_PRD_PVEM_PC_PANAL", "c_PRI_PRD_PVEM_PC_PANAL", c("PRI","PRD","PVEM","PC","PANAL"))
data_2011 <- merge_coal(data_2011, "PRI_PVEM",           "c_PRI_PVEM",           c("PRI","PVEM"))
data_2011 <- merge_coal(data_2011, "PRD_PT",             "c_PRD_PT",             c("PRD","PT"))
data_2011 <- merge_coal(data_2011, "PRD_PT_PC",          "c_PRD_PT_PC",          c("PRD","PT","PC"))
data_2011 <- merge_coal(data_2011, "PRD_PC",            "c_PRD_PC",             c("PRD","PC"))
data_2011 <- merge_coal(data_2011, "PT_PC",             "c_PT_PC",              c("PT","PC"))

# drop c_*
c_vars_drop <- grep("^c_", names(data_2011), value=TRUE)
data_2011 <- data_2011 %>% select(-all_of(c_vars_drop))

# -------------------------------------------------------------------
# 8. Now recalc 'valid'
#    egen valid = rowtotal(...) for big list
# -------------------------------------------------------------------
party_list2 <- c("PAN","PRI","PRD","PT","PVEM","PC","PANAL",
                 "PAN_PRI_PVEM","PAN_PRI_PVEM_PANAL","PAN_PRI_PANAL","PAN_PANAL",
                 "PRI_PRD_PVEM_PC_PANAL","PRI_PVEM","PRD_PT","PRD_PT_PC","PRD_PC","PT_PC")
party_list2 <- intersect(party_list2, names(data_2011))

data_2011 <- data_2011 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(all_of(party_list2)), na.rm=TRUE)) %>%
  ungroup()

# -------------------------------------------------------------------
# 12. gen year=2011, month="November"
# -------------------------------------------------------------------
data_2011 <- data_2011 %>%
  mutate(
    year = 2011,
    month= "November"
  )

# -------------------------------------------------------------------
# 1. IMPORT EXCEL
#    Stata: import excel "Resultados_electorales_extraordinaria_2012.xlsx", sheet("morelia2012_bd") cellrange(A8:S904) firstrow clear
# -------------------------------------------------------------------
data_2012 <- read_excel(
  "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Resultados_electorales_extraordinaria_2012.xlsx",
  sheet = "morelia2012_bd",     # adjust if needed
  range = "A8:S904",            # matches Stata cellrange
  col_names = TRUE              # firstrow is header
)
names(data_2012) <- gsub("[ ]", "", names(data_2012))
# -------------------------------------------------------------------
# 2. RENAME columns
#    SECCIÓN -> section, TOTALVOTOS -> total
# -------------------------------------------------------------------
data_2012 <- data_2012 %>%
  rename(
    section = SECCIÓN,
    total   = TOTALVOTOS
  )

# drop if section==.
data_2012 <- data_2012 %>%
  filter(!is.na(section))

# -------------------------------------------------------------------
# 3. CREATE municipality="MORELIA EXTRAORDINARIO", uniqueid=16053
#    drop if total==. or total==0
# -------------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(
    municipality = "MORELIA EXTRAORDINARIO",
    uniqueid     = 16053
  ) %>%
  filter(!(is.na(total) | total==0))

# -------------------------------------------------------------------
# 4. GENERATE coalition columns:
#    g PAN_PANAL = PAN + PNA + CCPANPNA
#    g PRD_PT_PMC = PRD + PT + PMC + CCPRDPTPMC
#    rename CCM -> PRI_PVEM
# -------------------------------------------------------------------
# We'll assume columns PAN, PNA, CCPANPNA, PRD, PT, PMC, CCPRDPTPMC, CCM exist in the data
# If your data has different names, adjust accordingly.

if(all(c("PAN","PNA","CCPANPNA") %in% names(data_2012))) {
  data_2012 <- data_2012 %>%
    mutate(PAN_PANAL = coalesce(PAN,0) + coalesce(PNA,0) + coalesce(CCPANPNA,0))
}

if(all(c("PRD","PT","PMC","CCPRDPTPMC") %in% names(data_2012))) {
  data_2012 <- data_2012 %>%
    mutate(PRD_PT_PMC = coalesce(PRD,0) + coalesce(PT,0) + coalesce(PMC,0) + coalesce(CCPRDPTPMC,0))
}

# rename CCM -> PRI_PVEM
if("CCM" %in% names(data_2012)) {
  data_2012 <- data_2012 %>%
    rename(PRI_PVEM = CCM)
}

# -------------------------------------------------------------------
# 5. collapse (sum) PAN_PANAL PRD_PT_PMC PRI_PVEM total, 
#    by (uniqueid municipality section)
# -------------------------------------------------------------------
vars_to_collapse <- c("PAN_PANAL","PRD_PT_PMC","PRI_PVEM","total")
vars_to_collapse <- intersect(vars_to_collapse, names(data_2012))

group_vars <- c("uniqueid","municipality","section")

data_2012 <- data_2012 %>%
  group_by(across(all_of(group_vars))) %>%
  summarize(across(all_of(vars_to_collapse), sum, na.rm=TRUE), .groups="drop")

# -------------------------------------------------------------------
# 6. MERGE with "..\..\all_months_years.dta"
#    keep if ed==16 & fecha=="01/07/2012" & year==2012
#    rename lista->listanominal, rename seccion->section
#    then merge 1:1 section using
# -------------------------------------------------------------------
all_months <- read_dta("../../all_months_years.dta") %>%
  filter(ed==16, fecha=="01/07/2012", year==2012) %>%
  rename(listanominal=lista, section=seccion)

# in R, we "restore" data_2012 and left_join
data_2012 <- data_2012 %>%
  left_join(all_months %>% select(section, listanominal), by="section")

# drop if _merge==2 => in left_join we can filter out unmatched
data_2012 <- data_2012 %>% filter(!is.na(listanominal))

# -------------------------------------------------------------------
# 7. gen turnout = total/listanominal
# -------------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(turnout = total / listanominal)

# -------------------------------------------------------------------
# 8. egen valid = rowtotal(PAN_PANAL PRD_PT_PMC PRI_PVEM)
# -------------------------------------------------------------------
party_vars <- c("PAN_PANAL","PRD_PT_PMC","PRI_PVEM")
party_vars <- intersect(party_vars, names(data_2012))

data_2012 <- data_2012 %>%
  mutate(valid = rowSums(across(all_of(party_vars)), na.rm=TRUE))


# -------------------------------------------------------------------
# 9. gen year=2012, month="July"
# -------------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(
    year = 2012,
    month= "July"
  )

###############################################################################
### PART 1. CREATE coalitions.dta FROM "computos_municipales_2015.xlsx"
###############################################################################
message("=== Creating coalitions.dta from computos_municipales_2015.xlsx ===")

# 1.1 Read Excel
coal_raw <- read_excel(
  "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/computos_municipales_2015.xlsx",
  sheet = "Hoja1",
  range = "A3:BY115",
  col_names = TRUE
)

# 1.2 Keep only columns D plus AW through BT
#   In Stata, `keep D AW-BT`.
#   We'll see the actual column names from R:
colnames(coal_raw)
# Suppose column "D" is literally named "D", 
# and columns AW-BT are also named "AW","AX",...,"BT".
# We'll identify them by position or by name.

# We find col "D" by name, and AW..BT by name. Adjust if your actual spreadsheet differs.
keep_cols <- c("D", paste0(LETTERS[1:2], LETTERS[23:26])) # This won't match automatically!
# Actually, let's do something simpler: pick column "D" plus columns AW..BT if they exist:
all_cols <- names(coal_raw)
# We'll find the index of "D"
idx_D <- match("D", all_cols)
# We'll find AW..BT in the columns
start_idx <- match("AW", all_cols)
end_idx   <- match("BT", all_cols)
if(is.na(idx_D) || is.na(start_idx) || is.na(end_idx)) {
  stop("Columns D or AW..BT not found in the data. Adjust code or check spreadsheet structure.")
}
subset_cols <- c("D", all_cols[start_idx:end_idx])

coal_part <- coal_raw[ , subset_cols]

# 1.3 For each varlist AW-BT, replace "-" with "", then destring, then `x' = (x != .) => means boolean
for(x in names(coal_part)[-1]) {  # -1 to skip "D"
  # replace `x'="" if `x'=="-"
  coal_part[[x]] <- if_else(coal_part[[x]]=="-", "", coal_part[[x]])
  # then convert to numeric
  tmp_num <- suppressWarnings(as.numeric(coal_part[[x]]))
  # if not NA => 1, else 0
  # The Stata code 'replace `x' = `x'!=.' means "if x is not missing =>1 else 0"
  # We do that in R:
  tmp_bool <- if_else(!is.na(tmp_num), 1, 0)
  coal_part[[x]] <- tmp_bool
}

# 1.4 rename (AW-BT) => (PAN_PRI_PRD_PANAL_PH_PES ...), then "rename coal_D MUNICIPIO"
# The code: 
# rename (AW-BT) (PAN_PRI_PRD_PANAL_PH_PES PAN_PRD_PT_PANAL_PH ... ) => We'll define them in a vector
new_names <- c(
  "PAN_PRI_PRD_PANAL_PH_PES","PAN_PRD_PT_PANAL_PH","PAN_PRD_PT_PANAL_PES",
  "PRD_PT_PANAL_PH","PRD_PT_PANAL_PES","PAN_PRI_PVEM","PAN_PRD_PT","PRD_PT_PANAL",
  "PRD_PT_PH","PRD_PT_PES","PRD_PANAL_PES","PT_PANAL_PH","PT_PES_PH","PAN_PRD",
  "PAN_PT","PAN_MC","PAN_PH","PRI_PVEM","PRD_PT","PRD_PANAL","PRD_PES","PT_MC",
  "PT_PH","PT_PES"
)
old_names <- names(coal_part)[2:(length(new_names)+1)]  # skip first col "D"

# rename them
names(coal_part)[2:(length(new_names)+1)] <- new_names

# rename "D" -> "MUNICIPIO" => or "coal_D" -> "MUNICIPIO" if we had "rename coal_D MUNICIPIO"
names(coal_part)[1] <- "MUNICIPIO"

# 1.5 fix municipality name accents
coal_part <- coal_part %>%
  mutate(MUNICIPIO = str_replace_all(MUNICIPIO, "Á", "A"),
         MUNICIPIO = str_replace_all(MUNICIPIO, "É", "E"),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Í", "I"),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ó", "O"),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ú", "U"),
         MUNICIPIO = case_when(
           MUNICIPIO=="COALCOMAN" ~ "COALCOMAN DE VAZQUEZ PALLARES",
           MUNICIPIO=="TIQUICHEO" ~ "TIQUICHEO DE NICOLAS ROMERO",
           MUNICIPIO=="REGULES"   ~ "COJUMATLAN DE REGULES",
           TRUE ~ MUNICIPIO
         ))

###############################################################################
### PART 2. PROCESS "Ayuntamientos_Mich_2015.xlsx" => Save Each Sheet as .dta
###############################################################################

message("=== Reading sheets from Ayuntamientos_Mich_2015.xlsx and saving each as .dta ===")

# 2.1 We'll discover sheet names in R:
sheets_info <- excel_sheets("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Ayuntamientos_Mich_2015.xlsx")
# For each sheet, we read, drop if CVO=="TOTAL" or "", replace blanks with "0", save as "<sheet>.dta".
for(sheetname in sheets_info) {
  message(paste("Processing sheet:", sheetname))
  temp_data <- read_excel("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Ayuntamientos_Mich_2015.xlsx", sheet=sheetname, col_names=TRUE)
  
  # drop if CVO=="TOTAL" or CVO=="" => We'll assume column named "CVO"
  if(! "CVO" %in% names(temp_data)) {
    message("Sheet", sheetname, "has no CVO column, skipping.")
    next
  }
  temp_data <- temp_data %>%
    filter(!(CVO=="TOTAL" | CVO==""))
  
  # replace blanks "" with "0" for all columns
  for(x in names(temp_data)) {
    temp_data[[x]] <- if_else(temp_data[[x]]=="", "0", temp_data[[x]])
  }
  
  # save "<sheetname>.dta"
  # In R, let's sanitize the sheetname for a safe filename:
  file_out <- paste0(gsub("[^A-Za-z0-9_]", "", sheetname), ".dta")
  write_dta(temp_data, file_out)
}

###############################################################################
### PART 3. CLEAR & APPEND MULTIPLE .dta FILES
###############################################################################
###############################################################################
### PART X: CLEAR & APPEND MULTIPLE .dta FILES (equivalent to multiple `append using` in Stata)
###############################################################################

# 1) List all the .dta files you want to append (in the same order as in Stata).
files_to_append <- c(
  "LAGUNILLAS.dta", "ARTEAGA.dta", "ZIRACUARETIRO.dta", "SUSUPUATO.dta", "TZINTZUNTZAN.dta",
  "ERONGARICUARO.dta", "TINGAMBATO.dta", "TEPALCATEPEC.dta", "JUAREZ.dta", "Regules .dta",
  "TUZANTLA.dta", "CHURINTZIO.dta", "CHURUMUCO.dta", "Periban.dta", "CHINICUILA.dta",
  "JUNGAPEO.dta", "Tanhuato.dta", "CHUCANDIRO.dta", "Yurecuaro.dta", "Marcos Castellanos.dta",
  "Tangamandapio.dta", "TLAZAZALCA.dta", "INDAPARAPEO.dta", "CHARO.dta", "ANGAMACUTIRO.dta",
  "APORO.dta", "Ixtlan.dta", "QUERENDARO.dta", "SAN LUCAS.dta", "IRIMBO.dta",
  "Copandaro.dta", "La Huacana.dta", "Huandacareo.dta", "La Piedad.dta", "LOS REYES.dta",
  "Epitacio huerta.dta", "TZITZIO.dta", "BRISEÑAS.dta", "Senguio.dta", "HUIRAMBA.dta",
  "Jiquilpan.dta", "MORELIA.dta", "Sahuayo.dta", "Venustiano Carranza.dta", "Vista Hermosa.dta",
  "Tlalpujahua.dta", "Pajacuaran.dta", "Chavinda.dta", "Jacona.dta", "TANGANCICUARO.dta",
  "COENEO.dta", "Villamar.dta", "BUENAVISTA.dta", "Zamora.dta", "HUANIQUEO.dta",
  "Jimenez.dta", "APATZINGAN.dta", "PUREPERO.dta", "Zacapu.dta", "OBREGON.dta",
  "CHARAPAN.dta", "CUITZEO.dta", "TARIMBARO.dta", "ZINAPECUARO.dta", "COTIJA.dta",
  "TANCITARO.dta", "TOCUMBO.dta", "NUEVO URECHO.dta", "ANGANGUEO.dta", "OCAMPO.dta",
  "NOCUPETARO.dta", "PATZCUARO.dta", "URUAPAN.dta", "MUGICA.dta", "NVO PARANGARICUTIRO.dta",
  "AGUILILLA.dta", "CARACUARO.dta", "SALVADOR ESCALANTE.dta", "ROMERO.dta", "QUIROGA.dta",
  "Morelos.dta", "HIDALGO.dta", "Ario.dta", "Aquila.dta", "Chilchota.dta",
  "ACUITZIO.dta", "ZITACUARO.dta", "COAHUAYANA.dta", "MADERO.dta", "PARACHO.dta",
  "TUXPAN.dta", "TACAMBARO.dta", "COALCOMAN.dta", "Cardenas.dta", "HUETAMO.dta",
  "ECUANDUREO.dta", "Nahuatzen.dta", "Jose Sixto Verduzco.dta", "Puruandiro.dta",
  "MARAVATIO.dta", "Paracuaro.dta", "Tumbiscatio.dta", "Numaran.dta", "TARETAN.dta",
  "TINGÜINDIN.dta", "SANTA ANA MAYA.dta", "Zinaparo.dta", "PANINDICUARO.dta", "CONTEPEC.dta",
  "GABRIEL ZAMORA.dta", "TURICATO.dta"
)

# 2) We'll create an empty object and then sequentially read & append each .dta file.
all_data <- NULL

# 3) Loop over each .dta file, read it with read_dta(), and bind it to `all_data`.
for(fname in files_to_append) {
  # Check if file actually exists before reading:
  if(!file.exists(fname)) {
    message("File not found, skipping: ", fname)
    next
  }
  
  message("Appending file: ", fname)
  temp_df <- haven::read_dta(fname)
  
  # If `all_data` is empty, initialize it with the first file.
  if(is.null(all_data)) {
    all_data <- temp_df
  } else {
    # Otherwise, "append" by binding rows
    all_data <- dplyr::bind_rows(all_data, temp_df)
  }
}

# 4) At this point, `all_data` is the combined dataset that in Stata you would have
# after all those "append using" lines. 
# You can now do further cleaning, merges, etc.

message("=== Done appending multiple .dta files in R. ===")

# You can save the combined data if you like:
# haven::write_dta(all_data, "All_Municipalities_2015.dta")

###############################################################################
### PART 4. drop W R V T U Q
###############################################################################
drop_vars2 <- c("W","R","V","T","U","Q")
drop_vars2 <- intersect(drop_vars2, names(all_data))
all_data <- all_data %>% select(-all_of(drop_vars2))

# destring *, replace => convert everything to numeric if possible
# Stata: destring * => in R, let's do across all columns
for(x in names(all_data)) {
  # attempt conversion
  all_data[[x]] <- suppressWarnings(as.numeric(as.character(all_data[[x]])))
}

# drop if MUNICIPIO==""
all_data <- all_data %>%
  filter(!is.na(MUNICIPIO) & MUNICIPIO!="")

###############################################################################
### PART 5. MERGE m:1 MUNICIPIO using coal_part
###############################################################################
# In R, we do a many-to-one or left_join if we want to keep all in 'all_data' 
all_data <- all_data %>%
  left_join(coal_part, by=c("MUNICIPIO"))

# drop if _merge==2 => in R, we can remove if any column from 'coalitions' is NA
# for example if "PAN_PRI_PRD_PANAL_PH_PES" is NA for all rows => we can check one
any_coal_col <- setdiff(names(coalitions), "MUNICIPIO")[1] 
all_data <- all_data %>% filter(!is.na(.data[[any_coal_col]]))

###############################################################################
### PART 6. rename MUNICIPIO->municipality, rename SECCIÓN->section (if needed)
###############################################################################
# from your code:
all_data <- all_data %>%
  rename(
    municipality = MUNICIPIO,
    section      = SECCIÓN # if you have a column named "SECCIÓN"
  )

# "rename SECCIÓN section" => we do so if it exists
if("SECCIÓN" %in% names(all_data)) {
  all_data <- all_data %>%
    rename(section = SECCIÓN)
}

# drop if PAN=="N/I" => in R we do 
if("PAN" %in% names(all_data)) {
  all_data <- all_data %>% filter(!(PAN=="N/I"))
  # then destring PAN
  all_data$PAN <- suppressWarnings(as.numeric(all_data$PAN))
}

###############################################################################
### PART 7. Coalition merges
###############################################################################

all_data$PAN  [all_data$coal_PAN_PRI_PRD_PANAL_PH_PES == 1] <- NA
all_data$PRI  [all_data$coal_PAN_PRI_PRD_PANAL_PH_PES == 1] <- NA
all_data$PRD  [all_data$coal_PAN_PRI_PRD_PANAL_PH_PES == 1] <- NA
all_data$PANAL[all_data$coal_PAN_PRI_PRD_PANAL_PH_PES == 1] <- NA
all_data$PH   [all_data$coal_PAN_PRI_PRD_PANAL_PH_PES == 1] <- NA
all_data$PES  [all_data$coal_PAN_PRI_PRD_PANAL_PH_PES == 1] <- NA

all_data$coal_PAN_PRI_PRD_PANAL_PH_PES <- NULL  # drop column

###############################################################################
# 2) replace PAN=. if coal_PAN_PRD_PT_PANAL_PH==1 
#    ...
#    drop coal_PAN_PRD_PT_PANAL_PH
###############################################################################
all_data$PAN  [all_data$coal_PAN_PRD_PT_PANAL_PH == 1] <- NA
all_data$PRD  [all_data$coal_PAN_PRD_PT_PANAL_PH == 1] <- NA
all_data$PT   [all_data$coal_PAN_PRD_PT_PANAL_PH == 1] <- NA
all_data$PES  [all_data$coal_PAN_PRD_PT_PANAL_PH == 1] <- NA  # matches your snippet
all_data$PANAL[all_data$coal_PAN_PRD_PT_PANAL_PH == 1] <- NA

all_data$coal_PAN_PRD_PT_PANAL_PH <- NULL

###############################################################################
# 3) replace PAN=. if coal_PAN_PRD_PT_PANAL_PES==1
#    ...
#    drop coal_PAN_PRD_PT_PANAL_PES
###############################################################################
all_data$PAN  [all_data$coal_PAN_PRD_PT_PANAL_PES == 1] <- NA
all_data$PRD  [all_data$coal_PAN_PRD_PT_PANAL_PES == 1] <- NA
all_data$PT   [all_data$coal_PAN_PRD_PT_PANAL_PES == 1] <- NA
all_data$PES  [all_data$coal_PAN_PRD_PT_PANAL_PES == 1] <- NA
all_data$PANAL[all_data$coal_PAN_PRD_PT_PANAL_PES == 1] <- NA

all_data$coal_PAN_PRD_PT_PANAL_PES <- NULL

###############################################################################
# 4) replace PRD=. if coal_PRD_PT_PANAL_PH==1
#    ...
#    drop coal_PRD_PT_PANAL_PH
###############################################################################
all_data$PRD   [all_data$coal_PRD_PT_PANAL_PH == 1] <- NA
all_data$PT    [all_data$coal_PRD_PT_PANAL_PH == 1] <- NA
all_data$PH    [all_data$coal_PRD_PT_PANAL_PH == 1] <- NA
all_data$PANAL [all_data$coal_PRD_PT_PANAL_PH == 1] <- NA

all_data$coal_PRD_PT_PANAL_PH <- NULL

###############################################################################
# 5) replace PRD=. if coal_PRD_PT_PANAL_PES==1
#    ...
#    drop coal_PRD_PT_PANAL_PES
###############################################################################
all_data$PRD   [all_data$coal_PRD_PT_PANAL_PES == 1] <- NA
all_data$PANAL [all_data$coal_PRD_PT_PANAL_PES == 1] <- NA
all_data$PES   [all_data$coal_PRD_PT_PANAL_PES == 1] <- NA
all_data$PT    [all_data$coal_PRD_PT_PANAL_PES == 1] <- NA

all_data$coal_PRD_PT_PANAL_PES <- NULL

###############################################################################
# 6) replace PAN=. if coal_PAN_PRI_PVEM==1
#    ...
#    drop coal_PAN_PRI_PVEM
###############################################################################
all_data$PAN [all_data$coal_PAN_PRI_PVEM == 1] <- NA
all_data$PRI [all_data$coal_PAN_PRI_PVEM == 1] <- NA
all_data$PVEM[all_data$coal_PAN_PRI_PVEM == 1] <- NA

all_data$coal_PAN_PRI_PVEM <- NULL

###############################################################################
# 7) replace PAN=. if coal_PAN_PRD_PT==1
#    ...
#    drop coal_PAN_PRD_PT
###############################################################################
all_data$PAN[all_data$coal_PAN_PRD_PT == 1] <- NA
all_data$PRD[all_data$coal_PAN_PRD_PT == 1] <- NA
all_data$PT [all_data$coal_PAN_PRD_PT == 1] <- NA

all_data$coal_PAN_PRD_PT <- NULL

###############################################################################
# 8) replace PRD=. if coal_PRD_PT_PANAL==1
#    ...
#    drop coal_PRD_PT_PANAL
###############################################################################
all_data$PRD   [all_data$coal_PRD_PT_PANAL == 1] <- NA
all_data$PT    [all_data$coal_PRD_PT_PANAL == 1] <- NA
all_data$PANAL [all_data$coal_PRD_PT_PANAL == 1] <- NA

all_data$coal_PRD_PT_PANAL <- NULL

###############################################################################
# 9) replace PRD=. if coal_PRD_PT_PH==1
#    ...
#    drop coal_PRD_PT_PH
###############################################################################
all_data$PRD[all_data$coal_PRD_PT_PH == 1] <- NA
all_data$PT [all_data$coal_PRD_PT_PH == 1] <- NA
all_data$PH [all_data$coal_PRD_PT_PH == 1] <- NA

all_data$coal_PRD_PT_PH <- NULL

###############################################################################
# 10) replace PRD=. if coal_PRD_PT_PES==1
#     ...
#     drop coal_PRD_PT_PES
###############################################################################
all_data$PRD[all_data$coal_PRD_PT_PES == 1] <- NA
all_data$PT [all_data$coal_PRD_PT_PES == 1] <- NA
all_data$PES[all_data$coal_PRD_PT_PES == 1] <- NA

all_data$coal_PRD_PT_PES <- NULL

###############################################################################
# 11) replace PRD=. if coal_PRD_PANAL_PES==1
#     ...
#     drop coal_PRD_PANAL_PES
###############################################################################
all_data$PRD   [all_data$coal_PRD_PANAL_PES == 1] <- NA
all_data$PANAL [all_data$coal_PRD_PANAL_PES == 1] <- NA
all_data$PES   [all_data$coal_PRD_PANAL_PES == 1] <- NA

all_data$coal_PRD_PANAL_PES <- NULL

###############################################################################
# 12) replace PT=. if coal_PT_PANAL_PH==1
#     ...
#     drop coal_PT_PANAL_PH
###############################################################################
all_data$PT   [all_data$coal_PT_PANAL_PH == 1] <- NA
all_data$PANAL[all_data$coal_PT_PANAL_PH == 1] <- NA
all_data$PH   [all_data$coal_PT_PANAL_PH == 1] <- NA

all_data$coal_PT_PANAL_PH <- NULL

###############################################################################
# 13) replace PT=. if coal_PT_PES_PH==1
#     ...
#     drop coal_PT_PES_PH
###############################################################################
all_data$PT [all_data$coal_PT_PES_PH == 1] <- NA
all_data$PES[all_data$coal_PT_PES_PH == 1] <- NA
all_data$PH [all_data$coal_PT_PES_PH == 1] <- NA

all_data$coal_PT_PES_PH <- NULL

###############################################################################
# 14) replace PAN=. if coal_PAN_PRD==1
#     ...
#     drop coal_PAN_PRD
###############################################################################
all_data$PAN[all_data$coal_PAN_PRD == 1] <- NA
all_data$PRD[all_data$coal_PAN_PRD == 1] <- NA

all_data$coal_PAN_PRD <- NULL

###############################################################################
# 15) replace PAN=. if coal_PAN_PT==1
#     ...
#     drop coal_PAN_PT
###############################################################################
all_data$PAN[all_data$coal_PAN_PT == 1] <- NA
all_data$PT [all_data$coal_PAN_PT == 1] <- NA

all_data$coal_PAN_PT <- NULL

###############################################################################
# 16) replace PAN=. if coal_PAN_MC==1
#     ...
#     drop coal_PAN_MC
###############################################################################
all_data$PAN[all_data$coal_PAN_MC == 1] <- NA
all_data$MC [all_data$coal_PAN_MC == 1] <- NA

all_data$coal_PAN_MC <- NULL

###############################################################################
# 17) replace PAN=. if coal_PAN_PH==1
#     ...
#     drop coal_PAN_PH
###############################################################################
all_data$PAN[all_data$coal_PAN_PH == 1] <- NA
all_data$PH [all_data$coal_PAN_PH == 1] <- NA

all_data$coal_PAN_PH <- NULL

###############################################################################
# 18) replace PRI=. if coal_PRI_PVEM==1
#     ...
#     drop coal_PRI_PVEM
###############################################################################
all_data$PRI [all_data$coal_PRI_PVEM == 1] <- NA
all_data$PVEM[all_data$coal_PRI_PVEM == 1] <- NA

all_data$coal_PRI_PVEM <- NULL

###############################################################################
# 19) replace PRD=. if coal_PRD_PT==1
#     ...
#     drop coal_PRD_PT
###############################################################################
all_data$PRD[all_data$coal_PRD_PT == 1] <- NA
all_data$PT [all_data$coal_PRD_PT == 1] <- NA

all_data$coal_PRD_PT <- NULL

###############################################################################
# 20) replace PRD=. if coal_PRD_PANAL==1
#     ...
#     drop coal_PRD_PANAL
###############################################################################
all_data$PRD   [all_data$coal_PRD_PANAL == 1] <- NA
all_data$PANAL [all_data$coal_PRD_PANAL == 1] <- NA

all_data$coal_PRD_PANAL <- NULL

###############################################################################
# 21) replace PRD=. if coal_PRD_PES==1
#     ...
#     drop coal_PRD_PES
###############################################################################
all_data$PRD[all_data$coal_PRD_PES == 1] <- NA
all_data$PES[all_data$coal_PRD_PES == 1] <- NA

all_data$coal_PRD_PES <- NULL

###############################################################################
# 22) replace PT=. if coal_PT_MC==1
#     ...
#     drop coal_PT_MC
###############################################################################
all_data$PT [all_data$coal_PT_MC == 1] <- NA
all_data$MC [all_data$coal_PT_MC == 1] <- NA

all_data$coal_PT_MC <- NULL

###############################################################################
# 23) replace PT=. if coal_PT_PH==1
#     ...
#     drop coal_PT_PH
###############################################################################
all_data$PT[all_data$coal_PT_PH == 1] <- NA
all_data$PH[all_data$coal_PT_PH == 1] <- NA

all_data$coal_PT_PH <- NULL

###############################################################################
# 24) replace PT=. if coal_PT_PES==1
#     ...
#     drop coal_PT_PES
###############################################################################
all_data$PT [all_data$coal_PT_PES == 1] <- NA
all_data$PES[all_data$coal_PT_PES == 1] <- NA

all_data$coal_PT_PES <- NULL

###############################################################################
### PART 8. rename municipality=proper(municipality)
###############################################################################
all_data <- all_data %>%
  mutate(municipality = str_to_title(municipality))

###############################################################################
### PART 9. uniqueid=..., merges LN2015
###############################################################################

# We'll replicate your code to set uniqueid:
all_data <- all_data %>%
  mutate(uniqueid=0) %>%
  mutate(uniqueid = case_when(
    municipality=="Acuitzio" ~ 16001,
    municipality=="Aguililla" ~ 16002,
    municipality=="Alvaro Obregon" ~ 16003,
    municipality=="Angamacutiro" ~ 16004,
    municipality=="Angamacutiro" ~ 16004,
    municipality=="Angangueo" ~ 16005,
    municipality=="Apatzingan" ~ 16006,
    municipality=="Aporo" ~ 16007,
    municipality=="Aquila" ~ 16008,
    municipality=="Ario" ~ 16009,
    municipality=="Arteaga" ~ 16010,
    municipality=="Brisenas" ~ 16011,
    municipality=="Buenavista" ~ 16012,
    municipality=="Caracuaro" ~ 16013,
    municipality=="Charapan" ~ 16021,
    municipality=="Charo" ~ 16022,
    municipality=="Chavinda" ~ 16023,
    municipality=="Cheran" ~ 16024,
    municipality=="Chilchota" ~ 16025,
    municipality=="Chinicuila" ~ 16026,
    municipality=="Chucandiro" ~ 16027,
    municipality=="Churintzio" ~ 16028,
    municipality=="Churumuco" ~ 16029,
    municipality=="Coahuayana" ~ 16014,
    municipality=="Coalcoman" ~ 16015,
    municipality=="Coeneo" ~ 16016,
    municipality=="Regules" ~ 16074,
    municipality=="Contepec" ~ 16017,
    municipality=="Copandaro" ~ 16018,
    municipality=="Cotija" ~ 16019,
    municipality=="Cuitzeo" ~ 16020,
    municipality=="Ecuandureo" ~ 16030,
    municipality=="Epitacio Huerta" ~ 16031,
    municipality=="Erongaricuaro" ~ 16032,
    municipality=="Gabriel Zamora" ~ 16033,
    municipality=="Hidalgo" ~ 16034,
    municipality=="Huandacareo" ~ 16036,
    municipality=="Huaniqueo" ~ 16037,
    municipality=="Huetamo" ~ 16038,
    municipality=="Huiramba" ~ 16039,
    municipality=="Indaparapeo" ~ 16040,
    municipality=="Irimbo" ~ 16041,
    municipality=="Ixtlan" ~ 16042,
    municipality=="Jacona" ~ 16043,
    municipality=="Jimenez" ~ 16044,
    municipality=="Jiquilpan" ~ 16045,
    municipality=="Jose Sixto Verduzco" ~ 16113,
    municipality=="Juarez" ~ 16046,
    municipality=="Jungapeo" ~ 16047,
    municipality=="La Huacana" ~ 16035,
    municipality=="La Piedad" ~ 16069,
    municipality=="Lagunillas" ~ 16048,
    municipality=="Lazaro Cardenas" ~ 16052,
    municipality=="Los Reyes" ~ 16075,
    municipality=="Madero" ~ 16049,
    municipality=="Maravatio" ~ 16050,
    municipality=="Marcos Castellanos" ~ 16051,
    municipality=="Morelia" ~ 16053,
    municipality=="Morelos" ~ 16054,
    municipality=="Mugica" ~ 16055,
    municipality=="Nahuatzen" ~ 16056,
    municipality=="Nocupetaro" ~ 16057,
    municipality=="Nuevo Parangaricutiro" ~ 16058,
    municipality=="Nuevo Urecho" ~ 16059,
    municipality=="Numaran" ~ 16060,
    municipality=="Ocampo" ~ 16061,
    municipality=="Pajacuaran" ~ 16062,
    municipality=="Panindicuaro" ~ 16063,
    municipality=="Paracho" ~ 16065,
    municipality=="Paracuaro" ~ 16064,
    municipality=="Patzcuaro" ~ 16066,
    municipality=="Penjamillo" ~ 16067,
    municipality=="Periban" ~ 16068,
    municipality=="Purepero" ~ 16070,
    municipality=="Puruandiro" ~ 16071,
    municipality=="Querendaro" ~ 16072,
    municipality=="Quiroga" ~ 16073,
    municipality=="Sahuayo" ~ 16076,
    municipality=="Salvador Escalante" ~ 16079,
    municipality=="San Lucas" ~ 16077,
    municipality=="Santa Ana Maya" ~ 16078,
    municipality=="Senguio" ~ 16080,
    municipality=="Susupuato" ~ 16081,
    municipality=="Tacambaro" ~ 16082,
    municipality=="Tancitaro" ~ 16083,
    municipality=="Tangamandapio" ~ 16084,
    municipality=="Tangancicuaro" ~ 16085,
    municipality=="Tanhuato" ~ 16086,
    municipality=="Taretan" ~ 16087,
    municipality=="Tarimbaro" ~ 16088,
    municipality=="Tepalcatepec" ~ 16089,
    municipality=="Tingambato" ~ 16090,
    municipality=="Tinguindin" ~ 16091,
    municipality=="Tiquicheo" ~ 16092,
    municipality=="Tlalpujahua" ~ 16093,
    municipality=="Tlazazalca" ~ 16094,
    municipality=="Tocumbo" ~ 16095,
    municipality=="Tumbiscatio" ~ 16096,
    municipality=="Turicato" ~ 16097,
    municipality=="Tuxpan" ~ 16098,
    municipality=="Tuzantla" ~ 16099,
    municipality=="Tzintzuntzan" ~ 16100,
    municipality=="Tzitzio" ~ 16101,
    municipality=="Uruapan" ~ 16102,
    municipality=="Venustiano Carranza" ~ 16103,
    municipality=="Villamar" ~ 16104,
    municipality=="Vista Hermosa" ~ 16105,
    municipality=="Yurecuaro" ~ 16106,
    municipality=="Zacapu" ~ 16107,
    municipality=="Zamora" ~ 16108,
    municipality=="Zinaparo" ~ 16109,
    municipality=="Zinapecuaro" ~ 16110,
    municipality=="Ziracuaretiro" ~ 16111,
    municipality=="Zitacuaro" ~ 16112,
    TRUE ~ uniqueid
  ))

# then municipality = upper(municipality)
all_data <- all_data %>%
  mutate(municipality = str_to_upper(municipality))

# drop SUMATOTALDEVOTOS VOTACIÓNEMITIDA if exist
drop_vars3 <- c("SUMATOTALDEVOTOS","VOTACIÓNEMITIDA")
drop_vars3 <- intersect(drop_vars3, names(all_data))
all_data <- all_data %>% select(-all_of(drop_vars3))

# collapse (sum) PAN-..., by (municipality uniqueid section)
# "PAN-PAN_PRD_PT_PANAL_PH" => we find them if exist
start_coal <- match("PAN", names(all_data))
end_coal   <- match("PAN_PRD_PT_PANAL_PH", names(all_data))
if(!is.na(start_coal) & !is.na(end_coal)) {
  vars_coal_collapse <- names(all_data)[start_coal:end_coal]
  # also keep everything we might need?
} else {
  vars_coal_collapse <- setdiff(names(all_data), c("municipality","uniqueid","section"))
}
all_data <- all_data %>%
  group_by(municipality, uniqueid, section) %>%
  summarize(across(all_of(vars_coal_collapse), sum, na.rm=TRUE), .groups="drop")

# "replace CI=CI+CI_1; drop CI_1 => rename CI CI_1"
if("CI" %in% names(all_data) & "CI_1" %in% names(all_data)) {
  all_data <- all_data %>%
    mutate(CI = CI + CI_1) %>%
    select(-CI_1)
  names(all_data)[names(all_data)=="CI"] <- "CI_1"
}

# new "egen valid= rowtotal(...)" 
# The code lists many parties. We'll collect them if they exist:
final_parties <- c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","MORENA","PH","PES","PRI_PVEM",
                   "PRD_PT_PANAL","PRD_PT_PANAL_PES","PRD_PANAL","PRD_PANAL_PES","PT_PANAL_PH","PAN_PH","PAN_MC","PAN_PRD_PT",
                   "PRD_PES","PRD_PT_PES","PT_PES_PH","CI_1","PRD_PT_PANAL_PH","PRD_PT_PH","PT_MC","PT_PH","PRD_PT","PAN_PT",
                   "PAN_PRI_PRD_PANAL_PH_PES","PAN_PRD","PAN_PRD_PT_PANAL_PES","PAN_PRI_PVEM","PAN_PRD_PT_PANAL_PH")
final_parties <- intersect(final_parties, names(all_data))

# Then valid = sum of these
all_data <- all_data %>%
  rowwise() %>%
  mutate(valid = sum(c_across(all_of(final_parties)), na.rm=TRUE)) %>%
  ungroup()

# then total = valid + VOTOSNULOS + NOREGISTRADOS if those exist
if("VOTOSNULOS" %in% names(all_data) & "NOREGISTRADOS" %in% names(all_data)) {
  all_data <- all_data %>%
    mutate(total = valid + VOTOSNULOS + NOREGISTRADOS) %>%
    select(-VOTOSNULOS, -NOREGISTRADOS)
}

# g year=2015, month="June", state="MICHOACAN"
all_data <- all_data %>%
  mutate(
    year=2015,
    month="June",
    STATE="MICHOACAN"
  )

###############################################################################
### PART 10. Merge LN2015 for listanominal
###############################################################################

ln2015 <- read_dta("../Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
  filter(entidad==16, month==6) %>%
  mutate(uniqueid = (entidad*1000)+ municipio) %>%
  filter(seccion!=0) %>%
  arrange(uniqueid,seccion) %>%
  select(uniqueid, section=seccion, lista)

data_2015 <- all_data %>%
  left_join(ln2015, by=c("uniqueid","section"))

# drop if _merge==2 => filter out unmatched
data_2015 <- data_2015 %>% filter(!is.na(lista))

# rename lista -> listanominal
data_2015 <- data_2015 %>%
  rename(listanominal = lista)

# gen mun_turnout = mun_total/mun_listanominal
# gen turnout=total/listanominal
data_2015 <- data_2015 %>%
  mutate(
    turnout = total / listanominal
  )

###############################################################################
### 1. Import Excel (Stata: import excel "SAHUAYO ELECCION EXTRAORDINARIA.xlsx" ...)
###############################################################################
data_sahuayo <- read_excel(
  "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/SAHUAYO ELECCION EXTRAORDINARIA.xlsx",
  sheet = "COMPUTO FINAL",
  range = "A9:AE101",
  col_names = TRUE
)

# Check current column names
names(data_sahuayo)

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
n_cols  <- ncol(data_sahuayo)        # how many columns do you have?
new_names <- gen_excel_col_names(n_cols)
names(data_sahuayo) <- new_names

# Check
names(data_sahuayo)

###############################################################################
### 2. Rename columns
###    E -> section, G -> listanominal, AC -> total
###############################################################################
# We'll assume columns are indeed "E", "G", and "AC" respectively due to the order position!
data_sahuayo <- data_sahuayo %>%
  rename(
    section      = E,
    listanominal = G,
    total        = AC
  )

###############################################################################
### 3. Create municipality and uniqueid, drop if total==0
###############################################################################
data_sahuayo <- data_sahuayo %>%
  mutate(
    municipality = "SAHUAYO EXTRAORDINARIO",
    uniqueid     = 16076
  ) %>%
  filter(!(is.na(total) | total==0))

###############################################################################
### 4. Rename additional columns
###    U -> PAN_PRD_PANAL
###    Z -> PRI_PT_PVEM
###    M -> MC
###    O -> MORENA
###    P -> PES
###############################################################################
# Adjust if your data has different names or columns:
data_sahuayo <- data_sahuayo %>%
  rename(
    PAN_PRD_PANAL = U,
    PRI_PT_PVEM   = Z,
    MC            = M,
    MORENA        = O,
    PES           = P
  )

###############################################################################
### 5. collapse (sum) by (uniqueid municipality section)
###############################################################################
vars_for_collapse <- c("listanominal","PAN_PRD_PANAL","PRI_PT_PVEM","MC","MORENA","PES","total")
vars_for_collapse <- intersect(vars_for_collapse, names(data_sahuayo))

group_vars <- c("uniqueid","municipality","section")

data_sahuayo <- data_sahuayo %>%
  group_by(across(all_of(group_vars))) %>%
  summarize(across(all_of(vars_for_collapse), sum, na.rm=TRUE), .groups="drop")

###############################################################################
### 6. Generate turnout = total/listanominal
###############################################################################
data_sahuayo <- data_sahuayo %>%
  mutate(turnout = total / listanominal)

###############################################################################
### 7. egen valid = rowtotal(PAN_PRD_PANAL PRI_PT_PVEM MC MORENA PES)
###############################################################################
party_vars <- c("PAN_PRD_PANAL","PRI_PT_PVEM","MC","MORENA","PES")
party_vars <- intersect(party_vars, names(data_sahuayo))

data_sahuayo <- data_sahuayo %>%
  rowwise() %>%
  mutate(valid = sum(c_across(all_of(party_vars)), na.rm=TRUE)) %>%
  ungroup()

###############################################################################
### 8. year=2015, month="December"
###############################################################################
data_sahuayo <- data_sahuayo %>%
  mutate(
    year  = 2015,
    month = "December"
  )

###############################################################################
### PART 1. READ SHEETS FROM "Ayuntamientos_Mich_2018.xlsx" AND SAVE EACH AS .DTA
###############################################################################

sheet_list <- excel_sheets("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Ayuntamientos_Mich_2018.xlsx")

for (sheetname in sheet_list) {
  message(paste("Processing sheet:", sheetname))
  
  # 1) Read the sheet
  df_sheet <- read_excel(
    "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018/Ayuntamientos_Mich_2018.xlsx",
    sheet = sheetname, 
    col_names = TRUE
  )
  
  # 2) Check for ID_MUNICIPIO
  if (!"ID_MUNICIPIO" %in% names(df_sheet)) {
    message("No ID_MUNICIPIO column in sheet: ", sheetname, ", skipping.")
    next
  }
  
  # 3) Drop rows where ID_MUNICIPIO is blank
  df_sheet <- df_sheet %>%
    filter(ID_MUNICIPIO != "")
  
  # 4) Convert all columns to character, replace blanks "" with "0"
  for (cn in names(df_sheet)) {
    df_sheet[[cn]] <- as.character(df_sheet[[cn]])
    df_sheet[[cn]][df_sheet[[cn]] == ""] <- "0"
  }
  
  # 5) Sanitize column names so that saving to .dta won't fail
  #    We'll remove any space or special character, replace with underscore
  df_sheet <- df_sheet %>% 
    rename_with(~ gsub("[^A-Za-z0-9_]", "_", .x))
  
  # 6) Build the output filename
  file_out <- paste0(gsub("[^A-Za-z0-9_]", "_", sheetname), ".dta")
  
  # 7) Save as .dta
  haven::write_dta(df_sheet, file_out)
  message(paste("Saved:", file_out))
}

###############################################################################
### PART X: CLEAR & APPEND MULTIPLE .DTA FILES IN R
###############################################################################

# 2) Define the list of .dta files in the same order as your Stata `append using` lines
files_to_append <- c(
  "Hoja1 (2).dta", "Hoja1 (3).dta", "Hoja1 (4).dta", "Hoja1 (5).dta", "Hoja1 (6).dta",
  "Hoja1 (7).dta", "Hoja1 (8).dta", "Hoja1 (9).dta", "Hoja1 (10).dta", "Hoja1 (11).dta",
  "Hoja1 (12).dta", "Hoja1 (13).dta", "Hoja1 (14).dta", "Hoja1 (15).dta", "Hoja1 (16).dta",
  "Hoja1 (17).dta", "Hoja1 (18).dta", "Hoja1 (19).dta", "Hoja1 (20).dta", "Hoja1 (21).dta",
  "Hoja1 (22).dta", "Hoja1 (23).dta", "Hoja1 (24).dta", "Hoja1 (25).dta", "Hoja1 (26).dta",
  "Hoja1 (27).dta", "Hoja1 (28).dta", "Hoja1 (29).dta", "Hoja1 (30).dta", "Hoja1 (31).dta",
  "Hoja1 (32).dta", "Hoja1 (33).dta", "Hoja1 (34).dta", "Hoja1 (35).dta", "Hoja1 (36).dta",
  "Hoja1 (37).dta", "Hoja1 (38).dta", "Hoja1 (39).dta", "Hoja1 (40).dta", "Hoja1 (41).dta",
  "Hoja1 (42).dta", "Hoja1 (43).dta", "Hoja1 (44).dta", "Hoja1 (45).dta", "Hoja1 (46).dta",
  "Hoja1 (47).dta", "Hoja1 (48).dta", "Hoja1 (49).dta", "Hoja1 (50).dta", "Hoja1 (51).dta",
  "Hoja1 (52).dta", "Hoja1 (53).dta", "Hoja1 (54).dta", "Hoja1 (55).dta", "Hoja1 (56).dta",
  "Hoja1 (57).dta", "Hoja1 (58).dta", "Hoja1 (59).dta", "Hoja1 (60).dta", "Hoja1 (61).dta",
  "Hoja1 (62).dta", "Hoja1 (63).dta", "Hoja1 (64).dta", "Hoja1 (65).dta", "Hoja1 (66).dta",
  "Hoja1 (67).dta", "Hoja1 (68).dta", "Hoja1 (69).dta", "Hoja1 (70).dta", "Hoja1 (71).dta",
  "Hoja1 (72).dta", "Hoja1 (73).dta", "Hoja1 (74).dta", "Hoja1 (75).dta", "Hoja1 (76).dta",
  "Hoja1 (77).dta", "Hoja1 (78).dta", "Hoja1 (79).dta", "Hoja1 (80).dta", "Hoja1 (81).dta",
  "Hoja1 (82).dta", "Hoja1 (83).dta", "Hoja1 (84).dta", "Hoja1 (85).dta", "Hoja1 (86).dta",
  "Hoja1 (87).dta", "Hoja1 (88).dta", "Hoja1 (89).dta", "Hoja1 (90).dta", "Hoja1 (91).dta",
  "Hoja1 (92).dta", "Hoja1 (93).dta", "Hoja1 (94).dta", "Hoja1 (95).dta", "Hoja1 (96).dta",
  "Hoja1 (97).dta", "Hoja1 (98).dta", "Hoja1 (99).dta", "Hoja1 (100).dta", "Hoja1 (101).dta",
  "Hoja1 (102).dta", "Hoja1 (103).dta", "Hoja1 (104).dta", "Hoja1 (105).dta", "Hoja1 (106).dta",
  "Hoja1 (107).dta", "Hoja1 (108).dta", "Hoja1 (109).dta", "Hoja1 (110).dta", "Hoja1 (111).dta",
  "Hoja1 (112).dta", "Hoja1 (113).dta"
)

# 3) Initialize an empty data frame to hold appended data
all_hoja <- NULL

# 4) Loop through each file, read & append
for(fname in files_to_append) {
  if(!file.exists(fname)) {
    message("File not found, skipping: ", fname)
    next
  }
  message("Appending file: ", fname)
  
  temp_df <- haven::read_dta(fname)
  
  if(is.null(all_hoja)) {
    # first .dta sets up the structure
    all_hoja <- temp_df
  } else {
    # subsequent .dta -> "append" by binding rows
    all_hoja <- dplyr::bind_rows(all_hoja, temp_df)
  }
}


all_hoja <- all_hoja %>% 
  mutate(municipality = str_to_title(MUNICIPIO))

###############################################################################
### 0. Remove all .dta files that were previously written, keep working with `all_hoja`
###############################################################################

# Suppose we had a vector `files_to_append` referencing all .dta files we wrote.
files_to_append <- c(
  "Hoja1 (2).dta", "Hoja1 (3).dta", "Hoja1 (4).dta", "Hoja1 (5).dta", "Hoja1 (6).dta",
  "Hoja1 (7).dta", "Hoja1 (8).dta", "Hoja1 (9).dta", "Hoja1 (10).dta", "Hoja1 (11).dta",
  "Hoja1 (12).dta", "Hoja1 (13).dta", "Hoja1 (14).dta", "Hoja1 (15).dta", "Hoja1 (16).dta",
  "Hoja1 (17).dta", "Hoja1 (18).dta", "Hoja1 (19).dta", "Hoja1 (20).dta", "Hoja1 (21).dta",
  "Hoja1 (22).dta", "Hoja1 (23).dta", "Hoja1 (24).dta", "Hoja1 (25).dta", "Hoja1 (26).dta",
  "Hoja1 (27).dta", "Hoja1 (28).dta", "Hoja1 (29).dta", "Hoja1 (30).dta", "Hoja1 (31).dta",
  "Hoja1 (32).dta", "Hoja1 (33).dta", "Hoja1 (34).dta", "Hoja1 (35).dta", "Hoja1 (36).dta",
  "Hoja1 (37).dta", "Hoja1 (38).dta", "Hoja1 (39).dta", "Hoja1 (40).dta", "Hoja1 (41).dta",
  "Hoja1 (42).dta", "Hoja1 (43).dta", "Hoja1 (44).dta", "Hoja1 (45).dta", "Hoja1 (46).dta",
  "Hoja1 (47).dta", "Hoja1 (48).dta", "Hoja1 (49).dta", "Hoja1 (50).dta", "Hoja1 (51).dta",
  "Hoja1 (52).dta", "Hoja1 (53).dta", "Hoja1 (54).dta", "Hoja1 (55).dta", "Hoja1 (56).dta",
  "Hoja1 (57).dta", "Hoja1 (58).dta", "Hoja1 (59).dta", "Hoja1 (60).dta", "Hoja1 (61).dta",
  "Hoja1 (62).dta", "Hoja1 (63).dta", "Hoja1 (64).dta", "Hoja1 (65).dta", "Hoja1 (66).dta",
  "Hoja1 (67).dta", "Hoja1 (68).dta", "Hoja1 (69).dta", "Hoja1 (70).dta", "Hoja1 (71).dta",
  "Hoja1 (72).dta", "Hoja1 (73).dta", "Hoja1 (74).dta", "Hoja1 (75).dta", "Hoja1 (76).dta",
  "Hoja1 (77).dta", "Hoja1 (78).dta", "Hoja1 (79).dta", "Hoja1 (80).dta", "Hoja1 (81).dta",
  "Hoja1 (82).dta", "Hoja1 (83).dta", "Hoja1 (84).dta", "Hoja1 (85).dta", "Hoja1 (86).dta",
  "Hoja1 (87).dta", "Hoja1 (88).dta", "Hoja1 (89).dta", "Hoja1 (90).dta", "Hoja1 (91).dta",
  "Hoja1 (92).dta", "Hoja1 (93).dta", "Hoja1 (94).dta", "Hoja1 (95).dta", "Hoja1 (96).dta",
  "Hoja1 (97).dta", "Hoja1 (98).dta", "Hoja1 (99).dta", "Hoja1 (100).dta", "Hoja1 (101).dta",
  "Hoja1 (102).dta", "Hoja1 (103).dta", "Hoja1 (104).dta", "Hoja1 (105).dta", "Hoja1 (106).dta",
  "Hoja1 (107).dta", "Hoja1 (108).dta", "Hoja1 (109).dta", "Hoja1 (110).dta", "Hoja1 (111).dta",
  "Hoja1 (112).dta", "Hoja1 (113).dta"
)

message("Deleting all individual .dta files we wrote earlier...")

for(fname in files_to_append) {
  if(file.exists(fname)) {
    file.remove(fname)
    message("Removed: ", fname)
  } else {
    message("File not found, skipping removal: ", fname)
  }
}

message("Now continuing with `all_hoja`...")

###############################################################################
### 1. We assume you already have `df` in memory (the appended data).
###    We replicate your Stata lines that set `uniqueid` based on `municipality`.
###############################################################################

# "gen uniqueid=0"
df <- all_hoja %>%
  mutate(uniqueid = NA)

# Then replicate each "replace uniqueid=16001 if municipality=='Acuitzio'", etc.
# In R, we do `case_when` or a chain of `if_else`.
df <- df %>%
  mutate(
    uniqueid = case_when(
      municipality == "Acuitzio" ~ 16001,
      municipality == "Aguililla" ~ 16002,
      municipality == "Álvaro Obregón" ~ 16003,
      municipality == "Angamacutiro" ~ 16004,
      municipality == "Angangueo" ~ 16005,
      municipality == "Apatzingán" ~ 16006,
      municipality == "Áporo" ~ 16007,
      municipality == "Aquila" ~ 16008,
      municipality == "Ario" ~ 16009,
      municipality == "Arteaga" ~ 16010,
      municipality == "Brisenas" | municipality == "BriseÑAs" ~ 16011,
      municipality == "Buenavista" ~ 16012,
      municipality == "Carácuaro" ~ 16013,
      municipality == "Charapan" ~ 16021,
      municipality == "Charo" ~ 16022,
      municipality == "Chavinda" ~ 16023,
      municipality == "Cheran" ~ 16024,
      municipality == "Chilchota" ~ 16025,
      municipality == "Chinicuila" ~ 16026,
      municipality == "Chucándiro" ~ 16027,
      municipality == "Churintzio" ~ 16028,
      municipality == "Churumuco" ~ 16029,
      municipality == "Coahuayana" ~ 16014,
      municipality == "Coalcomán De Vazquez Pallares" ~ 16015,
      municipality == "Coeneo" ~ 16016,
      municipality == "Cojumatlán De Régules" ~ 16074,
      municipality == "Contepec" ~ 16017,
      municipality == "Copándaro" ~ 16018,
      municipality == "Cotija" ~ 16019,
      municipality == "Cuitzeo" ~ 16020,
      municipality == "Ecuandureo" ~ 16030,
      municipality == "Epitacio Huerta" ~ 16031,
      municipality == "Erongarícuaro" ~ 16032,
      municipality == "Gabriel Zamora" ~ 16033,
      municipality == "Hidalgo" ~ 16034,
      municipality == "Huandacareo" ~ 16036,
      municipality == "Huaníqueo" ~ 16037,
      municipality == "Huetamo" ~ 16038,
      municipality == "Huiramba" ~ 16039,
      municipality == "Indaparapeo" ~ 16040,
      municipality == "Irimbo" ~ 16041,
      municipality == "Ixtlán" ~ 16042,
      municipality == "Jacona" ~ 16043,
      municipality == "Jiménez" ~ 16044,
      municipality == "Jiquilpan" ~ 16045,
      municipality == "José Sixto Verduzco" ~ 16113,
      municipality == "Juárez" ~ 16046,
      municipality == "Jungapeo" ~ 16047,
      municipality == "La Huacana" ~ 16035,
      municipality == "La Piedad" ~ 16069,
      municipality == "Lagunillas" ~ 16048,
      municipality == "Lázaro Cárdenas" ~ 16052,
      municipality == "Los Reyes" ~ 16075,
      municipality == "Madero" ~ 16049,
      municipality == "Maravatío" ~ 16050,
      municipality == "Marcos Castellanos" ~ 16051,
      municipality == "Morelia" ~ 16053,
      municipality == "Morelos" ~ 16054,
      municipality == "Múgica" ~ 16055,
      municipality == "Nahuátzen" ~ 16056,
      municipality == "Nocupétaro" ~ 16057,
      municipality == "Nuevo Parangaricutiro" ~ 16058,
      municipality == "Nuevo Urecho" ~ 16059,
      municipality == "Numarán" ~ 16060,
      municipality == "Ocampo" ~ 16061,
      municipality == "Pajacuarán" ~ 16062,
      municipality == "Panindícuaro" ~ 16063,
      municipality == "Paracho" ~ 16065,
      municipality == "Parácuaro" ~ 16064,
      municipality == "Pátzcuaro" ~ 16066,
      municipality == "Penjamillo" ~ 16067,
      municipality == "Peribán" ~ 16068,
      municipality == "Purépero" ~ 16070,
      municipality == "Puruándiro" ~ 16071,
      municipality == "Queréndaro" ~ 16072,
      municipality == "Quiroga" ~ 16073,
      municipality == "Sahuayo" ~ 16076,
      municipality == "Salvador Escalante" ~ 16079,
      municipality == "San Lucas" ~ 16077,
      municipality == "Santa Ana Maya" ~ 16078,
      municipality == "Senguio" ~ 16080,
      municipality == "Susupuato" ~ 16081,
      municipality == "Tacámbaro" ~ 16082,
      municipality == "Tancítaro" ~ 16083,
      municipality == "Tangamandapio" ~ 16084,
      municipality == "Tangancícuaro" ~ 16085,
      municipality == "Tanhuato" ~ 16086,
      municipality == "Taretan" ~ 16087,
      municipality == "Tarímbaro" ~ 16088,
      municipality == "Tepalcatepec" ~ 16089,
      municipality == "Tingambato" ~ 16090,
      municipality == "Tingüindín" ~ 16091,
      municipality == "Tiquicheo" | municipality=="Tiquicheo De Nicolas Romero" ~ 16092,
      municipality == "Tlalpujahua" ~ 16093,
      municipality == "Tlazazalca" ~ 16094,
      municipality == "Tocumbo" ~ 16095,
      municipality == "Tumbiscatio" ~ 16096,
      municipality == "Turicato" ~ 16097,
      municipality == "Tuxpan" ~ 16098,
      municipality == "Tuzantla" ~ 16099,
      municipality == "Tzintzuntzan" ~ 16100,
      municipality == "Tzitzio" ~ 16101,
      municipality == "Uruapan" ~ 16102,
      municipality == "Venustiano Carranza" ~ 16103,
      municipality == "Villamar" ~ 16104,
      municipality == "Vista Hermosa" ~ 16105,
      municipality == "Yurécuaro" ~ 16106,
      municipality == "Zacapu" ~ 16107,
      municipality == "Zamora" ~ 16108,
      municipality == "Zináparo" ~ 16109,
      municipality == "Zinapécuaro" ~ 16110,
      municipality == "Ziracuaretiro" ~ 16111,
      municipality == "Zitácuaro" ~ 16112,
      TRUE ~ uniqueid # keep old value if none matched
    )
  )

###############################################################################
### 2. "destring *, replace" => Convert all columns to numeric if possible
###############################################################################
# We'll replicate "destring" for all columns:
df <- df %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.)))))

###############################################################################
### 1) Generate or update coalition columns (PAN_PRD_PMC, etc.)
###    Then zero out the base parties if the coalition col is not missing
###############################################################################

# Example: g PAN_PRD_PMC = CO_PAN_PRD_PMC + CO_PAN_PRD + ... if CO_PAN_PRD_PMC != .
# In R, we do: if(!is.na(df$CO_PAN_PRD_PMC)) => sum them, else NA
if(all(c("CO_PAN_PRD_PMC","CO_PAN_PRD","CO_PAN_PMC","CO_PRD_PMC","PAN","PMC","PRD") %in% names(df))) {
  
  df <- df %>%
    mutate(
      # replicate "g PAN_PRD_PMC = (CO_PAN_PRD_PMC + CO_PAN_PRD + CO_PAN_PMC + CO_PRD_PMC + PAN + PMC + PRD) if CO_PAN_PRD_PMC !=."
      # i.e. only sum if CO_PAN_PRD_PMC is not NA, else leave as NA
      PAN_PRD_PMC = if_else(!is.na(CO_PAN_PRD_PMC),
                            rowSums(across(c(CO_PAN_PRD_PMC, CO_PAN_PRD, CO_PAN_PMC, CO_PRD_PMC, PAN, PMC, PRD)), na.rm=TRUE),
                            NA_real_
      ),
      # replicate "replace PAN=. if CO_PAN_PRD_PMC!=."
      PAN = if_else(!is.na(CO_PAN_PRD_PMC), NA_real_, PAN),
      PMC = if_else(!is.na(CO_PAN_PRD_PMC), NA_real_, PMC),
      PRD = if_else(!is.na(CO_PAN_PRD_PMC), NA_real_, PRD),
      # replicate "gen coalfrente = CO_PAN_PRD_PMC!=."
      coalfrente = if_else(!is.na(CO_PAN_PRD_PMC), 1, 0)
    ) %>%
    # replicate "drop CO_PAN_PRD_PMC CO_PAN_PRD CO_PAN_PMC CO_PRD_PMC"
    select(-CO_PAN_PRD_PMC, -CO_PAN_PRD, -CO_PAN_PMC, -CO_PRD_PMC)
}


# Similarly, do the logic for the other coalition blocks:

if(all(c("CC_PAN_PMC","PAN","PMC") %in% names(df))) {
  df <- df %>%
    mutate(
      PAN_PMC = if_else(!is.na(CC_PAN_PMC),
                        rowSums(across(c(CC_PAN_PMC, PAN, PMC)), na.rm=TRUE),
                        NA_real_
      ),
      PAN = if_else(!is.na(CC_PAN_PMC), 0, PAN),
      PMC = if_else(!is.na(CC_PAN_PMC), 0, PMC),
      coalpanmc = if_else(!is.na(CC_PAN_PMC), 1, 0)
    ) %>%
    select(-CC_PAN_PMC)
}

if(all(c("CC_PAN_PRD","PAN","PRD") %in% names(df))) {
  df <- df %>%
    mutate(
      PAN_PRD = if_else(!is.na(CC_PAN_PRD),
                        rowSums(across(c(CC_PAN_PRD, PAN, PRD)), na.rm=TRUE),
                        NA_real_
      ),
      PAN = if_else(!is.na(CC_PAN_PRD), 0, PAN),
      PRD = if_else(!is.na(CC_PAN_PRD), 0, PRD),
      coalpanprd = if_else(!is.na(CC_PAN_PRD), 1, 0)
    ) %>%
    select(-CC_PAN_PRD)
}

if(all(c("CC_PRD_PMC","PRD","PMC") %in% names(df))) {
  df <- df %>%
    mutate(
      PRD_PMC = if_else(!is.na(CC_PRD_PMC),
                        rowSums(across(c(CC_PRD_PMC, PRD, PMC)), na.rm=TRUE),
                        NA_real_
      ),
      PRD = if_else(!is.na(CC_PRD_PMC), 0, PRD),
      PMC = if_else(!is.na(CC_PRD_PMC), 0, PMC),
      coalprdmc = if_else(!is.na(CC_PRD_PMC), 1, 0)
    ) %>%
    select(-CC_PRD_PMC)
}

if(all(c("CC_PRD_PVEM","PRD","PVEM") %in% names(df))) {
  df <- df %>%
    mutate(
      PRD_PVEM = if_else(!is.na(CC_PRD_PVEM),
                         rowSums(across(c(CC_PRD_PVEM, PRD, PVEM)), na.rm=TRUE),
                         NA_real_
      ),
      PRD = if_else(!is.na(CC_PRD_PVEM), 0, PRD),
      PVEM = if_else(!is.na(CC_PRD_PVEM), 0, PVEM),
      coalprdpvem = if_else(!is.na(CC_PRD_PVEM), 1, 0)
    ) %>%
    select(-CC_PRD_PVEM)
}

if(all(c("CO_PT_MORENA","PT","MORENA") %in% names(df))) {
  df <- df %>%
    mutate(
      PT_MORENA = if_else(!is.na(CO_PT_MORENA),
                          rowSums(across(c(CO_PT_MORENA, PT, MORENA)), na.rm=TRUE),
                          NA_real_
      ),
      PT = if_else(!is.na(CO_PT_MORENA), 0, PT),
      MORENA = if_else(!is.na(CO_PT_MORENA), 0, MORENA),
      coalptmorena = if_else(!is.na(CO_PT_MORENA), 1, 0)
    ) %>%
    select(-CO_PT_MORENA)
}

###############################################################################
### 2) rename SECCION->section, rename PNA->PANAL, rename *PMC->*MC, etc.
###############################################################################
# We'll do them if they exist:
if("SECCION" %in% names(df)) {
  df <- df %>% rename(section = SECCION)
}
if("PNA" %in% names(df)) {
  df <- df %>% rename(PANAL = PNA)
}

# For rename `*PMC` -> `*MC`, we can do:
names(df) <- gsub("PMC$", "MC", names(df))

###############################################################################
### 3) "order coal*, a(PT_MORENA)" and 
###    "order PAN_PRD_MC PAN_MC PAN_PRD PRD_MC PRD_PVEM PT_MORENA CI_*, a(PES)"
###    - This changes variable order in Stata, not essential in R. We can skip or reorder if we want.
###############################################################################

###############################################################################
### 4) collapse (sum) PAN-CI_2 NOREG NULO (first) coal*, by (municipality section uniqueid)
###    In R, let's define a group_by() and summarize().
###############################################################################
# Stata code suggests summing columns from "PAN" through "CI_2", plus NOREG, NULO, and 'coal*' columns.
# We'll identify these columns if they exist. Then do a "by (municipality, section, uniqueid)"

group_vars <- c("municipality","section","uniqueid")

# Suppose we want to sum all columns from "PAN" to "CI_2", plus NOREG, NULO, and "coal..." columns
# We'll pick them if they exist:
all_cols <- names(df)
start_idx <- match("PAN", all_cols)
end_idx   <- match("CI_2", all_cols)
if(!is.na(start_idx) & !is.na(end_idx)) {
  collapse_vars <- all_cols[start_idx:end_idx]
} else {
  # fallback
  collapse_vars <- c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","MORENA","PES","CI_1","CI_2","NOREG","NULO")
}
# also add any "coal" columns if you want:
collapse_vars <- union(collapse_vars, grep("^coal", all_cols, value=TRUE))

data_2018 <- df %>%
  group_by(across(all_of(group_vars))) %>%
  summarize(across(all_of(collapse_vars), sum, na.rm=TRUE), .groups="drop")

###############################################################################
### 5) egen valid = rowtotal(...) => row sum of parties
###    gen total= valid + NULO + NOREG => drop NOREG NULO
###############################################################################
data_2018 <- data_2018 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","MORENA","PES",
                                "PAN_PRD_MC","PT_MORENA","PAN_MC","PRD_PVEM","PAN_PRD","PRD_MC","CI_1","CI_2")),
                     na.rm=TRUE)) %>%
  ungroup()

data_2018 <- data_2018 %>%
  mutate(
    total = valid + NULO + NOREG
  ) %>%
  select(-NOREG, -NULO)

###############################################################################
#g year=2018, g month="July", g STATE="MICHOACAN"
###############################################################################
data_2018 <- data_2018 %>%
  mutate(
    year  = 2018,
    month = "July",
    STATE = "MICHOACAN"
  )

###############################################################################
### 6) preserve, then use LN data => we read "ListadoNominalPREP2018.dta", keep if STATE=="MICHOACAN"
###    Then save as "MICH_LN18.dta"
###    Then restore
###############################################################################
ln2018 <- read_dta("../Listas Nominales/ListadoNominalPREP2018.dta") %>%
  filter(STATE=="MICHOACAN")

###############################################################################
### 9) merge 1:1 STATE section using MICH_LN18.dta => left_join by (STATE, section)
###    drop _merge
###    erase MICH_LN18.dta
###############################################################################
data_2018 <- data_2018 %>%
  left_join(ln2018 %>% select(STATE, section, ListadoNominalINE), by=c("STATE","section"))

###############################################################################
### 10) rename ListadoNominalINE->listanominal
###     gen turnout= total/listanominal
###     bys uniqueid: egen mun_listanominal= sum(listanominal)
###     gen mun_turnout= mun_total/mun_listanominal
###############################################################################
data_2018 <- data_2018 %>%
  rename(listanominal = ListadoNominalINE) %>%
  mutate(
    turnout = total / listanominal)

# Combine the dataframes, handling different columns by filling with NA
michoacan_all <- bind_rows(data_1995,
                            data_1998,
                            data_2001,
                            data_2005,
                            data_2007,
                            data_2008,
                            data_2011,
                            data_2012,
                            data_2015,
                            data_sahuayo,
                            data_2018)

data.table::fwrite(michoacan_all,"../../../Processed Data/Michoacan/Michoacan_process_raw_data.csv")




