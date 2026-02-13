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
data_1995 <- data.table::fread("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/Ayu_Seccion_1995_No_LN.csv")

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
# 1. READ CSV
# -------------------------------------------------------------------
data_1998 <- read_csv("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/Ayu_Seccion_1998_Half_Missing.csv", show_col_types = FALSE)
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
# 10. gen turnout = total/listanominal
# -------------------------------------------------------------------
data_1998 <- data_1998 %>%
  mutate(turnout = ifelse(listanominal==0, NA, total / listanominal))

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
  ) %>% 
  select(-municipality)

# -------------------------------------------------------------------
# 1. READ CSV
# -------------------------------------------------------------------
data_2001 <- read_csv("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/Ayu_Seccion_2001.csv", show_col_types = FALSE)
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
# -------------------------------------------------------------------
data_2005 <- read_excel(
  "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/resultados_electorales_2005.xls",
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
# 5. RENAME columns 
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
# -------------------------------------------------------------------
data_2007 <- read_excel("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/Ayu_Seccion_2007.xlsx", sheet = 1, # adjust if needed
                        col_names = TRUE) %>%
  # "case(lower)" might suggest we convert colnames to lowercase:
  rename_with(tolower)

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
# 4. DESTRING 
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
# -------------------------------------------------------------------
data_2008 <- read_excel(
  "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/resultados_2008.xls",
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
months_data <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta")
names(months_data)

months_data <- months_data %>%
  filter(state == "MICHOACAN ", month == "April", year==2008) %>%
  select(lista, section)

# Now restore data_2008 and merge 1:1 on section
# Instead of preserve/restore, we'll just do a left_join in R:
data_2008 <- data_2008 %>%
  left_join(months_data, by="section") %>% 
  rename(listanominal = lista)

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
# 1. READ DTA
# -------------------------------------------------------------------
# Adjust file path to your local environment:
data_2011 <- read_dta("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/Other/Ayu_Seccion_2011.dta")
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
# -------------------------------------------------------------------
data_2012 <- read_excel(
  "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/Resultados_electorales_extraordinaria_2012.xlsx",
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
months_data <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta")

months_data <- months_data %>%
  filter(state == "MICHOACAN ", month == "July", year==2012) %>%
  select(lista, section)

# in R, we "restore" data_2012 and left_join
data_2012 <- data_2012 %>%
  left_join(months_data, by="section") %>% 
  rename( listanominal = lista)

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
### 2015
###############################################################################

data_2015 <- read_csv("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/Ayuntamientos_2015.csv")

data_2015 <- data_2015 %>%
  mutate(
    year = 2015,
    month= "July"
  )


###############################################################################
### 1. Import Excel extraordinary elections
###############################################################################
data_sahuayo <- read_excel(
  "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/SAHUAYO ELECCION EXTRAORDINARIA.xlsx",
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

sheet_list <- excel_sheets("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/Ayuntamientos_Mich_2018.xlsx")

for (sheetname in sheet_list) {
  message(paste("Processing sheet:", sheetname))
  
  # 1) Read the sheet
  df_sheet <- read_excel(
    "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/Ayuntamientos_Mich_2018.xlsx",
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
  file_out <- paste0(sheetname, ".dta")
  
  # 7) Save as .dta
  haven::write_dta(df_sheet, file_out)
  message(paste("Saved:", file_out))
}

###############################################################################
### PART X: CLEAR & APPEND MULTIPLE .DTA FILES IN R
###############################################################################

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
# Convert to numeric only if the column contains all numeric values (allowing for NAs)
df <- df %>%
  mutate(across(where(is.character), ~ {
    numeric_version <- suppressWarnings(as.numeric(.x))
    # If conversion creates NAs where there weren't NAs before, keep as character
    if (sum(is.na(numeric_version)) > sum(is.na(.x))) {
      .x  # Keep as character
    } else {
      numeric_version  # Convert to numeric
    }
  }))

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
### 4) collapse (sum) PAN-CI_2 NOREG NULO (first) coal*, by (municipality section uniqueid)
###    In R, let's define a group_by() and summarize().
###############################################################################
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
  mutate(valid = sum(c_across(c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","MORENA","PES","CI_1","CI_2")),
                     na.rm=TRUE)) %>%
  ungroup()

data_2018 <- data_2018 %>%
  mutate(
    total = valid + NULOS + NOREG
  ) %>%
  select(-NOREG, -NULOS)

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
ln2018 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ListadoNominalPREP2018.dta") %>%
  filter(STATE == "MICHOACAN")

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

#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

### Process the 2021 Michoacan data
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(fs)
library(writexl)

# Define the base path for Michoacán 2021 data
base_path <- "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/21/"

# Main processing function for Michoacán municipalities
process_michoacan_elections <- function(base_path) {
  
  # Step 1: Find all Excel files in the directory
  excel_files <- dir_ls(base_path, 
                        regexp = "\\.(xlsx|xls)$", 
                        type = "file") %>%
    # Filter out temporary Excel files (start with ~$)
    .[!str_detect(path_file(.), "^~\\$")]
  
  cat("Found", length(excel_files), "Excel files\n")
  
  # Step 2: Extract municipality names from filenames
  file_info <- tibble(
    file_path = excel_files,
    filename = path_file(file_path),
    # Extract municipality name from filename pattern "Ayuntamiento_MUNICIPALITY.xlsx"
    municipality_raw = str_remove(filename, "\\.(xlsx|xls)$") %>%
      str_remove("^Ayuntamiento_"),
    municipality_clean = str_to_upper(municipality_raw) %>%
      str_trim() %>%
      # Replace common variations
      str_replace_all("_", " ")
  )
  
  # Step 3: Preview the file structure
  cat("\nFile structure preview:\n")
  print(file_info %>% select(filename, municipality_clean) %>% head(10))
  
  # Step 4: Function to safely read each Excel file
  read_excel_safely <- function(file_path, municipality) {
    tryCatch({
      
      # Read the Excel file - read everything as text first to avoid type conflicts
      data <- read_excel(file_path, 
                         sheet = 1,
                         col_types = "text")
      
      # Skip if data is empty
      if(nrow(data) == 0) {
        cat("Warning: Empty file:", file_path, "\n")
        return(NULL)
      }
      
      # Add metadata columns
      data <- data %>%
        mutate(
          source_file = basename(file_path),
          municipality = municipality,
          .before = 1
        )
      
      return(data)
      
    }, error = function(e) {
      cat("Error reading file:", file_path, "\n")
      cat("Error message:", e$message, "\n")
      return(NULL)
    })
  }
  
  # Step 5: Process all files
  cat("\nProcessing files...\n")
  
  all_data <- file_info %>%
    mutate(
      data = map2(file_path, municipality_clean, read_excel_safely)
    ) %>%
    filter(!map_lgl(data, is.null))  # Remove failed reads
  
  # Step 6: Check column consistency before binding
  cat("\nChecking column consistency...\n")
  
  # Get all unique column names across files
  all_columns <- all_data %>%
    pull(data) %>%
    map(names) %>%
    unlist() %>%
    unique() %>%
    sort()
  
  cat("Total unique columns found:", length(all_columns), "\n")
  cat("First 15 columns:", paste(head(all_columns, 15), collapse = ", "), "\n")
  
  # Function to standardize columns across all datasets
  standardize_columns <- function(df, standard_cols) {
    # Add missing columns as NA
    missing_cols <- setdiff(standard_cols, names(df))
    for(col in missing_cols) {
      df[[col]] <- NA_character_
    }
    # Reorder columns to match standard order
    df[standard_cols]
  }
  
  # Step 7: Standardize and combine all data
  cat("\nStandardizing and combining data...\n")
  
  standardized_data <- all_data %>%
    mutate(
      data_std = map(data, ~standardize_columns(.x, all_columns))
    )
  
  combined_data <- standardized_data %>%
    pull(data_std) %>%
    bind_rows()
  
  cat("Successfully processed", nrow(all_data), "files\n")
  cat("Combined dataset has", nrow(combined_data), "rows and", ncol(combined_data), "columns\n")
  
  return(list(
    combined_data = combined_data,
    file_info = all_data,
    failed_files = file_info %>% 
      anti_join(all_data, by = "file_path") %>% 
      pull(file_path),
    column_summary = all_columns
  ))
}

# Function to inspect file structure before processing
inspect_michoacan_files <- function(base_path, n_files = 3) {
  
  excel_files <- dir_ls(base_path, regexp = "\\.(xlsx|xls)$") %>%
    .[!str_detect(path_file(.), "^~\\$")]
  
  cat("Inspecting first", n_files, "files:\n\n")
  
  for(i in 1:min(n_files, length(excel_files))) {
    cat("File", i, ":", path_file(excel_files[i]), "\n")
    
    tryCatch({
      # Get sheet names
      sheets <- excel_sheets(excel_files[i])
      cat("  Sheets:", paste(sheets, collapse = ", "), "\n")
      
      # Read first few rows
      sample_data <- read_excel(excel_files[i], sheet = 1, n_max = 5)
      cat("  Columns:", paste(names(sample_data), collapse = ", "), "\n")
      cat("  Dimensions:", nrow(sample_data), "x", ncol(sample_data), "\n")
      
      # Show a sample of data
      if(ncol(sample_data) > 0 && nrow(sample_data) > 0) {
        cat("  Sample values from first column:", 
            paste(sample_data[[1]][1:min(3, nrow(sample_data))], collapse = ", "), "\n")
      }
      cat("\n")
      
    }, error = function(e) {
      cat("  Error:", e$message, "\n\n")
    })
  }
}

# Function to clean and process specific columns after initial reading
post_process_michoacan <- function(combined_data) {
  
  # This function can be customized based on the actual column structure found
  processed_data <- combined_data
  
  # Convert numeric columns (adjust based on your actual column names)
  numeric_cols <- names(combined_data)[str_detect(names(combined_data), 
                                                  "VOTOS|TOTAL|CASILLA|SECCION")]
  
  for(col in numeric_cols) {
    if(col %in% names(processed_data)) {
      processed_data[[col]] <- as.numeric(processed_data[[col]])
    }
  }
  
  # Clean municipality names
  if("municipality" %in% names(processed_data)) {
    processed_data <- processed_data %>%
      mutate(municipality = str_to_upper(str_trim(municipality)))
  }
  
  return(processed_data)
}

# Execution plan
cat("EXECUTION PLAN FOR MICHOACÁN 2021 ELECTION DATA:\n")
cat(strrep("=", 50), "\n\n")

cat("Step 1: Inspect files\n")
cat("inspect_michoacan_files(base_path)\n\n")

cat("Step 2: Process all files\n")
cat("results <- process_michoacan_elections(base_path)\n\n")

cat("Step 3: Post-process and clean data\n")
cat("final_data <- post_process_michoacan(results$combined_data)\n\n")

cat("Step 4: Export results\n")
cat("write_csv(final_data, 'michoacan_2021_combined.csv')\n")
cat("write_xlsx(final_data, 'michoacan_2021_combined.xlsx')\n\n")

# Uncomment to run:
inspect_michoacan_files(base_path)
results <- process_michoacan_elections(base_path)
final_data <- post_process_michoacan(results$combined_data)


output_path <- "../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/21/"
write.csv(results$combined_data, paste0(output_path, "michoacan_2021_combined.csv"), row.names = FALSE)


# Load the 2021 dataset
data_2021 <- read_csv("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/21/michoacan_2021_combined.csv")

names(data_2021)

# Rename columns
data_2021 <- data_2021 %>%
  dplyr::rename(section = Sección,
                listanominal = "Lista Nominal",
                no_reg = "No registrados",
                nulos = Nulos) %>%
  rename_with(~ gsub("CI ", "CI_", .x), starts_with("CI")) %>%
  rename_with(~ gsub("-", "_", .x)) %>% 
  dplyr::mutate(
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(section)
  ) %>%
  mutate(
    municipality = case_when(
      municipality == "BRISEДAS" ~ "BRISENAS",
      municipality == "TINGБINDIN" ~ "TINGUINDIN",
      TRUE ~ municipality
    )
  ) %>% 
  filter(section > 0) %>% 
  select(
    "municipality", "section",
    "PAN", "PRI", "PRD", "PT", "PVEM", "MC", "MORENA",
    "FXM", "RSP", "PES",  "PAN_PRD", "PAN_PRI", "PAN_PRI_PRD",
    "PRI_PRD", "PT_MORENA",
    "CI_1", "CI_2", "CI_3", "CI_4", "CI_5", "CI_6", "CI_7", "CI_8", 
    "CI_10", "CI_11", "CI_12",
    "no_reg", "nulos", "listanominal"
  )

# Assign uniqueids
data_2021 <- data_2021 %>%
  mutate(uniqueid = case_when(
    municipality == "ACUITZIO" ~ 16001,
    municipality == "AGUILILLA" ~ 16002,
    municipality == "ALVARO OBREGON" ~ 16003,
    municipality == "ANGAMACUTIRO" ~ 16004,
    municipality == "ANGANGUEO" ~ 16005,
    municipality == "APATZINGAN" ~ 16006,
    municipality == "APORO" ~ 16007,
    municipality == "AQUILA" ~ 16008,
    municipality == "ARIO" ~ 16009,
    municipality == "ARTEAGA" ~ 16010,
    municipality == "BRISENAS" ~ 16011, 
    municipality == "BUENAVISTA" ~ 16012,
    municipality == "CARACUARO" ~ 16013,
    municipality == "CHARAPAN" ~ 16014,
    municipality == "CHARO" ~ 16015,
    municipality == "CHAVINDA" ~ 16016,
    municipality == "CHILCHOTA" ~ 16017,
    municipality == "CHINICUILA" ~ 16018,
    municipality == "CHUCANDIRO" ~ 16019,
    municipality == "CHURINTZIO" ~ 16020,
    municipality == "CHURUMUCO" ~ 16021,
    municipality == "COAHUAYANA" ~ 16022,
    municipality == "COALCOMAN DE VAZQUEZ PALLARES" ~ 16023,
    municipality == "COENEO" ~ 16024,
    municipality == "COJUMATLAN DE REGULES" ~ 16025,
    municipality == "CONTEPEC" ~ 16026,
    municipality == "COPANDARO" ~ 16027,
    municipality == "COTIJA" ~ 16028,
    municipality == "CUITZEO" ~ 16029,
    municipality == "ECUANDUREO" ~ 16030,
    municipality == "EPITACIO HUERTA" ~ 16031,
    municipality == "ERONGARICUARO" ~ 16032,
    municipality == "GABRIEL ZAMORA" ~ 16033,
    municipality == "HIDALGO" ~ 16034,
    municipality == "HUANDACAREO" ~ 16035,
    municipality == "HUANIQUEO" ~ 16036,
    municipality == "HUETAMO" ~ 16037,
    municipality == "HUIRAMBA" ~ 16038,
    municipality == "INDAPARAPEO" ~ 16039,
    municipality == "IRIMBO" ~ 16040,
    municipality == "IXTLAN" ~ 16041,
    municipality == "JACONA" ~ 16042,
    municipality == "JIMENEZ" ~ 16043,
    municipality == "JIQUILPAN" ~ 16044,
    municipality == "JOSE SIXTO VERDUZCO" ~ 16045,
    municipality == "JUAREZ" ~ 16046,
    municipality == "JUNGAPEO" ~ 16047,
    municipality == "LA HUACANA" ~ 16048,
    municipality == "LA PIEDAD" ~ 16049,
    municipality == "LAGUNILLAS" ~ 16050,
    municipality == "LAZARO CARDENAS" ~ 16051,
    municipality == "LOS REYES" ~ 16052,
    municipality == "MADERO" ~ 16053,
    municipality == "MARAVATIO" ~ 16054,
    municipality == "MARCOS CASTELLANOS" ~ 16055,
    municipality == "MORELIA" ~ 16056,
    municipality == "MORELOS" ~ 16057,
    municipality == "MUGICA" ~ 16058,
    municipality == "NAHUATZEN" ~ 16059,
    municipality == "NOCUPETARO" ~ 16060,
    municipality == "NUEVO PARANGARICUTIRO" ~ 16061,
    municipality == "NUEVO URECHO" ~ 16062,
    municipality == "NUMARAN" ~ 16063,
    municipality == "OCAMPO" ~ 16064,
    municipality == "PAJACUARAN" ~ 16065,
    municipality == "PANINDICUARO" ~ 16066,
    municipality == "PARACHO" ~ 16067,
    municipality == "PARACUARO" ~ 16068,
    municipality == "PATZCUARO" ~ 16069,
    municipality == "PENJAMILLO" ~ 16070,
    municipality == "PERIBAN" ~ 16071,
    municipality == "PUREPERO" ~ 16072,
    municipality == "PURUANDIRO" ~ 16073,
    municipality == "QUERENDARO" ~ 16074,
    municipality == "QUIROGA" ~ 16075,
    municipality == "SAHUAYO" ~ 16076,
    municipality == "SALVADOR ESCALANTE" ~ 16077,
    municipality == "SAN LUCAS" ~ 16078,
    municipality == "SANTA ANA MAYA" ~ 16079,
    municipality == "SENGUIO" ~ 16080,
    municipality == "SUSUPUATO" ~ 16081,
    municipality == "TACAMBARO" ~ 16082,
    municipality == "TANCITARO" ~ 16083,
    municipality == "TANGAMANDAPIO" ~ 16084,
    municipality == "TANGANCICUARO" ~ 16085,
    municipality == "TANHUATO" ~ 16086,
    municipality == "TARETAN" ~ 16087,
    municipality == "TARIMBARO" ~ 16088,
    municipality == "TEPALCATEPEC" ~ 16089,
    municipality == "TINGAMBATO" ~ 16090,
    municipality == "TINGUINDIN" ~ 16091,
    municipality == "TIQUICHEO DE NICOLAS ROMERO" ~ 16092,
    municipality == "TLALPUJAHUA" ~ 16093,
    municipality == "TLAZAZALCA" ~ 16094,
    municipality == "TOCUMBO" ~ 16095,
    municipality == "TUMBISCATIO" ~ 16096,
    municipality == "TURICATO" ~ 16097,
    municipality == "TUXPAN" ~ 16098,
    municipality == "TUZANTLA" ~ 16099,
    municipality == "TZINTZUNTZAN" ~ 16100,
    municipality == "TZITZIO" ~ 16101,
    municipality == "URUAPAN" ~ 16102,
    municipality == "VENUSTIANO CARRANZA" ~ 16103,
    municipality == "VILLAMAR" ~ 16104,
    municipality == "VISTA HERMOSA" ~ 16105,
    municipality == "YURECUARO" ~ 16106,
    municipality == "ZACAPU" ~ 16107,
    municipality == "ZAMORA" ~ 16108,
    municipality == "ZINAPARO" ~ 16109,
    municipality == "ZINAPECUARO" ~ 16110,
    municipality == "ZIRACUARETIRO" ~ 16111,
    municipality == "ZITACUARO" ~ 16112,
    TRUE ~ NA_integer_
  ))

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:listanominal), 
                          sum, na.rm = TRUE))

# Calculate valid votes and final details
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    total = sum(c_across(PAN:nulos), na.rm = TRUE),
    turnout = total/listanominal,
    valid = sum(c_across(PAN:CI_12), na.rm = TRUE),
    year = 2021,
    month = "June"
  )

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 16) %>% 
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
  
  # Helper: extract party names from a column name (handles all CC/CO variations)
  extract_parties <- function(col_name) {
    # Remove CC_ or CO_ prefix if present
    col_name <- sub("^(CC|CO)_", "", col_name)
    # Remove _CO or _CC suffix if present
    col_name <- sub("_(CO|CC)$", "", col_name)
    # Split by underscore
    strsplit(col_name, "_")[[1]]
  }
  
  # Helper: find columns belonging to a coalition
  get_coalition_cols <- function(coal_name) {
    coal_parties <- strsplit(coal_name, "_")[[1]]
    party_cols[sapply(party_cols, function(col) {
      col_parties <- extract_parties(col)
      # Check if all parties in the column match the coalition parties
      setequal(col_parties, coal_parties)
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
collapsed_2021 <- process_coalitions(collapsed_2021, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)


#####################################
### PROCESSING DATA FOR 2024 -------
#####################################

# Load the 2024 dataset
data_2024 <- read_excel("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/24/resultados_ayuntamientos_X_casilla.xlsx") %>% 
  dplyr::filter(Municipio != "Irimbo") %>% 
  mutate(Seccion = as.numeric(Seccion))

data_ext <- read_excel("../../../Data/Raw Electoral Data/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011,2015,2018,2021,2024/resultados_X_casilla_irimbo_2024.xlsx") %>% 
  mutate(Municipio = "IRIMBO")

names(data_2024)

data_2024 <- bind_rows(data_2024, data_ext)

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(municipality = Municipio,
                section = Seccion,
                listanominal = "Lista Nominal",
                total = TOTAL,
                no_reg = NOREGISTRADOS,
                nulos = NULOS,
                PES = PESM,
                PT_PVEM = "CO PT PVEM",      # CO = Coalición
                PT_MORENA = "CO PT MORENA", 
                PVEM_MORENA = "CO PVEM MORENA",
                PAN_PRI_PRD_CC = "CC PAN PRI PRD",  # CC = Candidatura Común
                PAN_PRI_CC = "CC PAN PRI",
                PAN_PRD_CC = "CC PAN PRD", 
                PRI_PRD_CC = "CC PRI PRD",
                PRD_PESM_CC = "CC PRD PESM",
                PT_PVEM_CC = "CC PT PVEM",
                PT_MORENA_CC = "CC PT MORENA",
                PT_PESM_CC = "CC PT PESM",
                PVEM_MORENA_CC = "CC PVEM MORENA",
                total = TOTAL) %>%
  rename_with(~ gsub("CI 0", "CI_", .x), starts_with("CI")) %>%
  rename_with(~ gsub("-", "_", .x)) %>% 
  dplyr::mutate(municipality = toupper(municipality)) %>% 
  dplyr::mutate(
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(section)
  ) %>% 
  filter(section > 0)

# Assign uniqueids
data_2024 <- data_2024 %>%
  mutate(uniqueid = case_when(
    municipality == "ACUITZIO" ~ 16001,
    municipality == "AGUILILLA" ~ 16002,
    municipality == "ALVARO OBREGON" ~ 16003,
    municipality == "ANGAMACUTIRO" ~ 16004,
    municipality == "ANGANGUEO" ~ 16005,
    municipality == "APATZINGAN" ~ 16006,
    municipality == "APORO" ~ 16007,
    municipality == "AQUILA" ~ 16008,
    municipality == "ARIO" ~ 16009,
    municipality == "ARTEAGA" ~ 16010,
    municipality == "BRISENAS" ~ 16011, 
    municipality == "BUENAVISTA" ~ 16012,
    municipality == "CARACUARO" ~ 16013,
    municipality == "CHARAPAN" ~ 16014,
    municipality == "CHARO" ~ 16015,
    municipality == "CHAVINDA" ~ 16016,
    municipality == "CHILCHOTA" ~ 16017,
    municipality == "CHINICUILA" ~ 16018,
    municipality == "CHUCANDIRO" ~ 16019,
    municipality == "CHURINTZIO" ~ 16020,
    municipality == "CHURUMUCO" ~ 16021,
    municipality == "COAHUAYANA" ~ 16022,
    municipality == "COALCOMAN DE VAZQUEZ PALLARES" ~ 16023,
    municipality == "COENEO" ~ 16024,
    municipality == "COJUMATLAN DE REGULES" ~ 16025,
    municipality == "CONTEPEC" ~ 16026,
    municipality == "COPANDARO" ~ 16027,
    municipality == "COTIJA" ~ 16028,
    municipality == "CUITZEO" ~ 16029,
    municipality == "ECUANDUREO" ~ 16030,
    municipality == "EPITACIO HUERTA" ~ 16031,
    municipality == "ERONGARICUARO" ~ 16032,
    municipality == "GABRIEL ZAMORA" ~ 16033,
    municipality == "HIDALGO" ~ 16034,
    municipality == "HUANDACAREO" ~ 16035,
    municipality == "HUANIQUEO" ~ 16036,
    municipality == "HUETAMO" ~ 16037,
    municipality == "HUIRAMBA" ~ 16038,
    municipality == "INDAPARAPEO" ~ 16039,
    municipality == "IRIMBO" ~ 16040,
    municipality == "IXTLAN" ~ 16041,
    municipality == "JACONA" ~ 16042,
    municipality == "JIMENEZ" ~ 16043,
    municipality == "JIQUILPAN" ~ 16044,
    municipality == "JOSE SIXTO VERDUZCO" ~ 16045,
    municipality == "JUAREZ" ~ 16046,
    municipality == "JUNGAPEO" ~ 16047,
    municipality == "LA HUACANA" ~ 16048,
    municipality == "LA PIEDAD" ~ 16049,
    municipality == "LAGUNILLAS" ~ 16050,
    municipality == "LAZARO CARDENAS" ~ 16051,
    municipality == "LOS REYES" ~ 16052,
    municipality == "MADERO" ~ 16053,
    municipality == "MARAVATIO" ~ 16054,
    municipality == "MARCOS CASTELLANOS" ~ 16055,
    municipality == "MORELIA" ~ 16056,
    municipality == "MORELOS" ~ 16057,
    municipality == "MUGICA" ~ 16058,
    municipality == "NAHUATZEN" ~ 16059,
    municipality == "NOCUPETARO" ~ 16060,
    municipality == "NUEVO PARANGARICUTIRO" ~ 16061,
    municipality == "NUEVO URECHO" ~ 16062,
    municipality == "NUMARAN" ~ 16063,
    municipality == "OCAMPO" ~ 16064,
    municipality == "PAJACUARAN" ~ 16065,
    municipality == "PANINDICUARO" ~ 16066,
    municipality == "PARACHO" ~ 16067,
    municipality == "PARACUARO" ~ 16068,
    municipality == "PATZCUARO" ~ 16069,
    municipality == "PENJAMILLO" ~ 16070,
    municipality == "PERIBAN" ~ 16071,
    municipality == "PUREPERO" ~ 16072,
    municipality == "PURUANDIRO" ~ 16073,
    municipality == "QUERENDARO" ~ 16074,
    municipality == "QUIROGA" ~ 16075,
    municipality == "SAHUAYO" ~ 16076,
    municipality == "SALVADOR ESCALANTE" ~ 16077,
    municipality == "SAN LUCAS" ~ 16078,
    municipality == "SANTA ANA MAYA" ~ 16079,
    municipality == "SENGUIO" ~ 16080,
    municipality == "SUSUPUATO" ~ 16081,
    municipality == "TACAMBARO" ~ 16082,
    municipality == "TANCITARO" ~ 16083,
    municipality == "TANGAMANDAPIO" ~ 16084,
    municipality == "TANGANCICUARO" ~ 16085,
    municipality == "TANHUATO" ~ 16086,
    municipality == "TARETAN" ~ 16087,
    municipality == "TARIMBARO" ~ 16088,
    municipality == "TEPALCATEPEC" ~ 16089,
    municipality == "TINGAMBATO" ~ 16090,
    municipality == "TINGUINDIN" ~ 16091,
    municipality == "TIQUICHEO DE NICOLAS ROMERO" ~ 16092,
    municipality == "TLALPUJAHUA" ~ 16093,
    municipality == "TLAZAZALCA" ~ 16094,
    municipality == "TOCUMBO" ~ 16095,
    municipality == "TUMBISCATIO" ~ 16096,
    municipality == "TURICATO" ~ 16097,
    municipality == "TUXPAN" ~ 16098,
    municipality == "TUZANTLA" ~ 16099,
    municipality == "TZINTZUNTZAN" ~ 16100,
    municipality == "TZITZIO" ~ 16101,
    municipality == "URUAPAN" ~ 16102,
    municipality == "VENUSTIANO CARRANZA" ~ 16103,
    municipality == "VILLAMAR" ~ 16104,
    municipality == "VISTA HERMOSA" ~ 16105,
    municipality == "YURECUARO" ~ 16106,
    municipality == "ZACAPU" ~ 16107,
    municipality == "ZAMORA" ~ 16108,
    municipality == "ZINAPARO" ~ 16109,
    municipality == "ZINAPECUARO" ~ 16110,
    municipality == "ZIRACUARETIRO" ~ 16111,
    municipality == "ZITACUARO" ~ 16112,
    TRUE ~ NA_integer_
  ))

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:total, listanominal), 
                          sum, na.rm = TRUE))

# Calculate valid votes and final details
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(PAN:PVEM_MORENA_CC), na.rm = TRUE),
    year = 2024,
    month = case_when(
      municipality == "IRIMBO" ~ "December",
      TRUE ~ "June"
    )
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)


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
                           data_2018,
                           collapsed_2021,
                           collapsed_2024)

data.table::fwrite(michoacan_all,"../../../Processed Data/michoacan/michoacan_process_raw_data.csv")

