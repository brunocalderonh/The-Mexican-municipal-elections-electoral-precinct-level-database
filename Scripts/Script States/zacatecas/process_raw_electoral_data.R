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

# Comprehensive uniqueid assignment for Zacatecas (58 municipalities)
assign_zacatecas_uniqueid <- function(municipality) {
  municipality <- toupper(remove_accents(municipality))
  case_when(
    grepl("^APOZOL$", municipality) ~ 32001,
    grepl("^APULCO$", municipality) ~ 32002,
    grepl("^ATOLINGA$", municipality) ~ 32003,
    grepl("BENITO JUAREZ", municipality) ~ 32004,
    grepl("^CALERA$", municipality) ~ 32005,
    grepl("CANITAS|CA.ITAS|ITAS DE F.*PESCADOR", municipality) ~ 32006,
    grepl("CONCEPCION DEL ORO", municipality) ~ 32007,
    grepl("CUAUHTEMOC|CUAUHT.MOC", municipality) ~ 32008,
    grepl("CHALCHIHUITES", municipality) ~ 32009,
    grepl("^FRESNILLO$", municipality) ~ 32010,
    grepl("GARCIA DE LA CADENA|TRINIDAD GARCIA", municipality) ~ 32011,
    grepl("GENARO CODINA", municipality) ~ 32012,
    grepl("ENRIQUE ESTRADA|GRAL.*ENRIQUE", municipality) ~ 32013,
    grepl("MURGUIA|GRAL.*FCO.*R.*MURGUIA|FRANCISCO R.*MURGUIA", municipality) ~ 32014,
    grepl("PLATEADO|JOAQUIN AMARO|EL PLATEADO", municipality) ~ 32015,
    grepl("PANFILO NATERA|GRAL.*PANFILO", municipality) ~ 32016,
    grepl("^GUADALUPE$", municipality) ~ 32017,
    grepl("^HUANUSCO$", municipality) ~ 32018,
    grepl("^JALPA$", municipality) ~ 32019,
    grepl("^JEREZ$", municipality) ~ 32020,
    grepl("JIMENEZ DEL TEUL|JMIMENEZ", municipality) ~ 32021,
    grepl("JUAN ALDAMA", municipality) ~ 32022,
    grepl("^JUCHIPILA$", municipality) ~ 32023,
    grepl("^LORETO$", municipality) ~ 32024,
    grepl("LUIS MOYA|LUISMOYA", municipality) ~ 32025,
    grepl("^MAZAPIL$", municipality) ~ 32026,
    grepl("MELCHOR OCAMPO", municipality) ~ 32027,
    grepl("MEZQUITAL DEL ORO", municipality) ~ 32028,
    grepl("MIGUEL AUZA", municipality) ~ 32029,
    grepl("^MOMAX$", municipality) ~ 32030,
    grepl("MONTE ESCOBEDO", municipality) ~ 32031,
    grepl("^MORELOS$", municipality) ~ 32032,
    grepl("MOYAHUA", municipality) ~ 32033,
    grepl("NOCHISTLAN", municipality) ~ 32034,
    grepl("NORIA DE ANGELES", municipality) ~ 32035,
    grepl("^OJOCALIENTE$", municipality) ~ 32036,
    grepl("^PANUCO$", municipality) ~ 32037,
    grepl("^PINOS$", municipality) ~ 32038,
    grepl("RIO GRANDE", municipality) ~ 32039,
    grepl("SAIN ALTO|SA.N ALTO", municipality) ~ 32040,
    grepl("^(EL )?SALVADOR", municipality) ~ 32041,
    grepl("^SOMBRERETE$", municipality) ~ 32042,
    grepl("SUSTICACAN", municipality) ~ 32043,
    grepl("^TABASCO$", municipality) ~ 32044,
    grepl("TEPECHITLAN", municipality) ~ 32045,
    grepl("^TEPETONGO$", municipality) ~ 32046,
    grepl("TEUL DE G.*ORTEGA", municipality) ~ 32047,
    grepl("TLALTENANGO", municipality) ~ 32048,
    grepl("VALPARAISO", municipality) ~ 32049,
    grepl("^VETAGRANDE$", municipality) ~ 32050,
    grepl("VILLA DE COS", municipality) ~ 32051,
    grepl("VILLA GARCIA", municipality) ~ 32052,
    grepl("VILLA GONZALEZ ORTEGA", municipality) ~ 32053,
    grepl("VILLA HIDALGO", municipality) ~ 32054,
    grepl("^VILLANUEVA$", municipality) ~ 32055,
    grepl("^ZACATECAS$", municipality) & !grepl("EXTRAORDINARIO", municipality) ~ 32056,
    grepl("^TRANCOSO$", municipality) ~ 32057,
    grepl("SANTA MARIA DE LA PAZ", municipality) ~ 32058,
    TRUE ~ NA_real_
  )
}

################################################################################
# 1) Read CSV (equivalent to: insheet using "Ayu_Seccion_1998_No_LN.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/1998/Ayu_Seccion_1998_No_LN.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns, drop rows if municipality=="" & section missing,
#    and drop rows if total is missing or zero.
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion,
    total        = totalemitida,    # rename totalemitida -> total
    noreg = "no\nregistrados"
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 3) Convert (pan through total) to numeric (like "destring pan - total , replace")
################################################################################
df <- df %>%
  mutate(across(pan:total, as.numeric))

################################################################################
# 4) Collapse (sum) pan - total, by(municipality section)
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(pan:total, sum, na.rm = TRUE), .groups = "drop")

################################################################################
# 5) Rename columns, then drop nulos, noreg if they exist
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT  = pt,
    PDP = pdp
  ) %>%
  select(-any_of(c("nulos", "noreg")))  # drop them if they exist

################################################################################
# 6) Create uniqueid=0, then assign code by municipality
#    (the big chain of "replace uniqueid=32001 if municipality=="APOZOL"" etc.)
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                                 ~ 32001,
      municipality == "APULCO"                                 ~ 32002,
      municipality == "ATOLINGA"                               ~ 32003,
      municipality == "BENITO JUAREZ"                          ~ 32004,
      municipality == "CALERA"                                 ~ 32005,
      municipality == "CAÑITAS DE F. PESCADOR"                 ~ 32006,
      # If partial match for "TAS DE F. PESCADOR"
      str_detect(municipality, "TAS DE F. PESCADOR")           ~ 32006,
      municipality == "CHALCHIHUITES"                          ~ 32009,
      municipality == "CONCEPCION DEL ORO"                     ~ 32007,
      municipality == "CUAUHTEMOC"                             ~ 32008,
      municipality == "GRAL. JOAQUIN AMARO"                    ~ 32015,
      municipality == "EL SALVADOR"                            ~ 32041,
      municipality == "FRESNILLO"                              ~ 32010,
      municipality == "GENARO CODINA"                          ~ 32012,
      municipality == "GRAL. ENRIQUE ESTRADA"                  ~ 32013,
      municipality == "GRAL. FCO. R. MURGUIA"                  ~ 32014,
      municipality == "GRAL. PANFILO NATERA"                   ~ 32016,
      municipality == "GUADALUPE"                              ~ 32017,
      municipality == "HUANUSCO"                               ~ 32018,
      municipality == "JALPA"                                  ~ 32019,
      municipality == "JEREZ"                                  ~ 32020,
      municipality == "JIMENEZ DEL TEUL"                       ~ 32021,
      municipality == "JUAN ALDAMA"                            ~ 32022,
      municipality == "JUCHIPILA"                              ~ 32023,
      municipality == "LORETO"                                 ~ 32024,
      municipality == "LUIS MOYA"                              ~ 32025,
      municipality == "MAZAPIL"                                ~ 32026,
      municipality == "MELCHOR OCAMPO"                         ~ 32027,
      municipality == "MEZQUITAL DEL ORO"                      ~ 32028,
      municipality == "MIGUEL AUZA"                            ~ 32029,
      municipality == "MOMAX"                                  ~ 32030,
      municipality == "MONTE ESCOBEDO"                         ~ 32031,
      municipality == "MORELOS"                                ~ 32032,
      municipality == "MOYAHUA"                                ~ 32033,
      municipality == "NOCHISTLAN"                             ~ 32034,
      municipality == "NORIA DE ANGELES"                       ~ 32035,
      municipality == "OJOCALIENTE"                            ~ 32036,
      municipality == "PANUCO"                                 ~ 32037,
      municipality == "PINOS"                                  ~ 32038,
      municipality == "RIO GRANDE"                             ~ 32039,
      municipality == "SAIN ALTO"                              ~ 32040,
      municipality == "SOMBRERETE"                             ~ 32042,
      municipality == "SUSTICACAN"                             ~ 32043,
      municipality == "TABASCO"                                ~ 32044,
      municipality == "TEPECHITLAN"                            ~ 32045,
      municipality == "TEPETONGO"                              ~ 32046,
      municipality == "TEUL DE GONZALEZ ORTEGA"                ~ 32047,
      municipality == "TLALTENANGO"                            ~ 32048,
      municipality == "GARCIA DE LA CADENA"                    ~ 32011,
      municipality == "VALPARAISO"                             ~ 32049,
      municipality == "VETAGRANDE"                             ~ 32050,
      municipality == "VILLA DE COS"                           ~ 32051,
      municipality == "VILLA GARCIA"                           ~ 32052,
      municipality == "VILLA GONZALEZ ORTEGA"                  ~ 32053,
      municipality == "VILLA HIDALGO"                          ~ 32054,
      municipality == "VILLANUEVA"                             ~ 32055,
      municipality == "ZACATECAS"                              ~ 32056,
      TRUE                                                    ~ 0
    )
  )

################################################################################
# 7) Create 'valid' = rowtotal(PAN PRI PRD PT PDP)
################################################################################
# Create valid column
df_collapsed <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PRD, PT, PDP),
      na.rm = TRUE
    )
  )

################################################################################
# 9) Merge with external data "all_months_years.dta" on ed=32, seccion=section,
#    keep if month==6 & year==1998, drop unmatched merges
################################################################################

# Read the external data
df_all_1 <- tryCatch({
  read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") 
}, error = function(e) NULL)

# Define merge function
merge_fn <- function(df_main, df_all) {
  df_all_filtered <- df_all %>% 
    select(state, section, month, year, lista) %>%
    filter(month == "June", year == 1998, state == "ZACATECAS")  # Changed = to ==
  
  df_merge <- df_main %>%
    left_join(df_all_filtered, by = "section")  # Fixed join syntax
  
  return(df_merge)
}

# Execute the merge if data exists
if (!is.null(df_all_1)) {
  df_merged <- merge_fn(df_collapsed, df_all_1)
}

# drop unmatched. In a left_join, we can remove rows
# that have NA in 'lista' (the merged column)
df_merged <- df_merged %>% filter(!is.na(lista))

# drop _merge, ed, seccion, year, month
# (In R we didn't create _merge, so no need to drop. Just remove ed, seccion, year, month.)
df_merged <- df_merged %>%
  select(-state, -year, -month)

# rename lista -> listanominal
df_merged <- df_merged %>%
  rename(listanominal = lista)

################################################################################
# 10) turnout = total / listanominal, year=1998, month="July"
################################################################################
df_1998 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 1998,
    month   = "July"
  )

################################################################################
# 1) Read CSV (Equivalent to: insheet using "Ayu_Seccion_2001_No_LN.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2001/Ayu_Seccion_2001_No_LN.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns, remove rows if municipality = "" & section is NA,
#    or if total is NA or zero.
#    drop if municipality=="" & section==.
#            drop if total==. | total==0
################################################################################
df <- df %>%
  rename(
    municipality   = municipio,
    section        = seccion,
    noregistrados  = cnr,
    nulos          = vn,
    total          = "votacion\nemitida"
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 3) Convert (pan through total) to numeric (like "destring pan - total, replace")
################################################################################
df <- df %>%
  mutate(across(pan:total, as.numeric))

################################################################################
# 4) Collapse (sum) pan - total by (municipality, section)
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(pan:total, sum, na.rm = TRUE), .groups = "drop")

################################################################################
# 5) Rename columns (pan->PAN, pri->PRI, prd->PRD, etc.)
#    Then drop noregistrados, nulos
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN  = pan,
    PRI  = pri,
    PRD  = prd,
    PT   = pt,
    PVEM = pvem,
    PAS  = pas,
    PC   = cdppn,  # cdppn -> PC
    PNS  = pns
  ) %>%
  select(-any_of(c("noregistrados", "nulos")))

################################################################################
# 6) Create uniqueid=0, then replace for each municipality
#    This large chain of "replace uniqueid=XXXXX if municipality == '...'".
#    We'll do it via case_when plus str_detect for partial matches (e.g. "CAÑITAS").
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                ~ 32001,
      municipality == "APULCO"               ~ 32002,
      municipality == "ATOLINGA"             ~ 32003,
      municipality == "BENITO JUAREZ"        ~ 32004,
      municipality == "CALERA"               ~ 32005,
      municipality == "CAÑITAS"              ~ 32006,
      # If partial match for "CA" and "ITAS" within the string
      str_detect(municipality, "CA") & str_detect(municipality, "ITAS") ~ 32006,
      municipality == "CHALCHIHUITES"        ~ 32009,
      municipality == "CONCEPCION DEL ORO"   ~ 32007,
      municipality == "CUAUHTEMOC"           ~ 32008,
      municipality == "EL PLATEADO"          ~ 32015,
      municipality == "EL SALVADOR"          ~ 32041,
      municipality == "FRESNILLO"            ~ 32010,
      municipality == "GENARO CODINA"        ~ 32012,
      municipality == "GRAL. ENRIQUE ESTRADA"~ 32013,
      municipality == "GRAL FCO R MURGUIA"   ~ 32014,
      municipality == "GRAL. PANFILO NATERA" ~ 32016,
      municipality == "GUADALUPE"            ~ 32017,
      municipality == "HUANUSCO"             ~ 32018,
      municipality == "JALPA"                ~ 32019,
      municipality == "JEREZ"                ~ 32020,
      municipality == "JMIMENEZ DEL TEUL"    ~ 32021,
      municipality == "JUAN ALDAMA"          ~ 32022,
      municipality == "JUCHIPILA"            ~ 32023,
      municipality == "LORETO"               ~ 32024,
      municipality == "LUIS MOYA"            ~ 32025,
      municipality == "MAZAPIL"              ~ 32026,
      municipality == "MELCHOR OCAMPO"       ~ 32027,
      municipality == "MEZQUITAL DEL ORO"    ~ 32028,
      municipality == "MIGUEL AUZA"          ~ 32029,
      municipality == "MOMAX"                ~ 32030,
      municipality == "MONTE ESCOBEDO"       ~ 32031,
      municipality == "MORELOS"              ~ 32032,
      municipality == "MOYAHUA"              ~ 32033,
      municipality == "NOCHISTLAN"           ~ 32034,
      municipality == "NORIA DE ANGELES"     ~ 32035,
      municipality == "OJOCALIENTE"          ~ 32036,
      municipality == "PANUCO"               ~ 32037,
      municipality == "PINOS"                ~ 32038,
      municipality == "RIO GRANDE"           ~ 32039,
      municipality == "SAIN ALTO"            ~ 32040,
      municipality == "SOMBRERETE"           ~ 32042,
      municipality == "SUSTICACAN"           ~ 32043,
      municipality == "TABASCO"              ~ 32044,
      municipality == "TEPECHITLAN"          ~ 32045,
      municipality == "TEPETONGO"            ~ 32046,
      municipality == "TEUL DE GLEZ. ORTEGA" ~ 32047,
      municipality == "TLALTENANGO"          ~ 32048,
      municipality == "TRANCOSO"             ~ 32057,
      municipality == "GARCIA DE LA CADENA"  ~ 32011,
      municipality == "VALPARAISO"           ~ 32049,
      municipality == "VETAGRANDE"           ~ 32050,
      municipality == "VILLA DE COS"         ~ 32051,
      municipality == "VILLA GARCIA"         ~ 32052,
      municipality == "VILLA GONZALEZ ORTEGA"~ 32053,
      municipality == "VILLA HIDALGO"        ~ 32054,
      municipality == "VILLANUEVA"           ~ 32055,
      municipality == "ZACATECAS"            ~ 32056,
      TRUE                                   ~ 0
    )
  )

################################################################################
# 7) Create 'valid' = rowtotal(PAN PRI PRD PT PVEM PNS PAS PC),
#    set ed=32, seccion=section for merging
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    valid   = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PNS, PAS, PC), na.rm = TRUE)
  )

################################################################################
# 8) Merge with external "all_months_years.dta" (two attempts), keep if month==6
#    & year==2001, drop if _merge==2, rename lista->listanominal
################################################################################
# We'll replicate your two merges with capture. In R, we'll attempt reading both
# paths and do a left_join. If a file isn't found, skip that merge.

# Read the external data
df_all_1 <- tryCatch({
  read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") 
}, error = function(e) NULL)

# Define merge function
merge_fn <- function(df_main, df_all) {
  df_all_filtered <- df_all %>% 
    select(state, section, month, year, lista) %>%
    filter(month == "June", year == 2001, state == "ZACATECAS")  # Changed = to ==
  
  df_merge <- df_main %>%
    left_join(df_all_filtered, by = "section")  # Fixed join syntax
  
  return(df_merge)
}

# Execute the merge if data exists
if (!is.null(df_all_1)) {
  df_merged <- merge_fn(df_collapsed, df_all_1)
}

# drop unmatched. In a left_join, we can remove rows
# that have NA in 'lista' (the merged column)
df_merged <- df_merged %>% filter(!is.na(lista))

# drop _merge, ed, seccion, year, month
# (In R we didn't create _merge, so no need to drop. Just remove ed, seccion, year, month.)
df_merged <- df_merged %>%
  select(-state, -year, -month)

# rename lista -> listanominal
df_merged <- df_merged %>%
  rename(listanominal = lista)

################################################################################
# 9) turnout = total / listanominal, year=2001, month="July"
################################################################################
df_2001 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2001,
    month   = "July"
  )

################################################################################
# 1) Read the CSV (Equivalent to: insheet using "Ayu_Seccion_2004.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2004/Ayu_Seccion_2004.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns: nombre_municipio -> municipality, seccion -> section,
#    lista_nominal -> listanominal
################################################################################
df <- df %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  )

################################################################################
# 3) Drop rows where municipality == "" & section is missing
#    or total is missing/zero
################################################################################
df <- df %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 4) Convert listanominal, pan through total to numeric 
#    (like destring listanominal pan - total , replace)
################################################################################
df <- df %>%
  mutate(across(c(listanominal, pan:total), as.numeric))

################################################################################
# 5) Collapse (sum) listanominal, pan - total by (municipality, section)
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    listanominal = sum(listanominal, na.rm = TRUE),
    across(pan:total, sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 6) Rename columns 
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN        = pan,
    PRI        = pri,
    PRI_PT_PVEM= priptpvem,
    PRD        = prd,
    PT         = pt,
    PVEM       = pvem,
    PC         = pc
  )

################################################################################
# 7) Compute turnout = total / listanominal
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

################################################################################
# 8) Drop nulos if it exists
################################################################################
df_collapsed <- df_collapsed %>%
  select(-any_of("nulos"))

################################################################################
# 9) Create uniqueid=0, then assign codes for each municipality 
#    (long chain of "replace uniqueid=XXXXX if municipality=='...'")
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                                  ~ 32001,
      municipality == "APULCO"                                  ~ 32002,
      municipality == "ATOLINGA"                                ~ 32003,
      municipality == "BENITO JUAREZ"                           ~ 32004,
      municipality == "CALERA"                                  ~ 32005,
      municipality == "CA?ITAS DE FELIPE PESCADOR"              ~ 32006,
      municipality == "CHALCHIHUITES"                           ~ 32009,
      municipality == "CONCEPCION DEL ORO"                      ~ 32007,
      municipality == "CUAUHTEMOC"                              ~ 32008,
      municipality == "PLATEADO DE JOAQUIN AMARO EL"            ~ 32015,
      municipality == "SALVADOR EL"                             ~ 32041,
      municipality == "FRESNILLO"                               ~ 32010,
      municipality == "GENARO CODINA"                           ~ 32012,
      municipality == "GENERAL ENRIQUE ESTRADA"                 ~ 32013,
      municipality == "GENERAL FRANCISCO R. MURGUIA"            ~ 32014,
      municipality == "GENERAL PANFILO NATERA"                  ~ 32016,
      municipality == "GUADALUPE"                               ~ 32017,
      municipality == "HUANUSCO"                                ~ 32018,
      municipality == "JALPA"                                   ~ 32019,
      municipality == "JEREZ"                                   ~ 32020,
      municipality == "JIMENEZ DEL TEUL"                        ~ 32021,
      municipality == "JUAN ALDAMA"                             ~ 32022,
      municipality == "JUCHIPILA"                               ~ 32023,
      municipality == "LORETO"                                  ~ 32024,
      municipality == "LUIS MOYA"                               ~ 32025,
      municipality == "MAZAPIL"                                 ~ 32026,
      municipality == "MELCHOR OCAMPO"                          ~ 32027,
      municipality == "MEZQUITAL DEL ORO"                       ~ 32028,
      municipality == "MIGUEL AUZA"                             ~ 32029,
      municipality == "MOMAX"                                   ~ 32030,
      municipality == "MONTE ESCOBEDO"                          ~ 32031,
      municipality == "MORELOS"                                 ~ 32032,
      municipality == "MOYAHUA DE ESTRADA"                      ~ 32033,
      municipality == "NOCHISTLAN DE MEJIA"                     ~ 32034,
      municipality == "NORIA DE ANGELES"                        ~ 32035,
      municipality == "OJOCALIENTE"                             ~ 32036,
      municipality == "PANUCO"                                  ~ 32037,
      municipality == "PINOS"                                   ~ 32038,
      municipality == "RIO GRANDE"                              ~ 32039,
      municipality == "SAIN ALTO"                               ~ 32040,
      municipality == "SOMBRERETE"                              ~ 32042,
      municipality == "SUSTICACAN"                              ~ 32043,
      municipality == "TABASCO"                                 ~ 32044,
      municipality == "TEPECHITLAN"                             ~ 32045,
      municipality == "TEPETONGO"                               ~ 32046,
      municipality == "TEUL DE GONZALEZ ORTEGA"                 ~ 32047,
      municipality == "TLALTENANGO DE SANCHEZ ROMAN"            ~ 32048,
      municipality == "TRANCOSO"                                ~ 32057,
      municipality == "TRINIDAD GARCIA DE LA CADENA"            ~ 32011,
      municipality == "VALPARAISO"                              ~ 32049,
      municipality == "VETAGRANDE"                              ~ 32050,
      municipality == "VILLA DE COS"                            ~ 32051,
      municipality == "VILLA GARCIA"                            ~ 32052,
      municipality == "VILLA GONZALEZ ORTEGA"                   ~ 32053,
      municipality == "VILLA HIDALGO"                           ~ 32054,
      municipality == "VILLANUEVA"                              ~ 32055,
      municipality == "ZACATECAS"                               ~ 32056,
      TRUE                                                      ~ 0
    )
  )

################################################################################
# 10) Compute valid = rowtotal(PAN PRI_PT_PVEM PRI PRD PT PVEM PC),
#    then year=2004, month="July"
################################################################################
df_2004 <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI_PT_PVEM, PRI, PRD, PT, PVEM, PC),
      na.rm = TRUE
    ),
    year  = 2004,
    month = "July"
  )

###############################################################################
# 1) Read CSV (Equivalent to: insheet using "Ayu_Seccion_2007.csv", clear)
###############################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2007/Ayu_Seccion_2007.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
###############################################################################
# 2) Rename columns, drop rows where municipality=="" & section missing
#    or total is missing/zero
###############################################################################
df <- df %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

###############################################################################
# 3) Convert listanominal, pan through total to numeric (like destring)
###############################################################################
df <- df %>%
  mutate(across(c(listanominal, pan:total), as.numeric))

###############################################################################
# 4) Collapse (sum) listanominal, pan - total by municipality, section
#    (: collapse (sum) listanominal pan - total , by (municipality section))
###############################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    listanominal = sum(listanominal, na.rm = TRUE),
    across(pan:total, sum, na.rm = TRUE),
    .groups = "drop"
  )

###############################################################################
# 5) Rename columns to final forms (rename pan->PAN, pri->PRI, etc.)
###############################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN    = pan,
    PRI    = pri,
    PRD_PC = prdpc,
    PT     = pt,
    PVEM   = pvem,
    PANAL  = panal,
    PAS    = pas
  )

###############################################################################
# 6) Compute turnout = total / listanominal
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

###############################################################################
# 7) Drop nulos if it exists
###############################################################################
df_collapsed <- df_collapsed %>%
  select(-any_of("nulos"))

###############################################################################
# 8) Create uniqueid = 0, then assign codes based on municipality
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                                  ~ 32001,
      municipality == "APULCO"                                  ~ 32002,
      municipality == "ATOLINGA"                                ~ 32003,
      municipality == "BENITO JUAREZ"                           ~ 32004,
      municipality == "CALERA"                                  ~ 32005,
      municipality == "CA?ITAS DE FELIPE PESCADOR"              ~ 32006,
      municipality == "CHALCHIHUITES"                           ~ 32009,
      municipality == "CONCEPCION DEL ORO"                      ~ 32007,
      municipality == "CUAUHTEMOC"                              ~ 32008,
      municipality == "PLATEADO DE JOAQUIN AMARO EL"            ~ 32015,
      municipality == "SALVADOR EL"                             ~ 32041,
      municipality == "FRESNILLO"                               ~ 32010,
      municipality == "GENARO CODINA"                           ~ 32012,
      municipality == "GENERAL ENRIQUE ESTRADA"                 ~ 32013,
      municipality == "GENERAL FRANCISCO R MURGUIA"             ~ 32014,
      municipality == "GENERAL PANFILO NATERA"                  ~ 32016,
      municipality == "GUADALUPE"                               ~ 32017,
      municipality == "HUANUSCO"                                ~ 32018,
      municipality == "JALPA"                                   ~ 32019,
      municipality == "JEREZ"                                   ~ 32020,
      municipality == "JIMENEZ DEL TEUL"                        ~ 32021,
      municipality == "JUAN ALDAMA"                             ~ 32022,
      municipality == "JUCHIPILA"                               ~ 32023,
      municipality == "LORETO"                                  ~ 32024,
      municipality == "LUIS MOYA"                               ~ 32025,
      municipality == "MAZAPIL"                                 ~ 32026,
      municipality == "MELCHOR OCAMPO"                          ~ 32027,
      municipality == "MEZQUITAL DEL ORO"                       ~ 32028,
      municipality == "MIGUEL AUZA"                             ~ 32029,
      municipality == "MOMAX"                                   ~ 32030,
      municipality == "MONTE ESCOBEDO"                          ~ 32031,
      municipality == "MORELOS"                                 ~ 32032,
      municipality == "MOYAHUA DE ESTRADA"                      ~ 32033,
      municipality == "NOCHISTLAN DE MEJIA"                     ~ 32034,
      municipality == "NORIA DE ANGELES"                        ~ 32035,
      municipality == "OJOCALIENTE"                             ~ 32036,
      municipality == "PANUCO"                                  ~ 32037,
      municipality == "PINOS"                                   ~ 32038,
      municipality == "RIO GRANDE"                              ~ 32039,
      municipality == "SAIN ALTO"                               ~ 32040,
      municipality == "SANTA MARIA DE LA PAZ"                   ~ 32058,
      municipality == "SOMBRERETE"                              ~ 32042,
      municipality == "SUSTICACAN"                              ~ 32043,
      municipality == "TABASCO"                                 ~ 32044,
      municipality == "TEPECHITLAN"                             ~ 32045,
      municipality == "TEPETONGO"                               ~ 32046,
      municipality == "TEUL DE GONZALEZ ORTEGA"                 ~ 32047,
      municipality == "TLALTENANGO DE SANCHEZ ROMAN"            ~ 32048,
      municipality == "TRANCOSO"                                ~ 32057,
      municipality == "TRINIDAD GARCIA DE LA CADENA"            ~ 32011,
      municipality == "VALPARAISO"                              ~ 32049,
      municipality == "VETAGRANDE"                              ~ 32050,
      municipality == "VILLA DE COS"                            ~ 32051,
      municipality == "VILLA GARCIA"                            ~ 32052,
      municipality == "VILLA GONZALEZ ORTEGA"                   ~ 32053,
      municipality == "VILLA HIDALGO"                           ~ 32054,
      municipality == "VILLANUEVA"                              ~ 32055,
      municipality == "ZACATECAS"                               ~ 32056,
      TRUE                                                      ~ 0
    )
  )

###############################################################################
# 9) Compute valid = rowtotal(PAN PRI PRD_PC PT PVEM PANAL PAS), year=2007,
#    month="July". Then sort by section.
###############################################################################
df_2007 <- df_collapsed %>%
  mutate(
    valid = rowSums(select(., PAN, PRI, PRD_PC, PT, PVEM, PANAL, PAS), na.rm = TRUE),
    year  = 2007,
    month = "July"
  ) %>%
  arrange(section)

################################################################################
# 1) Read CSV (Equivalent to: insheet using "Ayu_Seccion_2010.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2010/Ayu_Seccion_2010.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns and drop rows where municipality == "" & section == .
#    or total is missing/zero
################################################################################
df <- df %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 3) Convert listanominal, pan through total to numeric 
################################################################################
df <- df %>%
  mutate(across(c(listanominal, pan:total), as.numeric))

################################################################################
# 4) Collapse (sum) listanominal, pan - total by (municipality, section)
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    listanominal = sum(listanominal, na.rm = TRUE),
    across(pan:total, sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 5) Handling the PAN+PT coalition columns:
#    - Create a dummy at the section level: sec_dummy_panpt = 1 if panpt>0 else 0
#    - By municipality, find if we have any section with panpt>0 => dummy_panpt
#    - If dummy_panpt==1 for that municipality, 
#      then pan_pt = panpt + pan + pt, and set pan=0, pt=0
################################################################################

# Create a local column to check if panpt>0
df_collapsed <- df_collapsed %>%
  mutate(sec_dummy_panpt = if_else(panpt > 0, 1, 0, missing = 0))

# By municipality: we find max of sec_dummy_panpt => that becomes dummy_panpt
df_collapsed <- df_collapsed %>%
  group_by(municipality) %>%
  mutate(dummy_panpt = max(sec_dummy_panpt, na.rm = TRUE)) %>%
  ungroup()

# If dummy_panpt==1, then pan_pt = panpt + pan + pt
# and set pan=0, pt=0. Then drop the old columns
df_collapsed <- df_collapsed %>%
  mutate(
    pan_pt = if_else(dummy_panpt == 1, coalesce(panpt, 0) + coalesce(pan, 0) + coalesce(pt, 0), panpt)
  ) %>%
  mutate(
    pan = if_else(dummy_panpt == 1, 0, pan),
    pt  = if_else(dummy_panpt == 1, 0, pt)
  ) %>%
  select(-panpt, -sec_dummy_panpt, -dummy_panpt)

################################################################################
# 6) Rename columns to final forms
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN                = pan,
    PAN_PT            = pan_pt,
    PRI_PVEM_PANAL    = pripvempanal,
    PRD_PC            = prdpc,
    PT                = pt
  )

################################################################################
# 7) Compute turnout = total / listanominal
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

################################################################################
# 8) Drop 'nulos' if it exists
################################################################################
df_collapsed <- df_collapsed %>%
  select(-any_of("nulos"))

################################################################################
# 9) Create uniqueid=0, then assign codes based on municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                                  ~ 32001,
      municipality == "APULCO"                                  ~ 32002,
      municipality == "ATOLINGA"                                ~ 32003,
      municipality == "BENITO JUAREZ"                           ~ 32004,
      municipality == "CALERA"                                  ~ 32005,
      municipality == "CA?ITAS DE FELIPE PESCADOR"              ~ 32006,
      municipality == "CHALCHIHUITES"                           ~ 32009,
      municipality == "CONCEPCION DEL ORO"                      ~ 32007,
      municipality == "CUAUHTEMOC"                              ~ 32008,
      municipality == "PLATEADO DE JOAQUIN AMARO EL"            ~ 32015,
      municipality == "SALVADOR EL"                             ~ 32041,
      municipality == "FRESNILLO"                               ~ 32010,
      municipality == "GENARO CODINA"                           ~ 32012,
      municipality == "GENERAL ENRIQUE ESTRADA"                 ~ 32013,
      municipality == "GENERAL FRANCISCO R MURGUIA"             ~ 32014,
      municipality == "GENERAL PANFILO NATERA"                  ~ 32016,
      municipality == "GUADALUPE"                               ~ 32017,
      municipality == "HUANUSCO"                                ~ 32018,
      municipality == "JALPA"                                   ~ 32019,
      municipality == "JEREZ"                                   ~ 32020,
      municipality == "JIMENEZ DEL TEUL"                        ~ 32021,
      municipality == "JUAN ALDAMA"                             ~ 32022,
      municipality == "JUCHIPILA"                               ~ 32023,
      municipality == "LORETO"                                  ~ 32024,
      municipality == "LUIS MOYA"                               ~ 32025,
      municipality == "MAZAPIL"                                 ~ 32026,
      municipality == "MELCHOR OCAMPO"                          ~ 32027,
      municipality == "MEZQUITAL DEL ORO"                       ~ 32028,
      municipality == "MIGUEL AUZA"                             ~ 32029,
      municipality == "MOMAX"                                   ~ 32030,
      municipality == "MONTE ESCOBEDO"                          ~ 32031,
      municipality == "MORELOS"                                 ~ 32032,
      municipality == "MOYAHUA DE ESTRADA"                      ~ 32033,
      municipality == "NOCHISTLAN DE MEJIA"                     ~ 32034,
      municipality == "NORIA DE ANGELES"                        ~ 32035,
      municipality == "OJOCALIENTE"                             ~ 32036,
      municipality == "PANUCO"                                  ~ 32037,
      municipality == "PINOS"                                   ~ 32038,
      municipality == "RIO GRANDE"                              ~ 32039,
      municipality == "SAIN ALTO"                               ~ 32040,
      municipality == "SANTA MARIA DE LA PAZ"                   ~ 32058,
      municipality == "SOMBRERETE"                              ~ 32042,
      municipality == "SUSTICACAN"                              ~ 32043,
      municipality == "TABASCO"                                 ~ 32044,
      municipality == "TEPECHITLAN"                             ~ 32045,
      municipality == "TEPETONGO"                               ~ 32046,
      municipality == "TEUL DE GONZALEZ ORTEGA"                 ~ 32047,
      municipality == "TLALTENANGO DE SANCHEZ ROMAN"            ~ 32048,
      municipality == "TRANCOSO"                                ~ 32057,
      municipality == "TRINIDAD GARCIA DE LA CADENA"            ~ 32011,
      municipality == "VALPARAISO"                              ~ 32049,
      municipality == "VETAGRANDE"                              ~ 32050,
      municipality == "VILLA DE COS"                            ~ 32051,
      municipality == "VILLA GARCIA"                            ~ 32052,
      municipality == "VILLA GONZALEZ ORTEGA"                   ~ 32053,
      municipality == "VILLA HIDALGO"                           ~ 32054,
      municipality == "VILLANUEVA"                              ~ 32055,
      municipality == "ZACATECAS"                               ~ 32056,
      TRUE                                                      ~ 0
    )
  )

################################################################################
# 10) Compute valid = rowtotal(PAN PAN_PT PRI_PVEM_PANAL PRD_PC PT), then 
#    year=2010, month="July", sort by section, and save
################################################################################
df_2010 <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PAN_PT, PRI_PVEM_PANAL, PRD_PC, PT),
      na.rm = TRUE
    ),
    year  = 2010,
    month = "July"
  ) %>%
  arrange(section)

################################################################################
# 1) Read Excel
################################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2013/CASILLAS_AYUNTAMIENTOS_2013.xlsx",
  sheet = "CASILLAS_AYUNTAMIENTOS_2013",
  col_names = TRUE
) %>%
  as.data.frame()
# Remove "-" and spaces
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns, drop rows where municipality=="" & section missing
################################################################################
df <- df %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality == "" & is.na(section)))

################################################################################
# 3) Combine PAN_PRD = PAN + PRD + PANPRD, then drop old columns
#    rename MC -> PC
################################################################################
df <- df %>%
  mutate(
    PAN_PRD = coalesce(PAN, 0) + coalesce(PRD, 0) + coalesce(PANPRD, 0)
  ) %>%
  select(-PAN, -PRD, -PANPRD) %>%
  rename(PC = MC)

################################################################################
# 4) Generate 'total' = rowtotal(...) including 
#    (PRI PT PVEM PC PANAL PAN_PRD MinorParty1 .. MinorParty9 Nulos)
################################################################################
# We'll assume these columns exist in your dataset. 
# "drop if total==. | total==0" means we drop rows after we find total=0 or NA.
df <- df %>%
  rowwise() %>%
  mutate(
    total = sum(
      c_across(c(PRI, PT, PVEM, PC, PANAL, PAN_PRD,
                 MinorParty1, MinorParty2, MinorParty3,
                 MinorParty4, MinorParty5, MinorParty6,
                 MinorParty7, MinorParty8, MinorParty9, Nulos)),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 5) Destring numeric columns (listanominal, PRI ... total)
################################################################################
# We'll convert relevant columns to numeric if needed
num_cols <- c("listanominal", "PRI", "PT", "PVEM", "PC", "PANAL",
              "PAN_PRD", "MinorParty1", "MinorParty2", "MinorParty3",
              "MinorParty4", "MinorParty5", "MinorParty6", "MinorParty7",
              "MinorParty8", "MinorParty9", "Nulos", "total")
df <- df %>%
  mutate(across(all_of(intersect(num_cols, names(df))), as.numeric))

################################################################################
# 6) Collapse (sum) listanominal, PRI - total by (municipality, section)
################################################################################
collapse_vars <- df %>%
  select(c(listanominal, PRI:total)) %>%  # from listanominal through total
  names()

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    across(all_of(collapse_vars), sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 7) turnout = total/listanominal
#    drop Nulos if present 
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal) %>%
  select(-any_of("Nulos"))

################################################################################
# 8) Create uniqueid=0, then assign municipality codes 
#    (the long chain of "replace uniqueid=XXXX if municipality=='...'")
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                                    ~ 32001,
      municipality == "APULCO"                                    ~ 32002,
      municipality == "ATOLINGA"                                  ~ 32003,
      municipality == "BENITO JUAREZ"                             ~ 32004,
      municipality == "CALERA"                                    ~ 32005,
      municipality == "CA+ITAS DE FELIPE PESCADOR"                ~ 32006,
      # partial match if "ITAS DE FELIPE PESCADOR" is in municipality
      str_detect(municipality, "ITAS DE FELIPE PESCADOR")         ~ 32006,
      municipality == "CHALCHIHUITES"                             ~ 32009,
      municipality == "CONCEPCION DEL ORO"                        ~ 32007,
      municipality == "CUAUHTEMOC"                                ~ 32008,
      municipality == "PLATEADO DE JOAQUIN AMARO, EL"             ~ 32015,
      municipality == "SALVADOR, EL"                              ~ 32041,
      municipality == "FRESNILLO"                                 ~ 32010,
      municipality == "GENARO CODINA"                             ~ 32012,
      municipality == "GENERAL ENRIQUE ESTRADA"                   ~ 32013,
      municipality == "GENERAL FRANCISCO R. MURGUIA"              ~ 32014,
      municipality == "GENERAL PANFILO NATERA"                    ~ 32016,
      municipality == "GUADALUPE"                                 ~ 32017,
      municipality == "HUANUSCO"                                  ~ 32018,
      municipality == "JALPA"                                     ~ 32019,
      municipality == "JEREZ"                                     ~ 32020,
      municipality == "JIMENEZ DEL TEUL"                          ~ 32021,
      municipality == "JUAN ALDAMA"                               ~ 32022,
      municipality == "JUCHIPILA"                                 ~ 32023,
      municipality == "LORETO"                                    ~ 32024,
      municipality == "LUIS MOYA"                                 ~ 32025,
      municipality == "MAZAPIL"                                   ~ 32026,
      municipality == "MELCHOR OCAMPO"                            ~ 32027,
      municipality == "MEZQUITAL DEL ORO"                         ~ 32028,
      municipality == "MIGUEL AUZA"                               ~ 32029,
      municipality == "MOMAX"                                     ~ 32030,
      municipality == "MONTE ESCOBEDO"                            ~ 32031,
      municipality == "MORELOS"                                   ~ 32032,
      municipality == "MOYAHUA DE ESTRADA"                        ~ 32033,
      municipality == "NOCHISTLAN DE MEJIA"                       ~ 32034,
      municipality == "NORIA DE ANGELES"                          ~ 32035,
      municipality == "OJOCALIENTE"                               ~ 32036,
      municipality == "PANUCO"                                    ~ 32037,
      municipality == "PINOS"                                     ~ 32038,
      municipality == "RIO GRANDE"                                ~ 32039,
      municipality == "SAIN ALTO"                                 ~ 32040,
      municipality == "SANTA MARIA DE LA PAZ"                     ~ 32058,
      municipality == "SOMBRERETE"                                ~ 32042,
      municipality == "SUSTICACAN"                                ~ 32043,
      municipality == "TABASCO"                                   ~ 32044,
      municipality == "TEPECHITLAN"                               ~ 32045,
      municipality == "TEPETONGO"                                 ~ 32046,
      municipality == "TEUL DE GONZALEZ ORTEGA"                   ~ 32047,
      municipality == "TLALTENANGO DE SANCHEZ ROMAN"              ~ 32048,
      municipality == "TRANCOSO"                                  ~ 32057,
      municipality == "TRINIDAD GARCIA DE LA CADENA"              ~ 32011,
      municipality == "VALPARAISO"                                ~ 32049,
      municipality == "VETAGRANDE"                                ~ 32050,
      municipality == "VILLA DE COS"                              ~ 32051,
      municipality == "VILLA GARCIA"                              ~ 32052,
      municipality == "VILLA GONZALEZ ORTEGA"                     ~ 32053,
      municipality == "VILLA HIDALGO"                             ~ 32054,
      municipality == "VILLANUEVA"                                ~ 32055,
      municipality == "ZACATECAS"                                 ~ 32056,
      TRUE                                                       ~ 0
    )
  )

################################################################################
# 9) Create 'valid' = rowtotal(...) (PRI PT PVEM PC PANAL PAN_PRD MinorParty1..9)
#    year=2013, month="July"
################################################################################
df_2013 <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(
      c_across(c(PRI, PT, PVEM, PC, PANAL, PAN_PRD,
                 MinorParty1, MinorParty2, MinorParty3,
                 MinorParty4, MinorParty5, MinorParty6,
                 MinorParty7, MinorParty8, MinorParty9)),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2013,
    month = "July"
  )

################################################################################
## 2016 (SALVADOR)
################################################################################

df_2016 <- read_excel("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2016/Ayuntamientos_2016.xlsx")

# Drop existing uniqueid if present
df_2016 <- df_2016 %>%
  select(-any_of("uniqueid"))

# Assign uniqueid
df_2016 <- df_2016 %>%
  mutate(municipality = toupper(remove_accents(municipality)),
         uniqueid = assign_zacatecas_uniqueid(municipality))

# Filter out empty districts
df_2016 <- df_2016 %>%
  filter(!is.na(Dtto) & Dtto != "")

# Convert to numeric
df_2016 <- df_2016 %>%
  mutate(across(-c(municipality, Dtto), ~as.numeric(as.character(.))))

# Collapse
df_2016 <- df_2016 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Handle independent candidates (CI columns)
ci_cols <- names(df_2016)[grepl("^CI[0-9]+$", names(df_2016))]
if (length(ci_cols) > 0) {
  df_2016 <- df_2016 %>%
    mutate(CI_1 = rowSums(select(., all_of(ci_cols)), na.rm = TRUE))
  
  # CI_2 and CI_3 for specific candidates (indices 9, 14, 18 and 13)
  ci_2_cols <- intersect(c("CI9", "CI14", "CI18"), names(df_2016))
  if (length(ci_2_cols) > 0) {
    df_2016 <- df_2016 %>%
      mutate(CI_2 = rowSums(select(., all_of(ci_2_cols)), na.rm = TRUE))
  }
  
  if ("CI13" %in% names(df_2016)) {
    df_2016 <- df_2016 %>%
      mutate(CI_3 = CI13)
  }
  
  df_2016 <- df_2016 %>%
    mutate(CI_1 = CI_1 - ifelse(!is.na(CI_2), CI_2, 0) - ifelse(!is.na(CI_3), CI_3, 0))
  
  # Drop original CI columns
  df_2016 <- df_2016 %>%
    select(-all_of(ci_cols))
}

# Handle coalition logic (indmun for municipality-level coalition indicators)
df_2016 <- df_2016 %>%
  group_by(uniqueid) %>%
  mutate(indmun_PRI_PVEM_PANAL = sum(PRI_PVEM_PANAL, na.rm = TRUE),
         indmun_PRI_PVEM = sum(PRI_PVEM, na.rm = TRUE)) %>%
  ungroup()

# PAN_PRD coalition - always combine
df_2016 <- df_2016 %>%
  mutate(PAN_PRD = PAN_PRD + PAN + PRD) %>%
  select(-PAN, -PRD)

# PRI_PVEM_PANAL coalition processing
df_2016 <- df_2016 %>%
  mutate(
    PRI_PVEM_PANAL = ifelse(indmun_PRI_PVEM_PANAL != 0,
                            PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL,
                            PRI_PVEM_PANAL),
    PRI = ifelse(indmun_PRI_PVEM_PANAL != 0, NA, PRI),
    PVEM = ifelse(indmun_PRI_PVEM_PANAL != 0, NA, PVEM),
    PANAL = ifelse(indmun_PRI_PVEM_PANAL != 0, NA, PANAL),
    PRI_PVEM = ifelse(indmun_PRI_PVEM_PANAL != 0, NA, PRI_PVEM)
  )

# PRI_PVEM coalition for municipalities without PRI_PVEM_PANAL
df_2016 <- df_2016 %>%
  mutate(
    PRI_PVEM = ifelse(indmun_PRI_PVEM_PANAL == 0 & indmun_PRI_PVEM != 0,
                      PRI + PVEM + PRI_PVEM, PRI_PVEM),
    PRI = ifelse(indmun_PRI_PVEM_PANAL == 0 & indmun_PRI_PVEM != 0, NA, PRI),
    PVEM = ifelse(indmun_PRI_PVEM_PANAL == 0 & indmun_PRI_PVEM != 0, NA, PVEM)
  )

# Remove intermediate columns
df_2016 <- df_2016 %>%
  select(-matches("^indmun_|^PRI_PANAL$|^PVEM_PANAL$|^PRI$|^PVEM$"))

# Calculate total and valid
df_2016 <- df_2016 %>%
  mutate(total = rowSums(select(., PT, MC, PANAL, MORENA, PES, PAN_PRD, 
                                PRI_PVEM_PANAL, PRI_PVEM, CI_1, CI_2, CI_3,
                                no_reg, nulo), na.rm = TRUE),
         valid = rowSums(select(., PT, MC, PANAL, MORENA, PES, PAN_PRD,
                                PRI_PVEM_PANAL, PRI_PVEM, CI_1, CI_2, CI_3), na.rm = TRUE)) %>%
  select(-nulo, -no_reg)

df_2016 <- df_2016 %>%
  mutate(turnout = total / listanominal)

# Municipal aggregates
df_2016 <- df_2016 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2016 <- df_2016 %>%
  mutate(year = 2016, month = "June", STATE = "ZACATECAS")

cat("2016 processed:", nrow(df_2016), "rows\n")

################################################################################
## 2016 Extraordinary - ZACATECAS (December)
################################################################################

df_2016_ext <- read_excel("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2016/Eleccion_ext_2016_CON_CASILLAS CON LISTA NOMINAL.xlsx",
                          sheet = "Ayuntamiento", range = "A2:Z200")

df_2016_ext <- df_2016_ext %>%
  rename(section = `Sección`,
         nulos = "Votos Nulos",
         no_reg = "No Registrados",
         listanominal = LN,
         total = `Votación Total`) %>%
  filter(section != "Total" & !is.na(section)) %>%
  mutate(uniqueid = 32056,
         municipality = "ZACATECAS")

# Convert to numeric
df_2016_ext <- df_2016_ext %>%
  mutate(across(-c(municipality), ~as.numeric(as.character(.))))

names(df_2016_ext)[4:22] <- LETTERS[4:22]

# Create coalition columns per SALVADOR do-file
df_2016_ext <- df_2016_ext %>%
  mutate(PAN_PRD_PT = D + F + G + L + M + N + O,
         PRI_PVEM_PANAL = E + H + J + P + Q + R + S,
         MC = I,
         PES = K,
         CI_1 = T,
         CI_3 = U,
         CI_2 = V)

# Select relevant columns
df_2016_ext <- df_2016_ext %>%
  select(municipality, uniqueid, section, PAN_PRD_PT, PRI_PVEM_PANAL, MC, PES, 
         CI_1, CI_2, CI_3, listanominal, total)

# Collapse
df_2016_ext <- df_2016_ext %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Calculate valid
df_2016_ext <- df_2016_ext %>%
  mutate(valid = rowSums(select(., PAN_PRD_PT, PRI_PVEM_PANAL, MC, PES, CI_1, CI_2, CI_3), na.rm = TRUE))

df_2016_ext <- df_2016_ext %>%
  mutate(turnout = total / listanominal)

# Municipal aggregates
df_2016_ext <- df_2016_ext %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2016_ext <- df_2016_ext %>%
  mutate(year = 2016, month = "December", STATE = "ZACATECAS")

cat("2016 Extraordinary processed:", nrow(df_2016_ext), "rows\n")

################################################################################
# 1) Read Excel (Equivalent to: import excel "Ayuntamientos_2018.xlsx", clear firstrow)
################################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2018/Ayuntamientos_2018.xlsx",
  col_names = TRUE
) %>%
  as.data.frame()

################################################################################
# 2) Remove any existing uniqueid, create uniqueid=0, then assign codes 
#    based on municipality
################################################################################
df <- df %>%
  select(-any_of("uniqueid")) %>%  # drop uniqueid if present
  mutate(uniqueid = 0) %>%
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                        ~ 32001,
      municipality == "APULCO"                        ~ 32002,
      municipality == "ATOLINGA"                      ~ 32003,
      municipality == "BENITO JUÁREZ"                 ~ 32004,
      municipality == "CALERA"                        ~ 32005,
      municipality == "CAÃ‘ITAS "                     ~ 32006,
      municipality == "CHALCHIHUITES"                 ~ 32009,
      municipality == "CONCEPCIÃ“N DEL ORO"           ~ 32007,
      municipality == "CUAUHTÃ‰MOC"                   ~ 32008,
      municipality == "EL PLATEADO "                  ~ 32015,
      municipality == "EL SALVADOR"                   ~ 32041,
      municipality == "FRESNILLO"                     ~ 32010,
      municipality == "GENARO CODINA"                 ~ 32012,
      municipality == "ENRIQUE ESTRADA"               ~ 32013,
      municipality == "FRANCISCO R. MURGUIA"          ~ 32014,
      municipality == "PÁNFILO NATERA"                ~ 32016,
      municipality == "GUADALUPE"                     ~ 32017,
      municipality == "HUANUSCO"                      ~ 32018,
      municipality == "JALPA"                         ~ 32019,
      municipality == "JEREZ"                         ~ 32020,
      municipality == "JIMÃ‰NEZ DEL TEUL"             ~ 32021,
      municipality == "JUAN ALDAMA"                   ~ 32022,
      municipality == "JUCHIPILA"                     ~ 32023,
      municipality == "LORETO"                        ~ 32024,
      municipality == "LUÍS MOYA"                     ~ 32025,
      municipality == "MAZAPIL"                       ~ 32026,
      municipality == "MELCHOR OCAMPO"                ~ 32027,
      municipality == "MEZQUITAL DEL ORO"             ~ 32028,
      municipality == "MIGUEL AUZA"                   ~ 32029,
      municipality == "MOMAX"                         ~ 32030,
      municipality == "MONTE ESCOBEDO"                ~ 32031,
      municipality == "MORELOS"                       ~ 32032,
      municipality == "MOYAHUA DE ESTRADA"            ~ 32033,
      municipality == "NOCHISTLÁN DE MEJÍA"           ~ 32034,
      municipality == "NORIA DE ÁNGELES"              ~ 32035,
      municipality == "OJOCALIENTE"                   ~ 32036,
      municipality == "PÁNUCO"                        ~ 32037,
      municipality == "PINOS"                         ~ 32038,
      municipality == "RÍO GRANDE"                    ~ 32039,
      municipality == "SÁN ALTO"                      ~ 32040,
      municipality == "SANTA MARÍA DE LA PAZ"         ~ 32058,
      municipality == "SOMBRERETE"                    ~ 32042,
      municipality == "SUSTICACÁN"                    ~ 32043,
      municipality == "TABASCO"                       ~ 32044,
      municipality == "TEPECHITLÁN"                   ~ 32045,
      municipality == "TEPETONGO"                     ~ 32046,
      municipality == "TEUL DE GONZÁLEZ "             ~ 32047,
      municipality == "TLALTENANGO "                  ~ 32048,
      municipality == "TRANCOSO"                      ~ 32057,
      municipality == "TRINIDAD GARCÍA"               ~ 32011,
      municipality == "VALPARAISO"                    ~ 32049,
      municipality == "VETAGRANDE"                    ~ 32050,
      municipality == "VILLA DE COS"                  ~ 32051,
      municipality == "VILLA GARCÍA"                  ~ 32052,
      municipality == "VILLA GONZÁLEZ ORTEGA"         ~ 32053,
      municipality == "VILLA HIDALGO"                 ~ 32054,
      municipality == "VILLANUEVA"                    ~ 32055,
      municipality == "ZACATECAS"                     ~ 32056,
      TRUE                                           ~ 0
    )
  )

################################################################################
# 3) Reorder columns so 'uniqueid' is before municipality 
################################################################################
df <- df %>%
  relocate(uniqueid, .before = municipality)

################################################################################
# 4) Rename "*ES*" => "*PES*"
################################################################################
df <- df %>%
  rename_with(
    ~ str_replace_all(.x, "ES", "PES"),
    .cols = everything()
  )

################################################################################
# 5) Rename "NA" -> "PANAL" if there's a column named "NA"
################################################################################
if ("NA" %in% names(df)) {
  df <- df %>%
    rename(PANAL = "NA")
}

################################################################################
# 6) Combine "PAN_PRD_MC" by adding (PAN_PRD, PAN_MC, PRD_MC, PAN, PRD, MC),
#    then drop those old columns
################################################################################
df <- df %>%
  mutate(
    PAN_PRD_MC = if_else(
      !is.na(PAN_PRD_MC),
      coalesce(PAN_PRD_MC, 0) +
        coalesce(PAN_PRD, 0) + coalesce(PAN_MC, 0) + coalesce(PRD_MC, 0) +
        coalesce(PAN, 0) + coalesce(PRD, 0) + coalesce(MC, 0),
      PAN_PRD_MC
    )
  ) %>%
  select(-c(PAN_PRD, PAN_MC, PRD_MC, PAN, PRD, MC))

################################################################################
# 7) Combine "PT_MORENA_PES" for municipalities != "LORETO"
#    e.g.: replace PT_MORENA_PES = PT_MORENA_PES + PT_MORENA + PT_PES + ...
#    and set PT=., PES=., MORENA=. if municipality!="LORETO"
################################################################################
df <- df %>%
  mutate(
    PT_MORENA_PES = if_else(
      municipality != "LORETO" & !is.na(PT_MORENA_PES),
      PT_MORENA_PES + coalesce(PT_MORENA, 0) + coalesce(PT_PES, 0) +
        coalesce(MORENA_PES, 0) + coalesce(MORENA, 0) + coalesce(PES, 0) + coalesce(PT, 0),
      PT_MORENA_PES
    ),
    PT    = if_else(municipality != "LORETO", NA_real_, PT),
    PES   = if_else(municipality != "LORETO", NA_real_, PES),
    MORENA= if_else(municipality != "LORETO", NA_real_, MORENA)
  ) %>%
  select(-c(PT_MORENA, PT_PES, MORENA_PES))

################################################################################
# 8) Create CI_1 = rowtotal(CAND_IND2 - CAN_IND7),
#    rename CAND_IND4 -> CI_2,
#    replace CI_1=CI_1 - CI_2,
#    drop CAND_IND* CAN_IND7
################################################################################
# We'll guess the columns are named exactly "CAND_IND2" through "CAND_INDx".
cand_cols <- str_subset(names(df), "^CAND_IND\\d+$")
# For CAN_IND7 specifically, we assume it's also among them
df <- df %>%
  rowwise() %>%
  mutate(CI_1 = sum(c_across(all_of(cand_cols)), na.rm = TRUE)) %>%
  ungroup()

# rename CAND_IND4 -> CI_2
if ("CAND_IND4" %in% names(df)) {
  df <- df %>% rename(CI_2 = CAND_IND4)
}

# "replace CI_1=CI_1 - CI_2"
df <- df %>%
  mutate(CI_1 = CI_1 - coalesce(CI_2, 0))

# drop all CAND_IND* and CAN_IND7
df <- df %>%
  select(-matches("^CAND_IND\\d+$"), -any_of("CAN_IND7"))

################################################################################
# 9) collapse (sum) PRI - CI_1, by(STATE municipality uniqueid section),
#    then drop NUM_VOTOS_VALIDOS total
#    reorder CI_2 listanominal a(CI_1)
################################################################################
# We'll guess "STATE" is in the dataset. If not, you might need to create it.
collapse_cols <- df %>%
  select(matches("^PRI$|^PT$|^PVEM$|^PANAL$|^MORENA$|^PES$|^PAZ$|^MDZ$|^PP$|^PAN_PRD_MC$|^PT_MORENA_PES$|
                 ^PT_MORENA_PES$|^CI_1$|^CI_2$|^listanominal$|^no_reg$|^nulo$|^total$")) %>%
  names()

df_collapsed <- df %>%
  group_by(STATE, municipality, uniqueid, section) %>%
  summarise(across(all_of(collapse_cols), sum, na.rm = TRUE), .groups="drop")

# drop NUM_VOTOS_VALIDOS total
df_collapsed <- df_collapsed %>%
  select(-any_of(c("NUM_VOTOS_VALIDOS", "total")))

# reorder CI_2 listanominal a(CI_1) => place CI_2, listanominal before CI_1
# In R, we can do:
df_collapsed <- df_collapsed %>%
  relocate(CI_2, listanominal, .before = CI_1)

################################################################################
# 10) egen total= rowtotal(...); drop nulo no_reg
#     egen valid= rowtotal(...), turnout= total/listanominal, year=2018, month="July"
################################################################################
df_2018 <- df_collapsed %>%
  mutate(
    total = rowSums(
      select(., PRI, PT, PVEM, PANAL, MORENA, PES, PAZ, MDZ, PP, PAN_PRD_MC, 
             PT_MORENA_PES, CI_1, CI_2, no_reg, nulo),
      na.rm = TRUE
    )
  ) %>%
  select(-nulo, -no_reg) %>%
  mutate(
    valid = rowSums(
      select(., PRI, PT, PVEM, PANAL, MORENA, PES, PAZ, MDZ, PP, PAN_PRD_MC, 
             PT_MORENA_PES, CI_1, CI_2),
      na.rm = TRUE
    ),
    turnout = total / listanominal,
    year    = 2018,
    month   = "July"
  )

#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

# Load the 2021 dataset
data_2021 <- read_excel("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2021/CÓMPUTO DE ELECCIÓN PROCESO ELECTORAL 2020-2021.xlsx", skip = 1, sheet = "20210720_1830_COMP_AYU_Zac")

# Rename columns
data_2021 <- data_2021 %>%
  dplyr::rename(municipality = MUNICIPIO_LOCAL,
                section = SECCION,
                listanominal = LISTA_NOMINAL_CASILLA,
                total = TOTAL_VOTOS,
                no_reg = NO_REGISTRADOS,
                nulos = NUM_VOTOS_NULOS,
                valid = NUMERO_VOTOS_VALIDOS,
                PANAL = "NA", 
                CI_1 = CAND_IND_1,
                CI_2 = CAND_IND_2,
                CI_3 = CAND_IND_3) %>%
  rename_with(~ gsub("NAZ", "PANAL", .x)) %>% 
  rename_with(~ gsub("-", "_", .x)) %>% 
  dplyr::mutate(
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(section)
  ) %>% 
  dplyr::filter(section > 0) %>% 
  dplyr::select(-PL_1, -PL_2)

# Assign uniqueids
data_2021 <- data_2021 %>% 
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                        ~ 32001,
      municipality == "APULCO"                        ~ 32002,
      municipality == "ATOLINGA"                      ~ 32003,
      municipality == "BENITO JUAREZ"                 ~ 32004,
      municipality == "CALERA"                        ~ 32005,
      municipality == "CANITAS DE FELIPE PESCADOR"                     ~ 32006,
      municipality == "CHALCHIHUITES"                 ~ 32009,
      municipality == "CONCEPCION DEL ORO"           ~ 32007,
      municipality == "CUAUHTEMOC"                   ~ 32008,
      municipality == "EL PLATEADO DE JOAQUIN AMARO"                  ~ 32015,
      municipality == "EL SALVADOR"                   ~ 32041,
      municipality == "FRESNILLO"                     ~ 32010,
      municipality == "GENARO CODINA"                 ~ 32012,
      municipality == "GRAL ENRIQUE ESTRADA"               ~ 32013,
      municipality == "GENERAL FRANCISCO R MURGUIA"          ~ 32014,
      municipality == "GENERAL PANFILO NATERA"                ~ 32016,
      municipality == "GUADALUPE"                     ~ 32017,
      municipality == "HUANUSCO"                      ~ 32018,
      municipality == "JALPA"                         ~ 32019,
      municipality == "JEREZ"                         ~ 32020,
      municipality == "JIMENEZ DEL TEUL"             ~ 32021,
      municipality == "JUAN ALDAMA"                   ~ 32022,
      municipality == "JUCHIPILA"                     ~ 32023,
      municipality == "LORETO"                        ~ 32024,
      municipality == "LUIS MOYA"                     ~ 32025,
      municipality == "MAZAPIL"                       ~ 32026,
      municipality == "MELCHOR OCAMPO"                ~ 32027,
      municipality == "MEZQUITAL DEL ORO"             ~ 32028,
      municipality == "MIGUEL AUZA"                   ~ 32029,
      municipality == "MOMAX"                         ~ 32030,
      municipality == "MONTE ESCOBEDO"                ~ 32031,
      municipality == "MORELOS"                       ~ 32032,
      municipality == "MOYAHUA DE ESTRADA"            ~ 32033,
      municipality == "NOCHISTLAN"           ~ 32034,
      municipality == "NORIA DE ANGELES"              ~ 32035,
      municipality == "OJOCALIENTE"                   ~ 32036,
      municipality == "PANUCO"                        ~ 32037,
      municipality == "PINOS"                         ~ 32038,
      municipality == "RIO GRANDE"                    ~ 32039,
      municipality == "SAIN ALTO"                      ~ 32040,
      municipality == "SANTA MARIA DE LA PAZ"         ~ 32058,
      municipality == "SOMBRERETE"                    ~ 32042,
      municipality == "SUSTICACAN"                    ~ 32043,
      municipality == "TABASCO"                       ~ 32044,
      municipality == "TEPECHITLAN"                   ~ 32045,
      municipality == "TEPETONGO"                     ~ 32046,
      municipality == "TEUL DE GONZALEZ ORTEGA"             ~ 32047,
      municipality == "TLALTENANGO DE SANCHEZ ROMAN"                  ~ 32048,
      municipality == "TRANCOSO"                      ~ 32057,
      municipality == "TRINIDAD GARCIA DE LA CADENA"               ~ 32011,
      municipality == "VALPARAISO"                    ~ 32049,
      municipality == "VETAGRANDE"                    ~ 32050,
      municipality == "VILLA DE COS"                  ~ 32051,
      municipality == "VILLA GARCIA"                  ~ 32052,
      municipality == "VILLA GONZALEZ ORTEGA"         ~ 32053,
      municipality == "VILLA  HIDALGO"                 ~ 32054,
      municipality == "VILLANUEVA"                    ~ 32055,
      municipality == "ZACATECAS"                     ~ 32056,
      TRUE                                           ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(listanominal:total,PAN:CI_3), 
           \(x) sum(x, na.rm = TRUE))
  )

# Calculate valid votes and final details
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    year = 2021,
    month = "June"
  )

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 32) %>% 
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
  
  # Helper: find columns belonging to a coalition
  get_coalition_cols <- function(coal_name) {
    parties <- strsplit(coal_name, "_")[[1]]
    party_cols[sapply(party_cols, function(col) {
      all(strsplit(col, "_")[[1]] %in% parties)
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
data_2024 <- read_excel("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/2024/Resultados computos eleccion de Ayuntamiento y Diputaciones por casilla y municipio.xlsx", skip = 2, sheet = "AYUNTAMIENTOS")

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = "SECCIÓN",
                listanominal = "LISTA\r\nNOMINAL",
                total = VTOTAL,
                no_reg = NOREG,
                nulos = NULOS,
                FXM = FMZ) %>%
  rename_with(~ gsub("NAZ", "PANAL", .x)) %>% 
  dplyr::mutate(
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(section)
  ) %>% 
  dplyr::filter(section > 0)

# Assign uniqueids
data_2024 <- data_2024 %>% 
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                        ~ 32001,
      municipality == "APULCO"                        ~ 32002,
      municipality == "ATOLINGA"                      ~ 32003,
      municipality == "BENITO JUAREZ"                 ~ 32004,
      municipality == "CALERA"                        ~ 32005,
      municipality == "CANITAS DE FELIPE PESCADOR"                     ~ 32006,
      municipality == "CHALCHIHUITES"                 ~ 32009,
      municipality == "CONCEPCION DEL ORO"           ~ 32007,
      municipality == "CUAUHTEMOC"                   ~ 32008,
      municipality == "EL PLATEADO DE JOAQUIN AMARO"                  ~ 32015,
      municipality == "EL SALVADOR"                   ~ 32041,
      municipality == "FRESNILLO"                     ~ 32010,
      municipality == "GENARO CODINA"                 ~ 32012,
      municipality == "GENERAL ENRIQUE ESTRADA"               ~ 32013,
      municipality == "GENERAL FRANCISCO R. MURGUIA"          ~ 32014,
      municipality == "GENERAL PANFILO NATERA"                ~ 32016,
      municipality == "GUADALUPE"                     ~ 32017,
      municipality == "HUANUSCO"                      ~ 32018,
      municipality == "JALPA"                         ~ 32019,
      municipality == "JEREZ"                         ~ 32020,
      municipality == "JIMENEZ DEL TEUL"             ~ 32021,
      municipality == "JUAN ALDAMA"                   ~ 32022,
      municipality == "JUCHIPILA"                     ~ 32023,
      municipality == "LORETO"                        ~ 32024,
      municipality == "LUIS MOYA"                     ~ 32025,
      municipality == "MAZAPIL"                       ~ 32026,
      municipality == "MELCHOR OCAMPO"                ~ 32027,
      municipality == "MEZQUITAL DEL ORO"             ~ 32028,
      municipality == "MIGUEL AUZA"                   ~ 32029,
      municipality == "MOMAX"                         ~ 32030,
      municipality == "MONTE ESCOBEDO"                ~ 32031,
      municipality == "MORELOS"                       ~ 32032,
      municipality == "MOYAHUA DE ESTRADA"            ~ 32033,
      municipality == "NOCHISTLAN DE MEJIA"           ~ 32034,
      municipality == "NORIA DE ANGELES"              ~ 32035,
      municipality == "OJOCALIENTE"                   ~ 32036,
      municipality == "PANUCO"                        ~ 32037,
      municipality == "PINOS"                         ~ 32038,
      municipality == "RIO GRANDE"                    ~ 32039,
      municipality == "SAIN ALTO"                      ~ 32040,
      municipality == "SANTA MARIA DE LA PAZ"         ~ 32058,
      municipality == "SOMBRERETE"                    ~ 32042,
      municipality == "SUSTICACAN"                    ~ 32043,
      municipality == "TABASCO"                       ~ 32044,
      municipality == "TEPECHITLAN"                   ~ 32045,
      municipality == "TEPETONGO"                     ~ 32046,
      municipality == "TEUL DE GONZALEZ ORTEGA"             ~ 32047,
      municipality == "TLALTENANGO DE SANCHEZ ROMAN"                  ~ 32048,
      municipality == "TRANCOSO"                      ~ 32057,
      municipality == "TRINIDAD GARCIA DE LA CADENA"               ~ 32011,
      municipality == "VALPARAISO"                    ~ 32049,
      municipality == "VETAGRANDE"                    ~ 32050,
      municipality == "VILLA DE COS"                  ~ 32051,
      municipality == "VILLA GARCIA"                  ~ 32052,
      municipality == "VILLA GONZALEZ ORTEGA"         ~ 32053,
      municipality == "VILLA HIDALGO"                 ~ 32054,
      municipality == "VILLANUEVA"                    ~ 32055,
      municipality == "ZACATECAS"                     ~ 32056,
      TRUE                                           ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(listanominal,PAN:total), 
           \(x) sum(x, na.rm = TRUE))
  )

# Calculate valid votes and final details
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(PAN:PRI_PRD), na.rm = TRUE),
    year = 2024,
    month = "June"
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

zacatecas_all <- bind_rows(df_1998,
                           df_2001,
                           df_2004,
                           df_2007,
                           df_2010,
                           df_2013,
                           df_2016,
                           df_2016_ext,
                           df_2018,
                           collapsed_2021,
                           collapsed_2024)

data.table::fwrite(zacatecas_all,"../../../Processed Data/zacatecas/zacatecas_process_raw_data.csv")
