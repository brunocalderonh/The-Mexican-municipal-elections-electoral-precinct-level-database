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
# 1) Read CSV (equivalent to: insheet using "Ayu_Seccion_1998_No_LN.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_1998_No_LN.csv", show_col_types = FALSE)
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
df_collapsed <- df_collapsed %>%
  mutate(
    ed      = 32,
    seccion = section
  )

# We'll attempt two merges, as in your code (capture merge ...).
# If the file doesn't exist in both paths, adapt accordingly.
df_all_1 <- tryCatch({
  read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") 
}, error=function(e) NULL)

# We'll define a function to do the merge if df_all is not NULL
merge_fn <- function(df_main, df_all) {
  df_all <- df_all %>% select(ed, seccion, month, year, lista) %>%
    filter(month == 6, year == 1998)
  df_merge <- df_main %>%
    left_join(df_all, by = c("ed", "seccion"))
  df_merge
}

# drop unmatched. In a left_join, we can remove rows
# that have NA in 'lista' (the merged column)
df_merged <- df_merged %>% filter(!is.na(lista))

# drop _merge, ed, seccion, year, month
# (In R we didn't create _merge, so no need to drop. Just remove ed, seccion, year, month.)
df_merged <- df_merged %>%
  select(-ed, -seccion, -year, -month)

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
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2001_No_LN.csv", show_col_types = FALSE)
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
    valid   = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PNS, PAS, PC), na.rm = TRUE),
    ed      = 32,
    seccion = section
  )

################################################################################
# 8) Merge with external "all_months_years.dta" (two attempts), keep if month==6
#    & year==2001, drop if _merge==2, rename lista->listanominal
################################################################################
# We'll replicate your two merges with capture. In R, we'll attempt reading both
# paths and do a left_join. If a file isn't found, skip that merge.

df_all_1 <- tryCatch({
  read_dta("C:/Users/Horacio/Dropbox/Turismo Electoral/all_months_years.dta")
}, error=function(e) NULL)


merge_fn <- function(df_main, df_all) {
  df_all_sub <- df_all %>%
    select(ed, seccion, month, year, lista) %>%
    filter(month == 6, year == 2001)
  df_merged <- df_main %>%
    left_join(df_all_sub, by = c("ed", "seccion"))
  df_merged
}

df_merged <- df_merged  %>%
  filter(!is.na(lista))  

# drop _merge, ed, seccion, year, month
df_merged <- df_merged %>%
  select(-ed, -seccion, -year, -month)

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
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2004.csv", show_col_types = FALSE)
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
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2007.csv", show_col_types = FALSE)
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
df <- read_csv("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2010.csv", show_col_types = FALSE)
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
  path = "../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/CASILLAS_AYUNTAMIENTOS_2013.xlsx",
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
  select(listanominal:total) %>%  # from listanominal through total
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
# 1) Read Excel (Equivalent to: import excel "Ayuntamientos_2016.xlsx", clear firstrow)
################################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayuntamientos_2016.xlsx",
  col_names = TRUE
) %>%
  as.data.frame()
names(df)
################################################################################
# 2) Drop any existing uniqueid, then create uniqueid=0; assign codes based on municipality
################################################################################
df <- df %>%
  select(-any_of("uniqueid")) %>%      # drop uniqueid if present
  mutate(uniqueid = 0) %>%
  mutate(
    uniqueid = case_when(
      municipality == "APOZOL"                                   ~ 32001,
      municipality == "APULCO"                                   ~ 32002,
      municipality == "ATOLINGA"                                 ~ 32003,
      municipality == "BENITO JUÁREZ"                            ~ 32004,
      municipality == "CALERA"                                   ~ 32005,
      municipality == "CA+ITAS DE FELIPE PESCADOR"               ~ 32006,
      # if partial match "ITAS DE FELIPE PESCADOR"
      str_detect(municipality, "ITAS DE FELIPE PESCADOR")        ~ 32006,
      municipality == "CHALCHIHUITES"                            ~ 32009,
      municipality == "CONCEPCIÓN DEL ORO"                       ~ 32007,
      municipality == "CUAUHTÉMOC"                               ~ 32008,
      municipality == "EL PLATEADO DE JOAQUÍN AMARO"             ~ 32015,
      municipality == "EL SALVADOR"                              ~ 32041,
      municipality == "FRESNILLO"                                ~ 32010,
      municipality == "GENARO CODINA"                            ~ 32012,
      municipality == "GENERAL ENRIQUE ESTRADA"                  ~ 32013,
      municipality == "GENERAL FRANCISCO R. MURGUÍA"             ~ 32014,
      municipality == "GENERAL PÁNFILO NATERA"                   ~ 32016,
      municipality == "GUADALUPE"                                ~ 32017,
      municipality == "HUANUSCO"                                 ~ 32018,
      municipality == "JALPA"                                    ~ 32019,
      municipality == "JEREZ"                                    ~ 32020,
      municipality == "JIMÉNEZ DEL TEUL"                         ~ 32021,
      municipality == "JUAN ALDAMA"                              ~ 32022,
      municipality == "JUCHIPILA"                                ~ 32023,
      municipality == "LORETO"                                   ~ 32024,
      municipality == "LUIS MOYA"                                ~ 32025,
      municipality == "MAZAPIL"                                  ~ 32026,
      municipality == "MELCHOR OCAMPO"                           ~ 32027,
      municipality == "MEZQUITAL DEL ORO"                        ~ 32028,
      municipality == "MIGUEL AUZA"                              ~ 32029,
      municipality == "MOMAX"                                    ~ 32030,
      municipality == "MONTE ESCOBEDO"                           ~ 32031,
      municipality == "MORELOS"                                  ~ 32032,
      municipality == "MOYAHUA DE ESTRADA"                       ~ 32033,
      municipality == "NOCHISTLÁN DE MEJÍA"                      ~ 32034,
      municipality == "NORIA DE ÁNGELES"                         ~ 32035,
      municipality == "OJOCALIENTE"                              ~ 32036,
      municipality == "PÁNUCO"                                   ~ 32037,
      municipality == "PINOS"                                    ~ 32038,
      municipality == "RÍO GRANDE"                               ~ 32039,
      municipality == "SAIN ALTO"                                ~ 32040,
      municipality == "SANTA MARÍA DE LA PAZ"                    ~ 32058,
      municipality == "SOMBRERETE"                               ~ 32042,
      municipality == "SUSTICACÁN"                               ~ 32043,
      municipality == "TABASCO"                                  ~ 32044,
      municipality == "TEPECHITLÁN"                              ~ 32045,
      municipality == "TEPETONGO"                                ~ 32046,
      municipality == "TEUL DE GONZÁLEZ ORTEGA"                  ~ 32047,
      municipality == "TLALTENANGO DE SÁNCHEZ ROMÁN"             ~ 32048,
      municipality == "TRANCOSO"                                 ~ 32057,
      municipality == "TRINIDAD GARCÍA DE LA CADENA"             ~ 32011,
      municipality == "VALPARAÍSO"                               ~ 32049,
      municipality == "VETAGRANDE"                               ~ 32050,
      municipality == "VILLA DE COS"                             ~ 32051,
      municipality == "VILLA GARCÍA"                             ~ 32052,
      municipality == "VILLA GONZÁLEZ ORTEGA"                    ~ 32053,
      municipality == "VILLA HIDALGO"                            ~ 32054,
      municipality == "VILLANUEVA"                               ~ 32055,
      municipality == "ZACATECAS"                                ~ 32056,
      TRUE                                                       ~ 0
    )
  )

################################################################################
# 3) Reorder columns so uniqueid is before municipality 
################################################################################
df <- df %>%
  relocate(uniqueid, .before = municipality)

################################################################################
# 4) Drop if Dtto==""
################################################################################
df <- df %>%
  filter(Dtto != "")

################################################################################
# 5) Destring all columns
################################################################################
df <- df %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

################################################################################
# 6) Collapse (sum) listanominal-nulo by (municipality, uniqueid, section)
################################################################################
collapse_cols <- df %>%
  select(matches("^listanominal$|^nulo$|^PAN$|^PRI$|^PVEM$|^MC$|^PRD$|^PT$|^PES$|
                 ^MORENA$|^PANAL$|^PAN_PRD$|^PRI_PVEM_PANAL$|^CI_\\d+$|^no_reg$|
                 ^[A-Za-z0-9_]+$")) %>%  # This is a very rough pattern; adapt it
  names()

# We'll interpret "collapse (sum) listanominal-nulo" as summing all numeric columns
# from listanominal through nulo. Adjust if needed for your exact columns.
df_collapsed <- df %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(listanominal:nulo, sum, na.rm = TRUE),
            .groups = "drop")

################################################################################
# 7) Recreate CI_1, CI_2, CI_3. Then adjust CI_1 as CI_1 - CI_2 - CI_3
################################################################################
# We'll assume columns CI1, CI2, ... CI18 exist. Let's gather them:
ci_cols <- paste0("CI", 1:18)
# row sum for CI_1
df_collapsed <- df_collapsed %>%
  mutate(
    CI_1 = rowSums(select(., any_of(ci_cols)), na.rm = TRUE),
    CI_2 = rowSums(select(., c("CI9", "CI14", "CI18")), na.rm = TRUE),
    CI_3 = coalesce(CI13, 0)  # replicate gen CI_3=CI13
  ) %>%
  mutate(CI_1 = CI_1 - CI_2 - CI_3) %>%
  select(-any_of(ci_cols))  # drop CI1-CI18

################################################################################
# 8) Combine alliances: e.g., PAN_PRD=PAN_PRD + PAN + PRD; then drop PAN, PRD
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    PAN_PRD = coalesce(PAN_PRD, 0) + coalesce(PAN, 0) + coalesce(PRD, 0)
  ) %>%
  select(-PAN, -PRD)

################################################################################
# 9) Combine PRI_PVEM_PANAL if indmun_PRI_PVEM_PANAL != 0, then zero out columns
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    PRI_PVEM_PANAL = if_else(
      coalesce(indmun_PRI_PVEM_PANAL, 0) != 0,
      coalesce(PRI_PVEM_PANAL, 0) + coalesce(PRI_PVEM, 0) + coalesce(PRI_PANAL, 0) +
        coalesce(PVEM_PANAL, 0) + coalesce(PRI, 0) + coalesce(PVEM, 0) + coalesce(PANAL, 0),
      PRI_PVEM_PANAL
    ),
    PRI = if_else(coalesce(indmun_PRI_PVEM_PANAL, 0) != 0, NA_real_, PRI),
    PVEM = if_else(coalesce(indmun_PRI_PVEM_PANAL, 0) != 0, NA_real_, PVEM),
    PANAL = if_else(coalesce(indmun_PRI_PVEM_PANAL, 0) != 0, NA_real_, PANAL),
    PRI_PVEM = if_else(coalesce(indmun_PRI_PVEM_PANAL, 0) != 0, NA_real_, PRI_PVEM),
    PVEM_PANAL = if_else(coalesce(indmun_PRI_PVEM_PANAL, 0) != 0, NA_real_, PVEM_PANAL)
  ) %>%
  select(-PRI_PANAL, -PVEM_PANAL)

################################################################################
# 10) For the case (indmun_PRI_PVEM_PANAL==0 & indmun_PRI_PVEM!=0):
#     replace PRI_PVEM = PRI + PVEM + PRI_PVEM; zero out PRI, PVEM
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    PRI_PVEM = if_else(
      coalesce(indmun_PRI_PVEM_PANAL, 0) == 0 & coalesce(indmun_PRI_PVEM, 0) != 0,
      coalesce(PRI_PVEM, 0) + coalesce(PRI, 0) + coalesce(PVEM, 0),
      PRI_PVEM
    ),
    PRI = if_else(
      coalesce(indmun_PRI_PVEM_PANAL, 0) == 0 & coalesce(indmun_PRI_PVEM, 0) != 0,
      NA_real_,
      PRI
    ),
    PVEM = if_else(
      coalesce(indmun_PRI_PVEM_PANAL, 0) == 0 & coalesce(indmun_PRI_PVEM, 0) != 0,
      NA_real_,
      PVEM
    )
  ) %>%
  # drop PRI, PVEM entirely if we are sure they are always replaced
  select(-PRI, -PVEM)

################################################################################
# 11) Drop indmun* columns
################################################################################
df_collapsed <- df_collapsed %>%
  select(-matches("^indmun"))

################################################################################
# 12) Compute total = rowtotal(PT MC PANAL MORENA PES PAN_PRD PRI_PVEM_PANAL
#       PRI_PVEM CI_1 CI_2 CI_3 no_reg nulo)
#     Then valid = rowtotal(PT MC PANAL MORENA PES PAN_PRD PRI_PVEM_PANAL
#       PRI_PVEM CI_1 CI_2 CI_3)
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    total = rowSums(select(., PT, MC, PANAL, MORENA, PES, PAN_PRD, 
                           PRI_PVEM_PANAL, PRI_PVEM, CI_1, CI_2, CI_3, no_reg, nulo),
                    na.rm = TRUE),
    valid = rowSums(select(., PT, MC, PANAL, MORENA, PES, PAN_PRD, 
                           PRI_PVEM_PANAL, PRI_PVEM, CI_1, CI_2, CI_3),
                    na.rm = TRUE)
  )

################################################################################
# 13) Drop nulo, no_reg; compute turnout=total/listanominal
#     year=2016, month="June", STATE="ZACATECAS"
################################################################################
df_2016 <- df_collapsed %>%
  select(-nulo, -no_reg) %>%
  mutate(
    turnout = total / listanominal,
    year    = 2016,
    month   = "June",
    STATE   = "ZACATECAS"
  )

################################################################################
# 1) Read from Excel
#    Equivalent to:
#      import excel "Eleccion_ext_2016_CON_CASILLAS CON LISTA NOMINAL.xlsx", 
#      sheet("Ayuntamiento") cellrange(A2:Z200) firstrow clear
################################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Eleccion_ext_2016_CON_CASILLAS CON LISTA NOMINAL.xlsx",
  sheet = "Ayuntamiento",
  range = "A2:Z200",     # Cell range
  col_names = TRUE
) %>%
  as.data.frame()

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
n_cols  <- ncol(df)        # how many columns do you have?
new_names <- gen_excel_col_names(n_cols)
names(df) <- new_names

# Check
names(df)


################################################################################
# 2) Rename and drop rows where section == "Total"
################################################################################
df <- df %>%
  rename(section = A,
         Casilla = B,
         LN = C,
         "NoRegistrados" = W,
         "VotosNulos" = X,
         "VotaciónTotal" = Y) %>%
  filter(section != "Total")

################################################################################
# 3) Create uniqueid=32056, municipality="ZACATECAS EXTRAORDINARIO"
################################################################################
df <- df %>%
  mutate(
    uniqueid    = 32056,
    municipality= "ZACATECAS EXTRAORDINARIO"
  )

################################################################################
# 4) Rename LN -> listanominal, VotaciónTotal -> total
################################################################################
df <- df %>%
  rename(
    listanominal = LN,
    total        = VotaciónTotal
  )

################################################################################
# 5) Convert all columns to numeric (like `destring _all, replace`)
################################################################################
# We must be careful—some columns may not parse as numeric. We do a best effort:
df <- df %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

################################################################################
# 6) Generate the alliances and rename columns
################################################################################
df <- df %>%
  mutate(
    PAN_PRD_PT       = coalesce(D,0) + coalesce(`F`,0) + coalesce(G,0) + 
      coalesce(L,0) + coalesce(M,0) + coalesce(N,0) + 
      coalesce(O,0),
    PRI_PVEM_PANAL   = coalesce(E,0) + coalesce(H,0) + coalesce(J,0) +
      coalesce(P,0) + coalesce(Q,0) + coalesce(R,0) + 
      coalesce(S,0)
  ) %>%
  rename(
    MC   = I,
    PES  = K,
    CI_1 = `T`,
    CI_3 = U,
    CI_2 = V
  )

################################################################################
# 7) Collapse (sum) columns by (municipality, uniqueid, section)
################################################################################
df_collapsed <- df %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(
    across(
      c(PAN_PRD_PT, PRI_PVEM_PANAL, MC, PES, CI_1, CI_2, CI_3, listanominal, total),
      sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

################################################################################
# 8) Compute valid = rowtotal(...) and turnout = total/listanominal
#    Then year=2016, month="December", STATE="ZACATECAS"
################################################################################
df_2016_extra <- df_collapsed %>%
  mutate(
    valid   = PAN_PRD_PT + PRI_PVEM_PANAL + MC + PES + CI_1 + CI_2 + CI_3,
    turnout = total / listanominal,
    year    = 2016,
    month   = "December",
    STATE   = "ZACATECAS"
  )

################################################################################
# 1) Read Excel (Equivalent to: import excel "Ayuntamientos_2018.xlsx", clear firstrow)
################################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayuntamientos_2018.xlsx",
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
    rename(PANAL = NA)
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
  select(matches("^PRI$|^PT$|^PVEM$|^PANAL$|^MORENA$|^PES$|^PAZ$|^MDZ$|^PP$|^PAN_PRD_MC$|
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
data_2021 <- read_excel("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/21/CÓMPUTO DE ELECCIÓN PROCESO ELECTORAL 2020-2021.xlsx", skip = 1, sheet = "20210720_1830_COMP_AYU_Zac")

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

#####################################
### PROCESSING DATA FOR 2024 -------
#####################################

# Load the 2024 dataset
data_2024 <- read_excel("../../../Data/Raw Electoral Data/Zacatecas 1998, 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/24/Resultados computos eleccion de Ayuntamiento y Diputaciones por casilla y municipio.xlsx", skip = 2, sheet = "AYUNTAMIENTOS")

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

zacatecas_all <- bind_rows(df_1998,
                           df_2001,
                           df_2004,
                           df_2007,
                           df_2010,
                           df_2013,
                           df_2016,
                           df_2016_extra,
                           df_2018)

data.table::fwrite(zacatecas_all,"../../../Processed Data/zacatecas/zacatecas_process_raw_data.csv")