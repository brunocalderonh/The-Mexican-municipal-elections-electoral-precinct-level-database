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

###############################################################################
# 1) Read CSV (equivalent to: insheet using Ayu_Seccion_1994_No_LN.csv, clear)
###############################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1994_No_LN.csv",
               show_col_types = FALSE)  # adjust path if needed
colnames(df) <- tolower(colnames(df))
###############################################################################
# 2) Rename columns (municipio -> municipality, seccion -> section)
#    and drop cases where municipality is "" AND section is missing
###############################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section = seccion
    # If you had a column "listanominal" to rename, do it here as well
    # nominal = listanominal
  ) %>%
  # Drop if municipality == "" AND section == NA
  filter(!(municipality == "" & is.na(section)))

###############################################################################
# 3) Generate total as the row sum of pan, pri, pps, prd, pfcrn, parm, pdm, pt, pvem
#    and drop if total is missing or zero
###############################################################################
# Create 'total'
df <- df %>%
  mutate(
    total = rowSums(
      select(., pan, pri, pps, prd, pfcrn, parm, pdm, pt, pvem),
      na.rm = TRUE
    )
  ) %>%
  # Drop where total is NA or 0
  filter(!is.na(total), total != 0)

###############################################################################
# 4) Convert columns (pan through pvem, and total) to numeric (like "destring ... replace")
###############################################################################
df <- df %>%
  mutate(across(
    c(pan, pri, pps, prd, pfcrn, parm, pdm, pt, pvem, total),
    as.numeric
  ))

###############################################################################
# 5) Collapse (sum) pan - pvem and total by municipality-section 
#    (equivalent to 'collapse (sum) ... , by (municipality section)')
###############################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(
    c(pan, pri, pps, prd, pfcrn, parm, pdm, pt, pvem, total),
    sum,
    na.rm = TRUE
  )) %>%
  ungroup()

###############################################################################
# 6) Rename party columns to uppercase or desired names
###############################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN  = pan,
    PRI  = pri,
    PRD  = prd,
    PT   = pt,
    PVEM = pvem,
    PartCardenista = pfcrn,
    PARM = parm,
    PPS  = pps,
    PDM  = pdm
  )

###############################################################################
# 7) Generate uniqueid = 0, then replace based on municipality 
#    (the large chain of "replace uniqueid=XXXX if municipality==...")
###############################################################################
# You can do this in many ways in R. Here, we use case_when.
# Note: Some municipality names repeat conditions for partial string matches 
#       (e.g., Benito Juárez or "Benito J"). We handle that with grepl.
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = 0,  # initialize
    uniqueid = case_when(
      municipality == "Aconchi"                ~ 26001,
      municipality == "Agua Prieta"            ~ 26002,
      municipality == "Alamos"                 ~ 26003,
      municipality == "Altar"                  ~ 26004,
      municipality == "Arivechi"               ~ 26005,
      municipality == "Arizpe"                 ~ 26006,
      municipality == "Atil"                   ~ 26007,
      municipality == "Bacadehuachi"           ~ 26008,
      municipality == "Bacanora"               ~ 26009,
      municipality == "Bacerac"                ~ 26010,
      municipality == "Bacoachi"               ~ 26011,
      municipality == "Bacum"                  ~ 26012,
      municipality == "Banamichi"              ~ 26013,
      municipality == "Baviacora"              ~ 26014,
      municipality == "Bavispe"                ~ 26015,
      municipality == "Benjamin Hill"          ~ 26016,
      municipality == "Caborca"                ~ 26017,
      municipality == "Cajeme"                 ~ 26018,
      municipality == "Cananea"                ~ 26019,
      municipality == "Carbo"                  ~ 26020,
      municipality == "Cucurpe"                ~ 26022,
      municipality == "Cumpas"                 ~ 26023,
      municipality == "Divisaderos"            ~ 26024,
      municipality == "Empalme"                ~ 26025,
      municipality == "Etchojoa"               ~ 26026,
      municipality == "Fronteras"              ~ 26027,
      municipality == "Gral. Plutarco Elias Calles" ~ 26070,
      municipality == "Granados"               ~ 26028,
      municipality == "Guaymas"                ~ 26029,
      municipality == "Hermosillo"             ~ 26030,
      municipality == "Huachinera"             ~ 26031,
      municipality == "Huasabas"               ~ 26032,
      municipality == "Huatabampo"             ~ 26033,
      municipality == "Huepac"                 ~ 26034,
      municipality == "Imuris"                 ~ 26035,
      municipality == "La Colorada"            ~ 26021,
      municipality == "Magdalena de Kino"      ~ 26036,
      municipality == "Mazatan"                ~ 26037,
      municipality == "Moctezuma"              ~ 26038,
      municipality == "Naco"                   ~ 26039,
      municipality == "Nacori Chico"           ~ 26040,
      municipality == "Nacozari de Garcia"     ~ 26041,
      municipality == "Navojoa"                ~ 26042,
      municipality == "Nogales"                ~ 26043,
      municipality == "Onavas"                 ~ 26044,
      municipality == "Opodepe"                ~ 26045,
      municipality == "Oquitoa"                ~ 26046,
      municipality == "Pitiquito"              ~ 26047,
      municipality == "Puerto Peñasco" |
        str_detect(municipality, "Puerto P")   ~ 26048,
      municipality == "Quiriego"               ~ 26049,
      municipality == "Rayon"                  ~ 26050,
      municipality == "Rosario de Tesopaco"    ~ 26051,
      municipality == "Sahuaripa"              ~ 26052,
      municipality == "San Felipe de Jesus"    ~ 26053,
      municipality == "San Javier"             ~ 26054,
      municipality == "San Luis Rio Colorado"  ~ 26055,
      municipality == "San Miguel De Horcasitas"   ~ 26056,
      municipality == "San Pedro De La Cueva"  ~ 26057,
      municipality == "Santa Ana"              ~ 26058,
      municipality == "Santa Cruz"             ~ 26059,
      municipality == "Saric"                  ~ 26060,
      municipality == "Soyopa"                 ~ 26061,
      municipality == "Suaqui Grande"          ~ 26062,
      municipality == "Tepache"                ~ 26063,
      municipality == "Trincheras"             ~ 26064,
      municipality == "Tubutama"               ~ 26065,
      municipality == "Ures"                   ~ 26066,
      municipality == "Villa Hidalgo"          ~ 26067,
      municipality == "Villa Pesqueira"        ~ 26068,
      municipality == "Yecora"                 ~ 26069,
      # Benito Juárez gets 26071 if exact match or partial "Benito J"
      municipality == "Benito Juárez" |
        str_detect(municipality, "Benito J")   ~ 26071,
      TRUE                                     ~ 0  # default if none matched
    )
  )

###############################################################################
# 8) Create 'valid' as row-sum of parties and other columns, plus 
#    generate 'ed', 'sec' for merging
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PPS, PRD, PartCardenista, PARM, PDM, PT, PVEM),
      na.rm = TRUE
    ),
    ed  = 26,
    sec = section
  )

###############################################################################
# 9) Merge (1:m) with a dataset from Stata (Nationwide Listanominal 1994.dta),
#    keeping only 'lista' from the using data. 
#    In R, we typically do a left_join if we want to preserve all rows 
#    in 'df_collapsed'.
###############################################################################
# Read the external .dta file
df_nationwide <- read_dta("../Nationwide Listanominal 1994.dta") %>%
  select(ed, sec, lista)  # keep only the needed columns

# Perform the merge
df_merged <- df_collapsed %>%
  left_join(df_nationwide, by = c("ed", "sec"))

df_merged <- df_merged %>% filter(!is.na(lista))

# Rename the 'lista' column to 'listanominal'
df_merged <- df_merged %>%
  rename(listanominal = lista)

###############################################################################
# 10) Generate 'turnout', 'year', 'month'
###############################################################################
df_1994 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 1994,
    month   = "August"
  )

###############################################################################
# 1) Read CSV (equivalent to: insheet using Ayu_Seccion_1997_No_LN.csv, clear)
###############################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997_No_LN.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
###############################################################################
# 2) Rename columns (municipio -> municipality, seccion -> section)
#    and drop rows where municipality = "" AND section is missing
###############################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion
    # If there was a column "listanominal" to rename, do it here as well
  ) %>%
  filter(!(municipality == "" & is.na(section)))

###############################################################################
# 3) Create 'total' as the row sum of pan, pri, prd, pc, pt, pvem, pps, pdm
#    and drop if total is NA or 0
###############################################################################
df <- df %>%
  mutate(
    total = rowSums(
      select(., pan, pri, prd, pc, pt, pvem, pps, pdm),
      na.rm = TRUE
    )
  ) %>%
  filter(!is.na(total), total != 0)

###############################################################################
# 4) Convert columns (pan:pdm, total) to numeric (like destring)
###############################################################################
df <- df %>%
  mutate(
    across(c(pan, pri, prd, pc, pt, pvem, pps, pdm, total), as.numeric)
  )

###############################################################################
# 5) Collapse (sum) pan - pdm and total by municipality-section 
#    (equivalent to collapse (sum) ... by (municipality section))
###############################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(
    c(pan, pri, prd, pc, pt, pvem, pps, pdm, total),
    sum,
    na.rm = TRUE
  )) %>%
  ungroup()

###############################################################################
# 6) Rename party columns
###############################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN             = pan,
    PRI             = pri,
    PRD             = prd,
    PT              = pt,
    PVEM            = pvem,
    PartCardenista  = pc,    # Partido Cardenista
    PPS             = pps,
    PDM             = pdm
  )

###############################################################################
# 7) Generate uniqueid = 0, then replace based on municipality 
#    (the large chain of "replace uniqueid=XXXX if municipality==...")
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACONCHI"               ~ 26001,
      municipality == "AGUA PRIETA"           ~ 26002,
      municipality == "ALAMOS"                ~ 26003,
      municipality == "ALTAR"                 ~ 26004,
      municipality == "ARIVECHI"              ~ 26005,
      municipality == "ARIZPE"                ~ 26006,
      municipality == "ATIL"                  ~ 26007,
      municipality == "BACADEHUACHI"          ~ 26008,
      municipality == "BACANORA"              ~ 26009,
      municipality == "BACERAC"               ~ 26010,
      municipality == "BACOACHI"              ~ 26011,
      municipality == "BACUM"                 ~ 26012,
      municipality == "BANAMICHI"             ~ 26013,
      municipality == "BAVIACORA"             ~ 26014,
      municipality == "BAVISPE"               ~ 26015,
      municipality == "BENITO JUAREZ"         ~ 26071,
      municipality == "BENJAMIN HILL"         ~ 26016,
      municipality == "CABORCA"               ~ 26017,
      municipality == "CAJEME"                ~ 26018,
      municipality == "CANANEA"               ~ 26019,
      municipality == "CARBO"                 ~ 26020,
      municipality == "CUCURPE"               ~ 26022,
      municipality == "CUMPAS"                ~ 26023,
      municipality == "DIVISADEROS"           ~ 26024,
      municipality == "EMPALME"               ~ 26025,
      municipality == "ETCHOJOA"              ~ 26026,
      municipality == "FRONTERAS"             ~ 26027,
      municipality == "PLUTARCO ELIAS CALLES" ~ 26070,
      municipality == "GRANADOS"              ~ 26028,
      municipality == "GUAYMAS"               ~ 26029,
      municipality == "HERMOSILLO"            ~ 26030,
      municipality == "HUACHINERA"            ~ 26031,
      municipality == "HUASABAS"              ~ 26032,
      municipality == "HUATABAMPO"            ~ 26033,
      municipality == "HUEPAC"                ~ 26034,
      municipality == "IMURIS"                ~ 26035,
      municipality == "LA COLORADA"           ~ 26021,
      municipality == "MAGDALENA"             ~ 26036,
      municipality == "MAZATAN"               ~ 26037,
      municipality == "MOCTEZUMA"             ~ 26038,
      municipality == "NACO"                  ~ 26039,
      municipality == "NACORI CHICO"          ~ 26040,
      municipality == "NACOZARI DE GARCIA"    ~ 26041,
      municipality == "NAVOJOA"               ~ 26042,
      municipality == "NOGALES"               ~ 26043,
      municipality == "ONAVAS"                ~ 26044,
      municipality == "OPODEPE"               ~ 26045,
      municipality == "OQUITOA"               ~ 26046,
      municipality == "PITIQUITO"             ~ 26047,
      municipality == "PUERTO PEÑASCO"        ~ 26048,
      municipality == "QUIRIEGO"              ~ 26049,
      municipality == "RAYON"                 ~ 26050,
      municipality == "ROSARIO TESOPACO"      ~ 26051,
      municipality == "SAHUARIPA"             ~ 26052,
      municipality == "SAN FELIPE DE JESUS"   ~ 26053,
      municipality == "SAN IGNACIO RIO MUERTO"~ 26072,
      municipality == "SAN JAVIER"            ~ 26054,
      municipality == "SAN LUIS RIO COLORADO" ~ 26055,
      municipality == "SAN MIGUEL DE HORCASITAS"  ~ 26056,
      municipality == "SAN PEDRO DE LA CUEVA" ~ 26057,
      municipality == "SANTA ANA"             ~ 26058,
      municipality == "SANTA CRUZ"            ~ 26059,
      municipality == "SARIC"                 ~ 26060,
      municipality == "SOYOPA"                ~ 26061,
      municipality == "SUAQUI GRANDE"         ~ 26062,
      municipality == "TEPACHE"               ~ 26063,
      municipality == "TRINCHERAS"            ~ 26064,
      municipality == "TUBUTAMA"              ~ 26065,
      municipality == "URES"                  ~ 26066,
      municipality == "VILLA HIDALGO"         ~ 26067,
      municipality == "VILLA PESQUEIRA"       ~ 26068,
      municipality == "YECORA"                ~ 26069,
      TRUE                                    ~ 0
    )
  )

###############################################################################
# 8) Recreate 'valid' as rowtotal of (PAN PRI PRD PartCardenista PT PVEM PPS PDM)
#    plus define 'ed' and 'seccion'
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    valid   = rowSums(
      select(., PAN, PRI, PRD, PartCardenista, PT, PVEM, PPS, PDM),
      na.rm = TRUE
    ),
    ed      = 26,
    seccion = section
  )

###############################################################################
# 9) Merge (1:m) with the external dataset:
#    using "..\..\all_months_years.dta", keepusing(month year lista)
#
#    Then:
#      keep if month==7 & year==1997
#      drop if _merge==2 
#      drop _merge ed seccion year month
###############################################################################
# Read the external .dta
df_using <- read_dta("../../all_months_years.dta") %>%
  select(ed, seccion, month, year, lista)

# Perform the merge. 
# We'll do a left_join to preserve rows in df_collapsed, and then
# filter out rows that don't match
df_merged <- df_collapsed %>%
  left_join(df_using, by = c("ed", "seccion")) %>%
  # drop the rows with no match from the "using" side
  filter(!is.na(lista)) %>%
  # keep only rows for month==7 & year==1997
  filter(month == 7, year == 1997)

# Drop columns _merge (not created by default), ed, seccion, year, month
df_merged <- df_merged %>%
  select(-ed, -seccion, -year, -month)

# rename lista to listanominal
df_merged <- df_merged %>%
  rename(listanominal = lista)

###############################################################################
# 10) Generate turnout = total/listanominal, year = 1997, month="July"
###############################################################################
df_1997 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 1997,
    month   = "July"
  )

################################################################################
# 1) Read Excel file (equivalent to "import excel ..., clear firstrow case(lower)")
#    Adjust path/filename as needed.
################################################################################
df <- read_excel("../../../Data/Raw Electoral Data/Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2000.xlsx", skip = 0) %>%
  rename_all(tolower)
names(df)
################################################################################
# 2) Rename variables, drop rows where municipality = "" and section is missing
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion,
    listanominal      ="lista nominal"
  ) %>%
  filter(!(municipality == "" & is.na(section)))

################################################################################
# 3) Convert pan through listanominal columns to numeric (like destring),
#    generate 'total' as row sum, and drop if total is NA or 0
################################################################################
df <- df %>%
  # Convert relevant columns to numeric
  mutate(
    across(c(pan, pri, prd, pt, pvem, pc, pcd, psn, parm, pas, pds, listanominal), 
           as.numeric)
  ) %>%
  # Generate total
  mutate(
    total = rowSums(
      select(., pan, pri, prd, pt, pvem, pc, pcd, psn, parm, pas, pds, nulos),
      na.rm = TRUE
    )
  ) %>%
  # Drop rows with total == NA or 0
  filter(!is.na(total), total != 0)

################################################################################
# 4) Collapse (sum) variables by municipality and section
#    (like collapse (sum) pan-listanominal total, by(municipality section))
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    across(
      c(pan, pri, prd, pt, pvem, pc, pcd, psn, parm, pas, pds, listanominal, total),
      sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

################################################################################
# 5) Rename variables to match
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN  = pan,
    PRI  = pri,
    PRD  = prd,
    PT   = pt,
    PVEM = pvem,
    PC   = pc,
    PCD  = pcd,
    PSN  = psn,
    PARM = parm,
    PAS  = pas,
    PDS  = pds
  )

################################################################################
# 6) Generate turnout = total / listanominal
#    Drop columns validos nulos (they do not appear in the collapsed data 
#    but if they did, we could remove them with select(-validos, -nulos)).
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    turnout = total / listanominal
  ) %>%
  # If you had columns named validos or nulos, drop them here if needed
  select(-matches("validos|nulos"), everything())

################################################################################
# 7) Assign uniqueid = 0, then replace for each municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(uniqueid = case_when(
    municipality == "ACONCHI"                    ~ 26001,
    municipality == "AGUA PRIETA"               ~ 26002,
    municipality == "ÁLAMOS"                    ~ 26003,
    municipality == "ALAMOS"                    ~ 26003,
    municipality == "ALTAR"                     ~ 26004,
    municipality == "ARIVECHI"                  ~ 26005,
    municipality == "ARIZPE"                    ~ 26006,
    municipality == "ÁTIL"                      ~ 26007,
    municipality == "ATIL"                      ~ 26007,
    municipality == "BACADEHUACHI"              ~ 26008,
    municipality == "BACANORA"                  ~ 26009,
    municipality == "BACERAC"                   ~ 26010,
    municipality == "BACOACHI"                  ~ 26011,
    municipality == "BÁCUM"                     ~ 26012,
    municipality == "BANÁMICHI"                 ~ 26013,
    municipality == "BAVIÁCORA"                 ~ 26014,
    municipality == "BAVISPE"                   ~ 26015,
    municipality == "BENITO JUÁREZ"             ~ 26071,
    municipality == "BENJAMÍN HILL"             ~ 26016,
    municipality == "CABORCA"                   ~ 26017,
    municipality == "CAJEME"                    ~ 26018,
    municipality == "CANANEA"                   ~ 26019,
    municipality == "CARBÓ"                     ~ 26020,
    municipality == "CUCURPE"                   ~ 26022,
    municipality == "CUMPAS"                    ~ 26023,
    municipality == "DIVISADEROS"               ~ 26024,
    municipality == "EMPALME"                   ~ 26025,
    municipality == "ETCHOJOA"                  ~ 26026,
    municipality == "FRONTERAS"                 ~ 26027,
    municipality == "GRAL . PLUTARCO ELÍAS CALLES" ~ 26070,
    municipality == "GRANADOS"                  ~ 26028,
    municipality == "GUAYMAS"                   ~ 26029,
    municipality == "HERMOSILLO"                ~ 26030,
    municipality == "HUACHINERA"                ~ 26031,
    municipality == "HUÁSABAS"                  ~ 26032,
    municipality == "HUATABAMPO"                ~ 26033,
    municipality == "HUÉPAC"                    ~ 26034,
    municipality == "ÍMURIS"                    ~ 26035,
    municipality == "LA COLORADA"               ~ 26021,
    municipality == "MAGDALENA DE KINO"         ~ 26036,
    municipality == "MAZATÁN"                   ~ 26037,
    municipality == "MOCTEZUMA"                 ~ 26038,
    municipality == "NACO"                      ~ 26039,
    municipality == "NÁCORI CHICO"              ~ 26040,
    municipality == "NACOZARI DE GARCÍA"        ~ 26041,
    municipality == "NAVOJOA"                   ~ 26042,
    municipality == "NOGALES"                   ~ 26043,
    municipality == "ÓNAVAS"                    ~ 26044,
    municipality == "OPODEPE"                   ~ 26045,
    municipality == "OQUITOA"                   ~ 26046,
    municipality == "PITIQUITO"                 ~ 26047,
    municipality == "PUERTO PEÑASCO"            ~ 26048,
    municipality == "QUIRIEGO"                  ~ 26049,
    municipality == "RAYÓN"                     ~ 26050,
    municipality == "ROSARIO"                   ~ 26051,
    municipality == "SAHUARIPA"                 ~ 26052,
    municipality == "SAN FELIPE DE JESÚS"       ~ 26053,
    municipality == "SAN IGNACIO RÍO MUERTO"    ~ 26072,
    municipality == "SAN JAVIER"               ~ 26054,
    municipality == "SAN LUIS RÍO COLORADO"     ~ 26055,
    municipality == "SAN MIGUEL DE HORCASITAS"  ~ 26056,
    municipality == "SAN PEDRO DE LA CUEVA"     ~ 26057,
    municipality == "SANTA ANA"                 ~ 26058,
    municipality == "SANTA CRUZ"                ~ 26059,
    municipality == "SÁRIC"                     ~ 26060,
    municipality == "SOYOPA"                    ~ 26061,
    municipality == "SUAQUI GRANDE"             ~ 26062,
    municipality == "TEPACHE"                   ~ 26063,
    municipality == "TRINCHERAS"                ~ 26064,
    municipality == "TUBUTAMA"                  ~ 26065,
    municipality == "URES"                      ~ 26066,
    municipality == "VILLA HIDALGO"             ~ 26067,
    municipality == "VILLA PESQUEIRA"           ~ 26068,
    municipality == "YÉCORA"                    ~ 26069,
    TRUE                                        ~ 0
  ))

################################################################################
# 8) Recreate 'valid' as row sum of (PAN, PRI, PRD, PT, PVEM, PC, PCD, PSN, PARM,
#    PAS, PDS), generate year=2000, month="July"
################################################################################
df_2000 <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PRD, PT, PVEM, PC, PCD, PSN, PARM, PAS, PDS),
      na.rm = TRUE
    ),
    year  = 2000,
    month = "July"
  )

################################################################################
# 1) Read CSV 
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
################################################################################
# 2) Rename variables (municipio -> municipality, casilla -> section)
#    Drop rows where municipality=="" & section is missing
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = casilla,
    listanominal      = "lista nominal",
  ) %>%
  filter(!(municipality == "" & is.na(section)))

################################################################################
# 3) Convert pan through listanominal to numeric (like destring)
################################################################################
df <- df %>%
  mutate(across(pan:listanominal, as.numeric))

################################################################################
# 4) Create dummy variables to track whether combined columns exist 
#    For instance: gen dummy_PAN_PRD = 1 if pan_prd != .
################################################################################
df <- df %>%
  mutate(
    dummy_PAN_PRD    = if_else(!is.na(pan_prd), 1, 0),
    dummy_PARD       = if_else(!is.na(pard), 1, 0),
    dummy_PRI_PVEM   = if_else(!is.na(pri_pvem), 1, 0),
    dummy_PRD_PT_PC  = if_else(!is.na(prd_pt_pc), 1, 0),
    dummy_PAS_PMP    = if_else(!is.na(pas_pmp), 1, 0),
    dummy_PRD_PC_PAS = if_else(!is.na(prd_pc_pas), 1, 0),
    dummy_PSN_PAS    = if_else(!is.na(psn_pas), 1, 0),
    dummy_PRD_PT     = if_else(!is.na(prd_pt), 1, 0)
  )

################################################################################
# 5) For each combined column, sum in the related parties and set original parties
#    to zero:
#
################################################################################
df <- df %>%
  # If dummy_PAN_PRD==1, pan_prd = pan + prd + pan_prd, then pan=0, prd=0
  mutate(
    pan_prd = if_else(dummy_PAN_PRD == 1, coalesce(pan,0) + coalesce(prd,0) + coalesce(pan_prd,0), pan_prd),
    pan     = if_else(dummy_PAN_PRD == 1, 0, pan),
    prd     = if_else(dummy_PAN_PRD == 1, 0, prd)
  ) %>%
  # If dummy_PARD==1, pan_prd = pan + prd + pard, pan=0, prd=0, drop pard
  mutate(
    pan_prd = if_else(dummy_PARD == 1, coalesce(pan,0) + coalesce(prd,0) + coalesce(pard,0) + coalesce(pan_prd,0), pan_prd),
    pan     = if_else(dummy_PARD == 1, 0, pan),
    prd     = if_else(dummy_PARD == 1, 0, prd)
  ) %>%
  # If dummy_PRI_PVEM==1, pri_pvem = pri + pvem + pri_pvem, pri=0, pvem=0
  mutate(
    pri_pvem = if_else(dummy_PRI_PVEM == 1, coalesce(pri,0) + coalesce(pvem,0) + coalesce(pri_pvem,0), pri_pvem),
    pri      = if_else(dummy_PRI_PVEM == 1, 0, pri),
    pvem     = if_else(dummy_PRI_PVEM == 1, 0, pvem)
  ) %>%
  # If dummy_PRD_PT_PC==1, prd_pt_pc = prd + pt + pc + prd_pt_pc, then prd=0, pt=0, pc=0
  mutate(
    prd_pt_pc = if_else(dummy_PRD_PT_PC == 1, coalesce(prd,0) + coalesce(pt,0) + coalesce(pc,0) + coalesce(prd_pt_pc,0), prd_pt_pc),
    prd       = if_else(dummy_PRD_PT_PC == 1, 0, prd),
    pt        = if_else(dummy_PRD_PT_PC == 1, 0, pt),
    pc        = if_else(dummy_PRD_PT_PC == 1, 0, pc)
  ) %>%
  # If dummy_PAS_PMP==1, pas_pmp = pas + pmp + pas_pmp; pas=0, pmp=0
  mutate(
    pas_pmp = if_else(dummy_PAS_PMP == 1, coalesce(pas,0) + coalesce(pmp,0) + coalesce(pas_pmp,0), pas_pmp),
    pas     = if_else(dummy_PAS_PMP == 1, 0, pas),
    pmp     = if_else(dummy_PAS_PMP == 1, 0, pmp)
  ) %>%
  # If dummy_PRD_PC_PAS==1, prd_pc_pas = prd + pc + pas + prd_pc_pas; prd=0, pc=0, pas=0
  mutate(
    prd_pc_pas = if_else(dummy_PRD_PC_PAS == 1, coalesce(prd,0) + coalesce(pc,0) + coalesce(pas,0) + coalesce(prd_pc_pas,0), prd_pc_pas),
    prd        = if_else(dummy_PRD_PC_PAS == 1, 0, prd),
    pc         = if_else(dummy_PRD_PC_PAS == 1, 0, pc),
    pas        = if_else(dummy_PRD_PC_PAS == 1, 0, pas)
  ) %>%
  # If dummy_PSN_PAS==1, psn_pas = psn + pas + psn_pas; psn=0, pas=0
  mutate(
    psn_pas = if_else(dummy_PSN_PAS == 1, coalesce(psn,0) + coalesce(pas,0) + coalesce(psn_pas,0), psn_pas),
    psn     = if_else(dummy_PSN_PAS == 1, 0, psn),
    pas     = if_else(dummy_PSN_PAS == 1, 0, pas)
  ) %>%
  # If dummy_PRD_PT==1, prd_pt = prd_pt + prd + pt; prd=0, pt=0
  mutate(
    prd_pt = if_else(dummy_PRD_PT == 1, coalesce(prd_pt,0) + coalesce(prd,0) + coalesce(pt,0), prd_pt),
    prd    = if_else(dummy_PRD_PT == 1, 0, prd),
    pt     = if_else(dummy_PRD_PT == 1, 0, pt)
  ) %>%
  # Finally, drop the pard column
  select(-pard)

################################################################################
# 6) Collapse (sum) pan-nulos, listanominal by municipality section
#    (like 'collapse (sum) pan - nulos (first) listanominal, by(municipality section)')
#
#    We'll sum over all relevant columns. 
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    across(
      c(pan, pri, prd, pvem, pt, pas, pfc, pc, pmp, psn,
        pan_prd, pri_pvem, prd_pt_pc, pas_pmp, prd_pc_pas, psn_pas, prd_pt,
        nulos, listanominal),
      sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

################################################################################
# 7) Create total = rowtotal(...) across all parties + nulos
#    Drop rows if total==0
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    total = rowSums(
      select(
        ., pan, pri, prd, pvem, pt, pas, pfc, pc, pmp, psn,
        pan_prd, pri_pvem, prd_pt_pc, pas_pmp, prd_pc_pas, psn_pas, prd_pt,
        nulos
      ),
      na.rm = TRUE
    )
  ) %>%
  filter(total != 0)

################################################################################
# 8) Rename columns to uppercase or combined names 
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN       = pan,
    PRI       = pri,
    PRD       = prd,
    PVEM      = pvem,
    PT        = pt,
    PAS       = pas,
    PFC       = pfc,
    PC        = pc,
    PMP       = pmp,
    PSN       = psn,
    PAN_PRD   = pan_prd,
    PRI_PVEM  = pri_pvem,
    PRD_PT_PC = prd_pt_pc,
    PAS_PMP   = pas_pmp,
    PRD_PC_PAS= prd_pc_pas,
    PSN_PAS   = psn_pas,
    PRD_PT    = prd_pt
  )

################################################################################
# 9) Generate turnout = total / listanominal
#    Drop validos, nulos (if they exist)
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    turnout = total / listanominal
  ) %>%
  select(-nulos, everything())  # if you also had 'validos' in your data, drop it similarly

################################################################################
# 10) Generate uniqueid = 0, then replace based on municipality 
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACONCHI"                     ~ 26001,
      municipality == "AGUA PRIETA"                 ~ 26002,
      municipality == "ALAMOS"                      ~ 26003,
      # Note: ALAMOS & ÁLAMOS into the same ID
      municipality == "ALTAR"                       ~ 26004,
      municipality == "ARIVECHI"                    ~ 26005,
      municipality == "ARIZPE"                      ~ 26006,
      municipality == "ATIL"                        ~ 26007,
      municipality == "BACADEHUACHI"                ~ 26008,
      municipality == "BACANORA"                    ~ 26009,
      municipality == "BACERAC"                     ~ 26010,
      municipality == "BACOACHI"                    ~ 26011,
      municipality == "BACUM"                       ~ 26012,
      municipality == "BANAMICHI"                   ~ 26013,
      municipality == "BAVIACORA"                   ~ 26014,
      municipality == "BAVISPE"                     ~ 26015,
      municipality == "BENITO JUAREZ"               ~ 26071,
      municipality == "BENJAMIN HILL"               ~ 26016,
      municipality == "CABORCA"                     ~ 26017,
      municipality == "CAJEME"                      ~ 26018,
      municipality == "CANANEA"                     ~ 26019,
      municipality == "CARBO"                       ~ 26020,
      municipality == "CUCURPE"                     ~ 26022,
      municipality == "CUMPAS"                      ~ 26023,
      municipality == "DIVISADEROS"                 ~ 26024,
      municipality == "EMPALME"                     ~ 26025,
      municipality == "ETCHOJOA"                    ~ 26026,
      municipality == "FRONTERAS"                   ~ 26027,
      municipality == "GRAL. PLUTARCO ELIAS CALLES" ~ 26070,
      municipality == "GRANADOS"                    ~ 26028,
      municipality == "GUAYMAS"                     ~ 26029,
      municipality == "HERMOSILLO"                  ~ 26030,
      municipality == "HUACHINERA"                  ~ 26031,
      municipality == "HUASABAS"                    ~ 26032,
      municipality == "HUATABAMPO"                  ~ 26033,
      municipality == "HUEPAC"                      ~ 26034,
      municipality == "IMURIS"                      ~ 26035,
      municipality == "LA COLORADA"                 ~ 26021,
      municipality == "MAGDALENA DE KINO"           ~ 26036,
      municipality == "MAZATAN"                     ~ 26037,
      municipality == "MOCTEZUMA"                   ~ 26038,
      municipality == "NACO"                        ~ 26039,
      municipality == "NACORI CHICO"                ~ 26040,
      municipality == "NACOZARI DE GARCIA"          ~ 26041,
      municipality == "NAVOJOA"                     ~ 26042,
      municipality == "NOGALES"                     ~ 26043,
      municipality == "ONAVAS"                      ~ 26044,
      municipality == "OPODEPE"                     ~ 26045,
      municipality == "OQUITOA"                     ~ 26046,
      municipality == "PITIQUITO"                   ~ 26047,
      municipality == "PUERTO PEñASCO"              ~ 26048,
      # Check partial string match for "PUERTO P"
      str_detect(municipality, "PUERTO P")          ~ 26048,
      municipality == "QUIRIEGO"                    ~ 26049,
      municipality == "RAYON"                       ~ 26050,
      municipality == "ROSARIO"                     ~ 26051,
      municipality == "SAHUARIPA"                   ~ 26052,
      municipality == "SAN FELIPE DE JESUS"         ~ 26053,
      municipality == "SAN IGNACIO RIO MUERTO"      ~ 26072,
      municipality == "SAN JAVIER"                  ~ 26054,
      municipality == "SAN LUIS RIO COLORADO"       ~ 26055,
      municipality == "SAN MIGUEL DE HORCASITAS"    ~ 26056,
      municipality == "SAN PEDRO DE LA CUEVA"       ~ 26057,
      municipality == "SANTA ANA"                   ~ 26058,
      municipality == "SANTA CRUZ"                  ~ 26059,
      municipality == "SARIC"                       ~ 26060,
      municipality == "SOYOPA"                      ~ 26061,
      municipality == "SUAQUI GRANDE"               ~ 26062,
      municipality == "TEPACHE"                     ~ 26063,
      municipality == "TRINCHERAS"                  ~ 26064,
      municipality == "TUBUTAMA"                    ~ 26065,
      municipality == "URES"                        ~ 26066,
      municipality == "VILLA HIDALGO"               ~ 26067,
      municipality == "VILLA PESQUEIRA"             ~ 26068,
      municipality == "YECORA"                      ~ 26069,
      TRUE                                          ~ 0
    )
  )

################################################################################
# 11) Recreate 'valid' as rowtotal(...) for final parties, plus year/month
################################################################################
df_2003 <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(
        ., PAN, PRI, PRD, PVEM, PT, PAS, PFC, PC, PMP, PSN,
        PAN_PRD, PRI_PVEM, PRD_PT_PC, PAS_PMP, PRD_PC_PAS, PSN_PAS, PRD_PT
      ),
      na.rm = TRUE
    ),
    year  = 2003,
    month = "July"
  ) %>%
  arrange(section)  # sort section


################################################################################
# 1) Read Excel file
################################################################################
df <- read_excel("../../../Data/Raw Electoral Data/Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2006.xlsx") %>%
  # If you want all column names to be lowercase, uncomment below:
  # rename_all(tolower) %>%
  as.data.frame()  # optional, to ensure a regular data frame
colnames(df) <- tolower(colnames(df))
################################################################################
# 2) Rename columns to match (municipio -> municipality, 
#    casilla -> section, totalvotos -> total)
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = casilla,
    total        = "total votos",
    listanominal ="lista nominal"
  )

################################################################################
# 3) Drop rows where municipality == "" & section == . (NA),
#    and also drop rows where total == NA or 0
################################################################################
df <- df %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 4) Convert pan through listanominal to numeric (equivalent to 
#    destring pan - listanominal, replace)
#
#    Make sure all relevant columns exist in your data. 
#    If column names differ, adjust them here.
################################################################################
df <- df %>%
  mutate(across(pan:listanominal, as.numeric))

################################################################################
# 5) Collapse (sum) pan - listanominal by (municipality, section)
#    (like collapse (sum) pan - listanominal , by (municipality section))
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    across(pan:listanominal, sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 6) Rename columns 
#    pan -> PAN, pri -> PRI, pripanal -> PRI_PANAL, prdpt -> PRD_PT,
#    pvem -> PVEM, convergencia -> PC, alternativa -> PAS
################################################################################

df_collapsed <- df_collapsed %>%
  rename(
    PAN       = pan,
    PRI       = pri,
    PRI_PANAL = `pri-panal`,
    PRD_PT    = `prd-pt`,
    PVEM      = pvem,
    PC        = convergencia,
    PAS       = alternativa
  )

################################################################################
# 7) Generate turnout = total / listanominal
#    Drop nulos (if it exists)
################################################################################
# If a 'nulos' column exists, you can remove it by selecting everything except 'nulos':
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal) %>%
  select(-any_of("nulos"), everything())

################################################################################
# 8) Generate uniqueid = 0, then replace based on municipality
#    (Using case_when to emulate the large chain of "replace uniqueid=..." calls)
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACONCHI"                        ~ 26001,
      municipality == "AGUA PRIETA"                   ~ 26002,
      municipality == "ÁLAMOS"                        ~ 26003,
      municipality == "ALTAR"                         ~ 26004,
      municipality == "ARIVECHI"                      ~ 26005,
      municipality == "ARIZPE"                        ~ 26006,
      municipality == "ÁTIL"                          ~ 26007,
      municipality == "BACADÉHUACHI"                  ~ 26008,
      municipality == "BACANORA"                      ~ 26009,
      municipality == "BACERAC"                       ~ 26010,
      municipality == "BACOACHI"                      ~ 26011,
      municipality == "BÁCUM"                         ~ 26012,
      municipality == "BANÁMICHI"                     ~ 26013,
      municipality == "BAVIÁCORA"                     ~ 26014,
      municipality == "BAVISPE"                       ~ 26015,
      municipality == "BENITO JUÁREZ"                 ~ 26071,
      municipality == "BENJAMÍN HILL"                 ~ 26016,
      municipality == "CABORCA"                       ~ 26017,
      municipality == "CAJEME"                        ~ 26018,
      municipality == "CANANEA"                       ~ 26019,
      municipality == "CARBÓ"                         ~ 26020,
      municipality == "CUCURPE"                       ~ 26022,
      municipality == "CUMPAS"                        ~ 26023,
      municipality == "DIVISADEROS"                   ~ 26024,
      municipality == "EMPALME"                       ~ 26025,
      municipality == "ETCHOJOA"                      ~ 26026,
      municipality == "FRONTERAS"                     ~ 26027,
      municipality == "GENERAL PLUTARCO ELÍAS CALLES" ~ 26070,
      municipality == "GRANADOS"                      ~ 26028,
      municipality == "GUAYMAS"                       ~ 26029,
      municipality == "HERMOSILLO"                    ~ 26030,
      municipality == "HUACHINERA"                    ~ 26031,
      municipality == "HUÁSABAS"                      ~ 26032,
      municipality == "HUATABAMPO"                    ~ 26033,
      municipality == "HUÉPAC"                        ~ 26034,
      municipality == "ÍMURIS"                        ~ 26035,
      municipality == "LA COLORADA"                   ~ 26021,
      municipality == "MAGDALENA"                     ~ 26036,
      municipality == "MAZATÁN"                       ~ 26037,
      municipality == "MOCTEZUMA"                     ~ 26038,
      municipality == "NACO"                          ~ 26039,
      municipality == "NÁCORI CHICO"                  ~ 26040,
      municipality == "NACOZARI DE GARCÍA"            ~ 26041,
      municipality == "NAVOJOA"                       ~ 26042,
      municipality == "NOGALES"                       ~ 26043,
      municipality == "ÓNAVAS"                        ~ 26044,
      municipality == "OPODEPE"                       ~ 26045,
      municipality == "OQUITOA"                       ~ 26046,
      municipality == "PITIQUITO"                     ~ 26047,
      municipality == "PUERTO PEÑASCO"                ~ 26048,
      municipality == "QUIRIEGO"                      ~ 26049,
      municipality == "RAYÓN"                         ~ 26050,
      municipality == "ROSARIO"                       ~ 26051,
      municipality == "SAHUARIPA"                     ~ 26052,
      municipality == "SAN FELIPE DE JESÚS"           ~ 26053,
      municipality == "SAN IGNACIO RÍO MUERTO"        ~ 26072,
      municipality == "SAN JAVIER"                    ~ 26054,
      municipality == "SAN LUIS RÍO COLORADO"         ~ 26055,
      municipality == "SAN MIGUEL DE HORCASITAS"      ~ 26056,
      municipality == "SAN PEDRO DE LA CUEVA"         ~ 26057,
      municipality == "SANTA ANA"                     ~ 26058,
      municipality == "SANTA CRUZ"                    ~ 26059,
      municipality == "SÁRIC"                         ~ 26060,
      municipality == "SOYOPA"                        ~ 26061,
      municipality == "SUAQUI GRANDE"                 ~ 26062,
      municipality == "TEPACHE"                       ~ 26063,
      municipality == "TRINCHERAS"                    ~ 26064,
      municipality == "TUBUTAMA"                      ~ 26065,
      municipality == "ÚRES"                          ~ 26066,
      municipality == "VILLA HIDALGO"                 ~ 26067,
      municipality == "VILLA PESQUEIRA"               ~ 26068,
      municipality == "YÉCORA"                        ~ 26069,
      TRUE                                           ~ 0
    )
  )

################################################################################
# 9) Create 'valid' = rowtotal(PAN PRI PRI_PANAL PRD_PT PVEM PC PAS), year=2006,
#    month="July". Then sort by section.
################################################################################
df_2006 <- df_collapsed %>%
  mutate(
    valid = rowSums(select(., PAN, PRI, PRI_PANAL, PRD_PT, PVEM, PC, PAS), 
                    na.rm = TRUE),
    year  = 2006,
    month = "July"
  ) %>%
  arrange(section)

###############################################################################
# 1) Read Excel file, dropping unused columns (DISTRITO, CASILLA),
#    rename columns, and drop empty municipality rows
###############################################################################
df <- read_excel("../../../Data/Raw Electoral Data/Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/CASILLAS_AYUNTAMIENTOS_2012.xlsx",
                 sheet = "ELECCION AYUNTAMIENTO",
                 col_types = "text")

# Remove "-" and spaces
names(df) <- gsub("[- ]", "", names(df))

df <- df %>%   # read everything as text initially
  select(-DISTRITO, -CASILLA) %>%          # drop columns DISTRITO, CASILLA
  rename(
    municipality = MUNICIPIO,
    section      = SECCION,
    listanominal = LISTANOMINAL
  ) %>%
  filter(municipality != "")               # drop rows with empty municipality

###############################################################################
# 2) Convert numeric columns from text to numeric
#    (Adjust the list if you have more/less columns.)
###############################################################################
# Identify which columns should be numeric. You might have columns like:
# PAN, PRI, PRD, PT, PVEM, PC, PANAL, PAN_PANAL, PRI_PVEM, PRD_PT, PRD_PC,
# PRD_PT_PC, VOTOSNULOS, PRIPVEM, etc. 
# Make sure to include them all here.
numeric_vars <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PANAL",
                  "PAN_PANAL", "PRI_PVEM", "PRD_PT", "PRD_PC", "PRD_PT_PC",
                  "VOTOSNULOS", "PRIPVEM")

# Also include any coalition columns: "PAN_PANAL", "PRI_PVEM", "PRD_PT",
# "PRD_PC", "PRD_PT_PC" if they are stored as numeric in the Excel.
# Additionally, "LISTANOMINAL" is stored in `listanominal` now.
numeric_vars <- union(numeric_vars, "listanominal")

# Convert to numeric if the column exists in df
df <- df %>%
  mutate(across(all_of(intersect(numeric_vars, names(.))), ~ as.numeric(.)))

###############################################################################
# 3) For each var in (PAN_PANAL, PRI_PVEM, PRD_PT, PRD_PC, PRD_PT_PC),
#    by municipality: egen COAL_var = sum(var), then COAL_var = (COAL_var>0)
#
###############################################################################
coal_vars <- c("PAN_PANAL", "PRI_PVEM", "PRD_PT", "PRD_PC", "PRD_PT_PC")

for (v in coal_vars) {
  coal_name <- paste0("COAL_", v)
  df <- df %>%
    group_by(municipality) %>%
    mutate(
      !!coal_name := sum(.data[[v]], na.rm = TRUE) # sum within municipality
    ) %>%
    ungroup() %>%
    mutate(
      !!coal_name := if_else(.data[[coal_name]] > 0, 1, 0)
    )
}

###############################################################################
# 4) "Things are added up in the excel" 
#    If the coalition is 1, set the original columns to missing (NA).
###############################################################################
df <- df %>%
  mutate(
    PAN   = if_else(COAL_PAN_PANAL == 1, NA_real_, PAN),
    PANAL = if_else(COAL_PAN_PANAL == 1, NA_real_, PANAL),
    
    PRI   = if_else(COAL_PRI_PVEM == 1, NA_real_, PRI),
    PVEM  = if_else(COAL_PRI_PVEM == 1, NA_real_, PVEM),
    
    PRD = if_else(COAL_PRD_PT == 1 | COAL_PRD_PC == 1 | COAL_PRD_PT_PC == 1,
                  NA_real_, PRD),
    PT  = if_else(COAL_PRD_PT == 1 | COAL_PRD_PT_PC == 1, NA_real_, PT),
    PC  = if_else(COAL_PRD_PC == 1 | COAL_PRD_PT_PC == 1, NA_real_, PC)
  )

###############################################################################
# 5) Drop columns: CC_PAN_PANAL, CC_PRI_PVEM, CC_PRD_PT, CC_PRD_PC, CC_PRD_PT_PC 
#    and COAL_PAN_PANAL ... COAL_PRD_PT_PC
#    (These may or may not exist in your dataset—adjust as needed.)
###############################################################################
drop_vars <- c("CC_PAN_PANAL", "CC_PRI_PVEM", "CC_PRD_PT", "CC_PRD_PC", "CC_PRD_PT_PC",
               "COAL_PAN_PANAL", "COAL_PRI_PVEM", "COAL_PRD_PT", "COAL_PRD_PC", "COAL_PRD_PT_PC")
df <- df %>% select(-any_of(drop_vars))

###############################################################################
# 6) replace PRI_PVEM = PRIPVEM if PRI_PVEM==. & PRIPVEM!=.
#    then drop PRIPVEM
###############################################################################
df <- df %>%
  mutate(
    PRI_PVEM = if_else(is.na(PRI_PVEM) & !is.na(PRIPVEM), PRIPVEM, PRI_PVEM)
  ) %>%
  select(-any_of("PRIPVEM"))

###############################################################################
# 7) egen total = rowtotal(...) of relevant columns, then collapse (sum) 
#    by municipality, section
###############################################################################
# First create "total" as row sum
df <- df %>%
  mutate(
    total = rowSums(
      select(
        .,
        PAN, PRI, PRD, PT, PVEM, PC, PANAL, PAN_PANAL, PRI_PVEM,
        PRD_PT, PRD_PC, PRD_PT_PC, VOTOSNULOS
      ),
      na.rm = TRUE
    )
  )

collapse_cols <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PANAL", "PAN_PANAL",
                   "PRI_PVEM", "PRD_PT", "PRD_PC", "PRD_PT_PC",
                   "VOTOSNULOS", "total")

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    listanominal = sum(listanominal, na.rm = TRUE),
    across(all_of(collapse_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

###############################################################################
# 8) drop if total==0
###############################################################################
df_collapsed <- df_collapsed %>%
  filter(total != 0)

###############################################################################
# 9) gen turnout = total/listanominal
#    drop VOTOSNULOS
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    turnout = total / listanominal
  ) %>%
  select(-VOTOSNULOS)

###############################################################################
# 10) Create uniqueid = 0, then replace for each municipality
#     Using case_when to replicate the large chain of "replace uniqueid=..."
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACONCHI"                      ~ 26001,
      municipality == "AGUA PRIETA"                 ~ 26002,
      municipality == "ALAMOS"                      ~ 26003,
      municipality == "ALTAR"                       ~ 26004,
      municipality == "ARIVECHI"                    ~ 26005,
      municipality == "ARIZPE"                      ~ 26006,
      municipality == "ATIL"                        ~ 26007,
      municipality == "BACADEHUACHI"                ~ 26008,
      municipality == "BACANORA"                    ~ 26009,
      municipality == "BACERAC"                     ~ 26010,
      municipality == "BACOACHI"                    ~ 26011,
      municipality == "BACUM"                       ~ 26012,
      municipality == "BANAMICHI"                   ~ 26013,
      municipality == "BAVIACORA"                   ~ 26014,
      municipality == "BAVISPE"                     ~ 26015,
      municipality == "BENITO JUAREZ"               ~ 26071,
      municipality == "BENJAMIN HILL"               ~ 26016,
      municipality == "CABORCA"                     ~ 26017,
      municipality == "CAJEME"                      ~ 26018,
      municipality == "CANANEA"                     ~ 26019,
      municipality == "CARBO"                       ~ 26020,
      municipality == "CUCURPE"                     ~ 26022,
      municipality == "CUMPAS"                      ~ 26023,
      municipality == "DIVISADEROS"                 ~ 26024,
      municipality == "EMPALME"                     ~ 26025,
      municipality == "ETCHOJOA"                    ~ 26026,
      municipality == "FRONTERAS"                   ~ 26027,
      municipality == "GRAL. PLUTARCO ELIAS CALLES" ~ 26070,
      municipality == "GRANADOS"                    ~ 26028,
      municipality == "GUAYMAS"                     ~ 26029,
      municipality == "HERMOSILLO"                  ~ 26030,
      municipality == "HUACHINERA"                  ~ 26031,
      municipality == "HUASABAS"                    ~ 26032,
      municipality == "HUATABAMPO"                  ~ 26033,
      municipality == "HUEPAC"                      ~ 26034,
      municipality == "IMURIS"                      ~ 26035,
      municipality == "LA COLORADA"                 ~ 26021,
      municipality == "MAGDALENA"                   ~ 26036,
      municipality == "MAZATAN"                     ~ 26037,
      municipality == "MOCTEZUMA"                   ~ 26038,
      municipality == "NACO"                        ~ 26039,
      municipality == "NACORI CHICO"                ~ 26040,
      municipality == "NACOZARI DE GARCIA"          ~ 26041,
      municipality == "NAVOJOA"                     ~ 26042,
      municipality == "NOGALES"                     ~ 26043,
      municipality == "ONAVAS"                      ~ 26044,
      municipality == "OPODEPE"                     ~ 26045,
      municipality == "OQUITOA"                     ~ 26046,
      municipality == "PITIQUITO"                   ~ 26047,
      municipality == "PUERTO PENASCO"              ~ 26048,
      municipality == "QUIRIEGO"                    ~ 26049,
      municipality == "RAYON"                       ~ 26050,
      municipality == "ROSARIO"                     ~ 26051,
      municipality == "SAHUARIPA"                   ~ 26052,
      municipality == "SAN FELIPE DE JESUS"         ~ 26053,
      municipality == "SAN IGNACIO RIO MUERTO"      ~ 26072,
      municipality == "SAN JAVIER"                  ~ 26054,
      municipality == "SAN LUIS RIO COLORADO"       ~ 26055,
      municipality == "SAN MIGUEL DE HORCASITAS"    ~ 26056,
      municipality == "SAN PEDRO DE LA CUEVA"       ~ 26057,
      municipality == "SANTA ANA"                   ~ 26058,
      municipality == "SANTA CRUZ"                  ~ 26059,
      municipality == "SARIC"                       ~ 26060,
      municipality == "SOYOPA"                      ~ 26061,
      municipality == "SUAQUI GRANDE"               ~ 26062,
      municipality == "TEPACHE"                     ~ 26063,
      municipality == "TRINCHERAS"                  ~ 26064,
      municipality == "TUBUTAMA"                    ~ 26065,
      municipality == "URES"                        ~ 26066,
      municipality == "VILLA HIDALGO"               ~ 26067,
      municipality == "VILLA PESQUEIRA"             ~ 26068,
      municipality == "YECORA"                      ~ 26069,
      TRUE                                         ~ 0
    )
  )

###############################################################################
# 11) egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAN_PANAL PRI_PVEM 
#                           PRD_PT PRD_PC PRD_PT_PC), year=2012, month="July"
#     Then sort by section and save 
###############################################################################
df_2012 <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PRD, PT, PVEM, PC, PANAL,
             PAN_PANAL, PRI_PVEM, PRD_PT, PRD_PC, PRD_PT_PC),
      na.rm = TRUE
    ),
    year  = 2012,
    month = "July"
  ) %>%
  arrange(section)  # sort section

################################################################################
# 1) Read Excel
################################################################################
df <- read_excel("../../../Data/Raw Electoral Data/Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/ComputoMpalAyuntamiento2015_casilla.xlsx",
                 sheet = "RES. CASILLA MUNICIPIOS")

# Remove "-" and spaces
names(df) <- gsub("[- ]", "", names(df))

################################################################################
# 2) Rename MUNICIPIO -> municipality, create uniqueid via case_when
#    (Equivalent to: rename MUNICIPIO municipality; gen uniqueid=0; replace uniqueid=...)
################################################################################
df <- df %>%
  rename(municipality = MUNICIPIO) %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACONCHI"                      ~ 26001,
      municipality == "AGUA PRIETA"                 ~ 26002,
      municipality == "ÁLAMOS"                      ~ 26003,
      municipality == "ALTAR"                       ~ 26004,
      municipality == "ARIVECHI"                    ~ 26005,
      municipality == "ARIZPE"                      ~ 26006,
      municipality == "ATIL"                        ~ 26007,
      municipality == "BACADÉHUACHI"                ~ 26008,
      municipality == "BACANORA"                    ~ 26009,
      municipality == "BACERAC"                     ~ 26010,
      municipality == "BACOACHI"                    ~ 26011,
      municipality == "BÁCUM"                       ~ 26012,
      municipality == "BANÁMICHI"                   ~ 26013,
      municipality == "BAVIÁCORA"                   ~ 26014,
      municipality == "BAVISPE"                     ~ 26015,
      municipality == "BENITO JUÁREZ"               ~ 26071,
      municipality == "BENJAMÍN HILL"               ~ 26016,
      municipality == "CABORCA"                     ~ 26017,
      municipality == "CAJEME"                      ~ 26018,
      municipality == "CANANEA"                     ~ 26019,
      municipality == "CARBÓ"                       ~ 26020,
      municipality == "CUCURPE"                     ~ 26022,
      municipality == "CUMPAS"                      ~ 26023,
      municipality == "DIVISADEROS"                 ~ 26024,
      municipality == "EMPALME"                     ~ 26025,
      municipality == "ETCHOJOA"                    ~ 26026,
      municipality == "FRONTERAS"                   ~ 26027,
      municipality == "GRAL. PLUTARCO ELÍAS CALLES" ~ 26070,
      municipality == "GRANADOS"                    ~ 26028,
      municipality == "GUAYMAS"                     ~ 26029,
      municipality == "HERMOSILLO"                  ~ 26030,
      municipality == "HUACHINERA"                  ~ 26031,
      municipality == "HUÁSABAS"                    ~ 26032,
      municipality == "HUATABAMPO"                  ~ 26033,
      municipality == "HUÉPAC"                      ~ 26034,
      municipality == "IMURIS"                      ~ 26035,
      municipality == "LA COLORADA"                 ~ 26021,
      municipality == "MAGDALENA"                   ~ 26036,
      municipality == "MAZATÁN"                     ~ 26037,
      municipality == "MOCTEZUMA"                   ~ 26038,
      municipality == "NACO"                        ~ 26039,
      municipality == "NÁCORI CHICO"                ~ 26040,
      municipality == "NACOZARI DE GARCÍA"          ~ 26041,
      municipality == "NAVOJOA"                     ~ 26042,
      municipality == "NOGALES"                     ~ 26043,
      municipality == "ONAVAS"                      ~ 26044,
      municipality == "OPODEPE"                     ~ 26045,
      municipality == "OQUITOA"                     ~ 26046,
      municipality == "PITIQUITO"                   ~ 26047,
      municipality == "PUERTO PEÑASCO"              ~ 26048,
      municipality == "QUIRIEGO"                    ~ 26049,
      municipality == "RAYÓN"                       ~ 26050,
      municipality == "ROSARIO"                     ~ 26051,
      municipality == "SAHUARIPA"                   ~ 26052,
      municipality == "SAN FELIPE DE JESÚS"         ~ 26053,
      municipality == "SAN IGNACIO RÍO MUERTO"      ~ 26072,
      municipality == "SAN JAVIER"                  ~ 26054,
      municipality == "SAN LUIS RÍO COLORADO"       ~ 26055,
      municipality == "SAN MIGUEL DE HORCASITAS"    ~ 26056,
      municipality == "SAN PEDRO DE LA CUEVA"       ~ 26057,
      municipality == "SANTA ANA"                   ~ 26058,
      municipality == "SANTA CRUZ"                  ~ 26059,
      municipality == "SÁRIC"                       ~ 26060,
      municipality == "SOYOPA"                      ~ 26061,
      municipality == "SUAQUI GRANDE"               ~ 26062,
      municipality == "TEPACHE"                     ~ 26063,
      municipality == "TRINCHERAS"                  ~ 26064,
      municipality == "TUBUTAMA"                    ~ 26065,
      municipality == "URES"                        ~ 26066,
      municipality == "VILLA HIDALGO"               ~ 26067,
      municipality == "VILLA PESQUEIRA"             ~ 26068,
      municipality == "YÉCORA"                      ~ 26069,
      TRUE                                         ~ 0
    )
  )

################################################################################
# 3) Reorder columns so uniqueid is adjacent to (MUNICIPIO). 
################################################################################
df <- df %>%
  relocate(uniqueid, .before = municipality)
names(df)
################################################################################
# 4) Rename columns in bulk, as in:
#    rename (MUNICIPIO SECCION NA PRIPVEMPNA PRIPVEM PRIPNA PVEMPNA NOREG NULOS PMOR PHUM) 
#           (municipality section PANAL PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL no_reg nulo MORENA PH)
#
#    Adjust to your actual column names in the Excel. 
################################################################################
df <- df %>%
  rename(
    section      = SECCION,
    PANAL        = "NA",
    PRI_PVEM_PANAL = PRIPVEMPNA,
    PRI_PVEM     = PRIPVEM,
    PRI_PANAL    = PRIPNA,
    PVEM_PANAL   = PVEMPNA,
    no_reg       = NOREG,
    nulo         = NULOS,
    MORENA       = PMOR,
    PH           = PHUM
  )

################################################################################
# 5) Create CI_1 = row sum of (JFC RFMM CMR CAVL), then drop them
################################################################################
df <- df %>%
  mutate(
    CI_1 = rowSums(select(., JFC, RFMM, CMR, CAVL), na.rm = TRUE)
  ) %>%
  select(-JFC, -RFMM, -CMR, -CAVL)

################################################################################
# 6) Consolidate columns for coalition: 
#    replace PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL 
#    if PRI_PVEM_PANAL != .
################################################################################
df <- df %>%
  mutate(
    PRI_PVEM_PANAL = if_else(
      !is.na(PRI_PVEM_PANAL),
      coalesce(PRI_PVEM_PANAL, 0) + coalesce(PRI_PVEM, 0) +
        coalesce(PRI_PANAL, 0) + coalesce(PVEM_PANAL, 0) +
        coalesce(PRI, 0) + coalesce(PVEM, 0) + coalesce(PANAL, 0),
      PRI_PVEM_PANAL
    ),
    # Then set PRI, PVEM, PANAL to NA if PRI_PVEM_PANAL != .
    PRI  = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PRI),
    PVEM = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PVEM),
    PANAL= if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PANAL)
  ) %>%
  # Drop PRI_PVEM, PRI_PANAL, PVEM_PANAL
  select(-PRI_PVEM, -PRI_PANAL, -PVEM_PANAL)

################################################################################
# 7) Collapse (sum) from PAN through CI_1, by (municipality uniqueid section)
################################################################################
collapse_vars <- c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PANAL", "MORENA", 
                   "PH", "PES", "PRI_PVEM_PANAL", "CI_1", "nulo", "no_reg")
df_collapsed <- df %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(all_of(collapse_vars), sum, na.rm = TRUE), .groups = "drop")

################################################################################
# 8) Add year=2015, month="June", STATE="SONORA"
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    year  = 2015,
    month = "June",
    STATE = "SONORA"
  )

################################################################################
# 9) Create 'valid' = rowtotal(...) then total = valid + nulo + no_reg
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PRD, PVEM, PT, MC, PANAL, MORENA, PH, PES,
             PRI_PVEM_PANAL, CI_1),
      na.rm = TRUE
    ),
    total = valid + nulo + no_reg
  ) %>%
  select(-nulo, -no_reg)

################################################################################
# 10) Now read the LN2015.dta (Listas Nominales)
################################################################################

# (A) Read LN2015.dta
df_ln <- read_dta("../Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
  filter(entidad == 26, month == 6) %>%
  mutate(
    uniqueid = (entidad * 1000) + municipio
  ) %>%
  filter(seccion != 0) %>%
  rename(section = seccion)

# We keep just uniqueid, section, lista
df_ln <- df_ln %>%
  select(uniqueid, section, lista)

# (B) Merge 1:1 on (uniqueid, section) with df_collapsed
df_merged <- df_collapsed %>%
  left_join(df_ln, by = c("uniqueid", "section"))

df_merged <- df_merged %>% filter(!is.na(lista))

# rename lista -> listanominal
df_merged <- df_merged %>%
  rename(listanominal = lista)

################################################################################
# 11) By municipality (uniqueid), sum listanominal -> mun_listanominal
#     Then compute mun_turnout = mun_total/mun_listanominal, 
#     and turnout = total/listanominal
#
#     NOTE: The original code references "mun_total" but doesn't explicitly
#     show how it was created. We'll assume it's the sum of 'total' by uniqueid.
################################################################################
df_merged <- df_merged %>%
  group_by(uniqueid) %>%
  mutate(
    mun_listanominal = sum(listanominal, na.rm = TRUE),
    mun_total        = sum(total, na.rm = TRUE)  # assuming 'mun_total' = sum of 'total'
  ) %>%
  ungroup() %>%
  mutate(
    mun_turnout = mun_total / mun_listanominal,
    turnout     = total / listanominal
  )

################################################################################
# 12) Reorder columns, sort by uniqueid/section,
################################################################################
df_2015 <- df_merged %>%
  relocate(STATE, municipality, uniqueid, section, year, month) %>%
  arrange(uniqueid, section)

###############################################################################
# 1) Read Excel
###############################################################################
df <- read_excel("../../../Data/Raw Electoral Data/Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2018.xlsx", sheet = "Sheet1") %>%
  select(-any_of("uniqueid"))

###############################################################################
# 2) Create 'uniqueid' = 0, then assign based on municipality 
#    (the big chain of 'replace uniqueid=... if municipality=="..."')
###############################################################################
df <- df %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACONCHI"                      ~ 26001,
      municipality == "AGUA PRIETA"                 ~ 26002,
      municipality == "ALAMOS"                      ~ 26003,
      municipality == "ALTAR"                       ~ 26004,
      municipality == "ARIVECHI"                    ~ 26005,
      municipality == "ARIZPE"                      ~ 26006,
      municipality == "ATIL"                        ~ 26007,
      municipality == "BACADEHUACHI"                ~ 26008,
      municipality == "BACANORA"                    ~ 26009,
      municipality == "BACERAC"                     ~ 26010,
      municipality == "BACOACHI"                    ~ 26011,
      municipality == "BACUM"                       ~ 26012,
      municipality == "BANAMICHI"                   ~ 26013,
      municipality == "BAVIACORA"                   ~ 26014,
      municipality == "BAVISPE"                     ~ 26015,
      municipality == "BENITO JUAREZ"               ~ 26071,
      municipality == "BENJAMIN HILL"               ~ 26016,
      municipality == "CABORCA"                     ~ 26017,
      municipality == "CAJEME"                      ~ 26018,
      municipality == "CANANEA"                     ~ 26019,
      municipality == "CARBO"                       ~ 26020,
      municipality == "CUCURPE"                     ~ 26022,
      municipality == "CUMPAS"                      ~ 26023,
      municipality == "DIVISADEROS"                 ~ 26024,
      municipality == "EMPALME"                     ~ 26025,
      municipality == "ETCHOJOA"                    ~ 26026,
      municipality == "FRONTERAS"                   ~ 26027,
      municipality == "GRAL. PLUTARCO ELIAS CALLES" ~ 26070,
      municipality == "GRANADOS"                    ~ 26028,
      municipality == "GUAYMAS"                     ~ 26029,
      municipality == "HERMOSILLO"                  ~ 26030,
      municipality == "HUACHINERA"                  ~ 26031,
      municipality == "HUASABAS"                    ~ 26032,
      municipality == "HUATABAMPO"                  ~ 26033,
      municipality == "HUEPAC"                      ~ 26034,
      municipality == "IMURIS"                      ~ 26035,
      municipality == "LA COLORADA"                 ~ 26021,
      municipality == "MAGDALENA"                   ~ 26036,
      municipality == "MAZATAN"                     ~ 26037,
      municipality == "MOCTEZUMA"                   ~ 26038,
      municipality == "NACO"                        ~ 26039,
      municipality == "NACORI CHICO"                ~ 26040,
      municipality == "NACOZARI DE GARCIA"          ~ 26041,
      municipality == "NAVOJOA"                     ~ 26042,
      municipality == "NOGALES"                     ~ 26043,
      municipality == "ONAVAS"                      ~ 26044,
      municipality == "OPODEPE"                     ~ 26045,
      municipality == "OQUITOA"                     ~ 26046,
      municipality == "PITIQUITO"                   ~ 26047,
      municipality == "PUERTO PEÑASCO"              ~ 26048,
      municipality == "QUIRIEGO"                    ~ 26049,
      municipality == "RAYON"                       ~ 26050,
      municipality == "ROSARIO"                     ~ 26051,
      municipality == "SAHUARIPA"                   ~ 26052,
      municipality == "SAN FELIPE DE JESUS"         ~ 26053,
      municipality == "SAN IGNACIO RIO MUERTO"      ~ 26072,
      municipality == "SAN JAVIER"                  ~ 26054,
      municipality == "SAN LUIS RIO COLORADO"       ~ 26055,
      municipality == "SAN MIGUEL DE HORCASITAS"    ~ 26056,
      municipality == "SAN PEDRO DE LA CUEVA"       ~ 26057,
      municipality == "SANTA ANA"                   ~ 26058,
      municipality == "SANTA CRUZ"                  ~ 26059,
      municipality == "SARIC"                       ~ 26060,
      municipality == "SOYOPA"                      ~ 26061,
      municipality == "SUAQUI GRANDE"               ~ 26062,
      municipality == "TEPACHE"                     ~ 26063,
      municipality == "TRINCHERAS"                  ~ 26064,
      municipality == "TUBUTAMA"                    ~ 26065,
      municipality == "URES"                        ~ 26066,
      municipality == "VILLA HIDALGO"               ~ 26067,
      municipality == "VILLA PESQUEIRA"             ~ 26068,
      municipality == "YECORA"                      ~ 26069,
      TRUE                                         ~ 0
    )
  )

###############################################################################
# 3) Reorder columns so 'uniqueid' is before 'municipality'
###############################################################################
df <- df %>%
  relocate(uniqueid, .before = municipality)

###############################################################################
# 4) Combine columns for PAN_PRD
###############################################################################
df <- df %>%
  mutate(
    PAN_PRD = coalesce(CO_PAN_PRD, 0) + coalesce(CO_PAN, 0) + coalesce(CO_PRD, 0)
  ) %>%
  mutate(
    PAN_PRD = if_else(!is.na(CC_PAN_PRD), CC_PAN_PRD, PAN_PRD)
  ) %>%
  select(-CO_PAN_PRD, -CO_PAN, -CO_PRD, -CC_PAN_PRD)

###############################################################################
# 5) Combine columns for PRI_PVEM_PANAL, then drop old columns
###############################################################################
df <- df %>%
  mutate(
    PRI_PVEM_PANAL = coalesce(CO_PRI_PVEM_PANAL, 0) +
      coalesce(CO_PRI_PVEM, 0) + coalesce(CO_PRI_NA, 0) + coalesce(CO_PVEM_NA, 0) +
      coalesce(CO_PRI, 0) + coalesce(CO_PVEM, 0) + coalesce(CO_NA, 0)
  ) %>%
  mutate(
    PRI_PVEM_PANAL = if_else(!is.na(CC_PRI_NA_PVEM), CC_PRI_NA_PVEM, PRI_PVEM_PANAL)
  ) %>%
  select(-CO_PRI_PVEM, -CO_PRI_NA, -CO_PVEM_NA, -CO_PRI, -CO_PVEM, -CO_NA,
         -CO_PRI_PVEM_PANAL, -CC_PRI_NA_PVEM)

###############################################################################
# 6) Combine columns for PT_MORENA_PES, then drop old columns
###############################################################################
df <- df %>%
  mutate(
    PT_MORENA_PES = coalesce(CO_PT_MORENA_ES, 0) + coalesce(CO_PT_MORENA, 0) +
      coalesce(CO_PT_PES, 0) + coalesce(CO_MORENA_PES, 0) +
      coalesce(CO_PT, 0) + coalesce(CO_MORENA, 0) + coalesce(CO_PES, 0)
  ) %>%
  select(-CO_PT_MORENA_ES, -CO_PT_MORENA, -CO_PT_PES, -CO_MORENA_PES, -CO_PT, -CO_MORENA, -CO_PES)

###############################################################################
# 7) Rename CAND_* columns to CI_* 
###############################################################################
# Suppose we have columns like CAND_1, CAND_2, ... that we want to rename to CI_1, CI_2, ...
# We'll do a generic approach that if a column name starts with "CAND_", rename it to "CI_".
df <- df %>%
  rename_with(.cols = starts_with("CAND_"), .fn = ~ str_replace(.x, "^CAND_", "CI_"))

###############################################################################
# 8) Create an 'ind1' from rowtotal(CI_1 - CI_13), rename it to CI_1, 
#    then drop the old CI_* columns. 
#    But in your code, you do "egen ind1 = rowtotal(CI_1-CI_13)" and then rename ind1 => CI_1, 
#    and drop CI_*? That suggests you might have 13 separate CI_* columns. 
#    We'll replicate the logic carefully.
###############################################################################
# First, identify which columns are "CI_1" through "CI_13" (if they exist).
ci_cols <- grep("^CI_\\d+$", names(df), value = TRUE)
# Then row-sum them into 'ind1'.
df <- df %>%
  mutate(
    ind1 = rowSums(select(., all_of(ci_cols)), na.rm = TRUE)
  ) %>%
  # Drop the old CI_* columns
  select(-all_of(ci_cols)) %>%
  # rename ind1 => CI_1
  rename(CI_1 = ind1)

###############################################################################
# 9) Reorder columns so that PAN_PRD, PRI_PVEM_PANAL, PT_MORENA_PES, CI_1 
#    appear before 'MAS' if that column exists
###############################################################################
# We'll do a partial reordering. If 'MAS' doesn't exist, no problem.
wanted_order <- c("PAN_PRD", "PRI_PVEM_PANAL", "PT_MORENA_PES", "CI_1")
if ("MAS" %in% names(df)) {
  # place them before 'MAS'
  df <- df %>%
    relocate(all_of(wanted_order), .before = MAS)
} else {
  # just relocate them to the front
  df <- df %>%
    relocate(all_of(wanted_order), .after = everything())
}

###############################################################################
# 10) Rename LISTA_NOMINAL -> listanominal
###############################################################################
df <- df %>%
  rename(listanominal = LISTA_NOMINAL)

###############################################################################
# 11) Collapse (sum) PAN - VOTOS_NULOS plus listanominal by (municipality uniqueid section)
###############################################################################
collapse_vars <- df %>%
  select(matches("^PAN$|^PRI$|^PRD$|^PT$|^PVEM$|^MC$|^PANAL$|^MORENA$|^PES$|^MAS$|^PAN_PRD$|^PRI_PVEM_PANAL$|^PT_MORENA_PES$|^CI_1$|^VOTOS_NULOS$|^CANDIDATO_NO_REGISTRADO$"),
         listanominal) %>%
  names()

df_collapsed <- df %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(all_of(collapse_vars), sum, na.rm = TRUE), .groups = "drop")

###############################################################################
# 12) Create valid = rowtotal(...) then total = valid + CANDIDATO_NO_REGISTRADO + VOTOS_NULOS
#    drop CANDIDATO_NO_REGISTRADO and VOTOS_NULOS
#    Then generate year=2018, month="July", STATE="SONORA"
###############################################################################
names(df_collapsed)
df_2018 <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PRD, PT, PVEM, MC, PANAL, MORENA, PES, MAS, 
             PAN_PRD, PRI_PVEM_PANAL, PT_MORENA_PES, CI_1),
      na.rm = TRUE
    ),
    total = valid + coalesce(CANDIDATO_NO_REGISTRADO, 0) + coalesce(VOTOS_NULOS, 0)
  ) %>%
  select(-CANDIDATO_NO_REGISTRADO, -VOTOS_NULOS) %>%
  mutate(
    year  = 2018,
    month = "July",
    STATE = "SONORA"
  )

# Combine the dataframes, handling different columns by filling with NA
SONORA_all <- bind_rows(df_1994,
                        df_1997,
                        df_2000,
                        df_2003,
                        df_2006,
                        df_2009,
                        df_2012,
                        df_2015,
                        df_2018)

data.table::fwrite(SONORA_all,"../../../Processed Data/sonora/sonora_process_raw_data.csv")



