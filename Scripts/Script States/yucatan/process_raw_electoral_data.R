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
# 1) Read CSV
###############################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_1995.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))

###############################################################################
# 2) Rename columns to match Stata code:
#    municipio -> municipality
#    secc -> section
#    emitidos -> total
###############################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion,
    total        = emitidos
  )

###############################################################################
# 3) Drop rows where municipality == "" AND section is missing,
#    or total is missing or zero
###############################################################################
df <- df %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

###############################################################################
# 4) Convert (pan through listanominal) to numeric 
#    (like "destring pan - listanominal, replace" in Stata)
###############################################################################
# We'll assume columns pan through listanominal exist in your data.
df <- df %>%
  mutate(across(pan:listanominal, as.numeric))

###############################################################################
# 5) Collapse (sum) pan - listanominal by (municipality, section)
#    (In Stata: collapse (sum) pan - listanominal, by(municipality section))
###############################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    across(pan:listanominal, sum, na.rm = TRUE),
    .groups = "drop"
  )

###############################################################################
# 6) Rename columns:
#    pan->PAN, pri->PRI, prd->PRD, pt->PT, pvem->PVEM, pfcrn->PartCardenista, otros->Otros
###############################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN              = pan,
    PRI              = pri,
    PRD              = prd,
    PT               = pt,
    PVEM             = pvem,
    PartCardenista   = pfcrn,
    Otros            = otros
  )

###############################################################################
# 7) turnout = total / listanominal
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

###############################################################################
# 8) Drop 'nulos' if it exists (Stata: drop nulos)
###############################################################################
df_collapsed <- df_collapsed %>%
  select(-any_of("nulos"))

###############################################################################
# 9) Create uniqueid=0, then assign codes based on municipality
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "Abalá"        ~ 31001,
      municipality == "Acanceh"      ~ 31002,
      municipality == "Akil"         ~ 31003,
      municipality == "Baca"         ~ 31004,
      municipality == "Bokobá"       ~ 31005,
      municipality == "Buctzotz"     ~ 31006,
      municipality == "Cacalchen"    ~ 31007,
      municipality == "Calotmul"     ~ 31008,
      municipality == "Cansahcab"    ~ 31009,
      municipality == "Cantamayec"   ~ 31010,
      municipality == "Celestun"     ~ 31011,
      municipality == "Cenotillo"    ~ 31012,
      municipality == "Chacsinkin"   ~ 31016,
      municipality == "Chankom"      ~ 31017,
      municipality == "Chapab"       ~ 31018,
      municipality == "Chemax"       ~ 31019,
      municipality == "Chichimila"   ~ 31021,
      municipality == "Chicxulub Pueblo" ~ 31020,
      municipality == "Chikidzonot"  ~ 31022,
      municipality == "Chocholá"     ~ 31023,
      municipality == "Chumayel"     ~ 31024,
      municipality == "Conkal"       ~ 31013,
      municipality == "Cuncunul"     ~ 31014,
      municipality == "Cuzama"       ~ 31015,
      municipality == "Dzan"         ~ 31025,
      municipality == "Dzemul"       ~ 31026,
      municipality == "Dzidzantun"   ~ 31027,
      municipality == "Dzilam Bravo" ~ 31028,
      municipality == "Dzilam Gonzalez" ~ 31029,
      municipality == "Dzitas"       ~ 31030,
      municipality == "Dzoncauich"   ~ 31031,
      municipality == "Espita"       ~ 31032,
      municipality == "Halachó"      ~ 31033,
      municipality == "Hocaba"       ~ 31034,
      municipality == "Hoctun"       ~ 31035,
      municipality == "Homun"        ~ 31036,
      municipality == "Huhi"         ~ 31037,
      municipality == "Hunucma"      ~ 31038,
      municipality == "Ixil"         ~ 31039,
      municipality == "Izamal"       ~ 31040,
      municipality == "Kanasin"      ~ 31041,
      municipality == "Kantunil"     ~ 31042,
      municipality == "Kaua"         ~ 31043,
      municipality == "Kinchil"      ~ 31044,
      municipality == "Kopomá"       ~ 31045,
      municipality == "Mama"         ~ 31046,
      municipality == "Mani"         ~ 31047,
      municipality == "Maxcanú"      ~ 31048,
      municipality == "Mayapan"      ~ 31049,
      municipality == "Mérida"       ~ 31050,
      municipality == "Mococha"      ~ 31051,
      municipality == "Motul"        ~ 31052,
      municipality == "Muna"         ~ 31053,
      municipality == "Muxupip"      ~ 31054,
      municipality == "Opichen"      ~ 31055,
      municipality == "Oxkutzcab"    ~ 31056,
      municipality == "Panaba"       ~ 31057,
      municipality == "Peto"         ~ 31058,
      municipality == "Progreso"     ~ 31059,
      municipality == "Quintana Roo" ~ 31060,
      municipality == "Rio lagartos" ~ 31061,
      municipality == "Sacalum"      ~ 31062,
      municipality == "Samahil"      ~ 31063,
      municipality == "San Felipe"   ~ 31065,
      municipality == "Sanahcat"     ~ 31064,
      municipality == "Santa Elena"  ~ 31066,
      municipality == "Seye"         ~ 31067,
      municipality == "Sinanche"     ~ 31068,
      municipality == "Sotuta"       ~ 31069,
      municipality == "Sucila"       ~ 31070,
      municipality == "Sudzal"       ~ 31071,
      municipality == "Suma de Hidalgo" ~ 31072,
      municipality == "Tahdziu"      ~ 31073,
      municipality == "Tahmek"       ~ 31074,
      municipality == "Teabo"        ~ 31075,
      municipality == "Tecoh"        ~ 31076,
      municipality == "Tekal de Venegas" ~ 31077,
      municipality == "Tekanto"      ~ 31078,
      municipality == "Tekax"        ~ 31079,
      municipality == "Tekit"        ~ 31080,
      municipality == "Tekom"        ~ 31081,
      municipality == "Telchac Pueblo"   ~ 31082,
      municipality == "Telchac Puerto"   ~ 31083,
      municipality == "Temax"        ~ 31084,
      municipality == "Temozón"      ~ 31085,
      municipality == "Tepakan"      ~ 31086,
      municipality == "Tetiz"        ~ 31087,
      municipality == "Teya"         ~ 31088,
      municipality == "Ticul"        ~ 31089,
      municipality == "Timucuy"      ~ 31090,
      municipality == "Tinum"        ~ 31091,
      municipality == "Tixcacalcupul"~ 31092,
      municipality == "Tixkokob"     ~ 31093,
      municipality == "Tixmehuac"    ~ 31094,
      municipality == "Tixpehual"    ~ 31095,
      municipality == "Tizimin"      ~ 31096,
      municipality == "Tunkas"       ~ 31097,
      municipality == "Tzucacab"     ~ 31098,
      municipality == "Uayma"        ~ 31099,
      municipality == "Ucu"          ~ 31100,
      municipality == "Umán"         ~ 31101,
      municipality == "Valladolid"   ~ 31102,
      municipality == "Xocchel"      ~ 31103,
      municipality == "Yaxcaba"      ~ 31104,
      municipality == "Yaxkukul"     ~ 31105,
      municipality == "Yobain"       ~ 31106,
      TRUE                           ~ 0
    )
  )

###############################################################################
# 10) Create 'valid' = rowtotal(PAN PRI PRD PartCardenista PT PVEM Otros)
#     year=1995, month="May"
###############################################################################
df_1995 <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(PAN, PRI, PRD, PartCardenista, PT, PVEM, Otros)),
                na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 1995,
    month = "May"
  )


################################################################################
# 1) Read CSV (Equivalent to: insheet using "Ayu_Seccion_1998_No_LN.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_1998_No_LN.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns (municipio->municipality, secc->section)
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion
  )

################################################################################
# 3) Drop rows if municipality=="" & section is NA; 
#    create total as row sum of (pan pri prd pt pvem noregistrados nulos), 
#    replace total=. if total==0
################################################################################
df <- df %>%
  filter(!(municipality == "" & is.na(section))) %>%
  rowwise() %>%
  mutate(
    total = sum(c_across(c(pan, pri, prd, pt, pvem, noregistrados, nulos)),
                na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    total = if_else(total == 0, NA_real_, total)
  )

################################################################################
# 4) Destring pan through nulos & total (Stata: destring  pan - nulos total, replace)
################################################################################
df <- df %>%
  mutate(across(c(pan, pri, prd, pt, pvem, noregistrados, nulos, total),
                ~ as.numeric(.)))

################################################################################
# 5) Collapse (sum) pan-nulos total by (municipality, section)
#    (Stata: collapse (sum) pan-nulos total, by(municipality section))
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    across(
      c(pan, pri, prd, pt, pvem, noregistrados, nulos, total),
      sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

################################################################################
# 6) Rename columns to final names (pan->PAN, pri->PRI, etc.); drop nulos, noreg
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT  = pt,
    PVEM= pvem
  ) %>%
  select(-nulos, -noreg)

################################################################################
# 7) Create uniqueid=0, then assign codes based on municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "ABALA"                    ~ 31001,
      municipality == "ACANCEH"                  ~ 31002,
      municipality == "AKIL"                     ~ 31003,
      municipality == "BACA"                     ~ 31004,
      municipality == "BOKOBA"                   ~ 31005,
      municipality == "BUCTZOTZ"                 ~ 31006,
      municipality == "CACALCHEN"                ~ 31007,
      municipality == "CALOTMUL"                 ~ 31008,
      municipality == "CANSAHCAB"                ~ 31009,
      municipality == "CANTAMAYEC"               ~ 31010,
      municipality == "CELESTUN"                 ~ 31011,
      municipality == "CENOTILLO"                ~ 31012,
      municipality == "CHACSINKIN"               ~ 31016,
      municipality == "CHANKOM"                  ~ 31017,
      municipality == "CHAPAB"                   ~ 31018,
      municipality == "CHEMAX"                   ~ 31019,
      municipality == "CHICHIMILA"               ~ 31021,
      municipality == "CHICXULUB PUEBLO"         ~ 31020,
      municipality == "CHIKINDZONOT"             ~ 31022,
      municipality == "CHOCHOLA"                 ~ 31023,
      municipality == "CHUMAYEL"                 ~ 31024,
      municipality == "CONKAL"                   ~ 31013,
      municipality == "CUNCUNUL"                 ~ 31014,
      municipality == "CUZAMA"                   ~ 31015,
      municipality == "DZAN"                     ~ 31025,
      municipality == "DZEMUL"                   ~ 31026,
      municipality == "DZIDZANTUN"               ~ 31027,
      municipality == "DZILAM DE BRAVO"          ~ 31028,
      municipality == "DZILAM GONZALEZ"          ~ 31029,
      municipality == "DZITAS"                   ~ 31030,
      municipality == "DZONCAUICH"               ~ 31031,
      municipality == "ESPITA"                   ~ 31032,
      municipality == "HALACHO"                  ~ 31033,
      municipality == "HOCABA"                   ~ 31034,
      municipality == "HOCTUN"                   ~ 31035,
      municipality == "HOMUN"                    ~ 31036,
      municipality == "HUHI"                     ~ 31037,
      municipality == "HUNUCMA"                  ~ 31038,
      municipality == "IXIL"                     ~ 31039,
      municipality == "IZAMAL"                   ~ 31040,
      municipality == "KANASIN"                  ~ 31041,
      municipality == "KANTUNIL"                 ~ 31042,
      municipality == "KAUA"                     ~ 31043,
      municipality == "KINCHIL"                  ~ 31044,
      municipality == "KOPOMA"                   ~ 31045,
      municipality == "MAMA"                     ~ 31046,
      municipality == "MANI"                     ~ 31047,
      municipality == "MAXCANU"                  ~ 31048,
      municipality == "MAYAPAN"                  ~ 31049,
      municipality == "MERIDA"                   ~ 31050,
      municipality == "MOCOCHA"                  ~ 31051,
      municipality == "MOTUL"                    ~ 31052,
      municipality == "MUNA"                     ~ 31053,
      municipality == "MUXUPIP"                  ~ 31054,
      municipality == "OPICHEN"                  ~ 31055,
      municipality == "OXKUTZCAB"                ~ 31056,
      municipality == "PANABA"                   ~ 31057,
      municipality == "PETO"                     ~ 31058,
      municipality == "PROGRESO"                 ~ 31059,
      municipality == "QUINTANA ROO"             ~ 31060,
      municipality == "RIO LAGARTOS"             ~ 31061,
      municipality == "SACALUM"                  ~ 31062,
      municipality == "SAMAHIL"                  ~ 31063,
      municipality == "SAN FELIPE"               ~ 31065,
      municipality == "SANAHCAT"                 ~ 31064,
      municipality == "SANTA ELENA"              ~ 31066,
      municipality == "SEYE"                     ~ 31067,
      municipality == "SINANCHE"                 ~ 31068,
      municipality == "SOTUTA"                   ~ 31069,
      municipality == "SUCILA"                   ~ 31070,
      municipality == "SUDZAL"                   ~ 31071,
      municipality == "SUMA DE HIDALGO"          ~ 31072,
      municipality == "TAHDZIU"                  ~ 31073,
      municipality == "TAHMEK"                   ~ 31074,
      municipality == "TEABO"                    ~ 31075,
      municipality == "TECOH"                    ~ 31076,
      municipality == "TEKAL DE VENEGAS"         ~ 31077,
      municipality == "TEKANTO"                  ~ 31078,
      municipality == "TEKAX"                    ~ 31079,
      municipality == "TEKIT"                    ~ 31080,
      municipality == "TEKOM"                    ~ 31081,
      municipality == "TELCHAC PUEBLO"           ~ 31082,
      municipality == "TELCHAC PUERTO"           ~ 31083,
      municipality == "TEMAX"                    ~ 31084,
      municipality == "TEMOZON"                  ~ 31085,
      municipality == "TEPAKAN"                  ~ 31086,
      municipality == "TETIZ"                    ~ 31087,
      municipality == "TEYA"                     ~ 31088,
      municipality == "TICUL"                    ~ 31089,
      municipality == "TIMUCUY"                  ~ 31090,
      municipality == "TINUM"                    ~ 31091,
      municipality == "TIXCACALCUPUL"            ~ 31092,
      municipality == "TIXKOKOB"                 ~ 31093,
      municipality == "TIXMEHUAC"                ~ 31094,
      municipality == "TIXPEHUAL"                ~ 31095,
      municipality == "TIZIMIN"                  ~ 31096,
      municipality == "TUNKAS"                   ~ 31097,
      municipality == "TZUCACAB"                 ~ 31098,
      municipality == "UAYMA"                    ~ 31099,
      municipality == "UCU"                      ~ 31100,
      municipality == "UMAN"                     ~ 31101,
      municipality == "VALLADOLID"               ~ 31102,
      municipality == "XOCCHEL"                  ~ 31103,
      municipality == "YAXCABA"                  ~ 31104,
      municipality == "YAXKUKUL"                 ~ 31105,
      municipality == "YOBAIN"                   ~ 31106,
      TRUE                                       ~ 0
    )
  )

################################################################################
# 8) Create valid = rowtotal(PAN PRI PRD PT PVEM),
#    set ed=31, rename section->seccion to merge, keep if month==6 & year==1998
################################################################################
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(PAN, PRI, PRD, PT, PVEM)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    ed      = 31,
    seccion = section  # for merging
  )

################################################################################
# 9) Attempt two merges with "all_months_years.dta" from two possible paths,
#    keep if month==6 & year==1998, drop unmatched, rename lista->listanominal
################################################################################
df_all_1 <- tryCatch({
  read_dta("C:/Users/Horacio/Dropbox/Turismo Electoral/all_months_years.dta") 
}, error=function(e) NULL)

merge_fn <- function(df_main, df_using) {
  df_using_sub <- df_using %>% select(ed, seccion, month, year, lista) %>%
    filter(month == 6, year == 1998)
  df_merged <- df_main %>% left_join(df_using_sub, by=c("ed", "seccion"))
  df_merged
}

df_merged <- df_collapsed
if (!is.null(df_all_1)) {
  df_merged <- merge_fn(df_merged, df_all_1)
}
if (!is.null(df_all_2)) {
  df_merged <- merge_fn(df_merged, df_all_2)
}


# drop _merge, ed, seccion, year, month
df_merged <- df_merged %>%
  select(-ed, -seccion, -year, -month)

df_merged <- df_merged %>%
  rename(listanominal = lista)

################################################################################
# 10) turnout = total/listanominal, year=1998, month="May"
################################################################################
df_1998 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 1998,
    month   = "May"
  )

################################################################################
# 1) Read CSV (Equivalent to insheet using "Ayu_Seccion_2001.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_2001.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns (municipio->municipality, secc->section)
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion
  )

################################################################################
# 3) Drop rows if municipality=="" & section missing, or total is missing/zero
################################################################################
df <- df %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 4) Destring (listanominal through total), then collapse (sum) 
#    by (municipality, section)
################################################################################
df <- df %>%
  mutate(across(listanominal:total, as.numeric))

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:total, sum, na.rm = TRUE), .groups = "drop")

################################################################################
# 5) Combining columns in certain municipalities:
#    pan_pvem = pan + pvem if municipality=="TEKAX", then zero out pan, pvem
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    pan_pvem = if_else(
      municipality == "TEKAX",
      coalesce(pan, 0) + coalesce(pvem, 0),
      pan_pvem
    ),
    pan = if_else(municipality == "TEKAX", 0, pan),
    pvem= if_else(municipality == "TEKAX", 0, pvem)
  )

################################################################################
# 6) prd_pt_pvem = prd + pt + pvem if municipality=="UMAN" | "VALLADOLID", 
#    zero out prd, pt, pvem for those municipalities
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    prd_pt_pvem = if_else(
      municipality %in% c("UMAN", "VALLADOLID"),
      coalesce(prd, 0) + coalesce(pt, 0) + coalesce(pvem, 0),
      prd_pt_pvem
    ),
    prd = if_else(municipality %in% c("UMAN","VALLADOLID"), 0, prd),
    pt  = if_else(municipality %in% c("UMAN","VALLADOLID"), 0, pt),
    pvem= if_else(municipality %in% c("UMAN","VALLADOLID"), 0, pvem)
  )

################################################################################
# 7) pt_cpd = pt + cpd if municipality=="MOTUL"
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    pt_cpd = if_else(
      municipality == "MOTUL",
      coalesce(pt, 0) + coalesce(cpd, 0),
      pt_cpd
    ),
    pt  = if_else(municipality=="MOTUL", 0, pt),
    cpd = if_else(municipality=="MOTUL", 0, cpd)
  )

################################################################################
# 8) pt_pvem = pt + pvem if municipality=="HUNUCMA", zero out pt, pvem
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    pt_pvem = if_else(
      municipality == "HUNUCMA",
      coalesce(pt, 0) + coalesce(pvem, 0),
      pt_pvem
    ),
    pt  = if_else(municipality=="HUNUCMA", 0, pt),
    pvem= if_else(municipality=="HUNUCMA", 0, pvem)
  )

################################################################################
# 9) pan_prd = pan + prd in a large list of municipalities, then zero out pan, prd
################################################################################
munis_pan_prd <- c(
  "BACA", "BOKOBA", "BUCTZOTZ", "CALOTMUL", "CELESTUN", 
  "CENOTILLO", "CHOCHOLA", "CHUMAYEL", "DZAN", "DZEMUL", "DZILAM BRAVO",
  "DZILAM GONZALEZ", "DZONCAUICH", "ESPITA", "HOCABA", "KANASIN", "KOPOMA",
  "MAMA", "MANI", "MAYAPAN", "MOTUL", "MUXUPIP", "MERIDA", "OPICHEN", "PETO",
  "PROGRESO", "QUINTANA ROO", "RIO LAGARTOS", "SAN FELIPE", "SANAHCAT",
  "SANTA ELENA", "SUDZAL", "TAHDZIU", "TEKAL DE VENEGAS", "TEKANTO",
  "TELCHAC PUERTO", "TEPAKAN", "TIMUCUY", "TIXKOKOB", "TIXMEUAC", "TIZIMIN",
  "UAYMA", "YAXCABA", "YAXKUKUL", "YOBAIN"
)

df_collapsed <- df_collapsed %>%
  mutate(
    pan_prd = if_else(
      municipality %in% munis_pan_prd,
      coalesce(pan, 0) + coalesce(prd, 0),
      pan_prd
    ),
    pan = if_else(municipality %in% munis_pan_prd, 0, pan),
    prd = if_else(municipality %in% munis_pan_prd, 0, prd)
  )

################################################################################
# 10) Rename columns to final names
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN            = pan,
    PAN_PRD        = pan_prd,
    PAN_PVEM       = pan_pvem,
    PRI            = pri,
    PRD            = prd,
    PRD_PT_PVEM    = prd_pt_pvem,
    PT             = pt,
    PT_PC          = pt_cpd,   # interpret cpd -> PC
    PT_PVEM        = pt_pvem,
    PVEM           = pvem,
    PC             = cpd,      # cpd -> PC (if that’s the correct interpretation)
    PAS            = pas,
    PY             = py,
    PAY            = pay,
    Otros          = otros
  )

################################################################################
# 11) turnout = total/listanominal; drop nulos if it exists
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    turnout = total / listanominal
  ) %>%
  select(-any_of("nulos"))

################################################################################
# 12) Create uniqueid=0, then assign codes for each municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "ABALA"           ~ 31001,
      municipality == "ACANCEH"         ~ 31002,
      municipality == "AKIL"            ~ 31003,
      municipality == "BACA"            ~ 31004,
      municipality == "BOKOBA"          ~ 31005,
      municipality == "BUCTZOTZ"        ~ 31006,
      municipality == "CACALCHEN"       ~ 31007,
      municipality == "CALOTMUL"        ~ 31008,
      municipality == "CANSAHCAB"       ~ 31009,
      municipality == "CANTAMAYEC"      ~ 31010,
      municipality == "CELESTUN"        ~ 31011,
      municipality == "CENOTILLO"       ~ 31012,
      municipality == "CHACSINKIN"      ~ 31016,
      municipality == "CHANKOM"         ~ 31017,
      municipality == "CHAPAB"          ~ 31018,
      municipality == "CHEMAX"          ~ 31019,
      municipality == "CHICHIMILA"      ~ 31021,
      municipality == "CHICXULUB PUEBLO"~ 31020,
      municipality == "CHIKINDZONOT"    ~ 31022,
      municipality == "CHOCHOLA"        ~ 31023,
      municipality == "CHUMAYEL"        ~ 31024,
      municipality == "CONKAL"          ~ 31013,
      municipality == "CUNCUNUL"        ~ 31014,
      municipality == "CUZAMA"          ~ 31015,
      municipality == "DZAN"            ~ 31025,
      municipality == "DZEMUL"          ~ 31026,
      municipality == "DZIDZANTUN"      ~ 31027,
      municipality == "DZILAM BRAVO"    ~ 31028,
      municipality == "DZILAM GONZALEZ" ~ 31029,
      municipality == "DZITAS"          ~ 31030,
      municipality == "DZONCAUICH"      ~ 31031,
      municipality == "ESPITA"          ~ 31032,
      municipality == "HALACHO"         ~ 31033,
      municipality == "HOCABA"          ~ 31034,
      municipality == "HOCTUN"          ~ 31035,
      municipality == "HOMUN"           ~ 31036,
      municipality == "HUHI"            ~ 31037,
      municipality == "HUNUCMA"         ~ 31038,
      municipality == "IXIL"            ~ 31039,
      municipality == "IZAMAL"          ~ 31040,
      municipality == "KANASIN"         ~ 31041,
      municipality == "KANTUNIL"        ~ 31042,
      municipality == "KAUA"            ~ 31043,
      municipality == "KINCHIL"         ~ 31044,
      municipality == "KOPOMA"          ~ 31045,
      municipality == "MAMA"            ~ 31046,
      municipality == "MANI"            ~ 31047,
      municipality == "MAXCANU"         ~ 31048,
      municipality == "MAYAPAN"         ~ 31049,
      municipality == "MERIDA"          ~ 31050,
      municipality == "MOCOCHA"         ~ 31051,
      municipality == "MOTUL"           ~ 31052,
      municipality == "MUNA"            ~ 31053,
      municipality == "MUXUPIP"         ~ 31054,
      municipality == "OPICHEN"         ~ 31055,
      municipality == "OXKUTZCAB"       ~ 31056,
      municipality == "PANABÁ"          ~ 31057,
      municipality == "PETO"            ~ 31058,
      municipality == "PROGRESO"        ~ 31059,
      municipality == "QUINTANA ROO"    ~ 31060,
      municipality == "RIO LAGARTOS"    ~ 31061,
      municipality == "SACALUM"         ~ 31062,
      municipality == "SAMAHIL"         ~ 31063,
      municipality == "SAN FELIPE"      ~ 31065,
      municipality == "SANAHCAT"        ~ 31064,
      municipality == "SANTA ELENA"     ~ 31066,
      municipality == "SEYE"            ~ 31067,
      municipality == "SINANCHE"        ~ 31068,
      municipality == "SOTUTA"          ~ 31069,
      municipality == "SUCILÁ"          ~ 31070,
      municipality == "SUDZAL"          ~ 31071,
      municipality == "SUMA DE HIDALGO" ~ 31072,
      municipality == "TAHDZIU"         ~ 31073,
      municipality == "TAHMEK"          ~ 31074,
      municipality == "TEABO"           ~ 31075,
      municipality == "TECOH"           ~ 31076,
      municipality == "TEKAL DE VENEGAS"~ 31077,
      municipality == "TEKANTO"         ~ 31078,
      municipality == "TEKAX"           ~ 31079,
      municipality == "TEKIT"           ~ 31080,
      municipality == "TEKOM"           ~ 31081,
      municipality == "TELCHAC PUEBLO"  ~ 31082,
      municipality == "TELCHAC PUERTO"  ~ 31083,
      municipality == "TEMAX"           ~ 31084,
      municipality == "TEMOZON"         ~ 31085,
      municipality == "TEPAKAN"         ~ 31086,
      municipality == "TETIZ"           ~ 31087,
      municipality == "TEYA"            ~ 31088,
      municipality == "TICUL"           ~ 31089,
      municipality == "TIMUCUY"         ~ 31090,
      municipality == "TINUM"           ~ 31091,
      municipality == "TIXCACALCUPUL"   ~ 31092,
      municipality == "TIXKOKOB"        ~ 31093,
      municipality == "TIXMEUAC"        ~ 31094,
      municipality == "TIXPEHUAL"       ~ 31095,
      municipality == "TIZIMIN"         ~ 31096,
      municipality == "TUNKAS"          ~ 31097,
      municipality == "TZUCACAB"        ~ 31098,
      municipality == "UAYMA"           ~ 31099,
      municipality == "UCU"             ~ 31100,
      municipality == "UMAN"            ~ 31101,
      municipality == "VALLADOLID"      ~ 31102,
      municipality == "XOCCHEL"         ~ 31103,
      municipality == "YAXCABA"         ~ 31104,
      municipality == "YAXKUKUL"        ~ 31105,
      municipality == "YOBAIN"          ~ 31106,
      TRUE                               ~ 0
    )
  )

################################################################################
# 13) Create valid = rowtotal(PAN PRI PRD PT PVEM PC PAS PY PAY Otros
#                             PAN_PVEM PRD_PT_PVEM PT_PC PT_PVEM PAN_PRD)
#     Then year=2001, month="May", sort section, and save
################################################################################
df_2001 <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(
      PAN, PRI, PRD, PT, PVEM, PC, PAS, PY, PAY, Otros, 
      PAN_PVEM, PRD_PT_PVEM, PT_PC, PT_PVEM, PAN_PRD
    )), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2001,
    month = "May"
  ) %>%
  arrange(section)

################################################################################
# 1) Read CSV (Equivalent to: insheet using "Ayu_Seccion_2004.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_2004.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns (municipio -> municipality, secc -> section)
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion
  )

################################################################################
# 3) Drop rows if municipality=="" & section==. (NA), or total==. or 0
################################################################################
df <- df %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 4) Destring (listanominal through total), then collapse by municipality, section
#    (Stata: destring listanominal - total, replace; collapse (sum) listanominal - total...)
################################################################################
df <- df %>%
  mutate(across(listanominal:total, as.numeric))

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:total, sum, na.rm = TRUE), .groups = "drop")

################################################################################
# 5) Rename columns
#    pan->PAN, pri->PRI, pripvem->PRI_PVEM, prd->PRD, pt->PT, pvem->PVEM,
#    conv->PC, py->PY, pay->PAY
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN      = pan,
    PRI      = pri,
    PRI_PVEM = pripvem,
    PRD      = prd,
    PT       = pt,
    PVEM     = pvem,
    PC       = conv,
    PY       = py,
    PAY      = pay
  )

################################################################################
# 6) If municipality=="Tekit" => PRI_PVEM = PRI + PVEM + PRI_PVEM, then set PRI=0, PVEM=0
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    PRI_PVEM = if_else(
      municipality == "Tekit",
      coalesce(PRI_PVEM, 0) + coalesce(PRI, 0) + coalesce(PVEM, 0),
      PRI_PVEM
    ),
    PRI      = if_else(municipality=="Tekit", 0, PRI),
    PVEM     = if_else(municipality=="Tekit", 0, PVEM)
  )

################################################################################
# 7) turnout = total / listanominal; drop nulos, noreg if they exist
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    turnout = total / listanominal
  ) %>%
  select(-any_of(c("nulos", "noreg")))

################################################################################
# 8) Create uniqueid=0, then assign codes for each municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "AbaLá"                  ~ 31001,
      municipality == "Acanceh"                ~ 31002,
      municipality == "Akil"                   ~ 31003,
      municipality == "Baca"                   ~ 31004,
      municipality == "Bokobá"                 ~ 31005,
      municipality == "Buctzotz"               ~ 31006,
      municipality == "Cacalchén"              ~ 31007,
      municipality == "Calotmul"               ~ 31008,
      municipality == "Cansahcab"              ~ 31009,
      municipality == "Cantamayec"             ~ 31010,
      municipality == "Celestún"               ~ 31011,
      municipality == "Cenotillo"              ~ 31012,
      municipality == "Chacsinkín"             ~ 31016,
      municipality == "Chankom"                ~ 31017,
      municipality == "Chapab"                 ~ 31018,
      municipality == "Chemax"                 ~ 31019,
      municipality == "Chichimilá"             ~ 31021,
      municipality == "Chicxulub Pueblo"       ~ 31020,
      municipality == "Chikindzonot"           ~ 31022,
      municipality == "Chocholá"               ~ 31023,
      municipality == "Chumayel"               ~ 31024,
      municipality == "Conkal"                 ~ 31013,
      municipality == "Cuncunul"               ~ 31014,
      municipality == "Cuzumá"                 ~ 31015,
      municipality == "Dzan"                   ~ 31025,
      municipality == "Dzemul"                 ~ 31026,
      municipality == "Dzidzantún"             ~ 31027,
      municipality == "Dizlam de Bravo"        ~ 31028,
      municipality == "Dzilam González"        ~ 31029,
      municipality == "Dzitás"                 ~ 31030,
      municipality == "Dzoncauich"             ~ 31031,
      municipality == "Espita"                 ~ 31032,
      municipality == "Halachó"                ~ 31033,
      municipality == "Hocabá"                 ~ 31034,
      municipality == "Hoctún"                 ~ 31035,
      municipality == "Homún"                  ~ 31036,
      municipality == "Huhí"                   ~ 31037,
      municipality == "Hunucmá"                ~ 31038,
      municipality == "Ixil"                   ~ 31039,
      municipality == "Izamal"                 ~ 31040,
      municipality == "Kanasín"                ~ 31041,
      municipality == "Kantunil"               ~ 31042,
      municipality == "Kaua"                   ~ 31043,
      municipality == "Kinchil"                ~ 31044,
      municipality == "Kopomá"                 ~ 31045,
      municipality == "Mama"                   ~ 31046,
      municipality == "Maní"                   ~ 31047,
      municipality == "Maxcanú"                ~ 31048,
      municipality == "Mayapán"                ~ 31049,
      municipality == "Mérida"                 ~ 31050,
      municipality == "Mocochá"                ~ 31051,
      municipality == "Motul"                  ~ 31052,
      municipality == "Muna"                   ~ 31053,
      municipality == "Muxupip"                ~ 31054,
      municipality == "Opichén"                ~ 31055,
      municipality == "Oxkutzcab"              ~ 31056,
      municipality == "Panabá"                 ~ 31057,
      municipality == "Peto"                   ~ 31058,
      municipality == "Progreso"               ~ 31059,
      municipality == "Quintana Roo"           ~ 31060,
      municipality == "Río Lagartos"           ~ 31061,
      municipality == "Sacalum"                ~ 31062,
      municipality == "Samahil"                ~ 31063,
      municipality == "San Felipe"             ~ 31065,
      municipality == "Sanahcat"               ~ 31064,
      municipality == "Santa Elena"            ~ 31066,
      municipality == "Seyé"                   ~ 31067,
      municipality == "Sinanché"               ~ 31068,
      municipality == "Sotuta"                 ~ 31069,
      municipality == "Sucilá"                 ~ 31070,
      municipality == "Sudzal"                 ~ 31071,
      municipality == "Suma de Hidalgo"        ~ 31072,
      municipality == "Tahdziú"                ~ 31073,
      municipality == "Tahmek"                 ~ 31074,
      municipality == "Teabo"                  ~ 31075,
      municipality == "Tecoh"                  ~ 31076,
      municipality == "Tekal de Venegas"       ~ 31077,
      municipality == "Tekantó"                ~ 31078,
      municipality == "Tekax"                  ~ 31079,
      municipality == "Tekit"                  ~ 31080,
      municipality == "Tekom"                  ~ 31081,
      municipality == "Telchac Pueblo"         ~ 31082,
      municipality == "Telchac Puerto"         ~ 31083,
      municipality == "Temax"                  ~ 31084,
      municipality == "Temozón"                ~ 31085,
      municipality == "Tepakán"                ~ 31086,
      municipality == "Tetiz"                  ~ 31087,
      municipality == "Teya"                   ~ 31088,
      municipality == "Ticul"                  ~ 31089,
      municipality == "Timucuy"                ~ 31090,
      municipality == "Tinum"                  ~ 31091,
      municipality == "Tixcacalcupul"          ~ 31092,
      municipality == "Tixkokob"               ~ 31093,
      municipality == "Tixmehuac"              ~ 31094,
      municipality == "Tixpehual"              ~ 31095,
      municipality == "Tizimín"                ~ 31096,
      municipality == "Tunkás"                 ~ 31097,
      municipality == "Tzucacab"               ~ 31098,
      municipality == "Uayma"                  ~ 31099,
      municipality == "Ucú"                    ~ 31100,
      municipality == "Umán"                   ~ 31101,
      municipality == "Valladolid"             ~ 31102,
      municipality == "Xocchel"                ~ 31103,
      municipality == "Yaxcabá"                ~ 31104,
      municipality == "Yaxkukul"               ~ 31105,
      municipality == "Yobaín"                 ~ 31106,
      TRUE                                     ~ 0
    )
  )

################################################################################
# 9) Create valid = rowtotal(PAN PRI PRI_PVEM PRD PT PVEM PC PY PAY)
#    Then year=2004, month="May", sort by section, and save
################################################################################
df_2004 <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(
      PAN, PRI, PRI_PVEM, PRD, PT, PVEM, PC, PY, PAY
    )), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2004,
    month = "May"
  ) %>%
  arrange(section)

################################################################################
# 1) Read CSV (equivalent to insheet using "Ayu_Seccion_2007.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_2007.csv", show_col_types = FALSE)
names(df) <- gsub("[. ]", "", names(df))
colnames(df) <- tolower(colnames(df))
################################################################################
# 2) Rename columns, drop rows if municipality is empty & section missing, 
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
# 3) Convert (listanominal, pan through total) to numeric 
#    (equivalent to "destring listanominal pan - total, replace")
################################################################################
df <- df %>%
  mutate(across(c(listanominal, pan:total), as.numeric))

################################################################################
# 4) Collapse (sum) listanominal, pan - total by (municipality, section)
#    (Stata: collapse (sum) listanominal pan - total, by(municipality section))
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    across(c(listanominal, pan:total), sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 5) Combine `nulosdirectos + marginalX` => nulosdirectos, then rename -> nulos
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    nulosdirectos = coalesce(nulosdirectos, 0) +
      coalesce(marginal1, 0) +
      coalesce(marginal2, 0) +
      coalesce(marginal3, 0) +
      coalesce(marginal4, 0)
  ) %>%
  select(-marginal1, -marginal2, -marginal3, -marginal4) %>%
  rename(nulos = nulosdirectos)

################################################################################
# 6) Create ptpc = pt + pc, then drop pt, pc
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(ptpc = coalesce(pt, 0) + coalesce(pc, 0)) %>%
  select(-pt, -pc)

################################################################################
# 7) Rename columns: 
#    pan->PAN, pri->PRI, prd->PRD, ptpc->PT_PC, pvem->PVEM, pay->PAY, panal->PANAL, pas->PAS, ci->CI
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN    = pan,
    PRI    = pri,
    PRD    = prd,
    PT_PC  = ptpc,
    PVEM   = pvem,
    PAY    = pay,
    PANAL  = panal,
    PAS    = pas,
    CI     = ci
  )

################################################################################
# 8) turnout = total / listanominal, drop nulos
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal) %>%
  select(-nulos)

################################################################################
# 9) Create uniqueid=0, then assign codes for each municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "ABALA"           ~ 31001,
      municipality == "ACANCEH"         ~ 31002,
      municipality == "AKIL"            ~ 31003,
      municipality == "BACA"            ~ 31004,
      municipality == "BOKOBA"          ~ 31005,
      municipality == "BUCTZOTZ"        ~ 31006,
      municipality == "CACALCHEN"       ~ 31007,
      municipality == "CALOTMUL"        ~ 31008,
      municipality == "CANSAHCAB"       ~ 31009,
      municipality == "CANTAMAYEC"      ~ 31010,
      municipality == "CELESTUN"        ~ 31011,
      municipality == "CENOTILLO"       ~ 31012,
      municipality == "CHACSINKIN"      ~ 31016,
      municipality == "CHANKOM"         ~ 31017,
      municipality == "CHAPAB"          ~ 31018,
      municipality == "CHEMAX"          ~ 31019,
      municipality == "CHICHIMILA"      ~ 31021,
      municipality == "CHICXULUB PUEBLO"~ 31020,
      municipality == "CHIKINDZONOT"    ~ 31022,
      municipality == "CHOCHOLA"        ~ 31023,
      municipality == "CHUMAYEL"        ~ 31024,
      municipality == "CONKAL"          ~ 31013,
      municipality == "CUNCUNUL"        ~ 31014,
      municipality == "CUZAMA"          ~ 31015,
      municipality == "DZAN"            ~ 31025,
      municipality == "DZEMUL"          ~ 31026,
      municipality == "DZIDZANTUN"      ~ 31027,
      municipality == "DZILAM DE BRAVO" ~ 31028,
      municipality == "DZILAM GONZALEZ" ~ 31029,
      municipality == "DZITAS"          ~ 31030,
      municipality == "DZONCAUICH"      ~ 31031,
      municipality == "ESPITA"          ~ 31032,
      municipality == "HALACHO"         ~ 31033,
      municipality == "HOCABA"          ~ 31034,
      municipality == "HOCTUN"          ~ 31035,
      municipality == "HOMUN"           ~ 31036,
      municipality == "HUHI"            ~ 31037,
      municipality == "HUNUCMA"         ~ 31038,
      municipality == "IXIL"            ~ 31039,
      municipality == "IZAMAL"          ~ 31040,
      municipality == "KANASIN"         ~ 31041,
      municipality == "KANTUNIL"        ~ 31042,
      municipality == "KAUA"            ~ 31043,
      municipality == "KINCHIL"         ~ 31044,
      municipality == "KOPOMA"          ~ 31045,
      municipality == "MAMA"            ~ 31046,
      municipality == "MANI"            ~ 31047,
      municipality == "MAXCANU"         ~ 31048,
      municipality == "MAYAPAN"         ~ 31049,
      municipality == "MERIDA"          ~ 31050,
      municipality == "MOCOCHA"         ~ 31051,
      municipality == "MOTUL"           ~ 31052,
      municipality == "MUNA"            ~ 31053,
      municipality == "MUXUPIP"         ~ 31054,
      municipality == "OPICHEN"         ~ 31055,
      municipality == "OXKUTZCAB"       ~ 31056,
      municipality == "PANABA"          ~ 31057,
      municipality == "PETO"            ~ 31058,
      municipality == "PROGRESO"        ~ 31059,
      municipality == "QUINTANA ROO"    ~ 31060,
      municipality == "RIO LAGARTOS"    ~ 31061,
      municipality == "SACALUM"         ~ 31062,
      municipality == "SAMAHIL"         ~ 31063,
      municipality == "SAN FELIPE"      ~ 31065,
      municipality == "SANAHCAT"        ~ 31064,
      municipality == "SANTA ELENA"     ~ 31066,
      municipality == "SEYE"            ~ 31067,
      municipality == "SINANCHE"        ~ 31068,
      municipality == "SOTUTA"          ~ 31069,
      municipality == "SUCILA"          ~ 31070,
      municipality == "SUDZAL"          ~ 31071,
      municipality == "SUMA"            ~ 31072,
      municipality == "TAHDZIU"         ~ 31073,
      municipality == "TAHMEK"          ~ 31074,
      municipality == "TEABO"           ~ 31075,
      municipality == "TECOH"           ~ 31076,
      municipality == "TEKAL DE VENEGAS"~ 31077,
      municipality == "TEKANTO"         ~ 31078,
      municipality == "TEKAX"           ~ 31079,
      municipality == "TEKIT"           ~ 31080,
      municipality == "TEKOM"           ~ 31081,
      municipality == "TELCHAC PUEBLO"  ~ 31082,
      municipality == "TELCHAC PUERTO"  ~ 31083,
      municipality == "TEMAX"           ~ 31084,
      municipality == "TEMOZON"         ~ 31085,
      municipality == "TEPAKAN"         ~ 31086,
      municipality == "TETIZ"           ~ 31087,
      municipality == "TEYA"            ~ 31088,
      municipality == "TICUL"           ~ 31089,
      municipality == "TIMUCUY"         ~ 31090,
      municipality == "TINUM"           ~ 31091,
      municipality == "TIXCACALCUPUL"   ~ 31092,
      municipality == "TIXKOKOB"        ~ 31093,
      municipality == "TIXMEHUAC"       ~ 31094,
      municipality == "TIXPEHUAL"       ~ 31095,
      municipality == "TIZIMIN"         ~ 31096,
      municipality == "TUNKAS"          ~ 31097,
      municipality == "TZUCACAB"        ~ 31098,
      municipality == "UAYMA"           ~ 31099,
      municipality == "UCU"             ~ 31100,
      municipality == "UMAN"            ~ 31101,
      municipality == "VALLADOLID"      ~ 31102,
      municipality == "XOCCHEL"         ~ 31103,
      municipality == "YAXCABA"         ~ 31104,
      municipality == "YAXKUKUL"        ~ 31105,
      municipality == "YOBAIN"          ~ 31106,
      TRUE                              ~ 0
    )
  )

################################################################################
# 10) Create valid = rowtotal(PAN PRI PRD PVEM PAY PANAL PAS CI PT_PC)
#     Then year=2007, month="May", sort by section
################################################################################
df_2007 <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(PAN, PRI, PRD, PVEM, PAY, PANAL, PAS, CI, PT_PC)),
                na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2007,
    month = "May"
  ) %>%
  arrange(section)

################################################################################
# 1) Read CSV (Equivalent to insheet using "Ayu_Seccion_2010.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_2010.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns, drop rows if municipality == "" & section is missing,
#    and also if total is missing or zero
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
# 3) Convert (listanominal, pan through total) to numeric (Stata: destring ...)
################################################################################
df <- df %>%
  mutate(across(c(listanominal, pan:total), as.numeric))

################################################################################
# 4) Collapse (sum) listanominal, pan - total by (municipality, section)
#    (like collapse (sum) listanominal pan - total, by(municipality section))
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan:total), sum, na.rm = TRUE), .groups = "drop")

################################################################################
# 5) Merge PRI & PVEM into pripvem for specific municipalities:
#    if municipality in certain list => pripvem = pri + pvem + pripvem; then zero out pri, pvem
#    e.g. (Stata: replace pripvem=pri + pvem + pripvem if municipality=="CELESTÍN" ... or ... "DZIDZANTÍN" etc.)
#
#    We'll define a character vector of those municipality names, then do the sums.
################################################################################
merge_pri_pvem_munis <- c(
  "CELESTÍN", "DZIDZANTÍN", "ESPITA", "IZAMAL", "KANASÍN", "MERIDA",
  "PETO", "SEYE", "TEKAX", "TEMOZÓN", "TICUL", "TINUM", "TIZIMÍN",
  "UMÁN", "VALLADOLID", "YAXCABA"
)

df_collapsed <- df_collapsed %>%
  mutate(
    pripvem = if_else(
      municipality %in% merge_pri_pvem_munis,
      coalesce(pripvem, 0) + coalesce(pri, 0) + coalesce(pvem, 0),
      pripvem
    ),
    pri   = if_else(municipality %in% merge_pri_pvem_munis, 0, pri),
    pvem  = if_else(municipality %in% merge_pri_pvem_munis, 0, pvem)
  )

################################################################################
# 6) Rename columns to final versions:
#    rename pan->PAN, pri->PRI (but pri was zeroed out?), ...
#    Also rename pri->PRI_PVEM (?), prd->PRD, pt->PT, pvem->PVEM, pay->PAY,
#    panal->PANAL, pc->PC
################################################################################
# The code shows a confusion: 
#   rename pan->PAN
#   rename pri->PRI
#   rename pri->PRI_PVEM
# So we must interpret carefully. Possibly the final "rename pri -> PRI_PVEM" is
# actually for `pripvem` column. We'll assume that's the case. We'll do step by step.

df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan,          # from code: rename pan PAN
    # The user wrote "rename pri PRI" then "rename pri PRI_PVEM", 
    #   but we suspect it's actually rename pripvem -> PRI_PVEM
    PRI_PVEM = pripvem, # let's interpret that the final name for pripvem is PRI_PVEM
    PRD = prd,
    PT  = pt,
    PVEM = pvem,
    PAY  = pay,
    PANAL= panal,
    PC   = pc
  )

################################################################################
# 7) Compute turnout = total / listanominal; drop nulos, noregistrados if present
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal) %>%
  select(-any_of(c("nulos", "noregistrados")))

################################################################################
# 8) Create uniqueid=0, then assign codes for each municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "ABALÁ"                      ~ 31001,
      municipality == "ACANCEH"                    ~ 31002,
      municipality == "AKIL"                       ~ 31003,
      municipality == "BACA"                       ~ 31004,
      municipality == "BOKOBÁ"                     ~ 31005,
      municipality == "BUCTZOTZ"                   ~ 31006,
      municipality == "CACALCHÁN"                  ~ 31007,
      municipality == "CALOTMUL"                   ~ 31008,
      municipality == "CANSAHCAB"                  ~ 31009,
      municipality == "CANTAMAYEC"                 ~ 31010,
      municipality == "CELESTÍN"                   ~ 31011,
      municipality == "CENOTILLO"                  ~ 31012,
      municipality == "CHACSINKÍN"                 ~ 31016,
      municipality == "CHANKOM"                    ~ 31017,
      municipality == "CHAPAB"                     ~ 31018,
      municipality == "CHEMAX"                     ~ 31019,
      municipality == "CHICHIMILÁ"                 ~ 31021,
      municipality == "CHICXULUB PUEBLO"           ~ 31020,
      municipality == "CHIKINDZONOT"               ~ 31022,
      municipality == "CHOCHOLÁ"                   ~ 31023,
      municipality == "CHUMAYEL"                   ~ 31024,
      municipality == "CONKAL"                     ~ 31013,
      municipality == "CUNCUNUL"                   ~ 31014,
      municipality == "CUZAMÁ"                     ~ 31015,
      municipality == "DZAN"                       ~ 31025,
      municipality == "DZEMUL"                     ~ 31026,
      municipality == "DZIDZANTÚN"                 ~ 31027,
      municipality == "DZILAM DE BRAVO"            ~ 31028,
      municipality == "DZILAM GONZALEZ"            ~ 31029,
      municipality == "DZITAS"                     ~ 31030,
      municipality == "DZONCAUICH"                 ~ 31031,
      municipality == "ESPITA"                     ~ 31032,
      municipality == "HALACHÓ"                    ~ 31033,
      municipality == "HOCABA"                     ~ 31034,
      municipality == "HOCTUN"                     ~ 31035,
      municipality == "HOMÚN"                      ~ 31036,
      municipality == "HUHI"                       ~ 31037,
      municipality == "HUNUCMÁ"                    ~ 31038,
      municipality == "IXIL"                       ~ 31039,
      municipality == "IZAMAL"                     ~ 31040,
      municipality == "KANASÍN"                    ~ 31041,
      municipality == "KANTUNIL"                   ~ 31042,
      municipality == "KAUA"                       ~ 31043,
      municipality == "KINCHIL"                    ~ 31044,
      municipality == "KOPOMÁ"                     ~ 31045,
      municipality == "MAMA"                       ~ 31046,
      municipality == "MANÍ"                       ~ 31047,
      municipality == "MAXCANÚ"                    ~ 31048,
      municipality == "MAYAPAN"                    ~ 31049,
      municipality == "MERIDA"                     ~ 31050,
      municipality == "MOCOCHÁ"                    ~ 31051,
      municipality == "MOTUL"                      ~ 31052,
      municipality == "MUNA"                       ~ 31053,
      municipality == "MUXUPIP"                    ~ 31054,
      municipality == "OPICHÉN"                    ~ 31055,
      municipality == "OXKUTZCAB"                  ~ 31056,
      municipality == "PANABÁ"                     ~ 31057,
      municipality == "PETO"                       ~ 31058,
      municipality == "PROGRESO"                   ~ 31059,
      municipality == "QUINTANA ROO"               ~ 31060,
      municipality == "RIO LAGARTOS"               ~ 31061,
      municipality == "SACALUM"                    ~ 31062,
      municipality == "SAMAHIL"                    ~ 31063,
      municipality == "SAN FELIPE"                 ~ 31065,
      municipality == "SANAHCAT"                   ~ 31064,
      municipality == "SANTA ELENA"                ~ 31066,
      municipality == "SEYE"                       ~ 31067,
      municipality == "SINANCHÉ"                   ~ 31068,
      municipality == "SOTUTA"                     ~ 31069,
      municipality == "SUCILÁ"                     ~ 31070,
      municipality == "SUDZAL"                     ~ 31071,
      municipality == "SUMA"                       ~ 31072,
      municipality == "TAHDZIU"                    ~ 31073,
      municipality == "TAHMEK"                     ~ 31074,
      municipality == "TEABO"                      ~ 31075,
      municipality == "TECOH"                      ~ 31076,
      municipality == "TEKAL DE VENEGAS"           ~ 31077,
      municipality == "TEKANTO"                    ~ 31078,
      municipality == "TEKAX"                      ~ 31079,
      municipality == "TEKIT"                      ~ 31080,
      municipality == "TEKOM"                      ~ 31081,
      municipality == "TELCHAC PUEBLO"             ~ 31082,
      municipality == "TELCHAC PUERTO"             ~ 31083,
      municipality == "TEMAX"                      ~ 31084,
      municipality == "TEMOZÓN"                    ~ 31085,
      municipality == "TEPAKAN"                    ~ 31086,
      municipality == "TETIZ"                      ~ 31087,
      municipality == "TEYA"                       ~ 31088,
      municipality == "TICUL"                      ~ 31089,
      municipality == "TIMUCUY"                    ~ 31090,
      municipality == "TINUM"                      ~ 31091,
      municipality == "TIXCACALCUPUL"              ~ 31092,
      municipality == "TIXKOKOB"                   ~ 31093,
      municipality == "TIXMEHUAC"                  ~ 31094,
      municipality == "TIXPEHUAL"                  ~ 31095,
      municipality == "TIZIMÍN"                    ~ 31096,
      municipality == "TUNKAS"                     ~ 31097,
      municipality == "TZUCACAB"                   ~ 31098,
      municipality == "UAYMA"                      ~ 31099,
      municipality == "UCÚ"                        ~ 31100,
      municipality == "UMÁN"                       ~ 31101,
      municipality == "VALLADOLID"                 ~ 31102,
      municipality == "XOCCHEL"                    ~ 31103,
      municipality == "YAXCABA"                    ~ 31104,
      municipality == "YAXKUKUL"                   ~ 31105,
      municipality == "YOBAÍN"                     ~ 31106,
      TRUE                                         ~ 0
    )
  )

################################################################################
# 9) Compute valid = rowtotal(PAN PRI PRD PT PVEM PC PAY PANAL PRI_PVEM)
#    year=2010, month="May", then sort by section
################################################################################
df_2010 <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(PAN, PRI, PRD, PT, PVEM, PC, PAY, PANAL, PRI_PVEM)),
                na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2010,
    month = "May"
  ) %>%
  arrange(section)

################################################################################
# 1) First Excel: "Coaltion_2012.xlsx", reading the coalition flags
#    (Equivalent to: import excel "Coaltion_2012.xlsx", sheet("Sheet1") firstrow clear)
################################################################################
df_coalition <- read_excel(
  path = "../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Coaltion_2012.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()

################################################################################
# 2) For each party in (PRI_PVEM, PRI_PVEM_PSD, PRD_MC, PT_PRD_MC, PT_PRD_PSD, PT_PRD, PT_MC):
#    - replace party=0 if party==.
#    - replace party=(party>0) => effectively a 0/1 indicator
#    - rename party => c_party
################################################################################
party_vec <- c("PRI_PVEM", "PRI_PVEM_PSD", "PRD_MC", "PT_PRD_MC", 
               "PT_PRD_PSD", "PT_PRD", "PT_MC")

for (party in party_vec) {
  # replace party=0 if party==.
  df_coalition[[party]] <- ifelse(is.na(df_coalition[[party]]), 0, df_coalition[[party]])
  # replace party=(party>0)
  df_coalition[[party]] <- ifelse(df_coalition[[party]] > 0, 1, 0)
  # rename party => c_party
  new_name <- paste0("c_", party)
  names(df_coalition)[ names(df_coalition) == party ] <- new_name
}

################################################################################
# 3) Rename MUNICIPIO->municipality; replace "TIXCACALCUPU" if partial match
################################################################################
df_coalition <- df_coalition %>%
  rename(municipality = MUNICIPIO) %>%
  mutate(
    municipality = if_else(
      str_detect(municipality, "TIXCACALCUPU"),
      "TIXCACALCUPU",
      municipality
    )
  )

################################################################################
# 5) Second Excel: "Ayu_Seccion_2012.xlsx", main dataset
#    (Equivalent to: import excel "Ayu_Seccion_2012.xlsx", sheet("Sheet1") firstrow clear)
################################################################################
df_main <- read_excel(
  path = "../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayu_Seccion_2012.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()
names(df_main) <- gsub("[- ]", "", names(df_main))
################################################################################
# 6) Drop if Municipio==""; 
#    "count if Listanominal==." is just a check in Stata, not needed in R 
#    rename Municipio->municipality, Seccin->section, Listanominal->listanominal, TOTAL->total
################################################################################
df_main <- df_main %>%
  filter(Municipio != "") %>%
  rename(
    municipality = Municipio,
    section      = Sección,
    listanominal = Listanominal,
    total        = TOTAL
  )

################################################################################
# 7) Drop if municipality=="" & section==., or total==. or total==0
#    replace municipality="TIXCACALCUPU" if strpos(municipality,"TIXCACALCUPU")>0
################################################################################
df_main <- df_main %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0)) %>%
  mutate(
    municipality = if_else(
      str_detect(municipality, "TIXCACALCUPU"),
      "TIXCACALCUPU",
      municipality
    )
  )

################################################################################
# 8) Merge m:1 municipality using Coaltion_2012.dta
#    (In R, let's do a left_join or similar, 
#     then drop the rows that didn't match if needed)
################################################################################
df_coalition2 <- read_dta("Coaltion_2012.dta")

# We'll do a many-to-1 merge on "municipality" only:
df_merged <- df_main %>%
  left_join(df_coalition2, by="municipality")

# drop _merge in Stata => in R, we just remove rows with no match (if needed).
# If you truly want to replicate Stata's "merge m:1", 
# you might keep duplicates or handle them differently. 
# We'll assume we want to keep all from df_main. 
# If you want to drop unmatched, do:
df_merged <- df_merged %>%
  filter(!is.na(df_merged[[ "c_PRI_PVEM" ]])) # or any "c_" column

################################################################################
# 9) Next part in Stata is code for computing combined columns 
#    (PRI_PVEM, PRI_PVEM_PSD, PRD_MC, etc.) using the c_ flags. 
#    However, the user commented out a portion 
#    (the "foreach party in PRI_PVEM..." blocks).
#    They then create t_PRI_PVEM, t_PRI_PVEM_PSD, etc. based on c_ flags,
#    zero out certain columns if c_ flags are 1, etc.
#
#    We'll replicate the lines that are not commented.
################################################################################

df_merged <- df_merged %>%
  mutate(
    t_PRI_PVEM = if_else(c_PRI_PVEM == 1,
                         PRI_PVEM + PRI + PVEM,
                         NA_real_),
    PRI_PVEM = if_else(c_PRI_PVEM == 1, 0, PRI_PVEM),
    PRI = if_else(c_PRI_PVEM == 1, 0, PRI),
    PVEM= if_else(c_PRI_PVEM == 1, 0, PVEM),
    
    t_PRI_PVEM_PSD = if_else(c_PRI_PVEM_PSD == 1,
                             PRI_PVEM + PRI + PVEM + PSDYUC,
                             NA_real_),
    PRI_PVEM_PSD = if_else(c_PRI_PVEM_PSD == 1, 0, PRI_PVEM_PSD),
    PRI = if_else(c_PRI_PVEM_PSD == 1, 0, PRI),
    PVEM= if_else(c_PRI_PVEM_PSD == 1, 0, PVEM),
    PSDYUC= if_else(c_PRI_PVEM_PSD == 1, 0, PSDYUC),
    
    t_PRD_MC = if_else(c_PRD_MC == 1,
                       PRD_MC + PRD + MC,
                       NA_real_),
    PRD_MC= if_else(c_PRD_MC == 1, 0, PRD_MC),
    PRD   = if_else(c_PRD_MC == 1, 0, PRD),
    MC    = if_else(c_PRD_MC == 1, 0, MC),
    
    t_PT_PRD_MC= if_else(c_PT_PRD_MC == 1,
                         PT_PRD_MC + PT + PRD + MC,
                         NA_real_),
    PT_PRD_MC= if_else(c_PT_PRD_MC == 1, 0, PT_PRD_MC),
    PT= if_else(c_PT_PRD_MC == 1, 0, PT),
    PRD= if_else(c_PT_PRD_MC == 1, 0, PRD),
    MC= if_else(c_PT_PRD_MC == 1, 0, MC),
    
    t_PT_PRD_PSD = if_else(c_PT_PRD_PSD == 1,
                           PT_PRD_PSD + PT + PRD + PSDYUC,
                           NA_real_),
    PT_PRD_PSD= if_else(c_PT_PRD_PSD == 1, 0, PT_PRD_PSD),
    PT= if_else(c_PT_PRD_PSD == 1, 0, PT),
    PRD= if_else(c_PT_PRD_PSD == 1, 0, PRD),
    PSDYUC= if_else(c_PT_PRD_PSD == 1, 0, PSDYUC),
    
    t_PT_PRD = if_else(c_PT_PRD == 1,
                       PT_PRD + PT + PRD,
                       NA_real_),
    PT_PRD= if_else(c_PT_PRD == 1, 0, PT_PRD),
    PT= if_else(c_PT_PRD == 1, 0, PT),
    PRD= if_else(c_PT_PRD == 1, 0, PRD),
    
    t_PT_MC = if_else(c_PT_MC == 1,
                      PT_MC + PT + MC,
                      NA_real_),
    PT_MC= if_else(c_PT_MC == 1, 0, PT_MC),
    PT= if_else(c_PT_MC == 1, 0, PT),
    MC= if_else(c_PT_MC == 1, 0, MC)
  )

# drop columns we don't need:
df_merged <- df_merged %>%
  select(-c(PRI_PVEM_PSD, PT_PRD_MC, PT_PRD_PSD, PT_PRD, PRD_MC, PT_MC, PRI_PVEM))

# Then the code also drops c_*

df_merged <- df_merged %>%
  select(!starts_with("c_"))

################################################################################
# 10) "foreach party in PRI_PVEM PRI_PVEM_PSD PRD_MC PT_PRD_MC PT_PRD_PSD PT_PRD PT_MC {
#         rename t_`party' `party'
#       }"
################################################################################
rename_map <- c("t_PRI_PVEM"      ="PRI_PVEM",
                "t_PRI_PVEM_PSD" ="PRI_PVEM_PSD",
                "t_PRD_MC"       ="PRD_MC",
                "t_PT_PRD_MC"    ="PT_PRD_MC",
                "t_PT_PRD_PSD"   ="PT_PRD_PSD",
                "t_PT_PRD"       ="PT_PRD",
                "t_PT_MC"        ="PT_MC")

for (old_name in names(rename_map)) {
  new_name <- rename_map[[old_name]]
  if (old_name %in% names(df_merged)) {
    names(df_merged)[ names(df_merged)==old_name ] <- new_name
  }
}

################################################################################
# 11) destring listanominal pan - PT_MC , replace
#     Then collapse (sum) listanominal PAN - PT_MC , by (municipality section)
################################################################################
# We'll assume the relevant columns are from "PAN" through "PT_MC", plus "listanominal".
to_num <- c("listanominal", "PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", 
            "PSDYUC", "PRI_PVEM", "PRI_PVEM_PSD", "PRD_MC", "PT_PRD_MC", 
            "PT_PRD_PSD", "PT_PRD", "PT_MC", "total")
df_merged <- df_merged %>%
  mutate(across(all_of(intersect(to_num, names(df_merged))), as.numeric))

df_collapsed <- df_merged %>%
  group_by(municipality, section) %>%
  summarise(
    across(all_of(intersect(to_num, names(df_merged))), sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 12) gen turnout = total/listanominal; drop NOREGISTRADOS NULOS if they exist
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal) %>%
  select(-any_of(c("NOREGISTRADOS", "NULOS")))

################################################################################
# 13) Create uniqueid=0, then assign codes for each municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "ABALA"          ~ 31001,
      municipality == "ACANCEH"        ~ 31002,
      municipality == "AKIL"           ~ 31003,
      municipality == "BACA"           ~ 31004,
      municipality == "BOKOBA"         ~ 31005,
      municipality == "BUCTZOTZ"       ~ 31006,
      municipality == "CACALCHEN"      ~ 31007,
      municipality == "CALOTMUL"       ~ 31008,
      municipality == "CANSAHCAB"      ~ 31009,
      municipality == "CANTAMAYEC"     ~ 31010,
      municipality == "CELESTUN"       ~ 31011,
      municipality == "CENOTILLO"      ~ 31012,
      municipality == "CHACSINKIN"     ~ 31016,
      municipality == "CHANKOM"        ~ 31017,
      municipality == "CHAPAB"         ~ 31018,
      municipality == "CHEMAX"         ~ 31019,
      municipality == "CHICHIMILA"     ~ 31021,
      municipality == "CHICXULUB PUEBLO" ~ 31020,
      municipality == "CHIKINDZONOT"   ~ 31022,
      municipality == "CHOCHOLA"       ~ 31023,
      municipality == "CHUMAYEL"       ~ 31024,
      municipality == "CONKAL"         ~ 31013,
      municipality == "CUNCUNUL"       ~ 31014,
      municipality == "CUZAMA"         ~ 31015,
      municipality == "DZAN"           ~ 31025,
      municipality == "DZEMUL"         ~ 31026,
      municipality == "DZIDZANTUN"     ~ 31027,
      municipality == "DZILAM DE BRAVO"~ 31028,
      municipality == "DZILAM GONZALEZ"~ 31029,
      municipality == "DZITAS"         ~ 31030,
      municipality == "DZONCAUICH"     ~ 31031,
      municipality == "ESPITA"         ~ 31032,
      municipality == "HALACHO"        ~ 31033,
      municipality == "HOCABA"         ~ 31034,
      municipality == "HOCTUN"         ~ 31035,
      municipality == "HOMUN"          ~ 31036,
      municipality == "HUHI"           ~ 31037,
      municipality == "HUNUCMA"        ~ 31038,
      municipality == "IXIL"           ~ 31039,
      municipality == "IZAMAL"         ~ 31040,
      municipality == "KANASIN"        ~ 31041,
      municipality == "KANTUNIL"       ~ 31042,
      municipality == "KAUA"           ~ 31043,
      municipality == "KINCHIL"        ~ 31044,
      municipality == "KOPOMA"         ~ 31045,
      municipality == "MAMA"           ~ 31046,
      municipality == "MANI"           ~ 31047,
      municipality == "MAXCANU"        ~ 31048,
      municipality == "MAYAPAN"        ~ 31049,
      municipality == "MERIDA"         ~ 31050,
      municipality == "MOCOCHA"        ~ 31051,
      municipality == "MOTUL"          ~ 31052,
      municipality == "MUNA"           ~ 31053,
      municipality == "MUXUPIP"        ~ 31054,
      municipality == "OPICHEN"        ~ 31055,
      municipality == "OXKUTZCAB"      ~ 31056,
      municipality == "PANABA"         ~ 31057,
      municipality == "PETO"           ~ 31058,
      municipality == "PROGRESO"       ~ 31059,
      municipality == "QUINTANA ROO"   ~ 31060,
      municipality == "RIO LAGARTOS"   ~ 31061,
      municipality == "SACALUM"        ~ 31062,
      municipality == "SAMAHIL"        ~ 31063,
      municipality == "SAN FELIPE"     ~ 31065,
      municipality == "SANAHCAT"       ~ 31064,
      municipality == "SANTA ELENA"    ~ 31066,
      municipality == "SEYE"           ~ 31067,
      municipality == "SINANCHE"       ~ 31068,
      municipality == "SOTUTA"         ~ 31069,
      municipality == "SUCILA"         ~ 31070,
      municipality == "SUDZAL"         ~ 31071,
      municipality == "SUMA"           ~ 31072,
      municipality == "TAHDZIU"        ~ 31073,
      municipality == "TAHMEK"         ~ 31074,
      municipality == "TEABO"          ~ 31075,
      municipality == "TECOH"          ~ 31076,
      municipality == "TEKAL DE VENEGAS" ~ 31077,
      municipality == "TEKANTO"        ~ 31078,
      municipality == "TEKAX"          ~ 31079,
      municipality == "TEKIT"          ~ 31080,
      municipality == "TEKOM"          ~ 31081,
      municipality == "TELCHAC PUEBLO" ~ 31082,
      municipality == "TELCHAC PUERTO" ~ 31083,
      municipality == "TEMAX"          ~ 31084,
      municipality == "TEMOZON"        ~ 31085,
      municipality == "TEPAKAN"        ~ 31086,
      municipality == "TETIZ"          ~ 31087,
      municipality == "TEYA"           ~ 31088,
      municipality == "TICUL"          ~ 31089,
      municipality == "TIMUCUY"        ~ 31090,
      municipality == "TINUM"          ~ 31091,
      municipality == "TIXCACALCUPU"   ~ 31092,
      municipality == "TIXKOKOB"       ~ 31093,
      municipality == "TIXMEHUAC"      ~ 31094,
      municipality == "TIXPEHUAL"      ~ 31095,
      municipality == "TIZIMIN"        ~ 31096,
      municipality == "TUNKAS"         ~ 31097,
      municipality == "TZUCACAB"       ~ 31098,
      municipality == "UAYMA"          ~ 31099,
      municipality == "UCU"            ~ 31100,
      municipality == "UMAN"           ~ 31101,
      municipality == "VALLADOLID"     ~ 31102,
      municipality == "XOCCHEL"        ~ 31103,
      municipality == "YAXCABA"        ~ 31104,
      municipality == "YAXKUKUL"       ~ 31105,
      municipality == "YOBAIN"         ~ 31106,
      TRUE                             ~ 0
    )
  )

################################################################################
# 14) Create 'valid' = rowtotal(...) 
#     year=2012, month="July", sort by section, and finally save
################################################################################
df_2012 <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(
      PAN, PRI, PRD, PT, PVEM, MC, PANAL, PSDYUC, 
      PRI_PVEM, PRI_PVEM_PSD, PRD_MC, PT_PRD_MC, 
      PT_PRD_PSD, PT_PRD, PT_MC
    )), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2012,
    month = "July"
  ) %>%
  arrange(section)

################################################################################
# 1) Read "communal candidates 2015.xlsx" (Equivalent to 
#    import excel "communal candidates 2015.xlsx", sheet("Ayuntamientos") firstrow clear)
#    rename CC* -> comun*, and save "merge.dta"
################################################################################
df_communal <- read_excel(
  path = "../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/communal candidates 2015.xlsx",
  sheet = "Ayuntamientos",
  col_names = TRUE
) %>%
  as.data.frame()

# rename any columns starting with CC to start with comun
names(df_communal) <- sub("^CC", "comun", names(df_communal))

################################################################################
# 2) Read "Ayuntamientos_Yuc_2015.xlsx" (sheet "REGIDORES"), merge m:1 on 'municipality'
#    (Equivalent to: import excel "Ayuntamientos_Yuc_2015.xlsx", sheet("REGIDORES") firstrow clear
#                   merge m:1 municipality using "merge.dta"
#                   erase "merge.dta")
################################################################################
df_main <- read_excel(
  path = "../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayuntamientos_Yuc_2015.xlsx",
  sheet = "REGIDORES",
  col_names = TRUE
) %>%
  as.data.frame()

# merge m:1 municipality
df_merge <- df_main %>%
  left_join(df_communal, by = "municipality")

################################################################################
# 3) Create uniqueid=0, then fill municipality codes
################################################################################
df_merge <- df_merge %>%
  mutate(uniqueid = 0) %>%
  mutate(uniqueid = case_when(
    municipality == "ABALÁ"           ~ 31001,
    municipality == "ACANCEH"         ~ 31002,
    municipality == "AKIL"            ~ 31003,
    municipality == "BACA"            ~ 31004,
    municipality == "BOKOBÁ"          ~ 31005,
    municipality == "BUCTZOTZ"        ~ 31006,
    municipality == "CACALCHÉN"       ~ 31007,
    municipality == "CALOTMUL"        ~ 31008,
    municipality == "CANSAHCAB"       ~ 31009,
    municipality == "CANTAMAYEC"      ~ 31010,
    municipality == "CELESTÚN"        ~ 31011,
    municipality == "CENOTILLO"       ~ 31012,
    municipality == "CHACSINKÍN"      ~ 31016,
    municipality == "CHANKOM"         ~ 31017,
    municipality == "CHAPAB"          ~ 31018,
    municipality == "CHEMAX"          ~ 31019,
    municipality == "CHICHIMILÁ"      ~ 31021,
    municipality == "CHICXULUB PUEBLO"~ 31020,
    municipality == "CHIKINDZONOT"    ~ 31022,
    municipality == "CHOCHOLÁ"        ~ 31023,
    municipality == "CHUMAYEL"        ~ 31024,
    municipality == "CONKAL"          ~ 31013,
    municipality == "CUNCUNUL"        ~ 31014,
    municipality == "CUZAMÁ"          ~ 31015,
    municipality == "DZÁN"            ~ 31025,
    municipality == "DZEMUL"          ~ 31026,
    municipality == "DZIDZANTÚN"      ~ 31027,
    municipality == "DZILAM BRAVO"    ~ 31028,
    municipality == "DZILAM GONZÁLEZ" ~ 31029,
    municipality == "DZITÁS"          ~ 31030,
    municipality == "DZONCAUICH"      ~ 31031,
    municipality == "ESPITA"          ~ 31032,
    municipality == "HALACHÓ"         ~ 31033,
    municipality == "HOCABÁ"          ~ 31034,
    municipality == "HOCTÚN"          ~ 31035,
    municipality == "HOMÚN"           ~ 31036,
    municipality == "HUHÍ"            ~ 31037,
    municipality == "HUNUCMÁ"         ~ 31038,
    municipality == "IXIL"            ~ 31039,
    municipality == "IZAMAL"          ~ 31040,
    municipality == "KANASÍN"         ~ 31041,
    municipality == "KANTUNIL"        ~ 31042,
    municipality == "KAUA"            ~ 31043,
    municipality == "KINCHIL"         ~ 31044,
    municipality == "KOPOMÁ"          ~ 31045,
    municipality == "MAMA"            ~ 31046,
    municipality == "MANÍ"            ~ 31047,
    municipality == "MAXCANÚ"         ~ 31048,
    municipality == "MAYAPAN"         ~ 31049,
    municipality == "MERIDA"          ~ 31050,
    municipality == "MOCOCHÁ"         ~ 31051,
    municipality == "MOTUL"           ~ 31052,
    municipality == "MUNA"            ~ 31053,
    municipality == "MUXUPIP"         ~ 31054,
    municipality == "OPICHEN"         ~ 31055,
    municipality == "OXKUTZCAB"       ~ 31056,
    municipality == "PANABÁ"          ~ 31057,
    municipality == "PETO"            ~ 31058,
    municipality == "PROGRESO"        ~ 31059,
    municipality == "QUINTANA ROO"    ~ 31060,
    municipality == "RÍO LAGARTOS"    ~ 31061,
    municipality == "SACALUM"         ~ 31062,
    municipality == "SAMAHIL"         ~ 31063,
    municipality == "SAN FELIPE"      ~ 31065,
    municipality == "SANAHCAT"        ~ 31064,
    municipality == "SANTA ELENA"     ~ 31066,
    municipality == "SEYÉ"            ~ 31067,
    municipality == "SINANCHE"        ~ 31068,
    municipality == "SOTUTA"          ~ 31069,
    municipality == "SUCILÁ"          ~ 31070,
    municipality == "SUDZAL"          ~ 31071,
    municipality == "SUMA DE HIDALGO" ~ 31072,
    municipality == "TAHDZIÚ"         ~ 31073,
    municipality == "TAHMEK"          ~ 31074,
    municipality == "TEABO"           ~ 31075,
    municipality == "TECOH"           ~ 31076,
    municipality == "TEKAL DE VENEGAS"~ 31077,
    municipality == "TEKANTÓ"         ~ 31078,
    municipality == "TEKAX"           ~ 31079,
    municipality == "TEKIT"           ~ 31080,
    municipality == "TEKOM"           ~ 31081,
    municipality == "TELCHAC PUEBLO"  ~ 31082,
    municipality == "TELCHAC PUERTO"  ~ 31083,
    municipality == "TEMAX"           ~ 31084,
    municipality == "TEMOZÓN"         ~ 31085,
    municipality == "TEPAKÁN"         ~ 31086,
    municipality == "TETIZ"           ~ 31087,
    municipality == "TEYA"            ~ 31088,
    municipality == "TICUL"           ~ 31089,
    municipality == "TIMUCUY"         ~ 31090,
    municipality == "TINUM"           ~ 31091,
    municipality == "TIXCACALCUPUL"   ~ 31092,
    municipality == "TIXKOKOB"        ~ 31093,
    municipality == "TIXMÉHUAC"       ~ 31094,
    municipality == "TIXPÉHUAL"       ~ 31095,
    municipality == "TIZIMÍN"         ~ 31096,
    municipality == "TUNKÁS"          ~ 31097,
    municipality == "TZUCACAB"        ~ 31098,
    municipality == "UAYMA"           ~ 31099,
    municipality == "UCÚ"             ~ 31100,
    municipality == "UMÁN"            ~ 31101,
    municipality == "VALLADOLID"      ~ 31102,
    municipality == "XOCCHEL"         ~ 31103,
    municipality == "YAXCABÁ"         ~ 31104,
    municipality == "YAXKUKUL"        ~ 31105,
    municipality == "YOBAÍN"          ~ 31106,
    TRUE                              ~ 0
  ))

################################################################################
# 4) Drop if section==. (NA) 
#    Then collapse (sum) columns PAN-CI_1 total (first) comun*, by (municipality uniqueid section)
################################################################################
df_merge <- df_merge %>%
  filter(!is.na(section))

# Identify the columns to sum: from "PAN" through "CI_1" plus "total", 
# plus anything that matches "comun*" if present
# We'll do a partial approach
cols_to_sum <- c()
if ("PAN" %in% names(df_merge)) {
  start_idx <- which(names(df_merge)=="PAN")
  # we assume columns up to "CI_1" (wherever that is)
  end_idx <- which(names(df_merge)=="CI_1")
  if (length(start_idx)>0 && length(end_idx)>0 && start_idx<end_idx) {
    cols_to_sum <- names(df_merge)[start_idx:end_idx]
  }
}
# also include "total"
if ("total" %in% names(df_merge)) {
  cols_to_sum <- c(cols_to_sum, "total")
}
# also include columns that match "comun" 
comun_cols <- names(df_merge)[grepl("^comun", names(df_merge))]
cols_to_sum <- unique(c(cols_to_sum, comun_cols))

# For "collapse (sum) ..., by(municipality uniqueid section) (first) for comun*, 
# we can do a group_by + summarise. 
# The (first) comun* might be done with a 'first' approach, but your code 
# explicitly says "collapse (sum) ... (first) comun*, by(...)". 
# We'll interpret that as summing everything except we pick the "first" for comun* columns

# We'll separate the "comun*" columns for "first" approach
comun_cols_for_first <- comun_cols
sum_cols <- setdiff(cols_to_sum, comun_cols_for_first)

df_collapsed <- df_merge %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(
    across(all_of(sum_cols), sum, na.rm=TRUE),
    across(all_of(comun_cols_for_first), ~ dplyr::first(.x), .names = "{.col}"),
    .groups = "drop"
  )

################################################################################
# 5) Creating columns for coalition merges (like PRI_PANAL, PRI_PANAL_PES, etc.) 
#    Based on comun1 or comun2 columns (the user merges them).
################################################################################
# We'll interpret the next steps as:
#   if comun1=="PRI_PANAL_PES", then "PRI_PANAL_PES = PRI + PANAL + PES + CC1"
#   then set certain columns to 0 if comun1 references them.

# For each line, we create a new variable X, then we do "replace X=..."

df_collapsed <- df_collapsed %>%
  mutate(
    PRI_PANAL = if_else(comun1=="PRI_PANAL", coalesce(PRI,0) + coalesce(PANAL,0) + coalesce(CC1,0), NA_real_),
    PRI_PANAL_PES = if_else(comun1=="PRI_PANAL_PES", coalesce(PRI,0) + coalesce(PANAL,0) + coalesce(PES,0) + coalesce(CC1,0), NA_real_),
    PRI_PANAL_PH_PES = if_else(comun1=="PRI_PANAL_PH_PES", coalesce(PRI,0) + coalesce(PANAL,0) + coalesce(PES,0) + coalesce(PH,0) + coalesce(CC1,0), NA_real_),
    PRI_PES = if_else(comun1=="PRI_PES", coalesce(PRI,0) + coalesce(PES,0) + coalesce(CC1,0), NA_real_),
    PRI_PH = if_else(comun1=="PRI_PH", coalesce(PRI,0) + coalesce(PH,0) + coalesce(CC1,0), NA_real_),
    PRI_PH_PES = if_else(comun1=="PRI_PH_PES", coalesce(PRI,0) + coalesce(PES,0) + coalesce(PH,0) + coalesce(CC1,0), NA_real_),
    PRI_PVEM = if_else(comun1=="PRI_PVEM", coalesce(PRI,0) + coalesce(PVEM,0) + coalesce(CC1,0), NA_real_),
    PRI_PVEM_PANAL = if_else(comun1=="PRI_PVEM_PANAL", coalesce(PRI,0) + coalesce(PANAL,0) + coalesce(PVEM,0) + coalesce(CC1,0), NA_real_),
    PRI_PVEM_PANAL_PES = if_else(comun1=="PRI_PVEM_PANAL_PES", coalesce(PRI,0) + coalesce(PANAL,0) + coalesce(PVEM,0) + coalesce(PES,0) + coalesce(CC1,0), NA_real_),
    PRI_PVEM_PANAL_PH = if_else(comun1=="PRI_PVEM_PANAL_PH", coalesce(PRI,0) + coalesce(PANAL,0) + coalesce(PVEM,0) + coalesce(PH,0) + coalesce(CC1,0), NA_real_),
    PRI_PVEM_PANAL_PH_PES = if_else(comun1=="PRI_PVEM_PANAL_PH_PES", coalesce(PRI,0) + coalesce(PANAL,0) + coalesce(PVEM,0) + coalesce(PES,0) + coalesce(PH,0) + coalesce(CC1,0), NA_real_),
    PRI_PVEM_PES = if_else(comun1=="PRI_PVEM_PES", coalesce(PRI,0) + coalesce(PVEM,0) + coalesce(PES,0) + coalesce(CC1,0), NA_real_),
    PRI_PVEM_PH_PES= if_else(comun1=="PRI_PVEM_PH_PES", coalesce(PRI,0) + coalesce(PVEM,0) + coalesce(PES,0) + coalesce(PH,0) + coalesce(CC1,0), NA_real_)
  ) %>%
  # then we set PRI=., PVEM=., PANAL=., PH=., PES=. if strpos(comun1,"X")>0
  mutate(
    PRI = if_else(str_detect(comun1,"PRI"), NA_real_, PRI),
    PVEM= if_else(str_detect(comun1,"PVEM"), NA_real_, PVEM),
    PANAL= if_else(str_detect(comun1,"PANAL"), NA_real_, PANAL),
    PH= if_else(str_detect(comun1,"PH"), NA_real_, PH),
    PES= if_else(str_detect(comun1,"PES"), NA_real_, PES)
  )

# Then for comun2 we do similar lines for PAN_PANAL, PAN_PRD, PAN_PRD_PT, etc.
df_collapsed <- df_collapsed %>%
  mutate(
    PAN_PANAL= if_else(comun2=="PAN_PANAL", coalesce(PAN,0) + coalesce(PANAL,0) + coalesce(CC2,0), NA_real_),
    PAN_PRD=   if_else(comun2=="PAN_PRD",   coalesce(PAN,0) + coalesce(PRD,0) + coalesce(CC2,0), NA_real_),
    PAN_PRD_PT=if_else(comun2=="PAN_PRD_PT",coalesce(PAN,0) + coalesce(PRD,0) + coalesce(PT,0) + coalesce(CC2,0), NA_real_),
    PRD_PT=    if_else(comun2=="PRD_PT",    coalesce(PRD,0) + coalesce(PT,0) + coalesce(CC2,0), NA_real_),
    PRD_PT_PANAL= if_else(comun2=="PRD_PT_PANAL", coalesce(PAN,0)+coalesce(PRD,0)+coalesce(PT,0)+coalesce(PANAL,0)+coalesce(CC2,0), NA_real_)
  ) %>%
  # then we set columns to NA if strpos(comun2,"X")>0
  mutate(
    PANAL= if_else(str_detect(comun2,"PANAL"), NA_real_, PANAL),
    PRD  = if_else(str_detect(comun2,"PRD"),   NA_real_, PRD),
    PT   = if_else(str_detect(comun2,"PT"),    NA_real_, PT),
    PAN  = if_else(comun2 %in% c("PAN_PANAL", "PAN_PRD","PAN_PRD_PT"), NA_real_, PAN)
  )

################################################################################
# 6) drop comun* CC* columns
################################################################################
df_collapsed <- df_collapsed %>%
  select(-matches("^comun"), -matches("^CC"))

################################################################################
# 7) order PRI_PANAL-PRD_PT_PANAL, b(CI_1) [We can do partial ordering in R if needed]
#    We'll just relocate them near CI_1 for demonstration
################################################################################
to_relocate <- c("PRI_PANAL","PRI_PANAL_PES","PRI_PANAL_PH_PES","PRI_PES","PRI_PH","PRI_PH_PES",
                 "PRI_PVEM","PRI_PVEM_PANAL","PRI_PVEM_PANAL_PES","PRI_PVEM_PANAL_PH",
                 "PRI_PVEM_PANAL_PH_PES","PRI_PVEM_PES","PRI_PVEM_PH_PES","PAN_PANAL",
                 "PAN_PRD","PAN_PRD_PT","PRD_PT","PRD_PT_PANAL")

if ("CI_1" %in% names(df_collapsed)) {
  df_collapsed <- df_collapsed %>%
    relocate(any_of(to_relocate), .before = CI_1)
}

################################################################################
# 8) Compute valid = rowtotal(...) with the final columns, 
#    then year=2015, month="June", STATE="YUCATAN"
################################################################################
# We'll guess the final set of columns is something like:
final_cols <- c("PAN","PRD","PVEM","PT","MC","PANAL","MORENA","PH","PES",
                "PRI_PANAL","PRI_PANAL_PES","PRI_PANAL_PH_PES","PRI_PES","PRI_PH","PRI_PH_PES",
                "PRI_PVEM","PRI_PVEM_PANAL","PRI_PVEM_PANAL_PES","PRI_PVEM_PANAL_PH",
                "PRI_PVEM_PANAL_PH_PES","PRI_PVEM_PES","PRI_PVEM_PH_PES","PAN_PANAL","PAN_PRD",
                "PAN_PRD_PT","PRD_PT","PRD_PT_PANAL","CI_1")

df_final <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(any_of(final_cols)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2015,
    month = "June",
    STATE = "YUCATAN"
  )

################################################################################
# 9) Merge with LN2015.dta for listanominal using LN2015, then keep uniqueid==31xxx, 
#    keep if seccion!=0, rename seccion->section, merge 1:1, rename lista->listanominal
################################################################################
# We'll replicate your preserve/restore approach in a simpler way:
df_ln_full <- read_dta("../Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
  filter(entidad==31, month==6, seccion!=0) %>%
  mutate(uniqueid = entidad*1000 + municipio) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section=seccion)

# Merge 1:1 on (uniqueid, section)
df_merged2 <- df_final %>%
  left_join(df_ln_full, by=c("uniqueid","section"))

df_merged2 <- df_merged2 %>%
  filter(!is.na(lista))  # drop if _merge==2

# rename lista->listanominal
df_2015 <- df_merged2 %>%
  rename(listanominal = lista)

################################################################################
# 1) Read "Ayuntamientos_Yuc_2018.xlsx" (sheet "RESULTADOS"), 
#    rename municipality codes, then collapse
################################################################################
df_main <- read_excel(
  path = "../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/Ayuntamientos_Yuc_2018.xlsx",
  sheet = "RESULTADOS",
  col_names = TRUE
) %>%
  as.data.frame()

# Drop any existing 'uniqueid', create uniqueid=0
df_main <- df_main %>%
  select(-any_of("uniqueid")) %>%   # if uniqueid already exists
  mutate(uniqueid = 0)

# Assign municipality codes
df_main <- df_main %>%
  mutate(uniqueid = case_when(
    municipality == "ABALA"           ~ 31001,
    municipality == "ACANCEH"         ~ 31002,
    municipality == "AKIL"            ~ 31003,
    municipality == "BACA"            ~ 31004,
    municipality == "BOKOBA"          ~ 31005,
    municipality == "BUCTZOTZ"        ~ 31006,
    municipality == "CACALCHEN"       ~ 31007,
    municipality == "CALOTMUL"        ~ 31008,
    municipality == "CANSAHCAB"       ~ 31009,
    municipality == "CANTAMAYEC"      ~ 31010,
    municipality == "CELESTUN"        ~ 31011,
    municipality == "CENOTILLO"       ~ 31012,
    municipality == "CHACSINKIN"      ~ 31016,
    municipality == "CHANKOM"         ~ 31017,
    municipality == "CHAPAB"          ~ 31018,
    municipality == "CHEMAX"          ~ 31019,
    municipality == "CHICHIMILA"      ~ 31021,
    municipality == "CHICXULUB PUEBLO"~ 31020,
    municipality == "CHIKINDZONOT"    ~ 31022,
    municipality == "CHOCHOLA"        ~ 31023,
    municipality == "CHUMAYEL"        ~ 31024,
    municipality == "CONKAL"          ~ 31013,
    municipality == "CUNCUNUL"        ~ 31014,
    municipality == "CUZAMA"          ~ 31015,
    municipality == "DZAN"            ~ 31025,
    municipality == "DZEMUL"          ~ 31026,
    municipality == "DZIDZANTUN"      ~ 31027,
    municipality == "DZILAM DE BRAVO" ~ 31028,
    municipality == "DZILAM GONZALEZ" ~ 31029,
    municipality == "DZITAS"          ~ 31030,
    municipality == "DZONCAUICH"      ~ 31031,
    municipality == "ESPITA"          ~ 31032,
    municipality == "HALACHO"         ~ 31033,
    municipality == "HOCABA"          ~ 31034,
    municipality == "HOCTUN"          ~ 31035,
    municipality == "HOMUN"           ~ 31036,
    municipality == "HUHI"            ~ 31037,
    municipality == "HUNUCMA"         ~ 31038,
    municipality == "IXIL"            ~ 31039,
    municipality == "IZAMAL"          ~ 31040,
    municipality == "KANASIN"         ~ 31041,
    municipality == "KANTUNIL"        ~ 31042,
    municipality == "KAUA"            ~ 31043,
    municipality == "KINCHIL"         ~ 31044,
    municipality == "KOPOMA"          ~ 31045,
    municipality == "MAMA"            ~ 31046,
    municipality == "MANI"            ~ 31047,
    municipality == "MAXCANU"         ~ 31048,
    municipality == "MAYAPAN"         ~ 31049,
    municipality == "MERIDA"          ~ 31050,
    municipality == "MOCOCHA"         ~ 31051,
    municipality == "MOTUL"           ~ 31052,
    municipality == "MUNA"            ~ 31053,
    municipality == "MUXUPIP"         ~ 31054,
    municipality == "OPICHEN"         ~ 31055,
    municipality == "OXKUTZCAB"       ~ 31056,
    municipality == "PANABA"          ~ 31057,
    municipality == "PETO"            ~ 31058,
    municipality == "PROGRESO"        ~ 31059,
    municipality == "QUINTANA ROO"    ~ 31060,
    municipality == "RIO LAGARTOS"    ~ 31061,
    municipality == "SACALUM"         ~ 31062,
    municipality == "SAMAHIL"         ~ 31063,
    municipality == "SAN FELIPE"      ~ 31065,
    municipality == "SANAHCAT"        ~ 31064,
    municipality == "SANTA ELENA"     ~ 31066,
    municipality == "SEYE"            ~ 31067,
    municipality == "SINANCHE"        ~ 31068,
    municipality == "SOTUTA"          ~ 31069,
    municipality == "SUCILA"          ~ 31070,
    municipality == "SUDZAL"          ~ 31071,
    municipality == "SUMA"            ~ 31072,
    municipality == "TAHDZIU"         ~ 31073,
    municipality == "TAHMEK"          ~ 31074,
    municipality == "TEABO"           ~ 31075,
    municipality == "TECOH"           ~ 31076,
    municipality == "TEKAL DE VENEGAS"~ 31077,
    municipality == "TEKANTO"         ~ 31078,
    municipality == "TEKAX"           ~ 31079,
    municipality == "TEKIT"           ~ 31080,
    municipality == "TEKOM"           ~ 31081,
    municipality == "TELCHAC PUEBLO"  ~ 31082,
    municipality == "TELCHAC PUERTO"  ~ 31083,
    municipality == "TEMAX"           ~ 31084,
    municipality == "TEMOZON"         ~ 31085,
    municipality == "TEPAKAN"         ~ 31086,
    municipality == "TETIZ"           ~ 31087,
    municipality == "TEYA"            ~ 31088,
    municipality == "TICUL"           ~ 31089,
    municipality == "TIMUCUY"         ~ 31090,
    municipality == "TINUM"           ~ 31091,
    municipality == "TIXCACALCUPUL"   ~ 31092,
    municipality == "TIXKOKOB"        ~ 31093,
    municipality == "TIXMEHUAC"       ~ 31094,
    municipality == "TIXPEUAL"        ~ 31095,
    municipality == "TIZIMIN"         ~ 31096,
    municipality == "TUNKAS"          ~ 31097,
    municipality == "TZUCACAB"        ~ 31098,
    municipality == "UAYMA"           ~ 31099,
    municipality == "UCU"             ~ 31100,
    municipality == "UMAN"            ~ 31101,
    municipality == "VALLADOLID"      ~ 31102,
    municipality == "XOCCHEL"         ~ 31103,
    municipality == "YAXCABA"         ~ 31104,
    municipality == "YAXKUKUL"        ~ 31105,
    municipality == "YOBAIN"          ~ 31106,
    TRUE                               ~ 0
  ))

# Then we do "collapse (sum) PAN-CI_1 listanominal total, by (municipality uniqueid section)"
# i.e. group by municipality, uniqueid, section, sum the numeric columns from PAN to CI_1, plus listanominal, total
df_main <- df_main %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(
    across(c(PAN:CI_1, listanominal, total), sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 2) Read "computosRegidores2018-130918.xlsx", range(A3:AE109), keep columns, 
#    transform them into 0/1, rename, etc. Then we'll merge on municipality
################################################################################
df_regidores <- read_excel(
  path = "../../../Data/Raw Electoral Data/Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012,2015,2018/computosRegidores2018-130918.xlsx",
  sheet = "REGIDURIAS",
  range = "A3:AE109",
  col_names = TRUE
) %>%
  as.data.frame()

# keep MUNICIPIO and columns from PANPRDMOV to MORENAPES
keep_cols <- c("MUNICIPIO", "PANPRDMOV","PANMOV","PRDMC","PRIPVEM","PRIPVEMPANAL","PRIPANAL","PVEMPANAL",
               "PTMORENA","PTMORENAPES","PTPES","MORENAPES")  # adapt if needed
df_regidores <- df_regidores %>%
  select(any_of(keep_cols))

# For each of PANPRDMOV-MORENAPES => if it != "NA" => "1", else "0"
# Then destring to numeric
for (col_ in setdiff(names(df_regidores), "MUNICIPIO")) {
  df_regidores[[col_]] <- ifelse(df_regidores[[col_]] != "NA", "1","0")
}
df_regidores <- df_regidores %>%
  mutate(across(-MUNICIPIO, as.numeric))

# rename columns: rename * => "coal_*" except for MUNICIPIO -> municipality
names(df_regidores)[ names(df_regidores) != "MUNICIPIO" ] <-
  paste0("coal_", names(df_regidores)[ names(df_regidores) != "MUNICIPIO" ])

df_regidores <- df_regidores %>%
  rename(municipality = MUNICIPIO)

# Next steps: 
# "replace municipality=subinstr(...) for diacritical marks"
df_regidores <- df_regidores %>%
  mutate(
    municipality = str_replace_all(municipality, "Á", "A"),
    municipality = str_replace_all(municipality, "É", "E"),
    municipality = str_replace_all(municipality, "Í", "I"),
    municipality = str_replace_all(municipality, "Ó", "O"),
    municipality = str_replace_all(municipality, "Ú", "U")
  )

# "replace coal_PANPRD=0 if coal_PANPRDMOV==1", etc.
df_regidores <- df_regidores %>%
  mutate(
    coal_PANPRD= if_else(coal_PANPRDMOV==1, 0, coal_PANPRD),
    coal_PANMOV= if_else(coal_PANPRDMOV==1, 0, coal_PANMOV),
    coal_PRDMC= if_else(coal_PANPRDMOV==1, 0, coal_PRDMC),
    coal_PANPRDMOV= dplyr::rename_with(., ~"coal_PANPRDMC") # we interpret carefully, see below
  )

# Actually let's interpret each line carefully:
# The code: 
#   replace coal_PANPRD=0 if coal_PANPRDMOV==1
#   replace coal_PANMOV=0 if coal_PANPRDMOV==1
#   replace coal_PRDMC=0 if coal_PANPRDMOV==1
# rename coal_PANPRDMOV coal_PANPRDMC
# We'll do it step by step:
df_regidores <- df_regidores %>%
  mutate(
    coal_PANPRD = if_else(coal_PANPRDMOV==1, 0, coal_PANPRD),
    coal_PANMOV = if_else(coal_PANPRDMOV==1, 0, coal_PANMOV),
    coal_PRDMC  = if_else(coal_PANPRDMOV==1, 0, coal_PRDMC)
  )
# rename coal_PANPRDMOV => coal_PANPRDMC
names(df_regidores)[names(df_regidores)=="coal_PANPRDMOV"] <- "coal_PANPRDMC"

# Then "replace coal_PRIPVEM=0 if coal_PRIPVEMPANAL==1", etc.
df_regidores <- df_regidores %>%
  mutate(
    coal_PRIPVEM= if_else(coal_PRIPVEMPANAL==1, 0, coal_PRIPVEM),
    coal_PRIPANAL= if_else(coal_PRIPVEMPANAL==1, 0, coal_PRIPANAL),
    coal_PVEMPANAL= if_else(coal_PRIPVEMPANAL==1, 0, coal_PVEMPANAL),
    coal_PTMORENA= if_else(coal_PTMORENAPES==1, 0, coal_PTMORENA),
    coal_PTPES= if_else(coal_PTMORENAPES==1, 0, coal_PTPES)
  ) %>%
  # drop coal_MORENAPES
  select(-any_of("coal_MORENAPES"))

################################################################################
# 3) Merge m:1 municipality using coalindicators.dta => on df_main
################################################################################
df_merged <- df_main %>%
  mutate(
    municipality = if_else(municipality=="SUMA", "SUMA DE HIDALGO", municipality)
  ) %>%
  left_join(df_regidores, by="municipality")

################################################################################
# 4) rename *NVA_ALIANZA => *PANAL 
#    (assuming in df_merged, we do: rename xNVA_ALIANZA => xPANAL)
################################################################################
# We'll search for columns that contain 'NVA_ALIANZA' in their name
nva_cols <- grep("NVA_ALIANZA", names(df_merged), value = TRUE)
for (c_ in nva_cols) {
  new_name <- sub("NVA_ALIANZA", "PANAL", c_)
  names(df_merged)[names(df_merged)==c_] <- new_name
}

################################################################################
# 5) Replace PAN_PRD_MC = PAN_PRD_MC + ... if coal_PANPRDMC==1, else =., 
#    zero out columns if coalition used
################################################################################
df_merged <- df_merged %>%
  mutate(
    PAN_PRD_MC = if_else(
      coal_PANPRDMC==1,
      coalesce(PAN_PRD_MC,0) + coalesce(PAN_PRD,0) + coalesce(PAN_MC,0) + coalesce(PRD_MC,0) + coalesce(PAN,0) + coalesce(PRD,0) + coalesce(MC,0),
      NA_real_
    ),
    PAN= if_else(coal_PANPRDMC==1, NA_real_, PAN),
    PRD= if_else(coal_PANPRDMC==1, NA_real_, PRD),
    MC = if_else(coal_PANPRDMC==1, NA_real_, MC),
    PAN_PRD= if_else(coal_PANPRDMC==1, NA_real_, PAN_PRD),
    PAN_MC = if_else(coal_PANPRDMC==1, NA_real_, PAN_MC)
  ) %>%
  # drop PRD_MC
  select(-any_of("PRD_MC"))

df_merged <- df_merged %>%
  mutate(
    # replace PAN_PRD= PAN_PRD + PAN + PRD if coal_PANPRD==1 else =.
    PAN_PRD = if_else(coal_PANPRD==1, coalesce(PAN_PRD,0)+ coalesce(PAN,0)+ coalesce(PRD,0), NA_real_),
    PAN= if_else(coal_PANPRD==1, NA_real_, PAN),
    PRD= if_else(coal_PANPRD==1, NA_real_, PRD),
    
    PAN_MC= if_else(coal_PANMC==1, coalesce(PAN_MC,0)+coalesce(PAN,0)+coalesce(MC,0), NA_real_),
    PAN= if_else(coal_PANMC==1, NA_real_, PAN),
    MC= if_else(coal_PANMC==1, NA_real_, MC),
    
    PAN_PANAL= if_else(coal_PANPANAL==1, coalesce(PAN_PANAL,0)+coalesce(PAN,0)+coalesce(PANAL,0), NA_real_),
    PAN= if_else(coal_PANPANAL==1, NA_real_, PAN),
    PANAL= if_else(coal_PANPANAL==1, NA_real_, PANAL),
    
    PRI_PVEM_PANAL= if_else(coal_PRIPVEMPANAL==1, coalesce(PRI_PVEM_PANAL,0)+coalesce(PRI_PVEM,0)+coalesce(PRI_PANAL,0)+coalesce(PVEM_PANAL,0)+coalesce(PRI,0)+coalesce(PVEM,0)+coalesce(PANAL,0), NA_real_),
    PRI_PVEM= if_else(coal_PRIPVEMPANAL==1, NA_real_, PRI_PVEM),
    PRI_PANAL= if_else(coal_PRIPVEMPANAL==1, NA_real_, PRI_PANAL),
    PVEM_PANAL= if_else(coal_PRIPVEMPANAL==1, NA_real_, PVEM_PANAL),
    PRI= if_else(coal_PRIPVEMPANAL==1, NA_real_, PRI),
    PVEM= if_else(coal_PRIPVEMPANAL==1, NA_real_, PVEM),
    PANAL= if_else(coal_PRIPVEMPANAL==1, NA_real_, PANAL),
    
    PRI_PANAL= if_else(coal_PRIPANAL==1, coalesce(PRI_PANAL,0)+coalesce(PRI,0)+coalesce(PANAL,0), NA_real_),
    PRI= if_else(coal_PRIPANAL==1, NA_real_, PRI),
    PANAL= if_else(coal_PRIPANAL==1, NA_real_, PANAL),
    
    PVEM_PANAL= if_else(coal_PVEMPANAL==1, coalesce(PVEM_PANAL,0)+coalesce(PVEM,0)+coalesce(PANAL,0), NA_real_),
    PVEM= if_else(coal_PVEMPANAL==1, NA_real_, PVEM),
    PANAL= if_else(coal_PVEMPANAL==1, NA_real_, PANAL),
    
    PRI_PVEM= if_else(coal_PRIPVEM==1, coalesce(PRI_PVEM,0)+coalesce(PRI,0)+coalesce(PVEM,0), NA_real_),
    PRI= if_else(coal_PRIPVEM==1, NA_real_, PRI),
    PVEM= if_else(coal_PRIPVEM==1, NA_real_, PVEM),
    
    PT_MORENA_PES= if_else(coal_PTMORENAPES==1, coalesce(PT_MORENA_PES,0)+coalesce(PT_MORENA,0)+coalesce(PT_PES,0)+coalesce(MORENA_PES,0)+coalesce(PT,0)+coalesce(MORENA,0)+coalesce(PES,0), NA_real_),
    PT= if_else(coal_PTMORENAPES==1, NA_real_, PT),
    MORENA= if_else(coal_PTMORENAPES==1, NA_real_, MORENA),
    PES= if_else(coal_PTMORENAPES==1, NA_real_, PES)
  ) %>%
  select(-any_of("MORENA_PES")) %>%  # drop MORENA_PES
  
  # replace PT_MORENA= ... if coal_PTMORENA==1 etc.
  df_merged <- df_merged %>%
  mutate(
    PT_MORENA= if_else(coal_PTMORENA==1, coalesce(PT_MORENA,0)+coalesce(PT,0)+coalesce(MORENA,0), PT_MORENA),
    PT= if_else(coal_PVEMPANAL==1, NA_real_, PT),  # code is suspicious, see user's script
    MORENA= if_else(coal_PVEMPANAL==1, NA_real_, MORENA)
  )

df_merged <- df_merged %>%
  mutate(
    PT_PES= if_else(coal_PTPES==1, coalesce(PT_PES,0)+coalesce(PT,0)+coalesce(PES,0), PT_PES),
    PT= if_else(coal_PTPES==1, NA_real_, PT),
    PES= if_else(coal_PTPES==1, NA_real_, PES)
  )

# drop coal_*
df_merged <- df_merged %>%
  select(!starts_with("coal_"))

################################################################################
# 6) "order PRI_PANAL-PRD_PT_PANAL, b(CI_1)" => we'll do partial relocate if needed
################################################################################
# Just skip or do partial reorder.

################################################################################
# 7) valid= rowtotal(...), turnout= total/listanominal, year=2018, month="July", 
#    STATE="YUCATAN", then save
################################################################################
# We'll guess the final set of columns that you want to sum:
final_cols <- c("PAN","PRI","PRD","PVEM","PT","MC","PANAL","MORENA","PH","PES",
                "PAN_PRD_MC","PAN_PRD","PAN_MC","PAN_PANAL","PRI_PVEM_PANAL",
                "PRI_PVEM","PRI_PANAL","PVEM_PANAL","PT_MORENA_PES","PT_MORENA","PT_PES","CI_1")

df_2018 <- df_merged %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(any_of(final_cols)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    turnout = total/listanominal,
    year    = 2018,
    month   = "July",
    STATE   = "YUCATAN"
  )

#VALIDATION PARTY

df_2018 <- df_2018 %>% 
  dplyr::mutate(PT = ifelse(!is.na(PT_MORENA) & PT_MORENA>0 & PT >0, 0, PT),
                MORENA = ifelse(!is.na(PT_MORENA) & PT_MORENA>0 & MORENA >0, 0, MORENA))

# Combine the dataframes, handling different columns by filling with NA
yucatan_all <- bind_rows(df_1995,
                         df_1998,
                         df_2001,
                         df_2004,
                         df_2007,
                         df_2010,
                         df_2012,
                         df_2015,
                         df_2018)


data.table::fwrite(yucatan_all,"../../../Processed Data/yucatan/yucatan_process_raw_data.csv")

