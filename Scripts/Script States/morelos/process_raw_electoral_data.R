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
### Ayu_Seccion_1997_No_LN.csv
###############################################################################


# 1) Read CSV: "Ayu_Seccion_1997_No_LN.csv"
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_1997_No_LN.csv", 
               show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))

# 2) rename municipio->municipality, seccion->section
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion
  )

# 3) drop if municipality=="" & section==.
df <- df %>%
  filter(!(municipality=="" & is.na(section)))

# 4) egen total = rowtotal(pan pri prd pc pt pvem pcm pps pdm noregistrados nulos)
#    => we replicate with rowSums(...) ignoring NA
df <- df %>%
  rowwise() %>%
  mutate(total = sum(
    c_across(c("pan","pri","prd","pc","pt","pvem","pcm","pps","pdm","no registrados","nulos")),
    na.rm=TRUE)
  ) %>%
  ungroup()

# 5) drop if total==. | total==0
df <- df %>%
  filter(!is.na(total) & total != 0)

# 6) destring pan-nulos
to_destring <- c("pan","pri","prd","pc","pt","pvem","pcm","pps","pdm","no registrados","nulos")

for(cn in to_destring) {
  if(cn %in% names(df)) {
    df[[cn]] <- suppressWarnings(as.numeric(df[[cn]]))
  }
}

# 7) collapse (sum) pan-nulos total, by(municipality, section)
#    We do group_by(...) + summarize(...)
collapse_vars <- c("pan","pri","prd","pc","pt","pvem","pcm","pps","pdm","nulos","total")
collapse_vars <- intersect(collapse_vars, names(df))

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(collapse_vars), sum, na.rm=TRUE), .groups="drop")

# 8) rename pan->PAN, pri->PRI, prd->PRD, pc->PartCardenista, ...
#    drop noreg nulos
df_collapsed <- df_collapsed %>%
  rename(
    PAN            = pan,
    PRI            = pri,
    PRD            = prd,
    PartCardenista = pc,
    PT             = pt,
    PVEM           = pvem,
    PCM            = pcm,
    PPS            = pps,
    PDM            = pdm
  ) %>%
  select(-nulos)  # drop noreg, nulos

# 9) gen uniqueid=0
#    replace uniqueid=17001 if municipality=="AMACUZAC", etc.
#    We replicate with case_when
df_collapsed <- df_collapsed %>%
  mutate(uniqueid = 0)

df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality=="AMACUZAC" ~ 17001,
      municipality=="ATLATLAHUCAN" ~ 17002,
      municipality=="Axochiapan" ~ 17003,
      municipality=="AYALA" ~ 17004,
      municipality=="COATLAN DEL RIO" ~ 17005,
      municipality=="CUAUTLA" ~ 17006,
      municipality=="CUERNAVACA" ~ 17007,
      municipality=="EMILIANO ZAPATA" ~ 17008,
      municipality=="HUITZILAC" ~ 17009,
      municipality=="Jantetelco" ~ 17010,
      municipality=="JIUTEPEC" ~ 17011,
      municipality=="JOJUTLA" ~ 17012,
      municipality=="JONACATEPEC" ~ 17013,
      municipality=="MAZATEPEC" ~ 17014,
      municipality=="MIACATLAN" ~ 17015,
      municipality=="OCUITUCO" ~ 17016,
      municipality=="PUENTE DE IXTLA" ~ 17017,
      municipality=="TEMIXCO" ~ 17018,
      municipality=="TEMOAC" ~ 17033,
      municipality=="TEPALCINGO" ~ 17019,
      municipality=="TEPOZTLAN" ~ 17020,
      municipality=="TETECALA" ~ 17021,
      municipality=="TETELA DEL VOLCAN" ~ 17022,
      municipality=="TLALNEPANTLA" ~ 17023,
      municipality=="TLALTIZAPAN" ~ 17024,
      municipality=="TLAQUILTENANGO" ~ 17025,
      municipality=="TLAYACAPAN" ~ 17026,
      municipality=="TOTOLAPAN" ~ 17027,
      municipality=="XOCHITEPEC" ~ 17028,
      municipality=="YAUTEPEC" ~ 17029,
      municipality=="YECAPIXTLA" ~ 17030,
      municipality=="ZACATEPEC" ~ 17031,
      municipality=="ZACUALPAN DE AMILPAS" ~ 17032,
      TRUE ~ uniqueid
    )
  )

# 10) egen valid = rowtotal(PAN PRI PRD PartCardenista PT PVEM PCM PPS PDM)
#     replicate rowSums ignoring NA
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c("PAN","PRI","PRD","PartCardenista","PT","PVEM","PCM","PPS","PDM")), na.rm=TRUE)) %>%
  ungroup()

all_months <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") 

all_months <- all_months %>%
  filter(state == "MORELOS" &
         month== "January" & 
           year==1997 )

df_collapsed <- df_collapsed %>%
  left_join(all_months, by=c("section"))

# drop if _merge==2 => in R, we do not have _merge. But if some rows didn't match, they'd have NA. We'll just drop if 'lista' is NA:
df_collapsed <- df_collapsed %>%
  filter(!is.na(lista))

# rename lista -> listanominal
df_collapsed <- df_collapsed %>%
  rename(listanominal = lista)

# 13) gen turnout= total/listanominal
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# 17) gen year=1997, gen month="March"
df_1997 <- df_collapsed %>%
  mutate(
    year = 1997,
    month = "March"
  )

rm(df_collapsed)
rm(df)

###############################################################################
### Ayu_Seccion_2000.csv in R
###############################################################################

# 1) Read CSV
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2000.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
# 2) rename municipio->municipality, seccion->section
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion,
    listanominal="lista nominal"
  )

# 3) drop if municipality=="" & section==.
df <- df %>%
  filter(!(municipality=="" & is.na(section)))

# 4) egen total = rowtotal(pan pri apm pt pvem pcm parm pds pas noreg nulos)
#    => rowwise sum ignoring NA
df <- df %>%
  rowwise() %>%
  mutate(total = sum(c_across(c(
    "pan","pri","apm","pt","pvem","pcm","parm","pds","pas","no registrados","nulos"
  )), na.rm=TRUE)) %>%
  ungroup()

# 5) drop if total==. | total==0 => filter if total is NA or total==0
df <- df %>%
  filter(!is.na(total) & total != 0)

# 6) destring listanominal pan-nulos total => convert to numeric
to_destring <- c("listanominal","pan","pri","apm","pt","pvem","pcm","parm","pds",
                 "pas","no registrados","nulos","total")
for(cn in to_destring) {
  if(cn %in% names(df)) {
    df[[cn]] <- suppressWarnings(as.numeric(df[[cn]]))
  }
}

# 7) g missing = listanominal==.
#    In R, define missing=1 if listanominal is NA, else 0
df <- df %>%
  mutate(missing = if_else(is.na(listanominal), 1, 0))

# 8) collapse (sum) missing listanominal pan- nulos total, by(municipality, section)
#    => group_by, then summarize
collapse_cols <- c("missing","listanominal","pan","pri","apm","pt","pvem","pcm","parm","pds",
                   "pas","noreg","nulos","total")
collapse_cols <- intersect(collapse_cols, names(df))

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(collapse_cols), sum, na.rm=TRUE), .groups="drop")

# 9)

if("lista" %in% names(df_collapsed)) {
  df_collapsed <- df_collapsed %>%
    mutate(listanominal = if_else(missing >= 1, lista, listanominal)) %>%
    select(-lista, -missing)  # drop 'missing' and 'lista'
} else {
  # If there's no 'lista' column, skip this step
  df_collapsed <- df_collapsed %>% select(-missing)
}

# 10) rename pan->PAN, pri->PRI, apm->PRD_PC_PCD_PSN, etc.
df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD_PC_PCD_PSN = apm,  # from rename apm  PRD_PC_PCD_PSN
    PT = pt,
    PVEM = pvem,
    PCM = pcm,
    PARM = parm,
    PDS = pds,
    PAS = pas
  )

# gen turnout= total/listanominal
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# drop noreg nulos
df_collapsed <- df_collapsed %>%
  select(-nulos)

# 11) gen uniqueid=0, replace if ...
df_collapsed <- df_collapsed %>%
  mutate(uniqueid=0)

df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality=="AMACUZAC" ~ 17001,
      municipality=="ATLATLAHUCAN" ~ 17002,
      municipality=="AXOCHIAPAN" ~ 17003,
      municipality=="AYALA" ~ 17004,
      municipality=="COATLAN DEL RIO" ~ 17005,
      municipality=="CUAUTLA" ~ 17006,
      municipality=="CUERNAVACA" ~ 17007,
      municipality=="EMILIANO ZAPATA" ~ 17008,
      municipality=="HUITZILAC" ~ 17009,
      municipality=="JANTETELCO" ~ 17010,
      municipality=="JIUTEPEC" ~ 17011,
      municipality=="JOJUTLA" ~ 17012,
      municipality=="JONACATEPEC" ~ 17013,
      municipality=="MAZATEPEC" ~ 17014,
      municipality=="MIACATLAN" ~ 17015,
      municipality=="OCUITUCO" ~ 17016,
      municipality=="PUENTE DE IXTLA" ~ 17017,
      municipality=="TEMIXCO" ~ 17018,
      municipality=="TEMOAC" ~ 17033,
      municipality=="TEPALCINGO" ~ 17019,
      municipality=="TEPOZTLAN" ~ 17020,
      municipality=="TETECALA" ~ 17021,
      municipality=="TETELA DEL VOLCAN" ~ 17022,
      municipality=="TLALNEPANTLA" ~ 17023,
      municipality=="TLALTIZAPAN" ~ 17024,
      municipality=="TLAQUILTENANGO" ~ 17025,
      municipality=="TLAYACAPAN" ~ 17026,
      municipality=="TOTOLAPAN" ~ 17027,
      municipality=="XOCHITEPEC" ~ 17028,
      municipality=="YAUTEPEC" ~ 17029,
      municipality=="YECAPIXTLA" ~ 17030,
      municipality=="ZACATEPEC" ~ 17031,
      municipality=="ZACUALPAN" ~ 17032,
      TRUE ~ uniqueid
    )
  )

# 12) egen valid = rowtotal(PAN PRI PRD_PC_PCD_PSN PT PVEM PCM PARM PDS PAS)
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c("PAN","PRI","PRD_PC_PCD_PSN","PT","PVEM","PCM","PARM","PDS","PAS")),
                na.rm=TRUE)
  ) %>%
  ungroup()

# gen year=2000, month="July"
df_2000 <- df_collapsed %>%
  mutate(
    year  = 2000,
    month = "July"
  )

rm(df_collapsed)
rm(df)
###############################################################################
### "Extraordinario 2001.xlsx"
###############################################################################

# 1) Read Excel: 'Extraordinario 2001.xlsx', sheet="Sheet1", first row as headers
df <- read_excel("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Extraordinario 2001.xlsx", 
                 sheet = "Sheet1", col_names = TRUE)

# 2) rename SECCIÓN->section, SUMA->total, LISTA->listanominal
#    In R:
df <- df %>%
  rename(
    section     = "SECCIÓN",
    total       = SUMA,
    listanominal= LISTA
  )

# 3) collapse (sum) listanominal PAN-PVEM total, by(municipality, section)
#    i.e. group_by(municipality, section), then sum relevant columns
collapse_cols <- c("listanominal","PAN","PRI","PRD","UDO","PVEM","total")
# We'll keep only columns that actually exist in df
collapse_cols <- intersect(collapse_cols, names(df))

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(collapse_cols), sum, na.rm=TRUE), .groups="drop")

# 4) g uniqueid=17016
df_collapsed <- df_collapsed %>%
  mutate(uniqueid = 17016)

# 5) g turnout = total/listanominal
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# 6) egen valid = rowtotal(PAN PRI PRD UDO PVEM)
#    => rowwise sum ignoring NA
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c("PAN","PRI","PRD","UDO","PVEM")), na.rm=TRUE)) %>%
  ungroup()

# 11) gen year=2001, gen month="January"
df_2001 <- df_collapsed %>%
  mutate(
    year  = 2001,
    month = "January"
  )
rm(df_collapsed)
rm(df)

###############################################################################
### Ayu_Seccion_2003_No_LN.csv
###############################################################################

# 1) Read CSV
df <- fread("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2003_No_LN.csv", 
               encoding = "Latin-1")
colnames(df) <- tolower(colnames(df))
# 2) rename municipio->municipality, seccin->section
df <- df %>%
  rename(
    municipality = municipio,
    section      = sección
  )

# 3) drop if municipality=="" & section==.
df <- df %>%
  filter(!(municipality == "" & is.na(section)))

# 4) drop if total==. | total==0 
df <- df %>%
  filter(!(is.na(total) | total==0))

# 5) destring pan - total => in R, forcibly convert these columns to numeric
to_destring <- c("pan","pri","prd","udemor","pvem","c","psn","pas","mp","plm","fc","total")
for(cn in to_destring) {
  if(cn %in% names(df)) {
    df[[cn]] <- suppressWarnings(as.numeric(df[[cn]]))
  }
}

# 6) collapse (sum) pan-fc total, by(municipality, section)
#    i.e. group_by(...) and sum
collapse_vars <- c("pan","pri","prd","udemor","pvem","c","psn","pas","mp","plm","fc","total")
collapse_vars <- intersect(collapse_vars, names(df))

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(collapse_vars), sum, na.rm=TRUE), .groups="drop")

# 7) rename pan->PAN, pri->PRI, prd->PRD, udemor->UDM, pvem->PVEM, c->PC, psn->PSN, pas->PAS, 
#    mp->PMP, plm->PLM, fc->PFC
df_collapsed <- df_collapsed %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    UDM = udemor,
    PVEM= pvem,
    PC  = c,
    PSN = psn,
    PAS = pas,
    PMP = mp,
    PLM = plm,
    PFC = fc
  )

# 8) gen uniqueid=0; then replace uniqueid=xxx if municipality=="..."
df_collapsed <- df_collapsed %>%
  mutate(uniqueid=0)

df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality=="AMACUZAC" ~ 17001,
      municipality=="ATLATLAHUCAN" ~ 17002,
      municipality=="AXOCHIAPAN" ~ 17003,
      municipality=="AYALA" ~ 17004,
      municipality=="COATLAN DEL RIO" ~ 17005,
      municipality=="CUAUTLA" ~ 17006,
      municipality=="CUERNAVACA" ~ 17007,
      municipality=="EMILIANO ZAPATA" ~ 17008,
      municipality=="HUITZILAC" ~ 17009,
      municipality=="JANTETELCO" ~ 17010,
      municipality=="JIUTEPEC" ~ 17011,
      municipality=="JOJUTLA" ~ 17012,
      municipality=="JONACATEPEC" ~ 17013,
      municipality=="MAZATEPEC" ~ 17014,
      municipality=="MIACATLAN" ~ 17015,
      municipality=="OCUITUCO" ~ 17016,
      municipality=="PUENTE DE IXTLA" ~ 17017,
      municipality=="TEMIXCO" ~ 17018,
      municipality=="TEMOAC" ~ 17033,
      municipality=="TEPALCINGO" ~ 17019,
      municipality=="TEPOZTLAN" ~ 17020,
      municipality=="TETECALA" ~ 17021,
      municipality=="TETELA DEL VOLCAN" ~ 17022,
      municipality=="TLALNEPANTLA" ~ 17023,
      municipality=="TLALTIZAPAN" ~ 17024,
      municipality=="TLAQUILTENANGO" ~ 17025,
      municipality=="TLAYACAPAN" ~ 17026,
      municipality=="TOTOLAPAN" ~ 17027,
      municipality=="XOCHITEPEC" ~ 17028,
      municipality=="YAUTEPEC" ~ 17029,
      municipality=="YECAPIXTLA" ~ 17030,
      municipality=="ZACATEPEC" ~ 17031,
      municipality=="ZACUALPAN DE AMILPAS" ~ 17032,
      TRUE ~ uniqueid
    )
  )

# 9) egen valid = rowtotal(PAN PRI PRD UDM PVEM PC PSN PAS PMP PLM PFC)
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c("PAN","PRI","PRD","UDM","PVEM","PC","PSN","PAS","PMP","PLM","PFC")), na.rm=TRUE)) %>%
  ungroup()

# 10)merging with lista nominal

all_months <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") 

all_months <- all_months %>%
  filter(state == "MORELOS" &
           month== "July" & 
           year==2003 )

df_collapsed <- df_collapsed %>%
  left_join(all_months, by=c("section"))

# drop if _merge==2 => in R we check if matched => filter out any NA in `lista` 
df_collapsed <- df_collapsed %>%
  filter(!is.na(lista))

# rename lista->listanominal
df_collapsed <- df_collapsed %>%
  rename(listanominal = lista)

# 11) gen turnout= total/listanominal
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# gen year=2003, gen month="July"
df_collapsed <- df_collapsed %>%
  mutate(
    year = 2003,
    month= "July"
  )

# sort section => in R not strictly needed, but we can do:
df_2003 <- df_collapsed %>%
  arrange(section)

rm(df_collapsed)
rm(df)
###############################################################################
### "Ayu_Seccion_2006.csv" 
###############################################################################

# 1) Read CSV
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2006.csv", show_col_types = FALSE)

colnames(df) <- tolower(colnames(df))
# Remove "-" and spaces
names(df) <- gsub("[- ]", "", names(df))

# 2) rename municipio->municipality, seccion->section
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion
  )

# 3) drop if municipality=="" & section==.
df <- df %>%
  filter(!(municipality=="" & is.na(section)))

# 4) egen total = rowtotal(pan pri prdptpc pvem pna alternativa cc noregistrados nulos)
#    => might already have a 'total' column. The snippet does not explicitly create it here, 
#       but we see "drop if total==. | total==0".
#    If total doesn't exist, we create it rowwise. 
if(!"total" %in% names(df)) {
  df <- df %>%
    rowwise() %>%
    mutate(total = sum(c_across(c("pan","pri","prdptpc","pvem","pna","alternativa",
                                  "cc","noregistrados","nulos")), na.rm=TRUE)) %>%
    ungroup()
}

# 5) drop if total==. | total==0
df <- df %>%
  filter(!is.na(total) & total != 0)

# 6) destring listanominal pan-nulos total => convert them to numeric
to_destring <- c("listanominal","pan","pri","prdptpc","pvem","pna","alternativa",
                 "cc","noregistrados","nulos","total")
for(cn in to_destring) {
  if(cn %in% names(df)) {
    df[[cn]] <- suppressWarnings(as.numeric(df[[cn]]))
  }
}

# 7) collapse (sum) listanominal pan-nulos cc total, by(municipality, section)
collapse_cols <- c("listanominal","pan","pri","prdptpc","pvem","pna","alternativa",
                   "cc","noregistrados","nulos","total")
collapse_cols <- intersect(collapse_cols, names(df))

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(collapse_cols), sum, na.rm=TRUE), .groups="drop")

###############################################################################
### Candidaturas Comunes adjustments:
### * PVEM-PNA= CUAUTLA
### * PRI-PVEM= JIUTEPEC, PUENTE DE IXTLA, TOTOLAPAN, ZACUALPAN
###############################################################################

# define columns after collapse: cc, pvem, pna, pri. We'll handle them carefully
df_collapsed <- df_collapsed %>%
  mutate(
    # gen pvem_pna=0
    pvem_pna = 0,
    
    # replace pvem_pna= cc + pvem + pna if municipality=="CUAUTLA"
    pvem_pna = if_else(municipality=="CUAUTLA", cc + pvem + pna, pvem_pna),
    
    # zero out pvem, pna, cc if municipality=="CUAUTLA"
    pvem = if_else(municipality=="CUAUTLA", 0, pvem),
    pna  = if_else(municipality=="CUAUTLA", 0, pna),
    cc   = if_else(municipality=="CUAUTLA", 0, cc),
    
    # gen pri_pvem= pri + pvem + cc if municipality in [JIUTEPEC,PUENTE DE IXTLA,TOTOLAPAN,ZACUALPAN]
    pri_pvem = if_else(
      municipality %in% c("JIUTEPEC","PUENTE DE IXTLA","TOTOLAPAN","ZACUALPAN"),
      pri + pvem + cc,
      0
    ),
    
    # zero out pri, pvem, cc if municipality in that set
    pri = if_else(municipality %in% c("JIUTEPEC","PUENTE DE IXTLA","TOTOLAPAN","ZACUALPAN"), 0, pri),
    pvem= if_else(municipality %in% c("JIUTEPEC","PUENTE DE IXTLA","TOTOLAPAN","ZACUALPAN"), 0, pvem),
    cc  = if_else(municipality %in% c("JIUTEPEC","PUENTE DE IXTLA","TOTOLAPAN","ZACUALPAN"), 0, cc)
  )

# tab cc => we can check distribution, in R: table(df_collapsed$cc)
# drop cc
df_collapsed <- df_collapsed %>% select(-cc)

###############################################################################
### rename pan->PAN, pri->PRI, prdptpc->PRD_PT_PC, pvem->PVEM, pna->PANAL, alternativa->PAS
### rename pvem_pna->PVEM_PANAL, pri_pvem->PRI_PVEM
###############################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN        = pan,
    PRI        = pri,
    PRD_PT_PC  = prdptpc,
    PVEM       = pvem,
    PANAL      = pna,
    PAS        = alternativa,
    PVEM_PANAL = pvem_pna,
    PRI_PVEM   = pri_pvem
  )

###############################################################################
### gen turnout= total/listanominal
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

###############################################################################
### gen uniqueid=0, then replace for each municipality
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(uniqueid = 0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="AMACUZAC" ~ 17001,
      municipality=="ATLATLAHUCAN" ~ 17002,
      municipality=="AXOCHIAPAN" ~ 17003,
      municipality=="AYALA" ~ 17004,
      municipality=="COATLAN DEL RIO" ~ 17005,
      municipality=="CUAUTLA" ~ 17006,
      municipality=="CUERNAVACA" ~ 17007,
      municipality=="EMILIANO ZAPATA" ~ 17008,
      municipality=="HUITZILAC" ~ 17009,
      municipality=="JANTETELCO" ~ 17010,
      municipality=="JIUTEPEC" ~ 17011,
      municipality=="JOJUTLA" ~ 17012,
      municipality=="JONACATEPEC" ~ 17013,
      municipality=="MAZATEPEC" ~ 17014,
      municipality=="MIACATLAN" ~ 17015,
      municipality=="OCUITUCO" ~ 17016,
      municipality=="PUENTE DE IXTLA" ~ 17017,
      municipality=="TEMIXCO" ~ 17018,
      municipality=="TEMOAC" ~ 17033,
      municipality=="TEPALCINGO" ~ 17019,
      municipality=="TEPOZTLAN" ~ 17020,
      municipality=="TETECALA" ~ 17021,
      municipality=="TETELA DEL VOLCAN" ~ 17022,
      municipality=="TLALNEPANTLA" ~ 17023,
      municipality=="TLALTIZAPAN" ~ 17024,
      municipality=="TLAQUILTENANGO" ~ 17025,
      municipality=="TLAYACAPAN" ~ 17026,
      municipality=="TOTOLAPAN" ~ 17027,
      municipality=="XOCHITEPEC" ~ 17028,
      municipality=="YAUTEPEC" ~ 17029,
      municipality=="YECAPIXTLA" ~ 17030,
      municipality=="ZACATEPEC" ~ 17031,
      municipality=="ZACUALPAN" ~ 17032,
      TRUE ~ uniqueid
    )
  )

###############################################################################
### egen valid= rowtotal(PAN PRI PRD_PT_PC PVEM PANAL PAS PVEM_PANAL PRI_PVEM)
###############################################################################
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c("PAN","PRI","PRD_PT_PC","PVEM","PANAL","PAS","PVEM_PANAL","PRI_PVEM")), na.rm=TRUE)) %>%
  ungroup()

###############################################################################
### gen year=2006, gen month="July"
### sort section, then save Morelos_Section_2006.dta
###############################################################################
df_2006 <- df_collapsed %>%
  mutate(year=2006, month="July") %>%
  arrange(section)

rm(df_collapsed)
rm(df)

###############################################################################
### Ayu_Seccion_2009.csv
###############################################################################

# 1) Read CSV
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2009.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
# Remove "-" and spaces
names(df) <- gsub("[- ]", "", names(df))
# 2) rename nombre_municipio->municipality, seccion->section
df <- df %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion
  )

# 3) drop if municipality=="" & section==. 
#    Also drop if total==. | total==0
df <- df %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

# 4) destring listanominal nulos - total => forcibly convert to numeric
to_destring <- c("listanominal","nulos","pan","pri","prd","pt","pvem","pc",
                 "panal","psd","panpsd","pripc","prdpt","prdpc","prdptpc",
                 "total")
cols_in_df <- intersect(to_destring, names(df))
for(cn in cols_in_df) {
  df[[cn]] <- suppressWarnings(as.numeric(df[[cn]]))
}

# 5) collapse (sum) listanominal pan - total, by (municipality, section)
collapse_cols <- c("listanominal","pan","pri","prd","pt","pvem","pc","panal","psd",
                   "panpsd","pripc","prdpt","prdpc","prdptpc","nulos","total")
collapse_cols <- intersect(collapse_cols, names(df))

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(collapse_cols), sum, na.rm=TRUE), .groups="drop")

###############################################################################
### Candidaturas Comunes:
### * PRI-C => OCUITUCO
### * PAN-PSD => TETELA DEL VOLCAN
### * PRD-PT => CUERNAVACA, XOCHITEPEC
### * PRD-C  => (AXOCHIAPAN, JANTETELCO, JONACATEPEC, TEPALCINGO, TEPOZTLAN)
### * PRD-PT-C => JIUTEPEC
###############################################################################

# define dummy variables for each block
df_collapsed <- df_collapsed %>%
  mutate(
    dummy_pri_pc   = as.numeric(municipality=="OCUITUCO"),
    dummy_pan_psd  = as.numeric(municipality=="TETELA DEL VOLCAN"),
    dummy_prd_pt   = as.numeric(municipality %in% c("CUERNAVACA","XOCHITEPEC")),
    dummy_prd_pc   = as.numeric(municipality %in% c("AXOCHIAPAN","JANTETELCO",
                                                    "JONACATEPEC","TEPALCINGO","TEPOZTLAN")),
    dummy_prd_pt_pc= as.numeric(municipality=="JIUTEPEC")
  )

# replace panpsd = panpsd + pan + psd if dummy_pan_psd==1
df_collapsed <- df_collapsed %>%
  mutate(
    panpsd = if_else(dummy_pan_psd==1, panpsd + pan + psd, panpsd),
    pan    = if_else(dummy_pan_psd==1, 0, pan),
    psd    = if_else(dummy_pan_psd==1, 0, psd)
  ) %>%
  select(-dummy_pan_psd)

# replace pripc = pripc + pri + pc if dummy_pri_pc==1
df_collapsed <- df_collapsed %>%
  mutate(
    pripc = if_else(dummy_pri_pc==1, pripc + pri + pc, pripc),
    pri   = if_else(dummy_pri_pc==1, 0, pri),
    pc    = if_else(dummy_pri_pc==1, 0, pc)
  ) %>%
  select(-dummy_pri_pc)

# replace prdpt = prdpt + prd + pt if dummy_prd_pt==1
df_collapsed <- df_collapsed %>%
  mutate(
    prdpt = if_else(dummy_prd_pt==1, prdpt + prd + pt, prdpt),
    prd   = if_else(dummy_prd_pt==1, 0, prd),
    pt    = if_else(dummy_prd_pt==1, 0, pt)
  ) %>%
  select(-dummy_prd_pt)

# replace prdpc = prdpc + prd + pc if dummy_prd_pc==1
df_collapsed <- df_collapsed %>%
  mutate(
    prdpc = if_else(dummy_prd_pc==1, prdpc + prd + pc, prdpc),
    prd   = if_else(dummy_prd_pc==1, 0, prd),
    pc    = if_else(dummy_prd_pc==1, 0, pc)
  ) %>%
  select(-dummy_prd_pc)

# replace prdptpc = prdptpc + prd + pt + pc if dummy_prd_pt_pc==1
df_collapsed <- df_collapsed %>%
  mutate(
    prdptpc = if_else(dummy_prd_pt_pc==1, prdptpc + prd + pt + pc, prdptpc),
    prd     = if_else(dummy_prd_pt_pc==1, 0, prd),
    pt      = if_else(dummy_prd_pt_pc==1, 0, pt),
    pc      = if_else(dummy_prd_pt_pc==1, 0, pc)
  ) %>%
  select(-dummy_prd_pt_pc)

###############################################################################
### rename columns
###############################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN       = pan,
    PRI       = pri,
    PRD       = prd,
    PT        = pt,
    PVEM      = pvem,
    PC        = pc,
    PANAL     = panal,
    PSD       = psd,
    PAN_PSD   = panpsd,
    PRD_PT    = prdpt,
    PRD_PC    = prdpc,
    PRD_PT_PC = prdptpc,
    PRI_PC    = pripc
  )

###############################################################################
### gen turnout= total/listanominal
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

###############################################################################
### gen uniqueid=0, then set by municipality
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(uniqueid=0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="AMACUZAC"          ~ 17001,
      municipality=="ATLATLAHUCAN"      ~ 17002,
      municipality=="AXOCHIAPAN"        ~ 17003,
      municipality=="AYALA"             ~ 17004,
      municipality=="COATLAN DEL RIO"   ~ 17005,
      municipality=="CUAUTLA"           ~ 17006,
      municipality=="CUERNAVACA"        ~ 17007,
      municipality=="EMILIANO ZAPATA"   ~ 17008,
      municipality=="HUITZILAC"         ~ 17009,
      municipality=="JANTETELCO"        ~ 17010,
      municipality=="JIUTEPEC"          ~ 17011,
      municipality=="JOJUTLA"           ~ 17012,
      municipality=="JONACATEPEC"       ~ 17013,
      municipality=="MAZATEPEC"         ~ 17014,
      municipality=="MIACATLAN"         ~ 17015,
      municipality=="OCUITUCO"          ~ 17016,
      municipality=="PUENTE DE IXTLA"   ~ 17017,
      municipality=="TEMIXCO"           ~ 17018,
      municipality=="TEMOAC"            ~ 17033,
      municipality=="TEPALCINGO"        ~ 17019,
      municipality=="TEPOZTLAN"         ~ 17020,
      municipality=="TETECALA"          ~ 17021,
      municipality=="TETELA DEL VOLCAN" ~ 17022,
      municipality=="TLALNEPANTLA"      ~ 17023,
      municipality=="TLALTIZAPAN"       ~ 17024,
      municipality=="TLAQUILTENANGO"    ~ 17025,
      municipality=="TLAYACAPAN"        ~ 17026,
      municipality=="TOTOLAPAN"         ~ 17027,
      municipality=="XOCHITEPEC"        ~ 17028,
      municipality=="YAUTEPEC"          ~ 17029,
      municipality=="YECAPIXTLA"        ~ 17030,
      municipality=="ZACATEPEC"         ~ 17031,
      municipality=="ZACUALPAN"         ~ 17032,
      TRUE ~ uniqueid
    )
  )

###############################################################################
### egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PSD PAN_PSD PRI_PC PRD_PT PRD_PC PRD_PT_PC)
###############################################################################
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(
    "PAN","PRI","PRD","PT","PVEM","PC","PANAL","PSD",
    "PAN_PSD","PRI_PC","PRD_PT","PRD_PC","PRD_PT_PC"
  )), na.rm=TRUE)) %>%
  ungroup()

###############################################################################
### gen year=2009, gen month="July"
### sort section
### save Morelos_Section_2009.dta, replace
###############################################################################
df_2009<- df_collapsed %>%
  mutate(year=2009, month="July") %>%
  arrange(section)

rm(df_collapsed)
rm(df)

###############################################################################
### Ayu_Seccion_2012.csv
###############################################################################
# 1) Read CSV
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2012.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
# 2) replace municipality = subinstr(municipality,"*","",.)
#    i.e. remove asterisk from 'municipality'
#    drop if municipality==""
df <- df %>%
  mutate(
    municipality = str_replace_all(municipality, fixed("*"), "")
  ) %>%
  filter(municipality != "")

# 3) collapse (sum) pan - nulos, by (municipality, section)
collapse_cols <- c("pan","pri","prd","pt","pc","pvem","panal","psd","c_pri_panal",
                   "c_prd_pt_pc","c_prd_pc","c_pvem_panal","c_pri_pvem","nulos")

collapse_vars <- intersect(collapse_cols, names(df))

# Group by municipality,section => sum
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(collapse_vars), sum, na.rm=TRUE), .groups="drop")

# 7) rename c_pri_panal -> PRI_PANAL, c_prd_pt_pc -> PRD_PT_PC, c_prd_pc->PRD_PC, c_pvem_panal->PVEM_PANAL, c_pri_pvem->PRI_PVEM
df_collapsed <- df_collapsed %>%
  rename_with(~"PRI_PANAL", .cols="c_pri_panal") %>%
  rename_with(~"PRD_PT_PC",  .cols="c_prd_pt_pc") %>%
  rename_with(~"PRD_PC",     .cols="c_prd_pc") %>%
  rename_with(~"PVEM_PANAL", .cols="c_pvem_panal") %>%
  rename_with(~"PRI_PVEM",   .cols="c_pri_pvem")


df_collapsed <- df_collapsed %>%
  rename(
    PAN   = pan,
    PRI   = pri,
    PRD   = prd,
    PT    = pt,
    PC    = pc,
    PVEM  = pvem,
    PANAL = panal,
    PSD   = psd
  )
names(df_collapsed)
# 8) egen total= rowtotal(PAN PRI PRD PT PC PVEM PANAL PSD PRI_PANAL PRD_PT_PC PRD_PC PVEM_PANAL PRI_PVEM nulos)
#    => let's do rowwise sum ignoring NA, then snippet says 'drop nulos'
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(total = sum(c_across(c("PAN","PRI","PRD","PT","PC","PVEM","PANAL",
                                "PSD","PRI_PANAL","PRD_PT_PC","PRD_PC",
                                "PVEM_PANAL","PRI_PVEM","nulos")),
                     na.rm=TRUE)) %>%
  ungroup() %>%
  select(-nulos)

# 9) generate uniqueid=0, then replace for each municipality
df_collapsed <- df_collapsed %>%
  mutate(uniqueid=0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="AMACUZAC" ~ 17001,
      municipality=="ATLATLAHUCAN" ~ 17002,
      municipality=="AXOCHIAPAN" ~ 17003,
      municipality=="AYALA" ~ 17004,
      municipality=="COATLAN DEL RIO" ~ 17005,
      municipality=="CUAUTLA" ~ 17006,
      municipality=="CUERNAVACA" ~ 17007,
      municipality=="EMILIANO ZAPATA" ~ 17008,
      municipality=="HUITZILAC" ~ 17009,
      municipality=="JANTETELCO" ~ 17010,
      municipality=="JIUTEPEC" ~ 17011,
      municipality=="JOJUTLA" ~ 17012,
      municipality=="JONACATEPEC" ~ 17013,
      municipality=="MAZATEPEC" ~ 17014,
      municipality=="MIACATLAN" ~ 17015,
      municipality=="OCUITUCO" ~ 17016,
      municipality=="PUENTE DE IXTLA" ~ 17017,
      municipality=="TEMIXCO" ~ 17018,
      municipality=="TEMOAC" ~ 17033,
      municipality=="TEPALCINGO" ~ 17019,
      municipality=="TEPOZTLAN" ~ 17020,
      municipality=="TETECALA" ~ 17021,
      municipality=="TETELA DEL VOLCAN" ~ 17022,
      municipality=="TLALNEPANTLA" ~ 17023,
      municipality=="TLALTIZAPAN" ~ 17024,
      municipality=="TLAQUILTENANGO" ~ 17025,
      municipality=="TLAYACAPAN" ~ 17026,
      municipality=="TOTOLAPAN" ~ 17027,
      municipality=="XOCHITEPEC" ~ 17028,
      municipality=="YAUTEPEC" ~ 17029,
      municipality=="YECAPIXTLA" ~ 17030,
      municipality=="ZACATEPEC" ~ 17031,
      municipality=="ZACUALPAN" ~ 17032,
      TRUE ~ uniqueid
    )
  )

# 10) egen valid= rowtotal(...) => rowwise sum ignoring NA
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c("PAN","PRI","PRD","PT","PC","PVEM","PANAL","PSD",
                                "PRI_PANAL","PRD_PT_PC","PRD_PC","PVEM_PANAL","PRI_PVEM")),
                     na.rm=TRUE)) %>%
  ungroup()

df_collapsed <- df_collapsed %>%
  mutate(PRI_PANAL = ifelse(PRI_PANAL>0, PRI_PANAL + PRI + PANAL, 0),
         PRI = ifelse(PRI_PANAL>0, 0, PRI),
         PANAL = ifelse(PRI_PANAL>0, 0, PANAL),
         PVEM_PANAL = ifelse(PVEM_PANAL>0, PVEM_PANAL + PANAL + PVEM, 0),
         PVEM = ifelse(PRI_PANAL>0, 0, PVEM),
         PANAL = ifelse(PRI_PANAL>0, 0, PANAL),
         PRI_PVEM = ifelse(PRI_PVEM>0, PRI_PVEM + PRI + PVEM, 0),
         PVEM = ifelse(PRI_PVEM>0, 0, PVEM),
         PRI = ifelse(PRI_PVEM>0, 0, PRI),
         PRD_PC = ifelse(PRD_PC>0, PRD_PC + PC + PRD, 0),
         PRD = ifelse(PRD_PC>0, 0, PRD),
         PC = ifelse(PRD_PC>0, 0, PC),
         PRD_PT_PC = ifelse(PRD_PT_PC>0, PRD_PT_PC + PT + PRD + PC, 0),
         PRD = ifelse(PRD_PT_PC>0, 0, PRD),
         PT = ifelse(PRD_PT_PC>0, 0, PT),
         PC = ifelse(PRD_PT_PC>0, 0, PC)) 

# 11) Lista Nominal


all_months <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") 

all_months <- all_months %>%
  filter(state == "MORELOS" &
           month== "July" & 
           year==2012 ) %>% 
  select(section,lista)

df_collapsed <- df_collapsed %>%
  left_join(all_months, by=c("section"))

df_collapsed <- df_collapsed %>% 
  rename(listanominal = lista)

# 12) gen turnout= total/listanominal
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# 17) gen year=2012, gen month="July"
#     sort section
#     save Morelos_Section_2012.dta, replace
df_2012 <- df_collapsed %>%
  mutate(
    year=2012,
    month="July"
  ) %>%
  arrange(section)

###############################################################################
### Ayuntamientos_2015.xlsx
###############################################################################

# 1) Read Exce
df <- read_excel("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayuntamientos_2015.xlsx", 
                 col_names = TRUE)

# 2) replace PRI=. if C_PRI_PVEM_PANAL!=.
#    => if there's a non-missing C_PRI_PVEM_PANAL, set PRI to NA
#    same for PVEM, PANAL
if(!"C_PRI_PVEM_PANAL" %in% names(df)) {
  stop("Column 'C_PRI_PVEM_PANAL' not found in data; check your spreadsheet or code references.")
}

df <- df %>%
  mutate(
    PRI   = if_else(!is.na(C_PRI_PVEM_PANAL), NA_real_, PRI),
    PVEM  = if_else(!is.na(C_PRI_PVEM_PANAL), NA_real_, PVEM),
    PANAL = if_else(!is.na(C_PRI_PVEM_PANAL), NA_real_, PANAL)
  )

# rename C_PRI_PVEM_PANAL -> PRI_PVEM_PANAL
df <- df %>%
  rename(PRI_PVEM_PANAL = C_PRI_PVEM_PANAL)

# 3) replace PRD=. if C_PRD_PT!=.
#    replace PT=. if C_PRD_PT!=.
#    rename C_PRD_PT -> PRD_PT
if(!"C_PRD_PT" %in% names(df)) {
  stop("Column 'C_PRD_PT' not found in data; check your spreadsheet or code references.")
}

df <- df %>%
  mutate(
    PRD = if_else(!is.na(C_PRD_PT), NA_real_, PRD),
    PT  = if_else(!is.na(C_PRD_PT), NA_real_, PT)
  ) %>%
  rename(PRD_PT = C_PRD_PT)

# 4) collapse (sum) PAN - CI_1 total listanominal, by(municipality section uniqueid)
#    => group_by(...) sum relevant columns
collapse_cols <- c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","PSD","MORENA","PES",
                   "PH","PRD_PT","PRI_PVEM_PANAL","CI_1","total","listanominal")
collapse_cols <- intersect(collapse_cols, names(df))

df_collapsed <- df %>%
  group_by(municipality, section, uniqueid) %>%
  summarize(across(all_of(collapse_cols), sum, na.rm=TRUE), .groups="drop")

# 5) egen valid = rowtotal(...). In R => rowwise sum ignoring NA
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c("PAN","PRI","PRD","PT","PVEM","MC","PANAL",
                                "PSD","MORENA","PES","PH","PRD_PT","PRI_PVEM_PANAL",
                                "CI_1")),
                     na.rm=TRUE)) %>%
  ungroup()

# 11) gen year=2015, month="June", STATE="MORELOS"
df_2015 <- df_collapsed %>%
  mutate(
    year=2015,
    month="June",
    turnout = total/listanominal
  )
names(df_2015)

####### 2018

all_sheets <- excel_sheets("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayuntamientos_2018.xlsx")
sheet_index <- seq_along(all_sheets)

for (i in sheet_index) {
  sheetname <- all_sheets[i]
  message(paste("Processing sheet:", sheetname))
  
  # Read Excel
  df_sheet <- read_excel(
    "../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayuntamientos_2018.xlsx", 
    sheet = sheetname, 
    col_names = TRUE
  )
  
  # If there's no uniqueid column, skip
  if (!"uniqueid" %in% names(df_sheet)) {
    message("Sheet: ", sheetname, " has no 'uniqueid' column, skipping.")
    next
  }
  
  # Filter out empty uniqueid
  df_sheet <- df_sheet %>%
    filter(uniqueid != "")
  
  # Convert empty strings to "0"
  for (cn in names(df_sheet)) {
    df_sheet[[cn]] <- as.character(df_sheet[[cn]])
    df_sheet[[cn]][df_sheet[[cn]] == ""] <- "0"
  }
  
  # 1) Replace invalid chars with underscores
  # 2) Truncate to 32 chars to satisfy Stata .dta limit
  df_sheet <- df_sheet %>%
    rename_with(function(x) {
      # Step 1: replace all non-alphanumeric with underscores:
      x <- gsub("[^A-Za-z0-9_]", "_", x)
      # Step 2: truncate to 32 characters
      substr(x, 1, 32)
    })
  
  # Save as .dta
  file_out <- paste0(gsub("[^A-Za-z0-9_]", "_", sheetname), ".dta")
  write_dta(df_sheet, file_out)
  message(paste("Wrote:", file_out))
}

# 1) We'll define the list of files to append:
files_to_append <- c(
  "reporte__1_.dta","reporte__2_.dta","reporte__3_.dta","reporte__4_.dta",
  "reporte__5_.dta","reporte__6_.dta","reporte__7_.dta","reporte__8_.dta",
  "reporte__9_.dta","reporte__10_.dta","reporte__11_.dta","reporte__12_.dta",
  "reporte__13_.dta","reporte__14_.dta","reporte__15_.dta","reporte__16_.dta",
  "reporte__17_.dta","reporte__18_.dta","reporte__19_.dta","reporte__20_.dta",
  "reporte__21_.dta","reporte__22_.dta","reporte__23_.dta","reporte__24_.dta",
  "reporte__25_.dta","reporte__26_.dta","reporte__27_.dta","reporte__28_.dta",
  "reporte__29_.dta","reporte__30_.dta","reporte__31_.dta","reporte__32_.dta",
  "reporte__33_.dta"
)

## 2) Append them into a single data frame
df_appended <- NULL
for (ff in files_to_append) {
  message("Reading and appending: ", ff)
  temp_df <- read_dta(ff)
  df_appended <- if (is.null(df_appended)) {
    temp_df
  } else {
    bind_rows(df_appended, temp_df)
  }
}

# Suppose your data frame is called df_all
# We'll remove the prefix "votos_" from each column name
names(df_appended) <- sub("^votos_", "", names(df_appended))

## 3) Convert columns from character to numeric if possible ("destring")
df_all <- df_appended %>%
  mutate(across(pan:cand_ind_del_pueblo_para_e, ~ {
    if (is.character(.)) suppressWarnings(as.numeric(.)) else .
  }))

##############################################################################
# Step 2: Create PT_MORENA_PES by merging columns like cc_pt_morena_pes,
#         config_cc_pt_morena, etc.
##############################################################################
# Initialize PT_MORENA_PES to NA_real_ if it doesn't exist
if (!"PT_MORENA_PES" %in% names(df_all)) {
  df_all <- df_all %>% mutate(PT_MORENA_PES = NA_real_)
}

# If cc_pt_morena_pes columns exist
if ("cc_pt_morena_pes" %in% names(df_all)) {
  df_all <- df_all %>%
    mutate(
      PT_MORENA_PES = if_else(
        !is.na(cc_pt_morena_pes),
        coalesce(PT_MORENA_PES, 0) +
          coalesce(cc_pt_morena_pes, 0) +
          coalesce(config_cc_pt_morena, 0) +
          coalesce(config_cc_pt_pes, 0) +
          coalesce(config_cc_morena_pes, 0) +
          coalesce(morena, 0) +
          coalesce(pt, 0) +
          coalesce(es, 0),
        PT_MORENA_PES
      ),
      # Zero out / set NA the individual columns if cc_pt_morena_pes is not NA
      morena = if_else(!is.na(cc_pt_morena_pes), NA_real_, morena),
      es    = if_else(!is.na(cc_pt_morena_pes), NA_real_, es),
      pt     = if_else(!is.na(cc_pt_morena_pes), NA_real_, pt)
    ) %>%
    # optionally drop the used columns
    select(-any_of(c("cc_pt_morena_pes","config_cc_pt_morena","config_cc_pt_pes","config_cc_morena_pes")))
}

# If "coal_pt_morena_pes" also exists, do the same logic:
if ("coal_pt_morena_pes" %in% names(df_all)) {
  df_all <- df_all %>%
    mutate(
      PT_MORENA_PES = if_else(
        !is.na(coal_pt_morena_pes),
        coalesce(PT_MORENA_PES, 0) +
          coalesce(coal_pt_morena_pes, 0) +
          coalesce(config_coal_pt_morena, 0) +
          coalesce(config_coal_pt_pes, 0) +
          coalesce(config_coal_morena_pes, 0) +
          coalesce(morena, 0) +
          coalesce(pt, 0) +
          coalesce(es, 0),
        PT_MORENA_PES
      ),
      morena = if_else(!is.na(coal_pt_morena_pes), NA_real_, morena),
      es    = if_else(!is.na(coal_pt_morena_pes), NA_real_, es),
      pt     = if_else(!is.na(coal_pt_morena_pes), NA_real_, pt)
    ) %>%
    select(-any_of(c("coal_pt_morena_pes","config_coal_pt_morena","config_coal_pt_pes","config_coal_morena_pes")))
}

##############################################################################
# Step 3: Create/merge PAN_MC similarly
##############################################################################
if (!"PAN_MC" %in% names(df_all)) {
  df_all <- df_all %>% mutate(PAN_MC = NA_real_)
}

if ("cc_pan_mc" %in% names(df_all)) {
  df_all <- df_all %>%
    mutate(
      PAN_MC = if_else(
        !is.na(cc_pan_mc),
        coalesce(PAN_MC,0) + coalesce(cc_pan_mc,0) + coalesce(pan,0) + coalesce(mc,0),
        PAN_MC
      ),
      pan = if_else(!is.na(cc_pan_mc), NA_real_, pan),
      mc  = if_else(!is.na(cc_pan_mc), NA_real_, mc)
    ) %>%
    select(-cc_pan_mc)
}

##############################################################################
# Step 4: Create/merge PRD_PSD
##############################################################################
if (!"PRD_PSD" %in% names(df_all)) {
  df_all <- df_all %>% mutate(PRD_PSD=NA_real_)
}

if ("coal_prd_psd" %in% names(df_all)) {
  df_all <- df_all %>%
    mutate(
      PRD_PSD = if_else(
        !is.na(coal_prd_psd),
        coalesce(PRD_PSD,0) + coalesce(coal_prd_psd,0) + coalesce(prd,0) + coalesce(psd,0),
        PRD_PSD
      ),
      prd = if_else(!is.na(coal_prd_psd), NA_real_, prd),
      psd = if_else(!is.na(coal_prd_psd), NA_real_, psd)
    ) %>%
    select(-coal_prd_psd)
}

##############################################################################
# Step 5: Possibly "gen CI_1=..." or rename cand_ind columns, summing them to CI_1 / CI_2 / CI_3
##############################################################################
# For example, if we have cand_ind_luis*, cand_ind_juntos*, etc.
# We'll do rowSums into CI_1, CI_2, CI_3. If columns do not exist, rowSums returns zero.

cand_1_cols <- c("cand_ind_luis_granados_a_c","cand_ind_juntos_para_atlat",
                 "cand_ind_juntos_a_renovar_","cand_ind_es_el_tiempo_de_l",
                 "cand_ind_ayala_de_mis_amor","cand_ind_todos_somos_coatl")
df_all <- df_all %>%
  mutate(
    CI_1 = rowSums(across(any_of(cand_1_cols)), na.rm=TRUE)
  ) %>%
  select(-any_of(cand_1_cols))

cand_2_cols <- c("cand_ind_raul_aguirre_espi",
                 "cand_ind_movimiento_indepe",
                 "cand_ind_unidos_por_huitzi")
df_all <- df_all %>%
  mutate(
    CI_2 = rowSums(across(any_of(cand_2_cols)), na.rm=TRUE)
  ) %>%
  select(-any_of(cand_2_cols))

cand_3_cols <- c("cand_ind_miguel_angel_tova",
                 "cand_ind_zeus_el_destructo",
                 "cand_ind_reconstruccion_hu")

df_all <- df_all %>%
  mutate(
    CI_3 = rowSums(across(any_of(cand_3_cols)), na.rm=TRUE)
  ) %>%
  select(-any_of(cand_3_cols))

##############################################################################
# Step 6: Final "collapse (sum) ..." by (municipality, section, uniqueid)
##############################################################################
# We'll define the columns to sum
final_cols <- c("pan","pri","prd","pvem","pt","mc","na","psd","morena","es","humanista",
                "PT_MORENA_PES","PAN_MC","PRD_PSD","PRD_PVEM_PSD",
                "CI_1","CI_2","CI_3","Total_de_Votos","Lista_Nominal")

final_cols <- intersect(final_cols, names(df_all))

df_2018_collapsed <- df_all %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(all_of(final_cols), sum, na.rm=TRUE), .groups="drop") %>% 
  rename("PAN"="pan",
         "PRI"="pri",
         "PRD"="prd",
         "PVEM"="pvem",
         "PT"="pt",
         "MC"="mc",
         "PANAL"="na",
         "PSD"="psd",
         "MORENA"="morena",
         "PES"="es",
         "PH"="humanista",
         "total"="Total_de_Votos",
         "listanominal"="Lista_Nominal")

# Then compute valid, turnout, year=2018, etc.
df_2018_collapsed <- df_2018_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(any_of(c("PAN","PRI","PRD","PVEM","PT","MC","PANAL","PSD","MORENA","PES","PH",
                                  "PT_MORENA_PES","PAN_MC","PRD_PSD","PRD_PVEM_PSD","CI_1","CI_2","CI_3"))),
                na.rm=TRUE),
    turnout = total / listanominal,
    year    = 2018,
    month   = "July",
    STATE   = "MORELOS",
    section = as.numeric(section),
    uniqueid = as.numeric(uniqueid)
  ) %>%
  ungroup()


#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

# Process Morelos 2021 data
# to combine multiple municipal election files into a single dataset

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(fs)

# Define the base path for Morelos 2021 data
base_path <- "../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/21/"

# Main processing function for Morelos municipalities
process_morelos_elections <- function(base_path) {
  
  # Step 1: Find all CSV files in the directory
  csv_files <- dir_ls(base_path, 
                      regexp = "\\.csv$", 
                      type = "file") %>%
    # Filter out temporary files
    .[!str_detect(path_file(.), "^~")]
  
  cat("Found", length(csv_files), "CSV files\n")
  
  # Step 2: Extract municipality names from filenames
  file_info <- tibble(
    file_path = csv_files,
    filename = path_file(file_path),
    # Extract municipality name from complex filename pattern
    municipality_raw = filename %>%
      str_remove("\\.csv$") %>%
      str_remove("^Registro de Computos ") %>%
      str_remove(" Morelos Proceso Electoral 2020-2021 IMPEPAC$") %>%
      str_to_upper() %>%
      str_trim()
  )
  
  # Step 3: Preview the file structure
  cat("\nFile structure preview:\n")
  print(file_info %>% select(filename, municipality_raw) %>% head(10))
  
  # Step 4: Function to safely read each CSV file
  read_csv_safely <- function(file_path, municipality) {
    tryCatch({
      
      # Read the CSV file with robust parsing
      data <- read_csv(file_path, 
                       locale = locale(encoding = "UTF-8"),
                       col_types = cols(.default = col_character()),
                       na = c("", "NA", "N/A", "-"))
      
      # Skip if data is empty
      if(nrow(data) == 0) {
        cat("Warning: Empty file:", file_path, "\n")
        return(NULL)
      }
      
      # Clean column names (remove spaces, special characters)
      names(data) <- names(data) %>%
        str_trim() %>%
        str_replace_all("\\s+", "_") %>%
        str_replace_all("[^A-Za-z0-9_]", "") %>%
        str_to_upper()
      
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
      data = map2(file_path, municipality_raw, read_csv_safely)
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
  cat("Sample columns:", paste(head(all_columns, 10), collapse = ", "), "\n")
  
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
inspect_morelos_files <- function(base_path, n_files = 3) {
  
  csv_files <- dir_ls(base_path, regexp = "\\.csv$") %>%
    .[!str_detect(path_file(.), "^~")]
  
  cat("Inspecting first", n_files, "files:\n\n")
  
  for(i in 1:min(n_files, length(csv_files))) {
    cat("File", i, ":", path_file(csv_files[i]), "\n")
    
    tryCatch({
      # Read first few rows to understand structure
      sample_data <- read_csv(csv_files[i], n_max = 5, 
                              col_types = cols(.default = col_character()))
      
      cat("  Columns:", paste(names(sample_data), collapse = ", "), "\n")
      cat("  Dimensions:", nrow(sample_data), "x", ncol(sample_data), "\n")
      
      # Show sample of first column
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

# Function to post-process the combined data
post_process_morelos <- function(combined_data) {
  
  processed_data <- combined_data
  
  # Convert numeric columns (adjust column patterns based on your data)
  numeric_patterns <- c("VOTOS", "TOTAL", "CASILLA", "SECCION", "LISTA")
  
  for(pattern in numeric_patterns) {
    numeric_cols <- names(processed_data)[str_detect(names(processed_data), pattern)]
    for(col in numeric_cols) {
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

# Municipality name standardization for Morelos
standardize_morelos_municipalities <- function(municipality_names) {
  municipality_names %>%
    str_to_upper() %>%
    str_trim() %>%
    # Handle common variations
    str_replace("ZACUALPAN DE AMILPAS", "ZACUALPAN DE AMILPAS") %>%
    str_replace("CUERNAVACA", "CUERNAVACA") %>%
    # Add more standardizations as needed
    str_squish()
}

# Execution instructions
cat("EXECUTION PLAN FOR MORELOS 2021 ELECTION DATA:\n")
cat(strrep("=", 50), "\n\n")

cat("Step 1: Inspect files\n")
cat("inspect_morelos_files(base_path)\n\n")

cat("Step 2: Process all files\n")
cat("results <- process_morelos_elections(base_path)\n\n")

cat("Step 3: Post-process and clean data\n")
cat("final_data <- post_process_morelos(results$combined_data)\n\n")

cat("Step 4: Export results\n")
cat("write_csv(final_data, 'morelos_2021_combined.csv')\n\n")

inspect_morelos_files(base_path)
results <- process_morelos_elections(base_path)
final_data <- post_process_morelos(results$combined_data)

output_path <- "../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/"
write.csv(results$combined_data, paste0(output_path, "morelos_2021_combined.csv"), row.names = FALSE)

# Load the 2021 dataset
data_2021 <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/morelos_2021_combined.csv") %>% 
  mutate(listanominal = LNOMINAL + L_NOMINAL)

names(data_2021)

# Rename columns
data_2021 <- data_2021 %>%
  dplyr::rename(section = SECCION,
                total = TOTVOTOS,
                no_reg = NO_REGISTRADOS,
                nulos = NUM_VOTOS_NULOS,
                PANAL = "NA",
                FXM = FPM,
                PH = HUMANISTA,
                CI_1 = CAND_IND_AGUSTIN_TOLEDANO_AMARO,
                CI_2 = CAND_IND_CELSO_NIETO_ESTRADA,
                CI_3 = CAND_IND_PEDRO_ANTONIO_MONTENEGRO_MORGADO,
                MORENA_PANAL_ES_CC = CC_MORENA_NA_ES, # CC Candidatura Comun
                PAN_PH_CC = CC_PAN_HUMANISTA,
                PAN_PSD_CC = CC_PAN_PSD,
                PRD_PSD_CC = CC_PRD_PSD,
                PRD_PRI = COAL_PRD_PRI,
                MORENA_ES_CC = CONFIG_CC_MORENA_ES,
                MORENA_PANAL_CC = CONFIG_CC_MORENA_NA,
                PANAL_ES_CC = CONFIG_CC_NA_ES) %>%
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
  dplyr::filter(section > 0) %>% 
  select(municipality, section,
         PAN, PRI, PRD, PT, PVEM, MC, MORENA,
         APM, BC, ES, FM, FXM, FUTURO, PH, MAS, MP, PANAL, PAS, PES, 
         PODEMOS, PSD, RPM, RSP, CI_1, CI_2, CI_3,
         MORENA_PANAL_ES_CC, PAN_PH_CC, PAN_PSD_CC, PRD_PSD_CC, PRD_PRI, MORENA_ES_CC,
         MORENA_PANAL_CC, PANAL_ES_CC,
         no_reg, nulos, total, listanominal
  ) %>% 
  mutate(
    across(c(section, PAN, PRI, PRD, PT, PVEM, MC, MORENA,
             APM, BC, ES, FM, FXM, FUTURO, PH, MAS, MP, PANAL, PAS, PES, 
             PODEMOS, PSD, RPM, RSP, CI_1, CI_2, CI_3,
             MORENA_PANAL_ES_CC, PAN_PH_CC, PAN_PSD_CC, PRD_PSD_CC, PRD_PRI, 
             MORENA_ES_CC, MORENA_PANAL_CC, PANAL_ES_CC,
             no_reg, nulos, total, listanominal), 
           as.numeric)
  )

# Generate uniqueids
data_2021 <- data_2021 %>% 
  mutate(
    uniqueid = case_when(
      municipality=="AMACUZAC" ~ 17001,
      municipality=="ATLATLAHUCAN" ~ 17002,
      municipality=="AXOCHIAPAN" ~ 17003,
      municipality=="AYALA" ~ 17004,
      municipality=="COATLAN DEL RIO" ~ 17005,
      municipality=="CUAUTLA" ~ 17006,
      municipality=="CUERNAVACA" ~ 17007,
      municipality=="EMILIANO ZAPATA" ~ 17008,
      municipality=="HUITZILAC" ~ 17009,
      municipality=="JANTETELCO" ~ 17010,
      municipality=="JIUTEPEC" ~ 17011,
      municipality=="JOJUTLA" ~ 17012,
      municipality=="JONACATEPEC" ~ 17013,
      municipality=="MAZATEPEC" ~ 17014,
      municipality=="MIACATLAN" ~ 17015,
      municipality=="OCUITUCO" ~ 17016,
      municipality=="PUENTE DE IXTLA" ~ 17017,
      municipality=="TEMIXCO" ~ 17018,
      municipality=="TEMOAC" ~ 17033,
      municipality=="TEPALCINGO" ~ 17019,
      municipality=="TEPOZTLAN" ~ 17020,
      municipality=="TETECALA" ~ 17021,
      municipality=="TETELA DEL VOLCAN" ~ 17022,
      municipality=="TLALNEPANTLA" ~ 17023,
      municipality=="TLALTIZAPAN" ~ 17024,
      municipality=="TLAQUILTENANGO" ~ 17025,
      municipality=="TLAYACAPAN" ~ 17026,
      municipality=="TOTOLAPAN" ~ 17027,
      municipality=="XOCHITEPEC" ~ 17028,
      municipality=="YAUTEPEC" ~ 17029,
      municipality=="YECAPIXTLA" ~ 17030,
      municipality=="ZACATEPEC" ~ 17031,
      municipality=="ZACUALPAN" ~ 17032,
      TRUE ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:listanominal), 
                          sum, na.rm = TRUE))

# Calculate valid votes and final details
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    valid = sum(c_across(PAN:PANAL_ES_CC), na.rm = TRUE),
    turnout = total/listanominal,
    year = 2021,
    month = "June"
  )

#####################################
### PROCESSING DATA FOR 2024 -------
#####################################

# Morelos 2024 Data Processor
# to combine multiple municipal election files into a single dataset

# Load required libraries
library(tidyverse)
library(readxl)
library(writexl)

# Set working directory to your data folder
# Adjust this path to match your directory structure
base_path <- "../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/21/24/"

# Function to safely read files (handles both CSV and Excel formats)
safe_read_file <- function(file_path) {
  tryCatch({
    # Check file extension
    file_ext <- tools::file_ext(file_path)
    
    if (file_ext %in% c("csv", "CSV")) {
      # Try different encodings for CSV files
      df <- tryCatch({
        read.csv(file_path, stringsAsFactors = FALSE, encoding = "UTF-8")
      }, error = function(e) {
        tryCatch({
          read.csv(file_path, stringsAsFactors = FALSE, encoding = "latin1")
        }, error = function(e2) {
          read.csv(file_path, stringsAsFactors = FALSE)
        })
      })
    } else if (file_ext %in% c("xlsx", "xls", "XLSX", "XLS")) {
      # For Excel files, read the first sheet by default
      df <- read_excel(file_path, sheet = 1)
    } else {
      warning(paste("Unknown file format for:", basename(file_path)))
      return(NULL)
    }
    
    # Add municipality identifier based on filename
    municipality <- extract_municipality_name(basename(file_path))
    df$municipality <- municipality
    df$source_file <- basename(file_path)
    
    # Standardize column names (convert to lowercase, replace spaces with underscores)
    names(df) <- tolower(gsub("[^A-Za-z0-9]", "_", names(df)))
    names(df) <- gsub("_+", "_", names(df))  # Remove multiple underscores
    names(df) <- gsub("^_|_$", "", names(df))  # Remove leading/trailing underscores
    
    return(df)
    
  }, error = function(e) {
    warning(paste("Failed to read file:", basename(file_path), "- Error:", e$message))
    return(NULL)
  })
}

# Function to extract municipality name from filename
extract_municipality_name <- function(filename) {
  # Remove file extension
  name <- tools::file_path_sans_ext(filename)
  
  # Try to extract municipality name (usually appears after "COMPUTOS" or similar)
  # This is a basic extraction - you might need to adjust based on your filename patterns
  if (grepl("COMPUTOS", name, ignore.case = TRUE)) {
    # Split by spaces and look for the word after "COMPUTOS"
    parts <- strsplit(name, "\\s+")[[1]]
    computos_index <- grep("COMPUTOS", parts, ignore.case = TRUE)
    if (length(computos_index) > 0 && computos_index[1] < length(parts)) {
      return(toupper(parts[computos_index[1] + 1]))
    }
  }
  
  # Fallback: return the first word of the filename
  first_word <- strsplit(name, "\\s+")[[1]][1]
  return(toupper(gsub("[^A-Za-z]", "", first_word)))
}

# Function to find and process all election files
process_morelos_elections <- function(directory_path) {
  cat("Looking for election files in:", directory_path, "\n")
  
  # Find all CSV and Excel files
  file_patterns <- c("*.csv", "*.CSV", "*.xlsx", "*.xls", "*.XLSX", "*.XLS")
  all_files <- c()
  
  for (pattern in file_patterns) {
    files <- list.files(directory_path, pattern = glob2rx(pattern), 
                        full.names = TRUE, recursive = TRUE)
    all_files <- c(all_files, files)
  }
  
  if (length(all_files) == 0) {
    stop("No CSV or Excel files found in the specified directory")
  }
  
  cat("Found", length(all_files), "files:\n")
  for (file in all_files) {
    cat("  -", basename(file), "\n")
  }
  
  # Read all files
  cat("\nReading files...\n")
  all_data <- list()
  
  for (i in seq_along(all_files)) {
    cat("Processing file", i, "of", length(all_files), ":", basename(all_files[i]), "\n")
    df <- safe_read_file(all_files[i])
    
    if (!is.null(df) && nrow(df) > 0) {
      all_data[[i]] <- df
    }
  }
  
  # Remove NULL entries
  all_data <- all_data[!sapply(all_data, is.null)]
  
  if (length(all_data) == 0) {
    stop("No data could be read from any files")
  }
  
  cat("Successfully read", length(all_data), "files\n")
  
  # Check column compatibility before binding
  cat("\nChecking column compatibility...\n")
  all_columns <- unique(unlist(lapply(all_data, names)))
  
  # Show column summary
  column_counts <- table(unlist(lapply(all_data, names)))
  cat("Column frequency across files:\n")
  print(sort(column_counts, decreasing = TRUE))
  
  # Standardize columns - add missing columns as NA
  for (i in seq_along(all_data)) {
    missing_cols <- setdiff(all_columns, names(all_data[[i]]))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        all_data[[i]][[col]] <- NA
      }
    }
    # Reorder columns to match
    all_data[[i]] <- all_data[[i]][all_columns]
  }
  
  # Combine all data
  cat("\nCombining data...\n")
  combined_data <- bind_rows(all_data)
  
  cat("Combined dataset dimensions:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n")
  cat("Municipalities included:", paste(unique(combined_data$municipality), collapse = ", "), "\n")
  
  return(combined_data)
}

# Main execution
cat("=== Morelos Municipal Elections Data Processor ===\n\n")

# Process all files
combined_elections <- process_morelos_elections(base_path)

# Show summary statistics
cat("\n=== DATA SUMMARY ===\n")
cat("Total records:", nrow(combined_elections), "\n")
cat("Total columns:", ncol(combined_elections), "\n")
cat("Municipalities:", length(unique(combined_elections$municipality)), "\n")
cat("Source files:", length(unique(combined_elections$source_file)), "\n")

# Display first few rows
cat("\n=== SAMPLE DATA ===\n")
print(head(combined_elections, 3))

# Save the combined dataset
output_csv <- file.path(dirname(base_path), "morelos_elections_combined.csv")

cat("\n=== SAVING RESULTS ===\n")

# Save as CSV
write.csv(combined_elections, output_csv, row.names = FALSE, fileEncoding = "UTF-8")
cat("CSV saved to:", output_csv, "\n")

summary_file <- file.path(dirname(base_path), "processing_summary.csv")
write.csv(summary_stats, summary_file, row.names = FALSE)
cat("Processing summary saved to:", summary_file, "\n")

cat("\n=== PROCESS COMPLETED SUCCESSFULLY ===\n")

# Load the 2024 dataset
data_2024 <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/morelos_2024_combined.csv") %>% 
  dplyr::mutate(seccion = as.numeric(seccion)) %>% 
  dplyr::filter(seccion > 0)

names(data_2024)

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(section = seccion,
                listanominal = l_nominal,
                total = tot_votos,
                no_reg = no_registrados,
                nulos = num_votos_nulos,
                PAN = pan,
                PRI = pri,
                PRD = prd,
                PVEM = pvem,
                PT = pt,
                MC = mc,
                MORENA = morena,
                PANAL = na,
                PES = pes,
                MAS = mas,
                MP = progresa,
                RSP = rsp,
                PAN_PRI_PRD_RSP = c_pan_pri_prd_rsp,
                MORENA_NA_PES_MAS = c_morena_na_pes_mas,
                PAN_PRI_PRD = c_pan_pri_prd,
                PAN_PRI_RSP = c_pan_pri_rsp,
                PAN_PRD_RSP = c_pan_prd_rsp,
                PRI_PRD_RSP = c_pri_prd_rsp,
                MORENA_PANAL_PES = c_morena_na_pes,
                MORENA_PANAL_MAS = c_morena_na_mas,
                MORENA_PES_MAS = c_morena_pes_mas,
                PANAL_PES_MAS = c_na_pes_mas,
                PAN_PRI = c_pan_pri,
                PAN_PRD = c_pan_prd,
                PAN_RSP = c_pan_rsp,
                PRI_PRD = c_pri_prd,
                PRI_RSP = c_pri_rsp,
                PRD_RSP = c_prd_rsp,
                MORENA_PANAL = c_morena_na,
                MORENA_PES = c_morena_pes,
                MORENA_MAS = c_morena_mas,
                PANAL_PES = c_na_pes,
                NA_MAS = c_na_mas,
                PES_MAS = c_pes_mas,
                MC_MP = c_mc_progresa,
                CI_1 = cand_ind_luis_armando_jaime_maldonado,
                CI_2 = cand_ind_andres_tapia_franco,
                CI_3 = cand_ind_perseo_quiroz_rendon) %>% 
  dplyr::mutate(
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    municipality = case_when(
      municipality == "COATLÁN" ~ "COATLAN DEL RIO",
      municipality == "EMILIANO" ~ "EMILIANO ZAPATA",
      municipality == "PUENTE" ~ "PUENTE DE IXTLA",
      municipality == "TEPOZTLÁN" ~ "TEPOZTLAN",
      municipality == "TETELA" ~ "TETELA DEL VOLCAN",
      municipality == "TLALTIZAPÁN" ~ "TLALTIZAPAN",
      municipality == "TOTOLAPÁN" ~ "TOTOLAPAN", 
      municipality == "ZACUALPÁN" ~ "ZACUALPAN",
      TRUE ~ municipality
    )
  )

# Generate uniqueids
data_2024 <- data_2024 %>% 
  mutate(
    uniqueid = case_when(
      municipality=="AMACUZAC" ~ 17001,
      municipality=="ATLATLAHUCAN" ~ 17002,
      municipality=="AXOCHIAPAN" ~ 17003,
      municipality=="AYALA" ~ 17004,
      municipality=="COATLAN DEL RIO" ~ 17005,
      municipality=="CUAUTLA" ~ 17006,
      municipality=="CUERNAVACA" ~ 17007,
      municipality=="EMILIANO ZAPATA" ~ 17008,
      municipality=="HUITZILAC" ~ 17009,
      municipality=="JANTETELCO" ~ 17010,
      municipality=="JIUTEPEC" ~ 17011,
      municipality=="JOJUTLA" ~ 17012,
      municipality=="JONACATEPEC" ~ 17013,
      municipality=="MAZATEPEC" ~ 17014,
      municipality=="MIACATLAN" ~ 17015,
      municipality=="OCUITUCO" ~ 17016,
      municipality=="PUENTE DE IXTLA" ~ 17017,
      municipality=="TEMIXCO" ~ 17018,
      municipality=="TEMOAC" ~ 17033,
      municipality=="TEPALCINGO" ~ 17019,
      municipality=="TEPOZTLAN" ~ 17020,
      municipality=="TETECALA" ~ 17021,
      municipality=="TETELA DEL VOLCAN" ~ 17022,
      municipality=="TLALNEPANTLA" ~ 17023,
      municipality=="TLALTIZAPAN" ~ 17024,
      municipality=="TLAQUILTENANGO" ~ 17025,
      municipality=="TLAYACAPAN" ~ 17026,
      municipality=="TOTOLAPAN" ~ 17027,
      municipality=="XOCHITEPEC" ~ 17028,
      municipality=="YAUTEPEC" ~ 17029,
      municipality=="YECAPIXTLA" ~ 17030,
      municipality=="ZACATEPEC" ~ 17031,
      municipality=="ZACUALPAN" ~ 17032,
      TRUE ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(PAN:PRD_RSP, MORENA_NA_PES_MAS:CI_3, no_reg:listanominal), 
           \(x) sum(x, na.rm = TRUE))
  )

# Calculate valid votes and final details
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    valid = sum(c_across(PAN:CI_3), na.rm = TRUE),
    turnout = total/listanominal,
    year = 2024,
    month = "June"
  )

# Combine the dataframes, handling different columns by filling with NA
morelos_all <- bind_rows(df_1997,
                         df_2000,
                         df_2001,#extraordinary
                         df_2003,
                         df_2006,
                         df_2009,
                         df_2012,
                         df_2015,
                         df_2018_collapsed,
                         collapsed_2021,
                         collapsed_2024) %>% 
  dplyr::select(-c(day,month,state,noregistrados,nulos,STATE)) %>% 
  dplyr::mutate(turnout = ifelse(listanominal == 0, NA, turnout))

data.table::fwrite(morelos_all,"../../../Processed Data/morelos/morelos_process_raw_data.csv")

