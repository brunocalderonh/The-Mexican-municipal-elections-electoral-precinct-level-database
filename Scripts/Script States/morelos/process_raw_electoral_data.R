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
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997_No_LN.csv", 
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
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2000.csv", show_col_types = FALSE)
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
df <- read_excel("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Extraordinario 2001.xlsx", 
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
df <- fread("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003_No_LN.csv", 
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
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2006.csv", show_col_types = FALSE)

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
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2009.csv", show_col_types = FALSE)
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
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2012.csv", show_col_types = FALSE)
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
df <- read_excel("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2015.xlsx", 
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

all_sheets <- excel_sheets("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2018.xlsx")
sheet_index <- seq_along(all_sheets)

for (i in sheet_index) {
  sheetname <- all_sheets[i]
  message(paste("Processing sheet:", sheetname))
  
  # Read Excel
  df_sheet <- read_excel(
    "../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2018.xlsx", 
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


# Combine the dataframes, handling different columns by filling with NA
morelos_all <- bind_rows(df_1997,
                           df_2000,
                           df_2001,#extraordinary
                           df_2003,
                           df_2006,
                           df_2009,
                           df_2012,
                           df_2015,
                         df_2018_collapsed) %>% 
  dplyr::select(-c(day,month,state,noregistrados,nulos,STATE)) %>% 
  dplyr::mutate(turnout = ifelse(listanominal == 0, NA, turnout))

data.table::fwrite(morelos_all,"../../../Processed Data/morelos/morelos_process_raw_data.csv")

