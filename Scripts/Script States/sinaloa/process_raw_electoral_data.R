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
### PART X: Replicating the Stata snippet for Ayu_Seccion_1997_No_LN.csv
###############################################################################


# 1) Read CSV: "Ayu_Seccion_1997_No_LN.csv"
df <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997_No_LN.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
##############################################################################
# Load required libraries
##############################################################################
library(dplyr)
library(readr)    # For CSV input
library(readxl)   # For Excel input
library(haven)    # For reading/saving Stata .dta files
library(stringr)

##############################################################################
# 1) Ayu_Seccion_2001_No_LN.csv
##############################################################################
df_2001 <- read_csv("Ayu_Seccion_2001_No_LN.csv") %>%
  rename(
    municipality = municipio,
    section      = seccion
  ) %>%
  # drop if municipality=="" & section==.
  filter(!(municipality == "" & is.na(section)))

# Convert pan - ptcdppnpbs to numeric
vars_2001 <- c("pan","pri","prd","pt","pvem","cdppn","psn","pas","pbs",
               "noregistrados","nulos","panprdpvem","panpvem","prdpt","ptpsn",
               "psnpbs","ptpascdppnpsn","ptpas","prdcdppn","panprd","ptpbs",
               "panprdptpvem","ptcdppnpsn","prdpvem","ptcdppnpbs")
for (v in vars_2001) {
  if (v %in% names(df_2001)) {
    df_2001[[v]] <- as.numeric(df_2001[[v]])
  }
}

# The script then does many sums to combine partial coalition variables. 
# We keep that logic but skip aggregator (collapse) by municipality, rowranks, winners.
df_2001 <- df_2001 %>%
  # e.g., sum pan prd pvem panprdpvem if panprdpvem!=.
  mutate(
    panprdpvem = ifelse(!is.na(panprdpvem),
                        panprdpvem + coalesce(pan,0) + coalesce(prd,0) + coalesce(pvem,0),
                        panprdpvem),
    pan        = ifelse(!is.na(panprdpvem), 0, pan),
    prd        = ifelse(!is.na(panprdpvem), 0, prd),
    pvem       = ifelse(!is.na(panprdpvem), 0, pvem)
  ) %>%
  # Then sum pan pvem panpvem if panpvem!=.
  mutate(
    panpvem = ifelse(!is.na(panpvem),
                     panpvem + coalesce(pan,0) + coalesce(pvem,0),
                     panpvem),
    pan     = ifelse(!is.na(panpvem),0, pan),
    pvem    = ifelse(!is.na(panpvem),0, pvem)
  ) %>%
  # sum prdpt prd pt if prdpt!=.
  mutate(
    prdpt = ifelse(!is.na(prdpt),
                   prdpt + coalesce(prd,0) + coalesce(pt,0),
                   prdpt),
    prd   = ifelse(!is.na(prdpt),0, prd),
    pt    = ifelse(!is.na(prdpt),0, pt)
  ) %>%
  # sum ptpsn pt psn if ptpsn !=.
  mutate(
    ptpsn = ifelse(!is.na(ptpsn),
                   ptpsn + coalesce(pt,0) + coalesce(psn,0),
                   ptpsn),
    pt    = ifelse(!is.na(ptpsn),0, pt),
    psn   = ifelse(!is.na(ptpsn),0, psn)
  ) %>%
  # sum psnpbs psn pbs
  mutate(
    psnpbs = ifelse(!is.na(psnpbs),
                    psnpbs + coalesce(psn,0) + coalesce(pbs,0),
                    psnpbs),
    psn    = ifelse(!is.na(psnpbs),0, psn),
    pbs    = ifelse(!is.na(psnpbs),0, pbs)
  ) %>%
  # sum ptpascdppnpsn ptpas cdppn psn
  mutate(
    ptpascdppnpsn = ifelse(!is.na(ptpascdppnpsn),
                           ptpascdppnpsn + coalesce(ptpas,0) + coalesce(cdppn,0) + coalesce(psn,0),
                           ptpascdppnpsn),
    ptpas  = ifelse(!is.na(ptpascdppnpsn),0, ptpas),
    cdppn  = ifelse(!is.na(ptpascdppnpsn),0, cdppn),
    psn    = ifelse(!is.na(ptpascdppnpsn),0, psn)
  ) %>%
  # sum prdcdppn prd cdppn
  mutate(
    prdcdppn = ifelse(!is.na(prdcdppn),
                      prdcdppn + coalesce(prd,0) + coalesce(cdppn,0),
                      prdcdppn),
    prd      = ifelse(!is.na(prdcdppn),0, prd),
    cdppn    = ifelse(!is.na(prdcdppn),0, cdppn)
  ) %>%
  # sum panprd pan prd
  mutate(
    panprd   = ifelse(!is.na(panprd),
                      panprd + coalesce(pan,0) + coalesce(prd,0),
                      panprd),
    pan      = ifelse(!is.na(panprd),0, pan),
    prd      = ifelse(!is.na(panprd),0, prd)
  ) %>%
  # sum ptpbs pt pbs
  mutate(
    ptpbs = ifelse(!is.na(ptpbs),
                   ptpbs + coalesce(pt,0) + coalesce(pbs,0),
                   ptpbs),
    pt    = ifelse(!is.na(ptpbs),0, pt),
    pbs   = ifelse(!is.na(ptpbs),0, pbs)
  ) %>%
  # sum panprdptpvem pan prd pt pvem
  mutate(
    panprdptpvem = ifelse(!is.na(panprdptpvem),
                          panprdptpvem + coalesce(pan,0) + coalesce(prd,0) + coalesce(pt,0) + coalesce(pvem,0),
                          panprdptpvem),
    pan      = ifelse(!is.na(panprdptpvem),0, pan),
    prd      = ifelse(!is.na(panprdptpvem),0, prd),
    pt       = ifelse(!is.na(panprdptpvem),0, pt),
    pvem     = ifelse(!is.na(panprdptpvem),0, pvem)
  ) %>%
  # sum ptcdppnpsn pt cdppn psn
  mutate(
    ptcdppnpsn = ifelse(!is.na(ptcdppnpsn),
                        ptcdppnpsn + coalesce(pt,0) + coalesce(cdppn,0) + coalesce(psn,0),
                        ptcdppnpsn),
    pt         = ifelse(!is.na(ptcdppnpsn),0, pt),
    cdppn      = ifelse(!is.na(ptcdppnpsn),0, cdppn),
    psn        = ifelse(!is.na(ptcdppnpsn),0, psn)
  ) %>%
  # sum prdpvem prd pvem
  mutate(
    prdpvem = ifelse(!is.na(prdpvem),
                     prdpvem + coalesce(prd,0) + coalesce(pvem,0),
                     prdpvem),
    prd     = ifelse(!is.na(prdpvem),0, prd),
    pvem    = ifelse(!is.na(prdpvem),0, pvem)
  ) %>%
  # sum ptcdppnpbs pt cdppn pbs
  mutate(
    ptcdppnpbs = ifelse(!is.na(ptcdppnpbs),
                        ptcdppnpbs + coalesce(pt,0) + coalesce(cdppn,0) + coalesce(pbs,0),
                        ptcdppnpbs),
    pt         = ifelse(!is.na(ptcdppnpbs),0, pt),
    cdppn      = ifelse(!is.na(ptcdppnpbs),0, cdppn),
    pbs        = ifelse(!is.na(ptcdppnpbs),0, pbs)
  )

# Next, Stata does: collapse (sum) pan-ptcdppnpbs by(municipality section) => aggregator => omitted
# Then it creates `total = rowtotal(...)`, drops if total==0 => we'll skip aggregator as requested.

# We'll rename variables to e.g. PAN, PRI, PRD, ...
df_2001 <- df_2001 %>%
  rename(
    PAN                = pan,
    PRI                = pri,
    PRD                = prd,
    PT                 = pt,
    PVEM               = pvem,
    PC                 = cdppn,
    PSN                = psn,
    PAS                = pas,
    PBS                = pbs,
    PAN_PRD_PVEM       = panprdpvem,
    PAN_PVEM           = panpvem,
    PRD_PT             = prdpt,
    PT_PSN             = ptpsn,
    PSN_PBS            = psnpbs,
    PT_PAS_PC_PSN      = ptpascdppnpsn,
    prdcdppn           = prdcdppn,
    PAN_PRD            = panprd,
    PT_PBS             = ptpbs,
    PAN_PRD_PT_PVEM    = panprdptpvem,
    PT_PC_PSN          = ptcdppnpsn,
    PRD_PVEM           = prdpvem,
    PT_PC_PBS          = ptcdppnpbs
  ) %>%
  # drop noregistrados nulos
  select(-noregistrados, -nulos)

# Now merging with all_months_years, etc. We skip aggregator.

# We replicate partial logic:
df_all <- read_dta("C:/Users/Horacio/Dropbox/Turismo Electoral/all_months_years.dta") %>%
  bind_rows(read_dta("C:/Users/jmarshall/Dropbox/Turismo Electoral/all_months_years.dta")) %>%
  filter(ed==25, month==9, year==2001) %>%
  select(section=seccion, lista)

df_2001 <- df_2001 %>%
  mutate(ed=25, seccion=section) %>%
  left_join(df_all, by="section") %>%
  filter(!is.na(lista)) %>%
  select(-ed, -seccion) %>%
  rename(listanominal=lista)

df_2001 <- df_2001 %>%
  mutate(
    turnout = total / listanominal,
    # drop noregistrados, nulos => already dropped
    uniqueid = case_when(
      municipality=="AHOME"             ~ 25001,
      municipality=="ANGOSTURA"         ~ 25002,
      municipality=="BADIRAGUATO"       ~ 25003,
      municipality=="CHOIX"             ~ 25007,
      municipality=="CONCORDIA"         ~ 25004,
      municipality=="COSALA"            ~ 25005,
      municipality=="CULIACAN"          ~ 25006,
      municipality=="EL FUERTE"         ~ 25010,
      municipality=="ELOTA"             ~ 25008,
      municipality=="ESCUINAPA"         ~ 25009,
      municipality=="GUASAVE"           ~ 25011,
      municipality=="MAZATLAN"          ~ 25012,
      municipality=="MOCORITO"          ~ 25013,
      municipality=="NAVOLATO"          ~ 25018,
      municipality=="ROSARIO"           ~ 25014,
      municipality=="SALVADOR ALVARADO" ~ 25015,
      municipality=="SAN IGNACIO"       ~ 25016,
      municipality=="SINALOA"           ~ 25017,
      TRUE                              ~ 0
    )
  ) 

# aggregator, rowranks, winner => omitted
df_2001 <- df_2001 %>%
  mutate(
    year  = 2001,
    month = "November"
  ) %>%
  arrange(section)


##############################################################################
# 2) Ayu_Seccion_LN_2004.xlsx
##############################################################################
df_ln_2004 <- read_excel("Ayu_Seccion_LN_2004.xlsx", sheet="Sheet1", col_names=TRUE) %>%
  # rename SECCIÓN->SECCIN if needed
  # drop if SECCIN==.
  filter(!is.na(SECCIÓN)) %>%
  rename(
    section      = SECCIÓN,
    listanominal = LISTANOMINAL
  ) %>%
  # aggregator collapse => omitted
  arrange(section)


##############################################################################
# 3) Ayu_Seccion_2004_No_LN.csv
##############################################################################
df_2004 <- read_csv("Ayu_Seccion_2004_No_LN.csv") %>%
  rename(municipality=municipio, section=seccion) %>%
  filter(!(municipality=="" & is.na(section)))

vars_2004 <- c("pan","pri","prd","pt","pvem","pc","pbs","noregistrados","nulos",
               "prdpt","prdptpbs","prdpbs","panprd","pripvempbs","prdptpc")
for (v in vars_2004) {
  if (v %in% names(df_2004)) {
    df_2004[[v]] <- as.numeric(df_2004[[v]])
  }
}


df_2004 <- df_2004 %>%
  mutate(
    prdpt = ifelse(!is.na(prdpt), prdpt + coalesce(prd,0)+coalesce(pt,0), prdpt),
    prd   = ifelse(!is.na(prdpt), 0, prd),
    pt    = ifelse(!is.na(prdpt), 0, pt),
    prdptpbs = ifelse(!is.na(prdptpbs), prdptpbs + coalesce(prd,0)+coalesce(pt,0)+coalesce(pbs,0), prdptpbs),
    prd    = ifelse(!is.na(prdptpbs),0, prd),
    pt     = ifelse(!is.na(prdptpbs),0, pt),
    pbs    = ifelse(!is.na(prdptpbs),0, pbs),
    prdpbs = ifelse(!is.na(prdpbs), prdpbs+coalesce(prd,0)+coalesce(pbs,0), prdpbs),
    prd    = ifelse(!is.na(prdpbs),0, prd),
    pbs    = ifelse(!is.na(prdpbs),0, pbs),
    panprd = ifelse(!is.na(panprd), panprd+coalesce(pan,0)+coalesce(prd,0), panprd),
    pan    = ifelse(!is.na(panprd),0, pan),
    prd    = ifelse(!is.na(panprd),0, prd),
    prdptpc = ifelse(!is.na(prdptpc), prdptpc+coalesce(prd,0)+coalesce(pt,0)+coalesce(pc,0), prdptpc),
    prd     = ifelse(!is.na(prdptpc),0, prd),
    pt      = ifelse(!is.na(prdptpc),0, pt),
    pc      = ifelse(!is.na(prdptpc),0, pc)
  )


df_2004 <- df_2004 %>%
  rename(
    PAN         = pan,
    PRI         = pri,
    PRD         = prd,
    PT          = pt,
    PVEM        = pvem,
    PC          = pc,
    PBS         = pbs,
    PAN_PRD     = panprd,
    PRD_PT      = prdpt,
    PRD_PT_PC   = prdptpc,
    PRD_PT_PBS  = prdptpbs,
    PRD_PBS     = prdpbs,
    PRI_PVEM_PBS= pripvempbs
  ) %>%
  select(-noregistrados,-nulos)

# Merge with LN_2004
df_ln_2004 <- read_dta("Ayu_Seccion_LN_2004.dta")
df_2004 <- df_2004 %>%
  arrange(section) %>%
  left_join(df_ln_2004, by="section") %>%
  filter(!is.na(listanominal))

df_2004 <- df_2004 %>%
  mutate(
    turnout = total/listanominal,
    uniqueid = case_when(
      municipality=="AHOME"~25001,
      # ...
      municipality=="SINALOA"~25017,
      TRUE~0
    )
  )


df_2004 <- df_2004 %>%
  mutate(
    year  = 2004,
    month = "November"
  ) %>%
  arrange(section)


##############################################################################
# 4) Ayu_Seccion_LN_2007.xlsx
##############################################################################
df_ln_2007 <- read_excel("Ayu_Seccion_LN_2007.xlsx", sheet="Sheet1", col_names=TRUE) %>%
  filter(!is.na(SECCIÓN)) %>%
  rename(section=SECCIÓN, listanominal=LISTANOMINAL) %>%
  mutate(missing = (listanominal==0))


df_all <- read_dta("C:/Users/Horacio/Dropbox/Turismo Electoral/all_months_years.dta") %>%
  bind_rows(read_dta("C:/Users/jmarshall/Dropbox/Turismo Electoral/all_months_years.dta")) %>%
  filter(ed==25, month==9, year==2007) %>%
  select(section=seccion, lista)

df_ln_2007 <- df_ln_2007 %>%
  left_join(df_all, by="section") %>%
  filter(!is.na(lista)) %>%
  mutate(
    listanominal = ifelse(missing>=1 & lista>listanominal, lista, listanominal)
  ) %>%
  select(-missing, -lista) %>%
  arrange(section)

##############################################################################
# 5) Ayu_Seccion_2007_No_LN.csv
##############################################################################
df_2007 <- read_csv("Ayu_Seccion_2007_No_LN.csv") %>%
  rename(municipality=municipio, section=seccion) %>%
  filter(!(municipality=="" & is.na(section)))

vars_2007 <- c("pan","pripanal","prd","pt","pvem","pc","pas","noregistrados",
               "nulos","pripanalprd","prdptpc","prdpt","pripanalpc","ptpas")
for (v in vars_2007) {
  if (v %in% names(df_2007)) {
    df_2007[[v]] <- as.numeric(df_2007[[v]])
  }
}


df_2007 <- df_2007 %>%
  mutate(
    pripanalprd = ifelse(!is.na(pripanalprd),
                         pripanalprd + coalesce(pripanal,0)+coalesce(prd,0),
                         pripanalprd),
    pripanal    = ifelse(!is.na(pripanalprd),0, pripanal),
    prd         = ifelse(!is.na(pripanalprd),0, prd),
    prdptpc     = ifelse(!is.na(prdptpc),
                         prdptpc + coalesce(prd,0)+coalesce(pt,0)+coalesce(pc,0),
                         prdptpc),
    prd         = ifelse(!is.na(prdptpc),0, prd),
    pt          = ifelse(!is.na(prdptpc),0, pt),
    pc          = ifelse(!is.na(prdptpc),0, pc),
    prdpt       = ifelse(!is.na(prdpt),
                         prdpt + coalesce(prd,0)+coalesce(pt,0),
                         prdpt),
    prd         = ifelse(!is.na(prdpt),0, prd),
    pt          = ifelse(!is.na(prdpt),0, pt),
    pripanalpc  = ifelse(!is.na(pripanalpc),
                         pripanalpc + coalesce(pripanal,0)+coalesce(pc,0),
                         pripanalpc),
    pripanal    = ifelse(!is.na(pripanalpc),0, pripanal),
    pc          = ifelse(!is.na(pripanalpc),0, pc),
    ptpas       = ifelse(!is.na(ptpas),
                         ptpas+coalesce(pt,0)+coalesce(pas,0),
                         ptpas),
    pt          = ifelse(!is.na(ptpas),0, pt),
    pas         = ifelse(!is.na(ptpas),0, pas)
  )

df_2007 <- df_2007 %>%
  rename(
    PAN            = pan,
    PRI_PANAL      = pripanal,
    PRD            = prd,
    PT             = pt,
    PVEM           = pvem,
    PC             = pc,
    PAS            = pas,
    PRI_PRD_PANAL  = pripanalprd,
    PRD_PT_PC      = prdptpc,
    PRD_PT         = prdpt,
    PRI_PC_PANAL   = pripanalpc,
    PT_PAS         = ptpas
  ) %>%
  select(-noregistrados, -nulos)

# Merge with LN_2007
df_ln_2007 <- read_dta("Ayu_Seccion_LN_2007.dta")
df_2007 <- df_2007 %>%
  arrange(section) %>%
  left_join(df_ln_2007, by="section") %>%
  filter(!is.na(listanominal))

df_2007 <- df_2007 %>%
  mutate(
    turnout = total/listanominal,
    uniqueid = case_when(
      municipality=="AHOME" ~25001,
      # ...
      municipality=="SINALOA"~25017,
      TRUE~0
    )
  ) %>%
  mutate(
    year  = 2007,
    month = "October"
  ) %>%
  arrange(section)


##############################################################################
# 6) Ayu_Seccion_2010.csv
##############################################################################
df_2010 <- read_csv("Ayu_Seccion_2010.csv") %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

vars_2010 <- c("listanominal","nulos","missing","panprdptpc","pripvempanal",
               "total","noregistrados")
for (v in vars_2010) {
  if (v %in% names(df_2010)) {
    df_2010[[v]] <- as.numeric(df_2010[[v]])
  }
}

df_2010 <- df_2010 %>%
  group_by(municipality, section) %>%
  summarise(
    missing       = sum(missing, na.rm=TRUE),
    listanominal  = sum(listanominal, na.rm=TRUE),
    panprdptpc    = sum(panprdptpc, na.rm=TRUE),
    pripvempanal  = sum(pripvempanal, na.rm=TRUE),
    nulos         = sum(nulos, na.rm=TRUE),
    total         = sum(total, na.rm=TRUE),
    noregistrados = sum(noregistrados, na.rm=TRUE),
    .groups="drop"
  )

# merge with all_months_years => partial
df_all <- read_dta("C:/Users/Horacio/Dropbox/Turismo Electoral/all_months_years.dta") %>%
  bind_rows(read_dta("C:/Users/jmarshall/Dropbox/Turismo Electoral/all_months_years.dta")) %>%
  filter(ed==25, month==6, year==2010) %>%
  select(section=seccion, lista)

df_2010 <- df_2010 %>%
  left_join(df_all, by="section") %>%
  filter(!is.na(lista)) %>%
  mutate(
    listanominal = ifelse(missing>=1 & lista>listanominal, lista, listanominal)
  ) %>%
  select(-missing, -lista)

df_2010 <- df_2010 %>%
  rename(
    PAN_PRD_PT_PC   = panprdptpc,
    PRI_PVEM_PANAL  = pripvempanal
  ) %>%
  mutate(
    turnout = total/listanominal
  ) %>%
  select(-noregistrados, -nulos) %>%
  mutate(
    uniqueid=case_when(
      municipality=="AHOME"~25001,
      # ...
      municipality=="SINALOA"~25017,
      TRUE~0
    ),
    year  = 2010,
    month = "July"
  ) %>%
  arrange(section)

##############################################################################
# 7) Ayu_Seccion_2013.dta
##############################################################################
df_2013 <- read_dta("Ayu_Seccion_2013.dta") %>%
  mutate(
    turnout = total/listanominal
  ) %>%
  select(-NoRegistrados, -Nulos) %>%
  mutate(
    uniqueid=case_when(
      municipality=="AHOME"~25001,
      # ...
      municipality=="SINALOA"~25017,
      TRUE~0
    )
  )


df_2013 <- df_2013 %>%
  mutate(
    year  = 2013,
    month = "July"
  ) %>%
  arrange(section)


##############################################################################
# 8) Append 2001,2004,2007,2010,2013 => Sinaloa_ALL.dta
##############################################################################
df_2001 <- read_dta("Sinaloa_Section_2001.dta")
df_2004 <- read_dta("Sinaloa_Section_2004.dta")
df_2007 <- read_dta("Sinaloa_Section_2007.dta")
df_2010 <- read_dta("Sinaloa_Section_2010.dta")
df_2013 <- read_dta("Sinaloa_Section_2013.dta")

all_sin <- bind_rows(df_2001, df_2004, df_2007, df_2010, df_2013)

file.remove("Sinaloa_Section_2001.dta")
file.remove("Sinaloa_Section_2004.dta")
file.remove("Sinaloa_Section_2007.dta")
file.remove("Sinaloa_Section_2010.dta")
file.remove("Sinaloa_Section_2013.dta")

# winner logic => omitted
# "gen PAN_winner=..., tab winner_counter => omitted
write_dta(all_sin, "Sinaloa_ALL.dta", version=12)

##############################################################################
# 9) Ayuntamientos_2016.xlsx
##############################################################################
excel_2016 <- "Ayuntamientos_2016.xlsx"
sheet_names_2016 <- excel_sheets(excel_2016)

for (sname in sheet_names_2016) {
  df_sheet <- read_excel(excel_2016, sheet=sname, col_types="text") %>%
    mutate(municipality = sname) %>%
    filter(CASILLASECCIÓN!="") %>%
    mutate(across(everything(), ~ ifelse(.=="", "0", .)))
  
  safe_name <- str_replace_all(sname, "[^A-Za-z0-9_\\. ]", "_")
  write_dta(df_sheet, paste0(safe_name, ".dta"))
}

# Then the script appends e.g. CHOIX.dta, FUERTE.dta, etc.
files_2016 <- c("CHOIX.dta","FUERTE.dta","AHOME.dta","SINALOA.dta","GUASAVE.dta",
                "ANGOSTURA.dta","SALVADOR ALVARADO.dta","MOCORITO.dta",
                "BADIRAGUATO.dta","CULIACAN.dta","NAVOLATO.dta","COSALA.dta",
                "ELOTA.dta","SAN IGNACIO.dta","MAZATLAN.dta","CONCORDIA.dta",
                "ROSARIO.dta","ESCUINAPA.dta")

all_2016 <- NULL
for (f in files_2016) {
  if (file.exists(f)) {
    tmp <- read_dta(f)
    all_2016 <- bind_rows(all_2016, tmp)
  }
}

# Clean up
all_2016[] <- lapply(all_2016, function(x) as.numeric(as.character(x)))
all_2016 <- all_2016 %>%
  filter(!is.na(CASILLASECCIÓN)) %>%
  select(-O,-N) %>%  # drop O..N if exist
  rename(
    section = CASILLASECCIÓN,
    no_reg  = VOTOSACANDIDATOSNOREGISTRADO,
    nulo    = VOTOSNULOS,
    total   = `VOTACIÓNTOTAL`
  ) %>%
  mutate(
    uniqueid = case_when(
      municipality=="CHOIX" ~ 25007,
      # ...
      municipality=="ESCUINAPA" ~ 25009,
      TRUE ~ NA_real_
    )
  )

# rename MC_MAS -> MC_PAS, MAS->PAS in code => replicate partial
all_2016 <- all_2016 %>%
  rename(
    MC_PAS = MC_MAS,
    PAS    = MAS
  ) %>%
  mutate(
    # replace PRI_PANAL=PRI+PANAL+PRI_PANAL if PRI_PANAL!=. => aggregator step => omitted
    # We'll do partial coalition merges but not aggregator by municipality.
    PRI_PANAL = ifelse(!is.na(PRI_PANAL), PRI_PANAL + coalesce(PRI,0)+coalesce(PANAL,0), PRI_PANAL),
    PRI       = ifelse(!is.na(PRI_PANAL), NA, PRI),
    PANAL     = ifelse(!is.na(PRI_PANAL), NA, PANAL),
    
    MC_PAS    = ifelse(!is.na(MC_PAS), MC_PAS + coalesce(MC,0)+coalesce(PAS,0), MC_PAS),
    MC        = ifelse(!is.na(MC_PAS), NA, MC),
    PAS       = ifelse(!is.na(MC_PAS), NA, PAS)
  )

# aggregator => omitted
# Keep partial lumps
all_2016 <- all_2016 %>%
  mutate(
    year  = 2016,
    month = "June",
    STATE = "SINALOA"
  )

# Now merging LN2016 => replicate partial
df_ln2016 <- read_dta("../Listas Nominales/LN 2012-2019/2016/LN2016.dta") %>%
  filter(entidad==25, month==5, seccion!=0) %>%
  mutate(uniqueid = (entidad*1000)+municipio) %>%
  select(uniqueid, section=seccion, lista)

all_2016 <- all_2016 %>%
  left_join(df_ln2016, by=c("uniqueid","section")) %>%
  filter(!is.na(lista)) %>%
  rename(listanominal=lista)

all_2016 <- all_2016 %>%
  group_by(uniqueid, section) %>%
  summarise(across(everything(), ~sum(.x, na.rm=TRUE)), .groups="drop") %>%
  mutate(
    turnout = total/listanominal
  )

# aggregator => omitted
# rowranks => omitted
# winner => omitted

write_dta(all_2016, "Sinaloa_Section_2016.dta")

# remove the small .dta files
for (f in files_2016) {
  if (file.exists(f)) file.remove(f)
}

##############################################################################
# 10) 02 Resultados_Ayuntamientos_Sinaloa_2018_Casilla.xlsx
##############################################################################
df_2018 <- read_excel("02 Resultados_Ayuntamientos_Sinaloa_2018_Casilla.xlsx",
                      sheet="2018_SEE_AYUN_SIN_CAS", col_names=TRUE) %>%
  rename(section=SECCION, listanominal=LISTA, total=TOTAL) %>%
  filter(ESTATUS!="Paquete no entregado") %>%
  mutate(
    municipality = str_to_upper(MUNICIPIO)
  )

# aggregator merges => omitted
# partial coalition merges are done, skipping aggregator by municipality.

df_2018 <- df_2018 %>%
  mutate(
    # create PAN_PRD_MC_PAS by summation
    PAN_PRD_MC_PAS = PAN + PRD + MC + PAS + C_PAN_PRD_MC_PAS + C_PAN_PRD_MC + C_PAN_PRD_PAS + 
      C_PAN_MC_PAS + C_PRD_MC_PAS + C_PAN_PRD + C_PAN_MC + C_PAN_PAS + C_PRD_MC + 
      C_PRD_PAS + C_MC_PAS
  ) %>%
  select(-PAN, -PRD, -MC, -PAS, -C_PAN_PRD_MC_PAS, -C_PAN_PRD_MC, -C_PAN_PRD_PAS, -C_PAN_MC_PAS,
         -C_PRD_MC_PAS, -C_PAN_PRD, -C_PAN_MC, -C_PAN_PAS, -C_PRD_MC, -C_PRD_PAS, -C_MC_PAS)

df_2018 <- df_2018 %>%
  mutate(
    PT_MORENA_PES = ifelse(municipality!="CONCORDIA" & municipality!="ROSARIO" & municipality!="ESCUINAPA",
                           PT + MORENA + ES + C_PT_MORENA_ES + C_PT_MORENA + C_PT_ES + C_MORENA_ES,
                           NA_real_)
  ) %>%
  rename(PES=ES) %>%
  select(-PT, -MORENA, -ES, -C_PT_MORENA_ES, -C_PT_MORENA, -C_PT_ES, -C_MORENA_ES)

df_2018 <- df_2018 %>%
  mutate(
    # create PRI_PVEM_PANAL
    PRI_PVEM_PANAL = ifelse(municipality!="SAN IGNACIO" & municipality!="ROSARIO" & municipality!="ESCUINAPA",
                            PRI + PVEM + NA + CC_PRI_PVEM_NA + CC_PRI_PVEM + CC_PRI_NA + CC_PVEM_NA,
                            NA_real_),
    PANAL = NA_real_
  ) %>%
  rename(PANAL=NA) %>%
  select(-CC_PRI_PVEM_NA, -CC_PRI_PVEM, -CC_PRI_NA, -CC_PVEM_NA, -PRI, -PVEM)

df_2018 <- df_2018 %>%
  rename(
    CI_1 = CAND_IND1,
    CI_2 = CAND_IND2,
    CI_3 = CAND_IND3,
    CI_4 = CAND_IND4
  ) %>%
  mutate(
    uniqueid = case_when(
      municipality=="CHOIX" ~ 25007,
      # ...
      municipality=="ESCUINAPA" ~ 25009,
      TRUE~NA_real_
    )
  )

# aggregator => omitted
# rowranks => omitted
# winner => omitted
df_2018 <- df_2018 %>%
  mutate(
    year  = 2018,
    month = "July",
    STATE = "SINALOA"
  )


##############################################################################
# 11) Append Sinaloa_ALL.dta with 2016 + 2018 => Sinaloa_ALL_SALVADOR.dta
##############################################################################
df_all_sin <- read_dta("../../Precinct/Sinaloa_ALL.dta")


df_combo <- bind_rows(df_all_sin, df_2016, df_2018) %>%
  mutate(
    municipality = case_when(
      municipality=="EL ROSARIO" ~ "ROSARIO",
      uniqueid==25012           ~ "MAZATLAN",
      TRUE ~ municipality
    )
  )

data.table::fwrite(df_combo,"../../../Processed Data/sinaloa/Sinaloa_process_raw_data.csv")

