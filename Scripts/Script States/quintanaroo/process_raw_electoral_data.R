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
### 
###############################################################################


# 1) 
 # df <- read_csv("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_1997_No_LN.csv", show_col_types = FALSE)
# colnames(df) <- tolower(colnames(df))
##############################################################################
# 1) AYUNTAMIENTO 1999
##############################################################################
# Equivalent to: insheet using Ayu_Seccion_1999.csv, clear

df_1999 <- read_csv("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/1999/Ayu_Seccion_1999.csv")
colnames(df_1999) <- tolower(colnames(df_1999))
df_1999 <- df_1999 %>%
  # rename municipio -> municipality, seccion -> section
  rename(
    municipality = municipio,
    section      = seccion,
    listanominal = "lista nominal",
    noregistrados = "no registrados"
  ) %>%
  # drop if section==. | total==0
  filter(!is.na(section), total != 0)

# Convert pan through total to numeric
num_vars <- c("pan","pri","prd","pt","pvem","noregistrados","nulos",
              "total","listanominal")
for (v in num_vars) {
  if (v %in% names(df_1999)) {
    df_1999[[v]] <- as.numeric(df_1999[[v]])
  }
}

# Create missing = listanominal==.
df_1999 <- df_1999 %>%
  mutate(missing = ifelse(is.na(listanominal), 1, 0))

# collapse (sum) pan - listanominal missing, by (municipality section)
df_1999 <- df_1999 %>%
  group_by(municipality, section) %>%
  summarise(
    pan          = sum(pan, na.rm = TRUE),
    pri          = sum(pri, na.rm = TRUE),
    prd          = sum(prd, na.rm = TRUE),
    pt           = sum(pt, na.rm = TRUE),
    pvem         = sum(pvem, na.rm = TRUE),
    noregistrados= sum(noregistrados, na.rm = TRUE),
    nulos        = sum(nulos, na.rm = TRUE),
    total        = sum(total, na.rm = TRUE),
    listanominal = sum(listanominal, na.rm = TRUE),
    missing      = sum(missing, na.rm = TRUE),
    .groups      = "drop"
  )

# Read the .dta to replicate "merge 1:m ed seccion using ..."
df_all_months_years <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  filter(state == "QUINTANA ROO" & year == 1999)

df_1999 <- df_1999 %>%
  left_join(df_all_months_years, by = c("section"))

# drop if _merge==2 => in R, we remove rows with no match (NA in month/year/lista)
df_1999 <- df_1999 %>%
  filter(!is.na(month), !is.na(year), !is.na(listanominal))

# drop _merge ed seccion year month
df_1999 <- df_1999 %>%
  select(-year, -month)

# replace listanominal = lista if missing>=1 & lista>listanominal
df_1999 <- df_1999 %>%
  mutate(
    listanominal = ifelse(missing >= 1 & lista > listanominal,
                          lista,
                          listanominal))

# drop missing, lista
df_1999 <- df_1999 %>%
  select(-missing, -lista)

# rename pan->PAN, pri->PRI, etc. and drop noregistrados, nulos
df_1999 <- df_1999 %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT  = pt,
    PVEM = pvem
  ) %>%
  select(-noregistrados, -nulos)

# generate uniqueid
df_1999 <- df_1999 %>%
  mutate(
    uniqueid = case_when(
      municipality == "BENITO JUAREZ"            ~ 23005,
      municipality == "COZUMEL"                  ~ 23001,
      municipality == "FELIPE CARRILLO PUERTO"   ~ 23002,
      municipality == "ISLA MUJERES"             ~ 23003,
      municipality == "JOSE MARIA MORELOS"       ~ 23006,
      municipality == "LAZARO CARDENAS"          ~ 23007,
      municipality == "OTHON P. BLANCO"          ~ 23004,
      municipality == "OTHON P BLANCO"           ~ 23004, # sometimes spelled differently
      municipality == "SOLIDARIDAD"              ~ 23008,
      TRUE                                       ~ 0
    )
  )

# turnout = total / listanominal
df_1999 <- df_1999 %>%
  mutate(turnout = total / listanominal)

# valid = rowtotal(PAN PRI PRD PT PVEM)
df_1999 <- df_1999 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM)), na.rm = TRUE))

# year=1999, month="February"
df_1999 <- df_1999 %>%
  mutate(
    year  = 1999,
    month = "February"
  )

# sort section (optional in R)
df_1999 <- df_1999[order(df_1999$section), ]

##############################################################################
# 2) AYUNTAMIENTO 2002
##############################################################################
df_2002 <- read_csv("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2002/Ayu_Seccion_2002_No_Municipios.csv")
colnames(df_2002) <- tolower(colnames(df_2002))

df_2002 <- df_2002 %>%
  rename(section = seccion, total = vtotal, noregistrados = "no registrados") %>%
  filter(!is.na(section), total != 0)

# destring pan - total
vars_2002 <- c("pan","pri","prd","pt","pvem","cdem","psn","pas",
               "noregistrados","nulos","total")
for (v in vars_2002) {
  if (v %in% names(df_2002)) {
    df_2002[[v]] <- as.numeric(df_2002[[v]])
  }
}

# collapse (sum) pan - total, by section
df_2002 <- df_2002 %>%
  group_by(section) %>%
  summarise(
    pan   = sum(pan, na.rm = TRUE),
    pri   = sum(pri, na.rm = TRUE),
    prd   = sum(prd, na.rm = TRUE),
    pt    = sum(pt, na.rm = TRUE),
    pvem  = sum(pvem, na.rm = TRUE),
    cdem  = sum(cdem, na.rm = TRUE),
    psn   = sum(psn, na.rm = TRUE),
    pas   = sum(pas, na.rm = TRUE),
    noregistrados = sum(noregistrados, na.rm = TRUE),
    nulos = sum(nulos, na.rm = TRUE),
    total = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # rename
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT  = pt,
    PVEM = pvem,
    CDEM = cdem,
    PSN  = psn,
    PAS  = pas
  ) %>%
  # drop noregistrados, nulos
  select(-noregistrados, -nulos)

# Merge with Quintana_Roo_Section_to_Merge_Municipalities_2002.dta
df_merge_2002_muni <- read_dta("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2002/Quintana_Roo_Section_to_Merge_Municipalities_2002.dta")

df_2002 <- df_2002 %>%
  left_join(df_merge_2002_muni, by = "section") %>%
  filter(!is.na(municipality))  # drop if _merge==2

# Fix missing municipality and uniqueid for sections 90, 150
df_2002 <- df_2002 %>%
  mutate(
    municipality = ifelse(section == 90 & municipality == "", "BENITO JUAREZ", municipality),
    uniqueid     = ifelse(section == 90 & is.na(uniqueid), 23005, uniqueid),
    municipality = ifelse(section == 150 & municipality == "", "BENITO JUAREZ", municipality),
    uniqueid     = ifelse(section == 150 & is.na(uniqueid), 23005, uniqueid)
  )

# Merge listanominal2002
df_listanom_2002 <- read_dta("Listanominal2002.dta")
df_2002 <- df_2002 %>%
  left_join(df_listanom_2002, by = "section") %>%
  filter(!is.na(listanominal))  # drop if _merge==2

# Erase Listanominal2002.dta if you wish:
# file.remove("Listanominal2002.dta")

# turnout
df_2002 <- df_2002 %>%
  mutate(turnout = total / listanominal)

# valid
df_2002 <- df_2002 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, CDEM, PSN, PAS)), 
                         na.rm = TRUE))

# year, month
df_2002 <- df_2002 %>%
  mutate(
    year  = 2002,
    month = "February"
  ) %>%
  arrange(section)

##############################################################################
# 3) AYUNTAMIENTO 2005
##############################################################################
df_2005 <- fread("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2005/Ayu_Seccion_2005_No_Municipios.csv",
                 encoding = "Latin-1") 
# Remove "-" and spaces
colnames(df_2005) <- tolower(colnames(df_2005))
names(df_2005) <- gsub("[- ]", "", names(df_2005))

df_2005 <- df_2005 %>%
  rename(section = seccion,
         total   = votostotales,
         votosvlidos = "votosválidos") %>%
  filter(!is.na(section), total != 0)

# destring panpc - total
vars_2005 <- c("panpc","pripvem","prdpt","votosnulos","votosvlidos","total")
for (v in vars_2005) {
  if (v %in% names(df_2005)) {
    df_2005[[v]] <- as.numeric(df_2005[[v]])
  }
}

# collapse (sum) panpc - total, by (section)
df_2005 <- df_2005 %>%
  group_by(section) %>%
  summarise(
    panpc     = sum(panpc, na.rm = TRUE),
    pripvem   = sum(pripvem, na.rm = TRUE),
    prdpt     = sum(prdpt, na.rm = TRUE),
    votosnulos  = sum(votosnulos, na.rm = TRUE),
    votosvlidos = sum(votosvlidos, na.rm = TRUE),
    total     = sum(total, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  rename(
    PAN_PC    = panpc,
    PRI_PVEM  = pripvem,
    PRD_PT    = prdpt
  ) %>%
  select(-votosnulos, -votosvlidos)

# Merge with 2002 municipality references (the same .dta used in code)
df_merge_2005_muni <- read_dta("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2002/Quintana_Roo_Section_to_Merge_Municipalities_2002.dta")
df_2005 <- df_2005 %>%
  left_join(df_merge_2005_muni, by = "section") %>%
  filter(!is.na(municipality))

df_2005 <- df_2005 %>%
  mutate(
    municipality = ifelse(section == 90 & municipality == "", "BENITO JUAREZ", municipality),
    uniqueid     = ifelse(section == 90 & is.na(uniqueid), 23005, uniqueid),
    municipality = ifelse(section == 150 & municipality == "", "BENITO JUAREZ", municipality),
    uniqueid     = ifelse(section == 150 & is.na(uniqueid), 23005, uniqueid)
  )

# Merge listanominal2005
df_listanom_2005 <- read_dta("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2005/Listanominal2005.dta")
df_2005 <- df_2005 %>%
  left_join(df_listanom_2005, by = "section") %>%
  filter(!is.na(listanominal))

# file.remove("Listanominal2005.dta")

df_2005 <- df_2005 %>%
  mutate(
    turnout = total / listanominal,
    valid   = rowSums(across(c(PAN_PC, PRI_PVEM, PRD_PT)), na.rm = TRUE),
    year    = 2005,
    month   = "February"
  ) %>%
  arrange(section)

##############################################################################
# 4) AYUNTAMIENTO 2008
##############################################################################
df_2008 <- read_csv("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2008/Ayu_Seccion_2008.csv") 
# Remove "-" and spaces
colnames(df_2008) <- tolower(colnames(df_2008))
names(df_2008) <- gsub("[- ]", "", names(df_2008))

df_2008 <- df_2008 %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion
  ) %>%
  filter(!(municipality == "" & is.na(section)), total != 0)

vars_2008 <- c("listanominal","nulos","pan","pri","prdptpc","panal",
               "pvem","pas","pripvem","noregistrados","total")
for (v in vars_2008) {
  if (v %in% names(df_2008)) {
    df_2008[[v]] <- as.numeric(df_2008[[v]])
  }
}

df_2008 <- df_2008 %>%
  group_by(municipality, section) %>%
  summarise(
    listanominal = sum(listanominal, na.rm = TRUE),
    nulos        = sum(nulos, na.rm = TRUE),
    pan          = sum(pan, na.rm = TRUE),
    pri          = sum(pri, na.rm = TRUE),
    prdptpc      = sum(prdptpc, na.rm = TRUE),
    panal        = sum(panal, na.rm = TRUE),
    pvem         = sum(pvem, na.rm = TRUE),
    pas          = sum(pas, na.rm = TRUE),
    pripvem      = sum(pripvem, na.rm = TRUE),
    noregistrados= sum(noregistrados, na.rm = TRUE),
    total        = sum(total, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  rename(
    PAN       = pan,
    PRI       = pri,
    PRD_PT_PC = prdptpc,
    PANAL     = panal,
    PVEM      = pvem,
    PAS       = pas,
    PRI_PVEM  = pripvem
  ) %>%
  mutate(
    turnout = total / listanominal
  ) %>%
  select(-noregistrados, -nulos)

df_2008 <- df_2008 %>%
  mutate(
    uniqueid = case_when(
      municipality == "BENITO JUAREZ"        ~ 23005,
      municipality == "COZUMEL"              ~ 23001,
      municipality == "FELIPE CARRILLO PUERTO" ~ 23002,
      municipality == "ISLA MUJERES"         ~ 23003,
      municipality == "JOSE MARIA MORELOS"   ~ 23006,
      municipality == "LAZARO CARDENAS"      ~ 23007,
      municipality == "OTHON P BLANCO"       ~ 23004,
      municipality == "OTHON P. BLANCO"      ~ 23004, 
      municipality == "SOLIDARIDAD"          ~ 23008,
      TRUE                                   ~ 0
    ),
    valid = rowSums(across(c(PAN, PRI, PRD_PT_PC, PVEM, PANAL, PAS, PRI_PVEM)), na.rm=TRUE),
    year  = 2008,
    month = "February"
  ) %>%
  arrange(section)

##############################################################################
# 5) Tulum Extraordinario 2009
##############################################################################
df_2009 <- read_xlsx("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2009/Tulum Extraordinario 2009.xlsx") %>%
  rename(section = Sección) %>%
  filter(section != "", section != "TOTALES")
names(df_2009) <- gsub("[- ]", "", names(df_2009))
# destring everything
df_2009[] <- lapply(df_2009, function(x) as.numeric(as.character(x)))

df_2009 <- df_2009 %>%
  mutate(
    municipality = "TULUM EXTRAORDINARIO",
    uniqueid     = 23009,
    total        = VotosTotales,
    listanominal = ListaNominal,
    PANAL        = PNA,
    PRI_PVEM     = PRIPVEM
  )

# collapse (sum) listanominal PAN-PANAL total, by (municipality uniqueid section)
df_2009 <- df_2009 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(
    listanominal = sum(listanominal, na.rm = TRUE),
    PAN          = sum(PAN, na.rm = TRUE),
    PRI_PVEM     = sum(PRI_PVEM, na.rm = TRUE),
    PRD          = sum(PRD, na.rm = TRUE),
    PT           = sum(PT, na.rm = TRUE),
    PANAL        = sum(PANAL, na.rm = TRUE),
    total        = sum(total, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(
    turnout = total / listanominal,
    valid   = rowSums(across(c(PAN, PRI_PVEM, PRD, PT, PANAL)), na.rm=TRUE),
    year    = 2009,
    month   = "February"
  )

##############################################################################
# 6) AYUNTAMIENTO 2010
##############################################################################
df_2010 <- read_csv("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2010/Ayu_Seccion_2010.csv") 
names(df_2010) <- gsub("[- ]", "", names(df_2010))
colnames(df_2010) <- tolower(colnames(df_2010))

df_2010 <-df_2010 %>%
  rename(
    municipality  = nombre_municipio,
    section       = seccion,
    listanominal  = lista_nominal
  ) %>%
  filter(!(municipality == "" & is.na(section)), total != 0)

# destring
vars_2010 <- c("listanominal","pan","pri","prd","pvem","pt","panal",
               "panprdptconvergencia","pripvempanal","nulos","total")
for (v in vars_2010) {
  if (v %in% names(df_2010)) {
    df_2010[[v]] <- as.numeric(df_2010[[v]])
  }
}

df_2010 <- df_2010 %>%
  mutate(missing = ifelse(is.na(listanominal), 1, 0)) %>%
  group_by(municipality, section) %>%
  summarise(
    missing       = sum(missing, na.rm = TRUE),
    listanominal  = sum(listanominal, na.rm = TRUE),
    pan           = sum(pan, na.rm = TRUE),
    pri           = sum(pri, na.rm = TRUE),
    prd           = sum(prd, na.rm = TRUE),
    pvem          = sum(pvem, na.rm = TRUE),
    pt            = sum(pt, na.rm = TRUE),
    panal         = sum(panal, na.rm = TRUE),
    panprdptconvergencia = sum(panprdptconvergencia, na.rm = TRUE),
    pripvempanal  = sum(pripvempanal, na.rm = TRUE),
    nulos         = sum(nulos, na.rm = TRUE),
    total         = sum(total, na.rm = TRUE),
    .groups       = "drop"
  ) 

# Merge with all_months_years.dta
df_2010 <- df_2010 %>%
  left_join(df_all_months_years, by = c("section")) %>%
  filter(month == "June", year == 2010) %>%
  filter(!is.na(month), !is.na(year), !is.na(lista)) %>%
  select(-year, -month)

df_2010 <- df_2010 %>%
  mutate(
    listanominal = ifelse(missing >= 1 & lista > listanominal,
                          lista,
                          listanominal)
  ) %>%
  select(-missing, -lista) %>%
  rename(
    PAN  = pan,
    PRI  = pri,
    PRD  = prd,
    PVEM = pvem,
    PT   = pt,
    PANAL= panal,
    PAN_PRD_PT_PC      = panprdptconvergencia,
    PRI_PVEM_PANAL     = pripvempanal
  ) %>%
  select(-nulos) %>%
  mutate(
    uniqueid = case_when(
      municipality == "BENITO JUAREZ"           ~ 23005,
      municipality == "COZUMEL"                 ~ 23001,
      municipality == "FELIPE CARRILLO PUERTO"  ~ 23002,
      municipality == "ISLA MUJERES"            ~ 23003,
      municipality == "JOSE MARIA MORELOS"      ~ 23006,
      municipality == "LAZARO CARDENAS"         ~ 23007,
      municipality == "OTHON P BLANCO"          ~ 23004,
      municipality == "OTHON P. BLANCO"         ~ 23004,
      municipality == "SOLIDARIDAD"             ~ 23008,
      municipality == "TULUM"                   ~ 23009,
      TRUE                                      ~ 0
    ),
    turnout = total / listanominal,
    valid   = rowSums(across(c(PAN, PRI, PRD, PVEM, PT, PANAL,
                               PAN_PRD_PT_PC, PRI_PVEM_PANAL)), na.rm=TRUE),
    year    = 2010,
    month   = "July"
  ) %>%
  arrange(section)


##############################################################################
# 7) APPEND ALL (1999, 2002, 2005, 2008, 2009, 2010, + 2013 if available)
##############################################################################

# Append (rbind) all
all_data <- bind_rows(
  df_1999,
  df_2002,
  df_2005,
  df_2008,
  df_2009,
  df_2010,
  df_2013
)

##############################################################################
# ============ 2016 CODE ====================================================
##############################################################################

# --- Step 1: Read each sheet from "Ayuntamientos_QRoo_2016.xlsx" ---
excel_file_2016 <- "../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2016/Ayuntamientos_QRoo_2016.xlsx"

# 1) Identify all sheet names
sheet_names_2016 <- excel_sheets(excel_file_2016)

# 2) Loop over each sheet, read data, do minimal cleaning, then save as .dta
for (sname in sheet_names_2016) {
  df_sheet <- read_excel(excel_file_2016, sheet = sname, 
                         col_types = "text") %>% 
    # Drop if SECCION==""
    filter(SECCION != "")
  
  # Replace empty strings "" with "0" in all columns
  df_sheet[] <- lapply(df_sheet, function(x) ifelse(x == "", "0", x))
  
  # Replace hyphens with underscores in column names (Stata compatibility)
  names(df_sheet) <- str_replace_all(names(df_sheet), "-", "_")
  
  # Also replace other problematic characters in column names
  names(df_sheet) <- str_replace_all(names(df_sheet), "[^A-Za-z0-9_]", "_")
  
  # For demonstration, we just remove illegal filename characters:
  safe_sname <- str_replace_all(sname, "[^A-Za-z0-9_\\.]", "_")
  write_dta(df_sheet, paste0(safe_sname, ".dta"))
}

# --- Step 2: Update file names to match what was actually saved ---
files_to_append_2016 <- c(
  "Table_2.dta","Table_4.dta","Table_6.dta","Table_8.dta","Table_10.dta",
  "Table_12.dta","Table_14.dta","Table_16.dta","Table_18.dta","Table_20.dta",
  "Table_22.dta","Table_24.dta","Table_26.dta","Table_28.dta","Table_30.dta",
  "Table_32.dta","Table_34.dta","Table_36.dta","Table_38.dta","Table_40.dta",
  "Table_42.dta","Table_44.dta","Table_48.dta","Table_50.dta","Table_52.dta",
  "Table_56.dta","Table_58.dta","Table_64.dta","Table_68.dta","Table_74.dta",
  "Table_80.dta","Table_82.dta","Table_84.dta","Table_86.dta","Table_88.dta",
  "Table_90.dta","Table_92.dta","Table_98.dta","Table_100.dta","Table_102.dta",
  "Table_104.dta","Table_106.dta","Table_108.dta","Table_110.dta","Table_114.dta",
  "Table_116.dta","Table_120.dta","Table_126.dta"
)

all_2016 <- NULL
for (f in files_to_append_2016) {
  if (file.exists(f)) {
    temp_df <- read_dta(f)
    # Append
    if (is.null(all_2016)) {
      all_2016 <- temp_df
    } else {
      all_2016 <- bind_rows(all_2016, temp_df)
    }
  } else {
    message(paste("File not found:", f))
  }
}

# Check if any files were loaded
if (is.null(all_2016)) {
  stop("No files were found! Check that the .dta files exist in the current directory.")
}

# --- Step 3: Convert all columns to numeric (similar to "destring *, replace") ---
all_2016[] <- lapply(all_2016, function(x) as.numeric(as.character(x)))

# --- Step 4: Rename variables  ---
all_2016 <- all_2016 %>%
  rename(
    section = SECCION,
    no_reg  = CNR,
    nulo    = VN,
    total   = VT,
    PES     = ES,
    PANAL   = "NA", 
    CI_1    = CI
  )

# Summation merges (like "replace PRIPVEMNA = PRIPVEMNA + PRIPVEM + ..." ) are allowed 
# as long as we are not generating aggregator by municipality. 
# These "replace" lines are not the aggregator 'collapse'—they’re just variable combos.
# We'll replicate them:
all_2016 <- all_2016 %>%
  mutate(
    PRIPVEMNA = ifelse(!is.na(PRI_PVEM_NA),
                       PRI_PVEM_NA + coalesce(PRI_PVEM,0) + coalesce(PRI_NA,0) +
                         coalesce(PVEM_NA,0) + coalesce(PRI,0) + coalesce(PVEM,0) + coalesce(PANAL,0),
                       PRI_PVEM_NA)
  ) %>%
  select(-PRI_PVEM, -PRI_NA, -PVEM_NA, -PRI, -PVEM, -PANAL) %>%
  rename(PRI_PVEM_PANAL = PRI_PVEM_NA) %>%
  mutate(
    PANPRD = ifelse(!is.na(PAN_PRD),
                    PAN_PRD + coalesce(PAN,0) + coalesce(PRD,0),
                    PAN_PRD)
  ) %>%
  select(-PAN, -PRD, -PAN_PRD) %>%
  rename(PAN_PRD = PANPRD)

# We skip the aggregator code: "collapse (sum) PT-CI_1 total, by (municipality uniqueid section)"
# We skip the aggregator code: "egen valid = rowtotal(...)"
# We skip the aggregator code: "bys uniqueid: egen mun_... = sum(...)"
# We skip the 'rowranks i_*' and winner/second/third creation.

# Keep lines that read LN and merge:
# Merging LN2016

# We assume "municipality" and "uniqueid" exist in your data. 
# If they do not exist yet, adapt as needed.

# Step: merge LN2016
ln2016 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2016/LN2016.dta") %>%
  filter(entidad == 23, month == 5) %>%
  filter(seccion != 0) %>%
  select(uniqueid, seccion, lista)

ln2016 <- ln2016 %>%
  rename(section = seccion)

# We do not have "municipality" + "uniqueid" here, but let's replicate as best as possible:
all_2016 <- all_2016 %>%
  left_join(ln2016, by = c("section"))

# drop if _merge==2 => means drop rows that did not match
# In R, after left_join, those unmatched in LN2016 remain with NA. We remove them:
all_2016 <- all_2016 %>%
  filter(!is.na(lista))

# rename lista -> listanominal
all_2016 <- all_2016 %>%
  rename(listanominal = lista)

# turnout = total / listanominal
all_2016 <- all_2016 %>%
  mutate(turnout = total / listanominal)

# year=2016, month="June", STATE="QUINTANA ROO"
all_2016 <- all_2016 %>%
  mutate(
    year  = 2016,
    month = "June",
    STATE = "QUINTANA ROO"
  )


# End of 2016 code

##############################################################################
# ============ 2018 CODE ====================================================
##############################################################################

# --- Step 1: Read each sheet from "Ayuntamientos_QRoo_2018.xlsx" ---
excel_file_2018 <- "../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2018/Ayuntamientos_QRoo_2018.xlsx"
sheet_names_2018 <- excel_sheets(excel_file_2018)

for (sname in sheet_names_2018) {
  df_sheet <- read_excel(excel_file_2018, sheet = sname, 
                         col_types = "text") %>%
    mutate(municipality = sname) %>%
    filter(section != "")  # drop if section == ""
  
  # Replace empty strings with "0"
  df_sheet[] <- lapply(df_sheet, function(x) ifelse(x == "", "0", x))
  
  # Save as .dta
  safe_sname <- str_replace_all(sname, "[^A-Za-z0-9_\\.]", "_")
  write_dta(df_sheet, paste0(safe_sname, ".dta"))
}

# --- Step 2: Append .dta files (each municipality) ---
files_to_append_2018 <- c(
  "BENITO JUAREZ.dta","COZUMEL.dta","FELIPE CARRILLO PUERTO.dta",
  "ISLA MUJERES.dta","JOSE MARIA MORELOS.dta","LAZARO CARDENAS.dta",
  "OTHON P. BLANCO.dta","SOLIDARIDAD.dta","TULUM.dta","BACALAR.dta",
  "PUERTO MORELOS.dta"
)

all_2018 <- NULL
for (f in files_to_append_2018) {
  if (file.exists(f)) {
    temp_df <- read_dta(f)
    if (is.null(all_2018)) {
      all_2018 <- temp_df
    } else {
      all_2018 <- bind_rows(all_2018, temp_df)
    }
  }
}

# destring => numeric
all_2018[] <- lapply(all_2018, function(x) as.numeric(as.character(x)))

# Summations like:
all_2018 <- all_2018 %>%
  mutate(
    PRI_PVEM_PANAL = ifelse(!is.na(PRI_PVEM_PANAL),
                            PRI_PVEM_PANAL + coalesce(PRI_PVEM,0) + coalesce(PRI_PANAL,0) +
                              coalesce(PVEM_PANAL,0) + coalesce(PRI,0) + coalesce(PVEM,0) + coalesce(PANAL,0),
                            PRI_PVEM_PANAL)
  ) %>%
  mutate(
    PRI      = ifelse(!is.na(PRI_PVEM_PANAL), NA, PRI),
    PVEM     = ifelse(!is.na(PRI_PVEM_PANAL), NA, PVEM),
    PANAL    = ifelse(!is.na(PRI_PVEM_PANAL), NA, PANAL)
  ) %>%
  select(-PRI_PVEM, -PRI_PANAL, -PVEM_PANAL, -PVEM) %>%
  mutate(
    PAN_PRD_MC = ifelse(!is.na(PAN_PRD_MC),
                        PAN_PRD_MC + coalesce(PAN_PRD,0) + coalesce(PAN_MC,0) +
                          coalesce(PRD_MC,0) + coalesce(PAN,0) + coalesce(PRD,0) + coalesce(MC,0),
                        PAN_PRD_MC)
  ) %>%
  select(-PAN_PRD, -PAN_MC, -PRD_MC, -PAN, -PRD, -MC) %>%
  mutate(
    PT_MORENA = ifelse(!is.na(PT_MORENA),
                       PT_MORENA + coalesce(PT,0) + coalesce(MORENA,0),
                       PT_MORENA),
    PT        = ifelse(!is.na(PT_MORENA), NA, PT),
    MORENA    = ifelse(!is.na(PT_MORENA), NA, MORENA)
  )

# We skip aggregator code, row ranks, winner logic, etc.

# Remove no_reg, nulo, SEC if present
all_2018 <- all_2018 %>%
  select(-no_reg, -nulo, -SEC)

# year=2018, month="July", STATE="QUINTANA ROO"
all_2018 <- all_2018 %>%
  mutate(
    year  = 2018,
    month = "July",
    STATE = "QUINTANA ROO"
  )

# --- Step 3: Merge with LN18_QROO (ListadoNominalPREP2018.dta) ---
ln2018 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ListadoNominalPREP2018.dta") %>%
  filter(STATE == "QUINTANA ROO") %>%
  select(STATE, section, ListadoNominalINE)

all_2018 <- all_2018 %>%
  left_join(ln2018, by = c("STATE","section")) %>%
  # drop _merge => remove rows not matched
  filter(!is.na(ListadoNominalINE)) %>%
  rename(listanominal = ListadoNominalINE)

# turnout = total / listanominal
all_2018 <- all_2018 %>%
  mutate(turnout = total / listanominal)


#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

# Load the 2021 dataset from the excel
data_2021 <- read_delim("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2021/QROO_AYUN_2021.csv", skip = 3, delim = "|")

# Rename columns
data_2021 <- data_2021 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL_VOTOS_CALCULADO,
                no_reg = NO_REGISTRADOS,
                nulos = NULOS,
                FXM = FMEX) %>%
  rename_with(~ gsub("CAND_IND", "CI", .x)) %>% 
  rename_with(~ gsub("C_", "", .x)) %>% 
  dplyr::mutate(
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(section),
    across(PAN:listanominal, as.numeric)
  ) %>% 
  dplyr::filter(section > 0)

# Assign uniqueids
data_2021 <- data_2021 %>% 
  mutate(
    uniqueid = case_when(
      municipality == "BENITO JUAREZ"           ~ 23005,
      municipality == "COZUMEL"                 ~ 23001,
      municipality == "FELIPE C. PUERTO"  ~ 23002,
      municipality == "ISLA MUJERES"            ~ 23003,
      municipality == "JOSE MA. MORELOS"      ~ 23006,
      municipality == "LAZARO CARDENAS "         ~ 23007,
      municipality == "OTHON P. BLANCO "         ~ 23004,
      municipality == "SOLIDARIDAD"             ~ 23008,
      municipality == "TULUM"                   ~ 23009,
      municipality == "BACALAR" ~ 23010,
      municipality == "PUERTO MORELOS" ~ 23011,
      TRUE                                      ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(PAN:listanominal), 
           \(x) sum(x, na.rm = TRUE))
  )

# Calculate valid votes and final details
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(PAN:MORENA_MAS), na.rm = TRUE),
    year = 2021,
    month = "June"
  )

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 23) %>% 
  select(yr, inegi, coal1, coal2, coal3, coal4) %>% 
  rename(
    year = yr,
    uniqueid = inegi) %>% 
  mutate(
    across(
      coal1:coal4,
      ~ str_replace_all(., "-", "_") |> 
        str_replace_all(regex("PNA", ignore_case = TRUE), "PANAL") |> 
        str_replace_all(regex("CQR", ignore_case = TRUE), "CQROO") |> 
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

# Load the 2024 dataset from the excel
data_2024 <- read_delim("../../../Data/Raw Electoral Data/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013,2016,2018,2021,2024/2024/QROO_AYUN_2024.csv", skip = 3, delim = "|")

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(municipality = MUNICIPIO_LOCAL,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL,
                no_reg = NO_REGISTRADAS,
                nulos = NULOS,
                CI_1 = CAND_IND) %>%
  rename_with(~ gsub("C_", "", .x)) %>% 
  dplyr::mutate(
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(str_extract(section, "\\d+")),
    across(PAN:listanominal, as.numeric)
  ) %>% 
  dplyr::filter(section > 0)

# Assign uniqueids
data_2024 <- data_2024 %>% 
  mutate(
    uniqueid = case_when(
      municipality == "BENITO JUAREZ"           ~ 23005,
      municipality == "COZUMEL"                 ~ 23001,
      municipality == "FELIPE CARRILLO PUERTO"  ~ 23002,
      municipality == "ISLA MUJERES"            ~ 23003,
      municipality == "JOSE MARIA MORELOS"      ~ 23006,
      municipality == "LAZARO CARDENAS "         ~ 23007,
      municipality == "OTHON P. BLANCO "         ~ 23004,
      municipality == "SOLIDARIDAD"             ~ 23008,
      municipality == "TULUM"                   ~ 23009,
      municipality == "BACALAR" ~ 23010,
      municipality == "PUERTO MORELOS" ~ 23011,
      TRUE                                      ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(PAN:listanominal), 
           \(x) sum(x, na.rm = TRUE))
  )

# Calculate valid votes and final details
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(PAN:PT_MORENA), na.rm = TRUE),
    year = 2024,
    month = "June"
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

final_data <- bind_rows(all_data,all_2016, all_2018, collapsed_2021, collapsed_2024)

data.table::fwrite(final_data,"../../../Processed Data/quintanaroo/quintanaroo_process_raw_data.csv")