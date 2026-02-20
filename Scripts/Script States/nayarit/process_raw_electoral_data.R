###############################################################################
### process_raw_electoral_data_Nayarit.R
### Full pipeline: NAYARIT municipal election data processing
### Election years: 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017
### Source: Stata do-files (JOHN + SALVADOR) translated to R
### Self-contained pipeline: load -> clean -> harmonize -> collapse -> output
### Data path: state_folder/YYYY/filename (year-based subfolder structure)
###############################################################################

rm(list = ls())
cat("\014")
options(max.print = 5000, scipen = 10)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, haven, readstata13, readxl, tidyverse, tidyr,
               openxlsx, data.table)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(file.path(script_dir, ""))

# --- PATH CONFIGURATION ---
raw_data_path <- "../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017,2021,2024"
ln_path       <- "../../../Data/Raw Electoral Data/Listas Nominales"
processed_path <- "../../../Processed Data/nayarit"

STATE_NAME <- "NAYARIT"
state_ed   <- 18  # INEGI state code

<<<<<<< HEAD
# Helper function: remove accents
remove_accents <- function(x) {
  x <- gsub("\u00c1|\u00e1", "A", x)
  x <- gsub("\u00c9|\u00e9", "E", x)
  x <- gsub("\u00cd|\u00ed", "I", x)
  x <- gsub("\u00d3|\u00f3", "O", x)
  x <- gsub("\u00da|\u00fa", "U", x)
  x <- gsub("\u00dc|\u00fc", "U", x)
  x <- gsub("\u00d1|\u00f1", "N", x)
  return(x)
=======
# drop if municipality=="" & section==.
df1996 <- subset(df1996, !(municipality == "" & is.na(section)))

df1996 <- df1996 %>% 
  rename(
    parm = parmen,
    cd = "c.d.",
    noregistrados = "no.registrados"
  )

# create total = rowtotal(pan pri prd pt pvem prs parm cd noregistrados nulos)
df1996$total <- rowSums(df1996[, c("pan","pri","prd","pt","pvem","prs","parm","cd","noregistrados","nulos")],
                        na.rm = TRUE)

# Convert relevant columns to numeric (similar to destring)
num_vars_96 <- c("pan","pri","prd","pt","pvem","prs","parm","cd","noregistrados","nulos","total")
for(v in num_vars_96){
  if(v %in% colnames(df1996)){
    df1996[[v]] <- as.numeric(df1996[[v]])
  }
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
}

cat(paste0("=========================================\n"))
cat(paste0("Processing ", STATE_NAME, " electoral data\n"))
cat(paste0("Election years: 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017\n"))
cat(paste0("=========================================\n"))

<<<<<<< HEAD
# Load all_months_years LN data (used by multiple years)
ln_data <- tryCatch(
  read_dta(file.path(ln_path, "all_months_years.dta")),
  error = function(e) {
    cat("WARNING: Could not load all_months_years.dta\n")
    data.frame()
=======
# rename lower-case columns to final uppercase
names(df1996)[names(df1996) == "pan"]  <- "PAN"
names(df1996)[names(df1996) == "pri"]  <- "PRI"
names(df1996)[names(df1996) == "prd"]  <- "PRD"
names(df1996)[names(df1996) == "pt"]   <- "PT"
names(df1996)[names(df1996) == "pvem"] <- "PVEM"
names(df1996)[names(df1996) == "prs"]  <- "PRS"
names(df1996)[names(df1996) == "parm"] <- "PARM"
names(df1996)[names(df1996) == "cd"]   <- "PCD"

# drop noregistrados, nulos
df1996$noregistrados <- NULL
df1996$nulos         <- NULL

# generate uniqueid
df1996$uniqueid <- 0
df1996$uniqueid[df1996$municipality == "ACAPONETA"]          <- 18001
df1996$uniqueid[df1996$municipality == "AHUACATLAN"]         <- 18002
df1996$uniqueid[df1996$municipality == "AMATLAN DE CANAS"]   <- 18003
df1996$uniqueid[df1996$municipality == "BAHIA DE BANDERAS"]  <- 18020
df1996$uniqueid[df1996$municipality == "COMPOSTELA"]         <- 18004
df1996$uniqueid[df1996$municipality == "EL NAYAR"]           <- 18009
df1996$uniqueid[df1996$municipality == "HUAJICORI"]          <- 18005
df1996$uniqueid[df1996$municipality == "IXTLAN DEL RIO"]     <- 18006
df1996$uniqueid[df1996$municipality == "JALA"]               <- 18007
df1996$uniqueid[df1996$municipality == "LA YESCA"]           <- 18019
df1996$uniqueid[df1996$municipality == "ROSAMORADA"]         <- 18010
df1996$uniqueid[df1996$municipality == "RUIZ"]               <- 18011
df1996$uniqueid[df1996$municipality == "SAN BLAS"]           <- 18012
df1996$uniqueid[df1996$municipality == "SAN PEDRO LAGUNILLAS"] <- 18013
df1996$uniqueid[df1996$municipality == "SANTA MARIA DEL ORO"]  <- 18014
df1996$uniqueid[df1996$municipality == "SANTIAGO IXCUINTLA"]   <- 18015
df1996$uniqueid[df1996$municipality == "TECUALA"]            <- 18016
df1996$uniqueid[df1996$municipality == "TEPIC"]              <- 18017
df1996$uniqueid[df1996$municipality == "TUXPAN"]             <- 18018
df1996$uniqueid[df1996$municipality == "XALISCO"]            <- 18008

# generate valid
df1996$valid <- rowSums(df1996[, c("PAN","PRI","PRD","PT","PVEM","PRS","PARM","PCD")],
                        na.rm = TRUE)

# (Omit municipal aggregator, rowranks, winner lines)

# add year/month
df1996$year  <- 1996
df1996$month <- "July"

# We can save to an intermediate file or keep it in memory. Let's keep in memory.

# Keep a small subset for merging with 1999
df1996_merge <- df1996[, c("municipality","section","uniqueid")]
df1996_merge <- df1996_merge[order(df1996_merge$section), ]

# Also, drop if total == NA or total==0 
df1996 <- subset(df1996, !is.na(total) & total != 0)

###############################################################################
## 3) 1999 DATA
###############################################################################
df1999 <- read.csv("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017,2021,2024/Ayu_Seccion_1999_No_LN.csv", stringsAsFactors = FALSE)

# Remove upper, "-" and spaces

colnames(df1999) <- tolower(colnames(df1999))
names(df1999) <- gsub("[. ]", "", names(df1999))

names(df1999)[names(df1999) == "nomunicipio"] <- "municipality number"
names(df1999)[names(df1999) == "seccion"]     <- "section"

df1999 <- df1999 %>% 
  mutate(section = as.numeric(section))

df1999 <- subset(df1999, !(is.na("municipality number") & is.na(section)))

df1999$total <- rowSums(df1999[, c("pri","pvem","medp","parmen","pps","cac","candnoreg","votosnulos")],
                        na.rm = TRUE)
df1999 <- subset(df1999, !is.na(total) & total != 0)

num_vars_99 <- c("pri","pvem","medp","parmen","pps","cac","candnoreg","votosnulos","total")
for(v in num_vars_99){
  if(v %in% colnames(df1999)){
    df1999[[v]] <- as.numeric(df1999[[v]])
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  }
)

# =============================================================================
# MUNICIPALITY -> UNIQUEID MAPPING
# All name variants across all election years
# =============================================================================
assign_uniqueid <- function(municipality) {
  dplyr::case_when(
    municipality == "ACAPONETA" ~ 18001,
    municipality == "AHUACATLAN" ~ 18002,
    municipality == "AHUACATLÃN" ~ 18002,
    municipality == "AMATLAN DE CA?AS" ~ 18003,
    municipality == "AMATLAN DE CANAS" ~ 18003,
    municipality == "AMATLAN DE CAÃ‘AS" ~ 18003,
    municipality == "AMATLÃN DE CAÃ‘AS" ~ 18003,
    municipality == "COMPOSTELA" ~ 18004,
    municipality == "HUAJICORI" ~ 18005,
    municipality == "IXTLAN DEL RIO" ~ 18006,
    municipality == "IXTLÃN DEL RÃO" ~ 18006,
    municipality == "JALA" ~ 18007,
    municipality == "XALISCO" ~ 18008,
    municipality == "DEL NAYAR" ~ 18009,
    municipality == "EL NAYAR" ~ 18009,
    municipality == "ROSAMORADA" ~ 18010,
    municipality == "RUIZ" ~ 18011,
    municipality == "RUÃZ" ~ 18011,
    municipality == "SAN BLAS" ~ 18012,
    municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
    municipality == "SANTA MARIA DEL ORO" ~ 18014,
    municipality == "SANTIAGO IXCUINTLA" ~ 18015,
    municipality == "TECUALA" ~ 18016,
    municipality == "TEPIC" ~ 18017,
    municipality == "TUXPAN" ~ 18018,
    municipality == "LA YESCA" ~ 18019,
    municipality == "BAHIA DE BANDERAS" ~ 18020,
    municipality == "BAHÃA DE BANDERAS" ~ 18020,
    TRUE ~ NA_real_
  )
}

###############################################################################
### PROCESSING 1996 ELECTION
###############################################################################
cat("Processing 1996...\n")

data_1996 <- read.csv(file.path(raw_data_path, "1996", "Ayu_Seccion_1996_No_LN.csv"),
                      stringsAsFactors = FALSE)

data_1996 <- data_1996 %>%
  rename(municipality = municipio)

data_1996 <- data_1996 %>%
  filter(!(municipality == "" & is.na(section)))
data_1996 <- data_1996 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_1996 <- setdiff(names(data_1996), c("municipality", "section"))
data_1996 <- data_1996 %>%
  mutate(across(all_of(party_cols_1996), ~suppressWarnings(as.numeric(as.character(.)))))

data_1996$municipality <- toupper(trimws(data_1996$municipality))
data_1996$municipality <- remove_accents(data_1996$municipality)

# --- Collapse by municipality section ---
data_1996 <- data_1996 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Compute total ---
data_1996 <- data_1996 %>%
  mutate(total = rowSums(across(c(pan, pri, prd, pt, pvem, prs, parm, cd, noregistrados, nulos)), na.rm = TRUE))
data_1996 <- data_1996 %>% filter(total > 0)

# --- Rename to standard party names ---
data_1996 <- data_1996 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PRS = prs, PARM = parm, PCD = cd)

<<<<<<< HEAD
data_1996 <- data_1996 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_1996 <- data_1996 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))
=======
df1999 <- subset(df1999, !(is.na(df1999$uniqueid) & is.na(df1999$"municipality number")))
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# --- Assign uniqueid ---
data_1996 <- data_1996 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_1996 <- data_1996 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PRS, PARM, PCD)), na.rm = TRUE))

data_1996 <- data_1996 %>%
  mutate(year = 1996, month = "July")

collapsed_1996 <- data_1996
cat(paste0("  1996: ", nrow(collapsed_1996), " obs\n"))

###############################################################################
### PROCESSING 1999 ELECTION
###############################################################################
cat("Processing 1999...\n")

data_1999 <- read.csv(file.path(raw_data_path, "1999", "Ayu_Seccion_1999_No_LN.csv"),
                      stringsAsFactors = FALSE)

data_1999 <- data_1999 %>%
  rename(municipality = nomunicipio)

data_1999 <- data_1999 %>%
  filter(!(municipality == "" & is.na(section)))
data_1999 <- data_1999 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_1999 <- setdiff(names(data_1999), c("municipality", "section"))
data_1999 <- data_1999 %>%
  mutate(across(all_of(party_cols_1999), ~suppressWarnings(as.numeric(as.character(.)))))

data_1999$municipality <- toupper(trimws(data_1999$municipality))
data_1999$municipality <- remove_accents(data_1999$municipality)

# --- Collapse by municipality section ---
data_1999 <- data_1999 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Compute total ---
data_1999 <- data_1999 %>%
  mutate(total = rowSums(across(c(pri, pvem, medp, parmen, pps, cac, candnoreg, votosnulos)), na.rm = TRUE))
data_1999 <- data_1999 %>% filter(total > 0)

# --- Rename to standard party names ---
data_1999 <- data_1999 %>%
  rename(PAN_PRD_PT = cac, PRI = pri, PVEM = pvem, PMEP = medp, PARM = parm, PPS = pps)

# --- Lista nominal from all_months_years ---
ln_1999 <- ln_data %>%
  filter(ed == state_ed, month == 6, year == 1999) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

data_1999 <- data_1999 %>%
  left_join(ln_1999, by = "section")

data_1999 <- data_1999 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_1999 <- data_1999 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_1999 <- data_1999 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_1999 <- data_1999 %>%
  mutate(valid = rowSums(across(c(PRI, PVEM, PMEP, PARM, PPS, PAN_PRD_PT)), na.rm = TRUE))

data_1999 <- data_1999 %>%
  mutate(year = 1999, month = "July")

collapsed_1999 <- data_1999
cat(paste0("  1999: ", nrow(collapsed_1999), " obs\n"))

###############################################################################
### PROCESSING 2002 ELECTION
###############################################################################
cat("Processing 2002...\n")

data_2002 <- read.csv(file.path(raw_data_path, "2002", "Ayu_Seccion_2002_No_LN.csv"),
                      stringsAsFactors = FALSE)

data_2002 <- data_2002 %>%
  rename(municipality = nombre_municipio)

data_2002 <- data_2002 %>%
  filter(!(municipality == "" & is.na(section)))
data_2002 <- data_2002 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2002 <- setdiff(names(data_2002), c("municipality", "section"))
data_2002 <- data_2002 %>%
  mutate(across(all_of(party_cols_2002), ~suppressWarnings(as.numeric(as.character(.)))))

data_2002$municipality <- toupper(trimws(data_2002$municipality))
data_2002$municipality <- remove_accents(data_2002$municipality)

# --- Collapse by municipality section ---
data_2002 <- data_2002 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Rename to standard party names ---
data_2002 <- data_2002 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PRS = prs, PMEP = medp, PC = cdppn, PSN = psn, PAS = pas)

# --- Lista nominal from all_months_years ---
ln_2002 <- ln_data %>%
  filter(ed == state_ed, month == 6, year == 2002) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

data_2002 <- data_2002 %>%
  left_join(ln_2002, by = "section")

data_2002 <- data_2002 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2002 <- data_2002 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2002 <- data_2002 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2002 <- data_2002 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PRS, PMEP, PC, PSN, PAS)), na.rm = TRUE))

data_2002 <- data_2002 %>%
  mutate(year = 2002, month = "July")

collapsed_2002 <- data_2002
cat(paste0("  2002: ", nrow(collapsed_2002), " obs\n"))

###############################################################################
### PROCESSING 2005 ELECTION
###############################################################################
cat("Processing 2005...\n")

data_2005 <- read.csv(file.path(raw_data_path, "2005", "Ayu_Seccion_2005_No_LN.csv"),
                      stringsAsFactors = FALSE)

data_2005 <- data_2005 %>%
  rename(municipality = nombre_municipio)

data_2005 <- data_2005 %>%
  filter(!(municipality == "" & is.na(section)))
data_2005 <- data_2005 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2005 <- setdiff(names(data_2005), c("municipality", "section"))
data_2005 <- data_2005 %>%
  mutate(across(all_of(party_cols_2005), ~suppressWarnings(as.numeric(as.character(.)))))

data_2005$municipality <- toupper(trimws(data_2005$municipality))
data_2005$municipality <- remove_accents(data_2005$municipality)

# --- Collapse by municipality section ---
data_2005 <- data_2005 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Rename to standard party names ---
data_2005 <- data_2005 %>%
  rename(PAN = pan, PRI = pri, PRD_PT_PRS = prdptprs, PC = pc, PVEM = pvem)

# --- Lista nominal from all_months_years ---
ln_2005 <- ln_data %>%
  filter(ed == state_ed, month == 6, year == 2005) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

data_2005 <- data_2005 %>%
  left_join(ln_2005, by = "section")

data_2005 <- data_2005 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2005 <- data_2005 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2005 <- data_2005 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2005 <- data_2005 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PVEM, PC, PRD_PT_PRS)), na.rm = TRUE))

data_2005 <- data_2005 %>%
  mutate(year = 2005, month = "July")

collapsed_2005 <- data_2005
cat(paste0("  2005: ", nrow(collapsed_2005), " obs\n"))

###############################################################################
### PROCESSING 2008 ELECTION
###############################################################################
cat("Processing 2008...\n")

data_2008 <- read.csv(file.path(raw_data_path, "2008", "Ayu_Seccion_2008.csv"),
                      stringsAsFactors = FALSE)

data_2008 <- data_2008 %>%
  rename(municipality = nombre_municipio_nominal)

data_2008 <- data_2008 %>%
  filter(!(municipality == "" & is.na(section)))
data_2008 <- data_2008 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2008 <- setdiff(names(data_2008), c("municipality", "section"))
data_2008 <- data_2008 %>%
  mutate(across(all_of(party_cols_2008), ~suppressWarnings(as.numeric(as.character(.)))))

data_2008$municipality <- toupper(trimws(data_2008$municipality))
data_2008$municipality <- remove_accents(data_2008$municipality)

# --- Collapse by municipality section ---
data_2008 <- data_2008 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Rename to standard party names ---
data_2008 <- data_2008 %>%
  rename(PAN = pan, PRI_PANAL = pripanal, PRD_PVEM = prdpvem, PT = pt, PC_PRS = pcprs, PAS = pas)

data_2008 <- data_2008 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2008 <- data_2008 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2008 <- data_2008 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2008 <- data_2008 %>%
  mutate(valid = rowSums(across(c(PAN, PT, PAS, PRD_PVEM, PC_PRS, PRI_PANAL)), na.rm = TRUE))

data_2008 <- data_2008 %>%
  mutate(year = 2008, month = "July")

collapsed_2008 <- data_2008
cat(paste0("  2008: ", nrow(collapsed_2008), " obs\n"))

###############################################################################
### PROCESSING 2011 ELECTION
###############################################################################
cat("Processing 2011...\n")

data_2011 <- read_excel(file.path(raw_data_path, "2011", "Ayu_Seccion_2011.xlsx"),
                        sheet = "Sheet1")

data_2011 <- data_2011 %>%
  rename(municipality = MUNICIPIO, section = Section)

data_2011 <- data_2011 %>%
  filter(!(municipality == "" & is.na(section)))
data_2011 <- data_2011 %>% filter(!is.na(total) & total != 0)

# Convert party columns to numeric
party_cols_2011 <- setdiff(names(data_2011), c("municipality", "section"))
data_2011 <- data_2011 %>%
  mutate(across(all_of(party_cols_2011), ~suppressWarnings(as.numeric(as.character(.)))))

data_2011$municipality <- toupper(trimws(data_2011$municipality))
data_2011$municipality <- remove_accents(data_2011$municipality)

# --- Collapse by municipality section ---
data_2011 <- data_2011 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Compute total ---
data_2011 <- data_2011 %>%
  mutate(total = rowSums(across(c(PAN, PRD, PRS, PRI_PVEM_PANAL, PT_PC, CandidatosNoRegistrados, VotosNulos)), na.rm = TRUE))
data_2011 <- data_2011 %>% filter(total > 0)

# --- Lista nominal from all_months_years ---
ln_2011 <- ln_data %>%
  filter(ed == state_ed, month == 6, year == 2011) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

data_2011 <- data_2011 %>%
  left_join(ln_2011, by = "section")

data_2011 <- data_2011 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2011 <- data_2011 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2011 <- data_2011 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2011 <- data_2011 %>%
  mutate(valid = rowSums(across(c(PAN, PRD, PRS, PRI_PVEM_PANAL, PT_PC)), na.rm = TRUE))

data_2011 <- data_2011 %>%
  mutate(year = 2011, month = "July")

collapsed_2011 <- data_2011
cat(paste0("  2011: ", nrow(collapsed_2011), " obs\n"))

###############################################################################
### PROCESSING 2014 ELECTION
###############################################################################
cat("Processing 2014...\n")

data_2014 <- read_excel(file.path(raw_data_path, "2014", "Ayuntamientos_2014.xlsx"),
                        sheet = 1, col_types = "text")
# --- Multi-sheet import (20 municipality sheets) ---
sheets_2014 <- excel_sheets(file.path(raw_data_path, "2014", "Ayuntamientos_2014.xlsx"))
data_2014_list <- list()
for (sh in sheets_2014) {
  d <- tryCatch({
    read_excel(file.path(raw_data_path, "2014", "Ayuntamientos_2014.xlsx"),
               sheet = sh, col_types = "text")
  }, error = function(e) NULL)
  if (!is.null(d) && nrow(d) > 0) {
    data_2014_list[[sh]] <- d
  }
}
data_2014 <- bind_rows(data_2014_list)
data_2014 <- data_2014 %>% filter(!is.na(municipality) & municipality != "")


data_2014 <- data_2014 %>%
  filter(!(municipality == "" & is.na(section)))

# Convert party columns to numeric
party_cols_2014 <- setdiff(names(data_2014), c("municipality", "section"))
data_2014 <- data_2014 %>%
  mutate(across(all_of(party_cols_2014), ~suppressWarnings(as.numeric(as.character(.)))))

<<<<<<< HEAD
data_2014$municipality <- toupper(trimws(data_2014$municipality))
data_2014$municipality <- remove_accents(data_2014$municipality)

# --- Collapse by municipality uniqueid section ---
data_2014 <- data_2014 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# --- Rename to standard party names ---
data_2014 <- data_2014 %>%
  # NOTE: winner column not generated (no winner calculations)
  
  # --- Lista nominal from file ---
  # LN file: LN14_NAY.dta
  # NOTE: Update path as needed based on your file structure
  ln_2014_raw <- read_dta(file.path(raw_data_path, "2014", "LN14_NAY.dta"))
ln_2014 <- ln_2014_raw %>%
  select(section, listanominal) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")
data_2014 <- data_2014 %>%
  left_join(ln_2014, by = "section")

data_2014 <- data_2014 %>%
  mutate(turnout = total / listanominal)
=======
df2002 <- df2002 %>% 
  mutate(
    month = "June",
    year = 2002
  )

temp_2002 <- df2002 %>% 
  left_join(ln_all_months_years, by = c("section", "month", "year")) %>% 
  select(-month)
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# --- Drop noregistrados, nulos ---
data_2014 <- data_2014 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

<<<<<<< HEAD
# --- Assign uniqueid ---
data_2014 <- data_2014 %>%
  mutate(uniqueid = assign_uniqueid(municipality))
=======
temp_2002$month <- "July"
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# --- Valid votes ---
data_2014 <- data_2014 %>%
  mutate(valid = rowSums(across(c(PAN, PRI_PVEM_PANAL, PRD, PT, MC, PRS, CI_1)), na.rm = TRUE))

data_2014 <- data_2014 %>%
  mutate(year = 2014, month = "July")

collapsed_2014 <- data_2014
cat(paste0("  2014: ", nrow(collapsed_2014), " obs\n"))

###############################################################################
### PROCESSING 2017 ELECTION
###############################################################################
cat("Processing 2017...\n")

data_2017 <- read_dta(file.path(raw_data_path, "2017", "..\..\Precinct\Nayarit_ALL.dta"))


data_2017 <- data_2017 %>%
  filter(!(municipality == "" & is.na(section)))

# Convert party columns to numeric
party_cols_2017 <- setdiff(names(data_2017), c("municipality", "section"))
data_2017 <- data_2017 %>%
  mutate(across(all_of(party_cols_2017), ~suppressWarnings(as.numeric(as.character(.)))))

data_2017$municipality <- toupper(trimws(data_2017$municipality))
data_2017$municipality <- remove_accents(data_2017$municipality)

# --- Collapse by STATE municipality uniqueid section ---
data_2017 <- data_2017 %>%
  group_by(STATE, municipality, uniqueid, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

data_2017 <- data_2017 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_2017 <- data_2017 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

<<<<<<< HEAD
# --- Assign uniqueid ---
data_2017 <- data_2017 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2017 <- data_2017 %>%
  mutate(valid = rowSums(across(c(PAN_PRD_PT_PRS, PRI, PVEM, MC, PANAL, MORENA, PES, CI_1, CI_2, CI_3)), na.rm = TRUE))
=======
df2005 <- df2005 %>% 
  mutate(
    month = "June",
    year = 2005
  )

temp_2005 <- df2005 %>% 
  left_join(ln_all_months_years, by = c("section", "month", "year")) %>% 
  select(-month)
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

data_2017 <- data_2017 %>%
  mutate(year = 2017, month = "June")

collapsed_2017 <- data_2017
cat(paste0("  2017: ", nrow(collapsed_2017), " obs\n"))


###############################################################################
### COMBINE ALL YEARS
###############################################################################
cat("Combining all years...\n")

<<<<<<< HEAD
nayarit_all <- bind_rows(
  collapsed_1996,
  collapsed_1999,
  collapsed_2002,
  collapsed_2005,
  collapsed_2008,
  collapsed_2011,
  collapsed_2014,
  collapsed_2017
)

cat(paste0("Total observations: ", nrow(nayarit_all), "\n"))

# Save output
dir.create(processed_path, recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(nayarit_all, file.path(processed_path, "nayarit_process_raw_data.csv"))
cat(paste0("NAYARIT processing complete!\n"))
=======
names(df2008) <- tolower(gsub("\\.", "", names(df2008)))
names(df2008)[names(df2008) == "nombre_municipio"] <- "municipality"
names(df2008)[names(df2008) == "seccion"]          <- "section"
names(df2008)[names(df2008) == "lista_nominal"]    <- "listanominal"

df2008 <- subset(df2008, !(municipality == "" & is.na(section)))

df2008 <- df2008 %>% 
  mutate(total = as.numeric(total))

df2008 <- subset(df2008, !is.na(total) & total != 0)

num_vars_08 <- c("listanominal","pan","pripanal","prdpvem","pt","pcprs","pas","total")
for(v in num_vars_08){
  if(v %in% colnames(df2008)){
    df2008[[v]] <- as.numeric(df2008[[v]])
  }
}

names(df2008)[names(df2008) == "pan"]     <- "PAN"
names(df2008)[names(df2008) == "pripanal"]<- "PRI_PANAL"
names(df2008)[names(df2008) == "prdpvem"] <- "PRD_PVEM"
names(df2008)[names(df2008) == "pt"]      <- "PT"
names(df2008)[names(df2008) == "pcprs"]   <- "PC_PRS"
names(df2008)[names(df2008) == "pas"]     <- "PAS"

df2008$turnout <- df2008$total / df2008$listanominal

df2008$uniqueid <- 0
df2008$uniqueid[df2008$municipality == "ACAPONETA"]           <- 18001
df2008$uniqueid[df2008$municipality == "AHUACATLAN"]          <- 18002
df2008$uniqueid[df2008$municipality == "AMATLAN DE CA?AS"]    <- 18003
df2008$uniqueid[df2008$municipality == "BAHIA DE BANDERAS"]   <- 18020
df2008$uniqueid[df2008$municipality == "COMPOSTELA"]          <- 18004
df2008$uniqueid[df2008$municipality == "EL NAYAR"]            <- 18009
df2008$uniqueid[df2008$municipality == "HUAJICORI"]           <- 18005
df2008$uniqueid[df2008$municipality == "IXTLAN DEL RIO"]      <- 18006
df2008$uniqueid[df2008$municipality == "JALA"]                <- 18007
df2008$uniqueid[df2008$municipality == "LA YESCA"]            <- 18019
df2008$uniqueid[df2008$municipality == "ROSAMORADA"]          <- 18010
df2008$uniqueid[df2008$municipality == "RUIZ"]                <- 18011
df2008$uniqueid[df2008$municipality == "SAN BLAS"]            <- 18012
df2008$uniqueid[df2008$municipality == "SAN PEDRO LAGUNILLAS"]<- 18013
df2008$uniqueid[df2008$municipality == "SANTA MARIA DEL ORO"] <- 18014
df2008$uniqueid[df2008$municipality == "SANTIAGO IXCUINTLA"]  <- 18015
df2008$uniqueid[df2008$municipality == "TECUALA"]             <- 18016
df2008$uniqueid[df2008$municipality == "TEPIC"]               <- 18017
df2008$uniqueid[df2008$municipality == "TUXPAN"]              <- 18018
df2008$uniqueid[df2008$municipality == "XALISCO"]             <- 18008

df2008$valid <- rowSums(df2008[, c("PAN","PT","PAS","PRD_PVEM","PC_PRS","PRI_PANAL")],
                        na.rm = TRUE)

df2008$year  <- 2008
df2008$month <- "July"
df2008 <- df2008[order(df2008$section), ]

###############################################################################
## 7) 2011 DATA
###############################################################################
df2011 <- read_excel("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017,2021,2024/Ayu_Seccion_2011.xlsx", sheet = "Sheet1")

# drop K, L
df2011 <- df2011 %>% select(-c("...11", "...12"))

names(df2011)[names(df2011) == "MUNICIPIO:"] <- "municipality"
names(df2011)[names(df2011) == "Section"]   <- "section"

# fill municipality if blank
df2011 <- df2011 %>%
  tidyr::fill(municipality, .direction = "down")

df2011$total <- rowSums(df2011[, c("PAN","PRD","PRS","PRI_PVEM_PANAL","PT_PC",
                                   "Candidatos No Registrados","Votos Nulos")],
                        na.rm = TRUE)
df2011 <- subset(df2011, !(is.na(total) | total == 0))
df2011$CandidatosNoRegistrados <- NULL
df2011$VotosNulos             <- NULL

df2011$municipality <- trimws(df2011$municipality)

df2011$uniqueid <- 0
df2011$uniqueid[df2011$municipality == "ACAPONETA"]          <- 18001
df2011$uniqueid[df2011$municipality == "AHUACATLAN"]         <- 18002
df2011$uniqueid[df2011$municipality == "AMATLAN DE CANAS"]   <- 18003
df2011$uniqueid[df2011$municipality == "BAHIA DE BANDERAS"]  <- 18020
df2011$uniqueid[df2011$municipality == "COMPOSTELA"]         <- 18004
df2011$uniqueid[df2011$municipality == "EL NAYAR"]           <- 18009
df2011$uniqueid[df2011$municipality == "HUAJICORI"]          <- 18005
df2011$uniqueid[df2011$municipality == "IXTLAN DEL RIO"]     <- 18006
df2011$uniqueid[df2011$municipality == "JALA"]               <- 18007
df2011$uniqueid[df2011$municipality == "LA YESCA"]           <- 18019
df2011$uniqueid[df2011$municipality == "ROSAMORADA"]         <- 18010
df2011$uniqueid[df2011$municipality == "RUIZ"]               <- 18011
df2011$uniqueid[df2011$municipality == "SAN BLAS"]           <- 18012
df2011$uniqueid[df2011$municipality == "SAN PEDRO LAGUNILLAS"] <- 18013
df2011$uniqueid[df2011$municipality == "SANTA MARIA DEL ORO"]  <- 18014
df2011$uniqueid[df2011$municipality == "SANTIAGO IXCUINTLA"]   <- 18015
df2011$uniqueid[df2011$municipality == "TECUALA"]            <- 18016
df2011$uniqueid[df2011$municipality == "TEPIC"]              <- 18017
df2011$uniqueid[df2011$municipality == "TUXPAN"]             <- 18018
df2011$uniqueid[df2011$municipality == "XALISCO"]            <- 18008

df2011$valid <- rowSums(df2011[, c("PAN","PRD","PRS","PRI_PVEM_PANAL","PT_PC")], na.rm = TRUE)

# Merge with all_months_years

df2011 <- df2011 %>% 
  mutate(
    month = "June",
    year = 2011
  )

temp_2011 <- df2011 %>% 
  left_join(ln_all_months_years, by = c("section", "month", "year")) %>% 
  select(-month)

temp_2011$month   <- NULL

names(temp_2011)[names(temp_2011) == "lista"] <- "listanominal"
temp_2011$turnout <- temp_2011$total / temp_2011$listanominal

temp_2011$month <- "July"
temp_2011 <- temp_2011[order(temp_2011$section), ]

###############################################################################
## 8) APPEND 1996–2011 DATA
###############################################################################

df_1996_2011 <- bind_rows(df1996,df1999,temp_2002,temp_2005,df2008,temp_2011,)


###############################################################################
## 9) 2014 DATA
###############################################################################

excel_path_2014 <- "../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017,2021,2024/Ayuntamientos_2014.xlsx"
sheet_names_2014 <- excel_sheets(excel_path_2014)

# Read each sheet into a list
list_of_dfs_2014 <- lapply(sheet_names_2014, function(sn) {
  read_excel(excel_path_2014, sheet = sn, col_types = "text")
})

# Combine them
df2014 <- bind_rows(list_of_dfs_2014)

# Keep if municipality != ""
df2014 <- subset(df2014, municipality != "" & !is.na(municipality))

# Convert known numeric columns if needed; for example:
# Suppose these columns exist: Sección, etc.
names(df2014)[names(df2014) == "Sección"]       <- "section"
names(df2014)[names(df2014) == "CandidatoNoReg"]<- "no_reg"
names(df2014)[names(df2014) == "VotosNulos"]    <- "nulo"

df2014$uniqueid <- 0
df2014$uniqueid[df2014$municipality == "ACAPONETA"]           <- 18001
df2014$uniqueid[df2014$municipality == "AHUACATLÁN"]          <- 18002
df2014$uniqueid[df2014$municipality == "AMATLÁN DE CAÑAS"]    <- 18003
df2014$uniqueid[df2014$municipality == "BAHÍA DE BANDERAS"]   <- 18020
df2014$uniqueid[df2014$municipality == "COMPOSTELA"]          <- 18004
df2014$uniqueid[df2014$municipality == "DEL NAYAR"]           <- 18009
df2014$uniqueid[df2014$municipality == "HUAJICORI"]           <- 18005
df2014$uniqueid[df2014$municipality == "IXTLÁN DEL RÍO"]      <- 18006
df2014$uniqueid[df2014$municipality == "JALA"]                <- 18007
df2014$uniqueid[df2014$municipality == "LA YESCA"]            <- 18019
df2014$uniqueid[df2014$municipality == "ROSAMORADA"]          <- 18010
df2014$uniqueid[df2014$municipality == "RUÍZ"]                <- 18011
df2014$uniqueid[df2014$municipality == "SAN BLAS"]            <- 18012
df2014$uniqueid[df2014$municipality == "SAN PEDRO LAGUNILLAS"]<- 18013
df2014$uniqueid[df2014$municipality == "SANTA MARIA DEL ORO"] <- 18014
df2014$uniqueid[df2014$municipality == "SANTIAGO IXCUINTLA"]  <- 18015
df2014$uniqueid[df2014$municipality == "TECUALA"]             <- 18016
df2014$uniqueid[df2014$municipality == "TEPIC"]               <- 18017
df2014$uniqueid[df2014$municipality == "TUXPAN"]              <- 18018
df2014$uniqueid[df2014$municipality == "XALISCO"]             <- 18008

# We'll skip that aggregator step.

# Add year/month/state
df2014$year  <- 2014
df2014$month <- "July"
df2014$STATE <- "NAYARIT"

# This is your 2014 municipality-section data.
# We keep it at that level, skipping rowranks/winner.

###############################################################################
## 10) 2017 DATA
###############################################################################
df2017 <- read_excel("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017,2021,2024/Ayuntamientos_2017.xlsx", sheet = "AYUNTAMIENTOS", col_types = "text")

df2017$uniqueid <- 0
df2017$uniqueid[df2017$municipality == "ACAPONETA"]          <- 18001
df2017$uniqueid[df2017$municipality == "AHUACATLAN"]         <- 18002
df2017$uniqueid[df2017$municipality == "AMATLAN DE CAÑAS"]   <- 18003
df2017$uniqueid[df2017$municipality == "BAHIA DE BANDERAS"]  <- 18020
df2017$uniqueid[df2017$municipality == "COMPOSTELA"]         <- 18004
df2017$uniqueid[df2017$municipality == "DEL NAYAR"]          <- 18009
df2017$uniqueid[df2017$municipality == "HUAJICORI"]          <- 18005
df2017$uniqueid[df2017$municipality == "IXTLAN DEL RIO"]     <- 18006
df2017$uniqueid[df2017$municipality == "JALA"]               <- 18007
df2017$uniqueid[df2017$municipality == "LA YESCA"]           <- 18019
df2017$uniqueid[df2017$municipality == "ROSAMORADA"]         <- 18010
df2017$uniqueid[df2017$municipality == "RUIZ"]               <- 18011
df2017$uniqueid[df2017$municipality == "SAN BLAS"]           <- 18012
df2017$uniqueid[df2017$municipality == "SAN PEDRO LAGUNILLAS"] <- 18013
df2017$uniqueid[df2017$municipality == "SANTA MARIA DEL ORO"]  <- 18014
df2017$uniqueid[df2017$municipality == "SANTIAGO IXCUINTLA"]   <- 18015
df2017$uniqueid[df2017$municipality == "TECUALA"]            <- 18016
df2017$uniqueid[df2017$municipality == "TEPIC"]              <- 18017
df2017$uniqueid[df2017$municipality == "TUXPAN"]             <- 18018
df2017$uniqueid[df2017$municipality == "XALISCO"]            <- 18008

# drop columns not needed if they exist
# skip aggregator or rowranks or winner
df2017$year  <- 2017
df2017$month <- "June"
df2017$STATE <- "NAYARIT"

#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

# Load the 2021 dataset from the excel
data_2021 <- read_excel("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017,2021,2024/21/BaseDatosPresidenciaYSindicatura.xlsx", skip = 5)

data_ext <- read_excel("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017,2021,2024/PyS21-Ext.xlsx", skip = 5, sheet = "YES") %>% 
  dplyr::mutate(CABECERA_MUNICIPAL = "LA YESCA")

data_2021 <- bind_rows(data_2021, data_ext)
names(data_2021)

# Rename columns
data_2021 <- data_2021 %>%
  dplyr::rename(municipality = CABECERA_MUNICIPAL,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL_VOTOS,
                no_reg = NO_REGISTRADOS,
                nulos = NULOS,
                CI_1 = CI) %>%
  rename_with(~ gsub("NAN", "PANAL", .x)) %>% 
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
    municipality = case_when(municipality == "BUCERIAS" ~ "BAHIA DE BANDERAS",
                             municipality == "JESUS MARIA" ~ "DEL NAYAR",
                             TRUE ~ municipality)
  ) %>% 
  dplyr::filter(section > 0)

# Assign uniqueids
data_2021 <- data_2021 %>% 
  mutate(
    uniqueid = case_when(
      municipality == "ACAPONETA" ~ 18001,
      municipality == "AHUACATLAN" ~ 18002,  
      municipality == "AMATLAN DE CANAS" ~ 18003, 
      municipality == "BAHIA DE BANDERAS" ~ 18020, 
      municipality == "COMPOSTELA" ~ 18004,
      municipality == "HUAJICORI" ~ 18005,
      municipality == "IXTLAN DEL RIO" ~ 18006,  
      municipality == "JALA" ~ 18007,
      municipality == "DEL NAYAR" ~ 18009,  
      municipality == "ROSAMORADA" ~ 18010,
      municipality == "RUIZ" ~ 18011,  
      municipality == "SAN BLAS" ~ 18012,
      municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
      municipality == "SANTA MARIA DEL ORO" ~ 18014,
      municipality == "SANTIAGO IXCUINTLA" ~ 18015,
      municipality == "TECUALA" ~ 18016,
      municipality == "TEPIC" ~ 18017,
      municipality == "TUXPAN" ~ 18018,
      municipality == "LA YESCA" ~ 18019,
      municipality == "XALISCO" ~ 18008,
      TRUE ~ NA_real_
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
    valid = sum(c_across(PAN:MORENA_PANAL), na.rm = TRUE),
    year = 2021,
    month = case_when(
      municipality == "LA YESCA" ~ "December",
      TRUE ~ "June"
    ))

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 18) %>% 
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

# Load the 2024 dataset from the excel
data_2024 <- read_excel("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017,2021,2024/24/Presidencias y Sindicaturas.xlsx", skip = 5)

names(data_2024)

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL_VOTOS,
                no_reg = NO_REGISTRADOS,
                nulos = NULOS) %>%
  rename_with(~ gsub("NAN", "PANAL", .x)) %>% 
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
      municipality == "ACAPONETA" ~ 18001,
      municipality == "AHUACATLAN" ~ 18002,  
      municipality == "AMATLAN DE CANAS" ~ 18003, 
      municipality == "BAHIA DE BANDERAS" ~ 18020, 
      municipality == "COMPOSTELA" ~ 18004,
      municipality == "HUAJICORI" ~ 18005,
      municipality == "IXTLAN DEL RIO" ~ 18006,  
      municipality == "JALA" ~ 18007,
      municipality == "DEL NAYAR" ~ 18009,  
      municipality == "ROSAMORADA" ~ 18010,
      municipality == "RUIZ" ~ 18011,  
      municipality == "SAN BLAS" ~ 18012,
      municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
      municipality == "SANTA MARIA DEL ORO" ~ 18014,
      municipality == "SANTIAGO IXCUINTLA" ~ 18015,
      municipality == "TECUALA" ~ 18016,
      municipality == "TEPIC" ~ 18017,
      municipality == "TUXPAN" ~ 18018,
      municipality == "LA YESCA" ~ 18019,
      municipality == "XALISCO" ~ 18008,
      TRUE ~ NA_real_
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
    valid = sum(c_across(PAN:MORENA_FXM), na.rm = TRUE),
    year = 2024,
    month = "June"
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)


###############################################################################
## 11) APPEND (2014, 2017) + (1996–2011)
###############################################################################
# We'll append 2014 and 2017 together, then append them to the 1996–2011.

df_14_17 <- bind_rows(df2014, df2017) %>% 
  mutate(section = as.numeric(section),
         across(c(PRI_PVEM_PANAL:CI_1,PRI:listanominal), as.numeric)
         )

# Then combine with older data:
df_final <- bind_rows(df_1996_2011, df_14_17, collapsed_2021, collapsed_2024)

###############################################################################
## 12) SAVE FINAL DATASET
###############################################################################
# Now df_final is the full municipality-section panel for 1996–2011 + 2014 + 2017
# at the municipality-section level, skipping aggregator, ranking, and winner.

data.table::fwrite(df_final,"../../../Processed Data/nayarit/nayarit_process_raw_data.csv")
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
