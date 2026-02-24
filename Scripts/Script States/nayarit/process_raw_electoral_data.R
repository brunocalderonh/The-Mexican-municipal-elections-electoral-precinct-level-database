###############################################################################
### process_raw_electoral_data_Nayarit.R
### Full pipeline: NAYARIT municipal election data processing
### Election years: 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017
### Source: Stata do-files (JOHN + SALVADOR) translated to R
### Self-contained pipeline: load -> clean -> harmonize -> collapse -> output
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
}

# Helper function: standardize column names (lowercase, strip ALL separators)
standardize_colnames <- function(df) {
  nms <- names(df)
  nms <- tolower(nms)
  nms <- gsub("\\s+", "", nms)   # strip spaces
  nms <- gsub("\\.+", "", nms)   # strip dots
  nms <- gsub("_+", "", nms)    # strip underscores
  nms <- gsub("-+", "", nms)    # strip dashes
  nms <- gsub("^_|_$", "", nms) # trim leading/trailing
  names(df) <- nms
  return(df)
}

cat(paste0("=========================================\n"))
cat(paste0("Processing ", STATE_NAME, " electoral data\n"))
cat(paste0("Election years: 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017\n"))
cat(paste0("=========================================\n"))

# Load all_months_years LN data (used by multiple years)
ln_data <- tryCatch(
  read_dta(file.path(ln_path, "all_months_years.dta")),
  error = function(e) {
    cat("WARNING: Could not load all_months_years.dta\n")
    data.frame()
  }
)
# Note: LN file columns are: state (string), section, lista, day, month (string), year
# No standardize needed — columns are already clean

# =============================================================================
# MUNICIPALITY -> UNIQUEID MAPPING
# =============================================================================
assign_uniqueid <- function(municipality) {
  dplyr::case_when(
    municipality == "ACAPONETA" ~ 18001,
    municipality == "AHUACATLAN" ~ 18002,
    municipality == "AMATLAN DE CANAS" ~ 18003,
    municipality == "AMATLAN DE CA?AS" ~ 18003,
    municipality == "COMPOSTELA" ~ 18004,
    municipality == "HUAJICORI" ~ 18005,
    municipality == "IXTLAN DEL RIO" ~ 18006,
    municipality == "JALA" ~ 18007,
    municipality == "XALISCO" ~ 18008,
    municipality == "DEL NAYAR" ~ 18009,
    municipality == "EL NAYAR" ~ 18009,
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
    municipality == "BAHIA DE BANDERAS" ~ 18020,
    TRUE ~ NA_real_
  )
}

###############################################################################
### PROCESSING 1996 ELECTION
###############################################################################
cat("Processing 1996...\n")

# Stata: insheet using Ayu_Seccion_1996_No_LN.csv, clear
data_1996 <- read.csv(file.path(raw_data_path, "1996", "Ayu_Seccion_1996_No_LN.csv"),
                      stringsAsFactors = FALSE, check.names = FALSE)
data_1996 <- standardize_colnames(data_1996)

# Stata: rename municipio municipality / rename seccion section
data_1996 <- data_1996 %>%
  rename(municipality = municipio, section = seccion) 

# Stata: drop if municipality=="" & section==.
data_1996 <- data_1996 %>%
  filter(!(municipality == "" & is.na(section)))

# Convert party columns to numeric
data_1996 <- data_1996 %>%
  mutate(across(c(pan, pri, prd, pt, pvem, prs, parmen, cd, noregistrados, nulos),
                ~suppressWarnings(as.numeric(as.character(.)))))

# Stata: egen total = rowtotal(pan pri prd pt pvem prs parm cd noregistrados nulos)
data_1996 <- data_1996 %>%
  mutate(total = rowSums(across(c(pan, pri, prd, pt, pvem, prs, parmen, cd,
                                  noregistrados, nulos)), na.rm = TRUE))

data_1996$municipality <- toupper(trimws(data_1996$municipality))
data_1996$municipality <- remove_accents(data_1996$municipality)

# Stata: collapse (sum) pan - nulos total, by (municipality section)
data_1996 <- data_1996 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Stata: rename pan PAN ... rename cd PCD
data_1996 <- data_1996 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem,
         PRS = prs, PARM = parmen, PCD = cd)

# Stata: * gen turnout = total/listanominal (COMMENTED OUT — no LN for 1996)

# Stata: drop noreg nulos
data_1996 <- data_1996 %>%
  select(-any_of(c("noregistrados", "nulos")))

# Stata: gen uniqueid
data_1996 <- data_1996 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# Stata: egen valid = rowtotal(PAN PRI PRD PT PVEM PRS PARM PCD)
data_1996 <- data_1996 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PRS, PARM, PCD)), na.rm = TRUE))

# Stata: merge LN from all_months_years (ed=18, month=6, year=1999)
ln_1996 <- ln_data %>%
  filter(state == STATE_NAME, month == "June", year == 1999) %>%
  select(section, lista) %>%
  rename(listanominal = lista) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

data_1996 <- data_1996 %>%
  left_join(ln_1996, by = "section")

# Stata: gen turnout = total/listanominal (AFTER LN merge)
data_1996 <- data_1996 %>%
  mutate(turnout = total / listanominal)

# Stata: drop if total==. | total==0 (at end of block)
data_1996 <- data_1996 %>% filter(!is.na(total) & total > 0)

data_1996 <- data_1996 %>%
  mutate(year = 1996, month = "July", STATE = STATE_NAME)

collapsed_1996 <- data_1996
cat(paste0("  1996: ", nrow(collapsed_1996), " obs\n"))

###############################################################################
### PROCESSING 1999 ELECTION
###############################################################################
cat("Processing 1999...\n")

# Stata: insheet using Ayu_Seccion_1999_No_LN.csv, clear
data_1999 <- read.csv(file.path(raw_data_path, "1999", "Ayu_Seccion_1999_No_LN.csv"),
                      stringsAsFactors = FALSE, check.names = FALSE)
data_1999 <- standardize_colnames(data_1999)

# Stata: rename nomunicipio municipality / rename seccion section
data_1999 <- data_1999 %>%
  rename(municipality = nomunicipio, section = seccion) |> 
  mutate(uniqueid = 18000 + municipality)

# Stata: drop if municipality==. & section==.
data_1999 <- data_1999 %>%
  filter(!(is.na(municipality) & is.na(section)))

# Convert to numeric
data_1999 <- data_1999 %>%
  mutate(across(c(pri, pvem, medp, parmen, pps, cac, candnoreg, votosnulos),
                ~suppressWarnings(as.numeric(as.character(.)))))

# Stata: egen total = rowtotal(pri pvem medp parmen pps cac candnoreg votosnulos)
data_1999 <- data_1999 %>%
  mutate(total = rowSums(across(c(pri, pvem, medp, parmen, pps, cac,
                                  candnoreg, votosnulos)), na.rm = TRUE))

# Stata: drop if total==. | total==0
data_1999 <- data_1999 %>% filter(!is.na(total) & total > 0)

data_1999$municipality <- toupper(trimws(as.character(data_1999$municipality)))
data_1999$municipality <- remove_accents(data_1999$municipality)

# Stata: collapse (sum) pri - votosnulos total, by (municipality section)
data_1999 <- data_1999 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Stata: rename cac PAN_PRD_PT, etc.
data_1999 <- data_1999 %>%
  rename(PAN_PRD_PT = cac, PRI = pri, PVEM = pvem, PMEP = medp,
         PARM = parmen, PPS = pps)

# Stata: drop candnoreg votosnulos
data_1999 <- data_1999 %>%
  select(-any_of(c("candnoreg", "votosnulos")))

# Stata: egen valid = rowtotal(PRI PVEM PMEP PARM PPS PAN_PRD_PT)
data_1999 <- data_1999 %>%
  mutate(valid = rowSums(across(c(PRI, PVEM, PMEP, PARM, PPS, PAN_PRD_PT)), na.rm = TRUE))

# Stata: merge LN from all_months_years (ed=18, month=6, year=1999)
ln_1999 <- ln_data %>%
  filter(state == STATE_NAME, month == "June", year == 1999) %>%
  select(section, lista) %>%
  rename(listanominal = lista) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

data_1999 <- data_1999 %>%
  left_join(ln_1999, by = "section")

# Stata: gen turnout = total/listanominal (AFTER LN merge)
data_1999 <- data_1999 %>%
  mutate(turnout = total / listanominal)

# Uniqueid — Stata merges with 1996 file for uniqueid, we assign directly
data_1999 <- data_1999 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

data_1999 <- data_1999 %>%
  mutate(year = 1999, month = "July", STATE = STATE_NAME)

collapsed_1999 <- data_1999
cat(paste0("  1999: ", nrow(collapsed_1999), " obs\n"))

###############################################################################
### PROCESSING 2002 ELECTION
###############################################################################
cat("Processing 2002...\n")

# Stata: insheet using Ayu_Seccion_2002_No_LN.csv, clear
data_2002 <- read.csv(file.path(raw_data_path, "2002", "Ayu_Seccion_2002_No_LN.csv"),
                      stringsAsFactors = FALSE)
data_2002 <- standardize_colnames(data_2002)

names(data_2002) 

# Stata: rename nombre_municipio municipality / rename seccion section
data_2002 <- data_2002 %>%
  rename(municipality = nombremunicipio, section = seccion)

# Stata: drop if municipality=="" & section==.
data_2002 <- data_2002 %>%
  filter(!(municipality == "" & is.na(section)))

# Stata: drop if total==. | total==0 (total comes from CSV)
data_2002 <- data_2002 %>%
  mutate(across(-c(municipality, section),
                ~suppressWarnings(as.numeric(as.character(.)))))
data_2002 <- data_2002 %>% filter(!is.na(total) & total > 0)

data_2002$municipality <- toupper(trimws(data_2002$municipality))
data_2002$municipality <- remove_accents(data_2002$municipality)

# Stata: collapse (sum) pan - pas total, by (municipality section)
data_2002 <- data_2002 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Stata: rename parties
data_2002 <- data_2002 %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem,
         PRS = prs, PMEP = medp, PC = cdppn, PSN = psn, PAS = pas)

# Stata: gen uniqueid
data_2002 <- data_2002 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# Stata: egen valid = rowtotal(PAN PRI PRD PT PVEM PRS PMEP PC PSN PAS)
data_2002 <- data_2002 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PRS, PMEP, PC, PSN, PAS)), na.rm = TRUE))

# Stata: gen turnout = total/listanominal (AFTER LN merge)
data_2002 <- data_2002 %>%
  mutate(turnout = total / listanominal)

data_2002 <- data_2002 %>%
  mutate(year = 2002, month = "July", STATE = STATE_NAME)

collapsed_2002 <- data_2002
cat(paste0("  2002: ", nrow(collapsed_2002), " obs\n"))

###############################################################################
### PROCESSING 2005 ELECTION
###############################################################################
cat("Processing 2005...\n")

# Stata: insheet using Ayu_Seccion_2005_No_LN.csv, clear
data_2005 <- read.csv(file.path(raw_data_path, "2005", "Ayu_Seccion_2005_No_LN.csv"),
                      stringsAsFactors = FALSE)
data_2005 <- standardize_colnames(data_2005)
names(data_2005)
# Stata: rename nombre_municipio municipality / rename seccion section
data_2005 <- data_2005 %>%
  rename(municipality = nombremunicipio, section = seccion)

# Stata: drop if municipality=="" & section==.
data_2005 <- data_2005 %>%
  filter(!(municipality == "" & is.na(section)))

# Stata: drop if total==. | total==0 (total comes from CSV)
data_2005 <- data_2005 %>%
  mutate(across(-c(municipality, section),
                ~suppressWarnings(as.numeric(as.character(.)))))
data_2005 <- data_2005 %>% filter(!is.na(total) & total > 0)

data_2005$municipality <- toupper(trimws(data_2005$municipality))
data_2005$municipality <- remove_accents(data_2005$municipality)

# Stata: collapse (sum) pan - prdptprs total, by (municipality section)
data_2005 <- data_2005 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Stata: rename parties
data_2005 <- data_2005 %>%
  rename(PAN = pan, PRI = pri, PRD_PT_PRS = prdptprs, PC = pc, PVEM = pvem)

# Stata: gen uniqueid
data_2005 <- data_2005 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# Stata: egen valid = rowtotal(PAN PRI PVEM PC PRD_PT_PRS)
data_2005 <- data_2005 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PVEM, PC, PRD_PT_PRS)), na.rm = TRUE))


# Stata: gen turnout = total/listanominal (AFTER LN merge)
data_2005 <- data_2005 %>%
  mutate(turnout = total / listanominal)

data_2005 <- data_2005 %>%
  mutate(year = 2005, month = "July", STATE = STATE_NAME)

collapsed_2005 <- data_2005
cat(paste0("  2005: ", nrow(collapsed_2005), " obs\n"))

###############################################################################
### PROCESSING 2008 ELECTION
###############################################################################
cat("Processing 2008...\n")

# Stata: insheet using Ayu_Seccion_2008.csv, clear (HAS listanominal in CSV)
data_2008 <- read.csv(file.path(raw_data_path, "2008", "Ayu_Seccion_2008.csv"),
                      stringsAsFactors = FALSE)
data_2008 <- standardize_colnames(data_2008)
# After standardize: nombremunicipio, seccion, listanominal, pan, pripanal,
#                    prdpvem, pt, pcprs, pas, ..., total

# Stata: rename nombre_municipio municipality / rename seccion section
# rename lista_nominal listanominal — already done by standardize (strips underscore)
data_2008 <- data_2008 %>%
  rename(municipality = nombremunicipio, section = seccion)

# Stata: drop if municipality=="" & section==.
data_2008 <- data_2008 %>%
  filter(!(municipality == "" & is.na(section)))

# Stata: drop if total==. | total==0 (total comes from CSV)
data_2008 <- data_2008 %>%
  mutate(across(-c(municipality, section),
                ~suppressWarnings(as.numeric(as.character(.)))))
data_2008 <- data_2008 %>% filter(!is.na(total) & total > 0)

data_2008$municipality <- toupper(trimws(data_2008$municipality))
data_2008$municipality <- remove_accents(data_2008$municipality)

# Stata: collapse (sum) listanominal pan - pripanal total, by (municipality section)
data_2008 <- data_2008 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Stata: rename parties
data_2008 <- data_2008 %>%
  rename(PAN = pan, PRI_PANAL = pripanal, PRD_PVEM = prdpvem,
         PT = pt, PC_PRS = pcprs, PAS = pas)

# Stata: gen turnout = total/listanominal (LN is in the CSV file)
data_2008 <- data_2008 %>%
  mutate(turnout = total / listanominal)

# Stata: gen uniqueid
data_2008 <- data_2008 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# Stata: egen valid = rowtotal(PAN PT PAS PRD_PVEM PC_PRS PRI_PANAL)
data_2008 <- data_2008 %>%
  mutate(valid = rowSums(across(c(PAN, PT, PAS, PRD_PVEM, PC_PRS, PRI_PANAL)), na.rm = TRUE))

data_2008 <- data_2008 %>%
  mutate(year = 2008, month = "July", STATE = STATE_NAME)

collapsed_2008 <- data_2008
cat(paste0("  2008: ", nrow(collapsed_2008), " obs\n"))

###############################################################################
### PROCESSING 2011 ELECTION
###############################################################################
cat("Processing 2011...\n")

# Stata: import excel "Ayu_Seccion_2011.xlsx", sheet("Sheet1") firstrow clear
data_2011 <- read_excel(file.path(raw_data_path, "2011", "Ayu_Seccion_2011.xlsx"),
                        sheet = "Sheet1")
names(data_2011)
# Stata: drop K L (unnamed columns)
data_2011 <- data_2011 %>% select(-any_of(c("K", "L", "...11", "...12")))

data_2011 <- standardize_colnames(data_2011)

# Find and rename municipality column
muni_col <- names(data_2011)[grepl("^municipio", names(data_2011))][1]
if (!is.na(muni_col) && muni_col != "municipality") {
  names(data_2011)[names(data_2011) == muni_col] <- "municipality"
}
# Find and rename section column
if (!"section" %in% names(data_2011)) {
  sec_col <- names(data_2011)[grepl("^secc", names(data_2011))][1]
  if (!is.na(sec_col)) names(data_2011)[names(data_2011) == sec_col] <- "section"
}

# Stata: replace municipality = municipality[_n-1] if municipality=="" & municipality[_n-1]!=""
data_2011$municipality <- as.character(data_2011$municipality)
for (i in 2:nrow(data_2011)) {
  if (is.na(data_2011$municipality[i]) || data_2011$municipality[i] == "") {
    if (!is.na(data_2011$municipality[i-1]) && data_2011$municipality[i-1] != "") {
      data_2011$municipality[i] <- data_2011$municipality[i-1]
    }
  }
}

# Convert to numeric
data_2011 <- data_2011 %>%
  mutate(across(-c(municipality, section), ~suppressWarnings(as.numeric(as.character(.)))))

# Rename party columns from standardize output to match Stata
# After standardize: pan, prd, prs, pripvempanal, ptpc, candidatosnoregistrados, votosnulos
if ("pripvempanal" %in% names(data_2011)) data_2011 <- rename(data_2011, PRI_PVEM_PANAL = pripvempanal)
if ("ptpc" %in% names(data_2011)) data_2011 <- rename(data_2011, PT_PC = ptpc)
if ("candidatosnoregistrados" %in% names(data_2011)) data_2011 <- rename(data_2011, CandidatosNoRegistrados = candidatosnoregistrados)
if ("votosnulos" %in% names(data_2011)) data_2011 <- rename(data_2011, VotosNulos = votosnulos)
names(data_2011) <- ifelse(names(data_2011) %in% c("pan", "prd", "prs"),
                           toupper(names(data_2011)), names(data_2011))

# Stata: egen total = rowtotal(PAN PRD PRS PRI_PVEM_PANAL PT_PC CandidatosNoRegistrados VotosNulos)
data_2011 <- data_2011 %>%
  mutate(total = rowSums(across(c(PAN, PRD, PRS, PRI_PVEM_PANAL, PT_PC,
                                  CandidatosNoRegistrados, VotosNulos)), na.rm = TRUE))

# Stata: drop if total==. | total==0
data_2011 <- data_2011 %>% filter(!is.na(total) & total > 0)

# Stata: drop CandidatosNoRegistrados VotosNulos
data_2011 <- data_2011 %>%
  select(-any_of(c("CandidatosNoRegistrados", "VotosNulos")))

data_2011$municipality <- toupper(trimws(data_2011$municipality))
data_2011$municipality <- remove_accents(data_2011$municipality)

# Stata: collapse (sum) PAN - total, by (municipality section)
data_2011 <- data_2011 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Stata: gen uniqueid
data_2011 <- data_2011 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# Stata: egen valid = rowtotal(PAN PRD PRS PRI_PVEM_PANAL PT_PC)
data_2011 <- data_2011 %>%
  mutate(valid = rowSums(across(c(PAN, PRD, PRS, PRI_PVEM_PANAL, PT_PC)), na.rm = TRUE))

# Stata: merge LN from all_months_years (ed=18, month=6, year=2011)
ln_2011 <- ln_data %>%
  filter(state == STATE_NAME, month == "June", year == 2011) %>%
  select(section, lista) %>%
  rename(listanominal = lista) %>%
  group_by(section) %>%
  summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

data_2011 <- data_2011 %>%
  left_join(ln_2011, by = "section")

# Stata: gen turnout = total/listanominal (AFTER LN merge)
data_2011 <- data_2011 %>%
  mutate(turnout = total / listanominal)

data_2011 <- data_2011 %>%
  mutate(year = 2011, month = "July", STATE = STATE_NAME)

collapsed_2011 <- data_2011
cat(paste0("  2011: ", nrow(collapsed_2011), " obs\n"))

###############################################################################
### PROCESSING 2014 ELECTION (Salvador update)
###############################################################################
cat("Processing 2014...\n")

# Stata: import excel multi-sheet Ayuntamientos_2014.xlsx, append all sheets
sheets_2014 <- excel_sheets(file.path(raw_data_path, "2014", "Ayuntamientos_2014.xlsx"))
data_2014_list <- list()
for (sh in sheets_2014) {
  d <- tryCatch({
    read_excel(file.path(raw_data_path, "2014", "Ayuntamientos_2014.xlsx"),
               sheet = sh, col_types = "text")
  }, error = function(e) NULL)
  if (!is.null(d) && nrow(d) > 0) {
    d <- standardize_colnames(d)
    data_2014_list[[sh]] <- d
  }
}
data_2014 <- bind_rows(data_2014_list)

# Find municipality column
if ("municipality" %in% names(data_2014)) {
  # already named
} else if ("municipio" %in% names(data_2014)) {
  data_2014 <- data_2014 %>% rename(municipality = municipio)
} else {
  muni_col <- names(data_2014)[grepl("^municipio|^muni", names(data_2014))][1]
  if (!is.na(muni_col)) names(data_2014)[names(data_2014) == muni_col] <- "municipality"
}

# Stata: keep if municipality!=""
data_2014 <- data_2014 %>% filter(!is.na(municipality) & municipality != "")

# Stata: destring *, replace
data_2014 <- data_2014 %>%
  mutate(across(-municipality, ~suppressWarnings(as.numeric(as.character(.)))))

data_2014$municipality <- toupper(trimws(data_2014$municipality))
data_2014$municipality <- remove_accents(data_2014$municipality)

# Stata: gen uniqueid
data_2014 <- data_2014 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# Find and rename section column
sec_col <- names(data_2014)[grepl("^secc", names(data_2014))][1]
if (!is.na(sec_col)) names(data_2014)[names(data_2014) == sec_col] <- "section"

# Find and rename special columns
noreg_col <- names(data_2014)[grepl("candidatonoreg|candnoreg|candidatosnoregistrados", names(data_2014))][1]
if (!is.na(noreg_col)) names(data_2014)[names(data_2014) == noreg_col] <- "no_reg"
nulo_col <- names(data_2014)[grepl("votosnulos", names(data_2014))][1]
if (!is.na(nulo_col)) names(data_2014)[names(data_2014) == nulo_col] <- "nulo"

# Rename party columns from standardize output
# After standardize: pripvempanal, pan, prd, pt, mc, prs, ci1
party_rename <- c("pripvempanal" = "PRI_PVEM_PANAL",
                  "pan" = "PAN", "prd" = "PRD", "pt" = "PT",
                  "mc" = "MC", "prs" = "PRS", "ci1" = "CI_1")
for (old_nm in names(party_rename)) {
  if (old_nm %in% names(data_2014)) {
    names(data_2014)[names(data_2014) == old_nm] <- party_rename[old_nm]
  }
}

# Stata: collapse (sum) PRI_PVEM_PANAL-CI_1, by (municipality uniqueid section)
party_cols_2014 <- c("PAN", "PRI_PVEM_PANAL", "PRD", "PT", "MC", "PRS", "CI_1")
data_2014 <- data_2014 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(any_of(c(party_cols_2014, "no_reg", "nulo")),
                   ~sum(., na.rm = TRUE)), .groups = "drop")

# Stata: egen valid = rowtotal(PAN PRI_PVEM_PANAL PRD PT MC PRS CI_1)
data_2014 <- data_2014 %>%
  mutate(valid = rowSums(across(any_of(party_cols_2014)), na.rm = TRUE))

# Stata: gen total = valid + no_reg + nulo
data_2014 <- data_2014 %>%
  mutate(total = valid + coalesce(no_reg, 0) + coalesce(nulo, 0))

# Stata: drop no_reg nulo
data_2014 <- data_2014 %>%
  select(-any_of(c("no_reg", "nulo")))

# Stata: LN from LN2014.dta (entidad==18, month==5)
ln_2014_raw <- tryCatch(
  read_dta(file.path(ln_path, "LN 2012-2019", "2014", "LN2014.dta")),
  error = function(e) {
    cat("  WARNING: Could not load LN2014.dta\n")
    data.frame()
  }
)

if (nrow(ln_2014_raw) > 0) {
  ln_2014_raw <- standardize_colnames(ln_2014_raw)
  if ("entidad" %in% names(ln_2014_raw)) {
    ln_2014 <- ln_2014_raw %>%
      filter(entidad == state_ed, month == 5, seccion != 0) %>%
      rename(listanominal = lista,
             section = seccion) %>%
      select(section, listanominal) %>%
      group_by(section) %>%
      summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")
  } else {
    ln_2014 <- ln_2014_raw %>%
      select(any_of(c("section", "seccion", "lista", "listanominal")))
    if ("seccion" %in% names(ln_2014)) ln_2014 <- rename(ln_2014)
    if ("lista" %in% names(ln_2014)) ln_2014 <- rename(ln_2014, listanominal = lista)
    ln_2014 <- ln_2014 %>%
      group_by(section) %>%
      summarise(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")
  }
  
  # Stata: merge 1:1 section using LN14_NAY.dta
  data_2014 <- data_2014 %>%
    left_join(ln_2014, by = "section")
  
  # Stata: gen turnout = total/listanominal (AFTER LN merge)
  data_2014 <- data_2014 %>%
    mutate(turnout = total / listanominal)
}

data_2014 <- data_2014 %>%
  mutate(year = 2014, month = "July", STATE = STATE_NAME)

collapsed_2014 <- data_2014
cat(paste0("  2014: ", nrow(collapsed_2014), " obs\n"))

###############################################################################
### PROCESSING 2017 ELECTION (Salvador update)
###############################################################################
cat("Processing 2017...\n")

# Stata: import excel "Ayuntamientos_2017.xlsx", first sheet("AYUNTAMIENTOS") clear
data_2017 <- read_excel(file.path(raw_data_path, "2017", "Ayuntamientos_2017.xlsx"),
                        sheet = "AYUNTAMIENTOS")
data_2017 <- standardize_colnames(data_2017)
# After standardize: state, municipality, section, listanominal, pan, pri, prd, pt, pvem,
#   mc, panal, morena, pes, pripvempanal (coalition total),
#   cpripvempanal, cppripvem, cpripanal, cpvempanal (coalition subs),
#   cpanprdptprs, ... (coalition subs), candind1-5, total, etc.

data_2017$municipality <- toupper(trimws(as.character(data_2017$municipality)))
data_2017$municipality <- remove_accents(data_2017$municipality)

# Convert to numeric
data_2017 <- data_2017 %>%
  mutate(across(-c(any_of(c("state", "municipality"))),
                ~suppressWarnings(as.numeric(as.character(.)))))

# Stata: gen uniqueid
data_2017 <- data_2017 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# Stata: drop C_PRI_PVEM_PANAL CP_PRI_PVEM CP_PRI_PANAL CP_PVEM_PANAL
data_2017 <- data_2017 %>%
  select(-any_of(c("cpripvempanal", "cppripvem", "cppripanal", "cppvempanal",
                   "pripvempanal")))

# Stata: egen PAN_PRD_PT_PRS = rowtotal(C_PAN_PRD_PT_PRS-CP_PT_PRS)
# Stata: replace PAN_PRD_PT_PRS = PAN + PRD + PT + PRS
# Stata: drop coalition subcomponents and individual PAN PRD PT PRS
coal_sub_cols <- names(data_2017)[grepl("^c[cp].*panprdptprs|^cpanprdptprs|^c[cp].*panprd[^p]|^c[cp].*panpt[^p]|^c[cp].*panprs|^c[cp].*prdpt[^p]|^c[cp].*prdprs|^c[cp].*ptprs|^c[cp].*prdptprs|^c[cp].*panprdpt$", names(data_2017))]
data_2017 <- data_2017 %>%
  mutate(PAN_PRD_PT_PRS = coalesce(pan, 0) + coalesce(prd, 0) +
           coalesce(pt, 0) + coalesce(prs, 0))
data_2017 <- data_2017 %>%
  select(-any_of(c("pan", "prd", "pt", "prs", coal_sub_cols)))

# Drop remaining coalition sub-columns that match pattern
coal_remaining <- names(data_2017)[grepl("^cp|^c[a-z].*prd|^c[a-z].*pan", names(data_2017))]
coal_remaining <- setdiff(coal_remaining, c("PAN_PRD_PT_PRS"))
data_2017 <- data_2017 %>% select(-any_of(coal_remaining))

# Stata: drop CAND_IND_4 CAND_IND_5 / rename CAND_IND_* CI_*
data_2017 <- data_2017 %>%
  select(-any_of(c("candind4", "candind5")))
if ("candind1" %in% names(data_2017)) data_2017 <- rename(data_2017, CI_1 = candind1)
if ("candind2" %in% names(data_2017)) data_2017 <- rename(data_2017, CI_2 = candind2)
if ("candind3" %in% names(data_2017)) data_2017 <- rename(data_2017, CI_3 = candind3)

# Rename remaining party columns
for (nm in c("pri", "pvem", "mc", "panal", "morena", "pes")) {
  if (nm %in% names(data_2017)) {
    names(data_2017)[names(data_2017) == nm] <- toupper(nm)
  }
}

# Find section column
if (!"section" %in% names(data_2017)) {
  sec_col <- names(data_2017)[grepl("^secc", names(data_2017))][1]
  if (!is.na(sec_col)) names(data_2017)[names(data_2017) == sec_col] <- "section"
}

# Stata: collapse (sum) ..., by (STATE municipality uniqueid section)
party_cols_2017 <- c("PAN_PRD_PT_PRS", "PRI", "PVEM", "MC", "PANAL", "MORENA", "PES",
                     "CI_1", "CI_2", "CI_3", "totalvotos")
data_2017 <- data_2017 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(any_of(c(party_cols_2017, "total", "listanominal")),
                   ~sum(., na.rm = TRUE)), .groups = "drop")
names(data_2017)
# Stata: egen valid = rowtotal(PAN_PRD_PT_PRS PRI PVEM MC PANAL MORENA PES CI_1 CI_2 CI_3)
data_2017 <- data_2017 %>%
  mutate(valid = rowSums(across(any_of(party_cols_2017)), na.rm = TRUE)) |> 
  dplyr::rename(total = totalvotos)

# Stata: turnout = total/listanominal (LN is in the Excel file)
data_2017 <- data_2017 %>%
  mutate(turnout = total / listanominal)

data_2017 <- data_2017 %>%
  mutate(year = 2017, month = "June", STATE = STATE_NAME)

collapsed_2017 <- data_2017
cat(paste0("  2017: ", nrow(collapsed_2017), " obs\n"))

###############################################################################
### COMBINE ALL YEARS
###############################################################################
cat("\nCombining all years...\n")

all_data <- bind_rows(
  collapsed_1996, collapsed_1999, collapsed_2002, collapsed_2005,
  collapsed_2008, collapsed_2011, collapsed_2014, collapsed_2017
)

cat(paste0("Total observations: ", nrow(all_data), "\n"))
cat(paste0("Years: ", paste(sort(unique(all_data$year)), collapse = ", "), "\n"))

# Save
dir.create(processed_path, showWarnings = FALSE, recursive = TRUE)
write.csv(all_data, file.path(processed_path, "Nayarit_process_raw_data.csv"))

