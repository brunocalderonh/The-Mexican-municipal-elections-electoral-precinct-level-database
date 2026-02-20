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

data_1996 <- data_1996 %>%
  mutate(turnout = total / listanominal)

# --- Drop noregistrados, nulos ---
data_1996 <- data_1996 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

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

# --- Drop noregistrados, nulos ---
data_2014 <- data_2014 %>%
  select(-any_of(c("noregistrados", "nulos", "Nulos", "NoRegistrados",
                   "NOREGISTRADOS", "NULOS", "no_reg", "nulo")))

# --- Assign uniqueid ---
data_2014 <- data_2014 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

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

# --- Assign uniqueid ---
data_2017 <- data_2017 %>%
  mutate(uniqueid = assign_uniqueid(municipality))

# --- Valid votes ---
data_2017 <- data_2017 %>%
  mutate(valid = rowSums(across(c(PAN_PRD_PT_PRS, PRI, PVEM, MC, PANAL, MORENA, PES, CI_1, CI_2, CI_3)), na.rm = TRUE))

data_2017 <- data_2017 %>%
  mutate(year = 2017, month = "June")

collapsed_2017 <- data_2017
cat(paste0("  2017: ", nrow(collapsed_2017), " obs\n"))


###############################################################################
### COMBINE ALL YEARS
###############################################################################
cat("Combining all years...\n")

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