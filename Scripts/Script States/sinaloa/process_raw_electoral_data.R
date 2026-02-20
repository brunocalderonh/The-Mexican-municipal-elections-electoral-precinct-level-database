################################################################################
# SINALOA ELECTORAL DATA PROCESSING PIPELINE
# Complete Production-Ready R
# Years: 2001, 2004, 2007, 2010, 2013, 2016, 2018
################################################################################

rm(list = ls())
cat("\014")
options(max.print = 5000)
options(scipen = 10)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, haven, readxl, tidyverse, tidyr, openxlsx, data.table, 
               stringr, janitor)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(file.path(script_dir, ""))

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================
<<<<<<< HEAD
=======

remove_accents <- function(x) {
  x %>%
    str_replace_all("á|Á", "A") %>% str_replace_all("é|É", "E") %>%
    str_replace_all("í|Í", "I") %>% str_replace_all("ó|Ó", "O") %>%
    str_replace_all("ú|Ú", "U") %>% str_replace_all("ñ|Ñ", "N")
}

# Complete Sinaloa uniqueid mapping
assign_sinaloa_uniqueid <- function(municipality) {
  case_when(
    municipality == "AHOME" ~ 25001,
    municipality == "ANGOSTURA" ~ 25002,
    municipality == "BADIRAGUATO" ~ 25003,
    municipality == "CONCORDIA" ~ 25004,
    municipality == "COSALA" ~ 25005,
    municipality == "CULIACAN" ~ 25006,
    municipality == "CHOIX" ~ 25007,
    municipality == "ELOTA" ~ 25008,
    municipality == "ESCUINAPA" ~ 25009,
    municipality == "EL FUERTE" ~ 25010,
    municipality == "FUERTE" ~ 25010,
    municipality == "GUASAVE" ~ 25011,
    grepl("MAZATL", municipality) ~ 25012,
    municipality == "MOCORITO" ~ 25013,
    municipality == "ROSARIO" ~ 25014,
    municipality == "EL ROSARIO" ~ 25014,
    municipality == "SALVADOR ALVARADO" ~ 25015,
    municipality == "SAN IGNACIO" ~ 25016,
    municipality == "SINALOA" ~ 25017,
    municipality == "NAVOLATO" ~ 25018,
    TRUE ~ NA_real_
  )
}

##############################################################################
# 1) Ayu_Seccion_2001_No_LN.csv
##############################################################################
df_2001 <- read_csv("../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2001_No_LN.csv") 
colnames(df_2001) <- tolower(colnames(df_2001))
names(df_2001) <- gsub("[- ]", "", names(df_2001))
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

remove_accents <- function(x) {
  x %>%
    str_replace_all("á|Á", "A") %>% str_replace_all("é|É", "E") %>%
    str_replace_all("í|Í", "I") %>% str_replace_all("ó|Ó", "O") %>%
    str_replace_all("ú|Ú", "U") %>% str_replace_all("ñ|Ñ", "N")
}

# Complete Sinaloa uniqueid mapping
assign_sinaloa_uniqueid <- function(municipality) {
  case_when(
    municipality == "AHOME" ~ 25001,
    municipality == "ANGOSTURA" ~ 25002,
    municipality == "BADIRAGUATO" ~ 25003,
    municipality == "CONCORDIA" ~ 25004,
    municipality == "COSALA" ~ 25005,
    municipality == "CULIACAN" ~ 25006,
    municipality == "CHOIX" ~ 25007,
    municipality == "ELOTA" ~ 25008,
    municipality == "ESCUINAPA" ~ 25009,
    municipality == "EL FUERTE" ~ 25010,
    municipality == "FUERTE" ~ 25010,
    municipality == "GUASAVE" ~ 25011,
    grepl("MAZATL", municipality) ~ 25012,
    municipality == "MOCORITO" ~ 25013,
    municipality == "ROSARIO" ~ 25014,
    municipality == "EL ROSARIO" ~ 25014,
    municipality == "SALVADOR ALVARADO" ~ 25015,
    municipality == "SAN IGNACIO" ~ 25016,
    municipality == "SINALOA" ~ 25017,
    municipality == "NAVOLATO" ~ 25018,
    TRUE ~ NA_real_
  )
}

################################################################################
# 2001 PROCESSING - JOHN lines 8-150
################################################################################

df_2001 <- read_csv(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2001_No_LN.csv",
  show_col_types = FALSE
)

names(df_2001) <- tolower(names(df_2001))

df_2001 <- df_2001 %>%
<<<<<<< HEAD
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  mutate(across(c(pan, pri, prd, pt, pvem, pc, cdppn, psn, psd, pas, pmp, total), as.numeric)) %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  rename(
    PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc,
    CDPPN = cdppn, PSN = psn, PSD = psd, PAS = pas, PartidoMexicoPosible = pmp
=======
  # e.g., sum pan prd pvem panprdpvem if panprdpvem!=.
  mutate(
    total = rowSums(pick(any_of(vars_2001)), na.rm = TRUE),
    panprdpvem = ifelse(!is.na(panprdpvem),
                        panprdpvem + coalesce(pan,0) + coalesce(prd,0) + coalesce(pvem,0),
                        panprdpvem),
    pan        = ifelse(!is.na(panprdpvem), 0, pan),
    prd        = ifelse(!is.na(panprdpvem), 0, prd),
    pvem       = ifelse(!is.na(panprdpvem), 0, pvem)
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  ) %>%
  mutate(
    uniqueid = assign_sinaloa_uniqueid(municipality),
    valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, CDPPN, PSN, PSD, PAS, PartidoMexicoPosible), na.rm = TRUE),
    year = 2001, month = "November", STATE = "SINALOA"
  )

cat("2001:", nrow(df_2001), "rows\n")

################################################################################
# 2004 PROCESSING - JOHN lines 154-350
################################################################################

df_2004 <- read_csv(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2004_No_LN.csv",
  show_col_types = FALSE
)

<<<<<<< HEAD
names(df_2004) <- tolower(names(df_2004))
=======
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
  arrange(section) %>% 
  select(-distrito)

##############################################################################
# 2) Ayu_Seccion_LN_2004.xlsx
##############################################################################
df_ln_2004 <- read_excel("../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_LN_2004.xlsx", 
                         sheet="Sheet1", 
                         col_names=TRUE)
names(df_ln_2004) <- gsub("[- ]", "", names(df_ln_2004))

df_ln_2004 <-df_ln_2004 %>%
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
df_2004 <- read_csv("../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2004_No_LN.csv") 
colnames(df_2004) <- tolower(colnames(df_2004))
names(df_2004) <- gsub("[- ]", "", names(df_2004))
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

df_2004 <- df_2004 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  mutate(across(where(is.numeric), as.numeric))

# Handle PRI-PANAL coalition per JOHN
df_2004 <- df_2004 %>%
  mutate(
<<<<<<< HEAD
    PRI_PANAL = if_else(!is.na(pripanal), pri + panal + pripanal, as.numeric(NA)),
    pri = if_else(!is.na(pripanal), 0, pri),
    panal = if_else(!is.na(pripanal), 0, panal)
=======
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
    pc      = ifelse(!is.na(prdptpc),0, pc),
    total = rowSums(pick(any_of(vars_2004)), na.rm = TRUE)
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  )

df_2004 <- df_2004 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  rename(
    PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc, PANAL = panal
  ) %>%
  mutate(
    PRI_PANAL = coalesce(PRI_PANAL, 0),
    uniqueid = assign_sinaloa_uniqueid(municipality),
    valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, PANAL, PRI_PANAL), na.rm = TRUE),
    year = 2004, month = "October", STATE = "SINALOA"
  )

cat("2004:", nrow(df_2004), "rows\n")

<<<<<<< HEAD
################################################################################
# 2007 PROCESSING - JOHN lines 359-510
################################################################################
=======
df_2004 <- df_2004 %>%
  mutate(
    year  = 2004,
    month = "November"
  ) %>%
  arrange(section) %>% 
  select(-distrito)
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

df_2007 <- read_csv(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2007_No_LN.csv",
  show_col_types = FALSE
)

<<<<<<< HEAD
names(df_2007) <- tolower(names(df_2007))
=======
##############################################################################
# 4) Ayu_Seccion_LN_2007.xlsx
##############################################################################
df_ln_2007 <- read_excel("../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_LN_2007.xlsx", sheet="Sheet1", col_names=TRUE) 

names(df_ln_2007) <- gsub("[- ]", "", names(df_ln_2007))

df_ln_2007 <- df_ln_2007 %>%
  filter(!is.na(SECCIÓN)) %>%
  rename(section=SECCIÓN, listanominal=LISTANOMINAL) %>%
  mutate(missing = (listanominal==0))


df_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  filter(state=="SINALOA", month=="Speptember", year==2007) %>%
  select(section, lista)

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
df_2007 <- read_csv("../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2007_No_LN.csv")
colnames(df_2007) <- tolower(colnames(df_2007))
names(df_2007) <- gsub("[- ]", "", names(df_2007))

df_2007 <- df_2007 %>% 
  rename(municipality=municipio, section=seccion) %>%
  filter(!(municipality=="" & is.na(section)))

vars_2007 <- c("pan","pripanal","prd","pt","pvem","pc","pas","noregistrados",
               "nulos","pripanalprd","prdptpc","prdpt","pripanalpc","ptpas")

for (v in vars_2007) {
  if (v %in% names(df_2007)) {
    df_2007[[v]] <- as.numeric(df_2007[[v]])
  }
}

>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

df_2007 <- df_2007 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  mutate(across(where(is.numeric), as.numeric))

# Process coalitions per JOHN
df_2007 <- df_2007 %>%
  mutate(
<<<<<<< HEAD
    # PRI-PANAL-PRD coalition
    pripanalprd = if_else(!is.na(pripanalprd), pripanalprd + coalesce(pripanal, 0) + coalesce(prd, 0), pripanalprd),
    pripanal = if_else(!is.na(pripanalprd), 0, pripanal),
    prd_temp = if_else(!is.na(pripanalprd), 0, prd),
    # PRD-PT-PC coalition
    prdptpc = if_else(!is.na(prdptpc), prdptpc + coalesce(prd, 0) + coalesce(pt, 0) + coalesce(pc, 0), prdptpc),
    prd = if_else(!is.na(prdptpc), 0, prd),
    pt_temp = if_else(!is.na(prdptpc), 0, pt),
    pc_temp = if_else(!is.na(prdptpc), 0, pc),
    # PRD-PT coalition
    prdpt = if_else(!is.na(prdpt), prdpt + coalesce(prd, 0) + coalesce(pt, 0), prdpt),
    # PRI-PANAL-PC coalition
    pripanalpc = if_else(!is.na(pripanalpc), pripanalpc + coalesce(pripanal, 0) + coalesce(pc, 0), pripanalpc),
    # PT-PAS coalition
    ptpas = if_else(!is.na(ptpas), ptpas + coalesce(pt, 0) + coalesce(pas, 0), ptpas)
=======
    total = rowSums(pick(any_of(vars_2007)), na.rm = TRUE),
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
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  )

df_2007 <- df_2007 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

# Calculate total
df_2007 <- df_2007 %>%
  mutate(total = rowSums(select(., pan, pripanal, prd, pt, pvem, pc, pas, pripanalprd, prdptpc, prdpt, pripanalpc, ptpas), na.rm = TRUE)) %>%
  filter(total > 0)

# Rename columns
df_2007 <- df_2007 %>%
  rename(
    PAN = pan, PRI_PANAL = pripanal, PRD = prd, PT = pt, PVEM = pvem, PC = pc, PAS = pas,
    PRI_PRD_PANAL = pripanalprd, PRD_PT_PC = prdptpc, PRD_PT = prdpt, 
    PRI_PC_PANAL = pripanalpc, PT_PAS = ptpas
  ) %>%
  mutate(
<<<<<<< HEAD
    uniqueid = assign_sinaloa_uniqueid(municipality),
    valid = rowSums(select(., PAN, PRI_PANAL, PRD, PT, PVEM, PC, PAS, 
                           PRI_PRD_PANAL, PRD_PT_PC, PRD_PT, PRI_PC_PANAL, PT_PAS), na.rm = TRUE),
    year = 2007, month = "October", STATE = "SINALOA"
  )

cat("2007:", nrow(df_2007), "rows\n")

################################################################################
# 2010 PROCESSING - JOHN lines 514-590
# NOTE: Fixed valid calculation issue
################################################################################

df_2010 <- read_csv(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2010.csv",
  show_col_types = FALSE
)
=======
    turnout = total/listanominal,
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
  )  %>%
  mutate(
    year  = 2007,
    month = "October"
  ) %>%
  arrange(section) %>% 
  select(-DISTRITO)

################################################################################
# 2010 PROCESSING - JOHN lines 514-590
# NOTE: Fixed valid calculation issue
################################################################################

df_2010 <- read_csv(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2010.csv",
  show_col_types = FALSE
) %>% 
  rename_with(~ str_remove_all(., "-"))
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

names(df_2010) <- tolower(names(df_2010))

df_2010 <- df_2010 %>%
<<<<<<< HEAD
  rename(municipality = municipio, section = seccion) %>%
=======
  rename(municipality = nombre_municipio, section = seccion, listanominal = lista_nominal,
         noregistrados = "no registrados") %>%
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  mutate(across(c(listanominal, panprdptpc, pripvempanal, noregistrados, nulos, total), as.numeric))

df_2010 <- df_2010 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  rename(
    PAN_PRD_PT_PC = panprdptpc,
    PRI_PVEM_PANAL = pripvempanal
  ) %>%
<<<<<<< HEAD
  mutate(
    turnout = total / listanominal,
    uniqueid = assign_sinaloa_uniqueid(municipality),
    # FIXED: valid should include both coalitions
    valid = rowSums(select(., PAN_PRD_PT_PC, PRI_PVEM_PANAL), na.rm = TRUE),
    year = 2010, month = "July", STATE = "SINALOA"
  ) %>%
  select(-noregistrados, -nulos)

cat("2010:", nrow(df_2010), "rows\n")

################################################################################
# 2013 PROCESSING - JOHN lines 595-643
################################################################################

df_2013 <- read_dta(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2013.dta"
)

df_2013 <- df_2013 %>%
  rename(municipality = Municipio, section = Seccion, total = Total, listanominal = ListaNominal) %>%
  mutate(
    turnout = total / listanominal,
    uniqueid = assign_sinaloa_uniqueid(municipality)
  ) %>%
  select(-any_of(c("NoRegistrados", "Nulos")))

# Rename party columns to match expected format
if ("PAN_PRD_PT" %in% names(df_2013)) {
  df_2013 <- df_2013 %>%
    mutate(
      valid = rowSums(select(., any_of(c("PAN_PRD_PT", "PRI_PVEM_PANAL", "PAS", "PC", "PRI_PVEM_PANAL_PAS"))), na.rm = TRUE),
      year = 2013, month = "July", STATE = "SINALOA"
    )
}

cat("2013:", nrow(df_2013), "rows\n")

################################################################################
# 2016 PROCESSING - SALVADOR lines 10-165
# Excel file with multiple sheets
################################################################################

xlsx_path_2016 <- "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018/Ayuntamientos_2016.xlsx"
sheets_2016 <- excel_sheets(xlsx_path_2016)

df_2016_list <- lapply(sheets_2016, function(sheet) {
  tryCatch({
    temp <- read_excel(xlsx_path_2016, sheet = sheet, col_types = "text") %>%
      mutate(municipality = sheet) %>%
      filter(!is.na(`CASILLASECCIÓN`), `CASILLASECCIÓN` != "") %>%
      mutate(across(everything(), ~replace(., . == "", "0")))
    return(temp)
  }, error = function(e) return(NULL))
})

df_2016 <- bind_rows(df_2016_list) %>%
  mutate(across(-municipality, as.numeric)) %>%
  filter(!is.na(`CASILLASECCIÓN`))

# Rename columns per SALVADOR
df_2016 <- df_2016 %>%
  rename(
    section = `CASILLASECCIÓN`,
    no_reg = `VOTOSACANDIDATOSNOREGISTRADO`,
    nulo = `VOTOSNULOS`,
    total = `VOTACIÓNTOTAL`
  ) %>%
  rename_with(~gsub("MC_MAS", "MC_PAS", .)) %>%
  rename_with(~gsub("^MAS$", "PAS", .))

# Assign uniqueid
df_2016 <- df_2016 %>%
  mutate(
    uniqueid = assign_sinaloa_uniqueid(municipality),
    municipality = if_else(municipality == "FUERTE", "EL FUERTE", municipality)
  )

# Process coalitions per SALVADOR
if (all(c("PRI_PANAL", "PRI", "PANAL") %in% names(df_2016))) {
  df_2016 <- df_2016 %>%
    mutate(
      PRI_PANAL = if_else(!is.na(PRI_PANAL), PRI + PANAL + PRI_PANAL, PRI_PANAL),
      PRI = if_else(!is.na(PRI_PANAL), NA_real_, PRI),
      PANAL = if_else(!is.na(PRI_PANAL), NA_real_, PANAL)
    )
}

if (all(c("MC_PAS", "MC", "PAS") %in% names(df_2016))) {
  df_2016 <- df_2016 %>%
    mutate(
      MC_PAS = if_else(!is.na(MC_PAS), MC + PAS + MC_PAS, MC_PAS),
      MC = if_else(!is.na(MC_PAS), NA_real_, MC),
      PAS = if_else(!is.na(MC_PAS), NA_real_, PAS)
    )
}

# Collapse
df_2016 <- df_2016 %>%
  select(-any_of(c("no_reg", "nulo"))) %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

# Calculate valid
party_cols_2016 <- c("PAN", "PRI", "PRD", "PT", "MC", "PANAL", "PAS", "MORENA", 
                     "PRI_PANAL", "MC_PAS", "PVEM", "PES", "CI_1", "CI_2", "CI_3")

df_2016 <- df_2016 %>%
  mutate(
    valid = rowSums(select(., any_of(party_cols_2016)), na.rm = TRUE),
    year = 2016, month = "June", STATE = "SINALOA"
  )

cat("2016:", nrow(df_2016), "rows\n")

################################################################################
# 2018 PROCESSING - SALVADOR lines 193-303
################################################################################

df_2018 <- read_excel(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018/02 Resultados_Ayuntamientos_Sinaloa_2018_Casilla.xlsx",
  sheet = "2018_SEE_AYUN_SIN_CAS"
)

df_2018 <- df_2018 %>%
  rename(section = SECCION, listanominal = LISTA, total = TOTAL) %>%
  filter(ESTATUS != "Paquete no entregado") %>%
  mutate(municipality = toupper(MUNICIPIO))

# Process coalitions per SALVADOR
# PAN-PRD-MC-PAS coalition
df_2018 <- df_2018 %>%
  mutate(
    PAN_PRD_MC_PAS = rowSums(select(., any_of(c("PAN", "PRD", "MC", "PAS", 
      "C_PAN_PRD_MC_PAS", "C_PAN_PRD_MC", "C_PAN_PRD_PAS", "C_PAN_MC_PAS", "C_PRD_MC_PAS",
      "C_PAN_PRD", "C_PAN_MC", "C_PAN_PAS", "C_PRD_MC", "C_PRD_PAS", "C_MC_PAS"))), na.rm = TRUE)
  ) %>%
  select(-any_of(c("PAN", "PRD", "MC", "PAS", "C_PAN_PRD_MC_PAS", "C_PAN_PRD_MC", 
                   "C_PAN_PRD_PAS", "C_PAN_MC_PAS", "C_PRD_MC_PAS", "C_PAN_PRD", 
                   "C_PAN_MC", "C_PAN_PAS", "C_PRD_MC", "C_PRD_PAS", "C_MC_PAS")))

# PT-MORENA-PES coalition (not in specific municipalities)
no_pt_morena_pes <- c("CONCORDIA", "ROSARIO", "ESCUINAPA")
df_2018 <- df_2018 %>%
  mutate(
    PT_MORENA_PES = if_else(
      !(municipality %in% no_pt_morena_pes),
      rowSums(select(., any_of(c("PT", "MORENA", "ES", "C_PT_MORENA_ES", 
                                  "C_PT_MORENA", "C_PT_ES", "C_MORENA_ES"))), na.rm = TRUE),
      NA_real_
    ),
    PT = if_else(!(municipality %in% no_pt_morena_pes), NA_real_, PT),
    MORENA = if_else(!(municipality %in% no_pt_morena_pes), NA_real_, MORENA)
  ) %>%
  rename(PES = ES) %>%
  mutate(PES = if_else(!(municipality %in% c("SAN IGNACIO", "ROSARIO", "ESCUINAPA")), NA_real_, PES)) %>%
  select(-any_of(c("C_PT_MORENA_ES", "C_PT_MORENA", "C_PT_ES", "C_MORENA_ES")))
=======
  mutate(
    turnout = total / listanominal,
    uniqueid = assign_sinaloa_uniqueid(municipality),
    # FIXED: valid should include both coalitions
    valid = rowSums(select(., PAN_PRD_PT_PC, PRI_PVEM_PANAL), na.rm = TRUE),
    year = 2010, month = "July", STATE = "SINALOA"
  ) %>%
  select(-noregistrados, -nulos)

cat("2010:", nrow(df_2010), "rows\n")
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# PRI-PVEM-PANAL coalition
no_pri_pvem_panal <- c("SAN IGNACIO", "ROSARIO", "ESCUINAPA")
df_2018 <- df_2018 %>%
  rename(PANAL = `NA`) %>%
  mutate(
    PRI_PVEM_PANAL = if_else(
      !(municipality %in% no_pri_pvem_panal),
      rowSums(select(., any_of(c("PRI", "PVEM", "PANAL", "CC_PRI_PVEM_NA", 
                                  "CC_PRI_PVEM", "CC_PRI_NA", "CC_PVEM_NA"))), na.rm = TRUE),
      NA_real_
    ),
    PRI = if_else(!(municipality %in% no_pri_pvem_panal), NA_real_, PRI),
    PVEM = if_else(!(municipality %in% no_pri_pvem_panal), NA_real_, PVEM),
    PANAL = if_else(!(municipality %in% no_pri_pvem_panal), NA_real_, PANAL)
  ) %>%
  select(-any_of(c("CC_PRI_PVEM_NA", "CC_PRI_PVEM", "CC_PRI_NA", "CC_PVEM_NA")))

# Rename independent candidates
df_2018 <- df_2018 %>%
  rename_with(~gsub("CAND_IND", "CI_", .))

# Assign uniqueid
df_2018 <- df_2018 %>%
  mutate(uniqueid = assign_sinaloa_uniqueid(municipality))

# Collapse
df_2018 <- df_2018 %>%
  select(-any_of(c("MUNICIPIO", "CASILLA", "ESTATUS", "CNR", "NULOS"))) %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

<<<<<<< HEAD
# Calculate valid
party_cols_2018 <- c("PRI", "PVEM", "PANAL", "PES", "PAIS", "PAN_PRD_MC_PAS", 
                     "PT_MORENA_PES", "PRI_PVEM_PANAL", "CI_1", "CI_2", "CI_3", "CI_4")
=======
for (sname in sheet_names_2016) {
  df_sheet <- read_excel(excel_2016, sheet=sname, col_types="text") %>%
    mutate(municipality = sname) %>%
    filter(`CASILLA (SECCIÓN)` != "") %>%
    mutate(across(everything(), ~ ifelse(.=="", "0", .)))
  
  # Clean column names for Stata compatibility
  names(df_sheet) <- str_replace_all(names(df_sheet), "[^A-Za-z0-9_]", "_")
  
  # Shorten long column names to fit Stata's 32-character limit
  names(df_sheet) <- substr(names(df_sheet), 1, 32)
  
  # Make sure names are unique after truncation
  names(df_sheet) <- make.unique(names(df_sheet), sep = "_")
  
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
  } else {
    message(paste("File not found:", f))
  }
}

# Clean up - the column name is now CASILLA__SECCIÓN_ after cleaning
all_2016 <- all_2016 %>%
  mutate(across(-c(municipality), as.numeric))

# Find the actual casilla column name after cleaning
casilla_col <- names(all_2016)[str_detect(names(all_2016), "CASILLA.*SECCI")]

all_2016 <- all_2016 %>%
  filter(!is.na(!!sym(casilla_col))) %>%
  rename(
    section = !!sym(casilla_col),
    no_reg  = "VOTOS_A_CANDIDATOS_NO_REGISTRADO",
    nulo    = "VOTOS_NULOS",
    total   = "VOTACI_N_TOTAL"
  ) %>%
  mutate(
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
      TRUE                              ~ NA
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

# Now merging LN2016 => replicate partial
df_ln2016 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2016/LN2016.dta") %>%
  filter(entidad==25, month==5, seccion!=0) %>%
  select(uniqueid, section=seccion, lista)

all_2016 <- all_2016 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(everything(), ~sum(.x, na.rm=TRUE)), .groups="drop")

all_2016 <- all_2016 %>%
  left_join(df_ln2016, by=c("section", "uniqueid")) %>%
  filter(!is.na(lista)) %>%
  rename(listanominal=lista)

all_2016 <- all_2016 %>%
  mutate(
    year  = 2016,
    month = "June",
    STATE = "SINALOA",
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
df_2018 <- read_excel("../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/02 Resultados_Ayuntamientos_Sinaloa_2018_Casilla.xlsx",
                      sheet="2018_SEE_AYUN_SIN_CAS", col_names=TRUE) %>%
  rename(section=SECCION, listanominal=LISTA_NOMINAL, total=TOTAL_VOTOS) %>%
  filter(ESTATUS_ACTA!="Paquete no entregado") %>%
  mutate(
    municipality = str_to_upper(MUNICIPIO)
  )

# aggregator merges => omitted
# partial coalition merges are done, skipping aggregator by municipality.
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

df_2018 <- df_2018 %>%
  mutate(
    valid = rowSums(select(., any_of(party_cols_2018)), na.rm = TRUE),
    turnout = total / listanominal,
    year = 2018, month = "July", STATE = "SINALOA"
  )

cat("2018:", nrow(df_2018), "rows\n")

################################################################################
# FINAL APPEND AND OUTPUT
################################################################################

# Combine all years
sinaloa_all <- bind_rows(df_2001, df_2004, df_2007, df_2010, df_2013, df_2016, df_2018)

# Final corrections per SALVADOR
sinaloa_all <- sinaloa_all %>%
  mutate(
<<<<<<< HEAD
    municipality = if_else(municipality == "EL ROSARIO", "ROSARIO", municipality),
    municipality = if_else(uniqueid == 25012, "MAZATLAN", municipality)
=======
    PT_MORENA_PES = ifelse(municipality!="CONCORDIA" & municipality!="ROSARIO" & municipality!="ESCUINAPA",
                           PT + MORENA + ES + C_PT_MORENA_ES + C_PT_MORENA + C_PT_ES + C_MORENA_ES,
                           NA_real_)
  ) %>%
  rename(PES=ES) %>%
  select(-PT, -MORENA, -PES, -C_PT_MORENA_ES, -C_PT_MORENA, -C_PT_ES, -C_MORENA_ES)

df_2018 <- df_2018 %>%
  rename(PANAL="NA") %>%
  mutate(
    # create PRI_PVEM_PANAL
    PRI_PVEM_PANAL = ifelse(municipality!="SAN IGNACIO" & municipality!="ROSARIO" & municipality!="ESCUINAPA",
                            PRI + PVEM + PANAL + CC_PRI_PVEM_NA + CC_PRI_PVEM + CC_PRI_NA + CC_PVEM_NA,
                            NA_real_),
    PANAL = NA_real_
  ) %>%
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
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  )

# Order columns
sinaloa_all <- sinaloa_all %>%
  select(STATE, municipality, section, uniqueid, year, month, everything())

cat("\nFinal dataset:", nrow(sinaloa_all), "rows\n")
cat("Years:", paste(sort(unique(sinaloa_all$year)), collapse = ", "), "\n")

<<<<<<< HEAD
# Write output
data.table::fwrite(sinaloa_all, "../../../Processed Data/sinaloa/sinaloa_process_raw_data.csv")
=======
#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

# Load the 2021 dataset from the excel
data_2021 <- read_excel("../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/21/AYUNTAMIENTOS_21.xlsx")

# Rename columns
data_2021 <- data_2021 %>%
  dplyr::rename(municipality = MUNICIPIO_LOCAL,
                section = SECCION,
                total = TOTAL_VOTOS,
                no_reg = NO_REGISTRADOS,
                nulos = NULOS,
                CI_1 = CAND_IND_IZL,
                CI_2 = CAND_IND_JSLS,
                FXM = FxM) %>%
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
data_2021 <- data_2021 %>% 
  mutate(
    uniqueid=case_when(
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
      TRUE                              ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(PAN:total), 
           \(x) sum(x, na.rm = TRUE))
  )

# Load the Lista Nominal 2021 data and filter by criteria
ln_2021 <- read_excel("../../../Data/Raw Electoral Data/Listas Nominales/listanom_pef21.xlsx", skip = 3, 
                      col_names = c("state_code", "district_code", "mun_code", 
                                    "section", "col_e", "col_f", "col_g", "col_h", 
                                    "col_i", "col_j", "col_k", "col_l",
                                    "listanominal", "col_n", "col_o", "col_p")) %>%
  dplyr::select(state_code, mun_code, section, listanominal) %>% 
  dplyr::filter(state_code == 25) %>%
  dplyr::select(section,listanominal)


# Merge Lista Nominal data with the collapsed data
collapsed_2021 <- collapsed_2021 %>%
  left_join(ln_2021, by = "section")

# Calculate valid votes and final details
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(PAN:CC_MORENA_PAS), na.rm = TRUE),
    year = 2021,
    month = "June"
  )

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 25) %>% 
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
data_2024 <- read_csv("../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/24/SIN_AYUNTAMIENTO_2024.csv")

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(municipality = MUNICIPIO_LOCAL,
                section = SECCION,
                total = TOTAL_VOTOS,
                no_reg = VCN,
                nulos = VN,
                CI_1 = MAG,
                CI_2 = JSLS,
                CI_3 = VMSA) %>%
  rename_with(~ gsub("C_", "", .x), starts_with("C_")) %>%
  dplyr::mutate(
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    across(PAN:total, as.numeric),
    section = as.numeric(section)
  ) %>% 
  dplyr::filter(section > 0)

# Assign uniqueids
data_2024 <- data_2024 %>% 
  mutate(
    uniqueid=case_when(
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
      municipality == "ELDORADO" ~ 25019,
      municipality == "JUAN JOSE RIOS" ~ 25020,
      TRUE                              ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(PAN:total), 
           \(x) sum(x, na.rm = TRUE))
  )

# Load the Lista Nominal 2024 data and filter by criteria
ln_2024 <- read_excel("../../../Data/Raw Electoral Data/Listas Nominales/listanom_pef24.xlsx", skip = 2, 
                      col_names = c("state_code", "district_code", "mun_code", 
                                    "section", "col_e", "col_f", "col_g", "col_h", 
                                    "col_i", "col_j", "col_k", "listanominal")) %>%
  dplyr::select(state_code, mun_code, section, listanominal) %>% 
  dplyr::filter(state_code == 25) %>%
  dplyr::select(section,listanominal)

# Merge Lista Nominal data with the collapsed data
collapsed_2024 <- collapsed_2024 %>%
  left_join(ln_2024, by = "section")

# Calculate valid votes and final details
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(PAN:CC_PVEM_MORENA), na.rm = TRUE),
    year = 2024,
    month = "June"
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

df_combo <- bind_rows(df_2001, df_2004, df_2007, df_2010, df_2013, all_2016, df_2018, collapsed_2021, collapsed_2024) %>%
  mutate(
    municipality = case_when(
      municipality=="EL ROSARIO" ~ "ROSARIO",
      uniqueid==25012           ~ "MAZATLAN",
      TRUE ~ municipality
    )
  )

data.table::fwrite(df_combo,"../../../Processed Data/sinaloa/sinaloa_process_raw_data.csv")
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

cat("Output saved to: ../../../Processed Data/sinaloa/sinaloa_process_raw_data.csv\n")
