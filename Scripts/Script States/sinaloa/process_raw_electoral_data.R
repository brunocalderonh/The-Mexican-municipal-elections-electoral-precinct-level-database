################################################################################
# SINALOA ELECTORAL DATA PROCESSING PIPELINE
# Complete Production-Ready R
# Years: 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024
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
# 2001 PROCESSING - JOHN lines 8-210
# JOHN does 12+ coalition absorptions then valid = rowtotal of all party + coalition columns.
# Since sum(absorbed) = sum(original), we skip absorption and include ALL columns in valid.
################################################################################

df_2001 <- read_csv(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2001_No_LN.csv",
  show_col_types = FALSE
)

names(df_2001) <- tolower(names(df_2001))

df_2001 <- df_2001 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  mutate(across(where(is.character), ~suppressWarnings(as.numeric(.)))) %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(total = rowSums(select(., any_of(c(
    "pan","pri","prd","pt","pvem","cdppn","psn","pas","pbs",
    "noregistrados","nulos",
    "panprdpvem","panpvem","prdpt","ptpsn","psnpbs",
    "ptpascdppnpsn","prdcdppn","panprd","ptpbs",
    "panprdptpvem","ptcdppnpsn","prdpvem","ptcdppnpbs"))), na.rm = TRUE)) %>%
  filter(total > 0) %>%
  rename(
    PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem,
    PC = cdppn, PSN = psn, PAS = pas, PBS = pbs,
    PAN_PRD_PVEM = panprdpvem, PAN_PVEM = panpvem, PRD_PT = prdpt,
    PT_PSN = ptpsn, PSN_PBS = psnpbs, PT_PAS_PC_PSN = ptpascdppnpsn,
    PRD_PC = prdcdppn, PAN_PRD = panprd, PT_PBS = ptpbs,
    PAN_PRD_PT_PVEM = panprdptpvem, PT_PC_PSN = ptcdppnpsn,
    PRD_PVEM = prdpvem, PT_PC_PBS = ptcdppnpbs
  ) %>%
  select(-any_of(c("noregistrados", "nulos", "psd", "pmp"))) %>%
  mutate(
    uniqueid = assign_sinaloa_uniqueid(municipality),
    valid = rowSums(select(., any_of(c(
      "PAN","PRI","PRD","PT","PVEM","PC","PSN","PAS","PBS",
      "PAN_PRD_PVEM","PAN_PVEM","PRD_PT","PT_PSN","PSN_PBS",
      "PT_PAS_PC_PSN","PRD_PC","PAN_PRD","PT_PBS",
      "PAN_PRD_PT_PVEM","PT_PC_PSN","PRD_PVEM","PT_PC_PBS"))), na.rm = TRUE),
    year = 2001, month = "November", STATE = "SINALOA"
  )

cat("2001:", nrow(df_2001), "rows\n")

################################################################################
# 2004 PROCESSING - JOHN lines 154-350
################################################################################

df_2004 <- read_csv(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2004_No_LN.csv",
  show_col_types = FALSE
)

names(df_2004) <- tolower(names(df_2004))

df_2004 <- df_2004 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  mutate(across(where(is.numeric), as.numeric)) %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

# Recalculate total per JOHN (includes noregistrados + nulos)
df_2004 <- df_2004 %>%
  mutate(total = rowSums(select(., any_of(c(
    "pan","pri","prd","pt","pvem","pc","pbs",
    "noregistrados","nulos",
    "prdpt","prdptpbs","prdpbs","panprd","pripvempbs","prdptpc"))), na.rm = TRUE)) %>%
  filter(total > 0)

# Rename per JOHN
df_2004 <- df_2004 %>%
  rename(
    PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc, PBS = pbs,
    PAN_PRD = panprd, PRD_PT = prdpt, PRD_PT_PC = prdptpc,
    PRD_PT_PBS = prdptpbs, PRD_PBS = prdpbs, PRI_PVEM_PBS = pripvempbs
  ) %>%
  select(-any_of(c("noregistrados", "nulos"))) %>%
  mutate(
    uniqueid = assign_sinaloa_uniqueid(municipality),
    valid = rowSums(select(., any_of(c(
      "PAN","PRI","PRD","PT","PVEM","PC","PBS",
      "PRD_PT","PRD_PT_PBS","PRD_PBS","PAN_PRD","PRI_PVEM_PBS","PRD_PT_PC"))), na.rm = TRUE),
    year = 2004, month = "November", STATE = "SINALOA"
  )

cat("2004:", nrow(df_2004), "rows\n")

################################################################################
# 2007 PROCESSING - JOHN lines 359-510
################################################################################

df_2007 <- read_csv(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2007_No_LN.csv",
  show_col_types = FALSE
)

names(df_2007) <- tolower(names(df_2007))

df_2007 <- df_2007 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  mutate(across(where(is.numeric), as.numeric))

# Process coalitions per JOHN
df_2007 <- df_2007 %>%
  mutate(
    # PRI-PANAL-PRD coalition (JOHN: replace pripanalprd = pripanalprd + pripanal + prd if pripanalprd!=.)
    pripanalprd = if_else(!is.na(pripanalprd), pripanalprd + coalesce(pripanal, 0) + coalesce(prd, 0), pripanalprd),
    pripanal = if_else(!is.na(pripanalprd), 0, pripanal),
    prd = if_else(!is.na(pripanalprd), 0, prd),
    # PRD-PT-PC coalition
    prdptpc = if_else(!is.na(prdptpc), prdptpc + coalesce(prd, 0) + coalesce(pt, 0) + coalesce(pc, 0), prdptpc),
    prd = if_else(!is.na(prdptpc), 0, prd),
    pt = if_else(!is.na(prdptpc), 0, pt),
    pc = if_else(!is.na(prdptpc), 0, pc),
    # PRD-PT coalition
    prdpt = if_else(!is.na(prdpt), prdpt + coalesce(prd, 0) + coalesce(pt, 0), prdpt),
    prd = if_else(!is.na(prdpt), 0, prd),
    pt = if_else(!is.na(prdpt), 0, pt),
    # PRI-PANAL-PC coalition
    pripanalpc = if_else(!is.na(pripanalpc), pripanalpc + coalesce(pripanal, 0) + coalesce(pc, 0), pripanalpc),
    pripanal = if_else(!is.na(pripanalpc), 0, pripanal),
    pc = if_else(!is.na(pripanalpc), 0, pc),
    # PT-PAS coalition
    ptpas = if_else(!is.na(ptpas), ptpas + coalesce(pt, 0) + coalesce(pas, 0), ptpas),
    pt = if_else(!is.na(ptpas), 0, pt),
    pas = if_else(!is.na(ptpas), 0, pas)
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
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2010.csv",
  show_col_types = FALSE
)

names(df_2010) <- tolower(names(df_2010))

df_2010 <- df_2010 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  mutate(across(c(listanominal, panprdptpc, pripvempanal, noregistrados, nulos, total), as.numeric))

df_2010 <- df_2010 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  rename(
    PAN_PRD_PT_PC = panprdptpc,
    PRI_PVEM_PANAL = pripvempanal
  ) %>%
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
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayu_Seccion_2013.dta"
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

xlsx_path_2016 <- "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/Ayuntamientos_2016.xlsx"
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
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018,2021,2024/02 Resultados_Ayuntamientos_Sinaloa_2018_Casilla.xlsx",
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

# Calculate valid
party_cols_2018 <- c("PRI", "PVEM", "PANAL", "PES", "PAIS", "PAN_PRD_MC_PAS", 
                     "PT_MORENA_PES", "PRI_PVEM_PANAL", "CI_1", "CI_2", "CI_3", "CI_4")

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

cat("\n2001-2018 processing complete.\n")

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
    valid = rowSums(across(PAN:CC_MORENA_PAS), na.rm = TRUE),
    year = 2021,
    month = "June",
    STATE = "SINALOA"
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
    valid = rowSums(across(PAN:CC_PVEM_MORENA), na.rm = TRUE),
    year = 2024,
    month = "June",
    STATE = "SINALOA"
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

df_combo <- bind_rows(df_2001, df_2004, df_2007, df_2010, df_2013, df_2016, df_2018, collapsed_2021, collapsed_2024) %>%
  mutate(
    municipality = case_when(
      municipality=="EL ROSARIO" ~ "ROSARIO",
      uniqueid==25012           ~ "MAZATLAN",
      TRUE ~ municipality
    )
  )

data.table::fwrite(df_combo,"../../../Processed Data/sinaloa/sinaloa_process_raw_data.csv")

cat("Output saved to: ../../../Processed Data/sinaloa/sinaloa_process_raw_data.csv\n")