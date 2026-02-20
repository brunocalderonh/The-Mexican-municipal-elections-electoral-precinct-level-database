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
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  mutate(across(c(pan, pri, prd, pt, pvem, pc, cdppn, psn, psd, pas, pmp, total), as.numeric)) %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  rename(
    PAN = pan, PRI = pri, PRD = prd, PT = pt, PVEM = pvem, PC = pc,
    CDPPN = cdppn, PSN = psn, PSD = psd, PAS = pas, PartidoMexicoPosible = pmp
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

names(df_2004) <- tolower(names(df_2004))

df_2004 <- df_2004 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality == "" & is.na(section)), !is.na(total), total != 0) %>%
  mutate(across(where(is.numeric), as.numeric))

# Handle PRI-PANAL coalition per JOHN
df_2004 <- df_2004 %>%
  mutate(
    PRI_PANAL = if_else(!is.na(pripanal), pri + panal + pripanal, as.numeric(NA)),
    pri = if_else(!is.na(pripanal), 0, pri),
    panal = if_else(!is.na(pripanal), 0, panal)
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

################################################################################
# 2007 PROCESSING - JOHN lines 359-510
################################################################################

df_2007 <- read_csv(
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2007_No_LN.csv",
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
  "../../../Data/Raw Electoral Data/Sinaloa - 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2010.csv",
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

# Combine all years
sinaloa_all <- bind_rows(df_2001, df_2004, df_2007, df_2010, df_2013, df_2016, df_2018)

# Final corrections per SALVADOR
sinaloa_all <- sinaloa_all %>%
  mutate(
    municipality = if_else(municipality == "EL ROSARIO", "ROSARIO", municipality),
    municipality = if_else(uniqueid == 25012, "MAZATLAN", municipality)
  )

# Order columns
sinaloa_all <- sinaloa_all %>%
  select(STATE, municipality, section, uniqueid, year, month, everything())

cat("\nFinal dataset:", nrow(sinaloa_all), "rows\n")
cat("Years:", paste(sort(unique(sinaloa_all$year)), collapse = ", "), "\n")

# Write output
data.table::fwrite(sinaloa_all, "../../../Processed Data/sinaloa/sinaloa_process_raw_data.csv")

cat("Output saved to: ../../../Processed Data/sinaloa/sinaloa_process_raw_data.csv\n")
