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


################################################################################
# 1) Read CSV (equivalent to: insheet using "Ayu_Seccion_1997_No_LN.csv", clear)
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_1997_No_LN.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
################################################################################
# 2) Rename columns, drop empty municipality/section, and drop total == NA or 0
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 3) Convert (pan through total) to numeric (like "destring pan - total, replace")
################################################################################
df <- df %>%
  mutate(across(pan:total, as.numeric))

################################################################################
# 4) Collapse (sum) pan - total by municipality and section
#    (equivalent to: collapse (sum) pan - total, by(municipality section))
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    across(pan:total, sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 5) Rename columns 
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN             = pan,
    PRI             = pri,
    PRD             = prd,
    PT              = pt,
    PVEM            = pvem,
    PartCardenista  = pc,
    PPS             = pps,
    PDM             = pdm
  ) %>%
  select(-any_of("nulos"))  # drop nulos if it exists

################################################################################
# 6) Generate uniqueid = 0, then assign codes for each municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "BALANCAN"           ~ 27001,
      municipality == "CARDENAS"           ~ 27002,
      municipality == "CENTLA"             ~ 27003,
      municipality == "CENTRO"             ~ 27004,
      municipality == "COMALCALCO"         ~ 27005,
      municipality == "CUNDUACAN"          ~ 27006,
      municipality == "EMILIANO ZAPATA"    ~ 27007,
      municipality == "HUIMANGUILLO"       ~ 27008,
      municipality == "JALAPA"             ~ 27009,
      municipality == "JALPA DE MENDEZ"    ~ 27010,
      municipality == "JONUTA"             ~ 27011,
      municipality == "MACUSPANA"          ~ 27012,
      municipality == "NACAJUCA"           ~ 27013,
      municipality == "PARAISO"            ~ 27014,
      municipality == "TACOTALPA"          ~ 27015,
      municipality == "TEAPA"              ~ 27016,
      municipality == "TENOSIQUE"          ~ 27017,
      TRUE                                 ~ 0
    )
  )

################################################################################
# 7) Create 'valid' as rowtotal(...) of (PAN PRI PRD PartCardenista PT PVEM PPS PDM)
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PRD, PartCardenista, PT, PVEM, PPS, PDM),
      na.rm = TRUE
    )
  )

################################################################################
# 9) Merge with all_months_years.dta (like "capture merge 1:m ed seccion using ..."),
#    keep only month==12, year==1997, rename lista->listanominal
################################################################################

# We'll do something similar in R, reading the external .dta:
df_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  filter( state == "TABASCO") %>% 
  select(section, month, year, lista)

# merge 1:m on (ed, seccion)
df_merged <- df_collapsed %>%
  left_join(df_all, by = c("section"))

# keep only month==12 & year==1997
df_merged <- df_merged %>%
  filter(month == "December", year == 1997)

# after a left_join, no extra rows from "using". If we want to
# drop rows that didn't find a match (which would have NA in 'lista'),
# we do:
df_merged <- df_merged %>% filter(!is.na(lista))

# drop _merge, ed, seccion, year, month
df_merged <- df_merged %>%
  select(-year, -month)

# rename lista -> listanominal
df_merged <- df_merged %>%
  rename(listanominal = lista)

################################################################################
# 10) turnout = total / listanominal
################################################################################
df_merged <- df_merged %>%
  mutate(turnout = total / listanominal)

################################################################################
# 14) year=1997, month="November"
################################################################################
df_1997 <- df_merged %>%
  mutate(
    year  = 1997,
    month = "November"
  )

################################################################################
# 1) Read CSV (equivalent to "insheet using Ayu_Seccion_2000_No_LN.csv, clear")
################################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2000_No_LN.csv", show_col_types = FALSE)
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns, drop rows where municipality=="" & section==. (NA),
#    and drop rows where total is NA or 0
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = seccion
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 3) Convert (pan through total) to numeric 
################################################################################
df <- df %>%
  mutate(across(pan:total, as.numeric))

################################################################################
# 4) Collapse (sum) pan - total by (municipality, section)
#    (like "collapse (sum) pan - total, by(municipality section)")
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(pan:total, sum, na.rm = TRUE), .groups = "drop")

################################################################################
# 5) Rename columns to their final form
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN  = pan,
    PRI  = pri,
    PRD  = prd,
    PT   = pt,
    PVEM = pvem,
    PC   = cdppn,  # cdppn -> PC
    PCD  = pcd,
    PDS  = dsppn,  # dsppn -> PDS (Partido Democracia Social)
    PAS  = pas,
    PSN  = psn,
    PARM = parm
  )

################################################################################
# 6) Drop columns 'nulos' and 'noregistrados' if they exist
################################################################################
df_collapsed <- df_collapsed %>%
  select(-any_of(c("nulos", "noregistrados")))

################################################################################
# 7) Create uniqueid = 0, then replace based on municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "BALANCAN"         ~ 27001,
      municipality == "CARDENAS"         ~ 27002,
      municipality == "CENTLA"           ~ 27003,
      municipality == "CENTRO"           ~ 27004,
      municipality == "COMALCALCO"       ~ 27005,
      municipality == "CUNDUACAN"        ~ 27006,
      municipality == "EMILIANO ZAPATA"  ~ 27007,
      municipality == "HUIMANGUILLO"     ~ 27008,
      municipality == "JALAPA"           ~ 27009,
      municipality == "JALPA DE MENDEZ"  ~ 27010,
      municipality == "JONUTA"           ~ 27011,
      municipality == "MACUSPANA"        ~ 27012,
      municipality == "NACAJUCA"         ~ 27013,
      municipality == "PARAISO"          ~ 27014,
      municipality == "TACOTALPA"        ~ 27015,
      municipality == "TEAPA"            ~ 27016,
      municipality == "TENOSIQUE"        ~ 27017,
      TRUE                               ~ 0
    )
  )

################################################################################
# 8) Generate 'valid' = rowtotal(...) across relevant columns
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PRD, PT, PVEM, PC, PCD, PSN, PARM, PAS, PDS),
      na.rm = TRUE
    )
  )


################################################################################
# 10) Merge 1:m with "..\..\all_months_years.dta", keep only (month, year, lista).
#     Then filter for month==9 & year==2000. Drop unmatched. Rename lista -> listanominal
################################################################################
# Read the external .dta
df_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  select(state, section, month, year, lista)%>%
  filter(state == "TABASCO", month == "September", year == 2000)

# Merge
df_merged <- df_collapsed %>%
  left_join(df_all, by = c("section"))

# In left_join, 
# we remove rows with NA in 'lista' to emulate that behavior:
df_merged <- df_merged %>%
  filter(!is.na(lista))

# Drop _merge, ed, seccion, year, month from final
df_merged <- df_merged %>%
  select(-state, -year, -month)

# Rename lista -> listanominal
df_merged <- df_merged %>%
  rename(listanominal = lista)

################################################################################
# 11) Compute turnout = total / listanominal, set year=2000, month="October"
################################################################################
df_2000 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2000,
    month   = "October"
  )

################################################################################
# 1) Read CSV
################################################################################
df <- fread("../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2003.csv", 
            encoding = "Latin-1")
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns, drop rows where municipality=="" & section==. (NA),
#    and drop rows where total is missing or zero
################################################################################
df <- df %>%
  rename(
    municipality = municipio,
    section      = sección
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 3) Convert listanominal, pan through total to numeric (replicating 
#    'destring listanominal pan - total , replace')
################################################################################
df <- df %>%
  mutate(across(c(listanominal, pan:total), as.numeric))

################################################################################
# 4) Create a variable 'missing' indicating if listanominal was missing 
#    (listanominal==.)
################################################################################
df <- df %>%
  mutate(missing = if_else(is.na(listanominal), 1, 0))

################################################################################
# 5) Collapse (sum) missing, listanominal, pan - total by municipality, section
################################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    across(c(missing, listanominal, pan:total), sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 6) Merge with all_months_years (ed=27, seccion=section), keep only 
#    month==9 & year==2003, drop unmatched, then rename 'lista' -> 'listanominal'
################################################################################

df_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  select(state, section, month, year, lista) %>% 
  filter(state == "TABASCO")

df_merged <- df_collapsed %>%
  left_join(df_all, by = c("section")) %>%
  filter(month == "September", year == 2003) %>%
  filter(!is.na(lista)) %>% 
  select(-state, -year, -month)

# rename lista -> listanominal
# keep both old 'listanominal' (collapsed) and 'lista' from merge, see next step
df_merged <- df_merged %>%
  rename(lista_from_merge = lista)

################################################################################
# 7) If 'missing' >=1 AND merged 'lista_from_merge' > (collapsed) 'listanominal', 
#    replace listanominal = lista_from_merge
################################################################################
df_merged <- df_merged %>%
  mutate(
    listanominal = if_else(
      missing >= 1 & lista_from_merge > listanominal,
      lista_from_merge,
      listanominal
    )
  ) %>%
  # drop 'missing' and 'lista_from_merge'
  select(-missing, -lista_from_merge)

################################################################################
# 8) Rename columns
#    (like rename pan->PAN, pripvem->PRI_PVEM, prd->PRD, etc.)
################################################################################
df_merged <- df_merged %>%
  rename(
    PAN     = pan,
    PRI_PVEM= pripvem,
    PRD     = prd,
    PT      = pt,
    PC      = pc,
    PAS     = pas,
    PMP     = pmp,
    PFC     = fc
  )

################################################################################
# 9) Compute turnout = total / listanominal
################################################################################
df_merged <- df_merged %>%
  mutate(
    turnout = total / listanominal
  )

################################################################################
# 10) Drop 'nulos' and 'noregistrados' if they exist
################################################################################
df_merged <- df_merged %>%
  select(-any_of(c("nulos", "noregistrados")))

################################################################################
# 11) Create uniqueid=0, then assign codes based on municipality
################################################################################
df_merged <- df_merged %>%
  mutate(
    uniqueid = case_when(
      municipality == "BALANCAN"        ~ 27001,
      municipality == "CARDENAS"        ~ 27002,
      municipality == "CENTLA"          ~ 27003,
      municipality == "CENTRO"          ~ 27004,
      municipality == "COMALCALCO"      ~ 27005,
      municipality == "CUNDUACAN"       ~ 27006,
      municipality == "EMILIANO ZAPATA" ~ 27007,
      municipality == "HUIMANGUILLO"    ~ 27008,
      municipality == "JALAPA"          ~ 27009,
      municipality == "JALPA DE MENDEZ" ~ 27010,
      municipality == "JONUTA"          ~ 27011,
      municipality == "MACUSPANA"       ~ 27012,
      municipality == "NACAJUCA"        ~ 27013,
      municipality == "PARAISO"         ~ 27014,
      municipality == "TACOTALPA"       ~ 27015,
      municipality == "TEAPA"           ~ 27016,
      municipality == "TENOSIQUE"       ~ 27017,
      TRUE                              ~ 0
    )
  )

################################################################################
# 12) Compute valid = rowtotal(PAN PRI_PVEM PRD PT PC PAS PMP PFC)
#    Then year=2003, month="October", sort by section, and save
################################################################################
df_2003 <- df_merged %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI_PVEM, PRD, PT, PC, PAS, PMP, PFC),
      na.rm = TRUE
    ),
    year  = 2003,
    month = "October"
  ) %>%
  arrange(section)  # sort by section

################################################################################
# 1) Read from Excel
#    Equivalent to: 
#    import excel "estadistica_electoral_2006.xls", sheet("Casillas Regidores") 
#    cellrange(A4:L2429) firstrow clear
################################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/estadistica_electoral_2006.xls",
  sheet = "Casillas Regidores",
  range = "A4:L2429",    # cell range A4:L2429
  col_names = TRUE
) %>%
  as.data.frame()  # optional, for consistent data.frame behavior
names(df) <- gsub("[- ]", "", names(df))
################################################################################
# 2) Rename columns and convert municipality to uppercase without accents
################################################################################
df <- df %>%
  rename(municipality = MUNICIPIO) %>%
  # remove Spanish accents from municipality
  mutate(
    municipality = str_replace_all(municipality, "á", "a"),
    municipality = str_replace_all(municipality, "é", "e"),
    municipality = str_replace_all(municipality, "í", "i"),
    # then uppercase
    municipality = toupper(municipality)
  )

df <- df %>%
  rename(section = SECCIÓN)

################################################################################
# 3) Rename further columns:
#
#    CPBT         -> PRD_PT
#    NUEVAALIANZA -> PANAL
#    ALTERNATIVA  -> PAS
#    CANDIDATOSNOREGISTRADOS -> noregistrados
#    VOTOSNULOS   -> nulos
#    VOTACIÓNTOTAL -> total
################################################################################
df <- df %>%
  rename(
    PRD_PT       = CPBT,
    PANAL        = NUEVAALIANZA,
    PAS          = ALTERNATIVA,
    noregistrados = CANDIDATOSNOREGISTRADOS,
    nulos        = VOTOSNULOS
  ) %>%
  rename(total = VOTACIÓNTOTAL)

################################################################################
# 4) Drop rows where municipality=="" AND section is missing, or total is missing/zero
################################################################################
df <- df %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 5) Collapse (sum) PAN - total by municipality, section
################################################################################
# Suppose your data has columns named PAN, PRI, PVEM, PRD_PT, etc. up to 'total'.
# We'll find those columns automatically. Make sure "PAN" is the leftmost, 
# "total" is the rightmost of interest.
collapse_cols <- df %>%
  select(matches("^PAN$|^PRI$|^PVEM$|^PRD_PT$|^PRD$|^PT$|^PANAL$|^PAS$|^noregistrados$|^nulos$|^total$")) %>%
  names()

df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(collapse_cols), sum, na.rm = TRUE), .groups = "drop")

################################################################################
# 6) Drop 'nulos' and 'noregistrados' if they exist
################################################################################
df_collapsed <- df_collapsed %>%
  select(-any_of(c("nulos", "noregistrados")))

################################################################################
# 7) Generate uniqueid=0, then replace based on municipality
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "BALANCAN"        ~ 27001,
      municipality == "CARDENAS"        ~ 27002,
      municipality == "CENTLA"          ~ 27003,
      municipality == "CENTRO"          ~ 27004,
      municipality == "COMALCALCO"      ~ 27005,
      municipality == "CUNDUACAN"       ~ 27006,
      municipality == "E. ZAPATA"       ~ 27007,  # note "E. ZAPATA" in your script
      municipality == "HUIMANGUILLO"    ~ 27008,
      municipality == "JALAPA"          ~ 27009,
      municipality == "J. DE MENDEZ"    ~ 27010,  # note "J. DE MENDEZ" in your script
      municipality == "JONUTA"          ~ 27011,
      municipality == "MACUSPANA"       ~ 27012,
      municipality == "NACAJUCA"        ~ 27013,
      municipality == "PARAISO"         ~ 27014,
      municipality == "TACOTALPA"       ~ 27015,
      municipality == "TEAPA"           ~ 27016,
      municipality == "TENOSIQUE"       ~ 27017,
      TRUE                              ~ 0
    )
  )

################################################################################
# 8) Compute valid = rowtotal(PAN PRI PRD_PT PVEM PANAL PAS)
################################################################################
# If you have a PRD column, add it to the row-sum. 
# Your script doesn't rename PRD. Possibly the column is absent or included in PRD_PT?
# We'll assume you have columns named: PAN, PRI, PRD_PT, PVEM, PANAL, PAS. 
# If PRD also exists as a separate column, add it.
vars_valid <- c("PAN", "PRI", "PRD_PT", "PVEM", "PANAL", "PAS")

df_collapsed <- df_collapsed %>%
  mutate(
    valid = rowSums(select(., all_of(vars_valid)), na.rm = TRUE)
  )

################################################################################
# 9) Merge (1:m) with "..\..\all_months_years.dta", keep only (month, year, lista)
#    Then keep if month==9 & year==2006, drop unmatched, rename lista->listanominal
################################################################################
df_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  select(state, section, month, year, lista) %>% 
  filter(state == "TABASCO")

df_merged <- df_collapsed %>%
  left_join(df_all, by = c("section")) %>%
  filter(month == "September", year == 2006) %>%
  filter(!is.na(lista))  # drop if no match

df_merged <- df_merged %>%
  select(-state, -year, -month) %>%
  rename(listanominal = lista)

################################################################################
# 10) Generate turnout = total / listanominal, year=2006, month="October",
#     sort by section, and save
################################################################################
df_2006 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2006,
    month   = "October"
  ) %>%
  arrange(section)

###############################################################################
# 1) Read CSV (Equivalent to: insheet using "Ayu_Seccion_2009.csv", clear)
###############################################################################
df <- read_csv("../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2009.csv", show_col_types = FALSE)
names(df) <- gsub("[- ]", "", names(df))
colnames(df) <- tolower(colnames(df))
###############################################################################
# 2) Rename columns, drop rows where municipality is "" & section is missing,
#    and drop rows where total is missing or zero
###############################################################################
df <- df %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

###############################################################################
# 3) Convert listanominal, pan through total to numeric
#    (Equivalent to: destring listanominal pan - total, replace)
###############################################################################
df <- df %>%
  mutate(across(c(listanominal, pan:total), as.numeric))

###############################################################################
# 4) Collapse: sum(pan - total), but take the "first" for listanominal,
#    by (municipality, section).

###############################################################################
df_collapsed <- df %>%
  group_by(municipality, section) %>%
  summarise(
    listanominal = dplyr::first(listanominal),  # 'first' for listanominal
    across(pan:total, sum, na.rm = TRUE),
    .groups = "drop"
  )

###############################################################################
# 5) Combine (replace) pripanal = pri + panal + pripanal, then drop pri, panal
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    pripanal = coalesce(pripanal, 0) +
      coalesce(pri, 0) +
      coalesce(panal, 0)
  ) %>%
  select(-pri, -panal)

###############################################################################
# 6) Rename columns to final form
###############################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN       = pan,
    PRI_PANAL = pripanal,
    PRD       = prd,
    PT        = pt,
    PVEM      = pvem,
    PC        = convergencia
  )

###############################################################################
# 7) Compute turnout = total / listanominal
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

###############################################################################
# 8) Drop 'nulos' and 'noregistrados' if they exist
###############################################################################
df_collapsed <- df_collapsed %>%
  select(-any_of(c("nulos", "noregistrados")))

###############################################################################
# 9) Create uniqueid=0, then replace based on municipality
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality == "BALANCAN"         ~ 27001,
      municipality == "CARDENAS"         ~ 27002,
      municipality == "CENTLA"           ~ 27003,
      municipality == "CENTRO"           ~ 27004,
      municipality == "COMALCALCO"       ~ 27005,
      municipality == "CUNDUACAN"        ~ 27006,
      municipality == "EMILIANO ZAPATA"  ~ 27007,
      municipality == "HUIMANGUILLO"     ~ 27008,
      municipality == "JALAPA"           ~ 27009,
      municipality == "JALPA DE MENDEZ"  ~ 27010,
      municipality == "JONUTA"           ~ 27011,
      municipality == "MACUSPANA"        ~ 27012,
      municipality == "NACAJUCA"         ~ 27013,
      municipality == "PARAISO"          ~ 27014,
      municipality == "TACOTALPA"        ~ 27015,
      municipality == "TEAPA"            ~ 27016,
      municipality == "TENOSIQUE"        ~ 27017,
      TRUE                               ~ 0
    )
  )

###############################################################################
# 10) Calculate 'valid' = rowtotal(PAN PRI_PANAL PRD PT PVEM PC)
#     Then set year=2009, month="October"
###############################################################################
df_2009 <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI_PANAL, PRD, PT, PVEM, PC),
      na.rm = TRUE
    ),
    year  = 2009,
    month = "October"
  ) %>%
  arrange(section)  # sort by section

###############################################################################
# 1) Read Excel 
###############################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2012.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()  # optional: ensure a standard data.frame

###############################################################################
# 2) Drop rows where MUNICIPIO==.
#    Then rename SECCION -> section, drop CASILLA
###############################################################################
df <- df %>%
  filter(!is.na(MUNICIPIO)) %>%  # equivalent to drop if MUNICIPIO==.
  rename(section = SECCION) %>%  # rename SECCION -> section
  select(-CASILLA)               # drop CASILLA

###############################################################################
# 3) Create PRD_PT_PC, PRI_PVEM_PANAL, drop old coalition columns
###############################################################################
df <- df %>%
  mutate(
    PRD_PT_PC = coalesce(CC_PRD_PT_PC, 0) + coalesce(PRD, 0) + coalesce(PT, 0) + coalesce(PC, 0),
    PRI_PVEM_PANAL = coalesce(CC_PRI_PVEM_PANAL, 0) + coalesce(PRI, 0) + coalesce(PVEM, 0) + coalesce(PANAL, 0)
  ) %>%
  select(
    -CC_PRD_PT_PC, -PRD, -PT, -PC,
    -CC_PRI_PVEM_PANAL, -PRI, -PVEM, -PANAL
  )

###############################################################################
# 4) Collapse (sum) columns from PAN to PRI_PVEM_PANAL by (MUNICIPIO, section)
###############################################################################
# Let's identify columns from PAN to PRI_PVEM_PANAL automatically:
col_range <- df %>%
  select(matches("^PAN$|^PRD_PT_PC$|^PRI_PVEM_PANAL$|^NOREGISTRADOS$|^NULOS$")) %>%
  names()

df_collapsed <- df %>%
  group_by(MUNICIPIO, section) %>%
  summarise(across(all_of(col_range), sum, na.rm = TRUE), .groups = "drop")

###############################################################################
# 5) Generate total = rowtotal(PRD_PT_PC PRI_PVEM_PANAL NOREGISTRADOS NULOS),
#    then drop NOREGISTRADOS, NULOS
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    total = rowSums(
      select(., PRD_PT_PC, PRI_PVEM_PANAL, NOREGISTRADOS, NULOS),
      na.rm = TRUE
    )
  ) %>%
  select(-NOREGISTRADOS, -NULOS)

###############################################################################
# 6) Create uniqueid = 0, create municipality = "", 
#    then replace uniqueid and municipality based on MUNICIPIO codes.
#    Finally drop MUNICIPIO.
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = 0,
    municipality = ""
  ) %>%
  mutate(
    uniqueid = case_when(
      MUNICIPIO == 1  ~ 27001,
      MUNICIPIO == 2  ~ 27002,
      MUNICIPIO == 3  ~ 27003,
      MUNICIPIO == 4  ~ 27004,
      MUNICIPIO == 5  ~ 27005,
      MUNICIPIO == 6  ~ 27006,
      MUNICIPIO == 7  ~ 27007,
      MUNICIPIO == 8  ~ 27008,
      MUNICIPIO == 9  ~ 27009,
      MUNICIPIO == 10 ~ 27010,
      MUNICIPIO == 11 ~ 27011,
      MUNICIPIO == 12 ~ 27012,
      MUNICIPIO == 13 ~ 27013,
      MUNICIPIO == 14 ~ 27014,
      MUNICIPIO == 15 ~ 27015,
      MUNICIPIO == 16 ~ 27016,
      MUNICIPIO == 17 ~ 27017,
      TRUE            ~ 0
    ),
    municipality = case_when(
      MUNICIPIO == 1  ~ "BALANCAN",
      MUNICIPIO == 2  ~ "CARDENAS",
      MUNICIPIO == 3  ~ "CENTLA",
      MUNICIPIO == 4  ~ "CENTRO",
      MUNICIPIO == 5  ~ "COMALCALCO",
      MUNICIPIO == 6  ~ "CUNDUACAN",
      MUNICIPIO == 7  ~ "EMILIANO ZAPATA",
      MUNICIPIO == 8  ~ "HUIMANGUILLO",
      MUNICIPIO == 9  ~ "JALAPA",
      MUNICIPIO == 10 ~ "JALPA DE MENDEZ",
      MUNICIPIO == 11 ~ "JONUTA",
      MUNICIPIO == 12 ~ "MACUSPANA",
      MUNICIPIO == 13 ~ "NACAJUCA",
      MUNICIPIO == 14 ~ "PARAISO",
      MUNICIPIO == 15 ~ "TACOTALPA",
      MUNICIPIO == 16 ~ "TEAPA",
      MUNICIPIO == 17 ~ "TENOSIQUE",
      TRUE            ~ ""
    )
  ) %>%
  select(-MUNICIPIO)  # drop MUNICIPIO

###############################################################################
# 7) Generate valid = rowtotal(PAN PRD_PT_PC PRI_PVEM_PANAL),
#    then ed=27, seccion=section for merging with external data.
###############################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    valid = rowSums(select(., PAN, PRD_PT_PC, PRI_PVEM_PANAL), na.rm = TRUE)
  )

###############################################################################
# 8) Merge with "..\..\all_months_years.dta" (1:m on ed, seccion),
#    keep if month==7 & year==2012 & day==1, drop unmatched, rename lista->listanominal
###############################################################################
df_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  select(state, section, month, year, lista, day) %>% 
  filter(state=="TABASCO")

df_merged <- df_collapsed %>%
  left_join(df_all, by = c("section")) %>%
  filter(month == "July", year == 2012, day == 1) %>%
  filter(!is.na(lista))  # drop if no match 

# drop _merge, ed, seccion, year, month, day
df_merged <- df_merged %>%
  select(-state, -year, -month, -day) %>%
  rename(listanominal = lista)

###############################################################################
# 9) turnout = total / listanominal, year=2012, month="July"
###############################################################################
df_2012 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2012,
    month   = "July"
  ) %>%
  arrange(section)  # sort section

################################################################################
# 1) Read from Excel (equivalent to 
#    import excel "Ayuntamientos_2015.xlsx", sheet("DESGLOSE") clear firstrow)
################################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/Ayuntamientos_2015.xlsx",
  sheet = "DESGLOSE",
  col_names = TRUE
) %>%
  as.data.frame()

################################################################################
# 2) Convert section to numeric
################################################################################
df <- df %>%
  mutate(section = as.numeric(section))

################################################################################
# 3) Drop any existing uniqueid, then create uniqueid=0, 
#    and assign codes based on municipality 
################################################################################
df <- df %>%
  select(-any_of("uniqueid")) %>%  # drop uniqueid if it exists
  mutate(
    uniqueid = 0,
    uniqueid = case_when(
      municipality == "BALANCAN"         ~ 27001,
      municipality == "CARDENAS"         ~ 27002,
      municipality == "CENTLA"           ~ 27003,
      municipality == "CENTRO"           ~ 27004,
      municipality == "COMALCALCO"       ~ 27005,
      municipality == "CUNDUACAN"        ~ 27006,
      municipality == "EMILIANO ZAPATA"  ~ 27007,
      municipality == "HUIMANGUILLO"     ~ 27008,
      municipality == "JALAPA"           ~ 27009,
      municipality == "JALPA DE MENDEZ"  ~ 27010,
      municipality == "JONUTA"           ~ 27011,
      municipality == "MACUSPANA"        ~ 27012,
      municipality == "NACAJUCA"         ~ 27013,
      municipality == "PARAISO"          ~ 27014,
      municipality == "TACOTALPA"        ~ 27015,
      municipality == "TEAPA"            ~ 27016,
      municipality == "TENOSIQUE"        ~ 27017,
      TRUE                               ~ 0
    )
  )

# Also replace municipality = "CENTRO EXTRAORDINARIO" if municipality=="CENTRO"
df <- df %>%
  mutate(
    municipality = if_else(municipality == "CENTRO",
                           "CENTRO EXTRAORDINARIO",
                           municipality)
  )

################################################################################
# 4) Create CI_1 = rowtotal(CIND CI1 CI2 CI3 CI4 CI5), 
#    then drop those columns, reorder columns so CI_1 is just before PVEM_PANAL
################################################################################
# In R, we'll do a rowSums, then remove those columns, then reorder.
df <- df %>%
  mutate(
    CI_1 = rowSums(
      select(., CIND, CI1, CI2, CI3, CI4, CI5),
      na.rm = TRUE
    )
  ) %>%
  select(-CIND, -CI1, -CI2, -CI3, -CI4, -CI5)

# In R, we can do a partial relocate if columns exist.
if ("PVEM_PANAL" %in% names(df)) {
  df <- df %>%
    relocate(CI_1, .before = PVEM_PANAL)
} else {
  # If PVEM_PANAL doesn't exist yet, we can skip or do some other reordering
  # We'll proceed without strict reordering if the column doesn't exist.
}

################################################################################
# 5) Combine columns for PRD_PANAL, PRD_PT, PRI_PVEM_PANAL,
################################################################################
df <- df %>%
  mutate(
    PRD_PANAL = if_else(!is.na(PRD_PANAL),
                        coalesce(PRD, 0) + coalesce(PANAL, 0) + coalesce(PRD_PANAL, 0),
                        PRD_PANAL),
    # if PRD_PANAL != . => set PRD=0, PANAL=0
    PRD       = if_else(!is.na(PRD_PANAL), NA_real_, PRD),
    PANAL     = if_else(!is.na(PRD_PANAL), NA_real_, PANAL)
  ) %>%
  mutate(
    PRD_PT = if_else(!is.na(PRD_PT),
                     coalesce(PRD, 0) + coalesce(PT, 0) + coalesce(PRD_PT, 0),
                     PRD_PT),
    PRD    = if_else(!is.na(PRD_PT), NA_real_, PRD),
    PT     = if_else(!is.na(PRD_PT), NA_real_, PT)
  ) %>%
  mutate(
    PRI_PVEM_PANAL = if_else(!is.na(PRI_PVEM_PANAL),
                             coalesce(PRI, 0) + coalesce(PVEM, 0) + coalesce(PANAL, 0) +
                               coalesce(PRI_PVEM, 0) + coalesce(PRI_PANAL, 0) +
                               coalesce(PVEM_PANAL, 0) + coalesce(PRI_PVEM_PANAL, 0),
                             PRI_PVEM_PANAL),
    PRI   = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PRI),
    PVEM  = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PVEM),
    PANAL = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PANAL)
  ) %>%
  select(-any_of(c("PRI_PVEM", "PRI_PANAL", "PVEM_PANAL")))

################################################################################
# 6) Collapse (sum) columns from PAN through CI_1, plus total, listanominal,
#    by (municipality, uniqueid, section)
################################################################################
collapse_cols <- df %>%
  select(matches("^PAN$|^PRI$|^PRD$|^PVEM$|^PT$|^MC$|^PANAL$|^MORENA$|^PH$|^PES$|^PRD_PANAL$|^PRD_PT$|^PRI_PVEM_PANAL$|^CI_1$|^total$|^listanominal$")) %>%
  names()

df_collapsed <- df %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(
    across(all_of(collapse_cols), sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 7) Compute valid = rowtotal(...) among the relevant columns
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PRD, PVEM, PT, MC, PANAL, MORENA, PH, PES,
             PRD_PANAL, PRD_PT, PRI_PVEM_PANAL, CI_1),
      na.rm = TRUE
    )
  )

################################################################################
# 8)
################################################################################
# We'll read LN2015.dta 
df_ln <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
  select(entidad, municipio, seccion, lista, file, year, month) %>%
  mutate(uniqueid = (entidad * 1000) + municipio) %>%
  filter(uniqueid == 27004, month == 2, seccion != 0) %>%
  # rename seccion -> section
  rename(section = seccion) %>%
  select(uniqueid, section, lista)

# We'll do a left_join on (section) to replicate "merge 1:1 section using LN16_TAB.dta"
df_merged <- df_collapsed %>%
  left_join(df_ln, by = c("section", "uniqueid"))

# drop if _merge==2 => rows that don't match LN data
# In R, that means we drop rows where 'lista' is NA
df_merged <- df_merged %>%
  filter(!(uniqueid == 27004 & is.na(lista)))  # only for uniqueid=27004 does this matter

# after merging, "replace listanominal = lista if uniqueid==27004"
df_merged <- df_merged %>%
  mutate(
    listanominal = if_else(uniqueid == 27004 & !is.na(lista), lista, listanominal)
  ) %>%
  select(-lista)

################################################################################
# 9) Final transformations:
################################################################################
df_2015 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2015,
    year    = if_else(uniqueid == 27004, 2016, year),
    month   = "June",
    month   = if_else(uniqueid == 27004, "March", month),
    STATE   = "TABASCO",
    # force numeric in section (in case merging changed something)
    section = as.numeric(section)
  )

################################################################################
# 1) Read from Excel (equivalent to 
#    import excel "Ayuntamientos_2015.xlsx", sheet("DESGLOSE") clear firstrow)
################################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/Ayuntamientos_2015.xlsx",
  sheet = "DESGLOSE",
  col_names = TRUE
) %>%
  as.data.frame()

################################################################################
# 2) Convert section to numeric
################################################################################
df <- df %>%
  mutate(section = as.numeric(section))

################################################################################
# 3) Drop any existing uniqueid, then create uniqueid=0, 
#    and assign codes based on municipality 
################################################################################
df <- df %>%
  select(-any_of("uniqueid")) %>%  # drop uniqueid if it exists
  mutate(
    uniqueid = 0,
    uniqueid = case_when(
      municipality == "BALANCAN"         ~ 27001,
      municipality == "CARDENAS"         ~ 27002,
      municipality == "CENTLA"           ~ 27003,
      municipality == "CENTRO"           ~ 27004,
      municipality == "COMALCALCO"       ~ 27005,
      municipality == "CUNDUACAN"        ~ 27006,
      municipality == "EMILIANO ZAPATA"  ~ 27007,
      municipality == "HUIMANGUILLO"     ~ 27008,
      municipality == "JALAPA"           ~ 27009,
      municipality == "JALPA DE MENDEZ"  ~ 27010,
      municipality == "JONUTA"           ~ 27011,
      municipality == "MACUSPANA"        ~ 27012,
      municipality == "NACAJUCA"         ~ 27013,
      municipality == "PARAISO"          ~ 27014,
      municipality == "TACOTALPA"        ~ 27015,
      municipality == "TEAPA"            ~ 27016,
      municipality == "TENOSIQUE"        ~ 27017,
      TRUE                               ~ 0
    )
  )

# Also replace municipality = "CENTRO EXTRAORDINARIO" if municipality=="CENTRO"
df <- df %>%
  mutate(
    municipality = if_else(municipality == "CENTRO",
                           "CENTRO EXTRAORDINARIO",
                           municipality)
  )

################################################################################
# 4) Create CI_1 = rowtotal(CIND CI1 CI2 CI3 CI4 CI5), 
#    then drop those columns, reorder columns so CI_1 is just before PVEM_PANAL
################################################################################
# In R, we'll do a rowSums, then remove those columns, then reorder.
df <- df %>%
  mutate(
    CI_1 = rowSums(
      select(., CIND, CI1, CI2, CI3, CI4, CI5),
      na.rm = TRUE
    )
  ) %>%
  select(-CIND, -CI1, -CI2, -CI3, -CI4, -CI5)

# "order CI_1, a(PVEM_PANAL)"
# In R, we can do a partial relocate if columns exist.
if ("PVEM_PANAL" %in% names(df)) {
  df <- df %>%
    relocate(CI_1, .before = PVEM_PANAL)
} else {
  # If PVEM_PANAL doesn't exist yet, we can skip or do some other reordering
  # We'll proceed without strict reordering if the column doesn't exist.
}

################################################################################
# 5) Combine columns for PRD_PANAL, PRD_PT, PRI_PVEM_PANAL, etc.
################################################################################
df <- df %>%
  mutate(
    PRD_PANAL = if_else(!is.na(PRD_PANAL),
                        coalesce(PRD, 0) + coalesce(PANAL, 0) + coalesce(PRD_PANAL, 0),
                        PRD_PANAL),
    # if PRD_PANAL != . => set PRD=0, PANAL=0
    PRD       = if_else(!is.na(PRD_PANAL), NA_real_, PRD),
    PANAL     = if_else(!is.na(PRD_PANAL), NA_real_, PANAL)
  ) %>%
  mutate(
    PRD_PT = if_else(!is.na(PRD_PT),
                     coalesce(PRD, 0) + coalesce(PT, 0) + coalesce(PRD_PT, 0),
                     PRD_PT),
    PRD    = if_else(!is.na(PRD_PT), NA_real_, PRD),
    PT     = if_else(!is.na(PRD_PT), NA_real_, PT)
  ) %>%
  mutate(
    PRI_PVEM_PANAL = if_else(!is.na(PRI_PVEM_PANAL),
                             coalesce(PRI, 0) + coalesce(PVEM, 0) + coalesce(PANAL, 0) +
                               coalesce(PRI_PVEM, 0) + coalesce(PRI_PANAL, 0) +
                               coalesce(PVEM_PANAL, 0) + coalesce(PRI_PVEM_PANAL, 0),
                             PRI_PVEM_PANAL),
    PRI   = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PRI),
    PVEM  = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PVEM),
    PANAL = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PANAL)
  ) %>%
  select(-any_of(c("PRI_PVEM", "PRI_PANAL", "PVEM_PANAL")))

################################################################################
# 6) Collapse (sum) columns from PAN through CI_1, plus total, listanominal,
#    by (municipality, uniqueid, section)
################################################################################
collapse_cols <- df %>%
  select(matches("^PAN$|^PRI$|^PRD$|^PVEM$|^PT$|^MC$|^PANAL$|^MORENA$|^PH$|^PES$|^PRD_PANAL$|^PRD_PT$|^PRI_PVEM_PANAL$|^CI_1$|^total$|^listanominal$")) %>%
  names()

df_collapsed <- df %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(
    across(all_of(collapse_cols), sum, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# 7) Compute valid = rowtotal(...) among the relevant columns
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PAN, PRI, PRD, PVEM, PT, MC, PANAL, MORENA, PH, PES,
             PRD_PANAL, PRD_PT, PRI_PVEM_PANAL, CI_1),
      na.rm = TRUE
    )
  )

################################################################################
# 8) 
################################################################################
# We'll read LN2015.dta 
df_ln <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>%
  select(entidad, municipio, seccion, lista, file, year, month) %>%
  mutate(uniqueid = (entidad * 1000) + municipio) %>%
  filter(uniqueid == 27004, month == 2, seccion != 0) %>%
  # rename seccion -> section
  rename(section = seccion) %>%
  select(uniqueid, section, lista)

# We'll do a left_join on (section) to replicate "merge 1:1 section using LN16_TAB.dta"
df_merged <- df_collapsed %>%
  left_join(df_ln, by = c("uniqueid", "section"))

# drop if _merge==2 => rows that don't match LN data
# In R, that means we drop rows where 'lista' is NA
df_merged <- df_merged %>%
  filter(!(uniqueid == 27004 & is.na(lista)))  # only for uniqueid=27004 does this matter

# after merging, "replace listanominal = lista if uniqueid==27004"
df_merged <- df_merged %>%
  mutate(
    listanominal = if_else(uniqueid == 27004 & !is.na(lista), lista, listanominal)
  ) %>%
  select(-lista)

################################################################################
# 9) Final transformations:

################################################################################
df_2015 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2015,
    year    = if_else(uniqueid == 27004, 2016, year),
    month   = "June",
    month   = if_else(uniqueid == 27004, "March", month),
    STATE   = "TABASCO",
    # force numeric in section (in case merging changed something)
    section = as.numeric(section)
  )

###############################################################################
# 1) Read Excel (Equivalent to: 
#    import excel "Ayuntamientos_2018.xlsx", sheet("1 TAB RES AYUN") clear firstrow)
###############################################################################
df <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/Ayuntamientos_2018.xlsx",
  sheet = "1 TAB RES AYUN",
  col_names = TRUE
) %>%
  as.data.frame()

###############################################################################
# 2) Drop any existing 'uniqueid' column, then create uniqueid=0, 
#    and assign codes for each municipality
###############################################################################
df <- df %>%
  select(-any_of("uniqueid")) %>%  # drop 'uniqueid' if present
  mutate(
    uniqueid = 0,
    uniqueid = case_when(
      municipality == "BALANCAN"         ~ 27001,
      municipality == "CARDENAS"         ~ 27002,
      municipality == "CENTLA"           ~ 27003,
      municipality == "CENTRO"           ~ 27004,
      municipality == "COMALCALCO"       ~ 27005,
      municipality == "CUNDUACAN"        ~ 27006,
      municipality == "EMILIANO ZAPATA"  ~ 27007,
      municipality == "HUIMANGUILLO"     ~ 27008,
      municipality == "JALAPA"           ~ 27009,
      municipality == "JALPA DE MENDEZ"  ~ 27010,
      municipality == "JONUTA"           ~ 27011,
      municipality == "MACUSPANA"        ~ 27012,
      municipality == "NACAJUCA"         ~ 27013,
      municipality == "PARAISO"          ~ 27014,
      municipality == "TACOTALPA"        ~ 27015,
      municipality == "TEAPA"            ~ 27016,
      municipality == "TENOSIQUE"        ~ 27017,
      TRUE                               ~ 0
    )
  )

###############################################################################
# 3) Combine columns for PAN_PRD_MC, then drop old columns 
#    (PAN, PRD, MC, PAN_PRD, PAN_MC, PRD_MC)
###############################################################################
df <- df %>%
  mutate(
    PAN_PRD_MC = PAN_PRD_MC + coalesce(PAN, 0) + coalesce(PRD, 0) + coalesce(MC, 0) +
      coalesce(PAN_PRD, 0) + coalesce(PAN_MC, 0) + coalesce(PRD_MC, 0)
  ) %>%
  select(-PAN, -PRD, -MC, -PAN_PRD, -PAN_MC, -PRD_MC)

###############################################################################
# 4) Combine columns for PT_MORENA, dropping old PT, MORENA if PT_MORENA != NA
###############################################################################
df <- df %>%
  mutate(
    PT_MORENA = if_else(
      !is.na(PT_MORENA),
      PT_MORENA + coalesce(PT, 0) + coalesce(MORENA, 0),
      PT_MORENA
    ),
    PT     = if_else(!is.na(PT_MORENA), NA_real_, PT),
    MORENA = if_else(!is.na(PT_MORENA), NA_real_, MORENA)
  )

###############################################################################
# 5) Create CI_1 = row sum of (CAND_IND10 through CAND_IND17), then drop CAND*
###############################################################################
cand_cols <- grep("^CAND_IND\\d+$", names(df), value = TRUE)
df <- df %>%
  mutate(
    CI_1 = rowSums(select(., all_of(cand_cols)), na.rm = TRUE)
  ) %>%
  select(-all_of(cand_cols))  # drop all CAND_IND columns

if ("PT_MORENA" %in% names(df)) {
  df <- df %>%
    relocate(CI_1, .before = PT_MORENA)
}

###############################################################################
# 6) Collapse (sum) columns from PRI through CI_1, plus total and listanominal
#    by (municipality, uniqueid, section)
###############################################################################
collapse_cols <- df %>%
  select(matches("^PRI$|^PVEM$|^PT$|^PANAL$|^MORENA$|^PES$|^PAN_PRD_MC$|^PT_MORENA$|^CI_1$|^total$|^listanominal$")) %>%
  names()

df_collapsed <- df %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(
    across(all_of(collapse_cols), sum, na.rm = TRUE),
    .groups = "drop"
  )

###############################################################################
# 7) Compute 'valid' = rowtotal(PRI PVEM PT PANAL MORENA PES PAN_PRD_MC PT_MORENA CI_1)
#    Also compute turnout = total / listanominal, year=2018, month="July", STATE="TABASCO"
###############################################################################
df_2018 <- df_collapsed %>%
  mutate(
    valid = rowSums(
      select(., PRI, PVEM, PT, PANAL, MORENA, PES, PAN_PRD_MC, PT_MORENA, CI_1),
      na.rm = TRUE
    ),
    turnout = total / listanominal,
    year    = 2018,
    month   = "July",
    STATE   = "TABASCO"
  )

#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

# Load the 2021 dataset from the excel
data_2021 <- read_excel("../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/21/ayu_resumen_general_casillas.xlsx", skip = 6)

# Rename columns
data_2021 <- data_2021 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = "SECCIÓN ELECTORAL",
                listanominal = "LISTA NOMINAL",
                total = "TOTAL DE VOTOS",
                no_reg = "NÚMERO DE VOTOS CANDIDATURAS NO REGISTRADAS",
                nulos = "NÚMERO DE VOTOS NULOS",
                valid = "NÚMERO DE VOTOS VÁLIDOS",
                PAN_PRI = "PAN Y PRI",
                FXM = FPM,
                CI_1 = "C. IND. JOSÉ ANTONIO HERNÁNDEZ ROMERO",
                CI_2 = "C. IND. SERGIO VIDAL GONZÁLEZ",
                CI_3 = "C. IND. JESÚS ABRAHAM CANO GONZÁLEZ",
                CI_4 = "C. IND. DONALD JIMÉNEZ MONTEJO") %>%
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
    uniqueid = case_when(
      municipality == "BALANCAN"         ~ 27001,
      municipality == "CARDENAS"         ~ 27002,
      municipality == "CENTLA"           ~ 27003,
      municipality == "CENTRO"           ~ 27004,
      municipality == "COMALCALCO"       ~ 27005,
      municipality == "CUNDUACAN"        ~ 27006,
      municipality == "EMILIANO ZAPATA"  ~ 27007,
      municipality == "HUIMANGUILLO"     ~ 27008,
      municipality == "JALAPA"           ~ 27009,
      municipality == "JALPA DE MENDEZ"  ~ 27010,
      municipality == "JONUTA"           ~ 27011,
      municipality == "MACUSPANA"        ~ 27012,
      municipality == "NACAJUCA"         ~ 27013,
      municipality == "PARAISO"          ~ 27014,
      municipality == "TACOTALPA"        ~ 27015,
      municipality == "TEAPA"            ~ 27016,
      municipality == "TENOSIQUE"        ~ 27017,
      TRUE                               ~ NA
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
    year = 2021,
    month = "June"
  )

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 27) %>% 
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
data_2024 <- read_excel("../../../Data/Raw Electoral Data/Tabasco - 1997, 2000, 2003,2006, 2009, 2012,2015,2018,2021,2024/24/ayu_resumen_general_casillas.xlsx", skip = 7)

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = "SECCIÓN ELECTORAL",
                listanominal = "LISTA NOMINAL",
                total = "TOTAL DE VOTOS",
                no_reg = "NÚMERO DE VOTOS CANDIDATURAS NO REGISTRADAS",
                nulos = "NÚMERO DE VOTOS NULOS",
                valid = "NÚMERO DE VOTOS VÁLIDOS",
                PAN_PRI = "PAN Y PRI",
                PVEM_MORENA = "PVEM Y MORENA",
                CI_1 = "C. IND. JUAN CARLOS GUZMAN CORREA",
                CI_2 = "C. IND. LUIS ALFONSO OJEDA VILLASEÑOR", 
                CI_3 = "C. IND. JESUS ABRAHAM CANO GONZALEZ") %>%
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
  dplyr::filter(section > 0 & total > 0) %>% 
  dplyr:: select(-c("TOTAL\r\nPVEM Y MORENA", "TOTAL\r\nPAN Y PRI"))

# Assign uniqueids
data_2024 <- data_2024 %>% 
  mutate(
    uniqueid = case_when(
      municipality == "BALANCAN"         ~ 27001,
      municipality == "CARDENAS"         ~ 27002,
      municipality == "CENTLA"           ~ 27003,
      municipality == "CENTRO"           ~ 27004,
      municipality == "COMALCALCO"       ~ 27005,
      municipality == "CUNDUACAN"        ~ 27006,
      municipality == "EMILIANO ZAPATA"  ~ 27007,
      municipality == "HUIMANGUILLO"     ~ 27008,
      municipality == "JALAPA"           ~ 27009,
      municipality == "JALPA DE MENDEZ"  ~ 27010,
      municipality == "JONUTA"           ~ 27011,
      municipality == "MACUSPANA"        ~ 27012,
      municipality == "NACAJUCA"         ~ 27013,
      municipality == "PARAISO"          ~ 27014,
      municipality == "TACOTALPA"        ~ 27015,
      municipality == "TEAPA"            ~ 27016,
      municipality == "TENOSIQUE"        ~ 27017,
      TRUE                               ~ NA
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
    year = 2024,
    month = "June"
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)


# Combine the dataframes, handling different columns by filling with NA
tabasco_all <- bind_rows(df_1997,
                         df_2000,
                         df_2003,
                         df_2006,
                         df_2009,
                         df_2012,
                         df_2015,
                         df_2018,
                         collapsed_2021,
                         collapsed_2024)

data.table::fwrite(tabasco_all,"../../../Processed Data/Tabasco/Tabasco_process_raw_data.csv")
