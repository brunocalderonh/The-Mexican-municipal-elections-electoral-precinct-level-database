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

###########################################
### Step 1: Process 1997 Data
###########################################
# Load 1997 data
data_1997 <- read_csv("../../../Data/Raw Electoral Data/Guanajuato - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997.csv")

# Rename and clean columns
data_1997 <- data_1997 %>%
  rename(municipality = NOM_MPIO,
         section = SECCION,
         listanominal = LISTA_NOM) %>%
  filter(!(municipality == "" & section == "."), TOTAL != 0) %>%
  mutate(across(listanominal:TOTAL, as.numeric))

# Calculate missing values and collapse
data_1997 <- data_1997 %>%
  mutate(missing = is.na(listanominal)) %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, missing, PAN:TOTAL), sum, na.rm = TRUE)) %>%
  ungroup()

lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "GUANAJUATO" &
           month == "July" &
           year == 1997)

# Replace missing values
data_1997 <- data_1997 %>%
  left_join(lista_nominal, 
            by = c("section" = "section")) %>%
  mutate(listanominal = ifelse(missing >= 1, lista, listanominal)) %>%
  select(-c(missing, lista, state, day, month, year))


# Rename parties and calculate turnout
data_1997 <- data_1997 %>%
  select(-c("NO REG",NULOS)) %>% 
  rename(total = TOTAL,
         PartCardenista = PC) %>%
  mutate(turnout = total / listanominal)

# Assign unique IDs to municipalities
unique_ids <- c("ABASOLO" = 11001, "ACAMBARO" = 11002, "APASEO EL ALTO" = 11004, "APASEO EL GRANDE" = 11005, 
                "ATARJEA" = 11006, "CELAYA" = 11007, "COMONFORT" = 11009, "CORONEO" = 11010, "CORTAZAR" = 11011, 
                "CUERAMARO" = 11012, "DR. MORA" = 11013, "DOLORES HIDALGO" = 11014, "GUANAJUATO" = 11015, 
                "HUANIMARO" = 11016, "IRAPUATO" = 11017, "JARAL DEL PROGRESO" = 11018, "JERECUARO" = 11019, 
                "LEON" = 11020, "CD. MANUEL DOBLADO" = 11008, "MOROLEON" = 11021, "OCAMPO" = 11022, "PENJAMO" = 11023, 
                "PUEBLO NUEVO" = 11024, "PURISIMA DEL RINCON" = 11025, "ROMITA" = 11026, "SALAMANCA" = 11027, 
                "SALVATIERRA" = 11028, "SAN DIEGO DE LA UNION" = 11029, "SAN FELIPE" = 11030, "SAN FRANCISCO DEL RINCON" = 11031, 
                "SAN JOSE ITURBIDE" = 11032, "SAN LUIS DE LA PAZ" = 11033, "SAN MIGUEL DE ALLENDE" = 11003, 
                "SANTA CATARINA" = 11034, "STA. C. DE JUVENTINO ROSAS" = 11035, "SANTIAGO MARAVATIO" = 11036, 
                "SILAO" = 11037, "TARANDACUAO" = 11038, "TARIMORO" = 11039, "TIERRA BLANCA" = 11040, 
                "URIANGATO" = 11041, "VALLE DE SANTIAGO" = 11042, "VICTORIA" = 11043, "VILLAGRAN" = 11044, 
                "XICHU" = 11045, "YURIRIA" = 11046)

data_1997 <- data_1997 %>%
  mutate(uniqueid = unique_ids[municipality])
names(data_1997)
# Generate total valid votes
data_1997 <- data_1997 %>%
  mutate(valid = PAN + PRI + PRD + PartCardenista + PT + PVEM + PPS + PDM)

# Add year and month
data_1997_collapsed <- data_1997 %>%
  mutate(year = 1997, month = "July")

# remove
rm(data_1997)

###########################################
### Step 1: Process 2000 Data
###########################################
# Load 2000 data
data_2000 <- read_csv("../../../Data/Raw Electoral Data/Guanajuato - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2000.csv")

# Convert column names to lowercase
data_2000 <- data_2000 %>% rename_with(tolower)

# Rename variables
data_2000 <- data_2000 %>%
  rename(municipality = nom_mpio,
         section = seccion) %>%
  # Drop empty municipality and section rows, and those with total == 0 or missing
  filter(!(municipality == "" & section == "."),
         !(is.na(total) | total == 0))

# Convert numeric columns
# Identify numeric cols: from listanominal through total (adjust if needed)
num_vars <- c("lista nominal", "pan", "pri", "prd", "pt", "pvem", "pas", 
              "pcd", "psn", "parm", "cdppn", "ds", "prd_pt_pas_cd", 
              "prd_pt_pas", "prd_pas_cd", "total")

data_2000 <- data_2000 %>%
  mutate(across(all_of(num_vars), as.numeric))

# Create 'missing' variable
data_2000 <- data_2000 %>%
  mutate(missing = if_else(is.na(`lista nominal`), 1, 0))

# Collapse (sum) by municipality and section
data_2000 <- data_2000 %>%
  group_by(municipality, section) %>%
  summarise(across(c(`lista nominal`, missing, pan:total), 
                   sum, na.rm = TRUE), 
            .groups = "drop")

lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "GUANAJUATO" &
           month == "July" &
           year == 2000)

names(lista_nominal)

data_2000 <- data_2000 %>%
  left_join(lista_nominal %>% select(section, day, month, year, lista, state),
            by = c("section")) %>% 
  # Drop merge indicators and unnecessary columns
  select(-section, -day, -year, -month, -state)

# Replace listanominal using lista if missing>=1
data_2000 <- data_2000 %>%
  mutate(listanominal = as.numeric(ifelse(missing >= 1, lista, `lista nominal`))) %>%
  select(-`lista nominal`, -missing, -lista)

###########################################
### Party Aggregation
###########################################

# Create dummy variables by municipality
data_2000 <- data_2000 %>%
  group_by(municipality) %>%
  mutate(dummy_prd_pt_pas_cd = sum(prd_pt_pas_cd > 0),
         dummy_prd_pt_pas = sum(prd_pt_pas > 0),
         dummy_prd_pas_cd = sum(prd_pas_cd > 0)) %>%
  ungroup() %>%
  mutate(dummy_prd_pt_pas_cd = as.numeric(dummy_prd_pt_pas_cd > 0),
         dummy_prd_pt_pas = as.numeric(dummy_prd_pt_pas > 0),
         dummy_prd_pas_cd = as.numeric(dummy_prd_pas_cd > 0))

# Replace and aggregate votes
data_2000 <- data_2000 %>%
  mutate(
    prd_pt_pas_cd = if_else(dummy_prd_pt_pas_cd == 1, prd_pt_pas_cd + prd + pt + pas + pcd, prd_pt_pas_cd),
    prd = if_else(dummy_prd_pt_pas_cd == 1, 0, prd),
    pt = if_else(dummy_prd_pt_pas_cd == 1, 0, pt),
    pas = if_else(dummy_prd_pt_pas_cd == 1, 0, pas),
    pcd = if_else(dummy_prd_pt_pas_cd == 1, 0, pcd)
  ) %>%
  select(-dummy_prd_pt_pas_cd)

data_2000 <- data_2000 %>%
  mutate(
    prd_pt_pas = if_else(dummy_prd_pt_pas == 1, prd_pt_pas + prd + pt + pas, prd_pt_pas),
    prd = if_else(dummy_prd_pt_pas == 1, 0, prd),
    pt = if_else(dummy_prd_pt_pas == 1, 0, pt),
    pas = if_else(dummy_prd_pt_pas == 1, 0, pas)
  ) %>%
  select(-dummy_prd_pt_pas)

data_2000 <- data_2000 %>%
  mutate(
    prd_pas_cd = if_else(dummy_prd_pas_cd == 1, prd_pas_cd + prd + pas + pcd, prd_pas_cd),
    prd = if_else(dummy_prd_pas_cd == 1, 0, prd),
    pas = if_else(dummy_prd_pas_cd == 1, 0, pas),
    pcd = if_else(dummy_prd_pas_cd == 1, 0, pcd)
  ) %>%
  select(-dummy_prd_pas_cd)

###########################################
### Rename parties to uppercase
###########################################
data_2000 <- data_2000 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PAS = pas,
         PCD = pcd,
         PSN = psn,
         PARM = parm,
         PC = cdppn,  # cdppn is renamed to PC 
         PDS = ds,
         PRD_PT_PAS_PCD = prd_pt_pas_cd,
         PRD_PT_PAS = prd_pt_pas,
         PRD_PAS_PCD = prd_pas_cd
  )

# Turnout
data_2000 <- data_2000 %>%
  mutate(turnout = total / listanominal)

###########################################
### Unique IDs for municipalities
###########################################
unique_ids <- c("ABASOLO" = 11001, "ACAMBARO" = 11002, "APASEO EL ALTO" = 11004, "APASEO EL GRANDE" = 11005, 
                "ATARJEA" = 11006, "CELAYA" = 11007, "COMONFORT" = 11009, "CORONEO" = 11010, "CORTAZAR" = 11011, 
                "CUERAMARO" = 11012, "DOCTOR MORA" = 11013, "DOLORES HIDALGO" = 11014, "GUANAJUATO" = 11015, 
                "HUANIMARO" = 11016, "IRAPUATO" = 11017, "JARAL DEL PROGRESO" = 11018, "JERECUARO" = 11019, 
                "LEON" = 11020, "MANUEL DOBLADO" = 11008, "MOROLEON" = 11021, "OCAMPO" = 11022, "PENJAMO" = 11023, 
                "PUEBLO NUEVO" = 11024, "PURISIMA DEL RINCON" = 11025, "ROMITA" = 11026, "SALAMANCA" = 11027, 
                "SALVATIERRA" = 11028, "SAN DIEGO DE LA UNION" = 11029, "SAN FELIPE" = 11030, "SAN FRANCISCO DEL RINCON" = 11031, 
                "SAN JOSE ITURBIDE" = 11032, "SAN LUIS DE LA PAZ" = 11033, "ALLENDE" = 11003, 
                "SANTA CATARINA" = 11034, "SANTA CRUZ DE JUVENTINO ROSAS" = 11035, "SANTIAGO MARAVATIO" = 11036, 
                "SILAO" = 11037, "TARANDACUAO" = 11038, "TARIMORO" = 11039, "TIERRA BLANCA" = 11040, 
                "URIANGATO" = 11041, "VALLE DE SANTIAGO" = 11042, "VICTORIA" = 11043, "VILLAGRAN" = 11044, 
                "XICHU" = 11045, "YURIRIA" = 11046)

data_2000 <- data_2000 %>%
  mutate(uniqueid = unique_ids[municipality])

###########################################
### Valid Votes
###########################################
data_2000 <- data_2000 %>%
  mutate(valid = PAN + PRI + PRD + PT + PVEM + PAS + PCD + PSN + PARM + PC + PDS + PRD_PT_PAS_PCD + PRD_PT_PAS + PRD_PAS_PCD)

###########################################
#turnout
###########################################
data_2000 <- data_2000 %>%
  mutate(turnout = total / listanominal)

# Add year and month
data_2000_collapsed <- data_2000 %>%
  mutate(year = 2000, month = "July")

# remove
rm(data_2000)

###########################################
### 2003: Process Municipality Name File
###########################################

mun_data <- fread("../../../Data/Raw Electoral Data/Guanajuato - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003_Mun_Number_to_Name.csv") %>%
  rename(num_municipality = num_mun) %>%
  arrange(num_municipality)

data_2003 <- fread("../../../Data/Raw Electoral Data/Guanajuato - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003.csv")

# Convert column names to lowercase
data_2003 <- data_2003 %>% rename_with(tolower)

data_2003 <- data_2003 %>%
  rename(num_municipality = num_mun) %>%
  arrange(num_municipality)

# Merge with municipality name file
data_2003 <- data_2003 %>%
  left_join(mun_data, by = "num_municipality")

# Rename and filter
data_2003 <- data_2003 %>%
  rename(section = seccion) %>%
  filter(!(municipality == "" & section == ".")) %>%
  filter(!is.na(total), total != 0)

# Ensure numeric for listanominal, pan-total range
# Identify the range of columns from pan through total
num_cols <- c("lista nominal","pan","pri","prd","pt","pvem","c","psn","pas","mp","plm","fc","total")
data_2003 <- data_2003 %>%
  mutate(across(all_of(num_cols), as.numeric))

# Collapse sum by municipality, section
data_2003 <- data_2003 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(num_cols), sum, na.rm = TRUE), .groups = "drop")

###########################################
### Rename Parties
###########################################
data_2003 <- data_2003 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PC = c,
         PSN = psn,
         PAS = pas,
         MexicoPosible = mp,
         PLM = plm,
         FC = fc,
         listanominal = `lista nominal`)

###########################################
### Turnout
###########################################
data_2003 <- data_2003 %>%
  mutate(turnout = total / listanominal)

###########################################
### Unique ID Assignments
###########################################
unique_ids <- c("ABASOLO" = 11001, "ACAMBARO" = 11002, "APASEO EL ALTO" = 11004, "APASEO EL GRANDE" = 11005, 
                "ATARJEA" = 11006, "CELAYA" = 11007, "COMONFORT" = 11009, "CORONEO" = 11010, "CORTAZAR" = 11011, 
                "CUERAMARO" = 11012, "DOCTOR MORA" = 11013, "DOLORES HIDALGO" = 11014, "GUANAJUATO" = 11015, 
                "HUANIMARO" = 11016, "IRAPUATO" = 11017, "JARAL DEL PROGRESO" = 11018, "JERECUARO" = 11019, 
                "LEON" = 11020, "MANUEL DOBLADO" = 11008, "MOROLEON" = 11021, "OCAMPO" = 11022, "PENJAMO" = 11023, 
                "PUEBLO NUEVO" = 11024, "PURISIMA DEL RINCON" = 11025, "ROMITA" = 11026, "SALAMANCA" = 11027, 
                "SALVATIERRA" = 11028, "SAN DIEGO DE LA UNION" = 11029, "SAN FELIPE" = 11030, "SAN FRANCISCO DEL RINCON" = 11031, 
                "SAN JOSE ITURBIDE" = 11032, "SAN LUIS DE LA PAZ" = 11033, "ALLENDE" = 11003, 
                "SANTA CATARINA" = 11034, "SANTA CRUZ DE JUVENTINO ROSAS" = 11035, "SANTIAGO MARAVATIO" = 11036, 
                "SILAO" = 11037, "TARANDACUAO" = 11038, "TARIMORO" = 11039, "TIERRA BLANCA" = 11040, 
                "URIANGATO" = 11041, "VALLE DE SANTIAGO" = 11042, "VICTORIA" = 11043, "VILLAGRAN" = 11044, 
                "XICHU" = 11045, "YURIRIA" = 11046)

data_2003 <- data_2003 %>%
  mutate(uniqueid = unique_ids[municipality])

###########################################
### Valid Votes
###########################################

party_vars <- c("PAN","PRI","PRD","PT","PVEM","PC","PSN","PAS","MexicoPosible","PLM","FC")

data_2003 <- data_2003 %>%
  mutate(valid = rowSums(across(all_of(party_vars)), na.rm = TRUE))

###########################################
### Add Year and Month
###########################################
data_2003_collapsed <- data_2003 %>%
  mutate(year = 2003,
         month = "July")

rm(data_2003)

###########################################
### Process 2006 Data
###########################################

# Read the 2006 CSV (adjust file path)
data_2006 <- fread("../../../Data/Raw Electoral Data/Guanajuato - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2006.csv")
  
# Convert column names to lowercase
data_2006 <- data_2006 %>% rename_with(tolower) 
  
data_2006 <- data_2006 %>%  
  rename(municipality = nom_mpio,
         section = seccion,
         listanominal = lista_nom)

# drop if municipality=="" & section==.
# In R: remove rows where municipality is "" AND section is NA
data_2006 <- data_2006[!(data_2006$municipality == "" & is.na(data_2006$section)), ]

data_2006$total <- rowSums(data_2006[, c("pan","pri","co_ pbt","pvem","conv","v12","asdc","pan na","pri pvem","no reg","nulos")], na.rm = TRUE)

data_2006 <- data_2006[!is.na(data_2006$total) & data_2006$total != 0, ]

vars_to_numeric <- c("listanominal", "pan", "pri", "co_ pbt", "pvem", "conv", "v12", "asdc", "pan na", "pri pvem", "total")

data_2006 <- data_2006 %>%
  mutate(across(all_of(vars_to_numeric), as.numeric))

# collapse (sum) listanominal pan - pripvem total, by(municipality section)
# In R: group by municipality and section, sum the listed variables
data_2006 <- data_2006 %>%
  group_by(municipality, section) %>%
  summarise(
    listanominal = sum(listanominal, na.rm = TRUE),
    pan = sum(pan, na.rm = TRUE),
    pri = sum(pri, na.rm = TRUE),
    co_pbt = sum(`co_ pbt`, na.rm = TRUE),
    pvem = sum(pvem, na.rm = TRUE),
    conv = sum(conv, na.rm = TRUE),
    na = sum(v12, na.rm = TRUE),
    asdc = sum(asdc, na.rm = TRUE),
    panna = sum(`pan na`, na.rm = TRUE),
    pripvem = sum(`pri pvem`, na.rm = TRUE),
    total = sum(total, na.rm = TRUE)
  ) %>%
  ungroup()

# gen dummy_pan_na= (panna > 0)
data_2006$dummy_pan_na <- as.integer(data_2006$panna > 0)

# gen dummy_pri_pvem= (pripvem > 0)
data_2006$dummy_pri_pvem <- as.integer(data_2006$pripvem > 0)

# replace panna = panna + pan + na if dummy_pan_na==1
data_2006$panna[data_2006$dummy_pan_na == 1] <- data_2006$panna[data_2006$dummy_pan_na == 1] + 
  data_2006$pan[data_2006$dummy_pan_na == 1] + 
  data_2006$na[data_2006$dummy_pan_na == 1]

# replace pan = 0 if dummy_pan_na==1
data_2006$pan[data_2006$dummy_pan_na == 1] <- 0

# replace na = 0 if dummy_pan_na==1
data_2006$na[data_2006$dummy_pan_na == 1] <- 0

# drop dummy_pan_na
data_2006$dummy_pan_na <- NULL

# replace pripvem = pripvem + pri + pvem if dummy_pri_pvem==1
data_2006$pripvem[data_2006$dummy_pri_pvem == 1] <- data_2006$pripvem[data_2006$dummy_pri_pvem == 1] +
  data_2006$pri[data_2006$dummy_pri_pvem == 1] +
  data_2006$pvem[data_2006$dummy_pri_pvem == 1]

# replace pri = 0 if dummy_pri_pvem==1
data_2006$pri[data_2006$dummy_pri_pvem == 1] <- 0

# replace pvem = 0 if dummy_pri_pvem==1
data_2006$pvem[data_2006$dummy_pri_pvem == 1] <- 0

# drop dummy_pri_pvem
data_2006$dummy_pri_pvem <- NULL

# rename pan PAN
names(data_2006)[names(data_2006) == "pan"] <- "PAN"
# rename pri PRI
names(data_2006)[names(data_2006) == "pri"] <- "PRI"
# rename co_pbt PRD_PT
names(data_2006)[names(data_2006) == "co_pbt"] <- "PRD_PT"
# rename pvem PVEM
names(data_2006)[names(data_2006) == "pvem"] <- "PVEM"
# rename conv PC
names(data_2006)[names(data_2006) == "conv"] <- "PC"
# rename na PANAL
names(data_2006)[names(data_2006) == "na"] <- "PANAL"
# rename asdc ASDC
names(data_2006)[names(data_2006) == "asdc"] <- "ASDC"
# rename panna PAN_PANAL
names(data_2006)[names(data_2006) == "panna"] <- "PAN_PANAL"
# rename pripvem PRI_PVEM
names(data_2006)[names(data_2006) == "pripvem"] <- "PRI_PVEM"

# gen turnout = total/listanominal
data_2006$turnout <- data_2006$total / data_2006$listanominal

# gen uniqueid 
data_2006$uniqueid <- NA
# The following replace statements set uniqueid based on municipality
data_2006$uniqueid[data_2006$municipality == "ABASOLO"] <- 11001
data_2006$uniqueid[data_2006$municipality == "ACAMBARO"] <- 11002
data_2006$uniqueid[data_2006$municipality == "APASEO EL ALTO"] <- 11004
data_2006$uniqueid[data_2006$municipality == "APASEO EL GRANDE"] <- 11005
data_2006$uniqueid[data_2006$municipality == "ATARJEA"] <- 11006
data_2006$uniqueid[data_2006$municipality == "CELAYA"] <- 11007
data_2006$uniqueid[data_2006$municipality == "COMONFORT"] <- 11009
data_2006$uniqueid[data_2006$municipality == "CORONEO"] <- 11010
data_2006$uniqueid[data_2006$municipality == "CORTAZAR"] <- 11011
data_2006$uniqueid[data_2006$municipality == "CUERAMARO"] <- 11012
data_2006$uniqueid[data_2006$municipality == "DOCTOR MORA"] <- 11013
data_2006$uniqueid[data_2006$municipality == "DOLORES HIDALGO"] <- 11014
data_2006$uniqueid[data_2006$municipality == "GUANAJUATO"] <- 11015
data_2006$uniqueid[data_2006$municipality == "HUANIMARO"] <- 11016
data_2006$uniqueid[data_2006$municipality == "IRAPUATO"] <- 11017
data_2006$uniqueid[data_2006$municipality == "JARAL DEL PROGRESO"] <- 11018
data_2006$uniqueid[data_2006$municipality == "JERECUARO"] <- 11019
data_2006$uniqueid[data_2006$municipality == "LEON"] <- 11020
data_2006$uniqueid[data_2006$municipality == "CD. MANUEL DOBLADO"] <- 11008
data_2006$uniqueid[data_2006$municipality == "MOROLEON"] <- 11021
data_2006$uniqueid[data_2006$municipality == "OCAMPO"] <- 11022
data_2006$uniqueid[data_2006$municipality == "PENJAMO"] <- 11023
data_2006$uniqueid[data_2006$municipality == "PUEBLO NUEVO"] <- 11024
data_2006$uniqueid[data_2006$municipality == "PURISIMA DEL RINCON"] <- 11025
data_2006$uniqueid[data_2006$municipality == "ROMITA"] <- 11026
data_2006$uniqueid[data_2006$municipality == "SALAMANCA"] <- 11027
data_2006$uniqueid[data_2006$municipality == "SALVATIERRA"] <- 11028
data_2006$uniqueid[data_2006$municipality == "SAN DIEGO DE LA UNION"] <- 11029
data_2006$uniqueid[data_2006$municipality == "SAN FELIPE"] <- 11030
data_2006$uniqueid[data_2006$municipality == "SAN FRANCISCO DEL R."] <- 11031
data_2006$uniqueid[data_2006$municipality == "SAN JOSE ITURBIDE"] <- 11032
data_2006$uniqueid[data_2006$municipality == "SAN LUIS DE LA PAZ"] <- 11033
data_2006$uniqueid[data_2006$municipality == "ALLENDE"] <- 11003
data_2006$uniqueid[data_2006$municipality == "SANTA CATARINA"] <- 11034
data_2006$uniqueid[data_2006$municipality == "S.C. DE JUVETINO ROSAS"] <- 11035
data_2006$uniqueid[data_2006$municipality == "SANTIAGO MARAVATIO"] <- 11036
data_2006$uniqueid[data_2006$municipality == "SILAO"] <- 11037
data_2006$uniqueid[data_2006$municipality == "TARANDACUAO"] <- 11038
data_2006$uniqueid[data_2006$municipality == "TARIMORO"] <- 11039
data_2006$uniqueid[data_2006$municipality == "TIERRA BALNCA"] <- 11040
data_2006$uniqueid[data_2006$municipality == "URIANGATO"] <- 11041
data_2006$uniqueid[data_2006$municipality == "VALLE DE SANTIAGO"] <- 11042
data_2006$uniqueid[data_2006$municipality == "VICTORIA"] <- 11043
data_2006$uniqueid[data_2006$municipality == "VILLAGRAN"] <- 11044
data_2006$uniqueid[data_2006$municipality == "XICHU"] <- 11045
data_2006$uniqueid[data_2006$municipality == "YURIRIA"] <- 11046

# gen valid = rowtotal(PAN PRI PRD_PT PVEM PC PANAL ASDC PAN_PANAL PRI_PVEM)
data_2006$valid <- rowSums(data_2006[, 
                                     c("PAN","PRI","PRD_PT","PVEM","PC",
                                       "PANAL","ASDC","PAN_PANAL","PRI_PVEM")], 
                           na.rm = TRUE)

# gen year =2006
data_2006$year <- 2006

# gen month ="July"
data_2006$month <- "July"

# sort section
# In R: sort data frame by section
data_2006_collapsed <- data_2006[order(data_2006$section), ]
rm(data_2006)

###########################################
### Process 2009 Data
###########################################
data_2009 <- fread("../../../Data/Raw Electoral Data/Guanajuato - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2009.csv",
                   encoding = "UTF-8") %>%
  rename(municipality = NOM_MPIO,
         section = SECCION,
         listanominal = LISTA_NOM,
         total = Total,
         comn="Com\xfan") %>% 
  select(CLAVE_MPIO:total,Mayoria,comn)

data_2009 <- data_2009 %>%
  filter(!(municipality == "" & section == ".")) %>%
  filter(!is.na(total) & total != 0)

# Convert column names to lowercase
data_2009 <- data_2009 %>% rename_with(tolower) 

# Convert columns to numeric
num_cols_2009 <- c("listanominal","pan","pri",
                   "prd","pt","pvem","conv",
                   "v13","psd","pcomun",
                   "total","comn","mayoria")

data_2009 <- data_2009 %>%
  mutate(across(all_of(num_cols_2009), as.numeric))

# Collapse sum by municipality, section
data_2009 <- data_2009 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(num_cols_2009), sum, na.rm = TRUE), .groups = "drop")

# Coalition formation steps:
# dummy_pan_pvem, dummy_prd_pvem, dummy_pri_prd, dummy_pri_prd_pvem, dummy_pri_pvem

data_2009 <- data_2009 %>%
  mutate(dummy_pan_pvem = ifelse(municipality == "JERECUARO", 1, 0),
         dummy_prd_pvem = ifelse(municipality %in% c("ATARJEA","CUNA DE LA IN"), 1, 0),
         dummy_pri_prd = ifelse(municipality %in% c("ACAMBARO","CELAYA","DOCTOR MORA","GUANAJUATO","AL DEL PROGRE","OCAMPO","N LUIS DE LA P","UZ DE JUVENTI","SILAO"), 1, 0),
         dummy_pri_prd_pvem = ifelse(municipality %in% c("MIGUEL DE ALL","IRAPUATO","ANCISCO DEL R"), 1, 0),
         dummy_pri_pvem = ifelse(municipality %in% c("APASEO EL ALTO","ASEO EL GRAN","","PUEBLO NUEVO","ISIMA DEL RINC","N JOSE ITURBID","TARANDACUAO","LLE DE SANTIAG"), 1, 0))

# pan_pvem coalition
data_2009 <- data_2009 %>%
  mutate(pan_pvem = ifelse(dummy_pan_pvem == 1, comn + pcomun, 0),
         pan = ifelse(dummy_pan_pvem == 1, 0, pan),
         pvem = ifelse(dummy_pan_pvem == 1, 0, pvem),
         comn = ifelse(dummy_pan_pvem == 1, 0, comn),
         pcomun = ifelse(dummy_pan_pvem == 1, 0, pcomun)) %>%
  select(-dummy_pan_pvem)

# prd_pvem coalition
data_2009 <- data_2009 %>%
  mutate(prd_pvem = ifelse(dummy_prd_pvem == 1, comn + pcomun, 0),
         prd = ifelse(dummy_prd_pvem == 1, 0, prd),
         pvem = ifelse(dummy_prd_pvem == 1, 0, pvem),
         comn = ifelse(dummy_prd_pvem == 1, 0, comn),
         pcomun = ifelse(dummy_prd_pvem == 1, 0, pcomun)) %>%
  select(-dummy_prd_pvem)

# pri_prd coalition
data_2009 <- data_2009 %>%
  mutate(pri_prd = ifelse(dummy_pri_prd == 1, comn + pcomun, 0),
         pri = ifelse(dummy_pri_prd == 1, 0, pri),
         prd = ifelse(dummy_pri_prd == 1, 0, prd),
         comn = ifelse(dummy_pri_prd == 1, 0, comn),
         pcomun = ifelse(dummy_pri_prd == 1, 0, pcomun)) %>%
  select(-dummy_pri_prd)

# pri_prd_pvem coalition
data_2009 <- data_2009 %>%
  mutate(pri_prd_pvem = ifelse(dummy_pri_prd_pvem == 1, comn + pcomun, 0),
         pri = ifelse(dummy_pri_prd_pvem == 1, 0, pri),
         prd = ifelse(dummy_pri_prd_pvem == 1, 0, prd),
         pvem = ifelse(dummy_pri_prd_pvem == 1, 0, pvem),
         comn = ifelse(dummy_pri_prd_pvem == 1, 0, comn),
         pcomun = ifelse(dummy_pri_prd_pvem == 1, 0, pcomun)) %>%
  select(-dummy_pri_prd_pvem)

# pri_pvem coalition
data_2009 <- data_2009 %>%
  mutate(pri_pvem = ifelse(dummy_pri_pvem == 1, comn + pcomun, 0),
         pri = ifelse(dummy_pri_pvem == 1, 0, pri),
         pvem = ifelse(dummy_pri_pvem == 1, 0, pvem),
         comn = ifelse(dummy_pri_pvem == 1, 0, comn),
         pcomun = ifelse(dummy_pri_pvem == 1, 0, pcomun)) %>%
  select(-dummy_pri_pvem)

# Drop comn, pcomun, mayoria
data_2009 <- data_2009 %>%
  select(-comn, -pcomun, -mayoria)

# Rename parties to uppercase
data_2009 <- data_2009 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PC = conv,
         PANAL = v13,
         PSD = psd,
         PAN_PVEM = pan_pvem,
         PRD_PVEM = prd_pvem,
         PRI_PRD = pri_prd,
         PRI_PRD_PVEM = pri_prd_pvem,
         PRI_PVEM = pri_pvem)

# Compute turnout
data_2009 <- data_2009 %>%
  mutate(turnout = total / listanominal)

# uniqueid assignments
unique_ids_2009 <- c("ABASOLO"=11001, "ACAMBARO"=11002, "APASEO EL ALTO"=11004,
                     "ASEO EL GRAN"=11005, "ATARJEA"=11006, "CELAYA"=11007,
                     "COMONFORT"=11009, "CORONEO"=11010, "CORTAZAR"=11011,
                     "CUERAMARO"=11012, "DOCTOR MORA"=11013, "CUNA DE LA IN"=11014,
                     "GUANAJUATO"=11015, "HUANIMARO"=11016, "IRAPUATO"=11017,
                     "AL DEL PROGRE"=11018, "JERECUARO"=11019, "LEON"=11020,
                     "ANUEL DOBLAD"=11008, "MOROLEON"=11021, "OCAMPO"=11022,
                     "PENJAMO"=11023, "PUEBLO NUEVO"=11024, "ISIMA DEL RINC"=11025,
                     "ROMITA"=11026, "SALAMANCA"=11027, "SALVATIERRA"=11028,
                     "DIEGO DE LA UN"=11029, "SAN FELIPE"=11030, "ANCISCO DEL R"=11031,
                     "N JOSE ITURBID"=11032, "N LUIS DE LA P"=11033, "MIGUEL DE ALL"=11003,
                     "ANTA CATARIN"=11034, "UZ DE JUVENTI"=11035, "TIAGO MARAVA"=11036,
                     "SILAO"=11037, "TARANDACUAO"=11038, "TARIMORO"=11039,
                     "TIERRA BLANCA"=11040, "URIANGATO"=11041, "LLE DE SANTIAG"=11042,
                     "VICTORIA"=11043, "VILLAGRAN"=11044, "XICHU"=11045, "YURIRIA"=11046)

data_2009 <- data_2009 %>%
  mutate(uniqueid = unique_ids_2009[municipality])

# No municipal aggregation or winner code
# Just keep data as is.

# Add year and month
data_2009 <- data_2009 %>%
  mutate(year = 2009,
         month = "July")

# Sort by section
data_2009_collapsed <- data_2009 %>%
  arrange(section)

# Save final dataset
summary(data_2009_collapsed)
rm(data_2009)

###########################################
### Read 2012 Data
###########################################
data_2012 <- read_dta("../../../Data/Raw Electoral Data/Guanajuato - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Other/Ayu_Seccion_2012.dta") %>%
  rename(section = Casilla,
         PANAL = `NA`)

###########################################
### Compute Total Votes
###########################################

# Compute total using rowSums in R. Identify the variables included in rowtotal:
party_vars <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PANAL", "PAN_PANAL", "PRI_PVEM", "Noregistrados", "Nulos")

data_2012 <- data_2012 %>%
  mutate(total = rowSums(across(all_of(party_vars)), na.rm = TRUE)) %>%
  filter(!is.na(total) & total != 0)

###########################################
### Coalition Checks and Adjustments
###########################################

# Instead, we can create these indicators using a group_by municipality approach:
data_2012 <- data_2012 %>%
  group_by(municipality) %>%
  mutate(c_PAN_PANAL = as.numeric(sum(PAN_PANAL, na.rm = TRUE) > 0),
         c_PRI_PVEM = as.numeric(sum(PRI_PVEM, na.rm = TRUE) > 0)) %>%
  ungroup()

data_2012 <- data_2012 %>%
  mutate(
    PAN_PANAL = if_else(c_PAN_PANAL == 1, PAN_PANAL + PAN + PANAL, PAN_PANAL),
    PAN = if_else(c_PAN_PANAL == 1, 0, PAN),
    PANAL = if_else(c_PAN_PANAL == 1, 0, PANAL)
  ) %>%
  select(-c_PAN_PANAL)

data_2012 <- data_2012 %>%
  mutate(
    PRI_PVEM = if_else(c_PRI_PVEM == 1, PRI_PVEM + PRI + PVEM, PRI_PVEM),
    PRI = if_else(c_PRI_PVEM == 1, 0, PRI),
    PVEM = if_else(c_PRI_PVEM == 1, 0, PVEM)
  ) %>%
  select(-c_PRI_PVEM)

###########################################
### Collapse by Municipality and Section
###########################################

# Identify all variables from PAN to PRI_PVEM plus total:
vars_to_collapse <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PANAL", "PAN_PANAL", "PRI_PVEM", "Noregistrados", "Nulos", "total")

data_2012 <- data_2012 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(vars_to_collapse), sum, na.rm = TRUE), .groups = "drop")

###########################################
### Unique ID Assignment
###########################################

unique_ids <- c("ABASOLO" = 11001, "ACAMBARO" = 11002, "APASEO EL ALTO" = 11004, "APASEO EL GRANDE" = 11005, 
                "ATARJEA" = 11006, "CELAYA" = 11007, "COMONFORT" = 11009, "CORONEO" = 11010, "CORTAZAR" = 11011, 
                "CUERAMARO" = 11012, "DOCTOR MORA" = 11013, "DOLORES HIDALGO" = 11014, "GUANAJUATO" = 11015, 
                "HUANIMARO" = 11016, "IRAPUATO" = 11017, "JARAL DEL PROGRESO" = 11018, "JERECUARO" = 11019, 
                "LEON" = 11020, "MANUEL DOBLADO" = 11008, "MOROLEON" = 11021, "OCAMPO" = 11022, "PENJAMO" = 11023, 
                "PUEBLO NUEVO" = 11024, "PURISIMA DEL RINCON" = 11025, "ROMITA" = 11026, "SALAMANCA" = 11027, 
                "SALVATIERRA" = 11028, "SAN DIEGO DE LA UNION" = 11029, "SAN FELIPE" = 11030, "SAN FRANCISCO DEL RINCON" = 11031, 
                "SAN JOSE ITURBIDE" = 11032, "SAN LUIS DE LA PAZ" = 11033, "ALLENDE" = 11003, 
                "SANTA CATARINA" = 11034, "SANTA CRUZ DE JUVENTINO ROSAS" = 11035, "SANTIAGO MARAVATIO" = 11036, 
                "SILAO" = 11037, "TARANDACUAO" = 11038, "TARIMORO" = 11039, "TIERRA BLANCA" = 11040, 
                "URIANGATO" = 11041, "VALLE DE SANTIAGO" = 11042, "VICTORIA" = 11043, "VILLAGRAN" = 11044, 
                "XICHU" = 11045, "YURIRIA" = 11046)

data_2012 <- data_2012 %>%
  mutate(uniqueid = unique_ids[municipality])

###########################################
### Valid Votes and total
###########################################

valid_parties <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PANAL", "PAN_PANAL", "PRI_PVEM")

data_2012 <- data_2012 %>%
  mutate(valid = rowSums(across(all_of(valid_parties)), na.rm = TRUE))

###########################################
### Lista nominal and turnout
###########################################

lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "GUANAJUATO" &
           month == "July" &
           year == 2012)

data_2012 <- data_2012 %>%
  left_join(lista_nominal %>% select(section,lista), 
            by = c("section" = "section")) %>%
  mutate(listanominal = lista) %>%
  select(-lista)

###########################################
### Add Year and Month
###########################################

data_2012_collapsed <- data_2012 %>%
  mutate(year = 2012,
         month = "July",
         turnout = total/listanominal)

rm(data_2012)

data_2012_collapsed <- data_2012_collapsed %>%
  arrange(section)

###########################################
### Step 1: Process 2015 Data
###########################################
# Load 2015 data from Excel
data_2015 <- read_excel("../../../Data/Raw Electoral Data/Guanajuato - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2015.xlsx", sheet = "Sheet1", col_types = "text")

# Rename and clean columns
data_2015 <- data_2015 %>%
  rename(SECCION = SECCIÓN, 
         municipality = UBICACIÓN,
         PRI_PVEM_PANAL = "PRI-PVEM-NA",
         PRI_PVEM = "PRI-PVEM",
         PRI_PANAL = "PRI-NA",
         PVEM_PANAL = "PVEM-NA",
         PANAL ="NA") %>%
  mutate(municipality = ifelse(municipality == "DOLORES HIDALGO C.I.N.", 
                               "DOLORES HIDALGO", municipality)) %>%
  mutate(across(uniqueid:TOTAL, as.numeric))  # Convert all columns to numeric

# Assign unique IDs to municipalities
unique_ids <- c("ABASOLO" = 11001, "ACÁMBARO" = 11002, "APASEO EL ALTO" = 11004, "APASEO EL GRANDE" = 11005,
                "ATARJEA" = 11006, "CELAYA" = 11007, "COMONFORT" = 11009, "CORONEO" = 11010, "CORTAZAR" = 11011,
                "CUERÁMARO" = 11012, "DOCTOR MORA" = 11013, "DOLORES HIDALGO" = 11014, "GUANAJUATO" = 11015,
                "HUANÍMARO" = 11016, "IRAPUATO" = 11017, "JARAL DEL PROGRESO" = 11018, "JERÉCUARO" = 11019,
                "LEÓN" = 11020, "MANUEL DOBLADO" = 11008, "MOROLEÓN" = 11021, "OCAMPO" = 11022, "PÉNJAMO" = 11023,
                "PUEBLO NUEVO" = 11024, "PURÍSIMA DEL RINCÓN" = 11025, "ROMITA" = 11026, "SALAMANCA" = 11027,
                "SALVATIERRA" = 11028, "SAN DIEGO DE LA UNIÓN" = 11029, "SAN FELIPE" = 11030, "SAN FRANCISCO DEL RINCÓN" = 11031,
                "SAN JOSÉ ITURBIDE" = 11032, "SAN LUIS DE LA PAZ" = 11033, "SAN MIGUEL DE ALLENDE" = 11003,
                "SANTA CATARINA" = 11034, "SANTA CRUZ DE JUVENTINO ROSAS" = 11035, "SANTIAGO MARAVATÍO" = 11036,
                "SILAO DE LA VICTORIA" = 11037, "TARANDACUAO" = 11038, "TARIMORO" = 11039, "TIERRA BLANCA" = 11040,
                "URIANGATO" = 11041, "VALLE DE SANTIAGO" = 11042, "VICTORIA" = 11043, "VILLAGRÁN" = 11044,
                "XICHÚ" = 11045, "YURIRIA" = 11046)

data_2015 <- data_2015 %>%
  mutate(uniqueid = unique_ids[municipality])

# Handle party coalitions
data_2015 <- data_2015 %>%
  mutate(PRI_PVEM_PANAL = ifelse(!is.na(PRI_PVEM_PANAL), 
                                 PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL, 
                                 PRI_PVEM_PANAL),
         PRI = ifelse(!is.na(PRI_PVEM_PANAL), 
                                 0, 
                      PRI),
         PVEM = ifelse(!is.na(PRI_PVEM_PANAL), 
                                 0, 
                       PVEM),
         PANAL = ifelse(!is.na(PRI_PVEM_PANAL), 
                                 0, 
                        PANAL)) %>%
  mutate(across(c(PRI_PVEM, PRI_PANAL, PVEM_PANAL, PRI, PVEM, PANAL), ~ ifelse(!is.na(PRI_PVEM_PANAL), NA, .))) %>%
  select(-c(PRI_PVEM, PRI_PANAL, PVEM_PANAL))

# Summarise data by municipality, section, and uniqueid
data_2015 <- data_2015 %>%
  group_by(municipality, SECCION, uniqueid) %>%
  summarise(across(c(PAN:PRI_PVEM_PANAL, LN, TOTAL), sum, na.rm = TRUE)) %>%
  ungroup()

# Add additional columns
data_2015 <- data_2015 %>%
  rename(section = SECCION,
         listanominal = LN,
         total = TOTAL,
         CI_1 = Independiente
         ) %>%
  mutate(year = 2015, 
         month = "June")

# Calculate valid votes and turnout
data_2015 <- data_2015 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PVEM, PT, MC, PANAL, MORENA, PH, PES, PRI_PVEM_PANAL, CI_1)), na.rm = TRUE),
         turnout = total / listanominal)

data_2015_collapsed <- as.data.frame(data_2015)
summary(data_2015_collapsed)
rm(data_2015)

###########################################
### Step 2: Process 2018 Data
###########################################
# Load 2018 data
data_2018 <- read_excel("../../../Data/Raw Electoral Data/Guanajuato - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2018.xlsx", sheet = "Ayuntamientos", col_types = "text")
data_2018 <- data_2018 %>% 
  rename(municipality=MUNICIPIO,
         section=SECCION,
         total=TOTAL,
         listanominal=LN,
         PANAL="NA")

# Clean and prepare 2018 data
data_2018 <- data_2018 %>%
  mutate(municipality = ifelse(grepl("DOLORES HIDALGO", municipality), "DOLORES HIDALGO", municipality)) %>%
  mutate(across(uniqueid:listanominal, as.numeric))  # Convert all columns to numeric

# Handle MORENA_PT_PES coalition
data_2018 <- data_2018 %>%
  mutate(MORENA_PT_PES = rowSums(cbind(MORENA, PES, PT, 
                                       MORENA_PT_PES, MORENA_PT, 
                                       MORENA_PES, PT_PES), na.rm = TRUE)) %>%
  mutate(across(c(MORENA, PES, PT, MORENA_PT, MORENA_PES, PT_PES), ~ ifelse(!is.na(MORENA_PT_PES), 0, .))) %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(PAN:listanominal, sum, na.rm = TRUE)) %>%
  ungroup()

# Calculate valid votes and turnout
data_2018_collapsed <- data_2018 %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PVEM, PT, MC, PANAL, MORENA, PES, MORENA_PT_PES, CI_1, CI_2, CI_3)), na.rm = TRUE),
         turnout = total / listanominal) %>% 
  mutate(year = 2018)

rm(data_2018)

# Combine the dataframes, handling different columns by filling with NA
guanajuato_all <- bind_rows(data_1997_collapsed,
                            data_2000_collapsed,
                            data_2003_collapsed,
                            data_2006_collapsed,
                            data_2009_collapsed,
                            data_2012_collapsed,
                            data_2015_collapsed,
                            data_2018_collapsed) %>% 
  select(-c(NULO,NOREG,Nulos,Noregistrados))


data.table::fwrite(guanajuato_all,"../../../Processed Data/guanajuato/guanajuato_process_raw_data.csv")

