# Load required libraries
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(collapse)

# Step 1: Import Excel file, converting the first row to column names and all to lowercase
df <- read_excel("Ayu_Seccion_1997_No_LN.xlsx", col_names = TRUE) %>%
  mutate_all(tolower)

# Step 2: Rename columns municipio -> municipality, seccion -> section
df <- df %>%
  rename(municipality = municipio, 
         section = seccion)

# Step 3: Drop rows where municipality is empty and section is missing
df <- df %>%
  filter(!(municipality == "" & is.na(section)))

# Step 4: Destring columns from pan to total (convert to numeric)
num_cols <- c('pan', 'pri', 'prd', 'pc', 'pt', 'pvem', 'pps', 'pdm', 'total')
df <- df %>%
  mutate(across(all_of(num_cols), as.numeric))

# Step 5: Collapse data (sum) by municipality and section
df <- df %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(num_cols), sum, na.rm = TRUE))

# Step 6: Rename columns for political parties
df <- df %>%
  rename(PAN = pan, 
         PRI = pri, 
         PRD = prd, 
         PartCardenista = pc, 
         PT = pt, 
         PVEM = pvem, 
         PPS = pps, 
         PD = pdm)

# Step 7: Drop columns validos, noreg, nulos (if they exist)
df <- df %>%
  select(-validos, -noreg, -nulos)

# Step 8: Replace municipality names to uppercase and replace accented characters
df <- df %>%
  mutate(municipality = str_to_upper(municipality),
         municipality = str_replace_all(municipality, c("á" = "A", "é" = "E", "í" = "I", "ó" = "O", "ú" = "U", "ñ" = "N")))

# Step 9: Generate uniqueid based on municipality names
uniqueid_map <- c("ACATIC" = 14001, "ACATLAN DE JUAREZ" = 14002, "AHUALULCO DE MERCADO" = 14003, 
                  "AMACUECA" = 14004, "AMATITAN" = 14005, "AMECA" = 14006, 
                  "ANTONIO ESCOBEDO" = 14007, "ARANDAS" = 14008, "ATEMAJAC DE BRIZUELA" = 14010,
                  "ATENGO" = 14011, "ATENGUILLO" = 14012, "ATOTONILCO EL ALTO" = 14013, 
                  "ATOYAC" = 14014, "AUTLAN DE NAVARRO" = 14015, "AYOTLAN" = 14016, 
                  "AYUTLA" = 14017, "BOLANOS" = 14019, "CABO CORRIENTES" = 14020, 
                  "CANADAS DE OBREGON" = 14117, "CASIMIRO CASTILLO" = 14021, 
                  "CHAPALA" = 14030, "CHIMALTITAN" = 14031, "CHIQUILISTLAN" = 14032, 
                  "CIHUATLAN" = 14022, "CIUDAD GUZMAN" = 14023, "COCULA" = 14024, 
                  "COLOTLAN" = 14025, "CONCEPCION DE BUENOS AIRES" = 14026, 
                  "CUAUTITLAN" = 14027, "CUAUTLA" = 14028, "CUQUIO" = 14029, 
                  "DEGOLLADO" = 14033, "EJUTLA" = 14034, "EL ARENAL" = 14009, 
                  "EL GRULLO" = 14037, "EL LIMON" = 14054, "EL SALTO" = 14070, 
                  "ENCARNACION DE DIAZ" = 14035, "ETZATLAN" = 14036, 
                  "GOMEZ FARIAS" = 14079, "GUACHINANGO" = 14038, "GUADALAJARA" = 14039, 
                  "HOSTOTIPAQUILLO" = 14040, "HUEJUCAR" = 14041, "HUEJUQUILLA EL ALTO" = 14042, 
                  "IXTLAHUACAN DE LOS MEMBRILLOS" = 14044, "IXTLAHUACAN DEL RIO" = 14045, 
                  "JALOSTOTITLAN" = 14046, "JAMAY" = 14047, "JESUS MARIA" = 14048, 
                  "JILOTLAN DE LOS DOLORES" = 14049, "JOCOTEPEC" = 14050, 
                  "JUANACATLAN" = 14051, "JUCHITLAN" = 14052, "LA BARCA" = 14018, 
                  "LA HUERTA" = 14043, "MANZANILLA DE LA PAZ" = 14057, 
                  "LAGOS DE MORENO" = 14053, "MAGDALENA" = 14055, "MANUEL M.DIEGUEZ" = 14056, 
                  "MASCOTA" = 14058, "MAZAMITLA" = 14059, "MEXTICACAN" = 14060, 
                  "MEZQUITIC" = 14061, "MIXTLAN" = 14062, "OCOTLAN" = 14063, 
                  "OJUELOS DE JALISCO" = 14064, "PIHUAMO" = 14065, "PONCITLAN" = 14066, 
                  "PUERTO VALLARTA" = 14067, "QUITUPAN" = 14069, "SAN CRISTOBAL DE LA BARRANCA" = 14071, 
                  "SAN DIEGO DE ALEJANDRIA" = 14072, "SAN GABRIEL" = 14113, 
                  "SAN IGNACIO CERRO GORDO" = 14125, "SAN JUAN DE LOS LAGOS" = 14073, 
                  "SAN JUANITO DE ESCOBEDO" = 14007, "SAN JULIAN" = 14074, 
                  "SAN MARCOS" = 14075, "SAN MARTIN DE BOLANOS" = 14076, 
                  "SAN MARTIN HIDALGO" = 14077, "SAN MIGUEL EL ALTO" = 14078, 
                  "SAN SEBASTIAN DEL OESTE" = 14080, "SANTA MARIA DE LOS ANGELES" = 14081, 
                  "SANTA MARIA DEL ORO" = 14056, "SAYULA" = 14082, "TALA" = 14083, 
                  "TALPA DE ALLENDE" = 14084, "TAMAZULA DE GORDIANO" = 14085, 
                  "TAPALPA" = 14086, "TECALITLAN" = 14087, "TECHALUTA DE MONTENEGRO" = 14089, 
                  "TECOLOTLAN" = 14088, "TENAMAXTLAN" = 14090, "TEOCALTICHE" = 14091, 
                  "TEOCUITATLAN DE CORONA" = 14092, "TEPATITLAN DE MORELOS" = 14093, 
                  "TEQUILA" = 14094, "TEUCHITLAN" = 14095, "TIZAPAN EL ALTO" = 14096, 
                  "TLAJOMULCO DE ZUNIGA" = 14097, "TLAQUEPAQUE" = 14098, 
                  "TOLIMAN" = 14099, "TOMATLAN" = 14100, "TONALA" = 14101, 
                  "TONAYA" = 14102, "TONILA" = 14103, "TOTATICHE" = 14104, 
                  "TOTOTLAN" = 14105, "TUXCACUESCO" = 14106, "TUXCUECA" = 14107, 
                  "TUXPAN" = 14108, "UNION DE SAN ANTONIO" = 14109, "UNION DE TULA" = 14110, 
                  "VALLE DE GUADALUPE" = 14111, "VALLE DE JUAREZ" = 14112, 
                  "VILLA CORONA" = 14114, "VILLA GUERRERO" = 14115, "VILLA HIDALGO" = 14116, 
                  "VILLA OBREGON" = 14117, "VILLA PURIFICACION" = 14068, 
                  "YAHUALICA DE GONZALEZ GALLO" = 14118, "ZACOALCO DE TORRES" = 14119, 
                  "ZAPOPAN" = 14120, "ZAPOTILTIC" = 14121, "ZAPOTITLAN DE VADILLO" = 14122, 
                  "ZAPOTLAN DEL REY" = 14123, "ZAPOTLAN EL GRANDE" = 14023, 
                  "ZAPOTLANEJO" = 14124)

df <- df %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Step 10: Generate total valid votes (row sum)
df <- df %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PartCardenista, PT, PVEM, PPS, PD), na.rm = TRUE))

# Step 11: Generate municipality-level sums and inverse values
parties <- c('PAN', 'PRI', 'PRD', 'PartCardenista', 'PT', 'PVEM', 'PPS', 'PD', 'total', 'valid')
df_mun <- df %>%
  group_by(uniqueid) %>%
  summarise(across(all_of(parties), sum, na.rm = TRUE)) %>%
  mutate(across(all_of(parties), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 12: Add additional data (merge) and filter by month/year
df_extra <- fread("../../all_months_years.dta") %>%
  filter(month == 12 & year == 1997) %>%
  select(ed, section, month, year, lista)

df <- df %>%
  left_join(df_extra, by = c("section" = "section")) %>%
  rename(listanominal = lista) %>%
  filter(month == 12, year == 1997) %>%
  select(-month, -year, -ed)

# Step 13: Generate turnout rate
df <- df %>%
  mutate(turnout = total / listanominal)

# Step 14: Generate municipality-level turnout
df_mun <- df_mun %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 15: Rank by party inverse vote
df <- df %>%
  mutate(across(starts_with("inv_mun_"), ~ rank(.), .names = "{.col}_r"))

# Step 16: Determine the winning party for each section
df <- df %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PartCardenista_r == 1 ~ "PartCardenista",
    PPS_r == 1 ~ "PPS",
    PD_r == 1 ~ "PD"))

# Step 17: Final cleanup
df <- df %>%
  mutate(year = 1997, month = "November") %>%
  arrange(section)

# Step 18: Save datasets
write_dta(df, "Jalisco_Section_1997.dta")
df_for_merge <- df %>%
  select(municipality, section, uniqueid) %>%
  group_by(section) %>%
  mutate(x = row_number()) %>%
  filter(x == 1) %>%
  arrange(section)

write_dta(df_for_merge, "Jalisco_Section_1997_for_Merge.dta")

# Step 1: Read the CSV file
df <- read_csv("Ayu_Seccion_1995.csv")

# Step 2: Rename columns
df <- df %>%
  rename(municipality = municipio, section = seccion)

# Step 3: Drop rows where municipality and section are missing
df <- df %>%
  filter(!(is.na(municipality) & is.na(section)))

# Step 4: Drop the column 'votaron' (if it exists)
df <- df %>% select(-votaron)

# Step 5: Generate 'total' by summing across relevant columns
df <- df %>%
  mutate(total = rowSums(select(., pan, pri, pps, prd, pfcrn, parm, pdm, pt, pvem, ppj, nulos), na.rm = TRUE))

# Step 6: Drop rows where total is missing or zero
df <- df %>%
  filter(!is.na(total) & total != 0)

# Step 7: Convert listanominal to total columns to numeric
cols_to_convert <- c('listanominal', 'pan', 'pri', 'pps', 'prd', 'pfcrn', 'parm', 'pdm', 'pt', 'pvem', 'ppj', 'total')
df <- df %>%
  mutate(across(all_of(cols_to_convert), as.numeric))

# Step 8: Generate the 'missing' variable where listanominal equals zero
df <- df %>%
  mutate(missing = if_else(listanominal == 0, 1, 0))

# Step 9: Collapse the data (sum) by municipality and section
df <- df %>%
  group_by(municipality, section) %>%
  summarise(across(c(missing, listanominal, all_of(cols_to_convert)), sum, na.rm = TRUE))

# Step 10: Replace listanominal with missing values where missing >= 1
df <- df %>%
  mutate(listanominal = if_else(missing >= 1, NA_real_, listanominal))

# Step 11: Drop the 'missing' column
df <- df %>%
  select(-missing)

# Step 12: Rename political party columns
df <- df %>%
  rename(PAN = pan, 
         PRI = pri, 
         PRD = prd, 
         PartCardenista = pfcrn, 
         PT = pt, 
         PVEM = pvem, 
         PARM = parm, 
         PPS = pps, 
         PD = pdm, 
         PPJ = ppj)

# Step 13: Generate turnout variable
df <- df %>%
  mutate(turnout = total / listanominal)

# Step 14: Drop 'nulos' column
df <- df %>% select(-nulos)

# Step 15: Generate 'valid' as the sum of votes across all relevant columns
df <- df %>%
  mutate(valid = rowSums(select(., PAN, PRI, PPS, PRD, PartCardenista, PARM, PD, PT, PVEM, PPJ), na.rm = TRUE))

# Step 16: Municipality-level aggregation and inverse calculation
parties <- c("PAN", "PRI", "PPS", "PRD", "PartCardenista", "PARM", "PD", "PT", "PVEM", "PPJ", "total", "valid")
df_mun <- df %>%
  group_by(municipality) %>%
  summarise(across(all_of(parties), sum, na.rm = TRUE)) %>%
  mutate(across(everything(), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 17: Merge back to the original dataframe
df <- left_join(df, df_mun, by = "municipality")

# Step 18: Generate 'mun_turnout' (set to NA initially)
df <- df %>%
  mutate(mun_turnout = NA_real_)

# Step 19: Rank the inverse vote values
df <- df %>%
  mutate(across(starts_with("inv_mun_"), ~ rank(.), .names = "{.col}_r"))

# Step 20: Determine the winner for each section
df <- df %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PartCardenista_r == 1 ~ "PartCardenista",
    inv_mun_PPS_r == 1 ~ "PPS",
    inv_mun_PD_r == 1 ~ "PD",
    inv_mun_PARM_r == 1 ~ "PARM",
    inv_mun_PPJ_r == 1 ~ "PPJ"))

# Step 21: Drop the rank columns
df <- df %>% select(-starts_with("inv_mun_"))

# Step 22: Add year and month columns
df <- df %>%
  mutate(year = 1995, month = "February")

# Step 23: Drop municipality column
df <- df %>% select(-municipality)

# Step 24: Merge with external dataset on 'section'
df_merge <- fread("Jalisco_Section_1997_for_Merge.dta")
df <- merge(df, df_merge, by = "section", all.x = TRUE)

# Step 25: Handle cases where uniqueid is missing for specific sections
df <- df %>%
  mutate(uniqueid = case_when(
    is.na(uniqueid) & section == 2300 ~ 14089,
    is.na(uniqueid) & section == 3338 ~ 14092,
    TRUE ~ uniqueid))

# Step 26: Replace municipality names for specific sections
df <- df %>%
  mutate(municipality = case_when(
    uniqueid == 14089 & section == 2300 ~ "Techaluta de Montenegro",
    uniqueid == 14092 & section == 3338 ~ "Teocuitatlan de Corona",
    TRUE ~ municipality))

# Step 27: Drop unnecessary columns
df <- df %>%
  select(-turnout, -listanominal, -mun_turnout)

# Step 28: Save the final dataset to .dta format
write_dta(df, "Jalisco_Section_1995.dta")

# Step 1: Read the CSV file
df <- read_csv("Ayu_Seccion_2000_No_LN.csv")

# Step 2: Rename columns municipio -> municipality, seccion -> section
df <- df %>%
  rename(municipality = municipio, section = seccion)

# Step 3: Drop rows where both municipality and section are missing
df <- df %>%
  filter(!(is.na(municipality) & is.na(section)))

# Step 4: Drop "weird" cases based on municipality and section values
weird_cases <- list(
  c(99, 924), c(99, 925), c(99, 926), c(99, 928), 
  c(99, 2465), c(120, 2465), c(99, 3172), c(99, 3174), 
  c(99, 3199), c(99, 3206), c(99, 3223), c(99, 3225)
)

df <- df %>%
  filter(!(map2_lgl(municipality, section, ~ c(.x, .y) %in% weird_cases)))

# Step 5: Create 'total' by summing across relevant columns
df <- df %>%
  mutate(total = rowSums(select(., pan, pri, prd, pt, pvem, convergencia, pcd, psn, parm, pas, ds), na.rm = TRUE))

# Step 6: Drop rows where total is missing or zero
df <- df %>%
  filter(!is.na(total) & total != 0)

# Step 7: Convert columns from 'pan' to 'total' to numeric (destring)
cols_to_convert <- c('pan', 'pri', 'prd', 'pt', 'pvem', 'convergencia', 'pcd', 'psn', 'parm', 'pas', 'ds', 'total')
df <- df %>%
  mutate(across(all_of(cols_to_convert), as.numeric))

# Step 8: Collapse (sum) by municipality and section
df <- df %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(cols_to_convert), sum, na.rm = TRUE))

# Step 9: Rename columns for political parties
df <- df %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PC = convergencia, PT = pt, PVEM = pvem, 
         PCD = pcd, PARM = parm, PAS = pas, PDS = ds, PSN = psn)

# Step 10: Drop unnecessary columns (noregistrados, nulos if they exist)
df <- df %>%
  select(-noregistrados, -nulos)

# Step 11: Calculate the total valid votes
df <- df %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, PCD, PSN, PARM, PAS, PDS), na.rm = TRUE))

# Step 12: Generate municipality-level sums and inverse values
parties <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PCD", "PSN", "PARM", "PAS", "PDS", "total", "valid")
df_mun <- df %>%
  group_by(municipality) %>%
  summarise(across(all_of(parties), sum, na.rm = TRUE)) %>%
  mutate(across(everything(), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 13: Merge the municipality-level data back to the original dataframe
df <- left_join(df, df_mun, by = "municipality")

# Step 14: Create 'ed' and 'seccion' variables
df <- df %>%
  mutate(ed = 14, seccion = section)

# Step 15: Merge with external dataset using 'ed' and 'seccion'
df_extra <- fread("../../all_months_years.dta") %>%
  filter(month == 12 & year == 2000)

df <- merge(df, df_extra, by = c("ed", "seccion"), all.x = TRUE) %>%
  filter(month == 12, year == 2000) %>%
  select(-_merge, -ed, -seccion, -year, -month) %>%
  rename(listanominal = lista)

# Step 16: Calculate turnout
df <- df %>%
  mutate(turnout = total / listanominal)

# Step 17: Municipality-level aggregation for turnout
df_mun_turnout <- df %>%
  group_by(municipality) %>%
  summarise(mun_total = sum(total, na.rm = TRUE),
            mun_listanominal = sum(listanominal, na.rm = TRUE)) %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

df <- left_join(df, df_mun_turnout, by = "municipality")

# Step 18: Rank political party vote inverses
df <- df %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r"))

# Step 19: Determine the winner based on ranks
df <- df %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PCD_r == 1 ~ "PCD",
    PSN_r == 1 ~ "PSN",
    PARM_r == 1 ~ "PARM",
    PAS_r == 1 ~ "PAS",
    PDS_r == 1 ~ "PDS"))

# Step 20: Drop rank columns
df <- df %>% select(-starts_with("inv_mun_"))

# Step 21: Add year and month columns
df <- df %>%
  mutate(year = 2000, month = "November")

# Step 22: Drop municipality column
df <- df %>% select(-municipality)

# Step 23: Merge with external dataset (Jalisco_Section_1997_for_Merge.dta) on 'section'
df_merge <- fread("Jalisco_Section_1997_for_Merge.dta")
df <- merge(df, df_merge, by = "section", all.x = TRUE)

# Step 24: Handle specific section cases for missing unique IDs
df <- df %>%
  mutate(uniqueid = case_when(
    is.na(uniqueid) & section == 2300 ~ 14089,
    is.na(uniqueid) & section == 3310 ~ 14098,
    is.na(uniqueid) & section == 3311 ~ 14101,
    TRUE ~ uniqueid))

# Step 25: Replace municipality names based on uniqueid and section
df <- df %>%
  mutate(municipality = case_when(
    uniqueid == 14089 & section == 2300 ~ "Techaluta de Montenegro",
    uniqueid == 14098 & section == 3310 ~ "Tlaquepaque",
    uniqueid == 14101 & section == 3311 ~ "Tonala",
    TRUE ~ municipality))

# Step 26: Save the final dataset to a .dta file
write_dta(df, "Jalisco_Section_2000.dta")

# Step 27: Load Jalisco_Section_1997.dta, filter, and save
df_1997 <- fread("Jalisco_Section_1997.dta") %>%
  filter(total != 0)

write_dta(df_1997, "Jalisco_Section_1997.dta")

# Step 1: Read the CSV file Ayu_Seccion_2003_No_LN.csv and destring
df <- read_csv("Ayu_Seccion_2003_No_LN.csv") %>%
  mutate(across(everything(), as.numeric))

# Step 2: Save the dataset as Ayu_Seccion_2003_No_LN.dta
write_dta(df, "Ayu_Seccion_2003_No_LN.dta")

# Step 3: Read the extra CSV file Ayu_Seccion_2003_No_LN_Extra.csv and destring
df_extra <- read_csv("Ayu_Seccion_2003_No_LN_Extra.csv") %>%
  mutate(across(everything(), as.numeric))

# Step 4: Save the extra dataset as Ayu_Seccion_2003_No_LN_Extra.dta
write_dta(df_extra, "Ayu_Seccion_2003_No_LN_Extra.dta")

# Step 5: Load the two datasets and append them together
df <- fread("Ayu_Seccion_2003_No_LN.dta")
df_extra <- fread("Ayu_Seccion_2003_No_LN_Extra.dta")
df <- bind_rows(df, df_extra)

# Step 6: Rename municipio to municipality and seccion to section
df <- df %>%
  rename(municipality = municipio, section = seccion)

# Step 7: Drop rows where both municipality and section are missing
df <- df %>%
  filter(!(municipality == "" & is.na(section)))

# Step 8: Generate 'total' by summing specific vote columns
df <- df %>%
  mutate(total = rowSums(select(., pan, pri, prd, pt, pvem, pc, psn, pas, elbarzon, mexicoposible, plm, fc, prdpas, prdpc, prdpvem, pvempas, prdpvempas), na.rm = TRUE))

# Step 9: Drop rows where total is missing or zero
df <- df %>%
  filter(!is.na(total) & total != 0)

# Step 10: Collapse (sum) votes by municipality and section
df <- df %>%
  group_by(municipality, section) %>%
  summarise(across(c(pan:total), sum, na.rm = TRUE))

# Step 11: Rename political party columns
df <- df %>%
  rename(PAN = pan, PRI = pri, PRD = prd, PC = pc, PT = pt, PVEM = pvem, 
         PSN = psn, PAS = pas, ElBarzon = elbarzon, MexicoPosible = mexicoposible, 
         PLM = plm, FC = fc, PRD_PAS = prdpas, PRD_PC = prdpc, PRD_PVEM = prdpvem, 
         PVEM_PAS = pvempas, PRD_PVEM_PAS = prdpvempas)

# Step 12: Generate unique IDs for municipalities
uniqueid_map <- c(
  "ACATIC" = 14001, "ACATLAN DE JUAREZ" = 14002, "AHUALULCO DE MERCADO" = 14003,
  "AMACUECA" = 14004, "AMATITAN" = 14005, "AMECA" = 14006, "ANTONIO ESCOBEDO" = 14007,
  "ARANDAS" = 14008, "ATEMAJAC DE BRIZUELA" = 14010, "ATENGO" = 14011,
  "ATENGUILLO" = 14012, "ATOTONILCO EL ALTO" = 14013, "ATOYAC" = 14014,
  "AUTLAN DE NAVARRO" = 14015, "AYOTLAN" = 14016, "AYUTLA" = 14017,
  "BOLANOS" = 14019, "CABO CORRIENTES" = 14020, "CANADAS DE OBREGON" = 14117,
  "CASIMIRO CASTILLO" = 14021, "CHAPALA" = 14030, "CHIMALTITAN" = 14031,
  "CHIQUILISTLAN" = 14032, "CIHUATLAN" = 14022, "CIUDAD GUZMAN" = 14023,
  "COCULA" = 14024, "COLOTLAN" = 14025, "CONCEPCION DE BUENOS AIRES" = 14026,
  "CUAUTITLAN" = 14027, "CUAUTLA" = 14028, "CUQUIO" = 14029, "DEGOLLADO" = 14033,
  "EJUTLA" = 14034, "EL ARENAL" = 14009, "EL GRULLO" = 14037, "EL LIMON" = 14054,
  "EL SALTO" = 14070, "ENCARNACION DE DIAZ" = 14035, "ETZATLAN" = 14036,
  "GOMEZ FARIAS" = 14079, "GUACHINANGO" = 14038, "GUADALAJARA" = 14039,
  "HOSTOTIPAQUILLO" = 14040, "HUEJUCAR" = 14041, "HUEJUQUILLA EL ALTO" = 14042,
  "IXTLAHUACAN DE LOS MEMBRILLOS" = 14044, "IXTLAHUACAN DEL RIO" = 14045,
  "JALOSTOTITLAN" = 14046, "JAMAY" = 14047, "JESUS MARIA" = 14048,
  "JILOTLAN DE LOS DOLORES" = 14049, "JOCOTEPEC" = 14050, "JUANACATLAN" = 14051,
  "JUCHITLAN" = 14052, "LA BARCA" = 14018, "LA HUERTA" = 14043,
  "MANZANILLA DE LA PAZ" = 14057, "LAGOS DE MORENO" = 14053, "MAGDALENA" = 14055,
  "MANUEL M.DIEGUEZ" = 14056, "MASCOTA" = 14058, "MAZAMITLA" = 14059,
  "MEXTICACAN" = 14060, "MEZQUITIC" = 14061, "MIXTLAN" = 14062, "OCOTLAN" = 14063,
  "OJUELOS DE JALISCO" = 14064, "PIHUAMO" = 14065, "PONCITLAN" = 14066,
  "PUERTO VALLARTA" = 14067, "QUITUPAN" = 14069, "SAN CRISTOBAL DE LA BARRANCA" = 14071,
  "SAN DIEGO DE ALEJANDRIA" = 14072, "SAN GABRIEL" = 14113, "SAN IGNACIO CERRO GORDO" = 14125,
  "SAN JUAN DE LOS LAGOS" = 14073, "SAN JUANITO DE ESCOBEDO" = 14007,
  "SAN JULIAN" = 14074, "SAN MARCOS" = 14075, "SAN MARTIN DE BOLANOS" = 14076,
  "SAN MARTIN HIDALGO" = 14077, "SAN MIGUEL EL ALTO" = 14078, "SAN SEBASTIAN DEL OESTE" = 14080,
  "SANTA MARIA DE LOS ANGELES" = 14081, "SANTA MARIA DEL ORO" = 14056,
  "SAYULA" = 14082, "TALA" = 14083, "TALPA DE ALLENDE" = 14084,
  "TAMAZULA DE GORDIANO" = 14085, "TAPALPA" = 14086, "TECALITLAN" = 14087,
  "TECHALUTA DE MONTENEGRO" = 14089, "TECOLOTLAN" = 14088, "TENAMAXTLAN" = 14090,
  "TEOCALTICHE" = 14091, "TEOCUITATLAN DE CORONA" = 14092, "TEPATITLAN DE MORELOS" = 14093,
  "TEQUILA" = 14094, "TEUCHITLAN" = 14095, "TIZAPAN EL ALTO" = 14096,
  "TLAJOMULCO DE ZUNIGA" = 14097, "TLAQUEPAQUE" = 14098, "TOLIMAN" = 14099,
  "TOMATLAN" = 14100, "TONALA" = 14101, "TONAYA" = 14102, "TONILA" = 14103,
  "TOTATICHE" = 14104, "TOTOTLAN" = 14105, "TUXCACUESCO" = 14106, "TUXCUECA" = 14107,
  "TUXPAN" = 14108, "UNION DE SAN ANTONIO" = 14109, "UNION DE TULA" = 14110,
  "VALLE DE GUADALUPE" = 14111, "VALLE DE JUAREZ" = 14112, "VILLA CORONA" = 14114,
  "VILLA GUERRERO" = 14115, "VILLA HIDALGO" = 14116, "VILLA OBREGON" = 14117,
  "VILLA PURIFICACION" = 14068, "YAHUALICA DE GONZALEZ GALLO" = 14118,
  "ZACOALCO DE TORRES" = 14119, "ZAPOPAN" = 14120, "ZAPOTILTIC" = 14121,
  "ZAPOTITLAN DE VADILLO" = 14122, "ZAPOTLAN DEL REY" = 14123,
  "ZAPOTLAN EL GRANDE" = 14023, "ZAPOTLANEJO" = 14124
)
df <- df %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Step 13: Generate valid vote totals
df <- df %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PC, PSN, PAS, ElBarzon, MexicoPosible, PLM, FC, PRD_PAS, PRD_PC, PRD_PVEM, PVEM_PAS, PRD_PVEM_PAS), na.rm = TRUE))

# Step 14: Aggregate votes by uniqueid
party_list <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PC", "PSN", "PAS", "ElBarzon", "MexicoPosible", "PLM", "FC", "PRD_PAS", "PRD_PC", "PRD_PVEM", "PVEM_PAS", "PRD_PVEM_PAS", "total", "valid")
df <- df %>%
  group_by(uniqueid) %>%
  summarise(across(all_of(party_list), sum, na.rm = TRUE)) %>%
  mutate(across(all_of(party_list), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 15: Drop section 2484 (duplicate section)
df <- df %>%
  filter(section != 2484)

# Step 16: Merge with external dataset and process
df_extra <- fread("../../all_months_years.dta") %>%
  filter(month == 7 & year == 2003)

df <- merge(df, df_extra, by = c("section"), all.x = TRUE) %>%
  filter(!is.na(listanominal)) %>%
  mutate(turnout = total / listanominal)

# Step 17: Rank votes and determine the winner
df <- df %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r")) %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PSN_r == 1 ~ "PSN",
    PAS_r == 1 ~ "PAS",
    ElBarzon_r == 1 ~ "ElBarzon",
    MexicoPosible_r == 1 ~ "MexicoPosible",
    PLM_r == 1 ~ "PLM",
    FC_r == 1 ~ "FC",
    PRD_PAS_r == 1 ~ "PRD_PAS",
    PRD_PC_r == 1 ~ "PRD_PC",
    PRD_PVEM_r == 1 ~ "PRD_PVEM",
    PVEM_PAS_r == 1 ~ "PVEM_PAS",
    PRD_PVEM_PAS_r == 1 ~ "PRD_PVEM_PAS"
  )) %>%
  select(-starts_with("inv_mun_"))

# Step 18: Add year and month columns
df <- df %>%
  mutate(year = 2003, month = "July")

# Step 19: Save the final dataset
write_dta(df, "Jalisco_Section_2003.dta")

# Step 1: Import the Excel file and make columns lowercase
df <- read_excel("Ayu_Seccion_2006_No_LN.xlsx") %>%
  rename_all(tolower)

# Step 2: Rename columns
df <- df %>%
  rename(municipality = municipio, section = casillas)

# Step 3: Drop rows where municipality and section are missing, and filter out invalid totals
df <- df %>%
  filter(municipality != "" & !is.na(section)) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Destring columns from 'pan' to 'total'
cols_to_convert <- c('pan', 'pri', 'prdpt', 'pc', 'pvem', 'panal', 'pas', 'total')
df <- df %>%
  mutate(across(all_of(cols_to_convert), as.numeric))

# Step 5: Collapse data (sum) by municipality and section
df <- df %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(cols_to_convert), sum, na.rm = TRUE))

# Step 6: Rename political party columns
df <- df %>%
  rename(PAN = pan, PRI = pri, PRD_PT = prdpt, PC = pc, PVEM = pvem, PANAL = panal, PAS = pas)

# Step 7: Replace accents in municipality names
df <- df %>%
  mutate(municipality = str_replace_all(municipality, c("Á" = "A", "Í" = "I", "Ó" = "O", "Ú" = "U", "Ñ" = "N")))

# Step 8: Generate unique IDs for municipalities
uniqueid_map <- c(
  "ACATIC" = 14001, "ACATLAN DE JUAREZ" = 14002, "AHUALULCO DE MERCADO" = 14003,
  "AMACUECA" = 14004, "AMATITAN" = 14005, "AMECA" = 14006, "ANTONIO ESCOBEDO" = 14007,
  "ARANDAS" = 14008, "ATEMAJAC DE BRIZUELA" = 14010, "ATENGO" = 14011, "ATENGUILLO" = 14012,
  "ATOTONILCO EL ALTO" = 14013, "ATOYAC" = 14014, "AUTLAN DE NAVARRO" = 14015, "AYOTLAN" = 14016,
  "AYUTLA" = 14017, "BOLANOS" = 14019, "CABO CORRIENTES" = 14020, "CANADAS DE OBREGON" = 14117,
  "CASIMIRO CASTILLO" = 14021, "CHAPALA" = 14030, "CHIMALTITAN" = 14031, "CHIQUILISTLAN" = 14032,
  "CIHUATLAN" = 14022, "CIUDAD GUZMAN" = 14023, "COCULA" = 14024, "COLOTLAN" = 14025,
  "CONCEPCION DE BUENOS AIRES" = 14026, "CUAUTITLAN DE GARCIA BARRAGAN" = 14027,
  "CUAUTLA" = 14028, "CUQUIO" = 14029, "DEGOLLADO" = 14033, "EJUTLA" = 14034, "EL ARENAL" = 14009,
  "EL GRULLO" = 14037, "EL LIMON" = 14054, "EL SALTO" = 14070, "ENCARNACION DE DIAZ" = 14035,
  "ETZATLAN" = 14036, "GOMEZ FARIAS" = 14079, "GUACHINANGO" = 14038, "GUADALAJARA" = 14039,
  "HOSTOTIPAQUILLO" = 14040, "HUEJUCAR" = 14041, "HUEJUQUILLA EL ALTO" = 14042,
  "IXTLAHUACAN DE LOS MEMBRILLOS" = 14044, "IXTLAHUACAN DEL RIO" = 14045, "JALOSTOTITLAN" = 14046,
  "JAMAY" = 14047, "JESUS MARIA" = 14048, "JILOTLAN DE LOS DOLORES" = 14049, "JOCOTEPEC" = 14050,
  "JUANACATLAN" = 14051, "JUCHITLAN" = 14052, "LA BARCA" = 14018, "LA HUERTA" = 14043,
  "MANZANILLA DE LA PAZ" = 14057, "LAGOS DE MORENO" = 14053, "MAGDALENA" = 14055, 
  "MASCOTA" = 14058, "MAZAMITLA" = 14059, "MEXTICACAN" = 14060, "MEZQUITIC" = 14061,
  "MIXTLAN" = 14062, "OCOTLAN" = 14063, "OJUELOS DE JALISCO" = 14064, "PIHUAMO" = 14065,
  "PONCITLAN" = 14066, "PUERTO VALLARTA" = 14067, "QUITUPAN" = 14069, "SAN CRISTOBAL DE LA BARRANCA" = 14071,
  "SAN DIEGO DE ALEJANDRIA" = 14072, "SAN GABRIEL" = 14113, "SAN IGNACIO CERRO GORDO" = 14125,
  "SAN JUAN DE LOS LAGOS" = 14073, "SAN JUANITO DE ESCOBEDO" = 14007, "SAN JULIAN" = 14074,
  "SAN MARCOS" = 14075, "SAN MARTIN DE BOLANOS" = 14076, "SAN MARTIN HIDALGO" = 14077,
  "SAN MIGUEL EL ALTO" = 14078, "SAN SEBASTIAN DEL OESTE" = 14080, "SANTA MARIA DE LOS ANGELES" = 14081,
  "SANTA MARIA DEL ORO" = 14056, "SAYULA" = 14082, "TALA" = 14083, "TALPA DE ALLENDE" = 14084,
  "TAMAZULA DE GORDIANO" = 14085, "TAPALPA" = 14086, "TECALITLAN" = 14087, "TECHALUTA DE MONTENEGRO" = 14089,
  "TECOLOTLAN" = 14088, "TENAMAXTLAN" = 14090, "TEOCALTICHE" = 14091, "TEOCUITATLAN DE CORONA" = 14092,
  "TEPATITLAN DE MORELOS" = 14093, "TEQUILA" = 14094, "TEUCHITLAN" = 14095, "TIZAPAN EL ALTO" = 14096,
  "TLAJOMULCO DE ZUNIGA" = 14097, "TLAQUEPAQUE" = 14098, "TOLIMAN" = 14099, "TOMATLAN" = 14100,
  "TONALA" = 14101, "TONAYA" = 14102, "TONILA" = 14103, "TOTATICHE" = 14104, "TOTOTLAN" = 14105,
  "TUXCACUESCO" = 14106, "TUXCUECA" = 14107, "TUXPAN" = 14108, "UNION DE SAN ANTONIO" = 14109,
  "UNION DE TULA" = 14110, "VALLE DE GUADALUPE" = 14111, "VALLE DE JUAREZ" = 14112, 
  "VILLA CORONA" = 14114, "VILLA GUERRERO" = 14115, "VILLA HIDALGO" = 14116,
  "VILLA OBREGON" = 14117, "VILLA PURIFICACION" = 14068, "YAHUALICA DE GONZALEZ GALLO" = 14118,
  "ZACOALCO DE TORRES" = 14119, "ZAPOPAN" = 14120, "ZAPOTILTIC" = 14121, "ZAPOTITLAN DE VADILLO" = 14122,
  "ZAPOTLAN DEL REY" = 14123, "ZAPOTLAN EL GRANDE" = 14023, "ZAPOTLANEJO" = 14124
)

df <- df %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Step 9: Calculate valid votes by summing party votes
df <- df %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD_PT, PVEM, PC, PANAL, PAS), na.rm = TRUE))

# Step 10: Aggregate votes by uniqueid and generate inverse values
party_list <- c("PAN", "PRI", "PRD_PT", "PVEM", "PC", "PANAL", "PAS", "total", "valid")
df <- df %>%
  group_by(uniqueid) %>%
  summarise(across(all_of(party_list), sum, na.rm = TRUE)) %>%
  mutate(across(all_of(party_list), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 11: Remove duplicated section numbers
df <- df %>%
  group_by(section) %>%
  filter(row_number() == 1)

# Step 12: Merge with external data (all_months_years.dta) and filter for July 2006
df_extra <- fread("../../all_months_years.dta") %>%
  filter(month == 7 & year == 2006)

df <- merge(df, df_extra, by = "section", all.x = TRUE) %>%
  select(-c(_merge, ed, seccion, year, month)) %>%
  rename(listanominal = lista)

# Step 13: Calculate turnout
df <- df %>%
  mutate(turnout = total / listanominal)

# Step 14: Municipality-level turnout
df_mun_turnout <- df %>%
  group_by(uniqueid) %>%
  summarise(mun_total = sum(total, na.rm = TRUE),
            mun_listanominal = sum(listanominal, na.rm = TRUE)) %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

df <- left_join(df, df_mun_turnout, by = "uniqueid")

# Step 15: Rank political party inverse vote shares
df <- df %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r"))

# Step 16: Determine the winner
df <- df %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PRD_PT_r == 1 ~ "PRD_PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PANAL_r == 1 ~ "PANAL",
    PAS_r == 1 ~ "PAS"
  ))

# Step 17: Clean up the dataset and add year and month columns
df <- df %>%
  select(-starts_with("inv_mun_")) %>%
  mutate(year = 2006, month = "July")

# Step 18: Save the final dataset as .dta
write_dta(df, "Jalisco_Section_2006.dta")

# Load necessary libraries
library(readxl)
library(dplyr)
library(data.table)
library(collapse)

# Step 1: Import the Excel file and make columns lowercase
df <- read_excel("Ayu_Seccion_2009_No_LN.xlsx") %>%
  rename_all(tolower)

# Step 2: Rename columns
df <- df %>%
  rename(municipality = municipio, section = seccion)

# Step 3: Drop rows where total is missing or zero
df <- df %>%
  filter(!is.na(total) & total != 0)

# Step 4: Generate dummy variables
df <- df %>%
  mutate(dummy_pripanal = !is.na(totalpripanal),
         dummy_prdpt = !is.na(totalprdpt),
         dummy_ptpc = !is.na(totalptconvergencia))

# Step 5: Drop rows where both municipality and section are missing
df <- df %>%
  filter(municipality != "" & !is.na(section))

# Step 6: Destring columns from 'pan' to 'dummy_ptpc'
cols_to_convert <- c('pan', 'pri', 'prd', 'pt', 'pvem', 'convergencia', 'psd', 'totalpripanal', 'totalprdpt', 'totalptconvergencia', 'total', 'dummy_pripanal', 'dummy_prdpt', 'dummy_ptpc')
df <- df %>%
  mutate(across(all_of(cols_to_convert), as.numeric))

# Step 7: Collapse data (sum) by municipality and section
df <- df %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(cols_to_convert), sum, na.rm = TRUE))

# Step 8: Replace dummy variables to ensure binary values
df <- df %>%
  mutate(dummy_pripanal = ifelse(dummy_pripanal > 0, 1, 0),
         dummy_prdpt = ifelse(dummy_prdpt > 0, 1, 0),
         dummy_ptpc = ifelse(dummy_ptpc > 0, 1, 0))

# Step 9: Modify votes based on dummy variables and drop columns
df <- df %>%
  mutate(pri = ifelse(dummy_pripanal == 1, 0, pri),
         panal = ifelse(dummy_pripanal == 1, 0, panal),
         pripanal = ifelse(dummy_pripanal == 1, 0, pripanal),
         prd = ifelse(dummy_prdpt == 1, 0, prd),
         pt = ifelse(dummy_prdpt == 1, 0, pt),
         prdpt = ifelse(dummy_prdpt == 1, 0, prdpt),
         pt = ifelse(dummy_ptpc == 1, 0, pt),
         convergencia = ifelse(dummy_ptpc == 1, 0, convergencia),
         ptconvergencia = ifelse(dummy_ptpc == 1, 0, ptconvergencia)) %>%
  select(-pri, -panal, -prdpt, -ptconvergencia, -dummy_pripanal, -dummy_prdpt, -dummy_ptpc)

# Step 10: Rename columns for political parties
df <- df %>%
  rename(PAN = pan, PRI_PANAL = pripanal, PRD = prd, PT = pt, PVEM = pvem, PC = convergencia, PSD = psd, PRD_PT = totalprdpt, PT_PC = totalptconvergencia)

# Step 11: Replace accents in municipality names
df <- df %>%
  mutate(municipality = str_replace_all(municipality, c("Á" = "A", "Í" = "I", "Ó" = "O", "Ú" = "U", "Ñ" = "N")))

# Step 12: Generate unique IDs for municipalities
uniqueid_map <- c(
  "ACATIC" = 14001, "ACATLAN DE JUAREZ" = 14002, "AHUALULCO DE MERCADO" = 14003,
  "AMACUECA" = 14004, "AMATITAN" = 14005, "AMECA" = 14006, "ANTONIO ESCOBEDO" = 14007,
  "ARANDAS" = 14008, "ATEMAJAC DE BRIZUELA" = 14010, "ATENGO" = 14011, "ATENGUILLO" = 14012,
  "ATOTONILCO EL ALTO" = 14013, "ATOYAC" = 14014, "AUTLAN DE NAVARRO" = 14015, "AYOTLAN" = 14016,
  "AYUTLA" = 14017, "BOLANOS" = 14019, "CABO CORRIENTES" = 14020, "CANADAS DE OBREGON" = 14117,
  "CASIMIRO CASTILLO" = 14021, "CHAPALA" = 14030, "CHIMALTITAN" = 14031, "CHIQUILISTLAN" = 14032,
  "CIHUATLAN" = 14022, "CIUDAD GUZMAN" = 14023, "COCULA" = 14024, "COLOTLAN" = 14025,
  "CONCEPCION DE BUENOS AIRES" = 14026, "CUAUTITLAN DE GARCIA BARRAGAN" = 14027,
  "CUAUTLA" = 14028, "CUQUIO" = 14029, "DEGOLLADO" = 14033, "EJUTLA" = 14034, "EL ARENAL" = 14009,
  "EL GRULLO" = 14037, "EL LIMON" = 14054, "EL SALTO" = 14070, "ENCARNACION DE DIAZ" = 14035,
  "ETZATLAN" = 14036, "GOMEZ FARIAS" = 14079, "GUACHINANGO" = 14038, "GUADALAJARA" = 14039,
  "HOSTOTIPAQUILLO" = 14040, "HUEJUCAR" = 14041, "HUEJUQUILLA EL ALTO" = 14042,
  "IXTLAHUACAN DE LOS MEMBRILLOS" = 14044, "IXTLAHUACAN DEL RIO" = 14045, "JALOSTOTITLAN" = 14046,
  "JAMAY" = 14047, "JESUS MARIA" = 14048, "JILOTLAN DE LOS DOLORES" = 14049, "JOCOTEPEC" = 14050,
  "JUANACATLAN" = 14051, "JUCHITLAN" = 14052, "LA BARCA" = 14018, "LA HUERTA" = 14043,
  "MANZANILLA DE LA PAZ" = 14057, "LAGOS DE MORENO" = 14053, "MAGDALENA" = 14055, 
  "MASCOTA" = 14058, "MAZAMITLA" = 14059, "MEXTICACAN" = 14060, "MEZQUITIC" = 14061,
  "MIXTLAN" = 14062, "OCOTLAN" = 14063, "OJUELOS DE JALISCO" = 14064, "PIHUAMO" = 14065,
  "PONCITLAN" = 14066, "PUERTO VALLARTA" = 14067, "QUITUPAN" = 14069, "SAN CRISTOBAL DE LA BARRANCA" = 14071,
  "SAN DIEGO DE ALEJANDRIA" = 14072, "SAN GABRIEL" = 14113, "SAN IGNACIO CERRO GORDO" = 14125,
  "SAN JUAN DE LOS LAGOS" = 14073, "SAN JUANITO DE ESCOBEDO" = 14007, "SAN JULIAN" = 14074,
  "SAN MARCOS" = 14075, "SAN MARTIN DE BOLANOS" = 14076, "SAN MARTIN HIDALGO" = 14077,
  "SAN MIGUEL EL ALTO" = 14078, "SAN SEBASTIAN DEL OESTE" = 14080, "SANTA MARIA DE LOS ANGELES" = 14081,
  "SANTA MARIA DEL ORO" = 14056, "SAYULA" = 14082, "TALA" = 14083, "TALPA DE ALLENDE" = 14084,
  "TAMAZULA DE GORDIANO" = 14085, "TAPALPA" = 14086, "TECALITLAN" = 14087, "TECHALUTA DE MONTENEGRO" = 14089,
  "TECOLOTLAN" = 14088, "TENAMAXTLAN" = 14090, "TEOCALTICHE" = 14091, "TEOCUITATLAN DE CORONA" = 14092,
  "TEPATITLAN DE MORELOS" = 14093, "TEQUILA" = 14094, "TEUCHITLAN" = 14095, "TIZAPAN EL ALTO" = 14096,
  "TLAJOMULCO DE ZUNIGA" = 14097, "TLAQUEPAQUE" = 14098, "TOLIMAN" = 14099, "TOMATLAN" = 14100,
  "TONALA" = 14101, "TONAYA" = 14102, "TONILA" = 14103, "TOTATICHE" = 14104, "TOTOTLAN" = 14105,
  "TUXCACUESCO" = 14106, "TUXCUECA" = 14107, "TUXPAN" = 14108, "UNION DE SAN ANTONIO" = 14109,
  "UNION DE TULA" = 14110, "VALLE DE GUADALUPE" = 14111, "VALLE DE JUAREZ" = 14112, 
  "VILLA CORONA" = 14114, "VILLA GUERRERO" = 14115, "VILLA HIDALGO" = 14116,
  "VILLA OBREGON" = 14117, "VILLA PURIFICACION" = 14068, "YAHUALICA DE GONZALEZ GALLO" = 14118,
  "ZACOALCO DE TORRES" = 14119, "ZAPOPAN" = 14120, "ZAPOTILTIC" = 14121, "ZAPOTITLAN DE VADILLO" = 14122,
  "ZAPOTLAN DEL REY" = 14123, "ZAPOTLAN EL GRANDE" = 14023, "ZAPOTLANEJO" = 14124
)

df <- df %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Step 13: Calculate valid votes by summing party votes
df <- df %>%
  mutate(valid = rowSums(select(., PAN, PRD, PT, PVEM, PC, PSD, PRI_PANAL, PRD_PT, PT_PC), na.rm = TRUE))

# Step 14: Aggregate votes by uniqueid and generate inverse values
party_list <- c("PAN", "PRD", "PT", "PVEM", "PC", "PSD", "PRI_PANAL", "PRD_PT", "PT_PC", "total", "valid")
df <- df %>%
  group_by(uniqueid) %>%
  summarise(across(all_of(party_list), sum, na.rm = TRUE)) %>%
  mutate(across(all_of(party_list), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 15: Remove duplicated section numbers
df <- df %>%
  group_by(section) %>%
  filter(row_number() == 1)

# Step 16: Merge with external data (all_months_years.dta) and filter for July 2009
df_extra <- fread("../../all_months_years.dta") %>%
  filter(month == 7 & year == 2009)

df <- merge(df, df_extra, by = "section", all.x = TRUE) %>%
  select(-c(_merge, ed, seccion, year, month)) %>%
  rename(listanominal = lista)

# Step 17: Calculate turnout
df <- df %>%
  mutate(turnout = total / listanominal)

# Step 18: Municipality-level turnout
df_mun_turnout <- df %>%
  group_by(uniqueid) %>%
  summarise(mun_total = sum(total, na.rm = TRUE),
            mun_listanominal = sum(listanominal, na.rm = TRUE)) %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

df <- left_join(df, df_mun_turnout, by = "uniqueid")

# Step 19: Rank political party inverse vote shares
df <- df %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r"))

# Step 20: Determine the winner
df <- df %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_PANAL_r == 1 ~ "PRI_PANAL",
    PRD_r == 1 ~ "PRD",
    PT_r == 1 ~ "PT",
    PVEM_r == 1 ~ "PVEM",
    PC_r == 1 ~ "PC",
    PSD_r == 1 ~ "PSD",
    PRD_PT_r == 1 ~ "PRD_PT",
    PT_PC_r == 1 ~ "PT_PC"
  ))

# Step 21: Clean up the dataset and add year and month columns
df <- df %>%
  select(-starts_with("inv_mun_")) %>%
  mutate(year = 2009, month = "July")

# Step 22: Save the final dataset as .dta
write_dta(df, "Jalisco_Section_2009.dta")

# Step 1: Import Excel sheet "CasillaXCasilla" from Ayu_Seccion_2012.xlsx
df <- read_excel("Ayu_Seccion_2012.xlsx", sheet = "CasillaXCasilla") %>%
  rename(municipality = Municipio)

# Step 2: Modify the section column to remove "B", "C", "E" and numbers
df <- df %>%
  mutate(section = gsub("B", "", Casilla)) %>%
  mutate(across(starts_with("section"), ~gsub("C0[1-9]|E0[1-9]|C1[0-9]|E1[0-9]", "", .))) %>%
  mutate(across(starts_with("section"), as.numeric))  # Convert to numeric

# Step 3: Drop specific rows with zero votes
df <- df %>%
  filter(!(section == 995 & municipality == "SAN PEDRO TLAQUEPAQUE")) %>%
  filter(!(section == 2729 & municipality == "EL SALTO")) %>%
  filter(!(section == 2486 & municipality == "GUADALAJARA"))

# Step 4: Compute test variable and clean up party votes
df <- df %>%
  mutate(test = COALPRIPVEM - PRI - PVEM - PRIPVEM + COALPTMC - PT - MC - PTMC) %>%
  select(-PRI, -PVEM, -PRIPVEM, -PT, -MC, -PTMC, -test)

# Step 5: Rename columns for consistency
df <- df %>%
  rename(PANAL = NAL, PRI_PVEM = COALPRIPVEM, PT_PC = COALPTMC, listanominal = Boletas)

# Step 6: Generate total and collapse data by municipality and section
df <- df %>%
  mutate(total = Validos + NULOS + NO_REGISTRADO) %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN:listanominal, total), sum, na.rm = TRUE)) %>%
  ungroup()

# Step 7: Compute turnout
df <- df %>%
  mutate(turnout = total / listanominal)

# Step 8: Replace accents in municipality names
df <- df %>%
  mutate(municipality = gsub("Á", "A", municipality)) %>%
  mutate(municipality = gsub("Í", "I", municipality)) %>%
  mutate(municipality = gsub("Ó", "O", municipality)) %>%
  mutate(municipality = gsub("Ú", "U", municipality)) %>%
  mutate(municipality = gsub("Ñ", "N", municipality))

# Step 9: Assign unique ID to municipalities
uniqueid_map <- c(
  "ACATIC" = 14001, "ACATLAN DE JUAREZ" = 14002, "AHUALULCO DE MERCADO" = 14003,
  "AMACUECA" = 14004, "AMATITAN" = 14005, "AMECA" = 14006, "ANTONIO ESCOBEDO" = 14007,
  "ARANDAS" = 14008, "ATEMAJAC DE BRIZUELA" = 14010, "ATENGO" = 14011, "ATENGUILLO" = 14012,
  "ATOTONILCO EL ALTO" = 14013, "ATOYAC" = 14014, "AUTLAN DE NAVARRO" = 14015, "AYOTLAN" = 14016,
  "AYUTLA" = 14017, "BOLANOS" = 14019, "CABO CORRIENTES" = 14020, "CANADAS DE OBREGON" = 14117,
  "CASIMIRO CASTILLO" = 14021, "CHAPALA" = 14030, "CHIMALTITAN" = 14031, "CHIQUILISTLAN" = 14032,
  "CIHUATLAN" = 14022, "CIUDAD GUZMAN" = 14023, "COCULA" = 14024, "COLOTLAN" = 14025,
  "CONCEPCION DE BUENOS AIRES" = 14026, "CUAUTITLAN DE GARCIA BARRAGAN" = 14027,
  "CUAUTLA" = 14028, "CUQUIO" = 14029, "DEGOLLADO" = 14033, "EJUTLA" = 14034, "EL ARENAL" = 14009,
  "EL GRULLO" = 14037, "EL LIMON" = 14054, "EL SALTO" = 14070, "ENCARNACION DE DIAZ" = 14035,
  "ETZATLAN" = 14036, "GOMEZ FARIAS" = 14079, "GUACHINANGO" = 14038, "GUADALAJARA" = 14039,
  "HOSTOTIPAQUILLO" = 14040, "HUEJUCAR" = 14041, "HUEJUQUILLA EL ALTO" = 14042,
  "IXTLAHUACAN DE LOS MEMBRILLOS" = 14044, "IXTLAHUACAN DEL RIO" = 14045, "JALOSTOTITLAN" = 14046,
  "JAMAY" = 14047, "JESUS MARIA" = 14048, "JILOTLAN DE LOS DOLORES" = 14049, "JOCOTEPEC" = 14050,
  "JUANACATLAN" = 14051, "JUCHITLAN" = 14052, "LA BARCA" = 14018, "LA HUERTA" = 14043,
  "MANZANILLA DE LA PAZ" = 14057, "LAGOS DE MORENO" = 14053, "MAGDALENA" = 14055,
  "MASCOTA" = 14058, "MAZAMITLA" = 14059, "MEXTICACAN" = 14060, "MEZQUITIC" = 14061,
  "MIXTLAN" = 14062, "OCOTLAN" = 14063, "OJUELOS DE JALISCO" = 14064, "PIHUAMO" = 14065,
  "PONCITLAN" = 14066, "PUERTO VALLARTA" = 14067, "QUITUPAN" = 14069, "SAN CRISTOBAL DE LA BARRANCA" = 14071,
  "SAN DIEGO DE ALEJANDRIA" = 14072, "SAN GABRIEL" = 14113, "SAN IGNACIO CERRO GORDO" = 14125,
  "SAN JUAN DE LOS LAGOS" = 14073, "SAN JUANITO DE ESCOBEDO" = 14007, "SAN JULIAN" = 14074,
  "SAN MARCOS" = 14075, "SAN MARTIN DE BOLANOS" = 14076, "SAN MARTIN HIDALGO" = 14077,
  "SAN MIGUEL EL ALTO" = 14078, "SAN SEBASTIAN DEL OESTE" = 14080, "SANTA MARIA DE LOS ANGELES" = 14081,
  "SANTA MARIA DEL ORO" = 14056, "SAYULA" = 14082, "TALA" = 14083, "TALPA DE ALLENDE" = 14084,
  "TAMAZULA DE GORDIANO" = 14085, "TAPALPA" = 14086, "TECALITLAN" = 14087, "TECHALUTA DE MONTENEGRO" = 14089,
  "TECOLOTLAN" = 14088, "TENAMAXTLAN" = 14090, "TEOCALTICHE" = 14091, "TEOCUITATLAN DE CORONA" = 14092,
  "TEPATITLAN DE MORELOS" = 14093, "TEQUILA" = 14094, "TEUCHITLAN" = 14095, "TIZAPAN EL ALTO" = 14096,
  "TLAJOMULCO DE ZUNIGA" = 14097, "TLAQUEPAQUE" = 14098, "TOLIMAN" = 14099, "TOMATLAN" = 14100,
  "TONALA" = 14101, "TONAYA" = 14102, "TONILA" = 14103, "TOTATICHE" = 14104, "TOTOTLAN" = 14105,
  "TUXCACUESCO" = 14106, "TUXCUECA" = 14107, "TUXPAN" = 14108, "UNION DE SAN ANTONIO" = 14109,
  "UNION DE TULA" = 14110, "VALLE DE GUADALUPE" = 14111, "VALLE DE JUAREZ" = 14112,
  "VILLA CORONA" = 14114, "VILLA GUERRERO" = 14115, "VILLA HIDALGO" = 14116,
  "VILLA OBREGON" = 14117, "VILLA PURIFICACION" = 14068, "YAHUALICA DE GONZALEZ GALLO" = 14118,
  "ZACOALCO DE TORRES" = 14119, "ZAPOPAN" = 14120, "ZAPOTILTIC" = 14121, "ZAPOTITLAN DE VADILLO" = 14122,
  "ZAPOTLAN DEL REY" = 14123, "ZAPOTLAN EL GRANDE" = 14023, "ZAPOTLANEJO" = 14124
)

df <- df %>%
  mutate(uniqueid = uniqueid_map[municipality])

# Step 10: Calculate total valid votes by summing parties
df <- df %>%
  mutate(valid = rowSums(select(., PAN, PRD, PANAL, PRI_PVEM, PT_PC), na.rm = TRUE))

# Step 11: Collapse by uniqueid and compute inverse sums
party_list <- c("PAN", "PRD", "PANAL", "PRI_PVEM", "PT_PC", "total", "listanominal", "valid")
df <- df %>%
  group_by(uniqueid) %>%
  summarise(across(all_of(party_list), sum, na.rm = TRUE)) %>%
  mutate(across(all_of(party_list), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 12: Compute municipality-level turnout
df <- df %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 13: Rank inverse sums and determine the winner
df <- df %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r")) %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_PVEM_r == 1 ~ "PRI_PVEM",
    PRD_r == 1 ~ "PRD",
    PANAL_r == 1 ~ "PANAL",
    PT_PC_r == 1 ~ "PT_PC"
  )) %>%
  select(-starts_with("inv_mun_"))

# Step 14: Add year and month columns
df <- df %>%
  mutate(year = 2012, month = "July")

# Step 15: Sort by section and drop duplicates
df <- df %>%
  arrange(section)

df <- df %>%
  distinct(section, .keep_all = TRUE)

# Step 16: Save the dataset in .dta format
write_dta(df, "Jalisco_Section_2012.dta")

# Final Step: Append all years into a single dataset and save
all_datasets <- list.files(path = ".", pattern = "*.dta$", full.names = TRUE)

# Combine all the datasets
df_combined <- lapply(all_datasets, read_dta) %>%
  bind_rows()

# Save the final combined dataset
write_dta(df_combined, "Jalisco_ALL.dta")

# Load necessary libraries
library(readxl)
library(dplyr)
library(haven)

# Set working directory
setwd("D:/Dropbox/Salvador Ascencio/Update Precincts/Jalisco")

#####################################
### 1998 Extraordinary Election #####
#####################################

# Step 1: Import the 1998 data for Juchitlan Extraordinary Election
df_1998 <- read_excel("Juchitlan Extraordinario 1998.xlsx", sheet = "Sheet1") %>%
  rename(section = Sección, total = Totales) %>%
  filter(!is.na(section)) %>%
  filter(!is.na(total) & total != 0)

# Step 2: Collapse data by municipality and section
df_1998 <- df_1998 %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN:PRI, total), sum, na.rm = TRUE), .groups = "drop")

# Step 3: Add unique ID and municipality name
df_1998 <- df_1998 %>%
  mutate(uniqueid = 14052, municipality = "JUCHITLAN EXTRAORDINARIO")

# Step 4: Calculate valid votes
df_1998 <- df_1998 %>%
  mutate(valid = rowSums(select(., PAN, PRI), na.rm = TRUE))

# Step 5: Merge with "all_months_years.dta" for listanominal and calculate turnout
df_1998_merge <- read_dta("../../all_months_years.dta") %>%
  filter(month == 12 & year == 1997 & ed == 14) %>%
  rename(section = seccion, listanominal = lista)

df_1998 <- df_1998 %>%
  left_join(df_1998_merge, by = "section") %>%
  filter(!is.na(listanominal)) %>%
  mutate(turnout = total / listanominal)

# Step 6: Calculate municipal-level votes and inverse values
df_1998 <- df_1998 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN:total, valid, listanominal), sum, na.rm = TRUE)) %>%
  mutate(across(c(PAN:total), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 7: Rank inverse votes and determine the winner
df_1998 <- df_1998 %>%
  mutate(PAN_r = rank(inv_mun_PAN), PRI_r = rank(inv_mun_PRI)) %>%
  mutate(winner = ifelse(PAN_r == 1, "PAN", ifelse(PRI_r == 1, "PRI", "")))

# Step 8: Clean up and save
df_1998 <- df_1998 %>%
  select(uniqueid, section, winner, year = 1998, month = "February")

write_dta(df_1998, "Jalisco_Section_1998_Extraordinario.dta")

#####################################
### 2004 Extraordinary Election #####
#####################################

# Step 1: Import the 2004 data for Tamazula Extraordinary Election
df_2004 <- read_excel("Tamazula Extraordinario 2004.xlsx", sheet = "Sheet1") %>%
  rename(section = Sección, total = Totales, MC = CONV) %>%
  filter(!is.na(section)) %>%
  filter(!is.na(total) & total != 0)

# Step 2: Collapse data by municipality and section
df_2004 <- df_2004 %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN:MC, total), sum, na.rm = TRUE), .groups = "drop")

# Step 3: Add unique ID and municipality name
df_2004 <- df_2004 %>%
  mutate(uniqueid = 14085, municipality = "TAMAZULA EXTRAORDINARIO")

# Step 4: Calculate valid votes
df_2004 <- df_2004 %>%
  mutate(valid = rowSums(select(., PAN:MC), na.rm = TRUE))

# Step 5: Merge with "all_months_years.dta" for listanominal and calculate turnout
df_2004_merge <- read_dta("../../all_months_years.dta") %>%
  filter(month == 9 & year == 2003 & ed == 14) %>%
  rename(section = seccion, listanominal = lista)

df_2004 <- df_2004 %>%
  left_join(df_2004_merge, by = "section") %>%
  filter(!is.na(listanominal)) %>%
  mutate(turnout = total / listanominal)

# Step 6: Calculate municipal-level votes and inverse values
df_2004 <- df_2004 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN:total, valid, listanominal), sum, na.rm = TRUE)) %>%
  mutate(across(c(PAN:total), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 7: Rank inverse votes and determine the winner
df_2004 <- df_2004 %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r")) %>%
  mutate(winner = ifelse(PAN_r == 1, "PAN", ifelse(PRI_PVEM_r == 1, "PRI_PVEM", "")))

# Step 8: Clean up and save
df_2004 <- df_2004 %>%
  select(uniqueid, section, winner, year = 2004, month = "January")

write_dta(df_2004, "Jalisco_Section_2004_Extraordinario.dta")

#####################################
### 2007 Extraordinary Election #####
#####################################

# Step 1: Import the 2007 data for Tuxcueca Extraordinary Election
df_2007 <- read_excel("Tuxcueca Extraordinario 2007.xlsx", sheet = "Sheet1") %>%
  rename(section = Sección, total = Totales, PVEM_MC = PVEM_CONV) %>%
  filter(!is.na(section)) %>%
  filter(!is.na(total) & total != 0)

# Step 2: Collapse data by municipality and section
df_2007 <- df_2007 %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN:PVEM_MC, total), sum, na.rm = TRUE), .groups = "drop")

# Step 3: Add unique ID and municipality name
df_2007 <- df_2007 %>%
  mutate(uniqueid = 14107, municipality = "TUXCUECA EXTRAORDINARIO")

# Step 4: Calculate valid votes
df_2007 <- df_2007 %>%
  mutate(valid = rowSums(select(., PAN:PVEM_MC), na.rm = TRUE))

# Step 5: Merge with "all_months_years.dta" for listanominal and calculate turnout
df_2007_merge <- read_dta("../../all_months_years.dta") %>%
  filter(month == 1 & year == 2007 & ed == 14) %>%
  rename(section = seccion, listanominal = lista)

df_2007 <- df_2007 %>%
  left_join(df_2007_merge, by = "section") %>%
  filter(!is.na(listanominal)) %>%
  mutate(turnout = total / listanominal)

# Step 6: Calculate municipal-level votes and inverse values
df_2007 <- df_2007 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN:total, valid, listanominal), sum, na.rm = TRUE)) %>%
  mutate(across(c(PAN:total), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 7: Rank inverse votes and determine the winner
df_2007 <- df_2007 %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r")) %>%
  mutate(winner = ifelse(PAN_r == 1, "PAN", ifelse(PRI_r == 1, "PRI", ifelse(PT_r == 1, "PT", ""))))

# Step 8: Clean up and save
df_2007 <- df_2007 %>%
  select(uniqueid, section, winner, year = 2007, month = "February")

write_dta(df_2007, "Jalisco_Section_2007_Extraordinario.dta")

#####################################
### 2009 Extraordinary Election #####
#####################################

# Step 1: Import 2009 data for Gomez Farias and San Cristobal Extraordinary Elections
df_gomez <- read_excel("CASxCAS_GomezF2009.xls", sheet = "Gomez farias", range = "A7:N25") %>%
  mutate(municipality = "GOMEZ FARIAS EXTRAORDINARIO")

df_san_cristobal <- read_excel("CASxCAS_SnCristobal2009.xls", sheet = "San cristobal", range = "A7:N19") %>%
  mutate(municipality = "SAN CRISTOBAL DE LA BARRANCA EXTRAORDINARIO")

# Step 2: Combine datasets
df_2009 <- bind_rows(df_gomez, df_san_cristobal) %>%
  rename(section = Secc, total = VotaciónTotalEmitida, PRD_PT_MC = PRD_PT_CONV) %>%
  filter(!is.na(section)) %>%
  filter(!is.na(total) & total != 0)

# Step 3: Collapse data by municipality and section
df_2009 <- df_2009 %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN:PRD_PT_MC, total), sum, na.rm = TRUE), .groups = "drop")

# Step 4: Add unique IDs
df_2009 <- df_2009 %>%
  mutate(uniqueid = case_when(
    municipality == "GOMEZ FARIAS EXTRAORDINARIO" ~ 14079,
    municipality == "SAN CRISTOBAL DE LA BARRANCA EXTRAORDINARIO" ~ 14071
  ))

# Step 5: Calculate valid votes
df_2009 <- df_2009 %>%
  mutate(valid = rowSums(select(., PAN, PRI, PVEM, PANAL, PRD_PT_MC), na.rm = TRUE))

# Step 6: Merge with "all_months_years.dta" for listanominal and calculate turnout
df_2009_merge <- read_dta("../../all_months_years.dta") %>%
  filter(month == 11 & year == 2009 & ed == 14) %>%
  rename(section = seccion, listanominal = lista)

df_2009 <- df_2009 %>%
  left_join(df_2009_merge, by = "section") %>%
  filter(!is.na(listanominal)) %>%
  mutate(turnout = total / listanominal)

# Step 7: Calculate municipal-level votes and inverse values
df_2009 <- df_2009 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN:total, valid, listanominal), sum, na.rm = TRUE)) %>%
  mutate(across(c(PAN:total), ~ 1 / ., .names = "inv_mun_{col}"))

# Step 8: Rank inverse votes and determine the winner
df_2009 <- df_2009 %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{.col}_r")) %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_r == 1 ~ "PRI",
    PVEM_r == 1 ~ "PVEM",
    PANAL_r == 1 ~ "PANAL",
    PRD_PT_MC_r == 1 ~ "PRD_PT_MC",
    TRUE ~ ""
  ))

# Step 9: Clean up and save
df_2009 <- df_2009 %>%
  select(uniqueid, section, winner, year = 2009, month = "December")

write_dta(df_2009, "Jalisco_Section_2009_Extraordinario.dta")

# Load necessary libraries
library(readxl)
library(dplyr)
library(data.table)
library(haven)

# Set working directory
setwd("D:/Dropbox/Salvador Ascencio/Update Precincts/Jalisco")

# Step 1: Import Excel file for 2015 data
df <- read_excel("ResultadosPorCasilla2015.xlsx", sheet = "RESULTADOS", guess_max = 10000) %>%
  rename(municipality = Municipio, section = Sección) %>%
  mutate(Nulos = ifelse(Nulos == "NULL", NA, Nulos),
         NoRegistrados = ifelse(NoRegistrados == "NULL", NA, NoRegistrados))

# Step 2: Keep only rows related to "Municipes" elections
df <- df %>%
  filter(Elección == "Municipes") %>%
  mutate(across(everything(), as.numeric))  # Destring all variables

# Step 3: Handle candidate-specific cases
df <- df %>%
  mutate(CI_1 = 0) %>%
  mutate(CI_1 = case_when(
    uniqueid == 14108 ~ JESÚSOSWALDOSILVAMAGAÑA,
    uniqueid == 14112 ~ JOSEZEPEDACONTRERAS,
    uniqueid == 14067 ~ JOSEFRANCISCOSANCHEZPEREZ,
    uniqueid == 14039 ~ GUILLERMOSIENFUEGOSPEREZ,
    TRUE ~ CI_1
  ))

# Step 4: Drop specific candidates and vote columns
df <- df %>%
  select(-c(JOSEPEDROKUMAMOTOAGUILAR, JESÚSOSWALDOSILVAMAGAÑA, JOSEZEPEDACONTRERAS, JOSEFRANCISCOSANCHEZPEREZ, GUILLERMOSIENFUEGOSPEREZ, Boletas, starts_with("Votos")))

# Step 5: Handle coalitions and recode votes
df <- df %>%
  mutate(PRI_PVEM = ifelse(coalPRIPVEM == 1, PRI_PVEM + PRI + PVEM, PRI_PVEM),
         PRI = ifelse(coalPRIPVEM == 1, 0, PRI),
         PVEM = ifelse(coalPRIPVEM == 1, 0, PVEM),
         PAN_PRD = ifelse(coalPANPRD == 1, PAN_PRD + PAN + PRD, PAN_PRD),
         PAN = ifelse(coalPANPRD == 1, 0, PAN),
         PRD = ifelse(coalPANPRD == 1, 0, PRD))

# Step 6: Rename and clean up columns
df <- df %>%
  rename(nulo = Nulos, no_reg = NoRegistrados, PANAL = NA)

# Step 7: Collapse data by municipality and section
df <- df %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PAN:CI_1), sum, na.rm = TRUE), .groups = "drop")

# Step 8: Generate valid votes and total
df <- df %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, MC, PANAL, MORENA, PES, PH, PAN_PRD, PRI_PVEM, CI_1), na.rm = TRUE),
         total = valid + nulo + no_reg)

# Step 9: Compute inverse sums for rankings
party_list <- c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "MORENA", "PES", "PH", "PAN_PRD", "PRI_PVEM", "CI_1")
df <- df %>%
  group_by(uniqueid) %>%
  summarise(across(all_of(party_list), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(all_of(party_list), ~ 1 / ., .names = "i_{col}"))

# Step 10: Rank the parties
df <- df %>%
  mutate(across(starts_with("i_"), rank, .names = "{.col}_r"))

# Step 11: Determine winners, second, and third places
df <- df %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN",
    PRI_PVEM_r == 1 ~ "PRI_PVEM",
    PRD_r == 1 ~ "PRD",
    PANAL_r == 1 ~ "PANAL",
    MORENA_r == 1 ~ "MORENA",
    PT_r == 1 ~ "PT",
    CI_1_r == 1 ~ "Independent",
    TRUE ~ ""
  )) %>%
  mutate(second = case_when(
    PAN_r == 2 ~ "PAN",
    PRI_PVEM_r == 2 ~ "PRI_PVEM",
    PRD_r == 2 ~ "PRD",
    PANAL_r == 2 ~ "PANAL",
    MORENA_r == 2 ~ "MORENA",
    PT_r == 2 ~ "PT",
    CI_1_r == 2 ~ "Independent",
    TRUE ~ ""
  )) %>%
  mutate(third = case_when(
    PAN_r == 3 ~ "PAN",
    PRI_PVEM_r == 3 ~ "PRI_PVEM",
    PRD_r == 3 ~ "PRD",
    PANAL_r == 3 ~ "PANAL",
    MORENA_r == 3 ~ "MORENA",
    PT_r == 3 ~ "PT",
    CI_1_r == 3 ~ "Independent",
    TRUE ~ ""
  ))

# Step 12: Clean up and generate metadata
df <- df %>%
  mutate(year = 2015, month = "June", STATE = "JALISCO") %>%
  select(STATE, municipality, uniqueid, section, year, month, winner, second, third, total, valid, nulo, no_reg)

# Step 13: Save the dataset
write_dta(df, "Jalisco_Section_2015.dta")

# Step 14: Collapse to generate municipal-level data and save
df_municipal <- df %>%
  group_by(municipality, uniqueid) %>%
  summarise(across(c(PAN:total, valid), sum, na.rm = TRUE), .groups = "drop") %>%
  rename(turnout = mun_turnout)

write_dta(df_municipal, "Update_Municipal/Jalisco_2015.dta")

# Load necessary libraries
library(readxl)
library(dplyr)
library(haven)

# Set working directory
setwd("D:/Dropbox/Salvador Ascencio/Update Precincts/Jalisco")

###############################################
### Step 1: Import and Clean 2018 Election Data
###############################################

# Step 1: Import the 2018 election data from Excel
df_2018 <- read_excel("Ayuntamientos_2018.xlsx", sheet = "Ayuntamientos") %>%
  filter(MUNICIPIO != "MUNICIPIO") %>%
  select(-CASILLA) %>%
  rename(municipality = MUNICIPIO) %>%
  mutate(section = as.numeric(substr(CASILLA, 1, 4)))

# Step 2: Handle PT-MORENA-PES coalition (coalpbt == 1)
df_2018 <- df_2018 %>%
  mutate(PT_MORENA_PES = ifelse(coalpbt == 1, PESPTMORENA + PTMORENA + PTPES + MORENAPES + PT + MORENA + PES, PT_MORENA_PES),
         PT = ifelse(coalpbt == 1, NA, PT),
         MORENA = ifelse(coalpbt == 1, NA, MORENA),
         PES = ifelse(coalpbt == 1, NA, PES)) %>%
  select(-PESPTMORENA, -PTMORENA, -PTPES, -MORENAPES)

# Step 3: Handle PAN-PRD-MC coalition (coaljf == 1)
df_2018 <- df_2018 %>%
  mutate(PAN_PRD_MC = ifelse(coaljf == 1, PANPRDMC + PANPRD + PANMC + PRDMC + PAN + PRD + MC, PAN_PRD_MC),
         PAN = ifelse(coaljf == 1, NA, PAN),
         PRD = ifelse(coaljf == 1, NA, PRD),
         MC = ifelse(coaljf == 1, NA, MC)) %>%
  select(-PANPRDMC, -PANPRD, -PANMC, -PRDMC)

# Step 4: Rename columns for clarity
df_2018 <- df_2018 %>%
  rename(no_reg = NOREGISTRADOS, nulo = VOTOSNULOS)

##########################################
### Step 2: Collapse Data by Municipality and Section
##########################################

# Collapse the data by municipality, uniqueid, and section
df_2018 <- df_2018 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PAN:CI_5, PT_MORENA_PES, PAN_PRD_MC), sum, na.rm = TRUE), .groups = "drop")

##########################################
### Step 3: Calculate Valid Votes and Total Votes
##########################################

# Calculate valid votes (sum of party votes) and total votes
df_2018 <- df_2018 %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PVEM, PT, MC, PANAL, MORENA, PES, PT_MORENA_PES, PAN_PRD_MC, CI_1:CI_5), na.rm = TRUE),
         total = valid + nulo + no_reg)

##########################################
### Step 4: Municipal-Level Vote Calculation
##########################################

# Calculate municipality-level vote totals and inverse values for rankings
df_2018 <- df_2018 %>%
  group_by(uniqueid) %>%
  summarise(across(c(PAN:total, valid), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(PAN:total), ~ 1 / ., .names = "i_{col}"))

##########################################
### Step 5: Rank Parties by Inverse Votes
##########################################

# Rank parties based on inverse votes to determine winners
df_2018 <- df_2018 %>%
  mutate(PAN_r = rank(i_PAN), PRI_r = rank(i_PRI), PRD_r = rank(i_PRD), PVEM_r = rank(i_PVEM),
         PT_r = rank(i_PT), MC_r = rank(i_MC), PANAL_r = rank(i_PANAL), MORENA_r = rank(i_MORENA),
         PES_r = rank(i_PES), PT_MORENA_PES_r = rank(i_PT_MORENA_PES), PAN_PRD_MC_r = rank(i_PAN_PRD_MC),
         CI_1_r = rank(i_CI_1), CI_2_r = rank(i_CI_2), CI_3_r = rank(i_CI_3), CI_4_r = rank(i_CI_4), CI_5_r = rank(i_CI_5))

##########################################
### Step 6: Determine Winners, Second, and Third Places
##########################################

# Identify the winner, second, and third place for each section
df_2018 <- df_2018 %>%
  mutate(winner = case_when(
    PAN_r == 1 ~ "PAN", PRI_r == 1 ~ "PRI", PRD_r == 1 ~ "PRD", PVEM_r == 1 ~ "PVEM", 
    PT_r == 1 ~ "PT", MC_r == 1 ~ "MC", PANAL_r == 1 ~ "PANAL", MORENA_r == 1 ~ "MORENA", 
    PES_r == 1 ~ "PES", PT_MORENA_PES_r == 1 ~ "PT_MORENA_PES", PAN_PRD_MC_r == 1 ~ "PAN_PRD_MC", 
    CI_1_r == 1 ~ "CI_1", TRUE ~ ""),
    second = case_when(
      PAN_r == 2 ~ "PAN", PRI_r == 2 ~ "PRI", PRD_r == 2 ~ "PRD", PVEM_r == 2 ~ "PVEM", 
      PT_r == 2 ~ "PT", MC_r == 2 ~ "MC", PANAL_r == 2 ~ "PANAL", MORENA_r == 2 ~ "MORENA", 
      PES_r == 2 ~ "PES", PT_MORENA_PES_r == 2 ~ "PT_MORENA_PES", PAN_PRD_MC_r == 2 ~ "PAN_PRD_MC", 
      CI_1_r == 2 ~ "CI_1", TRUE ~ ""),
    third = case_when(
      PAN_r == 3 ~ "PAN", PRI_r == 3 ~ "PRI", PRD_r == 3 ~ "PRD", PVEM_r == 3 ~ "PVEM", 
      PT_r == 3 ~ "PT", MC_r == 3 ~ "MC", PANAL_r == 3 ~ "PANAL", MORENA_r == 3 ~ "MORENA", 
      PES_r == 3 ~ "PES", PT_MORENA_PES_r == 3 ~ "PT_MORENA_PES", PAN_PRD_MC_r == 3 ~ "PAN_PRD_MC", 
      CI_1_r == 3 ~ "CI_1", TRUE ~ ""))

##########################################
### Step 7: Replace "Independent" for CI_1 and Generate Metadata
##########################################

# Replace CI_1 with "Independent" and create year/month/state columns
df_2018 <- df_2018 %>%
  mutate(winner = ifelse(winner == "CI_1", "Independent", winner),
         second = ifelse(second == "CI_1", "Independent", second),
         third = ifelse(third == "CI_1", "Independent", third),
         year = 2018, month = "July", STATE = "JALISCO")

##########################################
### Step 8: Merge with Incumbents and Calculate Incumbent Vote
##########################################

# Merge with incumbents data
incumbents <- read_dta("incumbents2018.dta")
df_2018 <- df_2018 %>%
  left_join(incumbents, by = "uniqueid") %>%
  mutate(incumbent_vote = case_when(
    incumbent == "PAN" & coaljf == 0 ~ PAN,
    incumbent == "PRD" & coaljf == 0 ~ PRD,
    incumbent == "MC" & coaljf == 0 ~ MC,
    incumbent == "PAN_PRD" & coaljf == 1 ~ PAN_PRD_MC,
    incumbent == "PT" & coalpbt == 0 ~ PT,
    incumbent == "PES" & coalpbt == 0 ~ PES,
    incumbent == "PT" | incumbent == "PES" & coalpbt == 1 ~ PT_MORENA_PES,
    incumbent == "PRI" ~ PRI,
    incumbent == "PANAL" ~ PANAL,
    incumbent == "PRI_PVEM" ~ rowmax(c(PRI, PVEM)),
    TRUE ~ NA_real_))
##########################################
### Step 9: Merge with Listado Nominal for Listanominal and Calculate Turnout
##########################################

# Step 9: Merge with the Listado Nominal data
listado_nominal <- read_dta("ListadoNominalPREP2018.dta") %>%
  filter(STATE == "JALISCO")

df_2018 <- df_2018 %>%
  left_join(listado_nominal, by = c("STATE", "section")) %>%
  rename(listanominal = ListadoNominalINE)

# Step 10: Calculate turnout at section and municipal levels
df_2018 <- df_2018 %>%
  mutate(turnout = total / listanominal) %>%
  group_by(uniqueid) %>%
  summarise(mun_listanominal = sum(listanominal, na.rm = TRUE),
            mun_total = sum(total, na.rm = TRUE)) %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

##########################################
### Step 10: Save Section-Level Data
##########################################

# Step 11: Save section-level 2018 data to a .dta file
write_dta(df_2018, "Jalisco_Section_2018.dta")

##########################################
### Step 11: Generate Municipal-Level Data for 2018 and Save
##########################################

# Step 12: Collapse section-level data to municipal level
df_2018_municipal <- df_2018 %>%
  group_by(municipality, uniqueid) %>%
  summarise(across(c(PAN:total, valid, incumbent_vote), sum, na.rm = TRUE),
            turnout = first(mun_turnout),
            winner = first(winner),
            second = first(second),
            third = first(third)) %>%
  ungroup() %>%
  mutate(STATE = "JALISCO", year = 2018, month = "July")

# Step 13: Save municipal-level data for 2018
write_dta(df_2018_municipal, "../Update Municipal/Jalisco_2018.dta")

##########################################
### Step 12: Append 2015 and 2018 Section Data
##########################################

# Step 14: Load 2015 data and append 2018 data
df_2015 <- read_dta("Jalisco_Section_2015.dta")
df_combined <- bind_rows(df_2015, df_2018)

# Step 15: Save the combined 2015 and 2018 data
write_dta(df_combined, "Jalisco_Section_15_18.dta")

##########################################
### Step 13: Final Append of All Precinct-Level Data (All Years)
##########################################

# Step 16: Load full precinct panel data
df_all <- read_dta("../Precinct/Jalisco_ALL.dta")

# Step 17: Append 2015-2018 data to the full dataset
df_full_combined <- bind_rows(df_all, df_combined)

# Step 18: Save the final combined dataset
write_dta(df_full_combined, "Jalisco_ALL_SALVADOR.dta")

##########################################
### Step 14: Cleanup Temporary Files
##########################################

# Step 19: Remove intermediate files
file.remove("Jalisco_Section_2015.dta", "Jalisco_Section_2018.dta", "Jalisco_Section_15_18.dta")






