# Load necessary libraries
library(readr)
library(dplyr)
library(haven)

# STEP 1: SET WORKING DIRECTORY (Try setting to two possible locations depending on the machine being used)
dir1 <- "C:/Users/Horacio/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Nuevo Leon - 2000, 2003, 2006, 2009, 2012"
dir2 <- "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Nuevo Leon - 2000, 2003, 2006, 2009, 2012"

# Check if directory exists, and set the correct working directory
if (dir.exists(dir1)) {
  setwd(dir1)
} else if (dir.exists(dir2)) {
  setwd(dir2)
}

# Step 1: Load the data
data <- read_csv("Ayu_Seccion_2000.csv")

# Step 2: Rename columns for clarity
data <- data %>%
  rename(municipality = municipio, 
         section = seccion)

# Step 3: Drop rows where 'municipality' and 'section' are empty
data <- data %>%
  filter(municipality != "", section != "")

# Step 4: Drop rows where 'total' is missing or 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Step 5: Convert relevant columns (from 'pan' to 'total' and 'listanominal') to numeric
data <- data %>%
  mutate(across(pan:total, as.numeric), 
         listanominal = as.numeric(listanominal))

# Step 6: Collapse (sum) data by municipality and section
data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

data <- data %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD_PC_PPN_PSN_PAS = alianza,
    PT = pt,
    PVEM = pvem,
    PCD = pcd,
    PARM = parm
  )

# Step 1: Create turnout variable
data <- data %>%
  mutate(turnout = total / listanominal)

# Step 2: Drop 'votosnulos' column
data <- data %>% 
  select(-votosnulos)

# Step 3: Create uniqueid column based on the municipality names
municipality_map <- c("ABASOLO" = 19001, "AGUALEGUAS" = 19002, "ALLENDE" = 19004, "ANAHUAC" = 19005,
                      "APODACA" = 19006, "ARAMBERRI" = 19007, "BUSTAMANTE" = 19008, "CADEREYTA JIMENEZ" = 19009,
                      "EL CARMEN" = 19010, "CERRALVO" = 19011, "CHINA" = 19013, "CIENEGA DE FLORES" = 19012,
                      "DR. ARROYO" = 19014, "DR. COSS" = 19015, "DR. GONZALEZ" = 19016, "GALEANA" = 19017,
                      "GARCIA" = 19018, "GRAL. BRAVO" = 19020, "GRAL. ESCOBEDO" = 19021, "GRAL. TERAN" = 19022,
                      "GRAL. TREVIÑO" = 19023, "GENERAL ZARAGOZA" = 19024, "GRAL. ZUAZUA" = 19025, "GUADALUPE" = 19026,
                      "HIDALGO" = 19047, "HIGUERAS" = 19028, "HUALAHUISES" = 19029, "ITURBIDE" = 19030,
                      "JUAREZ" = 19031, "LAMPAZOS DE NARANJO" = 19032, "LINARES" = 19033, "LOS ALDAMAS" = 19003,
                      "LOS HERRERAS" = 19027, "LOS RAMONES" = 19042, "MARIN" = 19034, "MELCHOR OCAMPO" = 19035,
                      "MIER Y NORIEGA" = 19036, "MINA" = 19037, "MONTEMORELOS" = 19038, "MONTERREY" = 19039,
                      "PARAS" = 19040, "PESQUERIA" = 19041, "RAYONES" = 19043, "SABINAS HIDALGO" = 19044,
                      "SALINAS VICTORIA" = 19045, "SAN NICOLAS DE LOS GARZA" = 19046, "GARZA GARCIA" = 19019,
                      "SANTA CATARINA" = 19048, "SANTIAGO" = 19049, "VALLECILLO" = 19050, "VILLALDAMA" = 19051)

data <- data %>%
  mutate(uniqueid = municipality_map[municipality])

# Step 4: Create 'valid' variable using rowwise summation
data <- data %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD_PC_PPN_PSN_PAS, PT, PVEM, PCD, PARM, PDS), na.rm = TRUE))

# Step 5: Perform group calculations for each variable by 'uniqueid'
data <- data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN, PRI, PRD_PC_PPN_PSN_PAS, PT, PVEM, PCD, PARM, PDS, total, listanominal, valid),
                list(mun = ~sum(.x, na.rm = TRUE)),
                .names = "mun_{col}"))

# Step 6: Calculate inverse and municipal turnout
data <- data %>%
  mutate(across(starts_with("mun_"), ~1 / .x, .names = "inv_{col}"),
         mun_turnout = mun_total / mun_listanominal)

# Step 7: Rank the results and generate 'winner', 'second', and 'third' variables
rank_cols <- c("inv_mun_PAN", "inv_mun_PRI", "inv_mun_PRD_PC_PPN_PSN_PAS", "inv_mun_PT", "inv_mun_PVEM", "inv_mun_PCD", "inv_mun_PARM", "inv_mun_PDS")
data <- data %>%
  rowwise() %>%
  mutate(across(all_of(rank_cols), rank, .names = "{col}_r"))

# Step 8: Assign winner, second, and third
rank_order <- c("PAN", "PRI", "PRD_PC_PPN_PSN_PAS", "PT", "PVEM", "PCD", "PARM", "PDS")
for (var in rank_order) {
  data <- data %>%
    mutate(winner = ifelse(get(paste0("inv_mun_", var, "_r")) == 1, var, winner),
           second = ifelse(get(paste0("inv_mun_", var, "_r")) == 2, var, second),
           third = ifelse(get(paste0("inv_mun_", var, "_r")) == 3, var, third))
}

# Step 9: Add year and month columns
data <- data %>%
  mutate(year = 2000, month = "July")

# Step 10: Save the dataset
write_dta(data, "Nuevo_Leon_Section_2000.dta")

# Step 1: Read the CSV file
data <- fread("Ayu_Seccion_2003.csv")

# Step 2: Rename the columns `municipio` to `municipality` and `seccion` to `section`
data <- data %>%
  rename(municipality = municipio, section = seccion)

# Step 3: Drop rows where `municipality` or `section` is missing and where `total` is missing or zero
data <- data %>%
  filter(municipality != "" & section != "", !is.na(total) & total != 0)

# Step 4: Convert string variables to numeric (destring equivalent in Stata)
# Assuming `listanominal`, `pan`, `total`, and other similar variables are numeric but stored as character
numeric_vars <- c("listanominal", "pan", "pri", "prd", "pvem", "pt", "mexicoposible", "total")

data <- data %>%
  mutate(across(all_of(numeric_vars), as.numeric))

# Step 5: Collapse the data, summing values for the specified variables by `municipality` and `section`
data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:mexicoposible, sum, na.rm = TRUE), .groups = "drop")

# Step 6: View the collapsed dataset (optional)
print(head(data))

# Step 1: Create a new column `pri_pvem_plm_fc` initialized to 0
data <- data %>%
  mutate(pri_pvem_plm_fc = 0)

# Step 2: Replace `pri_pvem_plm_fc` with the value of `ca` if `pt > 0`
data <- data %>%
  mutate(pri_pvem_plm_fc = ifelse(pt > 0, ca, pri_pvem_plm_fc))

# Step 3: Create another new column `pri_pvem_pt_plm_fc` initialized to 0
data <- data %>%
  mutate(pri_pvem_pt_plm_fc = 0)

# Step 4: Replace `pri_pvem_pt_plm_fc` with the value of `ca` if `pt == 0`
data <- data %>%
  mutate(pri_pvem_pt_plm_fc = ifelse(pt == 0, ca, pri_pvem_pt_plm_fc))

# Step 5: Count the number of rows where the sum of `pri_pvem_plm_fc` and `pri_pvem_pt_plm_fc` is not equal to `ca`
incorrect_count <- data %>%
  filter(pri_pvem_plm_fc + pri_pvem_pt_plm_fc != ca) %>%
  tally()

# Print the count of mismatched rows
print(incorrect_count)

# Step 6: Drop the column `ca`
data <- data %>%
  select(-ca)

# Step 1: Rename the columns
data <- data %>%
  rename(
    PAN = pan,
    PRI_PVEM_PLM_FC = pri_pvem_plm_fc,
    PRI_PT_PVEM_PLM_FC = pri_pvem_pt_plm_fc,
    PRD = prd,
    PT = pt,
    PSN = psn,
    PAS = pas,
    PC = convergencia,
    MexicoPosible = mexicoposible
  )

# Step 2: Generate a new column `turnout` as the ratio of `total` to `listanominal`
data <- data %>%
  mutate(turnout = total / listanominal)

# View the first few rows of the data to ensure the changes are correct
head(data)

# Step 1: Import Data
data <- read_csv("Ayu_Seccion_2003.csv")

# Step 2: Generate unique IDs based on the municipality field
data <- data %>%
  mutate(uniqueid = case_when(
    municipality == "ABASOLO" ~ 19001,
    municipality == "AGUALEGUAS" ~ 19002,
    municipality == "ALLENDE" ~ 19004,
    municipality == "ANÁHUAC" ~ 19005,
    municipality == "APODACA" ~ 19006,
    municipality == "ARAMBERRI" ~ 19007,
    municipality == "BUSTAMANTE" ~ 19008,
    municipality == "CADEREYTA JIMÉNEZ" ~ 19009,
    municipality == "EL CARMEN" ~ 19010,
    municipality == "CERRALVO" ~ 19011,
    municipality == "CHINA" ~ 19013,
    municipality == "CIÉNEGA DE FLORES" ~ 19012,
    municipality == "DR. ARROYO" ~ 19014,
    municipality == "DR. COSS" ~ 19015,
    municipality == "DR. GONZÁLEZ" ~ 19016,
    municipality == "GALEANA" ~ 19017,
    municipality == "GARCÍA" ~ 19018,
    municipality == "GRAL. BRAVO" ~ 19020,
    municipality == "GRAL. ESCOBEDO" ~ 19021,
    municipality == "GRAL. TERÁN" ~ 19022,
    municipality == "GRAL. TREVIÑO" ~ 19023,
    municipality == "GRAL. ZARAGOZA" ~ 19024,
    municipality == "GRAL. ZUAZUA" ~ 19025,
    municipality == "GUADALUPE" ~ 19026,
    municipality == "HIDALGO" ~ 19047,
    municipality == "HIGUERAS" ~ 19028,
    municipality == "HUALAHUISES" ~ 19029,
    municipality == "ITURBIDE" ~ 19030,
    municipality == "JUÁREZ" ~ 19031,
    municipality == "LAMPAZOS DE NARANJO" ~ 19032,
    municipality == "LINARES" ~ 19033,
    municipality == "LOS ALDAMAS" ~ 19003,
    municipality == "LOS HERRERAS" ~ 19027,
    municipality == "LOS RAMONES" ~ 19042,
    municipality == "MARÍN" ~ 19034,
    municipality == "MELCHOR OCAMPO" ~ 19035,
    municipality == "MIER Y NORIEGA" ~ 19036,
    municipality == "MINA" ~ 19037,
    municipality == "MONTEMORELOS" ~ 19038,
    municipality == "MONTERREY" ~ 19039,
    municipality == "PARÁS" ~ 19040,
    municipality == "PESQUERÍA" ~ 19041,
    municipality == "RAYONES" ~ 19043,
    municipality == "SABINAS HIDALGO" ~ 19044,
    municipality == "SALINAS VICTORIA" ~ 19045,
    municipality == "SAN NICOLÁS DE LOS GARZA" ~ 19046,
    municipality == "SAN PEDRO GARZA GARCÍA" ~ 19019,
    municipality == "SANTA CATARINA" ~ 19048,
    municipality == "SANTIAGO" ~ 19049,
    municipality == "VALLECILLO" ~ 19050,
    municipality == "VILLALDAMA" ~ 19051,
    TRUE ~ 0  # Default value if no match
  ))

# Step 3: Calculate the 'valid' column
data <- data %>%
  mutate(valid = rowSums(select(., PAN, PRD, PT, PSN, PAS, PC, MexicoPosible, PRI_PVEM_PLM_FC, PRI_PT_PVEM_PLM_FC), na.rm = TRUE))

# Step 4: Calculate `mun_*` and `inv_mun_*`
vars <- c("PAN", "PRD", "PT", "PSN", "PAS", "PC", "MexicoPosible", "PRI_PVEM_PLM_FC", "PRI_PT_PVEM_PLM_FC", "total", "listanominal", "valid")

for (var in vars) {
  data <- data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    mutate(!!paste0("inv_mun_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Step 5: Generate `mun_turnout`
data <- data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 6: Calculate rankings
data <- data %>%
  mutate_at(vars(starts_with("inv_mun_")), rank, ties.method = "first") %>%
  rename_with(~ gsub("inv_mun_", "", .)) %>%
  rename_with(~ paste0(., "_r"))

# Step 7: Identify winner, second, and third
candidates <- c("PAN", "PRD", "PT", "PSN", "PAS", "PC", "MexicoPosible", "PRI_PVEM_PLM_FC", "PRI_PT_PVEM_PLM_FC")

data <- data %>%
  mutate(
    winner = "",
    second = "",
    third = ""
  )

for (var in candidates) {
  data <- data %>%
    mutate(winner = ifelse(!!sym(paste0(var, "_r")) == 1, var, winner),
           second = ifelse(!!sym(paste0(var, "_r")) == 2, var, second),
           third = ifelse(!!sym(paste0(var, "_r")) == 3, var, third))
}

# Step 8: Finalize data
data <- data %>%
  mutate(year = 2003, month = "July")

# Step 9: Save the data to a .dta file
write_dta(data, "Nuevo_Leon_Section_2003.dta")

# Step 1: Load the CSV file
data <- read_csv("Ayu_Seccion_2006.csv")

# Step 2: Rename columns
data <- data %>%
  rename(
    municipality = nombre_municipio,
    section = seccion
  )

# Step 3: Drop rows where 'municipality' or 'section' is empty, or 'total' is zero
data <- data %>%
  filter(municipality != "" & section != "" & total != 0)

# Step 4: Convert relevant columns to numeric (destring)
# Here, I'm assuming `listanominal`, `nulos`, and columns between `nulos` and `total`
# are the ones you're interested in destringing (i.e., converting to numeric)
data <- data %>%
  mutate(across(listanominal:total, as.numeric))

# Step 5: Group by 'municipality' and 'section', and sum the relevant columns
data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(listanominal:total, sum, na.rm = TRUE))

# Step 6: View the first few rows of the processed data
head(data)

# Save the processed data if needed (Optional)
# write_csv(data, "Processed_Ayu_Seccion_2006.csv")

# Step 1: Initialize 'prdpt' as 0
data <- data %>%
  mutate(prdpt = 0)

# Step 2: Replace 'prdpt' with 'prdptpc' where 'convergencia' > 0
data <- data %>%
  mutate(prdpt = ifelse(convergencia > 0, prdptpc, prdpt))

# Step 3: Set 'prdptpc' to 0 where 'convergencia' > 0
data <- data %>%
  mutate(prdptpc = ifelse(convergencia > 0, 0, prdptpc))

# Step 4: Rename columns
data <- data %>%
  rename(
    PAN = pan,
    PRI_PVEM = pripvem,
    PRD_PT_PC = prdptpc,
    PRD_PT = prdpt,
    PC = convergencia,
    PartidoRepublicano = pr,
    PAS = alternativa,
    PANAL = nuevaalianza
  )

# Step 5: Calculate 'turnout' as 'total / listanominal'
data <- data %>%
  mutate(turnout = total / listanominal)

# View the modified data (Optional)
head(data)

# Save the data if needed (Optional)
# write_csv(data, "Processed_Ayu_Seccion_2006_v2.csv")

# Step 2: Generate unique IDs based on the municipality field
data <- data %>%
  mutate(uniqueid = case_when(
    municipality == "ABASOLO" ~ 19001,
    municipality == "AGUALEGUAS" ~ 19002,
    municipality == "ALLENDE" ~ 19004,
    municipality == "ANÁHUAC" ~ 19005,
    municipality == "APODACA" ~ 19006,
    municipality == "ARAMBERRI" ~ 19007,
    municipality == "BUSTAMANTE" ~ 19008,
    municipality == "CADEREYTA JIMÉNEZ" ~ 19009,
    municipality == "EL CARMEN" ~ 19010,
    municipality == "CERRALVO" ~ 19011,
    municipality == "CHINA" ~ 19013,
    municipality == "CIÉNEGA DE FLORES" ~ 19012,
    municipality == "DR. ARROYO" ~ 19014,
    municipality == "DR. COSS" ~ 19015,
    municipality == "DR. GONZÁLEZ" ~ 19016,
    municipality == "GALEANA" ~ 19017,
    municipality == "GARCÍA" ~ 19018,
    municipality == "GRAL. BRAVO" ~ 19020,
    municipality == "GRAL. ESCOBEDO" ~ 19021,
    municipality == "GRAL. TERÁN" ~ 19022,
    municipality == "GRAL. TREVIÑO" ~ 19023,
    municipality == "GRAL. ZARAGOZA" ~ 19024,
    municipality == "GRAL. ZUAZUA" ~ 19025,
    municipality == "GUADALUPE" ~ 19026,
    municipality == "HIDALGO" ~ 19047,
    municipality == "HIGUERAS" ~ 19028,
    municipality == "HUALAHUISES" ~ 19029,
    municipality == "ITURBIDE" ~ 19030,
    municipality == "JUÁREZ" ~ 19031,
    municipality == "LAMPAZOS DE NARANJO" ~ 19032,
    municipality == "LINARES" ~ 19033,
    municipality == "LOS ALDAMAS" ~ 19003,
    municipality == "LOS HERRERAS" ~ 19027,
    municipality == "LOS RAMONES" ~ 19042,
    municipality == "MARÍN" ~ 19034,
    municipality == "MELCHOR OCAMPO" ~ 19035,
    municipality == "MIER Y NORIEGA" ~ 19036,
    municipality == "MINA" ~ 19037,
    municipality == "MONTEMORELOS" ~ 19038,
    municipality == "MONTERREY" ~ 19039,
    municipality == "PARÁS" ~ 19040,
    municipality == "PESQUERÍA" ~ 19041,
    municipality == "RAYONES" ~ 19043,
    municipality == "SABINAS HIDALGO" ~ 19044,
    municipality == "SALINAS VICTORIA" ~ 19045,
    municipality == "SAN NICOLÁS DE LOS GARZA" ~ 19046,
    municipality == "SAN PEDRO GARZA GARCÍA" ~ 19019,
    municipality == "SANTA CATARINA" ~ 19048,
    municipality == "SANTIAGO" ~ 19049,
    municipality == "VALLECILLO" ~ 19050,
    municipality == "VILLALDAMA" ~ 19051,
    TRUE ~ 0  # Default value if no match
  ))

# Assuming `data` is already loaded

# Step 1: Calculate 'valid' as row sum of selected columns
data <- data %>%
  mutate(valid = rowSums(select(., PAN, PRI_PVEM, PRD_PT_PC, PC, PartidoRepublicano, PAS, PANAL, PRD_PT), na.rm = TRUE))

# Step 2: Calculate municipality-level totals (mun_ prefix)
variables <- c("PAN", "PRI_PVEM", "PRD_PT_PC", "PC", "PartidoRepublicano", "PAS", "PANAL", "PRD_PT", "total", "listanominal", "valid")

for (var in variables) {
  data <- data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(!!paste0("inv_mun_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Step 3: Calculate 'mun_turnout'
data <- data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 4: Generate ranking variables (like PAN_r, PRI_PVEM_r, etc.)
data <- data %>%
  mutate(
    PAN_r = rank(-inv_mun_PAN, ties.method = "first"),
    PRI_PVEM_r = rank(-inv_mun_PRI_PVEM, ties.method = "first"),
    PRD_PT_PC_r = rank(-inv_mun_PRD_PT_PC, ties.method = "first"),
    PC_r = rank(-inv_mun_PC, ties.method = "first"),
    PartidoRepublicano_r = rank(-inv_mun_PartidoRepublicano, ties.method = "first"),
    PAS_r = rank(-inv_mun_PAS, ties.method = "first"),
    PANAL_r = rank(-inv_mun_PANAL, ties.method = "first"),
    PRD_PT_r = rank(-inv_mun_PRD_PT, ties.method = "first")
  )

# Step 5: Drop intermediate 'inv_mun_' columns
data <- data %>%
  select(-starts_with("inv_mun_"))

# Step 6: Assign 'winner', 'second', and 'third' based on the rankings
data <- data %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_PVEM_r == 1 ~ "PRI_PVEM",
      PRD_PT_PC_r == 1 ~ "PRD_PT_PC",
      PC_r == 1 ~ "PC",
      PartidoRepublicano_r == 1 ~ "PartidoRepublicano",
      PAS_r == 1 ~ "PAS",
      PANAL_r == 1 ~ "PANAL",
      PRD_PT_r == 1 ~ "PRD_PT",
      TRUE ~ ""
    ),
    second = case_when(
      PAN_r == 2 ~ "PAN",
      PRI_PVEM_r == 2 ~ "PRI_PVEM",
      PRD_PT_PC_r == 2 ~ "PRD_PT_PC",
      PC_r == 2 ~ "PC",
      PartidoRepublicano_r == 2 ~ "PartidoRepublicano",
      PAS_r == 2 ~ "PAS",
      PANAL_r == 2 ~ "PANAL",
      PRD_PT_r == 2 ~ "PRD_PT",
      TRUE ~ ""
    ),
    third = case_when(
      PAN_r == 3 ~ "PAN",
      PRI_PVEM_r == 3 ~ "PRI_PVEM",
      PRD_PT_PC_r == 3 ~ "PRD_PT_PC",
      PC_r == 3 ~ "PC",
      PartidoRepublicano_r == 3 ~ "PartidoRepublicano",
      PAS_r == 3 ~ "PAS",
      PANAL_r == 3 ~ "PANAL",
      PRD_PT_r == 3 ~ "PRD_PT",
      TRUE ~ ""
    )
  )

# Step 7: Generate 'year' and 'month' columns
data <- data %>%
  mutate(year = 2006, month = "July")

# Step 8: Sort the data by 'section'
data <- data %>%
  arrange(section)

# Step 9: Save the dataset (adjust file path as needed)
write_dta(data, "Nuevo_Leon_Section_2006.dta")

# Step 2: Load the dataset
data <- read.csv("Ayu_Seccion_2009.csv", stringsAsFactors = FALSE)

# Step 3: Drop rows with missing municipality or section, and with missing or zero 'total'
data <- data %>%
  filter(municipality != "" & section != "" & !is.na(total) & total != 0)

# Step 4: Convert necessary columns to numeric
cols_to_convert <- c("pan", "pri", "prd", "prdpsd", "pt", "conv", "psd", "panal", "listanominal", "total")
data[cols_to_convert] <- lapply(data[cols_to_convert], as.numeric)

# Step 5: Collapse (sum) at the municipality and section level
data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(c("pan", "pripvempdpcc", "prd", "prdpsd", "pt", "conv", "psd", "panal", "total", "listanominal"), sum, na.rm = TRUE))

# Step 6: Rename columns for clarity
data <- data %>%
  rename(
    PAN = pan,
    PRI_PVEM_PD_CC = pripvempdpcc,
    PRD = prd,
    PRD_PSD = prdpsd,
    PT = pt,
    PC = conv,
    PSD = psd,
    PANAL = panal
  )

# Step 7: Calculate turnout
data <- data %>%
  mutate(turnout = total / listanominal)

# Step 8: Assign unique ID to municipalities
municipality_ids <- c(
  "ABASOLO" = 19001, "AGUALEGUAS" = 19002, "ALLENDE" = 19004, "ANAHUAC" = 19005,
  "APODACA" = 19006, "ARAMBERRI" = 19007, "BUSTAMANTE" = 19008, "CADEREYTA JIMENEZ" = 19009,
  "CARMEN" = 19010, "CERRALVO" = 19011, "CHINA" = 19013, "CIENEGA DE FLORES" = 19012,
  "DR. ARROYO" = 19014, "DR. COSS" = 19015, "DR. GONZALEZ" = 19016, "GALEANA" = 19017,
  "GARCIA" = 19018, "GRAL. BRAVO" = 19020, "GRAL. ESCOBEDO" = 19021, "GRAL. TERAN" = 19022,
  "GRAL. TREVIÑO" = 19023, "GRAL. ZARAGOZA" = 19024, "GRAL. ZUAZUA" = 19025, "GUADALUPE" = 19026,
  "HIDALGO" = 19047, "HIGUERAS" = 19028, "HUALAHUISES" = 19029, "ITURBIDE" = 19030,
  "JUAREZ" = 19031, "LAMPAZOS DE NARANJO" = 19032, "LINARES" = 19033, "LOS ALDAMAS" = 19003,
  "LOS HERRERAS" = 19027, "LOS RAMONES" = 19042, "MARIN" = 19034, "MELCHOR OCAMPO" = 19035,
  "MIER Y NORIEGA" = 19036, "MINA" = 19037, "MONTEMORELOS" = 19038, "MONTERREY" = 19039,
  "PARAS" = 19040, "PESQUERIA" = 19041, "RAYONES" = 19043, "SABINAS HIDALGO" = 19044,
  "SALINAS VICTORIA" = 19045, "SAN NICOLAS DE LOS GARZA" = 19046, "SAN PEDRO GARZA GARCIA" = 19019,
  "SANTA CATARINA" = 19048, "SANTIAGO" = 19049, "VALLECILLO" = 19050, "VILLALDAMA" = 19051
)

data <- data %>%
  mutate(uniqueid = municipality_ids[municipality])

# Step 9: Calculate 'valid' and aggregate totals by municipality
data <- data %>%
  mutate(valid = rowSums(select(., PAN, PRI_PVEM_PD_CC, PRD, PRD_PSD, PT, PC, PSD, PANAL), na.rm = TRUE))

# Step 10: Calculate municipality-level aggregates and inverse sums
variables <- c("PAN", "PRI_PVEM_PD_CC", "PRD", "PRD_PSD", "PT", "PC", "PSD", "PANAL", "total", "listanominal", "valid")

for (var in variables) {
  data <- data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(!!paste0("inv_mun_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Step 11: Calculate municipality-level turnout
data <- data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 12: Rank the results by municipality
data <- data %>%
  mutate(
    PAN_r = rank(-inv_mun_PAN, ties.method = "first"),
    PRI_PVEM_PD_CC_r = rank(-inv_mun_PRI_PVEM_PD_CC, ties.method = "first"),
    PRD_r = rank(-inv_mun_PRD, ties.method = "first"),
    PRD_PSD_r = rank(-inv_mun_PRD_PSD, ties.method = "first"),
    PT_r = rank(-inv_mun_PT, ties.method = "first"),
    PC_r = rank(-inv_mun_PC, ties.method = "first"),
    PSD_r = rank(-inv_mun_PSD, ties.method = "first"),
    PANAL_r = rank(-inv_mun_PANAL, ties.method = "first")
  )

# Step 13: Determine winner, second, and third place
data <- data %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_PVEM_PD_CC_r == 1 ~ "PRI_PVEM_PD_CC",
      PRD_r == 1 ~ "PRD",
      PRD_PSD_r == 1 ~ "PRD_PSD",
      PT_r == 1 ~ "PT",
      PC_r == 1 ~ "PC",
      PSD_r == 1 ~ "PSD",
      PANAL_r == 1 ~ "PANAL",
      TRUE ~ ""
    ),
    second = case_when(
      PAN_r == 2 ~ "PAN",
      PRI_PVEM_PD_CC_r == 2 ~ "PRI_PVEM_PD_CC",
      PRD_r == 2 ~ "PRD",
      PRD_PSD_r == 2 ~ "PRD_PSD",
      PT_r == 2 ~ "PT",
      PC_r == 2 ~ "PC",
      PSD_r == 2 ~ "PSD",
      PANAL_r == 2 ~ "PANAL",
      TRUE ~ ""
    ),
    third = case_when(
      PAN_r == 3 ~ "PAN",
      PRI_PVEM_PD_CC_r == 3 ~ "PRI_PVEM_PD_CC",
      PRD_r == 3 ~ "PRD",
      PRD_PSD_r == 3 ~ "PRD_PSD",
      PT_r == 3 ~ "PT",
      PC_r == 3 ~ "PC",
      PSD_r == 3 ~ "PSD",
      PANAL_r == 3 ~ "PANAL",
      TRUE ~ ""
    )
  )

# Step 14: Create 'year' and 'month' columns
data <- data %>%
  mutate(year = 2009, month = "July")

# Step 15: Sort by 'section'
data <- data %>%
  arrange(section)

# Step 16: Save the data as a .dta file
write_dta(data, "Nuevo_Leon_Section_2009.dta")

# Step 2: Load the Excel sheet
data <- read_excel("Ayu_Seccion_2012.xlsx", sheet = "Sheet1")

# Step 3: Drop rows with empty municipality and convert columns to numeric
data <- data %>%
  filter(municipality != "") %>%
  mutate(across(everything(), as.numeric))

# Drop rows where total is missing or zero
data <- data %>%
  filter(!is.na(total) & total != 0)

# Step 4: Summarize (collapse) the data by municipality and section
data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN:total, listanominal), sum, na.rm = TRUE))

# Step 5: Calculate turnout
data <- data %>%
  mutate(turnout = total / listanominal)

# Step 6: Replace municipality names with uppercase
data <- data %>%
  mutate(municipality = toupper(municipality))

# Step 7: Assign unique IDs to municipalities
municipality_ids <- c(
  "ABASOLO" = 19001, "AGUALEGUAS" = 19002, "ALLENDE" = 19004, "ANAHUAC" = 19005,
  "APODACA" = 19006, "ARAMBERRI" = 19007, "BUSTAMANTE" = 19008, "CADEREYTA JIMENEZ" = 19009,
  "EL CARMEN" = 19010, "CERRALVO" = 19011, "CHINA" = 19013, "CIENEGA DE FLORES" = 19012,
  "DR. ARROYO" = 19014, "DR. COSS" = 19015, "DR. GONZALEZ" = 19016, "GALEANA" = 19017,
  "GARCIA" = 19018, "GRAL. BRAVO" = 19020, "GRAL. ESCOBEDO" = 19021, "GRAL. TERAN" = 19022,
  "GRAL. TREVINO" = 19023, "GRAL. ZARAGOZA" = 19024, "GRAL. ZUAZUA" = 19025, "GUADALUPE" = 19026,
  "HIDALGO" = 19047, "HIGUERAS" = 19028, "HUALAHUISES" = 19029, "ITURBIDE" = 19030,
  "JUAREZ" = 19031, "LAMPAZOS DE NARANJO" = 19032, "LINARES" = 19033, "LOS ALDAMAS" = 19003,
  "LOS HERRERAS" = 19027, "LOS RAMONES" = 19042, "MARIN" = 19034, "MELCHOR OCAMPO" = 19035,
  "MIER Y NORIEGA" = 19036, "MINA" = 19037, "MONTEMORELOS" = 19038, "MONTERREY" = 19039,
  "PARAS" = 19040, "PESQUERIA" = 19041, "RAYONES" = 19043, "SABINAS HIDALGO" = 19044,
  "SALINAS VICTORIA" = 19045, "SAN NICOLAS DE LOS GARZA" = 19046, "SAN PEDRO GARZA GARCIA" = 19019,
  "SANTA CATARINA" = 19048, "SANTIAGO" = 19049, "VALLECILLO" = 19050, "VILLALDAMA" = 19051
)

data <- data %>%
  mutate(uniqueid = municipality_ids[municipality])

# Step 8: Calculate valid votes
data <- data %>%
  mutate(valid = rowSums(select(., PAN, PRI_PVEM, PRD, PT, PC, PANAL), na.rm = TRUE))

# Step 9: Calculate municipality-level aggregates and the inverse of the sums
variables <- c("PAN", "PRI_PVEM", "PRD", "PT", "PC", "PANAL", "total", "listanominal", "valid")

for (var in variables) {
  data <- data %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(!!paste0("inv_mun_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Step 10: Calculate municipality-level turnout
data <- data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 11: Rank the results by municipality
data <- data %>%
  mutate(
    PAN_r = rank(-inv_mun_PAN, ties.method = "first"),
    PRI_PVEM_r = rank(-inv_mun_PRI_PVEM, ties.method = "first"),
    PRD_r = rank(-inv_mun_PRD, ties.method = "first"),
    PT_r = rank(-inv_mun_PT, ties.method = "first"),
    PC_r = rank(-inv_mun_PC, ties.method = "first"),
    PANAL_r = rank(-inv_mun_PANAL, ties.method = "first")
  )

# Step 12: Determine the winner, second, and third place
data <- data %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_PVEM_r == 1 ~ "PRI_PVEM",
      PRD_r == 1 ~ "PRD",
      PT_r == 1 ~ "PT",
      PC_r == 1 ~ "PC",
      PANAL_r == 1 ~ "PANAL",
      TRUE ~ ""
    ),
    second = case_when(
      PAN_r == 2 ~ "PAN",
      PRI_PVEM_r == 2 ~ "PRI_PVEM",
      PRD_r == 2 ~ "PRD",
      PT_r == 2 ~ "PT",
      PC_r == 2 ~ "PC",
      PANAL_r == 2 ~ "PANAL",
      TRUE ~ ""
    ),
    third = case_when(
      PAN_r == 3 ~ "PAN",
      PRI_PVEM_r == 3 ~ "PRI_PVEM",
      PRD_r == 3 ~ "PRD",
      PT_r == 3 ~ "PT",
      PC_r == 3 ~ "PC",
      PANAL_r == 3 ~ "PANAL",
      TRUE ~ ""
    )
  )

# Step 13: Drop the temporary ranking columns
data <- data %>%
  select(-starts_with("inv_mun_"), -starts_with("mun_"), -ends_with("_r"))

# Step 14: Create year and month columns
data <- data %>%
  mutate(year = 2012, month = "July")

# Step 15: Sort by section
data <- data %>%
  arrange(section)

# Step 16: Save the dataset as a .dta file
write_dta(data, "Nuevo_Leon_Section_2012.dta")

# Step 2: Load the .dta files
nuevo_leon_2000 <- read_dta("Nuevo_Leon_Section_2000.dta")
nuevo_leon_2003 <- read_dta("Nuevo_Leon_Section_2003.dta")
nuevo_leon_2006 <- read_dta("Nuevo_Leon_Section_2006.dta")
nuevo_leon_2009 <- read_dta("Nuevo_Leon_Section_2009.dta")
nuevo_leon_2012 <- read_dta("Nuevo_Leon_Section_2012.dta")

# Step 3: Append the datasets
nuevo_leon_all <- bind_rows(nuevo_leon_2000, nuevo_leon_2003, nuevo_leon_2006, nuevo_leon_2009, nuevo_leon_2012)

# Step 4: Delete (erase) the individual files
file.remove("Nuevo_Leon_Section_2000.dta")
file.remove("Nuevo_Leon_Section_2003.dta")
file.remove("Nuevo_Leon_Section_2006.dta")
file.remove("Nuevo_Leon_Section_2009.dta")
file.remove("Nuevo_Leon_Section_2012.dta")

# Step 5: Tabulate the winner column
table(nuevo_leon_all$winner, useNA = "ifany")

# Step 6: Create the PAN_winner column
nuevo_leon_all <- nuevo_leon_all %>%
  mutate(PAN_winner = ifelse(grepl("PAN", winner) & !grepl("PANAL", winner), 1, 0),
         winner_counter = PAN_winner)

# Step 7: Loop through the party variables and update winner_counter
party_vars <- c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "PD", "PSD", "PAS", 
                "PartidoMexicoPosible", "PPN", "PSN", "PLM", "FC", "CC")

for (var in party_vars) {
  nuevo_leon_all <- nuevo_leon_all %>%
    mutate(!!paste0(var, "_winner") := ifelse(grepl(var, winner), 1, 0)) %>%
    mutate(winner_counter = winner_counter + !!sym(paste0(var, "_winner")))
}

# Step 8: Tabulate winner_counter and count zeros
table(nuevo_leon_all$winner_counter)
nrow(filter(nuevo_leon_all, winner_counter == 0))

# Step 9: Save the final dataset
write_dta(nuevo_leon_all, "Nuevo_Leon_ALL.dta")

# Step 10: Save to Dropbox path
# Adjust the paths as per your environment
save_path <- if (file.exists("C:/Users/Horacio/Dropbox/Incumbency Advantage/Precinct")) {
  "C:/Users/Horacio/Dropbox/Incumbency Advantage/Precinct/Nuevo_Leon_ALL.dta"
} else if (file.exists("/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct")) {
  "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct/Nuevo_Leon_ALL.dta"
} else {
  "Nuevo_Leon_ALL.dta"
}

write_dta(nuevo_leon_all, save_path)

# Step 1: Set the working directory
setwd("D:/Dropbox/Salvador Ascencio/Update Precincts/Nuevo Leon")

# Step 2: Import the Excel data from the "Ayuntamientos_2015.xlsx" file
df <- read_excel("Ayuntamientos_2015.xlsx", sheet = "Ayuntamientos_2015")

# Step 3: Generate the coalitions 'coalprdpt' and 'coalpripvemnapd'
df <- df %>%
  mutate(coalprdpt = ifelse(uniqueid %in% (1 + 19000):(51 + 19000), 1, 0),
         coalpripvemnapd = ifelse(uniqueid %in% (7 + 19000):(51 + 19000), 1, 0))

# Step 4: Update PRD_PT and related variables based on the coalitions
df <- df %>%
  mutate(PRD_PT = ifelse(coalprdpt == 1, PRD_PT + PRD + PT, PRD_PT),
         PRD = ifelse(coalprdpt == 1, NA, PRD),
         PT = ifelse(coalprdpt == 1, NA, PT)) %>%
  mutate(PRI_PVEM_PANAL_PD = ifelse(coalpripvemnapd == 1, PRI_PVEM_PANAL_PD + PRI_PVEM_PANAL + PRI_PVEM_PD + PRI_PANAL_PD + PVEM_PANAL_PD + PRI_PVEM + PRI_PANAL + PRI_PD + PVEM_PANAL + PVEM_PD + PANAL_PD + PRI + PVEM + PANAL + PD, PRI_PVEM_PANAL_PD),
         PD = ifelse(coalpripvemnapd == 1, NA, PD),
         PRI = ifelse(coalpripvemnapd == 1, NA, PRI),
         PVEM = ifelse(coalpripvemnapd == 1, NA, PVEM),
         PANAL = ifelse(coalpripvemnapd == 1, NA, PANAL))

# Drop the unnecessary coalition-related columns
df <- df %>%
  select(-PRI_PVEM_PANAL, -PRI_PVEM_PD, -PRI_PANAL_PD, -PVEM_PANAL_PD, -PRI_PVEM, -PRI_PANAL, -PRI_PD, -PVEM_PANAL, -PVEM_PD, -PANAL_PD)

# Step 5: Rename the CI column to CI_1
df <- df %>%
  rename(CI_1 = CI)

# Step 6: Summarize the data by municipality, section, and uniqueid
df_summary <- df %>%
  group_by(municipality, section, uniqueid) %>%
  summarise(across(PAN:PRD_PT, sum, na.rm = TRUE),
            listanominal = sum(listanominal, na.rm = TRUE),
            total = sum(total, na.rm = TRUE))

# Step 7: Calculate the 'valid' column
df_summary <- df_summary %>%
  mutate(valid = rowSums(select(., PAN:CI_1), na.rm = TRUE))

# Step 8: Generate `mun_` and `inv_mun_` columns
for (var in c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "PD", "PCC", "MORENA", "PH", "PES", "PRI_PVEM_PANAL_PD", "PRD_PT", "CI_1", "total", "valid", "listanominal")) {
  df_summary <- df_summary %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(var), na.rm = TRUE),
           !!paste0("i_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Step 9: Calculate turnout and mun_turnout
df_summary <- df_summary %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / mun_listanominal)

# Step 10: Ranking parties based on their 'inv_mun_' values
party_ranks <- c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PANAL", "PD", "PCC", "MORENA", "PH", "PES", "PRI_PVEM_PANAL_PD", "PRD_PT", "CI_1")
for (party in party_ranks) {
  df_summary <- df_summary %>%
    mutate(!!paste0(party, "_r") := rank(!!sym(paste0("i_", party))))
}

# Drop the intermediate `i_` columns
df_summary <- df_summary %>%
  select(-starts_with("i_"))

# Step 11: Determine winner, second, and third place based on ranks
df_summary <- df_summary %>%
  mutate(winner = case_when(PAN_r == 1 ~ "PAN",
                            PRI_r == 1 ~ "PRI",
                            PRD_r == 1 ~ "PRD",
                            PT_r == 1 ~ "PT",
                            PVEM_r == 1 ~ "PVEM",
                            MC_r == 1 ~ "MC",
                            PANAL_r == 1 ~ "PANAL",
                            PD_r == 1 ~ "PD",
                            PCC_r == 1 ~ "PCC",
                            MORENA_r == 1 ~ "MORENA",
                            PH_r == 1 ~ "PH",
                            PES_r == 1 ~ "PES",
                            PRI_PVEM_PANAL_PD_r == 1 ~ "PRI_PVEM_PANAL_PD",
                            PRD_PT_r == 1 ~ "PRD_PT",
                            CI_1_r == 1 ~ "Independent"),
         second = case_when(PAN_r == 2 ~ "PAN",
                            PRI_r == 2 ~ "PRI",
                            PRD_r == 2 ~ "PRD",
                            PT_r == 2 ~ "PT",
                            PVEM_r == 2 ~ "PVEM",
                            MC_r == 2 ~ "MC",
                            PANAL_r == 2 ~ "PANAL",
                            PD_r == 2 ~ "PD",
                            PCC_r == 2 ~ "PCC",
                            MORENA_r == 2 ~ "MORENA",
                            PH_r == 2 ~ "PH",
                            PES_r == 2 ~ "PES",
                            PRI_PVEM_PANAL_PD_r == 2 ~ "PRI_PVEM_PANAL_PD",
                            PRD_PT_r == 2 ~ "PRD_PT",
                            CI_1_r == 2 ~ "Independent"),
         third = case_when(PAN_r == 3 ~ "PAN",
                           PRI_r == 3 ~ "PRI",
                           PRD_r == 3 ~ "PRD",
                           PT_r == 3 ~ "PT",
                           PVEM_r == 3 ~ "PVEM",
                           MC_r == 3 ~ "MC",
                           PANAL_r == 3 ~ "PANAL",
                           PD_r == 3 ~ "PD",
                           PCC_r == 3 ~ "PCC",
                           MORENA_r == 3 ~ "MORENA",
                           PH_r == 3 ~ "PH",
                           PES_r == 3 ~ "PES",
                           PRI_PVEM_PANAL_PD_r == 3 ~ "PRI_PVEM_PANAL_PD",
                           PRD_PT_r == 3 ~ "PRD_PT",
                           CI_1_r == 3 ~ "Independent"))

# Drop the rank columns
df_summary <- df_summary %>%
  select(-ends_with("_r"))

# Step 12: Add columns for state, year, and month
df_summary <- df_summary %>%
  mutate(STATE = "NUEVO LEON",
         year = 2015,
         month = "June")

# Step 13: Save the processed data to a new file
write_dta(df_summary, "Nuevo_Leon_Section_2015.dta")

# Step 1: Load your dataset (Nuevo Leon 2015 data)
df <- read_dta("Nuevo_Leon_Section_2015.dta")

# Step 2: Convert municipality names to uppercase
df <- df %>%
  mutate(municipality = toupper(municipality))

# Step 3: Drop the old uniqueid column
df <- df %>%
  select(-uniqueid)

# Step 4: Generate the 'uniqueid' column based on municipality names
df <- df %>%
  mutate(uniqueid = case_when(
    municipality == "ABASOLO" ~ 19001,
    municipality == "AGUALEGUAS" ~ 19002,
    municipality == "ALLENDE" ~ 19004,
    municipality == "ANÁHUAC" ~ 19005,
    municipality == "APODACA" ~ 19006,
    municipality == "ARAMBERRI" ~ 19007,
    municipality == "BUSTAMANTE" ~ 19008,
    municipality == "CADEREYTA JIMÉNEZ" ~ 19009,
    municipality == "EL CARMEN" ~ 19010,
    municipality == "CERRALVO" ~ 19011,
    municipality == "CHINA" ~ 19013,
    municipality == "CIÉNEGA DE FLORES" ~ 19012,
    municipality == "DR. ARROYO" ~ 19014,
    municipality == "DR. COSS" ~ 19015,
    municipality == "DR. GONZÁLEZ" ~ 19016,
    municipality == "GALEANA" ~ 19017,
    municipality == "GARCÍA" ~ 19018,
    municipality == "GRAL. BRAVO" ~ 19020,
    municipality == "GRAL. ESCOBEDO" ~ 19021,
    municipality == "GRAL. TERÁN" ~ 19022,
    municipality == "GRAL. TREVIÑO" ~ 19023,
    municipality == "GRAL. ZARAGOZA" ~ 19024,
    municipality == "GRAL. ZUAZUA" ~ 19025,
    municipality == "GUADALUPE" ~ 19026,
    municipality == "HIDALGO" ~ 19047,
    municipality == "HIGUERAS" ~ 19028,
    municipality == "HUALAHUISES" ~ 19029,
    municipality == "ITURBIDE" ~ 19030,
    municipality == "JUÁREZ" ~ 19031,
    municipality == "LAMPAZOS DE NARANJO" ~ 19032,
    municipality == "LINARES" ~ 19033,
    municipality == "LOS ALDAMAS" ~ 19003,
    municipality == "LOS HERRERAS" ~ 19027,
    municipality == "LOS RAMONES" ~ 19042,
    municipality == "MARÍN" ~ 19034,
    municipality == "MELCHOR OCAMPO" ~ 19035,
    municipality == "MIER Y NORIEGA" ~ 19036,
    municipality == "MINA" ~ 19037,
    municipality == "MONTEMORELOS" ~ 19038,
    municipality == "MONTERREY" ~ 19039,
    municipality == "PARÁS" ~ 19040,
    municipality == "PESQUERÍA" ~ 19041,
    municipality == "RAYONES" ~ 19043,
    municipality == "SABINAS HIDALGO" ~ 19044,
    municipality == "SALINAS VICTORIA" ~ 19045,
    municipality == "SAN NICOLÁS DE LOS GARZA" ~ 19046,
    municipality == "SAN PEDRO GARZA GARCÍA" ~ 19019,
    municipality == "SANTA CATARINA" ~ 19048,
    municipality == "SANTIAGO" ~ 19049,
    municipality == "VALLECILLO" ~ 19050,
    municipality == "VILLALDAMA" ~ 19051,
    TRUE ~ 0  # Default case, ensures no unmatched municipalities
  ))

# Step 5: Save the updated data as "Nuevo_Leon_Section_2015.dta"
write_dta(df, "Nuevo_Leon_Section_2015.dta")

# Step 6: Collapse the data to get the first 'winner' for each municipality and uniqueid, then rename 'winner' to 'incumbent'
df_incumbent <- df %>%
  group_by(municipality, uniqueid) %>%
  summarise(incumbent = first(winner))

# Step 7: Save the incumbents data as "incumbents2018.dta"
write_dta(df_incumbent, "incumbents2018.dta")

# Step 1: Load the dataset (Nuevo Leon Section 2015 data)
df <- read_dta("Nuevo_Leon_Section_2015.dta")

# Step 2: Collapse (aggregate) data
# Sum numeric columns listanominal to valid, and take the first value for other columns
df_collapsed <- df %>%
  group_by(municipality, uniqueid) %>%
  summarise(
    listanominal = sum(listanominal, na.rm = TRUE),
    valid = sum(valid, na.rm = TRUE),
    STATE = first(STATE),
    year = first(year),
    month = first(month),
    winner = first(winner),
    second = first(second),
    third = first(third),
    mun_turnout = first(mun_turnout)
  )

# Step 3: Rename 'mun_turnout' to 'turnout'
df_collapsed <- df_collapsed %>%
  rename(turnout = mun_turnout)

# Step 4: Sort the dataframe by 'uniqueid'
df_collapsed <- df_collapsed %>%
  arrange(uniqueid)

# Step 5: Save the collapsed data as "Nuevo_Leon_2015.dta"
write_dta(df_collapsed, "../../Update Municipal/Nuevo_Leon_2015.dta")

# Optional Step: Check the structure and data of the collapsed dataframe
glimpse(df_collapsed)

# Step 1: Import the Excel file "Ayuntamientos_2018.xlsx"
df <- read_excel("Ayuntamientos_2018.xlsx", sheet = 1)

# Step 2: Rename columns
df <- df %>%
  rename(municipality = MUNICIPIO,
         section = SECCION,
         nulo = VotosAnulados,
         listanominal = LISTA_NOMINAL)

# Step 3: Generate 'coalpripvem' and 'coalpbt' columns
df <- df %>%
  mutate(
    coalpripvem = ifelse(municipality %in% c("Abasolo", "Los Aldamas", "Cadereyta Jimenez", "El Carmen", 
                                             "Dr. Gonzalez", "Guadalupe", "Hidalgo", "Juarez", "Lampazos de Naranjo", 
                                             "Mier y Noriega", "Los Ramones", "Salinas Victoria", "Villaldama"), 1, 0),
    coalpbt = ifelse(municipality %in% c("Agualeguas", "Los Aldamas", "China", "Dr. Coss", "Melchor Ocampo"), 0, 1)
  )

# Step 4: Column renaming and merging coalitions
df <- df %>%
  mutate(
    PRI_PVEM = ifelse(coalpripvem == 1, PRI_PVEM + PRI + PVEM, PRI_PVEM),
    PRI = ifelse(coalpripvem == 1, NA, PRI),
    PVEM = ifelse(coalpripvem == 1, NA, PVEM),
    
    PT_MORENA_PES = ifelse(coalpbt == 1, PT_MORENA_PES + PT_MORENA + PT_PES + MORENA_PES + MORENA + PT + PES, PT_MORENA_PES),
    PT = ifelse(coalpbt == 1, NA, PT),
    PES = ifelse(coalpbt == 1, NA, PES),
    MORENA = ifelse(coalpbt == 1, NA, MORENA)
  ) %>%
  select(-PT_MORENA, -PT_PES, -MORENA_PES) # Drop unnecessary columns

# Step 5: Rename 'NAPPN' to 'PANAL'
df <- df %>%
  rename(PANAL = NAPPN)

# Step 6: Create 'total' column as the row total of certain columns
df <- df %>%
  rowwise() %>%
  mutate(total = sum(c_across(PAN:CI_6), na.rm = TRUE))

# Step 7: Collapse data (aggregation) by municipality and section
df_collapsed <- df %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(PAN:CI_6, sum, na.rm = TRUE),
            total = sum(total, na.rm = TRUE),
            listanominal = sum(listanominal, na.rm = TRUE),
            coalpripvem = first(coalpripvem),
            coalpbt = first(coalpbt))

# Step 8: Create 'valid' column as row total of selected columns
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(valid = sum(c_across(PAN:CI_6), na.rm = TRUE))

# Step 9: Calculate turnout and mun_turnout
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal,
         mun_turnout = total / listanominal)

# Step 10: Ranking based on inverse sums and assigning ranks
df_collapsed <- df_collapsed %>%
  mutate(across(PAN:CI_6, ~1 / ., .names = "inv_mun_{col}")) %>%
  mutate(across(starts_with("inv_mun"), rank, .names = "{col}_r"))

# Step 11: Determine the winner, second, and third
df_collapsed <- df_collapsed %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_MC_r == 1 ~ "MC",
    inv_mun_PANAL_r == 1 ~ "PANAL",
    inv_mun_MORENA_r == 1 ~ "MORENA",
    inv_mun_PES_r == 1 ~ "PES",
    inv_mun_RED_r == 1 ~ "RED",
    inv_mun_PRI_PVEM_r == 1 ~ "PRI_PVEM",
    inv_mun_PT_MORENA_PES_r == 1 ~ "PT_MORENA_PES",
    inv_mun_CI_1_r == 1 ~ "Independent",
    TRUE ~ winner
  ),
  second = case_when(
    inv_mun_PAN_r == 2 ~ "PAN",
    inv_mun_PRI_r == 2 ~ "PRI",
    inv_mun_PRD_r == 2 ~ "PRD",
    inv_mun_PT_r == 2 ~ "PT",
    inv_mun_PVEM_r == 2 ~ "PVEM",
    inv_mun_MC_r == 2 ~ "MC",
    inv_mun_PANAL_r == 2 ~ "PANAL",
    inv_mun_MORENA_r == 2 ~ "MORENA",
    inv_mun_PES_r == 2 ~ "PES",
    inv_mun_RED_r == 2 ~ "RED",
    inv_mun_PRI_PVEM_r == 2 ~ "PRI_PVEM",
    inv_mun_PT_MORENA_PES_r == 2 ~ "PT_MORENA_PES",
    inv_mun_CI_1_r == 2 ~ "Independent",
    TRUE ~ second
  ),
  third = case_when(
    inv_mun_PAN_r == 3 ~ "PAN",
    inv_mun_PRI_r == 3 ~ "PRI",
    inv_mun_PRD_r == 3 ~ "PRD",
    inv_mun_PT_r == 3 ~ "PT",
    inv_mun_PVEM_r == 3 ~ "PVEM",
    inv_mun_MC_r == 3 ~ "MC",
    inv_mun_PANAL_r == 3 ~ "PANAL",
    inv_mun_MORENA_r == 3 ~ "MORENA",
    inv_mun_PES_r == 3 ~ "PES",
    inv_mun_RED_r == 3 ~ "RED",
    inv_mun_PRI_PVEM_r == 3 ~ "PRI_PVEM",
    inv_mun_PT_MORENA_PES_r == 3 ~ "PT_MORENA_PES",
    inv_mun_CI_1_r == 3 ~ "Independent",
    TRUE ~ third
  ))

# Step 12: Add constant columns (year, month, and STATE)
df_collapsed <- df_collapsed %>%
  mutate(year = 2018, month = "July", STATE = "NUEVO LEON")

# Step 13: Save the result as a .dta file
write_dta(df_collapsed, "Nuevo_Leon_Section_2018.dta")

# Step 1: Load the dataset
df <- read_dta("Nuevo_Leon_Section_2018.dta")

# Step 2: Replace municipality names with uppercase and drop uniqueid
df <- df %>%
  mutate(municipality = toupper(municipality)) %>%
  select(-uniqueid)

# Step 3: Generate 'uniqueid' based on municipality name
df <- df %>%
  mutate(uniqueid = case_when(
    municipality == "ABASOLO" ~ 19001,
    municipality == "AGUALEGUAS" ~ 19002,
    municipality == "ALLENDE" ~ 19004,
    municipality == "ANAHUAC" ~ 19005,
    municipality == "APODACA" ~ 19006,
    municipality == "ARAMBERRI" ~ 19007,
    municipality == "BUSTAMANTE" ~ 19008,
    municipality == "CADEREYTA JIMENEZ" ~ 19009,
    municipality == "EL CARMEN" ~ 19010,
    municipality == "CERRALVO" ~ 19011,
    municipality == "CHINA" ~ 19013,
    municipality == "CIENEGA DE FLORES" ~ 19012,
    municipality == "DR. ARROYO" ~ 19014,
    municipality == "DR. COSS" ~ 19015,
    municipality == "DR. GONZALEZ" ~ 19016,
    municipality == "GALEANA" ~ 19017,
    municipality == "GARCIA" ~ 19018,
    municipality == "GRAL. BRAVO" ~ 19020,
    municipality == "GRAL. ESCOBEDO" ~ 19021,
    municipality == "GRAL. TERAN" ~ 19022,
    municipality == "GRAL. TREVINO" ~ 19023,
    municipality == "GRAL. ZARAGOZA" ~ 19024,
    municipality == "GRAL. ZUAZUA" ~ 19025,
    municipality == "GUADALUPE" ~ 19026,
    municipality == "HIDALGO" ~ 19047,
    municipality == "HIGUERAS" ~ 19028,
    municipality == "HUALAHUISES" ~ 19029,
    municipality == "ITURBIDE" ~ 19030,
    municipality == "JUAREZ" ~ 19031,
    municipality == "LAMPAZOS DE NARANJO" ~ 19032,
    municipality == "LINARES" ~ 19033,
    municipality == "LOS ALDAMAS" ~ 19003,
    municipality == "LOS HERRERAS" ~ 19027,
    municipality == "LOS RAMONES" ~ 19042,
    municipality == "MARIN" ~ 19034,
    municipality == "MELCHOR OCAMPO" ~ 19035,
    municipality == "MIER Y NORIEGA" ~ 19036,
    municipality == "MINA" ~ 19037,
    municipality == "MONTEMORELOS" ~ 19038,
    municipality == "MONTERREY" ~ 19039,
    municipality == "PARAS" ~ 19040,
    municipality == "PESQUERIA" ~ 19041,
    municipality == "RAYONES" ~ 19043,
    municipality == "SABINAS HIDALGO" ~ 19044,
    municipality == "SALINAS VICTORIA" ~ 19045,
    municipality == "SAN NICOLAS DE LOS GARZA" ~ 19046,
    municipality == "SAN PEDRO GARZA GARCIA" ~ 19019,
    municipality == "SANTA CATARINA" ~ 19048,
    municipality == "SANTIAGO" ~ 19049,
    municipality == "VALLECILLO" ~ 19050,
    municipality == "VILLALDAMA" ~ 19051
  ))

# Step 4: Merge with incumbents2018.dta using uniqueid
incumbents <- read_dta("incumbents2018.dta")
df <- df %>%
  left_join(incumbents, by = "uniqueid")

# Step 5: Drop the '_merge' column and remove the incumbents2018.dta file
df <- df %>% select(-_merge)
file.remove("incumbents2018.dta")

# Step 6: Create and update the 'incumbent_vote' column
df <- df %>%
  mutate(incumbent_vote = case_when(
    incumbent == "PAN" ~ PAN,
    incumbent == "PRD" ~ PRD,
    incumbent == "PRI" ~ PRI,
    incumbent == "PANAL" ~ PANAL,
    incumbent == "MC" ~ MC,
    incumbent == "PT" ~ PT,
    
    (incumbent %in% c("PRI", "PRI_PVEM_PANAL_PD") & coalpripvem == 1) ~ PRI_PVEM,
    (incumbent == "PRI_PVEM_PANAL_PD" & coalpripvem == 0) ~ PRI,
    
    incumbent == "PRD_PT" ~ PRD,
    (incumbent == "PT" & coalpbt == 0) ~ PT,
    
    (incumbent == "PT" & coalpbt == 1) ~ PT_MORENA_PES,
    (uniqueid == 19016 & coalpbt == 1) ~ PT_MORENA_PES
  ))

# Step 7: Drop the 'coal*' columns
df <- df %>% select(-starts_with("coal"))

# Step 8: Save the final dataset to a .dta file
write_dta(df, "Nuevo_Leon_Section_2018.dta")

# Load the dataset
df <- read_dta("Nuevo_Leon_Section_2018.dta")

# Step 1: Collapse the dataset by summing `PAN` to `total` and `incumbent_vote`, while keeping the first values for other variables
df_collapsed <- df %>%
  group_by(municipality, uniqueid) %>%
  summarise(across(c(PAN:total, incumbent_vote), sum, .names = "sum_{.col}"),
            STATE = first(STATE),
            year = first(year),
            month = first(month),
            winner = first(winner),
            second = first(second),
            third = first(third),
            mun_turnout = first(mun_turnout),
            incumbent = first(incumbent))

# Step 2: Rename `mun_turnout` to `turnout`
df_collapsed <- df_collapsed %>%
  rename(turnout = mun_turnout)

# Step 3: Sort by `uniqueid`
df_collapsed <- df_collapsed %>%
  arrange(uniqueid)

# Step 4: Reorder the columns: `STATE`, `municipality`, `uniqueid`, followed by the rest
df_collapsed <- df_collapsed %>%
  select(STATE, municipality, uniqueid, everything())

# Step 5: Save the collapsed dataset to a new .dta file
write_dta(df_collapsed, "..\\..\\Update Municipal\\Nuevo_Leon_2018.dta")

# Step 1: Import Excel data
df <- read_excel("NL_AYUN_2018_EXTRAORDINARIO.xlsx", sheet = "NL_AYUN_2018_EXTRAORDINARIO", range = "A6:AD1601")

# Step 2: Rename columns
df <- df %>%
  rename(section = SECCION, total = TOTAL_VOTOS_CALCULADO, listanominal = LISTA_NOMINAL)

# Step 3: Add municipality and uniqueid columns
df <- df %>%
  mutate(municipality = "MONTERREY EXTRAORDINARIO", uniqueid = 19039)

# Step 4: Collapse data by municipality, uniqueid, and section, summing PAN to CI_1 and listanominal
df_collapsed <- df %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(c(PAN:CI_1, total, listanominal), sum, .names = "sum_{.col}"))

# Step 5: Create a valid column that sums PAN to CI_1
df_collapsed <- df_collapsed %>%
  mutate(valid = rowSums(select(., starts_with("sum_PAN"):sum_CI_1)))

# Step 6: Generate mun_* columns
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("sum_"), ~ sum(.x), .names = "mun_{.col}"),
         across(starts_with("mun_sum_"), ~ 1 / .x, .names = "i_{.col}"))

# Step 7: Calculate turnout and mun_turnout
df_collapsed <- df_collapsed %>%
  mutate(turnout = sum_total / sum_listanominal,
         mun_turnout = mun_sum_total / mun_sum_listanominal)

# Step 8: Calculate row ranks for the candidates
df_collapsed <- df_collapsed %>%
  mutate(across(starts_with("i_sum_"), ~ rank(.x, ties.method = "min"), 
                .names = "{.col}_r"))

# Step 9: Generate winner, second, and third columns
df_collapsed <- df_collapsed %>%
  mutate(winner = case_when(i_sum_PAN_r == 1 ~ "PAN",
                            i_sum_PRI_r == 1 ~ "PRI",
                            i_sum_PRD_r == 1 ~ "PRD",
                            i_sum_PT_r == 1 ~ "PT",
                            i_sum_PVEM_r == 1 ~ "PVEM",
                            i_sum_MC_r == 1 ~ "MC",
                            i_sum_PANAL_r == 1 ~ "PANAL",
                            i_sum_RED_r == 1 ~ "RED",
                            i_sum_CI_1_r == 1 ~ "Independent",
                            TRUE ~ ""),
         second = case_when(i_sum_PAN_r == 2 ~ "PAN",
                            i_sum_PRI_r == 2 ~ "PRI",
                            i_sum_PRD_r == 2 ~ "PRD",
                            i_sum_PT_r == 2 ~ "PT",
                            i_sum_PVEM_r == 2 ~ "PVEM",
                            i_sum_MC_r == 2 ~ "MC",
                            i_sum_PANAL_r == 2 ~ "PANAL",
                            i_sum_RED_r == 2 ~ "RED",
                            i_sum_CI_1_r == 2 ~ "Independent",
                            TRUE ~ ""),
         third = case_when(i_sum_PAN_r == 3 ~ "PAN",
                           i_sum_PRI_r == 3 ~ "PRI",
                           i_sum_PRD_r == 3 ~ "PRD",
                           i_sum_PT_r == 3 ~ "PT",
                           i_sum_PVEM_r == 3 ~ "PVEM",
                           i_sum_MC_r == 3 ~ "MC",
                           i_sum_PANAL_r == 3 ~ "PANAL",
                           i_sum_RED_r == 3 ~ "RED",
                           i_sum_CI_1_r == 3 ~ "Independent",
                           TRUE ~ ""))

# Step 10: Replace `Independent` based on CI_ values
df_collapsed <- df_collapsed %>%
  mutate(winner = if_else(grepl("CI_", winner), "Independent", winner),
         second = if_else(grepl("CI_", second), "Independent", second),
         third = if_else(grepl("CI_", third), "Independent", third))

# Step 11: Add year, month, and STATE columns
df_collapsed <- df_collapsed %>%
  mutate(year = 2018, month = "December", STATE = "NUEVO LEON")

# Step 12: Save the dataset
write_dta(df_collapsed, "Nuevo_Leon_Section_2018_EXTRAORDINARIO.dta")

# Step 1: Load the datasets
nleon_2015 <- read_dta("Nuevo_Leon_Section_2015.dta")
nleon_2018 <- read_dta("Nuevo_Leon_Section_2018.dta")
nleon_2018_extra <- read_dta("Nuevo_Leon_Section_2018_EXTRAORDINARIO.dta")

# Step 2: Append datasets
nleon_combined <- bind_rows(nleon_2015, nleon_2018, nleon_2018_extra)

# Step 3: Remove the original files after appending
file.remove("Nuevo_Leon_Section_2015.dta")
file.remove("Nuevo_Leon_Section_2018.dta")
file.remove("Nuevo_Leon_Section_2018_EXTRAORDINARIO.dta")

# Step 4: Generate PAN_winner and winner_counter columns
nleon_combined <- nleon_combined %>%
  mutate(PAN_winner = ifelse(grepl("PAN", winner) & !grepl("PANAL", winner), 1, 0),
         winner_counter = PAN_winner)

# Step 5: Loop through other party variables (PRI, PRD, etc.) and update the corresponding _winner columns
for (var in c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "MORENA", "MC")) {
  nleon_combined <- nleon_combined %>%
    mutate(!!paste0(var, "_winner") := ifelse(grepl(var, winner), 1, 0)) %>%
    mutate(winner_counter = winner_counter + !!sym(paste0(var, "_winner")))
}

# Step 6: Tabulate winner_counter values (equivalent to `tab` in Stata)
table(nleon_combined$winner_counter)

# Step 7: Count rows where winner_counter is 0
nrow(filter(nleon_combined, winner_counter == 0))

# Step 8: Save the combined dataset
write_dta(nleon_combined, "Nuevo_Leon_Section_15_18.dta")

# Step 9: Load the larger existing dataset
nleon_all <- read_dta("../../Precinct/Nuevo_Leon_ALL.dta")

# Step 10: Append the new combined data to the larger dataset
nleon_all_combined <- bind_rows(nleon_all, nleon_combined)

# Step 11: Remove the temporary combined dataset
file.remove("Nuevo_Leon_Section_15_18.dta")

# Step 12: Replace `municipality` column values to uppercase
nleon_all_combined <- nleon_all_combined %>%
  mutate(municipality = toupper(municipality))

# Step 13: Save the final combined dataset
write_dta(nleon_all_combined, "Nuevo_Leon_ALL_SALVADOR.dta")
