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

# Step 1: Load the data
data <- read_csv("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2000.csv")

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

# Step 9: Add year and month columns
data_2000 <- data %>%
  mutate(year = 2000, month = "July")
rm(data)

# Step 1: Read the CSV file
data <- fread("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2003.csv")
colnames(data) <- tolower(colnames(data))
names(data) <- gsub("[. ]", "", names(data))
# Step 2: Rename the columns `municipio` to `municipality` and `seccion` to `section`
data <- data %>%
  rename(municipality = municipio, 
         section = seccion)

# Step 3: Drop rows where `municipality` or `section` is missing and where `total` is missing or zero
data <- data %>%
  filter(municipality != "" & section != "", !is.na(total) & total != 0)

# Step 4: Convert string variables to numeric 
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
data <- read_csv("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2003.csv")
colnames(data) <- tolower(colnames(data))
names(data) <- gsub("[. ]", "", names(data))
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

# Step 8: Finalize data
data_2003 <- data %>%
  mutate(year = 2003, month = "July")

# Step 1: Load the CSV file
data <- read_csv("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2006.csv")
colnames(data) <- tolower(colnames(data))
names(data) <- gsub("[. ]", "", names(data))
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

# Step 7: Generate 'year' and 'month' columns
data_2006 <- data %>%
  mutate(year = 2006, month = "July")

# Step 8: Sort the data by 'section'
data <- data %>%
  arrange(section)

# Step 2: Load the dataset
data <- read.csv("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2009.csv", stringsAsFactors = FALSE)
colnames(data) <- tolower(colnames(data))
names(data) <- gsub("[. ]", "", names(data))
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

# Step 14: Create 'year' and 'month' columns
data_2009 <- data %>%
  mutate(year = 2009, month = "July")

# Step 15: Sort by 'section'
data <- data %>%
  arrange(section)


# Step 2: Load the Excel sheet
data <- read_excel("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayu_Seccion_2012.xlsx", sheet = "Sheet1")
colnames(data) <- tolower(colnames(data))
names(data) <- gsub("[. ]", "", names(data))
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

# Step 14: Create year and month columns
data_2012 <- data %>%
  mutate(year = 2012, month = "July")

# Step 15: Sort by section
data <- data %>%
  arrange(section)

# Step 16: Save the dataset as a .dta file
write_dta(data, "Nuevo_Leon_Section_2012.dta")

# Step 3: Append the datasets
nuevo_leon_all <- bind_rows(nuevo_leon_2000, nuevo_leon_2003, nuevo_leon_2006, nuevo_leon_2009, nuevo_leon_2012)


# Step 2: Import the Excel data from the "Ayuntamientos_2015.xlsx" file
df <- read_excel("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayuntamientos_2015.xlsx", sheet = "Ayuntamientos_2015")
colnames(df) <- tolower(colnames(df))
names(df) <- gsub("[. ]", "", names(df))
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
# Step 9: Calculate turnout and mun_turnout
df_summary <- df_summary %>%
  mutate(turnout = total / listanominal)

# Step 12: Add columns for state, year, and month
df_2015 <- df_summary %>%
  mutate(STATE = "NUEVO LEON",
         year = 2015,
         month = "June")

# Step 2: Convert municipality names to uppercase
df_2015 <- df_2015 %>%
  mutate(municipality = toupper(municipality))

# Step 3: Drop the old uniqueid column
df_2015 <- df_2015 %>%
  select(-uniqueid)

# Step 4: Generate the 'uniqueid' column based on municipality names
df_2015 <- df_2015 %>%
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

# Step 1: Import the Excel file "Ayuntamientos_2018.xlsx"
df <- read_excel("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Ayuntamientos_2018.xlsx", sheet = 1)

names(df) <- gsub("[. ]", "", names(df))
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
  mutate(turnout = total / listanominal)

# Step 12: Add constant columns (year, month, and STATE)
df_2018 <- df_collapsed %>%
  mutate(year = 2018, month = "July", STATE = "NUEVO LEON")

# Step 2: Replace municipality names with uppercase and drop uniqueid
df_2018 <- df_2018 %>%
  mutate(municipality = toupper(municipality)) %>%
  select(-uniqueid)

# Step 3: Generate 'uniqueid' based on municipality name
df_2018 <- df_2018 %>%
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

# Step 1: Import Excel data
df <- read_excel("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/NL_AYUN_2018_EXTRAORDINARIO.xlsx", sheet = "NL_AYUN_2018_EXTRAORDINARIO", range = "A6:AD1601")
names(df)
# Step 2: Rename columns
df <- df %>%
  rename(section = SECCION, 
         total = TOTAL_VOTOS_CALCULADO, 
         listanominal = LISTA_NOMINAL)

# Step 3: Add municipality and uniqueid columns
df <- df %>%
  mutate(municipality = "MONTERREY EXTRAORDINARIO", uniqueid = 19039)

# Step 4: Collapse data by municipality, uniqueid, and section, summing PAN to CI_1 and listanominal
df_collapsed <- df %>%
  group_by(uniqueid, municipality, section) %>%
  summarise(across(c(PAN:CI_1, total, listanominal), sum))

# Step 5: Create a valid column that sums PAN to CI_1
df_collapsed <- df_collapsed %>%
  mutate(valid = rowSums(across(c(PAN:CI_1))))


# Step 7: Calculate turnout and mun_turnout
df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

# Step 11: Add year, month, and STATE columns
df_2018_extra <- df_collapsed %>%
  mutate(year = 2018, month = "December", STATE = "NUEVO LEON")

#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

# Load the 2021 dataset
data_2021 <- read_csv("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/21/NL_AYUN_2021.csv", skip = 5) %>% 
  filter(MUNICIPIO != "Gral. Zuazua")

data_ext <- read_csv("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/NL_AYUN_ext_2021.csv", skip = 5)

data_2021 <- bind_rows(data_2021, data_ext)
names(data_2021)

# Rename columns
data_2021 <- data_2021 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL_VOTOS_CALCULADO,
                no_reg = NO_REGISTRADOS,
                nulos = NULOS) %>%
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
    section = as.numeric(section)
  ) %>% 
  filter(section > 0)

# Assign uniqueids
data_2021 <- data_2021 %>% 
  mutate(
    uniqueid = case_when(
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

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(PAN:nulos, total:listanominal), 
           \(x) sum(x, na.rm = TRUE))
  )


# Calculate valid votes and final details
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(PAN:MORENA_PANAL), na.rm = TRUE),
    year = 2021,
    month = case_when(
      municipality == "GRAL. ZUAZUA" ~ "November",
      TRUE ~ "June"
    )
  )

#####################################
### PROCESSING DATA FOR 2024 -------
#####################################

# Load the 2024 dataset
data_2024 <- read_excel("../../../Data/Raw Electoral Data/Nuevo Leon - 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/24/NL_AYUN_2024.xlsx", skip = 6)

names(data_2024)

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL_VOTOS_CALCULADO,
                no_reg = NO_REGISTRADAS,
                nulos = NULOS) %>%
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
    section = as.numeric(section)
  ) %>% 
  filter(section > 0)

# Assign uniqueids
data_2024 <- data_2024 %>% 
  mutate(
    uniqueid = case_when(
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
    valid = sum(c_across(PAN:PVEM_MORENA), na.rm = TRUE),
    year = 2024,
    month = "June"
  )

# Step 2: Append datasets
nleon_combined <- bind_rows(df_2015, df_2018, df_2018_extra, collapsed_2021, collapsed_2024)


# Step 10: Append the new combined data to the larger dataset
nleon_all_combined <- bind_rows(nleon_all, nleon_combined)

# Step 12: Replace `municipality` column values to uppercase
nleon_all_combined <- nleon_all_combined %>%
  mutate(municipality = toupper(municipality))

data.table::fwrite(nleon_all_combined,"../../../Processed Data/nuevoleon/nuevoleon_process_raw_data.csv")

