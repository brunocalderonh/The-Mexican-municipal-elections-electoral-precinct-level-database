# Load necessary libraries
library(dplyr)
library(readr)

# Set the working directory
setwd("D:/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011 - Missing 2014 (but PREP available; requested it)")
# Alternate path if required
#setwd("C:/Users/Horacio/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011 - Missing 2014 (but PREP available; requested it)")

# Step 1: Load CSV data
data <- read_csv("Ayu_Seccion_1996_No_LN.csv")

# Step 2: Rename columns
data <- data %>%
  rename(
    municipality = municipio,
    section = seccion
  )

# Step 3: Drop rows where both `municipality` and `section` are missing
data <- data %>%
  filter(!(is.na(municipality) & is.na(section)))

# Step 4: Create total variable summing selected columns
data <- data %>%
  rowwise() %>%
  mutate(
    total = sum(c_across(c(pan, pri, prd, pt, pvem, prs, parm, cd, noregistrados, nulos)), na.rm = TRUE)
  )

# Step 5: Convert columns from character to numeric (destring equivalent)
data <- data %>%
  mutate(across(c(pan:nulos), as.numeric))

# Step 6: Collapse (sum) by municipality and section
collapsed_data <- data %>%
  group_by(municipality, section) %>%
  summarise(
    PAN = sum(pan, na.rm = TRUE),
    PRI = sum(pri, na.rm = TRUE),
    PRD = sum(prd, na.rm = TRUE),
    PT = sum(pt, na.rm = TRUE),
    PVEM = sum(pvem, na.rm = TRUE),
    PRS = sum(prs, na.rm = TRUE),
    PARM = sum(parm, na.rm = TRUE),
    PCD = sum(cd, na.rm = TRUE),
    total = sum(total, na.rm = TRUE)
  )

# Step 7: Drop unnecessary columns
collapsed_data <- collapsed_data %>%
  select(-noregistrados, -nulos)

# Step 8: Save the processed data (optional step)
write_csv(collapsed_data, "Nayarit_Section_1996.csv")

# Step 9: Drop 'noreg' and 'nulos' columns (if they exist)
collapsed_data <- collapsed_data %>%
  select(-noreg, -nulos)

# Step 10: Generate a uniqueid column initialized to 0
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = 0)

# Step 11: Replace 'uniqueid' based on 'municipality' names
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPONETA" ~ 18001,
    municipality == "AHUACATLAN" ~ 18002,
    municipality == "AMATLAN DE CANAS" ~ 18003,
    municipality == "BAHIA DE BANDERAS" ~ 18020,
    municipality == "COMPOSTELA" ~ 18004,
    municipality == "EL NAYAR" ~ 18009,
    municipality == "HUAJICORI" ~ 18005,
    municipality == "IXTLAN DEL RIO" ~ 18006,
    municipality == "JALA" ~ 18007,
    municipality == "LA YESCA" ~ 18019,
    municipality == "ROSAMORADA" ~ 18010,
    municipality == "RUIZ" ~ 18011,
    municipality == "SAN BLAS" ~ 18012,
    municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
    municipality == "SANTA MARIA DEL ORO" ~ 18014,
    municipality == "SANTIAGO IXCUINTLA" ~ 18015,
    municipality == "TECUALA" ~ 18016,
    municipality == "TEPIC" ~ 18017,
    municipality == "TUXPAN" ~ 18018,
    municipality == "XALISCO" ~ 18008,
    TRUE ~ 0 # default case if none of the above matches
  ))

# Step 12: Check the data
print(collapsed_data)

# Optional: Save the processed data
write_csv(collapsed_data, "Nayarit_Section_1996_with_uniqueid.csv")

# Assuming collapsed_data is your current dataset

# Step 1: Generate the 'valid' column by summing across PAN, PRI, PRD, PT, PVEM, PRS, PARM, PCD
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(select(., PAN, PRI, PRD, PT, PVEM, PRS, PARM, PCD), na.rm = TRUE))

# Step 2: For each variable (PAN, PRI, PRD, PT, PVEM, PRS, PARM, PCD, total, valid)
# Group by 'uniqueid' and sum the values, then calculate the inverse
vars_to_sum <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PRS", "PARM", "PCD", "total", "valid")

# Summing within each 'uniqueid' group and calculating the inverse
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  summarise(across(all_of(vars_to_sum), sum, na.rm = TRUE)) %>%
  mutate(across(all_of(vars_to_sum), ~ 1 / ., .names = "inv_mun_{col}"))

# View the result
print(collapsed_data)

# Optional: Save the result to a file
write_csv(collapsed_data, "Nayarit_Section_1996_with_valid_and_inv_mun.csv")

# Step 1: Rank the inverse values (row ranks)
collapsed_data <- collapsed_data %>%
  mutate(across(starts_with("inv_mun_"), ~ rank(desc(.), ties.method = "first"), 
                .names = "{col}_r"))

# Step 2: Drop inv_mun_* columns (the original inverse values)
collapsed_data <- collapsed_data %>%
  select(-starts_with("inv_mun_"))

# Step 3: Generate the 'winner' column based on the ranks
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PRS_r == 1 ~ "PRS",
    inv_mun_PCD_r == 1 ~ "PCD",
    inv_mun_PARM_r == 1 ~ "PARM",
    TRUE ~ NA_character_
  ))

# Step 4: Drop all rank columns (like *_r)
collapsed_data <- collapsed_data %>%
  select(-ends_with("_r"))

# Step 5: Add 'year' and 'month' columns
collapsed_data <- collapsed_data %>%
  mutate(year = 1996, 
         month = "July")

# Step 6: Save the data
write.csv(collapsed_data, "Nayarit_Section_1996.csv", row.names = FALSE)

# Step 7: Prepare the file for merging with 1999 data
collapsed_data_to_merge <- collapsed_data %>%
  select(municipality, section, uniqueid) %>%
  arrange(section)

# Save the subset for merging with 1999 data
write.csv(collapsed_data_to_merge, "Nayarit_Section_1996_to_Merge_with_1999.csv", row.names = FALSE)

# Step 8: Drop rows where total is NA or 0
collapsed_data_clean <- collapsed_data %>%
  filter(!is.na(total) & total != 0)

# Save the cleaned data
write.csv(collapsed_data_clean, "Nayarit_Section_1996_Clean.csv", row.names = FALSE)

# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv("Ayu_Seccion_1999_No_LN.csv")

# Step 1: Rename columns
data <- data %>%
  rename(municipality = nomunicipio, section = seccion)

# Step 2: Drop rows where both `municipality` and `section` are missing
data <- data %>%
  filter(!is.na(municipality) & !is.na(section))

# Step 3: Create the `total` column as the sum of `pri`, `pvem`, `medp`, `parmen`, `pps`, `cac`, `candnoreg`, `votosnulos`
data <- data %>%
  mutate(total = rowSums(across(pri:votosnulos), na.rm = TRUE))

# Step 4: Drop rows where `total` is missing or 0
data <- data %>%
  filter(!is.na(total) & total != 0)

# Step 5: Convert all vote columns from character to numeric (like `destring` in Stata)
data <- data %>%
  mutate(across(pri:votosnulos, as.numeric))

# Step 6: Collapse data by `municipality` and `section`
collapsed_data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(pri:votosnulos, sum, na.rm = TRUE), total = sum(total, na.rm = TRUE))

# Step 7: Rename columns as required
collapsed_data <- collapsed_data %>%
  rename(PAN_PRD_PT = cac,
         PRI = pri,
         PVEM = pvem,
         PMEP = medp,  # Partido del Movimiento Electoral del Pueblo
         PARM = parm,
         PPS = pps)

# Step 8: Drop `candnoreg` and `votosnulos`
collapsed_data <- collapsed_data %>%
  select(-candnoreg, -votosnulos)

# Step 9: Create the `valid` column as the sum of the valid votes
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(PRI:PAN_PRD_PT), na.rm = TRUE))

# Step 10: Generate the municipality-level totals for each variable and calculate inverse values
collapsed_data <- collapsed_data %>%
  group_by(municipality) %>%
  mutate(across(c(PRI, PVEM, PMEP, PARM, PPS, PAN_PRD_PT, total, valid), 
                list(mun_total = sum), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(ends_with("mun_total"), ~ 1 / ., .names = "inv_{col}"))

# Save the resulting dataset
write.csv(collapsed_data, "Nayarit_Seccion_1999_Collapsed.csv", row.names = FALSE)

# Load necessary libraries
library(dplyr)

# Step 1: Add 'ed' and 'seccion' variables
data <- data %>%
  mutate(ed = 18, seccion = section)

# Step 2: Load external data and merge it
all_months_years <- read.csv("../../all_months_years.csv")  # Adjust for the correct path
merged_data <- data %>%
  left_join(all_months_years, by = c("ed", "seccion")) %>%
  filter(month == 6, year == 1999) %>%
  filter(is.na(_merge) == FALSE) %>%
  select(-c(_merge, ed, seccion, year, month))

# Step 3: Rename 'lista' to 'listanominal'
merged_data <- merged_data %>%
  rename(listanominal = lista)

# Step 4: Calculate 'turnout'
merged_data <- merged_data %>%
  mutate(turnout = total / listanominal)

# Step 5: Generate municipality-level sums and inverse values for 'listanominal'
merged_data <- merged_data %>%
  group_by(municipality) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE),
         inv_mun_listanominal = 1 / mun_listanominal)

# Step 6: Generate 'mun_turnout'
merged_data <- merged_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 7: Generate row ranks
merged_data <- merged_data %>%
  group_by(municipality) %>%
  mutate(PRI_r = rank(-inv_mun_PRI),
         PVEM_r = rank(-inv_mun_PVEM),
         PMEP_r = rank(-inv_mun_PMEP),
         PARM_r = rank(-inv_mun_PARM),
         PPS_r = rank(-inv_mun_PPS),
         PAN_PRD_PT_r = rank(-inv_mun_PAN_PRD_PT))

# Step 8: Assign winners based on ranks
merged_data <- merged_data %>%
  mutate(winner = case_when(
    PAN_PRD_PT_r == 1 ~ "PAN_PRD_PT",
    PRI_r == 1 ~ "PRI",
    PVEM_r == 1 ~ "PVEM",
    PPS_r == 1 ~ "PPS",
    PMEP_r == 1 ~ "PMEP",
    PARM_r == 1 ~ "PARM",
    TRUE ~ winner  # Keep previous value if no condition is met
  ))

# Step 9: Add year and month variables
merged_data <- merged_data %>%
  mutate(year = 1999, month = "July")

# Step 10: Drop 'municipality' and sort by 'section'
merged_data <- merged_data %>%
  select(-municipality) %>%
  arrange(section)

# Step 11: Merge with Nayarit 1996 data and drop unmatched rows
nayarit_1996_data <- read.csv("Nayarit_Section_1996_to_Merge_with_1999.csv")  # Adjust path as needed
final_data <- merged_data %>%
  inner_join(nayarit_1996_data, by = "section") %>%
  filter(is.na(_merge) == FALSE) %>%
  select(-_merge)

# Step 12: Save the resulting dataset
write.csv(final_data, "Nayarit_Section_1999.csv", row.names = FALSE)

# Remove the 1996 to merge dataset as per your final command
file.remove("Nayarit_Section_1996_to_Merge_with_1999.csv")

# Step 1: Load the data
data <- read.csv("Ayu_Seccion_2002_No_LN.csv")

# Step 2: Rename columns
data <- data %>%
  rename(municipality = nombre_municipio,
         section = seccion)

# Step 3: Drop rows where municipality and section are missing or where total is 0
data <- data %>%
  filter(municipality != "", section != ".", total != 0)

# Step 4: Convert relevant columns to numeric (destring equivalent)
data <- data %>%
  mutate(across(c(pan:total), as.numeric))

# Step 5: Collapse (sum) data by municipality and section
collapsed_data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(pan:total, sum, na.rm = TRUE))

# Step 6: Rename variables
collapsed_data <- collapsed_data %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PRS = prs,
         PMEP = medp,
         PC = cdppn,
         PSN = psn,
         PAS = pas)

# Step 7: Create uniqueid based on municipality name
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPONETA" ~ 18001,
    municipality == "AHUACATLAN" ~ 18002,
    municipality == "AMATLAN DE CA?AS" ~ 18003,
    municipality == "BAHIA DE BANDERAS" ~ 18020,
    municipality == "COMPOSTELA" ~ 18004,
    municipality == "EL NAYAR" ~ 18009,
    municipality == "HUAJICORI" ~ 18005,
    municipality == "IXTLAN DEL RIO" ~ 18006,
    municipality == "JALA" ~ 18007,
    municipality == "LA YESCA" ~ 18019,
    municipality == "ROSAMORADA" ~ 18010,
    municipality == "RUIZ" ~ 18011,
    municipality == "SAN BLAS" ~ 18012,
    municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
    municipality == "SANTA MARIA DEL ORO" ~ 18014,
    municipality == "SANTIAGO IXCUINTLA" ~ 18015,
    municipality == "TECUALA" ~ 18016,
    municipality == "TEPIC" ~ 18017,
    municipality == "TUXPAN" ~ 18018,
    municipality == "XALISCO" ~ 18008,
    TRUE ~ 0))

# Step 8: Calculate 'valid' as the sum of relevant variables
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PRD, PT, PVEM, PRS, PMEP, PC, PSN, PAS))))

# Step 9: Calculate municipal level sums and inverse values
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN:valid, total), ~ sum(.x, na.rm = TRUE), .names = "mun_{.col}"),
         across(c(mun_PAN:mun_valid), ~ 1 / .x, .names = "inv_{.col}"))

# Step 10: Merge with external data
all_months_years <- read.csv("../../all_months_years.csv")  # Load the external file
merged_data <- collapsed_data %>%
  left_join(all_months_years, by = c("ed" = 18, "section")) %>%
  filter(month == 6, year == 2002) %>%
  select(-c(_merge, ed, month, year))

# Step 11: Rename 'lista' to 'listanominal'
merged_data <- merged_data %>%
  rename(listanominal = lista)

# Step 12: Calculate 'turnout'
merged_data <- merged_data %>%
  mutate(turnout = total / listanominal)

# Step 13: Calculate 'mun_listanominal'
merged_data <- merged_data %>%
  group_by(uniqueid) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE),
         inv_mun_listanominal = 1 / mun_listanominal)

# Step 14: Calculate 'mun_turnout'
merged_data <- merged_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 15: Generate row ranks
merged_data <- merged_data %>%
  group_by(uniqueid) %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{col}_r"))

# Step 16: Assign winners based on ranks
merged_data <- merged_data %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PRS_r == 1 ~ "PRS",
    inv_mun_PMEP_r == 1 ~ "PMEP",
    inv_mun_PC_r == 1 ~ "PC",
    inv_mun_PSN_r == 1 ~ "PSN",
    inv_mun_PAS_r == 1 ~ "PAS",
    TRUE ~ NA_character_))

# Step 17: Add year and month variables
merged_data <- merged_data %>%
  mutate(year = 2002, month = "July")

# Step 18: Sort by 'section'
merged_data <- merged_data %>%
  arrange(section)

# Step 19: Save the final dataset
write.csv(merged_data, "Nayarit_Section_2002.csv", row.names = FALSE)

# Step 1: Load the data
data <- read.csv("Ayu_Seccion_2005_No_LN.csv")

# Step 2: Rename columns
data <- data %>%
  rename(municipality = nombre_municipio,
         section = seccion)

# Step 3: Drop rows where municipality and section are missing or where total is 0
data <- data %>%
  filter(municipality != "", section != ".", total != 0)

# Step 4: Convert relevant columns to numeric (destring equivalent)
data <- data %>%
  mutate(across(c(pan:total), as.numeric))

# Step 5: Collapse (sum) data by municipality and section
collapsed_data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(pan:total, sum, na.rm = TRUE))

# Step 6: Rename variables
collapsed_data <- collapsed_data %>%
  rename(PAN = pan,
         PRI = pri,
         PRD_PT_PRS = prdptprs,
         PC = pc,
         PVEM = pvem)

# Step 7: Create uniqueid based on municipality name
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPONETA" ~ 18001,
    municipality == "AHUACATLAN" ~ 18002,
    municipality == "AMATLAN DE CA?AS" ~ 18003,
    municipality == "BAHIA DE BANDERAS" ~ 18020,
    municipality == "COMPOSTELA" ~ 18004,
    municipality == "EL NAYAR" ~ 18009,
    municipality == "HUAJICORI" ~ 18005,
    municipality == "IXTLAN DEL RIO" ~ 18006,
    municipality == "JALA" ~ 18007,
    municipality == "LA YESCA" ~ 18019,
    municipality == "ROSAMORADA" ~ 18010,
    municipality == "RUIZ" ~ 18011,
    municipality == "SAN BLAS" ~ 18012,
    municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
    municipality == "SANTA MARIA DEL ORO" ~ 18014,
    municipality == "SANTIAGO IXCUINTLA" ~ 18015,
    municipality == "TECUALA" ~ 18016,
    municipality == "TEPIC" ~ 18017,
    municipality == "TUXPAN" ~ 18018,
    municipality == "XALISCO" ~ 18008,
    TRUE ~ 0))

# Step 8: Calculate 'valid' as the sum of relevant variables
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN, PRI, PVEM, PC, PRD_PT_PRS))))

# Step 9: Calculate municipal-level sums and inverse values
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN:valid, total), ~ sum(.x, na.rm = TRUE), .names = "mun_{.col}"),
         across(c(mun_PAN:mun_valid), ~ 1 / .x, .names = "inv_{.col}"))

# Step 10: Merge with external data
all_months_years <- read.csv("../../all_months_years.csv")  # Load the external file
merged_data <- collapsed_data %>%
  left_join(all_months_years, by = c("ed" = 18, "section")) %>%
  filter(month == 6, year == 2005) %>%
  select(-c(_merge, ed, month, year))

# Step 11: Rename 'lista' to 'listanominal'
merged_data <- merged_data %>%
  rename(listanominal = lista)

# Step 12: Calculate 'turnout'
merged_data <- merged_data %>%
  mutate(turnout = total / listanominal)

# Step 13: Calculate 'mun_listanominal'
merged_data <- merged_data %>%
  group_by(uniqueid) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE),
         inv_mun_listanominal = 1 / mun_listanominal)

# Step 14: Calculate 'mun_turnout'
merged_data <- merged_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 15: Generate row ranks
merged_data <- merged_data %>%
  group_by(uniqueid) %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{col}_r"))

# Step 16: Assign winners based on ranks
merged_data <- merged_data %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_r == 1 ~ "PRI",
    inv_mun_PRD_PT_PRS_r == 1 ~ "PRD_PT_PRS",
    inv_mun_PVEM_r == 1 ~ "PVEM",
    inv_mun_PC_r == 1 ~ "PC",
    TRUE ~ NA_character_))

# Step 17: Add year and month variables
merged_data <- merged_data %>%
  mutate(year = 2005, month = "July")

# Step 18: Sort by 'section'
merged_data <- merged_data %>%
  arrange(section)

# Step 19: Save the final dataset
write.csv(merged_data, "Nayarit_Section_2005.csv", row.names = FALSE)

# Step 1: Load the data
data <- read.csv("Ayu_Seccion_2008.csv")

# Step 2: Rename columns
data <- data %>%
  rename(municipality = nombre_municipio,
         section = seccion,
         listanominal = lista_nominal)

# Step 3: Drop rows where municipality, section, or total is missing or total is 0
data <- data %>%
  filter(municipality != "", section != ".", total != 0)

# Step 4: Convert relevant columns to numeric
data <- data %>%
  mutate(across(c(listanominal, pan:total), as.numeric))

# Step 5: Collapse (sum) data by municipality and section
collapsed_data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(c(listanominal, pan:pripanal, total), sum, na.rm = TRUE))

# Step 6: Rename variables
collapsed_data <- collapsed_data %>%
  rename(PAN = pan,
         PRI_PANAL = pripanal,
         PRD_PVEM = prdpvem,
         PT = pt,
         PC_PRS = pcprs,
         PAS = pas)

# Step 7: Calculate turnout
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal)

# Step 8: Create uniqueid based on municipality name
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPONETA" ~ 18001,
    municipality == "AHUACATLAN" ~ 18002,
    municipality == "AMATLAN DE CA?AS" ~ 18003,
    municipality == "BAHIA DE BANDERAS" ~ 18020,
    municipality == "COMPOSTELA" ~ 18004,
    municipality == "EL NAYAR" ~ 18009,
    municipality == "HUAJICORI" ~ 18005,
    municipality == "IXTLAN DEL RIO" ~ 18006,
    municipality == "JALA" ~ 18007,
    municipality == "LA YESCA" ~ 18019,
    municipality == "ROSAMORADA" ~ 18010,
    municipality == "RUIZ" ~ 18011,
    municipality == "SAN BLAS" ~ 18012,
    municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
    municipality == "SANTA MARIA DEL ORO" ~ 18014,
    municipality == "SANTIAGO IXCUINTLA" ~ 18015,
    municipality == "TECUALA" ~ 18016,
    municipality == "TEPIC" ~ 18017,
    municipality == "TUXPAN" ~ 18018,
    municipality == "XALISCO" ~ 18008,
    TRUE ~ 0))

# Step 9: Calculate 'valid' as the sum of relevant variables
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(across(c(PAN, PT, PAS, PRD_PVEM, PC_PRS, PRI_PANAL))))

# Step 10: Calculate municipal-level sums and inverse values
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN:valid, total, listanominal), ~ sum(.x, na.rm = TRUE), .names = "mun_{.col}"),
         across(starts_with("mun_"), ~ 1 / .x, .names = "inv_{.col}"))

# Step 11: Calculate 'mun_turnout'
collapsed_data <- collapsed_data %>%
  mutate(mun_turnout = mun_total / mun_listanominal)

# Step 12: Generate row ranks
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{col}_r"))

# Step 13: Assign winners based on ranks
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_PANAL_r == 1 ~ "PRI_PANAL",
    inv_mun_PRD_PVEM_r == 1 ~ "PRD_PVEM",
    inv_mun_PT_r == 1 ~ "PT",
    inv_mun_PAS_r == 1 ~ "PAS",
    inv_mun_PC_PRS_r == 1 ~ "PC_PRS",
    TRUE ~ NA_character_))

# Step 14: Add year and month variables
collapsed_data <- collapsed_data %>%
  mutate(year = 2008, month = "July")

# Step 15: Sort by 'section'
collapsed_data <- collapsed_data %>%
  arrange(section)

# Step 16: Save the final dataset
write.csv(collapsed_data, "Nayarit_Section_2008.csv", row.names = FALSE)

# Step 1: Load the Excel data (ensure you have readxl installed)
data <- read_excel("Ayu_Seccion_2011.xlsx", sheet = "Sheet1")

# Step 2: Drop columns K and L
data <- data %>% select(-K, -L)

# Step 3: Rename columns
data <- data %>%
  rename(municipality = MUNICIPIO,
         section = Section)

# Step 4: Replace missing municipalities with previous row values
data <- data %>%
  fill(municipality, .direction = "down")

# Step 5: Calculate total and drop rows with missing or zero total
data <- data %>%
  mutate(total = rowSums(select(., PAN, PRD, PRS, PRI_PVEM_PANAL, PT_PC), na.rm = TRUE)) %>%
  filter(!is.na(total) & total != 0)

# Step 6: Drop unnecessary columns
data <- data %>%
  select(-CandidatosNoRegistrados, -VotosNulos)

# Step 7: Trim whitespace from municipality
data <- data %>%
  mutate(municipality = trimws(municipality))

# Step 8: Collapse (sum) data by municipality and section
collapsed_data <- data %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN, PRD, PRS, PRI_PVEM_PANAL, PT_PC, total), sum, na.rm = TRUE))

# Step 9: Generate 'uniqueid' for each municipality
collapsed_data <- collapsed_data %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPONETA" ~ 18001,
    municipality == "AHUACATLAN" ~ 18002,
    municipality == "AMATLAN DE CANAS" ~ 18003,
    municipality == "BAHIA DE BANDERAS" ~ 18020,
    municipality == "COMPOSTELA" ~ 18004,
    municipality == "EL NAYAR" ~ 18009,
    municipality == "HUAJICORI" ~ 18005,
    municipality == "IXTLAN DEL RIO" ~ 18006,
    municipality == "JALA" ~ 18007,
    municipality == "LA YESCA" ~ 18019,
    municipality == "ROSAMORADA" ~ 18010,
    municipality == "RUIZ" ~ 18011,
    municipality == "SAN BLAS" ~ 18012,
    municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
    municipality == "SANTA MARIA DEL ORO" ~ 18014,
    municipality == "SANTIAGO IXCUINTLA" ~ 18015,
    municipality == "TECUALA" ~ 18016,
    municipality == "TEPIC" ~ 18017,
    municipality == "TUXPAN" ~ 18018,
    municipality == "XALISCO" ~ 18008,
    TRUE ~ 0
  ))

# Step 10: Calculate 'valid' as the sum of relevant columns
collapsed_data <- collapsed_data %>%
  mutate(valid = rowSums(select(., PAN, PRD, PRS, PRI_PVEM_PANAL, PT_PC)))

# Step 11: Generate municipal-level sums and inverse values
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN, PRD, PRS, PRI_PVEM_PANAL, PT_PC, total, valid), ~ sum(.x, na.rm = TRUE), .names = "mun_{.col}"),
         across(starts_with("mun_"), ~ 1 / .x, .names = "inv_mun_{.col}"))

# Step 12: Merge external data for listanominal
# (Assuming 'all_months_years.dta' is pre-processed and saved as CSV or available for reading in R)
all_data <- read.csv("..\\..\\all_months_years.csv")
collapsed_data <- collapsed_data %>%
  left_join(all_data, by = c("section" = "seccion", "uniqueid" = "ed")) %>%
  filter(month == 6 & year == 2011) %>%
  select(-month, -year)

# Step 13: Calculate turnout and municipal turnout
collapsed_data <- collapsed_data %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / mun_listanominal)

# Step 14: Calculate ranks
collapsed_data <- collapsed_data %>%
  group_by(uniqueid) %>%
  mutate(across(starts_with("inv_mun_"), rank, .names = "{col}_r"))

# Step 15: Determine the winner based on ranks
collapsed_data <- collapsed_data %>%
  mutate(winner = case_when(
    inv_mun_PAN_r == 1 ~ "PAN",
    inv_mun_PRI_PVEM_PANAL_r == 1 ~ "PRI_PVEM_PANAL",
    inv_mun_PRD_r == 1 ~ "PRD",
    inv_mun_PRS_r == 1 ~ "PRS",
    inv_mun_PT_PC_r == 1 ~ "PT_PC",
    TRUE ~ NA_character_))

# Step 16: Add year and month variables
collapsed_data <- collapsed_data %>%
  mutate(year = 2011, month = "July")

# Step 17: Sort by section
collapsed_data <- collapsed_data %>%
  arrange(section)

# Step 18: Save the final dataset
write.csv(collapsed_data, "Nayarit_Section_2011.csv", row.names = FALSE)

# Step 1: Load the datasets
nayarit_1996 <- read_dta("Nayarit_Section_1996.dta")
nayarit_1999 <- read_dta("Nayarit_Section_1999.dta")
nayarit_2002 <- read_dta("Nayarit_Section_2002.dta")
nayarit_2005 <- read_dta("Nayarit_Section_2005.dta")
nayarit_2008 <- read_dta("Nayarit_Section_2008.dta")
nayarit_2011 <- read_dta("Nayarit_Section_2011.dta")

# Step 2: Append datasets using bind_rows
nayarit_all <- bind_rows(nayarit_1996, nayarit_1999, nayarit_2002, nayarit_2005, nayarit_2008, nayarit_2011)

# Step 3: Remove the individual datasets (in R, we don't need to "erase" files, but we can remove objects from memory)
rm(nayarit_1996, nayarit_1999, nayarit_2002, nayarit_2005, nayarit_2008, nayarit_2011)

# Step 4: Generate a new binary variable "PAN_winner"
nayarit_all <- nayarit_all %>%
  mutate(PAN_winner = ifelse(grepl("PAN", winner) & !grepl("PANAL", winner), 1, 0))

# Step 5: Initialize winner_counter with PAN_winner and process other parties
nayarit_all <- nayarit_all %>%
  mutate(winner_counter = PAN_winner)

# Define a vector of other party variables
party_vars <- c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "PD", "PSD", "PAS", "PSN", "PartidoMexicoPosible", "PRS")

# Loop through each party variable and update winner_counter
for (party in party_vars) {
  nayarit_all <- nayarit_all %>%
    mutate(!!paste0(party, "_winner") := ifelse(grepl(party, winner), 1, 0),
           winner_counter = winner_counter + !!sym(paste0(party, "_winner")))
}

# Step 6: Tabulate winner_counter and count zero cases
print(table(nayarit_all$winner_counter))
print(sum(nayarit_all$winner_counter == 0))

# Step 7: Save the dataset in Stata format (version 12)
write_dta(nayarit_all, "Nayarit_ALL.dta", version = 12)

# Save in an alternative directory (adjust the path as necessary)
write_dta(nayarit_all, "../Nayarit_ALL.dta", version = 12)

# Step 1: Import Excel Sheets and Save Individually as .dta
sheetnames <- excel_sheets("Ayuntamientos_2014.xlsx")

for (sheet in sheetnames) {
  data <- read_excel("Ayuntamientos_2014.xlsx", sheet = sheet)
  
  # Save each sheet as a separate .dta file
  write_dta(data, paste0(sheet, ".dta"))
}

# Step 2: Append Multiple .dta Files into One Dataset
file_list <- list.files(pattern = "*.dta")
data_combined <- do.call(bind_rows, lapply(file_list, read_dta))

# Step 3: Keep only valid rows where municipality is not empty
data_combined <- data_combined %>%
  filter(municipality != "")

# Step 4: Convert character columns to numeric where needed
data_combined <- data_combined %>%
  mutate(across(everything(), ~ as.numeric(.), .names = "numeric_{col}"))

# Step 5: Generate `uniqueid`
data_combined <- data_combined %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACAPONETA" ~ 18001,
      municipality == "AHUACATLÁN" ~ 18002,
      municipality == "AMATLÁN DE CAÑAS" ~ 18003,
      municipality == "BAHÍA DE BANDERAS" ~ 18020,
      municipality == "COMPOSTELA" ~ 18004,
      municipality == "DEL NAYAR" ~ 18009,
      municipality == "HUAJICORI" ~ 18005,
      municipality == "IXTLÁN DEL RÍO" ~ 18006,
      municipality == "JALA" ~ 18007,
      municipality == "LA YESCA" ~ 18019,
      municipality == "ROSAMORADA" ~ 18010,
      municipality == "RUÍZ" ~ 18011,
      municipality == "SAN BLAS" ~ 18012,
      municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
      municipality == "SANTA MARIA DEL ORO" ~ 18014,
      municipality == "SANTIAGO IXCUINTLA" ~ 18015,
      municipality == "TECUALA" ~ 18016,
      municipality == "TEPIC" ~ 18017,
      municipality == "TUXPAN" ~ 18018,
      municipality == "XALISCO" ~ 18008,
      TRUE ~ 0
    )
  )

# Step 6: Rename Columns
data_combined <- data_combined %>%
  rename(section = Sección, no_reg = CandidatoNoReg, nulo = VotosNulos)

# Step 7: Collapse Data by Municipality, Section, and Unique ID
data_combined <- data_combined %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(c(PAN, PRI_PVEM_PANAL, PRD, PT, MC, PRS, CI_1, no_reg, nulo), sum, .names = "sum_{col}"))

# Step 8: Generate Valid, Total, and Municipal-Level Variables
data_combined <- data_combined %>%
  rowwise() %>%
  mutate(valid = sum(c_across(starts_with("sum_")), na.rm = TRUE),
         total = valid + no_reg + nulo)

# Step 9: Create Inverse Municipal Variables and Ranks
for (var in c("PAN", "PRI_PVEM_PANAL", "PRD", "PT", "MC", "PRS", "CI_1", "total", "valid")) {
  data_combined <- data_combined %>%
    group_by(uniqueid) %>%
    mutate(!!paste0("mun_", var) := sum(!!sym(paste0("sum_", var)))) %>%
    mutate(!!paste0("i_", var) := 1 / !!sym(paste0("mun_", var)))
}

# Step 10: Rank Candidates by Performance
data_combined <- data_combined %>%
  mutate(
    PAN_r = rank(-i_PAN),
    PRI_PVEM_PANAL_r = rank(-i_PRI_PVEM_PANAL),
    PRD_r = rank(-i_PRD),
    PT_r = rank(-i_PT),
    MC_r = rank(-i_MC),
    PRS_r = rank(-i_PRS),
    CI_1_r = rank(-i_CI_1)
  )

# Step 11: Assign Winners, Second, and Third Places
data_combined <- data_combined %>%
  mutate(
    winner = case_when(
      PAN_r == 1 ~ "PAN",
      PRI_PVEM_PANAL_r == 1 ~ "PRI_PVEM_PANAL",
      PRD_r == 1 ~ "PRD",
      PT_r == 1 ~ "PT",
      MC_r == 1 ~ "MC",
      PRS_r == 1 ~ "PRS",
      CI_1_r == 1 ~ "Independent",
      TRUE ~ ""
    ),
    second = case_when(
      PAN_r == 2 ~ "PAN",
      PRI_PVEM_PANAL_r == 2 ~ "PRI_PVEM_PANAL",
      PRD_r == 2 ~ "PRD",
      PT_r == 2 ~ "PT",
      MC_r == 2 ~ "MC",
      PRS_r == 2 ~ "PRS",
      CI_1_r == 2 ~ "Independent",
      TRUE ~ ""
    ),
    third = case_when(
      PAN_r == 3 ~ "PAN",
      PRI_PVEM_PANAL_r == 3 ~ "PRI_PVEM_PANAL",
      PRD_r == 3 ~ "PRD",
      PT_r == 3 ~ "PT",
      MC_r == 3 ~ "MC",
      PRS_r == 3 ~ "PRS",
      CI_1_r == 3 ~ "Independent",
      TRUE ~ ""
    )
  )

# Step 12: Drop Extra Columns and Finalize
data_combined <- data_combined %>%
  select(-starts_with("i_"), -starts_with("sum_"))

# Step 13: Add Metadata (Year, Month, State)
data_combined <- data_combined %>%
  mutate(year = 2014, month = "July", STATE = "NAYARIT")

# Step 14: Merge with Nominal List and Calculate Turnout
ln2014 <- read_dta("..\Listas Nominales\LN 2012-2019\2014\LN2014.dta") %>%
  filter(entidad == 18, month == 5) %>%
  select(entidad, municipio, seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

data_combined <- data_combined %>%
  left_join(ln2014, by = c("uniqueid", "section")) %>%
  mutate(mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal,
         turnout = total / listanominal)

# Step 15: Save Final Datasets
write_dta(data_combined, "Nayarit_Section_2014.dta")

# Load the Nayarit 2014 section data
nayarit_2014 <- read_dta("Nayarit_Section_2014.dta")

# Collapse the data, summing columns by municipality and uniqueid
nayarit_2014_collapsed <- nayarit_2014 %>%
  group_by(municipality, uniqueid) %>%
  summarise(
    PAN = sum(PAN, na.rm = TRUE),
    total = sum(total, na.rm = TRUE),
    listanominal = sum(listanominal, na.rm = TRUE),
    STATE = first(STATE),
    year = first(year),
    month = first(month),
    winner = first(winner),
    second = first(second),
    third = first(third),
    turnout = first(mun_turnout)  # Use first value of municipal turnout
  ) %>%
  ungroup()

# Rename the `mun_turnout` column to `turnout`
nayarit_2014_collapsed <- nayarit_2014_collapsed %>%
  rename(turnout = turnout)

# Sort by uniqueid
nayarit_2014_collapsed <- nayarit_2014_collapsed %>%
  arrange(uniqueid)

# Save the updated dataset
write_dta(nayarit_2014_collapsed, "../../Update Municipal/Nayarit_2014.dta")

# Import Excel  2017
nayarit_2017 <- read_excel("Ayuntamientos_2017.xlsx", sheet = "AYUNTAMIENTOS")

# Generate uniqueid based on municipality
nayarit_2017 <- nayarit_2017 %>%
  mutate(uniqueid = case_when(
    municipality == "ACAPONETA" ~ 18001,
    municipality == "AHUACATLAN" ~ 18002,
    municipality == "AMATLAN DE CAÑAS" ~ 18003,
    municipality == "BAHIA DE BANDERAS" ~ 18020,
    municipality == "COMPOSTELA" ~ 18004,
    municipality == "DEL NAYAR" ~ 18009,
    municipality == "HUAJICORI" ~ 18005,
    municipality == "IXTLAN DEL RIO" ~ 18006,
    municipality == "JALA" ~ 18007,
    municipality == "LA YESCA" ~ 18019,
    municipality == "ROSAMORADA" ~ 18010,
    municipality == "RUIZ" ~ 18011,
    municipality == "SAN BLAS" ~ 18012,
    municipality == "SAN PEDRO LAGUNILLAS" ~ 18013,
    municipality == "SANTA MARIA DEL ORO" ~ 18014,
    municipality == "SANTIAGO IXCUINTLA" ~ 18015,
    municipality == "TECUALA" ~ 18016,
    municipality == "TEPIC" ~ 18017,
    municipality == "TUXPAN" ~ 18018,
    municipality == "XALISCO" ~ 18008
  ))

# Remove unnecessary columns
nayarit_2017 <- nayarit_2017 %>%
  select(-c(C_PRI_PVEM_PANAL, CP_PRI_PVEM, CP_PRI_PANAL, CP_PVEM_PANAL))

# Create PAN_PRD_PT_PRS by summing relevant columns
nayarit_2017 <- nayarit_2017 %>%
  mutate(PAN_PRD_PT_PRS = PAN + PRD + PT + PRS)

# Drop individual PAN, PRD, PT, and PRS columns
nayarit_2017 <- nayarit_2017 %>%
  select(-PAN, -PRD, -PT, -PRS)

# Drop unnecessary independent candidate columns and rename remaining ones
nayarit_2017 <- nayarit_2017 %>%
  select(-CAND_IND_4, -CAND_IND_5) %>%
  rename(CI_1 = CAND_IND_1, CI_2 = CAND_IND_2, CI_3 = CAND_IND_3)

# Collapse data by summing the specified columns, grouped by STATE, municipality, uniqueid, and section
nayarit_2017_collapsed <- nayarit_2017 %>%
  group_by(STATE, municipality, uniqueid, section) %>%
  summarise(across(c(PAN_PRD_PT_PRS, PRI, PVEM, MC, PANAL, MORENA, PES, CI_1, CI_2, CI_3, total, listanominal), sum, na.rm = TRUE)) %>%
  ungroup()

# Create the `valid` column as the row sum of certain columns
nayarit_2017_collapsed <- nayarit_2017_collapsed %>%
  mutate(valid = rowSums(across(c(PAN_PRD_PT_PRS, PRI, PVEM, MC, PANAL, MORENA, PES, CI_1, CI_2, CI_3)), na.rm = TRUE))

# Summing at municipality level and generating inverse variables
nayarit_2017_collapsed <- nayarit_2017_collapsed %>%
  group_by(uniqueid) %>%
  mutate(across(c(PAN_PRD_PT_PRS, PRI, PVEM, MC, PANAL, MORENA, PES, CI_1, CI_2, CI_3, total, valid, listanominal), 
                list(mun = sum), .names = "mun_{col}"),
         across(starts_with("mun_"), ~1/.x, .names = "i_{col}")) %>%
  ungroup()

# Calculate turnout and municipal turnout
nayarit_2017_collapsed <- nayarit_2017_collapsed %>%
  mutate(turnout = total / listanominal,
         mun_turnout = mun_total / mun_listanominal)

# Generate row ranks for each candidate/party combination
nayarit_2017_collapsed <- nayarit_2017_collapsed %>%
  mutate(across(starts_with("i_"), rank, .names = "{col}_r"))

# Initialize winner, second, and third placeholders
nayarit_2017_collapsed <- nayarit_2017_collapsed %>%
  mutate(winner = "",
         second = "",
         third = "")

# Determine winner, second, and third
nayarit_2017_collapsed <- nayarit_2017_collapsed %>%
  rowwise() %>%
  mutate(winner = case_when(i_PAN_PRD_PT_PRS_r == 1 ~ "PAN_PRD_PT_PRS",
                            PRI_r == 1 ~ "PRI",
                            PVEM_r == 1 ~ "PVEM",
                            MC_r == 1 ~ "MC",
                            PANAL_r == 1 ~ "PANAL",
                            MORENA_r == 1 ~ "MORENA",
                            PES_r == 1 ~ "PES",
                            CI_1_r == 1 ~ "CI_1",
                            CI_2_r == 1 ~ "CI_2",
                            CI_3_r == 1 ~ "CI_3"),
         second = case_when(i_PAN_PRD_PT_PRS_r == 2 ~ "PAN_PRD_PT_PRS",
                            PRI_r == 2 ~ "PRI",
                            PVEM_r == 2 ~ "PVEM",
                            MC_r == 2 ~ "MC",
                            PANAL_r == 2 ~ "PANAL",
                            MORENA_r == 2 ~ "MORENA",
                            PES_r == 2 ~ "PES",
                            CI_1_r == 2 ~ "CI_1",
                            CI_2_r == 2 ~ "CI_2",
                            CI_3_r == 2 ~ "CI_3"),
         third = case_when(i_PAN_PRD_PT_PRS_r == 3 ~ "PAN_PRD_PT_PRS",
                           PRI_r == 3 ~ "PRI",
                           PVEM_r == 3 ~ "PVEM",
                           MC_r == 3 ~ "MC",
                           PANAL_r == 3 ~ "PANAL",
                           MORENA_r == 3 ~ "MORENA",
                           PES_r == 3 ~ "PES",
                           CI_1_r == 3 ~ "CI_1",
                           CI_2_r == 3 ~ "CI_2",
                           CI_3_r == 3 ~ "CI_3")) %>%
  ungroup()

# Replace independent winners with "Independent"
nayarit_2017_collapsed <- nayarit_2017_collapsed %>%
  mutate(winner = ifelse(winner %in% c("CI_1", "CI_2", "CI_3"), "Independent", winner),
         second = ifelse(second %in% c("CI_1", "CI_2", "CI_3"), "Independent", second),
         third = ifelse(third %in% c("CI_1", "CI_2", "CI_3"), "Independent", third))

# Add year and month columns
nayarit_2017_collapsed <- nayarit_2017_collapsed %>%
  mutate(year = 2017,
         month = "June")

# Save as Stata file
write_dta(nayarit_2017_collapsed, "Nayarit_Section_2017.dta")

# Load incumbents data and merge
incumbents <- read_dta("incumbents17.dta")
nayarit_2017_merged <- merge(nayarit_2017_collapsed, incumbents, by = "uniqueid", all.x = TRUE)

# Handle incumbent votes
nayarit_2017_merged <- nayarit_2017_merged %>%
  mutate(incumbent_vote = case_when(
    uniqueid == 18012 ~ "CI_1",
    incumbent == "PAN" ~ "PAN_PRD_PT_PRS",
    incumbent == "PRI_PVEM_PANAL" ~ "PRI"
  ))

# Save the final dataset
write_dta(nayarit_2017_merged, "Nayarit_Section_2017.dta")

# Load the Nayarit_Section_2017 dataset
nayarit_2017 <- read_dta("Nayarit_Section_2017.dta")

# Collapse the data by summing specified variables and taking the first value for others
nayarit_collapsed <- nayarit_2017 %>%
  group_by(municipality, uniqueid) %>%
  summarise(across(c(PRI, total, incumbent_vote), sum, na.rm = TRUE), # Sum variables
            across(c(STATE, year, month, winner, second, third, mun_turnout, incumbent), first, na.rm = TRUE)) # Take the first value

# Rename mun_turnout to turnout
nayarit_collapsed <- nayarit_collapsed %>%
  rename(turnout = mun_turnout)

# Sort by uniqueid
nayarit_collapsed <- nayarit_collapsed %>%
  arrange(uniqueid)

# Reorder columns (this step in R allows you to arrange columns manually if needed)
nayarit_collapsed <- nayarit_collapsed %>%
  select(STATE, municipality, uniqueid, everything()) # Place important columns first

# Save the collapsed data as a .dta file
write_dta(nayarit_collapsed, "../Update Municipal/Nayarit_2017.dta")

# Load necessary libraries
library(dplyr)
library(haven)

# Load datasets
nayarit_2014 <- read_dta("Nayarit_Section_2014.dta")
nayarit_2017 <- read_dta("Nayarit_Section_2017.dta")

# Append datasets
nayarit_combined <- bind_rows(nayarit_2014, nayarit_2017)

# Create and update PAN_winner and winner_counter columns
nayarit_combined <- nayarit_combined %>%
  mutate(PAN_winner = ifelse(grepl("PAN", winner) & !grepl("PANAL", winner), 1, 0),
         winner_counter = PAN_winner)

# Loop through variables (PRI, PRD, etc.) and update the corresponding _winner columns and winner_counter
for (var in c("PRI", "PRD", "PT", "PC", "PVEM", "PANAL", "MORENA", "MC")) {
  nayarit_combined <- nayarit_combined %>%
    mutate(!!paste0(var, "_winner") := ifelse(grepl(var, winner), 1, 0)) %>%
    mutate(winner_counter = winner_counter + !!sym(paste0(var, "_winner")))
}

# Check winner_counter values
table(nayarit_combined$winner_counter)

# Count the number of rows where winner_counter equals 0
nrow(filter(nayarit_combined, winner_counter == 0))

# Save combined data
write_dta(nayarit_combined, "Nayarit_Section_14_17.dta")

# Load Nayarit_ALL dataset and append the combined 2014-2017 dataset
nayarit_all <- read_dta("../../Precinct/Nayarit_ALL.dta")
nayarit_all_combined <- bind_rows(nayarit_all, nayarit_combined)

# Save the appended dataset
write_dta(nayarit_all_combined, "Nayarit_ALL_SALVADOR.dta")

# Erase unnecessary datasets (deleting files)
file.remove("Nayarit_Section_2014.dta")
file.remove("Nayarit_Section_2017.dta")
file.remove("Nayarit_Section_14_17.dta")

# Erase all temporary "Table" files
for (i in 2:21) {
  file.remove(paste0("Table ", i, ".dta"))
}












