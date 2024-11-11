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


# Set the working directory (adjust the path as needed)
Ayu_Seccion_1995_No_LN <- read_csv("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_1995_No_LN.csv")
colnames(Ayu_Seccion_1995_No_LN) <- tolower(colnames(Ayu_Seccion_1995_No_LN))
# Step 2: Rename columns
Ayu_Seccion_1995_No_LN <- Ayu_Seccion_1995_No_LN %>%
  rename(
    municipality = municipio,
    section = secion
  )

# Step 3: Drop invalid rows
Ayu_Seccion_1995_No_LN <- Ayu_Seccion_1995_No_LN %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert numeric columns
Ayu_Seccion_1995_No_LN <- Ayu_Seccion_1995_No_LN %>%
  mutate(across(pan:total, as.numeric))

# Step 5: Collapse data by `municipality` and `section`
collapsed_1995 <- Ayu_Seccion_1995_No_LN %>%
  group_by(municipality, section) %>%
  summarize(across(pan:total, sum, na.rm = TRUE), .groups = "drop")

# Step 6: Rename columns
collapsed_1995 <- collapsed_1995 %>%
  rename(
    PartCardenista = pfcrn,
    PT = pt,
    PVEM = pvem,
    PAN = pan,
    PRI = pri,
    PRD = prd
  )

# Step 7: Drop unnecessary columns
collapsed_1995 <- collapsed_1995 %>%
  select(-c(valida, nulos, "no registrados"))

# Step 8: Add `uniqueid`
collapsed_1995 <- collapsed_1995 %>%
  mutate(
    uniqueid = case_when(
      municipality == "CANATLAN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO DE COMONFORT" ~ 10003,
      municipality == "CUENCAME" ~ 10004,
      municipality == "DUANGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      municipality == "SIMON BOLIVAR" ~ 10006,
      municipality == "GOMEZ PALACIO" ~ 10007,
      municipality == "GUADALUPE VICTORIA" ~ 10008,
      municipality == "GUANACEVI" ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "INDE" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIMI" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTAEZ" ~ 10019,
      municipality == "PANUCO DE CORONADO" ~ 10020,
      municipality == "PENON BLANCO" ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
      municipality == "SAN JUAN DEL RIO" ~ 10028,
      municipality == "SAN LUIS DEL CORDERO" ~ 10029,
      municipality == "SAN PEDRO DEL GALLO" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
      municipality == "SUCHIL" ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRERO" ~ 10038,
      TRUE ~ 0
    )
  )

# Step 9: Compute `valid` as the row total
collapsed_1995 <- collapsed_1995 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PRD, PartCardenista, PT, PVEM)), na.rm = TRUE)) %>%
  ungroup()

# Step 12: Add year and month, and save the file
collapsed_1995 <- collapsed_1995 %>%
  mutate(
    year = 1995,
    month = "July")

# Step 1: Load the CSV file
Ayu_Seccion_1998_No_LN <- read.csv("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_1998_No_LN.csv")
colnames(Ayu_Seccion_1998_No_LN) <- tolower(colnames(Ayu_Seccion_1998_No_LN))

# Step 2: Rename columns
Ayu_Seccion_1998_No_LN <- Ayu_Seccion_1998_No_LN %>%
  rename(
    municipality = municipio,
    section = seccion
  )

# Step 3: Drop invalid rows
Ayu_Seccion_1998_No_LN <- Ayu_Seccion_1998_No_LN %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert numeric columns
Ayu_Seccion_1998_No_LN <- Ayu_Seccion_1998_No_LN %>%
  mutate(across(pan:total, as.numeric))

# Step 5: Collapse data by `municipality` and `section`
collapsed_1998 <- Ayu_Seccion_1998_No_LN %>%
  group_by(municipality, section) %>%
  summarize(across(pan:total, sum, na.rm = TRUE), .groups = "drop")

# Step 6: Rename columns
collapsed_1998 <- collapsed_1998 %>%
  rename(
    PT = pt,
    PVEM = pvem,
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT = pt,
  )

# Step 7: Drop unnecessary columns
collapsed_1998 <- collapsed_1998 %>%
  select(-c(valida, nulos, "no.registrados"))

# Step 8: Add `uniqueid`
collapsed_1998 <- collapsed_1998 %>%
  mutate(
    uniqueid = case_when(
      municipality == "CANATLAN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO COMONFORT" ~ 10003,
      municipality == "CUENCAME" ~ 10004,
      municipality == "DURNAGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      municipality == "SIMON BOLIVAR" ~ 10006,
      municipality == "GOMEZ PALACIO" ~ 10007,
      municipality == "GUADALUPE VICTORIA" ~ 10008,
      municipality == "GUANACEVI" ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "INDE" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIMI" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTAEZ" ~ 10019,
      municipality == "PANUCO DE CORONADO" ~ 10020,
      municipality == "PENON BLANCO" ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
      municipality == "SAN JUAN DEL RIO" ~ 10028,
      municipality == "SAN LUIS DEL CORDERO" ~ 10029,
      municipality == "SAN PEDRO DEL GALLO" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
      municipality == "SUCHIL" ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRERO" ~ 10038,
      TRUE ~ 0
    )
  )

# Step 9: Compute `valid` as the row total
collapsed_1998 <- collapsed_1998 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PRD, PT, PVEM)), na.rm = TRUE)) %>%
  ungroup()

# Step 11: Add external data (merge listanominal data)
ln_data <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>%
  filter(month == "June" & year == 1998) %>%
  select(state, section, lista)

collapsed_1998 <- collapsed_1998 %>%
  mutate(state = "DURANGO") %>%
  left_join(ln_data, by = c("state", "section")) %>%
  filter(!is.na(lista)) %>%
  rename(listanominal = lista)

# Step 12: Compute turnout
collapsed_1998 <- collapsed_1998 %>%
  mutate(
    turnout = total / listanominal
  )

# Step 15: Add metadata and save the file
collapsed_1998 <- collapsed_1998 %>%
  mutate(
    year = 1998,
    month = "July"
  )

# Step 1: Load and process the first CSV file
Ayu_Seccion_2001_No_LN <- read.csv("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_2001_No_LN.csv") 
colnames(Ayu_Seccion_2001_No_LN) <- tolower(colnames(Ayu_Seccion_2001_No_LN))

Ayu_Seccion_2001_No_LN <- Ayu_Seccion_2001_No_LN %>%
  rename(
    municipality = municipio,
    section = seccion
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0) %>%
  mutate(across(pan:total, as.numeric))

# Step 2: Load and process the second CSV file
durango_data <- read.csv("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/Durango_Seccion_2001_No_LN.csv") 
colnames(durango_data) <- tolower(colnames(durango_data))


durango_data <- durango_data %>%
  rename(
    municipality = municipio,
    section = seccion
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  mutate(across(pan:total, as.numeric)) %>%
  filter(!is.na(total) & total != 0)

# Append the two datasets
combined_data <- bind_rows(durango_data, Ayu_Seccion_2001_No_LN)

# Step 3: Collapse data by `municipality` and `section`
collapsed_2001<- combined_data %>%
  group_by(municipality, section) %>%
  summarize(across(pan:pt, sum, na.rm = TRUE), .groups = "drop")

# Step 4: Rename columns
collapsed_2001 <- collapsed_2001 %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PRD_PT = prd.pt,
    PT = pt,
    PVEM = pvem,
    PC = pc,
    PSN = psn,
    PAS = pas,
    PD = pd
  ) %>%
  select(-c(validos, nulos, no.registrados))

# Step 5: Add `uniqueid`
collapsed_2001 <- collapsed_2001 %>%
  mutate(
    uniqueid = case_when(
      municipality == "CANATLAN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO DE COMONFORT" ~ 10003,
      municipality == "CUENCAME" ~ 10004,
      municipality == "DURANGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      municipality == "SIMON BOLIVAR" ~ 10006,
      municipality == "GOMEZ PALACIO" ~ 10007,
      municipality == "GUADALUPE VICTORIA" ~ 10008,
      municipality == "GUANACEVI" ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "INDE" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIMI" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTAEZ" ~ 10019,
      municipality == "PANUCO DE CORONADO" ~ 10020,
      municipality == "PENON BLANCO" ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
      municipality == "SAN JUAN DEL RIO" ~ 10028,
      municipality == "SAN LUIS DEL CORDERO" ~ 10029,
      municipality == "SAN PEDRO DEL GALLO" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
      municipality == "SUCHIL" ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRERO" ~ 10038,
      TRUE ~ 0
    )
  )

# Step 6: Compute `valid` as the row total
collapsed_2001 <- collapsed_2001 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PRD_PT, PVEM, PC, PSN, PAS, PD, PRD, PT)), na.rm = TRUE)) %>%
  ungroup()

# Step 8: Add external data (merge `listanominal`)
ln_data <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>%
  filter(month == "June" & year == 2001) %>%
  select(state, section, lista)

collapsed_2001 <- collapsed_2001 %>%
  mutate(state = "DURANGO") %>%
  left_join(ln_data, by = c("state", "section")) %>%
  filter(!is.na(lista)) %>%
  rename(listanominal = lista)

# Step 9: Compute turnout
collapsed_2001 <- collapsed_2001 %>%
  mutate(
    turnout = total / listanominal
  )

# Step 11: Add metadata and save the file
collapsed_2001 <- collapsed_2001 %>%
  mutate(
    year = 2001,
    month = "July"
  )

# Step 1: Import the Excel file and filter by date
lista_nominal <- read_excel("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/INE-CI141-2014 Horacio Larreguy Arbesu/pdln10_edms_PEL_2004_2007.xls", 
                            sheet = "pdln10_edms") %>%
  filter(FECHA == "20040704") %>%
  rename(
    section = SEC,
    listanominal = LISTA
  ) %>%
  group_by(section) %>%
  summarize(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")

# Step 2: Import and process the CSV file
Ayu_Seccion_2004_No_LN <- read.csv("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_2004_No_LN.csv")
colnames(Ayu_Seccion_2004_No_LN) <- tolower(colnames(Ayu_Seccion_2004_No_LN))

Ayu_Seccion_2004_No_LN <- Ayu_Seccion_2004_No_LN %>%
  rename(
    municipality = municipio,
    section = seccion
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0) %>%
  mutate(across(pan:total, as.numeric)) %>%
  group_by(municipality, section) %>%
  summarize(across(pan:total, sum, na.rm = TRUE), .groups = "drop")

# Rename columns
collapsed_2004 <- Ayu_Seccion_2004_No_LN %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD_PT = prd.pt,
    PVEM = pvem,
    PD = pd
  )

# Step 3: Merge with ListaNominal2004
collapsed_2004 <- collapsed_2004 %>%
  left_join(lista_nominal, by = "section") %>%
  filter(!is.na(listanominal)) # Drop rows with missing `listanominal`

# Step 4: Compute turnout
collapsed_2004 <- collapsed_2004 %>%
  mutate(turnout = total / listanominal)

# Step 5: Add `uniqueid`
collapsed_2004 <- collapsed_2004 %>%
  mutate(
    uniqueid = case_when(
      municipality == "CANATLAN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO DE COMONFORT" ~ 10003,
      municipality == "CUENCAME" ~ 10004,
      municipality == "DURANGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      municipality == "SIMON BOLIVAR" ~ 10006,
      municipality == "GOMEZ PALACIO" ~ 10007,
      municipality == "GUADALUPE VICTORIA" ~ 10008,
      municipality == "GUANACEVI" ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "INDE" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIMI" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTAEZ" ~ 10019,
      municipality == "PANUCO DE CORONADO" ~ 10020,
      municipality == "PENON BLANCO" ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
      municipality == "SAN JUAN DEL RIO" ~ 10028,
      municipality == "SAN LUIS DEL CORDERO" ~ 10029,
      municipality == "SAN PEDRO DEL GALLO" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
      municipality == "SUCHIL" ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRERO" ~ 10038,
      TRUE ~ 0
    )
  )

# Step 6: Compute `valid` as the row total
collapsed_2004 <- collapsed_2004 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PRD_PT, PVEM, PD)), na.rm = TRUE)) %>%
  ungroup()


# Step 10: Add metadata and save the file
collapsed_2004 <- collapsed_2004 %>%
  mutate(
    year = 2004,
    month = "July"
  ) %>%
  arrange(section)

# Step 1: Import the Excel file and filter by date
lista_nominal <- read_excel("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/INE-CI141-2014 Horacio Larreguy Arbesu/pdln10_edms_PEL_2004_2007.xls", 
                            sheet = "pdln10_edms") %>%
  filter(FECHA == "20070701") %>%
  rename(
    section = SEC,
    listanominal = LISTA
  ) %>%
  group_by(section) %>%
  summarize(listanominal = sum(listanominal, na.rm = TRUE), .groups = "drop")


# Step 2: Import and process the dataset
Ayu_Seccion_2007 <- read_dta("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_2007.dta") 
colnames(Ayu_Seccion_2007) <- tolower(colnames(Ayu_Seccion_2007))

Ayu_Seccion_2007 <- Ayu_Seccion_2007 %>%
  rename(
    municipality = municipio,
    section = seccion
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0) %>%
  mutate(across(pan:pripanalpd, as.numeric))

# Step 3: Collapse data by `municipality` and `section`
collapsed_2007 <- Ayu_Seccion_2007 %>%
  group_by(municipality, section) %>%
  summarize(across(pan:pripanalpd, sum, na.rm = TRUE), .groups = "drop")

# Step 4: Rename columns
collapsed_2007 <- collapsed_2007 %>%
  rename(
    PAN = pan,
    PRI_PANAL = pripanal,
    PRI_PANAL_PD = pripanalpd,
    PRD = prd,
    PT_PC = ptpc,
    PVEM = pvem,
    PAS = pas,
    PD = pd
  )

# Step 5: Merge with ListaNominal2007
collapsed_2007 <- collapsed_2007 %>%
  left_join(lista_nominal, by = "section") %>%
  filter(!is.na(listanominal)) # Drop rows with missing `listanominal`

# Step 6: Compute turnout
collapsed_2007 <- collapsed_2007 %>%
  mutate(turnout = total / listanominal)

# Step 7: Add `uniqueid`
collapsed_2007 <- collapsed_2007 %>%
  mutate(
    uniqueid = case_when(
      municipality == "CANATLAN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO DE COMONFORT" ~ 10003,
      municipality == "CUENCAME" ~ 10004,
      municipality == "DURANGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      grepl("BOLIVAR", municipality) ~ 10006,
      grepl("MEZ PALACIO", municipality) ~ 10007,
      municipality == "GUADALUPE  VICTORIA" ~ 10008,
      grepl("GUANACEV", municipality) ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "INDE" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIMI" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTAEZ" ~ 10019,
      grepl("NUCO DE CORONADO", municipality) ~ 10020,
      grepl("ON BLANCO", municipality) ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
      municipality == "SAN JUAN DEL RIO" ~ 10028,
      municipality == "SAN LUIS DEL CORDERO" ~ 10029,
      municipality == "SAN PEDRO DEL GALLO" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
      grepl("CHIL", municipality) ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRERO" ~ 10038,
      TRUE ~ 0
    )
  )

# Step 8: Compute `valid` as the row total
collapsed_2007 <- collapsed_2007 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI_PANAL, PRI_PANAL_PD, PRD, PT_PC, PVEM, PD, PAS)), na.rm = TRUE)) %>%
  ungroup()

# Step 12: Add metadata and save the file
collapsed_2007 <- collapsed_2007 %>%
  mutate(
    year = 2007,
    month = "July"
  ) %>%
  arrange(section)

# Step 1: Import and clean the data
Ayu_Seccion_2010 <- read_csv("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_2010.csv")

colnames(Ayu_Seccion_2010) <- tolower(colnames(Ayu_Seccion_2010))

Ayu_Seccion_2010 <- Ayu_Seccion_2010 %>%
  rename(
    municipality = municipio,
    section = seccion,
    listanominal = nominal
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>% 
  filter(!is.na(total) & total != 0) %>%
  mutate(across("pan-prd-pc":listanominal, as.numeric))

# Step 2: Collapse data by `municipality` and `section`
collapsed_2010 <- Ayu_Seccion_2010 %>%
  group_by(municipality, section) %>%
  summarize(across("pan-prd-pc":listanominal, sum, na.rm = TRUE), .groups = "drop")

# Step 3: Rename columns
collapsed_2010 <- collapsed_2010 %>%
  rename(
    PAN_PRD_PC = "pan-prd-pc",
    PRI_PVEM_PANAL_PD = "pri-pvem-panal-pd",
    PT = pt
  )

# Step 4: Compute turnout
collapsed_2010 <- collapsed_2010 %>%
  mutate(turnout = total / listanominal)

# Step 5: Add `uniqueid`
collapsed_2010 <- collapsed_2010 %>%
  mutate(
    uniqueid = case_when(
      municipality == "CANATLAN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO DE COMONFORT" ~ 10003,
      municipality == "CUENCAME" ~ 10004,
      municipality == "DURANGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      municipality == "SIMON BOLIVAR" ~ 10006,
      municipality == "GOMEZ PALACIO" ~ 10007,
      municipality == "GUADALUPE VICTORIA" ~ 10008,
      municipality == "GUANACEVI" ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "INDE" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIMI" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTAEZ" ~ 10019,
      municipality == "PANUCO DE CORONADO" ~ 10020,
      municipality == "PENON BLANCO" ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
      municipality == "SAN JUAN DEL RIO" ~ 10028,
      municipality == "SAN LUIS DEL CORDERO" ~ 10029,
      municipality == "SAN PEDRO DEL GALLO" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
      municipality == "SUCHIL" ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRERO" ~ 10038,
      TRUE ~ 0
    )
  )

# Step 6: Compute `valid` as the row total
collapsed_2010 <- collapsed_2010 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN_PRD_PC, PRI_PVEM_PANAL_PD, PT)), na.rm = TRUE)) %>%
  ungroup()

# Step 10: Add metadata and save the file
collapsed_2010 <- collapsed_2010 %>%
  mutate(
    year = 2010,
    month = "July"
  ) %>%
  arrange(section)

# Step 1: Import and clean the data
Ayu_Seccion_2013 <- read_csv("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/Ayu_Seccion_2013.csv", 
                             stringsAsFactors = FALSE, 
                             fileEncoding = "UTF-8")

colnames(Ayu_Seccion_2013) <- tolower(colnames(Ayu_Seccion_2013))

Ayu_Seccion_2013 <- Ayu_Seccion_2013 %>%
  rename(
    municipality = ayuntamiento,
    section = seccion
  ) %>%
  mutate(
    municipality = iconv(municipality, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""),
    municipality = toupper(municipality)
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>% 
  mutate(
    total = as.numeric(valid) + as.numeric(`no registrados`) + as.numeric(nulos),
    across(`lista nominal`:mc, as.numeric)
  ) %>%
  filter(!is.na(total) & total != 0)
names(Ayu_Seccion_2013)
# Step 2: Collapse data by `municipality` and `section`
collapsed_2013 <- Ayu_Seccion_2013 %>%
  group_by(municipality, section) %>%
  summarize(across("lista nominal":total, sum, na.rm = TRUE), .groups = "drop")

# Step 3: Rename columns
collapsed_2013 <- collapsed_2013 %>%
  rename(
    PAN = pan,
    PRI_PVEM_PANAL_PD = pri_pvem_panal_pd,
    PRD = prd,
    PT = pt,
    PC = mc,
    "listanominal" = "lista nominal"
  )

# Step 4: Compute turnout
collapsed_2013 <- collapsed_2013 %>%
  mutate(turnout = total / listanominal)
unique(collapsed_2013$municipality)
# Step 5: Add `uniqueid`
collapsed_2013 <- collapsed_2013 %>%
  mutate(
    uniqueid = case_when(
      municipality == "CANATLN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO DE COMON" ~ 10003,
      municipality == "CUENCAM" ~ 10004,
      municipality == "DURANGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      municipality == "GENERAL SIMN B" ~ 10006,
      municipality == "GMEZ PALACIO" ~ 10007,
      municipality == "GUADALUPE VICTO" ~ 10008,
      municipality == "GUANACEV" ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "IND" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIM" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTEZ" ~ 10019,
      municipality == "PNUCO DE CORON" ~ 10020,
      municipality == "PEN BLANCO" ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUA" ~ 10027,
      municipality == "SAN JUAN DEL R" ~ 10028,
      municipality == "SAN LUIS DEL CO" ~ 10029,
      municipality == "SAN PEDRO DEL G" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQ" ~ 10032,
      municipality == "SCHIL" ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRER" ~ 10038,
      TRUE ~ 0
    )
  )

# Step 6: Compute `valid` as the row total
collapsed_2013 <- collapsed_2013 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI_PVEM_PANAL_PD, PRD, PT, PC)), na.rm = TRUE)) %>%
  ungroup()

# Step 10: Add metadata and save the file
collapsed_2013 <- collapsed_2013 %>%
  mutate(
    year = 2013,
    month = "July"
  ) %>%
  arrange(section)

# Step 1: Import and process sheets from the Excel file
file_path <- "../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/Ayuntamientos Municipio_2016.xlsx"
sheet_names <- excel_sheets(file_path)

# Import each sheet and save it as an individual data frame
for (sheet in sheet_names) {
  temp_data <- read_excel(file_path, sheet = sheet, col_names = TRUE)
  temp_data <- temp_data %>%
    mutate(municipio = sheet)
  assign(sheet, temp_data)
}

# Combine all the individual municipality data into one dataset
combined_data <- bind_rows(
  `01. CANATLÁN`, `02. CANELAS`, `03. CONETO DE COMONFORT`, `04. CUENCAMÉ`,
  `05. DURANGO`, `06. GÓMEZ PALACIO`, `07. GRAL. SIMÓN BOLÍVAR`, `08. GUADALUPE VICTORIA`,
  `09. GUANACEVÍ`, `10. HIDALGO`, `11. INDÉ`, `12. LERDO`, `13. MAPIMÍ`, `14. MEZQUITAL`,
  `15. NAZAS`, `16. NOMBRE DE DIOS`, `17. NUEVO IDEAL`, `18. OCAMPO`, `19. EL ORO`,
  `20. OTAÉZ`, `21. PÁNUCO DE CORONADO`, `22. PEÑÓN BLANCO`, `23. POANAS`, `24. PUEBLO NUEVO`,
  `25. RODEO`, `26. SAN BERNARDO`, `27. SAN DIMAS`, `28. SAN JUAN DE GUADALUPE`,
  `29. SAN JUAN DEL RÍO`, `30. SAN LUIS DEL CORDERO`, `31. SAN PEDRO DEL GALLO`,
  `32. SANTA CLARA`, `33. SANTIAGO PAPASQUIARO`, `34. SÚCHIL`, `35. TAMAZULA`,
  `36. TEPEHUANES`, `37. TLAHUALILO`, `38. TOPIA`, `39. VICENTE GUERRERO`
)

# Step 2: Clean up temporary datasets
rm(list = sheet_names)
colnames(combined_data) <- tolower(colnames(combined_data))

# Remove rows containing "DTTO" in any column
combined_data <- combined_data %>%
  filter(!apply(across(everything(), ~ grepl("DTTO", ., ignore.case = TRUE)), 1, any))

# Step 3: Standardize and clean up combined data
cleaned_data <- combined_data %>%
  mutate(
    municipio = sub("^[0-9]+\\.\\s", "", municipio),
    municipio = toupper(municipio),
    municipio = gsub("Á", "A", municipio),
    municipio = gsub("É", "E", municipio),
    municipio = gsub("Í", "I", municipio),
    municipio = gsub("Ó", "O", municipio),
    municipio = gsub("Ú", "U", municipio),
    municipio = gsub("Ñ", "N", municipio)
  ) %>%
  filter(!is.na(sección)) %>%
  mutate(
    across(c(sección:`l. nominal`,pri:pd), as.numeric)
  ) %>%
  filter(!is.na(total) & total != 0) %>% 
  rename(municipality=municipio)

# Step 5: Add `uniqueid`
cleaned_data <- cleaned_data %>%
  mutate(
    uniqueid = case_when(
      municipality == "CANATLAN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO DE COMONFORT" ~ 10003,
      municipality == "CUENCAME" ~ 10004,
      municipality == "DURANGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      municipality == "GRAL. SIMON BOLIVAR" ~ 10006,
      municipality == "GOMEZ PALACIO" ~ 10007,
      municipality == "GUADALUPE VICTORIA" ~ 10008,
      municipality == "GUANACEVI" ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "INDE" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIMI" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTAEZ" ~ 10019,
      municipality == "PANUCO DE CORONADO" ~ 10020,
      municipality == "PENON BLANCO" ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
      municipality == "SAN JUAN DEL RIO" ~ 10028,
      municipality == "SAN LUIS DEL CORDERO" ~ 10029,
      municipality == "SAN PEDRO DEL GALLO" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
      municipality == "SUCHIL" ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRERO" ~ 10038,
      TRUE ~ NA
    )
  )

# Step 4: Generate `uniqueid` and process municipality numbers
cleaned_data <- cleaned_data %>%
  rename(
    section = sección,
    PAN_PRD = "pan-prd",
    PRI_PVEM_PD_PANAL = "pri-pvem-pd-pna",
    PT = pt,
    MORENA = morena,
    PES = pes,
    CI_1 = ci_1,
    PRI = pri,
    PVEM = pvem,
    PANAL = pna,
    CI_2 = ci_2,
    MC = mc,
    PD = pd,
    listanominal = "l. nominal",
  )

# Step 5: Aggregate data
collapsed_2016 <- cleaned_data %>%
  group_by(municipality, section, uniqueid) %>%
  summarize(across(PAN_PRD:PD, sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    valid = rowSums(across(PAN_PRD:PD)),
    turnout = total / listanominal
  )

# Step 9: Add metadata and save data
collapsed_2016 <- collapsed_2016 %>%
  mutate(
    year = 2016,
    month = "June",
    STATE = "DURANGO"
  )

# Load the data
data <- read_csv("../../../Data/Raw Electoral Data/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2019/2019_SEE_AYUN _DGO_CAS.csv")
colnames(data) <- tolower(colnames(data))
# Rename columns
data <- data %>%
  rename(
    section = seccion,
    municipality = municipio_local,
    listanominal = lista_nominal,
    total = total_votos
  )

# Combine PAN and PRD into a coalition variable if applicable
data <- data %>%
  mutate(
    PAN_PRD = ifelse(!is.na(coal_pan_prd), pan + prd + coal_pan_prd, NA),
    pan = ifelse(!is.na(coal_pan_prd), NA, pan),
    prd = ifelse(!is.na(coal_pan_prd), NA, prd)
  ) %>%
  select(-coal_pan_prd)
names(data)
# Rename other columns
data <- data %>%
  rename(
    PAN = "pan",
    PRI = "pri",
    PRD = "prd",
    PVEM = "pvem",
    PT = pt,
    MORENA = morena,
    PRI = pri,
    PVEM = pvem,
    CI_1 = cand_ind1,
    CI_2 = cand_ind2,
    CI_3 = cand_ind3,
    CI_4 = cand_ind4,
    CI_5 = cand_ind5,
    CI_6 = cand_ind6,
    MC = mc,
    PD = pd,
  )

# Aggregate by municipality and section
collapsed_2019 <- data %>%
  group_by(municipality, section) %>%
  summarize(across(c(PAN:CI_6, listanominal, total, PAN_PRD), sum, na.rm = TRUE)) %>%
  ungroup()

# Add `uniqueid` based on municipality names
collapsed_2019 <- collapsed_2019 %>%
  mutate(
    uniqueid = case_when(
      municipality == "CANATLAN" ~ 10001,
      municipality == "CANELAS" ~ 10002,
      municipality == "CONETO DE COMONFORT" ~ 10003,
      municipality == "CUENCAME" ~ 10004,
      municipality == "DURANGO" ~ 10005,
      municipality == "EL ORO" ~ 10018,
      municipality == "SIMON BOLIVAR" ~ 10006,
      municipality == "GOMEZ PALACIO" ~ 10007,
      municipality == "GUADALUPE VICTORIA" ~ 10008,
      municipality == "GUANACEVI" ~ 10009,
      municipality == "HIDALGO" ~ 10010,
      municipality == "INDE" ~ 10011,
      municipality == "LERDO" ~ 10012,
      municipality == "MAPIMI" ~ 10013,
      municipality == "MEZQUITAL" ~ 10014,
      municipality == "NAZAS" ~ 10015,
      municipality == "NOMBRE DE DIOS" ~ 10016,
      municipality == "NUEVO IDEAL" ~ 10039,
      municipality == "OCAMPO" ~ 10017,
      municipality == "OTAEZ" ~ 10019,
      municipality == "PANUCO DE CORONADO" ~ 10020,
      municipality %in% c("PEÑON BLANCO", "PEÃON BLANCO") ~ 10021,
      municipality == "POANAS" ~ 10022,
      municipality == "PUEBLO NUEVO" ~ 10023,
      municipality == "RODEO" ~ 10024,
      municipality == "SAN BERNARDO" ~ 10025,
      municipality == "SAN DIMAS" ~ 10026,
      municipality == "SAN JUAN DE GUADALUPE" ~ 10027,
      municipality == "SAN JUAN DEL RIO" ~ 10028,
      municipality == "SAN LUIS DEL CORDERO" ~ 10029,
      municipality == "SAN PEDRO DEL GALLO" ~ 10030,
      municipality == "SANTA CLARA" ~ 10031,
      municipality == "SANTIAGO PAPASQUIARO" ~ 10032,
      municipality == "SUCHIL" ~ 10033,
      municipality == "TAMAZULA" ~ 10034,
      municipality == "TEPEHUANES" ~ 10035,
      municipality == "TLAHUALILO" ~ 10036,
      municipality == "TOPIA" ~ 10037,
      municipality == "VICENTE GUERRERO" ~ 10038,
      TRUE ~ NA_real_
    )
  )

# Calculate valid votes
collapsed_2019 <- collapsed_2019 %>%
  mutate(valid = rowSums(select(., c(PAN:CI_6,PAN_PRD)), na.rm = TRUE))

# Turnout calculations
collapsed_2019 <- collapsed_2019 %>%
  mutate(
    turnout = total / listanominal
  )

# Add metadata
collapsed_2019 <- collapsed_2019 %>%
  mutate(
    year = 2019,
    month = "June",
    STATE = "DURANGO"
  )

# Combine the dataframes, handling different columns by filling with NA
Durango_all <- bind_rows(collapsed_1995,
                         collapsed_1998,
                         collapsed_2001,
                         collapsed_2004,
                                collapsed_2007,
                                collapsed_2010,
                                collapsed_2013,
                                collapsed_2016,
                                collapsed_2019)

data.table::fwrite(Durango_all,"../../../Processed Data/durango/durango_process_raw_data.csv")
