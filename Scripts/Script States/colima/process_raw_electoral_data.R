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

#######################-------------------1994---------------###############################

ayu_seccion <- read.csv("../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1994_No_LN.csv")
colnames(ayu_seccion) <- tolower(colnames(ayu_seccion))
# Step 2: Rename columns
ayu_seccion <- ayu_seccion %>%
  rename(
    municipality = municipio,
    section = seccion
  )

# Step 3: Drop empty rows
ayu_seccion <- ayu_seccion %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert numeric columns (destring equivalent)
ayu_seccion <- ayu_seccion %>%
  mutate(across(pan:total, as.numeric))

# Step 5: Collapse data (group by municipality and section, sum numeric columns)
collapsed_1994 <- ayu_seccion %>%
  group_by(municipality, section) %>%
  summarize(across(pan:total, sum, na.rm = TRUE), .groups = "drop")

# Step 6: Rename columns
collapsed_1994 <- collapsed_1994 %>%
  rename(
    PAN = pan, 
    PRI = pri, 
    PRD = prd, 
    PartCardenista = pfcrn,
    PT = ptc, 
    PRT = prt, 
    PPS = pps, 
    PDM = pdm
  )

# Step 7: Drop unnecessary columns
collapsed_1994 <- collapsed_1994 %>%
  select(-c(nulos, "no.reg."))

# Step 8: Generate `uniqueid`
collapsed_1994 <- collapsed_1994 %>%
  mutate(
    uniqueid = case_when(
      municipality == "ARMERIA" ~ 6001,
      municipality == "COLIMA" ~ 6002,
      municipality == "COMALA" ~ 6003,
      municipality == "COQUIMATLAN" ~ 6004,
      municipality == "CUAUHTEMOC" ~ 6005,
      municipality == "IXTLAHUACAN" ~ 6006,
      municipality == "MANZANILLO" ~ 6007,
      municipality == "MINATITLAN" ~ 6008,
      municipality == "TECOMAN" ~ 6009,
      municipality == "VILLA DE ALVAREZ" ~ 6010,
      TRUE ~ 0
    )
  )

# Step 9: Compute `valid` as the row total
collapsed_1994 <- collapsed_1994 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PPS, PRD, PartCardenista, PDM, PRT, PT)), na.rm = TRUE)) %>%
  ungroup()

# Step 15: Add year and month, sort, and save
collapsed_1994 <- collapsed_1994 %>%
  mutate(
    year = 1994,
    month = "August"
  ) %>%
  arrange(section)

#######################-------------------1997---------------###############################

# Step 1: Load the CSV file
ayu_seccion <- read.csv("../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997.csv")
colnames(ayu_seccion) <- tolower(colnames(ayu_seccion))
colnames(ayu_seccion) <- gsub("\\.", "", colnames(ayu_seccion))
# Step 2: Rename columns
ayu_seccion <- ayu_seccion %>%
  rename(
    municipality = municipio,
    section = casillas
  )

# Step 3: Drop empty rows and rows where `total` is missing or zero
ayu_seccion <- ayu_seccion %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert numeric columns (`destring`)
ayu_seccion <- ayu_seccion %>%
  mutate(across(listanominal:total, as.numeric))
names(ayu_seccion)
# Step 5: Collapse data (sum columns by municipality and section)
collapsed_1997 <- ayu_seccion %>%
  group_by(municipality, section) %>%
  summarize(across(listanominal:total, sum, na.rm = TRUE), .groups = "drop")

# Step 6: Rename columns
collapsed_1997 <- collapsed_1997 %>%
  rename(
    PAN = pan, 
    PRI = pri, 
    PRD = prd, 
    PartCardenista = pc,
    PT = pt, 
    PVEM = pvem, 
    PPS = pps, 
    PDM = pdm
  )

# Step 7: Compute turnout
collapsed_1997 <- collapsed_1997 %>%
  mutate(turnout = total / listanominal)

# Step 8: Drop unnecessary columns
collapsed_1997 <- collapsed_1997 %>%
  select(-c(noreg, nulos))

# Step 9: Generate `uniqueid`
collapsed_1997 <- collapsed_1997 %>%
  mutate(
    uniqueid = case_when(
      municipality == "ARMERIA" ~ 6001,
      municipality == "COLIMA" ~ 6002,
      municipality == "COMALA" ~ 6003,
      municipality == "COQUIMATLAN" ~ 6004,
      municipality == "CUAUHTEMOC" ~ 6005,
      municipality == "IXTLAHUACAN" ~ 6006,
      municipality == "MANZANILLO" ~ 6007,
      municipality == "MINATITLAN" ~ 6008,
      municipality == "TECOMAN" ~ 6009,
      municipality == "VILLA DE ALVAREZ" ~ 6010,
      TRUE ~ 0
    )
  )

# Step 10: Compute `valid` as the row total
collapsed_1997 <- collapsed_1997 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PRD, PartCardenista, PT, PVEM, PPS, PDM)), na.rm = TRUE)) %>%
  ungroup()


# Step 14: Add year and month, and save the file
collapsed_1997 <- collapsed_1997 %>%
  mutate(
    year = 1997,
    month = "July"
  )

#######################-------------------2000---------------###############################

ayu_seccion <- read.csv("../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2000_No_LN.csv")
colnames(ayu_seccion) <- tolower(colnames(ayu_seccion))
colnames(ayu_seccion) <- gsub("\\.", "", colnames(ayu_seccion))
# Step 2: Rename columns
ayu_seccion <- ayu_seccion %>%
  rename(
    municipality = municipio,
    section = seccion
  )

# Step 3: Drop empty rows and rows where `total` is missing or zero
ayu_seccion <- ayu_seccion %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert numeric columns (`destring`)
ayu_seccion <- ayu_seccion %>%
  mutate(across(total:nulos, as.numeric))

# Step 5: Collapse data (sum columns by municipality and section)
collapsed_2000<- ayu_seccion %>%
  group_by(municipality, section) %>%
  summarize(across(total:nulos, sum, na.rm = TRUE), .groups = "drop")

# Step 6: Rename columns
collapsed_2000 <- collapsed_2000 %>%
  rename(
    PAN = pan, 
    PRI = pri, 
    PRD = prd, 
    PVEM = pvem,
    PDS = ds, 
    ADC = adc, 
    PAN_PRD = prdpan
  )

# Step 7: Drop unnecessary columns
collapsed_2000 <- collapsed_2000 %>%
  select(-c(noreg, nulos))

# Step 8: Generate `uniqueid`
collapsed_2000 <- collapsed_2000 %>%
  mutate(
    uniqueid = case_when(
      municipality == "ARMERIA" ~ 6001,
      municipality == "COLIMA" ~ 6002,
      municipality == "COMALA" ~ 6003,
      municipality == "COQUIMATLAN" ~ 6004,
      municipality == "CUAUHTEMOC" ~ 6005,
      municipality == "IXTLAHUACAN" ~ 6006,
      municipality == "MANZANILLO" ~ 6007,
      municipality == "MINATLAN" ~ 6008,
      municipality == "TECOMAN" ~ 6009,
      municipality == "VILLA DE ALVAREZ" ~ 6010,
      TRUE ~ 0
    )
  )

# Step 9: Compute `valid` as the row total
collapsed_2000 <- collapsed_2000 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PRD, PVEM, PDS, ADC, PAN_PRD)), na.rm = TRUE)) %>%
  ungroup()

# Step 11: Add `ed` and `seccion`, merge with listanominal data
collapsed_2000 <- collapsed_2000 %>%
  mutate(state = "COLIMA", 
         seccion = section)

ln_data <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>%
  filter(month == "July" & year == 2000) %>%
  select(state, section, lista)

collapsed_2000 <- collapsed_2000 %>%
  left_join(ln_data, by = c("state", "section")) %>%
  filter(!is.na(lista))  # Drop unmatched rows

# Drop temporary columns
collapsed_2000 <- collapsed_2000 %>%
  select(-c(state, seccion)) %>%
  rename(listanominal = lista)

# Step 12: Compute turnout
collapsed_2000 <- collapsed_2000 %>%
  mutate(
    turnout = total / listanominal
  )

# Step 15: Add year and month, and save the file
collapsed_2000 <- collapsed_2000 %>%
  mutate(
    year = 2000,
    month = "July"
  )

#######################-------------------2003---------------###############################


# Step 1: Load the CSV file
ayu_seccion <- read.csv("../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003.csv")
colnames(ayu_seccion) <- gsub("\\.", "", colnames(ayu_seccion))
# Step 2: Rename columns
ayu_seccion <- ayu_seccion %>%
  rename(
    municipality = nombre_municipio,
    section = seccion
  )

# Step 3: Drop empty rows and rows where `total` is missing or zero
ayu_seccion <- ayu_seccion %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert numeric columns (`destring`)
ayu_seccion <- ayu_seccion %>%
  mutate(across(listanominal:total, as.numeric))
names(ayu_seccion)
# Step 5: Collapse data (sum columns by municipality and section)
collapsed_2003 <- ayu_seccion %>%
  group_by(municipality, section) %>%
  summarize(across(c(PAN:total,listanominal), sum, na.rm = TRUE), .groups = "drop")

# Step 6: Rename columns
collapsed_2003 <- collapsed_2003 %>%
  rename(
    MexicoPosible = "MexicoPosible", 
    FC = "FuerzaCiudadana"
  )

# Step 7: Compute turnout
collapsed_2003 <- collapsed_2003 %>%
  mutate(turnout = total / listanominal)

# Step 8: Generate `uniqueid`
collapsed_2003 <- collapsed_2003 %>%
  mutate(
    uniqueid = case_when(
      municipality == "ARMERIA" ~ 6001,
      municipality == "COLIMA" ~ 6002,
      municipality == "COMALA" ~ 6003,
      municipality == "COQUIMATLAN" ~ 6004,
      municipality == "CUAUHTEMOC" ~ 6005,
      municipality == "IXTLAHUACAN" ~ 6006,
      municipality == "MANZANILLO" ~ 6007,
      municipality == "MINATITLAN" ~ 6008,
      municipality == "TECOMAN" ~ 6009,
      municipality == "VILLA DE ALVAREZ" ~ 6010,
      TRUE ~ 0
    )
  )

# Step 9: Compute `valid` as the row total
collapsed_2003 <- collapsed_2003 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI, PRD, PT, PVEM, PC, PSN, PAS, ADC, MexicoPosible, FC)), na.rm = TRUE)) %>%
  ungroup()

# Step 13: Add year and month, and save the file
collapsed_2003 <- collapsed_2003 %>%
  mutate(
    year = 2003,
    month = "July"
  ) %>%
  arrange(section)

#######################-------------------2006---------------###############################

# Step 1: Load the CSV file
ayu_seccion <- read.csv("../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2006.csv")
colnames(ayu_seccion) <- tolower(colnames(ayu_seccion))
colnames(ayu_seccion) <- gsub("\\.", "", colnames(ayu_seccion))
# Step 2: Rename columns
ayu_seccion <- ayu_seccion %>%
  rename(
    municipality = nombre_municipio,
    section = seccion
  )

# Step 3: Drop empty rows and rows where `total` is missing or zero
ayu_seccion <- ayu_seccion %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert numeric columns (`destring`)
ayu_seccion <- ayu_seccion %>%
  mutate(across(listanominal:total, as.numeric))
names(ayu_seccion)
# Step 5: Collapse data (sum columns by municipality and section)
collapsed_2006 <- ayu_seccion %>%
  group_by(municipality, section) %>%
  summarize(across(c(pan:total,listanominal), sum, na.rm = TRUE), .groups = "drop")

# Step 6: Rename columns
collapsed_2006 <- collapsed_2006 %>%
  rename(
    PAN = pan, 
    PRI_PVEM = pripvem, 
    PRD_ADC = prdadc,
    PT_PC = ptpc, 
    PAS = coalicion
  )

# Step 7: Compute turnout
collapsed_2006 <- collapsed_2006 %>%
  mutate(turnout = total / listanominal)

# Step 8: Generate `uniqueid`
collapsed_2006 <- collapsed_2006 %>%
  mutate(
    uniqueid = case_when(
      municipality == "ARMERIA" ~ 6001,
      municipality == "COLIMA" ~ 6002,
      municipality == "COMALA" ~ 6003,
      municipality == "COQUIMATLAN" ~ 6004,
      municipality == "CUAUHTEMOC" ~ 6005,
      municipality == "IXTLAHUACAN" ~ 6006,
      municipality == "MANZANILLO" ~ 6007,
      municipality == "MINATITLAN" ~ 6008,
      municipality == "TECOMAN" ~ 6009,
      municipality == "VILLA DE ALVAREZ" ~ 6010,
      TRUE ~ 0
    )
  )

# Step 9: Compute `valid` as the row total
collapsed_2006 <- collapsed_2006 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI_PVEM, PRD_ADC, PT_PC, PAS)), na.rm = TRUE)) %>%
  ungroup()

# Step 13: Add year and month, and sort by section
collapsed_2006 <- collapsed_2006 %>%
  mutate(
    year = 2006,
    month = "July"
  ) %>%
  arrange(section)

#######################-------------------2009---------------###############################

# Step 1: Load the CSV file
ayu_seccion <- read.csv("../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2009.csv")
colnames(ayu_seccion) <- tolower(colnames(ayu_seccion))
colnames(ayu_seccion) <- gsub("\\.", "", colnames(ayu_seccion))

# Step 2: Rename columns
ayu_seccion <- ayu_seccion %>%
  rename(
    municipality = nombre_municipio,
    section = seccion
  )

# Step 3: Drop empty rows and rows where `total` is missing or zero
ayu_seccion <- ayu_seccion %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(total) & total != 0)

# Step 4: Convert numeric columns (`destring`)
ayu_seccion <- ayu_seccion %>%
  mutate(across(listanominal:total, as.numeric))

# Step 5: Collapse data (sum columns by municipality and section)
collapsed_2009 <- ayu_seccion %>%
  group_by(municipality, section) %>%
  summarize(across(listanominal:total, sum, na.rm = TRUE), .groups = "drop")

# Step 6: Replace `pripanal` and `prdpsd` and drop unnecessary columns
collapsed_2009 <- collapsed_2009 %>%
  mutate(
    pripanal = pri + panal + pripanal,
    prdpsd = prd + psd + prdpsd
  ) %>%
  select(-c(pri, panal, prd, psd))

# Step 7: Rename columns
collapsed_2009 <- collapsed_2009 %>%
  rename(
    PAN_ADC = panadc, 
    PRI_PANAL = pripanal, 
    PRD_PSD = prdpsd,
    PT = pt, 
    PVEM = pvem, 
    PC = pc
  )

# Step 8: Compute turnout
collapsed_2009 <- collapsed_2009 %>%
  mutate(turnout = total / listanominal)

# Step 9: Generate `uniqueid`
collapsed_2009 <- collapsed_2009 %>%
  mutate(
    uniqueid = case_when(
      municipality == "ARMERIA" ~ 6001,
      municipality == "COLIMA" ~ 6002,
      municipality == "COMALA" ~ 6003,
      municipality == "COQUIMATLAN" ~ 6004,
      municipality == "CUAUHTEMOC" ~ 6005,
      municipality == "IXTLAHUACAN" ~ 6006,
      municipality == "MANZANILLO" ~ 6007,
      municipality == "MINATITLAN" ~ 6008,
      municipality == "TECOMAN" ~ 6009,
      municipality == "VILLA DE ALVAREZ" ~ 6010,
      TRUE ~ 0
    )
  )

# Step 10: Compute `valid` as the row total
collapsed_2009 <- collapsed_2009 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN_ADC, PT, PVEM, PC, PRI_PANAL, PRD_PSD)), na.rm = TRUE)) %>%
  ungroup()

# Step 14: Add year and month, and sort by section
collapsed_2009 <- collapsed_2009 %>%
  mutate(
    year = 2009,
    month = "July"
  ) %>%
  arrange(section)

#######################-------------------2012---------------###############################

# Step 1: Load the dataset
ayu_seccion <- read_dta("../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/CASILLAS_AYUNTAMIENTOS_2012/Ayu_Seccion_2012.dta")

# Step 2: Drop empty rows and rows where `total` is missing or zero
ayu_seccion <- ayu_seccion %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!is.na(Total) & Total != 0)

# Step 3: Rename `Total` to `total`
ayu_seccion <- ayu_seccion %>%
  rename(total = Total)

# Step 4: Collapse data (sum columns by municipality and section)
collapsed_2012 <- ayu_seccion %>%
  group_by(municipality, section) %>%
  summarize(across(PAN:ADC, sum, na.rm = TRUE), total = sum(total, na.rm = TRUE), .groups = "drop")

# Step 5: Generate `uniqueid`
collapsed_2012 <- collapsed_2012 %>%
  mutate(
    uniqueid = case_when(
      municipality == "ARMERIA" ~ 6001,
      municipality == "COLIMA" ~ 6002,
      municipality == "COMALA" ~ 6003,
      municipality == "COQUIMATLAN" ~ 6004,
      municipality == "CUAUHTEMOC" ~ 6005,
      municipality == "IXTLAHUACAN" ~ 6006,
      municipality == "MANZANILLO" ~ 6007,
      municipality == "MINATITLAN" ~ 6008,
      municipality == "TECOMAN" ~ 6009,
      municipality == "VILLA DE ALVAREZ" ~ 6010,
      TRUE ~ 0
    )
  )

# Step 6: Compute `valid` as the row total
collapsed_2012 <- collapsed_2012 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN, PRI_PANAL, PRD, PT, PVEM, PC, ADC)), na.rm = TRUE)) %>%
  ungroup()

# Step 8: Add `ed` and `seccion`, merge with listanominal data
collapsed_2012 <- collapsed_2012 %>%
  mutate(state = "COLIMA", seccion = section)

ln_data <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>%
  filter(month == "July" & year == 2012 & day == 1) %>%
  select(state, section, lista)

collapsed_2012 <- collapsed_2012 %>%
  left_join(ln_data, by = c("state", "section")) %>%
  filter(!is.na(lista))  # Drop unmatched rows

# Drop temporary columns
collapsed_2012 <- collapsed_2012 %>%
  rename(listanominal = lista)

# Step 9: Compute turnout
collapsed_2012 <- collapsed_2012 %>%
  mutate(
    turnout = total / listanominal
  )

# Step 12: Add year and month, and sort by section
collapsed_2012 <- collapsed_2012 %>%
  mutate(
    year = 2012,
    month = "July"
  ) %>%
  arrange(section)

#######################-------------------2015---------------###############################

# Define a function to process individual sheets
process_sheet <- function(sheet_name, municipality_name, uniqueid) {
  # Load the Excel sheet
  data <- read_excel(
    "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls",
    sheet = sheet_name,
    na = "",
    guess_max = 10000
  )
  
  # Process the data
  data <- data %>%
    filter(!is.na(`TIPO CASILLA`)) %>% # Drop rows where `TIPOCASILLA` is empty
    mutate(
      SECCION = ifelse(is.na(SECCION), lag(SECCION), SECCION), # Fill missing `SECCION` values with previous
      municipality = municipality_name, # Add `municipality`
      uniqueid = uniqueid # Add `uniqueid`
    ) %>%
    select(SECCION, `LISTA NOMINAL`:TOTAL, municipality, uniqueid) %>%
    rename(
      section = SECCION,
      listanominal = `LISTA NOMINAL`,
      total = TOTAL,
      no_reg = `CANDIDATOS NO REGISTRADOS`,
      nulo = `VOTOS NULOS`,
      MORENA = Morena
    ) %>%
    mutate(across(listanominal:total, ~replace_na(as.numeric(.), 0))) %>% # Replace missing numeric values with 0
    group_by(municipality, uniqueid, section) %>%
    summarize(across(listanominal:total, sum, na.rm = TRUE), .groups = "drop") # Collapse by municipality, uniqueid, and section
  
  write_dta(data, paste0("../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/",municipality_name, "_PS.dta"))
}

# Process each sheet
process_sheet("ARMERIA", "ARMERIA", 6001)
process_sheet("COLIMA", "COLIMA", 6002)
process_sheet("COMALA", "COMALA", 6003)
process_sheet("COQUIMATLAN", "COQUIMATLAN", 6004)
process_sheet("CUAUHTEMOC", "CUAUHTEMOC", 6005)
process_sheet("IXTLAHUACAN", "IXTLAHUACAN", 6006)
process_sheet("MANZANILLO", "MANZANILLO", 6007)
process_sheet("MINATITLAN", "MINATITLAN", 6008)
process_sheet("TECOMAN", "TECOMAN", 6009)
process_sheet("VILLA DE ALVAREZ", "VILLA DE ALVAREZ", 6010)

# Step 2: Append all the processed files
file_list <- c(
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/ARMERIA_PS.dta", 
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/COLIMA_PS.dta", 
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/COMALA_PS.dta",
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/COQUIMATLAN_PS.dta", 
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/CUAUHTEMOC_PS.dta", 
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/IXTLAHUACAN_PS.dta",
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/MANZANILLO_PS.dta", 
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/MINATITLAN_PS.dta", 
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/TECOMAN_PS.dta",
  "../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/VILLA DE ALVAREZ_PS.dta"
)

# Load and append all files into a single dataframe
all_data <- bind_rows(lapply(file_list, read_dta))

# Step 3: Clean up and finalize
all_data <- all_data %>%
  mutate(
    PES = ifelse(is.na(PES) & !is.na(PESE), PESE, PES), # Replace missing `PES` with `PESE` values
    PRI_PVEM_PANAL = ifelse(!is.na(PRI_PVEM_PANAL), PRI_PVEM_PANAL + PRI + PVEM + PANAL, PRI_PVEM_PANAL),
    PRI = ifelse(!is.na(PRI_PVEM_PANAL), NA, PRI), # Set `PRI` to NA if `PRI_PVEM_PANAL` is not NA
    PVEM = ifelse(!is.na(PRI_PVEM_PANAL), NA, PVEM),
    PANAL = ifelse(!is.na(PRI_PVEM_PANAL), NA, PANAL)) %>%
  select(-PESE) # Drop `PESE`

# Step 4: Collapse data (sum columns by municipality and section)
collapsed_2015 <- all_data %>%
  group_by(municipality, uniqueid, section) %>%
  summarize(across(listanominal:PRI_PVEM_PANAL, sum, na.rm = TRUE), total = sum(total, na.rm = TRUE), .groups = "drop")

# Step 9: Compute turnout
collapsed_2015 <- collapsed_2015 %>%
  mutate(
    turnout = total / listanominal
  )

# Step 12: Add year and month, and sort by section
collapsed_2015 <- collapsed_2015 %>%
  mutate(
    year = 2015,
    month = "June"
  ) %>%
  arrange(section)

# Step 5: Clean up temporary files
file.remove(file_list)

#######################-------------------2018---------------###############################

# Step 1: Load the Excel file
data <- read_excel("../../../Data/Raw Electoral Data/Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/1 COL RES AYUN.xlsx", sheet = "Reporte_Casillas_Ayuntamientos", na = "", guess_max = 10000)
colnames(data) <- tolower(colnames(data))
colnames(data) <- gsub(" ", "", colnames(data))
colnames(data) <- gsub("-", "", colnames(data))
# Step 2: Rename columns
data <- data %>%
  rename(
    municipality = municipio,
    section = sección,
    no_reg = noreg,
    nulo = nulos,
    total = totaldevotos
  )

# Step 3: Convert all columns to numeric where applicable
data <- data %>%
  mutate(across(listanominal:total, as.numeric))

# Step 4: Generate combined columns and drop unnecessary ones
data <- data %>%
  mutate(
    PAN_PRD = pan + prd + panprd,
    PRI_PVEM_PANAL = pri + pvem + pripvem,
    MORENA_PT_PES = ptmorenapes + ptmorena + morenapes + ptpes + pt + morena + pes,
    CI_1 = municipiocomala + municipiomanzanillo
  ) %>%
  select(-c(pan, prd, panprd, pri, pvem, pripvem, ptmorenapes, ptmorena, morenapes, ptpes, pt, morena, pes, municipiocomala, municipiomanzanillo))

# Step 5: Rename columns for consistency
data <- data %>%
  rename(PANAL = pna,
         MC=mc)

# Step 6: Reorder columns
data <- data %>%
  select(PAN_PRD, PRI_PVEM_PANAL, MORENA_PT_PES, CI_1, everything())

# Step 7: Collapse data by `municipality`, `section`, and `uniqueid`
collapsed_2018 <- data %>%
  group_by(municipality, section, uniqueid) %>%
  summarize(across(c(PAN_PRD,
                     PRI_PVEM_PANAL,
                     MORENA_PT_PES, 
                     CI_1,
                     listanominal,
                     PANAL,
                     MC,
                     total
                     ), sum, na.rm = TRUE), .groups = "drop")

# Step 8: Compute `valid` as the row total
collapsed_2018 <- collapsed_2018 %>%
  rowwise() %>%
  mutate(valid = sum(c_across(c(MC, PANAL, PAN_PRD, PRI_PVEM_PANAL, MORENA_PT_PES, CI_1)), na.rm = TRUE)) %>%
  ungroup()

# Step 10: Compute turnout
collapsed_2018 <- collapsed_2018 %>%
  mutate(
    turnout = total / listanominal
  )

# Step 12: Add metadata and save the file
collapsed_2018 <- collapsed_2018 %>%
  mutate(
    year = 2018,
    month = "July",
    STATE = "COLIMA"
  )

# Combine the dataframes, handling different columns by filling with NA
Colima_all <- bind_rows(collapsed_1994,
                                collapsed_1997,
                                collapsed_2000,
                                collapsed_2003,
                                collapsed_2006,
                                collapsed_2009,
                                collapsed_2012,
                                collapsed_2015,
                                collapsed_2018)

data.table::fwrite(Colima_all,"../../../Processed Data/colima/colima_process_raw_data.csv")
summary(Colima_all)



