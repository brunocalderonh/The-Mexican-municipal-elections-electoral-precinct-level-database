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
### Read and Prepare Data
###########################################

data_1996 <- read.csv("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/1996/Ayu_Seccion_1996.csv")

# Convert column names to lowercase
data_1996 <- data_1996 %>% rename_with(tolower)

data_1996 <- data_1996 %>%
  rename(municipality = nombre_municipio,
         section = seccion,
         listanominal = lista_nominal)

# drop if municipality=="" & section==.
data_1996 <- data_1996 %>%
  filter(!(municipality == "" & section == "."))

# Convert columns pan - total to numeric
num_vars <- c("listanominal","pan","pri","prd","pc","pt","pvem","nulos","total")

data_1996 <- data_1996 %>%
  mutate(across(all_of(num_vars), as.numeric))

# drop if total==. | total==0
data_1996 <- data_1996 %>%
  filter(!is.na(total) & total != 0)

###########################################
### Collapse (Sum) by Municipality, Section
###########################################
# Summation over municipality and section
data_1996 <- data_1996 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(num_vars), sum, na.rm = TRUE), .groups = "drop")

###########################################
### Rename Parties
###########################################
# pan -> PAN
# pri -> PRI
# prd -> PRD
# pc -> PartCardenista
# pt -> PT
# pvem -> PVEM
data_1996 <- data_1996 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PartCardenista = pc,
         PT = pt,
         PVEM = pvem)

###########################################
### Compute Turnout
###########################################
# gen turnout = total/listanominal
data_1996 <- data_1996 %>%
  mutate(turnout = total / listanominal)

# drop nulos
data_1996 <- data_1996 %>%
  select(-nulos)

###########################################
### Assign Unique IDs for Municipalities
###########################################
data_1996 <- data_1996 %>%
  mutate(uniqueid = NA)

# Create a named vector for uniqueid mapping:
unique_ids <- c("ACATLAN"=13001,"ACAXOCHITLAN"=13002,"ACTOPAN"=13003,
                "AGUA BLANCA DE ITURBIDE"=13004,"AJACUBA"=13005,"ALFAJAYUCAN"=13006,"ALMOLOYA"=13007,"APAN"=13008,
                "ATITALAQUIA"=13010,"ATLAPEXCO"=13011,"ATOTONILCO DE TULA"=13013,"ATOTONILCO EL GRANDE"=13012,
                "CALNALI"=13014,"CARDONAL"=13015,"CHAPANTONGO"=13017,"CHAPULHUACAN"=13018,"CHILCUAUTLA"=13019,
                "CUAUTEPEC DE HINOJOSA"=13016,"EL ARENAL"=13009,"ELOXOCHITLAN"=13020,"EMILIANO ZAPATA"=13021,
                "EPAZOYUCAN"=13022,"FRANCISCO I. MADERO"=13023,"HUASCA DE OCAMPO"=13024,"HUAUTLA"=13025,
                "HUAZALINGO"=13026,"HUEHUETLA"=13027,"HUEJUTLA DE REYES"=13028,"HUICHAPAN"=13029,
                "IXMIQUILPAN"=13030,"JACALA DE LEDEZMA"=13031,"JALTOCAN"=13032,"JUAREZ HIDALGO"=13033,
                "LA MISION"=13040,"LOLOTLA"=13034,"METEPEC"=13035,"METZTITLAN"=13037,
                "MINERAL DE LA REFORMA"=13051,"MINERAL DEL CHICO"=13038,"MINERAL DEL MONTE"=13039,
                "MIXQUIAHUALA DE JUAREZ"=13041,"MOLANGO DE ESCAMILLA"=13042,"NICOLAS FLORES"=13043,
                "NOPALA DE VILLAGRAN"=13044,"OMITLAN DE JUAREZ"=13045,"PACHUCA DE SOTO"=13048,"PACULA"=13047,
                "PISAFLORES"=13049,"PROGRESO DE OBREGON"=13050,"SAN AGUSTIN METZQUITITLAN"=13036,
                "SAN AGUSTIN TLAXIACA"=13052,"SAN BARTOLO TUTOTEPEC"=13053,"SAN FELIPE ORIZATLAN"=13046,
                "SAN SALVADOR"=13054,"SANTIAGO DE ANAYA"=13055,"SANTIAGO TULANTEPEC DE LUGO GU"=13056,
                "SINGUILUCAN"=13057,"TASQUILLO"=13058,"TECOZAUTLA"=13059,"TENANGO DE DORIA"=13060,
                "TEPEAPULCO"=13061,"TEPEHUACAN DE GUERRERO"=13062,"TEPEJI DEL RIO DE OCAMPO"=13063,
                "TEPETITLAN"=13064,"TETEPANGO"=13065,"TEZONTEPEC DE ALDAMA"=13067,"TIANGUISTENGO"=13068,
                "TIZAYUCA"=13069,"TLAHUELILPAN"=13070,"TLAHUILTEPA"=13071,"TLANALAPA"=13072,
                "TLANCHINOL"=13073,"TLAXCOAPAN"=13074,"TOLCAYUCA"=13075,"TULA DE ALLENDE"=13076,
                "TULANCINGO DE BRAVO"=13077,"VILLA DE TEZONTEPEC"=13066,"XOCHIATIPAN"=13078,"XOCHICOATLAN"=13079,
                "YAHUALICA"=13080,"ZACUALTIPAN DE ANGELES"=13081,"ZAPOTLAN DE JUAREZ"=13082,
                "ZEMPOALA"=13083,"ZIMAPAN"=13084)

data_1996 <- data_1996 %>%
  mutate(uniqueid = if_else(municipality %in% names(unique_ids), unique_ids[municipality], uniqueid))

###########################################
### Compute Valid Votes
###########################################
# egen valid = rowtotal(PAN PRI PartCardenista PRD PT PVEM)
valid_parties <- c("PAN","PRI","PartCardenista","PRD","PT","PVEM")
data_1996 <- data_1996 %>%
  mutate(valid = rowSums(across(all_of(valid_parties)), na.rm = TRUE))

###########################################
### Add Year and Month
###########################################
data_1996 <- data_1996 %>%
  mutate(year = 1996,
         month = "November")

###########################################
### Step 1: Read Data
###########################################
# Equivalent to: insheet using Ayu_Seccion_1999.csv, clear
data_1999 <- read.csv("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/1999/Ayu_Seccion_1999.csv")

# Convert column names to lowercase
data_1999 <- data_1999 %>% rename_with(tolower)

###########################################
### Step 2: Rename Variables
###########################################
# rename nombre_municipio municipality
# rename seccion section
# rename lista_nominal listanominal
data_1999 <- data_1999 %>%
  rename(municipality = nombre_municipio,
         section = seccion,
         listanominal = lista_nominal)

###########################################
### Step 3: Filter Rows
###########################################
# drop if municipality=="" & section==.
data_1999 <- data_1999 %>%
  filter(!(municipality == "" & section == "."))

###########################################
### Step 4: Convert to Numeric
###########################################
# destring listanominal pan - total, replace
# Identify columns from pan to total (adjust if needed)
num_vars <- c("listanominal", "pan", "pri", "prd", "pt", "pvem", "nulos", "total")

data_1999 <- data_1999 %>%
  mutate(across(all_of(num_vars), as.numeric))

# drop if total==. | total==0
data_1999 <- data_1999 %>%
  filter(!is.na(total) & total != 0)

###########################################
### Step 5: Collapse (Sum)
###########################################
# collapse (sum) listanominal pan - total, by (municipality section)
data_1999 <- data_1999 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(num_vars), sum, na.rm = TRUE), .groups = "drop")

###########################################
### Step 6: Rename Parties
###########################################
# rename pan PAN, pri PRI, prd PRD, pt PT, pvem PVEM
data_1999 <- data_1999 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem)

###########################################
### Step 7: Compute Turnout
###########################################
# gen turnout = total/listanominal
data_1999 <- data_1999 %>%
  mutate(turnout = total/listanominal)

###########################################
### Step 8: Drop Nulos
###########################################
data_1999 <- data_1999 %>%
  select(-nulos)

###########################################
### Step 9: Unique IDs for Municipalities
###########################################
# gen uniqueid=0 and then replacements
data_1999 <- data_1999 %>%
  mutate(uniqueid = 0)

unique_ids <- c("ACATLAN"=13001,"ACAXOCHITLAN"=13002,"ACTOPAN"=13003,
                "AGUA BLANCA DE ITURBIDE"=13004,"AJACUBA"=13005,"ALFAJAYUCAN"=13006,"ALMOLOYA"=13007,"APAN"=13008,
                "ATITALAQUIA"=13010,"ATLAPEXCO"=13011,"ATOTONILCO DE TULA"=13013,"ATOTONILCO EL GRANDE"=13012,
                "CALNALI"=13014,"CARDONAL"=13015,"CHAPANTONGO"=13017,"CHAPULHUACAN"=13018,"CHILCUAUTLA"=13019,
                "CUAUTEPEC DE HINOJOSA"=13016,"EL ARENAL"=13009,"ELOXOCHITLAN"=13020,"EMILIANO ZAPATA"=13021,
                "EPAZOYUCAN"=13022,"FRANCISCO I. MADERO"=13023,"HUASCA DE OCAMPO"=13024,"HUAUTLA"=13025,
                "HUAZALINGO"=13026,"HUEHUETLA"=13027,"HUEJUTLA DE REYES"=13028,"HUICHAPAN"=13029,
                "IXMIQUILPAN"=13030,"JACALA DE LEDEZMA"=13031,"JALTOCAN"=13032,"JUAREZ HIDALGO"=13033,
                "LA MISION"=13040,"LOLOTLA"=13034,"METEPEC"=13035,"METZTITLAN"=13037,
                "MINERAL DE LA REFORMA"=13051,"MINERAL DEL CHICO"=13038,"MINERAL DEL MONTE"=13039,
                "MIXQUIAHUALA DE JUAREZ"=13041,"MOLANGO DE ESCAMILLA"=13042,"NICOLAS FLORES"=13043,
                "NOPALA DE VILLAGRAN"=13044,"OMITLAN DE JUAREZ"=13045,"PACHUCA DE SOTO"=13048,"PACULA"=13047,
                "PISAFLORES"=13049,"PROGRESO DE OBREGON"=13050,"SAN AGUSTIN METZQUITITLAN"=13036,
                "SAN AGUSTIN TLAXIACA"=13052,"SAN BARTOLO TUTOTEPEC"=13053,"SAN FELIPE ORIZATLAN"=13046,
                "SAN SALVADOR"=13054,"SANTIAGO DE ANAYA"=13055,"SANTIAGO TULANTEPEC DE LUGO GU"=13056,
                "SINGUILUCAN"=13057,"TASQUILLO"=13058,"TECOZAUTLA"=13059,"TENANGO DE DORIA"=13060,
                "TEPEAPULCO"=13061,"TEPEHUACAN DE GUERRERO"=13062,"TEPEJI DEL RIO DE OCAMPO"=13063,
                "TEPETITLAN"=13064,"TETEPANGO"=13065,"TEZONTEPEC DE ALDAMA"=13067,"TIANGUISTENGO"=13068,
                "TIZAYUCA"=13069,"TLAHUELILPAN"=13070,"TLAHUILTEPA"=13071,"TLANALAPA"=13072,
                "TLANCHINOL"=13073,"TLAXCOAPAN"=13074,"TOLCAYUCA"=13075,"TULA DE ALLENDE"=13076,
                "TULANCINGO DE BRAVO"=13077,"VILLA DE TEZONTEPEC"=13066,"XOCHIATIPAN"=13078,"XOCHICOATLAN"=13079,
                "YAHUALICA"=13080,"ZACUALTIPAN DE ANGELES"=13081,"ZAPOTLAN DE JUAREZ"=13082,
                "ZEMPOALA"=13083,"ZIMAPAN"=13084)

data_1999 <- data_1999 %>%
  mutate(uniqueid = if_else(municipality %in% names(unique_ids), unique_ids[municipality], uniqueid))

###########################################
### Compute Valid Votes
###########################################
# egen valid = rowtotal(PAN PRI PRD PT PVEM)
data_1999 <- data_1999 %>%
  mutate(valid = rowSums(across(c("PAN","PRI","PRD","PT","PVEM")), na.rm = TRUE))

###########################################
### Add Year and Month
###########################################
data_1999 <- data_1999 %>%
  mutate(year = 1999,
         month = "November")

###########################################
### Step 1: Read Data
###########################################
data_2002 <- read.csv("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2002/Ayu_Seccion_2002.csv")

# Convert column names to lowercase
data_2002 <- data_2002 %>% rename_with(tolower)

###########################################
### Step 2: Rename Variables and Filter
###########################################
# rename nombre_municipio municipality
# rename seccion section
# rename lista_nominal listanominal
data_2002 <- data_2002 %>%
  rename(municipality = nombre_municipio,
         section = seccion,
         listanominal = lista_nominal)

# drop if municipality=="" & section==.
data_2002 <- data_2002 %>%
  filter(!(municipality == "" & section == "."))

# drop if total==. | total==0
# total might be missing or zero
data_2002 <- data_2002 %>%
  filter(!is.na(total) & total != 0)

###########################################
### Step 3: Convert Variables to Numeric
###########################################
# destring listanominal pan - pripvem total, replace
# Identify columns from pan to pripvem and total
# Assuming the dataset has pan, pri, prd, pt, pvem, psn, pc, pas, pripvem, total in sequence
num_vars <- c("listanominal","pan","pri","prd","pt","pvem","psn","pc","pas","pri.pvem","total")

data_2002 <- data_2002 %>%
  mutate(across(all_of(num_vars), as.numeric))

###########################################
### Step 4: Collapse (Sum) by Municipality and Section
###########################################
# collapse (sum) listanominal pan - pripvem total, by(municipality section)
data_2002 <- data_2002 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(num_vars), sum, na.rm = TRUE), .groups = "drop")

###########################################
### Step 5: Rename Parties
###########################################
# rename pan PAN, pri PRI, prd PRD, pt PT, pvem PVEM
# rename psn PSN, pc PC, pas PAS, pripvem PRI_PVEM
data_2002 <- data_2002 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PSN = psn,
         PC = pc,
         PAS = pas,
         PRI_PVEM = pri.pvem)

###########################################
### Step 6: Compute Turnout
###########################################
# gen turnout = total/listanominal
data_2002 <- data_2002 %>%
  mutate(turnout = total/listanominal)

###########################################
### Step 7: Assign Unique IDs
###########################################
data_2002 <- data_2002 %>%
  mutate(uniqueid = 0)

unique_ids <- c("ACATLAN"=13001,"ACAXOCHITLAN"=13002,"ACTOPAN"=13003,
                "AGUA BLANCA DE ITURBIDE"=13004,"AJACUBA"=13005,"ALFAJAYUCAN"=13006,"ALMOLOYA"=13007,"APAN"=13008,
                "ATITALAQUIA"=13010,"ATLAPEXCO"=13011,"ATOTONILCO DE TULA"=13013,"ATOTONILCO EL GRANDE"=13012,
                "CALNALI"=13014,"CARDONAL"=13015,"CHAPANTONGO"=13017,"CHAPULHUACAN"=13018,"CHILCUAUTLA"=13019,
                "CUAUTEPEC DE HINOJOSA"=13016,"EL ARENAL"=13009,"ELOXOCHITLAN"=13020,"EMILIANO ZAPATA"=13021,
                "EPAZOYUCAN"=13022,"FRANCISCO I. MADERO"=13023,"HUASCA DE OCAMPO"=13024,"HUAUTLA"=13025,
                "HUAZALINGO"=13026,"HUEHUETLA"=13027,"HUEJUTLA DE REYES"=13028,"HUICHAPAN"=13029,
                "IXMIQUILPAN"=13030,"JACALA DE LEDEZMA"=13031,"JALTOCAN"=13032,"JUAREZ HIDALGO"=13033,
                "LA MISION"=13040,"LOLOTLA"=13034,"METEPEC"=13035,"METZTITLAN"=13037,
                "MINERAL DE LA REFORMA"=13051,"MINERAL DEL CHICO"=13038,"MINERAL DEL MONTE"=13039,
                "MIXQUIAHUALA DE JUAREZ"=13041,"MOLANGO DE ESCAMILLA"=13042,"NICOLAS FLORES"=13043,
                "NOPALA DE VILLAGRAN"=13044,"OMITLAN DE JUAREZ"=13045,"PACHUCA DE SOTO"=13048,"PACULA"=13047,
                "PISAFLORES"=13049,"PROGRESO DE OBREGON"=13050,"SAN AGUSTIN METZQUITITLAN"=13036,
                "SAN AGUSTIN TLAXIACA"=13052,"SAN BARTOLO TUTOTEPEC"=13053,"SAN FELIPE ORIZATLAN"=13046,
                "SAN SALVADOR"=13054,"SANTIAGO DE ANAYA"=13055,"SANTIAGO TULANTEPEC DE LUGO GU"=13056,
                "SINGUILUCAN"=13057,"TASQUILLO"=13058,"TECOZAUTLA"=13059,"TENANGO DE DORIA"=13060,
                "TEPEAPULCO"=13061,"TEPEHUACAN DE GUERRERO"=13062,"TEPEJI DEL RIO DE OCAMPO"=13063,
                "TEPETITLAN"=13064,"TETEPANGO"=13065,"TEZONTEPEC DE ALDAMA"=13067,"TIANGUISTENGO"=13068,
                "TIZAYUCA"=13069,"TLAHUELILPAN"=13070,"TLAHUILTEPA"=13071,"TLANALAPA"=13072,
                "TLANCHINOL"=13073,"TLAXCOAPAN"=13074,"TOLCAYUCA"=13075,"TULA DE ALLENDE"=13076,
                "TULANCINGO DE BRAVO"=13077,"VILLA DE TEZONTEPEC"=13066,"XOCHIATIPAN"=13078,"XOCHICOATLAN"=13079,
                "YAHUALICA"=13080,"ZACUALTIPAN DE ANGELES"=13081,"ZAPOTLAN DE JUAREZ"=13082,
                "ZEMPOALA"=13083,"ZIMAPAN"=13084)

data_2002 <- data_2002 %>%
  mutate(uniqueid = if_else(municipality %in% names(unique_ids), unique_ids[municipality], uniqueid))

###########################################
### Compute Valid Votes
###########################################
# egen valid = rowtotal(PAN PRI PRD PT PVEM PSN PC PAS PRI_PVEM)
valid_parties <- c("PAN","PRI","PRD","PT","PVEM","PSN","PC","PAS","PRI_PVEM")
data_2002 <- data_2002 %>%
  mutate(valid = rowSums(across(all_of(valid_parties)), na.rm = TRUE))

###########################################
### Add Year and Month
###########################################
data_2002 <- data_2002 %>%
  mutate(year = 2002,
         month = "November")

###########################################
### Sort by Section and Save
###########################################
data_2002 <- data_2002 %>%
  arrange(section)

###########################################
### Step 1: Import Data
###########################################
data_2005 <- fread("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2005/Ayu_Seccion_2005.csv",
                   encoding = "Latin-1")

# Convert column names to lowercase
data_2005 <- data_2005 %>% rename_with(tolower)

###########################################
### Step 2: Rename Variables
###########################################
# rename nombre municipality
# rename casilla section
# rename listadonominal listanominal
# rename totales total
data_2005 <- data_2005 %>%
  rename(municipality = nombre,
         section = casilla,
         listanominal = listadonominal,
         total = totales)

###########################################
### Step 3: Filter Rows
###########################################
# drop if municipality=="" & section==.
data_2005 <- data_2005 %>%
  filter(!(municipality == "" & section == "."))

# drop if total==. | total==0
data_2005 <- data_2005 %>%
  filter(!is.na(total) & total != 0)

###########################################
### Step 4: Convert to Numeric
###########################################
# destring listanominal pan - total, replace
# Identify the columns from pan through total
# Suppose we have variables: listanominal, pan, pri, prd, pt, pvem, pc, alternativa, total
num_vars <- c("listanominal","pan","pri","prd","pt","pvem","pc","alternativa","total")
data_2005 <- data_2005 %>%
  mutate(across(all_of(num_vars), as.numeric))

###########################################
### Step 5: Collapse by Municipality, Section
###########################################
# collapse (sum) listanominal pan - alternativa total, by (municipality section)
data_2005 <- data_2005 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(num_vars), sum, na.rm = TRUE), .groups = "drop")

###########################################
### Step 6: Rename Parties
###########################################
# pan → PAN, pri → PRI, prd → PRD, pt → PT, pvem → PVEM, pc → PC, alternativa → PAS
data_2005 <- data_2005 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PC = pc,
         PAS = alternativa)

###########################################
### Step 7: Compute Turnout
###########################################
data_2005 <- data_2005 %>%
  mutate(turnout = total / listanominal)

###########################################
### Step 8: Assign Unique IDs
###########################################
data_2005 <- data_2005 %>%
  mutate(uniqueid = NA)

unique_ids <- c("ACATLÁN"=13001,"ACAXOCHITLÁN"=13002,"ACTOPAN"=13003,
                "AGUA BLANCA DE ITURBIDE"=13004,"AJACUBA"=13005,"ALFAJAYUCAN"=13006,"ALMOLOYA"=13007,"APAN"=13008,
                "ATITALAQUIA"=13010,"ATLAPEXCO"=13011,"ATOTONILCO DE TULA"=13013,"ATOTONILCO EL GRANDE"=13012,
                "CALNALI"=13014,"CARDONAL"=13015,"CHAPANTONGO"=13017,"CHAPULHUACÁN"=13018,"CHILCUAUTLA"=13019,
                "CUAUTEPEC DE HINOJOSA"=13016,"EL ARENAL"=13009,"ELOXOCHITLÁN"=13020,"EMILIANO ZAPATA"=13021,
                "EPAZOYUCAN"=13022,"FRANCISCO I. MADERO"=13023,"HUASCA DE OCAMPO"=13024,"HUAUTLA"=13025,
                "HUAZALINGO"=13026,"HUEHUETLA"=13027,"HUEJUTLA DE REYES"=13028,"HUICHAPAN"=13029,
                "IXMIQUILPAN"=13030,"JACALA DE LEDEZMA"=13031,"JALTOCAN"=13032,"JUÁREZ HIDALGO"=13033,
                "LA MISIÓN"=13040,"LOLOTLA"=13034,"METEPEC"=13035,"METZTITLÁN"=13037,
                "MINERAL DE LA REFORMA"=13051,"MINERAL DEL CHICO"=13038,"MINERAL DEL MONTE"=13039,
                "MIXQUIAHUALA DE JUÁREZ"=13041,"MOLANGO DE ESCAMILLA"=13042,"NICOLÁS FLORES"=13043,
                "NOPALA DE VILLAGRÁN"=13044,"OMITLÁN DE JUÁREZ"=13045,"PACHUCA DE SOTO"=13048,"PACULA"=13047,
                "PISAFLORES"=13049,"PROGRESO DE OBREGÓN"=13050,"SAN AGUSTÍN METZQUITITLÁN"=13036,
                "SAN AGUSTÍN TLAXIACA"=13052,"SAN BARTOLO TUTOTEPEC"=13053,"SAN FELIPE ORIZATLÁN"=13046,
                "SAN SALVADOR"=13054,"SANTIAGO DE ANAYA"=13055,"SANTIAGO TULANTEPEC DE LUGO GU"=13056,
                "SINGUILUCAN"=13057,"TASQUILLO"=13058,"TECOZAUTLA"=13059,"TENANGO DE DORIA"=13060,
                "TEPEAPULCO"=13061,"TEPEHUACÁN DE GUERRERO"=13062,"TEPEJI DEL RÍO DE OCAMPO"=13063,
                "TEPETITLÁN"=13064,"TETEPANGO"=13065,"TEZONTEPEC DE ALDAMA"=13067,"TIANGUISTENGO"=13068,
                "TIZAYUCA"=13069,"TLAHUELILPAN"=13070,"TLAHUILTEPA"=13071,"TLANALAPA"=13072,
                "TLANCHINOL"=13073,"TLAXCOAPAN"=13074,"TOLCAYUCA"=13075,"TULA DE ALLENDE"=13076,
                "TULANCINGO DE BRAVO"=13077,"VILLA DE TEZONTEPEC"=13066,"XOCHIATIPAN"=13078,"XOCHICOATLÁN"=13079,
                "YAHUALICA"=13080,"ZACUALTIPÁN DE ÁNGELES"=13081,"ZAPOTLÁN DE JUÁREZ"=13082,
                "ZEMPOALA"=13083,"ZIMAPÁN"=13084)

data_2005 <- data_2005 %>%
  mutate(uniqueid = if_else(municipality %in% names(unique_ids), unique_ids[municipality], uniqueid))

###########################################
### Compute Valid Votes
###########################################
# egen valid = rowtotal(PAN PRI PRD PT PVEM PC PAS)
valid_parties <- c("PAN","PRI","PRD","PT","PVEM","PC","PAS")
data_2005 <- data_2005 %>%
  mutate(valid = rowSums(across(all_of(valid_parties)), na.rm = TRUE))


###########################################
### Year and Month
###########################################
data_2005 <- data_2005 %>%
  mutate(year = 2005,
         month = "November")

###########################################
### Sort by Section and Save
###########################################
data_2005 <- data_2005 %>%
  arrange(section)

###########################################
### Step 1: Import Data
###########################################
data_2008 <- fread("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2008/Ayu_Seccion_2008.csv")

# Convert column names to lowercase
data_2008 <- data_2008 %>% rename_with(tolower)

###########################################
### Step 2: Rename Variables
###########################################
# rename nombre municipality
# rename seccion section
# rename listadonominal listanominal
data_2008 <- data_2008 %>%
  rename(municipality = nombre,
         section = seccion,
         listanominal = listadonominal)

###########################################
### Step 3: Filter Rows
###########################################
# drop if municipality=="" & section==.
data_2008 <- data_2008 %>%
  filter(!(municipality == "" & section == "."))

# drop if total==. | total==0
data_2008 <- data_2008 %>%
  filter(!is.na(total) & total != 0)

###########################################
### Step 4: Convert to Numeric
###########################################
# destring listanominal pan - total, replace
# Identify columns from pan through prdpt total (adjust based on the dataset)
# Assuming variables: listanominal, pan, pri, prd, pt, pv, pc, psd, alianza, prialianza, prdpt, total
num_vars <- c("listanominal", "pan", "pri", "prd", "pt", "pv", 
              "pc", "psd", "alianza", "pri-alianza", "prd-pt", "total")

data_2008 <- data_2008 %>%
  mutate(across(all_of(num_vars), as.numeric))

###########################################
### Step 5: Collapse by Municipality, Section
###########################################
# collapse (sum) listanominal pan - prdpt total, by (municipality section)
data_2008 <- data_2008 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(num_vars), sum, na.rm = TRUE), .groups = "drop")

###########################################
### Step 6: Rename Parties
###########################################
# rename pan PAN, pri PRI, prd PRD, pt PT, pv PVEM, pc PC, psd PSD, alianza PANAL, prialianza PRI_PANAL, prdpt PRD_PT
data_2008 <- data_2008 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PT = pt,
         PVEM = pv,
         PC = pc,
         PSD = psd,
         PANAL = alianza,
         PRI_PANAL = "pri-alianza",
         PRD_PT = "prd-pt")

###########################################
### Additional Drops
###########################################
# drop if municipality=="HUAZALINGO" | municipality=="ZIMAPAN"
data_2008 <- data_2008 %>%
  filter(!(municipality %in% c("HUAZALINGO", "ZIMAPAN")))

# drop if municipality=="EMILIANO ZAPATA" 
data_2008 <- data_2008 %>%
  filter(municipality != "EMILIANO ZAPATA")

###########################################
### Uniqueid Assignments
###########################################
# Initially gen uniqueid is not shown, but we must assume uniqueid starts at NA or 0
data_2008 <- data_2008 %>%
  mutate(uniqueid = NA)

# Replace uniqueid for specific municipalities
# (The code snippet is incomplete with a line: 'drop if municipality=="EMILIANO ZAPATA" =13011 if municipality =="ATLAPEXCO"'
# It seems a formatting error occurred in the original code. We will ignore that line as it’s broken.
# We’ll follow the pattern of assigning uniqueid based on municipality.)

unique_ids <- c("ACATLAN"=13001,"ACAXOCHITLAN"=13002,"ACTOPAN"=13003,
                "AGUA BLANCA DE ITURBIDE"=13004,"AJACUBA"=13005,"ALFAJAYUCAN"=13006,"ALMOLOYA"=13007,"APAN"=13008,
                "ATITALAQUIA"=13010,
                "ATLAPEXCO"=13011,"ATOTONILCO DE TULA"=13013,"ATOTONILCO EL GRANDE"=13012,
                "CALNALI"=13014,"CARDONAL"=13015,"CHAPANTONGO"=13017,"CHAPULHUACAN"=13018,
                "CHILCUAUTLA"=13019,"CUAUTEPEC DE HINOJOSA"=13016,"EL ARENAL"=13009,"ELOXOCHITLAN"=13020,
                "EPAZOYUCAN"=13022,"FRANCISCO I. MADERO"=13023,"HUASCA DE OCAMPO"=13024,"HUAUTLA"=13025,
                "HUEHUETLA"=13027,"HUEJUTLA DE REYES"=13028,"HUICHAPAN"=13029,"IXMIQUILPAN"=13030,
                "JACALA DE LEDEZMA"=13031,"JALTOCAN"=13032,"JUAREZ HIDALGO"=13033,"LA MISION"=13040,
                "LOLOTLA"=13034,"METEPEC"=13035,"METZTITLAN"=13037,"MINERAL DE LA REFORMA"=13051,
                "MINERAL DEL CHICO"=13038,"MINERAL DEL MONTE"=13039,"MIXQUIAHUALA DE JUAREZ"=13041,
                "MOLANGO DE ESCAMILLA"=13042,"NICOLAS DE FLORES"=13043,"NOPALA DE VILLAGRAN"=13044,
                "OMITLAN DE JUAREZ"=13045,"PACHUCA DE SOTO"=13048,"PACULA"=13047,"PISAFLORES"=13049,
                "PROGRESO DE OBREGON"=13050,"SAN AGUSTIN METZQUITITLAN"=13036,"SAN AGUSTIN TLAXIACA"=13052,
                "SAN BARTOLO TUTOTEPEC"=13053,"SAN FELIPE ORIZATLAN"=13046,"SAN SALVADOR"=13054,
                "SANTIAGO DE ANAYA"=13055,"SANTIAGO TULANTEPEC DE LUGO GU"=13056,"SINGUILUCAN"=13057,
                "TASQUILLO"=13058,"TECOZAUTLA"=13059,"TENANGO DE DORIA"=13060,"TEPEAPULCO"=13061,
                "TEPEHUACAN DE GUERRERO"=13062,"TEPEJI DEL RIO DE OCAMPO"=13063,"TEPETITLAN"=13064,
                "TETEPANGO"=13065,"TEZONTEPEC DE ALDAMA"=13067,"TIANGUISTENGO"=13068,"TIZAYUCA"=13069,
                "TLAHUELILPAN"=13070,"TLAHUILTEPA"=13071,"TLANALAPA"=13072,"TLANCHINOL"=13073,
                "TLAXCOAPAN"=13074,"TOLCAYUCA"=13075,"TULA DE ALLENDE"=13076,"TULANCINGO DE BRAVO"=13077,
                "VILLA DE TEZONTEPEC"=13066,"XOCHIATIPAN"=13078,"XOCHICOATLAN"=13079,"YAHUALICA"=13080,
                "ZACUALTIPAN DE ANGELES"=13081,"ZAPOTLAN DE JUAREZ"=13082,"ZEMPOALA"=13083,"ZIMAPAN"=13084)

data_2008 <- data_2008 %>%
  mutate(uniqueid = if_else(municipality %in% names(unique_ids), unique_ids[municipality], uniqueid))

###########################################
### Compute Valid Votes
###########################################
# egen valid = rowtotal(PAN PRI PRD PT PVEM PC PSD PANAL PRI_PANAL PRD_PT)
valid_parties <- c("PAN","PRI","PRD","PT","PVEM","PC","PSD","PANAL","PRI_PANAL","PRD_PT")

data_2008 <- data_2008 %>%
  mutate(valid = rowSums(across(all_of(valid_parties)), na.rm = TRUE),
         turnout = total/listanominal)


###########################################
### Add Year and Month
###########################################
data_2008 <- data_2008 %>%
  mutate(year = 2008,
         month = "November")

###########################################
### Sort by Section and Save
###########################################
data_2008 <- data_2008 %>%
  arrange(section)

###########################################
### Step 1: Import Excel
###########################################
data_2011 <- read_excel("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2011/Ayu_Seccion_2011.xlsx", sheet = "Ayu_Seccion_2011")

###########################################
### Step 2: Rename and Initial Drops
###########################################
# rename NOMBRE municipality
data_2011 <- data_2011 %>%
  rename(municipality = NOMBRE)

# drop if CASILLAS=="CASILLAS"
# drop if LISTANOMINAL=="NOMINAL"
# drop if IdMun==""
data_2011 <- data_2011 %>%
  filter(CASILLAS != "CASILLAS",
         `LISTA NOMINAL` != "LISTADO",
         IdMun != "IdMun")

# replace CASILLAS= subinstr(CASILLAS, "B", "", .)
data_2011$CASILLAS <- str_replace_all(data_2011$CASILLAS, "B", "")

# forvalues i=1(1)10 remove C`i' and X`i'
for(i in 1:10) {
  data_2011$CASILLAS <- str_replace_all(data_2011$CASILLAS, paste0("C", i), "")
  data_2011$CASILLAS <- str_replace_all(data_2011$CASILLAS, paste0("X", i), "")
}

# destring CASILLAS, replace
data_2011 <- data_2011 %>%
  mutate(CASILLAS = as.numeric(CASILLAS))

# rename CASILLAS section
data_2011 <- data_2011 %>%
  rename(section = CASILLAS)

# rename LISTANOMINAL listanominal
# rename TOTAL total
data_2011 <- data_2011 %>%
  rename(listanominal = `LISTA NOMINAL`,
         total = TOTAL)

###########################################
### Step 3: Clean Party Variables
###########################################
# foreach var in PAN PRI PRD PT PVEM PC PANAL PAN_PRD PT_PC PT_PC PRI_PVEM {
# replace var= subinstr(var, "N/R", "", .)
party_vars <- c("PAN","PRI","PRD","PT","PVEM","PC","PANAL","PAN_PRD","PT_PC","PRI_PVEM")

for(v in party_vars){
  data_2011[[v]] <- str_replace_all(data_2011[[v]], "N/R", "")
}

# drop if strpos(PC, "CASILLA ANULADA")>0 or PVEM similar
data_2011 <- data_2011 %>%
  filter(!str_detect(PC, "CASILLA ANULADA"),
         !str_detect(PVEM, "CASILLA ANULADA"))

# destring all
data_2011 <- data_2011 %>%
  mutate(across(everything(), ~if_else(. == "", NA_character_, as.character(.)))) %>%
  mutate(across(c(listanominal, total, all_of(party_vars)), ~as.numeric(.)))

###########################################
### Step 4: Drops and Filters
###########################################
# drop if municipality=="" & section==.
data_2011 <- data_2011 %>%
  filter(!(municipality == "" & is.na(section)))

# drop if total==. | total==0
data_2011 <- data_2011 %>%
  filter(!is.na(total) & total != 0)

###########################################
### Step 5: Collapse (Sum)
###########################################
# collapse (sum) listanominal - PRI_PVEM total, by(municipality section)
# Identify columns from listanominal through PRI_PVEM
collapse_vars <- c("listanominal", party_vars, "total")
data_2011 <- data_2011 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(collapse_vars), sum, na.rm = TRUE), .groups="drop")

# drop if municipality=="SANTIAGO TULANTEPEC" | municipality=="XOCHICOATLAN"
data_2011 <- data_2011 %>%
  filter(!(municipality %in% c("SANTIAGO TULANTEPEC","XOCHICOATLAN")))

###########################################
### Compute Turnout
###########################################
data_2011 <- data_2011 %>%
  mutate(turnout = total/listanominal)

###########################################
### Unique ID Assignments
###########################################
data_2011 <- data_2011 %>%
  mutate(uniqueid = NA_real_)

# Assign uniqueid based on municipality
unique_ids <- c("ACATLAN"=13001,"ACAXOCHITLAN"=13002,"ACTOPAN"=13003,"AGUA BLANCA DE ITURBIDE"=13004,
                "AJACUBA"=13005,"ALFAJAYUCAN"=13006,"ALMOLOYA"=13007,"APAN"=13008,"ATITALAQUIA"=13010,
                "ATLAPEXCO"=13011,"ATOTONILCO DE TULA"=13013,"ATOTONILCO EL GRANDE"=13012,"CALNALI"=13014,
                "CARDONAL"=13015,"CHAPANTONGO"=13017,"CHAPULHUACAN"=13018,"CHILCUAUTLA"=13019,
                "CUAUTEPEC DE HINOJOS"=13016,"EL ARENAL"=13009,"ELOXOCHITLAN"=13020,"EMILIANO ZAPATA"=13021,
                "EPAZOYUCAN"=13022,"FRANCISCO I. MADERO"=13023,"HUASCA DE OCAMPO"=13024,"HUAUTLA"=13025,
                "HUAZALINGO"=13026,"HUEHUETLA"=13027,"HUEJUTLA DE REYES"=13028,"HUICHAPAN"=13029,
                "IXMIQUILPAN"=13030,"JACALA DE LEDEZMA"=13031,"JALTOCAN"=13032,"JUAREZ HIDALGO"=13033,
                "LA MISION"=13040,"LOLOTLA"=13034,"METEPEC"=13035,"METZTITLAN"=13037,"MINERAL DE LA REFORM"=13051,
                "MINERAL DEL CHICO"=13038,"MINERAL DEL MONTE"=13039,"MIXQUIAHUALA DE JUAR"=13041,
                "MOLANGO DE ESCAMILLA"=13042,"NICOLAS DE FLORES"=13043,"NICOLAS  FLORES"=13043,
                "NOPALA DE VILLAGRAN"=13044,"OMITLAN DE JUAREZ"=13045,"PACHUCA DE SOTO"=13048,"PACULA"=13047,
                "PISAFLORES"=13049,"PROGRESO DE OBREGON"=13050,"SAN AGUSTIN METZQUITITLAN"=13036,
                "SAN AGUSTIN TLAXIACA"=13052,"SAN BARTOLO TUTOTEPEC"=13053,"SAN FELIPE ORIZATLAN"=13046,
                "SAN SALVADOR"=13054,"SANTIAGO DE ANAYA"=13055,"SANTIAGO TULANTEPEC DE LUGO GU"=13056,
                "SANTIAGO TULANTEPEC"=13056,"SINGUILUCAN"=13057,"TASQUILLO"=13058,"TECOZAUTLA"=13059,
                "TENANGO DE DORIA"=13060,"TEPEAPULCO"=13061,"TEPEHUACAN DE GUERRERO"=13062,
                "TEPEHUACAN DE GUERR"=13062,"TEPEJI DEL RIO DE OCAMPO"=13063,"TEPETITLAN"=13064,"TETEPANGO"=13065,
                "TEZONTEPEC DE ALDAMA"=13067,"TIANGUISTENGO"=13068,"TIZAYUCA"=13069,"TLAHUELILPAN"=13070,
                "TLAHUILTEPA"=13071,"TLANALAPA"=13072,"TLANCHINOL"=13073,"TLAXCOAPAN"=13074,
                "TOLCAYUCA"=13075,"TULA DE ALLENDE"=13076,"TULANCINGO DE BRAVO"=13077,"VILLA DE TEZONTEPEC"=13066,
                "XOCHIATIPAN"=13078,"XOCHICOATLAN"=13079,"YAHUALICA"=13080,"ZACUALTIPAN DE ANGEL"=13081,
                "ZAPOTLAN DE JUAREZ"=13082,"ZEMPOALA"=13083,"ZIMAPAN"=13084)

data_2011 <- data_2011 %>%
  mutate(uniqueid = if_else(municipality %in% names(unique_ids), unique_ids[municipality], uniqueid))

###########################################
### Compute Valid Votes
###########################################
# egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAN_PRD PT_PC PRI_PVEM)
valid_parties <- c("PAN","PRI","PRD","PT","PVEM","PC","PANAL","PAN_PRD","PT_PC","PRI_PVEM")
data_2011 <- data_2011 %>%
  mutate(valid = rowSums(across(all_of(valid_parties)), na.rm = TRUE))


###########################################
### Add Year and Month
###########################################
data_2011 <- data_2011 %>%
  mutate(year = 2011,
         month = "Julio",
         section = as.numeric(section))

###########################################
### Sort by Section and Save
###########################################
data_2011 <- data_2011 %>%
  arrange(section)

###########################################
### Step 1: We import All Sheets from Excel and Save as Individual Files in dta
###########################################

###########################################
### Step 2: Append All Municipality Files
###########################################

# Paths
input_file <- "../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2016/Ayuntamientos_Hgo_2016.xlsx"
output_dir <- "../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/Other"

# Create the output directory if it doesn't exist
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get all sheet names
sheets <- excel_sheets(input_file)

# Loop through each sheet and write to CSV
for (sheetname in sheets) {
  # Read the sheet as text columns
  df <- read_excel(input_file, sheet = sheetname, col_types = "text")
  
  # Replace empty strings with "0"
  df[df == ""] <- "0"
  
  # Construct a valid filename from the sheet name
  # Replace spaces and special chars with underscores
  csv_filename <- paste0(gsub("[^A-Za-z0-9_]", "_", sheetname), ".csv")
  
  # Write CSV
  write.csv(df, file.path(output_dir, csv_filename), row.names = FALSE)
}

# The code appends each municipality .dta:
# We'll just list them and append
files_to_append <- list.files(output_dir, pattern="_A.csv$", full.names=TRUE)

data_2016 <- files_to_append %>%
  lapply(read.csv) %>%
  bind_rows()

# replace Municipio conditions
data_2016 <- data_2016 %>%
  mutate(Municipio = case_when(
    Municipio=="Zapotlán De Juárez1619" | Municipio=="Zapotlán De Juárez1620" ~ "Zapotlán De Juárez",
    Municipio=="Tula De Allende " | Municipio=="Tula De Allende  " | Municipio=="Tula De Allende   " ~ "Tula De Allende",
    TRUE ~ Municipio
  ))

# municipality = upper(Municipio)
data_2016 <- data_2016 %>%
  mutate(municipality = toupper(Municipio))

# Assign uniqueid
data_2016 <- data_2016 %>%
  mutate(uniqueid = NA)

# Uniqueid replacements
unique_ids <- c("ACATLÁN"=13001,"ACAXOCHITLÁN"=13002,"ACTOPAN"=13003,"AGUA BLANCA DE ITURBIDE"=13004,
                "AJACUBA"=13005,"ALFAJAYUCAN"=13006,"ALMOLOYA"=13007,"APAN"=13008,"ATITALAQUIA"=13010,
                "ATLAPEXCO"=13011,"ATOTONILCO DE TULA"=13013,"ATOTONILCO EL GRANDE"=13012,"CALNALI"=13014,
                "CARDONAL"=13015,"CHAPANTONGO"=13017,"CHAPULHUACÁN"=13018,"CHILCUAUTLA"=13019,
                "CUAUTEPEC DE HINOJOSA"=13016,"EL ARENAL"=13009,"ELOXOCHITLÁN"=13020,"EMILIANO ZAPATA"=13021,
                "EPAZOYUCAN"=13022,"FRANCISCO I. MADERO"=13023,"HUASCA DE OCAMPO"=13024,"HUAUTLA"=13025,
                "HUAZALINGO"=13026,"HUEHUETLA"=13027,"HUEJUTLA DE REYES"=13028,"HUICHAPAN"=13029,
                "IXMIQUILPAN"=13030,"JACALA DE LEDEZMA"=13031,"JALTOCAN"=13032,"JUÁREZ HIDALGO"=13033,
                "LA MISIÓN"=13040,"LOLOTLA"=13034,"METEPEC"=13035,"METZTITLÁN"=13037,
                "MINERAL DE LA REFORMA"=13051,"MINERAL DEL CHICO"=13038,"MINERAL DEL MONTE"=13039,
                "MIXQUIAHUALA DE JUÁREZ"=13041,"MOLANGO DE ESCAMILLA"=13042,"NICOLAS DE FLORES"=13043,
                "NICOLÁS FLORES"=13043,"NOPALA DE VILLAGRÁN"=13044,"OMITLÁN DE JUÁREZ"=13045,
                "PACHUCA DE SOTO"=13048,"PACULA"=13047,"PISAFLORES"=13049,"PROGRESO DE OBREGÓN"=13050,
                "SAN AGUSTÍN METZQUITITLÁN"=13036,"SAN AGUSTÍN TLAXIACA"=13052,"SAN BARTOLO TUTOTEPEC"=13053,
                "SAN FELIPE ORIZATLÁN"=13046,"SAN SALVADOR"=13054,"SANTIAGO DE ANAYA"=13055,
                "SANTIAGO TULANTEPEC DE LUGO GUERRERO"=13056,"SANTIAGO TULANTEPEC"=13056,"SINGUILUCAN"=13057,
                "TASQUILLO"=13058,"TECOZAUTLA"=13059,"TENANGO DE DORIA"=13060,"TEPEAPULCO"=13061,
                "TEPEHUACÁN DE GUERRERO"=13062,"TEPEJI DEL RÍO DE OCAMPO"=13063,"TEPETITLÁN"=13064,
                "TETEPANGO"=13065,"TEZONTEPEC DE ALDAMA"=13067,"TIANGUISTENGO"=13068,"TIZAYUCA"=13069,
                "TLAHUELILPAN"=13070,"TLAHUILTEPA"=13071,"TLANALAPA"=13072,"TLANCHINOL"=13073,
                "TLAXCOAPAN"=13074,"TOLCAYUCA"=13075,"TULA DE ALLENDE"=13076,"TULANCINGO DE BRAVO"=13077,
                "VILLA DE TEZONTEPEC"=13066,"XOCHIATIPAN"=13078,"XOCHICOATLÁN"=13079,"YAHUALICA"=13080,
                "ZACUALTIPÁN DE ÁNGELES"=13081,"ZAPOTLÁN DE JUÁREZ"=13082,"ZEMPOALA"=13083,"ZIMAPÁN"=13084)

data_2016 <- data_2016 %>%
  mutate(uniqueid = if_else(municipality %in% names(unique_ids), unique_ids[municipality], uniqueid))

# destring all numeric vars
# Already "0" and numeric: convert what we can
data_2016 <- data_2016 %>%
  mutate(across(everything(), ~ifelse(.=="",NA,.))) %>%
  mutate(across(where(is.character), ~type.convert(., as.is=TRUE)))

# Renaming columns by removing dots and spaces
colnames(data_2016) <- gsub("[\\. ]", "", colnames(data_2016))

# rename ENRIQUEPACHECOLOPEZ CI_3, etc.
# Set CI_3=0 if CI_3==.
if("ENRIQUEPACHECOLOPEZ" %in% names(data_2016)) {
  data_2016 <- data_2016 %>%
    rename(CI_3 = ENRIQUEPACHECOLOPEZ) %>%
    mutate(CI_3 = if_else(is.na(CI_3),0,CI_3))
}

# For the listed independent candidates set them to 0 if NA
indep_vars_set1 <- c("JGUADALUPESANTIAGOAGUILAR","ANDRÉSGILDARDOOCADIZIBARRA","JOSÉLUISARROYAVEHERNÁNDEZ","LUCIACARIÑOVITEHERNANDEZ","JUANLUISHERNÁNDEZCÁZARES","MARIOJARILLOHERNÁNDEZ","JAVIERGÓMEZPICHARDO")
indep_vars_set1 <- indep_vars_set1[indep_vars_set1 %in% names(data_2016)]
data_2016[indep_vars_set1] <- lapply(data_2016[indep_vars_set1], function(x) ifelse(is.na(x),0,x))

data_2016 <- data_2016 %>%
  mutate(CI_2 = rowSums(across(all_of(indep_vars_set1)),na.rm=TRUE)) %>%
  select(-all_of(indep_vars_set1))

# For PRI: Create PRI and PVEM and PRD from their components
if("PartidoRevolucionarioInstitutcional" %in% names(data_2016)){
  data_2016 <- data_2016 %>%
    mutate(PartidoRevolucionarioInstitutcional = ifelse(is.na(PartidoRevolucionarioInstitutcional),0,PartidoRevolucionarioInstitucional),
           PartidoRevolucionarioInstitucional = ifelse(is.na(PartidoRevolucionarioInstitucional),0,PartidoRevolucionarioInstitucional),
           PRI = PartidoRevolucionarioInstitutcional+PartidoRevolucionarioInstitucional) %>%
    select(-PartidoRevolucionarioInstitutcional,-PartidoRevolucionarioInstitucional)
}

if("PartidoVerde" %in% names(data_2016)){
  data_2016 <- data_2016 %>%
    mutate(PartidoVerde = ifelse(is.na(PartidoVerde),0,PartidoVerde),
           PartidoVerdeEcologistadeMéxico = ifelse(is.na(PartidoVerdeEcologistadeMéxico),0,PartidoVerdeEcologistadeMéxico),
           PVEM = PartidoVerde + PartidoVerdeEcologistadeMéxico) %>%
    select(-PartidoVerde,-PartidoVerdeEcologistadeMéxico)
}

if("PartidodelaRevolcuiónDemocrática" %in% names(data_2016)){
  data_2016 <- data_2016 %>%
    mutate(PartidodelaRevolcuiónDemocrática = ifelse(is.na(PartidodelaRevolcuiónDemocrática),0,PartidodelaRevolcuiónDemocrática),
           PartidodelaRevoluciónDemocrática= ifelse(is.na(PartidodelaRevoluciónDemocrática),0,PartidodelaRevoluciónDemocrática),
           PRD = PartidodelaRevolcuiónDemocrática + PartidodelaRevoluciónDemocrática) %>%
    select(-PartidodelaRevolcuiónDemocrática,-PartidodelaRevoluciónDemocrática)
}


# rename and set to 0 if NA
rename_map <- c("PartidoAcciónNacional"="PAN",
                "PartidodelTrabajo"="PT",
                "PartidoMovimientoCiudadano"="MC",
                "PartidoNuevaAlianza"="PANAL",
                "PartidoMorena"="MORENA",
                "PartidoEncuentroSocial"="PES")

for(old in names(rename_map)){
  if(old %in% names(data_2016)){
    data_2016 <- data_2016 %>%
      rename(!!rename_map[old] := all_of(old)) %>%
      mutate(!!rename_map[old] := ifelse(is.na(.data[[rename_map[old]]]),0,.data[[rename_map[old]]]))
  }
}

# CI_1a, CI_1b, CI_1c sets
set_CI_1a <- c("LUCINAMARÍACONCEPCIÓNANTÚNEZS","NÉSTORMEJÍANERI","ARMANDOSALOMÓNCAMARGO","ÁNGELCRISTIANSOLÍSSOTO","HÉCTORHERRERAOCAMPO","ADÁNRÍOSESTRADA","RUFINOMONTIELESCUDERO")
set_CI_1a <- set_CI_1a[set_CI_1a %in% names(data_2016)]
data_2016[set_CI_1a] <- lapply(data_2016[set_CI_1a], function(x) ifelse(is.na(x),0,x))
data_2016 <- data_2016 %>%
  mutate(CI_1a = rowSums(across(all_of(set_CI_1a)),na.rm=TRUE)) %>%
  select(-all_of(set_CI_1a))

set_CI_1b <- c("JUANCARLOSSÁNCHEZRIVERA","FERNANDOCERÓNSOSA","VÍCTORAUGUSTOSÁNCHEZDELGADO","HÉCTORGARCÍABRAVO","JOSÉLUISMUÑOZSOTO","FORTUNATOGONZÁLEZISLAS","HUMBERTOENDONIOSALINAS","NOLDYNTORRESMARTÍNEZ","FRANCISCOMACIASBELTRAN")
set_CI_1b <- set_CI_1b[set_CI_1b %in% names(data_2016)]
data_2016[set_CI_1b] <- lapply(data_2016[set_CI_1b], function(x) ifelse(is.na(x),0,x))
data_2016 <- data_2016 %>%
  mutate(CI_1b = rowSums(across(all_of(set_CI_1b)),na.rm=TRUE)) %>%
  select(-all_of(set_CI_1b))

set_CI_1c <- c("MINERVADURANVIVAR","JULIORAMÓNMENCHACASALAZAR","HEBLENANGELESHERNÁNDEZ","LISSETMARCELINOTOVAR","MARCOANTONIOOLVERAJIMÉNEZ","JAVIERTELLEZMENDOZA","MARCOSGONZALEZTREJO","RAÚLGONZÁLEZGARCÍA","JESÚSORTIZCANO","MARÍAELENAJIMÉNEZGUZMÁN","FRANCISCOJAVIERHERNANDEZCORTE","CRISOFORORUANOVITE","ALEJANDROAHUEDSARQUIS")
set_CI_1c <- set_CI_1c[set_CI_1c %in% names(data_2016)]
data_2016[set_CI_1c] <- lapply(data_2016[set_CI_1c], function(x) ifelse(is.na(x),0,x))
data_2016 <- data_2016 %>%
  mutate(CI_1c = rowSums(across(all_of(set_CI_1c)),na.rm=TRUE)) %>%
  select(-all_of(set_CI_1c))

if("CI" %in% names(data_2016)){
  data_2016$CI <- ifelse(is.na(data_2016$CI),0,data_2016$CI)
} else {
  data_2016$CI <- 0
}

data_2016 <- data_2016 %>%
  mutate(CI_1 = CI_1a + CI_1b + CI_1c + CI) %>%
  select(-CI_1a,-CI_1b,-CI_1c,-CI)
names(data_2016)

# PRI_PVEM_PANAL creation:
if("PRIVERDENUEVAALIANZA" %in% names(data_2016)){
  data_2016 <- data_2016 %>%
    mutate(PRIVERDENUEVAALIANZA = ifelse(is.na(PRIVERDENUEVAALIANZA),0,PRIVERDENUEVAALIANZA),
           PRIVERDE = ifelse(is.na(PRIVERDE),0,PRIVERDE),
           PRINUEVAALIANZA = ifelse(is.na(PRINUEVAALIANZA),0,PRINUEVAALIANZA),
           VERDENUEVAALIANZA = ifelse(is.na(VERDENUEVAALIANZA),0,VERDENUEVAALIANZA),
           PANAL = ifelse(is.na(PANAL),0,PANAL)) %>%
    mutate(PRI_PVEM_PANAL = PRIVERDENUEVAALIANZA,
           PRI_PVEM_PANAL = ifelse(PRI_PVEM_PANAL!=0, PRI_PVEM_PANAL + PRIVERDE + PRINUEVAALIANZA + VERDENUEVAALIANZA + PRI + PVEM + PANAL, PRI_PVEM_PANAL)) %>%
    mutate(PRI = ifelse(PRI_PVEM_PANAL!=0,NA,PRI),
           PVEM = ifelse(PRI_PVEM_PANAL!=0,NA,PVEM),
           PANAL= ifelse(PRI_PVEM_PANAL!=0,NA,PANAL)) %>%
    select(-PRIVERDE,-PRINUEVAALIANZA,-VERDENUEVAALIANZA,-PRIVERDENUEVAALIANZA)
}
names(data_2016)
# no_reg and nulo
data_2016 <- data_2016 %>%
  select(c(municipality,uniqueid,Sección,
           PAN,PT,MC,PANAL,MORENA,PES,PRI,
           PVEM,PRD,PRI_PVEM_PANAL,
           CI_1,CI_2,CI_3,NOREGISTRADO,NULO,Nulos,VOTOSNULOS))

# collapse (sum) after these transformations is complicated due to the complexity of the code. 
# According to instructions, we omit mun_ and rowranks. The code does a final collapse after merges. We'll still do final collapse.

# The code merges LN data:
ln2016 <- read_dta("../../../Data/Raw Electoral Data//Listas Nominales/LN 2012-2019/2016/LN2016.dta") %>%
  filter(entidad==13 & month==5) %>%
  mutate(uniqueid = entidad*1000+municipio) %>%
  filter(seccion!=0)

ln2016 <- ln2016 %>%
  select(uniqueid, seccion, lista)%>%
  rename(section = seccion)

data_2016 <- data_2016 %>%
  rename(section = Sección)

summary(data_2016)

data_2016 <- data_2016 %>%
  group_by(municipality,uniqueid,section) %>% 
  summarise(across(c(PAN:VOTOSNULOS), sum, na.rm=TRUE), .groups="drop")

# rename lista to listanominal after merge
data_2016 <- data_2016 %>%
  left_join(ln2016, by=c("uniqueid","section")) %>%
  mutate(listanominal = lista) %>%
  select(-lista) 

data_2016 <- data_2016 %>%
  mutate(year=2016, 
         month="July") %>% 
  rowwise() %>%
  mutate(total = sum(c_across(c(PAN:VOTOSNULOS)),na.rm=TRUE)) %>%
  mutate(turnout = total/listanominal) %>% 
  select(-c(NOREGISTRADO,NULO,Nulos,VOTOSNULOS))

# Next, read Ayu_Seccion_2016_Extraordinario.csv
extra_data <- read_delim("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2016/Ayu_Seccion_2016_Extraordinario.csv", delim=",")
# Convert column names to lowercase
extra_data <- extra_data %>% rename_with(tolower)

extra_data <- extra_data %>%
  rename(section=seccion, municipality=municipio, total=total_votos, listanominal=lista_nominal,
         PAN=pan, PRI=pri, PRD=prd, PVEM=pvem, PT=pt, MC=mc) %>%
  filter(!is.na(total) & total!=0) %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN,PRI,PRD,PVEM,PT,MC,total,listanominal), sum, na.rm=TRUE), .groups="drop") %>%
  mutate(turnout=total/listanominal,
         uniqueid=13045,
         municipality = "Omitlán de Juárez EXTRAORDINARIO") %>%
  # Compute valid
  rowwise() %>%
  mutate(valid = sum(c_across(c(PAN,PRI,PRD,PVEM,PT,MC)),na.rm=TRUE)) %>%
  ungroup()

# year month
extra_data <- extra_data %>%
  mutate(year=2016, month="December")

#####################################
### PROCESSING DATA FOR 2020 -------
#####################################

# Load the 2020 dataset from the CSV
data_2020 <- read_csv("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2020/2020_AYUN_HGO_CASILLAS.csv") %>% 
  dplyr::filter(MUNICIPIO != "ACAXOCHITLAN" & MUNICIPIO != "IXMIQUILPAN")

ln_ext <- read_csv("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2020/2020_AYUN_HGO_CASILLAS.csv") %>% 
  dplyr::filter(MUNICIPIO %in% c("ACAXOCHITLAN","IXMIQUILPAN")) %>% 
  select(MUNICIPIO, SECCION, CASILLA, LISTA_NOMINAL)

data_acaxoch <- read_excel("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2021/ext_2021.xlsx", sheet = "ACAXOCHITLAN", skip = 2) %>% 
  mutate(MUNICIPIO = "ACAXOCHITLAN")

data_ixmi <- read_excel("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2021/ext_2021.xlsx", sheet = "IXMIQUILPAN", skip = 1) %>% 
  mutate(MUNICIPIO = "IXMIQUILPAN")

data_ext <- bind_rows(data_acaxoch, data_ixmi) %>% 
  rename(SECCION = "SECCIÓN") %>% 
  mutate(
    valid = sum(c_across(c(PAN:MAS,PT:MORENA_PANAL)), na.rm = TRUE),
    CASILLA_RECODED = case_when(
      CASILLA == "BASICA" ~ "B",
      CASILLA == "CONTIGUA 01" ~ "C01",
      CASILLA == "CONTIGUA 02" ~ "C02",
      CASILLA == "CONTIGUA 03" ~ "C03",
      CASILLA == "CONTIGUA 04" ~ "C04",
      CASILLA == "CONTIGUA 05" ~ "C05",
      CASILLA == "ESPECIAL 01" ~ "S01",
      CASILLA == "ESPECIAL 02" ~ "S02",
      CASILLA == "EXTRAORDINARIA 01" ~ "E01",
      CASILLA == "EXTRAORDINARIA 02" ~ "E02",
      CASILLA == "EXTRAORDINARIA 03" ~ "E03",
      CASILLA == "EXTRAORDINARIA 01 CONTIGUA 01" ~ "E01C01",
      CASILLA == "EXTRAORDINARIA 01 CONTIGUA 02" ~ "E01C02",
      CASILLA == "EXTRAORDINARIA 01 CONTIGUA 03" ~ "E01C03",
      CASILLA == "EXTRAORDINARIA 02 CONTIGUA 01" ~ "E02C01",
      CASILLA == "EXTRAORDINARIA 02 CONTIGUA 02" ~ "E02C02",
      CASILLA == "EXTRAORDINARIA 03 CONTIGUA 01" ~ "E03C01",
      TRUE ~ CASILLA  # Keep original if not matched
    )
  ) %>%
  select(-CASILLA) %>%
  rename(CASILLA = CASILLA_RECODED)

data_ext <- left_join(data_ext, ln_ext, by = c("MUNICIPIO","SECCION", "CASILLA"))

# Rename columns
data_2020 <- data_2020 %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL_VOTOS,
                valid = NUM_VOTOS_VALIDOS,
                no_reg = NUM_VOTOS_CAN_NREG,
                nulos = NUM_VOTOS_NULOS,
                PANAL = NVA_ALIANZA,
                MXH = MAS_X_HIDALGO) %>%
  rename_with(~ gsub("CAND_IND", "CI_", .x), starts_with("cand_ind")) %>%
  rename_with(~ str_replace_all(., "ESH", "PES")) %>% 
  dplyr::mutate(
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    section = as.numeric(section)
  )

data_ext <- data_ext %>%
  dplyr::rename(municipality = MUNICIPIO,
                section = SECCION,
                listanominal = LISTA_NOMINAL,
                total = TOTAL,
                no_reg = "NO REGISTRADO",
                nulos = "VOTOS NULOS",
                MXH = MAS) %>%
  dplyr::mutate(
    section = as.numeric(section)
  ) %>% 
  filter(section > 0)

data_2020 <- bind_rows(data_2020, data_ext)

# Assign uniqueids
data_2020 <- data_2020 %>%
  mutate(uniqueid = case_when(
    municipality == "ACATLAN" ~ 13001,
    municipality == "ACAXOCHITLAN" ~ 13002,
    municipality == "ACTOPAN" ~ 13003,
    municipality == "AGUA BLANCA DE ITURBIDE" ~ 13004,
    municipality == "AJACUBA" ~ 13005,
    municipality == "ALFAJAYUCAN" ~ 13006,
    municipality == "ALMOLOYA" ~ 13007,
    municipality == "APAN" ~ 13008,
    municipality == "ATITALAQUIA" ~ 13010,
    municipality == "ATLAPEXCO" ~ 13011,
    municipality == "ATOTONILCO DE TULA" ~ 13013,
    municipality == "ATOTONILCO EL GRANDE" ~ 13012,
    municipality == "CALNALI" ~ 13014,
    municipality == "CARDONAL" ~ 13015,
    municipality == "CHAPANTONGO" ~ 13017,
    municipality == "CHAPULHUACAN" ~ 13018,
    municipality == "CHILCUAUTLA" ~ 13019,
    municipality == "CUAUTEPEC DE HINOJOSA" ~ 13016,
    municipality == "EL ARENAL" ~ 13009,
    municipality == "ELOXOCHITLAN" ~ 13020,
    municipality == "EMILIANO ZAPATA" ~ 13021,
    municipality == "EPAZOYUCAN" ~ 13022,
    municipality == "FRANCISCO I. MADERO" ~ 13023,
    municipality == "HUASCA DE OCAMPO" ~ 13024,
    municipality == "HUAUTLA" ~ 13025,
    municipality == "HUAZALINGO" ~ 13026,
    municipality == "HUEHUETLA" ~ 13027,
    municipality == "HUEJUTLA DE REYES" ~ 13028,
    municipality == "HUICHAPAN" ~ 13029,
    municipality == "IXMIQUILPAN" ~ 13030,
    municipality == "JACALA DE LEDEZMA" ~ 13031,
    municipality == "JALTOCAN" ~ 13032,
    municipality == "JUAREZ HIDALGO" ~ 13033,
    municipality == "LA MISION" ~ 13040,
    municipality == "LOLOTLA" ~ 13034,
    municipality == "METEPEC" ~ 13035,
    municipality == "METZTITLAN" ~ 13037,
    municipality == "MINERAL DE LA REFORMA" ~ 13051,
    municipality == "MINERAL DEL CHICO" ~ 13038,
    municipality == "MINERAL DEL MONTE" ~ 13039,
    municipality == "MIXQUIAHUALA DE JUAREZ" ~ 13041,
    municipality == "MOLANGO DE ESCAMILLA" ~ 13042,
    municipality == "NICOLAS FLORES" ~ 13043,
    municipality == "NOPALA DE VILLAGRAN" ~ 13044,
    municipality == "OMITLAN DE JUAREZ" ~ 13045,
    municipality == "PACHUCA DE SOTO" ~ 13048,
    municipality == "PACULA" ~ 13047,
    municipality == "PISAFLORES" ~ 13049,
    municipality == "PROGRESO DE OBREGON" ~ 13050,
    municipality == "SAN AGUSTIN METZQUITITLAN" ~ 13036,
    municipality == "SAN AGUSTIN TLAXIACA" ~ 13052,
    municipality == "SAN BARTOLO TUTOTEPEC" ~ 13053,
    municipality == "SAN FELIPE ORIZATLAN" ~ 13046,
    municipality == "SAN SALVADOR" ~ 13054,
    municipality == "SANTIAGO DE ANAYA" ~ 13055,
    municipality == "SANTIAGO TULANTEPEC DE LUGO GUERRERO" ~ 13056,
    municipality == "SINGUILUCAN" ~ 13057,
    municipality == "TASQUILLO" ~ 13058,
    municipality == "TECOZAUTLA" ~ 13059,
    municipality == "TENANGO DE DORIA" ~ 13060,
    municipality == "TEPEAPULCO" ~ 13061,
    municipality == "TEPEHUACAN DE GUERRERO" ~ 13062,
    municipality == "TEPEJI DEL RIO DE OCAMPO" ~ 13063,
    municipality == "TEPETITLAN" ~ 13064,
    municipality == "TETEPANGO" ~ 13065,
    municipality == "TEZONTEPEC DE ALDAMA" ~ 13067,
    municipality == "TIANGUISTENGO" ~ 13068,
    municipality == "TIZAYUCA" ~ 13069,
    municipality == "TLAHUELILPAN" ~ 13070,
    municipality == "TLAHUILTEPA" ~ 13071,
    municipality == "TLANALAPA" ~ 13072,
    municipality == "TLANCHINOL" ~ 13073,
    municipality == "TLAXCOAPAN" ~ 13074,
    municipality == "TOLCAYUCA" ~ 13075,
    municipality == "TULA DE ALLENDE" ~ 13076,
    municipality == "TULANCINGO DE BRAVO" ~ 13077,
    municipality == "VILLA DE TEZONTEPEC" ~ 13066,
    municipality == "XOCHIATIPAN" ~ 13078,
    municipality == "XOCHICOATLAN" ~ 13079,
    municipality == "YAHUALICA" ~ 13080,
    municipality == "ZACUALTIPAN DE ANGELES" ~ 13081,
    municipality == "ZAPOTLAN DE JUAREZ" ~ 13082,
    municipality == "ZEMPOALA" ~ 13083,
    municipality == "ZIMAPAN" ~ 13084,
    TRUE ~ NA_integer_
  ))

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2020 <- data_2020 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:listanominal, PT_PVEM_MORENA_PANAL:MORENA_PANAL), 
                          sum, na.rm = TRUE))

# Calculate valid votes and final details
collapsed_2020 <- collapsed_2020 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    year = case_when(
      municipality %in% c("ACAXOCHITLAN","IXMIQUILPAN") ~ 2021,
      TRUE ~ 2020
    ),
    month = case_when(
      municipality %in% c("ACAXOCHITLAN","IXMIQUILPAN") ~ "June",
      TRUE ~ "October"
    )
  )

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 13) %>% 
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
collapsed_2020 <- process_coalitions(collapsed_2020, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)


#####################################
### PROCESSING DATA FOR 2024 -------
#####################################

# Process data into a single file
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(fs)
library(writexl)

# Define the base path
base_path <- "../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2024"

# Strategy 1: Recursive file discovery and processing
process_hidalgo_elections <- function(base_path) {
  
  # Step 1: Find all Excel files recursively (exclude temp files)
  excel_files <- dir_ls(base_path, 
                        recurse = TRUE, 
                        regexp = "\\.(xlsx|xls)$", 
                        type = "file") %>%
    # Filter out temporary Excel files (start with ~$)
    .[!str_detect(path_file(.), "^~\\$")]
  
  cat("Found", length(excel_files), "Excel files\n")
  
  # Step 2: Extract metadata from file paths
  file_info <- tibble(
    file_path = excel_files,
    district = str_extract(file_path, "Distrito \\d+"),
    filename = path_file(file_path),
    # Extract municipality name from filename (remove numbers, underscores, extensions)
    municipality_raw = str_remove(filename, "\\.(xlsx|xls)$"),
    municipality_clean = str_remove_all(municipality_raw, "^\\d+_?") %>% 
      str_to_upper() %>%
      str_trim()
  )
  
  # Step 3: Preview the file structure
  cat("\nFile structure preview:\n")
  print(file_info %>% select(district, filename, municipality_clean) %>% head(10))
  
  # Step 4: Function to safely read each Excel file
  read_excel_safely <- function(file_path, district, municipality) {
    tryCatch({
      
      # Read the Excel file - you may need to adjust sheet names or ranges
      data <- read_excel(file_path, 
                         sheet = 1,  # Adjust if needed
                         skip = 0,   # Adjust if there are header rows to skip
                         col_types = "text")  # Read everything as text first
      
      # Clean up special characters in municipality name
      municipality_clean <- municipality %>%
        # Fix encoding issues with special characters
        str_replace_all("†", "Á") %>%
        str_replace_all("¢", "Ó") %>%
        str_replace_all("‡", "É") %>%
        str_replace_all("¡", "Í") %>%
        str_replace_all("£", "Ú") %>%
        str_replace_all("¤", "Ñ")
      
      # Add metadata columns
      data <- data %>%
        mutate(
          source_file = basename(file_path),
          district = district,
          municipality = municipality_clean,
          .before = 1
        )
      
      return(data)
      
    }, error = function(e) {
      cat("Error reading file:", file_path, "\n")
      cat("Error message:", e$message, "\n")
      return(NULL)
    })
  }
  
  # Step 5: Process all files
  cat("\nProcessing files...\n")
  
  all_data <- file_info %>%
    mutate(
      data = pmap(list(file_path, district, municipality_clean), 
                  read_excel_safely)
    ) %>%
    filter(!map_lgl(data, is.null))  # Remove failed reads
  
  # Step 6: Combine all data
  combined_data <- all_data %>%
    pull(data) %>%
    bind_rows()
  
  cat("Successfully processed", nrow(all_data), "files\n")
  cat("Combined dataset has", nrow(combined_data), "rows and", ncol(combined_data), "columns\n")
  
  return(list(
    combined_data = combined_data,
    file_info = all_data,
    failed_files = file_info %>% 
      anti_join(all_data, by = "file_path") %>% 
      pull(file_path)
  ))
}

results <- process_hidalgo_elections(base_path)

# Or save to the same folder as your source data
output_path <- "../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/"
write.csv(results$combined_data, paste0(output_path, "hidalgo_2024_combined.csv"), row.names = FALSE)


# Load the 2024 dataset from the CSV
data_2024 <- read_csv("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2024/hidalgo_2024_combined.csv") %>% 
  dplyr::filter(nombremunicipio !=  "CUAUTEPEC DE HINOJOSA")

data_ext <- read_csv("../../../Data/Raw Electoral Data/Hidalgo - 1996, 1999, 2002, 2005, 2008, 2011,2016,2020,2024/2024/ext_2024.csv")

names(data_2024)
names(data_ext)

# Rename columns
data_2024 <- data_2024 %>%
  dplyr::rename(section = seccion,
                otro = municipality,
                municipality = nombremunicipio,
                total = TOTAL,
                no_reg = CNR,
                nulos = VN,
                PANAL = PNAH,
                CI_1 = CI_01,
                CI_2 = CI_02) %>%
  rename_with(~ str_replace_all(., "PNAH", "PANAL")) %>% 
  dplyr::mutate(
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    municipality = case_when(
      municipality == "ACATLΜN" ~ "ACATLAN",
      municipality == "ACAXOCHITLΜN" ~ "ACAXOCHITLAN",
      TRUE ~ municipality
    ),
    section = as.numeric(section) 
  ) %>% 
  dplyr::filter(section > 0)

data_ext <- data_ext %>%
  dplyr::rename(municipality = NOMBRE_MUNICIPIO,
                section = SECCION,
                total = TOTAL_VOTOS,
                no_reg = CNR,
                nulos = VN,
                PANAL = PNAH) %>%
  rename_with(~ str_replace_all(., "PNAH", "PANAL")) %>% 
  dplyr::mutate(
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

# Combine with extraordinary election
data_2024 <- bind_rows(data_2024, data_ext)

# Assign uniqueids
data_2024 <- data_2024 %>%
  mutate(uniqueid = case_when(
    municipality == "ACATLAN" ~ 13001,
    municipality == "ACAXOCHITLAN" ~ 13002,
    municipality == "ACTOPAN" ~ 13003,
    municipality == "AGUA BLANCA DE ITURBIDE" ~ 13004,
    municipality == "AJACUBA" ~ 13005,
    municipality == "ALFAJAYUCAN" ~ 13006,
    municipality == "ALMOLOYA" ~ 13007,
    municipality == "APAN" ~ 13008,
    municipality == "ATITALAQUIA" ~ 13010,
    municipality == "ATLAPEXCO" ~ 13011,
    municipality == "ATOTONILCO DE TULA" ~ 13013,
    municipality == "ATOTONILCO EL GRANDE" ~ 13012,
    municipality == "CALNALI" ~ 13014,
    municipality == "CARDONAL" ~ 13015,
    municipality == "CHAPANTONGO" ~ 13017,
    municipality == "CHAPULHUACAN" ~ 13018,
    municipality == "CHILCUAUTLA" ~ 13019,
    municipality == "CUAUTEPEC DE HINOJOSA" ~ 13016,
    municipality == "EL ARENAL" ~ 13009,
    municipality == "ELOXOCHITLAN" ~ 13020,
    municipality == "EMILIANO ZAPATA" ~ 13021,
    municipality == "EPAZOYUCAN" ~ 13022,
    municipality == "FRANCISCO I MADERO" ~ 13023,
    municipality == "HUASCA DE OCAMPO" ~ 13024,
    municipality == "HUAUTLA" ~ 13025,
    municipality == "HUAZALINGO" ~ 13026,
    municipality == "HUEHUETLA" ~ 13027,
    municipality == "HUEJUTLA DE REYES" ~ 13028,
    municipality == "HUICHAPAN" ~ 13029,
    municipality == "IXMIQUILPAN" ~ 13030,
    municipality == "JACALA DE LEDEZMA" ~ 13031,
    municipality == "JALTOCAN" ~ 13032,
    municipality == "JUAREZ HIDALGO" ~ 13033,
    municipality == "LA MISION" ~ 13040,
    municipality == "LOLOTLA" ~ 13034,
    municipality == "METEPEC" ~ 13035,
    municipality == "METZTITLAN" ~ 13037,
    municipality == "MINERAL DE LA REFORMA" ~ 13051,
    municipality == "MINERAL DEL CHICO" ~ 13038,
    municipality == "MINERAL DEL MONTE" ~ 13039,
    municipality == "MIXQUIAHUALA DE JUAREZ" ~ 13041,
    municipality == "MOLANGO DE ESCAMILLA" ~ 13042,
    municipality == "NICOLAS FLORES" ~ 13043,
    municipality == "NOPALA DE VILLAGRAN" ~ 13044,
    municipality == "OMITLAN DE JUAREZ" ~ 13045,
    municipality == "PACHUCA DE SOTO" ~ 13048,
    municipality == "PACULA" ~ 13047,
    municipality == "PISAFLORES" ~ 13049,
    municipality == "PROGRESO DE OBREGON" ~ 13050,
    municipality == "SAN AGUSTIN METZQUITITLAN" ~ 13036,
    municipality == "SAN AGUSTIN TLAXIACA" ~ 13052,
    municipality == "SAN BARTOLO TUTOTEPEC" ~ 13053,
    municipality == "SAN FELIPE ORIZATLAN" ~ 13046,
    municipality == "SAN SALVADOR" ~ 13054,
    municipality == "SANTIAGO DE ANAYA" ~ 13055,
    municipality == "SANTIAGO TULANTEPEC DE LUGO GUERRERO" ~ 13056,
    municipality == "SINGUILUCAN" ~ 13057,
    municipality == "TASQUILLO" ~ 13058,
    municipality == "TECOZAUTLA" ~ 13059,
    municipality == "TENANGO DE DORIA" ~ 13060,
    municipality == "TEPEAPULCO" ~ 13061,
    municipality == "TEPEHUACAN DE GUERRERO" ~ 13062,
    municipality == "TEPEJI DEL RIO DE OCAMPO" ~ 13063,
    municipality == "TEPETITLAN" ~ 13064,
    municipality == "TETEPANGO" ~ 13065,
    municipality == "TEZONTEPEC DE ALDAMA" ~ 13067,
    municipality == "TIANGUISTENGO" ~ 13068,
    municipality == "TIZAYUCA" ~ 13069,
    municipality == "TLAHUELILPAN" ~ 13070,
    municipality == "TLAHUILTEPA" ~ 13071,
    municipality == "TLANALAPA" ~ 13072,
    municipality == "TLANCHINOL" ~ 13073,
    municipality == "TLAXCOAPAN" ~ 13074,
    municipality == "TOLCAYUCA" ~ 13075,
    municipality == "TULA DE ALLENDE" ~ 13076,
    municipality == "TULANCINGO DE BRAVO" ~ 13077,
    municipality == "VILLA DE TEZONTEPEC" ~ 13066,
    municipality == "XOCHIATIPAN" ~ 13078,
    municipality == "XOCHICOATLAN" ~ 13079,
    municipality == "YAHUALICA" ~ 13080,
    municipality == "ZACUALTIPAN DE ANGELES" ~ 13081,
    municipality == "ZAPOTLAN DE JUAREZ" ~ 13082,
    municipality == "ZEMPOALA" ~ 13083,
    municipality == "ZIMAPAN" ~ 13084,
    TRUE ~ NA_integer_
  ))

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(across(c(PAN:total), 
                          sum, na.rm = TRUE))

# Load the Lista Nominal 2024 data and filter by criteria
ln_2024 <- read_excel("../../../Data/Raw Electoral Data/Listas Nominales/listanom_pef24.xlsx", skip = 2, 
                      col_names = c("state_code", "district_code", "mun_code", 
                                    "section", "col_e", "col_f", "col_g", "col_h", 
                                    "col_i", "col_j", "col_k", "listanominal")) %>%
  dplyr::select(state_code, mun_code, section, listanominal) %>% 
  dplyr::filter(state_code == 13) %>%
  dplyr::select(section,listanominal)

# Merge Lista Nominal data with the collapsed data
collapsed_2024 <- collapsed_2024 %>%
  left_join(ln_2024, by = "section")

# Calculate valid votes and final details
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    valid = sum(c_across(PAN:CI_2), na.rm = TRUE),
    turnout = total/listanominal,
    year = 2024,
    month = case_when(
      municipality == "CUAUTEPEC DE HINOJOSA" ~ "December",
      TRUE ~ "June"
    )
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

# Combine the dataframes, handling different columns by filling with NA
Hidalgo_all <- bind_rows(data_1996,
                         data_1999,
                         data_2002,
                         data_2005,
                         data_2008,
                         data_2011,
                         data_2016,
                         extra_data,
                         collapsed_2020,
                         collapsed_2024)

data.table::fwrite(Hidalgo_all,"../../../Processed Data/hidalgo/hidalgo_process_raw_data.csv")
