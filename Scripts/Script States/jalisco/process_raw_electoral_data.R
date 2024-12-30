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

#-------------------------------------------------------------
# 1. Read data (Ayu_Seccion_1995.csv)
#-------------------------------------------------------------

data_1995 <- read_csv("../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1995.csv", show_col_types = FALSE)
names(data_1995) <- tolower(names(data_1995))

#-------------------------------------------------------------
# 2. Rename variables municipio -> municipality, seccion -> section
#-------------------------------------------------------------

data_1995 <- data_1995 %>%
  rename(municipality = municipio,
         section = seccion)

#-------------------------------------------------------------
# 3. Drop if municipality==. & section==.
# In R, is.na() is used, and the original code checks numeric missing.
# The original code: drop if municipality==. & section==.
# If municipality and section are numeric, check their types.
# If they are character, municipality==. wouldn't apply. We'll assume they mean missing (NA).
data_1995 <- data_1995 %>% filter(!(is.na(municipality) & is.na(section)))

#-------------------------------------------------------------
# 4. drop votaron if it exists
#-------------------------------------------------------------
if("votaron" %in% names(data_1995)) {
  data_1995 <- data_1995 %>% select(-votaron)
}

#-------------------------------------------------------------
# 5. Generate total = rowtotal(pan pri pps prd pfcrn parm pdm pt pvem ppj nulos)
vars_for_total <- c("pan","pri","pps","prd","pfcrn","parm","pdm","pt","pvem","ppj","nulos")
vars_for_total <- intersect(vars_for_total, names(data_1995))
data_1995 <- data_1995 %>%
  mutate(total = rowSums(across(all_of(vars_for_total)), na.rm = TRUE))

#-------------------------------------------------------------
# 6. Drop if total==. or total==0
# In R, total==. means NA. Check NA with is.na().
data_1995 <- data_1995 %>% filter(!is.na(total) & total != 0)

#-------------------------------------------------------------
# 7. destring listanominal - total, replace
# Convert these variables to numeric if they're not already.
vars_to_num <- c("lista nominal","pan","pri","pps","prd","pfcrn","parm","pdm","pt","pvem","ppj","nulos","total")
vars_to_num <- intersect(vars_to_num, names(data_1995))
data_1995 <- data_1995 %>% mutate(across(all_of(vars_to_num), as.numeric))

names(data_1995)
#-------------------------------------------------------------
# 9. collapse (sum) missing listanominal - total, by (municipality section)
data_1995 <- data_1995 %>%
  rename(listanominal="lista nominal") %>% 
  group_by(municipality, section) %>%
  summarise(across(c(listanominal:total), 
                   sum, na.rm = TRUE), .groups = "drop")
#-------------------------------------------------------------
# 12. Rename parties
#-------------------------------------------------------------

data_1995 <- data_1995 %>%
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

#-------------------------------------------------------------
# 13. gen turnout = total/listanominal
#-------------------------------------------------------------
data_1995 <- data_1995 %>% mutate(turnout = total / listanominal)

#-------------------------------------------------------------
# 14. drop nulos
if("nulos" %in% names(data_1995)) {
  data_1995 <- data_1995 %>% select(-nulos)
}

#-------------------------------------------------------------
# 15. egen valid = rowtotal(PAN PRI PPS PRD PartCardenista PARM PD PT PVEM PPJ)
valid_vars <- c("PAN","PRI","PPS","PRD","PartCardenista","PARM","PD","PT","PVEM","PPJ")
valid_vars <- intersect(valid_vars, names(data_1995))
data_1995 <- data_1995 %>% mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

#-------------------------------------------------------------
# 21. gen year = 1995
#    gen month = "February"
data_1995 <- data_1995 %>%
  mutate(year = 1995,
         month = "February")

#-------------------------------------------------------------
# 23. sort section
data_1995 <- data_1995 %>% arrange(section)

#--------------------------------------------
# 1. Import Excel data (Ayu_Seccion_1997_No_LN.xlsx)
#--------------------------------------------
data_1997 <- read_excel("../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997_No_LN.xlsx") 
names(data_1997) <- tolower(names(data_1997))

#--------------------------------------------
# 2. Rename and basic cleaning
#--------------------------------------------
# rename municipio to municipality and seccion to section
data_1997 <- data_1997 %>%
  rename(municipality = municipio,
         section = seccion)

# Drop observations if municipality = "" & section = NA
data_1997 <- data_1997 %>% filter(!(municipality == "" & is.na(section)))

vars_to_num <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","total")
data_1997 <- data_1997 %>% mutate(across(all_of(vars_to_num), as.numeric))

#--------------------------------------------
# 3. Rename parties to uppercase versions
#--------------------------------------------
data_1997 <- data_1997 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PartCardenista = pc,
         PT = pt,
         PVEM = pvem,
         PPS = pps,
         PD = pdm)

#--------------------------------------------
# 4. Clean municipality names (uppercase and remove accents)
#--------------------------------------------
data_1997 <- data_1997 %>%
  mutate(municipality = toupper(municipality),
         municipality = str_replace_all(municipality, "Á", "A"),
         municipality = str_replace_all(municipality, "É", "E"),
         municipality = str_replace_all(municipality, "Í", "I"),
         municipality = str_replace_all(municipality, "Ó", "O"),
         municipality = str_replace_all(municipality, "Ú", "U"),
         municipality = str_replace_all(municipality, "Ñ", "N"))

#--------------------------------------------
# 5. Generate uniqueid for each municipality
#--------------------------------------------
data_1997 <- data_1997 %>%
  mutate(uniqueid = NA) %>%
  mutate(uniqueid = case_when(
    municipality == "ACATIC" ~ 14001,
    municipality == "ACATLAN DE JUAREZ" ~ 14002,
    municipality == "AHUALULCO DE MERCADO" ~ 14003,
    municipality == "AMACUECA" ~ 14004,
    municipality == "AMATITAN" ~ 14005,
    municipality == "AMECA" ~ 14006,
    municipality == "ANTONIO ESCOBEDO" ~ 14007,
    municipality == "ARANDAS" ~ 14008,
    municipality == "ATEMAJAC DE BRIZUELA" ~ 14010,
    municipality == "ATENGO" ~ 14011,
    municipality == "ATENGUILLO" ~ 14012,
    municipality == "ATOTONILCO EL ALTO" ~ 14013,
    municipality == "ATOYAC" ~ 14014,
    municipality == "AUTLAN DE NAVARRO" ~ 14015,
    municipality == "AYOTLAN" ~ 14016,
    municipality == "AYUTLA" ~ 14017,
    municipality == "BOLANOS" ~ 14019,
    municipality == "CABO CORRIENTES" ~ 14020,
    municipality == "CANADAS DE OBREGON" ~ 14117,
    municipality == "CASIMIRO CASTILLO" ~ 14021,
    municipality == "CHAPALA" ~ 14030,
    municipality == "CHIMALTITAN" ~ 14031,
    municipality == "CHIQUILISTLAN" ~ 14032,
    municipality == "CIHUATLAN" ~ 14022,
    municipality == "CIUDAD GUZMAN" ~ 14023,
    municipality == "COCULA" ~ 14024,
    municipality == "COLOTLAN" ~ 14025,
    municipality == "CONCEPCION DE BUENOS AIRES" ~ 14026,
    municipality == "CUAUTITLAN DE GARCIA BARRAGAI" | municipality == "CUAUTITLAN" ~ 14027,
    municipality == "CUAUTLA" ~ 14028,
    municipality == "CUQUIO" ~ 14029,
    municipality == "DEGOLLADO" ~ 14033,
    municipality == "EJUTLA" ~ 14034,
    municipality == "EL ARENAL" ~ 14009,
    municipality == "EL GRULLO" ~ 14037,
    municipality == "EL LIMON" ~ 14054,
    municipality == "EL SALTO" ~ 14070,
    municipality == "ENCARNACION DE DIAZ" ~ 14035,
    municipality == "ETZATLAN" ~ 14036,
    municipality == "GOMEZ FARIAS" ~ 14079,
    municipality == "GUACHINANGO" ~ 14038,
    municipality == "GUADALAJARA" ~ 14039,
    municipality == "HOSTOTIPAQUILLO" ~ 14040,
    municipality == "HUEJUCAR" ~ 14041,
    municipality == "HUEJUQUILLA EL ALTO" ~ 14042,
    municipality == "IXTLAHUACAN DE LOS MEMBRILLO" | municipality == "IXTLAHUACAN DE LOS MEMBRILLOS" ~ 14044,
    municipality == "IXTLAHUACAN DEL RIO" ~ 14045,
    municipality == "JALOSTOTITLAN" ~ 14046,
    municipality == "JAMAY" ~ 14047,
    municipality == "JESUS MARIA" ~ 14048,
    municipality == "JILOTLAN DE LOS DOLORES" ~ 14049,
    municipality == "JOCOTEPEC" ~ 14050,
    municipality == "JUANACATLAN" ~ 14051,
    municipality == "JUCHITLAN" ~ 14052,
    municipality == "LA BARCA" ~ 14018,
    municipality == "LA HUERTA" ~ 14043,
    municipality == "MANZANILLA DE LA PAZ" ~ 14057,
    municipality == "LAGOS DE MORENO" ~ 14053,
    municipality == "MAGDALENA" ~ 14055,
    municipality == "MANUEL M.DIEGUEZ" ~ 14056,
    municipality == "MASCOTA" ~ 14058,
    municipality == "MAZAMITLA" ~ 14059,
    municipality == "MEXTICACAN" ~ 14060,
    municipality == "MEZQUITIC" ~ 14061,
    municipality == "MIXTLAN" ~ 14062,
    municipality == "OCOTLAN" ~ 14063,
    municipality == "OJUELOS DE JALISCO" ~ 14064,
    municipality == "PIHUAMO" ~ 14065,
    municipality == "PONCITLAN" ~ 14066,
    municipality == "PUERTO VALLARTA" ~ 14067,
    municipality == "QUITUPAN" ~ 14069,
    municipality == "SAN CRISTOBAL DE LA BARRANCA" ~ 14071,
    municipality == "SAN DIEGO DE ALEJANDRIA" ~ 14072,
    municipality == "SAN GABRIEL" ~ 14113,
    municipality == "SAN IGNACIO CERRO GORDO" ~ 14125,
    municipality == "SAN JUAN DE LOS LAGOS" ~ 14073,
    municipality == "SAN JUANITO DE ESCOBEDO" ~ 14007, # duplicates the uniqueid for ANTONIO ESCOBEDO?
    municipality == "SAN JULIAN" ~ 14074,
    municipality == "SAN MARCOS" ~ 14075,
    municipality == "SAN MARTIN DE BOLANOS" ~ 14076,
    municipality == "SAN MARTIN HIDALGO" ~ 14077,
    municipality == "SAN MIGUEL EL ALTO" ~ 14078,
    municipality == "SAN SEBASTIAN DEL OESTE" ~ 14080,
    municipality == "SANTA MARIA DE LOS ANGELES" ~ 14081,
    municipality == "SANTA MARIA DEL ORO" ~ 14056, 
    municipality == "SAYULA" ~ 14082,
    municipality == "TALA" ~ 14083,
    municipality == "TALPA DE ALLENDE" ~ 14084,
    municipality == "TAMAZULA DE GORDIANO" ~ 14085,
    municipality == "TAPALPA" ~ 14086,
    municipality == "TECALITLAN" ~ 14087,
    municipality == "TECHALUTA DE MONTENEGRO" ~ 14089,
    municipality == "TECOLOTLAN" ~ 14088,
    municipality == "TENAMAXTLAN" ~ 14090,
    municipality == "TEOCALTICHE" ~ 14091,
    municipality == "TEOCUITATLAN DE CORONA" ~ 14092,
    municipality == "TEPATITLAN DE MORELOS" ~ 14093,
    municipality == "TEQUILA" ~ 14094,
    municipality == "TEUCHITLAN" ~ 14095,
    municipality == "TIZAPAN EL ALTO" ~ 14096,
    municipality == "TLAJOMULCO DE ZUNIGA" ~ 14097,
    municipality == "TLAQUEPAQUE" ~ 14098,
    municipality == "TOLIMAN" ~ 14099,
    municipality == "TOMATLAN" ~ 14100,
    municipality == "TONALA" ~ 14101,
    municipality == "TONAYA" ~ 14102,
    municipality == "TONILA" ~ 14103,
    municipality == "TOTATICHE" ~ 14104,
    municipality == "TOTOTLAN" ~ 14105,
    municipality == "TUXCACUESCO" ~ 14106,
    municipality == "TUXCUECA" ~ 14107,
    municipality == "TUXPAN" ~ 14108,
    municipality == "UNION DE SAN ANTONIO" ~ 14109,
    municipality == "UNION DE TULA" ~ 14110,
    municipality == "VALLE DE GUADALUPE" ~ 14111,
    municipality == "VALLE DE JUAREZ" ~ 14112,
    municipality == "VILLA CORONA" ~ 14114,
    municipality == "VILLA GUERRERO" ~ 14115,
    municipality == "VILLA HIDALGO" ~ 14116,
    municipality == "VILLA OBREGON" ~ 14117,
    municipality == "VILLA PURIFICACION" ~ 14068,
    municipality == "YAHUALICA DE GONZALEZ GALLO" ~ 14118,
    municipality == "ZACOALCO DE TORRES" ~ 14119,
    municipality == "ZAPOPAN" ~ 14120,
    municipality == "ZAPOTILTIC" ~ 14121,
    municipality == "ZAPOTITLAN DE VADILLO" ~ 14122,
    municipality == "ZAPOTLAN DEL REY" ~ 14123,
    municipality == "ZAPOTLAN EL GRANDE" ~ 14023,
    municipality == "ZAPOTLANEJO" ~ 14124,
    TRUE ~ uniqueid
  ))

# Create 'valid' as the sum of all party votes
data_1997 <- data_1997 %>% 
  mutate(valid = PAN + PRI + PRD + PartCardenista + PT + PVEM + PPS + PD)

#--------------------------------------------
# 6. Merge with another dataset (all_months_years.dta)
#--------------------------------------------
data_1997 <- data_1997 %>%
  mutate(seccion = section)

names(data_1997)

data_1997 <- data_1997 %>%
  group_by(municipality,uniqueid,section) %>%
  summarise(across(c(PAN:total), 
                   sum, na.rm = TRUE), .groups = "drop")

# Merge with the dataset "ln_all_months_years.dta" using seccion (section) and ed
lista_nominal <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

lista_nominal <- lista_nominal %>% 
  dplyr::filter(state == "JALISCO" & month == "January" & year == 1997)

# Merge the datasets
data_1997 <- data_1997 %>%
  dplyr::left_join(lista_nominal %>% dplyr::select(section,lista), by = c("section")) %>% 
  dplyr::mutate(listanominal = lista) %>% 
  dplyr::select(-lista)

# drop _merge==2 not needed since we did left_join
# drop _merge ed seccion year month
drops <- c("year","month","validos" ,
           "no reg.","nulos","state","day")
data_1997 <- data_1997 %>% select(-all_of(drops))

# rename lista to listanominal
data_1997 <- data_1997 %>% rename(listanominal = lista)

#--------------------------------------------
# 7. Create turnout
#--------------------------------------------
data_1997 <- data_1997 %>% mutate(turnout = total / listanominal)
#--------------------------------------------
# 9. Add year and month fields as in final code
#--------------------------------------------
data_1997 <- data_1997 %>%
  mutate(year = 1997,
         month = "November")

# Sort by section
data_1997 <- data_1997 %>% arrange(section)


#-------------------------------------------------------------
# 1. Read data (Ayu_Seccion_2000_No_LN.csv)
#-------------------------------------------------------------
data_2000 <- read_csv("../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2000_No_LN.csv",
                      show_col_types = FALSE)
names(data_2000) <- tolower(names(data_2000))
#-------------------------------------------------------------
# 2. Rename variables municipio -> municipality, seccion -> section
#-------------------------------------------------------------
data_2000 <- data_2000 %>%
  rename(municipality = municipio,
         section = seccion)

#-------------------------------------------------------------
# 3. drop if municipality==. & section==.
#   In R, that means drop if both are NA.
#-------------------------------------------------------------
data_2000 <- data_2000 %>%
  filter(!(is.na(municipality) & is.na(section)))

#-------------------------------------------------------------
# 4. Drop "very weird cases"
#-------------------------------------------------------------
weird_cases <- list(
  c(99,924), c(99,925), c(99,926), c(99,928),
  c(99,2465), c(120,2465), c(99,3172), c(99,3174),
  c(99,3199), c(99,3206), c(99,3223), c(99,3225)
)

for (wc in weird_cases) {
  data_2000 <- data_2000 %>%
    filter(!(municipality == wc[1] & section == wc[2]))
}

#-------------------------------------------------------------
# 5. Generate total votes row-wise
#   (pan pri prd pt pvem convergencia pcd psn parm pas ds)
#-------------------------------------------------------------
vars_for_total <- c("pan","pri","prd","pt","pvem","convergencia",
                    "pcd","psn","parm","pas","ds")
vars_for_total <- intersect(vars_for_total, names(data_2000))

data_2000 <- data_2000 %>%
  mutate(total = rowSums(across(all_of(vars_for_total)), na.rm = TRUE))

#-------------------------------------------------------------
# 6. drop if total==. | total==0
#-------------------------------------------------------------
data_2000 <- data_2000 %>%
  filter(!is.na(total) & total != 0)

#-------------------------------------------------------------
# 7. destring pan - total, replace
#   Convert relevant columns to numeric if needed
#-------------------------------------------------------------
vars_to_num <- c("pan","pri","prd","pt","pvem","convergencia","pcd",
                 "psn","parm","pas","ds","total")
vars_to_num <- intersect(vars_to_num, names(data_2000))

data_2000 <- data_2000 %>%
  mutate(across(all_of(vars_to_num), as.numeric))

#-------------------------------------------------------------
# 8. collapse (sum) pan - total, by (municipality section)
#   Summation by municipality and section
#-------------------------------------------------------------
start_var <- "pan"
end_var <- "total"
all_cols <- names(data_2000)
start_pos <- match(start_var, all_cols)
end_pos <- match(end_var, all_cols)
vars_collapse <- all_cols[seq(from = start_pos, to = end_pos)]

data_2000 <- data_2000 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups = "drop")

#-------------------------------------------------------------
# 9. Rename variables to uppercase party names
#   convergencia -> PC, ds -> PDS, etc.
#-------------------------------------------------------------
data_2000 <- data_2000 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD = prd,
         PC = convergencia,
         PT = pt,
         PVEM = pvem,
         PCD = pcd,
         PARM = parm,
         PAS = pas,
         PDS = "d. s.",
         PSN = psn)

#-------------------------------------------------------------
# 10. drop noregistrados nulos if they exist
#-------------------------------------------------------------
drop_vars <- c("noregistrados", "nulos")
drop_vars <- intersect(drop_vars, names(data_2000))
data_2000 <- data_2000 %>% select(-all_of(drop_vars))

#-------------------------------------------------------------
# 11. egen valid = rowtotal(PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS)
#-------------------------------------------------------------
valid_vars <- c("PAN","PRI","PRD","PT","PVEM","PC","PCD","PSN","PARM","PAS","PDS")
valid_vars <- intersect(valid_vars, names(data_2000))

data_2000 <- data_2000 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

#-------------------------------------------------------------
# 12.gen seccion = section
#-------------------------------------------------------------
data_2000 <- data_2000 %>%
  mutate(seccion = section)

#-------------------------------------------------------------
# 13. merge 1:m ed seccion using "..\..\all_months_years.dta"
#   keepusing(month year lista), keep if month==12 & year==2000
#   drop if _merge==2, drop _merge ed seccion year month
#   rename lista to listanominal
#-------------------------------------------------------------
lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "JALISCO" &
           month == "December" &
           year == 2000)

data_2000 <- data_2000 %>%
  left_join(lista_nominal, by = c("section"))
names(data_2000)
# Next, drop ed, seccion, year, month
data_2000 <- data_2000 %>%
  select(-seccion, -year, -month, -state)

# rename lista -> listanominal
data_2000 <- data_2000 %>% rename(listanominal = lista)

#-------------------------------------------------------------
# 14. gen turnout = total/listanominal
#-------------------------------------------------------------
data_2000 <- data_2000 %>%
  mutate(turnout = total / listanominal)

#-------------------------------------------------------------
# Omit second municipal sum for listanominal and mun_turnout,
# also omit rowranks and winner
#-------------------------------------------------------------

#-------------------------------------------------------------
# 15. gen year = 2000, gen month = "November"
#-------------------------------------------------------------
data_2000 <- data_2000 %>%
  mutate(year = 2000,
         month = "November")

#-------------------------------------------------------------
# 16. drop no registrados
#-------------------------------------------------------------
data_2000 <- data_2000 %>% select(-"no registrados")

#-------------------------------------------------------------
# 17. sort section
#-------------------------------------------------------------
data_2000 <- data_2000 %>% arrange(section)

#-------------------------------------------------------------
# 18. merge section using Jalisco_Section_1997_for_Merge.dta
#   drop if _merge==2
#-------------------------------------------------------------
validation <- data_1997 %>% 
  select(municipality,uniqueid,section)

data_2000 <- data_2000 %>% 
  select(-municipality) %>% 
  left_join(validation,by ="section")

# -----------------------------------------------------
# 1. Read CSV files
#    (1) Ayu_Seccion_2003_No_LN.csv
#    (2) Ayu_Seccion_2003_No_LN_Extra.csv
#    Convert strings to numeric (as per "destring")
#    Save them as .dta if desired (omitting commented steps).
# -----------------------------------------------------
data_2003_main <- read_csv(
  "../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003_No_LN.csv",
  show_col_types = FALSE
)

# Convert all columns to numeric where possible
data_2003_main <- data_2003_main %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

# data_2003_main.dta can be saved if needed:
# write_dta(data_2003_main, "Ayu_Seccion_2003_No_LN.dta")

data_2003_extra <- read_csv(
  "../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003_No_LN_Extra.csv",
  show_col_types = FALSE
)

data_2003_extra <- data_2003_extra %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

# -----------------------------------------------------
# 2. Append data (mimicking "use ... append using ...")
# -----------------------------------------------------
# Combine the two dataframes by rows
data_2003 <- bind_rows(data_2003_main, data_2003_extra)

names(data_2003) <- tolower(names(data_2003))
# -----------------------------------------------------
# 3. Rename variables
# -----------------------------------------------------
data_2003 <- data_2003 %>%
  rename(municipality = municipio,
         section = seccion)

# -----------------------------------------------------
# 4. Drop if municipality = "" & section == .
#    In R, we interpret "" as empty char, and check if section is NA
# -----------------------------------------------------
data_2003 <- data_2003 %>%
  filter(!(municipality == "" & is.na(section)))

# -----------------------------------------------------
# 5. Create 'total' = rowtotal(...)
#    Variables: pan pri prd pt pvem pc psn pas elbarzon mexicoposible plm fc prdpas prdpc prdpvem pvempas prdpvempas
# -----------------------------------------------------
vars_for_total <- c("pan","pri","prd","pt","pvem","pc","psn","pas","elbarzon",
                    "mexicoposible","plm","fc","prdpas","prdpc","prdpvem","pvempas","prdpvempas")
vars_for_total <- intersect(vars_for_total, names(data_2003))

data_2003 <- data_2003 %>%
  mutate(total = rowSums(across(all_of(vars_for_total)), na.rm=TRUE))

# Drop if total == . or total == 0
data_2003 <- data_2003 %>% filter(!is.na(total) & total != 0)

# -----------------------------------------------------
# 6. collapse (sum) pan - fc prdpas - total, by(municipality section)
#    This means sum all variables from 'pan' through 'total'
# -----------------------------------------------------
start_var <- "pan"
end_var   <- "total"
all_cols  <- names(data_2003)
start_pos <- match(start_var, all_cols)
end_pos   <- match(end_var, all_cols)
vars_collapse <- all_cols[seq(from=start_pos, to=end_pos)]

data_2003 <- data_2003 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

# -----------------------------------------------------
# 7. Rename party variables
# -----------------------------------------------------
data_2003 <- data_2003 %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PC = pc,
    PT = pt,
    PVEM = pvem,
    PSN = psn,
    PAS = pas,
    ElBarzon = "el barzon",
    MexicoPosible = "mexico posible",
    PLM = plm,
    FC = fc,
    PRD_PAS = "prd-pas",
    PRD_PC = "prd-pc",
    PRD_PVEM = "prd-pvem",
    PVEM_PAS = "pvem-pas",
    PRD_PVEM_PAS = "prd-pvem-pas"
  )

# -----------------------------------------------------
# 8. Generate uniqueid based on municipality
#    Clean up using case_when in R, but here we do direct if_else replacements
# -----------------------------------------------------
data_2003 <- data_2003 %>%
  mutate(uniqueid = 0)

data_2003 <- data_2003 %>%
  mutate(uniqueid = case_when(
    municipality == "ACATIC" ~ 14001,
    municipality == "ACATLAN DE JUAREZ" ~ 14002,
    municipality == "AHUALULCO DE MERCADO" ~ 14003,
    municipality == "AMACUECA" ~ 14004,
    municipality == "AMATITAN" ~ 14005,
    municipality == "AMECA" ~ 14006,
    municipality == "ANTONIO ESCOBEDO" ~ 14007,
    municipality == "ARANDAS" ~ 14008,
    municipality == "ATEMAJAC DE BRIZUELA" ~ 14010,
    municipality == "ATENGO" ~ 14011,
    municipality == "ATENGUILLO" ~ 14012,
    municipality == "ATOTONILCO EL ALTO" ~ 14013,
    municipality == "ATOYAC" ~ 14014,
    municipality == "AUTLAN DE NAVARRO" ~ 14015,
    municipality == "AYOTLAN" ~ 14016,
    municipality == "AYUTLA" ~ 14017,
    municipality == "BOLANOS" ~ 14019,
    municipality == "CABO CORRIENTES" ~ 14020,
    municipality == "CANADAS DE OBREGON" ~ 14117,
    municipality == "CASIMIRO CASTILLO" ~ 14021,
    municipality == "CHAPALA" ~ 14030,
    municipality == "CHIMALTITAN" ~ 14031,
    municipality == "CHIQUILISTLAN" ~ 14032,
    municipality == "CIHUATLAN" ~ 14022,
    municipality == "CIUDAD GUZMAN" ~ 14023,
    municipality == "COCULA" ~ 14024,
    municipality == "COLOTLAN" ~ 14025,
    municipality == "CONCEPCION DE BUENOS AIRES" ~ 14026,
    municipality == "CUAUTITLAN DE GARCIA BARRAGAI" | municipality == "CUAUTITLAN" ~ 14027,
    municipality == "CUAUTLA" ~ 14028,
    municipality == "CUQUIO" ~ 14029,
    municipality == "DEGOLLADO" ~ 14033,
    municipality == "EJUTLA" ~ 14034,
    municipality == "EL ARENAL" ~ 14009,
    municipality == "EL GRULLO" ~ 14037,
    municipality == "EL LIMON" ~ 14054,
    municipality == "EL SALTO" ~ 14070,
    municipality == "ENCARNACION DE DIAZ" ~ 14035,
    municipality == "ETZATLAN" ~ 14036,
    municipality == "GOMEZ FARIAS" ~ 14079,
    municipality == "GUACHINANGO" ~ 14038,
    municipality == "GUADALAJARA" ~ 14039,
    municipality == "HOSTOTIPAQUILLO" ~ 14040,
    municipality == "HUEJUCAR" ~ 14041,
    municipality == "HUEJUQUILLA EL ALTO" ~ 14042,
    municipality == "IXTLAHUACAN DE LOS MEMBRILLO" | municipality == "IXTLAHUACAN DE LOS MEMBRILLOS" ~ 14044,
    municipality == "IXTLAHUACAN DEL RIO" ~ 14045,
    municipality == "JALOSTOTITLAN" ~ 14046,
    municipality == "JAMAY" ~ 14047,
    municipality == "JESUS MARIA" ~ 14048,
    municipality == "JILOTLAN DE LOS DOLORES" ~ 14049,
    municipality == "JOCOTEPEC" ~ 14050,
    municipality == "JUANACATLAN" ~ 14051,
    municipality == "JUCHITLAN" ~ 14052,
    municipality == "LA BARCA" ~ 14018,
    municipality == "LA HUERTA" ~ 14043,
    municipality == "MANZANILLA DE LA PAZ" ~ 14057,
    municipality == "LAGOS DE MORENO" ~ 14053,
    municipality == "MAGDALENA" ~ 14055,
    municipality == "MANUEL M.DIEGUEZ" ~ 14056,
    municipality == "MASCOTA" ~ 14058,
    municipality == "MAZAMITLA" ~ 14059,
    municipality == "MEXTICACAN" ~ 14060,
    municipality == "MEZQUITIC" ~ 14061,
    municipality == "MIXTLAN" ~ 14062,
    municipality == "OCOTLAN" ~ 14063,
    municipality == "OJUELOS DE JALISCO" ~ 14064,
    municipality == "PIHUAMO" ~ 14065,
    municipality == "PONCITLAN" ~ 14066,
    municipality == "PUERTO VALLARTA" ~ 14067,
    municipality == "QUITUPAN" ~ 14069,
    municipality == "SAN CRISTOBAL DE LA BARRANCA" ~ 14071,
    municipality == "SAN DIEGO DE ALEJANDRIA" ~ 14072,
    municipality == "SAN GABRIEL" ~ 14113,
    municipality == "SAN IGNACIO CERRO GORDO" ~ 14125,
    municipality == "SAN JUAN DE LOS LAGOS" ~ 14073,
    municipality == "SAN JUANITO DE ESCOBEDO" ~ 14007,
    municipality == "SAN JULIAN" ~ 14074,
    municipality == "SAN MARCOS" ~ 14075,
    municipality == "SAN MARTIN DE BOLANOS" ~ 14076,
    municipality == "SAN MARTIN HIDALGO" ~ 14077,
    municipality == "SAN MIGUEL EL ALTO" ~ 14078,
    municipality == "SAN SEBASTIAN DEL OESTE" ~ 14080,
    municipality == "SANTA MARIA DE LOS ANGELES" ~ 14081,
    municipality == "SANTA MARIA DEL ORO" ~ 14056,
    municipality == "SAYULA" ~ 14082,
    municipality == "TALA" ~ 14083,
    municipality == "TALPA DE ALLENDE" ~ 14084,
    municipality == "TAMAZULA DE GORDIANO" ~ 14085,
    municipality == "TAPALPA" ~ 14086,
    municipality == "TECALITLAN" ~ 14087,
    municipality == "TECHALUTA DE MONTENEGRO" ~ 14089,
    municipality == "TECOLOTLAN" ~ 14088,
    municipality == "TENAMAXTLAN" ~ 14090,
    municipality == "TEOCALTICHE" ~ 14091,
    municipality == "TEOCUITATLAN DE CORONA" ~ 14092,
    municipality == "TEPATITLAN DE MORELOS" ~ 14093,
    municipality == "TEQUILA" ~ 14094,
    municipality == "TEUCHITLAN" ~ 14095,
    municipality == "TIZAPAN EL ALTO" ~ 14096,
    municipality == "TLAJOMULCO DE ZUNIGA" ~ 14097,
    municipality == "TLAQUEPAQUE" ~ 14098,
    municipality == "TOLIMAN" ~ 14099,
    municipality == "TOMATLAN" ~ 14100,
    municipality == "TONALA" ~ 14101,
    municipality == "TONAYA" ~ 14102,
    municipality == "TONILA" ~ 14103,
    municipality == "TOTATICHE" ~ 14104,
    municipality == "TOTOTLAN" ~ 14105,
    municipality == "TUXCACUESCO" ~ 14106,
    municipality == "TUXCUECA" ~ 14107,
    municipality == "TUXPAN" ~ 14108,
    municipality == "UNION DE SAN ANTONIO" ~ 14109,
    municipality == "UNION DE TULA" ~ 14110,
    municipality == "VALLE DE GUADALUPE" ~ 14111,
    municipality == "VALLE DE JUAREZ" ~ 14112,
    municipality == "VILLA CORONA" ~ 14114,
    municipality == "VILLA GUERRERO" ~ 14115,
    municipality == "VILLA HIDALGO" ~ 14116,
    municipality == "VILLA OBREGON" ~ 14117,
    municipality == "VILLA PURIFICACION" ~ 14068,
    municipality == "YAHUALICA DE GONZALEZ GALLO" ~ 14118,
    municipality == "ZACOALCO DE TORRES" ~ 14119,
    municipality == "ZAPOPAN" ~ 14120,
    municipality == "ZAPOTILTIC" ~ 14121,
    municipality == "ZAPOTITLAN DE VADILLO" ~ 14122,
    municipality == "ZAPOTLAN DEL REY" ~ 14123,
    municipality == "ZAPOTLAN EL GRANDE" ~ 14023,
    municipality == "ZAPOTLANEJO" ~ 14124,
    TRUE ~ uniqueid
  ))

# -----------------------------------------------------
# 9. egen valid = rowtotal(...)
# -----------------------------------------------------
valid_vars <- c("PAN","PRI","PRD","PT","PVEM","PC","PSN","PAS","ElBarzon","MexicoPosible",
                "PLM","FC","PRD_PAS","PRD_PC","PRD_PVEM","PVEM_PAS","PRD_PVEM_PAS")
valid_vars <- intersect(valid_vars, names(data_2003))

data_2003 <- data_2003 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# -----------------------------------------------------
# 10. * Drop duplicated section number
# -----------------------------------------------------
data_2003 <- data_2003 %>% filter(section != 2484)

# -----------------------------------------------------
# Omit municipal aggregation and inverse variables,
# Omit rowranks, Omit winner generation
# -----------------------------------------------------

# -----------------------------------------------------
# 11. Merge with ..\..\all_months_years.dta
#    keep if month==7 & year==2003
# -----------------------------------------------------
data_2003 <- data_2003 %>%
  mutate(seccion = section)

lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "JALISCO" &
           month == "July" &
           year == 2003)

data_2003 <- data_2003 %>%
  left_join(lista_nominal, by = c("section"))

names(data_2003)
# drop if _merge==2 is not needed because left_join doesn’t create _merge
data_2003 <- data_2003 %>% select(-seccion, -year, -month, -state, -state)

# rename lista -> listanominal
data_2003 <- data_2003 %>% rename(listanominal = lista)

# -----------------------------------------------------
# 12. gen turnout = total/listanominal
# -----------------------------------------------------
data_2003 <- data_2003 %>%
  mutate(turnout = total / listanominal)

# -----------------------------------------------------
# 13. gen year=2003, month="July"
# -----------------------------------------------------
data_2003 <- data_2003 %>%
  mutate(year = 2003,
         month = "July")

#-------------------------------------------------------------
# 1. Import Excel data (Ayu_Seccion_2006_No_LN.xlsx)
#-------------------------------------------------------------
data_2006 <- read_excel("../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2006_No_LN.xlsx")
names(data_2006) <- tolower(names(data_2006)) # convert column names to lowercase to match case(lower)

#-------------------------------------------------------------
# 2. Rename and basic cleaning
#-------------------------------------------------------------
data_2006 <- data_2006 %>%
  rename(municipality = municipio,
         section = casillas)

# Drop if municipality=="" & section==.
data_2006 <- data_2006 %>% filter(!(municipality == "" & is.na(section)))

# Drop if total==. | total==0
data_2006 <- data_2006 %>% filter(!is.na(total) & total != 0)

#-------------------------------------------------------------
# 3. Convert pan - total to numeric if needed
#-------------------------------------------------------------
vars_to_num <- c("pan","pri","prd-pt","pc","pvem","panal","pas","total")
vars_to_num <- intersect(vars_to_num, names(data_2006))
data_2006 <- data_2006 %>% mutate(across(all_of(vars_to_num), as.numeric))

#-------------------------------------------------------------
# 4. collapse (sum) pan - pas total, by (municipality section)
#   We sum over these groups.
#-------------------------------------------------------------
start_var <- "pan"
end_var <- "pas"  # we also have 'total' after 'pas', so actually from pan to total
all_cols <- names(data_2006)
start_pos <- match(start_var, all_cols)
end_pos <- match("total", all_cols)
vars_collapse <- all_cols[seq(from=start_pos, to=end_pos)]

data_2006 <- data_2006 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

#-------------------------------------------------------------
# 5. Rename parties
#-------------------------------------------------------------
data_2006 <- data_2006 %>%
  rename(PAN = pan,
         PRI = pri,
         PRD_PT = "prd-pt",
         PC = pc,
         PVEM = pvem,
         PANAL = panal,
         PAS = pas)

#-------------------------------------------------------------
# 6. Clean municipality names
#   Replace accented characters with unaccented
#-------------------------------------------------------------
data_2006 <- data_2006 %>%
  mutate(municipality = str_replace_all(municipality, "Á", "A"),
         municipality = str_replace_all(municipality, "Í", "I"),
         municipality = str_replace_all(municipality, "Ó", "O"),
         municipality = str_replace_all(municipality, "Ú", "U"),
         municipality = str_replace_all(municipality, "Ñ", "N"))

#-------------------------------------------------------------
# 7. Assign uniqueid based on municipality
#-------------------------------------------------------------
data_2006 <- data_2006 %>%
  mutate(uniqueid = 0) %>%
  mutate(uniqueid = case_when(
    municipality == "ACATIC" ~ 14001,
    municipality == "ACATLAN DE JUAREZ" ~ 14002,
    municipality == "AHUALULCO DE MERCADO" ~ 14003,
    municipality == "AMACUECA" ~ 14004,
    municipality == "AMATITAN" ~ 14005,
    municipality == "AMECA" ~ 14006,
    municipality == "ANTONIO ESCOBEDO" ~ 14007,
    municipality == "ARANDAS" ~ 14008,
    municipality == "ATEMAJAC DE BRIZUELA" ~ 14010,
    municipality == "ATENGO" ~ 14011,
    municipality == "ATENGUILLO" ~ 14012,
    municipality == "ATOTONILCO EL ALTO" ~ 14013,
    municipality == "ATOYAC" ~ 14014,
    municipality == "AUTLAN DE NAVARRO" ~ 14015,
    municipality == "AYOTLAN" ~ 14016,
    municipality == "AYUTLA" ~ 14017,
    municipality == "BOLANOS" ~ 14019,
    municipality == "CABO CORRIENTES" | municipality=="CABO CORRIENTE" ~ 14020,
    municipality == "CANADAS DE OBREGON" ~ 14117,
    municipality == "CASIMIRO CASTILLO" ~ 14021,
    municipality == "CHAPALA" ~ 14030,
    municipality == "CHIMALTITAN" ~ 14031,
    municipality == "CHIQUILISTLAN" ~ 14032,
    municipality == "CIHUATLAN" ~ 14022,
    municipality == "CIUDAD GUZMAN" ~ 14023,
    municipality == "COCULA" ~ 14024,
    municipality == "COLOTLAN" ~ 14025,
    municipality == "CONCEPCION DE BUENOS AIRES" ~ 14026,
    municipality == "CUAUTITLAN DE GARCIA BARRAGAI" | municipality == "CUAUTITLAN" | municipality=="CUAUTITLAN DE GARCIA BARRAGAN" ~ 14027,
    municipality == "CUAUTLA" ~ 14028,
    municipality == "CUQUIO" ~ 14029,
    municipality == "DEGOLLADO" ~ 14033,
    municipality == "EJUTLA" ~ 14034,
    municipality == "EL ARENAL" ~ 14009,
    municipality == "EL GRULLO" ~ 14037,
    municipality == "EL LIMON" ~ 14054,
    municipality == "EL SALTO" ~ 14070,
    municipality == "ENCARNACION DE DIAZ" ~ 14035,
    municipality == "ETZATLAN" ~ 14036,
    municipality == "GOMEZ FARIAS" ~ 14079,
    municipality == "GUACHINANGO" ~ 14038,
    municipality == "GUADALAJARA" ~ 14039,
    municipality == "HOSTOTIPAQUILLO" ~ 14040,
    municipality == "HUEJUCAR" ~ 14041,
    municipality == "HUEJUQUILLA EL ALTO" ~ 14042,
    municipality == "IXTLAHUACAN DE LOS MEMBRILLO" | municipality =="IXTLAHUACAN DE LOS MEMBRILLOS" ~ 14044,
    municipality == "IXTLAHUACAN DEL RIO" ~ 14045,
    municipality == "JALOSTOTITLAN" ~ 14046,
    municipality == "JAMAY" ~ 14047,
    municipality == "JESUS MARIA" ~ 14048,
    municipality == "JILOTLAN DE LOS DOLORES" ~ 14049,
    municipality == "JOCOTEPEC" ~ 14050,
    municipality == "JUANACATLAN" ~ 14051,
    municipality == "JUCHITLAN" ~ 14052,
    municipality == "LA BARCA" ~ 14018,
    municipality == "LA HUERTA" ~ 14043,
    municipality == "LA MANZANILLA DE LA PAZ" | municipality=="MANZANILLA DE LA PAZ" ~ 14057,
    municipality == "LAGOS DE MORENO" ~ 14053,
    municipality == "MAGDALENA" ~ 14055,
    municipality == "MANUEL M.DIEGUEZ" ~ 14056,
    municipality == "MASCOTA" ~ 14058,
    municipality == "MAZAMITLA" ~ 14059,
    municipality == "MEXTICACAN" ~ 14060,
    municipality == "MEZQUITIC" ~ 14061,
    municipality == "MIXTLAN" ~ 14062,
    municipality == "OCOTLAN" ~ 14063,
    municipality == "OJUELOS DE JALISCO" ~ 14064,
    municipality == "PIHUAMO" ~ 14065,
    municipality == "PONCITLAN" ~ 14066,
    municipality == "PUERTO VALLARTA" ~ 14067,
    municipality == "QUITUPAN" ~ 14069,
    municipality == "SAN CRISTOBAL DE LA BARRANCA" ~ 14071,
    municipality == "SAN DIEGO DE ALEJANDRIA" ~ 14072,
    municipality == "SAN GABRIEL" ~ 14113,
    municipality == "SAN IGNACIO CERRO GORDO" ~ 14125,
    municipality == "SAN JUAN DE LOS LAGOS" ~ 14073,
    municipality == "SAN JUANITO DE ESCOBEDO" ~ 14007,
    municipality == "SAN JULIAN" ~ 14074,
    municipality == "SAN MARCOS" ~ 14075,
    municipality == "SAN MARTIN DE BOLANOS" ~ 14076,
    municipality == "SAN MARTIN HIDALGO" ~ 14077,
    municipality == "SAN MIGUEL EL ALTO" ~ 14078,
    municipality == "SAN SEBASTIAN DEL OESTE" ~ 14080,
    municipality == "SANTA MARIA DE LOS ANGELES" ~ 14081,
    municipality == "SANTA MARIA DEL ORO" ~ 14056,
    municipality == "SAYULA" ~ 14082,
    municipality == "TALA" ~ 14083,
    municipality == "TALPA DE ALLENDE" ~ 14084,
    municipality == "TAMAZULA DE GORDIANO" ~ 14085,
    municipality == "TAPALPA" ~ 14086,
    municipality == "TECALITLAN" ~ 14087,
    municipality == "TECHALUTA DE MONTENEGRO" ~ 14089,
    municipality == "TECOLOTLAN" ~ 14088,
    municipality == "TENAMAXTLAN" ~ 14090,
    municipality == "TEOCALTICHE" ~ 14091,
    municipality == "TEOCUITATLAN DE CORONA" ~ 14092,
    municipality == "TEPATITLAN DE MORELOS" ~ 14093,
    municipality == "TEQUILA" ~ 14094,
    municipality == "TEUCHITLAN" ~ 14095,
    municipality == "TIZAPAN EL ALTO" ~ 14096,
    municipality == "TLAJOMULCO DE ZUNIGA" ~ 14097,
    municipality == "TLAQUEPAQUE" ~ 14098,
    municipality == "TOLIMAN" ~ 14099,
    municipality == "TOMATLAN" ~ 14100,
    municipality == "TONALA" ~ 14101,
    municipality == "TONAYA" ~ 14102,
    municipality == "TONILA" ~ 14103,
    municipality == "TOTATICHE" ~ 14104,
    municipality == "TOTOTLAN" ~ 14105,
    municipality == "TUXCACUESCO" ~ 14106,
    municipality == "TUXCUECA" ~ 14107,
    municipality == "TUXPAN" ~ 14108,
    municipality == "UNION DE SAN ANTONIO" ~ 14109,
    municipality == "UNION DE TULA" ~ 14110,
    municipality == "VALLE DE GUADALUPE" ~ 14111,
    municipality == "VALLE DE JUAREZ" ~ 14112,
    municipality == "VILLA CORONA" ~ 14114,
    municipality == "VILLA GUERRERO" ~ 14115,
    municipality == "VILLA HIDALGO" ~ 14116,
    municipality == "VILLA OBREGON" ~ 14117,
    municipality == "VILLA PURIFICACION" ~ 14068,
    municipality == "YAHUALICA DE GONZALEZ GALLO" ~ 14118,
    municipality == "ZACOALCO DE TORRES" ~ 14119,
    municipality == "ZAPOPAN" ~ 14120,
    municipality == "ZAPOTILTIC" ~ 14121,
    municipality == "ZAPOTITLAN DE VADILLO" ~ 14122,
    municipality == "ZAPOTLAN DEL REY" ~ 14123,
    municipality == "ZAPOTLAN EL GRANDE" ~ 14023,
    municipality == "ZAPOTLANEJO" ~ 14124,
    TRUE ~ uniqueid
  ))

#-------------------------------------------------------------
# 8. egen valid = rowtotal(PAN PRI PRD_PT PVEM PC PANAL PAS)
#-------------------------------------------------------------
valid_vars <- c("PAN","PRI","PRD_PT","PVEM","PC","PANAL","PAS")
valid_vars <- intersect(valid_vars, names(data_2006))
data_2006 <- data_2006 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

#-------------------------------------------------------------
# 9. * Drop duplicated section numbers
#-------------------------------------------------------------
data_2006 <- data_2006 %>%
  group_by(section) %>%
  mutate(tag = n()) %>%
  ungroup() %>%
  filter(tag == 1) %>%
  select(-tag)

#-------------------------------------------------------------
# 10. Merge with all_months_years.dta
#     keep if month==7 & year==2006
#-------------------------------------------------------------
lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "JALISCO" &
           month == "July" &
           year == 2006)

data_2006 <- data_2006 %>%
  left_join(lista_nominal, by = c("section"))

data_2006 <- data_2006 %>% select(-state, -year, -month, -day)
data_2006 <- data_2006 %>% rename(listanominal = lista)

#-------------------------------------------------------------
# 11. gen turnout = total/listanominal
#-------------------------------------------------------------
data_2006 <- data_2006 %>%
  mutate(turnout = total / listanominal)

#-------------------------------------------------------------
# Omit the municipal aggregation again (mun_...),
# Omit the rowranks and winner generation.
#-------------------------------------------------------------

#-------------------------------------------------------------
# 12. gen year=2006, gen month="July"
#-------------------------------------------------------------
data_2006 <- data_2006 %>%
  mutate(year = 2006,
         month = "July")

#-------------------------------------------------------------
# 13. sort section (just arrange)
#-------------------------------------------------------------
data_2006 <- data_2006 %>% arrange(section)
summary(data_2006)
#-------------------------------------------------------------
# 1. Read Excel Data
#-------------------------------------------------------------
data_2009 <- read_excel("../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2009_No_LN.xlsx")
names(data_2009) <- tolower(names(data_2009))

#-------------------------------------------------------------
# 2. Rename variables
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  rename(municipality = municipio,
         section = seccion)

#-------------------------------------------------------------
# 3. drop if total==. | total==0
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  filter(!is.na(total) & total != 0)

data_2009 <- data_2009 %>%
  rename_with(~ gsub("[- ]", "", .x))

#-------------------------------------------------------------
# 4. Generate dummy variables
#   gen dummy_pripanal = (totalpripanal!=.)
#   gen dummy_prdpt = (totalprdpt!=.)
#   gen dummy_ptpc = (totalptconvergencia!=.)
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(dummy_pripanal = !is.na(totalpripanal),
         dummy_prdpt = !is.na(totalprdpt),
         dummy_ptpc = !is.na(totalptconvergencia))

#-------------------------------------------------------------
# 5. drop if municipality=="" & section==.
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  filter(!(municipality == "" & is.na(section)))

#-------------------------------------------------------------
# 6. destring pan - dummy_ptpc, replace
#   Convert relevant variables to numeric if needed
#   We find range pan - dummy_ptpc in columns.
#-------------------------------------------------------------
# Identify variables from pan to dummy_ptpc
all_cols <- names(data_2009)
start_var <- "pan"
end_var <- "dummy_ptpc"
start_pos <- match(start_var, all_cols)
end_pos <- match(end_var, all_cols)
vars_to_num <- all_cols[seq(start_pos, end_pos)]
# Convert to numeric
data_2009 <- data_2009 %>%
  mutate(across(all_of(vars_to_num), as.numeric))

#-------------------------------------------------------------
# 7. collapse (sum) 
#-------------------------------------------------------------
# Variables from pan to totalptconvergencia
end_var_2 <- "totalptconvergencia"
end_pos_2 <- match(end_var_2, all_cols)
vars_collapse_1 <- all_cols[seq(start_pos, end_pos_2)]

# Variables from total to dummy_ptpc
start_var_2 <- "total"
start_pos_2 <- match(start_var_2, all_cols)
vars_collapse_2 <- all_cols[seq(start_pos_2, end_pos)]

vars_collapse <- unique(c(vars_collapse_1, vars_collapse_2))

data_2009 <- data_2009 %>%
  group_by(municipality, section) %>%
  summarize(across(all_of(vars_collapse), sum, na.rm=TRUE), .groups="drop")

#-------------------------------------------------------------
# 8. replace dummy_pripanal = 1 if dummy_pripanal>0, similarly for dummy_prdpt and dummy_ptpc
#   After collapse (sum), if >0, set to 1
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(dummy_pripanal = if_else(dummy_pripanal > 0, 1, dummy_pripanal),
         dummy_prdpt = if_else(dummy_prdpt > 0, 1, dummy_prdpt),
         dummy_ptpc = if_else(dummy_ptpc > 0, 1, dummy_ptpc))

#-------------------------------------------------------------
# 9. Adjust values based on dummy variables
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(pri = if_else(dummy_pripanal == 1, 0, pri),
         panal = if_else(dummy_pripanal == 1, 0, panal),
         pripanal = if_else(dummy_pripanal == 1, 0, pripanal))

# After that sum, drop pri panal pripanal, rename totalpripanal -> pripanal
data_2009 <- data_2009 %>% select(-pri, -panal, -pripanal)
data_2009 <- data_2009 %>% rename(pripanal = totalpripanal)

# PRD_PT adjustments
data_2009 <- data_2009 %>%
  mutate(prd = if_else(dummy_prdpt == 1, 0, prd),
         pt = if_else(dummy_prdpt == 1, 0, pt),
         prdpt = if_else(dummy_prdpt == 1, 0, prdpt))

data_2009 <- data_2009 %>% select(-prdpt)
data_2009 <- data_2009 %>% rename(prdpt = totalprdpt)

# PT_PC adjustments
data_2009 <- data_2009 %>%
  mutate(pt = if_else(dummy_ptpc == 1, 0, pt),
         convergencia = if_else(dummy_ptpc == 1, 0, convergencia),
         ptconvergencia = if_else(dummy_ptpc == 1, 0, ptconvergencia))

data_2009 <- data_2009 %>% select(-ptconvergencia)
data_2009 <- data_2009 %>% rename(ptpc = totalptconvergencia)

# drop dummy variables
data_2009 <- data_2009 %>% select(-dummy_pripanal, -dummy_prdpt, -dummy_ptpc)

#-------------------------------------------------------------
# 10. Rename parties
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  rename(PAN = pan,
         PRI_PANAL = pripanal,
         PRD = prd,
         PT = pt,
         PVEM = pvem,
         PC = convergencia,  # convergencia renamed to PC
         PSD = psd,
         PRD_PT = prdpt,
         PT_PC = ptpc)

#-------------------------------------------------------------
# 11. Clean municipality (replace accented characters)
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(municipality = str_replace_all(municipality, "Á", "A"),
         municipality = str_replace_all(municipality, "Í", "I"),
         municipality = str_replace_all(municipality, "Ó", "O"),
         municipality = str_replace_all(municipality, "Ú", "U"),
         municipality = str_replace_all(municipality, "Ñ", "N"))

#-------------------------------------------------------------
# 12. Assign uniqueid
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(uniqueid = 0) %>%
  mutate(uniqueid = case_when(
    municipality == "ACATIC" ~ 14001,
    municipality == "ACATLAN DE JUAREZ" ~ 14002,
    municipality == "AHUALULCO DE MERCADO" ~ 14003,
    municipality == "AMACUECA" ~ 14004,
    municipality == "AMATITAN" ~ 14005,
    municipality == "AMECA" ~ 14006,
    municipality == "ANTONIO ESCOBEDO" ~ 14007,
    municipality == "ARANDAS" ~ 14008,
    municipality == "ATEMAJAC DE BRIZUELA" ~ 14010,
    municipality == "ATENGO" ~ 14011,
    municipality == "ATENGUILLO" ~ 14012,
    municipality == "ATOTONILCO EL ALTO" ~ 14013,
    municipality == "ATOYAC" ~ 14014,
    municipality == "AUTLAN DE NAVARRO" ~ 14015,
    municipality == "AYOTLAN" ~ 14016,
    municipality == "AYUTLA" ~ 14017,
    municipality == "BOLANOS" ~ 14019,
    municipality == "CABO CORRIENTES" ~ 14020,
    municipality == "CANADAS DE OBREGON" ~ 14117,
    municipality == "CASIMIRO CASTILLO" ~ 14021,
    municipality == "CHAPALA" ~ 14030,
    municipality == "CHIMALTITAN" ~ 14031,
    municipality == "CHIQUILISTLAN" ~ 14032,
    municipality == "CIHUATLAN" ~ 14022,
    municipality == "CIUDAD GUZMAN" ~ 14023,
    municipality == "COCULA" ~ 14024,
    municipality == "COLOTLAN" ~ 14025,
    municipality == "CONCEPCION DE BUENOS AIRES" ~ 14026,
    municipality == "CUAUTITLAN DE GARCIA BARRAGAI" | municipality =="CUAUTITLAN" | municipality=="CUAUTITLAN DE GARCIA BARRAGAN" ~ 14027,
    municipality == "CUAUTLA" ~ 14028,
    municipality == "CUQUIO" ~ 14029,
    municipality == "DEGOLLADO" ~ 14033,
    municipality == "EJUTLA" ~ 14034,
    municipality == "EL ARENAL" ~ 14009,
    municipality == "EL GRULLO" ~ 14037,
    municipality == "EL LIMON" ~ 14054,
    municipality == "EL SALTO" ~ 14070,
    municipality == "ENCARNACION DE DIAZ" ~ 14035,
    municipality == "ETZATLAN" ~ 14036,
    municipality == "GOMEZ FARIAS" ~ 14079,
    municipality == "GUACHINANGO" ~ 14038,
    municipality == "GUADALAJARA" ~ 14039,
    municipality == "HOSTOTIPAQUILLO" ~ 14040,
    municipality == "HUEJUCAR" ~ 14041,
    municipality == "HUEJUQUILLA EL ALTO" ~ 14042,
    municipality == "IXTLAHUACAN DE LOS MEMBRILLO" | municipality =="IXTLAHUACAN DE LOS MEMBRILLOS" ~ 14044,
    municipality == "IXTLAHUACAN DEL RIO" ~ 14045,
    municipality == "JALOSTOTITLAN" ~ 14046,
    municipality == "JAMAY" ~ 14047,
    municipality == "JESUS MARIA" ~ 14048,
    municipality == "JILOTLAN DE LOS DOLORES" ~ 14049,
    municipality == "JOCOTEPEC" ~ 14050,
    municipality == "JUANACATLAN" ~ 14051,
    municipality == "JUCHITLAN" ~ 14052,
    municipality == "LA BARCA" ~ 14018,
    municipality == "LA HUERTA" ~ 14043,
    municipality == "MANZANILLA DE LA PAZ" ~ 14057,
    municipality == "LAGOS DE MORENO" ~ 14053,
    municipality == "MAGDALENA" ~ 14055,
    municipality == "MANUEL M.DIEGUEZ" ~ 14056,
    municipality == "MASCOTA" ~ 14058,
    municipality == "MAZAMITLA" ~ 14059,
    municipality == "MEXTICACAN" ~ 14060,
    municipality == "MEZQUITIC" ~ 14061,
    municipality == "MIXTLAN" ~ 14062,
    municipality == "OCOTLAN" ~ 14063,
    municipality == "OJUELOS DE JALISCO" ~ 14064,
    municipality == "PIHUAMO" ~ 14065,
    municipality == "PONCITLAN" ~ 14066,
    municipality == "PUERTO VALLARTA" ~ 14067,
    municipality == "QUITUPAN" ~ 14069,
    municipality == "SAN CRISTOBAL DE LA BARRANCA" ~ 14071,
    municipality == "SAN DIEGO DE ALEJANDRIA" ~ 14072,
    municipality == "SAN GABRIEL" ~ 14113,
    municipality == "SAN IGNACIO CERRO GORDO" ~ 14125,
    municipality == "SAN JUAN DE LOS LAGOS" ~ 14073,
    municipality == "SAN JUANITO DE ESCOBEDO" ~ 14007,
    municipality == "SAN JULIAN" ~ 14074,
    municipality == "SAN MARCOS" ~ 14075,
    municipality == "SAN MARTIN DE BOLANOS" ~ 14076,
    municipality == "SAN MARTIN HIDALGO" ~ 14077,
    municipality == "SAN MIGUEL EL ALTO" ~ 14078,
    municipality == "SAN SEBASTIAN DEL OESTE" ~ 14080,
    municipality == "SANTA MARIA DE LOS ANGELES" ~ 14081,
    municipality == "SANTA MARIA DEL ORO" ~ 14056,
    municipality == "SAYULA" ~ 14082,
    municipality == "TALA" ~ 14083,
    municipality == "TALPA DE ALLENDE" ~ 14084,
    municipality == "TAMAZULA DE GORDIANO" ~ 14085,
    municipality == "TAPALPA" ~ 14086,
    municipality == "TECALITLAN" ~ 14087,
    municipality == "TECHALUTA DE MONTENEGRO" ~ 14089,
    municipality == "TECOLOTLAN" ~ 14088,
    municipality == "TENAMAXTLAN" ~ 14090,
    municipality == "TEOCALTICHE" ~ 14091,
    municipality == "TEOCUITATLAN DE CORONA" ~ 14092,
    municipality == "TEPATITLAN DE MORELOS" ~ 14093,
    municipality == "TEQUILA" ~ 14094,
    municipality == "TEUCHITLAN" ~ 14095,
    municipality == "TIZAPAN EL ALTO" ~ 14096,
    municipality == "TLAJOMULCO DE ZUNIGA" ~ 14097,
    municipality == "TLAQUEPAQUE" ~ 14098,
    municipality == "TOLIMAN" ~ 14099,
    municipality == "TOMATLAN" ~ 14100,
    municipality == "TONALA" ~ 14101,
    municipality == "TONAYA" ~ 14102,
    municipality == "TONILA" ~ 14103,
    municipality == "TOTATICHE" ~ 14104,
    municipality == "TOTOTLAN" ~ 14105,
    municipality == "TUXCACUESCO" ~ 14106,
    municipality == "TUXCUECA" ~ 14107,
    municipality == "TUXPAN" ~ 14108,
    municipality == "UNION DE SAN ANTONIO" ~ 14109,
    municipality == "UNION DE TULA" ~ 14110,
    municipality == "VALLE DE GUADALUPE" ~ 14111,
    municipality == "VALLE DE JUAREZ" ~ 14112,
    municipality == "VILLA CORONA" ~ 14114,
    municipality == "VILLA GUERRERO" ~ 14115,
    municipality == "VILLA HIDALGO" ~ 14116,
    municipality == "VILLA OBREGON" ~ 14117,
    municipality == "VILLA PURIFICACION" ~ 14068,
    municipality == "YAHUALICA DE GONZALEZ GALLO" ~ 14118,
    municipality == "ZACOALCO DE TORRES" ~ 14119,
    municipality == "ZAPOPAN" ~ 14120,
    municipality == "ZAPOTILTIC" ~ 14121,
    municipality == "ZAPOTITLAN DE VADILLO" ~ 14122,
    municipality == "ZAPOTLAN DEL REY" ~ 14123,
    municipality == "ZAPOTLAN EL GRANDE" ~ 14023,
    municipality == "ZAPOTLANEJO" ~ 14124,
    TRUE ~ uniqueid
  ))

#-------------------------------------------------------------
# 13. egen valid = rowtotal(PAN PRD PT PVEM PC PSD PRI_PANAL PRD_PT PT_PC)
#-------------------------------------------------------------
valid_vars <- c("PAN","PRD","PT","PVEM","PC","PSD","PRI_PANAL","PRD_PT","PT_PC")
valid_vars <- intersect(valid_vars, names(data_2009))
data_2009 <- data_2009 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))
#-------------------------------------------------------------
# * Drop duplicated section numbers
#   duplicates tag section, g(tag)
#   drop if tag>0
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  group_by(section) %>%
  mutate(tag = n()) %>%
  ungroup() %>%
  filter(tag == 1) %>%
  select(-tag)

#-------------------------------------------------------------
# 14. Merge with all_months_years.dta
#    keep if month==7 & year==2009
#-------------------------------------------------------------
lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta") %>% 
  filter(state == "JALISCO" &
           month == "July" &
           year == 2009)

data_2009 <- data_2009 %>%
  left_join(lista_nominal, by = c("section"))

data_2009 <- data_2009 %>% select(-state, -year, -month, -day)

data_2009 <- data_2009 %>% rename(listanominal = lista)

#-------------------------------------------------------------
# 15. gen turnout = total/listanominal
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(turnout = total / listanominal)

#-------------------------------------------------------------
# Omit second municipal aggregation and ranks/winner
#-------------------------------------------------------------

#-------------------------------------------------------------
# 16. gen year=2009, gen month="July"
#-------------------------------------------------------------
data_2009 <- data_2009 %>%
  mutate(year = 2009,
         month = "July")

#-------------------------------------------------------------
# 17. sort section
#-------------------------------------------------------------
data_2009 <- data_2009 %>% arrange(section)

#-------------------------------------------------------------
# 1. Import Excel data
#-------------------------------------------------------------
data_2012 <- read_excel("../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2012.xlsx", 
                        sheet = "CasillaXCasilla")

# Convert column names to lowercase
names(data_2012) <- tolower(names(data_2012))

data_2012 <- data_2012 %>%
  rename_with(~ gsub("[- ]", "", .x))

#-------------------------------------------------------------
# 2. Rename variables
#   rename Municipio municipality
#   gen section = subinstr(Casilla, "B", "", .)
#   remove specific substrings from section
#-------------------------------------------------------------
data_2012 <- data_2012 %>%
  rename(municipality = municipio)

# Create 'section' from 'casilla' by removing certain patterns
data_2012 <- data_2012 %>%
  mutate(section = str_replace_all(casilla, "B", ""))

# Remove C0i, E0i for i=1 to 9
for(i in 1:9) {
  data_2012 <- data_2012 %>%
    mutate(section = str_replace_all(section, paste0("C0", i), "")) %>%
    mutate(section = str_replace_all(section, paste0("E0", i), ""))
}

# For values 10 to 21
for(i in 10:21) {
  data_2012 <- data_2012 %>%
    mutate(section = str_replace_all(section, paste0("C", i), "")) %>%
    mutate(section = str_replace_all(section, paste0("E", i), ""))
}

# Convert section to numeric
data_2012 <- data_2012 %>%
  mutate(section = as.numeric(section))

#-------------------------------------------------------------
# 3. Drop duplicated sections with zero votes
#-------------------------------------------------------------
data_2012 <- data_2012 %>%
  filter(!(section == 995 & municipality == "SAN PEDRO TLAQUEPAQUE")) %>%
  filter(!(section == 2729 & municipality == "EL SALTO")) %>%
  filter(!(section == 2486 & municipality == "GUADALAJARA"))

#-------------------------------------------------------------
# 4. Generate test variable and drop some variables
#-------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(test = coalpripvem - pri - pvem - pripvem + coalptmc - pt - mc - ptmc)

# Drop PRI PVEM PRIPVEM PT MC PTMC test
data_2012 <- data_2012 %>% select(-pri, -pvem, -pripvem, -pt, -mc, -ptmc, -test)

#-------------------------------------------------------------
# 6. collapse (sum) PAN - listanominal total, by(municipality section)
#   This means sum all variables from PAN through listanominal and total as well.
#   Identify columns from PAN to listanominal + total.
#-------------------------------------------------------------

data_2012 <- data_2012 %>%
  group_by(municipality, section) %>%
  summarize(across(pan:no_registrado, sum, na.rm=TRUE), .groups="drop")

#-------------------------------------------------------------
# 8. Replace accented characters in municipality
#-------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(municipality = str_replace_all(municipality, "Á", "A"),
         municipality = str_replace_all(municipality, "Í", "I"),
         municipality = str_replace_all(municipality, "Ó", "O"),
         municipality = str_replace_all(municipality, "Ú", "U"),
         municipality = str_replace_all(municipality, "Ñ", "N"))

#-------------------------------------------------------------
# 9. Assign uniqueid by municipality
#-------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(uniqueid = 0) %>%
  mutate(uniqueid = case_when(
    municipality == "ACATIC" ~ 14001,
    municipality == "ACATLAN DE JUAREZ" ~ 14002,
    municipality == "AHUALULCO DE MERCADO" ~ 14003,
    municipality == "AMACUECA" ~ 14004,
    municipality == "AMATITAN" ~ 14005,
    municipality == "AMECA" ~ 14006,
    municipality == "ANTONIO ESCOBEDO" ~ 14007,
    municipality == "ARANDAS" ~ 14008,
    municipality == "ATEMAJAC DE BRIZUELA" ~ 14010,
    municipality == "ATENGO" ~ 14011,
    municipality == "ATENGUILLO" ~ 14012,
    municipality == "ATOTONILCO EL ALTO" ~ 14013,
    municipality == "ATOYAC" ~ 14014,
    municipality == "AUTLAN DE NAVARRO" ~ 14015,
    municipality == "AYOTLAN" ~ 14016,
    municipality == "AYUTLA" ~ 14017,
    municipality == "BOLANOS" ~ 14019,
    municipality == "CABO CORRIENTES" ~ 14020,
    municipality == "CANADAS DE OBREGON" ~ 14117,
    municipality == "CASIMIRO CASTILLO" ~ 14021,
    municipality == "CHAPALA" ~ 14030,
    municipality == "CHIMALTITAN" ~ 14031,
    municipality == "CHIQUILISTLAN" ~ 14032,
    municipality == "CIHUATLAN" ~ 14022,
    municipality == "CIUDAD GUZMAN" ~ 14023,
    municipality == "COCULA" ~ 14024,
    municipality == "COLOTLAN" ~ 14025,
    municipality == "CONCEPCION DE BUENOS AIRES" ~ 14026,
    municipality == "CUAUTITLAN DE GARCIA BARRAGAI" | municipality =="CUAUTITLAN" | municipality =="CUAUTITLAN DE GARCIA BARRAGAN" ~ 14027,
    municipality == "CUAUTLA" ~ 14028,
    municipality == "CUQUIO" ~ 14029,
    municipality == "DEGOLLADO" ~ 14033,
    municipality == "EJUTLA" ~ 14034,
    municipality == "EL ARENAL" ~ 14009,
    municipality == "EL GRULLO" ~ 14037,
    municipality == "EL LIMON" ~ 14054,
    municipality == "EL SALTO" ~ 14070,
    municipality == "ENCARNACION DE DIAZ" ~ 14035,
    municipality == "ETZATLAN" ~ 14036,
    municipality == "GOMEZ FARIAS" ~ 14079,
    municipality == "GUACHINANGO" ~ 14038,
    municipality == "GUADALAJARA" ~ 14039,
    municipality == "HOSTOTIPAQUILLO" ~ 14040,
    municipality == "HUEJUCAR" ~ 14041,
    municipality == "HUEJUQUILLA EL ALTO" ~ 14042,
    municipality == "IXTLAHUACAN DE LOS MEMBRILLO" | municipality =="IXTLAHUACAN DE LOS MEMBRILLOS" ~ 14044,
    municipality == "IXTLAHUACAN DEL RIO" ~ 14045,
    municipality == "JALOSTOTITLAN" ~ 14046,
    municipality == "JAMAY" ~ 14047,
    municipality == "JESUS MARIA" ~ 14048,
    municipality == "JILOTLAN DE LOS DOLORES" ~ 14049,
    municipality == "JOCOTEPEC" ~ 14050,
    municipality == "JUANACATLAN" ~ 14051,
    municipality == "JUCHITLAN" ~ 14052,
    municipality == "LA BARCA" ~ 14018,
    municipality == "LA HUERTA" ~ 14043,
    municipality == "MANZANILLA DE LA PAZ" ~ 14057,
    municipality == "LAGOS DE MORENO" ~ 14053,
    municipality == "MAGDALENA" ~ 14055,
    municipality == "MANUEL M.DIEGUEZ" ~ 14056,
    municipality == "MASCOTA" ~ 14058,
    municipality == "MAZAMITLA" ~ 14059,
    municipality == "MEXTICACAN" ~ 14060,
    municipality == "MEZQUITIC" ~ 14061,
    municipality == "MIXTLAN" ~ 14062,
    municipality == "OCOTLAN" ~ 14063,
    municipality == "OJUELOS DE JALISCO" ~ 14064,
    municipality == "PIHUAMO" ~ 14065,
    municipality == "PONCITLAN" ~ 14066,
    municipality == "PUERTO VALLARTA" ~ 14067,
    municipality == "QUITUPAN" ~ 14069,
    municipality == "SAN CRISTOBAL DE LA BARRANCA" ~ 14071,
    municipality == "SAN DIEGO DE ALEJANDRIA" ~ 14072,
    municipality == "SAN GABRIEL" ~ 14113,
    municipality == "SAN IGNACIO CERRO GORDO" ~ 14125,
    municipality == "SAN JUAN DE LOS LAGOS" ~ 14073,
    municipality == "SAN JUANITO DE ESCOBEDO" ~ 14007,
    municipality == "SAN JULIAN" ~ 14074,
    municipality == "SAN MARCOS" ~ 14075,
    municipality == "SAN MARTIN DE BOLANOS" ~ 14076,
    municipality == "SAN MARTIN HIDALGO" ~ 14077,
    municipality == "SAN MIGUEL EL ALTO" ~ 14078,
    municipality == "SAN SEBASTIAN DEL OESTE" ~ 14080,
    municipality == "SANTA MARIA DE LOS ANGELES" ~ 14081,
    municipality == "SANTA MARIA DEL ORO" ~ 14056,
    municipality == "SAYULA" ~ 14082,
    municipality == "TALA" ~ 14083,
    municipality == "TALPA DE ALLENDE" ~ 14084,
    municipality == "TAMAZULA DE GORDIANO" ~ 14085,
    municipality == "TAPALPA" ~ 14086,
    municipality == "TECALITLAN" ~ 14087,
    municipality == "TECHALUTA DE MONTENEGRO" ~ 14089,
    municipality == "TECOLOTLAN" ~ 14088,
    municipality == "TENAMAXTLAN" ~ 14090,
    municipality == "TEOCALTICHE" ~ 14091,
    municipality == "TEOCUITATLAN DE CORONA" ~ 14092,
    municipality == "TEPATITLAN DE MORELOS" ~ 14093,
    municipality == "TEQUILA" ~ 14094,
    municipality == "TEUCHITLAN" ~ 14095,
    municipality == "TIZAPAN EL ALTO" ~ 14096,
    municipality == "TLAJOMULCO DE ZUNIGA" ~ 14097,
    municipality == "TLAQUEPAQUE" | municipality =="SAN PEDRO TLAQUEPAQUE" ~ 14098,
    municipality == "TOLIMAN" ~ 14099,
    municipality == "TOMATLAN" ~ 14100,
    municipality == "TONALA" ~ 14101,
    municipality == "TONAYA" ~ 14102,
    municipality == "TONILA" ~ 14103,
    municipality == "TOTATICHE" ~ 14104,
    municipality == "TOTOTLAN" ~ 14105,
    municipality == "TUXCACUESCO" ~ 14106,
    municipality == "TUXCUECA" ~ 14107,
    municipality == "TUXPAN" ~ 14108,
    municipality == "UNION DE SAN ANTONIO" ~ 14109,
    municipality == "UNION DE TULA" ~ 14110,
    municipality == "VALLE DE GUADALUPE" ~ 14111,
    municipality == "VALLE DE JUAREZ" ~ 14112,
    municipality == "VILLA CORONA" ~ 14114,
    municipality == "VILLA GUERRERO" ~ 14115,
    municipality == "VILLA HIDALGO" ~ 14116,
    municipality == "VILLA OBREGON" ~ 14117,
    municipality == "VILLA PURIFICACION" ~ 14068,
    municipality == "YAHUALICA DE GONZALEZ GALLO" ~ 14118,
    municipality == "ZACOALCO DE TORRES" ~ 14119,
    municipality == "ZAPOPAN" ~ 14120,
    municipality == "ZAPOTILTIC" ~ 14121,
    municipality == "ZAPOTITLAN DE VADILLO" ~ 14122,
    municipality == "ZAPOTLAN DEL REY" ~ 14123,
    municipality == "ZAPOTLAN EL GRANDE" ~ 14023,
    municipality == "ZAPOTLANEJO" ~ 14124,
    TRUE ~ uniqueid
  ))

#-------------------------------------------------------------
# 11. Rename NAL to PANAL, COALPRIPVEM to PRI_PVEM, COALPTMC to PT_PC, Boletas to listanominal
#   gen total = Validos + NULOS + NO_REGISTRADO
#-------------------------------------------------------------
data_2012 <- data_2012 %>%
  rename(PANAL = nal,
         PRI_PVEM = coalpripvem,
         PT_PC = coalptmc,
         listanominal = boletas,
         PAN = pan,
         PRD = prd,) %>%
  mutate(total = validos + nulos + no_registrado)

#-------------------------------------------------------------
# 7. gen turnout = total/listanominal
#-------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(turnout = total / listanominal)

#-------------------------------------------------------------
# 10. egen valid = rowtotal(PAN PRD PANAL PRI_PVEM PT_PC)
#-------------------------------------------------------------
valid_vars <- c("pan","prd","PANAL","PRI_PVEM","PT_PC")
valid_vars <- intersect(valid_vars, names(data_2012))

data_2012 <- data_2012 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

#-------------------------------------------------------------
# 11. sort section
#-------------------------------------------------------------
data_2012 <- data_2012 %>% arrange(section)

#-------------------------------------------------------------
# 12. duplicates tag section, g(tag)
#    drop if tag>0
# In R:
data_2012 <- data_2012 %>%
  group_by(section) %>%
  mutate(tag = n()) %>%
  ungroup() %>%
  filter(tag == 1) %>%
  select(-tag)

#-------------------------------------------------------------
# 13. gen year=2012, gen month="July"
#-------------------------------------------------------------
data_2012 <- data_2012 %>%
  mutate(year = 2012,
         month = "July")

#-------------------------------------------------------------
#2015
#-------------------------------------------------------------

# Import Excel for 2015
data_2015 <- read_excel("../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/ResultadosPorCasilla2015.xlsx", 
                        sheet="RESULTADOS", 
                        guess_max=100000)

names(data_2015) <- toupper(names(data_2015))

# Rename Municipio and Seccion
data_2015 <- data_2015 %>%
  rename(MUNICIPIO = MUNICIPIO,
         SECCION = SECCIÓN)

# Replace "NULL" with ""
data_2015 <- data_2015 %>%
  mutate(NULOS = if_else(NULOS=="NULL","",NULOS),
         NOREGISTRADOS = if_else(NOREGISTRADOS=="NULL","",NOREGISTRADOS))

# Keep only Elección=="Municipes"
data_2015 <- data_2015 %>% filter(ELECCIÓN=="Municipes")

# drop columns not for municipalties


vars_to_drop <- c("JOSE PEDRO KUMAMOTO AGUILAR",
                  "JESÚS OSWALDO SILVA MAGAÑA",
                  "JOSE ZEPEDA CONTRERAS",
                  "JOSE FRANCISCO SANCHEZ PEREZ",
                  "GUILLERMO SIENFUEGOS PEREZ",
                  "BOLETAS")

vars_to_drop <- intersect(vars_to_drop, names(data_2015))

data_2015 <- data_2015 %>% select(-all_of(vars_to_drop))

votos_vars <- grep("^Votos", names(data_2015), value=TRUE)
if(length(votos_vars)>0) data_2015 <- data_2015 %>% select(-all_of(votos_vars))

names(data_2015)
# Replace PRI_PVEM=... etc. for coalitions
# Omit if these variables do not exist. Adjust code accordingly.

if("COAL PRIPVEM" %in% names(data_2015) && "PRI_PVEM" %in% names(data_2015)) {
  data_2015 <- data_2015 %>%
    mutate(PRI_PVEM = if_else(`COAL PRIPVEM`==1, PRI_PVEM+PRI+PVEM, PRI_PVEM),
           PRI = if_else(`COAL PRIPVEM`==1, 0, PRI),
           PVEM = if_else(`COAL PRIPVEM`==1, 0, PVEM))
}

if("COAL PANPRD" %in% names(data_2015) && "PAN_PRD" %in% names(data_2015)) {
  data_2015 <- data_2015 %>%
    mutate(PAN_PRD = if_else(`COAL PANPRD`==1, PAN_PRD+PAN+PRD, PAN_PRD),
           PAN = if_else(`COAL PANPRD`==1,0,PAN),
           PRD = if_else(`COAL PANPRD`==1,0,PRD))
}
names(data_2015)
# rename Nulos nulo and NoRegistrados no_reg
data_2015 <- data_2015 %>%
  rename(nulo=NULOS, 
         no_reg=NOREGISTRADOS, 
         PANAL=`NA`)

# collapse (sum) PAN-CI_1 (first) MUNICIPIO uniqueid , by ( SECCION )
# Adjust variables from PAN to CI_1 if they exist
# Just example: if CI_1 not defined, do from PAN to no_reg or from PAN to no_reg. Identify the range:
all_cols <- names(data_2015)
start_var <- "PAN"
end_var <- "no_reg"
end_pos <- match("no_reg", all_cols)
start_pos <- match("PAN", all_cols)
vars_to_sum <- all_cols[start_pos:end_pos]
vars_to_first <- c("MUNICIPIO","uniqueid") %>% intersect(all_cols)

data_2015$nulo <- as.numeric(data_2015$nulo)
data_2015$no_reg <- as.numeric(data_2015$no_reg)

data_2015 <- data_2015 %>%
  group_by(UNIQUEID,SECCION) %>%
  summarize(across(all_of(vars_to_sum), sum, na.rm=TRUE),
            across(all_of(vars_to_first), ~first(.x)),
            .groups="drop")

# egen valid=rowtotal(...)
valid_vars <- c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","MORENA","PES","PH","PAN_PRD","PRI_PVEM")
valid_vars <- intersect(valid_vars, names(data_2015))
data_2015 <- data_2015 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)),na.rm=TRUE))

# gen total = valid+ nulo + no_reg
data_2015 <- data_2015 %>%
  mutate(total = valid + nulo + no_reg)

# Omit mun_ aggregation, rowranks, and winner steps

data_2015 <- data_2015 %>%
  mutate(year=2015,
         month="June",
         STATE="JALISCO")

data_2015 <- data_2015 %>%
  rename(municipality=MUNICIPIO,
         section=SECCION,
         uniqueid=UNIQUEID)

summary(data_2015)

# Just proceed with the LN 2015 merge and corrections

data_2015 <- data_2015 %>%
  mutate(section = ifelse((uniqueid==14098 & section==995) | 
                            (uniqueid==14098 & section==1548) |
                             (uniqueid==14101 & section==2024) | 
                            (uniqueid==14101 & section==2025) |
                             (uniqueid==14044 & section==2462) | 
                            (uniqueid==14039 & (section==2484 |
                                                  section==2485 | section==2486)) |
                             (uniqueid==14039 & section==2484) | (uniqueid==14101 & section==2563) |
                             (uniqueid==14098 & section==2720) | (uniqueid==14098 & section==2721) |
                             (uniqueid==14070 & section==2729) | (uniqueid==14098 & section==3206),
                           NA, section))


#Lista Nominal

lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2015/LN2015.dta") %>% 
  filter(month == 6 &
          entidad ==14) %>%
  mutate(uniqueid = (entidad*1000)+municipio) %>%
  filter(seccion!=0) %>%
  arrange(uniqueid,seccion) %>%
  rename(section=seccion) %>%
  select(uniqueid, section, lista)

data_2015 <- data_2015 %>%
  filter(!is.na(section)) %>%
  left_join(lista_nominal, by = c("section"))

data_2015 <- data_2015 %>% rename(listanominal = lista)

data_2015 <- data_2015 %>%
  mutate(turnout=total/listanominal)


data_2015 <- data_2015 %>%
  relocate(STATE, municipality, uniqueid, section, year, month)

data_2015 <- data_2015 %>% arrange(uniqueid, section)

# ------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------
rm(list = ls())
cat("\014")
options(max.print = 5000, scipen=10)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, haven, readxl, stringr, tidyr, data.table)

# ------------------------------------------------------------------
# 1. IMPORT EXCEL DATA 2018
# ------------------------------------------------------------------
data_2018 <- read_excel(
  "../../../Data/Raw Electoral Data/Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2018.xlsx", 
  sheet = "Ayuntamientos",
  guess_max = 100000  # to handle large sheets
)

# ------------------------------------------------------------------
# 2. DATA CLEANING & BASIC MANIPULATIONS
# ------------------------------------------------------------------
# drop if MUNICIPIO=="MUNICIPIO"
data_2018 <- data_2018 %>%
  filter(MUNICIPIO != "MUNICIPIO")

# drop CASILLA column
if ("CASILLA" %in% names(data_2018)) {
  data_2018 <- data_2018 %>% select(-CASILLA)
}

if ("poll_station" %in% names(data_2018)) {
  data_2018 <- data_2018 %>% rename(CASILLA = poll_station)
}

# gen section=substr(CASILLA,1,4)
# Make sure CASILLA is string
data_2018 <- data_2018 %>%
  mutate(CASILLA = as.character(CASILLA),
         section = substr(CASILLA, 1, 4))

# destring *, replace
# Convert all remaining non-ID columns to numeric if possible
data_2018 <- data_2018 %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.)))))

names(data_2018) <- str_replace_all(names(data_2018), "-", "_")
# ------------------------------------------------------------------
# 3. CREATE & ADJUST COALITION VARIABLES
# ------------------------------------------------------------------
# g PT_MORENA_PES = .
# replace PT_MORENA_PES = PESPTMORENA + PTMORENA + PTPES + MORENAPES + PT + MORENA + PES if coalpbt==1
if(!"PT_MORENA_PES" %in% names(data_2018)) {
  data_2018 <- data_2018 %>% mutate(PT_MORENA_PES = NA_real_)
}

data_2018 <- data_2018 %>%
  mutate(
    PT_MORENA_PES = if_else(
      !is.na(coalpbt) & coalpbt==1,
      (coalesce(PES_PT_MORENA,0) + 
         coalesce(PT_MORENA,0) + 
         coalesce(PT_PES,0) + 
         coalesce(MORENA_PES,0)
       + coalesce(PT,0) + coalesce(MORENA,0) + coalesce(PES,0)),
      PT_MORENA_PES
    ),
    PT = if_else(!is.na(coalpbt) & coalpbt==1, NA_real_, PT),
    MORENA = if_else(!is.na(coalpbt) & coalpbt==1, NA_real_, MORENA),
    PES = if_else(!is.na(coalpbt) & coalpbt==1, NA_real_, PES)
  ) %>%
  select(-PES_PT_MORENA, -PT_MORENA, -PT_PES, -MORENA_PES)

# g PAN_PRD_MC = .
# replace PAN_PRD_MC = PANPRDMC + PANPRD + PANMC + PRDMC + PAN + PRD + MC if coaljf==1
if(!"PAN_PRD_MC" %in% names(data_2018)) {
  data_2018 <- data_2018 %>% mutate(PAN_PRD_MC = NA_real_)
}

data_2018 <- data_2018 %>%
  mutate(
    PAN_PRD_MC = if_else(
      !is.na(coaljf) & coaljf==1,
      (coalesce(PAN_PRD_MC,0) + 
         coalesce(PAN_PRD,0) + 
         coalesce(PAN_MC,0) + 
         coalesce(PRD_MC,0)
       + coalesce(PAN,0) + coalesce(PRD,0) + coalesce(MC,0)),
      PAN_PRD_MC
    ),
    PAN = if_else(!is.na(coaljf) & coaljf==1, NA_real_, PAN),
    PRD = if_else(!is.na(coaljf) & coaljf==1, NA_real_, PRD),
    MC  = if_else(!is.na(coaljf) & coaljf==1, NA_real_, MC)
  ) %>%
  select(-PAN_PRD_MC, -PAN_PRD, -PAN_MC, -PRD_MC)

# ------------------------------------------------------------------
# 4. RENAME & COLLAPSE
# ------------------------------------------------------------------
# rename NOREGISTRADOS no_reg
# rename VOTOSNULOS nulo
# rename MUNICIPIO municipality
data_2018 <- data_2018 %>%
  rename(
    no_reg = `NO REGISTRADOS`,
    nulo = `VOTOS NULOS`,
    municipality = MUNICIPIO
  )


data_2018 <- data_2018 %>%
  group_by(municipality,uniqueid,section) %>%
  summarize(
    across(PAN:PT_MORENA_PES, sum, na.rm=TRUE),
    .groups="drop"
  )

# rename NA -> PANAL if needed
if("NA" %in% names(data_2018)) {
  data_2018 <- data_2018 %>% rename(PANAL = `NA`)
}

# ------------------------------------------------------------------
# 5. CREATE valid, total
# ------------------------------------------------------------------
# egen valid=rowtotal(PAN PRI PRD PVEM PT MC PANAL MORENA PES PT_MORENA_PES PAN_PRD_MC CI_1 CI_2 CI_3 CI_4 CI_5 )
valid_vars <- c("PAN","PRI","PRD","PVEM","PT","MC","PANAL","MORENA","PES",
                "PT_MORENA_PES","PAN_PRD_MC","CI_1","CI_2","CI_3","CI_4","CI_5")
valid_vars <- intersect(valid_vars, names(data_2018))

data_2018 <- data_2018 %>%
  mutate(valid = rowSums(across(all_of(valid_vars)), na.rm=TRUE))

# gen total=valid+ nulo + no_reg
data_2018 <- data_2018 %>%
  mutate(total = valid + coalesce(nulo,0) + coalesce(no_reg,0))

# ------------------------------------------------------------------
# 9. MERGE LISTADO NOMINAL (LN) for 2018
# ------------------------------------------------------------------
#Lista Nominal

lista_nominal <- read.dta13("../../../Data/Raw Electoral Data/Listas Nominales/ListadoNominalPREP2018.dta")  %>%
  filter(STATE=="JALISCO")

data_2018 <- data_2018 %>%
  filter(!is.na(section)) %>%
  left_join(lista_nominal, by = c("section"))

data_2018 <- data_2018 %>% rename(listanominal = ListadoNominalINE)

data_2018 <- data_2018 %>%
  mutate(turnout=total/listanominal)


# Omit municipal aggregator steps (mun_listanominal, mun_turnout)

# ------------------------------------------------------------------
# 10. SAVE SECTION-LEVEL DATA
# ------------------------------------------------------------------
# Combine the dataframes, handling different columns by filling with NA
jalisco_all <- bind_rows(data_1997,
                            data_2000,
                            data_2003,
                            data_2006,
                            data_2009,
                            data_2012,
                            data_2015,
                            data_2018)

data.table::fwrite(jalisco_all,"../../../Processed Data/jalisco/jalisco_process_raw_data.csv")








