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

##############################################################################
# ============== 1) Ayu_Seccion_1997_No_LN.csv ==============================
##############################################################################

# Equivalent to: insheet using Ayu_Seccion_1997_No_LN.csv, clear
df_1997 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997_No_LN.csv") 
colnames(df_1997) <- tolower(colnames(df_1997))

df_1997 <-df_1997 %>%
  rename(
    municipality = municipio,
    section      = seccion
    # listanominal = nominal  # (commented out because it’s commented in Stata)
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

# Convert pan-through-total to numeric
num_vars <- c("pan","pri","prd","pt","pvem","pfcrn","pps","pdm","npp","nulos","noreg","total")
for (v in num_vars) {
  if (v %in% names(df_1997)) {
    df_1997[[v]] <- as.numeric(df_1997[[v]])
  }
}

# In Stata: collapse (sum) pan - total, by(municipality section) => omitted aggregator
# So we skip aggregator. We keep the data as is.

# rename pan->PAN, pri->PRI, etc.
df_1997 <- df_1997 %>%
  rename(
    PAN  = pan,
    PRI  = pri,
    PRD  = prd,
    PT   = pt,
    PVEM = pvem,
    PartCardenista = pfcrn,
    PPS  = pps,
    PD   = pdm,  # 'PD' from 'pdm'
    NPP  = npp
  ) %>%
  select(-nulos, -noreg)  # drop nulos, noreg

df_1997 <- df_1997 %>%
  mutate(
    uniqueid = case_when(
      municipality == "AHUALULCO"                ~ 24001,
      municipality == "ALAQUINES"               ~ 24002,
      municipality == "AQUISMON"                ~ 24003,
      municipality == "ARMADILLO DE LOS INFANTE" ~ 24004,
      municipality == "AXTLA DE TERRAZAS"       ~ 24053,
      municipality == "CARDENAS"               ~ 24005,
      municipality == "CATORCE"                ~ 24006,
      municipality == "CEDRAL"                 ~ 24007,
      municipality == "CERRITOS"               ~ 24008,
      municipality == "CERRO DE SAN PEDRO"     ~ 24009,
      municipality == "CHARCAS"                ~ 24015,
      municipality == "CIUDAD DEL MAIZ"        ~ 24010,
      municipality == "CIUDAD FERNANDEZ"       ~ 24011,
      municipality == "CIUDAD VALLES"          ~ 24013,
      municipality == "COXCATLAN"              ~ 24014,
      municipality == "EBANO"                  ~ 24016,
      municipality == "EL NARANJO"             ~ 24058,
      municipality == "GUADALCAZAR"            ~ 24017,
      municipality == "HUEHUETLAN"             ~ 24018,
      municipality == "LAGUNILLAS"             ~ 24019,
      municipality == "MATEHUALA"              ~ 24020,
      municipality == "MATLAPA"                ~ 24057,
      municipality == "MEXQUITIC DE CARMONA"   ~ 24021,
      municipality == "MOCTEZUMA"              ~ 24022,
      municipality == "RAYON"                  ~ 24023,
      municipality == "RIOVERDE"               ~ 24024,
      municipality == "SALINAS"                ~ 24025,
      municipality == "SAN ANTONIO"            ~ 24026,
      municipality == "SAN CIRO DE ACOSTA"     ~ 24027,
      municipality == "SAN LUIS POTOSI"        ~ 24028,
      municipality == "SAN MARTIN CHALCHICUAUTLA" ~ 24029,
      municipality == "SAN NICOLAS TOLENTINO"  ~ 24030,
      municipality == "SAN VICENTE TANCUAYALAB" ~ 24034,
      municipality == "SANTA CATARINA"         ~ 24031,
      municipality == "SANTA MARIA DEL RIO"    ~ 24032,
      municipality == "SANTO DOMINGO"          ~ 24033,
      municipality == "SOLEDAD DE GRACIANO SANCHEZ" ~ 24035,
      municipality == "TAMASOPO"               ~ 24036,
      municipality == "TAMAZUNCHALE"           ~ 24037,
      municipality == "TAMPACAN"               ~ 24038,
      municipality == "TAMPAMOLON CORONA"      ~ 24039,
      municipality == "TAMUIN"                 ~ 24040,
      municipality == "TANCANHUITZ"            ~ 24012,
      municipality == "TANLAJAS"               ~ 24041,
      municipality == "TANQUIAN DE ESCOBEDO"   ~ 24042,
      municipality == "TIERRANUEVA"            ~ 24043,
      municipality == "VANEGAS"                ~ 24044,
      municipality == "VENADO"                 ~ 24045,
      municipality == "VILLA DE ARISTA"        ~ 24056,
      municipality == "VILLA DE ARRIAGA"       ~ 24046,
      municipality == "VILLA DE GUADALUPE"     ~ 24047,
      municipality == "VILLA DE LA PAZ"        ~ 24048,
      municipality == "VILLA DE RAMOS"         ~ 24049,
      municipality == "VILLA DE REYES"         ~ 24050,
      municipality == "VILLA HIDALGO"          ~ 24051,
      municipality == "VILLA JUAREZ"           ~ 24052,
      municipality == "XILITLA"                ~ 24054,
      municipality == "ZARAGOZA"               ~ 24055,
      TRUE ~ 0
    )
  )

# Lines merging with all_months_years.dta
df_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta") %>%
  select(ed, seccion, month, year, lista)

df_1997 <- df_1997 %>%
  mutate(ed = 24, seccion = section) %>%
  left_join(df_all, by = c("ed","seccion")) %>%
  filter(month == 7, year == 1997) %>%
  filter(!is.na(lista)) %>%
  select(-ed, -seccion, -year, -month)

# rename lista -> listanominal
df_1997 <- df_1997 %>%
  rename(listanominal = lista)

# turnout = total / listanominal
df_1997 <- df_1997 %>%
  mutate(turnout = total / listanominal)

df_1997 <- df_1997 %>%
  mutate(
    year  = 1997,
    month = "July"
  )

##############################################################################
# ============== 3) Ayu_Seccion_2000.csv  ====================================
##############################################################################
df_2000 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2000.csv") %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

# destring pan - listanominal
vars_2000 <- c("pan","pri","prd","pt","pvem","pcd","parm","pas","pds","npp","psn","pcp","axslp","cfcp","nulos","noreg","total","listanominal")
for (v in vars_2000) {
  if (v %in% names(df_2000)) {
    df_2000[[v]] <- as.numeric(df_2000[[v]])
  }
}

# aggregator => omitted
# rename
df_2000 <- df_2000 %>%
  rename(
    PAN = pan,
    PRI = pri,
    PRD = prd,
    PT  = pt,
    PVEM= pvem,
    PCD = pcd,
    PARM= parm,
    PAS = pas,
    PDS = pds,
    NPP = npp,
    PSN = psn,
    PCP = pcp,
    PRD_PT_PCD_PSN = axslp,  # from axslp
    PVEM_PCP       = cfcp
  ) %>%
  mutate(turnout = total / listanominal) %>%
  select(-nulos, -noreg)

#party vote validation

df_2000 <- df_2000 %>% 
  dplyr::mutate(PVEM = ifelse(!is.na(PVEM_PCP) & PVEM_PCP>0 & PVEM >0, 0, PVEM),
                PCP = ifelse(!is.na(PVEM_PCP) & PVEM_PCP>0 & PCP >0, 0, PCP))

# uniqueid
df_2000 <- df_2000 %>%
  uniqueid = case_when(
    municipality == "AHUALULCO"                ~ 24001,
    municipality == "ALAQUINES"               ~ 24002,
    municipality == "AQUISMON"                ~ 24003,
    municipality == "ARMADILLO DE LOS INFANTE" ~ 24004,
    municipality == "AXTLA DE TERRAZAS"       ~ 24053,
    municipality == "CARDENAS"               ~ 24005,
    municipality == "CATORCE"                ~ 24006,
    municipality == "CEDRAL"                 ~ 24007,
    municipality == "CERRITOS"               ~ 24008,
    municipality == "CERRO DE SAN PEDRO"     ~ 24009,
    municipality == "CHARCAS"                ~ 24015,
    municipality == "CIUDAD DEL MAIZ"        ~ 24010,
    municipality == "CIUDAD FERNANDEZ"       ~ 24011,
    municipality == "CIUDAD VALLES"          ~ 24013,
    municipality == "COXCATLAN"              ~ 24014,
    municipality == "EBANO"                  ~ 24016,
    municipality == "EL NARANJO"             ~ 24058,
    municipality == "GUADALCAZAR"            ~ 24017,
    municipality == "HUEHUETLAN"             ~ 24018,
    municipality == "LAGUNILLAS"             ~ 24019,
    municipality == "MATEHUALA"              ~ 24020,
    municipality == "MATLAPA"                ~ 24057,
    municipality == "MEXQUITIC DE CARMONA"   ~ 24021,
    municipality == "MOCTEZUMA"              ~ 24022,
    municipality == "RAYON"                  ~ 24023,
    municipality == "RIOVERDE"               ~ 24024,
    municipality == "SALINAS"                ~ 24025,
    municipality == "SAN ANTONIO"            ~ 24026,
    municipality == "SAN CIRO DE ACOSTA"     ~ 24027,
    municipality == "SAN LUIS POTOSI"        ~ 24028,
    municipality == "SAN MARTIN CHALCHICUAUTLA" ~ 24029,
    municipality == "SAN NICOLAS TOLENTINO"  ~ 24030,
    municipality == "SAN VICENTE TANCUAYALAB" ~ 24034,
    municipality == "SANTA CATARINA"         ~ 24031,
    municipality == "SANTA MARIA DEL RIO"    ~ 24032,
    municipality == "SANTO DOMINGO"          ~ 24033,
    municipality == "SOLEDAD DE GRACIANO SANCHEZ" ~ 24035,
    municipality == "TAMASOPO"               ~ 24036,
    municipality == "TAMAZUNCHALE"           ~ 24037,
    municipality == "TAMPACAN"               ~ 24038,
    municipality == "TAMPAMOLON CORONA"      ~ 24039,
    municipality == "TAMUIN"                 ~ 24040,
    municipality == "TANCANHUITZ"            ~ 24012,
    municipality == "TANLAJAS"               ~ 24041,
    municipality == "TANQUIAN DE ESCOBEDO"   ~ 24042,
    municipality == "TIERRANUEVA"            ~ 24043,
    municipality == "VANEGAS"                ~ 24044,
    municipality == "VENADO"                 ~ 24045,
    municipality == "VILLA DE ARISTA"        ~ 24056,
    municipality == "VILLA DE ARRIAGA"       ~ 24046,
    municipality == "VILLA DE GUADALUPE"     ~ 24047,
    municipality == "VILLA DE LA PAZ"        ~ 24048,
    municipality == "VILLA DE RAMOS"         ~ 24049,
    municipality == "VILLA DE REYES"         ~ 24050,
    municipality == "VILLA HIDALGO"          ~ 24051,
    municipality == "VILLA JUAREZ"           ~ 24052,
    municipality == "XILITLA"                ~ 24054,
    municipality == "ZARAGOZA"               ~ 24055,
    TRUE ~ 0
  )


df_2000 <- df_2000 %>%
  mutate(
    year  = 2000,
    month = "July"
  )


##############################################################################
# ============== 4) Ayu_Seccion_2003.csv  ====================================
##############################################################################
df_2003 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2003.csv") 
colnames(df_2003) <- tolower(colnames(df_2003))

df_2003 <-df_2003 %>%
  rename(municipality = municipio, section = seccion) %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

vars_2003 <- c("listanominal","nulos","noreg","pan","pri","prd","pt",
               "pvem","pcp","pas","psn","pc","sumacc1","cc1","sumacc2","cc2",
               "total","coalicion1","coalicion2")
for (v in vars_2003) {
  if (v %in% names(df_2003)) {
    df_2003[[v]] <- as.numeric(df_2003[[v]])
  }
}

# aggregator => omitted
# The block with gen PRI_PVEM_PCP..., merges, etc. => we keep the merges of variables but skip aggregator steps.
# Eventually we do rename at the bottom:
df_2003 <- df_2003 %>%
  # Do the coalition logic => okay to keep the recodes that reassign variables
  mutate(turnout = total/listanominal) %>%
  select(-nulos, -noreg)

# uniqueid
df_2003 <- df_2003 %>%
  mutate(
    uniqueid = case_when(
      str_detect(municipality, "AHUALULCO") ~ 24001,
      str_detect(municipality, "ALAQUINES") ~ 24002,
      str_detect(municipality, "AQUISMON") ~ 24003,
      str_detect(municipality, "ARMADILLO DE LOS INFANTE") ~ 24004,
      str_detect(municipality, "AXTLA DE TERRAZAS") ~ 24053,
      str_detect(municipality, "CARDENAS") ~ 24005,
      str_detect(municipality, "CATORCE") ~ 24006,
      str_detect(municipality, "CEDRAL") ~ 24007,
      str_detect(municipality, "CERRITOS") ~ 24008,
      str_detect(municipality, "CERRO DE SAN PEDRO") ~ 24009,
      str_detect(municipality, "CHARCAS") ~ 24015,
      str_detect(municipality, "CIUDAD DEL MAIZ") ~ 24010,
      str_detect(municipality, "CIUDAD FERNANDEZ") ~ 24011,
      str_detect(municipality, "CIUDAD VALLES") ~ 24013,
      str_detect(municipality, "COXCATLAN") ~ 24014,
      str_detect(municipality, "EBANO") ~ 24016,
      str_detect(municipality, "EL NARANJO") ~ 24058,
      str_detect(municipality, "GUADALCAZAR") ~ 24017,
      str_detect(municipality, "HUEHUETLAN") ~ 24018,
      str_detect(municipality, "LAGUNILLAS") ~ 24019,
      str_detect(municipality, "MATEHUALA") ~ 24020,
      str_detect(municipality, "MATLAPA") ~ 24057,
      str_detect(municipality, "MEXQUITIC DE CARMONA") ~ 24021,
      str_detect(municipality, "MOCTEZUMA") ~ 24022,
      str_detect(municipality, "RAYON") ~ 24023,
      str_detect(municipality, "RIOVERDE") ~ 24024,
      str_detect(municipality, "SALINAS") ~ 24025,
      str_detect(municipality, "SAN ANTONIO") ~ 24026,
      str_detect(municipality, "SAN CIRO DE ACOSTA") ~ 24027,
      str_detect(municipality, "SAN LUIS POTOSI") ~ 24028,
      str_detect(municipality, "SAN MARTIN CHALCHICUAUTLA") ~ 24029,
      str_detect(municipality, "SAN NICOLAS TOLENTINO") ~ 24030,
      str_detect(municipality, "SAN VICENTE TANCUAYALAB") ~ 24034,
      str_detect(municipality, "SANTA CATARINA") ~ 24031,
      str_detect(municipality, "SANTA MARIA DEL RIO") ~ 24032,
      str_detect(municipality, "SANTO DOMINGO") ~ 24033,
      str_detect(municipality, "SOLEDAD DE GRACIANO SANCHEZ") ~ 24035,
      str_detect(municipality, "TAMASOPO") ~ 24036,
      str_detect(municipality, "TAMAZUNCHALE") ~ 24037,
      str_detect(municipality, "TAMPACAN") ~ 24038,
      str_detect(municipality, "TAMPAMOLON CORONA") ~ 24039,
      str_detect(municipality, "TAMUIN") ~ 24040,
      str_detect(municipality, "TANCANHUITZ") ~ 24012,
      str_detect(municipality, "TANLAJAS") ~ 24041,
      str_detect(municipality, "TANQUIAN DE ESCOBEDO") ~ 24042,
      str_detect(municipality, "TIERRA NUEVA") ~ 24043,
      str_detect(municipality, "VANEGAS") ~ 24044,
      str_detect(municipality, "VENADO") ~ 24045,
      str_detect(municipality, "VILLA DE ARISTA") ~ 24056,
      str_detect(municipality, "VILLA DE ARRIAGA") ~ 24046,
      str_detect(municipality, "VILLA DE GUADALUPE") ~ 24047,
      str_detect(municipality, "VILLA DE LA PAZ") ~ 24048,
      str_detect(municipality, "VILLA DE RAMOS") ~ 24049,
      str_detect(municipality, "VILLA DE REYES") ~ 24050,
      str_detect(municipality, "VILLA HIDALGO") ~ 24051,
      str_detect(municipality, "VILLA JUAREZ") ~ 24052,
      str_detect(municipality, "XILITLA") ~ 24054,
      str_detect(municipality, "ZARAGOZA") ~ 24055,
      TRUE ~ 0
    )
  )

# aggregator => omitted
# rowranks => omitted
# winner => omitted

df_2003 <- df_2003 %>%
  mutate(
    year  = 2003,
    month = "October"
  )

##############################################################################
# ============== 6) Ayu_Seccion_2006.csv  ====================================
##############################################################################
df_2006 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2006.csv") %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

vars_2006 <- c("listanominal","nulos","pan","pri","prdptconvergencia","pvem",
               "pcp","pna","pasc","panpna","panpcppna","pcppna","pripasc",
               "pripcppasc","total","noreg")
for (v in vars_2006) {
  if (v %in% names(df_2006)) {
    df_2006[[v]] <- as.numeric(df_2006[[v]])
  }
}

# aggregator => omitted
# rename
df_2006 <- df_2006 %>%
  mutate(turnout = total/listanominal) %>%
  select(-nulos, -noreg)

df_2006 <- df_2006 %>%
  mutate(
    uniqueid = case_when(
      municipality == "AHUALULCO"                ~ 24001,
      municipality == "ALAQUINES"               ~ 24002,
      municipality == "AQUISMON"                ~ 24003,
      municipality == "ARMADILLO DE LOS INFANTE" ~ 24004,
      municipality == "AXTLA DE TERRAZAS"       ~ 24053,
      municipality == "CARDENAS"               ~ 24005,
      municipality == "CATORCE"                ~ 24006,
      municipality == "CEDRAL"                 ~ 24007,
      municipality == "CERRITOS"               ~ 24008,
      municipality == "CERRO DE SAN PEDRO"     ~ 24009,
      municipality == "CHARCAS"                ~ 24015,
      municipality == "CIUDAD DEL MAIZ"        ~ 24010,
      municipality == "CIUDAD FERNANDEZ"       ~ 24011,
      municipality == "CIUDAD VALLES"          ~ 24013,
      municipality == "COXCATLAN"              ~ 24014,
      municipality == "EBANO"                  ~ 24016,
      municipality == "EL NARANJO"             ~ 24058,
      municipality == "GUADALCAZAR"            ~ 24017,
      municipality == "HUEHUETLAN"             ~ 24018,
      municipality == "LAGUNILLAS"             ~ 24019,
      municipality == "MATEHUALA"              ~ 24020,
      municipality == "MATLAPA"                ~ 24057,
      municipality == "MEXQUITIC DE CARMONA"   ~ 24021,
      municipality == "MOCTEZUMA"              ~ 24022,
      municipality == "RAYON"                  ~ 24023,
      municipality == "RIOVERDE"               ~ 24024,
      municipality == "SALINAS"                ~ 24025,
      municipality == "SAN ANTONIO"            ~ 24026,
      municipality == "SAN CIRO DE ACOSTA"     ~ 24027,
      municipality == "SAN LUIS POTOSI"        ~ 24028,
      municipality == "SAN MARTIN CHALCHICUAUTLA" ~ 24029,
      municipality == "SAN NICOLAS TOLENTINO"  ~ 24030,
      municipality == "SAN VICENTE TANCUAYALAB" ~ 24034,
      municipality == "SANTA CATARINA"         ~ 24031,
      municipality == "SANTA MARIA DEL RIO"    ~ 24032,
      municipality == "SANTO DOMINGO"          ~ 24033,
      municipality == "SOLEDAD DE GRACIANO SANCHEZ" ~ 24035,
      municipality == "TAMASOPO"               ~ 24036,
      municipality == "TAMAZUNCHALE"           ~ 24037,
      municipality == "TAMPACAN"               ~ 24038,
      municipality == "TAMPAMOLON CORONA"      ~ 24039,
      municipality == "TAMUIN"                 ~ 24040,
      municipality == "TANCANHUITZ"            ~ 24012,
      municipality == "TANLAJAS"               ~ 24041,
      municipality == "TANQUIAN DE ESCOBEDO"   ~ 24042,
      municipality == "TIERRANUEVA"            ~ 24043,
      municipality == "VANEGAS"                ~ 24044,
      municipality == "VENADO"                 ~ 24045,
      municipality == "VILLA DE ARISTA"        ~ 24056,
      municipality == "VILLA DE ARRIAGA"       ~ 24046,
      municipality == "VILLA DE GUADALUPE"     ~ 24047,
      municipality == "VILLA DE LA PAZ"        ~ 24048,
      municipality == "VILLA DE RAMOS"         ~ 24049,
      municipality == "VILLA DE REYES"         ~ 24050,
      municipality == "VILLA HIDALGO"          ~ 24051,
      municipality == "VILLA JUAREZ"           ~ 24052,
      municipality == "XILITLA"                ~ 24054,
      municipality == "ZARAGOZA"               ~ 24055,
      TRUE ~ 0
    ),
    year  = 2006,
    month = "July"
  ) %>%
  arrange(section)


##############################################################################
# ============== 7) Ayu_Seccion_2009.csv  ====================================
##############################################################################
df_2009 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2009.csv") %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

vars_2009 <- c("listanominal","nulos","pan","pri","prd","pt","pvem","pcp","convergencia",
               "panal","psd","pas","psn","noreg","total")
for (v in vars_2009) {
  if (v %in% names(df_2009)) {
    df_2009[[v]] <- as.numeric(df_2009[[v]])
  }
}


df_2009 <- df_2009 %>%
  mutate(turnout = total/listanominal) %>%
  select(-nulos)

df_2009 <- df_2009 %>%
  mutate(
    uniqueid = case_when(
      str_detect(municipality, "AHUALULCO") ~ 24001,
      str_detect(municipality, "ALAQUINES") ~ 24002,
      str_detect(municipality, "AQUISMON") ~ 24003,
      str_detect(municipality, "ARMADILLO DE LOS INFANTE") ~ 24004,
      str_detect(municipality, "AXTLA DE TERRAZAS") ~ 24053,
      str_detect(municipality, "CARDENAS") ~ 24005,
      str_detect(municipality, "CATORCE") ~ 24006,
      str_detect(municipality, "CEDRAL") ~ 24007,
      str_detect(municipality, "CERRITOS") ~ 24008,
      str_detect(municipality, "CERRO DE SAN PEDRO") ~ 24009,
      str_detect(municipality, "CHARCAS") ~ 24015,
      str_detect(municipality, "CIUDAD DEL MAIZ") ~ 24010,
      str_detect(municipality, "CIUDAD FERNANDEZ") ~ 24011,
      str_detect(municipality, "CIUDAD VALLES") ~ 24013,
      str_detect(municipality, "COXCATLAN") ~ 24014,
      str_detect(municipality, "EBANO") ~ 24016,
      str_detect(municipality, "EL NARANJO") ~ 24058,
      str_detect(municipality, "GUADALCAZAR") ~ 24017,
      str_detect(municipality, "HUEHUETLAN") ~ 24018,
      str_detect(municipality, "LAGUNILLAS") ~ 24019,
      str_detect(municipality, "MATEHUALA") ~ 24020,
      str_detect(municipality, "MATLAPA") ~ 24057,
      str_detect(municipality, "MEXQUITIC DE CARMONA") ~ 24021,
      str_detect(municipality, "MOCTEZUMA") ~ 24022,
      str_detect(municipality, "RAYON") ~ 24023,
      str_detect(municipality, "RIOVERDE") ~ 24024,
      str_detect(municipality, "SALINAS") ~ 24025,
      str_detect(municipality, "SAN ANTONIO") ~ 24026,
      str_detect(municipality, "SAN CIRO DE ACOSTA") ~ 24027,
      str_detect(municipality, "SAN LUIS POTOSI") ~ 24028,
      str_detect(municipality, "SAN MARTIN CHALCHICUAUTLA") ~ 24029,
      str_detect(municipality, "SAN NICOLAS TOLENTINO") ~ 24030,
      str_detect(municipality, "SAN VICENTE TANCUAYALAB") ~ 24034,
      str_detect(municipality, "SANTA CATARINA") ~ 24031,
      str_detect(municipality, "SANTA MARIA DEL RIO") ~ 24032,
      str_detect(municipality, "SANTO DOMINGO") ~ 24033,
      str_detect(municipality, "SOLEDAD DE GRACIANO SANCHEZ") ~ 24035,
      str_detect(municipality, "TAMASOPO") ~ 24036,
      str_detect(municipality, "TAMAZUNCHALE") ~ 24037,
      str_detect(municipality, "TAMPACAN") ~ 24038,
      str_detect(municipality, "TAMPAMOLON CORONA") ~ 24039,
      str_detect(municipality, "TAMUIN") ~ 24040,
      str_detect(municipality, "TANCANHUITZ") ~ 24012,
      str_detect(municipality, "TANLAJAS") ~ 24041,
      str_detect(municipality, "TANQUIAN DE ESCOBEDO") ~ 24042,
      str_detect(municipality, "TIERRA NUEVA") ~ 24043,
      str_detect(municipality, "VANEGAS") ~ 24044,
      str_detect(municipality, "VENADO") ~ 24045,
      str_detect(municipality, "VILLA DE ARISTA") ~ 24056,
      str_detect(municipality, "VILLA DE ARRIAGA") ~ 24046,
      str_detect(municipality, "VILLA DE GUADALUPE") ~ 24047,
      str_detect(municipality, "VILLA DE LA PAZ") ~ 24048,
      str_detect(municipality, "VILLA DE RAMOS") ~ 24049,
      str_detect(municipality, "VILLA DE REYES") ~ 24050,
      str_detect(municipality, "VILLA HIDALGO") ~ 24051,
      str_detect(municipality, "VILLA JUAREZ") ~ 24052,
      str_detect(municipality, "XILITLA") ~ 24054,
      str_detect(municipality, "ZARAGOZA") ~ 24055,
      TRUE ~ 0
    )
  )


##############################################################################
# ============== 8) Ayu_Seccion_2012.dta  ====================================
##############################################################################
df_2012 <- read_dta("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_2012.dta") %>%
  mutate(turnout = total/listanominal) %>%
  mutate(municipality = trimws(municipality)) %>%
  mutate(
    uniqueid = case_when(
      str_detect(municipality, "AHUALULCO") ~ 24001,
      str_detect(municipality, "ALAQUINES") ~ 24002,
      str_detect(municipality, "AQUISMON") ~ 24003,
      str_detect(municipality, "ARMADILLO DE LOS INFANTE") ~ 24004,
      str_detect(municipality, "AXTLA DE TERRAZAS") ~ 24053,
      str_detect(municipality, "CARDENAS") ~ 24005,
      str_detect(municipality, "CATORCE") ~ 24006,
      str_detect(municipality, "CEDRAL") ~ 24007,
      str_detect(municipality, "CERRITOS") ~ 24008,
      str_detect(municipality, "CERRO DE SAN PEDRO") ~ 24009,
      str_detect(municipality, "CHARCAS") ~ 24015,
      str_detect(municipality, "CIUDAD DEL MAIZ") ~ 24010,
      str_detect(municipality, "CIUDAD FERNANDEZ") ~ 24011,
      str_detect(municipality, "CIUDAD VALLES") ~ 24013,
      str_detect(municipality, "COXCATLAN") ~ 24014,
      str_detect(municipality, "EBANO") ~ 24016,
      str_detect(municipality, "EL NARANJO") ~ 24058,
      str_detect(municipality, "GUADALCAZAR") ~ 24017,
      str_detect(municipality, "HUEHUETLAN") ~ 24018,
      str_detect(municipality, "LAGUNILLAS") ~ 24019,
      str_detect(municipality, "MATEHUALA") ~ 24020,
      str_detect(municipality, "MATLAPA") ~ 24057,
      str_detect(municipality, "MEXQUITIC DE CARMONA") ~ 24021,
      str_detect(municipality, "MOCTEZUMA") ~ 24022,
      str_detect(municipality, "RAYON") ~ 24023,
      str_detect(municipality, "RIOVERDE") ~ 24024,
      str_detect(municipality, "SALINAS") ~ 24025,
      str_detect(municipality, "SAN ANTONIO") ~ 24026,
      str_detect(municipality, "SAN CIRO DE ACOSTA") ~ 24027,
      str_detect(municipality, "SAN LUIS POTOSI") ~ 24028,
      str_detect(municipality, "SAN MARTIN CHALCHICUAUTLA") ~ 24029,
      str_detect(municipality, "SAN NICOLAS TOLENTINO") ~ 24030,
      str_detect(municipality, "SAN VICENTE TANCUAYALAB") ~ 24034,
      str_detect(municipality, "SANTA CATARINA") ~ 24031,
      str_detect(municipality, "SANTA MARIA DEL RIO") ~ 24032,
      str_detect(municipality, "SANTO DOMINGO") ~ 24033,
      str_detect(municipality, "SOLEDAD DE GRACIANO SANCHEZ") ~ 24035,
      str_detect(municipality, "TAMASOPO") ~ 24036,
      str_detect(municipality, "TAMAZUNCHALE") ~ 24037,
      str_detect(municipality, "TAMPACAN") ~ 24038,
      str_detect(municipality, "TAMPAMOLON CORONA") ~ 24039,
      str_detect(municipality, "TAMUIN") ~ 24040,
      str_detect(municipality, "TANCANHUITZ") ~ 24012,
      str_detect(municipality, "TANLAJAS") ~ 24041,
      str_detect(municipality, "TANQUIAN DE ESCOBEDO") ~ 24042,
      str_detect(municipality, "TIERRA NUEVA") ~ 24043,
      str_detect(municipality, "VANEGAS") ~ 24044,
      str_detect(municipality, "VENADO") ~ 24045,
      str_detect(municipality, "VILLA DE ARISTA") ~ 24056,
      str_detect(municipality, "VILLA DE ARRIAGA") ~ 24046,
      str_detect(municipality, "VILLA DE GUADALUPE") ~ 24047,
      str_detect(municipality, "VILLA DE LA PAZ") ~ 24048,
      str_detect(municipality, "VILLA DE RAMOS") ~ 24049,
      str_detect(municipality, "VILLA DE REYES") ~ 24050,
      str_detect(municipality, "VILLA HIDALGO") ~ 24051,
      str_detect(municipality, "VILLA JUAREZ") ~ 24052,
      str_detect(municipality, "XILITLA") ~ 24054,
      str_detect(municipality, "ZARAGOZA") ~ 24055,
      TRUE ~ 0
    )
  )


df_2012 <- df_2012 %>%
  filter(section!=1107) %>%
  arrange(section) %>%
  mutate(
    year  = 2012,
    month = "July"
  )


all_slp <- bind_rows(df_1997, df_2000, df_2003)

# append the rest
all_slp <- bind_rows(all_slp, df_2006, df_2009, df_2012)

##############################################################################
# ============== 10) Ayuntamientos_2015.xlsx  ===============================
##############################################################################
# Import each sheet, drop if Seccion=="", fill empty strings with "0", save .dta
excel_file_2015 <- "../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayuntamientos_2015.xlsx"
sheet_names_2015 <- excel_sheets(excel_file_2015)

for (sname in sheet_names_2015) {
  df_sheet <- read_excel(excel_file_2015, sheet=sname, col_types="text") %>%
    filter(Seccion!="") %>%
    mutate(across(everything(), ~ ifelse(.=="", "0", .)))
  
  out_name <- gsub("[^A-Za-z0-9_\\. ]", "_", sname)
  write_dta(df_sheet, paste0(out_name, ".dta"))
}

# Then we append the listed .dta files for each municipality.

# We'll do something like:
files_2015 <- c(
    "AHUALULCO.dta",
    "ALAQUINES.dta",
    "AQUISMON.dta",
    "ARMADILLO DE LOS INFANTE.dta",
    "VILLA DE ARRIAGA.dta",
    "AXTLA DE TERRAZAS.dta",
    "CARDENAS.dta",
    "CATORCE.dta",
    "CEDRAL.dta",
    "CERRITOS.dta",
    "CHARCAS.dta",
    "CIUDAD DEL MAIZ.dta",
    "CIUDAD FERNANDEZ.dta",
    "COXCATLAN.dta",
    "EBANO.dta",
    "EL NARANJO.dta",
    "GUADALCAZAR.dta",
    "HUEHUETLÁN.dta",
    "LAGUNILLAS.dta",
    "MATEHUALA.dta",
    "MATLAPA.dta",
    "MEXQUITIC.dta",
    "MOCTEZUMA.dta",
    "RAYON.dta",
    "RIOVERDE.dta",
    "SALINAS.dta",
    "SAN ANTONIO.dta",
    "SAN CIRO DE ACOSTA.dta",
    "SAN MARTIN CHALCH.dta",
    "SAN NICOLAS TOLENTINO.dta",
    "SAN VICENTE TANCUAYALAB.dta",
    "CERRO DE SAN PEDRO.dta",
    "SANTA CATARINA.dta",
    "SANTA MARIA DEL RIO.dta",
    "SANTO DOMINGO.dta",
    "SAN LUIS POTOSI.dta",
    "SOLEDAD.dta",
    "TAMASOPO.dta",
    "TAMAZUNCHALE.dta",
    "TAMPACAN.dta",
    "TAMPAMOLON CORONA.dta",
    "TAMUIN.dta",
    "TANCANHUITZ.dta",
    "TANLAJAS.dta",
    "TANQUIAN DE ESCOBEDO.dta",
    "TIERRA NUEVA.dta",
    "CIUDAD VALLES.dta",
    "VANEGAS.dta",
    "VENADO.dta",
    "VILLA DE ARISTA.dta",
    "VILLA DE LA PAZ.dta",
    "VILLA DE RAMOS.dta",
    "VILLA DE REYES.dta",
    "VILLA DE GUADALUPE.dta",
    "VILLA HIDALGO.dta",
    "VILLA JUAREZ.dta",
    "XILITLA.dta",
    "ZARAGOZA.dta"
  )


all_2015 <- NULL
for (f in files_2015) {
  if (file.exists(f)) {
    temp_df <- read_dta(f)
    all_2015 <- bind_rows(all_2015, temp_df)
  }
}

# drop unwanted columns
all_2015 <- all_2015 %>%
  select(-EXCEDENODEBOLETAS,-PorcentajedeVotacionEmitida,-PorcentajedeVotosNulos,
         -VOTACIONNULAMAYORAL5, -CAPTURADA, -VOTACIONVALIDAEMITIDA)

# destring => numeric
all_2015[] <- lapply(all_2015, function(x) as.numeric(as.character(x)))

all_2015 <- all_2015 %>%
  rename(municipality = Municipio) %>%
  mutate(
    uniqueid = case_when(
      str_detect(municipality, "AHUALULCO") ~ 24001,
      str_detect(municipality, "ALAQUINES") ~ 24002,
      str_detect(municipality, "AQUISMON") ~ 24003,
      str_detect(municipality, "ARMADILLO DE LOS INFANTE") ~ 24004,
      str_detect(municipality, "AXTLA DE TERRAZAS") ~ 24053,
      str_detect(municipality, "CARDENAS") ~ 24005,
      str_detect(municipality, "CATORCE") ~ 24006,
      str_detect(municipality, "CEDRAL") ~ 24007,
      str_detect(municipality, "CERRITOS") ~ 24008,
      str_detect(municipality, "CERRO DE SAN PEDRO") ~ 24009,
      str_detect(municipality, "CHARCAS") ~ 24015,
      str_detect(municipality, "CIUDAD DEL MAIZ") ~ 24010,
      str_detect(municipality, "CIUDAD FERNANDEZ") ~ 24011,
      str_detect(municipality, "CIUDAD VALLES") ~ 24013,
      str_detect(municipality, "COXCATLAN") ~ 24014,
      str_detect(municipality, "EBANO") ~ 24016,
      str_detect(municipality, "EL NARANJO") ~ 24058,
      str_detect(municipality, "GUADALCAZAR") ~ 24017,
      str_detect(municipality, "HUEHUETLAN") ~ 24018,
      str_detect(municipality, "LAGUNILLAS") ~ 24019,
      str_detect(municipality, "MATEHUALA") ~ 24020,
      str_detect(municipality, "MATLAPA") ~ 24057,
      str_detect(municipality, "MEXQUITIC DE CARMONA") ~ 24021,
      str_detect(municipality, "MOCTEZUMA") ~ 24022,
      str_detect(municipality, "RAYON") ~ 24023,
      str_detect(municipality, "RIOVERDE") ~ 24024,
      str_detect(municipality, "SALINAS") ~ 24025,
      str_detect(municipality, "SAN ANTONIO") ~ 24026,
      str_detect(municipality, "SAN CIRO DE ACOSTA") ~ 24027,
      str_detect(municipality, "SAN LUIS POTOSI") ~ 24028,
      str_detect(municipality, "SAN MARTIN CHALCHICUAUTLA") ~ 24029,
      str_detect(municipality, "SAN NICOLAS TOLENTINO") ~ 24030,
      str_detect(municipality, "SAN VICENTE TANCUAYALAB") ~ 24034,
      str_detect(municipality, "SANTA CATARINA") ~ 24031,
      str_detect(municipality, "SANTA MARIA DEL RIO") ~ 24032,
      str_detect(municipality, "SANTO DOMINGO") ~ 24033,
      str_detect(municipality, "SOLEDAD DE GRACIANO SANCHEZ") ~ 24035,
      str_detect(municipality, "TAMASOPO") ~ 24036,
      str_detect(municipality, "TAMAZUNCHALE") ~ 24037,
      str_detect(municipality, "TAMPACAN") ~ 24038,
      str_detect(municipality, "TAMPAMOLON CORONA") ~ 24039,
      str_detect(municipality, "TAMUIN") ~ 24040,
      str_detect(municipality, "TANCANHUITZ") ~ 24012,
      str_detect(municipality, "TANLAJAS") ~ 24041,
      str_detect(municipality, "TANQUIAN DE ESCOBEDO") ~ 24042,
      str_detect(municipality, "TIERRA NUEVA") ~ 24043,
      str_detect(municipality, "VANEGAS") ~ 24044,
      str_detect(municipality, "VENADO") ~ 24045,
      str_detect(municipality, "VILLA DE ARISTA") ~ 24056,
      str_detect(municipality, "VILLA DE ARRIAGA") ~ 24046,
      str_detect(municipality, "VILLA DE GUADALUPE") ~ 24047,
      str_detect(municipality, "VILLA DE LA PAZ") ~ 24048,
      str_detect(municipality, "VILLA DE RAMOS") ~ 24049,
      str_detect(municipality, "VILLA DE REYES") ~ 24050,
      str_detect(municipality, "VILLA HIDALGO") ~ 24051,
      str_detect(municipality, "VILLA JUAREZ") ~ 24052,
      str_detect(municipality, "XILITLA") ~ 24054,
      str_detect(municipality, "ZARAGOZA") ~ 24055,
      TRUE ~ 0
    )
  )

# rename PNA->PANAL, PMC->MC, etc. => replicate those replacements in R
# aggregator => omitted
# rowranks => omitted
# winner => omitted

all_2015 <- all_2015 %>%
  mutate(
    year  = 2015,
    month = "June",
    STATE = "SAN LUIS POTOSI"
  )

# The script also does "collapse (first) winner, by(municipality uniqueid)" => aggregator => omitted.

# Then "use San_Luis_Potosi_Section_2015.dta, collapse => municipal => omitted aggregator"

# Erase .dta files
for (f in files_2015) {
  if (file.exists(f)) file.remove(f)
}

##############################################################################
# ============== 11) MunicipiosSLP_2018.xlsx  ===============================
##############################################################################
# Similar approach for 2018
excel_file_2018 <- "../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/MunicipiosSLP_2018.xlsx"
sheet_names_2018 <- excel_sheets(excel_file_2018)

for (sname in sheet_names_2018) {
  df_sheet <- read_excel(excel_file_2018, sheet=sname, col_types="text") %>%
    mutate(municipality = sname) %>%
    filter(CASILLAACTA!="") %>%
    mutate(across(everything(), ~ifelse(.=="", "0", .)))
  
  safe_sname <- gsub("[^A-Za-z0-9_\\. ]", "_", sname)
  write_dta(df_sheet, paste0(safe_sname, ".dta"))
}

###############################################################################
# 2) List of .dta Files to Append
###############################################################################
filenames <- c(
  "AHUALULCO.dta",
  "ALAQUINES.dta",
  "AQUISMON.dta",
  "ARMADILLO DE LOS INFANTE.dta",
  "VILLA DE ARRIAGA.dta",
  "AXTLA DE TERRAZAS.dta",
  "CARDENAS.dta",
  "CATORCE.dta",
  "CEDRAL.dta",
  "CERRITOS.dta",
  "CHARCAS.dta",
  "CIUDAD DEL MAIZ.dta",
  "CIUDAD FERNANDEZ.dta",
  "COXCATLAN.dta",
  "EBANO.dta",
  "EL NARANJO.dta",
  "GUADALCAZAR.dta",
  "HUEHUETLÁN.dta",
  "LAGUNILLAS.dta",
  "MATEHUALA.dta",
  "MATLAPA.dta",
  "MEXQUITIC.dta",
  "MOCTEZUMA.dta",
  "RAYON.dta",
  "RIOVERDE.dta",
  "SALINAS.dta",
  "SAN ANTONIO.dta",
  "SAN CIRO DE ACOSTA.dta",
  "SAN MARTIN CHALCH.dta",
  "SAN NICOLAS TOLENTINO.dta",
  "SAN VICENTE TANCUAYALAB.dta",
  "CERRO DE SAN PEDRO.dta",
  "SANTA CATARINA.dta",
  "SANTA MARIA DEL RIO.dta",
  "SANTO DOMINGO.dta",
  "SAN LUIS POTOSI.dta",
  "SOLEDAD.dta",
  "TAMASOPO.dta",
  "TAMAZUNCHALE.dta",
  "TAMPACAN.dta",
  "TAMPAMOLON CORONA.dta",
  "TAMUIN.dta",
  "TANCANHUITZ.dta",
  "TANLAJAS.dta",
  "TANQUIAN DE ESCOBEDO.dta",
  "TIERRA NUEVA.dta",
  "CIUDAD VALLES.dta",
  "VANEGAS.dta",
  "VENADO.dta",
  "VILLA DE ARISTA.dta",
  "VILLA DE LA PAZ.dta",
  "VILLA DE RAMOS.dta",
  "VILLA DE REYES.dta",
  "VILLA DE GUADALUPE.dta",
  "VILLA HIDALGO.dta",
  "VILLA JUAREZ.dta",
  "XILITLA.dta",
  "ZARAGOZA.dta"
)

###############################################################################
# 3) Read the First File
###############################################################################

all_2018 <- NULL
for (f in files_2018) {
  if (file.exists(f)) {
    temp_df <- read_dta(f)
    all_2018 <- bind_rows(all_2018, temp_df)
  }
}

# drop O,P,section
all_2018 <- all_2018 %>%
  select(-O, -P, -section)

# create SECCION = substr(CASILLAACTA,1,4)
all_2018 <- all_2018 %>%
  mutate(SECCION = substr(CASILLAACTA,1,4))

# destring => numeric
all_2018[] <- lapply(all_2018, function(x) as.numeric(as.character(x)))

all_2018 <- all_2018 %>%
  mutate(PTMORENAPES = ifelse(row_number()==3011, 0, PTMORENAPES)) %>%
  mutate(
    year  = 2018,
    month = "July",
    STATE = "SAN LUIS POTOSI"
  )

# Merge m:1 municipality using uniqueids.dta => replicate partial
df_uniqueids <- read_dta("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Other/uniqueids.dta")  # created by code above
all_2018 <- all_2018 %>%
  left_join(df_uniqueids, by="municipality") %>%
  select(-municipality) %>%
  rename(municipality = municipality2)

# aggregator => omitted
# Then merges incumbents2018.dta => also partial
df_incs_2018 <- read_dta("incumbents2018.dta")
all_2018 <- all_2018 %>%
  left_join(df_incs_2018, by="uniqueid")

# more recodes => see code
all_2018 <- all_2018 %>%
  mutate(
    STATE="SAN LUIS POTOSI"
  )

# Merge ListadoNominalPREP2018 => replicate partial
df_ln18 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ListadoNominalPREP2018.dta") %>%
  filter(STATE=="SAN LUIS POTOSI") %>%
  rename(listanominal=ListadoNominalINE)

all_2018 <- all_2018 %>%
  left_join(df_ln18, by=c("STATE","SECCION"="section")) %>%
  filter(!is.na(listanominal))

all_2018 <- all_2018 %>%
  mutate(
    turnout = total/listanominal
  )


# Erase the .dta files
for (f in files_2018) {
  if (file.exists(f)) file.remove(f)
}

##############################################################################
# ============== 12) Append 2015 + 2018, then with older data ===============
##############################################################################

all_slp_final <- bind_rows(all_slp,df_2015, df_2018)

##############################################################################
# Done
##############################################################################


data.table::fwrite(all_slp_final,"../../../Processed Data/sanluispotosi/slp_process_raw_data.csv")

